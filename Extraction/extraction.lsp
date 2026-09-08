;;; ============================================================
;;; extraction.lsp  (исправления повторного открытия и проверки)
;;; Команды: EXTRACTION, ЭКСТРАКЦИЯ
;;; ============================================================
(vl-load-com)

;; ---------- Глобальные переменные для хранения последних настроек ----------
(if (not (boundp '*EXTRACTION-LAST-TASK*)) (setq *EXTRACTION-LAST-TASK* 'FASONKA))
(if (not (boundp '*EXTRACTION-LAST-REPORT-MODE*)) (setq *EXTRACTION-LAST-REPORT-MODE* "DETAIL"))
(if (not (boundp '*EXTRACTION-LAST-EXPORT-EXCEL*)) (setq *EXTRACTION-LAST-EXPORT-EXCEL* nil))
(if (not (boundp '*EXTRACTION-LAST-EXPORT-TXT*)) (setq *EXTRACTION-LAST-EXPORT-TXT* nil))
(if (not (boundp '*EXTRACTION-LAST-CREATE-TABLE*)) (setq *EXTRACTION-LAST-CREATE-TABLE* T))
(if (not (boundp '*EXTRACTION-LAST-SELECTED-LAYERS*)) (setq *EXTRACTION-LAST-SELECTED-LAYERS* nil))
(if (not (boundp '*EXTRACTION-LAST-FILTER-FACADES*)) (setq *EXTRACTION-LAST-FILTER-FACADES* nil))
(if (not (boundp '*EXTRACTION-LAST-FILTER-VITRAZH*)) (setq *EXTRACTION-LAST-FILTER-VITRAZH* nil))
(if (not (boundp '*EXTRACTION-LAST-FILTER-FONAR*)) (setq *EXTRACTION-LAST-FILTER-FONAR* nil))
(if (not (boundp '*EXTRACTION-LAST-SUBSYSTEM-CHECKS*)) (setq *EXTRACTION-LAST-SUBSYSTEM-CHECKS* '(T T T)))

;; ---------- Глобальное состояние (рабочие переменные) ----------
(setq *EXTRACTION-DCL-ID* nil)
(setq *EXTRACTION-ALL-LAYERS* nil)
(setq *EXTRACTION-VISIBLE-LAYERS* nil)
(setq *EXTRACTION-SELECTED-LAYERS* nil)
(setq *EXTRACTION-SELECTED-INDICES* nil)
(setq *EXTRACTION-FILTER-FACADES* nil)
(setq *EXTRACTION-FILTER-VITRAZH* nil)
(setq *EXTRACTION-FILTER-FONAR* nil)
(setq *EXTRACTION-TASK-ID* *EXTRACTION-LAST-TASK*)
(setq *EXTRACTION-REPORT-MODE* *EXTRACTION-LAST-REPORT-MODE*)
(setq *EXTRACTION-EXPORT-EXCEL* *EXTRACTION-LAST-EXPORT-EXCEL*)
(setq *EXTRACTION-EXPORT-TXT* *EXTRACTION-LAST-EXPORT-TXT*)
(setq *EXTRACTION-CREATE-TABLE* *EXTRACTION-LAST-CREATE-TABLE*)
(setq *EXTRACTION-ACTION* 'CANCEL)

;; ---------- Безопасное получение списка слоёв ----------
(defun extraction-unique-ci (lst / out x key)
  (setq out '())
  (if (listp lst)
    (foreach x lst
      (if (= (type x) 'STR)
        (progn
          (setq key (strcase x))
          (if (not (vl-some '(lambda (y) (= (strcase y) key)) out))
            (setq out (append out (list x)))
          )
        )
      )
    )
  )
  out
)

(defun extraction-layer-names ( / acad doc layers out name)
  (setq out '())
  (setq acad (vl-catch-all-apply 'vlax-get-acad-object '()))
  (if (and (not (vl-catch-all-error-p acad)) acad)
    (progn
      (setq doc (vl-catch-all-apply 'vla-get-ActiveDocument (list acad)))
      (if (and (not (vl-catch-all-error-p doc)) doc)
        (progn
          (setq layers (vl-catch-all-apply 'vla-get-Layers (list doc)))
          (if (and (not (vl-catch-all-error-p layers)) layers)
            (vlax-for lay layers
              (setq name (vl-catch-all-apply 'vla-get-Name (list lay)))
              (if (and (not (vl-catch-all-error-p name))
                       (= (type name) 'STR)
                       (> (strlen name) 0))
                (setq out (cons name out))
              )
            )
          )
        )
      )
    )
  )
  (setq out (vl-remove-if-not '(lambda (x) (= (type x) 'STR)) out))
  (setq out (vl-sort out '(lambda (a b) (< (strcase a) (strcase b)))))
  (extraction-unique-ci out)
)

;; ---------- Загрузка модулей ----------
(defun extraction-project-root ( / dsp)
  (setq dsp (findfile "extraction.lsp"))
  (if dsp (vl-filename-directory (vl-filename-directory dsp)) nil)
)
(defun extraction-modules-dir ( / dsp)
  (setq dsp (findfile "extraction.lsp"))
  (if dsp (vl-filename-directory dsp) nil)
)
(defun extraction-load-all ( / root common f path)
  (setq root (extraction-project-root))
  (if root
    (progn
      (setq common (strcat root "\\common\\"))
      (foreach f '("task-utils.lsp" "layer-utils.lsp" "select-utils.lsp" "excel-utils.lsp" "table-utils.lsp" "txt-utils.lsp")
        (setq path (strcat common f))
        (if (findfile path) (load path)
          (princ (strcat "\n[EXTRACTION] Не найден: " path)))
      )
      (setq path (strcat root "\\Extraction\\fasonka.lsp"))
      (if (findfile path) (load path)
        (princ (strcat "\n[EXTRACTION] Не найден: " path)))
      (setq path (strcat root "\\Extraction\\subsystem.lsp"))
      (if (findfile path) (load path)
        (princ (strcat "\n[EXTRACTION] Не найден: " path)))
      (setq path (strcat root "\\Extraction\\cutline.lsp"))
      (if (findfile path) (load path)
        (princ (strcat "\n[EXTRACTION] Не найден: " path)))
      (setq path (strcat root "\\Extraction\\cutsheet.lsp"))
      (if (findfile path) (load path)
        (princ (strcat "\n[EXTRACTION] Не найден: " path)))
    )
    (princ "\n[EXTRACTION] Корень проекта не найден.")
  )
  T
)

;; ---------- Список слоёв в диалоге ----------
(defun extraction-parse-indices (s / x)
  (if (and (= (type s) 'STR) (/= s ""))
    (progn
      (setq x (read (strcat "(" s ")")))
      (if (= (type x) 'LIST) x nil)
    )
    nil
  )
)

(defun extraction-selected-names ( / s indices out i n)
  (setq s (get_tile "lst_layers"))
  (if (or (null s) (/= (type s) 'STR) (= s ""))
    nil
    (progn
      (setq indices (extraction-parse-indices s))
      (setq out '())
      (setq n (length *EXTRACTION-VISIBLE-LAYERS*))
      (foreach i indices
        (if (and (numberp i) (>= i 0) (< i n))
          (setq out (cons (nth i *EXTRACTION-VISIBLE-LAYERS*) out))
        )
      )
      (extraction-unique-ci out)
    )
  )
)

;; Получение слоёв из выбранных фильтров по ключевым словам
(defun extraction-filter-layers-by-keywords (keywords / filters result f fname)
  (setq filters (vl-catch-all-apply 'tu-group-filter-names-and-layers '()))
  (if (vl-catch-all-error-p filters) (setq filters nil))
  (setq result '())
  (if (listp filters)
    (foreach f filters
      (setq fname (car f))
      (if (and (= (type fname) 'STR)
               (vl-some '(lambda (k)
                           (and (= (type k) 'STR)
                                (vl-string-search (strcase k) (strcase fname))))
                        keywords))
        (setq result (append result (cadr f)))
      )
    )
  )
  (extraction-unique-ci result)
)

(defun extraction-rebuild-layer-list ( / restore vis keywords selected i item)
  ;; Перестраиваем список без восстановления выбора
  (setq *EXTRACTION-SELECTED-INDICES* '())

  (setq keywords '())
  (if *EXTRACTION-FILTER-FACADES* (setq keywords (cons "фасад" keywords)))
  (if *EXTRACTION-FILTER-VITRAZH* (setq keywords (cons "витраж" keywords)))
  (if *EXTRACTION-FILTER-FONAR*   (setq keywords (cons "фонар" keywords)))

  (if keywords
    (progn
      (setq vis (extraction-filter-layers-by-keywords keywords))
      (if (or (null vis) (not (listp vis)))
        (setq vis *EXTRACTION-ALL-LAYERS*)
      )
      (setq *EXTRACTION-VISIBLE-LAYERS* vis)
    )
    (setq *EXTRACTION-VISIBLE-LAYERS* *EXTRACTION-ALL-LAYERS*)
  )

  (setq *EXTRACTION-VISIBLE-LAYERS*
    (vl-sort
      (vl-remove-if-not '(lambda (x) (= (type x) 'STR))
                        (if (listp *EXTRACTION-VISIBLE-LAYERS*) *EXTRACTION-VISIBLE-LAYERS* '()))
      '(lambda (a b) (< (strcase a) (strcase b)))
    )
  )

  (start_list "lst_layers")
  (mapcar 'add_list *EXTRACTION-VISIBLE-LAYERS*)
  (end_list)

  ;; Очищаем выбор
  (set_tile "lst_layers" "")
)

;; ---------- Сброс выбора в списке слоёв ----------
(defun extraction-clear-layer-selection ()
  (set_tile "lst_layers" "")
)

;; ---------- Установка выделения определённых слоёв ----------
(defun extraction-select-layers-in-list (layers-to-select / i item selected str)
  (setq selected '())
  (setq i 0)
  (foreach item *EXTRACTION-VISIBLE-LAYERS*
    (if (vl-some '(lambda (x) (= (strcase x) (strcase item))) layers-to-select)
      (setq selected (cons i selected))
    )
    (setq i (1+ i))
  )
  (setq selected (reverse selected))
  (setq str "")
  (foreach i selected
    (setq str (if (= str "") (itoa i) (strcat str " " (itoa i))))
  )
  (set_tile "lst_layers" str)
)

;; ---------- Синхронизация чекбоксов подсистемы из списка слоёв ----------
(defun extraction-sync-checks-from-layers ()
  (setq selected (extraction-selected-names))
  (set_tile "chk_subsystem_1" (if (member "Подсистема" selected) "1" "0"))
  (set_tile "chk_subsystem_2" (if (member "Подсистема оцинкованная" selected) "1" "0"))
  (set_tile "chk_subsystem_3" (if (member "Подсистема алюминиевая" selected) "1" "0"))
)

;; ---------- Обработчик чекбокса подсистемы ----------
(defun extraction-subsystem-check-changed (key / val layers-to-select)
  (setq val (= (get_tile (strcat "chk_subsystem_" (itoa key))) "1"))
  (setq *EXTRACTION-LAST-SUBSYSTEM-CHECKS*
        (list
          (if (= key 1) val (car *EXTRACTION-LAST-SUBSYSTEM-CHECKS*))
          (if (= key 2) val (cadr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*))
          (if (= key 3) val (caddr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*))
        )
  )
  (setq layers-to-select '())
  (if (car *EXTRACTION-LAST-SUBSYSTEM-CHECKS*) (setq layers-to-select (cons "Подсистема" layers-to-select)))
  (if (cadr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*) (setq layers-to-select (cons "Подсистема оцинкованная" layers-to-select)))
  (if (caddr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*) (setq layers-to-select (cons "Подсистема алюминиевая" layers-to-select)))
  (extraction-select-layers-in-list layers-to-select)
  (extraction-sync-checks-from-layers)
)

;; ---------- Переключение задачи ----------
(defun extraction-toggle-subsystem-layers ( / layers-to-select)
  (if (= (get_tile "rb_task_subsystem") "1")
    (progn
      (mode_tile "box_subsystem_layers" 0)
      (mode_tile "chk_subsystem_1" 0)
      (mode_tile "chk_subsystem_2" 0)
      (mode_tile "chk_subsystem_3" 0)
      (set_tile "chk_subsystem_1" (if (car *EXTRACTION-LAST-SUBSYSTEM-CHECKS*) "1" "0"))
      (set_tile "chk_subsystem_2" (if (cadr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*) "1" "0"))
      (set_tile "chk_subsystem_3" (if (caddr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*) "1" "0"))
      (setq *EXTRACTION-FILTER-FACADES* nil)
      (setq *EXTRACTION-FILTER-VITRAZH* nil)
      (setq *EXTRACTION-FILTER-FONAR* nil)
      (extraction-rebuild-layer-list)
      (setq layers-to-select '())
      (if (car *EXTRACTION-LAST-SUBSYSTEM-CHECKS*) (setq layers-to-select (cons "Подсистема" layers-to-select)))
      (if (cadr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*) (setq layers-to-select (cons "Подсистема оцинкованная" layers-to-select)))
      (if (caddr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*) (setq layers-to-select (cons "Подсистема алюминиевая" layers-to-select)))
      (extraction-select-layers-in-list layers-to-select)
    )
    (progn
      (mode_tile "box_subsystem_layers" 1)
      (mode_tile "chk_subsystem_1" 1)
      (mode_tile "chk_subsystem_2" 1)
      (mode_tile "chk_subsystem_3" 1)
      (extraction-clear-layer-selection)
      (setq *EXTRACTION-SELECTED-LAYERS* nil)
      (setq *EXTRACTION-LAST-SELECTED-LAYERS* nil)
    )
  )
)

;; ---------- Обработчик изменения выбора в списке слоёв ----------
(defun extraction-layer-selection-changed ()
  (setq *EXTRACTION-SELECTED-LAYERS* (extraction-selected-names))
  (if (eq *EXTRACTION-TASK-ID* 'SUBSYSTEM)
    (extraction-sync-checks-from-layers)
  )
)

;; ---------- Чтение параметров ----------
(defun extraction-read-params ( / selected)
  (setq selected (extraction-selected-names))
  (setq *EXTRACTION-SELECTED-LAYERS* (if selected selected nil))
  (setq *EXTRACTION-LAST-SELECTED-LAYERS* *EXTRACTION-SELECTED-LAYERS*)
  (setq *EXTRACTION-REPORT-MODE* (if (= (get_tile "rb_summary") "1") "SUMMARY" "DETAIL"))
  (setq *EXTRACTION-LAST-REPORT-MODE* *EXTRACTION-REPORT-MODE*)
  (setq *EXTRACTION-EXPORT-EXCEL* (= (get_tile "chk_xls") "1"))
  (setq *EXTRACTION-LAST-EXPORT-EXCEL* *EXTRACTION-EXPORT-EXCEL*)
  (setq *EXTRACTION-EXPORT-TXT* (= (get_tile "chk_txt") "1"))
  (setq *EXTRACTION-LAST-EXPORT-TXT* *EXTRACTION-EXPORT-TXT*)
  (setq *EXTRACTION-CREATE-TABLE* (= (get_tile "chk_acad") "1"))
  (setq *EXTRACTION-LAST-CREATE-TABLE* *EXTRACTION-CREATE-TABLE*)
  (cond
    ((= (get_tile "rb_task_fasonka") "1")      (setq *EXTRACTION-TASK-ID* 'FASONKA))
    ((= (get_tile "rb_task_subsystem") "1")    (setq *EXTRACTION-TASK-ID* 'SUBSYSTEM))
    ((= (get_tile "rb_task_cladding") "1")     (setq *EXTRACTION-TASK-ID* 'CLADDING))
    ((= (get_tile "rb_task_vitrazh") "1")      (setq *EXTRACTION-TASK-ID* 'VITRAZH))
    ((= (get_tile "rb_task_zapolnenie") "1")   (setq *EXTRACTION-TASK-ID* 'ZAPOLNENIE))
    (t (setq *EXTRACTION-TASK-ID* 'FASONKA))
  )
  (setq *EXTRACTION-LAST-TASK* *EXTRACTION-TASK-ID*)
  (if (and (eq *EXTRACTION-TASK-ID* 'SUBSYSTEM) (null *EXTRACTION-SELECTED-LAYERS*))
    (progn
      (alert "Не выбрано ни одного слоя. Поиск по всем слоям может занять много времени.")
      nil
    )
    T
  )
)

;; ---------- Диспетчер ----------
(defun run-task (task-id layers report-mode export-excel export-txt create-table save-base / r)
  (cond
    ((eq task-id 'FASONKA)
     (setq r (vl-catch-all-apply 'fasonka-main
               (list layers report-mode export-excel export-txt create-table save-base)))
     (if (vl-catch-all-error-p r)
       (princ "\nМодуль Фасонка не загружен или ошибка выполнения.")))
    ((eq task-id 'SUBSYSTEM)
     (setq r (vl-catch-all-apply 'subsystem-main
               (list layers report-mode export-excel export-txt create-table save-base)))
     (if (vl-catch-all-error-p r)
       (princ "\nМодуль Подсистема не загружен или ошибка выполнения.")))
    ((eq task-id 'CLADDING)
     (setq r (vl-catch-all-apply 'cladding-main
               (list layers report-mode export-excel export-txt create-table save-base)))
     (if (vl-catch-all-error-p r) (princ "\nМодуль Облицовка не загружен.")))
    ((eq task-id 'VITRAZH)
     (setq r (vl-catch-all-apply 'vitrazh-main
               (list layers report-mode export-excel export-txt create-table save-base)))
     (if (vl-catch-all-error-p r) (princ "\nМодуль Витраж не загружен.")))
    ((eq task-id 'ZAPOLNENIE)
     (setq r (vl-catch-all-apply 'zapolnenie-main
               (list layers report-mode export-excel export-txt create-table save-base)))
     (if (vl-catch-all-error-p r) (princ "\nМодуль Заполнение не загружен.")))
    (t (princ "\nНеизвестная задача."))
  )
)

;; ---------- Обработчики ----------
(defun extraction-help ()
  (alert (strcat "Окно запуска задач.\n\n"
                 "Выбор одной задачи, слоёв, режима отчёта и форматов вывода.\n"
                 "Кнопки Раскрой хлыста / Раскрой листа запускают модули раскроя.\n\n"
                 "Сохранить - имя файла по умолчанию.\n"
                 "Сохранить как... - выбор пути и имени.\n"
                 "Если слои не выбраны - поиск по всем слоям."))
)
(defun extraction-save ()
  (if (extraction-read-params)
    (progn
      (setq *EXTRACTION-ACTION* 'SAVE)
      (done_dialog 1)
    )
  )
)
(defun extraction-saveas ()
  (if (extraction-read-params)
    (progn
      (setq *EXTRACTION-ACTION* 'SAVEAS)
      (done_dialog 1)
    )
  )
)
(defun extraction-close  () (setq *EXTRACTION-ACTION* 'CANCEL) (done_dialog 0))
(defun extraction-cutline () (setq *EXTRACTION-ACTION* 'CUTLINE) (done_dialog 1))
(defun extraction-cutsheet () (setq *EXTRACTION-ACTION* 'CUTSHEET) (done_dialog 1))

(defun extraction-filter-facades ()
  (setq *EXTRACTION-FILTER-FACADES* (= (get_tile "chk_filter_facades") "1"))
  (setq *EXTRACTION-LAST-FILTER-FACADES* *EXTRACTION-FILTER-FACADES*)
  (extraction-rebuild-layer-list)
)
(defun extraction-filter-vitrazh ()
  (setq *EXTRACTION-FILTER-VITRAZH* (= (get_tile "chk_filter_vitrazh") "1"))
  (setq *EXTRACTION-LAST-FILTER-VITRAZH* *EXTRACTION-FILTER-VITRAZH*)
  (extraction-rebuild-layer-list)
)
(defun extraction-filter-fonar ()
  (setq *EXTRACTION-FILTER-FONAR* (= (get_tile "chk_filter_fonar") "1"))
  (setq *EXTRACTION-LAST-FILTER-FONAR* *EXTRACTION-FILTER-FONAR*)
  (extraction-rebuild-layer-list)
)

(defun extraction-layer-selection ()
  (extraction-layer-selection-changed)
)

;; ---------- Основная команда ----------
(defun c:extraction ( / dcl-file save-base modules-dir r)
  (vl-load-com)
  (extraction-load-all)

  (setq dcl-file nil)
  (setq modules-dir (extraction-modules-dir))
  (if modules-dir (setq dcl-file (findfile (strcat modules-dir "\\extraction.dcl"))))
  (if (null dcl-file) (setq dcl-file (findfile "extraction.dcl")))

  (if (null dcl-file)
    (progn (alert "Не найден файл extraction.dcl.") (princ))
    (progn
      ;; Инициализация рабочих переменных из сохранённых
      (setq *EXTRACTION-ALL-LAYERS* (extraction-layer-names))
      (setq *EXTRACTION-VISIBLE-LAYERS* *EXTRACTION-ALL-LAYERS*)
      (setq *EXTRACTION-SELECTED-LAYERS* nil)
      (setq *EXTRACTION-FILTER-FACADES* *EXTRACTION-LAST-FILTER-FACADES*)
      (setq *EXTRACTION-FILTER-VITRAZH* *EXTRACTION-LAST-FILTER-VITRAZH*)
      (setq *EXTRACTION-FILTER-FONAR* *EXTRACTION-LAST-FILTER-FONAR*)
      (setq *EXTRACTION-TASK-ID* *EXTRACTION-LAST-TASK*)
      (setq *EXTRACTION-REPORT-MODE* *EXTRACTION-LAST-REPORT-MODE*)
      (setq *EXTRACTION-EXPORT-EXCEL* *EXTRACTION-LAST-EXPORT-EXCEL*)
      (setq *EXTRACTION-EXPORT-TXT* *EXTRACTION-LAST-EXPORT-TXT*)
      (setq *EXTRACTION-CREATE-TABLE* *EXTRACTION-LAST-CREATE-TABLE*)
      (setq *EXTRACTION-ACTION* 'CANCEL)

      (setq *EXTRACTION-DCL-ID* (load_dialog dcl-file))
      (if (< *EXTRACTION-DCL-ID* 0)
        (alert "Не удалось загрузить extraction.dcl.")
        (progn
          (if (new_dialog "extraction_dialog" *EXTRACTION-DCL-ID*)
            (progn
              ;; Установка радиокнопок и чекбоксов из сохранённых настроек
              (set_tile "rb_detail" (if (= *EXTRACTION-LAST-REPORT-MODE* "DETAIL") "1" "0"))
              (set_tile "rb_summary" (if (= *EXTRACTION-LAST-REPORT-MODE* "SUMMARY") "1" "0"))
              (set_tile "chk_xls" (if *EXTRACTION-LAST-EXPORT-EXCEL* "1" "0"))
              (set_tile "chk_txt" (if *EXTRACTION-LAST-EXPORT-TXT* "1" "0"))
              (set_tile "chk_acad" (if *EXTRACTION-LAST-CREATE-TABLE* "1" "0"))

              (set_tile "chk_filter_facades" (if *EXTRACTION-LAST-FILTER-FACADES* "1" "0"))
              (set_tile "chk_filter_vitrazh" (if *EXTRACTION-LAST-FILTER-VITRAZH* "1" "0"))
              (set_tile "chk_filter_fonar" (if *EXTRACTION-LAST-FILTER-FONAR* "1" "0"))

              (cond
                ((eq *EXTRACTION-LAST-TASK* 'FASONKA) (set_tile "rb_task_fasonka" "1"))
                ((eq *EXTRACTION-LAST-TASK* 'SUBSYSTEM) (set_tile "rb_task_subsystem" "1"))
                ((eq *EXTRACTION-LAST-TASK* 'CLADDING) (set_tile "rb_task_cladding" "1"))
                ((eq *EXTRACTION-LAST-TASK* 'VITRAZH) (set_tile "rb_task_vitrazh" "1"))
                ((eq *EXTRACTION-LAST-TASK* 'ZAPOLNENIE) (set_tile "rb_task_zapolnenie" "1"))
                (t (set_tile "rb_task_fasonka" "1"))
              )

              ;; Установка чекбоксов подсистемы
              (set_tile "chk_subsystem_1" (if (car *EXTRACTION-LAST-SUBSYSTEM-CHECKS*) "1" "0"))
              (set_tile "chk_subsystem_2" (if (cadr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*) "1" "0"))
              (set_tile "chk_subsystem_3" (if (caddr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*) "1" "0"))

              ;; Перестроение списка слоёв
              (extraction-rebuild-layer-list)

              ;; Восстановление полного выбора слоёв (если он был сохранён)
              (if *EXTRACTION-LAST-SELECTED-LAYERS*
                (extraction-select-layers-in-list *EXTRACTION-LAST-SELECTED-LAYERS*)
              )

              ;; Показать/скрыть блок подсистемы в зависимости от задачи
              (if (eq *EXTRACTION-TASK-ID* 'SUBSYSTEM)
                (progn
                  (mode_tile "box_subsystem_layers" 0)
                  (mode_tile "chk_subsystem_1" 0)
                  (mode_tile "chk_subsystem_2" 0)
                  (mode_tile "chk_subsystem_3" 0)
                  (extraction-sync-checks-from-layers)
                )
                (progn
                  (mode_tile "box_subsystem_layers" 1)
                  (mode_tile "chk_subsystem_1" 1)
                  (mode_tile "chk_subsystem_2" 1)
                  (mode_tile "chk_subsystem_3" 1)
                )
              )

              ;; Обработчики
              (action_tile "btn_help"   "(extraction-help)")
              (action_tile "btn_save"   "(extraction-save)")
              (action_tile "btn_saveas" "(extraction-saveas)")
              (action_tile "btn_close"  "(extraction-close)")
              (action_tile "btn_cutline" "(extraction-cutline)")
              (action_tile "btn_cutsheet" "(extraction-cutsheet)")
              (action_tile "chk_filter_facades" "(extraction-filter-facades)")
              (action_tile "chk_filter_vitrazh" "(extraction-filter-vitrazh)")
              (action_tile "chk_filter_fonar"   "(extraction-filter-fonar)")
              (action_tile "lst_layers" "(extraction-layer-selection)")
              (action_tile "rb_detail"  "(setq *EXTRACTION-REPORT-MODE* \"DETAIL\")(setq *EXTRACTION-LAST-REPORT-MODE* \"DETAIL\")")
              (action_tile "rb_summary" "(setq *EXTRACTION-REPORT-MODE* \"SUMMARY\")(setq *EXTRACTION-LAST-REPORT-MODE* \"SUMMARY\")")
              (action_tile "rb_task_fasonka"   "(setq *EXTRACTION-TASK-ID* 'FASONKA)(setq *EXTRACTION-LAST-TASK* 'FASONKA)(extraction-toggle-subsystem-layers)")
              (action_tile "rb_task_subsystem" "(setq *EXTRACTION-TASK-ID* 'SUBSYSTEM)(setq *EXTRACTION-LAST-TASK* 'SUBSYSTEM)(extraction-toggle-subsystem-layers)")
              (action_tile "rb_task_cladding"  "(setq *EXTRACTION-TASK-ID* 'CLADDING)(setq *EXTRACTION-LAST-TASK* 'CLADDING)(extraction-toggle-subsystem-layers)")
              (action_tile "rb_task_vitrazh"   "(setq *EXTRACTION-TASK-ID* 'VITRAZH)(setq *EXTRACTION-LAST-TASK* 'VITRAZH)(extraction-toggle-subsystem-layers)")
              (action_tile "rb_task_zapolnenie" "(setq *EXTRACTION-TASK-ID* 'ZAPOLNENIE)(setq *EXTRACTION-LAST-TASK* 'ZAPOLNENIE)(extraction-toggle-subsystem-layers)")
              (action_tile "chk_subsystem_1" "(extraction-subsystem-check-changed 1)")
              (action_tile "chk_subsystem_2" "(extraction-subsystem-check-changed 2)")
              (action_tile "chk_subsystem_3" "(extraction-subsystem-check-changed 3)")

              (start_dialog)

              (cond
                ((eq *EXTRACTION-ACTION* 'SAVE)
                 (run-task *EXTRACTION-TASK-ID* *EXTRACTION-SELECTED-LAYERS*
                           *EXTRACTION-REPORT-MODE* *EXTRACTION-EXPORT-EXCEL*
                           *EXTRACTION-EXPORT-TXT* *EXTRACTION-CREATE-TABLE* nil))
                ((eq *EXTRACTION-ACTION* 'SAVEAS)
                 (setq save-base (vl-catch-all-apply 'tu-get-save-base (list *EXTRACTION-TASK-ID*)))
                 (if (vl-catch-all-error-p save-base) (setq save-base nil))
                 (run-task *EXTRACTION-TASK-ID* *EXTRACTION-SELECTED-LAYERS*
                           *EXTRACTION-REPORT-MODE* *EXTRACTION-EXPORT-EXCEL*
                           *EXTRACTION-EXPORT-TXT* *EXTRACTION-CREATE-TABLE* save-base))
                ((eq *EXTRACTION-ACTION* 'CUTLINE)
                 (setq r (vl-catch-all-apply 'c:CUTLINE '()))
                 (if (vl-catch-all-error-p r)
                   (princ "\nCUTLINE не загружен. Загрузите cutline.lsp.")))
                ((eq *EXTRACTION-ACTION* 'CUTSHEET)
                 (setq r (vl-catch-all-apply 'c:CUTSHEET '()))
                 (if (vl-catch-all-error-p r)
                   (princ "\nРаскрой листа: модуль в разработке (CUTSHEET).")))
                (t nil))
            )
            (alert "Не удалось создать диалог extraction_dialog.")
          )
          (unload_dialog *EXTRACTION-DCL-ID*)
          (setq *EXTRACTION-DCL-ID* nil)
        )
      )
    )
  )
  (princ)
)

;; Русская команда
(defun c:Экстракция () (c:extraction))

(princ "\nEXTRACTION.LSP загружен. Команды: EXTRACTION, ЭКСТРАКЦИЯ")
(princ)