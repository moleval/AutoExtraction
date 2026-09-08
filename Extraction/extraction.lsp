;;; ============================================================
;;; extraction.lsp  (версия с тремя отдельными фильтрами)
;;; Команды: EXTRACTION, ЭКСТРАКЦИЯ
;;; ============================================================
(vl-load-com)

;; ---------- Глобальное состояние ----------
(setq *EXTRACTION-DCL-ID* nil)
(setq *EXTRACTION-ALL-LAYERS* nil)
(setq *EXTRACTION-VISIBLE-LAYERS* nil)
(setq *EXTRACTION-SELECTED-LAYERS* nil)
(setq *EXTRACTION-SELECTED-INDICES* nil)
(setq *EXTRACTION-FILTER-FACADES* nil)   ; Фасады
(setq *EXTRACTION-FILTER-VITRAZH* nil)   ; Витражи
(setq *EXTRACTION-FILTER-FONAR* nil)     ; Фонарь 3D
(setq *EXTRACTION-TASK-ID* 'FASONKA)
(setq *EXTRACTION-REPORT-MODE* "DETAIL")
(setq *EXTRACTION-EXPORT-EXCEL* nil)
(setq *EXTRACTION-EXPORT-TXT* nil)
(setq *EXTRACTION-CREATE-TABLE* T)
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
      (foreach f '("task-utils.lsp" "layer-utils.lsp" "excel-utils.lsp" "table-utils.lsp" "txt-utils.lsp")
        (setq path (strcat common f))
        (if (findfile path) (load path)
          (princ (strcat "\n[EXTRACTION] Не найден: " path)))
      )
      (setq path (strcat root "\\Extraction\\fasonka.lsp"))
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
  (setq restore (extraction-selected-names))
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

  (setq i 0)
  (foreach item *EXTRACTION-VISIBLE-LAYERS*
    (if (vl-some '(lambda (x) (and (= (type x) 'STR) (= (type item) 'STR)
                                   (= (strcase x) (strcase item)))) restore)
      (setq *EXTRACTION-SELECTED-INDICES* (cons i *EXTRACTION-SELECTED-INDICES*))
    )
    (setq i (1+ i))
  )

  (setq selected "")
  (foreach i (reverse *EXTRACTION-SELECTED-INDICES*)
    (setq selected (if (= selected "") (itoa i) (strcat selected " " (itoa i))))
  )
  (set_tile "lst_layers" selected)
)

;; ---------- Чтение параметров ----------
(defun extraction-read-params ( / selected)
  (setq selected (extraction-selected-names))
  (setq *EXTRACTION-SELECTED-LAYERS* (if selected selected nil))
  (setq *EXTRACTION-REPORT-MODE* (if (= (get_tile "rb_summary") "1") "SUMMARY" "DETAIL"))
  (setq *EXTRACTION-EXPORT-EXCEL* (= (get_tile "chk_xls") "1"))
  (setq *EXTRACTION-EXPORT-TXT* (= (get_tile "chk_txt") "1"))
  (setq *EXTRACTION-CREATE-TABLE* (= (get_tile "chk_acad") "1"))
  (cond
    ((= (get_tile "rb_task_fasonka") "1")      (setq *EXTRACTION-TASK-ID* 'FASONKA))
    ((= (get_tile "rb_task_subsystem") "1")    (setq *EXTRACTION-TASK-ID* 'SUBSYSTEM))
    ((= (get_tile "rb_task_cladding") "1")     (setq *EXTRACTION-TASK-ID* 'CLADDING))
    ((= (get_tile "rb_task_vitrazh") "1")      (setq *EXTRACTION-TASK-ID* 'VITRAZH))
    ((= (get_tile "rb_task_zapolnenie") "1")   (setq *EXTRACTION-TASK-ID* 'ZAPOLNENIE))
    (t (setq *EXTRACTION-TASK-ID* 'FASONKA))
  )
  T
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
     (if (vl-catch-all-error-p r) (princ "\nМодуль Подсистема не загружен.")))
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
(defun extraction-save   () (extraction-read-params) (setq *EXTRACTION-ACTION* 'SAVE)   (done_dialog 1))
(defun extraction-saveas () (extraction-read-params) (setq *EXTRACTION-ACTION* 'SAVEAS) (done_dialog 1))
(defun extraction-close  () (setq *EXTRACTION-ACTION* 'CANCEL) (done_dialog 0))
(defun extraction-cutline () (setq *EXTRACTION-ACTION* 'CUTLINE) (done_dialog 1))
(defun extraction-cutsheet () (setq *EXTRACTION-ACTION* 'CUTSHEET) (done_dialog 1))

(defun extraction-filter-facades ()
  (setq *EXTRACTION-FILTER-FACADES* (= (get_tile "chk_filter_facades") "1"))
  (extraction-rebuild-layer-list)
)
(defun extraction-filter-vitrazh ()
  (setq *EXTRACTION-FILTER-VITRAZH* (= (get_tile "chk_filter_vitrazh") "1"))
  (extraction-rebuild-layer-list)
)
(defun extraction-filter-fonar ()
  (setq *EXTRACTION-FILTER-FONAR* (= (get_tile "chk_filter_fonar") "1"))
  (extraction-rebuild-layer-list)
)

(defun extraction-layer-selection ()
  (setq *EXTRACTION-SELECTED-LAYERS* (extraction-selected-names))
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
      (setq *EXTRACTION-ALL-LAYERS* (extraction-layer-names))
      (setq *EXTRACTION-VISIBLE-LAYERS* *EXTRACTION-ALL-LAYERS*)
      (setq *EXTRACTION-SELECTED-LAYERS* nil)
      (setq *EXTRACTION-FILTER-FACADES* nil)
      (setq *EXTRACTION-FILTER-VITRAZH* nil)
      (setq *EXTRACTION-FILTER-FONAR* nil)
      (setq *EXTRACTION-TASK-ID* 'FASONKA)
      (setq *EXTRACTION-REPORT-MODE* "DETAIL")
      (setq *EXTRACTION-EXPORT-EXCEL* T)
      (setq *EXTRACTION-EXPORT-TXT* nil)
      (setq *EXTRACTION-CREATE-TABLE* T)
      (setq *EXTRACTION-ACTION* 'CANCEL)

      (setq *EXTRACTION-DCL-ID* (load_dialog dcl-file))
      (if (< *EXTRACTION-DCL-ID* 0)
        (alert "Не удалось загрузить extraction.dcl.")
        (progn
          (if (new_dialog "extraction_dialog" *EXTRACTION-DCL-ID*)
            (progn
              (set_tile "rb_detail" "1")
              (set_tile "rb_summary" "0")
              (set_tile "chk_xls" "0")
              (set_tile "chk_txt" "0")
              (set_tile "chk_acad" "1")
              (set_tile "chk_filter_facades" "0")
              (set_tile "chk_filter_vitrazh" "0")
              (set_tile "chk_filter_fonar" "0")
              (set_tile "rb_task_fasonka" "1")
              (mode_tile "rb_task_subsystem" 1)
              (mode_tile "rb_task_cladding" 1)
              (mode_tile "rb_task_vitrazh" 1)
              (mode_tile "rb_task_zapolnenie" 1)
              (mode_tile "lst_blocks" 1)

              (extraction-rebuild-layer-list)

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
              (action_tile "rb_detail"  "(setq *EXTRACTION-REPORT-MODE* \"DETAIL\")")
              (action_tile "rb_summary" "(setq *EXTRACTION-REPORT-MODE* \"SUMMARY\")")

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