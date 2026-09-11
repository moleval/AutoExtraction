;;; ============================================================
;;; extraction.lsp — Диспетчер EXTRACTION / ЭКСТРАКЦИЯ
;;;
;;; Возможности:
;;;   - кнопки "Выбрать все" / "Снять выделение" для слоёв;
;;;   - фильтры Фасад / Витраж / Фонарь / Анонимные блоки;
;;;   - синхронизация чекбоксов Подсистемы;
;;;   - сохранение предварительного выбора;
;;;   - задачи: Фасонка, Подсистема, Облицовка, Витраж,
;;;     Заполнение;
;;;   - модуль "Переименователь" (blockrename).
;;;
;;; ДОБАВЛЕНО: автоматическое выделение слоёв по умолчанию
;;;   для каждой задачи. Если пользователь не менял выбор,
;;;   при следующем запуске задачи эти слои будут выделены
;;;   автоматически. Если пользователь изменил выбор, новые
;;;   слои сохраняются и используются при следующем запуске.
;;;
;;; ИСПРАВЛЕНО: сохранение слоёв в правильную переменную задачи
;;;   (ранее все задачи, кроме Подсистемы, сохраняли слои в
;;;   переменную Фасонки).
;;;
;;; Маски с * поддерживаются через wcmatch.
;;;
;;; Переименование блоков — только по кнопке "Переименовать".
;;; Enter в поле "Новое имя" НЕ вызывает переименование.
;;; ============================================================

(vl-load-com)

;; ============================================================
;; ПОСЛЕДНИЕ НАСТРОЙКИ
;; ============================================================

(if (not (boundp '*EXTRACTION-LAST-TASK*))
  (setq *EXTRACTION-LAST-TASK* 'FASONKA)
)

(if (not (boundp '*EXTRACTION-LAST-REPORT-MODE*))
  (setq *EXTRACTION-LAST-REPORT-MODE* "DETAIL")
)

(if (not (boundp '*EXTRACTION-LAST-EXPORT-EXCEL*))
  (setq *EXTRACTION-LAST-EXPORT-EXCEL* nil)
)

(if (not (boundp '*EXTRACTION-LAST-EXPORT-TXT*))
  (setq *EXTRACTION-LAST-EXPORT-TXT* nil)
)

(if (not (boundp '*EXTRACTION-LAST-CREATE-TABLE*))
  (setq *EXTRACTION-LAST-CREATE-TABLE* T)
)

;; Последние слои Фасонки
(if (not (boundp '*EXTRACTION-LAST-FASONKA-LAYERS*))
  (setq *EXTRACTION-LAST-FASONKA-LAYERS* nil)
)

;; Последние слои Подсистемы
(if (not (boundp '*EXTRACTION-LAST-SUBSYSTEM-LAYERS*))
  (setq
    *EXTRACTION-LAST-SUBSYSTEM-LAYERS*
    '("Подсистема"
      "Подсистема алюминиевая"
      "Подсистема оцинкованная")
  )
)

;; Состояние трёх чекбоксов Подсистемы
(if (not (boundp '*EXTRACTION-LAST-SUBSYSTEM-CHECKS*))
  (setq *EXTRACTION-LAST-SUBSYSTEM-CHECKS* '(T T T))
)

;; Последние фильтры
(if (not (boundp '*EXTRACTION-LAST-FILTER-FACADES*))
  (setq *EXTRACTION-LAST-FILTER-FACADES* nil)
)

(if (not (boundp '*EXTRACTION-LAST-FILTER-VITRAZH*))
  (setq *EXTRACTION-LAST-FILTER-VITRAZH* nil)
)

(if (not (boundp '*EXTRACTION-LAST-FILTER-FONAR*))
  (setq *EXTRACTION-LAST-FILTER-FONAR* nil)
)

;; Переменная для сохранения предварительного выбора
(if (not (boundp '*extraction-preselected-set*))
  (setq *extraction-preselected-set* nil)
)

;; Флаг: идёт синхронизация чекбоксов из обработчика чекбокса
(if (not (boundp '*extraction-syncing-checks*))
  (setq *extraction-syncing-checks* nil)
)

;; Флаг: блокировка сохранения слоёв при программном изменении
;; ДОБАВЛЕНО: защита от ложного срабатывания при переключении задач
(if (not (boundp '*extraction-syncing-layers*))
  (setq *extraction-syncing-layers* nil)
)

;; ============================================================
;; Слои по умолчанию для каждой задачи
;; ДОБАВЛЕНО: автоматическое выделение слоёв при выборе задачи
;;
;; Если пользователь не менял выбор, при следующем запуске
;; задачи эти слои будут выделены автоматически.
;; Если пользователь изменил выбор, новые слои сохраняются
;; и используются при следующем запуске.
;;
;; Маски с * поддерживаются через wcmatch.
;; ============================================================

;; Фасонка — слои по умолчанию
(if (not (boundp '*EXTRACTION-LAST-FASONKA-LAYERS*))
  (setq *EXTRACTION-LAST-FASONKA-LAYERS* '("Фасонка*" "Железо*"))
)

;; Заполнение — слои по умолчанию
(if (not (boundp '*EXTRACTION-LAST-ZAPOLNENIE-LAYERS*))
  (setq *EXTRACTION-LAST-ZAPOLNENIE-LAYERS* '("Заполнение" "Стекло" "Обозначение ст-т"))
)

;; Облицовка — слои по умолчанию
(if (not (boundp '*EXTRACTION-LAST-CLADDING-LAYERS*))
  (setq *EXTRACTION-LAST-CLADDING-LAYERS* '("*Облицовка*" "*Кассет*" "*Керамогранит*"))
)

;; Витраж — слои по умолчанию
(if (not (boundp '*EXTRACTION-LAST-VITRAZH-LAYERS*))
  (setq *EXTRACTION-LAST-VITRAZH-LAYERS* '("Витражи" "Стойк*" "Ригел*"))
)

;; Раскрой хлыстов — слои по умолчанию
(if (not (boundp '*EXTRACTION-LAST-CUTLINE-LAYERS*))
  (setq *EXTRACTION-LAST-CUTLINE-LAYERS* nil)  ;; пусто — пользователь выбирает
)


;; ============================================================
;; РАБОЧЕЕ СОСТОЯНИЕ
;; ============================================================

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


;; ============================================================
;; УНИКАЛЬНЫЕ СТРОКИ
;; ============================================================

(defun extraction-unique-ci (lst / out x key)
  (setq out '())
  (if (listp lst)
    (foreach x lst
      (if (= (type x) 'STR)
        (progn
          (setq key (strcase x))
          (if (not (vl-some '(lambda (y) (= (strcase y) key)) out))
            (setq out (cons x out))
          )
        )
      )
    )
  )
  (reverse out)
)


;; ============================================================
;; ПОЛУЧЕНИЕ ВСЕХ СЛОЁВ ЧЕРТЕЖА
;; ============================================================

(defun extraction-layer-names ( / acad doc layers out name)
  (setq out '())
  (setq acad (vl-catch-all-apply 'vlax-get-acad-object '()))

  (if (and (not (vl-catch-all-error-p acad)) acad)
    (progn
      (setq doc
        (vl-catch-all-apply 'vla-get-ActiveDocument (list acad)))

      (if (and (not (vl-catch-all-error-p doc)) doc)
        (progn
          (setq layers
            (vl-catch-all-apply 'vla-get-Layers (list doc)))

          (if (and (not (vl-catch-all-error-p layers)) layers)
            (vlax-for lay layers
              (setq name
                (vl-catch-all-apply 'vla-get-Name (list lay)))

              (if (and
                    (not (vl-catch-all-error-p name))
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

  (setq out
    (vl-remove-if-not '(lambda (x) (= (type x) 'STR)) out))

  (setq out
    (vl-sort out '(lambda (a b) (< (strcase a) (strcase b)))))

  (extraction-unique-ci out)
)


;; ============================================================
;; ЗАГРУЗКА МОДУЛЕЙ
;; ============================================================

(defun extraction-project-root ( / dsp)
  (setq dsp (findfile "extraction.lsp"))
  (if dsp
    (vl-filename-directory (vl-filename-directory dsp))
    nil
  )
)


(defun extraction-modules-dir ( / dsp)
  (setq dsp (findfile "extraction.lsp"))
  (if dsp
    (vl-filename-directory dsp)
    nil
  )
)


(defun extraction-load-all ( / root common f path)
  (setq root (extraction-project-root))

  (if root
    (progn

      ;; --------------------------------------------------------
      ;; COMMON
      ;; --------------------------------------------------------

      (setq common (strcat root "\\common\\"))

      (foreach f
        '("task-utils.lsp"
          "layer-utils.lsp"
          "select-utils.lsp"
          "excel-utils.lsp"
          "table-utils.lsp"
          "txt-utils.lsp")

        (setq path (strcat common f))

        (if (findfile path)
          (load path)
          (princ (strcat "\n[EXTRACTION] Не найден: " path))
        )
      )

      ;; --------------------------------------------------------
      ;; EXTRACTION / FASONKA
      ;; --------------------------------------------------------

      (setq path (strcat root "\\Extraction\\fasonka.lsp"))
      (if (findfile path)
        (load path)
        (princ (strcat "\n[EXTRACTION] Не найден: " path)))

      ;; --------------------------------------------------------
      ;; EXTRACTION / SUBSYSTEM
      ;; --------------------------------------------------------

      (setq path (strcat root "\\Extraction\\subsystem.lsp"))
      (if (findfile path)
        (load path)
        (princ (strcat "\n[EXTRACTION] Не найден: " path)))

      ;; --------------------------------------------------------
      ;; EXTRACTION / CUTLINE
      ;; --------------------------------------------------------

      (setq path (strcat root "\\Extraction\\cutline.lsp"))
      (if (findfile path)
        (load path)
        (princ (strcat "\n[EXTRACTION] Не найден: " path)))

      ;; --------------------------------------------------------
      ;; EXTRACTION / CUTSHEET
      ;; --------------------------------------------------------

      (setq path (strcat root "\\Extraction\\cutsheet.lsp"))
      (if (findfile path)
        (load path)
        (princ (strcat "\n[EXTRACTION] Не найден: " path)))

      ;; --------------------------------------------------------
      ;; EXTRACTION / ZAPOLNENIE
      ;; --------------------------------------------------------

      (setq path (strcat root "\\Extraction\\zapolnenie.lsp"))
      (if (findfile path)
        (load path)
        (princ (strcat "\n[EXTRACTION] Не найден: " path)))

      ;; --------------------------------------------------------
      ;; EXTRACTION / BLOCKRENAME
      ;; --------------------------------------------------------

      (setq path (strcat root "\\Extraction\\blockrename.lsp"))
      (if (findfile path)
        (load path)
        (princ (strcat "\n[EXTRACTION] Не найден: " path)))
    )

    (princ "\n[EXTRACTION] Корень проекта не найден.")
  )

  T
)


;; ============================================================
;; РАЗБОР ИНДЕКСОВ DCL
;; ============================================================

(defun extraction-parse-indices (s / x)
  (if (and (= (type s) 'STR) (/= s ""))
    (progn
      (setq x (read (strcat "(" s ")")))
      (if (= (type x) 'LIST) x nil)
    )
    nil
  )
)


;; ============================================================
;; ПОЛУЧЕНИЕ ВЫБРАННЫХ СЛОЁВ
;; ============================================================

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


;; ============================================================
;; ФИЛЬТРАЦИЯ СЛОЁВ ПО КЛЮЧЕВЫМ СЛОВАМ
;; ============================================================

(defun extraction-filter-layers-by-keywords (keywords / filters result f fname)
  (setq filters
    (vl-catch-all-apply 'tu-group-filter-names-and-layers '()))

  (if (vl-catch-all-error-p filters)
    (setq filters nil))

  (setq result '())

  (if (listp filters)
    (foreach f filters
      (setq fname (car f))

      (if (and
            (= (type fname) 'STR)
            (vl-some
              '(lambda (k)
                 (and
                   (= (type k) 'STR)
                   (vl-string-search (strcase k) (strcase fname))))
              keywords))
        (setq result (append result (cadr f)))
      )
    )
  )

  (extraction-unique-ci result)
)


;; ============================================================
;; ПЕРЕСТРОЕНИЕ СПИСКА СЛОЁВ
;; ============================================================

(defun extraction-rebuild-layer-list ( / vis keywords)
  (setq *EXTRACTION-SELECTED-INDICES* '())
  (setq keywords '())

  (if *EXTRACTION-FILTER-FACADES*
    (setq keywords (cons "фасад" keywords)))

  (if *EXTRACTION-FILTER-VITRAZH*
    (setq keywords (cons "витраж" keywords)))

  (if *EXTRACTION-FILTER-FONAR*
    (setq keywords (cons "фонар" keywords)))

  (if keywords
    (progn
      (setq vis (extraction-filter-layers-by-keywords keywords))
      (if (or (null vis) (not (listp vis)))
        (setq vis *EXTRACTION-ALL-LAYERS*))
      (setq *EXTRACTION-VISIBLE-LAYERS* vis)
    )
    (setq *EXTRACTION-VISIBLE-LAYERS* *EXTRACTION-ALL-LAYERS*)
  )

  (setq *EXTRACTION-VISIBLE-LAYERS*
    (vl-sort
      (vl-remove-if-not
        '(lambda (x) (= (type x) 'STR))
        (if (listp *EXTRACTION-VISIBLE-LAYERS*)
          *EXTRACTION-VISIBLE-LAYERS*
          '()))
      '(lambda (a b) (< (strcase a) (strcase b)))))

  (start_list "lst_layers")
  (mapcar 'add_list *EXTRACTION-VISIBLE-LAYERS*)
  (end_list)
  (set_tile "lst_layers" "")
  (extraction-update-select-buttons)
)


;; ============================================================
;; СПЕЦИАЛЬНЫЙ СПИСОК ПОДСИСТЕМЫ
;; ============================================================

(defun extraction-subsystem-layer-list ()
  (setq *EXTRACTION-VISIBLE-LAYERS*
    '("Подсистема"
      "Подсистема алюминиевая"
      "Подсистема оцинкованная"))

  (start_list "lst_layers")
  (mapcar 'add_list *EXTRACTION-VISIBLE-LAYERS*)
  (end_list)
  (set_tile "lst_layers" "")
  (extraction-update-select-buttons)
)


;; ============================================================
;; ОЧИСТКА ВЫБОРА
;; ============================================================

(defun extraction-clear-layer-selection ()
  (set_tile "lst_layers" "")
  (extraction-layer-selection-changed)
)


;; ============================================================
;; УСТАНОВКА ВЫДЕЛЕНИЯ СЛОЁВ
;; ОБНОВЛЕНО: поддержка масок через wcmatch
;; ОБНОВЛЕНО: блокировка сохранения при программном изменении
;; ============================================================
(defun extraction-select-layers-in-list
       (layers-to-select / i item selected str after-set)

  (setq selected '())
  (setq i 0)

  (foreach item *EXTRACTION-VISIBLE-LAYERS*
    ;; Проверяем: слой совпадает с одним из шаблонов?
    ;; Используем wcmatch для поддержки масок с *
    (if (vl-some
          '(lambda (x)
             (or
               ;; Точное совпадение (регистронезависимо)
               (= (strcase x) (strcase item))
               ;; Совпадение по маске (wcmatch)
               (wcmatch (strcase item) (strcase x))
             )
           )
          layers-to-select)
      (setq selected (cons i selected))
    )
    (setq i (1+ i))
  )

  (setq selected (reverse selected))
  (setq str "")

  (foreach i selected
    (setq str
      (if (= str "")
        (itoa i)
        (strcat str " " (itoa i))))
  )

  (set_tile "lst_layers" str)
  (setq after-set (get_tile "lst_layers"))

  (if (/= str after-set)
    (progn
      (mode_tile "lst_layers" 1)
      (mode_tile "lst_layers" 0)
      (set_tile "lst_layers" str)
      (setq after-set (get_tile "lst_layers"))
    )
  )

  (if (/= str after-set)
    (progn
      (start_list "lst_layers")
      (mapcar 'add_list *EXTRACTION-VISIBLE-LAYERS*)
      (end_list)
      (set_tile "lst_layers" str)
      (setq after-set (get_tile "lst_layers"))
    )
  )

  ;; Блокируем сохранение при программном изменении
  ;; Это защищает от ложного срабатывания при переключении задач
  (setq *extraction-syncing-layers* T)
  (extraction-layer-selection-changed)
  (setq *extraction-syncing-layers* nil)

  (extraction-update-select-buttons)
)


;; ============================================================
;; ПРОВЕРКА, ВСЕ ЛИ ВИДИМЫЕ СЛОИ ВЫБРАНЫ
;; ============================================================

(defun extraction-all-layers-selected-p ( / selected total)
  (setq selected (extraction-selected-names))
  (setq total (length *EXTRACTION-VISIBLE-LAYERS*))
  (and selected (= (length selected) total))
)


;; ============================================================
;; ОБНОВЛЕНИЕ АКТИВНОСТИ КНОПОК ВЫБОРА
;; ============================================================

(defun extraction-update-select-buttons ()
  (if (extraction-all-layers-selected-p)
    (progn
      (mode_tile "btn_select_all" 1)
      (mode_tile "btn_clear_all" 0)
    )
    (progn
      (mode_tile "btn_select_all" 0)
      (mode_tile "btn_clear_all" 1)
    )
  )
)


;; ============================================================
;; ВЫБРАТЬ ВСЕ ВИДИМЫЕ СЛОИ
;; ============================================================

(defun extraction-select-all-layers ( / str i)
  (setq str "")
  (setq i 0)

  (foreach item *EXTRACTION-VISIBLE-LAYERS*
    (setq str
      (if (= str "")
        (itoa i)
        (strcat str " " (itoa i))))
    (setq i (1+ i))
  )

  (extraction-select-layers-in-list *EXTRACTION-VISIBLE-LAYERS*)
)


;; ============================================================
;; СНЯТЬ ВЫДЕЛЕНИЕ СО ВСЕХ СЛОЁВ
;; ============================================================

(defun extraction-clear-all-layers ()
  (set_tile "lst_layers" "")
  (extraction-layer-selection-changed)
  (extraction-update-select-buttons)
)


;; ============================================================
;; СИНХРОНИЗАЦИЯ ЧЕКБОКСОВ ПОДСИСТЕМЫ
;; ============================================================

(defun extraction-sync-checks-from-layers ( / selected)
  (if *extraction-syncing-checks*
    nil
    (progn
      (setq selected (extraction-selected-names))

      (set_tile "chk_subsystem_1"
        (if (member "Подсистема" selected) "1" "0"))

      (set_tile "chk_subsystem_2"
        (if (member "Подсистема алюминиевая" selected) "1" "0"))

      (set_tile "chk_subsystem_3"
        (if (member "Подсистема оцинкованная" selected) "1" "0"))
    )
  )
)


;; ============================================================
;; ИЗМЕНЕНИЕ ЧЕКБОКСА ПОДСИСТЕМЫ
;; ============================================================

(defun extraction-subsystem-check-changed
       (key / val layers-to-select current-selection)

  (setq *extraction-syncing-checks* T)

  (setq val
    (= (get_tile
         (strcat "chk_subsystem_" (itoa key))) "1"))

  (setq *EXTRACTION-LAST-SUBSYSTEM-CHECKS*
    (list
      (if (= key 1) val (car *EXTRACTION-LAST-SUBSYSTEM-CHECKS*))
      (if (= key 2) val (cadr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*))
      (if (= key 3) val (caddr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*))))

  (setq layers-to-select '())

  (if (car *EXTRACTION-LAST-SUBSYSTEM-CHECKS*)
    (setq layers-to-select (cons "Подсистема" layers-to-select)))

  (if (cadr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*)
    (setq layers-to-select (cons "Подсистема алюминиевая" layers-to-select)))

  (if (caddr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*)
    (setq layers-to-select (cons "Подсистема оцинкованная" layers-to-select)))

  (setq current-selection (extraction-selected-names))

  (setq current-selection
    (vl-remove-if
      '(lambda (x)
         (or (= (strcase x) "ПОДСИСТЕМА")
             (= (strcase x) "ПОДСИСТЕМА АЛЮМИНИЕВАЯ")
             (= (strcase x) "ПОДСИСТЕМА ОЦИНКОВАННАЯ")))
      current-selection))

  (setq current-selection
    (append current-selection layers-to-select))

  (extraction-select-layers-in-list current-selection)

  (setq *extraction-syncing-checks* nil)
)


;; ============================================================
;; ПЕРЕКЛЮЧЕНИЕ ЗАДАЧИ И ВОССТАНОВЛЕНИЕ СЛОЁВ ПО УМОЛЧАНИЮ
;; ОБНОВЛЕНО: поддержка всех задач (не только Подсистема)
;; ============================================================
(defun extraction-toggle-subsystem-layers ( / layers-to-select)
  (cond
    ;; ============================================================
    ;; ПОДСИСТЕМА
    ;; ============================================================
    ((= (get_tile "rb_task_subsystem") "1")
     (mode_tile "box_subsystem_layers" 0)
     (mode_tile "chk_subsystem_1" 0)
     (mode_tile "chk_subsystem_2" 0)
     (mode_tile "chk_subsystem_3" 0)

     (extraction-rebuild-layer-list)

     ;; Восстанавливаем слои Подсистемы
     (setq layers-to-select *EXTRACTION-LAST-SUBSYSTEM-LAYERS*)

     (if (null layers-to-select)
       (progn
         (setq layers-to-select '())
         (if (car *EXTRACTION-LAST-SUBSYSTEM-CHECKS*)
           (setq layers-to-select (cons "Подсистема" layers-to-select)))
         (if (cadr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*)
           (setq layers-to-select (cons "Подсистема алюминиевая" layers-to-select)))
         (if (caddr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*)
           (setq layers-to-select (cons "Подсистема оцинкованная" layers-to-select)))
       )
     )

     (extraction-select-layers-in-list layers-to-select)
     (extraction-sync-checks-from-layers)
    )

    ;; ============================================================
    ;; ФАСОНКА
    ;; ============================================================
    ((= (get_tile "rb_task_fasonka") "1")
     (mode_tile "box_subsystem_layers" 1)
     (mode_tile "chk_subsystem_1" 1)
     (mode_tile "chk_subsystem_2" 1)
     (mode_tile "chk_subsystem_3" 1)

     (extraction-rebuild-layer-list)

     ;; Восстанавливаем слои Фасонки
     (if *EXTRACTION-LAST-FASONKA-LAYERS*
       (extraction-select-layers-in-list *EXTRACTION-LAST-FASONKA-LAYERS*)
     )
    )

    ;; ============================================================
    ;; ЗАПОЛНЕНИЕ
    ;; ============================================================
    ((= (get_tile "rb_task_zapolnenie") "1")
     (mode_tile "box_subsystem_layers" 1)
     (mode_tile "chk_subsystem_1" 1)
     (mode_tile "chk_subsystem_2" 1)
     (mode_tile "chk_subsystem_3" 1)

     (extraction-rebuild-layer-list)

     ;; Восстанавливаем слои Заполнения
     (if *EXTRACTION-LAST-ZAPOLNENIE-LAYERS*
       (extraction-select-layers-in-list *EXTRACTION-LAST-ZAPOLNENIE-LAYERS*)
     )
    )

    ;; ============================================================
    ;; ОБЛИЦОВКА
    ;; ============================================================
    ((= (get_tile "rb_task_cladding") "1")
     (mode_tile "box_subsystem_layers" 1)
     (mode_tile "chk_subsystem_1" 1)
     (mode_tile "chk_subsystem_2" 1)
     (mode_tile "chk_subsystem_3" 1)

     (extraction-rebuild-layer-list)

     ;; Восстанавливаем слои Облицовки
     (if *EXTRACTION-LAST-CLADDING-LAYERS*
       (extraction-select-layers-in-list *EXTRACTION-LAST-CLADDING-LAYERS*)
     )
    )

    ;; ============================================================
    ;; ВИТРАЖ
    ;; ============================================================
    ((= (get_tile "rb_task_vitrazh") "1")
     (mode_tile "box_subsystem_layers" 1)
     (mode_tile "chk_subsystem_1" 1)
     (mode_tile "chk_subsystem_2" 1)
     (mode_tile "chk_subsystem_3" 1)

     (extraction-rebuild-layer-list)

     ;; Восстанавливаем слои Витража
     (if *EXTRACTION-LAST-VITRAZH-LAYERS*
       (extraction-select-layers-in-list *EXTRACTION-LAST-VITRAZH-LAYERS*)
     )
    )

    ;; ============================================================
    ;; ДРУГИЕ ЗАДАЧИ (по умолчанию — без автоматического выделения)
    ;; ============================================================
    (T
     (mode_tile "box_subsystem_layers" 1)
     (mode_tile "chk_subsystem_1" 1)
     (mode_tile "chk_subsystem_2" 1)
     (mode_tile "chk_subsystem_3" 1)

     (extraction-rebuild-layer-list)
    )
  )
)


;; ============================================================
;; ИЗМЕНЕНИЕ ВЫБОРА В СПИСКЕ
;; ОБНОВЛЕНО: сохранение слоёв в переменную, соответствующую
;; текущей задаче (а не всегда в Фасонку)
;; ============================================================
(defun extraction-layer-selection-changed ( / selected)
  (setq selected (extraction-selected-names))
  (setq *EXTRACTION-SELECTED-LAYERS* selected)

  ;; Сохраняем только если это НЕ программное изменение
  ;; Флаг устанавливается в extraction-select-layers-in-list
  (if (not *extraction-syncing-layers*)
    (cond
      ((eq *EXTRACTION-TASK-ID* 'SUBSYSTEM)
       (setq *EXTRACTION-LAST-SUBSYSTEM-LAYERS* selected)
       (extraction-sync-checks-from-layers))

      ((eq *EXTRACTION-TASK-ID* 'FASONKA)
       (setq *EXTRACTION-LAST-FASONKA-LAYERS* selected))

      ((eq *EXTRACTION-TASK-ID* 'ZAPOLNENIE)
       (setq *EXTRACTION-LAST-ZAPOLNENIE-LAYERS* selected))

      ((eq *EXTRACTION-TASK-ID* 'CLADDING)
       (setq *EXTRACTION-LAST-CLADDING-LAYERS* selected))

      ((eq *EXTRACTION-TASK-ID* 'VITRAZH)
       (setq *EXTRACTION-LAST-VITRAZH-LAYERS* selected))

      ;; Для других задач (Раскрой и т.д.) — не сохраняем
    )
  )

  (extraction-update-select-buttons)
)


(defun extraction-layer-selection ()
  (extraction-layer-selection-changed)
)


;; ============================================================
;; ЧТЕНИЕ ПАРАМЕТРОВ
;; ============================================================

(defun extraction-read-params ( / selected)

  (cond
    ((= (get_tile "rb_task_fasonka") "1")
     (setq *EXTRACTION-TASK-ID* 'FASONKA))

    ((= (get_tile "rb_task_subsystem") "1")
     (setq *EXTRACTION-TASK-ID* 'SUBSYSTEM))

    ((= (get_tile "rb_task_cladding") "1")
     (setq *EXTRACTION-TASK-ID* 'CLADDING))

    ((= (get_tile "rb_task_vitrazh") "1")
     (setq *EXTRACTION-TASK-ID* 'VITRAZH))

    ((= (get_tile "rb_task_zapolnenie") "1")
     (setq *EXTRACTION-TASK-ID* 'ZAPOLNENIE))

    (T
     (setq *EXTRACTION-TASK-ID* 'FASONKA))
  )

  (setq *EXTRACTION-LAST-TASK* *EXTRACTION-TASK-ID*)
  (setq selected (extraction-selected-names))

  (if (and (null selected)
           (or *EXTRACTION-FILTER-FACADES*
               *EXTRACTION-FILTER-VITRAZH*
               *EXTRACTION-FILTER-FONAR*))
    (setq selected *EXTRACTION-VISIBLE-LAYERS*)
  )

  (setq *EXTRACTION-SELECTED-LAYERS* selected)

  (if (eq *EXTRACTION-TASK-ID* 'SUBSYSTEM)
    (progn
      (setq *EXTRACTION-LAST-SUBSYSTEM-LAYERS* selected)

      (if (null selected)
        (progn
          (alert "Не выбрано ни одного слоя подсистемы.")
          nil
        )
        T
      )
    )
    (progn
      (setq *EXTRACTION-LAST-FASONKA-LAYERS* selected)
      T
    )
  )

  (setq *EXTRACTION-REPORT-MODE*
    (if (= (get_tile "rb_summary") "1") "SUMMARY" "DETAIL"))

  (setq *EXTRACTION-LAST-REPORT-MODE* *EXTRACTION-REPORT-MODE*)

  (setq *EXTRACTION-EXPORT-EXCEL*
    (= (get_tile "chk_xls") "1"))

  (setq *EXTRACTION-LAST-EXPORT-EXCEL* *EXTRACTION-EXPORT-EXCEL*)

  (setq *EXTRACTION-EXPORT-TXT*
    (= (get_tile "chk_txt") "1"))

  (setq *EXTRACTION-LAST-EXPORT-TXT* *EXTRACTION-EXPORT-TXT*)

  (setq *EXTRACTION-CREATE-TABLE*
    (= (get_tile "chk_acad") "1"))

  (setq *EXTRACTION-LAST-CREATE-TABLE* *EXTRACTION-CREATE-TABLE*)

  T
)


;; ============================================================
;; ЗАПУСК ЗАДАЧИ
;; ОБНОВЛЕНО: сохранение выбранных слоёв после выполнения
;; ============================================================
(defun run-task
       (task-id layers report-mode export-excel export-txt
                create-table save-base / r)

  ;; Сохраняем выбранные слои для следующего запуска
  (cond
    ((eq task-id 'FASONKA)
     (setq *EXTRACTION-LAST-FASONKA-LAYERS* layers))
    ((eq task-id 'SUBSYSTEM)
     (setq *EXTRACTION-LAST-SUBSYSTEM-LAYERS* layers))
    ((eq task-id 'ZAPOLNENIE)
     (setq *EXTRACTION-LAST-ZAPOLNENIE-LAYERS* layers))
    ((eq task-id 'CLADDING)
     (setq *EXTRACTION-LAST-CLADDING-LAYERS* layers))
    ((eq task-id 'VITRAZH)
     (setq *EXTRACTION-LAST-VITRAZH-LAYERS* layers))
  )

  ;; Запускаем задачу
  (cond
    ((eq task-id 'FASONKA)
     (setq r
       (vl-catch-all-apply 'fasonka-main
         (list layers report-mode export-excel export-txt
               create-table save-base)))
     (if (vl-catch-all-error-p r)
       (princ "\nМодуль Фасонка не загружен или ошибка выполнения."))
    )

    ((eq task-id 'SUBSYSTEM)
     (setq r
       (vl-catch-all-apply 'subsystem-main
         (list layers report-mode export-excel export-txt
               create-table save-base)))
     (if (vl-catch-all-error-p r)
       (princ "\nМодуль Подсистема не загружен или ошибка выполнения."))
    )

    ((eq task-id 'CLADDING)
     (setq r
       (vl-catch-all-apply 'cladding-main
         (list layers report-mode export-excel export-txt
               create-table save-base)))
     (if (vl-catch-all-error-p r)
       (princ "\nМодуль Облицовка не загружен или ошибка выполнения."))
    )

    ((eq task-id 'VITRAZH)
     (setq r
       (vl-catch-all-apply 'vitrazh-main
         (list layers report-mode export-excel export-txt
               create-table save-base)))
     (if (vl-catch-all-error-p r)
       (princ "\nМодуль Витраж не загружен или ошибка выполнения."))
    )

    ((eq task-id 'ZAPOLNENIE)
     (setq r
       (vl-catch-all-apply 'zapolnenie-main
         (list layers report-mode export-excel export-txt
               create-table save-base)))
     (if (vl-catch-all-error-p r)
       (princ "\nМодуль Заполнение не загружен или ошибка выполнения."))
    )

    (T
     (princ "\nНеизвестная задача."))
  )
)


;; ============================================================
;; ПОМОЩЬ
;; ============================================================

(defun extraction-help ()
  (alert
    (strcat
      "Окно запуска задач.\n\n"
      "Выбор одной задачи, слоёв, режима отчёта "
      "и форматов вывода.\n"
      "Кнопки Раскрой хлыста / Раскрой листа "
      "запускают модули раскроя.\n\n"
      "Сохранить — имя файла по умолчанию.\n"
      "Сохранить как... — выбор пути и имени.\n"
      "Для Подсистемы доступны все слои; "
      "три чекбокса — быстрый выбор типовых слоёв.\n"
      "Кнопка \"Выбрать все\" выделяет все слои, "
      "отображаемые с учётом фильтров.\n"
      "Слои по умолчанию выделяются автоматически "
      "при выборе задачи.\n"
      "Изменённый выбор сохраняется для каждого "
      "задачи отдельно.\n\n"
      "Блоки: фильтр \"Анонимные блоки\" показывает "
      "только PASTEBLOCK-блоки (A$C...). "
      "Переименование — по кнопке."
    )
  )
)


;; ============================================================
;; КНОПКИ
;; ============================================================

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


(defun extraction-close ()
  (setq *EXTRACTION-ACTION* 'CANCEL)
  (done_dialog 0)
)


(defun extraction-cutline ()
  (setq *CUTLINE-CREATE-TABLE*
    (= (get_tile "chk_acad") "1"))

  (setq *CUTLINE-CREATE-XLS*
    (= (get_tile "chk_xls") "1"))

  (setq *EXTRACTION-ACTION* 'CUTLINE)
  (done_dialog 1)
)


(defun extraction-cutsheet ()
  (setq *CUTSHEET-CREATE-TABLE*
    (= (get_tile "chk_acad") "1"))

  (setq *CUTSHEET-CREATE-XLS*
    (= (get_tile "chk_xls") "1"))

  (setq *EXTRACTION-ACTION* 'CUTSHEET)
  (done_dialog 1)
)


;; ============================================================
;; ФИЛЬТРЫ
;; ============================================================

(defun extraction-filter-facades ()
  (setq *EXTRACTION-FILTER-FACADES*
    (= (get_tile "chk_filter_facades") "1"))

  (setq *EXTRACTION-LAST-FILTER-FACADES*
    *EXTRACTION-FILTER-FACADES*)

  (extraction-rebuild-layer-list)

  (if (eq *EXTRACTION-TASK-ID* 'SUBSYSTEM)
    (if *EXTRACTION-LAST-SUBSYSTEM-LAYERS*
      (extraction-select-layers-in-list
        *EXTRACTION-LAST-SUBSYSTEM-LAYERS*))
    (if *EXTRACTION-LAST-FASONKA-LAYERS*
      (extraction-select-layers-in-list
        *EXTRACTION-LAST-FASONKA-LAYERS*))
  )
)


(defun extraction-filter-vitrazh ()
  (setq *EXTRACTION-FILTER-VITRAZH*
    (= (get_tile "chk_filter_vitrazh") "1"))

  (setq *EXTRACTION-LAST-FILTER-VITRAZH*
    *EXTRACTION-FILTER-VITRAZH*)

  (extraction-rebuild-layer-list)

  (if (eq *EXTRACTION-TASK-ID* 'SUBSYSTEM)
    (if *EXTRACTION-LAST-SUBSYSTEM-LAYERS*
      (extraction-select-layers-in-list
        *EXTRACTION-LAST-SUBSYSTEM-LAYERS*))
    (if *EXTRACTION-LAST-FASONKA-LAYERS*
      (extraction-select-layers-in-list
        *EXTRACTION-LAST-FASONKA-LAYERS*))
  )
)


(defun extraction-filter-fonar ()
  (setq *EXTRACTION-FILTER-FONAR*
    (= (get_tile "chk_filter_fonar") "1"))

  (setq *EXTRACTION-LAST-FILTER-FONAR*
    *EXTRACTION-FILTER-FONAR*)

  (extraction-rebuild-layer-list)

  (if (eq *EXTRACTION-TASK-ID* 'SUBSYSTEM)
    (if *EXTRACTION-LAST-SUBSYSTEM-LAYERS*
      (extraction-select-layers-in-list
        *EXTRACTION-LAST-SUBSYSTEM-LAYERS*))
    (if *EXTRACTION-LAST-FASONKA-LAYERS*
      (extraction-select-layers-in-list
        *EXTRACTION-LAST-FASONKA-LAYERS*))
  )
)


;; ============================================================
;; ОСНОВНАЯ КОМАНДА
;; ============================================================

(defun c:extraction
       ( / dcl-file save-base modules-dir r)

  (vl-load-com)

  ;; ----------------------------------------------------------
  ;; Загрузка всех модулей
  ;; ----------------------------------------------------------

  (extraction-load-all)

  ;; ----------------------------------------------------------
  ;; Сохраняем предварительный выбор
  ;; ----------------------------------------------------------

  (setq *extraction-preselected-set* (ssget "_I"))

  ;; ----------------------------------------------------------
  ;; Поиск DCL
  ;; ----------------------------------------------------------

  (setq dcl-file nil)

  (setq modules-dir (extraction-modules-dir))

  (if modules-dir
    (setq dcl-file
      (findfile (strcat modules-dir "\\extraction.dcl"))))

  (if (null dcl-file)
    (setq dcl-file (findfile "extraction.dcl")))

  ;; ----------------------------------------------------------
  ;; Проверка DCL
  ;; ----------------------------------------------------------

  (if (null dcl-file)
    (progn
      (alert "Не найден файл extraction.dcl.")
      (princ)
    )
    (progn

      ;; ------------------------------------------------------
      ;; Получение слоёв
      ;; ------------------------------------------------------

      (setq *EXTRACTION-ALL-LAYERS* (extraction-layer-names))
      (setq *EXTRACTION-SELECTED-LAYERS* nil)

      ;; ------------------------------------------------------
      ;; Последние фильтры
      ;; ------------------------------------------------------

      (setq *EXTRACTION-FILTER-FACADES* *EXTRACTION-LAST-FILTER-FACADES*)
      (setq *EXTRACTION-FILTER-VITRAZH* *EXTRACTION-LAST-FILTER-VITRAZH*)
      (setq *EXTRACTION-FILTER-FONAR*   *EXTRACTION-LAST-FILTER-FONAR*)

      ;; ------------------------------------------------------
      ;; Последние параметры
      ;; ------------------------------------------------------

      (setq *EXTRACTION-TASK-ID*      *EXTRACTION-LAST-TASK*)
      (setq *EXTRACTION-REPORT-MODE*  *EXTRACTION-LAST-REPORT-MODE*)
      (setq *EXTRACTION-EXPORT-EXCEL* *EXTRACTION-LAST-EXPORT-EXCEL*)
      (setq *EXTRACTION-EXPORT-TXT*   *EXTRACTION-LAST-EXPORT-TXT*)
      (setq *EXTRACTION-CREATE-TABLE* *EXTRACTION-LAST-CREATE-TABLE*)
      (setq *EXTRACTION-ACTION* 'CANCEL)

      ;; ------------------------------------------------------
      ;; Загрузка DCL
      ;; ------------------------------------------------------

      (setq *EXTRACTION-DCL-ID* (load_dialog dcl-file))

      (if (< *EXTRACTION-DCL-ID* 0)
        (alert "Не удалось загрузить extraction.dcl.")
        (progn

          ;; --------------------------------------------------
          ;; Создание диалога
          ;; --------------------------------------------------

          (if (new_dialog "extraction_dialog" *EXTRACTION-DCL-ID*)
            (progn

              ;; ==============================================
              ;; Установка элементов диалога
              ;; ==============================================

              (set_tile "rb_detail"
                (if (= *EXTRACTION-LAST-REPORT-MODE* "DETAIL") "1" "0"))
              (set_tile "rb_summary"
                (if (= *EXTRACTION-LAST-REPORT-MODE* "SUMMARY") "1" "0"))

              (set_tile "chk_xls"
                (if *EXTRACTION-LAST-EXPORT-EXCEL* "1" "0"))
              (set_tile "chk_txt"
                (if *EXTRACTION-LAST-EXPORT-TXT* "1" "0"))
              (set_tile "chk_acad"
                (if *EXTRACTION-LAST-CREATE-TABLE* "1" "0"))

              ;; ------------------------------------------------
              ;; Фильтры слоёв
              ;; ------------------------------------------------

              (set_tile "chk_filter_facades"
                (if *EXTRACTION-LAST-FILTER-FACADES* "1" "0"))
              (set_tile "chk_filter_vitrazh"
                (if *EXTRACTION-LAST-FILTER-VITRAZH* "1" "0"))
              (set_tile "chk_filter_fonar"
                (if *EXTRACTION-LAST-FILTER-FONAR* "1" "0"))

              ;; ------------------------------------------------
              ;; Задача
              ;; ------------------------------------------------

              (set_tile "rb_task_fasonka"
                (if (eq *EXTRACTION-LAST-TASK* 'FASONKA) "1" "0"))
              (set_tile "rb_task_subsystem"
                (if (eq *EXTRACTION-LAST-TASK* 'SUBSYSTEM) "1" "0"))
              (set_tile "rb_task_cladding"
                (if (eq *EXTRACTION-LAST-TASK* 'CLADDING) "1" "0"))
              (set_tile "rb_task_vitrazh"
                (if (eq *EXTRACTION-LAST-TASK* 'VITRAZH) "1" "0"))
              (set_tile "rb_task_zapolnenie"
                (if (eq *EXTRACTION-LAST-TASK* 'ZAPOLNENIE) "1" "0"))

              ;; ------------------------------------------------
              ;; Чекбоксы Подсистемы
              ;; ------------------------------------------------

              (set_tile "chk_subsystem_1"
                (if (car *EXTRACTION-LAST-SUBSYSTEM-CHECKS*) "1" "0"))
              (set_tile "chk_subsystem_2"
                (if (cadr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*) "1" "0"))
              (set_tile "chk_subsystem_3"
                (if (caddr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*) "1" "0"))

              ;; =================================================
              ;; ИНИЦИАЛИЗАЦИЯ ПЕРЕИМЕНОВАТОРА
              ;; =================================================

              (if (= (type blockrename-init) 'SUBR)
                (blockrename-init)
              )

              ;; =================================================
              ;; НАСТРОЙКА СОСТОЯНИЯ ПОДСИСТЕМЫ
              ;; =================================================

              (if (eq *EXTRACTION-LAST-TASK* 'SUBSYSTEM)
                (progn
                  (mode_tile "box_subsystem_layers" 0)
                  (mode_tile "chk_subsystem_1" 0)
                  (mode_tile "chk_subsystem_2" 0)
                  (mode_tile "chk_subsystem_3" 0)

                  (extraction-rebuild-layer-list)

                  (if *EXTRACTION-LAST-SUBSYSTEM-LAYERS*
                    (extraction-select-layers-in-list
                      *EXTRACTION-LAST-SUBSYSTEM-LAYERS*))

                  (extraction-sync-checks-from-layers)
                )
                (progn
                  (mode_tile "box_subsystem_layers" 1)
                  (mode_tile "chk_subsystem_1" 1)
                  (mode_tile "chk_subsystem_2" 1)
                  (mode_tile "chk_subsystem_3" 1)

                  (extraction-rebuild-layer-list)

                  (if *EXTRACTION-LAST-FASONKA-LAYERS*
                    (extraction-select-layers-in-list
                      *EXTRACTION-LAST-FASONKA-LAYERS*))
                )
              )

              ;; =================================================
              ;; НАЗНАЧЕНИЕ ДЕЙСТВИЙ
              ;; =================================================

              ;; -------------------------------------------------
              ;; Основные кнопки
              ;; -------------------------------------------------

              (action_tile "btn_help"   "(extraction-help)")
              (action_tile "btn_save"   "(extraction-save)")
              (action_tile "btn_saveas" "(extraction-saveas)")
              (action_tile "btn_close"  "(extraction-close)")

              ;; -------------------------------------------------
              ;; Раскрой
              ;; -------------------------------------------------

              (action_tile "btn_cutline"  "(extraction-cutline)")
              (action_tile "btn_cutsheet" "(extraction-cutsheet)")

              ;; -------------------------------------------------
              ;; Слои
              ;; -------------------------------------------------

              (action_tile "btn_select_all"      "(extraction-select-all-layers)")
              (action_tile "btn_clear_all"       "(extraction-clear-all-layers)")
              (action_tile "chk_filter_facades"  "(extraction-filter-facades)")
              (action_tile "chk_filter_vitrazh"  "(extraction-filter-vitrazh)")
              (action_tile "chk_filter_fonar"    "(extraction-filter-fonar)")
              (action_tile "lst_layers"          "(extraction-layer-selection)")

              ;; -------------------------------------------------
              ;; Режим отчёта
              ;; -------------------------------------------------

              (action_tile "rb_detail"
                "(setq *EXTRACTION-REPORT-MODE* \"DETAIL\")(setq *EXTRACTION-LAST-REPORT-MODE* \"DETAIL\")")
              (action_tile "rb_summary"
                "(setq *EXTRACTION-REPORT-MODE* \"SUMMARY\")(setq *EXTRACTION-LAST-REPORT-MODE* \"SUMMARY\")")

              ;; -------------------------------------------------
              ;; Задачи
              ;; -------------------------------------------------

              (action_tile "rb_task_fasonka"
                "(setq *EXTRACTION-TASK-ID* 'FASONKA)(setq *EXTRACTION-LAST-TASK* 'FASONKA)(extraction-toggle-subsystem-layers)")
              (action_tile "rb_task_subsystem"
                "(setq *EXTRACTION-TASK-ID* 'SUBSYSTEM)(setq *EXTRACTION-LAST-TASK* 'SUBSYSTEM)(extraction-toggle-subsystem-layers)")
              (action_tile "rb_task_cladding"
                "(setq *EXTRACTION-TASK-ID* 'CLADDING)(setq *EXTRACTION-LAST-TASK* 'CLADDING)(extraction-toggle-subsystem-layers)")
              (action_tile "rb_task_vitrazh"
                "(setq *EXTRACTION-TASK-ID* 'VITRAZH)(setq *EXTRACTION-LAST-TASK* 'VITRAZH)(extraction-toggle-subsystem-layers)")
              (action_tile "rb_task_zapolnenie"
                "(setq *EXTRACTION-TASK-ID* 'ZAPOLNENIE)(setq *EXTRACTION-LAST-TASK* 'ZAPOLNENIE)(extraction-toggle-subsystem-layers)")

              ;; -------------------------------------------------
              ;; Чекбоксы Подсистемы
              ;; -------------------------------------------------

              (action_tile "chk_subsystem_1" "(extraction-subsystem-check-changed 1)")
              (action_tile "chk_subsystem_2" "(extraction-subsystem-check-changed 2)")
              (action_tile "chk_subsystem_3" "(extraction-subsystem-check-changed 3)")

              ;; =================================================
              ;; НОВЫЙ БЛОК — ПЕРЕИМЕНОВАТОР
              ;; =================================================

              (action_tile "chk_filter_anonymous" "(blockrename-filter-anonymous)")
              (action_tile "edt_block_search"     "(blockrename-search-changed)")
              (action_tile "lst_blocks"           "(blockrename-selected)")
              (action_tile "btn_block_rename"     "(blockrename-rename)")

              ;; =================================================
              ;; ЗАПУСК ДИАЛОГА
              ;; =================================================

              (start_dialog)

              ;; =================================================
              ;; ОБРАБОТКА ДЕЙСТВИЙ ПОСЛЕ ЗАКРЫТИЯ
              ;; =================================================

              (cond

                ;; ------------------------------------------------
                ;; SAVE
                ;; ------------------------------------------------

                ((eq *EXTRACTION-ACTION* 'SAVE)
                 (run-task
                   *EXTRACTION-TASK-ID*
                   *EXTRACTION-SELECTED-LAYERS*
                   *EXTRACTION-REPORT-MODE*
                   *EXTRACTION-EXPORT-EXCEL*
                   *EXTRACTION-EXPORT-TXT*
                   *EXTRACTION-CREATE-TABLE*
                   nil)
                )

                ;; ------------------------------------------------
                ;; SAVE AS
                ;; ------------------------------------------------

                ((eq *EXTRACTION-ACTION* 'SAVEAS)
                 (setq save-base
                   (vl-catch-all-apply 'tu-get-save-base
                     (list *EXTRACTION-TASK-ID*)))

                 (if (vl-catch-all-error-p save-base)
                   (princ "\nОшибка выбора файла.")
                   (if save-base
                     (run-task
                       *EXTRACTION-TASK-ID*
                       *EXTRACTION-SELECTED-LAYERS*
                       *EXTRACTION-REPORT-MODE*
                       *EXTRACTION-EXPORT-EXCEL*
                       *EXTRACTION-EXPORT-TXT*
                       *EXTRACTION-CREATE-TABLE*
                       save-base)
                   )
                 )
                )

                ;; ------------------------------------------------
                ;; CUTLINE
                ;; ------------------------------------------------

                ((eq *EXTRACTION-ACTION* 'CUTLINE)
                 (setq r
                   (vl-catch-all-apply 'cutline-main
                     (list *EXTRACTION-SELECTED-LAYERS*)))

                 (if (vl-catch-all-error-p r)
                   (princ "\nМодуль CUTLINE не загружен или ошибка выполнения."))
                )

                ;; ------------------------------------------------
                ;; CUTSHEET
                ;; ------------------------------------------------

                ((eq *EXTRACTION-ACTION* 'CUTSHEET)
                 (setq r
                   (vl-catch-all-apply 'cutsheet-main
                     (list *EXTRACTION-SELECTED-LAYERS*)))

                 (if (vl-catch-all-error-p r)
                   (princ "\nМодуль CUTSHEET не загружен или ошибка выполнения."))
                )
              )

              ;; --------------------------------------------------
              ;; Выгрузка DCL
              ;; --------------------------------------------------

              (unload_dialog *EXTRACTION-DCL-ID*)
            )
            (alert "Не удалось открыть диалог EXTRACTION.")
          )
        )
      )
    )
  )

  (princ)
)


;; ============================================================
;; РУССКАЯ КОМАНДА
;; ============================================================

(defun c:ЭКСТРАКЦИЯ ()
  (c:extraction)
)


(princ "\nEXTRACTION.LSP загружен.")
(princ)