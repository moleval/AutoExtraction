;;; ============================================================
;;; extraction.lsp
;;; Диспетчер EXTRACTION / ЭКСТРАКЦИЯ
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
      "Подсистема оцинкованная"
      "Подсистема алюминиевая")
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

(defun extraction-unique-ci
       (lst / out x key)

  (setq out '())

  (if (listp lst)

    (foreach x lst

      (if (= (type x) 'STR)

        (progn

          (setq key (strcase x))

          (if
            (not
              (vl-some
                '(lambda (y)
                   (= (strcase y) key)
                 )
                out
              )
            )

            (setq
              out
              (append out (list x))
            )
          )
        )
      )
    )
  )

  out
)


;; ============================================================
;; ПОЛУЧЕНИЕ ВСЕХ СЛОЁВ ЧЕРТЕЖА
;; ============================================================

(defun extraction-layer-names
       ( / acad doc layers out name)

  (setq out '())

  (setq
    acad
    (vl-catch-all-apply
      'vlax-get-acad-object
      '()
    )
  )

  (if
    (and
      (not (vl-catch-all-error-p acad))
      acad
    )

    (progn

      (setq
        doc
        (vl-catch-all-apply
          'vla-get-ActiveDocument
          (list acad)
        )
      )

      (if
        (and
          (not (vl-catch-all-error-p doc))
          doc
        )

        (progn

          (setq
            layers
            (vl-catch-all-apply
              'vla-get-Layers
              (list doc)
            )
          )

          (if
            (and
              (not (vl-catch-all-error-p layers))
              layers
            )

            (vlax-for lay layers

              (setq
                name
                (vl-catch-all-apply
                  'vla-get-Name
                  (list lay)
                )
              )

              (if
                (and
                  (not (vl-catch-all-error-p name))
                  (= (type name) 'STR)
                  (> (strlen name) 0)
                )

                (setq
                  out
                  (cons name out)
                )
              )
            )
          )
        )
      )
    )
  )

  (setq
    out
    (vl-remove-if-not
      '(lambda (x)
         (= (type x) 'STR)
       )
      out
    )
  )

  (setq
    out
    (vl-sort
      out
      '(lambda (a b)
         (<
           (strcase a)
           (strcase b)
         )
       )
    )
  )

  (extraction-unique-ci out)
)


;; ============================================================
;; ЗАГРУЗКА МОДУЛЕЙ
;; ============================================================

(defun extraction-project-root
       ( / dsp)

  (setq dsp (findfile "extraction.lsp"))

  (if dsp
    (vl-filename-directory
      (vl-filename-directory dsp)
    )
    nil
  )
)


(defun extraction-modules-dir
       ( / dsp)

  (setq dsp (findfile "extraction.lsp"))

  (if dsp
    (vl-filename-directory dsp)
    nil
  )
)


(defun extraction-load-all
       ( / root common f path)

  (setq root (extraction-project-root))

  (if root

    (progn

      (setq
        common
        (strcat root "\\common\\")
      )

      (foreach f
        '(
          "task-utils.lsp"
          "layer-utils.lsp"
          "select-utils.lsp"
          "excel-utils.lsp"
          "table-utils.lsp"
          "txt-utils.lsp"
         )

        (setq
          path
          (strcat common f)
        )

        (if
          (findfile path)

          (load path)

          (princ
            (strcat
              "\n[EXTRACTION] Не найден: "
              path
            )
          )
        )
      )

      (setq
        path
        (strcat root "\\Extraction\\fasonka.lsp")
      )

      (if
        (findfile path)
        (load path)
        (princ
          (strcat
            "\n[EXTRACTION] Не найден: "
            path
          )
        )
      )

      (setq
        path
        (strcat root "\\Extraction\\subsystem.lsp")
      )

      (if
        (findfile path)
        (load path)
        (princ
          (strcat
            "\n[EXTRACTION] Не найден: "
            path
          )
        )
      )

      (setq
        path
        (strcat root "\\Extraction\\cutline.lsp")
      )

      (if
        (findfile path)
        (load path)
        (princ
          (strcat
            "\n[EXTRACTION] Не найден: "
            path
          )
        )
      )

      (setq
        path
        (strcat root "\\Extraction\\cutsheet.lsp")
      )

      (if
        (findfile path)
        (load path)
        (princ
          (strcat
            "\n[EXTRACTION] Не найден: "
            path
          )
        )
      )
    )

    (princ
      "\n[EXTRACTION] Корень проекта не найден."
    )
  )

  T
)


;; ============================================================
;; РАЗБОР ИНДЕКСОВ DCL
;; ============================================================

(defun extraction-parse-indices
       (s / x)

  (if
    (and
      (= (type s) 'STR)
      (/= s "")
    )

    (progn

      (setq
        x
        (read
          (strcat "(" s ")")
        )
      )

      (if
        (= (type x) 'LIST)
        x
        nil
      )
    )

    nil
  )
)


;; ============================================================
;; ПОЛУЧЕНИЕ ВЫБРАННЫХ СЛОЁВ
;; ============================================================

(defun extraction-selected-names
       ( / s indices out i n)

  (setq
    s
    (get_tile "lst_layers")
  )

  (if
    (or
      (null s)
      (/= (type s) 'STR)
      (= s "")
    )

    nil

    (progn

      (setq
        indices
        (extraction-parse-indices s)
      )

      (setq out '())

      (setq
        n
        (length *EXTRACTION-VISIBLE-LAYERS*)
      )

      (foreach i indices

        (if
          (and
            (numberp i)
            (>= i 0)
            (< i n)
          )

          (setq
            out
            (cons
              (nth i *EXTRACTION-VISIBLE-LAYERS*)
              out
            )
          )
        )
      )

      (extraction-unique-ci out)
    )
  )
)


;; ============================================================
;; ФИЛЬТРАЦИЯ СЛОЁВ ПО КЛЮЧЕВЫМ СЛОВАМ
;; ============================================================

(defun extraction-filter-layers-by-keywords
       (keywords / filters result f fname)

  (setq
    filters
    (vl-catch-all-apply
      'tu-group-filter-names-and-layers
      '()
    )
  )

  (if
    (vl-catch-all-error-p filters)
    (setq filters nil)
  )

  (setq result '())

  (if (listp filters)

    (foreach f filters

      (setq fname (car f))

      (if
        (and
          (= (type fname) 'STR)

          (vl-some
            '(lambda (k)
               (and
                 (= (type k) 'STR)
                 (vl-string-search
                   (strcase k)
                   (strcase fname)
                 )
               )
             )
            keywords
          )
        )

        (setq
          result
          (append
            result
            (cadr f)
          )
        )
      )
    )
  )

  (extraction-unique-ci result)
)


;; ============================================================
;; ПЕРЕСТРОЕНИЕ СПИСКА СЛОЁВ
;; ============================================================

(defun extraction-rebuild-layer-list
       ( / vis keywords)

  (setq
    *EXTRACTION-SELECTED-INDICES*
    '()
  )

  (setq keywords '())

  (if *EXTRACTION-FILTER-FACADES*
    (setq
      keywords
      (cons "фасад" keywords)
    )
  )

  (if *EXTRACTION-FILTER-VITRAZH*
    (setq
      keywords
      (cons "витраж" keywords)
    )
  )

  (if *EXTRACTION-FILTER-FONAR*
    (setq
      keywords
      (cons "фонар" keywords)
    )
  )

  (if keywords

    (progn

      (setq
        vis
        (extraction-filter-layers-by-keywords keywords)
      )

      (if
        (or
          (null vis)
          (not (listp vis))
        )

        (setq
          vis
          *EXTRACTION-ALL-LAYERS*
        )
      )

      (setq
        *EXTRACTION-VISIBLE-LAYERS*
        vis
      )
    )

    (setq
      *EXTRACTION-VISIBLE-LAYERS*
      *EXTRACTION-ALL-LAYERS*
    )
  )

  (setq
    *EXTRACTION-VISIBLE-LAYERS*
    (vl-sort
      (vl-remove-if-not
        '(lambda (x)
           (= (type x) 'STR)
         )

        (if
          (listp *EXTRACTION-VISIBLE-LAYERS*)
          *EXTRACTION-VISIBLE-LAYERS*
          '()
        )
      )

      '(lambda (a b)
         (<
           (strcase a)
           (strcase b)
         )
       )
    )
  )

  (start_list "lst_layers")

  (mapcar
    'add_list
    *EXTRACTION-VISIBLE-LAYERS*
  )

  (end_list)

  (set_tile "lst_layers" "")
)


;; ============================================================
;; СПЕЦИАЛЬНЫЙ СПИСОК ПОДСИСТЕМЫ
;; ============================================================

(defun extraction-subsystem-layer-list ()

  (setq
    *EXTRACTION-VISIBLE-LAYERS*
    '(
      "Подсистема"
      "Подсистема оцинкованная"
      "Подсистема алюминиевая"
     )
  )

  (start_list "lst_layers")

  (mapcar
    'add_list
    *EXTRACTION-VISIBLE-LAYERS*
  )

  (end_list)

  (set_tile "lst_layers" "")
)


;; ============================================================
;; ОЧИСТКА ВЫБОРА
;; ============================================================

(defun extraction-clear-layer-selection ()

  (set_tile "lst_layers" "")
)


;; ============================================================
;; УСТАНОВКА ВЫДЕЛЕНИЯ СЛОЁВ
;; ============================================================

(defun extraction-select-layers-in-list
       (layers-to-select / i item selected str)

  (setq selected '())
  (setq i 0)

  (foreach item *EXTRACTION-VISIBLE-LAYERS*

    (if
      (vl-some
        '(lambda (x)
           (= (strcase x) (strcase item))
         )
        layers-to-select
      )

      (setq
        selected
        (cons i selected)
      )
    )

    (setq i (1+ i))
  )

  (setq selected (reverse selected))
  (setq str "")

  (foreach i selected

    (setq
      str
      (if (= str "")
        (itoa i)
        (strcat str " " (itoa i))
      )
    )
  )

  (set_tile "lst_layers" str)
)


;; ============================================================
;; СИНХРОНИЗАЦИЯ ЧЕКБОКСОВ ПОДСИСТЕМЫ
;; ============================================================

(defun extraction-sync-checks-from-layers ()

  (setq
    selected
    (extraction-selected-names)
  )

  (set_tile
    "chk_subsystem_1"
    (if
      (member "Подсистема" selected)
      "1"
      "0"
    )
  )

  (set_tile
    "chk_subsystem_2"
    (if
      (member "Подсистема оцинкованная" selected)
      "1"
      "0"
    )
  )

  (set_tile
    "chk_subsystem_3"
    (if
      (member "Подсистема алюминиевая" selected)
      "1"
      "0"
    )
  )
)


;; ============================================================
;; ИЗМЕНЕНИЕ ЧЕКБОКСА ПОДСИСТЕМЫ
;; ============================================================

(defun extraction-subsystem-check-changed
       (key / val layers-to-select)

  (setq
    val
    (=
      (get_tile
        (strcat
          "chk_subsystem_"
          (itoa key)
        )
      )
      "1"
    )
  )

  (setq
    *EXTRACTION-LAST-SUBSYSTEM-CHECKS*
    (list

      (if
        (= key 1)
        val
        (car
          *EXTRACTION-LAST-SUBSYSTEM-CHECKS*
        )
      )

      (if
        (= key 2)
        val
        (cadr
          *EXTRACTION-LAST-SUBSYSTEM-CHECKS*
        )
      )

      (if
        (= key 3)
        val
        (caddr
          *EXTRACTION-LAST-SUBSYSTEM-CHECKS*
        )
      )
    )
  )

  (setq layers-to-select '())

  (if
    (car *EXTRACTION-LAST-SUBSYSTEM-CHECKS*)

    (setq
      layers-to-select
      (cons
        "Подсистема"
        layers-to-select
      )
    )
  )

  (if
    (cadr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*)

    (setq
      layers-to-select
      (cons
        "Подсистема оцинкованная"
        layers-to-select
      )
    )
  )

  (if
    (caddr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*)

    (setq
      layers-to-select
      (cons
        "Подсистема алюминиевая"
        layers-to-select
      )
    )
  )

  ;; Чекбоксы являются быстрым выбором именно этих
  ;; трёх слоёв. Другие выбранные вручную слои не
  ;; должны исчезать при переключении чекбокса.
  (setq
    current-selection
    (extraction-selected-names)
  )

  ;; Удаляем из текущего выбора только три специальных слоя.
  (setq
    current-selection
    (vl-remove-if
      '(lambda (x)
         (or
           (= (strcase x) "ПОДСИСТЕМА")
           (= (strcase x) "ПОДСИСТЕМА ОЦИНКОВАННАЯ")
           (= (strcase x) "ПОДСИСТЕМА АЛЮМИНИЕВАЯ")
         )
       )
      current-selection
    )
  )

  ;; Добавляем выбранные чекбоксами слои.
  (setq
    current-selection
    (append
      current-selection
      layers-to-select
    )
  )

  (extraction-select-layers-in-list current-selection)
  (extraction-sync-checks-from-layers)
)


;; ============================================================
;; ПЕРЕКЛЮЧЕНИЕ ЗАДАЧИ
;; ============================================================

(defun extraction-toggle-subsystem-layers
       ( / layers-to-select)

  (if
    (= (get_tile "rb_task_subsystem") "1")

    ;; --------------------------------------------------------
    ;; ПОДСИСТЕМА
    ;; --------------------------------------------------------
    (progn

      (mode_tile "box_subsystem_layers" 0)

      (mode_tile "chk_subsystem_1" 0)
      (mode_tile "chk_subsystem_2" 0)
      (mode_tile "chk_subsystem_3" 0)

      ;; В списке "Слои" показываем ВСЕ слои чертежа.
      ;; Фильтры Фасад/Витраж/Фонарь продолжают работать.
      (extraction-rebuild-layer-list)

      ;; Восстанавливаем отдельное состояние Подсистемы.
      (setq
        layers-to-select
        *EXTRACTION-LAST-SUBSYSTEM-LAYERS*
      )

      ;; Первое использование: берём состояние чекбоксов.
      (if
        (null layers-to-select)

        (progn

          (setq layers-to-select '())

          (if
            (car *EXTRACTION-LAST-SUBSYSTEM-CHECKS*)

            (setq
              layers-to-select
              (cons
                "Подсистема"
                layers-to-select
              )
            )
          )

          (if
            (cadr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*)

            (setq
              layers-to-select
              (cons
                "Подсистема оцинкованная"
                layers-to-select
              )
            )
          )

          (if
            (caddr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*)

            (setq
              layers-to-select
              (cons
                "Подсистема алюминиевая"
                layers-to-select
              )
            )
          )
        )
      )

      (extraction-select-layers-in-list
        layers-to-select
      )

      (extraction-sync-checks-from-layers)
    )

    ;; --------------------------------------------------------
    ;; ВСЕ ОСТАЛЬНЫЕ ЗАДАЧИ
    ;; --------------------------------------------------------
    (progn

      (mode_tile "box_subsystem_layers" 1)

      (mode_tile "chk_subsystem_1" 1)
      (mode_tile "chk_subsystem_2" 1)
      (mode_tile "chk_subsystem_3" 1)

      (extraction-rebuild-layer-list)

      ;; Восстанавливаем состояние обычных задач.
      (if *EXTRACTION-LAST-FASONKA-LAYERS*

        (extraction-select-layers-in-list
          *EXTRACTION-LAST-FASONKA-LAYERS*
        )
      )
    )
  )
)


;; ============================================================
;; ИЗМЕНЕНИЕ ВЫБОРА В СПИСКЕ
;; ============================================================

(defun extraction-layer-selection-changed
       ( / selected)

  (setq
    selected
    (extraction-selected-names)
  )

  (setq
    *EXTRACTION-SELECTED-LAYERS*
    selected
  )

  (if
    (eq *EXTRACTION-TASK-ID* 'SUBSYSTEM)

    (progn

      (setq
        *EXTRACTION-LAST-SUBSYSTEM-LAYERS*
        selected
      )

      (extraction-sync-checks-from-layers)
    )

    (setq
      *EXTRACTION-LAST-FASONKA-LAYERS*
      selected
    )
  )
)


(defun extraction-layer-selection ()

  (extraction-layer-selection-changed)
)


;; ============================================================
;; ЧТЕНИЕ ПАРАМЕТРОВ
;; ============================================================

(defun extraction-read-params
       ( / selected)

  ;; СНАЧАЛА определяем текущую задачу из DCL.
  ;; Это важно: нельзя сохранять выбранные слои,
  ;; ориентируясь на старое значение *EXTRACTION-TASK-ID*.
  (cond

    ((= (get_tile "rb_task_fasonka") "1")
     (setq *EXTRACTION-TASK-ID* 'FASONKA)
    )

    ((= (get_tile "rb_task_subsystem") "1")
     (setq *EXTRACTION-TASK-ID* 'SUBSYSTEM)
    )

    ((= (get_tile "rb_task_cladding") "1")
     (setq *EXTRACTION-TASK-ID* 'CLADDING)
    )

    ((= (get_tile "rb_task_vitrazh") "1")
     (setq *EXTRACTION-TASK-ID* 'VITRAZH)
    )

    ((= (get_tile "rb_task_zapolnenie") "1")
     (setq *EXTRACTION-TASK-ID* 'ZAPOLNENIE)
    )

    (T
     (setq *EXTRACTION-TASK-ID* 'FASONKA)
    )
  )

  (setq
    *EXTRACTION-LAST-TASK*
    *EXTRACTION-TASK-ID*
  )

  ;; Читаем фактическое выделение списка "Слои".
  (setq
    selected
    (extraction-selected-names)
  )

  (setq
    *EXTRACTION-SELECTED-LAYERS*
    selected
  )

  ;; Отдельное сохранение для Подсистемы и остальных задач.
  (if
    (eq *EXTRACTION-TASK-ID* 'SUBSYSTEM)

    (progn

      (setq
        *EXTRACTION-LAST-SUBSYSTEM-LAYERS*
        selected
      )

      (if
        (null selected)

        (progn

          (alert
            "Не выбрано ни одного слоя подсистемы."
          )

          nil
        )

        T
      )
    )

    (progn

      (setq
        *EXTRACTION-LAST-FASONKA-LAYERS*
        selected
      )

      T
    )
  )

  ;; Общие параметры.
  (setq
    *EXTRACTION-REPORT-MODE*
    (if
      (= (get_tile "rb_summary") "1")
      "SUMMARY"
      "DETAIL"
    )
  )

  (setq
    *EXTRACTION-LAST-REPORT-MODE*
    *EXTRACTION-REPORT-MODE*
  )

  (setq
    *EXTRACTION-EXPORT-EXCEL*
    (= (get_tile "chk_xls") "1")
  )

  (setq
    *EXTRACTION-LAST-EXPORT-EXCEL*
    *EXTRACTION-EXPORT-EXCEL*
  )

  (setq
    *EXTRACTION-EXPORT-TXT*
    (= (get_tile "chk_txt") "1")
  )

  (setq
    *EXTRACTION-LAST-EXPORT-TXT*
    *EXTRACTION-EXPORT-TXT*
  )

  (setq
    *EXTRACTION-CREATE-TABLE*
    (= (get_tile "chk_acad") "1")
  )

  (setq
    *EXTRACTION-LAST-CREATE-TABLE*
    *EXTRACTION-CREATE-TABLE*
  )

  T
)


;; ============================================================
;; ЗАПУСК ЗАДАЧИ
;; ============================================================

(defun run-task
       (task-id layers report-mode export-excel export-txt
                create-table save-base / r)

  (cond

    ((eq task-id 'FASONKA)

     (setq
       r
       (vl-catch-all-apply
         'fasonka-main
         (list
           layers
           report-mode
           export-excel
           export-txt
           create-table
           save-base
         )
       )
     )

     (if
       (vl-catch-all-error-p r)

       (princ
         "\nМодуль Фасонка не загружен или ошибка выполнения."
       )
     )
    )

    ((eq task-id 'SUBSYSTEM)

     (setq
       r
       (vl-catch-all-apply
         'subsystem-main
         (list
           layers
           report-mode
           export-excel
           export-txt
           create-table
           save-base
         )
       )
     )

     (if
       (vl-catch-all-error-p r)

       (princ
         "\nМодуль Подсистема не загружен или ошибка выполнения."
       )
     )
    )

    ((eq task-id 'CLADDING)

     (if
       (fboundp 'cladding-main)

       (setq
         r
         (vl-catch-all-apply
           'cladding-main
           (list
             layers
             report-mode
             export-excel
             export-txt
             create-table
             save-base
           )
         )
       )

       (princ
         "\nМодуль Облицовка не загружен."
       )
     )
    )

    ((eq task-id 'VITRAZH)

     (if
       (fboundp 'vitrazh-main)

       (setq
         r
         (vl-catch-all-apply
           'vitrazh-main
           (list
             layers
             report-mode
             export-excel
             export-txt
             create-table
             save-base
           )
         )
       )

       (princ
         "\nМодуль Витраж не загружен."
       )
     )
    )

    ((eq task-id 'ZAPOLNENIE)

     (if
       (fboundp 'zapolnenie-main)

       (setq
         r
         (vl-catch-all-apply
           'zapolnenie-main
           (list
             layers
             report-mode
             export-excel
             export-txt
             create-table
             save-base
           )
         )
       )

       (princ
         "\nМодуль Заполнение не загружен."
       )
     )
    )

    (T
     (princ "\nНеизвестная задача.")
    )
  )
)


;; ============================================================
;; ПОМОЩЬ
;; ============================================================

(defun extraction-help ()

  (alert
    (strcat
      "Окно запуска задач.\n\n"
      "Выбор одной задачи, слоёв, режима отчёта и форматов вывода.\n"
      "Кнопки Раскрой хлыста / Раскрой листа запускают модули раскроя.\n\n"
      "Сохранить — имя файла по умолчанию.\n"
      "Сохранить как... — выбор пути и имени.\n"
      "Для Подсистемы доступны все слои; три чекбокса — быстрый выбор типовых слоёв."
    )
  )
)


;; ============================================================
;; КНОПКИ
;; ============================================================

(defun extraction-save ()

  (if
    (extraction-read-params)

    (progn

      (setq
        *EXTRACTION-ACTION*
        'SAVE
      )

      (done_dialog 1)
    )
  )
)


(defun extraction-saveas ()

  (if
    (extraction-read-params)

    (progn

      (setq
        *EXTRACTION-ACTION*
        'SAVEAS
      )

      (done_dialog 1)
    )
  )
)


(defun extraction-close ()

  (setq
    *EXTRACTION-ACTION*
    'CANCEL
  )

  (done_dialog 0)
)


(defun extraction-cutline ()

  (setq
    *EXTRACTION-ACTION*
    'CUTLINE
  )

  (done_dialog 1)
)


(defun extraction-cutsheet ()

  (setq
    *EXTRACTION-ACTION*
    'CUTSHEET
  )

  (done_dialog 1)
)


;; ============================================================
;; ФИЛЬТРЫ
;; ============================================================

(defun extraction-filter-facades ()

  (setq
    *EXTRACTION-FILTER-FACADES*
    (= (get_tile "chk_filter_facades") "1")
  )

  (setq
    *EXTRACTION-LAST-FILTER-FACADES*
    *EXTRACTION-FILTER-FACADES*
  )

  ;; Фильтры работают и для Подсистемы.
  ;; После перестроения восстанавливаем состояние текущей группы.
  (extraction-rebuild-layer-list)

  (if
    (eq *EXTRACTION-TASK-ID* 'SUBSYSTEM)

    (if
      *EXTRACTION-LAST-SUBSYSTEM-LAYERS*

      (extraction-select-layers-in-list
        *EXTRACTION-LAST-SUBSYSTEM-LAYERS*
      )
    )

    (if
      *EXTRACTION-LAST-FASONKA-LAYERS*

      (extraction-select-layers-in-list
        *EXTRACTION-LAST-FASONKA-LAYERS*
      )
    )
  )
)


(defun extraction-filter-vitrazh ()

  (setq
    *EXTRACTION-FILTER-VITRAZH*
    (= (get_tile "chk_filter_vitrazh") "1")
  )

  (setq
    *EXTRACTION-LAST-FILTER-VITRAZH*
    *EXTRACTION-FILTER-VITRAZH*
  )

  ;; Фильтры работают и для Подсистемы.
  ;; После перестроения восстанавливаем состояние текущей группы.
  (extraction-rebuild-layer-list)

  (if
    (eq *EXTRACTION-TASK-ID* 'SUBSYSTEM)

    (if
      *EXTRACTION-LAST-SUBSYSTEM-LAYERS*

      (extraction-select-layers-in-list
        *EXTRACTION-LAST-SUBSYSTEM-LAYERS*
      )
    )

    (if
      *EXTRACTION-LAST-FASONKA-LAYERS*

      (extraction-select-layers-in-list
        *EXTRACTION-LAST-FASONKA-LAYERS*
      )
    )
  )
)


(defun extraction-filter-fonar ()

  (setq
    *EXTRACTION-FILTER-FONAR*
    (= (get_tile "chk_filter_fonar") "1")
  )

  (setq
    *EXTRACTION-LAST-FILTER-FONAR*
    *EXTRACTION-FILTER-FONAR*
  )

  ;; Фильтры работают и для Подсистемы.
  ;; После перестроения восстанавливаем состояние текущей группы.
  (extraction-rebuild-layer-list)

  (if
    (eq *EXTRACTION-TASK-ID* 'SUBSYSTEM)

    (if
      *EXTRACTION-LAST-SUBSYSTEM-LAYERS*

      (extraction-select-layers-in-list
        *EXTRACTION-LAST-SUBSYSTEM-LAYERS*
      )
    )

    (if
      *EXTRACTION-LAST-FASONKA-LAYERS*

      (extraction-select-layers-in-list
        *EXTRACTION-LAST-FASONKA-LAYERS*
      )
    )
  )
)


;; ============================================================
;; ОСНОВНАЯ КОМАНДА
;; ============================================================

(defun c:extraction
       ( / dcl-file save-base modules-dir r)

  (vl-load-com)

  ;; Всегда загружаем актуальный проект
  (extraction-load-all)

  (setq dcl-file nil)

  (setq
    modules-dir
    (extraction-modules-dir)
  )

  (if modules-dir

    (setq
      dcl-file
      (findfile
        (strcat
          modules-dir
          "\\extraction.dcl"
        )
      )
    )
  )

  (if (null dcl-file)
    (setq
      dcl-file
      (findfile "extraction.dcl")
    )
  )

  (if (null dcl-file)

    (progn

      (alert
        "Не найден файл extraction.dcl."
      )

      (princ)
    )

    (progn

      ;; --------------------------------------------
      ;; Состояние чертежа
      ;; --------------------------------------------

      (setq
        *EXTRACTION-ALL-LAYERS*
        (extraction-layer-names)
      )

      (setq
        *EXTRACTION-SELECTED-LAYERS*
        nil
      )

      (setq
        *EXTRACTION-FILTER-FACADES*
        *EXTRACTION-LAST-FILTER-FACADES*
      )

      (setq
        *EXTRACTION-FILTER-VITRAZH*
        *EXTRACTION-LAST-FILTER-VITRAZH*
      )

      (setq
        *EXTRACTION-FILTER-FONAR*
        *EXTRACTION-LAST-FILTER-FONAR*
      )

      (setq
        *EXTRACTION-TASK-ID*
        *EXTRACTION-LAST-TASK*
      )

      (setq
        *EXTRACTION-REPORT-MODE*
        *EXTRACTION-LAST-REPORT-MODE*
      )

      (setq
        *EXTRACTION-EXPORT-EXCEL*
        *EXTRACTION-LAST-EXPORT-EXCEL*
      )

      (setq
        *EXTRACTION-EXPORT-TXT*
        *EXTRACTION-LAST-EXPORT-TXT*
      )

      (setq
        *EXTRACTION-CREATE-TABLE*
        *EXTRACTION-LAST-CREATE-TABLE*
      )

      (setq
        *EXTRACTION-ACTION*
        'CANCEL
      )

      ;; --------------------------------------------
      ;; DCL
      ;; --------------------------------------------

      (setq
        *EXTRACTION-DCL-ID*
        (load_dialog dcl-file)
      )

      (if
        (< *EXTRACTION-DCL-ID* 0)

        (alert
          "Не удалось загрузить extraction.dcl."
        )

        (progn

          (if
            (new_dialog
              "extraction_dialog"
              *EXTRACTION-DCL-ID*
            )

            (progn

              ;; ------------------------------------
              ;; Общие параметры
              ;; ------------------------------------

              (set_tile
                "rb_detail"
                (if
                  (= *EXTRACTION-LAST-REPORT-MODE* "DETAIL")
                  "1"
                  "0"
                )
              )

              (set_tile
                "rb_summary"
                (if
                  (= *EXTRACTION-LAST-REPORT-MODE* "SUMMARY")
                  "1"
                  "0"
                )
              )

              (set_tile
                "chk_xls"
                (if
                  *EXTRACTION-LAST-EXPORT-EXCEL*
                  "1"
                  "0"
                )
              )

              (set_tile
                "chk_txt"
                (if
                  *EXTRACTION-LAST-EXPORT-TXT*
                  "1"
                  "0"
                )
              )

              (set_tile
                "chk_acad"
                (if
                  *EXTRACTION-LAST-CREATE-TABLE*
                  "1"
                  "0"
                )
              )

              ;; ------------------------------------
              ;; Фильтры
              ;; ------------------------------------

              (set_tile
                "chk_filter_facades"
                (if
                  *EXTRACTION-LAST-FILTER-FACADES*
                  "1"
                  "0"
                )
              )

              (set_tile
                "chk_filter_vitrazh"
                (if
                  *EXTRACTION-LAST-FILTER-VITRAZH*
                  "1"
                  "0"
                )
              )

              (set_tile
                "chk_filter_fonar"
                (if
                  *EXTRACTION-LAST-FILTER-FONAR*
                  "1"
                  "0"
                )
              )

              ;; ------------------------------------
              ;; Задача
              ;; ------------------------------------

              (set_tile
                "rb_task_fasonka"
                (if
                  (eq *EXTRACTION-LAST-TASK* 'FASONKA)
                  "1"
                  "0"
                )
              )

              (set_tile
                "rb_task_subsystem"
                (if
                  (eq *EXTRACTION-LAST-TASK* 'SUBSYSTEM)
                  "1"
                  "0"
                )
              )

              (set_tile
                "rb_task_cladding"
                (if
                  (eq *EXTRACTION-LAST-TASK* 'CLADDING)
                  "1"
                  "0"
                )
              )

              (set_tile
                "rb_task_vitrazh"
                (if
                  (eq *EXTRACTION-LAST-TASK* 'VITRAZH)
                  "1"
                  "0"
                )
              )

              (set_tile
                "rb_task_zapolnenie"
                (if
                  (eq *EXTRACTION-LAST-TASK* 'ZAPOLNENIE)
                  "1"
                  "0"
                )
              )

              ;; ------------------------------------
              ;; Чекбоксы Подсистемы
              ;; ------------------------------------

              (set_tile
                "chk_subsystem_1"
                (if
                  (car *EXTRACTION-LAST-SUBSYSTEM-CHECKS*)
                  "1"
                  "0"
                )
              )

              (set_tile
                "chk_subsystem_2"
                (if
                  (cadr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*)
                  "1"
                  "0"
                )
              )

              (set_tile
                "chk_subsystem_3"
                (if
                  (caddr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*)
                  "1"
                  "0"
                )
              )

              ;; ------------------------------------
              ;; Список слоёв
              ;; ------------------------------------

              (if
                (eq *EXTRACTION-LAST-TASK* 'SUBSYSTEM)

                (progn

                  (mode_tile
                    "box_subsystem_layers"
                    0
                  )

                  (mode_tile
                    "chk_subsystem_1"
                    0
                  )

                  (mode_tile
                    "chk_subsystem_2"
                    0
                  )

                  (mode_tile
                    "chk_subsystem_3"
                    0
                  )

                  ;; Все слои остаются доступны в списке.
                  ;; Три чекбокса — быстрый выбор типовых слоёв.
                  (extraction-rebuild-layer-list)

                  (if *EXTRACTION-LAST-SUBSYSTEM-LAYERS*
                    (extraction-select-layers-in-list
                      *EXTRACTION-LAST-SUBSYSTEM-LAYERS*
                    )
                  )

                  (extraction-sync-checks-from-layers)
                )

                (progn

                  (mode_tile
                    "box_subsystem_layers"
                    1
                  )

                  (mode_tile
                    "chk_subsystem_1"
                    1
                  )

                  (mode_tile
                    "chk_subsystem_2"
                    1
                  )

                  (mode_tile
                    "chk_subsystem_3"
                    1
                  )

                  (extraction-rebuild-layer-list)

                  ;; Восстанавливаем только Фасонку.
                  (if *EXTRACTION-LAST-FASONKA-LAYERS*
                    (extraction-select-layers-in-list
                      *EXTRACTION-LAST-FASONKA-LAYERS*
                    )
                  )
                )
              )

              ;; ------------------------------------
              ;; ActionTile
              ;; ------------------------------------

              (action_tile
                "btn_help"
                "(extraction-help)"
              )

              (action_tile
                "btn_save"
                "(extraction-save)"
              )

              (action_tile
                "btn_saveas"
                "(extraction-saveas)"
              )

              (action_tile
                "btn_close"
                "(extraction-close)"
              )

              (action_tile
                "btn_cutline"
                "(extraction-cutline)"
              )

              (action_tile
                "btn_cutsheet"
                "(extraction-cutsheet)"
              )

              (action_tile
                "chk_filter_facades"
                "(extraction-filter-facades)"
              )

              (action_tile
                "chk_filter_vitrazh"
                "(extraction-filter-vitrazh)"
              )

              (action_tile
                "chk_filter_fonar"
                "(extraction-filter-fonar)"
              )

              (action_tile
                "lst_layers"
                "(extraction-layer-selection)"
              )

              (action_tile
                "rb_detail"
                "(setq *EXTRACTION-REPORT-MODE* \"DETAIL\")(setq *EXTRACTION-LAST-REPORT-MODE* \"DETAIL\")"
              )

              (action_tile
                "rb_summary"
                "(setq *EXTRACTION-REPORT-MODE* \"SUMMARY\")(setq *EXTRACTION-LAST-REPORT-MODE* \"SUMMARY\")"
              )

              (action_tile
                "rb_task_fasonka"
                "(setq *EXTRACTION-TASK-ID* 'FASONKA)(setq *EXTRACTION-LAST-TASK* 'FASONKA)(extraction-toggle-subsystem-layers)"
              )

              (action_tile
                "rb_task_subsystem"
                "(setq *EXTRACTION-TASK-ID* 'SUBSYSTEM)(setq *EXTRACTION-LAST-TASK* 'SUBSYSTEM)(extraction-toggle-subsystem-layers)"
              )

              (action_tile
                "rb_task_cladding"
                "(setq *EXTRACTION-TASK-ID* 'CLADDING)(setq *EXTRACTION-LAST-TASK* 'CLADDING)(extraction-toggle-subsystem-layers)"
              )

              (action_tile
                "rb_task_vitrazh"
                "(setq *EXTRACTION-TASK-ID* 'VITRAZH)(setq *EXTRACTION-LAST-TASK* 'VITRAZH)(extraction-toggle-subsystem-layers)"
              )

              (action_tile
                "rb_task_zapolnenie"
                "(setq *EXTRACTION-TASK-ID* 'ZAPOLNENIE)(setq *EXTRACTION-LAST-TASK* 'ZAPOLNENIE)(extraction-toggle-subsystem-layers)"
              )

              (action_tile
                "chk_subsystem_1"
                "(extraction-subsystem-check-changed 1)"
              )

              (action_tile
                "chk_subsystem_2"
                "(extraction-subsystem-check-changed 2)"
              )

              (action_tile
                "chk_subsystem_3"
                "(extraction-subsystem-check-changed 3)"
              )

              ;; ------------------------------------
              ;; Диалог
              ;; ------------------------------------

              (start_dialog)

              ;; ------------------------------------
              ;; Выполнение
              ;; ------------------------------------

              (cond

                ((eq *EXTRACTION-ACTION* 'SAVE)

                 (run-task
                   *EXTRACTION-TASK-ID*
                   *EXTRACTION-SELECTED-LAYERS*
                   *EXTRACTION-REPORT-MODE*
                   *EXTRACTION-EXPORT-EXCEL*
                   *EXTRACTION-EXPORT-TXT*
                   *EXTRACTION-CREATE-TABLE*
                   nil
                 )
                )

                ((eq *EXTRACTION-ACTION* 'SAVEAS)

                 (setq
                   save-base
                   (vl-catch-all-apply
                     'tu-get-save-base
                     (list
                       *EXTRACTION-TASK-ID*
                     )
                   )
                 )

                 (if
                   (vl-catch-all-error-p save-base)

                   (princ
                     "\nОшибка выбора файла."
                   )

                   (if save-base

                     (run-task
                       *EXTRACTION-TASK-ID*
                       *EXTRACTION-SELECTED-LAYERS*
                       *EXTRACTION-REPORT-MODE*
                       *EXTRACTION-EXPORT-EXCEL*
                       *EXTRACTION-EXPORT-TXT*
                       *EXTRACTION-CREATE-TABLE*
                       save-base
                     )
                   )
                 )
                )

                ((eq *EXTRACTION-ACTION* 'CUTLINE)

                 (if
                   (fboundp 'cutline-main)

                   (cutline-main)

                   (princ
                     "\nМодуль CUTLINE не загружен."
                   )
                 )
                )

                ((eq *EXTRACTION-ACTION* 'CUTSHEET)

                 (if
                   (fboundp 'cutsheet-main)

                   (cutsheet-main)

                   (princ
                     "\nМодуль CUTSHEET не загружен."
                   )
                 )
                )
              )

              (unload_dialog
                *EXTRACTION-DCL-ID*
              )
            )

            (alert
              "Не удалось открыть диалог EXTRACTION."
            )
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


(princ
  "\nEXTRACTION.LSP загружен."
)

(princ)