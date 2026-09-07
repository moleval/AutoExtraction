;;; ============================================================
;;; fasonka.lsp
;;; Задача FASONKA — подсчёт количества и погонажа фасонного железа
;;; ============================================================

(vl-load-com)

;; ------------------------------------------------------------
;; Загрузка общих библиотек, если они ещё не загружены.
;; ------------------------------------------------------------

(defun fasonka-load-common ( / fasonka-file tasks-root project-root files f)
  (setq fasonka-file (findfile "fasonka.lsp"))

  (if fasonka-file
    (progn
      (setq tasks-root (vl-filename-directory fasonka-file))
      (setq project-root (vl-filename-directory tasks-root))

      (setq files
        (list
          (strcat project-root "\\common\\task-utils.lsp")
          (strcat project-root "\\common\\layer-utils.lsp")
          (strcat project-root "\\common\\excel-utils.lsp")
          (strcat project-root "\\common\\table-utils.lsp")
        )
      )

      (foreach f files
        (if (findfile f)
          (load f)
        )
      )
    )
  )
  T
)

(fasonka-load-common)

;; ------------------------------------------------------------
;; Строковые / числовые функции
;; ------------------------------------------------------------

(defun fasonka-clean-block-name (name / prefix rest)
  (if (null name)
    ""
    (progn
      (setq prefix "Железо ")
      (if (and
            (>= (strlen name) (strlen prefix))
            (= (strcase (substr name 1 (strlen prefix)))
               (strcase prefix))
          )
        (progn
          (setq rest (substr name (1+ (strlen prefix))))
          (if (= rest "")
            name
            (strcat
              (strcase (substr rest 1 1))
              (substr rest 2)
            )
          )
        )
        (strcat
          (strcase (substr name 1 1))
          (substr name 2)
        )
      )
    )
  )
)

(defun fasonka-value-to-number (value / x s)
  (cond
    ((numberp value)
     (float value)
    )

    ((= (type value) 'VARIANT)
     (setq x
       (vl-catch-all-apply
         'vlax-variant-value
         (list value)
       )
     )
     (if (vl-catch-all-error-p x)
       nil
       (fasonka-value-to-number x)
     )
    )

    ((= (type value) 'STR)
     (setq s (vl-string-trim " \t\r\n" value))
     (if (> (strlen s) 0)
       (progn
         (setq s (vl-string-translate "," "." s))
         (while (vl-string-search " " s)
           (setq s (vl-string-subst "" " " s))
         )
         (setq x (vl-catch-all-apply 'atof (list s)))
         (if (vl-catch-all-error-p x)
           nil
           x
         )
       )
       nil
     )
    )

    (t nil)
  )
)

;; ------------------------------------------------------------
;; Защищённое получение EffectiveName
;; ------------------------------------------------------------

(defun fasonka-get-effective-name (obj / r name)
  (setq r
    (vl-catch-all-apply
      'vla-get-EffectiveName
      (list obj)
    )
  )
  (if (vl-catch-all-error-p r)
    (progn
      (setq r
        (vl-catch-all-apply
          'vla-get-Name
          (list obj)
        )
      )
      (if (vl-catch-all-error-p r)
        nil
        r
      )
    )
    r
  )
)

;; ------------------------------------------------------------
;; Получение длины из динамических свойств
;; ------------------------------------------------------------

(defun fasonka-get-length (obj / dynprops prop pname value result)
  (setq dynprops
    (vl-catch-all-apply
      'vlax-invoke
      (list obj 'GetDynamicBlockProperties)
    )
  )

  (if (vl-catch-all-error-p dynprops)
    nil
    (progn
      (setq result nil)
      (foreach prop dynprops
        (if (null result)
          (progn
            (setq pname
              (vl-catch-all-apply
                'vla-get-PropertyName
                (list prop)
              )
            )
            (if (and
                  (not (vl-catch-all-error-p pname))
                  pname
                  (= (type pname) 'STR)
                  (vl-string-search "ДЛИНА" (strcase pname))
                )
              (progn
                (setq value
                  (vl-catch-all-apply
                    'vla-get-Value
                    (list prop)
                  )
                )
                (if (not (vl-catch-all-error-p value))
                  (setq result
                    (fasonka-value-to-number value)
                  )
                )
              )
            )
          )
        )
      )
      result
    )
  )
)

;; ------------------------------------------------------------
;; Проверка слоя
;; ------------------------------------------------------------

(defun fasonka-layer-selected-p (layer layers / x)
  (if (null layers)
    T
    (progn
      (setq x nil)
      (foreach name layers
        (if (= (strcase name) (strcase layer))
          (setq x T)
        )
      )
      x
    )
  )
)

;; ------------------------------------------------------------
;; Выбор INSERT (с поддержкой предварительного выделения)
;; ------------------------------------------------------------

(defun fasonka-select-inserts (layers / ss ss-first i ent data layer out)
  (setq out '())

  ;; Проверяем предварительный выбор
  (setq ss-first (ssgetfirst))
  (if (and ss-first (setq ss (car ss-first)))
    ;; Если есть выбранные объекты, используем их
    (setq ss ss)
    ;; Иначе выбираем все INSERT
    (setq ss (ssget "_X" '((0 . "INSERT"))))
  )

  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq data (entget ent))
        ;; Обрабатываем только вхождения блоков
        (if (= (cdr (assoc 0 data)) "INSERT")
          (progn
            (setq layer (cdr (assoc 8 data)))
            (if (fasonka-layer-selected-p layer layers)
              (setq out (cons ent out))
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )

  (reverse out)
)

;; ------------------------------------------------------------
;; DETAIL aggregation
;; ------------------------------------------------------------

(defun fasonka-aggregate-detail (records / acc rec name len found old)
  (setq acc '())

  (foreach rec records
    (setq name (nth 0 rec)
          len  (nth 1 rec)
          found nil)

    (foreach old acc
      (if (and
            (= (nth 0 old) name)
            (= (nth 1 old) len)
          )
        (setq found old)
      )
    )

    (if found
      (setq acc
        (subst
          (list
            name
            len
            (1+ (nth 2 found))
            (* len (1+ (nth 2 found)) 0.001)
          )
          found
          acc
        )
      )
      (setq acc
        (cons
          (list name len 1 (* len 0.001))
          acc
        )
      )
    )
  )

  (vl-sort
    acc
    '(lambda (a b)
       (if (= (strcase (nth 0 a))
              (strcase (nth 0 b)))
         (< (nth 1 a) (nth 1 b))
         (< (strcase (nth 0 a))
            (strcase (nth 0 b)))
       )
     )
  )
)

;; ------------------------------------------------------------
;; SUMMARY aggregation
;; ------------------------------------------------------------

(defun fasonka-aggregate-summary (detail / acc rec name found old)
  (setq acc '())

  (foreach rec detail
    (setq name (nth 0 rec)
          found nil)

    (foreach old acc
      (if (= (strcase (nth 0 old))
             (strcase name))
        (setq found old)
      )
    )

    (if found
      (setq acc
        (subst
          (list
            (nth 0 found)
            (+ (nth 1 found) (nth 2 rec))
            (+ (nth 2 found) (nth 3 rec))
          )
          found
          acc
        )
      )
      (setq acc
        (cons
          (list name
                (nth 2 rec)
                (nth 3 rec))
          acc
        )
      )
    )
  )

  (vl-sort
    acc
    '(lambda (a b)
       (< (strcase (nth 0 a))
          (strcase (nth 0 b)))
     )
  )
)

;; ------------------------------------------------------------
;; TXT
;; ------------------------------------------------------------

(defun fasonka-write-txt (model report-mode path / f row rec name len cnt mp
                                  group-name group-total)
  (setq f (open path "w"))

  (if (null f)
    nil
    (progn
      (if (= report-mode "DETAIL")
        (progn
          (write-line
            "№;Тип фасонки;Длина, мм;Кол-во, шт.;Сумма, м.п."
            f
          )

          (setq row 0
                group-name nil
                group-total 0.0)

          (foreach rec model
            (setq name (nth 0 rec)
                  len  (nth 1 rec)
                  cnt  (nth 2 rec)
                  mp   (nth 3 rec))

            (if (and group-name
                     (/= (strcase group-name)
                         (strcase name)))
              (progn
                (write-line
                  (strcat
                    "      " group-name ";;;;"
                    (rtos group-total 2 2)
                  )
                  f
                )
                (setq group-total 0.0)
              )
            )

            (if (or (null group-name)
                    (/= (strcase group-name)
                        (strcase name)))
              (setq group-name name
                    row 0)
            )

            (setq row (1+ row)
                  group-total (+ group-total mp))

            (write-line
              (strcat
                (itoa row) ";"
                name ";"
                (rtos len 2 0) ";"
                (itoa cnt) ";"
                (rtos mp 2 2)
              )
              f
            )
          )

          (if group-name
            (write-line
              (strcat
                "      " group-name ";;;;"
                (rtos group-total 2 2)
              )
              f
            )
          )
        )

        (progn
          (write-line
            "Тип фасонки;Кол-во, шт.;Сумма, м.п."
            f
          )

          (foreach rec model
            (write-line
              (strcat
                (nth 0 rec) ";"
                (itoa (nth 1 rec)) ";"
                (rtos (nth 2 rec) 2 2)
              )
              f
            )
          )

          (setq group-total 0.0)
          (foreach rec model
            (setq group-total (+ group-total (nth 2 rec)))
          )

          (write-line
            (strcat
              "Итого;"
              (itoa
                (apply '+
                  (cons 0
                    (mapcar
                      '(lambda (x) (nth 1 x))
                      model
                    )
                  )
                )
              )
              ";"
              (rtos group-total 2 2)
            )
            f
          )
        )
      )

      (close f)
      path
    )
  )
)

;; ------------------------------------------------------------
;; CSV — fallback Excel
;; ------------------------------------------------------------

(defun fasonka-write-csv (model report-mode path / f row rec name len cnt mp
                                  group-name group-total total-count)
  (setq f (open path "w"))

  (if (null f)
    nil
    (progn
      (if (= report-mode "DETAIL")
        (progn
          (write-line
            "№;Тип фасонки;Длина, мм;Кол-во, шт.;Сумма, м.п."
            f
          )

          (setq row 0
                group-name nil
                group-total 0.0)

          (foreach rec model
            (setq name (nth 0 rec)
                  len  (nth 1 rec)
                  cnt  (nth 2 rec)
                  mp   (nth 3 rec))

            (if (and group-name
                     (/= (strcase group-name)
                         (strcase name)))
              (progn
                (write-line
                  (strcat
                    "      " group-name ";;;;"
                    (rtos group-total 2 2)
                  )
                  f
                )
                (setq group-total 0.0)
              )
            )

            (if (or (null group-name)
                    (/= (strcase group-name)
                        (strcase name)))
              (setq group-name name
                    row 0)
            )

            (setq row (1+ row)
                  group-total (+ group-total mp))

            (write-line
              (strcat
                (itoa row) ";"
                name ";"
                (rtos len 2 0) ";"
                (itoa cnt) ";"
                (rtos mp 2 2)
              )
              f
            )
          )

          (if group-name
            (write-line
              (strcat
                "      " group-name ";;;;"
                (rtos group-total 2 2)
              )
              f
            )
          )
        )

        (progn
          (write-line
            "Тип фасонки;Кол-во, шт.;Сумма, м.п."
            f
          )

          (foreach rec model
            (write-line
              (strcat
                (nth 0 rec) ";"
                (itoa (nth 1 rec)) ";"
                (rtos (nth 2 rec) 2 2)
              )
              f
            )
          )

          (setq total-count 0
                group-total 0.0)

          (foreach rec model
            (setq total-count (+ total-count (nth 1 rec))
                  group-total (+ group-total (nth 2 rec)))
          )

          (write-line
            (strcat
              "Итого;"
              (itoa total-count) ";"
              (rtos group-total 2 2)
            )
            f
          )
        )
      )

      (close f)
      path
    )
  )
)

;; ------------------------------------------------------------
;; Командная строка
;; ------------------------------------------------------------

(defun fasonka-ask-yes-no (prompt default / ans)
  (initget "Yes No")
  (setq ans
    (getkword
      (strcat
        "\n" prompt
        " [Да(Y)/Нет(N)] <"
        (if default "Y" "N")
        ">: "
      )
    )
  )
  (if ans
    (= ans "Yes")
    default
  )
)

(defun fasonka-ask-report-mode ( / ans)
  (setq ans
    (strcase
      (getstring T
        "\nРежим отчёта [Подробный(D)/Краткий(S)] <D>: "
      )
    )
  )
  (cond
    ((or (= ans "") (= ans "D") (= ans "ПОДРОБНЫЙ"))
     "DETAIL")
    (t
     "SUMMARY")
  )
)

(defun c:fasonka
       ( / layers-input layers report-mode export-excel export-txt
           create-table use-default save-base)

  (vl-load-com)

  (princ
    "\n--- ФАСОНКА ---"
  )

  (setq layers-input
    (getstring T
      "\nВведите слои через запятую (Enter — все слои): "
    )
  )

  (setq layers (tu-parse-comma-list layers-input))
  (if (null layers)
    (setq layers nil)
  )

  (setq report-mode (fasonka-ask-report-mode))

  (setq export-excel
    (fasonka-ask-yes-no
      "Экспорт в Excel?"
      nil
    )
  )

  (setq export-txt
    (fasonka-ask-yes-no
      "Экспорт в TXT?"
      nil
    )
  )

  (setq create-table
    (fasonka-ask-yes-no
      "Создать таблицу AutoCAD?"
      T
    )
  )

  (initget "Yes No")
  (setq use-default
    (getkword
      "\nИспользовать путь по умолчанию? [Да(Y)/Нет(N)] <Y>: "
    )
  )

  (setq use-default
    (if use-default
      (= use-default "Yes")
      T
    )
  )

  (if use-default
    (setq save-base nil)
    (setq save-base
      (tu-get-save-base 'FASONKA)
    )
  )

  (if (or use-default save-base)
    (fasonka-main
      layers
      report-mode
      export-excel
      export-txt
      create-table
      save-base
    )
    (princ "\nОперация отменена.")
  )

  (princ)
)

;; ------------------------------------------------------------
;; ОСНОВНАЯ ФУНКЦИЯ ЗАДАЧИ
;; ------------------------------------------------------------

(defun fasonka-main
       (layers report-mode export-excel export-txt create-table save-base
        / inserts records detail summary ent obj name len
          total-blocks detail-count type-count total-length
          error-count excel-file csv-file txt-file table-created
          save-root excel-result path)

  (vl-load-com)

  (if (or (null layers) (= layers '()))
    (setq layers nil)
    (setq layers
      (tu-list-unique-ci layers)
    )
  )

  (if (/= report-mode "SUMMARY")
    (setq report-mode "DETAIL")
  )

  (setq inserts (fasonka-select-inserts layers))

  (if (null inserts)
    (progn
      (princ "\nБлоки со свойством Длина не найдены.")
      (list
        :task 'FASONKA
        :success nil
        :found-blocks 0
        :detail-count 0
        :type-count 0
        :total-length 0.0
        :excel-file nil
        :csv-file nil
        :txt-file nil
        :table-created nil
        :error-count 0
      )
    )

    (progn
      (setq records '()
            total-blocks 0
            error-count 0)

      (foreach ent inserts
        (setq obj
          (vl-catch-all-apply
            'vlax-ename->vla-object
            (list ent)
          )
        )

        (if (vl-catch-all-error-p obj)
          (setq error-count (1+ error-count))
          (progn
            (setq name (fasonka-get-effective-name obj))
            (setq len  (fasonka-get-length obj))

            (if (and name
                     (numberp len)
                     (> len 0.0))
              (progn
                (setq records
                  (cons
                    (list
                      (fasonka-clean-block-name name)
                      len
                    )
                    records
                  )
                )
                (setq total-blocks (1+ total-blocks))
              )
              (setq error-count (1+ error-count))
            )
          )
        )
      )

      (if (null records)
        (progn
          (princ "\nБлоки со свойством Длина не найдены.")
          (list
            :task 'FASONKA
            :success nil
            :found-blocks 0
            :detail-count 0
            :type-count 0
            :total-length 0.0
            :excel-file nil
            :csv-file nil
            :txt-file nil
            :table-created nil
            :error-count error-count
          )
        )

        (progn
          (setq detail
            (fasonka-aggregate-detail records)
          )

          (setq summary
            (fasonka-aggregate-summary detail)
          )

          (setq detail-count (length detail))
          (setq type-count   (length summary))

          (setq total-length 0.0)
          (foreach rec detail
            (setq total-length
              (+ total-length (nth 3 rec))
            )
          )

          (setq excel-file nil
                csv-file nil
                txt-file nil)

          (if (null save-base)
            (setq save-root
              (tu-default-save-base 'FASONKA)
            )
            (setq save-root save-base)
          )

          (if export-excel
            (progn
              (if (tu-ensure-directory-exists-p
                    (vl-filename-directory save-root))
                (progn
                  (setq excel-result
                    (tu-excel-export-fasonka
                      (if (= report-mode "DETAIL")
                        detail
                        summary
                      )
                      report-mode
                      save-root
                    )
                  )

                  (if excel-result
                    (setq excel-file excel-result)
                    (progn
                      (setq csv-file
                        (fasonka-write-csv
                          (if (= report-mode "DETAIL")
                            detail
                            summary
                          )
                          report-mode
                          (strcat save-root ".csv")
                        )
                      )
                      (princ "\nЭкспорт в Excel недоступен. Создан CSV-файл.")
                    )
                  )
                )
                (princ "\nПапка для сохранения не существует. Excel/CSV не создан.")
              )
            )
          )

          (if export-txt
            (if (tu-ensure-directory-exists-p
                  (vl-filename-directory save-root))
              (setq txt-file
                (fasonka-write-txt
                  (if (= report-mode "DETAIL")
                    detail
                    summary
                  )
                  report-mode
                  (strcat save-root ".txt")
                )
              )
              (princ "\nПапка для сохранения не существует. TXT не создан.")
            )
          )

          (setq table-created nil)

          (if create-table
            (setq table-created
              (tu-table-create
                report-mode
                (if (= report-mode "DETAIL")
                  detail
                  summary
                )
              )
            )
          )

          (if (= report-mode "DETAIL")
            (princ
              (strcat
                "\nФасонка: найдено блоков "
                (itoa total-blocks)
                ", позиций "
                (itoa detail-count)
                ", общий погонаж "
                (rtos total-length 2 2)
                " м.п."
              )
            )
            (princ
              (strcat
                "\nФасонка: найдено блоков "
                (itoa total-blocks)
                ", типов "
                (itoa type-count)
                ", общий погонаж "
                (rtos total-length 2 2)
                " м.п."
              )
            )
          )

          (if (> error-count 0)
            (princ
              (strcat
                "\nПропущено/ошибок: "
                (itoa error-count)
                "."
              )
            )
          )

          (list
            :task 'FASONKA
            :success T
            :found-blocks total-blocks
            :detail-count detail-count
            :type-count type-count
            :total-length total-length
            :excel-file excel-file
            :csv-file csv-file
            :txt-file txt-file
            :table-created table-created
            :error-count error-count
          )
        )
      )
    )
  )
)

(princ "\nFASONKA.LSP загружен. Команда: FASONKA")
(princ)