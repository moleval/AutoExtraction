;;; ============================================================
;;; SUBSYSTEM.LSP
;;; Извлечение данных о подсистеме
;;; Мерные блоки (с ДЛИНА) — погонный метраж
;;; Штучные блоки (без ДЛИНА) — только количество
;;; DETAIL / SUMMARY
;;; ============================================================

(vl-load-com)

;; ------------------------------------------------------------
;; Формирование имени элемента
;; ------------------------------------------------------------
;; Если есть свойство Видимость1 / VISIBILITY / ВИДИМОСТЬ —
;; используем его значение.
;; Если свойства видимости нет — сохраняем ПОЛНЫЙ EffectiveName.
;; Никакой очистки префикса здесь нет.
;; ------------------------------------------------------------

(defun subsystem-element-name (obj / effname vis)
  (setq effname (su-get-effective-name obj))
  (setq vis (su-get-visibility obj))

  (if (and vis (/= vis ""))
    vis
    (if effname
      effname
      "Без имени"
    )
  )
)

;; ------------------------------------------------------------
;; Ключи сортировки.
;;
;; Правило:
;;   1. Штучные элементы идут раньше мерных.
;;   2. Внутри каждой группы элементы сортируются по алфавиту.
;;   3. "Несущий" и "опорный" с общей основой считаются парой:
;;      несущий всегда перед опорным.
;;
;; Для обозначений КР-Н / КР-О дополнительно используется первая
;; размерная величина после КР-..., чтобы, например,
;; КР-Н-100/95/80 и КР-О-100/60/60 попали в одну пару.
;; ------------------------------------------------------------

(defun subsystem-sort-name (name / s pos prefix)
  (setq s (strcase (if name name "")))

  ;; Убираем слова типа элемента из ключа.
  (setq s (vl-string-subst " " "НЕСУЩИЙ" s))
  (setq s (vl-string-subst " " "ОПОРНЫЙ" s))
  (setq s (vl-string-subst " " "НЕСУЩАЯ" s))
  (setq s (vl-string-subst " " "ОПОРНАЯ" s))

  ;; КР-Н и КР-О являются двумя исполнениями одной группы.
  ;; Для пары берём общий префикс + первую размерную величину.
  (cond
    ((setq pos (vl-string-search "КР-Н-" s))
      (setq prefix (substr s 1 pos))
      (setq s
        (strcat
          prefix
          "КР-"
          (subsystem-first-number
            (substr s (+ pos 5))
          )
        )
      )
    )

    ((setq pos (vl-string-search "КР-О-" s))
      (setq prefix (substr s 1 pos))
      (setq s
        (strcat
          prefix
          "КР-"
          (subsystem-first-number
            (substr s (+ pos 5))
          )
        )
      )
    )
  )

  ;; Нормализуем остаточные пробелы.
  s
)


(defun subsystem-first-number (s / i ch result)
  (setq
    i 1
    result ""
  )

  ;; Ищем первую последовательность цифр.
  (while
    (and
      (<= i (strlen s))
      (= result "")
    )

    (setq ch (substr s i 1))

    (if
      (and
        (>= (ascii ch) 48)
        (<= (ascii ch) 57)
      )
      (setq result ch)
    )

    (setq i (1+ i))
  )

  ;; Собираем остальные цифры первой размерной величины.
  (while
    (and
      (< i (1+ (strlen s)))
      (>= (ascii (substr s i 1)) 48)
      (<= (ascii (substr s i 1)) 57)
    )

    (setq
      result
      (strcat
        result
        (substr s i 1)
      )
      i (1+ i)
    )
  )

  (if (= result "")
    "0"
    result
  )
)


(defun subsystem-sort-rank (name / s)
  (setq s (strcase (if name name "")))

  (cond
    ((or
       (vl-string-search "НЕСУЩИЙ" s)
       (vl-string-search "НЕСУЩАЯ" s)
       (vl-string-search "КР-Н-" s)
     )
     0
    )

    ((or
       (vl-string-search "ОПОРНЫЙ" s)
       (vl-string-search "ОПОРНАЯ" s)
       (vl-string-search "КР-О-" s)
     )
     1
    )

    (T 2)
  )
)


(defun subsystem-sort-less (a b / na nb ka kb ra rb ca cb)
  ;; A/B могут быть:
  ;;   (name length count)
  ;; или
  ;;   (name count sum)
  ;; Поэтому признак "штучный/мерный" передаётся отдельно
  ;; через наличие второго поля в вызывающем коде.

  (setq na (car a))
  (setq nb (car b))

  (setq ka (subsystem-sort-name na))
  (setq kb (subsystem-sort-name nb))

  (setq ra (subsystem-sort-rank na))
  (setq rb (subsystem-sort-rank nb))

  (cond
    ((< ka kb) T)
    ((> ka kb) nil)

    ((< ra rb) T)
    ((> ra rb) nil)

    ;; При одинаковой паре сохраняем детерминированный
    ;; алфавитный порядок.
    ((< (strcase na) (strcase nb)) T)
    ((> (strcase na) (strcase nb)) nil)

    (T nil)
  )
)


(defun subsystem-detail-less (a b / la lb)
  (setq la (cadr a))
  (setq lb (cadr b))

  (cond
    ;; Штучные всегда раньше мерных.
    ((and (null la) (null lb))
      (subsystem-sort-less a b)
    )

    ((null la)
      T
    )

    ((null lb)
      nil
    )

    ;; Для мерных сначала объединяем пары, затем сортируем
    ;; одинаковое имя по длине.
    ((subsystem-sort-less a b)
      T
    )

    ((subsystem-sort-less b a)
      nil
    )

    ((/= (cadr a) (cadr b))
      (< (cadr a) (cadr b))
    )

    (T nil)
  )
)


(defun subsystem-summary-less (a b / sa sb)
  ;; SUMMARY: сумма nil означает штучный элемент.
  (setq sa (caddr a))
  (setq sb (caddr b))

  (cond
    ((and (null sa) (null sb))
      (subsystem-sort-less a b)
    )

    ((null sa)
      T
    )

    ((null sb)
      nil
    )

    (T
      (subsystem-sort-less a b)
    )
  )
)



(defun subsystem-aggregate
       (inserts / i ent obj name len rec found acc)

  (setq acc '())
  (setq i 0)

  (repeat (length inserts)

    (setq ent (nth i inserts))
    (setq obj (vlax-ename->vla-object ent))

    (setq name (subsystem-element-name obj))
    (setq len  (su-get-length obj))

    ;; Один и тот же элемент с одинаковой длиной объединяется.
    ;; Штучный элемент (nil) группируется отдельно.
    (setq found nil)

    (foreach rec acc
      (if
        (and
          (= (car rec) name)
          (equal (cadr rec) len)
        )
        (setq found rec)
      )
    )

    (if found
      (setq
        acc
        (subst
          (list name len (1+ (caddr found)))
          found
          acc
        )
      )
      (setq
        acc
        (cons (list name len 1) acc)
      )
    )

    (setq i (1+ i))
  )

  ;; Штучные — сначала, затем мерные.
  ;; Внутри групп "несущий / опорный" идут парой.
  (setq
    acc
    (vl-sort
      acc
      'subsystem-detail-less
    )
  )

  acc
)

;; ------------------------------------------------------------
;; SUMMARY из DETAIL-данных
;; Запись: (наименование количество сумма-или-nil)
;; ------------------------------------------------------------

(defun subsystem-build-summary
       (data / summary-data rec name len cnt sum found)

  (setq summary-data '())

  (foreach rec data

    (setq
      name (car rec)
      len  (cadr rec)
      cnt  (caddr rec)
      sum  (if len
             (/ (* len cnt) 1000.0)
             nil
           )
      found nil
    )

    (foreach rec2 summary-data
      (if (= (car rec2) name)
        (setq found rec2)
      )
    )

    (if found

      (setq
        summary-data
        (subst
          (list
            name
            (+ (cadr found) cnt)
            (if (caddr found)
              (+ (caddr found) (if sum sum 0.0))
              sum
            )
          )
          found
          summary-data
        )
      )

      (setq
        summary-data
        (cons
          (list name cnt sum)
          summary-data
        )
      )
    )
  )

  (setq
    summary-data
    (vl-sort
      summary-data
      'subsystem-summary-less
    )
  )

  summary-data
)

;; ------------------------------------------------------------
;; DETAIL -> формат, необходимый tx-export-gal
;;
;; GAL предназначен для мерных позиций. Штучные позиции
;; без длины в GAL не добавляются, чтобы не передавать nil
;; в rtos внутри общего GAL-экспортера.
;; ------------------------------------------------------------

(defun subsystem-build-gal-detail
       (data / result group-index groups rec name len)

  (setq result '())
  (setq groups '())

  (foreach rec data
    (if (cadr rec)
      (if (not (assoc (car rec) groups))
        (setq
          groups
          (cons (list (car rec)) groups)
        )
      )
    )
  )

  (setq
    groups
    (vl-sort
      groups
      '(lambda (a b)
         (subsystem-sort-less a b)
       )
    )
  )

  (setq group-index 0)

  (foreach grp groups

    (setq
      group-index (1+ group-index)
      name (car grp)
      recs '()
    )

    (foreach rec data
      (if
        (and
          (= (car rec) name)
          (cadr rec)
        )
        (setq
          recs
          (append
            recs
            (list rec)
          )
        )
      )
    )

    (setq
      result
      (append
        result
        (list
          (list
            group-index
            name
            recs
          )
        )
      )
    )
  )

  result
)

;; ------------------------------------------------------------
;; AutoCAD DETAIL
;; ------------------------------------------------------------

(defun subsystem-create-table-detail
       (data / pt tbl row nRows nCols space
             name len cnt sum totalSum
             groupIndex itemNum groups groupName groupRows)

  (setq
    pt
    (getpoint
      "\nУкажите точку вставки таблицы: "
    )
  )

  (if pt

    (progn

      (setvar "CMDECHO" 0)

      (setq nCols 5)
      (setq nRows 2)

      ;; Штучные строки.
      (foreach rec data
        (if (null (cadr rec))
          (setq nRows (1+ nRows))
        )
      )

      ;; Группы мерных элементов.
      (setq groups '())

      (foreach rec data
        (if (cadr rec)
          (if (not (assoc (car rec) groups))
            (setq
              groups
              (cons (list (car rec)) groups)
            )
          )
        )
      )

      (foreach grp groups
        (setq
          nRows
          (+
            nRows
            (length
              (vl-remove-if-not
                '(lambda (x)
                   (and
                     (= (car x) (car grp))
                     (cadr x)
                   )
                )
                data
              )
            )
            1
          )
        )
      )

      (setq
        space
        (vla-get-modelspace
          (vla-get-activedocument
            (vlax-get-acad-object)
          )
        )
      )

      (setq
        tbl
        (vla-addtable
          space
          (vlax-3d-point pt)
          nRows
          nCols
          10.0
          50.0
        )
      )

      (vla-SetColumnWidth tbl 0 15.0)
      (vla-SetColumnWidth tbl 1 150.0)
      (vla-SetColumnWidth tbl 2 25.0)
      (vla-SetColumnWidth tbl 3 25.0)
      (vla-SetColumnWidth tbl 4 30.0)

      (vla-MergeCells tbl 0 0 0 4)
      (vla-SetText tbl 0 0 "{\\LПодсистема}")

      (vla-SetText tbl 1 0 "№")
      (vla-SetText tbl 1 1 "Наименование")
      (vla-SetText tbl 1 2 "Длина, мм")
      (vla-SetText tbl 1 3 "Кол-во, шт.")
      (vla-SetText tbl 1 4 "Сумма, м.п.")

      (vla-SetCellAlignment tbl 1 0 5)
      (vla-SetCellAlignment tbl 1 1 5)
      (vla-SetCellAlignment tbl 1 2 5)
      (vla-SetCellAlignment tbl 1 3 5)
      (vla-SetCellAlignment tbl 1 4 5)

      (setq
        row 2
        itemNum 0
      )

      ;; --------------------------------------------------------
      ;; ШТУЧНЫЕ
      ;; --------------------------------------------------------

      (foreach rec data

        (setq
          name (car rec)
          len  (cadr rec)
          cnt  (caddr rec)
        )

        (if (null len)

          (progn

            (setq itemNum (1+ itemNum))

            (vla-SetText tbl row 0 (itoa itemNum))
            (vla-SetText tbl row 1 name)
            (vla-SetText tbl row 2 "")
            (vla-SetText tbl row 3 (itoa cnt))
            (vla-SetText tbl row 4 "")

            ;; Наименование — ВЛЕВО.
            (vla-SetCellAlignment tbl row 0 5)
            (vla-SetCellAlignment tbl row 1 4)
            (vla-SetCellAlignment tbl row 2 5)
            (vla-SetCellAlignment tbl row 3 5)
            (vla-SetCellAlignment tbl row 4 5)

            (setq row (1+ row))
          )
        )
      )

      ;; --------------------------------------------------------
      ;; МЕРНЫЕ
      ;; --------------------------------------------------------

      (setq
        groups
        (vl-sort
          groups
          '(lambda (a b)
             (subsystem-sort-less a b)
           )
        )
      )

      (foreach grp groups

        (setq
          groupName (car grp)
          groupRows '()
          totalSum 0.0
        )

        (foreach rec data
          (if
            (and
              (= (car rec) groupName)
              (cadr rec)
            )
            (setq
              groupRows
              (append groupRows (list rec))
            )
          )
        )

        (foreach rec groupRows

          (setq
            len (cadr rec)
            cnt (caddr rec)
            sum (/ (* len cnt) 1000.0)
            totalSum (+ totalSum sum)
          )

          (setq itemNum (1+ itemNum))

          (vla-SetText tbl row 0 (itoa itemNum))
          (vla-SetText tbl row 1 groupName)
          (vla-SetText tbl row 2 (rtos len 2 0))
          (vla-SetText tbl row 3 (itoa cnt))
          (vla-SetText tbl row 4 (rtos sum 2 2))

          ;; Наименование — ВЛЕВО.
          (vla-SetCellAlignment tbl row 0 5)
          (vla-SetCellAlignment tbl row 1 4)
          (vla-SetCellAlignment tbl row 2 5)
          (vla-SetCellAlignment tbl row 3 5)
          (vla-SetCellAlignment tbl row 4 5)

          (setq row (1+ row))
        )

        ;; Подитог группы.
        (vla-MergeCells tbl row row 1 3)

        (vla-SetText tbl row 0 "")
        (vla-SetText
          tbl row 1
          (strcat "{\\L" groupName "}")
        )
        (vla-SetCellAlignment tbl row 1 4)

        (vla-SetText tbl row 4 (rtos totalSum 2 2))
        (vla-SetCellAlignment tbl row 4 5)

        (setq row (1+ row))
      )

      (vla-update tbl)
      (setvar "CMDECHO" 1)

      tbl
    )
  )
)

;; ------------------------------------------------------------
;; AutoCAD SUMMARY
;; ------------------------------------------------------------

(defun subsystem-create-table-summary
       (data / pt tbl row nRows nCols space
             name cnt sum summaryList rec found)

  (setq
    pt
    (getpoint
      "\nУкажите точку вставки таблицы: "
    )
  )

  (if pt

    (progn

      (setvar "CMDECHO" 0)

      (setq summaryList (subsystem-build-summary data))

      (setq nCols 4)
      (setq nRows (+ 2 (length summaryList)))

      (setq
        space
        (vla-get-modelspace
          (vla-get-activedocument
            (vlax-get-acad-object)
          )
        )
      )

      (setq
        tbl
        (vla-addtable
          space
          (vlax-3d-point pt)
          nRows
          nCols
          10.0
          50.0
        )
      )

      (vla-SetColumnWidth tbl 0 15.0)
      (vla-SetColumnWidth tbl 1 150.0)
      (vla-SetColumnWidth tbl 2 25.0)
      (vla-SetColumnWidth tbl 3 30.0)

      (vla-MergeCells tbl 0 0 0 3)
      (vla-SetText tbl 0 0 "{\\LПодсистема}")

      (vla-SetText tbl 1 0 "№")
      (vla-SetText tbl 1 1 "Наименование")
      (vla-SetText tbl 1 2 "Кол-во, шт.")
      (vla-SetText tbl 1 3 "Сумма, м.п.")

      (vla-SetCellAlignment tbl 1 0 5)
      (vla-SetCellAlignment tbl 1 1 5)
      (vla-SetCellAlignment tbl 1 2 5)
      (vla-SetCellAlignment tbl 1 3 5)

      (setq row 2)

      (foreach rec summaryList

        (setq
          name (car rec)
          cnt  (cadr rec)
          sum  (caddr rec)
        )

        (vla-SetText tbl row 0 (itoa (1+ (- row 2))))
        (vla-SetText tbl row 1 name)
        (vla-SetText tbl row 2 (itoa cnt))

        (if sum
          (vla-SetText tbl row 3 (rtos sum 2 2))
          (vla-SetText tbl row 3 "")
        )

        (vla-SetCellAlignment tbl row 0 5)
        (vla-SetCellAlignment tbl row 1 4)
        (vla-SetCellAlignment tbl row 2 5)
        (vla-SetCellAlignment tbl row 3 5)

        (setq row (1+ row))
      )

      (vla-update tbl)
      (setvar "CMDECHO" 1)

      tbl
    )
  )
)

;; ------------------------------------------------------------
;; Основная функция
;; ------------------------------------------------------------

(defun subsystem-main
       (layers report-mode export-excel export-txt create-table save-base
        / *error*
          inserts data
          csvfile xlsfile base-name
          total-count total-length
          rec summary-data
          gal-data)

  (vl-load-com)

  (sssetfirst nil nil)

  (defun *error* (msg)
    (if
      (and
        msg
        (not
          (wcmatch
            (strcase msg)
            "*BREAK*,*CANCEL*,*QUIT*,*EXIT*"
          )
        )
      )
      (princ (strcat "\nОшибка: " msg))
    )
    (princ)
  )

  (setq inserts (su-select-inserts layers))

  (if inserts

    (progn

      (setq data (subsystem-aggregate inserts))

      (if data

        (progn

          (if (null save-base)
            (setq
              base-name
              (strcat
                (getvar "dwgprefix")
                (vl-filename-base (getvar "dwgname"))
                " Подсистема "
                (if (= (strcase report-mode) "DETAIL") "подробный" "краткий")
              )
            )
            (setq
              base-name
              (strcat
                save-base
                " "
                (if (= (strcase report-mode) "DETAIL") "подробный" "краткий")
              )
            )
          )

          ;; ==================================================
          ;; XLS / CSV
          ;; ==================================================

          (if export-excel

            (progn

              (setq xlsfile (strcat base-name ".xls"))

              (if (= (strcase report-mode) "DETAIL")

                (if
                  (eu-export-subsystem-detail data xlsfile)

                  (princ
                    (strcat
                      "\nXLS сохранён: "
                      xlsfile
                    )
                  )

                  (progn
                    (princ
                      "\nНе удалось сохранить XLS. Сохраняю CSV..."
                    )

                    (setq
                      csvfile
                      (strcat base-name ".csv")
                    )

                    (if
                      (eu-export-subsystem-csv-detail
                        data
                        csvfile
                      )

                      (princ
                        (strcat
                          "\nCSV сохранён: "
                          csvfile
                        )
                      )

                      (princ
                        "\nНе удалось создать CSV."
                      )
                    )
                  )
                )

                ;; SUMMARY
                (progn

                  (setq
                    summary-data
                    (subsystem-build-summary data)
                  )

                  (if
                    (eu-export-subsystem-summary
                      summary-data
                      xlsfile
                    )

                    (princ
                      (strcat
                        "\nXLS сохранён: "
                        xlsfile
                      )
                    )

                    (progn

                      (princ
                        "\nНе удалось сохранить XLS. Сохраняю CSV..."
                      )

                      (setq
                        csvfile
                        (strcat base-name ".csv")
                      )

                      (if
                        (eu-export-subsystem-csv-summary
                          summary-data
                          csvfile
                        )

                        (princ
                          (strcat
                            "\nCSV сохранён: "
                            csvfile
                          )
                        )

                        (princ
                          "\nНе удалось создать CSV."
                        )
                      )
                    )
                  )
                )
              )
            )
          )

          ;; ==================================================
          ;; TXT / GAL
          ;; ==================================================

          (if export-txt

            (if (= (strcase report-mode) "DETAIL")

              (progn
                (setq gal-data
                  (subsystem-build-gal-detail data)
                )

                (if gal-data
                  (tx-export-gal
                    "DETAIL"
                    gal-data
                    base-name
                  )
                  (princ
                    "\nНет мерных позиций для GAL."
                  )
                )
              )

              (progn
                (setq summary-data
                  (subsystem-build-summary data)
                )

                (tx-export-gal
                  "SUMMARY"
                  summary-data
                  base-name
                )
              )
            )
          )

          ;; ==================================================
          ;; AutoCAD
          ;; ==================================================

          (if create-table

            (if (= (strcase report-mode) "DETAIL")
              (subsystem-create-table-detail data)
              (subsystem-create-table-summary data)
            )
          )

          ;; ==================================================
          ;; Сводка
          ;; ==================================================

          (setq
            total-count 0
            total-length 0.0
          )

          (foreach rec data

            (setq
              total-count
              (+ total-count (caddr rec))
            )

            (if (cadr rec)
              (setq
                total-length
                (+
                  total-length
                  (/ (* (cadr rec) (caddr rec)) 1000.0)
                )
              )
            )
          )

          (princ
            (strcat
              "\nПодсистема: элементов "
              (itoa total-count)
              ", общая длина "
              (rtos total-length 2 2)
              " м.п."
            )
          )
        )

        (princ "\nНет данных для отчёта.")
      )
    )

    (princ "\nБлоки подсистемы не найдены.")
  )

  (princ)
)

;; ------------------------------------------------------------
;; Автономная команда (модифицирована: запрос слоёв)
;; ------------------------------------------------------------

(defun c:subsystem ( / layers-str layers report-mode export-excel export-txt create-table use-default save-base)
  ;; Запрос слоёв
  (setq layers-str (getstring T "\nВведите слои через запятую (Enter — все слои): "))
  (if (= layers-str "")
    (setq layers nil)
    (setq layers (mapcar 'strcase (split-string layers-str ",")))
  )

  (initget "D S")
  (setq report-mode (getkword "\nРежим отчёта [Подробный(D)/Краткий(S)] <D>: "))
  (if (null report-mode) (setq report-mode "D"))
  (setq report-mode (if (= report-mode "D") "DETAIL" "SUMMARY"))

  (initget "Y N")
  (setq export-excel (getkword "\nЭкспорт в Excel? [Да(Y)/Нет(N)] <N>: "))
  (if (or (null export-excel) (= export-excel "N")) (setq export-excel nil) (setq export-excel T))

  (initget "Y N")
  (setq export-txt (getkword "\nЭкспорт в TXT (GAL)? [Да(Y)/Нет(N)] <N>: "))
  (if (or (null export-txt) (= export-txt "N")) (setq export-txt nil) (setq export-txt T))

  (initget "Y N")
  (setq create-table (getkword "\nСоздать таблицу AutoCAD? [Да(Y)/Нет(N)] <Y>: "))
  (if (or (null create-table) (= create-table "Y")) (setq create-table T) (setq create-table nil))

  (initget "Y N")
  (setq use-default (getkword "\nИспользовать путь по умолчанию? [Да(Y)/Нет(N)] <Y>: "))
  (if (or (null use-default) (= use-default "Y"))
    (setq save-base nil)
    (setq save-base (getstring "\nБазовое имя файла (без расширения): "))
  )

  (subsystem-main layers report-mode export-excel export-txt create-table save-base)
  (princ)
)

;; Русская команда
(defun c:ПОДСИСТЕМА ()
  (c:subsystem)
)

(princ
  "\nSUBSYSTEM.LSP загружен. Команды: SUBSYSTEM, ПОДСИСТЕМА"
)
(princ)