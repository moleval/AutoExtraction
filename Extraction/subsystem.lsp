;;; ============================================================
;;;  SUBSYSTEM.LSP
;;;  Подсчёт элементов подсистемы по состоянию видимости
;;;  Мерные блоки (с ДЛИНА) ? погонаж
;;;  Штучные блоки (без ДЛИНА) ? только количество
;;;  DETAIL: сначала штучные, затем мерные с подитогами по группам
;;;  SUMMARY: сводная таблица по наименованиям
;;; ============================================================

(vl-load-com)

;; ------------------------------------------------------------
;; Агрегация данных Подсистемы
;; Возвращает список групп: (наименование длина-или-nil количество)
;; Сортировка: сначала штучные (nil), затем мерные, по имени и длине
;; ------------------------------------------------------------
(defun subsystem-aggregate (inserts / i ent obj effname vis len name rec found acc)
  (setq acc '())
  (setq i 0)
  (repeat (length inserts)
    (setq ent (nth i inserts))
    (setq obj (vlax-ename->vla-object ent))
    (setq effname (su-get-effective-name obj))
    (setq vis (su-get-visibility obj))
    (setq len (su-get-length obj))

    ;; Наименование: видимость или EffectiveName (без очистки)
    (setq name
      (if (and vis (/= vis ""))
        vis
        (if effname
          effname
          "Без имени"
        )
      )
    )

    (setq found nil)
    (foreach rec acc
      (if (and (= (car rec) name)
               (equal (cadr rec) len))
        (setq found rec)
      )
    )

    (if found
      (setq acc (subst (list name len (1+ (caddr found))) found acc))
      (setq acc (cons (list name len 1) acc))
    )
    (setq i (1+ i))
  )

  (setq acc
    (vl-sort acc
      '(lambda (a b)
         (cond
           ((and (null (cadr a)) (null (cadr b)))
            (< (car a) (car b)))
           ((null (cadr a)) T)
           ((null (cadr b)) nil)
           (t
            (if (= (car a) (car b))
              (< (cadr a) (cadr b))
              (< (car a) (car b))
            )
           )
         )
      )
    )
  )
  acc
)

;; ------------------------------------------------------------
;; Создание таблицы AutoCAD для DETAIL
;; ------------------------------------------------------------
(defun subsystem-create-table-detail (data / pt tbl row nRows nCols space
                                        name len cnt sum totalSum groupIndex itemNum
                                        groups groupName groupRows)
  (setq pt (getpoint "\nУкажите точку вставки таблицы: "))
  (if pt
    (progn
      (setvar "CMDECHO" 0)
      (setq nCols 5)
      (setq nRows 2)
      (foreach rec data
        (if (null (cadr rec))
          (setq nRows (1+ nRows))
        )
      )
      (setq groups '())
      (foreach rec data
        (if (cadr rec)
          (if (not (assoc (car rec) groups))
            (setq groups (cons (list (car rec)) groups))
          )
        )
      )
      (foreach grp groups
        (setq nRows (+ nRows (length (vl-remove-if-not '(lambda (x) (and (= (car x) (car grp)) (cadr x))) data)) 1))
      )
      (setq space (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object))))
      (setq tbl (vla-addtable space (vlax-3d-point pt) nRows nCols 10.0 50.0))

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

      (setq row 2
            itemNum 0)

      ;; Штучные
      (foreach rec data
        (setq name (car rec)
              len  (cadr rec)
              cnt  (caddr rec))
        (if (null len)
          (progn
            (setq itemNum (1+ itemNum))
            (vla-SetText tbl row 0 (itoa itemNum))
            (vla-SetText tbl row 1 name)
            (vla-SetText tbl row 2 "")
            (vla-SetText tbl row 3 (itoa cnt))
            (vla-SetText tbl row 4 "")
            (vla-SetCellAlignment tbl row 0 5)
            (vla-SetCellAlignment tbl row 1 4)
            (vla-SetCellAlignment tbl row 2 5)
            (vla-SetCellAlignment tbl row 3 5)
            (vla-SetCellAlignment tbl row 4 5)
            (setq row (1+ row))
          )
        )
      )

      ;; Мерные
      (setq groups (vl-sort groups '(lambda (a b) (< (car a) (car b)))))
      (foreach grp groups
        (setq groupName (car grp)
              groupRows '()
              totalSum 0.0)
        (foreach rec data
          (if (and (= (car rec) groupName) (cadr rec))
            (setq groupRows (append groupRows (list rec)))
          )
        )
        (foreach rec groupRows
          (setq len (cadr rec)
                cnt (caddr rec)
                sum (/ (* len cnt) 1000.0)
                totalSum (+ totalSum sum))
          (setq itemNum (1+ itemNum))
          (vla-SetText tbl row 0 (itoa itemNum))
          (vla-SetText tbl row 1 (strcat " " groupName))
          (vla-SetText tbl row 2 (rtos len 2 0))
          (vla-SetText tbl row 3 (itoa cnt))
          (vla-SetText tbl row 4 (rtos sum 2 2))
          (vla-SetCellAlignment tbl row 0 5)
          (vla-SetCellAlignment tbl row 1 4)
          (vla-SetCellAlignment tbl row 2 5)
          (vla-SetCellAlignment tbl row 3 5)
          (vla-SetCellAlignment tbl row 4 5)
          (setq row (1+ row))
        )
        (vla-MergeCells tbl row row 1 3)
        (vla-SetText tbl row 0 "")
        (vla-SetText tbl row 1 (strcat "   " "{\\L" groupName "}"))
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
;; Создание таблицы AutoCAD для SUMMARY
;; ------------------------------------------------------------
(defun subsystem-create-table-summary (data / pt tbl row nRows nCols space
                                        name len cnt sum totalCount totalLen
                                        summaryList rec found)
  (setq pt (getpoint "\nУкажите точку вставки таблицы: "))
  (if pt
    (progn
      (setvar "CMDECHO" 0)
      (setq summaryList '())
      (foreach rec data
        (setq name (car rec)
              len  (cadr rec)
              cnt  (caddr rec)
              sum  (if len (/ (* len cnt) 1000.0) nil))
        (setq found nil)
        (foreach s summaryList
          (if (= (car s) name)
            (setq found s)
          )
        )
        (if found
          (progn
            (setq summaryList
              (subst
                (list name
                      (+ (cadr found) cnt)
                      (if (caddr found)
                        (+ (caddr found) (if sum sum 0))
                        (if sum sum nil)
                      )
                )
                found
                summaryList
              )
            )
          )
          (setq summaryList (cons (list name cnt sum) summaryList))
        )
      )
      (setq summaryList (reverse summaryList))

      (setq nCols 4)
      (setq nRows (+ 2 (length summaryList)))
      (setq space (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object))))
      (setq tbl (vla-addtable space (vlax-3d-point pt) nRows nCols 10.0 50.0))

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
      (foreach s summaryList
        (setq name (car s)
              cnt  (cadr s)
              sum  (caddr s))
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
;; ОСНОВНАЯ ФУНКЦИЯ
;; ------------------------------------------------------------
(defun subsystem-main (layers report-mode export-excel export-txt create-table save-base
                       / *error* inserts data csvfile xlsfile base-name
                         total-count total-length rec summary-data)
  (vl-load-com)

  (sssetfirst nil nil)

  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*QUIT*,*EXIT*")))
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
            (setq base-name (strcat (getvar "dwgprefix")
                                    (vl-filename-base (getvar "dwgname"))
                                    " Подсистема"))
            (setq base-name save-base)
          )

          ;; Экспорт в Excel/CSV
          (if export-excel
            (progn
              (setq xlsfile (strcat base-name ".xls"))
              (if (= report-mode "DETAIL")
                (if (eu-export-subsystem-detail data xlsfile)
                  (princ (strcat "\nXLS сохранён: " xlsfile))
                  (progn
                    (princ "\nНе удалось сохранить XLS. Сохраняю CSV...")
                    (setq csvfile (strcat base-name ".csv"))
                    (if (eu-export-subsystem-csv-detail data csvfile)
                      (princ (strcat "\nCSV сохранён: " csvfile))
                      (princ "\nНе удалось открыть CSV-файл.")
                    )
                  )
                )
                ;; SUMMARY: сначала агрегируем по имени
                (progn
                  (setq summary-data '())
                  (foreach rec data
                    (setq name (car rec)
                          len  (cadr rec)
                          cnt  (caddr rec)
                          sum  (if len (/ (* len cnt) 1000.0) nil))
                    (setq found nil)
                    (foreach s summary-data
                      (if (= (car s) name)
                        (setq found s)
                      )
                    )
                    (if found
                      (progn
                        (setq summary-data
                          (subst
                            (list name
                                  (+ (cadr found) cnt)
                                  (if (caddr found)
                                    (+ (caddr found) (if sum sum 0))
                                    (if sum sum nil)
                                  )
                            )
                            found
                            summary-data
                          )
                        )
                      )
                      (setq summary-data (cons (list name cnt sum) summary-data))
                    )
                  )
                  (setq summary-data (reverse summary-data))

                  (if (eu-export-subsystem-summary summary-data xlsfile)
                    (princ (strcat "\nXLS сохранён: " xlsfile))
                    (progn
                      (princ "\nНе удалось сохранить XLS. Сохраняю CSV...")
                      (setq csvfile (strcat base-name ".csv"))
                      (if (eu-export-subsystem-csv-summary summary-data csvfile)
                        (princ (strcat "\nCSV сохранён: " csvfile))
                        (princ "\nНе удалось открыть CSV-файл.")
                      )
                    )
                  )
                )
              )
            )
          )

          ;; Экспорт TXT (пока не реализован для Подсистемы, но параметр принимаем)
          (if export-txt
            (princ "\nЭкспорт в TXT для Подсистемы пока не реализован.")
          )

          ;; Таблица AutoCAD
          (if create-table
            (if (= report-mode "DETAIL")
              (subsystem-create-table-detail data)
              (subsystem-create-table-summary data)
            )
          )

          ;; Сводка
          (setq total-count 0
                total-length 0.0)
          (foreach rec data
            (setq total-count (+ total-count (caddr rec)))
            (if (cadr rec)
              (setq total-length (+ total-length (/ (* (cadr rec) (caddr rec)) 1000.0)))
            )
          )
          (princ (strcat "\nПодсистема: элементов " (itoa total-count)
                         ", общая длина " (rtos total-length 2 2) " м.п."))
        )
        (princ "\nНет данных для отчёта.")
      )
    )
    (princ "\nБлоки подсистемы не найдены.")
  )
  (princ)
)

;; ------------------------------------------------------------
;; Интерактивная команда (автономный запуск)
;; ------------------------------------------------------------
(defun c:subsystem ( / layers report-mode export-excel export-txt create-table use-default save-base)
  (if (not (type su-get-visibility))
    (progn
      (princ "\nСначала выполните RELOAD для загрузки общих модулей.")
      (princ)
      (exit)
    )
  )

  (setq layers '("Подсистема" "Подсистема оцинкованная" "Подсистема алюминиевая"))

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

(princ "\nSUBSYSTEM.LSP загружен. Команда: SUBSYSTEM")
(princ)