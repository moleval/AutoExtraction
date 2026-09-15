;;; ============================================================
;;;  FASONKA.LSP
;;;  Извлечение данных о фасонном железе
;;;  Поддержка режимов DETAIL и SUMMARY
;;;  Выборка и извлечение длины — в common/select-utils.lsp
;;;  Экспорт XLS/CSV — в common/excel-utils.lsp
;;;  GAL — в common/txt-utils.lsp
;;;  Таблицы AutoCAD — в common/table-utils.lsp
;;
;;;  ИСПРАВЛЕНИЕ (аудит Этап 3, пункт B2):
;;;  clean-name обрезает префикс ТОЛЬКО если имя начинается с него.
;;;  Если префикс не найден — имя возвращается без изменений.
;;; ============================================================

;; ============================================================
;; Кускование Фасонки — подготовка плоского списка
;; Вход: indexed-detail = список (groupIndex groupName groupRecs)
;; ============================================================

(defun fs-build-flat-items (indexed-detail / items grp gIdx gName gRecs
                             rec totalSum)
  (setq items '())
  (foreach grp indexed-detail
    (setq gIdx  (car grp))
    (setq gName (cadr grp))
    (setq gRecs (caddr grp))
    (setq totalSum 0.0)

    (foreach rec gRecs
      (setq items (append items
        (list (list 'data gIdx (car rec) (cadr rec) (caddr rec)))))
      (setq totalSum (+ totalSum (/ (* (cadr rec) (caddr rec)) 1000.0)))
    )
    (setq items (append items
      (list (list 'subtotal gIdx gName totalSum))))
  )
  items
)


(defun fs-build-units (indexed-detail idealRows / items chunks ch result)
  (setq items (fs-build-flat-items indexed-detail))
  (setq chunks (tc-partition-flat items idealRows))
  (setq result '())
  (foreach ch chunks
    (setq result (append result (list (cons (length ch) ch)))))
  result
)


;; ============================================================
;; ТАБЛИЦА AUTOCAD — DETAIL (кускованная)
;; ============================================================

(defun fasonka-create-table-detail (indexed-detail /
    pt pt_wcs units total-chunks chunk-idx is-last chunk items item
    nCols nRows space tbl row oldEcho doc
    lastGroupIdx rowInGroup maxNameLen nameStr
    gIdx gName len cnt sum totalSum)

  (if (null indexed-detail)
    (progn (princ "\nНет данных для таблицы Фасонки.") nil)
    (progn
      (setq pt (getpoint "\nУкажите точку вставки таблицы: "))
      (if (null pt)
        (progn (princ "\nТаблица пропущена.") nil)
        (progn
          (setq doc (vlax-get-acad-object))
          (setq doc (vla-get-activedocument doc))
          (setq space (vla-get-modelspace doc))
          (setq pt_wcs (trans pt 1 0))
          (setq oldEcho (getvar "CMDECHO"))
          (vl-catch-all-apply 'setvar (list "CMDECHO" 0))
          (vla-startundomark doc)

          ;; Максимальная длина имени для ширины колонки
          (setq maxNameLen 10)
          (foreach item indexed-detail
            (setq nameStr (cadr item))
            (if (> (strlen nameStr) maxNameLen)
              (setq maxNameLen (strlen nameStr))))

          (setq units (fs-build-units indexed-detail *TU-IDEAL-ROWS*))
          (setq total-chunks (length units))
          (setq chunk-idx 0 nCols 5)

          ;; Нумерация продолжается через куски
          (setq lastGroupIdx -1 rowInGroup 0)

          (foreach chunk units
            (setq is-last (tu-is-last-chunk chunk-idx total-chunks))
            (setq nRows (+ 2 (car chunk)))
            (setq items (cdr chunk))
            (setq tbl (vl-catch-all-apply 'vla-addtable
              (list space (vlax-3d-point pt_wcs) nRows nCols 10.0 50.0)))

            (if (vl-catch-all-error-p tbl)
              (princ (strcat "\nОшибка создания таблицы Фасонки: "
                             (vl-catch-all-error-message tbl)))
              (progn
                (vla-SetColumnWidth tbl 0 17.5)
                (vla-SetColumnWidth tbl 1 (* maxNameLen 3.0))
                (vla-SetColumnWidth tbl 2 25.0)
                (vla-SetColumnWidth tbl 3 25.0)
                (vla-SetColumnWidth tbl 4 30.0)

                (vla-MergeCells tbl 0 0 0 4)
                (vla-SetText tbl 0 0 "{\\LФасонное железо}")

                (vla-SetText tbl 1 0 "№")
                (vla-SetText tbl 1 1 "Тип фасонки")
                (vla-SetText tbl 1 2 "Длина, мм")
                (vla-SetText tbl 1 3 "Кол-во, шт.")
                (vla-SetText tbl 1 4 "Сумма, м.п.")

                (vla-SetCellAlignment tbl 1 0 5)
                (vla-SetCellAlignment tbl 1 1 5)
                (vla-SetCellAlignment tbl 1 2 5)
                (vla-SetCellAlignment tbl 1 3 5)
                (vla-SetCellAlignment tbl 1 4 5)

                (setq row 2)

                (foreach item items
                  (if (eq (car item) 'data)
                    ;; Строка данных
                    (progn
                      (setq gIdx  (cadr item))
                      (setq gName (caddr item))
                      (setq len   (cadddr item))
                      (setq cnt   (caddr (cddr item)))
                      (setq sum   (/ (* len cnt) 1000.0))

                      (if (/= gIdx lastGroupIdx)
                        (progn
                          (setq lastGroupIdx gIdx)
                          (setq rowInGroup 0)))
                      (setq rowInGroup (1+ rowInGroup))

                      (vla-SetText tbl row 0
                        (strcat (itoa gIdx) "." (itoa rowInGroup)))
                      (vla-SetText tbl row 1 (strcat " " gName))
                      (vla-SetText tbl row 2 (rtos len 2 0))
                      (vla-SetText tbl row 3 (itoa cnt))
                      (vla-SetText tbl row 4 (rtos sum 2 2))

                      (vla-SetCellAlignment tbl row 0 5)
                      (vla-SetCellAlignment tbl row 1 4)
                      (vla-SetCellAlignment tbl row 2 5)
                      (vla-SetCellAlignment tbl row 3 5)
                      (vla-SetCellAlignment tbl row 4 5)

                      (setq row (1+ row)))

                    ;; Подитог группы
                    (progn
                      (setq gIdx     (cadr item))
                      (setq gName    (nth 2 item))
                      (setq totalSum (nth 3 item))

                      (vla-MergeCells tbl row row 1 3)
                      (vla-SetText tbl row 0
                        (strcat "{\\fArial|b1|i0|c0|p34;" (itoa gIdx) "}"))
                      (vla-SetCellAlignment tbl row 0 5)
                      (vla-SetText tbl row 1 (strcat "{\\L" gName "}"))
                      (vla-SetCellAlignment tbl row 1 4)
                      (vla-SetText tbl row 4 (rtos totalSum 2 2))
                      (vla-SetCellAlignment tbl row 4 5)

                      (setq row (1+ row)))))

                (vla-update tbl)
                (princ (strcat "\nТаблица Фасонки "
                               (itoa (1+ chunk-idx)) " создана."))
                (setq pt_wcs (tu-next-table-point pt_wcs nRows 10.0 20.0))))

            (setq chunk-idx (1+ chunk-idx)))

          (vla-endundomark doc)
          (vl-catch-all-apply 'setvar (list "CMDECHO" oldEcho))
          (princ (strcat "\nВсего создано таблиц Фасонки: "
                         (itoa total-chunks)))
          T)))))

(defun c:fasonka ( / layers-str layers report-mode export-excel export-txt create-table use-default save-base)
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

  (fasonka-main layers report-mode export-excel export-txt create-table save-base)
  (princ)
)

(defun c:Фасонка ()
  (c:fasonka)
)

;; ------------------------------------------------------------
;; ОСНОВНАЯ ФУНКЦИЯ
;; ------------------------------------------------------------
(defun fasonka-main (layers report-mode export-excel export-txt create-table save-base
                     / *error*
                     csvfile
                     val name len acc rec found
                     inserts i ent obj effname dynprops prop
                     layer-filter lay
                     detail-groups curName curRecs
                     indexed-detail groupIndex
                     summary-groups totalCount totalSum
                     report-data report-type
                     total-blocks total-pos total-types total-sum
                     base-name xlsfile)

  (vl-load-com)

  ;; Обработчик ошибок
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\nОшибка: " msg))
    )
    (princ)
  )

  ;; ============================================================
  ;; Очистка имени блока от префикса
  ;; ИСПРАВЛЕНО (аудит Этап 3, пункт B2):
  ;; Префикс обрезается ТОЛЬКО если имя начинается с него.
  ;; Если префикс не найден — имя возвращается без изменений.
  ;; ============================================================
  (defun clean-name (str / prefixes p result found first rest)
    ;; Список возможных префиксов
    ;; Каждый префикс должен включать завершающий символ (пробел, подчёркивание)
    (setq prefixes '("Железо " "Железо_" "ФАСОНКА_" "ФАСОНКА " "Фасонка_" "Фасонка "))
    (setq result str
          found nil)
    ;; Проверяем каждый префикс (без учёта регистра)
    (if (and str (> (strlen str) 0))
      (progn
        (foreach p prefixes
          (if (not found)
            (progn
              ;; Проверяем, начинается ли имя с префикса
              (if (and (>= (strlen str) (strlen p))
                       (= (strcase (substr str 1 (strlen p))) (strcase p)))
                (progn
                  ;; Обрезаем префикс
                  (setq result (substr str (1+ (strlen p))))
                  (setq found T)
                )
              )
            )
          )
        )
        ;; Делаем первую букву заглавной (только если префикс был обрезан)
        (if (and found (> (strlen result) 0))
          (progn
            (setq first (strcase (substr result 1 1))
                  rest  (substr result 2)
                  result (strcat first rest))
          )
        )
      )
    )
    result
  )

  ;; ==================== Выборка блоков ====================
  (setq inserts (su-select-inserts layers))

  (if inserts
    (progn
      (setq i 0 acc '())
      (repeat (length inserts)
        (setq ent (nth i inserts)
              obj (vlax-ename->vla-object ent)
              effname (su-get-effective-name obj)
              len  (su-get-length obj))

        (if (and effname (numberp len) (> len 0.0))
          (progn
            (setq name (clean-name effname)
                  found nil)
            (foreach rec acc
              (if (and (= (car rec) name) (= (cadr rec) len))
                (setq found rec)
              )
            )
            (if found
              (setq acc (subst (list name len (1+ (caddr found))) found acc))
              (setq acc (cons (list name len 1) acc))
            )
          )
        )
        (setq i (1+ i))
      )

      (if acc
        (progn
          ;; Сортировка
          (setq acc (vl-sort acc '(lambda (a b) (if (= (car a) (car b)) (< (cadr a) (cadr b)) (< (car a) (car b))))))

          ;; Группировка для DETAIL
          (setq detail-groups '() curName nil curRecs '())
          (foreach rec acc
            (setq name (car rec))
            (if (not (equal name curName))
              (progn (if curName (setq detail-groups (cons (cons curName (reverse curRecs)) detail-groups))) (setq curName name curRecs (list rec)))
              (setq curRecs (cons rec curRecs))
            )
          )
          (if curName (setq detail-groups (cons (cons curName (reverse curRecs)) detail-groups)))
          (setq detail-groups (reverse detail-groups))

          ;; Нумерация групп
          (setq indexed-detail '() groupIndex 0)
          (foreach grp detail-groups
            (setq groupIndex (1+ groupIndex))
            (setq indexed-detail (append indexed-detail (list (list groupIndex (car grp) (cdr grp)))))
          )

          ;; SUMMARY
          (setq summary-groups '())
          (foreach grp detail-groups
            (setq name (car grp)
                  recs (cdr grp)
                  totalCount 0
                  totalSum 0.0)
            (foreach rec recs
              (setq totalCount (+ totalCount (caddr rec))
                    totalSum   (+ totalSum (/ (* (cadr rec) (caddr rec)) 1000.0)))
            )
            (setq summary-groups (append summary-groups (list (list name totalCount totalSum))))
          )

          ;; Выбор данных
          (if (= (strcase report-mode) "SUMMARY")
            (setq report-data summary-groups
                  report-type "SUMMARY")
            (setq report-data indexed-detail
                  report-type "DETAIL")
          )

          ;; Сводка
          (setq total-blocks 0)
          (foreach rec acc (setq total-blocks (+ total-blocks (caddr rec))))

          (if (= report-type "DETAIL")
            (progn
              (setq total-pos (length acc))
              (setq total-types (length summary-groups))
              (setq total-sum 0.0)
              (foreach rec acc (setq total-sum (+ total-sum (/ (* (cadr rec) (caddr rec)) 1000.0))))
              (princ (strcat "\nФасонка: найдено блоков " (itoa total-blocks)
                             ", позиций " (itoa total-pos)
                             ", общий погонаж " (rtos total-sum 2 2) " м.п."))
            )
            (progn
              (setq total-types (length summary-groups))
              (setq total-sum 0.0)
              (foreach g summary-groups (setq total-sum (+ total-sum (caddr g))))
              (princ (strcat "\nФасонка: найдено блоков " (itoa total-blocks)
                             ", типов " (itoa total-types)
                             ", общий погонаж " (rtos total-sum 2 2) " м.п."))
            )
          )

          ;; Имена файлов (с добавлением "подробный"/"краткий")
          (if (null save-base)
            (setq base-name (strcat (getvar "dwgprefix")
                                    (vl-filename-base (getvar "dwgname"))
                                    " Фасонка "
                                    (if (= (strcase report-mode) "DETAIL") "подробный" "краткий")))
            (setq base-name (strcat save-base " " (if (= (strcase report-mode) "DETAIL") "подробный" "краткий")))
          )

          ;; Excel / CSV
          (if export-excel
            (progn
              (setq xlsfile (strcat base-name ".xls"))
              (if (= report-type "DETAIL")
                (if (eu-export-xls-detail report-data xlsfile)
                  (princ (strcat "\nXLS сохранён: " xlsfile))
                  (progn
                    (princ "\nНе удалось сохранить XLS. Сохраняю CSV...")
                    (setq csvfile (strcat base-name ".csv"))
                    (if (eu-export-csv-detail report-data csvfile)
                      (princ (strcat "\nCSV сохранён: " csvfile))
                      (princ "\nНе удалось открыть CSV-файл.")
                    )
                  )
                )
                (if (eu-export-xls-summary report-data xlsfile)
                  (princ (strcat "\nXLS сохранён: " xlsfile))
                  (progn
                    (princ "\nНе удалось сохранить XLS. Сохраняю CSV...")
                    (setq csvfile (strcat base-name ".csv"))
                    (if (eu-export-csv-summary report-data csvfile)
                      (princ (strcat "\nCSV сохранён: " csvfile))
                      (princ "\nНе удалось открыть CSV-файл.")
                    )
                  )
                )
              )
            )
          )

          ;; GAL
          (if export-txt
            (tx-export-gal report-type report-data base-name)
          )

          ;; Таблица AutoCAD
          (if create-table
            (if (= report-type "DETAIL")
              (fasonka-create-table-detail report-data)
              (tbl-create-report report-type report-data)
            )
          )
        )
        (princ "\nБлоки со свойством 'Длина' не найдены.")
      )
    )
    (princ "\nОбъекты не найдены.")
  )
  (princ)
)

(princ "\nКоманды: FASONKA, ФАСОНКА")
(princ)