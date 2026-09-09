;;; ============================================================
;;;  FASONKA.LSP
;;;  Извлечение данных о фасонном железе
;;;  Поддержка режимов DETAIL и SUMMARY
;;;  Выборка и извлечение длины — в common/select-utils.lsp
;;;  Экспорт XLS/CSV — в common/excel-utils.lsp
;;;  GAL — в common/txt-utils.lsp
;;;  Таблицы AutoCAD — в common/table-utils.lsp
;;; ============================================================

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

  ;; Очистка имени блока
  (defun clean-name (str / tmp first rest)
    (if (and str (> (strlen str) 7))
      (progn
        (setq tmp (substr str 8))
        (if (> (strlen tmp) 0)
          (setq first (strcase (substr tmp 1 1))
                rest  (substr tmp 2)
                tmp   (strcat first rest))
        )
        tmp
      )
      str
    )
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
            (tbl-create-report report-type report-data)
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