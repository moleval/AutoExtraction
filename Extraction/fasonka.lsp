;;; ============================================================
;;;  FASONKA.LSP
;;;  Извлечение данных о фасонном железе
;;;  Поддержка режимов DETAIL и SUMMARY
;;;  Экспорт XLS – через common/excel-utils.lsp
;;;  Экспорт CSV – через common/excel-utils.lsp (fallback)
;;;  GAL – через common/txt-utils.lsp
;;;  Таблицы AutoCAD – через common/table-utils.lsp
;;; ============================================================

(defun c:fasonka () (fasonka-main))
(defun c:Фасонка () (fasonka-main))

;; ------------------------------------------------------------
;; Загрузка общих библиотек (используется при автономном запуске)
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
          (strcat project-root "\\common\\txt-utils.lsp")
        )
      )
      (foreach f files
        (if (findfile f)
          (load f)
          (princ (strcat "\nПредупреждение: " f " не найден."))
        )
      )
    )
    (princ "\nПредупреждение: не удалось определить расположение fasonka.lsp.")
  )
  T
)

(defun fasonka-main (layers report-mode export-excel export-txt create-table save-base
                     / *error*
                     val name len acc rec found
                     ss i ent obj effname dynprops prop
                     layer-filter lay
                     detail-groups curName curRecs
                     indexed-detail groupIndex
                     summary-groups totalCount totalSum
                     report-data report-type
                     total-blocks total-pos total-types total-sum
                     base-name xlsfile csvfile)

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

  ;; ==================== Формирование выборки блоков ====================
  (setq ss (ssget "_I"))
  (if (null ss)
    (progn
      (cond
        ((null layers)
         (setq ss (ssget "_X" '((0 . "INSERT")))))
        (t
         (setq layer-filter (list '(0 . "INSERT")))
         (foreach lay layers
           (setq layer-filter (append layer-filter (list (cons 8 lay)))))
         (setq ss (ssget "_X" layer-filter))
        )
      )
    )
  )

  (if ss
    (progn
      (setq i 0 acc '())
      (repeat (sslength ss)
        (setq ent (ssname ss i)
              obj (vlax-ename->vla-object ent)
              effname (vla-get-effectivename obj)
              dynprops (vlax-invoke obj 'GetDynamicBlockProperties)
              val nil)
        (foreach prop dynprops
          (if (= (strcase (vla-get-propertyname prop)) "ДЛИНА")
            (setq val (vlax-get prop 'Value))
          )
        )
        (if val
          (progn
            (setq name (clean-name effname)
                  len  (atoi (rtos val 2 0))
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
          ;; Сортировка по имени и длине
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

          ;; Нумерация групп для DETAIL
          (setq indexed-detail '() groupIndex 0)
          (foreach grp detail-groups
            (setq groupIndex (1+ groupIndex))
            (setq indexed-detail (append indexed-detail (list (list groupIndex (car grp) (cdr grp)))))
          )

          ;; Группировка для SUMMARY (по имени)
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

          ;; Определяем данные для отчёта
          (if (= (strcase report-mode) "SUMMARY")
            (setq report-data summary-groups
                  report-type "SUMMARY")
            (setq report-data indexed-detail
                  report-type "DETAIL")
          )

          ;; Вычисляем сводку
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

          ;; ==================== Формирование имён файлов ====================
          (if (null save-base)
            (setq base-name (strcat (getvar "dwgprefix")
                                    (vl-filename-base (getvar "dwgname"))
                                    " Фасонка"))
            (setq base-name save-base)
          )

          ;; ==================== Экспорт в Excel / CSV ====================
          (if export-excel
            (progn
              (setq xlsfile (strcat base-name ".xls"))
              (if (= report-type "DETAIL")
                (if (eu-export-xls-detail report-data xlsfile)
                  (princ (strcat "\nXLS сохранён: " xlsfile))
                  ;; CSV fallback для DETAIL
                  (progn
                    (princ "\nНе удалось сохранить XLS. Сохраняю CSV...")
                    (setq csvfile (strcat base-name ".csv"))
                    (if (eu-export-csv-detail report-data csvfile)
                      (princ (strcat "\nCSV сохранён: " csvfile))
                      (princ "\nНе удалось открыть CSV-файл.")
                    )
                  )
                )
                ;; SUMMARY
                (if (eu-export-xls-summary report-data xlsfile)
                  (princ (strcat "\nXLS сохранён: " xlsfile))
                  ;; CSV fallback для SUMMARY
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

          ;; ==================== Экспорт в GAL (TXT) ====================
          (if export-txt
            (tx-export-gal report-type report-data base-name)
          )

          ;; ==================== Создание таблиц AutoCAD ====================
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

;; ==================== Интерактивные команды ====================
(defun c:fasonka ( / layers-str layers report-mode export-excel export-txt create-table use-default save-base)
  ;; Загружаем общие модули, если они ещё не загружены
  (fasonka-load-common)

  (setq layers-str (getstring "\nВведите слои через запятую (Enter — все слои): "))
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

;; Вспомогательная функция разбиения строки
(defun split-string (str delim / pos result)
  (setq result '())
  (while (setq pos (vl-string-search delim str))
    (setq result (append result (list (substr str 1 pos))))
    (setq str (substr str (+ pos 2)))
  )
  (setq result (append result (list str)))
  result
)

(princ "\nКоманды: FASONKA, ФАСОНКА")
(princ)