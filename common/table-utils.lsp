;;; ============================================================
;;; common/table-utils.lsp
;;; Создание таблиц AutoCAD для отчётов Фасонки
;;
;;; ИСПРАВЛЕНИЯ (аудит Этап 4.1 и 4.2):
;;;   Этап 4.1:
;;;   D7: добавлены переменные ans, oldEcho, recCount в /-список
;;;       tbl-create-report
;;;   D8: убран интерактивный вопрос о создании таблицы.
;;;   БАГ: в ветке SUMMARY не обновлялся список createdTables.
;;
;;;   Этап 4.2:
;;;   D4: Оптимизация группировки в ветке DETAIL —
;;;       append заменён на cons (O(1) вместо O(N)).
;;;       Порядок групп сохраняется через reverse перед использованием.
;;; ============================================================

(vl-load-com)

;; ------------------------------------------------------------
;; Функция заполнения DETAIL-таблицы
;; ------------------------------------------------------------
(defun tbl-fill-detail (table groups / row g gIdx gName gRecs totalSum itemNum len count sum)
  (vla-SetColumnWidth table 0 17.5)
  (vla-SetColumnWidth table 1 150.0)
  (vla-SetColumnWidth table 2 25.0)
  (vla-SetColumnWidth table 3 25.0)
  (vla-SetColumnWidth table 4 30.0)
  (vla-MergeCells table 0 0 0 4)
  (vla-SetText table 0 0 "{\\LФасонное железо}")
  (vla-SetText table 1 0 "№")
  (vla-SetText table 1 1 "Тип фасонки")
  (vla-SetText table 1 2 "Длина, мм")
  (vla-SetText table 1 3 "Кол-во, шт.")
  (vla-SetText table 1 4 "Сумма, м.п.")
  (vla-SetCellAlignment table 1 0 5)
  (vla-SetCellAlignment table 1 1 5)
  (vla-SetCellAlignment table 1 2 5)
  (vla-SetCellAlignment table 1 3 5)
  (vla-SetCellAlignment table 1 4 5)
  (setq row 2)
  (foreach g groups
    (setq gIdx     (car g)
          gName    (cadr g)
          gRecs    (caddr g)
          totalSum 0.0
          itemNum  0)
    (foreach rec gRecs
      (setq itemNum (1+ itemNum)
            len     (cadr rec)
            count   (caddr rec)
            sum     (/ (* len count) 1000.0)
            totalSum (+ totalSum sum))
      (vla-SetText table row 0 (strcat (itoa gIdx) "." (itoa itemNum)))
      (vla-SetText table row 1 (strcat " " gName))
      (vla-SetText table row 2 (rtos len 2 0))
      (vla-SetText table row 3 (itoa count))
      (vla-SetText table row 4 (rtos sum 2 2))
      (vla-SetCellAlignment table row 0 5)
      (vla-SetCellAlignment table row 1 4)
      (vla-SetCellAlignment table row 2 5)
      (vla-SetCellAlignment table row 3 5)
      (vla-SetCellAlignment table row 4 5)
      (setq row (1+ row))
    )
    (vla-MergeCells table row row 1 3)
    (vla-SetText table row 0 (strcat "{\\fArial|b1|i0|c0|p34;" (itoa gIdx) "}"))
    (vla-SetCellAlignment table row 0 5)
    (vla-SetText table row 1 (strcat "{\\L" gName "}"))
    (vla-SetCellAlignment table row 1 4)
    (vla-SetText table row 4 (rtos totalSum 2 2))
    (vla-SetCellAlignment table row 4 5)
    (setq row (1+ row))
  )
)

;; ------------------------------------------------------------
;; Функция заполнения SUMMARY-таблицы
;; ------------------------------------------------------------
(defun tbl-fill-summary (table groups / row i name count sum totalCount totalSum)
  (vla-SetColumnWidth table 0 10.0)
  (vla-SetColumnWidth table 1 150.0)
  (vla-SetColumnWidth table 2 25.0)
  (vla-SetColumnWidth table 3 30.0)
  (vla-MergeCells table 0 0 0 3)
  (vla-SetText table 0 0 "{\\LФасонное железо}")
  (vla-SetText table 1 0 "№")
  (vla-SetText table 1 1 "Тип фасонки")
  (vla-SetText table 1 2 "Кол-во, шт.")
  (vla-SetText table 1 3 "Сумма, м.п.")
  (vla-SetCellAlignment table 1 0 5)
  (vla-SetCellAlignment table 1 1 5)
  (vla-SetCellAlignment table 1 2 5)
  (vla-SetCellAlignment table 1 3 5)
  (setq row 2
        i 0)
  (foreach g groups
    (setq name  (car g)
          count (cadr g)
          sum   (caddr g))
    (setq i (1+ i))
    (vla-SetText table row 0 (itoa i))
    (vla-SetText table row 1 (strcat " " name))
    (vla-SetText table row 2 (itoa count))
    (vla-SetText table row 3 (rtos sum 2 2))
    (vla-SetCellAlignment table row 0 5)
    (vla-SetCellAlignment table row 1 4)
    (vla-SetCellAlignment table row 2 5)
    (vla-SetCellAlignment table row 3 5)
    (setq row (1+ row))
  )
  ;; Итоговая строка
  (setq totalCount 0
        totalSum   0.0)
  (foreach g groups
    (setq totalCount (+ totalCount (cadr g))
          totalSum   (+ totalSum (caddr g)))
  )
  (vla-MergeCells table row row 0 1)
  (vla-SetText table row 0 "      Итого")
  (vla-SetCellAlignment table row 0 4)
  (vla-SetText table row 2 (itoa totalCount))
  (vla-SetCellAlignment table row 2 5)
  (vla-SetText table row 3 (rtos totalSum 2 2))
  (vla-SetCellAlignment table row 3 5)
)

;; ------------------------------------------------------------
;; Основная функция создания таблиц AutoCAD
;; ИСПРАВЛЕНО (аудит Этап 4.1):
;;   D7: добавлены ans, oldEcho, recCount в /-список
;;   D8: убран вопрос о создании таблицы.
;;   БАГ: в ветке SUMMARY не обновлялся список createdTables.
;; ИСПРАВЛЕНО (аудит Этап 4.2):
;;   D4: Оптимизация группировки в ветке DETAIL —
;;       append заменён на cons (O(1) вместо O(N)).
;;       Порядок групп сохраняется через reverse перед использованием.
;; ------------------------------------------------------------
(defun tbl-create-report (report-type report-data / acad doc space pt pt_wcs
                          doTotals skipSingleTotals mergeTotals alignData
                          maxRowsPerTable idealRowsPerTable minFill
                          indexed-groups groupIndex currentGroups currentDataRows
                          tableIndex createdTables
                          gIndex gName gRecs tableObj groupRows
                          neededRows ig canAdd
                          ans oldEcho recCount)

  ;; Решение о создании таблицы принято на уровне вызывающего кода.
  ;; Эта функция вызывается только если create-table = T.
  ;; Никаких дополнительных вопросов пользователю не задаётся.
  (setq pt (getpoint "\nУкажите точку вставки первой таблицы: "))
  (if pt
    (progn
      (setq acad (vlax-get-acad-object)
            doc (vla-get-activedocument acad)
            space (vla-get-modelspace doc)
            pt_wcs (trans pt 1 0))
      (setq doTotals T skipSingleTotals nil mergeTotals T alignData T)
      (setq maxRowsPerTable 60 idealRowsPerTable 45 minFill 40)
      (setq oldEcho (getvar "CMDECHO"))
      (vl-catch-all-apply 'setvar (list "CMDECHO" 0))
      (vla-startundomark doc)

      ;; Инициализация списка созданных таблиц и счётчика
      (setq createdTables '()
            tableIndex 0)

      (if (= report-type "DETAIL")
        ;; ============================================================
        ;; ВЕТКА DETAIL
        ;; ============================================================
        (progn
          (setq indexed-groups report-data)
          (setq currentGroups '() currentDataRows 0)
          (while indexed-groups
            (setq ig (car indexed-groups) indexed-groups (cdr indexed-groups)
                  gIndex (car ig) gName (cadr ig) gRecs (caddr ig)
                  recCount (length gRecs)
                  groupRows (+ recCount (if doTotals 1 0)))
            (setq canAdd nil)
            (cond
              ((zerop currentDataRows) (setq canAdd T))
              ((<= (+ currentDataRows groupRows) idealRowsPerTable) (setq canAdd T))
              ((< currentDataRows minFill) (if (<= (+ currentDataRows groupRows) maxRowsPerTable) (setq canAdd T) (setq canAdd nil)))
              (t (setq canAdd nil))
            )
            (if canAdd
              ;; ИСПРАВЛЕНО (аудит Этап 4.2, пункт D4):
              ;; cons вместо append для накопления групп
              (progn (setq currentGroups (cons ig currentGroups) currentDataRows (+ currentDataRows groupRows)))
              (progn
                (if currentGroups
                  (progn
                    ;; ИСПРАВЛЕНО (аудит Этап 4.2, пункт D4):
                    ;; Восстанавливаем порядок групп перед использованием
                    (setq currentGroups (reverse currentGroups))
                    (setq neededRows 2)
                    (foreach g currentGroups (setq neededRows (+ neededRows (length (caddr g)) (if doTotals 1 0))))
                    (setq tableObj (vl-catch-all-apply 'vla-addtable (list space (vlax-3d-point pt_wcs) neededRows 5 10.0 50.0)))
                    (if (vl-catch-all-error-p tableObj)
                      (princ (strcat "\nОшибка при создании таблицы: " (vl-catch-all-error-message tableObj)))
                      (progn
                        (tbl-fill-detail tableObj currentGroups)
                        (vla-update tableObj)
                        (setq createdTables (cons tableObj createdTables) tableIndex (1+ tableIndex))
                        (princ (strcat "\nТаблица " (itoa tableIndex) " создана."))
                        (setq pt_wcs (list (car pt_wcs) (- (cadr pt_wcs) (+ (* neededRows 10.0) 20.0)) 0.0))
                      )
                    )
                  )
                )
                (setq currentGroups (list ig) currentDataRows groupRows)
              )
            )
          )
          (if currentGroups
            (progn
              ;; ИСПРАВЛЕНО (аудит Этап 4.2, пункт D4):
              ;; Восстанавливаем порядок групп перед использованием
              (setq currentGroups (reverse currentGroups))
              (setq neededRows 2)
              (foreach g currentGroups (setq neededRows (+ neededRows (length (caddr g)) (if doTotals 1 0))))
              (setq tableObj (vl-catch-all-apply 'vla-addtable (list space (vlax-3d-point pt_wcs) neededRows 5 10.0 50.0)))
              (if (vl-catch-all-error-p tableObj)
                (princ (strcat "\nОшибка при создании таблицы: " (vl-catch-all-error-message tableObj)))
                (progn
                  (tbl-fill-detail tableObj currentGroups)
                  (vla-update tableObj)
                  (setq createdTables (cons tableObj createdTables) tableIndex (1+ tableIndex))
                  (princ (strcat "\nТаблица " (itoa tableIndex) " создана."))
                )
              )
            )
          )
        )
        ;; ============================================================
        ;; ВЕТКА SUMMARY
        ;; ============================================================
        (progn
          (setq neededRows (+ 3 (length report-data)))
          (setq tableObj (vl-catch-all-apply 'vla-addtable (list space (vlax-3d-point pt_wcs) neededRows 4 10.0 50.0)))
          (if (vl-catch-all-error-p tableObj)
            (princ (strcat "\nОшибка при создании таблицы: " (vl-catch-all-error-message tableObj)))
            (progn
              (tbl-fill-summary tableObj report-data)
              (vla-update tableObj)
              ;; ИСПРАВЛЕНО (аудит Этап 4.1): добавляем таблицу в список созданных
              (setq createdTables (cons tableObj createdTables)
                    tableIndex (1+ tableIndex))
              (princ "\nТаблица SUMMARY создана.")
            )
          )
        )
      )

      (vla-endundomark doc)
      (vl-catch-all-apply 'setvar (list "CMDECHO" oldEcho))
      (if createdTables
        (princ (strcat "\nВсего создано таблиц: " (itoa tableIndex)))
        (princ "\nТаблицы не созданы.")
      )
    )
    (princ "\nТочка не указана.")
  )
)

(princ "\nTABLE-UTILS.LSP загружен.")
(princ)