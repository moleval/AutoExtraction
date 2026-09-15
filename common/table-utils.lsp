;;; ============================================================
;;; common/table-utils.lsp
;;; Создание таблиц AutoCAD для отчётов
;;;
;;; УНИФИЦИРОВАННЫЙ УПАКОВЩИК ТАБЛИЦ:
;;;   *TU-MAX-ROWS* / *TU-IDEAL-ROWS* / *TU-MIN-FILL*
;;;   tu-pack-groups, tu-next-table-point, tu-is-last-chunk
;;;
;;; Отчёты Фасонки:
;;;   tbl-fill-detail, tbl-fill-summary, tbl-create-report
;;; ============================================================

(vl-load-com)

;; ============================================================
;; УНИФИЦИРОВАННЫЙ УПАКОВЩИК ТАБЛИЦ
;;
;; Единые лимиты строк на таблицу AutoCAD:
;;   *TU-MAX-ROWS*   = 95  ; жёсткий потолок
;;   *TU-IDEAL-ROWS* = 50  ; желаемая наполненность
;;   *TU-MIN-FILL*   = 35  ; минимум наполнения при делении
;;
;; SUMMARY-таблицы не кускуются.
;; Общий итог кладётся только в последнюю таблицу (Вариант А).
;; ============================================================

(setq *TU-MAX-ROWS*   95)
(setq *TU-IDEAL-ROWS* 50)
(setq *TU-MIN-FILL*   35)

;; ------------------------------------------------------------
;; tu-pack-groups
;; Вход:  units = список (nRows . data)
;; Выход: список кусков; кусок = список units (в исходном порядке)
;; Группа НЕ рвётся.
;; ------------------------------------------------------------
(defun tu-pack-groups (units / chunks cur curRows u n canAdd)
  (setq chunks '()  cur '()  curRows 0)
  (foreach u units
    (setq n (car u)
          canAdd nil)
    (cond
      ((zerop curRows)                                   (setq canAdd T))
      ((<= (+ curRows n) *TU-IDEAL-ROWS*)                (setq canAdd T))
      ((and (< curRows *TU-MIN-FILL*)
            (<= (+ curRows n) *TU-MAX-ROWS*))            (setq canAdd T))
      (T                                                 (setq canAdd nil))
    )
    (if canAdd
      (setq cur (cons u cur)  curRows (+ curRows n))
      (progn
        (if cur (setq chunks (cons (reverse cur) chunks)))
        (setq cur (list u)  curRows n)
      )
    )
  )
  (if cur (setq chunks (cons (reverse cur) chunks)))
  (reverse chunks)
)

;; ------------------------------------------------------------
;; tu-next-table-point — точка следующей таблицы (вниз по Y)
;; ------------------------------------------------------------
(defun tu-next-table-point (pt neededRows rowHeight gap)
  (list (car pt)
        (- (cadr pt) (+ (* neededRows rowHeight) gap))
        0.0)
)

;; ------------------------------------------------------------
;; tu-is-last-chunk — признак последнего куска
;; ------------------------------------------------------------
(defun tu-is-last-chunk (idx total)
  (= idx (1- total))
)

;; ------------------------------------------------------------
;; Функция заполнения DETAIL-таблицы Фасонки
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
;; Функция заполнения SUMMARY-таблицы Фасонки
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
  (setq row 2 i 0)
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
  (setq totalCount 0 totalSum 0.0)
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
;; Основная функция создания таблиц Фасонки
;; ------------------------------------------------------------
(defun tbl-create-report (report-type report-data / acad doc space pt pt_wcs
                          doTotals skipSingleTotals mergeTotals alignData
                          maxRowsPerTable idealRowsPerTable minFill
                          indexed-groups currentGroups currentDataRows
                          tableIndex createdTables
                          gIndex gName gRecs tableObj groupRows
                          neededRows ig canAdd
                          ans oldEcho recCount)

  (setq pt (getpoint "\nУкажите точку вставки первой таблицы: "))
  (if pt
    (progn
      (setq acad (vlax-get-acad-object)
            doc (vla-get-activedocument acad)
            space (vla-get-modelspace doc)
            pt_wcs (trans pt 1 0))
      (setq doTotals T skipSingleTotals nil mergeTotals T alignData T)
      (setq maxRowsPerTable *TU-MAX-ROWS*
            idealRowsPerTable *TU-IDEAL-ROWS*
            minFill *TU-MIN-FILL*)

      (setq oldEcho (getvar "CMDECHO"))
      (vl-catch-all-apply 'setvar (list "CMDECHO" 0))
      (vla-startundomark doc)

      (setq createdTables '() tableIndex 0)

      (if (= report-type "DETAIL")
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
              (progn (setq currentGroups (cons ig currentGroups) currentDataRows (+ currentDataRows groupRows)))
              (progn
                (if currentGroups
                  (progn
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
                        (setq pt_wcs (tu-next-table-point pt_wcs neededRows 10.0 20.0))
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
        (progn
          (setq neededRows (+ 3 (length report-data)))
          (setq tableObj (vl-catch-all-apply 'vla-addtable (list space (vlax-3d-point pt_wcs) neededRows 4 10.0 50.0)))
          (if (vl-catch-all-error-p tableObj)
            (princ (strcat "\nОшибка при создании таблицы: " (vl-catch-all-error-message tableObj)))
            (progn
              (tbl-fill-summary tableObj report-data)
              (vla-update tableObj)
              (setq createdTables (cons tableObj createdTables) tableIndex (1+ tableIndex))
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

;; ============================================================
;; КУСКОВАНИЕ ТАБЛИЦ — математика распределения строк
;; ЭТАЛОННЫЙ алгоритм проекта (перенесён из Облицовки, Этап 2).
;; Не зависит от AutoCAD: работает со списком тегированных строк,
;; где тег 'data или 'subtotal.
;;
;; Инвариант: разрез допускается ТОЛЬКО перед строкой 'data.
;; Строка 'subtotal никогда не оказывается первой в кусочке —
;; она остаётся в том же кусочке, что и последняя строка данных
;; своей группы.
;;
;; На вход:  items     - плоский список физических строк;
;;           idealRows - целевое число строк на таблицу.
;; На выход: список кусочков; каждый кусочек - список строк.
;; ============================================================
(defun tc-partition-flat (items idealRows / total numTables rowsPerTable
                            chunks current currentCount item)
  (setq total (length items))
  (setq numTables (fix (+ (/ (float total) idealRows) 0.5)))
  (if (< numTables 1) (setq numTables 1))
  (setq rowsPerTable (fix (+ (/ (float total) numTables) 0.5)))
  (if (< rowsPerTable 1) (setq rowsPerTable 1))
  (setq chunks '() current '() currentCount 0)
  (foreach item items
    (if (and (>= currentCount rowsPerTable)
             (eq (car item) 'data))
      (progn
        (setq chunks (append chunks (list current)))
        (setq current '() currentCount 0)))
    (setq current (append current (list item)))
    (setq currentCount (1+ currentCount)))
  (if current (setq chunks (append chunks (list current))))
  chunks
)

(princ "\nTABLE-UTILS.LSP загружен.")
(princ)