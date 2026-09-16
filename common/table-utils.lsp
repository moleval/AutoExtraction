;;; ============================================================
;;; common/table-utils.lsp
;;; Создание таблиц AutoCAD для отчётов
;;;
;;; ЭТАЛОН КУСКОВАНИЯ:
;;;   tc-partition-flat — равномерное распределение строк
;;;   *TU-IDEAL-ROWS*  — целевая наполненность таблицы
;;;
;;; Вспомогательные:
;;;   tu-next-table-point, tu-is-last-chunk
;;;
;;; ОФОРМЛЕНИЕ ТАБЛИЦ:
;;;   ts-ac-title, ts-ac-header, ts-ac-subtotal, ts-ac-total
;;;
;;; SUMMARY Фасонки:
;;;   tbl-fill-summary, tbl-create-summary
;;; ============================================================

(vl-load-com)

;; ============================================================
;; Единый лимит строк на таблицу AutoCAD
;; Используется всеми модулями как параметр для tc-partition-flat
;; ============================================================

(setq *TU-IDEAL-ROWS* 50)

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
  (vla-SetText table row 0 "{     \\LИтого по всем позициям:}")
  (vla-SetCellAlignment table row 0 4)
  (vla-SetText table row 2 (itoa totalCount))
  (vla-SetCellAlignment table row 2 5)
  (vla-SetText table row 3 (rtos totalSum 2 2))
  (vla-SetCellAlignment table row 3 5)
)

;; ------------------------------------------------------------
;; Создание SUMMARY-таблицы Фасонки
;; (переименована из tbl-create-report, DETAIL-ветка удалена)
;; ------------------------------------------------------------
(defun tbl-create-summary (report-type report-data / acad doc space pt pt_wcs
                          tableIndex createdTables
                          tableObj neededRows oldEcho)

  (setq pt (getpoint "\nУкажите точку вставки таблицы: "))
  (if pt
    (progn
      (setq acad (vlax-get-acad-object)
            doc (vla-get-activedocument acad)
            space (vla-get-modelspace doc)
            pt_wcs (trans pt 1 0))

      (setq oldEcho (getvar "CMDECHO"))
      (vl-catch-all-apply 'setvar (list "CMDECHO" 0))
      (vla-startundomark doc)

      (setq createdTables '() tableIndex 0)

      (setq neededRows (+ 3 (length report-data)))
      (setq tableObj (vl-catch-all-apply 'vla-addtable
        (list space (vlax-3d-point pt_wcs) neededRows 4 10.0 50.0)))
      (if (vl-catch-all-error-p tableObj)
        (princ (strcat "\nОшибка при создании таблицы: "
                       (vl-catch-all-error-message tableObj)))
        (progn
          (tbl-fill-summary tableObj report-data)
          (vla-update tableObj)
          (setq createdTables (cons tableObj createdTables)
                tableIndex (1+ tableIndex))
          (princ "\nТаблица SUMMARY создана.")
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
;; ОФОРМЛЕНИЕ ТАБЛИЦ — единый визуальный стандарт
;;
;; Четыре функции покрывают все паттерны оформления,
;; используемые в модулях Облицовка, Подсистема, Заполнение,
;; Фасонка. Модуль-потребитель вызывает эти функции вместо
;; инлайн-вызовов vla-MergeCells / vla-SetText / vla-SetCellAlignment.
;;
;; ts-ac-title    — заголовок таблицы (объединение + подчёркивание)
;; ts-ac-header   — шапка (заполнение + центрирование)
;; ts-ac-subtotal — подитог группы (жирный номер + подчёркивание)
;; ts-ac-total    — общий итог (объединение + подчёркивание)
;; ============================================================

;; ============================================================
;; ОФОРМЛЕНИЕ ТАБЛИЦ — единый визуальный стандарт
;; ============================================================

(defun ts-ac-title (tbl row text nCols)
  (vla-MergeCells tbl row row 0 (1- nCols))
  (vla-SetText tbl row 0 (strcat "{\\L" text "}"))
  (vla-SetCellAlignment tbl row 0 5)
)

(defun ts-ac-header (tbl row headers / i h)
  (setq i 0)
  (foreach h headers
    (vla-SetText tbl row i h)
    (vla-SetCellAlignment tbl row i 5)
    (setq i (1+ i))
  )
)

(defun ts-ac-subtotal (tbl row groupIdx labelText mergeStart mergeEnd)
  (vla-MergeCells tbl row row mergeStart mergeEnd)
  (if groupIdx
    (progn
      (vla-SetText tbl row 0
        (strcat "{\\fArial|b1|i0|c0|p34;" (itoa groupIdx) "}"))
      (vla-SetCellAlignment tbl row 0 5))
    (vla-SetText tbl row 0 "")
  )
  (vla-SetText tbl row mergeStart labelText)
  (vla-SetCellAlignment tbl row mergeStart 4)
)

(defun ts-ac-total (tbl row labelText mergeStart mergeEnd align)
  (vla-MergeCells tbl row row mergeStart mergeEnd)
  (vla-SetText tbl row 0 labelText)
  (vla-SetCellAlignment tbl row 0 align)
)

(princ "\nTABLE-UTILS.LSP загружен.")
(princ)