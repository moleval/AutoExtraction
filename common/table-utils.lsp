;;; ============================================================
;;; common/table-utils.lsp
;;; —оздание таблиц AutoCAD дл€ отчЄтов
;;;
;;; Ё“јЋќЌ  ”— ќ¬јЌ»я:
;;;   tc-partition-flat Ч равномерное распределение строк
;;;   *TU-IDEAL-ROWS*  Ч целева€ наполненность таблицы
;;;
;;; ¬спомогательные:
;;;   tu-next-table-point, tu-is-last-chunk
;;;
;;; ќ‘ќ–ћЋ≈Ќ»≈ “јЅЋ»÷:
;;;   ts-ac-title, ts-ac-header, ts-ac-subtotal, ts-ac-total
;;;
;;; SUMMARY ‘асонки:
;;;   tbl-fill-summary, tbl-create-summary
;;; ============================================================

(vl-load-com)

;; ============================================================
;; ≈диный лимит строк на таблицу AutoCAD
;; »спользуетс€ всеми модул€ми как параметр дл€ tc-partition-flat
;; ============================================================

(setq *TU-IDEAL-ROWS* 50)

;; ------------------------------------------------------------
;; tu-next-table-point Ч точка следующей таблицы (вниз по Y)
;; ------------------------------------------------------------
(defun tu-next-table-point (pt neededRows rowHeight gap)
  (list (car pt)
        (- (cadr pt) (+ (* neededRows rowHeight) gap))
        0.0)
)

;; ------------------------------------------------------------
;; tu-is-last-chunk Ч признак последнего куска
;; ------------------------------------------------------------
(defun tu-is-last-chunk (idx total)
  (= idx (1- total))
)

;; ============================================================
;;  ”— ќ¬јЌ»≈ “јЅЋ»÷ Ч математика распределени€ строк
;; Ё“јЋќЌЌџ… алгоритм проекта (перенесЄн из ќблицовки, Ётап 2).
;; Ќе зависит от AutoCAD: работает со списком тегированных строк,
;; где тег 'data или 'subtotal.
;;
;; »нвариант: разрез допускаетс€ “ќЋ№ ќ перед строкой 'data.
;; —трока 'subtotal никогда не оказываетс€ первой в кусочке Ч
;; она остаЄтс€ в том же кусочке, что и последн€€ строка данных
;; своей группы.
;;
;; Ќа вход:  items     - плоский список физических строк;
;;           idealRows - целевое число строк на таблицу.
;; Ќа выход: список кусочков; каждый кусочек - список строк.
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
;; ‘ункци€ заполнени€ SUMMARY-таблицы ‘асонки
;; ------------------------------------------------------------
(defun tbl-fill-summary (table groups / row i name count sum totalCount totalSum)
  (vla-SetColumnWidth table 0 10.0)
  (vla-SetColumnWidth table 1 150.0)
  (vla-SetColumnWidth table 2 25.0)
  (vla-SetColumnWidth table 3 30.0)
  (vla-MergeCells table 0 0 0 3)
  (vla-SetText table 0 0 "{\\L‘асонное железо}")
  (vla-SetText table 1 0 "є")
  (vla-SetText table 1 1 "“ип фасонки")
  (vla-SetText table 1 2 " ол-во, шт.")
  (vla-SetText table 1 3 "—умма, м.п.")
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
  (vla-SetText table row 0 "{     \\L»того по всем позици€м:}")
  (vla-SetCellAlignment table row 0 4)
  (vla-SetText table row 2 (itoa totalCount))
  (vla-SetCellAlignment table row 2 5)
  (vla-SetText table row 3 (rtos totalSum 2 2))
  (vla-SetCellAlignment table row 3 5)
)

;; ------------------------------------------------------------
;; —оздание SUMMARY-таблицы ‘асонки
;; (переименована из tbl-create-report, DETAIL-ветка удалена)
;; ------------------------------------------------------------
(defun tbl-create-summary (report-type report-data / acad doc space pt pt_wcs
                          tableIndex createdTables
                          tableObj neededRows oldEcho)

  (setq pt (getpoint "\n”кажите точку вставки таблицы: "))
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
        (princ (strcat "\nќшибка при создании таблицы: "
                       (vl-catch-all-error-message tableObj)))
        (progn
          (tbl-fill-summary tableObj report-data)
          (vla-update tableObj)
          (setq createdTables (cons tableObj createdTables)
                tableIndex (1+ tableIndex))
          (princ "\n“аблица SUMMARY создана.")
        )
      )

      (vla-endundomark doc)
      (vl-catch-all-apply 'setvar (list "CMDECHO" oldEcho))
      (if createdTables
        (princ (strcat "\n¬сего создано таблиц: " (itoa tableIndex)))
        (princ "\n“аблицы не созданы.")
      )
    )
    (princ "\n“очка не указана.")
  )
)

;; ============================================================
;; ќ‘ќ–ћЋ≈Ќ»≈ “јЅЋ»÷ Ч единый визуальный стандарт
;;
;; „етыре функции покрывают все паттерны оформлени€,
;; используемые в модул€х ќблицовка, ѕодсистема, «аполнение,
;; ‘асонка. ћодуль-потребитель вызывает эти функции вместо
;; инлайн-вызовов vla-MergeCells / vla-SetText / vla-SetCellAlignment.
;;
;; ts-ac-title    Ч заголовок таблицы (объединение + подчЄркивание)
;; ts-ac-header   Ч шапка (заполнение + центрирование)
;; ts-ac-subtotal Ч подитог группы (жирный номер + подчЄркивание)
;; ts-ac-total    Ч общий итог (объединение + подчЄркивание)
;; ============================================================

;; ------------------------------------------------------------
;; ts-ac-title Ч заголовок таблицы
;;
;; ќбъедин€ет все колонки строки, устанавливает подчЄркнутый
;; текст, центрирует.
;;
;; ¬ход:
;;   tbl    Ч VLA-объект таблицы
;;   row    Ч номер строки (обычно 0)
;;   text   Ч текст заголовка (без форматировани€)
;;   nCols  Ч количество колонок в таблице
;; ------------------------------------------------------------
(defun ts-ac-title (tbl row text nCols)
  (vla-MergeCells tbl row row 0 (1- nCols))
  (vla-SetText tbl row 0 (strcat "{\\L" text "}"))
  (vla-SetCellAlignment tbl row 0 5)
)

;; ------------------------------------------------------------
;; ts-ac-header Ч шапка таблицы
;;
;; «аполн€ет €чейки строки текстами из списка headers,
;; центрирует каждую €чейку.
;;
;; ¬ход:
;;   tbl     Ч VLA-объект таблицы
;;   row     Ч номер строки (обычно 1)
;;   headers Ч список строк, например '("є" "“ип" " ол-во")
;; ------------------------------------------------------------
(defun ts-ac-header (tbl row headers / i h)
  (setq i 0)
  (foreach h headers
    (vla-SetText tbl row i h)
    (vla-SetCellAlignment tbl row i 5)
    (setq i (1+ i))
  )
)

;; ------------------------------------------------------------
;; ts-ac-subtotal Ч подитог группы
;;
;; ќбъедин€ет колонки от mergeStart до mergeEnd, устанавливает
;; жирный номер группы в колонке 0 и подчЄркнутое им€ группы
;; в колонке mergeStart.
;;
;; ¬ход:
;;   tbl        Ч VLA-объект таблицы
;;   row        Ч номер строки подитога
;;   groupIdx   Ч номер группы (целое число)
;;   groupName  Ч им€ группы (строка)
;;   mergeStart Ч перва€ колонка объединени€ (обычно 1)
;;   mergeEnd   Ч последн€€ колонка объединени€ (обычно 3)
;; ------------------------------------------------------------
(defun ts-ac-subtotal (tbl row groupIdx groupName mergeStart mergeEnd)
  (vla-MergeCells tbl row row mergeStart mergeEnd)
  (vla-SetText tbl row 0
    (strcat "{\\fArial|b1|i0|c0|p34;" (itoa groupIdx) "}"))
  (vla-SetCellAlignment tbl row 0 5)
  (vla-SetText tbl row mergeStart (strcat "{\\L" groupName "}"))
  (vla-SetCellAlignment tbl row mergeStart 4)
)

;; ------------------------------------------------------------
;; ts-ac-total Ч общий итог
;;
;; ќбъедин€ет колонки от mergeStart до mergeEnd, устанавливает
;; подчЄркнутый текст метки.
;;
;; ¬ход:
;;   tbl        Ч VLA-объект таблицы
;;   row        Ч номер строки итога
;;   label      Ч текст метки (например "»того по всем позици€м:")
;;   mergeStart Ч перва€ колонка объединени€ (обычно 0)
;;   mergeEnd   Ч последн€€ колонка объединени€ (обычно 3)
;; ------------------------------------------------------------
(defun ts-ac-total (tbl row label mergeStart mergeEnd)
  (vla-MergeCells tbl row row mergeStart mergeEnd)
  (vla-SetText tbl row 0 (strcat "{\\L" label "}"))
  (vla-SetCellAlignment tbl row 0 5)
)

(princ "\nTABLE-UTILS.LSP загружен.")
(princ)