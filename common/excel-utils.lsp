;;; ============================================================
;;; common/excel-utils.lsp
;;; Экспорт моделей задач в Excel .xls
;;; ============================================================

(vl-load-com)

(defun tu-excel-set-cell (sheet row col value / cell r)
  (setq cell
    (tu-safe-call
      'vla-Item
      (list (tu-safe-call 'vla-get-Cells (list sheet)) row col)
    )
  )
  (if cell
    (progn
      (setq r
        (vl-catch-all-apply
          '(lambda () (vla-put-Value2 cell value))
          '()
        )
      )
      (if (vl-catch-all-error-p r) nil T)
    )
    nil
  )
)

(defun tu-excel-set-formula (sheet row col formula / cell r)
  (setq cell
    (tu-safe-call
      'vla-Item
      (list (tu-safe-call 'vla-get-Cells (list sheet)) row col)
    )
  )
  (if cell
    (progn
      (setq r
        (vl-catch-all-apply
          '(lambda () (vla-put-Formula cell formula))
          '()
        )
      )
      (if (vl-catch-all-error-p r) nil T)
    )
    nil
  )
)

(defun tu-excel-set-number-format (sheet row col fmt / cell r)
  (setq cell
    (tu-safe-call
      'vla-Item
      (list (tu-safe-call 'vla-get-Cells (list sheet)) row col)
    )
  )
  (if cell
    (progn
      (setq r
        (vl-catch-all-apply
          '(lambda () (vla-put-NumberFormat cell fmt))
          '()
        )
      )
      (if (vl-catch-all-error-p r) nil T)
    )
    nil
  )
)

(defun tu-excel-format-header (sheet row last-col / range r)
  (setq range
    (tu-safe-call
      'vla-Range
      (list
        sheet
        (strcat "A" (itoa row))
        (strcat last-col (itoa row))
      )
    )
  )
  (if range
    (progn
      (setq r
        (vl-catch-all-apply
          '(lambda ()
             (vla-put-Font range
               (vla-get-Font range)
             )
             (vla-put-Bold (vla-get-Font range) :vlax-true)
           )
          '()
        )
      )
      T
    )
    nil
  )
)

(defun tu-excel-autofit (sheet / cols r)
  (setq cols (tu-safe-call 'vla-get-Columns (list sheet)))
  (if cols
    (progn
      (setq r
        (vl-catch-all-apply
          '(lambda () (vla-AutoFit cols))
          '()
        )
      )
      (not (vl-catch-all-error-p r))
    )
    nil
  )
)

(defun tu-excel-export-detail (sheet model / row name len cnt mp
                                      group-name group-first group-last
                                      idx)
  (tu-excel-set-cell sheet 1 1 "Фасонное железо")
  (tu-excel-set-cell sheet 2 1 "№")
  (tu-excel-set-cell sheet 2 2 "Тип фасонки")
  (tu-excel-set-cell sheet 2 3 "Длина, мм")
  (tu-excel-set-cell sheet 2 4 "Кол-во, шт.")
  (tu-excel-set-cell sheet 2 5 "Сумма, м.п.")

  (setq row 3
        idx 0
        group-name nil
        group-first nil)

  (foreach rec model
    (setq name (nth 0 rec)
          len  (nth 1 rec)
          cnt  (nth 2 rec)
          mp   (nth 3 rec))

    (if (or (null group-name) (/= group-name name))
      (progn
        (if group-name
          (progn
            (setq group-last (1- row))
            (tu-excel-set-formula
              sheet row 5
              (strcat
                "=SUM(E"
                (itoa group-first)
                ":E"
                (itoa group-last)
                ")"
              )
            )
            (tu-excel-set-cell sheet row 2 group-name)
            (setq row (1+ row))
          )
        )
        (setq group-name name
              group-first row
              idx 0)
      )
    )

    (setq idx (1+ idx))
    (tu-excel-set-cell sheet row 1 idx)
    (tu-excel-set-cell sheet row 2 name)
    (tu-excel-set-cell sheet row 3 len)
    (tu-excel-set-cell sheet row 4 cnt)
    (tu-excel-set-formula
      sheet row 5
      (strcat "=C" (itoa row) "*D" (itoa row) "/1000")
    )
    (tu-excel-set-number-format sheet row 3 "0")
    (tu-excel-set-number-format sheet row 4 "0")
    (tu-excel-set-number-format sheet row 5 "0.00")
    (setq row (1+ row))
  )

  (if group-name
    (progn
      (setq group-last (1- row))
      (tu-excel-set-cell sheet row 2 group-name)
      (tu-excel-set-formula
        sheet row 5
        (strcat
          "=SUM(E"
          (itoa group-first)
          ":E"
          (itoa group-last)
          ")"
        )
      )
      (tu-excel-set-number-format sheet row 5 "0.00")
      (setq row (1+ row))
    )
  )

  (tu-excel-format-header sheet 2 "E")
  row
)

(defun tu-excel-export-summary (sheet model / row rec name cnt mp last)
  (tu-excel-set-cell sheet 1 1 "Фасонное железо")
  (tu-excel-set-cell sheet 2 1 "Тип фасонки")
  (tu-excel-set-cell sheet 2 2 "Кол-во, шт.")
  (tu-excel-set-cell sheet 2 3 "Сумма, м.п.")

  (setq row 3)
  (foreach rec model
    (setq name (nth 0 rec)
          cnt  (nth 1 rec)
          mp   (nth 2 rec))
    (tu-excel-set-cell sheet row 1 name)
    (tu-excel-set-cell sheet row 2 cnt)
    (tu-excel-set-cell sheet row 3 mp)
    (tu-excel-set-number-format sheet row 2 "0")
    (tu-excel-set-number-format sheet row 3 "0.00")
    (setq row (1+ row))
  )

  (setq last (1- row))
  (tu-excel-set-cell sheet row 1 "Итого")
  (if (> last 2)
    (progn
      (tu-excel-set-formula sheet row 2
        (strcat "=SUM(B3:B" (itoa last) ")")
      )
      (tu-excel-set-formula sheet row 3
        (strcat "=SUM(C3:C" (itoa last) ")")
      )
    )
    (progn
      (tu-excel-set-cell sheet row 2 0)
      (tu-excel-set-cell sheet row 3 0.0)
    )
  )
  (tu-excel-set-number-format sheet row 2 "0")
  (tu-excel-set-number-format sheet row 3 "0.00")
  (tu-excel-format-header sheet 2 "C")
  row
)

(defun tu-excel-export-fasonka (model report-mode save-base / excel books book sheets sheet
                                       result path)
  (setq path (strcat save-base ".xls"))
  (setq result
    (vl-catch-all-apply
      '(lambda ()
         (setq excel (vlax-create-object "Excel.Application"))
         (if (null excel)
           (throw 'tu-excel-error "Excel.Application недоступен")
         )
         (vla-put-Visible excel :vlax-false)
         (vla-put-DisplayAlerts excel :vlax-false)
         (setq books (vla-get-Workbooks excel))
         (setq book  (vla-Add books))
         (setq sheets (vla-get-Worksheets book))
         (setq sheet (vla-Item sheets 1))

         (if (= report-mode "DETAIL")
           (tu-excel-export-detail sheet model)
           (tu-excel-export-summary sheet model)
         )

         (tu-excel-autofit sheet)

         ;; Excel 97-2003 workbook format = 56 (.xls)
         (vla-SaveAs book path 56)

         (vla-Close book :vlax-true)
         (vlax-release-object sheet)
         (vlax-release-object sheets)
         (vlax-release-object book)
         (vlax-release-object books)
         (vla-Quit excel)
         (vlax-release-object excel)
         path
       )
      '()
    )
  )
  (if (vl-catch-all-error-p result)
    (progn
      (if excel
        (progn
          (vl-catch-all-apply 'vla-Quit (list excel))
          (vl-catch-all-apply 'vlax-release-object (list excel))
        )
      )
      nil
    )
    result
  )
)

(princ)
