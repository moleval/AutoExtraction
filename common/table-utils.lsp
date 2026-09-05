;;; ============================================================
;;; common/table-utils.lsp
;;; Создание статических таблиц AutoCAD
;;; ============================================================

(vl-load-com)

(defun tu-table-set-text (table row col text / r)
  (setq r
    (vl-catch-all-apply
      'vla-SetText
      (list table row col text)
    )
  )
  (not (vl-catch-all-error-p r))
)

(defun tu-table-set-width (table col width / r)
  (setq r
    (vl-catch-all-apply
      'vla-SetColumnWidth
      (list table col width)
    )
  )
  (not (vl-catch-all-error-p r))
)

(defun tu-table-align (table row col align / r)
  (setq r
    (vl-catch-all-apply
      'vla-SetCellAlignment
      (list table row col align)
    )
  )
  (not (vl-catch-all-error-p r))
)

(defun tu-table-merge (table r1 r2 c1 c2 / r)
  (setq r
    (vl-catch-all-apply
      'vla-MergeCells
      (list table r1 r2 c1 c2)
    )
  )
  (not (vl-catch-all-error-p r))
)

(defun tu-table-format (table row col fmt / r)
  (setq r
    (vl-catch-all-apply
      'vla-SetCellFormat
      (list table row col fmt)
    )
  )
  (not (vl-catch-all-error-p r))
)

(defun tu-table-create (report-mode model / acad doc space pt table
                               rows cols result row rec name len cnt mp
                               group-name group-total
                               title-row header-row
                               start-row end-row n)
  (setq acad (vlax-get-acad-object))
  (setq doc (tu-safe-call 'vla-get-ActiveDocument (list acad)))
  (setq space (if doc (tu-safe-call 'vla-get-ModelSpace (list doc))))
  (setq pt (getpoint "\nУкажите точку вставки таблицы: "))

  (if (null pt)
    nil
    (progn
      (setq title-row 0
            header-row 1)

      (if (= report-mode "DETAIL")
        (progn
          ;; 1 title + 1 header + detail rows + group totals
          (setq rows (+ 2
                        (length model)
                        (length (tu-detail-group-names model))))
          (setq cols 5)
        )
        (progn
          (setq rows (+ 3 (length model))
                cols 3)
        )
      )

      (setq result
        (vl-catch-all-apply
          'vla-AddTable
          (list
            space
            (vlax-3d-point (trans pt 1 0))
            rows
            cols
            10.0
            50.0
          )
        )
      )

      (if (vl-catch-all-error-p result)
        nil
        (progn
          (setq table result)

          (if (= report-mode "DETAIL")
            (progn
              (tu-table-set-width table 0 15.0)
              (tu-table-set-width table 1 110.0)
              (tu-table-set-width table 2 25.0)
              (tu-table-set-width table 3 25.0)
              (tu-table-set-width table 4 30.0)

              (tu-table-merge table 0 0 0 4)
              (tu-table-set-text table 0 0 "{\\LФасонное железо}")

              (tu-table-set-text table 1 0 "№")
              (tu-table-set-text table 1 1 "Тип фасонки")
              (tu-table-set-text table 1 2 "Длина, мм")
              (tu-table-set-text table 1 3 "Кол-во, шт.")
              (tu-table-set-text table 1 4 "Сумма, м.п.")

              (foreach c '(0 2 3 4)
                (tu-table-align table 1 c 5)
              )
              (tu-table-align table 1 1 4)

              (setq row 2
                    group-name nil
                    n 0)

              (foreach rec model
                (setq name (nth 0 rec)
                      len  (nth 1 rec)
                      cnt  (nth 2 rec)
                      mp   (nth 3 rec))

                (if (or (null group-name) (/= group-name name))
                  (progn
                    (if group-name
                      (progn
                        (setq group-total 0.0)
                        (foreach r (reverse
                                      (tu-detail-group-records
                                        model group-name
                                      )
                                    )
                          (setq group-total
                            (+ group-total (nth 3 r))
                          )
                        )
                        (setq end-row (1- row))
                        (tu-table-merge table row row 0 3)
                        (tu-table-set-text
                          table row 0
                          (strcat "      {\\L" group-name "}")
                        )
                        (tu-table-set-text
                          table row 4
                          (rtos group-total 2 2)
                        )
                        (tu-table-align table row 0 4)
                        (tu-table-align table row 4 5)
                        (setq row (1+ row))
                      )
                    )
                    (setq group-name name
                          n 0)
                  )
                )

                (setq n (1+ n))
                (tu-table-set-text table row 0 (itoa n))
                (tu-table-set-text table row 1 (strcat " " name))
                (tu-table-set-text table row 2 (rtos len 2 0))
                (tu-table-set-text table row 3 (itoa cnt))
                (tu-table-set-text table row 4 (rtos mp 2 2))

                (tu-table-align table row 0 5)
                (tu-table-align table row 1 4)
                (tu-table-align table row 2 5)
                (tu-table-align table row 3 5)
                (tu-table-align table row 4 5)
                (tu-table-format table row 4 "0.00")

                (setq row (1+ row))
              )

              (if group-name
                (progn
                  (setq group-total 0.0)
                  (foreach r (tu-detail-group-records model group-name)
                    (setq group-total
                      (+ group-total (nth 3 r))
                    )
                  )
                  (tu-table-merge table row row 0 3)
                  (tu-table-set-text
                    table row 0
                    (strcat "      {\\L" group-name "}")
                  )
                  (tu-table-set-text
                    table row 4
                    (rtos group-total 2 2)
                  )
                  (tu-table-align table row 0 4)
                  (tu-table-align table row 4 5)
                  (tu-table-format table row 4 "0.00")
                )
              )
            )
            (progn
              (tu-table-set-width table 0 110.0)
              (tu-table-set-width table 1 25.0)
              (tu-table-set-width table 2 30.0)

              (tu-table-merge table 0 0 0 2)
              (tu-table-set-text table 0 0 "Фасонное железо")

              (tu-table-set-text table 1 0 "Тип фасонки")
              (tu-table-set-text table 1 1 "Кол-во, шт.")
              (tu-table-set-text table 1 2 "Сумма, м.п.")

              (tu-table-align table 1 0 4)
              (tu-table-align table 1 1 5)
              (tu-table-align table 1 2 5)

              (setq row 2)
              (foreach rec model
                (tu-table-set-text table row 0 (nth 0 rec))
                (tu-table-set-text table row 1 (itoa (nth 1 rec)))
                (tu-table-set-text table row 2 (rtos (nth 2 rec) 2 2))
                (tu-table-align table row 0 4)
                (tu-table-align table row 1 5)
                (tu-table-align table row 2 5)
                (tu-table-format table row 2 "0.00")
                (setq row (1+ row))
              )

              (tu-table-merge table row row 0 1)
              (tu-table-set-text table row 0 "      Итого")
              (tu-table-align table row 0 4)

              (setq group-total 0.0)
              (foreach rec model
                (setq group-total (+ group-total (nth 2 rec)))
              )
              (tu-table-set-text table row 2 (rtos group-total 2 2))
              (tu-table-align table row 2 5)
              (tu-table-format table row 2 "0.00")
            )
          )

          (vl-catch-all-apply 'vla-Update (list table))
          T
        )
      )
    )
  )
)

(defun tu-detail-group-names (model / out name)
  (setq out '())
  (foreach rec model
    (setq name (nth 0 rec))
    (if (not (member name out))
      (setq out (append out (list name)))
    )
  )
  out
)

(defun tu-detail-group-records (model name / out rec)
  (setq out '())
  (foreach rec model
    (if (= (nth 0 rec) name)
      (setq out (cons rec out))
    )
  )
  (reverse out)
)

(princ)
