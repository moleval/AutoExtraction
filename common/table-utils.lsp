;;; ============================================================
;;; common/table-utils.lsp
;;; Стабильная медленная версия с итоговыми строками групп
;;; ============================================================

(vl-load-com)

;; ------------------------------------------------------------
;; Группировка записей по имени
;; ------------------------------------------------------------
(defun tu-group-by-name (model / out name found group)
  (setq out '())
  (foreach rec model
    (setq name (car rec))
    (setq found nil)
    (foreach g out
      (if (= (strcase (car g)) (strcase name))
        (setq found g)
      )
    )
    (if found
      (setq out (subst (append found (list rec)) found out))
      (setq out (cons (list name rec) out))
    )
  )
  (reverse out)
)

;; ------------------------------------------------------------
;; Создание таблицы AutoCAD (медленный, но рабочий вариант)
;; ------------------------------------------------------------
(defun tu-table-create (report-mode model / pt tbl nRows nCols row space maxRows usedRows)
  (setq pt (getpoint "\nУкажите точку вставки таблицы: "))
  (if (null pt)
    nil
    (progn
      (setvar "CMDECHO" 0)

      ;; Создаём таблицу с запасом строк
      (setq maxRows 1000)
      (setq nCols (if (= report-mode "DETAIL") 5 3))

      (setq space (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object))))
      (setq tbl (vla-addtable space (vlax-3d-point pt) maxRows nCols 10.0 50.0))

      ;; Ширины столбцов
      (if (= report-mode "DETAIL")
        (progn
          (vla-setcolumnwidth tbl 0 15.0)
          (vla-setcolumnwidth tbl 1 110.0)
          (vla-setcolumnwidth tbl 2 25.0)
          (vla-setcolumnwidth tbl 3 25.0)
          (vla-setcolumnwidth tbl 4 30.0)
        )
        (progn
          (vla-setcolumnwidth tbl 0 110.0)
          (vla-setcolumnwidth tbl 1 25.0)
          (vla-setcolumnwidth tbl 2 30.0)
        )
      )

      ;; Заголовок
      (vla-mergecells tbl 0 0 0 (1- nCols))
      (vla-settext tbl 0 0 "{\\LФасонное железо}")

      ;; Шапка
      (if (= report-mode "DETAIL")
        (progn
          (vla-settext tbl 1 0 "№")
          (vla-settext tbl 1 1 "Тип фасонки")
          (vla-settext tbl 1 2 "Длина, мм")
          (vla-settext tbl 1 3 "Кол-во, шт.")
          (vla-settext tbl 1 4 "Сумма, м.п.")
        )
        (progn
          (vla-settext tbl 1 0 "Тип фасонки")
          (vla-settext tbl 1 1 "Кол-во, шт.")
          (vla-settext tbl 1 2 "Сумма, м.п.")
        )
      )

      ;; Заполнение данных
      (setq row 2)

      (if (= report-mode "DETAIL")
        ;; Детальный режим с группировкой
        (foreach group-name (mapcar 'car (tu-group-by-name model))
          (setq row (tu-fill-detail-group tbl row group-name model))
        )
        ;; Сводный режим
        (progn
          (foreach rec model
            (vla-settext tbl row 0 (nth 0 rec))
            (vla-settext tbl row 1 (itoa (nth 1 rec)))
            (vla-settext tbl row 2 (rtos (nth 2 rec) 2 2))
            (setq row (1+ row))
          )
          ;; Итого
          (vla-settext tbl row 0 "Итого")
          (vla-settext tbl row 1 (itoa (apply '+ (mapcar '(lambda (r) (nth 1 r)) model))))
          (vla-settext tbl row 2 (rtos (apply '+ (mapcar '(lambda (r) (nth 2 r)) model)) 2 2))
        )
      )

      ;; Удаляем лишние строки
      (setq usedRows row)
      (if (< usedRows maxRows)
        (vla-deleterows tbl usedRows (- maxRows usedRows))
      )

      (vla-update tbl)
      (setvar "CMDECHO" 1)
      tbl
    )
  )
)

;; ------------------------------------------------------------
;; Заполнение детальной группы (возвращает следующую строку)
;; ------------------------------------------------------------
(defun tu-fill-detail-group (tbl row group-name model / group-rec rec n len cnt mp total)
  (setq group-rec '())
  (foreach rec model
    (if (= (strcase (car rec)) (strcase group-name))
      (setq group-rec (cons rec group-rec))
    )
  )
  (setq group-rec (reverse group-rec))
  (setq n 0
        total 0.0)

  (foreach rec group-rec
    (setq n (1+ n)
          len (nth 1 rec)
          cnt (nth 2 rec)
          mp  (nth 3 rec)
          total (+ total mp))

    (vla-settext tbl row 0 (itoa n))
    (vla-settext tbl row 1 (strcat " " group-name))
    (vla-settext tbl row 2 (rtos len 2 0))
    (vla-settext tbl row 3 (itoa cnt))
    (vla-settext tbl row 4 (rtos mp 2 2))

    (vla-setcellalignment tbl row 0 5)
    (vla-setcellalignment tbl row 1 4)
    (vla-setcellalignment tbl row 2 5)
    (vla-setcellalignment tbl row 3 5)
    (vla-setcellalignment tbl row 4 5)

    (setq row (1+ row))
  )

  ;; Итоговая строка группы
  (vla-mergecells tbl row 0 row 3)
  (vla-settext tbl row 0 (strcat "      " group-name))
  (vla-setcellalignment tbl row 0 4)
  (vla-settext tbl row 4 (rtos total 2 2))
  (vla-setcellalignment tbl row 4 5)

  (1+ row)
)

(princ "\nTABLE-UTILS.LSP загружен (стабильная медленная версия).")
(princ)