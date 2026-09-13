;;; ============================================================
;;; CLADDING-TEST.LSP — тестовые помощники для К1
;;; Команды: MKTEST, RMTEST, TEST-FALLBACK, CLCOUNTERS
;;; РЕДАКЦИЯ 3: удаление слоёв через ActiveX (у .-LAYER нет
;;; опции Delete в этой локали); добавлен CLCOUNTERS
;;; ============================================================

(vl-load-com)

;; ============================================================
;; СЛУЖЕБНЫЕ
;; ============================================================

(defun cl-test-ensure-layer (name)
  (if (not (tblsearch "LAYER" name))
    (if (null
          (entmake (list (cons 0 "LAYER")
                         (cons 100 "AcDbSymbolTableRecord")
                         (cons 100 "AcDbLayerTableRecord")
                         (cons 2 name)
                         (cons 70 0)
                         (cons 62 7))))
      (princ (strcat "\n[TEST] ВНИМАНИЕ: не создан слой " name))
    )
  )
)

;; Удаление слоя через ActiveX с защитой
(defun cl-test-delete-layer (name / doc layers lay r)
  (setq doc (vl-catch-all-apply 'vla-get-ActiveDocument
                                (list (vlax-get-acad-object))))
  (if (not (vl-catch-all-error-p doc))
    (progn
      (setq layers (vla-get-Layers doc))
      (setq lay (vl-catch-all-apply 'vla-item (list layers name)))
      (if (not (vl-catch-all-error-p lay))
        (progn
          (setq r (vl-catch-all-apply 'vla-delete (list lay)))
          (if (vl-catch-all-error-p r)
            (princ (strcat "\n[TEST] слой не удалён (занят?): " name))
          )
        )
      )
    )
  )
)

;; LWPOLYLINE: точки строго 2D; булж только ненулевой
(defun cl-test-lw (layer closed pts blgs / i e bv res)
  (setq e (list (cons 0 "LWPOLYLINE")
                (cons 100 "AcDbEntity")
                (cons 8 layer)
                (cons 100 "AcDbPolyline")
                (cons 90 (length pts))
                (cons 70 (if closed 1 0))))
  (setq i 0)
  (foreach p pts
    (setq e (append e (list (cons 10 (list (car p) (cadr p))))))
    (setq bv (nth i blgs))
    (if (and bv (/= bv 0.0))
      (setq e (append e (list (cons 42 bv))))
    )
    (setq i (1+ i))
  )
  (setq res (entmake e))
  (if (null res)
    (princ (strcat "\n[TEST] ВНИМАНИЕ: entmake не создал полилинию на слое " layer))
  )
  res
)

;; ============================================================
;; ГЕНЕРАТОРЫ ФИГУР
;; ============================================================

(defun cl-test-rect (layer x y w h)
  (cl-test-lw layer T
    (list (list x y) (list (+ x w) y) (list (+ x w) (+ y h)) (list x (+ y h)))
    (list 0 0 0 0))
)

(defun cl-test-rounded (layer x y w h r / b)
  (setq b 0.414214)  ; tan(90/4 град) — скругление 90 град
  (cl-test-lw layer T
    (list (list (+ x r) y) (list (- (+ x w) r) y)
          (list (+ x w) (+ y r)) (list (+ x w) (- (+ y h) r))
          (list (- (+ x w) r) (+ y h)) (list (+ x r) (+ y h))
          (list x (- (+ y h) r)) (list x (+ y r)))
    (list 0 b 0 b 0 b 0 b))
)

(defun cl-test-arc (layer x y w h b)
  (cl-test-lw layer T
    (list (list x y) (list (+ x w) y) (list (+ x w) (+ y h)) (list x (+ y h)))
    (list 0 0 b 0))
)

(defun cl-test-rot (layer x y w h ang / a ca sa pts rot)
  (setq a (* ang (/ pi 180.0)) ca (cos a) sa (sin a))
  (setq rot '(lambda (p)
               (list (+ x (- (* (car p) ca) (* (cadr p) sa)))
                     (+ y (+ (* (car p) sa) (* (cadr p) ca))))))
  (setq pts (mapcar rot (list (list 0 0) (list w 0) (list w h) (list 0 h))))
  (cl-test-lw layer T pts (list 0 0 0 0))
)

(defun cl-test-open (layer x y)
  (cl-test-lw layer nil
    (list (list x y) (list (+ x 500) y) (list (+ x 500) (+ y 300)))
    (list 0 0 0))
)

;; Тест A: незамкнутая + bulge
(defun cl-test-open-bulge (layer x y)
  (cl-test-lw layer nil
    (list (list x y) (list (+ x 500) y) (list (+ x 500) (+ y 300)))
    (list 0.3 0 0))
)

;; ============================================================
;; MKTEST: создать тестовый набор
;; ============================================================
(defun c:mktest ()
  (vl-load-com)
  (cl-test-ensure-layer "ТЕСТ_КЕРАМОГРАНИТ")
  (cl-test-ensure-layer "ТЕСТ_КАССЕТЫ")

  (cl-test-rect       "ТЕСТ_КЕРАМОГРАНИТ"    0.0    0.0 600.0 600.0)
  (cl-test-rounded    "ТЕСТ_КЕРАМОГРАНИТ" 1000.0   0.0 600.0 600.0 10.0)
  (cl-test-arc        "ТЕСТ_КЕРАМОГРАНИТ" 2000.0   0.0 1000.0 500.0 -0.3)
  (cl-test-rot        "ТЕСТ_КЕРАМОГРАНИТ" 4000.0   0.0 800.0 400.0 30.0)
  (cl-test-open       "ТЕСТ_КЕРАМОГРАНИТ" 5000.0   0.0)
  (cl-test-open-bulge "ТЕСТ_КЕРАМОГРАНИТ" 6000.0   0.0)
  (cl-test-rect       "ТЕСТ_КАССЕТЫ"         0.0 2000.0 1200.0 600.0)
  (cl-test-rect       "ТЕСТ_КАССЕТЫ"      2000.0 2000.0 1200.0 600.0)

  (princ "\nТестовый набор создан: 8 полилиний на 2 слоях.")
  (princ)
)

;; ============================================================
;; RMTEST: удалить тестовый набор
;; ============================================================
(defun c:rmtest ( / ss lay)
  (foreach lay '("ТЕСТ_КЕРАМОГРАНИТ" "ТЕСТ_КАССЕТЫ")
    (setq ss (ssget "_X" (list (cons 8 lay))))
    (if ss (command "._ERASE" ss ""))
  )
  (foreach lay '("ТЕСТ_КЕРАМОГРАНИТ" "ТЕСТ_КАССЕТЫ")
    (cl-test-delete-layer lay)
  )
  (princ "\nТестовый набор удалён.")
  (princ)
)

;; ============================================================
;; CLCOUNTERS: показать счётчики предупреждений
;; ============================================================
(defun c:clcounters ()
  (princ (strcat "\nopen="     (itoa *cladding-skipped-open*)
                 "  zero="    (itoa *cladding-skipped-zero*)
                 "  arcs="    (itoa *cladding-with-arcs*)
                 "  mismatch=" (itoa *cladding-check-mismatch*)))
  (princ)
)

;; ============================================================
;; TEST-FALLBACK: сверка AutoCAD Area и Green (тест B)
;; ============================================================
(defun c:test-fallback ( / ent obj a-cad a-green diff)
  (princ "\n=== Тест B: сверка AutoCAD Area и Green ===")
  (setq ent (car (entsel "\nВыберите полилинию: ")))
  (if ent
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (setq a-cad (vlax-get obj 'Area))
      (setq a-green (cl-green-area ent))
      (setq diff (abs (- a-cad a-green)))
      (princ (strcat "\nAutoCAD Area: " (rtos a-cad 2 2) " мм2"))
      (princ (strcat "\nGreen Area:   " (rtos a-green 2 2) " мм2"))
      (princ (strcat "\nРасхождение:  " (rtos diff 2 2) " мм2"))
      (if (<= diff 1.0)
        (princ "\nСовпадение в пределах 1 мм2")
        (princ "\nВНИМАНИЕ: расхождение > 1 мм2 — требуется анализ")
      )
    )
    (princ "\nОтмена.")
  )
  (princ)
)

(princ "\nCLADDING-TEST.LSP загружен (ред. 3).")
(princ "\nКоманды: MKTEST, RMTEST, TEST-FALLBACK, CLCOUNTERS")
(princ)