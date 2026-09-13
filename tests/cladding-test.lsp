;;; ============================================================
;;; CLADDING-TEST.LSP — тестовые помощники для Облицовки
;;; РЕДАКЦИЯ 4: добавлены временная команда RELOAD
;;; и зонд TBLTEST (пошаговая проверка создания таблицы)
;;
;;; Команды:
;;;   MKTEST       — создать тестовый набор (8 полилиний, 2 слоя)
;;;   RMTEST       — удалить набор и слои
;;;   CLCOUNTERS   — показать счётчики предупреждений
;;;   TEST-FALLBACK— сверка AutoCAD Area и Green на выбранной фигуре
;;;   RELOAD       — ВРЕМЕННАЯ перезагрузка модулей облицовки (до К4)
;;;   TBLTEST      — зонд создания таблицы AutoCAD по шагам
;;; ============================================================

(vl-load-com)

;; ============================================================
;; СЛУЖЕБНЫЕ: слой и полилиния
;; ============================================================

;; Создание слоя через entmake (без команды .-LAYER)
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

;; LWPOLYLINE: точки строго 2D; булж добавляется только ненулевой
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

;; ============================================================
;; ВРЕМЕННАЯ КОМАНДА RELOAD: перезагрузка модулей облицовки
;; ДОБАВЛЕНО (К2): используется до этапа К4, пока cladding.lsp
;; не входит в загрузочную цепочку диспетчера.
;; ВНИМАНИЕ: перекрывает проектную c:RELOAD из reload.lsp,
;; если та загружена в сеансе; после К4 удалить этот defun.
;; ============================================================
(defun c:reload ()
  (load "D:/AutoExtraction/common/select-utils.lsp")
  (load "D:/AutoExtraction/Extraction/cladding.lsp")
  (load "D:/AutoExtraction/tests/cladding-test.lsp")
  (princ "\nМодули облицовки перезагружены.")
  (princ)
)

;; ============================================================
;; TBLTEST: зонд создания таблицы AutoCAD по шагам
;; Повторяет последовательность cl-create-table-summary
;; с перехватом каждого шага для локализации ошибки
;; ============================================================
(defun c:tbltest ( / pt doc space tbl r)
  (vl-load-com)
  (setq pt (getpoint "\nТочка: "))
  (if (null pt)
    (princ "\nОтмена.")
    (progn
      (setq doc (vl-catch-all-apply 'vla-get-ActiveDocument
                                    (list (vlax-get-acad-object))))
      (princ (if (vl-catch-all-error-p doc)
               (strcat "\nШАГ ОШИБКА: activedocument: "
                       (vl-catch-all-error-message doc))
               "\nШаг 1 activedocument: OK"))

      (setq space (vl-catch-all-apply 'vla-get-modelspace (list doc)))
      (princ (if (vl-catch-all-error-p space)
               (strcat "\nШАГ ОШИБКА: modelspace: "
                       (vl-catch-all-error-message space))
               "\nШаг 2 modelspace: OK"))

      (setq tbl (vl-catch-all-apply 'vla-addtable
                    (list space (vlax-3d-point pt) 5 4 10.0 50.0)))
      (princ (if (vl-catch-all-error-p tbl)
               (strcat "\nШАГ ОШИБКА: addtable: "
                       (vl-catch-all-error-message tbl))
               "\nШаг 3 addtable: OK"))

      (if (not (vl-catch-all-error-p tbl))
        (progn
          (setq r (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 0 15.0)))
          (princ (if (vl-catch-all-error-p r)
                   (strcat "\nШАГ ОШИБКА: setcolumnwidth: "
                           (vl-catch-all-error-message r))
                   "\nШаг 4 setcolumnwidth: OK"))

          (setq r (vl-catch-all-apply 'vla-MergeCells (list tbl 0 0 0 3)))
          (princ (if (vl-catch-all-error-p r)
                   (strcat "\nШАГ ОШИБКА: mergecells: "
                           (vl-catch-all-error-message r))
                   "\nШаг 5 mergecells: OK"))

          (setq r (vl-catch-all-apply 'vla-SetText (list tbl 0 0 "{\\LТест}")))
          (princ (if (vl-catch-all-error-p r)
                   (strcat "\nШАГ ОШИБКА: settext: "
                           (vl-catch-all-error-message r))
                   "\nШаг 6 settext: OK"))

          (setq r (vl-catch-all-apply 'vla-SetCellAlignment (list tbl 0 0 5)))
          (princ (if (vl-catch-all-error-p r)
                   (strcat "\nШАГ ОШИБКА: setcellalignment: "
                           (vl-catch-all-error-message r))
                   "\nШаг 7 setcellalignment: OK"))

          (setq r (vl-catch-all-apply 'vla-update (list tbl)))
          (princ (if (vl-catch-all-error-p r)
                   (strcat "\nШАГ ОШИБКА: update: "
                           (vl-catch-all-error-message r))
                   "\nШаг 8 update: OK"))
        )
      )
    )
  )
  (princ)
)

;; ============================================================
;; TBLTEST2: точная реплика cl-create-table-summary по шагам
;; Печатает имя каждого шага; останавливается на первом отказе
;; ============================================================
(defun c:tbltest2 ( / pt doc space tbl bad st)

  (defun st (name fn args / r)
    (if bad
      nil
      (progn
        (setq r (vl-catch-all-apply fn args))
        (if (vl-catch-all-error-p r)
          (progn
            (setq bad T)
            (princ (strcat "\nШАГ ОШИБКА: " name ": "
                           (vl-catch-all-error-message r)))
          )
          (princ (strcat "\nШаг " name ": OK"))
        )
        r
      )
    )
  )

  (setq bad nil)
  (setq pt (getpoint "\nТочка: "))
  (if (null pt)
    (princ "\nОтмена.")
    (progn
      (setq doc (st "01 activedocument" 'vla-get-ActiveDocument
                    (list (vlax-get-acad-object))))
      (setq space (st "02 modelspace" 'vla-get-modelspace (list doc)))
      (setq tbl (st "03 addtable" 'vla-addtable
                    (list space (vlax-3d-point pt) 5 4 10.0 50.0)))
      (st "04 width col0" 'vla-SetColumnWidth (list tbl 0 15.0))
      (st "05 width col1" 'vla-SetColumnWidth (list tbl 1 90.0))
      (st "06 width col2" 'vla-SetColumnWidth (list tbl 2 30.0))
      (st "07 width col3" 'vla-SetColumnWidth (list tbl 3 35.0))
      (st "08 merge title" 'vla-MergeCells (list tbl 0 0 0 3))
      (st "09 text title" 'vla-SetText (list tbl 0 0 "{\\LОблицовка}"))
      (st "10 text h0" 'vla-SetText (list tbl 1 0 "№"))
      (st "11 text h1" 'vla-SetText (list tbl 1 1 "Слой"))
      (st "12 text h2" 'vla-SetText (list tbl 1 2 "Кол-во, шт."))
      (st "13 text h3" 'vla-SetText (list tbl 1 3 "Площадь, м2"))
      (st "14 align h0" 'vla-SetCellAlignment (list tbl 1 0 5))
      (st "15 align h1" 'vla-SetCellAlignment (list tbl 1 1 5))
      (st "16 align h2" 'vla-SetCellAlignment (list tbl 1 2 5))
      (st "17 align h3" 'vla-SetCellAlignment (list tbl 1 3 5))
      ;; строка данных 1
      (st "18 text r2c0" 'vla-SetText (list tbl 2 0 "1"))
      (st "19 text r2c1" 'vla-SetText (list tbl 2 1 "ТЕСТ_КАССЕТЫ"))
      (st "20 text r2c2" 'vla-SetText (list tbl 2 2 "2"))
      (st "21 text r2c3" 'vla-SetText (list tbl 2 3 "1,44"))
      (st "22 align r2c0" 'vla-SetCellAlignment (list tbl 2 0 5))
      (st "23 align r2c1" 'vla-SetCellAlignment (list tbl 2 1 4))
      (st "24 align r2c2" 'vla-SetCellAlignment (list tbl 2 2 5))
      (st "25 align r2c3" 'vla-SetCellAlignment (list tbl 2 3 5))
      ;; строка данных 2
      (st "26 text r3c0" 'vla-SetText (list tbl 3 0 "2"))
      (st "27 text r3c1" 'vla-SetText (list tbl 3 1 "ТЕСТ_КЕРАМОГРАНИТ"))
      (st "28 text r3c2" 'vla-SetText (list tbl 3 2 "4"))
      (st "29 text r3c3" 'vla-SetText (list tbl 3 3 "1,44"))
      (st "30 align r3c0" 'vla-SetCellAlignment (list tbl 3 0 5))
      (st "31 align r3c1" 'vla-SetCellAlignment (list tbl 3 1 4))
      (st "32 align r3c2" 'vla-SetCellAlignment (list tbl 3 2 5))
      (st "33 align r3c3" 'vla-SetCellAlignment (list tbl 3 3 5))
      ;; итог
      (st "34 merge total" 'vla-MergeCells (list tbl 4 0 4 1))
      (st "35 text total0" 'vla-SetText (list tbl 4 0 "{\\LИтого}"))
      (st "36 text total2" 'vla-SetText (list tbl 4 2 "6"))
      (st "37 text total3" 'vla-SetText (list tbl 4 3 "2,88"))
      (st "38 align total0" 'vla-SetCellAlignment (list tbl 4 0 5))
      (st "39 align total2" 'vla-SetCellAlignment (list tbl 4 2 5))
      (st "40 align total3" 'vla-SetCellAlignment (list tbl 4 3 5))
      (st "41 update" 'vla-update (list tbl))
      (if bad
        (princ "\n=== реплика остановлена на шаге с ошибкой ===")
        (princ "\n=== реплика прошла полностью: отказ вне этих шагов ===")
      )
    )
  )
  (princ)
)

(princ "\nCLADDING-TEST.LSP загружен (ред. 4).")
(princ "\nКоманды: MKTEST, RMTEST, TEST-FALLBACK, CLCOUNTERS, RELOAD, TBLTEST")
(princ)