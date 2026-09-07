;;; ============================================================
;;; NEST1DS.LSP — раскрой хлыстов по выбранным элементам
;;; Команда: NEST1DS
;;; Поддержка: LWPOLYLINE, POLYLINE, LINE, ARC, ELLIPSE, SPLINE, MLINE
;;; Алгоритм: First-Fit Decreasing (FFD)
;;; ============================================================
(vl-load-com)

;; ================= НАСТРОЙКИ =================
(setq *NEST-TRANSPARENCY* 70)
(setq *NEST-PALETTE* '(1 2 3 4 5 6 30 210 140 90))
(setq *NEST-STYLE-NAME* "Основной стиль (раскрой)")   ; имя стиля курсива
(setq *NEST-ITALIC-ANGLE* 0.26)                       ; наклон курсива, радианы (~15°)
(setq *NEST-TEXT-STYLE* nil)
(setq *NEST-COLOR-OUTLINE* 7)
(setq *NEST-COLOR-LABEL*   7)
(setq *NEST-COLOR-WASTE*   8)
(setq *NEST-COLOR-TITLE*   5)
(setq *NEST-COLOR-HEADER*  3)
(setq *NEST-COLOR-VALUE*   7)
(setq *NEST-COLOR-KPD*     1)
;; =============================================

;; ---------- прозрачность для DXF 440 ----------
(defun n1-trans-value (percent)
  (fix (* 255.0 (/ (- 100.0 (float percent)) 100.0)))
)

;; ---------- курсивный стиль на основе Arial ----------
(defun n1-ensure-italic-style ( / result)
  (if (tblsearch "STYLE" *NEST-STYLE-NAME*)
    (progn
      (setq *NEST-TEXT-STYLE* *NEST-STYLE-NAME*)
      (princ (strcat "\nСтиль курсива уже существует: " *NEST-STYLE-NAME*))
    )
    (progn
      (setq result
        (entmake
          (list
            (cons 0 "STYLE")
            (cons 2 *NEST-STYLE-NAME*)
            (cons 70 0)
            (cons 40 0.0)
            (cons 41 1.0)
            (cons 50 *NEST-ITALIC-ANGLE*)
            (cons 71 0)
            (cons 42 2.5)
            (cons 3 "Arial")
            (cons 4 "")
          )
        )
      )
      (if (and result (tblsearch "STYLE" *NEST-STYLE-NAME*))
        (progn
          (setq *NEST-TEXT-STYLE* *NEST-STYLE-NAME*)
          (princ (strcat "\nСоздан стиль курсива: " *NEST-STYLE-NAME*))
        )
        (progn
          (setq *NEST-TEXT-STYLE* nil)
          (princ "\nНе удалось создать стиль курсива — используется текущий.")
        )
      )
    )
  )
)

;; ---------- уникальное имя блока (база, при занятости — с номером) ----------
(defun n1-unique-block-name (base / name n)
  (setq n 0)
  (setq name (strcat base " " (itoa n)))
  (while (tblsearch "BLOCK" name)
    (setq n (1+ n))
    (setq name (strcat base " " (itoa n)))
  )
  name
)

;; ---------- вставка блока ----------
(defun n1-block-insert (name insPt)
  (entmake
    (list
      (cons 0 "INSERT")
      (cons 100 "AcDbEntity")
      (cons 100 "AcDbBlockReference")
      (cons 2 name)
      (cons 10 (list (car insPt) (cadr insPt) 0.0))
      (cons 41 1.0)
      (cons 42 1.0)
      (cons 43 1.0)
      (cons 50 0.0)
    )
  )
)

;; ---------- цвет для группы длины ----------
(defun n1-build-color-map (pieces / palette i map rec)
  (setq palette *NEST-PALETTE*)
  (setq i 0 map '())
  (foreach rec pieces
    (setq map (cons (cons (car rec) (nth (rem i (length palette)) palette)) map))
    (setq i (1+ i))
  )
  (reverse map)
)

(defun n1-get-color (color-map len / pair)
  (setq pair (assoc len color-map))
  (if pair (cdr pair) 7)
)

;; ---------- текст с цветом и стилем ----------
(defun n1-draw-text (pt h str color / style)
  (setq style (if (and *NEST-TEXT-STYLE* (/= *NEST-TEXT-STYLE* ""))
                *NEST-TEXT-STYLE*
                (getvar "TEXTSTYLE")))
  (entmake
    (list
      (cons 0 "TEXT")
      (cons 62 color)
      (cons 7 style)
      (cons 10 (list (car pt) (cadr pt) 0.0))
      (cons 40 h)
      (cons 1 str)
      (cons 50 0.0)
    )
  )
)

;; ---------- линия ----------
(defun n1-draw-line (p1 p2 color)
  (entmake
    (list
      (cons 0 "LINE")
      (cons 62 color)
      (cons 10 (list (car p1) (cadr p1) 0.0))
      (cons 11 (list (car p2) (cadr p2) 0.0))
    )
  )
)

;; ---------- прямоугольник ----------
(defun n1-draw-rect (p1 p2 color / x1 y1 x2 y2)
  (setq x1 (car p1) y1 (cadr p1) x2 (car p2) y2 (cadr p2))
  (entmake
    (list
      (cons 0 "LWPOLYLINE")
      (cons 100 "AcDbEntity")
      (cons 62 color)
      (cons 100 "AcDbPolyline")
      (cons 90 4)
      (cons 70 1)
      (cons 10 (list x1 y1))
      (cons 10 (list x2 y1))
      (cons 10 (list x2 y2))
      (cons 10 (list x1 y2))
    )
  )
)

;; ---------- заливка ----------
(defun n1-draw-hatch (p1 p2 color trans / x1 y1 x2 y2)
  (setq x1 (car p1) y1 (cadr p1) x2 (car p2) y2 (cadr p2))
  (entmake
    (list
      (cons 0 "HATCH")
      (cons 100 "AcDbEntity")
      (cons 62 color)
      (cons 440 trans)
      (cons 100 "AcDbHatch")
      (cons 10 (list 0.0 0.0 0.0))
      (cons 210 (list 0.0 0.0 1.0))
      (cons 2 "SOLID")
      (cons 70 1)
      (cons 71 0)
      (cons 91 1)
      (cons 92 2)
      (cons 72 0)
      (cons 73 1)
      (cons 93 4)
      (cons 10 (list x1 y1))
      (cons 10 (list x2 y1))
      (cons 10 (list x2 y2))
      (cons 10 (list x1 y2))
      (cons 75 0)
      (cons 76 1)
      (cons 47 1.0)
      (cons 78 0)
      (cons 98 0)
    )
  )
)

;; ---------- развернуть группы ----------
(defun n1-expand (pieces / sorted-groups out rec len cnt i)
  (setq sorted-groups
    (vl-sort pieces '(lambda (a b) (> (car a) (car b))))
  )
  (setq out '())
  (foreach rec sorted-groups
    (setq len (car rec))
    (setq cnt (fix (cadr rec)))
    (setq i 0)
    (while (< i cnt)
      (setq out (append out (list len)))
      (setq i (1+ i))
    )
  )
  out
)

(defun n1-replace-nth (lst idx new / i out)
  (setq i 0 out '())
  (foreach x lst
    (if (= i idx)
      (setq out (append out (list new)))
      (setq out (append out (list x)))
    )
    (setq i (1+ i))
  )
  out
)

;; ---------- FFD ----------
(defun n1-ffd (sorted-pieces stock kerf / bars p placed j bar newbar)
  (setq bars '())
  (foreach p sorted-pieces
    (setq placed nil j 0)
    (while (and (not placed) (< j (length bars)))
      (setq bar (nth j bars))
      (if (>= (car bar) (+ p kerf))
        (progn
          (setq newbar (cons (- (car bar) (+ p kerf))
                             (append (cdr bar) (list p))))
          (setq bars (n1-replace-nth bars j newbar))
          (setq placed T)
        )
      )
      (setq j (1+ j))
    )
    (if (not placed)
      (setq bars (append bars (list (list (- stock (+ p kerf)) p))))
    )
  )
  bars
)

;; ---------- длина мультилинии ----------
(defun n1-mline-length (ent / obj numEl copyObj arr safe sub elen total)
  (setq numEl (cdr (assoc 72 (entget ent))))
  (if (or (null numEl) (< numEl 1)) (setq numEl 1))
  (setq obj (vlax-ename->vla-object ent))
  (setq copyObj (vla-Copy obj))
  (setq arr (vl-catch-all-apply 'vla-Explode (list copyObj)))
  (if (vl-catch-all-error-p arr)
    (progn (vl-catch-all-apply 'vla-Delete (list copyObj)) nil)
    (progn
      (setq safe (vlax-safearray->list (vlax-variant-value arr)))
      (setq total 0.0)
      (foreach sub safe
        (setq elen (vl-catch-all-apply 'vlax-curve-getDistAtParam
                    (list (vlax-vla-object->ename sub)
                          (vlax-curve-getEndParam (vlax-vla-object->ename sub)))))
        (if (numberp elen) (setq total (+ total elen)))
        (vl-catch-all-apply 'vla-Delete (list sub))
      )
      (/ total (float numEl))
    )
  )
)

(defun n1-add-group (groups key / found)
  (setq found (assoc key groups))
  (if found
    (mapcar '(lambda (x) (if (= (car x) key) (cons (car x) (1+ (cdr x))) x)) groups)
    (cons (cons key 1) groups)
  )
)

(defun n1-extract-pieces (ss tol / i ent typ len key pieces total measured skipped)
  (setq pieces '())
  (setq i 0)
  (setq total (sslength ss))
  (setq measured 0 skipped 0)
  (repeat total
    (setq ent (ssname ss i))
    (setq typ (cdr (assoc 0 (entget ent))))
    (if (= typ "MLINE")
      (setq len (n1-mline-length ent))
      (setq len (vl-catch-all-apply 'vlax-curve-getDistAtParam
                  (list ent (vlax-curve-getEndParam ent))))
    )
    (if (and (numberp len) (> len 0.0))
      (progn
        (setq measured (1+ measured))
        (setq key (fix (+ (/ len tol) 0.5)))
        (setq pieces (n1-add-group pieces key))
      )
      (setq skipped (1+ skipped))
    )
    (setq i (1+ i))
  )
  (princ (strcat "\nИзмерено: " (itoa measured) " из " (itoa total)
                 (if (> skipped 0) (strcat ", пропущено: " (itoa skipped)) "")))
  (mapcar '(lambda (x) (list (* (float (car x)) tol) (cdr x))) (reverse pieces))
)

(defun n1-list-to-str (lst / s x)
  (setq s "")
  (foreach x lst (setq s (strcat s (if (= s "") "" " ") (rtos x 2 0))))
  s
)

;; ---------- визуализация хлыстов (в точке вставки) ----------
(defun n1-draw-layout (bars stock kerf insPt color-map /
    barHeight gap txtH x0 y0 maxy miny i bar pieces waste used util
    curx p halfw str col sp)
  (setq barHeight (/ stock 30.0))
  (setq gap (* barHeight 0.7))
  (setq txtH (* barHeight 0.30))
  (setq x0 (car insPt))
  (setq y0 (cadr insPt))
  (setq maxy (+ y0 barHeight))
  (setq miny y0)
  (setq i 0)
  (foreach bar bars
    (setq i (1+ i))
    (setq pieces (cdr bar))
    (setq waste (car bar))
    (setq used (- stock waste))
    (setq util (* 100.0 (/ used stock)))
    (setq miny y0)

    (setq curx x0)
    (foreach p pieces
      (setq col (n1-get-color color-map p))
      (n1-draw-hatch (list curx y0) (list (+ curx p) (+ y0 barHeight))
                     col (n1-trans-value *NEST-TRANSPARENCY*))
      (setq curx (+ curx p kerf))
    )

    (if (> waste 0.0)
      (n1-draw-hatch (list (- (+ x0 stock) waste) y0)
                     (list (+ x0 stock) (+ y0 barHeight))
                     *NEST-COLOR-WASTE* (n1-trans-value *NEST-TRANSPARENCY*))
    )

    (n1-draw-rect (list x0 y0) (list (+ x0 stock) (+ y0 barHeight)) *NEST-COLOR-OUTLINE*)

    (setq curx x0)
    (foreach p pieces
      (n1-draw-line (list curx y0) (list curx (+ y0 barHeight)) *NEST-COLOR-OUTLINE*)
      (setq curx (+ curx p kerf))
    )
    (n1-draw-line (list curx y0) (list curx (+ y0 barHeight)) *NEST-COLOR-OUTLINE*)

    (setq sp (cond ((< i 10)   "   ")   ; Хлыст 1…9    ? 3 пробела
                   ((< i 100)  "  ")    ; Хлыст 10…99  ? 2 пробела
                   (t           " ")))   ; Хлыст 100+   ? 1 пробел
    (n1-draw-text (list (- x0 (* barHeight 2.2) 100.0) (+ y0 (* barHeight 0.35))) txtH
                  (strcat "Хлыст " (itoa i) sp "[" (rtos util 2 1) "%]") *NEST-COLOR-LABEL*)

    (setq curx x0)
    (foreach p pieces
      (setq str (rtos p 2 0))
      (setq halfw (* (strlen str) txtH 0.4))
      (setq col (n1-get-color color-map p))
      (n1-draw-text (list (+ curx (* p 0.5) (- halfw)) (+ y0 (* barHeight 0.35))) txtH str col)
      (setq curx (+ curx p kerf))
    )

    (if (> waste 0.0)
      (progn
        (setq str (strcat "отход " (rtos waste 2 0)))
        (setq halfw (* (strlen str) txtH 0.4))
        (n1-draw-text (list (+ (- (+ x0 stock) waste) (* waste 0.5) (- halfw))
                            (+ y0 (* barHeight 0.35))) txtH str *NEST-COLOR-WASTE*)
      )
    )

    (setq y0 (- y0 barHeight gap))
  )
  (list (list (- x0 (* barHeight 3.0) 100.0) miny) (list (+ x0 stock) maxy))
)

;; ---------- таблица итогов ----------
(defun n1-draw-summary (bars pieces stock insPt color-map /
    barHeight th rowH pad col1W col2W col3W tableW tableH
    left top x1 x2 x3 y bottom
    num-bars stock-total-mm stock-total-m
    total-cnt total-product-mm total-product-m kpd rec)
  (setq barHeight (/ stock 30.0))
  (setq th (* barHeight 0.30))
  (setq rowH (* barHeight 0.6))
  (setq pad (* barHeight 0.6))
  (setq col1W (* barHeight 5.0))
  (setq col2W (* barHeight 3.5))
  (setq col3W (* barHeight 4.5))
  (setq tableW (+ col1W col2W col3W (* pad 2)))
  (setq num-bars (length bars))
  (setq stock-total-mm (* num-bars stock))
  (setq stock-total-m (/ stock-total-mm 1000.0))
  (setq total-cnt 0 total-product-mm 0.0)
  (foreach rec pieces
    (setq total-cnt (+ total-cnt (cadr rec)))
    (setq total-product-mm (+ total-product-mm (* (car rec) (cadr rec))))
  )
  (setq total-product-m (/ total-product-mm 1000.0))
  (setq kpd (if (> stock-total-mm 0)
              (* 100.0 (/ (float total-product-mm) (float stock-total-mm)))
              0.0))
  (setq left (car insPt))
  (setq top (cadr insPt))
  (setq tableH (+ (* pad 2) (* (+ 11.0 (length pieces)) rowH)))
  (setq bottom (- top tableH))
  (setq x1 (+ left pad))
  (setq x2 (+ left pad col1W))
  (setq x3 (+ left pad col1W col2W))
  (n1-draw-rect (list left bottom) (list (+ left tableW) top) *NEST-COLOR-OUTLINE*)
  (setq y (- top pad 100.0))
  (n1-draw-text (list x1 y) (* th 1.3) "ИТОГИ РАСКРОЯ" *NEST-COLOR-TITLE*)
  (setq y (- y rowH) y (- y (* rowH 0.5)))
  (n1-draw-text (list x1 y) th "ХЛЫСТЫ" *NEST-COLOR-HEADER*)
  (setq y (- y rowH))
  (n1-draw-text (list x1 y) th "Длина, мм" *NEST-COLOR-HEADER*)
  (n1-draw-text (list x2 y) th "Кол-во, шт" *NEST-COLOR-HEADER*)
  (n1-draw-text (list x3 y) th "Погонаж, м.п." *NEST-COLOR-HEADER*)
  (setq y (- y rowH))
  (n1-draw-text (list x1 y) th (rtos stock 2 0) *NEST-COLOR-VALUE*)
  (n1-draw-text (list x2 y) th (itoa num-bars) *NEST-COLOR-VALUE*)
  (n1-draw-text (list x3 y) th (rtos stock-total-m 2 2) *NEST-COLOR-VALUE*)
  (setq y (- y rowH) y (- y (* rowH 0.5)))
  (n1-draw-text (list x1 y) th "ИЗДЕЛИЯ" *NEST-COLOR-HEADER*)
  (setq y (- y rowH))
  (n1-draw-text (list x1 y) th "Длина, мм" *NEST-COLOR-HEADER*)
  (n1-draw-text (list x2 y) th "Кол-во, шт" *NEST-COLOR-HEADER*)
  (n1-draw-text (list x3 y) th "Погонаж, м.п." *NEST-COLOR-HEADER*)
  (setq y (- y rowH))
  (foreach rec pieces
    (n1-draw-text (list x1 y) th (rtos (car rec) 2 0) (n1-get-color color-map (car rec)))
    (n1-draw-text (list x2 y) th (itoa (cadr rec)) *NEST-COLOR-VALUE*)
    (n1-draw-text (list x3 y) th (rtos (/ (* (car rec) (cadr rec)) 1000.0) 2 2) *NEST-COLOR-VALUE*)
    (setq y (- y rowH))
  )
  (setq y (- y (* rowH 0.5)))
  (n1-draw-text (list x1 y) th (strcat "Всего изделий: " (itoa total-cnt) " шт") *NEST-COLOR-VALUE*)
  (setq y (- y rowH))
  (n1-draw-text (list x1 y) th (strcat "Погонаж изделий: " (rtos total-product-m 2 2) " м.п.") *NEST-COLOR-VALUE*)
  (setq y (- y rowH))
  (n1-draw-text (list x1 y) (* th 1.2) (strcat "КПД использования: " (rtos kpd 2 1) " %") *NEST-COLOR-KPD*)
  (list (list left bottom) (list (+ left tableW) top))
)

(defun n1-combine-bbox (b1 b2)
  (list
    (list (min (car (car b1)) (car (car b2)))
          (min (cadr (car b1)) (cadr (car b2))))
    (list (max (car (cadr b1)) (car (cadr b2)))
          (max (cadr (cadr b1)) (cadr (cadr b2))))
  )
)

(defun n1-report (bars stock kerf / i bar pieces waste used util)
  (princ (strcat "\nВсего хлыстов: " (itoa (length bars))))
  (setq i 0)
  (foreach bar bars
    (setq i (1+ i))
    (setq pieces (cdr bar))
    (setq waste (car bar))
    (setq used (- stock waste))
    (setq util (* 100.0 (/ used stock)))
    (princ (strcat "\nХлыст " (itoa i) ": " (n1-list-to-str pieces)
                   " | исп. " (rtos used 2 1) " | отход " (rtos waste 2 1) " | " (rtos util 2 1) "%"))
  )
  (princ)
)

(defun n1-write-csv (bars stock kerf / fname f i bar pieces waste used util)
  (setq fname (strcat (getvar "DWGPREFIX") "nest1ds_result.csv"))
  (setq f (open fname "w"))
  (if f
    (progn
      (write-line "Хлыст;Отрезки;Использовано;Отход;Использование_%" f)
      (setq i 0)
      (foreach bar bars
        (setq i (1+ i))
        (setq pieces (cdr bar))
        (setq waste (car bar))
        (setq used (- stock waste))
        (setq util (* 100.0 (/ used stock)))
        (write-line (strcat (itoa i) ";" (n1-list-to-str pieces) ";" (rtos used 2 1) ";"
                            (rtos waste 2 1) ";" (rtos util 2 1)) f)
      )
      (close f)
      (princ (strcat "\nCSV сохранён: " fname))
    )
  )
)

;; ---------- главная команда ----------
(defun c:NEST1DS ( / ss tol stock kerf insPt pieces sorted bars
                    bbox1 bbox2 bbox dbg-cnt p1 p2 color-map
                    barHeight sumInsPt num-bars stock-total-mm
                    total-cnt total-product-mm kpd rec blockName baseName
                    lastEnt ssNew ent oldEcho)
  (princ "\n=== РАСКРОЙ ХЛЫСТОВ ПО ВЫБРАННЫМ ЭЛЕМЕНТАМ ===")
  (princ "\nВыберите полилинии/линии - будущие отрезки:")
  (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE,LINE,ARC,ELLIPSE,SPLINE,MLINE"))))
  (if (null ss) (progn (princ "\nНичего не выбрано.") (princ) (exit)))
  (princ (strcat "\nВыбрано объектов: " (itoa (sslength ss))))
  (setq tol (getreal "\nТочность группировки длин (мм) <1>: "))
  (if (or (null tol) (<= tol 0.0)) (setq tol 1.0))
  (setq stock (getreal "\nДлина хлыста (мм) <6000>: "))
  (if (null stock) (setq stock 6000.0))
  (setq kerf (getreal "\nШирина реза (мм) <0>: "))
  (if (null kerf) (setq kerf 0.0))

  (setq pieces (n1-extract-pieces ss tol))
  (if (null pieces) (progn (princ "\nНе удалось получить длины.") (princ) (exit)))

  (setq dbg-cnt 0)
  (foreach rec pieces (setq dbg-cnt (+ dbg-cnt (cadr rec))))
  (princ (strcat "\nГрупп длин: " (itoa (length pieces)) ", всего отрезков: " (itoa dbg-cnt)))

  (foreach rec pieces
    (if (> (car rec) stock)
      (princ (strcat "\nВНИМАНИЕ: отрезок " (rtos (car rec) 2 2) " длиннее хлыста!"))
    )
  )

  (setq sorted (n1-expand pieces))
  (princ (strcat "\nРазвёрнуто отрезков: " (itoa (length sorted))))
  (setq bars (n1-ffd sorted stock kerf))
  (princ (strcat "\nХлыстов после раскроя: " (itoa (length bars))))
  (n1-report bars stock kerf)

  (setq num-bars (length bars))
  (setq stock-total-mm (* num-bars stock))
  (setq total-cnt 0 total-product-mm 0.0)
  (foreach rec pieces
    (setq total-cnt (+ total-cnt (cadr rec)))
    (setq total-product-mm (+ total-product-mm (* (car rec) (cadr rec))))
  )
  (setq kpd (if (> stock-total-mm 0)
              (* 100.0 (/ total-product-mm stock-total-mm))
              0.0))
  (princ (strcat "\nВсего изделий: " (itoa total-cnt) " шт, погонаж "
                 (rtos (/ total-product-mm 1000.0) 2 2) " м.п."))
  (princ (strcat "\nХлыстов: " (itoa num-bars) " шт, общий погонаж "
                 (rtos (/ stock-total-mm 1000.0) 2 2) " м.п."))
  (princ (strcat "\nКПД использования: " (rtos kpd 2 1) " %"))

  (setq color-map (n1-build-color-map pieces))

  ;; CSV — сохраняем независимо от визуализации
  (n1-write-csv bars stock kerf)

  (setq insPt (getpoint "\nУкажите точку вставки визуализации: "))
  (if insPt
    (progn
      ;; курсивный стиль
      (n1-ensure-italic-style)

      ;; имя блока: Раскрой_<имя_файла>
      (setq baseName (vl-filename-base (getvar "DWGNAME")))
      (setq blockName (n1-unique-block-name (strcat "Раскрой " baseName)))

      ;; маркер до отрисовки
      (setq lastEnt (entlast))

      ;; рисуем всё в точке вставки
      (setq bbox1 (n1-draw-layout bars stock kerf insPt color-map))
      (setq barHeight (/ stock 30.0))
      (setq sumInsPt (list (+ (car (cadr bbox1)) (* barHeight 2.0))
                           (cadr (cadr bbox1))))
      (setq bbox2 (n1-draw-summary bars pieces stock sumInsPt color-map))

      ;; собираем все созданные объекты
      (setq ssNew (ssadd))
      (if lastEnt
        (setq ent (entnext lastEnt))
        (setq ent (entnext))
      )
      (while ent
        (ssadd ent ssNew)
        (setq ent (entnext ent))
      )

      ;; создаём блок командой -BLOCK и сразу вставляем его
      (if (> (sslength ssNew) 0)
        (progn
          (setq oldEcho (getvar "CMDECHO"))
          (setvar "CMDECHO" 0)
          (command "._-BLOCK" blockName insPt ssNew "")
          (setvar "CMDECHO" oldEcho)
          (if (tblsearch "BLOCK" blockName)
            (progn
              ;; -BLOCK создаёт определение; явно вставляем вхождение в точку
              (n1-block-insert blockName insPt)
              (princ (strcat "\nСоздан и вставлен блок: " blockName))
            )
            (princ "\nНе удалось создать блок.")
          )
        )
        (princ "\nНет объектов для создания блока.")
      )

      ;; зум к результату
      (setq bbox (n1-combine-bbox bbox1 bbox2))
      (setq p1 (vlax-3d-point (list (car (car bbox)) (cadr (car bbox)) 0.0)))
      (setq p2 (vlax-3d-point (list (car (cadr bbox)) (cadr (cadr bbox)) 0.0)))
      (vl-catch-all-apply 'vla-ZoomWindow (list (vlax-get-acad-object) p1 p2))
    )
    (princ "\nВизуализация пропущена.")
  )

  (princ)
)

(princ "\nNEST1DS.LSP загружен. Команда: NEST1DS")
(princ)