;;; ============================================================
;;; CUTLINE.LSP — модуль линейного раскроя мерного материала
;;; Команды: CUTLINE, РАСКРОЙХЛЫСТА
;;; Объекты: LWPOLYLINE, POLYLINE, LINE, ARC, ELLIPSE, SPLINE, MLINE
;;; Алгоритм: First-Fit Decreasing (FFD)
;;
;;; ФУНКЦИОНАЛЬНОСТЬ:
;;;   - Извлечение длин из линий, полилиний, мультилиний
;;;   - Алгоритм FFD (First-Fit Decreasing)
;;;   - Отбрасывание деталей длиннее хлыста
;;;   - Раскладка в блок AutoCAD с Undo-группой
;;;   - Экспорт в XLS (основной) / CSV (резервный)
;;;   - Блок отчёта с итогами и секцией неразмещённых деталей
;;;   - Управление через галки диспетчера (флаги)
;;;
;;; ИСПРАВЛЕНИЯ:
;;;   A4: отбрасывание деталей длиннее хлыста + отображение в отчётах
;;;   A5: *error* handler + Undo-группа + восстановление CMDECHO
;;;   B1: расчёт длины MLINE через DXF 71 (число полос)
;;;   B3: нормальное имя файла + проверка записи
;;;   + оптимизация cons/reverse в циклах
;;;   + форматирование длин без разделителей тысяч
;;;   + блок отчёта в XLS с рамкой по периметру
;;;   + длина хлыста в блоке отчёта
;;;   + пустая строка между блоками отчёта и неразмещённых
;;;   + оформление заголовка НЕРАЗМЕЩЕННЫЕ как у ОТЧЁТ (красный шрифт)
;;;   + русская команда РАСКРОЙХЛЫСТА
;;;   + флаги управления из диспетчера
;;; ============================================================
(vl-load-com)

;; ================= Настройки =================
(setq *NEST-TRANSPARENCY* 70)
(setq *NEST-PALETTE* '(1 2 3 4 5 6 30 210 140 90))
(setq *NEST-STYLE-NAME* "Раскрой Italic")
(setq *NEST-ITALIC-ANGLE* 0.26)
(setq *NEST-TEXT-STYLE* nil)
(setq *NEST-COLOR-OUTLINE* 7)
(setq *NEST-COLOR-LABEL*   7)
(setq *NEST-COLOR-WASTE*   8)
(setq *NEST-COLOR-TITLE*   5)
(setq *NEST-COLOR-HEADER*  3)
(setq *NEST-COLOR-VALUE*   7)
(setq *NEST-COLOR-KPD*     1)
(setq *NEST-COLOR-SKIP*    1)

;; ============================================================
;; Флаги управления экспортом (устанавливаются из диспетчера)
;; По умолчанию — всё включено (для автономного запуска)
;; ============================================================
(if (not (boundp '*CUTLINE-CREATE-TABLE*))
  (setq *CUTLINE-CREATE-TABLE* T)
)
(if (not (boundp '*CUTLINE-CREATE-XLS*))
  (setq *CUTLINE-CREATE-XLS* T)
)
;; =============================================

;; ---------- Прозрачность для DXF 440 ----------
(defun n1-trans-value (percent)
  (fix (* 255.0 (/ (- 100.0 (float percent)) 100.0)))
)

;; ---------- Создание курсивного стиля на базе Arial ----------
(defun n1-ensure-italic-style ( / result)
  (if (tblsearch "STYLE" *NEST-STYLE-NAME*)
    (progn
      (setq *NEST-TEXT-STYLE* *NEST-STYLE-NAME*)
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
        (setq *NEST-TEXT-STYLE* *NEST-STYLE-NAME*)
        (setq *NEST-TEXT-STYLE* nil)
      )
    )
  )
)

;; ---------- Генерация уникального имени блока ----------
(defun n1-unique-block-name (base / name n)
  (setq n 0)
  (setq name (strcat base " " (itoa n)))
  (while (tblsearch "BLOCK" name)
    (setq n (1+ n))
    (setq name (strcat base " " (itoa n)))
  )
  name
)

;; ---------- Вставка блока ----------
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

;; ---------- Карта цветов для разных длин ----------
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

;; ---------- Текст ----------
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

;; ---------- Линия ----------
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

;; ---------- Прямоугольник ----------
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

;; ---------- Штриховка ----------
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

;; ============================================================
;; Оптимизация: разворачивание групп в список (cons/reverse)
;; ============================================================
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
      (setq out (cons len out))
      (setq i (1+ i))
    )
  )
  (reverse out)
)

;; ============================================================
;; Оптимизация: замена элемента в списке (cons/reverse)
;; ============================================================
(defun n1-replace-nth (lst idx new / i out)
  (setq i 0 out '())
  (foreach x lst
    (if (= i idx)
      (setq out (cons new out))
      (setq out (cons x out))
    )
    (setq i (1+ i))
  )
  (reverse out)
)

;; ============================================================
;; FFD: First-Fit Decreasing
;; Деталь длиннее хлыста НЕ размещается (дополнительная защита)
;; ============================================================
(defun n1-ffd (sorted-pieces stock kerf / bars p placed j bar newbar skip)
  (setq bars '())
  (setq skip 0)
  (foreach p sorted-pieces
    (if (> p stock)
      (setq skip (1+ skip))
      (progn
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
    )
  )
  (if (> skip 0)
    (princ (strcat "\nВНИМАНИЕ: в FFD пропущено деталей длиннее хлыста: " (itoa skip)))
  )
  bars
)

;; ============================================================
;; Длина мультилинии: делитель — число полос стиля (DXF 71)
;; 72 используем как запасной вариант
;; ============================================================
(defun n1-mline-length (ent / obj numEl copyObj arr safe sub elen total data)
  (setq data (entget ent))
  (setq numEl (cdr (assoc 71 data)))
  (if (or (null numEl) (< numEl 1))
    (setq numEl (cdr (assoc 72 data)))
  )
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

;; ============================================================
;; Преобразование списка длин в строку
;; ВАЖНО: использует (itoa (fix x)) — без разделителей тысяч
;; ============================================================
(defun n1-list-to-str (lst / s x)
  (setq s "")
  (foreach x lst
    (setq s (strcat s (if (= s "") "" " ") (itoa (fix x))))
  )
  s
)

;; ---------- Вывод раскладки (по хлыстам) ----------
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

    (setq sp (cond ((< i 10)   "   ")
                   ((< i 100)  "  ")
                   (t           " ")))
    (n1-draw-text (list (- x0 (* barHeight 2.2) 100.0) (+ y0 (* barHeight 0.35))) txtH
                  (strcat "Хлыст " (itoa i) sp "[" (rtos util 2 1) "%]") *NEST-COLOR-LABEL*)

    (setq curx x0)
    (foreach p pieces
      (setq str (itoa (fix p)))
      (setq halfw (* (strlen str) txtH 0.4))
      (setq col (n1-get-color color-map p))
      (n1-draw-text (list (+ curx (* p 0.5) (- halfw)) (+ y0 (* barHeight 0.35))) txtH str col)
      (setq curx (+ curx p kerf))
    )

    (if (> waste 0.0)
      (progn
        (setq str (strcat "Отход " (itoa (fix waste))))
        (setq halfw (* (strlen str) txtH 0.4))
        (n1-draw-text (list (+ (- (+ x0 stock) waste) (* waste 0.5) (- halfw))
                            (+ y0 (* barHeight 0.35))) txtH str *NEST-COLOR-WASTE*)
      )
    )

    (setq y0 (- y0 barHeight gap))
  )
  (list (list (- x0 (* barHeight 3.0) 100.0) miny) (list (+ x0 stock) maxy))
)

;; ============================================================
;; Сводная таблица с секцией неразмещённых деталей
;; ============================================================
(defun n1-draw-summary (bars pieces oversized stock insPt color-map /
    barHeight th rowH pad col1W col2W col3W tableW tableH
    left top x1 x2 x3 y bottom
    num-bars stock-total-mm stock-total-m
    total-cnt total-product-mm total-product-m kpd rec
    total-skip-cnt total-skip-mm skip-rows)
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
  (setq total-skip-cnt 0 total-skip-mm 0.0)
  (if oversized
    (foreach rec oversized
      (setq total-skip-cnt (+ total-skip-cnt (cadr rec)))
      (setq total-skip-mm (+ total-skip-mm (* (car rec) (cadr rec))))
    )
  )
  (setq skip-rows (if oversized (+ 4.0 (length oversized)) 0.0))

  (setq left (car insPt))
  (setq top (cadr insPt))
  (setq tableH (+ (* pad 2)
                  (* (+ 11.0 (length pieces) skip-rows) rowH)))
  (setq bottom (- top tableH))
  (setq x1 (+ left pad))
  (setq x2 (+ left pad col1W))
  (setq x3 (+ left pad col1W col2W))
  (n1-draw-rect (list left bottom) (list (+ left tableW) top) *NEST-COLOR-OUTLINE*)
  (setq y (- top pad 100.0))
  (n1-draw-text (list x1 y) (* th 1.3) "Раскрой хлыста" *NEST-COLOR-TITLE*)
  (setq y (- y rowH) y (- y (* rowH 0.5)))
  (n1-draw-text (list x1 y) th "Длина" *NEST-COLOR-HEADER*)
  (setq y (- y rowH))
  (n1-draw-text (list x1 y) th "Хлыст, мм" *NEST-COLOR-HEADER*)
  (n1-draw-text (list x2 y) th "Кол-во, шт" *NEST-COLOR-HEADER*)
  (n1-draw-text (list x3 y) th "Сумма, м.п." *NEST-COLOR-HEADER*)
  (setq y (- y rowH))
  (n1-draw-text (list x1 y) th (itoa (fix stock)) *NEST-COLOR-VALUE*)
  (n1-draw-text (list x2 y) th (itoa num-bars) *NEST-COLOR-VALUE*)
  (n1-draw-text (list x3 y) th (rtos stock-total-m 2 2) *NEST-COLOR-VALUE*)
  (setq y (- y rowH) y (- y (* rowH 0.5)))
  (n1-draw-text (list x1 y) th "Изделия" *NEST-COLOR-HEADER*)
  (setq y (- y rowH))
  (n1-draw-text (list x1 y) th "Длина, мм" *NEST-COLOR-HEADER*)
  (n1-draw-text (list x2 y) th "Кол-во, шт" *NEST-COLOR-HEADER*)
  (n1-draw-text (list x3 y) th "Сумма, м.п." *NEST-COLOR-HEADER*)
  (setq y (- y rowH))
  (foreach rec pieces
    (n1-draw-text (list x1 y) th (itoa (fix (car rec))) (n1-get-color color-map (car rec)))
    (n1-draw-text (list x2 y) th (itoa (cadr rec)) *NEST-COLOR-VALUE*)
    (n1-draw-text (list x3 y) th (rtos (/ (* (car rec) (cadr rec)) 1000.0) 2 2) *NEST-COLOR-VALUE*)
    (setq y (- y rowH))
  )
  (setq y (- y (* rowH 0.5)))
  (n1-draw-text (list x1 y) th (strcat "Всего изделий: " (itoa total-cnt) " шт") *NEST-COLOR-VALUE*)
  (setq y (- y rowH))
  (n1-draw-text (list x1 y) th (strcat "Суммарная длина: " (rtos total-product-m 2 2) " м.п.") *NEST-COLOR-VALUE*)
  (setq y (- y rowH))
  (n1-draw-text (list x1 y) (* th 1.2) (strcat "КПД использования: " (rtos kpd 2 1) " %") *NEST-COLOR-KPD*)

  (if oversized
    (progn
      (setq y (- y (* rowH 1.5)))
      (n1-draw-text (list x1 y) (* th 1.2) "НЕРАЗМЕЩЕННЫЕ ДЕТАЛИ (длиннее хлыста)" *NEST-COLOR-SKIP*)
      (setq y (- y rowH))
      (n1-draw-text (list x1 y) th "Длина, мм" *NEST-COLOR-HEADER*)
      (n1-draw-text (list x2 y) th "Кол-во, шт" *NEST-COLOR-HEADER*)
      (n1-draw-text (list x3 y) th "Сумма, м.п." *NEST-COLOR-HEADER*)
      (setq y (- y rowH))
      (foreach rec oversized
        (n1-draw-text (list x1 y) th (itoa (fix (car rec))) *NEST-COLOR-SKIP*)
        (n1-draw-text (list x2 y) th (itoa (cadr rec)) *NEST-COLOR-SKIP*)
        (n1-draw-text (list x3 y) th (rtos (/ (* (car rec) (cadr rec)) 1000.0) 2 2) *NEST-COLOR-SKIP*)
        (setq y (- y rowH))
      )
      (setq y (- y (* rowH 0.5)))
      (n1-draw-text (list x1 y) th (strcat "Всего неразмещенных: " (itoa total-skip-cnt) " шт, "
                                           (rtos (/ total-skip-mm 1000.0) 2 2) " м.п.") *NEST-COLOR-SKIP*)
    )
  )

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
  (princ (strcat "\nКоличество хлыстов: " (itoa (length bars))))
  (setq i 0)
  (foreach bar bars
    (setq i (1+ i))
    (setq pieces (cdr bar))
    (setq waste (car bar))
    (setq used (- stock waste))
    (setq util (* 100.0 (/ used stock)))
    (princ (strcat "\nХлыст " (itoa i) ": " (n1-list-to-str pieces)
                   " | исп. " (rtos used 2 1) " | Отход " (rtos waste 2 1) " | " (rtos util 2 1) "%"))
  )
  (princ)
)

;; ============================================================
;; Экспорт в XLS (XML Spreadsheet) с блоком отчёта
;; Имя файла: <название файла> Раскрой хлыстов.xls
;; ============================================================
(defun n1-write-xls (bars pieces oversized stock kerf /
                     fname f i bar pieces-bar waste used util rec
                     total-cnt total-product-mm num-bars stock-total-mm
                     stock-total-m total-product-m kpd
                     total-skip-cnt total-skip-mm)
  (setq fname (strcat (getvar "DWGPREFIX")
                      (vl-filename-base (getvar "DWGNAME"))
                      " Раскрой хлыстов.xls"))
  (setq f (open fname "w"))
  (if (null f)
    nil
    (progn
      ;; Подсчёт итогов для блока отчёта
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
      (setq total-skip-cnt 0 total-skip-mm 0.0)
      (if oversized
        (foreach rec oversized
          (setq total-skip-cnt (+ total-skip-cnt (cadr rec)))
          (setq total-skip-mm (+ total-skip-mm (* (car rec) (cadr rec))))
        )
      )

      ;; XML заголовок и стили
      (write-line "<?xml version=\"1.0\" encoding=\"windows-1251\"?>" f)
      (write-line "<?mso-application progid=\"Excel.Sheet\"?>" f)
      (write-line "<Workbook xmlns=\"urn:schemas-microsoft-com:office:spreadsheet\"" f)
      (write-line " xmlns:o=\"urn:schemas-microsoft-com:office:office\"" f)
      (write-line " xmlns:x=\"urn:schemas-microsoft-com:office:excel\"" f)
      (write-line " xmlns:ss=\"urn:schemas-microsoft-com:office:spreadsheet\"" f)
      (write-line " xmlns:html=\"http://www.w3.org/TR/REC-html40\">" f)
      (write-line " <Styles>" f)

      (write-line "  <Style ss:ID=\"Default\" ss:Name=\"Normal\">" f)
      (write-line "   <Alignment ss:Vertical=\"Center\"/>" f)
      (write-line "  </Style>" f)

      (write-line "  <Style ss:ID=\"Data\">" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)

      (write-line "  <Style ss:ID=\"Header\">" f)
      (write-line "   <Font ss:Bold=\"1\" ss:Size=\"12\" ss:Underline=\"Single\"/>" f)
      (write-line "   <Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)

      ;; Заголовок блока отчёта (жирный, подчёркнутый, серый фон, толстая рамка)
      (write-line "  <Style ss:ID=\"ReportTitle\">" f)
      (write-line "   <Font ss:Bold=\"1\" ss:Size=\"12\" ss:Underline=\"Single\"/>" f)
      (write-line "   <Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"2\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"2\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"2\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"2\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)

      ;; Метка блока отчёта (толстая граница слева — периметр)
      (write-line "  <Style ss:ID=\"ReportLabel\">" f)
      (write-line "   <Alignment ss:Horizontal=\"Left\" ss:Vertical=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"2\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)

      ;; Значение блока отчёта
      (write-line "  <Style ss:ID=\"ReportValue\">" f)
      (write-line "   <Alignment ss:Horizontal=\"Left\" ss:Vertical=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)

      ;; Пустая ячейка значения (тонкие границы внутри блока)
      (write-line "  <Style ss:ID=\"ReportValueEmpty\">" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)

      ;; Последняя ячейка значения (толстая граница справа — периметр)
      (write-line "  <Style ss:ID=\"ReportValueRight\">" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"2\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)

      ;; КПД (жирный, красный)
      (write-line "  <Style ss:ID=\"ReportKpd\">" f)
      (write-line "   <Font ss:Bold=\"1\" ss:Color=\"#FF0000\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Left\" ss:Vertical=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)

      ;; Пустая ячейка КПД (толстая граница снизу — периметр)
      (write-line "  <Style ss:ID=\"ReportKpdEmpty\">" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"2\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)

      ;; Последняя ячейка КПД (толстые границы снизу и справа — периметр)
      (write-line "  <Style ss:ID=\"ReportKpdRight\">" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"2\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"2\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)

      ;; Заголовок секции неразмещённых (как у ОТЧЁТ, но красный шрифт)
      (write-line "  <Style ss:ID=\"SkipTitle\">" f)
      (write-line "   <Font ss:Bold=\"1\" ss:Size=\"12\" ss:Underline=\"Single\" ss:Color=\"#FF0000\"/>" f)
      (write-line "   <Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"2\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"2\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"2\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"2\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)

      ;; Шапка секции неразмещённых
      (write-line "  <Style ss:ID=\"SkipHeader\">" f)
      (write-line "   <Font ss:Bold=\"1\"/>" f)
      (write-line "   <Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"2\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)

      ;; Пустая ячейка шапки неразмещённых
      (write-line "  <Style ss:ID=\"SkipHeaderEmpty\">" f)
      (write-line "   <Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)

      ;; Последняя ячейка шапки неразмещённых (толстая граница справа)
      (write-line "  <Style ss:ID=\"SkipHeaderRight\">" f)
      (write-line "   <Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"2\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)

      ;; Данные неразмещённых (красный)
      (write-line "  <Style ss:ID=\"SkipData\">" f)
      (write-line "   <Font ss:Color=\"#FF0000\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"2\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)

      ;; Пустая ячейка данных неразмещённых
      (write-line "  <Style ss:ID=\"SkipDataEmpty\">" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)

      ;; Последняя ячейка данных неразмещённых (толстая граница справа)
      (write-line "  <Style ss:ID=\"SkipDataRight\">" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"2\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)

      ;; Итог секции неразмещённых (красный, жирный, толстая рамка снизу)
      (write-line "  <Style ss:ID=\"SkipTotal\">" f)
      (write-line "   <Font ss:Bold=\"1\" ss:Color=\"#FF0000\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Left\" ss:Vertical=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"2\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"2\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"2\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)

      (write-line " </Styles>" f)

      ;; Таблица
      (write-line " <Worksheet ss:Name=\"Раскрой\">" f)
      (write-line "  <Table>" f)
      (write-line "   <Column ss:Width=\"60\"/>" f)
      (write-line "   <Column ss:Width=\"200\"/>" f)
      (write-line "   <Column ss:Width=\"100\"/>" f)
      (write-line "   <Column ss:Width=\"80\"/>" f)
      (write-line "   <Column ss:Width=\"100\"/>" f)

      ;; Заголовок таблицы
      (write-line "   <Row ss:Height=\"20\">" f)
      (write-line "    <Cell ss:StyleID=\"Header\" ss:MergeAcross=\"4\"><Data ss:Type=\"String\">Раскрой хлыстов</Data></Cell>" f)
      (write-line "   </Row>" f)

      ;; Шапка колонок
      (write-line "   <Row>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Хлыст</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Детали</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Использовано</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Отход</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Использование_%</Data></Cell>" f)
      (write-line "   </Row>" f)

      ;; Данные по хлыстам
      (setq i 0)
      (foreach bar bars
        (setq i (1+ i))
        (setq pieces-bar (cdr bar))
        (setq waste (car bar))
        (setq used (- stock waste))
        (setq util (* 100.0 (/ used stock)))
        (write-line "   <Row>" f)
        (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (itoa i) "</Data></Cell>") f)
        (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"String\">" (n1-list-to-str pieces-bar) "</Data></Cell>") f)
        (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (rtos used 2 1) "</Data></Cell>") f)
        (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (rtos waste 2 1) "</Data></Cell>") f)
        (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (rtos util 2 1) "</Data></Cell>") f)
        (write-line "   </Row>" f)
      )

      ;; Пропуск одной строки
      (write-line "   <Row>" f)
      (write-line "    <Cell><Data ss:Type=\"String\"></Data></Cell>" f)
      (write-line "   </Row>" f)

      ;; ================================================
      ;; Блок отчёта
      ;; ================================================

      ;; Заголовок блока отчёта
      (write-line "   <Row>" f)
      (write-line "    <Cell ss:StyleID=\"ReportTitle\" ss:MergeAcross=\"4\"><Data ss:Type=\"String\">ОТЧЁТ</Data></Cell>" f)
      (write-line "   </Row>" f)

      ;; Длина хлыста (заготовка для раскроя)
      (write-line "   <Row>" f)
      (write-line "    <Cell ss:StyleID=\"ReportLabel\"><Data ss:Type=\"String\">Длина хлыста (заготовка):</Data></Cell>" f)
      (write-line (strcat "    <Cell ss:StyleID=\"ReportValue\"><Data ss:Type=\"String\">" (itoa (fix stock)) " мм</Data></Cell>") f)
      (write-line "    <Cell ss:StyleID=\"ReportValueEmpty\"><Data ss:Type=\"String\"></Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"ReportValueEmpty\"><Data ss:Type=\"String\"></Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"ReportValueRight\"><Data ss:Type=\"String\"></Data></Cell>" f)
      (write-line "   </Row>" f)

      ;; Всего изделий
      (write-line "   <Row>" f)
      (write-line "    <Cell ss:StyleID=\"ReportLabel\"><Data ss:Type=\"String\">Всего изделий:</Data></Cell>" f)
      (write-line (strcat "    <Cell ss:StyleID=\"ReportValue\"><Data ss:Type=\"String\">" (itoa total-cnt) " шт</Data></Cell>") f)
      (write-line "    <Cell ss:StyleID=\"ReportValueEmpty\"><Data ss:Type=\"String\"></Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"ReportValueEmpty\"><Data ss:Type=\"String\"></Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"ReportValueRight\"><Data ss:Type=\"String\"></Data></Cell>" f)
      (write-line "   </Row>" f)

      ;; Суммарная длина
      (write-line "   <Row>" f)
      (write-line "    <Cell ss:StyleID=\"ReportLabel\"><Data ss:Type=\"String\">Суммарная длина:</Data></Cell>" f)
      (write-line (strcat "    <Cell ss:StyleID=\"ReportValue\"><Data ss:Type=\"String\">" (rtos total-product-m 2 2) " м.п.</Data></Cell>") f)
      (write-line "    <Cell ss:StyleID=\"ReportValueEmpty\"><Data ss:Type=\"String\"></Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"ReportValueEmpty\"><Data ss:Type=\"String\"></Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"ReportValueRight\"><Data ss:Type=\"String\"></Data></Cell>" f)
      (write-line "   </Row>" f)

      ;; Хлыстов
      (write-line "   <Row>" f)
      (write-line "    <Cell ss:StyleID=\"ReportLabel\"><Data ss:Type=\"String\">Хлыстов:</Data></Cell>" f)
      (write-line (strcat "    <Cell ss:StyleID=\"ReportValue\"><Data ss:Type=\"String\">" (itoa num-bars) " шт</Data></Cell>") f)
      (write-line "    <Cell ss:StyleID=\"ReportValueEmpty\"><Data ss:Type=\"String\"></Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"ReportValueEmpty\"><Data ss:Type=\"String\"></Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"ReportValueRight\"><Data ss:Type=\"String\"></Data></Cell>" f)
      (write-line "   </Row>" f)

      ;; Общая длина хлыстов
      (write-line "   <Row>" f)
      (write-line "    <Cell ss:StyleID=\"ReportLabel\"><Data ss:Type=\"String\">Общая длина хлыстов:</Data></Cell>" f)
      (write-line (strcat "    <Cell ss:StyleID=\"ReportValue\"><Data ss:Type=\"String\">" (rtos stock-total-m 2 2) " м.п.</Data></Cell>") f)
      (write-line "    <Cell ss:StyleID=\"ReportValueEmpty\"><Data ss:Type=\"String\"></Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"ReportValueEmpty\"><Data ss:Type=\"String\"></Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"ReportValueRight\"><Data ss:Type=\"String\"></Data></Cell>" f)
      (write-line "   </Row>" f)

      ;; КПД использования
      (write-line "   <Row>" f)
      (write-line "    <Cell ss:StyleID=\"ReportLabel\"><Data ss:Type=\"String\">КПД использования:</Data></Cell>" f)
      (write-line (strcat "    <Cell ss:StyleID=\"ReportKpd\"><Data ss:Type=\"String\">" (rtos kpd 2 1) " %</Data></Cell>") f)
      (write-line "    <Cell ss:StyleID=\"ReportKpdEmpty\"><Data ss:Type=\"String\"></Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"ReportKpdEmpty\"><Data ss:Type=\"String\"></Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"ReportKpdRight\"><Data ss:Type=\"String\"></Data></Cell>" f)
      (write-line "   </Row>" f)

      ;; ================================================
      ;; Секция неразмещённых деталей (если есть)
      ;; ================================================
      (if oversized
        (progn
          ;; Пустая строка между блоком отчёта и секцией неразмещённых
          (write-line "   <Row>" f)
          (write-line "    <Cell><Data ss:Type=\"String\"></Data></Cell>" f)
          (write-line "   </Row>" f)

          ;; Заголовок секции неразмещённых
          (write-line "   <Row>" f)
          (write-line "    <Cell ss:StyleID=\"SkipTitle\" ss:MergeAcross=\"4\"><Data ss:Type=\"String\">НЕРАЗМЕЩЕННЫЕ ДЕТАЛИ (длиннее хлыста)</Data></Cell>" f)
          (write-line "   </Row>" f)

          ;; Шапка секции
          (write-line "   <Row>" f)
          (write-line "    <Cell ss:StyleID=\"SkipHeader\"><Data ss:Type=\"String\">Длина, мм</Data></Cell>" f)
          (write-line "    <Cell ss:StyleID=\"SkipHeader\"><Data ss:Type=\"String\">Кол-во, шт</Data></Cell>" f)
          (write-line "    <Cell ss:StyleID=\"SkipHeader\"><Data ss:Type=\"String\">Сумма, м.п.</Data></Cell>" f)
          (write-line "    <Cell ss:StyleID=\"SkipHeaderEmpty\"><Data ss:Type=\"String\"></Data></Cell>" f)
          (write-line "    <Cell ss:StyleID=\"SkipHeaderRight\"><Data ss:Type=\"String\"></Data></Cell>" f)
          (write-line "   </Row>" f)

          ;; Данные неразмещённых
          (foreach rec oversized
            (write-line "   <Row>" f)
            (write-line (strcat "    <Cell ss:StyleID=\"SkipData\"><Data ss:Type=\"Number\">" (itoa (fix (car rec))) "</Data></Cell>") f)
            (write-line (strcat "    <Cell ss:StyleID=\"SkipData\"><Data ss:Type=\"Number\">" (itoa (cadr rec)) "</Data></Cell>") f)
            (write-line (strcat "    <Cell ss:StyleID=\"SkipData\"><Data ss:Type=\"Number\">" (rtos (/ (* (car rec) (cadr rec)) 1000.0) 2 2) "</Data></Cell>") f)
            (write-line "    <Cell ss:StyleID=\"SkipDataEmpty\"><Data ss:Type=\"String\"></Data></Cell>" f)
            (write-line "    <Cell ss:StyleID=\"SkipDataRight\"><Data ss:Type=\"String\"></Data></Cell>" f)
            (write-line "   </Row>" f)
          )

          ;; Итог секции неразмещённых
          (write-line "   <Row>" f)
          (write-line (strcat "    <Cell ss:StyleID=\"SkipTotal\" ss:MergeAcross=\"4\"><Data ss:Type=\"String\">Всего неразмещенных: " (itoa total-skip-cnt) " шт, " (rtos (/ total-skip-mm 1000.0) 2 2) " м.п.</Data></Cell>") f)
          (write-line "   </Row>" f)
        )
      )

      (write-line "  </Table>" f)
      (write-line " </Worksheet>" f)
      (write-line "</Workbook>" f)
      (close f)
      (princ (strcat "\nXLS сохранен: " fname))
      T
    )
  )
)

;; ============================================================
;; Экспорт в CSV (резервный вариант)
;; ============================================================
(defun n1-write-csv (bars pieces oversized stock kerf / fname f i bar pieces-bar waste used util rec)
  (setq fname (strcat (getvar "DWGPREFIX")
                      (vl-filename-base (getvar "DWGNAME"))
                      " Раскрой хлыстов.csv"))
  (setq f (open fname "w"))
  (if f
    (progn
      (write-line "Хлыст;Детали;Использовано;Отход;Использование_%" f)
      (setq i 0)
      (foreach bar bars
        (setq i (1+ i))
        (setq pieces-bar (cdr bar))
        (setq waste (car bar))
        (setq used (- stock waste))
        (setq util (* 100.0 (/ used stock)))
        (write-line (strcat (itoa i) ";" (n1-list-to-str pieces-bar) ";" (rtos used 2 1) ";"
                            (rtos waste 2 1) ";" (rtos util 2 1)) f)
      )

      (if oversized
        (progn
          (write-line "" f)
          (write-line "НЕРАЗМЕЩЕННЫЕ ДЕТАЛИ (длиннее хлыста)" f)
          (write-line "Длина, мм;Кол-во, шт;Сумма, м.п." f)
          (foreach rec oversized
            (write-line (strcat (itoa (fix (car rec))) ";"
                                (itoa (cadr rec)) ";"
                                (rtos (/ (* (car rec) (cadr rec)) 1000.0) 2 2)) f)
          )
        )
      )

      (close f)
      (princ (strcat "\nCSV сохранен: " fname))
    )
    (princ (strcat "\nОШИБКА: не удалось создать файл " fname))
  )
)

;; ============================================================
;; Экспорт с переключением: XLS ? CSV (если файл открыт)
;; ============================================================
(defun n1-export-report (bars pieces oversized stock kerf)
  (if (n1-write-xls bars pieces oversized stock kerf)
    T
    (progn
      (princ "\nНе удалось сохранить XLS. Сохраняю CSV...")
      (n1-write-csv bars pieces oversized stock kerf)
    )
  )
)

;; ============================================================
;; Главная команда
;; ============================================================
(defun c:cutline ( / *error*
                    ss tol stock kerf insPt pieces sorted bars
                    bbox1 bbox2 bbox p1 p2 color-map
                    barHeight sumInsPt num-bars stock-total-mm
                    total-cnt total-product-mm kpd rec blockName baseName
                    lastEnt ssNew ent oldEcho doc uMark
                    oversized valid-pieces)
  ;; Обработчик ошибок
  (defun *error* (msg)
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (if (and uMark doc)
      (vl-catch-all-apply 'vla-EndUndoMark (list doc))
    )
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\nОшибка: " msg))
    )
    (princ)
  )

  (princ "\n=== Линейный раскрой мерного материала ===")
  (princ "\nВыберите полилинии/линии - исходные детали:")
  (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE,LINE,ARC,ELLIPSE,SPLINE,MLINE"))))
  (if (null ss) (progn (princ "\nНичего не выбрано.") (princ) (exit)))
  (princ (strcat "\nВыбрано объектов: " (itoa (sslength ss))))
  (setq tol (getreal "\nДопуск округления длины (мм) <1>: "))
  (if (or (null tol) (<= tol 0.0)) (setq tol 1.0))
  (setq stock (getreal "\nДлина хлыста (мм) <6000>: "))
  (if (null stock) (setq stock 6000.0))
  (setq kerf (getreal "\nШирина реза (мм) <0>: "))
  (if (null kerf) (setq kerf 0.0))

  (setq pieces (n1-extract-pieces ss tol))
  (if (null pieces) (progn (princ "\nНе удалось извлечь длины.") (princ) (exit)))

  ;; Отбрасывание деталей длиннее хлыста
  (setq oversized '()
        valid-pieces '())
  (foreach rec pieces
    (if (> (car rec) stock)
      (setq oversized (append oversized (list rec)))
      (setq valid-pieces (append valid-pieces (list rec)))
    )
  )
  (setq pieces valid-pieces)

  ;; Вывод предупреждения о неразмещённых деталях
  (if oversized
    (progn
      (princ (strcat "\nВНИМАНИЕ: " (itoa (length oversized))
                     " типов деталей длиннее хлыста НЕ размещены:"))
      (foreach rec oversized
        (princ (strcat "\n  Длина " (itoa (fix (car rec))) " мм, кол-во "
                       (itoa (cadr rec)) " шт."))
      )
    )
  )

  ;; Проверка, что остались детали для раскроя
  (if (null pieces)
    (progn
      (princ "\nВсе детали длиннее хлыста. Раскрой невозможен.")
      (princ)
      (exit)
    )
  )

  ;; Подсчёт общего количества деталей
  (setq total-cnt 0)
  (foreach rec pieces (setq total-cnt (+ total-cnt (cadr rec))))
  (princ (strcat "\nВсего типов деталей: " (itoa (length pieces))
                 ", общее количество: " (itoa total-cnt)))

  (setq sorted (n1-expand pieces))
  (princ (strcat "\nРазвернуто элементов: " (itoa (length sorted))))
  (setq bars (n1-ffd sorted stock kerf))
  (princ (strcat "\nПолучено хлыстов: " (itoa (length bars))))
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
  (princ (strcat "\nВсего изделий: " (itoa total-cnt) " шт, суммарная длина "
                 (rtos (/ total-product-mm 1000.0) 2 2) " м.п."))
  (princ (strcat "\nХлыстов: " (itoa num-bars) " шт, общая длина "
                 (rtos (/ stock-total-mm 1000.0) 2 2) " м.п."))
  (princ (strcat "\nКПД использования: " (rtos kpd 2 1) " %"))

  (setq color-map (n1-build-color-map pieces))

  ;; ============================================================
  ;; Экспорт с переключением XLS ? CSV (если включён в диспетчере)
  ;; ============================================================
  (if *CUTLINE-CREATE-XLS*
    (n1-export-report bars pieces oversized stock kerf)
    (princ "\nЭкспорт в XLS/CSV отключён.")
  )

  ;; ============================================================
  ;; Раскладка в автокад (если включена в диспетчере)
  ;; ============================================================
  (setq insPt (if *CUTLINE-CREATE-TABLE*
                (getpoint "\nУкажите точку вставки раскладки: ")
                nil))
  (if insPt
    (progn
      (n1-ensure-italic-style)

      (setq baseName (vl-filename-base (getvar "DWGNAME")))
      (setq blockName (n1-unique-block-name (strcat "Раскрой " baseName)))

      ;; Undo-группа для атомарности
      (setq doc (vl-catch-all-apply 'vla-get-ActiveDocument
                                    (list (vlax-get-acad-object))))
      (if (and (not (vl-catch-all-error-p doc)) doc)
        (progn
          (vla-StartUndoMark doc)
          (setq uMark T)
        )
      )

      (setq lastEnt (entlast))

      ;; Рисуем раскладку и сводку
      (setq bbox1 (n1-draw-layout bars stock kerf insPt color-map))
      (setq barHeight (/ stock 30.0))
      (setq sumInsPt (list (+ (car (cadr bbox1)) (* barHeight 2.0))
                           (cadr (cadr bbox1))))
      (setq bbox2 (n1-draw-summary bars pieces oversized stock sumInsPt color-map))

      ;; Собираем созданные объекты в набор
      (setq ssNew (ssadd))
      (if lastEnt
        (setq ent (entnext lastEnt))
        (setq ent (entnext))
      )
      (while ent
        (ssadd ent ssNew)
        (setq ent (entnext ent))
      )

      ;; Создаём блок
      (if (> (sslength ssNew) 0)
        (progn
          (setq oldEcho (getvar "CMDECHO"))
          (setvar "CMDECHO" 0)
          (command "._-BLOCK" blockName insPt ssNew "")
          (setvar "CMDECHO" oldEcho)
          (setq oldEcho nil)
          (if (tblsearch "BLOCK" blockName)
            (progn
              (n1-block-insert blockName insPt)
              (princ (strcat "\nСоздан блок с раскладкой: " blockName))
            )
            (princ "\nНе удалось создать блок.")
          )
        )
        (princ "\nНет объектов для создания блока.")
      )

      ;; Область на экране
      (setq bbox (n1-combine-bbox bbox1 bbox2))
      (setq p1 (vlax-3d-point (list (car (car bbox)) (cadr (car bbox)) 0.0)))
      (setq p2 (vlax-3d-point (list (car (cadr bbox)) (cadr (cadr bbox)) 0.0)))
      (vl-catch-all-apply 'vla-ZoomWindow (list (vlax-get-acad-object) p1 p2))

      ;; Закрываем undo-группу
      (if (and uMark doc)
        (progn
          (vla-EndUndoMark doc)
          (setq uMark nil)
        )
      )
    )
    (if (not *CUTLINE-CREATE-TABLE*)
      (princ "\nРаскладка в автокад отключена.")
      (princ "\nРаскладка пропущена.")
    )
  )

  (princ)
)

;; ============================================================
;; Обёртка для запуска из диспетчера
;; ============================================================
(defun cutline-main ()
  (c:cutline)
)

;; ============================================================
;; Русская команда-обёртка
;; ============================================================
(defun c:раскройхлыста ()
  (c:cutline)
)

(princ "\nCUTLINE.LSP загружен. Команды: CUTLINE, РАСКРОЙХЛЫСТА")
(princ)