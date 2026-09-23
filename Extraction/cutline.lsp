;;; ============================================================
;;; CUTLINE.LSP — модуль линейного раскроя мерного материала
;;; Команда: CUTLINE / РАСКРОЙХЛЫСТА
;;; Объекты: LINE, MLINE, динамические блоки со свойством "Длина"
;;; Алгоритм: First-Fit Decreasing (FFD)
;;;
;;; ЗАЛИВКА ДЕТАЛЕЙ:
;;;   - SOLID + ActiveX EntityTransparency (0..90%)
;;;   - TRANSPARENCYDISPLAY включается автоматически
;;;
;;; ШАПКА КАРТЫ РАСКРОЯ:
;;;   - заголовок, подзаголовок, линейка с метками 0 и stock
;;;   - рисуется над первым хлыстом
;;; ============================================================
(vl-load-com)

;; ================= Константы =================
(setq *CUTLINE-MIN-LENGTH*   100.0)
(setq *CUTLINE-MAX-LENGTH* 500000.0)
(setq *CUTLINE-DEFAULT-TOL*    1.0)
(setq *CUTLINE-DEFAULT-STOCK* 6000.0)
(setq *CUTLINE-DEFAULT-KERF*   0.0)

;; ЗАЛИВКА: прозрачность 0.0..1.0 (где 1.0 = 90% прозрачности)
(setq *CUTLINE-PART-TRANSPARENCY* 0.70)
(setq *CUTLINE-WASTE-TRANSPARENCY* 0.3)

(setq *NEST-PALETTE* '(1 2 3 4 5 6 30 210 140 90))
(setq *NEST-STYLE-NAME* "Раскрой Italic")
(setq *NEST-ITALIC-ANGLE* 0.26)
(setq *NEST-TEXT-STYLE* nil)
;; Жирный стиль текста
(setq *NEST-BOLD-STYLE-NAME* "Основной стиль (надписи без наклона)")
(setq *NEST-BOLD-TEXT-STYLE* nil)
(setq *NEST-COLOR-OUTLINE* 7)
(setq *NEST-COLOR-LABEL*   7)
(setq *NEST-COLOR-WASTE*   8)
(setq *NEST-COLOR-TITLE*   5)
(setq *NEST-COLOR-HEADER*  3)
(setq *NEST-COLOR-VALUE*   7)
(setq *NEST-COLOR-KPD*     1)
;; Цвет текста длины детали (желтый)
(setq *NEST-COLOR-PART-TEXT* 2)

;; ================= ПАРАМЕТРЫ ШАПКИ КАРТЫ РАСКРОЯ =================
(setq *CUTLINE-ROW-H*          200.0)   ;; высота строки шапки
(setq *CUTLINE-TEXT-H*          75.0)   ;; высота подзаголовка и меток
(setq *CUTLINE-TITLE-H*        100.0)   ;; высота заголовка
(setq *CUTLINE-OUTLINE-COLOR*     7)    ;; цвет линейки
(setq *CUTLINE-TITLE-COLOR*       5)    ;; цвет заголовка
(setq *CUTLINE-HEADER-COLOR*      3)    ;; цвет подзаголовка
(setq *CUTLINE-VALUE-COLOR*       7)    ;; цвет меток линейки
;; ====================================================================

;; ================= РАМКА ВОКРУГ КАРТЫ РАСКРОЯ =================
(setq *CUTLINE-FRAME-LAYER*       "Невидимые")   ;; слой рамки
(setq *CUTLINE-FRAME-PAD-LEFT*     300.0)        ;; отступ слева (мм)
(setq *CUTLINE-FRAME-PAD-RIGHT*    125.0)        ;; отступ справа
(setq *CUTLINE-FRAME-PAD-TOP*      125.0)        ;; отступ сверху
(setq *CUTLINE-FRAME-PAD-BOTTOM*   200.0)        ;; отступ снизу
;; =================================================================

(if (not (boundp '*n1-tmp-choice*))
  (setq *n1-tmp-choice* 'ALL))

(if (not (boundp '*n1-tmp-stock*))   (setq *n1-tmp-stock* *CUTLINE-DEFAULT-STOCK*))
(if (not (boundp '*n1-tmp-kerf*))    (setq *n1-tmp-kerf*  *CUTLINE-DEFAULT-KERF*))
(if (not (boundp '*n1-tmp-chk-xls*)) (setq *n1-tmp-chk-xls* T))
(if (not (boundp '*n1-tmp-chk-acad*)) (setq *n1-tmp-chk-acad* T))

(if (not (boundp '*n1-tmp-dynblock-type*))
  (setq *n1-tmp-dynblock-type* "")
)

(if (not (boundp '*n1-dynblock-types-list*))
  (setq *n1-dynblock-types-list* '())
)

(if (not (boundp '*n1-tmp-mline-type*))
  (setq *n1-tmp-mline-type* "")
)

(if (not (boundp '*n1-mline-types-list*))
  (setq *n1-mline-types-list* '())
)

(if (not (boundp '*n1-cutline-ss*))
  (setq *n1-cutline-ss* nil)
)

(if (not (boundp '*CUTLINE-LAST-STOCK*)) (setq *CUTLINE-LAST-STOCK* *CUTLINE-DEFAULT-STOCK*))
(if (not (boundp '*CUTLINE-LAST-KERF*))  (setq *CUTLINE-LAST-KERF*  *CUTLINE-DEFAULT-KERF*))
(if (not (boundp '*CUTLINE-LAST-XLS*))   (setq *CUTLINE-LAST-XLS*   T))
(if (not (boundp '*CUTLINE-LAST-ACAD*))  (setq *CUTLINE-LAST-ACAD*  T))

;; ---------- Утилиты ----------
;; Этап 2 (V5): лимит количества деталей - защита от зависания
;; при ошибочной выборке (x100 объектов). Именованный, изменяемый.
(setq *n1-max-parts* 5000)

;; Этап 2 (V6): страж итераций FFD - верхняя граница числа попыток размещения.
(setq *n1-max-placement-attempts* 1000000)

(defun n1-split-string (str delim / pos result item)
  (setq result '())
  (while (setq pos (vl-string-search delim str))
    (setq item (vl-string-trim " " (substr str 1 pos)))
    (setq result (cons item result))
    (setq str (substr str (+ pos 2)))
  )
  (setq item (vl-string-trim " " str))
  (if (> (strlen item) 0)
    (setq result (cons item result))
  )
  (reverse result)
)

(defun n1-ensure-italic-style ( / result)
  (if (tblsearch "STYLE" *NEST-STYLE-NAME*)
    (progn
      (setq *NEST-TEXT-STYLE* *NEST-STYLE-NAME*)
    )
    (progn
      (setq result
        (entmake (list (cons 0 "STYLE") (cons 2 *NEST-STYLE-NAME*)
                       (cons 70 0) (cons 40 0.0) (cons 41 1.0)
                       (cons 50 *NEST-ITALIC-ANGLE*) (cons 71 0)
                       (cons 42 2.5) (cons 3 "Arial") (cons 4 ""))))
      (if (and result (tblsearch "STYLE" *NEST-STYLE-NAME*))
        (setq *NEST-TEXT-STYLE* *NEST-STYLE-NAME*)
        (setq *NEST-TEXT-STYLE* nil)
      )
    )
  )
)

;; ============================================================
;; СОЗДАНИЕ ЖИРНОГО СТИЛЯ ТЕКСТА
;; "Основной стиль (надписи без наклона)" со шрифтом arialbd.ttf
;; ============================================================
(defun n1-ensure-bold-style ( / result)
  (if (tblsearch "STYLE" *NEST-BOLD-STYLE-NAME*)
    (setq *NEST-BOLD-TEXT-STYLE* *NEST-BOLD-STYLE-NAME*)
    (progn
      (setq result
        (entmake
          (list '(0 . "STYLE")
                '(100 . "AcDbSymbolTableRecord")
                '(100 . "AcDbTextStyleTableRecord")
                (cons 2 *NEST-BOLD-STYLE-NAME*)
                '(70 . 0)
                '(40 . 0.0)
                '(41 . 1.0)
                '(50 . 0.0)
                '(71 . 0)
                '(42 . 2.5)
                '(3 . "arialbd.ttf")
                '(4 . ""))))
      (if (and result (tblsearch "STYLE" *NEST-BOLD-STYLE-NAME*))
        (setq *NEST-BOLD-TEXT-STYLE* *NEST-BOLD-STYLE-NAME*)
        (setq *NEST-BOLD-TEXT-STYLE* nil)
      )
    )
  )
)

(defun n1-unique-block-name (base / name n)
  (setq n 0 name (strcat base " " (itoa n)))
  (while (tblsearch "BLOCK" name)
    (setq n (1+ n) name (strcat base " " (itoa n)))
  )
  name
)

(defun n1-block-insert (name insPt)
  (entmake (list (cons 0 "INSERT") (cons 100 "AcDbEntity")
                 (cons 100 "AcDbBlockReference") (cons 2 name)
                 (cons 10 (list (car insPt) (cadr insPt) 0.0))
                 (cons 41 1.0) (cons 42 1.0) (cons 43 1.0) (cons 50 0.0)))
)

(defun n1-build-color-map (pieces / palette i map rec)
  (setq palette *NEST-PALETTE* i 0 map '())
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

(defun n1-draw-text (pt h str color / style c)
  (setq style (if (and *NEST-TEXT-STYLE* (/= *NEST-TEXT-STYLE* ""))
                *NEST-TEXT-STYLE* (getvar "TEXTSTYLE")))
  (setq c (if (and color (numberp color)) color 7))
  (entmake (list (cons 0 "TEXT") (cons 62 c) (cons 7 style)
                 (cons 10 (list (car pt) (cadr pt) 0.0))
                 (cons 40 h) (cons 1 str) (cons 50 0.0)))
)

;; ============================================================
;; ЖИРНЫЙ ТЕКСТ С ПОВОРОТОМ
;; angle: угол поворота в ГРАДУСАХ (конвертируется в радианы)
;; ============================================================
(defun n1-draw-text-bold (pt h str color angle / c style angle-rad)
  (setq c (if (and color (numberp color)) color 7))
  (n1-ensure-bold-style)
  (setq style (if (and *NEST-BOLD-TEXT-STYLE* (/= *NEST-BOLD-TEXT-STYLE* ""))
                *NEST-BOLD-TEXT-STYLE* (getvar "TEXTSTYLE")))
  (setq angle-rad (if (and angle (numberp angle)) (* angle (/ pi 180.0)) 0.0))
  (entmake (list (cons 0 "TEXT") (cons 62 c) (cons 7 style)
                 (cons 10 (list (car pt) (cadr pt) 0.0))
                 (cons 40 h) (cons 1 str) (cons 50 angle-rad)))
)

;; ============================================================
;; ЖИРНЫЙ ТЕКСТ С ПОВОРОТОМ И ЦЕНТРИРОВАНИЕМ
;; Текст центрируется относительно точки вставки (коды 72=1, 73=2, 11)
;; ============================================================
(defun n1-draw-text-bold-center (pt h str color angle / c style angle-rad)
  (setq c (if (and color (numberp color)) color 7))
  (n1-ensure-bold-style)
  (setq style (if (and *NEST-BOLD-TEXT-STYLE* (/= *NEST-BOLD-TEXT-STYLE* ""))
                *NEST-BOLD-TEXT-STYLE* (getvar "TEXTSTYLE")))
  (setq angle-rad (if (and angle (numberp angle)) (* angle (/ pi 180.0)) 0.0))
  (entmake (list (cons 0 "TEXT") (cons 62 c) (cons 7 style)
                 (cons 10 (list (car pt) (cadr pt) 0.0))
                 (cons 11 (list (car pt) (cadr pt) 0.0))
                 (cons 40 h) (cons 1 str) (cons 50 angle-rad)
                 (cons 72 1)
                 (cons 73 2)))
)

;; ============================================================
;; ТЕКСТ С ВЫРАВНИВАНИЕМ ПО ПРАВОМУ КРАЮ И ВЕРТИКАЛЬНОЙ СЕРЕДИНЕ
;; Используется для меток хлыста слева (не наезжают на хлыст)
;; ============================================================
(defun n1-draw-text-right (pt h str color / c style)
  (setq c (if (and color (numberp color)) color 7))
  (setq style (if (and *NEST-TEXT-STYLE* (/= *NEST-TEXT-STYLE* ""))
                *NEST-TEXT-STYLE* (getvar "TEXTSTYLE")))
  (entmake (list (cons 0 "TEXT") (cons 62 c) (cons 7 style)
                 (cons 10 (list (car pt) (cadr pt) 0.0))
                 (cons 11 (list (car pt) (cadr pt) 0.0))
                 (cons 40 h) (cons 1 str) (cons 50 0.0)
                 (cons 72 2)    ;; горизонтальное: по правому краю
                 (cons 73 2)))  ;; вертикальное: середина
)

;; ============================================================
;; ЖИРНЫЙ ТЕКСТ С ВЫРАВНИВАНИЕМ ПО ПРАВОМУ КРАЮ И ВЕРТИКАЛЬНОЙ СЕРЕДИНЕ
;; Стиль Arial Bold, используется для меток шапки слева
;; ============================================================
(defun n1-draw-text-bold-right (pt h str color angle / c style angle-rad)
  (setq c (if (and color (numberp color)) color 7))
  (n1-ensure-bold-style)
  (setq style (if (and *NEST-BOLD-TEXT-STYLE* (/= *NEST-BOLD-TEXT-STYLE* ""))
                *NEST-BOLD-TEXT-STYLE* (getvar "TEXTSTYLE")))
  (setq angle-rad (if (and angle (numberp angle)) (* angle (/ pi 180.0)) 0.0))
  (entmake (list (cons 0 "TEXT") (cons 62 c) (cons 7 style)
                 (cons 10 (list (car pt) (cadr pt) 0.0))
                 (cons 11 (list (car pt) (cadr pt) 0.0))
                 (cons 40 h) (cons 1 str) (cons 50 angle-rad)
                 (cons 72 2)    ;; горизонтальное: по правому краю
                 (cons 73 2)))  ;; вертикальное: середина
)

;; ============================================================
;; ЖИРНЫЙ ТЕКСТ С ВЫРАВНИВАНИЕМ ПО ЛЕВОМУ КРАЮ И ВЕРТИКАЛЬНОЙ СЕРЕДИНЕ
;; Стиль Arial Bold. Зеркально n1-draw-text-bold-right.
;; Используется для меток справа (правее хлыста / линейки).
;; ============================================================
(defun n1-draw-text-bold-left (pt h str color angle / c style angle-rad)
  (setq c (if (and color (numberp color)) color 7))
  (n1-ensure-bold-style)
  (setq style (if (and *NEST-BOLD-TEXT-STYLE* (/= *NEST-BOLD-TEXT-STYLE* ""))
                *NEST-BOLD-TEXT-STYLE* (getvar "TEXTSTYLE")))
  (setq angle-rad (if (and angle (numberp angle)) (* angle (/ pi 180.0)) 0.0))
  (entmake (list (cons 0 "TEXT") (cons 62 c) (cons 7 style)
                 (cons 10 (list (car pt) (cadr pt) 0.0))
                 (cons 11 (list (car pt) (cadr pt) 0.0))
                 (cons 40 h) (cons 1 str) (cons 50 angle-rad)
                 (cons 72 0)    ;; горизонтальное: по левому краю
                 (cons 73 2)))  ;; вертикальное: середина
)

;; ============================================================
;; ДВОЙНАЯ ВЕРТИКАЛЬНАЯ ЛИНИЯ РЕЗА
;; Рисует две параллельные линии со сдвигом ±offset
;; ============================================================

(defun n1-draw-line (p1 p2 color)
  (entmake (list (cons 0 "LINE") (cons 62 color)
                 (cons 10 (list (car p1) (cadr p1) 0.0))
                 (cons 11 (list (car p2) (cadr p2) 0.0))))
)

(defun n1-draw-rect (p1 p2 color / x1 y1 x2 y2)
  (setq x1 (car p1) y1 (cadr p1) x2 (car p2) y2 (cadr p2))
  (entmake (list (cons 0 "LWPOLYLINE") (cons 100 "AcDbEntity")
                 (cons 62 color) (cons 100 "AcDbPolyline")
                 (cons 90 4) (cons 70 1)
                 (cons 10 (list x1 y1)) (cons 10 (list x2 y1))
                 (cons 10 (list x2 y2)) (cons 10 (list x1 y2))))
)

;; ============================================================
;; ЗАЛИВКА С ПРОЗРАЧНОСТЬЮ
;; SOLID + ActiveX EntityTransparency (0..90)
;; ============================================================

(defun n1-apply-transparency (ent val90 / obj r)
  (if ent
    (progn
      (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
      (if (not (vl-catch-all-error-p obj))
        (progn
          (setq r (vl-catch-all-apply 'vlax-put-property
                    (list obj 'EntityTransparency val90)))
          (if (vl-catch-all-error-p r)
            (vl-catch-all-apply 'vlax-put-property
              (list obj 'Transparency val90)))
        )
      )
    )
  )
  ent
)

(defun n1-draw-filled-rect (p1 p2 color transparency / x1 y1 x2 y2 ent res aci)
  (setq x1 (car p1) y1 (cadr p1) x2 (car p2) y2 (cadr p2))
  (setq aci (fix (+ 0.5 (* transparency 90.0))))
  (if (> aci 90) (setq aci 90))
  (if (< aci 0) (setq aci 0))

  (setq res
    (entmake
      (list
        (cons 0 "SOLID")
        (cons 100 "AcDbEntity")
        (cons 100 "AcDbTrace")
        (cons 10 (list x1 y1 0.0))
        (cons 11 (list x2 y1 0.0))
        (cons 12 (list x1 y2 0.0))
        (cons 13 (list x2 y2 0.0))
        (cons 62 color)
        (cons 39 0.0)
      )
    )
  )

  (if res
    (progn
      (setq ent (entlast))
      (n1-apply-transparency ent aci)
      ent
    )
    nil
  )
)

(defun n1-enable-transparency-display ( / )
  (vl-catch-all-apply 'setvar (list "TRANSPARENCYDISPLAY" 1))
)

(defun n1-disable-transparency-display (old-val / )
  (if (and old-val (numberp old-val))
    (vl-catch-all-apply 'setvar (list "TRANSPARENCYDISPLAY" old-val))
  )
)

;; ============================================================
;; СОЗДАНИЕ СЛОЯ РАМКИ (если не существует)
;; ============================================================
(defun n1-ensure-frame-layer ( / )
  (if (not (tblsearch "LAYER" *CUTLINE-FRAME-LAYER*))
    (entmake
      (list '(0 . "LAYER")
            '(100 . "AcDbSymbolTableRecord")
            '(100 . "AcDbLayerTableRecord")
            (cons 2 *CUTLINE-FRAME-LAYER*)
            '(70 . 0)
            '(62 . 7)
            '(6 . "Continuous")
            '(370 . -3)))
  )
)

;; ============================================================
;; РАМКА ВОКРУГ КАРТЫ РАСКРОЯ (LWPOLYLINE на слое *CUTLINE-FRAME-LAYER*)
;; bbox: ((x1 y1) (x2 y2)) — границы шапки+хлыстов
;; Отступы задаются константами *CUTLINE-FRAME-PAD-*
;; Возвращает bbox рамки (с учетом отступов)
;; ============================================================
(defun n1-draw-frame (bbox / x1 y1 x2 y2)
  (n1-ensure-frame-layer)
  (setq x1 (- (car  (car  bbox)) *CUTLINE-FRAME-PAD-LEFT*))
  (setq y1 (- (cadr (car  bbox)) *CUTLINE-FRAME-PAD-BOTTOM*))
  (setq x2 (+ (car  (cadr bbox)) *CUTLINE-FRAME-PAD-RIGHT*))
  (setq y2 (+ (cadr (cadr bbox)) *CUTLINE-FRAME-PAD-TOP*))
  (entmake
    (list '(0 . "LWPOLYLINE")
          '(100 . "AcDbEntity")
          (cons 8 *CUTLINE-FRAME-LAYER*)
          '(100 . "AcDbPolyline")
          '(90 . 4)
          '(70 . 1)
          (cons 10 (list x1 y1))
          (cons 10 (list x2 y1))
          (cons 10 (list x2 y2))
          (cons 10 (list x1 y2))))
  (list (list x1 y1) (list x2 y2))
)

;; ============================================================
;; ШАПКА КАРТЫ РАСКРОЯ
;; - заголовок "КАРТА РАСКРОЯ ХЛЫСТОВ" (жирный);
;; - подзаголовок "Заготовка: X мм || Пропил: Y мм" (шрифт как у "Хлыст N");
;; - линейка с одинарными засечками;
;; - метки 0 (справа-налево) и stock (слева-направо) — зеркально.
;; ============================================================
(defun n1-draw-header (insPt stock kerf / x0 y0 rowH txtH titleH barHeight
                       labelX labelX-right tick-half tick-base layout-txtH)
  (setq x0        (car insPt)
        y0        (cadr insPt)
        rowH      *CUTLINE-ROW-H*
        txtH      *CUTLINE-TEXT-H*
        titleH    *CUTLINE-TITLE-H*
        barHeight (/ stock 45.0))

  ;; Высота шрифта как у метки "Хлыст N" в n1-draw-layout
  (setq layout-txtH (* (/ stock 30.0) 0.30))

  ;; Позиции меток (отступ = barHeight * 0.6, как у "Хлыст 1")
  (setq labelX       (- x0 (* barHeight 0.6)))
  (setq labelX-right (+ x0 stock (* barHeight 0.6)))

  ;; Параметры одинарных засечек (небольшой размер)
  (setq tick-half (* rowH 0.12))
  (setq tick-base (+ y0 (* rowH 2.0)))

  ;; 1. Заголовок (жирный)
  (n1-draw-text-bold
    (list x0 (+ y0 (* rowH 3.4)))
    titleH
    "КАРТА РАСКРОЯ ХЛЫСТОВ"
    *CUTLINE-TITLE-COLOR* 0.0)

  ;; 2. Подзаголовок с || — высота шрифта как у "Хлыст N"
  (n1-draw-text
    (list x0 (+ y0 (* rowH 2.7)))
    layout-txtH
    (strcat "Заготовка: " (rtos stock 2 0)
            " мм   ||   Пропил: " (rtos kerf 2 1) " мм")
    *CUTLINE-HEADER-COLOR*)

  ;; 3. Линейка
  (n1-draw-line
    (list x0 tick-base)
    (list (+ x0 stock) tick-base)
    *CUTLINE-OUTLINE-COLOR*)

  ;; 4. Одинарные вертикальные засечки
  (n1-draw-line (list x0 (- tick-base tick-half))
                (list x0 (+ tick-base tick-half))
                *CUTLINE-OUTLINE-COLOR*)
  (n1-draw-line (list (+ x0 stock) (- tick-base tick-half))
                (list (+ x0 stock) (+ tick-base tick-half))
                *CUTLINE-OUTLINE-COLOR*)

  ;; 5. Левая метка "0" — выровнена по правому краю
  (n1-draw-text-bold-right
    (list labelX tick-base)
    (* layout-txtH 1.1)
    "0"
    *CUTLINE-VALUE-COLOR* 0.0)

  ;; 6. Правая метка stock — ПРАВЕЕ линейки, выровнена по левому краю
  ;;    (зеркально метке "0", та же высота шрифта)
  (n1-draw-text-bold-left
    (list labelX-right tick-base)
    (* layout-txtH 1.1)
    (rtos stock 2 0)
    *CUTLINE-VALUE-COLOR* 0.0)

  ;; BBox шапки (расширен вправо для метки stock)
  (list
    (list (- x0 (* barHeight 2.0)) (+ y0 (* rowH 2.0)))
    (list (+ x0 stock (* barHeight 2.0)) (+ y0 (* rowH 3.9))))
)

;; ============================================================

(defun n1-expand (pieces / sorted-groups out rec len cnt i)
  (setq sorted-groups (vl-sort pieces '(lambda (a b) (> (car a) (car b)))))
  (setq out '())
  (foreach rec sorted-groups
    (setq len (car rec) cnt (fix (cadr rec)) i 0)
    (while (< i cnt)
      (setq out (cons len out))
      (setq i (1+ i))
    )
  )
  (reverse out)
)

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
;; Алгоритм раскроя FFD
;; ============================================================
(defun n1-ffd (sorted-pieces stock kerf / bars p placed j bar newbar skip attempts stopGuard)
  ;; V6: страж итераций - считает попытки размещения; при превышении лимита
  ;; остаток деталей уходит в пропущенные (честный частичный результат).
  (setq bars '())
  (setq skip 0)
  (setq attempts 0)
  (setq stopGuard nil)
  (foreach p sorted-pieces
    (cond
      (stopGuard
       (setq skip (1+ skip)))
      ((> p stock)
       (setq skip (1+ skip)))
      (T
        (setq placed nil j 0)
        (while (and (not placed) (< j (length bars)) (not stopGuard))
          (setq bar (nth j bars))
          (setq attempts (1+ attempts))
          (if (>= attempts *n1-max-placement-attempts*)
            (setq stopGuard T)
            (if (>= (car bar) (+ kerf p))
              (progn
                (setq newbar (cons (- (car bar) kerf p)
                                   (append (cdr bar) (list p))))
                (setq bars (n1-replace-nth bars j newbar))
                (setq placed T)
              )
            )
          )
          (setq j (1+ j))
        )
        (if (not placed)
          (if stopGuard
            (setq skip (1+ skip))
            (setq bars (append bars (list (list (- stock p) p))))
          )
        )
      )
    )
  )
  (if stopGuard
    (princ (strcat "\n[CUTLINE][GUARD] Алгоритм остановлен ограничителем попыток ("
                    (itoa *n1-max-placement-attempts*) "). Размещено: "
                    (itoa (- (length sorted-pieces) skip)) "/"
                    (itoa (length sorted-pieces)))))
  (if (> skip 0)
    (princ (strcat "\nВНИМАНИЕ: в FFD пропущено деталей длиннее хлыста: " (itoa skip)))
  )
  bars
)

(defun n1-add-group (groups key / found)
  (setq found (assoc key groups))
  (if found
    (mapcar '(lambda (x) (if (= (car x) key) (cons (car x) (1+ (cdr x))) x)) groups)
    (cons (cons key 1) groups)
  )
)

;; ============================================================
;; Подсчет объектов по типам
;; ============================================================
(defun n1-count-by-type (ss / i ent typ counts found obj len)
  (setq counts '(("LINE" . 0) ("MLINE" . 0) ("DYNBLOCK" . 0)) i 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (setq typ (cdr (assoc 0 (entget ent))))
    (cond
      ((or (= typ "LINE") (= typ "MLINE"))
       (setq found (assoc typ counts))
       (if found (setq counts (subst (cons typ (1+ (cdr found))) found counts)))
      )
      ((= typ "INSERT")
       (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
       (if (and
             (not (vl-catch-all-error-p obj))
             (su-is-valid-stock-block obj))
         (progn
           (setq found (assoc "DYNBLOCK" counts))
           (if found (setq counts (subst (cons "DYNBLOCK" (1+ (cdr found))) found counts)))
         )
       )
      )
    )
    (setq i (1+ i))
  )
  counts
)

(defun n1-print-type-counts (counts / s rec)
  (setq s "")
  (foreach rec counts
    (if (> (cdr rec) 0)
      (setq s (strcat s (if (= s "") "" ", ")
                      (itoa (cdr rec)) " "
                      (cond ((= (car rec) "LINE")      "линий")
                            ((= (car rec) "MLINE")     "мультилиний")
                            ((= (car rec) "DYNBLOCK")  "дин. блоков")
                            (T (strcat (car rec) " шт")))))
    )
  )
  (if (= s "")
    (princ "\n  (объектов подходящих типов нет)")
    (princ (strcat "\n  " s))
  )
)

;; ============================================================
;; Фильтрация набора по типу
;; ============================================================
(defun n1-filter-ss-by-type (ss typ / i ent new-ss ent-typ obj)
  (setq new-ss (ssadd) i 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (setq ent-typ (cdr (assoc 0 (entget ent))))
    (cond
      ((or (= typ "LINE") (= typ "MLINE"))
       (if (= ent-typ typ) (ssadd ent new-ss))
      )
      ((= typ "DYNBLOCK")
       (if (= ent-typ "INSERT")
         (progn
           (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
           (if (and
                 (not (vl-catch-all-error-p obj))
                 (su-is-valid-stock-block obj))
             (ssadd ent new-ss)
           )
         )
       )
      )
      ((= typ "ALL")
       (ssadd ent new-ss)
      )
    )
    (setq i (1+ i))
  )
  new-ss
)

;; ============================================================
;; Фильтрация набора по типу И имени типа блока
;; ============================================================
(defun n1-filter-ss-by-type-and-name (ss typ type-name
                                      / i ent new-ss ent-typ obj block-type-name)
  (setq new-ss (ssadd) i 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (setq ent-typ (cdr (assoc 0 (entget ent))))
    (cond
      ((or (= typ "LINE") (= typ "MLINE"))
       (if (= ent-typ typ) (ssadd ent new-ss))
      )
      ((= typ "DYNBLOCK")
       (if (= ent-typ "INSERT")
         (progn
           (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
           (if (and (not (vl-catch-all-error-p obj))
                    (su-is-valid-stock-block obj))
             (progn
               (if (or (null type-name)
                       (= type-name "")
                       (= type-name "Все типы блоков"))
                 (ssadd ent new-ss)
                 (progn
                   (setq block-type-name (su-get-dynblock-type-name ent))
                   (if (= block-type-name type-name)
                     (ssadd ent new-ss)
                   )
                 )
               )
             )
           )
         )
       )
      )
      ((= typ "ALL")
       (ssadd ent new-ss)
      )
    )
    (setq i (1+ i))
  )
  new-ss
)

;; ============================================================
;; Фильтрация набора по типу мультилинии
;; ============================================================
(defun n1-filter-ss-by-mline-type (ss type-name / i ent new-ss)
  (setq new-ss (ssadd) i 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (if (= (cdr (assoc 0 (entget ent))) "MLINE")
      (if (or (null type-name) (= type-name "")
              (= (su-mline-type-name ent) type-name))
        (ssadd ent new-ss)
      )
    )
    (setq i (1+ i))
  )
  new-ss
)

;; ============================================================
;; Подсчет динамических блоков конкретного типа
;; ============================================================
(defun n1-count-dynblock-by-type (ss type-name
                                  / i ent typ obj count block-type-name)
  (setq count 0 i 0)
  (if (null ss)
    0
    (progn
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq typ (cdr (assoc 0 (entget ent))))
        (if (= typ "INSERT")
          (progn
            (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
            (if (and (not (vl-catch-all-error-p obj))
                     (su-is-valid-stock-block obj))
              (progn
                (if (or (null type-name)
                        (= type-name "")
                        (= type-name "Все типы блоков"))
                  (setq count (1+ count))
                  (progn
                    (setq block-type-name (su-get-dynblock-type-name ent))
                    (if (= block-type-name type-name)
                      (setq count (1+ count))
                    )
                  )
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
      count
    )
  )
)

(defun n1-filter-name-str ( / f1 f2 f3 s)
  (setq f1 (if (boundp '*EXTRACTION-FILTER-FACADES*) *EXTRACTION-FILTER-FACADES* nil))
  (setq f2 (if (boundp '*EXTRACTION-FILTER-VITRAZH*) *EXTRACTION-FILTER-VITRAZH* nil))
  (setq f3 (if (boundp '*EXTRACTION-FILTER-FONAR*) *EXTRACTION-FILTER-FONAR* nil))
  (setq s "")
  (if f1 (setq s "Фасады"))
  (if f2 (setq s (if (= s "") "Витражи" (strcat s ", Витражи"))))
  (if f3 (setq s (if (= s "") "Фонарь 3D" (strcat s ", Фонарь 3D"))))
  (if (= s "") nil s)
)

;; ============================================================
;; Отображение списка слоев
;; ============================================================
(defun n1-layer-display-list (layers / fname suffix)
  (setq fname (n1-filter-name-str))
  (setq suffix "")

  (cond
    ((and (boundp '*CUTLINE-IS-AUTO-FILTER*)
          *CUTLINE-IS-AUTO-FILTER*
          fname)
     (setq suffix " (за исключением слоя 0)")
     (list (strcat "Групповой фильтр " fname suffix))
    )
    ((or (null layers) (not (listp layers)) (= (length layers) 0))
     (list "Все слои")
    )
    (T layers)
  )
)

;; Безопасные обертки
(defun n1-safe-set-tile (key value)
  (vl-catch-all-apply 'set_tile (list key value))
)
(defun n1-safe-action-tile (key action)
  (vl-catch-all-apply 'action_tile (list key action))
)
(defun n1-safe-mode-tile (key mode)
  (vl-catch-all-apply 'mode_tile (list key mode))
)

;; ============================================================
;; Ручное управление радиокнопками
;; ============================================================
(defun n1-select-radio (selected / keys k)
  (setq keys '("rb_line" "rb_mline" "rb_dynblock" "rb_both"))
  (foreach k keys
    (n1-safe-set-tile k (if (= k selected) "1" "0"))
  )
  (cond
    ((= selected "rb_line")     (setq *n1-tmp-choice* 'LINE))
    ((= selected "rb_mline")    (setq *n1-tmp-choice* 'MLINE))
    ((= selected "rb_dynblock") (setq *n1-tmp-choice* 'DYNBLOCK))
    ((= selected "rb_both")     (setq *n1-tmp-choice* 'ALL))
  )

  (if (= selected "rb_dynblock")
    (n1-safe-mode-tile "popup_dynblock_type" 0)
    (progn
      (n1-safe-mode-tile "popup_dynblock_type" 1)
      (setq *n1-tmp-dynblock-type* "")
    )
  )

  (if (= selected "rb_mline")
    (n1-safe-mode-tile "popup_mline_type" 0)
    (progn
      (n1-safe-mode-tile "popup_mline_type" 1)
      (setq *n1-tmp-mline-type* "")
    )
  )
)

;; ============================================================
;; Обработчик выбора типа динамического блока
;; ============================================================
(defun n1-on-dynblock-type-changed (value / idx type-name new-count)
  (setq idx (atoi value))
  (if (= idx 0)
    (progn
      (setq *n1-tmp-dynblock-type* "")
      (setq new-count (n1-count-dynblock-by-type *n1-cutline-ss* ""))
      (n1-safe-set-tile "txt_dynblock_count"
        (strcat (itoa new-count) " шт."))
    )
    (progn
      (setq type-name (nth (1- idx) *n1-dynblock-types-list*))
      (setq *n1-tmp-dynblock-type* type-name)
      (setq new-count (n1-count-dynblock-by-type *n1-cutline-ss* type-name))
      (n1-safe-set-tile "txt_dynblock_count"
        (strcat (itoa new-count) " шт."))
      (n1-select-radio "rb_dynblock")
    )
  )
)

;; ============================================================
;; Обработчик выбора типа мультилинии
;; ============================================================
(defun n1-on-mline-type-changed (value / idx type-name new-count)
  (setq idx (atoi value))
  (if (= idx 0)
    (progn
      (setq *n1-tmp-mline-type* "")
      (setq new-count (su-count-mline-by-type *n1-cutline-ss* ""))
      (n1-safe-set-tile "txt_mline_count"
        (strcat (itoa new-count) " шт."))
    )
    (progn
      (setq type-name (nth (1- idx) *n1-mline-types-list*))
      (setq *n1-tmp-mline-type* type-name)
      (setq new-count (su-count-mline-by-type *n1-cutline-ss* type-name))
      (n1-safe-set-tile "txt_mline_count"
        (strcat (itoa new-count) " шт."))
      (n1-select-radio "rb_mline")
    )
  )
)

;; ============================================================
;; Диалог параметров раскроя
;; ============================================================
;; Этап 2 (V1+V2): прием диалога = проверка введенных чисел.
;; Заменяет старый молчаливый пресет на дефолт: теперь мусор
;; в полях "хлыст"/"рез" не пропускается - диалог остается открытым.
(defun n1-dialog-accept (/ s k)
  (setq s *n1-tmp-stock* k *n1-tmp-kerf*)
  (if (and (tu-valid-bar-length-p s)
           (tu-valid-kerf-p k)
           (< k s))
    (done_dialog 1)
    (alert (strcat "[AutoExtraction][CUTLINE][VALIDATION]\n"
                   "Длина хлыста: число > 0 (лимит "
                   (rtos *n1-max-bar-length* 2 0) " мм).\n"
                   "Рез: число от 0 до " (rtos *n1-max-kerf* 2 0)
                   " мм и меньше длины хлыста."))
  )
)

(defun n1-cutline-dialog (line-cnt mline-cnt dynblock-cnt
                          ss-for-types
                          default-tol default-stock default-kerf
                          layers
                          default-xls default-acad
                          / dcl-file dcl-id result
                            all-cnt base-layers tn)

  (setq dcl-file (findfile "cutline_filter.dcl"))

  (if (null dcl-file)
    (progn
      (princ "\n[cutline] не найден cutline_filter.dcl")
      nil
    )
    (progn
      (setq dcl-id (load_dialog dcl-file))
      (if (< dcl-id 0)
        (progn
          (princ "\n[cutline] ошибка load_dialog")
          nil
        )
        (progn
          (setq *n1-tmp-choice* 'ALL)
          (setq *n1-tmp-stock* default-stock)
          (setq *n1-tmp-kerf* default-kerf)
          (setq *n1-tmp-chk-xls* default-xls)
          (setq *n1-tmp-chk-acad* default-acad)

          (if (vl-catch-all-error-p
                (vl-catch-all-apply 'new_dialog (list "cutline_filter_dialog" dcl-id)))
            (progn
              (princ "\n[cutline] ошибка new_dialog")
              (vl-catch-all-apply 'unload_dialog (list dcl-id))
              nil
            )
            (progn
              (setq all-cnt (+ line-cnt mline-cnt dynblock-cnt))

              (n1-safe-set-tile "txt_line_count"
                (strcat (itoa line-cnt) " шт."))
              (n1-safe-set-tile "txt_mline_count"
                (strcat (itoa mline-cnt) " шт."))
              (n1-safe-set-tile "txt_dynblock_count"
                (strcat (itoa dynblock-cnt) " шт."))
              (n1-safe-set-tile "txt_both_count"
                (strcat (itoa all-cnt) " шт."))

              (setq *n1-cutline-ss* ss-for-types)

              (setq *n1-tmp-dynblock-type* "")
              (setq *n1-dynblock-types-list*
                (su-collect-dynblock-types ss-for-types 'su-is-valid-stock-block))

              (start_list "popup_dynblock_type")
              (add_list "Все типы блоков")
              (foreach tn *n1-dynblock-types-list*
                (add_list tn)
              )
              (end_list)
              (set_tile "popup_dynblock_type" "0")

              (if (or (<= dynblock-cnt 0)
                      (and (not (eq *n1-tmp-choice* 'DYNBLOCK))
                           (not (eq *n1-tmp-choice* 'ALL))))
                (n1-safe-mode-tile "popup_dynblock_type" 1)
              )

              (setq *n1-tmp-mline-type* "")
              (setq *n1-mline-types-list*
                (su-collect-mline-types ss-for-types))

              (start_list "popup_mline_type")
              (add_list "Все типы мультилиний")
              (foreach tn *n1-mline-types-list*
                (add_list tn)
              )
              (end_list)
              (set_tile "popup_mline_type" "0")

              (if (or (<= mline-cnt 0)
                      (and (not (eq *n1-tmp-choice* 'MLINE))
                           (not (eq *n1-tmp-choice* 'ALL))))
                (n1-safe-mode-tile "popup_mline_type" 1)
              )

              (setq base-layers (n1-layer-display-list layers))
              (vl-catch-all-apply
                '(lambda ()
                   (start_list "lst_layers")
                   (foreach l base-layers (add_list l))
                   (end_list))
                nil)

              (cond
                ((and (> line-cnt 0) (> mline-cnt 0))
                 (n1-safe-set-tile "rb_both" "1")
                 (setq *n1-tmp-choice* 'ALL))
                ((and (> line-cnt 0) (> dynblock-cnt 0))
                 (n1-safe-set-tile "rb_both" "1")
                 (setq *n1-tmp-choice* 'ALL))
                ((and (> mline-cnt 0) (> dynblock-cnt 0))
                 (n1-safe-set-tile "rb_both" "1")
                 (setq *n1-tmp-choice* 'ALL))
                ((> line-cnt 0)
                 (n1-safe-set-tile "rb_line" "1")
                 (setq *n1-tmp-choice* 'LINE))
                ((> mline-cnt 0)
                 (n1-safe-set-tile "rb_mline" "1")
                 (setq *n1-tmp-choice* 'MLINE))
                ((> dynblock-cnt 0)
                 (n1-safe-set-tile "rb_dynblock" "1")
                 (setq *n1-tmp-choice* 'DYNBLOCK))
                (T
                 (n1-safe-set-tile "rb_both" "1")
                 (setq *n1-tmp-choice* 'ALL))
              )

              (if (<= line-cnt 0)     (n1-safe-mode-tile "rb_line" 1))
              (if (<= mline-cnt 0)    (n1-safe-mode-tile "rb_mline" 1))
              (if (<= dynblock-cnt 0) (n1-safe-mode-tile "rb_dynblock" 1))
              (if (<= all-cnt 0)      (n1-safe-mode-tile "rb_both" 1))

              (n1-safe-set-tile "edt_stock" (rtos default-stock 2 0))
              (n1-safe-set-tile "edt_kerf"  (rtos default-kerf 2 0))

              (n1-safe-set-tile "chk_xls"  (if default-xls  "1" "0"))
              (n1-safe-set-tile "chk_acad" (if default-acad "1" "0"))

              (n1-safe-action-tile "rb_line"     "(n1-select-radio \"rb_line\")")
              (n1-safe-action-tile "rb_mline"    "(n1-select-radio \"rb_mline\")")
              (n1-safe-action-tile "rb_dynblock" "(n1-select-radio \"rb_dynblock\")")
              (n1-safe-action-tile "rb_both"     "(n1-select-radio \"rb_both\")")

              (n1-safe-action-tile "popup_dynblock_type"
                "(n1-on-dynblock-type-changed $value)")
              (n1-safe-action-tile "popup_mline_type"
                "(n1-on-mline-type-changed $value)")

              (n1-safe-action-tile "edt_stock"
                "(setq *n1-tmp-stock* (tu-parse-number $value))")
              (n1-safe-action-tile "edt_kerf"
                "(setq *n1-tmp-kerf* (tu-parse-number $value))")

              (n1-safe-action-tile "chk_xls"
                "(setq *n1-tmp-chk-xls* (= $value \"1\"))")
              (n1-safe-action-tile "chk_acad"
                "(setq *n1-tmp-chk-acad* (= $value \"1\"))")

              (n1-safe-action-tile "btn_ok"     "(n1-dialog-accept)")
              (n1-safe-action-tile "btn_cancel" "(done_dialog 0)")

              (setq result (start_dialog))

              (vl-catch-all-apply 'unload_dialog (list dcl-id))

              (if (= result 1)
                (list
                  *n1-tmp-choice*
                  default-tol
                  *n1-tmp-stock*
                  *n1-tmp-kerf*
                  *n1-tmp-chk-xls*
                  *n1-tmp-chk-acad*
                  *n1-tmp-dynblock-type*
                  *n1-tmp-mline-type*
                )
                nil
              )
            )
          )
        )
      )
    )
  )
)

;; ============================================================
;; Извлечение длин
;; ============================================================
(defun n1-extract-pieces (ss tol min-len max-len /
                            i ent typ len key pieces total geom
                            measured skipped skipped-short skipped-long
                            skipped-broken skipped-diag obj)
  (setq pieces '() i 0 total (sslength ss)
        measured 0 skipped 0 skipped-short 0 skipped-long 0
        skipped-broken 0 skipped-diag 0)
  (repeat total
    (setq ent (ssname ss i))
    (setq typ (cdr (assoc 0 (entget ent))))

    (cond
      ((= typ "MLINE")
       (setq geom (su-mline-cut-geom ent))
       (if geom
         (setq len (car geom))
         (progn
           (setq len nil)
           (if (> (su-mline-vertex-count ent) 2)
             (setq skipped-broken (1+ skipped-broken))
             (setq skipped-diag (1+ skipped-diag))
           )
         )
       )
      )
      ((= typ "LINE")
       (setq len (vl-catch-all-apply 'vlax-curve-getDistAtParam
                   (list ent (vlax-curve-getEndParam ent))))
       (if (vl-catch-all-error-p len) (setq len nil))
      )
      ((= typ "INSERT")
       (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
       (if (not (vl-catch-all-error-p obj))
         (setq len (su-get-length obj))
         (setq len nil)
       )
      )
      (T (setq len nil))
    )

    (cond
      ((or (null len) (not (numberp len)) (<= len 0.0))
       (setq skipped (1+ skipped)))
      ((< len min-len) (setq skipped-short (1+ skipped-short)))
      ((> len max-len) (setq skipped-long (1+ skipped-long)))
      (T (setq measured (1+ measured))
         (setq key (fix (+ (/ len tol) 0.5)))
         (setq pieces (n1-add-group pieces key)))
    )
    (setq i (1+ i))
  )
  (princ (strcat "\nИзмерено: " (itoa measured) " из " (itoa total)))
  (if (> skipped 0)
    (princ (strcat "\n  Пропущено (нулевые/ошибки): " (itoa skipped))))
  (if (> skipped-short 0)
    (princ (strcat "\n  Пропущено (короче " (rtos min-len 2 0) " мм): "
                   (itoa skipped-short))))
  (if (> skipped-long 0)
    (princ (strcat "\n  Пропущено (длиннее " (rtos max-len 2 0) " мм): "
                   (itoa skipped-long))))
  (if (> skipped-broken 0)
    (princ (strcat "\n  Пропущено (ломаные MLINE, >2 вершин): "
                   (itoa skipped-broken)))
  )
  (if (> skipped-diag 0)
    (princ (strcat "\n  Пропущено (диагональные MLINE): "
                   (itoa skipped-diag)))
  )
  (mapcar '(lambda (x) (list (* (float (car x)) tol) (cdr x)))
          (reverse pieces))
)

(defun n1-split-by-stock (pieces stock / ok oversized rec)
  (setq ok '() oversized '())
  (foreach rec pieces
    (if (<= (car rec) stock)
      (setq ok (cons rec ok))
      (setq oversized (cons rec oversized))
    )
  )
  (list (reverse ok) (reverse oversized))
)

;; ============================================================
;; Вывод неразмещенных деталей
;; ============================================================
(defun n1-report-oversized (oversized stock / rec total-cnt sorted)
  (if oversized
    (progn
      (princ "\n")
      (princ "\n=== НЕРАЗМЕЩЕННЫЕ ДЕТАЛИ ===")
      (princ (strcat "\n(длина превышает хлыст " (rtos stock 2 0) " мм)"))

      (setq sorted (vl-sort oversized '(lambda (a b) (> (car a) (car b)))))

      (setq total-cnt 0)
      (foreach rec sorted
        (setq total-cnt (+ total-cnt (cadr rec)))
        (princ (strcat "\n  Длина " (rtos (car rec) 2 0)
                       " мм, кол-во " (itoa (cadr rec)) " шт."))
      )
      (princ (strcat "\nВсего неразмещенных: " (itoa total-cnt) " шт."))
    )
  )
)

;; ============================================================
;; Преобразование списка длин в строку
;; ============================================================
(defun n1-list-to-str (lst sep / s x)
  ;; Разделитель по умолчанию " + ": Excel сливает "2000 2000" в число
  (if (null sep) (setq sep " + "))
  (setq s "")
  (foreach x lst
    (setq s (strcat s (if (= s "") "" sep) (itoa (fix x))))
  )
  s
)

;; ============================================================
;; Раскладка хлыстов
;; ============================================================
(defun n1-draw-layout (bars stock kerf insPt color-map /
    barHeight gap txtH axisStep x0 y0 maxy miny i bar pieces waste used util
    curx p str col labelX labelY1 labelY2 centerY
    waste-txt-h waste-center-y waste-x)

  ;; ============================================================
  ;; ПАРАМЕТРЫ РАСКЛАДКИ (РЕГУЛИРОВАТЬ ЗДЕСЬ)
  ;; ============================================================
  ;; ОСЕВОЕ РАССТОЯНИЕ между хлыстами. Регулировать здесь.
  (setq axisStep (* (/ stock 30.0) 1.7))
  ;; ВЫСОТА ХЛЫСТА. Делитель: чем больше, тем тоньше хлыст.
  (setq barHeight (/ stock 45.0))
  ;; Зазор между хлыстами вычисляется автоматически
  (setq gap (- axisStep barHeight))
  ;; ВЫСОТА ТЕКСТА (фиксирована, НЕ зависит от высоты хлыста)
  (setq txtH (* (/ stock 30.0) 0.30))
  ;; ============================================================

  (setq x0 (car insPt) y0 (cadr insPt) maxy (+ y0 barHeight) miny y0 i 0)
  (foreach bar bars
    (setq i (1+ i))
    (setq pieces (cdr bar) waste (car bar) used (- stock waste)
          util (* 100.0 (/ used stock)) miny y0)
    (setq curx x0)

    ;; ЗАЛИВКА ДЕТАЛЕЙ
    (foreach p pieces
      (setq col (n1-get-color color-map p))
      (n1-draw-filled-rect (list curx y0) (list (+ curx p) (+ y0 barHeight))
                           col *CUTLINE-PART-TRANSPARENCY*)
      (setq curx (+ curx p kerf))
    )

    ;; ЗАЛИВКА ОТХОДА
    (if (> waste 0.0)
      (n1-draw-filled-rect (list (- (+ x0 stock) waste) y0)
                           (list (+ x0 stock) (+ y0 barHeight))
                           *NEST-COLOR-WASTE* *CUTLINE-WASTE-TRANSPARENCY*)
    )

    ;; Обводка хлыста
    (n1-draw-rect (list x0 y0) (list (+ x0 stock) (+ y0 barHeight)) *NEST-COLOR-OUTLINE*)

    ;; ВЕРТИКАЛЬНЫЕ ЛИНИИ РЕЗА (две на деталь: граница детали + граница реза)
    (setq curx x0)
    (foreach p pieces
      (n1-draw-line (list curx y0) (list curx (+ y0 barHeight)) *NEST-COLOR-OUTLINE*)
      (n1-draw-line (list (+ curx p) y0) (list (+ curx p) (+ y0 barHeight)) *NEST-COLOR-OUTLINE*)
      (setq curx (+ curx p kerf))
    )
    (n1-draw-line (list curx y0) (list curx (+ y0 barHeight)) *NEST-COLOR-OUTLINE*)

    ;; Метки хлыста: выравнивание по правому краю, вертикально от центра хлыста
    ;; 0.6 — отступ левее от хлыста
    ;; 1.0 — половина расстояния между метками
    (setq labelX (- x0 (* barHeight 0.6)))
    (setq centerY (+ y0 (* barHeight 0.5)))
    (setq labelY1 (+ centerY (* txtH 1.0)))
    (setq labelY2 (- centerY (* txtH 1.0)))
    (n1-draw-text-right (list labelX labelY1) txtH
                        (strcat "Хлыст " (itoa i))
                        *NEST-COLOR-LABEL*)
    (n1-draw-text-right (list labelX labelY2) txtH
                        (strcat "[" (rtos util 2 1) "%]")
                        *NEST-COLOR-LABEL*)

    ;; ТЕКСТ ВНУТРИ ДЕТАЛЕЙ (жирный, желтый, середина)
    (setq curx x0)
    (foreach p pieces
      (setq str (itoa (fix p)))
      (n1-draw-text-bold-center (list (+ curx (* p 0.5)) (+ y0 (* barHeight 0.5)))
                                (* txtH 1.1) str *NEST-COLOR-PART-TEXT* 0.0)
      (setq curx (+ curx p kerf))
    )

    ;; ТЕКСТ ОТХОДА (длина, поворот 90°, по центру хлыста, отступ 0.75)
    (if (> waste 0.0)
      (progn
        (setq str (itoa (fix waste)))
        (setq waste-txt-h (* txtH 1.25))
        (setq waste-center-y (+ y0 (* barHeight 0.5)))
        (setq waste-x (+ x0 stock (* barHeight 0.75)))
        (n1-draw-text-bold-center (list waste-x waste-center-y)
                                  waste-txt-h str *NEST-COLOR-WASTE* 90.0)
      )
    )
    ;; Переход к следующему хлысту (осевое расстояние = barHeight + gap = axisStep)
    (setq y0 (- y0 barHeight gap))
  )
  (list (list (- x0 (* barHeight 2.0)) miny) (list (+ x0 stock) maxy))
)

;; ============================================================
;; Сводная таблица раскроя
;; ============================================================
(defun n1-draw-summary (bars pieces stock insPt color-map oversized /
    barHeight th rowH pad col1W col2W col3W tableW tableH
    left top x1 x2 x3 y bottom
    num-bars stock-total-mm stock-total-m
    total-cnt total-product-mm total-product-m kpd rec oversized-cnt
    num-piece-rows)
  (setq barHeight (/ stock 30.0) th (* barHeight 0.30)
        rowH (* barHeight 0.6) pad (* barHeight 0.6)
        col1W (* barHeight 5.0) col2W (* barHeight 3.5) col3W (* barHeight 4.5))
  (setq tableW (+ col1W col2W col3W (* pad 2)))
  (setq num-bars (length bars) stock-total-mm (* num-bars stock)
        stock-total-m (/ stock-total-mm 1000.0))
  (setq total-cnt 0 total-product-mm 0.0)
  (foreach rec pieces
    (setq total-cnt (+ total-cnt (cadr rec)))
    (setq total-product-mm (+ total-product-mm (* (car rec) (cadr rec))))
  )
  (setq total-product-m (/ total-product-mm 1000.0))
  (setq kpd (if (> stock-total-mm 0)
              (* 100.0 (/ (float total-product-mm) (float stock-total-mm))) 0.0))

  (setq pieces (vl-sort pieces '(lambda (a b) (> (car a) (car b)))))
  (setq num-piece-rows (length pieces))

  (setq left (car insPt) top (cadr insPt))

  (setq tableH (+ (* pad 2)
                  (* (+ 11.0 num-piece-rows (if oversized 1.0 0.0)) rowH)))

  (setq bottom (- top tableH))
  (setq x1 (+ left pad) x2 (+ left pad col1W) x3 (+ left pad col1W col2W))

  (n1-draw-rect (list left bottom) (list (+ left tableW) top) *NEST-COLOR-OUTLINE*)

  (setq y (- top pad rowH))

  (n1-draw-text (list x1 y) (* th 1.3) "Раскрой хлыста" *NEST-COLOR-TITLE*)

  (setq y (- y rowH))
  (n1-draw-text (list x1 y) th "Длина" *NEST-COLOR-HEADER*)

  (setq y (- y rowH))
  (n1-draw-text (list x1 y) th "Хлыст, мм" *NEST-COLOR-HEADER*)
  (n1-draw-text (list x2 y) th "Кол-во, шт" *NEST-COLOR-HEADER*)
  (n1-draw-text (list x3 y) th "Сумма, м.п." *NEST-COLOR-HEADER*)

  (setq y (- y rowH))
  (n1-draw-text (list x1 y) th (itoa (fix stock)) *NEST-COLOR-VALUE*)
  (n1-draw-text (list x2 y) th (itoa num-bars) *NEST-COLOR-VALUE*)
  (n1-draw-text (list x3 y) th (rtos stock-total-m 2 2) *NEST-COLOR-VALUE*)

  (setq y (- y rowH))
  (n1-draw-text (list x1 y) th "Изделия" *NEST-COLOR-HEADER*)

  (setq y (- y rowH))
  (n1-draw-text (list x1 y) th "Длина, мм" *NEST-COLOR-HEADER*)
  (n1-draw-text (list x2 y) th "Кол-во, шт" *NEST-COLOR-HEADER*)
  (n1-draw-text (list x3 y) th "Сумма, м.п." *NEST-COLOR-HEADER*)

  (setq y (- y rowH))
  (foreach rec pieces
    (n1-draw-text (list x1 y) th (itoa (fix (car rec)))
                  (n1-get-color color-map (car rec)))
    (n1-draw-text (list x2 y) th (itoa (cadr rec)) *NEST-COLOR-VALUE*)
    (n1-draw-text (list x3 y) th
                  (rtos (/ (* (car rec) (cadr rec)) 1000.0) 2 2) *NEST-COLOR-VALUE*)
    (setq y (- y rowH))
  )

  (setq y (- y (* rowH 0.5)))
  (n1-draw-text (list x1 y) th (strcat "Всего изделий: " (itoa total-cnt) " шт")
                *NEST-COLOR-VALUE*)
  (setq y (- y rowH))
  (n1-draw-text (list x1 y) th
                (strcat "Суммарная длина: " (rtos total-product-m 2 2) " м.п.")
                *NEST-COLOR-VALUE*)
  (setq y (- y rowH))
  (n1-draw-text (list x1 y) (* th 1.2)
                (strcat "КПД использования: " (rtos kpd 2 1) " %")
                *NEST-COLOR-KPD*)

  (if oversized
    (progn
      (setq oversized-cnt 0)
      (foreach rec oversized
        (setq oversized-cnt (+ oversized-cnt (cadr rec)))
      )
      (setq y (- y rowH))
      (n1-draw-text (list x1 y) th
                    (strcat "Неразмещенных: " (itoa oversized-cnt)
                            " шт (см. таблицу ниже)")
                    *NEST-COLOR-KPD*)
    )
  )

  (list (list left bottom) (list (+ left tableW) top))
)

;; ============================================================
;; Таблица неразмещенных деталей
;; ============================================================
(defun n1-draw-oversized (oversized stock insPt /
    barHeight th rowH pad pad-bottom col1W col2W col3W tableW tableH
    left top x1 x2 x3 y bottom total-cnt rec)
  (if (null oversized)
    nil
    (progn
      (setq barHeight (/ stock 30.0) th (* barHeight 0.30)
            rowH (* barHeight 0.6) pad (* barHeight 0.6)
            pad-bottom (* barHeight 1.4)
            col1W (* barHeight 5.0) col2W (* barHeight 3.5) col3W (* barHeight 4.5))
      (setq tableW (+ col1W col2W col3W (* pad 2)))
      (setq total-cnt 0)
      (foreach rec oversized (setq total-cnt (+ total-cnt (cadr rec))))
      (setq left (car insPt) top (cadr insPt))

      (setq tableH (+ pad pad-bottom (* (+ 4.0 (length oversized)) rowH)))
      (setq bottom (- top tableH))
      (setq x1 (+ left pad) x2 (+ left pad col1W) x3 (+ left pad col1W col2W))

      (n1-draw-rect (list left bottom) (list (+ left tableW) top) *NEST-COLOR-OUTLINE*)

      (setq y (- top pad rowH))

      (n1-draw-text (list x1 y) (* th 1.3) "Неразмещенные детали" *NEST-COLOR-TITLE*)

      (setq y (- y rowH))
      (n1-draw-text (list x1 y) th
                    (strcat "(длина превышает хлыст " (rtos stock 2 0) " мм)")
                    *NEST-COLOR-VALUE*)

      (setq oversized (vl-sort oversized '(lambda (a b) (> (car a) (car b)))))

      (setq y (- y rowH))
      (n1-draw-text (list x1 y) th "Длина, мм" *NEST-COLOR-HEADER*)
      (n1-draw-text (list x2 y) th "Кол-во, шт" *NEST-COLOR-HEADER*)
      (n1-draw-text (list x3 y) th "Сумма, м.п." *NEST-COLOR-HEADER*)

      (setq y (- y rowH))
      (foreach rec oversized
        (n1-draw-text (list x1 y) th (itoa (fix (car rec))) *NEST-COLOR-VALUE*)
        (n1-draw-text (list x2 y) th (itoa (cadr rec)) *NEST-COLOR-VALUE*)
        (n1-draw-text (list x3 y) th
                      (rtos (/ (* (car rec) (cadr rec)) 1000.0) 2 2)
                      *NEST-COLOR-VALUE*)
        (setq y (- y rowH))
      )

      (setq y (- y (* rowH 0.5)))
      (n1-draw-text (list x1 y) th (strcat "Всего: " (itoa total-cnt) " шт")
                    *NEST-COLOR-KPD*)

      (list (list left bottom) (list (+ left tableW) top))
    )
  )
)

(defun n1-combine-bbox (b1 b2)
  (list (list (min (car (car b1)) (car (car b2)))
              (min (cadr (car b1)) (cadr (car b2))))
        (list (max (car (cadr b1)) (car (cadr b2)))
              (max (cadr (cadr b1)) (cadr (cadr b2))))))

;; ============================================================
;; Консольный отчет по хлыстам
;; ============================================================
(defun n1-report (bars stock kerf / i bar pieces waste used util)
  (princ (strcat "\nКоличество хлыстов: " (itoa (length bars))))
  (setq i 0)
  (foreach bar bars
    (setq i (1+ i))
    (setq pieces (cdr bar) waste (car bar) used (- stock waste)
          util (* 100.0 (/ used stock)))
    (princ (strcat "\nХлыст " (itoa i) ": " (n1-list-to-str pieces " + ")
                   " | исп. " (rtos used 2 1) " | Отход " (rtos waste 2 1)
                   " | " (rtos util 2 1) "%"))
  )
  (princ)
)

;; ============================================================
;; XLS-экспорт
;; ============================================================
(defun n1-write-xls (bars stock kerf oversized
                     num-bars stock-total-mm product-total-mm kpd /
                     fname f i bar pieces waste used util rec
                     total-cnt-unplaced total-sum-unplaced)
  (setq fname (strcat (getvar "DWGPREFIX")
                      (vl-filename-base (getvar "DWGNAME"))
                      " Раскрой хлыстов.xls"))
  (setq f (open fname "w"))
  (if (null f)
    nil
    (progn
      (setq total-cnt-unplaced 0 total-sum-unplaced 0.0)

      (if oversized
        (progn
          (setq oversized (vl-sort oversized '(lambda (a b) (> (car a) (car b)))))
          (foreach rec oversized
            (setq total-cnt-unplaced (+ total-cnt-unplaced (cadr rec)))
            (setq total-sum-unplaced (+ total-sum-unplaced
                                        (/ (* (car rec) (cadr rec)) 1000.0)))
          )
        )
      )

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
      (write-line "   <Alignment ss:Vertical=\"Center\"/>" f)
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

      (write-line "  <Style ss:ID=\"ReportLabel\">" f)
      (write-line "   <Alignment ss:Horizontal=\"Left\" ss:Vertical=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"2\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)

      (write-line "  <Style ss:ID=\"ReportValueRight\">" f)
      (write-line "   <Alignment ss:Horizontal=\"Left\" ss:Vertical=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"2\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)

      (write-line "  <Style ss:ID=\"ReportKpdLabel\">" f)
      (write-line "   <Alignment ss:Horizontal=\"Left\" ss:Vertical=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"2\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"2\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)

      (write-line "  <Style ss:ID=\"ReportKpd\">" f)
      (write-line "   <Font ss:Bold=\"1\" ss:Color=\"#FF0000\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Left\" ss:Vertical=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"2\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"2\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)

      (write-line "  <Style ss:ID=\"SectionTitle\">" f)
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

      (write-line "  <Style ss:ID=\"SkipHeaderLeft\">" f)
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

      (write-line "  <Style ss:ID=\"SkipHeaderMid\">" f)
      (write-line "   <Font ss:Bold=\"1\"/>" f)
      (write-line "   <Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)

      (write-line "  <Style ss:ID=\"SkipHeaderRight\">" f)
      (write-line "   <Font ss:Bold=\"1\"/>" f)
      (write-line "   <Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"2\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)

      (write-line "  <Style ss:ID=\"SkipDataLeft\">" f)
      (write-line "   <Font ss:Color=\"#FF0000\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"2\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)

      (write-line "  <Style ss:ID=\"SkipDataMid\">" f)
      (write-line "   <Font ss:Color=\"#FF0000\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)

      (write-line "  <Style ss:ID=\"SkipDataRight\">" f)
      (write-line "   <Font ss:Color=\"#FF0000\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"2\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)

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

      (write-line " <Worksheet ss:Name=\"Раскрой\">" f)
      (write-line "  <Table>" f)

      (write-line "   <Column ss:Index=\"1\" ss:AutoFitWidth=\"0\" ss:Width=\"60\"/>" f)
      (write-line "   <Column ss:Index=\"2\" ss:AutoFitWidth=\"1\" ss:Width=\"200\"/>" f)
      (write-line "   <Column ss:Index=\"3\" ss:AutoFitWidth=\"0\" ss:Width=\"120\"/>" f)
      (write-line "   <Column ss:Index=\"4\" ss:AutoFitWidth=\"0\" ss:Width=\"100\"/>" f)
      (write-line "   <Column ss:Index=\"5\" ss:AutoFitWidth=\"0\" ss:Width=\"120\"/>" f)

      (write-line "   <Row ss:Height=\"20\">" f)
      (write-line "    <Cell ss:StyleID=\"Header\" ss:MergeAcross=\"4\"><Data ss:Type=\"String\">Раскрой хлыстов</Data></Cell>" f)
      (write-line "   </Row>" f)

      (write-line "   <Row>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Хлыст</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Детали</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Использовано</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Отход</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Использование_%</Data></Cell>" f)
      (write-line "   </Row>" f)

      (setq i 0)
      (foreach bar bars
        (setq i (1+ i))
        (setq pieces (cdr bar) waste (car bar) used (- stock waste)
              util (* 100.0 (/ used stock)))
        (write-line "   <Row>" f)
        (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (itoa i) "</Data></Cell>") f)
        (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"String\">" (n1-list-to-str pieces " + ") "</Data></Cell>") f)
        (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (rtos used 2 1) "</Data></Cell>") f)
        (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (rtos waste 2 1) "</Data></Cell>") f)
        (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (rtos util 2 1) "</Data></Cell>") f)
        (write-line "   </Row>" f)
      )

      (write-line "   <Row>" f)
      (write-line "    <Cell><Data ss:Type=\"String\"></Data></Cell>" f)
      (write-line "   </Row>" f)

      (write-line "   <Row>" f)
      (write-line "    <Cell ss:Index=\"2\" ss:StyleID=\"ReportTitle\" ss:MergeAcross=\"2\"><Data ss:Type=\"String\">ОТЧЕТ</Data></Cell>" f)
      (write-line "   </Row>" f)

      (write-line "   <Row>" f)
      (write-line "    <Cell ss:Index=\"2\" ss:StyleID=\"ReportLabel\"><Data ss:Type=\"String\">Длина хлыста (заготовка):</Data></Cell>" f)
      (write-line (strcat "    <Cell ss:StyleID=\"ReportValueRight\" ss:MergeAcross=\"1\"><Data ss:Type=\"String\">" (itoa (fix stock)) " мм</Data></Cell>") f)
      (write-line "   </Row>" f)

      (setq total-cnt 0 total-product-mm 0.0)
      (foreach bar bars
        (foreach p (cdr bar)
          (setq total-cnt (1+ total-cnt))
          (setq total-product-mm (+ total-product-mm p))
        )
      )
      (write-line "   <Row>" f)
      (write-line "    <Cell ss:Index=\"2\" ss:StyleID=\"ReportLabel\"><Data ss:Type=\"String\">Всего изделий:</Data></Cell>" f)
      (write-line (strcat "    <Cell ss:StyleID=\"ReportValueRight\" ss:MergeAcross=\"1\"><Data ss:Type=\"String\">" (itoa total-cnt) " шт</Data></Cell>") f)
      (write-line "   </Row>" f)

      (write-line "   <Row>" f)
      (write-line "    <Cell ss:Index=\"2\" ss:StyleID=\"ReportLabel\"><Data ss:Type=\"String\">Суммарная длина:</Data></Cell>" f)
      (write-line (strcat "    <Cell ss:StyleID=\"ReportValueRight\" ss:MergeAcross=\"1\"><Data ss:Type=\"String\">" (rtos (/ product-total-mm 1000.0) 2 2) " м.п.</Data></Cell>") f)
      (write-line "   </Row>" f)

      (write-line "   <Row>" f)
      (write-line "    <Cell ss:Index=\"2\" ss:StyleID=\"ReportLabel\"><Data ss:Type=\"String\">Хлыстов:</Data></Cell>" f)
      (write-line (strcat "    <Cell ss:StyleID=\"ReportValueRight\" ss:MergeAcross=\"1\"><Data ss:Type=\"String\">" (itoa num-bars) " шт</Data></Cell>") f)
      (write-line "   </Row>" f)

      (write-line "   <Row>" f)
      (write-line "    <Cell ss:Index=\"2\" ss:StyleID=\"ReportLabel\"><Data ss:Type=\"String\">Общая длина хлыстов:</Data></Cell>" f)
      (write-line (strcat "    <Cell ss:StyleID=\"ReportValueRight\" ss:MergeAcross=\"1\"><Data ss:Type=\"String\">" (rtos (/ stock-total-mm 1000.0) 2 2) " м.п.</Data></Cell>") f)
      (write-line "   </Row>" f)

      (write-line "   <Row>" f)
      (write-line "    <Cell ss:Index=\"2\" ss:StyleID=\"ReportKpdLabel\"><Data ss:Type=\"String\">КПД использования:</Data></Cell>" f)
      (write-line (strcat "    <Cell ss:StyleID=\"ReportKpd\" ss:MergeAcross=\"1\"><Data ss:Type=\"String\">" (rtos kpd 2 1) " %</Data></Cell>") f)
      (write-line "   </Row>" f)

      (if oversized
        (progn
          (write-line "   <Row>" f)
          (write-line "    <Cell><Data ss:Type=\"String\"></Data></Cell>" f)
          (write-line "   </Row>" f)

          (write-line "   <Row>" f)
          (write-line "    <Cell ss:Index=\"2\" ss:StyleID=\"SectionTitle\" ss:MergeAcross=\"2\"><Data ss:Type=\"String\">НЕРАЗМЕЩЕННЫЕ ДЕТАЛИ (длиннее хлыста)</Data></Cell>" f)
          (write-line "   </Row>" f)

          (write-line "   <Row>" f)
          (write-line "    <Cell ss:Index=\"2\" ss:StyleID=\"SkipHeaderLeft\"><Data ss:Type=\"String\">Длина, мм</Data></Cell>" f)
          (write-line "    <Cell ss:StyleID=\"SkipHeaderMid\"><Data ss:Type=\"String\">Кол-во, шт</Data></Cell>" f)
          (write-line "    <Cell ss:StyleID=\"SkipHeaderRight\"><Data ss:Type=\"String\">Сумма, м.п.</Data></Cell>" f)
          (write-line "   </Row>" f)

          (foreach rec oversized
            (write-line "   <Row>" f)
            (write-line (strcat "    <Cell ss:Index=\"2\" ss:StyleID=\"SkipDataLeft\"><Data ss:Type=\"Number\">" (itoa (fix (car rec))) "</Data></Cell>") f)
            (write-line (strcat "    <Cell ss:StyleID=\"SkipDataMid\"><Data ss:Type=\"Number\">" (itoa (cadr rec)) "</Data></Cell>") f)
            (write-line (strcat "    <Cell ss:StyleID=\"SkipDataRight\"><Data ss:Type=\"Number\">" (rtos (/ (* (car rec) (cadr rec)) 1000.0) 2 2) "</Data></Cell>") f)
            (write-line "   </Row>" f)
          )

          (write-line "   <Row>" f)
          (write-line (strcat "    <Cell ss:Index=\"2\" ss:StyleID=\"SkipTotal\" ss:MergeAcross=\"2\"><Data ss:Type=\"String\">Всего неразмещенных: " (itoa total-cnt-unplaced) " шт, " (rtos total-sum-unplaced 2 2) " м.п.</Data></Cell>") f)
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

;; ---------- CSV-экспорт (fallback) ----------
(defun n1-write-csv (bars stock kerf oversized /
                       fname f i bar pieces waste used util rec
                       total-cnt-unplaced total-sum-unplaced)
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
        (setq pieces (cdr bar) waste (car bar) used (- stock waste)
              util (* 100.0 (/ used stock)))
        (write-line (strcat (itoa i) ";" (n1-list-to-str pieces " + ") ";"
                            (rtos used 2 1) ";" (rtos waste 2 1) ";"
                            (rtos util 2 1)) f)
      )
      (if oversized
        (progn
          (setq oversized (vl-sort oversized '(lambda (a b) (> (car a) (car b)))))

          (setq total-cnt-unplaced 0 total-sum-unplaced 0.0)
          (foreach rec oversized
            (setq total-cnt-unplaced (+ total-cnt-unplaced (cadr rec)))
            (setq total-sum-unplaced (+ total-sum-unplaced
                                        (/ (* (car rec) (cadr rec)) 1000.0)))
          )
          (write-line "" f)
          (write-line "НЕРАЗМЕЩЕННЫЕ ДЕТАЛИ (длиннее хлыста)" f)
          (write-line "Длина, мм;Кол-во, шт;Сумма, м.п." f)
          (foreach rec oversized
            (write-line
              (strcat (itoa (fix (car rec))) ";" (itoa (cadr rec)) ";"
                      (vl-string-translate "." ","
                        (rtos (/ (* (car rec) (cadr rec)) 1000.0) 2 2))) f)
          )
          (write-line (strcat "Всего;" (itoa total-cnt-unplaced) ";"
                              (vl-string-translate "." ","
                                (rtos total-sum-unplaced 2 2))) f)
        )
      )
      (close f)
      (princ (strcat "\nCSV сохранен: " fname))
      T
    )
    nil
  )
)

;; ============================================================
;; Главная функция
;; ============================================================
(defun cutline-main (layers-from-caller / *error* ss tol stock kerf insPt
                       pieces pieces-ok pieces-oversized split
                       sorted bars
                       bbox0 bbox1 bbox2 bbox3 bbox-frame bbox p1 p2 color-map
                       barHeight sumInsPt num-bars stock-total-mm
                       total-cnt total-product-mm kpd rec blockName baseName
                       lastEnt ssNew ent oldEcho doc uMark
                       blkRefsSet blkRefsBefore blkIdx blkRefsAfter blkCmdResult
                       layers layers-str total-input type-counts
                       user-filter line-cnt mline-cnt dynblock-cnt
                       dynblock-type mline-type
                       export-xls export-acad
                       default-xls default-acad
                       default-stock default-kerf
                       dialog-result r xls-ok
                       old-transparency-display)
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*,*EXIT*")))
      (princ (strcat "\n[CUTLINE ERROR] " msg)))
    (if (and uMark doc) (vl-catch-all-apply 'vla-EndUndoMark (list doc)))
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (princ))
  

  (princ "\n=== Линейный раскрой мерного материала ===")

  (if (eq layers-from-caller 'ASK)
    (progn
      (setq layers-str (getstring T "\nВведите слои через запятую (Enter — все слои): "))
      (if (/= layers-str "")
        (setq layers (mapcar '(lambda (x) (vl-string-trim " " x))
                             (n1-split-string layers-str ",")))
        (setq layers nil)
      )
    )
    (setq layers layers-from-caller)
  )

  (princ (strcat "\n" (car (n1-layer-display-list layers))))

  (princ "\nВыберите объекты — исходные детали:")
  (princ "\n(принимаются LINE, MLINE и динамические блоки с длиной)")
  (princ "\n(если объекты уже выделены — Enter)")

  (setq ss (su-select-cutline-objects layers))

  (if (null ss)
    (progn (princ "\nНичего не выбрано.") (princ) (exit)))

  (setq total-input (sslength ss))
  (princ (strcat "\nВыбрано объектов: " (itoa total-input)))

  ;; Этап 2 (V5): лимит количества деталей
  (if (> total-input *n1-max-parts*)
    (progn
      (princ (strcat "\n[CUTLINE][GUARD] Обнаружено " (itoa total-input)
                      " деталей. Обработка остановлена. Лимит: " (itoa *n1-max-parts*)
                      ". Проверьте выборку/слои."))
      (princ) (exit)))

  (setq type-counts (n1-count-by-type ss))
  (n1-print-type-counts type-counts)

  (setq line-cnt     (cdr (assoc "LINE" type-counts)))
  (setq mline-cnt    (cdr (assoc "MLINE" type-counts)))
  (setq dynblock-cnt (cdr (assoc "DYNBLOCK" type-counts)))
  (if (null line-cnt)     (setq line-cnt 0))
  (if (null mline-cnt)    (setq mline-cnt 0))
  (if (null dynblock-cnt) (setq dynblock-cnt 0))

  (if (and (= line-cnt 0) (= mline-cnt 0) (= dynblock-cnt 0))
    (progn
      (princ "\nНет объектов подходящих типов.")
      (princ) (exit)
    )
  )

  (setq default-stock *CUTLINE-LAST-STOCK*)
  (setq default-kerf  *CUTLINE-LAST-KERF*)
  (setq default-xls   *CUTLINE-LAST-XLS*)
  (setq default-acad  *CUTLINE-LAST-ACAD*)

  (if (not (eq layers-from-caller 'ASK))
    (progn
      (if (boundp '*CUTLINE-CREATE-XLS*)
        (setq default-xls *CUTLINE-CREATE-XLS*))
      (if (boundp '*CUTLINE-CREATE-TABLE*)
        (setq default-acad *CUTLINE-CREATE-TABLE*))
    )
  )

  (setq tol *CUTLINE-DEFAULT-TOL*)

  (setq r (vl-catch-all-apply
            'n1-cutline-dialog
            (list line-cnt mline-cnt dynblock-cnt ss *CUTLINE-DEFAULT-TOL*
                  default-stock default-kerf layers default-xls default-acad)))

  (cond
    ((vl-catch-all-error-p r)
     (princ (strcat "\nОшибка диалога: " (vl-catch-all-error-message r)))
     (princ "\nРаскрой отменен.")
     (princ) (exit)
    )
    ((null r)
     (princ "\nРаскрой отменен пользователем.")
     (princ) (exit)
    )
    (T (setq dialog-result r))
  )

  (setq user-filter   (car dialog-result)
        tol           (cadr dialog-result)
        stock         (caddr dialog-result)
        kerf          (cadddr dialog-result)
        export-xls    (nth 4 dialog-result)
        export-acad   (nth 5 dialog-result)
        dynblock-type (nth 6 dialog-result)
        mline-type    (nth 7 dialog-result))

  (setq *CUTLINE-LAST-STOCK* stock
        *CUTLINE-LAST-KERF*  kerf
        *CUTLINE-LAST-XLS*   export-xls
        *CUTLINE-LAST-ACAD*  export-acad)

  (princ "\nПараметры приняты из окна диалога.")

  (cond
    ((eq user-filter 'LINE)
     (setq ss (n1-filter-ss-by-type ss "LINE"))
     (princ "\nОставлены только линии (LINE)."))
    ((eq user-filter 'MLINE)
     (setq ss (n1-filter-ss-by-mline-type ss mline-type))
     (if (and mline-type (/= mline-type ""))
       (princ (strcat "\nОставлены мультилинии стиля: " mline-type))
       (princ "\nОставлены все мультилинии."))
    )
    ((eq user-filter 'DYNBLOCK)
     (setq ss (n1-filter-ss-by-type-and-name ss "DYNBLOCK" dynblock-type))
     (if (and dynblock-type (/= dynblock-type "") (/= dynblock-type "Все типы блоков"))
       (princ (strcat "\nОставлены динамические блоки типа: " dynblock-type))
       (princ "\nОставлены все динамические блоки."))
    )
    (T (princ "\nОставлены линии, мультилинии и динамические блоки."))
  )

  (if (= (sslength ss) 0)
    (progn (princ "\nПосле фильтрации не осталось объектов.") (princ) (exit)))

  (setq type-counts (n1-count-by-type ss))
  (n1-print-type-counts type-counts)

  (princ (strcat "\nПараметры: допуск " (rtos tol 2 2)
                 " мм, хлыст " (rtos stock 2 0)
                 " мм, рез " (rtos kerf 2 2) " мм."))
  (princ (strcat "\nЭкспорт: "
                 (if export-xls  ".xls" "без .xls") ", "
                 (if export-acad "таблица AutoCAD" "без таблицы")))

  (setq pieces (n1-extract-pieces ss tol
                                  *CUTLINE-MIN-LENGTH* *CUTLINE-MAX-LENGTH*))
  (if (null pieces)
    (progn (princ "\nНе удалось извлечь длины.") (princ) (exit)))

  (setq total-cnt 0)
  (foreach rec pieces (setq total-cnt (+ total-cnt (cadr rec))))
  (princ (strcat "\nВсего деталей: " (itoa (length pieces))
                 ", общее количество: " (itoa total-cnt)))

  (setq split (n1-split-by-stock pieces stock))
  (setq pieces-ok (car split) pieces-oversized (cadr split))

  (n1-report-oversized pieces-oversized stock)

  (if (null pieces-ok)
    (progn
      (princ "\nВсе детали превышают длину хлыста. Раскрой невозможен.")
      (princ) (exit)
    )
  )

  (setq sorted (n1-expand pieces-ok))
  (setq bars (n1-ffd sorted stock kerf))
  (n1-report bars stock kerf)

  (setq num-bars (length bars))
  (setq stock-total-mm (* num-bars stock))
  (setq total-cnt 0 total-product-mm 0.0)
  (foreach rec pieces-ok
    (setq total-cnt (+ total-cnt (cadr rec)))
    (setq total-product-mm (+ total-product-mm (* (car rec) (cadr rec))))
  )
  (setq kpd (if (> stock-total-mm 0)
              (* 100.0 (/ total-product-mm stock-total-mm)) 0.0))
  (princ (strcat "\nВсего изделий: " (itoa total-cnt)
                 " шт, суммарная длина "
                 (rtos (/ total-product-mm 1000.0) 2 2) " м.п."))
  (princ (strcat "\nХлыстов: " (itoa num-bars)
                 " шт, общая длина "
                 (rtos (/ stock-total-mm 1000.0) 2 2) " м.п."))
  (princ (strcat "\nКПД использования: " (rtos kpd 2 1) " %"))

  (if export-xls
    (progn
      (setq xls-ok
        (n1-write-xls bars stock kerf pieces-oversized
                      num-bars stock-total-mm total-product-mm kpd))
      (if (not xls-ok)
        (progn
          (princ "\nНе удалось создать XLS. Сохраняю CSV...")
          (n1-write-csv bars stock kerf pieces-oversized)
        )
      )
    )
    (princ "\nГалочка .xls снята — файл не создается.")
  )

  (if export-acad
    (progn
      (setq insPt (getpoint "\nУкажите точку вставки раскладки: "))
      (if insPt
        (progn
          (setq old-transparency-display (getvar "TRANSPARENCYDISPLAY"))
          (n1-enable-transparency-display)

          (n1-ensure-italic-style)
          (setq color-map (n1-build-color-map pieces-ok))
          (setq baseName (vl-filename-base (getvar "DWGNAME")))
          (setq blockName (n1-unique-block-name (strcat "Раскрой " baseName)))

          (setq doc (vl-catch-all-apply 'vla-get-ActiveDocument
                                        (list (vlax-get-acad-object))))
          (if (and (not (vl-catch-all-error-p doc)) doc)
            (progn
              (vla-StartUndoMark doc)
              (setq uMark T)
            )
          )

          (setq lastEnt (entlast))

          ;; ШАПКА КАРТЫ РАСКРОЯ (над первым хлыстом)
          (setq bbox0 (n1-draw-header insPt stock kerf))

          ;; Раскладка хлыстов
          (setq bbox1 (n1-draw-layout bars stock kerf insPt color-map))
          (setq barHeight (/ stock 30.0))

          ;; РАМКА вокруг шапки и хлыстов (без таблиц)
          (setq bbox-frame (n1-draw-frame (n1-combine-bbox bbox0 bbox1)))

          ;; Таблицы сдвинуты правее рамки, чтобы не пересекаться
          (setq sumInsPt (list (+ (car (cadr bbox-frame)) (* barHeight 2.0))
                               (cadr (cadr bbox-frame))))
          (setq bbox2 (n1-draw-summary bars pieces-ok stock sumInsPt color-map
                                        pieces-oversized))
          (setq bbox3
            (if pieces-oversized
              (n1-draw-oversized pieces-oversized stock
                (list (car (car bbox2))
                      (- (cadr (car bbox2)) (* barHeight 2.0))))
              nil))

          (setq ssNew (ssadd))
          (if lastEnt
            (setq ent (entnext lastEnt))
            (setq ent (entnext)))
          (while ent
            (ssadd ent ssNew)
            (setq ent (entnext ent)))

          (if (> (sslength ssNew) 0)
            (progn
              (setq oldEcho (getvar "CMDECHO"))
              (setvar "CMDECHO" 0)
              ;; Число INSERT с этим именем ДО -BLOCK (защита от двойного INSERT, Шаг 4):
              ;; в версиях AutoCAD, где -BLOCK спрашивает [Преобразовать/Удалить],
              ;; ENTER выбирает «Преобразовать» и INSERT создается самим -BLOCK.
              (setq blkRefsSet (ssget "_X" (list '(0 . "INSERT") (cons 2 blockName))))
              (setq blkRefsBefore (if blkRefsSet (sslength blkRefsSet) 0))
              (setq blkCmdResult
                    (vl-catch-all-apply 'vl-cmdf
                      (list "_.-BLOCK" blockName insPt ssNew "")))
              (setvar "CMDECHO" oldEcho)
              (cond
                ((vl-catch-all-error-p blkCmdResult)
                 (princ (strcat "\nОшибка при создании блока: "
                                (vl-catch-all-error-message blkCmdResult))))
                ((not (tblsearch "BLOCK" blockName))
                 (princ "\nНе удалось создать блок."))
                (T
                 (progn
                  (setq blkRefsSet (ssget "_X" (list '(0 . "INSERT") (cons 2 blockName))))
                  (setq blkRefsAfter (if blkRefsSet (sslength blkRefsSet) 0))
                  (cond
                    ((= blkRefsAfter (1+ blkRefsBefore))
                     ;; Ровно один новый INSERT — его создал сам -BLOCK (Шаг 4), второй не нужен.
                     (princ (strcat "\nСоздан блок с раскладкой: " blockName
                                    " (INSERT создан самим -BLOCK, ровно 1)")))
                    ((> blkRefsAfter (1+ blkRefsBefore))
                     (princ (strcat "\nСоздан блок с раскладкой: " blockName
                                    "\nВНИМАНИЕ: после -BLOCK новых INSERT: " (itoa (- blkRefsAfter blkRefsBefore))
                                    " (ожидался 1) — повторная вставка отменена.")))
                    (T
(progn
                      ;; INSERT не создан: стираем оригиналы, пережившие -BLOCK
                      ;; (режим «оставить»), и вставляем ровно один INSERT.
                      (setq blkIdx 0)
                      (repeat (sslength ssNew)
                        (setq ent (ssname ssNew blkIdx))
                        (if (entget ent) (entdel ent))
                        (setq blkIdx (1+ blkIdx)))
                      (n1-block-insert blockName insPt)
                      ;; Контроль (Шаг 4): должен остаться ровно один новый INSERT
                      (setq blkRefsSet (ssget "_X" (list '(0 . "INSERT") (cons 2 blockName))))
                      (setq blkRefsAfter (if blkRefsSet (sslength blkRefsSet) 0))
                      (princ (strcat "\nСоздан блок с раскладкой: " blockName
                                     " (ссылок до: " (itoa blkRefsBefore)
                                     ", после: " (itoa blkRefsAfter) ")"
                                     (if (= blkRefsAfter (1+ blkRefsBefore))
                                       ""
                                       " (ВНИМАНИЕ: прирост не равен 1!)")))))))))
            )
            (princ "\nНет объектов для создания блока.")
          )

          ;; Объединяем bbox: рамка (уже включает шапку и хлысты) + таблицы
          (setq bbox (n1-combine-bbox bbox-frame
                        (if bbox3 (n1-combine-bbox bbox2 bbox3) bbox2)))
          (setq p1 (vlax-3d-point (list (car (car bbox)) (cadr (car bbox)) 0.0)))
          (setq p2 (vlax-3d-point (list (car (cadr bbox)) (cadr (cadr bbox)) 0.0)))
          (vl-catch-all-apply 'vla-ZoomWindow (list (vlax-get-acad-object) p1 p2))

          (if (and uMark doc)
            (progn
              (vla-EndUndoMark doc)
              (setq uMark nil)
            )
          )

          (n1-disable-transparency-display old-transparency-display)
        )
        (princ "\nРаскладка пропущена.")
      )
    )
    (princ "\nГалочка Таблица AutoCAD снята — раскладка не строится.")
  )

  (princ)
)

;; ============================================================
;; Автономные команды
;; ============================================================
(defun c:cutline () (cutline-main 'ASK))
(defun c:РАСКРОЙХЛЫСТА () (cutline-main 'ASK))

(princ "\nCUTLINE.LSP загружен (ред. 1: лимиты V5/V6). Команды: CUTLINE, РАСКРОЙХЛЫСТА")
(princ)