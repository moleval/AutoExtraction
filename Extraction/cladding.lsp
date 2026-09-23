;;; ============================================================
;;; CLADDING.LSP — Облицовка. РЕДАКЦИЯ 29.
;;; Полилинии К1-К3, блоки Б1-Б4, диспетчер do-blocks.
;;; Хвосты функций — по одной закрывающей скобке на строку.
;;; ============================================================

(vl-load-com)

(if (not (boundp '*CLADDING-CHECK-AREA*))
  (setq *CLADDING-CHECK-AREA* nil)
)

;; ============================================================
;; ЧАСТЬ 1: ПОЛИЛИНИИ
;; ============================================================

(setq *cladding-skipped-open*   0)
(setq *cladding-skipped-zero*   0)
(setq *cladding-with-arcs*      0)
(setq *cladding-check-mismatch* 0)

(defun cl-round3 (x)
  (/ (fix (+ (* x 1000.0) 0.5)) 1000.0)
)

(defun cl-round2 (x)
  (/ (fix (+ (* x 100.0) 0.5)) 100.0)
)

(defun cl-format-area (area / int-part frac-hundredths)
  (setq int-part (fix area))
  (setq frac-hundredths (fix (+ (* (- area int-part) 100.0) 0.5)))
  (cond
    ((= frac-hundredths 0) (itoa int-part))
    ((= (rem frac-hundredths 10) 0)
     (strcat (itoa int-part) "," (itoa (/ frac-hundredths 10))))
    (T
     (if (< frac-hundredths 10)
       (strcat (itoa int-part) ",0" (itoa frac-hundredths))
       (strcat (itoa int-part) "," (itoa frac-hundredths))))
  )
)

(defun cl-split-string (str delim / pos result item)
  (setq result '())
  (while (setq pos (vl-string-search delim str))
    (setq item (vl-string-trim " " (substr str 1 pos)))
    (setq result (cons item result))
    (setq str (substr str (+ pos 2))))
  (setq item (vl-string-trim " " str))
  (if (> (strlen item) 0) (setq result (cons item result)))
  (reverse result)
)

(defun cl-poly-vertices (ent / pts)
  (setq pts '())
  (foreach g (entget ent)
    (cond
      ((= (car g) 10) (setq pts (cons (cons (cdr g) 0.0) pts)))
      ((= (car g) 42)
       (if pts (setq pts (cons (cons (caar pts) (cdr g)) (cdr pts))))))
  )
  (reverse pts)
)

(defun cl-poly-closed-p (ent / f)
  (setq f (cdr (assoc 70 (entget ent))))
  (if f (= 1 (logand 1 f)) nil)
)

(defun cl-poly-has-arcs (vb)
  (vl-some '(lambda (v) (> (abs (cdr v)) 1e-8)) vb)
)

(defun cl-green-area (ent / vb n i s v1 v2 p1 p2 b c r u nx ny
                            mx my d cx cy a1 a2 dl)
  (setq vb (cl-poly-vertices ent))
  (setq n (length vb))
  (if (< n 3)
    0.0
    (progn
      (setq s 0.0 i 0)
      (while (< i n)
        (setq v1 (nth i vb) v2 (nth (rem (1+ i) n) vb))
        (setq p1 (car v1) p2 (car v2) b (cdr v1))
        (if (> (abs b) 1e-8)
          (progn
            (setq c (distance p1 p2))
            (setq r (/ (* c (+ 1.0 (* b b))) (* 4.0 (abs b))))
            (setq u (list (/ (- (car p2) (car p1)) c)
                          (/ (- (cadr p2) (cadr p1)) c)))
            (setq nx (- (cadr u)) ny (car u))
            (setq mx (/ (+ (car p1) (car p2)) 2.0)
                  my (/ (+ (cadr p1) (cadr p2)) 2.0))
            (setq d (/ (* c (- 1.0 (* b b))) (* 4.0 b)))
            (setq cx (+ mx (* nx d)) cy (+ my (* ny d)))
            (setq a1 (atan (- (cadr p1) cy) (- (car p1) cx)))
            (setq a2 (atan (- (cadr p2) cy) (- (car p2) cx)))
            (setq dl (- a2 a1))
            (if (> b 0)
              (while (<= dl 0.0) (setq dl (+ dl (* 2.0 pi))))
              (while (>= dl 0.0) (setq dl (- dl (* 2.0 pi)))))
            (setq s (+ s (* 0.5 (+ (* r r dl)
                                   (* cx r (- (sin a2) (sin a1)))
                                   (* cy r (- (cos a1) (cos a2))))))))
          (setq s (+ s (/ (- (* (car p1) (cadr p2))
                             (* (car p2) (cadr p1))) 2.0))))
        (setq i (1+ i)))
      (abs s)))
)

(defun cl-poly-area (ent / obj a)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
  (setq a (if (vl-catch-all-error-p obj) nil
              (vl-catch-all-apply 'vla-get-Area (list obj))))
  (cond
    ((and a (not (vl-catch-all-error-p a)) (numberp a) (> a 0.0)) a)
    (T (cl-green-area ent)))
)

(defun cl-check-area (ent a / g)
  (if *CLADDING-CHECK-AREA*
    (progn
      (setq g (cl-green-area ent))
      (if (> (abs (- a g)) 1.0)
        (setq *cladding-check-mismatch* (1+ *cladding-check-mismatch*)))))
)

;; Запись полилинии: (слой номинал высота ширина площадь)
(defun cl-poly-record (ent / vb layer area xs ys dx dy nominal
                             minx maxx miny maxy)
  (setq layer (cdr (assoc 8 (entget ent))))
  (if (not (cl-poly-closed-p ent))
    (progn (setq *cladding-skipped-open* (1+ *cladding-skipped-open*)) nil)
    (progn
      (setq vb (cl-poly-vertices ent))
      (if (cl-poly-has-arcs vb)
        (setq *cladding-with-arcs* (1+ *cladding-with-arcs*)))
      (setq area (cl-poly-area ent))
      (cl-check-area ent area)
      (if (or (null area) (<= area 0.0))
        (progn (setq *cladding-skipped-zero* (1+ *cladding-skipped-zero*)) nil)
        (progn
          (setq xs (mapcar '(lambda (v) (car (car v))) vb)
                ys (mapcar '(lambda (v) (cadr (car v))) vb))
          (setq minx (apply 'min xs) maxx (apply 'max xs)
                miny (apply 'min ys) maxy (apply 'max ys))
          (setq dx (- maxx minx) dy (- maxy miny))
          (if (> (abs (- area (* dx dy))) (max 1.0 (* 0.01 dx dy)))
            (setq nominal "_НЕПРЯМОУГ_")
            (setq nominal (strcat (itoa (fix (+ dx 0.5))) "x"
                                  (itoa (fix (+ dy 0.5))))))
          (list layer nominal (fix (+ dy 0.5)) (fix (+ dx 0.5))
                (cl-round3 (/ area 1e6)))))))
)

(defun cl-collect (layers / inserts rec records)
  (setq *cladding-skipped-open* 0 *cladding-skipped-zero* 0
        *cladding-with-arcs* 0 *cladding-check-mismatch* 0)
  (setq inserts (su-select-lwpolylines layers))
  (setq records '())
  (foreach ent inserts
    (setq rec (cl-poly-record ent))
    (if rec (setq records (cons rec records))))
  (reverse records)
)

;; Агрегат полилиний: (ключ слой номинал высота ширина кол-во площадь)
;; Агрегат полилиний: (ключ слой номинал высота ширина кол-во площадь)
;; Сортировка: слой ? тип (номинал) ? высота ? ширина
(defun cl-aggregate (records detail / acc rec key found out)
  (setq acc '())
  (foreach rec records
    (setq key (if detail (strcat (car rec) "|" (cadr rec)) (car rec)))
    (setq found (assoc key acc))
    (if found
      (if (= (cadr rec) "_НЕПРЯМОУГ_")
        (setq acc (subst (list key (car rec) (cadr rec)
                               (max (nth 3 found) (nth 2 rec))
                               (max (nth 4 found) (nth 3 rec))
                               (1+ (nth 5 found))
                               (+ (nth 6 found) (nth 4 rec))) found acc))
        (setq acc (subst (list key (car rec) (cadr rec)
                               (nth 2 rec) (nth 3 rec)
                               (1+ (nth 5 found))
                               (+ (nth 6 found) (nth 4 rec))) found acc)))
      (setq acc (cons (list key (car rec) (cadr rec)
                            (nth 2 rec) (nth 3 rec) 1 (nth 4 rec)) acc))))
  (setq out (vl-sort acc
    '(lambda (a b)
       (cond
         ;; Слой
         ((< (strcase (nth 1 a)) (strcase (nth 1 b))) T)
         ((> (strcase (nth 1 a)) (strcase (nth 1 b))) nil)
         ;; Высота (nth 3)
         ((< (nth 3 a) (nth 3 b)) T)
         ((> (nth 3 a) (nth 3 b)) nil)
         ;; Ширина (nth 4)
         ((< (nth 4 a) (nth 4 b)) T)
         ((> (nth 4 a) (nth 4 b)) nil)
         (T nil)))))
  out
)

(defun cl-report (data report-mode / total-cnt total-area rec cur-layer)
  (setq total-cnt 0 total-area 0.0 cur-layer nil)
  (foreach rec data
    (setq total-cnt (+ total-cnt (nth 5 rec)))
    (setq total-area (+ total-area (cl-round2 (nth 6 rec))))
    (if (= (strcase report-mode) "DETAIL")
      (progn
        (if (not (equal cur-layer (cadr rec)))
          (progn (setq cur-layer (cadr rec))
                 (princ (strcat "\n  Слой: " cur-layer))))
        (princ (strcat "\n    " (caddr rec) ": " (itoa (nth 5 rec)) " шт, "
                       (cl-format-area (cl-round2 (nth 6 rec))) " м2")))
      (princ (strcat "\n  " (cadr rec) ": " (itoa (nth 5 rec)) " шт, "
                     (cl-format-area (cl-round2 (nth 6 rec))) " м2"))))
  (princ (strcat "\nИтого: " (itoa total-cnt) " шт, "
                 (cl-format-area (cl-round2 total-area)) " м2"))
)

(defun cl-warnings ()
  (if (> *cladding-skipped-open* 0)
    (princ (strcat "\nПредупреждение: исключено незамкнутых полилиний: "
                   (itoa *cladding-skipped-open*))))
  (if (> *cladding-skipped-zero* 0)
    (princ (strcat "\nПредупреждение: исключено полилиний без площади: "
                   (itoa *cladding-skipped-zero*))))
  (if (> *cladding-with-arcs* 0)
    (princ (strcat "\nПредупреждение: полилиний со скруглениями (дугами): "
                   (itoa *cladding-with-arcs*)
                   " - учтены с точным расчетом площади")))
  (if (> *cladding-check-mismatch* 0)
    (princ (strcat "\nВНИМАНИЕ: расхождение площади > 1 мм2: "
                   (itoa *cladding-check-mismatch*) " эл.")))
)

;; ============================================================
;; ЧАСТЬ 2: ДИНАМИЧЕСКИЕ БЛОКИ (Б1-Б4)
;; ============================================================

(setq *cladding-block-total* 0)
(setq *cladding-block-regular* 0)
(setq *cladding-block-cut* 0)
(setq *cladding-block-skipped-nodim* 0)
(setq *cladding-block-skipped-zero* 0)

(defun cl-block-all-props (obj / dynprops prop pname pval out)
  (setq out '())
  (setq dynprops (vl-catch-all-apply 'vlax-invoke
                    (list obj 'GetDynamicBlockProperties)))
  (if (not (vl-catch-all-error-p dynprops))
    (foreach prop dynprops
      (setq pname (vl-catch-all-apply 'vla-get-PropertyName (list prop)))
      (if (and (not (vl-catch-all-error-p pname)) pname (= (type pname) 'STR))
        (progn
          (setq pname (vl-string-trim " \t\r\n" pname))
          (setq pval (vl-catch-all-apply 'vla-get-Value (list prop)))
          (if (not (vl-catch-all-error-p pval))
            (setq out (cons (cons pname pval) out)))))))
  (reverse out)
)

(defun cl-block-get-num (props name / found)
  (setq found nil)
  (foreach p props
    (if (and (null found) (= (strcase (car p)) (strcase name)))
      (setq found (cdr p))))
  (if found (su-value-to-number found) nil)
)

;; Запись блока: (слой тип Ширина Высота площадь подрезная D E F G)
(defun cl-block-record (ent / obj layer name vis display is-target is-cut
                          props B C D E F G area)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
  (if (vl-catch-all-error-p obj)
    nil
    (progn
      (setq layer (cdr (assoc 8 (entget ent))))
      (setq name (su-get-effective-name obj))
      (if (or (null name) (vl-catch-all-error-p name)) (setq name ""))
      (setq vis (su-get-visibility obj))
      (if (or (null vis) (vl-catch-all-error-p vis)) (setq vis ""))
      (setq display (if (> (strlen vis) 0) vis name))
      (setq is-target
        (or (vl-string-search "КАССЕТА" (strcase name))
            (vl-string-search "ПАНЕЛЬ"  (strcase name))
            (vl-string-search "КАССЕТА" (strcase vis))
            (vl-string-search "ПАНЕЛЬ"  (strcase vis))))
      (if (not is-target)
        nil
        (progn
          (setq is-cut
            (or (vl-string-search "ПОДРЕЗНАЯ" (strcase name))
                (vl-string-search "ПОДРЕЗНАЯ" (strcase vis))))
          (setq props (cl-block-all-props obj))
          (setq B (cl-block-get-num props "Высота"))
          (setq C (cl-block-get-num props "Ширина"))
          (if (or (null B) (null C) (<= B 0.0) (<= C 0.0))
            (progn (setq *cladding-block-skipped-nodim*
                     (1+ *cladding-block-skipped-nodim*)) nil)
            (progn
              (if is-cut
                (progn
                  (setq D (cl-block-get-num props "Правый угол"))
                  (setq E (cl-block-get-num props "Левый угол"))
                  (setq F (cl-block-get-num props "Верхний угол"))
                  (setq G (cl-block-get-num props "Нижний угол"))
                  (if (null D) (setq D 0.0))
                  (if (null E) (setq E 0.0))
                  (if (null F) (setq F 0.0))
                  (if (null G) (setq G 0.0))
                  (setq area (/ (- (* B C)
                                   (* (- C D) G)
                                   (* (- C D) (- B F))
                                   (* E G)
                                   (* E (- B F))) 1000000.0)))
                (progn
                  (setq D 0.0 E 0.0 F 0.0 G 0.0)
                  (setq area (/ (* B C) 1000000.0))))
              (if (<= area 0.0)
                (progn (setq *cladding-block-skipped-zero*
                         (1+ *cladding-block-skipped-zero*)) nil)
                (list layer display C B (cl-round3 area) is-cut D E F G))))))))
)

(defun cl-collect-blocks (layers / inserts rec records)
  (setq *cladding-block-total* 0 *cladding-block-regular* 0
        *cladding-block-cut* 0 *cladding-block-skipped-nodim* 0
        *cladding-block-skipped-zero* 0)
  (setq inserts (su-select-inserts layers))
  (setq records '())
  (foreach ent inserts
    (setq rec (cl-block-record ent))
    (if rec
      (progn
        (setq records (cons rec records))
        (setq *cladding-block-total* (1+ *cladding-block-total*))
        (if (nth 5 rec)
          (setq *cladding-block-cut* (1+ *cladding-block-cut*))
          (setq *cladding-block-regular* (1+ *cladding-block-regular*))))))
  (reverse records)
)

;; Агрегат блоков: (ключ слой тип rC rB кол-во площадь подрезная)
;; Сортировка: слой ? тип ? высота ? ширина (для всех кассет)
(defun cl-blocks-aggregate (records / acc rec layer disp c b area is-cut
                             rC rB key found)
  (setq acc '())
  (foreach rec records
    (setq layer  (nth 0 rec)
          disp   (nth 1 rec)
          c      (nth 2 rec)
          b      (nth 3 rec)
          area   (nth 4 rec)
          is-cut (nth 5 rec))
    (setq rC (fix (+ c 0.5)))
    (setq rB (fix (+ b 0.5)))
    (setq key (strcat layer "|" disp "|" (itoa rC) "x" (itoa rB)))
    (setq found (assoc key acc))
    (if found
      (setq acc (subst (list key layer disp rC rB
                             (1+ (nth 5 found))
                             (+ (nth 6 found) area)
                             is-cut) found acc))
      (setq acc (cons (list key layer disp rC rB 1 area is-cut) acc))))
  (setq acc (vl-sort acc
    '(lambda (a b)
       (cond
         ;; Слой
         ((< (strcase (nth 1 a)) (strcase (nth 1 b))) T)
         ((> (strcase (nth 1 a)) (strcase (nth 1 b))) nil)
         ;; Тип
         ((< (strcase (nth 2 a)) (strcase (nth 2 b))) T)
         ((> (strcase (nth 2 a)) (strcase (nth 2 b))) nil)
         ;; Высота (nth 4)
         ((< (nth 4 a) (nth 4 b)) T)
         ((> (nth 4 a) (nth 4 b)) nil)
         ;; Ширина (nth 3)
         ((< (nth 3 a) (nth 3 b)) T)
         ((> (nth 3 a) (nth 3 b)) nil)
         (T nil)))))
  acc
)

(defun cl-blocks-report (groups / grp cur-layer total-cnt total-area size-str)
  (setq total-cnt 0 total-area 0.0 cur-layer nil)
  (foreach grp groups
    (setq total-cnt (+ total-cnt (nth 5 grp)))
    (setq total-area (+ total-area (cl-round2 (nth 6 grp))))
    (if (not (equal cur-layer (nth 1 grp)))
      (progn (setq cur-layer (nth 1 grp))
             (princ (strcat "\nСлой \"" cur-layer "\":"))))
    (setq size-str (strcat (itoa (nth 3 grp)) "x" (itoa (nth 4 grp))))
    (if (nth 7 grp)
      (setq size-str (strcat size-str " (опис. прямоуг.)")))
    (princ (strcat "\n  " (nth 2 grp) "   " size-str "   "
                   (itoa (nth 5 grp)) " шт   "
                   (cl-format-area (cl-round2 (nth 6 grp))))))
  (princ (strcat "\nИтого по блокам: " (itoa total-cnt) " шт, "
                 (cl-format-area (cl-round2 total-area)) " м2"))
)

(defun cl-clean-type-name (name / s word pos)
  (if (null name)
    ""
    (progn
      (setq s name word "рядовая")
      (while (setq pos (vl-string-search (strcase word) (strcase s)))
        (setq s (strcat (substr s 1 pos)
                        (substr s (+ pos (strlen word) 1)))))
      (while (vl-string-search "  " s)
        (setq s (vl-string-subst " " "  " s)))
      (vl-string-trim " " s)))
)

(defun cl-group-blocks-by-layer (data / result cur-layer cur-rows grp)
  (setq result '() cur-layer nil cur-rows '())
  (foreach grp data
    (if (not (equal (nth 1 grp) cur-layer))
      (progn
        (if cur-layer
          (setq result (cons (cons cur-layer (reverse cur-rows)) result)))
        (setq cur-layer (nth 1 grp) cur-rows (list grp)))
      (setq cur-rows (cons grp cur-rows))))
  (if cur-layer
    (setq result (cons (cons cur-layer (reverse cur-rows)) result)))
  (reverse result)
)

;; ---------- плоский список элементов (универсальный) ----------
;; элемент данных: (data индексСлоя . строка); подитог: (подитог индексСлоя слой кол-во площадь)
(defun cl-build-flat-items (data / layerGroups items lg layer rows
                              layCnt layArea r layerIdx)
  (setq layerGroups (cl-group-blocks-by-layer data))
  (setq items '() layerIdx 0)
  (foreach lg layerGroups
    (setq layer (car lg) rows (cdr lg))
    (setq layCnt 0 layArea 0.0)
    (foreach r rows
      (setq layCnt (+ layCnt (nth 5 r)))
      (setq layArea (+ layArea (cl-round2 (nth 6 r)))))
    (foreach r rows
      (setq items (append items (list (cons 'data (cons layerIdx r))))))
    (setq items (append items
      (list (list 'subtotal layerIdx layer layCnt layArea))))
    (setq layerIdx (1+ layerIdx)))
  items
)

;; ---------- сборка кусков ----------
(defun cl-build-block-units (data idealRows / items chunks ch result)
  (setq items (cl-build-flat-items data))
  (setq chunks (tc-partition-flat items idealRows))
  (setq result '())
  (foreach ch chunks
    (setq result (append result (list (cons (length ch) ch)))))
  result
)

;; ---------- Б3: таблица блоков (подробный, 7 колонок) ----------
(defun cl-create-blocks-table (data /
    pt pt_wcs units total-chunks chunk-idx is-last chunk items item
    total-cnt total-area grp layer layerIdx subCnt subArea
    nCols nRows space tbl row itemNum oldEcho doc
    lastLayerIdx rowInLayer maxLayerLen maxTypeLen layerStr typeStr
    maxNumLen layerGroups lg layCnt numStr col0Width r)
  (if (null data)
    (progn (princ "\nНет данных блоков для таблицы.") nil)
    (progn
      (setq pt (getpoint "\nУкажите точку вставки таблицы блоков: "))
      (if (null pt)
        (progn (princ "\nТаблица блоков пропущена.") nil)
        (progn
          (setq doc (vlax-get-acad-object))
          (setq doc (vla-get-activedocument doc))
          (setq space (vla-get-modelspace doc))
          (setq pt_wcs (trans pt 1 0))
          (setq oldEcho (getvar "CMDECHO"))
          (vl-catch-all-apply 'setvar (list "CMDECHO" 0))
          (tu-undo-begin)
          (setq maxLayerLen 10 maxTypeLen 10)
          (foreach grp data
            (setq layerStr (nth 1 grp))
            (setq typeStr (cl-clean-type-name (nth 2 grp)))
            (if (> (strlen layerStr) maxLayerLen)
              (setq maxLayerLen (strlen layerStr)))
            (if (> (strlen typeStr) maxTypeLen)
              (setq maxTypeLen (strlen typeStr))))
          ;; Итоги и автоподгон — один проход по слоям
          (setq total-cnt 0 total-area 0.0 maxNumLen 3)
          (setq layerGroups (cl-group-blocks-by-layer data))
          (setq layerIdx 1)
          (foreach lg layerGroups
            (setq layCnt 0 layArea 0.0)
            (foreach r (cdr lg)
              (setq layCnt (+ layCnt (nth 5 r)))
              (setq layArea (+ layArea (cl-round2 (nth 6 r)))))
            (setq total-cnt (+ total-cnt layCnt))
            (setq total-area (+ total-area (cl-round2 layArea)))
            (setq numStr (strcat (itoa layerIdx) "." (itoa layCnt)))
            (if (> (strlen numStr) maxNumLen)
              (setq maxNumLen (strlen numStr)))
            (setq layerIdx (1+ layerIdx)))
          (setq col0Width (max 15.0 (* (+ maxNumLen 1) 3.5)))
          (setq units (cl-build-block-units data *TU-IDEAL-ROWS*))
          (setq total-chunks (length units))
          (setq chunk-idx 0 itemNum 0 nCols 7)
          (setq lastLayerIdx -1 rowInLayer 0)
          (foreach chunk units
            (setq is-last (tu-is-last-chunk chunk-idx total-chunks))
            (setq nRows (+ 2 (car chunk)))
            (if is-last (setq nRows (1+ nRows)))
            (setq items (cdr chunk))
            (setq tbl (vl-catch-all-apply 'vla-addtable
              (list space (vlax-3d-point pt_wcs) nRows nCols 10.0 50.0)))
            (if (vl-catch-all-error-p tbl)
              (princ (strcat "\nОшибка создания таблицы блоков: "
                             (vl-catch-all-error-message tbl)))
              (progn
                (vla-SetColumnWidth tbl 0 col0Width)
                (vla-SetColumnWidth tbl 1 (* maxLayerLen 3.5))
                (vla-SetColumnWidth tbl 2 (* maxTypeLen 3.5))
                (vla-SetColumnWidth tbl 3 30.0)
                (vla-SetColumnWidth tbl 4 30.0)
                (vla-SetColumnWidth tbl 5 25.0)
                (vla-SetColumnWidth tbl 6 30.0)
                (ts-ac-title tbl 0 "Облицовка (динамические блоки)" 7)
                (ts-ac-header tbl 1
                  '("№" "Слой" "Тип" "Высота, мм" "Ширина, мм" "Кол-во, шт." "Площадь, м2"))
                (setq row 2)
                (foreach item items
                  (if (eq (car item) 'data)
                    (progn
                      (setq layerIdx (cadr item))
                      (setq grp (cddr item))
                      (if (/= layerIdx lastLayerIdx)
                        (progn
                          (setq lastLayerIdx layerIdx)
                          (setq rowInLayer 0)))
                      (setq rowInLayer (1+ rowInLayer))
                      (vla-SetText tbl row 0
                        (strcat (itoa (1+ layerIdx)) "." (itoa rowInLayer)))
                      (vla-SetText tbl row 1 (nth 1 grp))
                      (vla-SetText tbl row 2 (cl-clean-type-name (nth 2 grp)))
                      (vla-SetText tbl row 3 (itoa (nth 4 grp)))
                      (vla-SetText tbl row 4 (itoa (nth 3 grp)))
                      (vla-SetText tbl row 5 (itoa (nth 5 grp)))
                      (vla-SetText tbl row 6
                        (cl-format-area (cl-round2 (nth 6 grp))))
                      (vla-SetCellAlignment tbl row 0 5)
                      (vla-SetCellAlignment tbl row 1 4)
                      (vla-SetCellAlignment tbl row 2 4)
                      (vla-SetCellAlignment tbl row 3 5)
                      (vla-SetCellAlignment tbl row 4 5)
                      (vla-SetCellAlignment tbl row 5 5)
                      (vla-SetCellAlignment tbl row 6 5)
                      (setq row (1+ row)))
                    (progn
                      (setq layerIdx (nth 1 item))
                      (setq layer (nth 2 item))
                      (setq subCnt (nth 3 item))
                      (setq subArea (nth 4 item))
                      (ts-ac-subtotal tbl row (1+ layerIdx)
                        (strcat "   Итого: " layer) 1 4)
                      (vla-SetText tbl row 5 (itoa subCnt))
                      (vla-SetCellAlignment tbl row 5 5)
                      (vla-SetText tbl row 6
                        (cl-format-area (cl-round2 subArea)))
                      (vla-SetCellAlignment tbl row 6 5)
                      (setq row (1+ row)))))
                (if is-last
                  (progn
                    (ts-ac-total tbl row "           {\\LИтого по всем позициям:}" 0 4 4)
                    (vla-SetText tbl row 5 (itoa total-cnt))
                    (vla-SetCellAlignment tbl row 5 5)
                    (vla-SetText tbl row 6
                      (cl-format-area (cl-round2 total-area)))
                    (vla-SetCellAlignment tbl row 6 5)))
                (vla-update tbl)
                (princ (strcat "\nТаблица блоков "
                               (itoa (1+ chunk-idx)) " создана."))
                (setq pt_wcs (tu-next-table-point pt_wcs nRows 10.0 20.0))))
            (setq chunk-idx (1+ chunk-idx)))
          (tu-undo-end doc)
          (vl-catch-all-apply 'setvar (list "CMDECHO" oldEcho))
          (princ (strcat "\nВсего создано таблиц блоков: "
                         (itoa total-chunks)))
          T
        )
      )
    )
  )
)

;; ---------- Б3: сводная таблица блоков (краткий, 4 колонки) ----------
(defun cl-create-blocks-table-summary (data / pt pt_wcs layerGroups lg layer
                                          g cnt area total-cnt total-area
                                          nCols nRows space tbl row oldEcho doc)
  (if (null data)
    (progn (princ "\nНет данных блоков для таблицы.") nil)
    (progn
      (setq pt (getpoint "\nУкажите точку вставки таблицы блоков: "))
      (if (null pt)
        (progn (princ "\nТаблица блоков пропущена.") nil)
        (progn
          (setq doc (vlax-get-acad-object))
          (setq doc (vla-get-activedocument doc))
          (setq space (vla-get-modelspace doc))
          (setq pt_wcs (trans pt 1 0))
          (setq oldEcho (getvar "CMDECHO"))
          (vl-catch-all-apply 'setvar (list "CMDECHO" 0))
          (tu-undo-begin)
          (setq layerGroups (cl-group-blocks-by-layer data))
          (setq nCols 4)
          (setq nRows (+ 3 (length layerGroups)))
          (setq tbl (vl-catch-all-apply 'vla-addtable
                    (list space (vlax-3d-point pt_wcs) nRows nCols 10.0 50.0)))
          (if (vl-catch-all-error-p tbl)
            (princ (strcat "\nОшибка создания таблицы блоков: "
                           (vl-catch-all-error-message tbl)))
            (progn
              (vla-SetColumnWidth tbl 0 15.0)
              (vla-SetColumnWidth tbl 1 150.0)
              (vla-SetColumnWidth tbl 2 25.0)
              (vla-SetColumnWidth tbl 3 30.0)
              (ts-ac-title tbl 0 "Облицовка (динамические блоки)" 4)
              (ts-ac-header tbl 1 '("№" "Слой" "Кол-во, шт." "Площадь, м2"))
              (setq row 2 total-cnt 0 total-area 0.0)
              (foreach lg layerGroups
                (setq layer (car lg))
                (setq cnt 0 area 0.0)
                (foreach g (cdr lg)
                  (setq cnt  (+ cnt  (nth 5 g)))
                  (setq area (+ area (cl-round2 (nth 6 g)))))
                (setq total-cnt  (+ total-cnt  cnt))
                (setq total-area (+ total-area (cl-round2 area)))
                (vla-SetText tbl row 0 (itoa (- row 1)))
                (vla-SetText tbl row 1 layer)
                (vla-SetText tbl row 2 (itoa cnt))
                (vla-SetText tbl row 3 (cl-format-area (cl-round2 area)))
                (vla-SetCellAlignment tbl row 0 5)
                (vla-SetCellAlignment tbl row 1 4)
                (vla-SetCellAlignment tbl row 2 5)
                (vla-SetCellAlignment tbl row 3 5)
                (setq row (1+ row)))
              (ts-ac-total tbl row "        {\\LИтого по всем позициям:}" 0 1 4)
              (vla-SetText tbl row 2 (itoa total-cnt))
              (vla-SetCellAlignment tbl row 2 5)
              (vla-SetText tbl row 3 (cl-format-area (cl-round2 total-area)))
              (vla-SetCellAlignment tbl row 3 5)
              (vla-update tbl)
              (princ "\nТаблица блоков (кратко) создана.")))
          (tu-undo-end doc)
          (vl-catch-all-apply 'setvar (list "CMDECHO" oldEcho))
          T
        )
      )
    )
  )
)

;; ============================================================
;; Б4: ЭКСПОРТ БЛОКОВ В XLS/CSV
;; ============================================================
(defun cl-blocks-write-xls (groups report-mode fname / f brd grp total-cnt total-area
                              layerGroups lg layer layer-cnt layer-area
                              grpCnt grpArea itemNum row-num is-cut
                              layer-start-row layer-end-row subtotal-row
                              subtotal-rows qsum psum r detail)
  (setq f (open fname "w"))
  (if (null f)
    nil
    (progn
      (setq detail (= (strcase report-mode) "DETAIL"))
      (setq brd (strcat "<Borders>"
        "<Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>"
        "<Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>"
        "<Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>"
        "<Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>"
        "</Borders>"))
      (eu-doc-begin f "BASIC")
(write-line " <Styles>" f)
      (write-line "  <Style ss:ID=\"Default\"><Alignment ss:Vertical=\"Center\"/></Style>" f)
      (write-line (strcat "  <Style ss:ID=\"Title\"><Font ss:Bold=\"1\" ss:Size=\"12\" ss:Underline=\"Single\"/>"
                "<Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>"
                "<Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" brd "</Style>") f)
      (write-line (strcat "  <Style ss:ID=\"Header\"><Font ss:Bold=\"1\"/>"
                "<Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>"
                "<Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" brd "</Style>") f)
      (write-line (strcat "  <Style ss:ID=\"Data\"><Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" brd "</Style>") f)
      (write-line (strcat "  <Style ss:ID=\"DataLeft\"><Alignment ss:Horizontal=\"Left\" ss:Vertical=\"Center\"/>" brd "</Style>") f)
      (write-line (strcat "  <Style ss:ID=\"Cut\"><Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>"
                "<Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" brd "</Style>") f)
      (write-line (strcat "  <Style ss:ID=\"Total\"><Font ss:Bold=\"1\"/>"
                "<Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>"
                "<Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" brd "</Style>") f)
      (write-line (strcat "  <Style ss:ID=\"Num\"><NumberFormat ss:Format=\"0.00\"/>"
                "<Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" brd "</Style>") f)
      (write-line (strcat "  <Style ss:ID=\"TotalNum\"><Font ss:Bold=\"1\"/>"
                "<Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>"
                "<NumberFormat ss:Format=\"0.00\"/>"
                "<Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" brd "</Style>") f)
      (write-line " </Styles>" f)

      (eu-worksheet f "Облицовка (блоки)")

      (if detail
        ;; ========== ПОДРОБНЫЙ ==========
        (progn
          (eu-column f "40" "0")
          (eu-column f "120" "0")
          (eu-column f "80" "0")
          (eu-column f "40" "0")
          (eu-column f "40" "0")
          (eu-column f "30" "0")
          (eu-column f "40" "0")
          (eu-row-begin f "ss:Height=\"20\"")
          (eu-cell f "Title" "String" "Облицовка (динамические блоки)" "ss:MergeAcross=\"6\"")
          (eu-row-end f)
          (eu-row-begin f "")
          (eu-cell f "Header" "String" "№" "")
          (eu-cell f "Header" "String" "Слой" "")
          (eu-cell f "Header" "String" "Тип" "")
          (eu-cell f "Header" "String" "Высота, мм" "")
          (eu-cell f "Header" "String" "Ширина, мм" "")
          (eu-cell f "Header" "String" "Кол-во, шт" "")
          (eu-cell f "Header" "String" "Площадь, м2" "")
          (eu-row-end f)
          (setq total-cnt 0 total-area 0.0 itemNum 0 row-num 3 subtotal-rows '())
          (setq layerGroups (cl-group-blocks-by-layer groups))
          (foreach lg layerGroups
            (setq layer (car lg))
            (setq layer-cnt 0 layer-area 0.0)
            (setq layer-start-row row-num)
            (foreach grp (cdr lg)
              (setq itemNum (1+ itemNum))
              (setq grpCnt (nth 5 grp))
              (setq grpArea (nth 6 grp))
              (setq is-cut (nth 7 grp))
              (setq layer-cnt (+ layer-cnt grpCnt))
              (setq layer-area (+ layer-area (cl-round2 grpArea)))
              (setq total-cnt (+ total-cnt grpCnt))
              (eu-row-begin f "")
              (eu-cell f "Data" "Number" (itoa itemNum) "")
              (eu-cell f "DataLeft" "String" (nth 1 grp) "")
              (eu-cell f "DataLeft" "String" (cl-clean-type-name (nth 2 grp)) "")
              (eu-cell f "Data" "Number" (itoa (nth 4 grp)) "")
              (eu-cell f "Data" "Number" (itoa (nth 3 grp)) "")
              (eu-cell f "Data" "Number" (itoa grpCnt) "")
              (if is-cut
                (eu-cell f "Cut" "Number" (rtos (cl-round2 grpArea) 2 2) "")
                (eu-cell f "Num" "Number" (rtos (cl-round2 grpArea) 2 2)
                         (strcat "ss:Formula=\"=ROUND(R" (itoa row-num) "C4*R"
                                 (itoa row-num) "C5/1000000*R" (itoa row-num) "C6,2)\"")))
              (eu-row-end f)
              (setq row-num (1+ row-num)))
            (setq total-area (+ total-area (cl-round2 layer-area)))
            (setq layer-end-row (1- row-num))
            (setq subtotal-row row-num)
            (setq subtotal-rows (append subtotal-rows (list subtotal-row)))
            (eu-row-begin f "")
            (eu-cell f "Total" "String" (strcat "        Итого: " layer) "ss:MergeAcross=\"4\"")
            (eu-cell f "Total" "Number" (itoa layer-cnt)
                     (strcat "ss:Formula=\"=SUM(R" (itoa layer-start-row) "C6:R"
                             (itoa layer-end-row) "C6)\""))
            (eu-cell f "TotalNum" "Number" (rtos (cl-round2 layer-area) 2 2)
                     (strcat "ss:Formula=\"=ROUND(SUM(R" (itoa layer-start-row) "C7:R"
                             (itoa layer-end-row) "C7),2)\""))
            (eu-row-end f)
            (setq row-num (1+ row-num)))
          (setq qsum "=SUM(")
          (setq psum "=ROUND(SUM(")
          (foreach r subtotal-rows
            (setq qsum (strcat qsum "R" (itoa r) "C6,"))
            (setq psum (strcat psum "R" (itoa r) "C7,")))
          (setq qsum (strcat (substr qsum 1 (1- (strlen qsum))) ")"))
          (setq psum (strcat (substr psum 1 (1- (strlen psum))) "),2)"))
          (eu-row-begin f "")
          (eu-cell f "Total" "String" "        Итого по всем позициям:" "ss:MergeAcross=\"4\"")
          (eu-cell f "Total" "Number" (itoa total-cnt) (strcat "ss:Formula=\"" qsum "\""))
          (eu-cell f "TotalNum" "Number" (rtos (cl-round2 total-area) 2 2) (strcat "ss:Formula=\"" psum "\""))
          (eu-row-end f))

        ;; ========== КРАТКИЙ ==========
        (progn
          (eu-column f "40" "0")
          (eu-column f "150" "0")
          (eu-column f "40" "0")
          (eu-column f "40" "0")
          (eu-row-begin f "ss:Height=\"20\"")
          (eu-cell f "Title" "String" "Облицовка (динамические блоки)" "ss:MergeAcross=\"3\"")
          (eu-row-end f)
          (eu-row-begin f "")
          (eu-cell f "Header" "String" "№" "")
          (eu-cell f "Header" "String" "Слой" "")
          (eu-cell f "Header" "String" "Кол-во, шт" "")
          (eu-cell f "Header" "String" "Площадь, м2" "")
          (eu-row-end f)
          (setq total-cnt 0 total-area 0.0 itemNum 0 row-num 3)
          (setq layerGroups (cl-group-blocks-by-layer groups))
          (foreach lg layerGroups
            (setq layer (car lg))
            (setq layer-cnt 0 layer-area 0.0)
            (foreach grp (cdr lg)
              (setq layer-cnt (+ layer-cnt (nth 5 grp)))
              (setq layer-area (+ layer-area (cl-round2 (nth 6 grp)))))
            (setq total-cnt (+ total-cnt layer-cnt))
            (setq total-area (+ total-area (cl-round2 layer-area)))
            (setq itemNum (1+ itemNum))
            (eu-row-begin f "")
            (eu-cell f "Data" "Number" (itoa itemNum) "")
            (eu-cell f "DataLeft" "String" layer "")
            (eu-cell f "Data" "Number" (itoa layer-cnt) "")
            (eu-cell f "Num" "Number" (rtos (cl-round2 layer-area) 2 2) "")
            (eu-row-end f)
            (setq row-num (1+ row-num)))
          (eu-row-begin f "")
          (eu-cell f "Total" "String" "Итого по всем позициям:" "ss:MergeAcross=\"1\"")
          (eu-cell f "Total" "Number" (itoa total-cnt) "")
          (eu-cell f "TotalNum" "Number" (rtos (cl-round2 total-area) 2 2) "")
          (eu-row-end f)))

      (eu-worksheet-end f)
      (eu-doc-end f)
      (close f)
      T
    )
  )
)

(defun cl-blocks-write-csv (groups fname / f grp total-cnt total-area
                              layerGroups lg layer layer-cnt layer-area
                              grpCnt grpArea itemNum)
  (setq f (open fname "w"))
  (if (null f)
    nil
    (progn
      (write-line "Слой;Тип;Высота, мм;Ширина, мм;Кол-во, шт;Площадь, м2" f)
      (setq total-cnt 0 total-area 0.0 itemNum 0)
      (setq layerGroups (cl-group-blocks-by-layer groups))
      (foreach lg layerGroups
        (setq layer (car lg))
        (setq layer-cnt 0 layer-area 0.0)
        (foreach grp (cdr lg)
          (setq itemNum (1+ itemNum))
          (setq grpCnt (nth 5 grp))
          (setq grpArea (nth 6 grp))
          (setq layer-cnt (+ layer-cnt grpCnt))
          (setq layer-area (+ layer-area grpArea))
          (setq total-cnt (+ total-cnt grpCnt))
          (setq total-area (+ total-area grpArea))
          (write-line (strcat (nth 1 grp) ";"
                              (cl-clean-type-name (nth 2 grp)) ";"
                              (itoa (nth 4 grp)) ";"
                              (itoa (nth 3 grp)) ";"
                              (itoa grpCnt) ";"
                              (cl-format-area (cl-round2 grpArea))) f))
        (write-line (strcat "        Итого: " layer ";;;;"
                            (itoa layer-cnt) ";"
                            (cl-format-area (cl-round2 layer-area))) f))
      (write-line (strcat "        Итого по всем позициям:;;;;"
                          (itoa total-cnt) ";"
                          (cl-format-area (cl-round2 total-area))) f)
      (close f)
      T
    )
  )
)

;; ============================================================
;; ГЛОБАЛКИ ПОСЛЕДНЕГО ПРОГОНА ПОЛИЛИНИЙ (К2)
;; ============================================================
(if (not (boundp '*CLADDING-LAST-RECORDS*)) (setq *CLADDING-LAST-RECORDS* nil))
(if (not (boundp '*CLADDING-LAST-DATA*))    (setq *CLADDING-LAST-DATA* nil))
(if (not (boundp '*CLADDING-LAST-MODE*))    (setq *CLADDING-LAST-MODE* nil))

;; ---------- подбор свободного имени файла ----------
;; Проба режимом "a" (не уссекает живой файл). Если имя занято
;; (открыто в Excel) - подбираем суффикс (1), (2), ...
;; Возвращает путь, который реально можно открыть на запись.
(defun cl-free-path (base ext / p i fh)
  (setq p (strcat base ext) i 1)
  (setq fh (open p "a"))
  (while (null fh)
    (setq p (strcat base " (" (itoa i) ")" ext))
    (setq i (1+ i))
    (setq fh (open p "a")))
  (close fh)
  p
)

;; ============================================================
;; ОСНОВНАЯ ФУНКЦИЯ (7-й параметр do-blocks)
;; ============================================================
(defun cladding-main (layers report-mode export-excel export-txt
                      create-table save-base do-blocks
                      / *error* svSaved records data xls-base xlsfile csvfile
                        brec bgroups bxls-base bxls bcsv)
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg)
                           "*BREAK*,*CANCEL*,*QUIT*,*EXIT*,*ПРЕРВА*")))
      (princ (strcat "\nОшибка: " msg))
    )
    (tu-sysvar-restore svSaved)
    (princ)
  )

  ;; V8: guard - обрыв вернёт CMDECHO
  (setq svSaved (tu-sysvar-save '("CMDECHO")))

  ;; ---------- Часть 1: полилинии ----------
  (princ "\n=== Облицовка: сбор полилиний ===")
  (setq records (cl-collect layers))
  (if records
    (progn
      (setq data (cl-aggregate records (= (strcase report-mode) "DETAIL")))
      (setq *CLADDING-LAST-RECORDS* records)
      (setq *CLADDING-LAST-DATA* data)
      (setq *CLADDING-LAST-MODE* report-mode)
      (cl-report data report-mode)
      (cl-warnings)
      (if export-excel
        (progn
          (setq xls-base
            (if save-base
              (strcat save-base " "
                (if (= (strcase report-mode) "DETAIL") "подробный" "краткий"))
              (strcat (getvar "DWGPREFIX")
                      (vl-filename-base (getvar "DWGNAME"))
                      " Облицовка полилинии "
                      (if (= (strcase report-mode) "DETAIL") "подробный" "краткий"))))
          (setq xlsfile (strcat xls-base ".xls"))
          (if (cl-write-xls data report-mode xlsfile)
            (princ (strcat "\nXLS сохранен: " xlsfile))
            (progn
              (princ "\nНе удалось сохранить XLS. Сохраняю CSV...")
              (setq csvfile (cl-free-path xls-base ".csv"))
              (if (cl-write-csv data report-mode csvfile)
                (princ (strcat "\nCSV сохранен: " csvfile))
                (princ "\nНе удалось создать CSV.")))))
      )
      (if export-txt (princ "\nTXT: для облицовки не предусмотрен."))
      (if create-table
        (if (= (strcase report-mode) "DETAIL")
          (cl-create-table-detail data)
          (cl-create-table-summary data)
        )
      )
    )
    (progn
      (princ "\nДанных по полилиниям нет.")
      (cl-warnings)
    )
  )

  ;; ---------- Часть 2: блоки (если запрошено диспетчером) ----------
  (if do-blocks
    (progn
      (princ "\n=== Облицовка: сбор блоков ===")
      (setq brec (cl-collect-blocks layers))
      (if brec
        (progn
          (setq bgroups (cl-blocks-aggregate brec))
          (cl-blocks-report bgroups)
          (if create-table
            (if (= (strcase report-mode) "DETAIL")
              (cl-create-blocks-table bgroups)
              (cl-create-blocks-table-summary bgroups)
            )
          )
          (if export-excel
            (progn
              (setq bxls-base (strcat (getvar "DWGPREFIX")
                                      (vl-filename-base (getvar "DWGNAME"))
                                      " Облицовка динамические блоки "
                                      (if (= (strcase report-mode) "DETAIL") "подробный" "краткий")))
              (setq bxls (strcat bxls-base ".xls"))
              (if (cl-blocks-write-xls bgroups report-mode bxls)
                (princ (strcat "\nXLS сохранен: " bxls))
                (progn
                  (princ "\nНе удалось сохранить XLS. Сохраняю CSV...")
                  (setq bcsv (cl-free-path bxls-base ".csv"))
                  (if (cl-blocks-write-csv bgroups bcsv)
                    (princ (strcat "\nCSV сохранен: " bcsv))
                    (princ "\nНе удалось создать CSV.")))))
            )
          )
        )
        (princ "\nБлоки облицовки не найдены.")
      )
    )
  
  (princ)
)

;; ============================================================
;; АВТОНОМНЫЕ КОМАНДЫ
;; ============================================================
(defun c:cladding ( / layers-str layers report-mode)
  (vl-load-com)
  (setq layers-str (getstring T "\nСлои через запятую (Enter - все слои): "))
  (if (= layers-str "")
    (setq layers nil)
    (setq layers (mapcar '(lambda (x) (strcase (vl-string-trim " " x)))
                         (cl-split-string layers-str ","))))
  (initget "D S")
  (setq report-mode (getkword "\nРежим отчета [Подробный(D)/Краткий(S)] <S>: "))
  (if (null report-mode) (setq report-mode "S"))
  (setq report-mode (if (= report-mode "D") "DETAIL" "SUMMARY"))
  (cladding-main layers report-mode nil nil nil nil nil)
  (princ)
)

(defun c:ОБЛИЦОВКА () (c:cladding))

(defun c:clblocks ( / layers-str layers records groups export-excel xls-base xlsfile csvfile)
  (vl-load-com)
  (setq layers-str (getstring T "\nСлои через запятую (Enter - все слои): "))
  (if (= layers-str "")
    (setq layers nil)
    (setq layers (mapcar '(lambda (x) (strcase (vl-string-trim " " x)))
                         (cl-split-string layers-str ","))))
  (setq records (cl-collect-blocks layers))
  (princ "\n--- Блоки облицовки ---")
  (princ (strcat "\nНайдено кассет/панелей: " (itoa *cladding-block-total*)))
  (princ (strcat "\n  обычных:   " (itoa *cladding-block-regular*)))
  (princ (strcat "\n  подрезных: " (itoa *cladding-block-cut*)))
  (if (> *cladding-block-skipped-nodim* 0)
    (princ (strcat "\nПропущено (нет Высоты/Ширины): "
                   (itoa *cladding-block-skipped-nodim*))))
  (if (> *cladding-block-skipped-zero* 0)
    (princ (strcat "\nПропущено (площадь <= 0): "
                   (itoa *cladding-block-skipped-zero*))))
  (setq groups (cl-blocks-aggregate records))
  (if groups
    (progn
      (cl-blocks-report groups)
      (cl-create-blocks-table groups)
      (initget "Y N")
      (setq export-excel (getkword "\nЭкспорт в Excel? [Да(Y)/Нет(N)] <N>: "))
      (if (and export-excel (= export-excel "Y"))
        (progn
          (setq xls-base
            (strcat (getvar "DWGPREFIX")
                    (vl-filename-base (getvar "DWGNAME"))
                    " Облицовка динамические блоки"))
          (setq xlsfile (strcat xls-base ".xls"))
          (if (cl-blocks-write-xls groups "DETAIL" xlsfile)
            (princ (strcat "\nXLS сохранен: " xlsfile))
            (progn
              (princ "\nНе удалось сохранить XLS. Сохраняю CSV...")
              (setq csvfile (strcat xls-base ".csv"))
              (if (cl-blocks-write-csv groups csvfile)
                (princ (strcat "\nCSV сохранен: " csvfile))
                (princ "\nНе удалось создать CSV."))))))))
  (princ)
)

;; ============================================================
;; ТАБЛИЦЫ ПОЛИЛИНИЙ (К2)
;; ============================================================
(defun cl-create-table-summary (data / pt tbl row nRows nCols space
                                    rec total-cnt total-area maxLayerLen layerStr)
  (setq pt (getpoint "\nУкажите точку вставки таблицы: "))
  (if (null pt)
    (progn (princ "\nТаблица пропущена.") nil)
    (progn
      (setvar "CMDECHO" 0)
      (setq nCols 4)
      (setq nRows (+ 3 (length data)))
      (setq space (vla-get-modelspace
                    (vla-get-activedocument (vlax-get-acad-object))))
      (setq tbl (vla-addtable space (vlax-3d-point pt) nRows nCols 10.0 50.0))
      
      (setq maxLayerLen 10)
      (foreach rec data
        (setq layerStr (nth 1 rec))
        (if (> (strlen layerStr) maxLayerLen)
          (setq maxLayerLen (strlen layerStr))))
      
      (vla-SetColumnWidth tbl 0 15.0)
      (vla-SetColumnWidth tbl 1 (* maxLayerLen 3.5))
      (vla-SetColumnWidth tbl 2 30.0)
      (vla-SetColumnWidth tbl 3 35.0)
      (ts-ac-title tbl 0 "Облицовка (полилинии)" 4)
      (ts-ac-header tbl 1 '("№" "Слой" "Кол-во, шт." "Площадь, м2"))
      (setq row 2 total-cnt 0 total-area 0.0)
      (foreach rec data
        (setq total-cnt (+ total-cnt (nth 5 rec)))
        (setq total-area (+ total-area (cl-round2 (nth 6 rec))))
        (vla-SetText tbl row 0 (itoa (1+ (- row 2))))
        (vla-SetText tbl row 1 (cadr rec))
        (vla-SetText tbl row 2 (itoa (nth 5 rec)))
        (vla-SetText tbl row 3 (cl-format-area (cl-round2 (nth 6 rec))))
        (vla-SetCellAlignment tbl row 0 5)
        (vla-SetCellAlignment tbl row 1 4)
        (vla-SetCellAlignment tbl row 2 5)
        (vla-SetCellAlignment tbl row 3 5)
        (setq row (1+ row)))
      (ts-ac-total tbl row "{\\LИтого по всем позициям:}" 0 1 5)
      (vla-SetText tbl row 2 (itoa total-cnt))
      (vla-SetCellAlignment tbl row 2 5)
      (vla-SetText tbl row 3 (cl-format-area (cl-round2 total-area)))
      (vla-SetCellAlignment tbl row 3 5)
      (vla-update tbl)
      (setvar "CMDECHO" 1)
      tbl
    )
  )
)

;; ---------- К2: таблица полилиний подробная (кускованная, 6 колонок) ----------
(defun cl-create-table-detail (data /
    pt pt_wcs units total-chunks chunk-idx is-last chunk items item
    total-cnt total-area grp layer layerIdx subCnt subArea
    nCols nRows space tbl row itemNum oldEcho doc
    lastLayerIdx rowInLayer maxLayerLen layerStr
    maxNumLen layerGroups lg layCnt numStr col0Width r)
  (if (null data)
    (progn (princ "\nНет данных для таблицы.") nil)
    (progn
      (setq pt (getpoint "\nУкажите точку вставки таблицы: "))
      (if (null pt)
        (progn (princ "\nТаблица пропущена.") nil)
        (progn
          (setq doc (vlax-get-acad-object))
          (setq doc (vla-get-activedocument doc))
          (setq space (vla-get-modelspace doc))
          (setq pt_wcs (trans pt 1 0))
          (setq oldEcho (getvar "CMDECHO"))
          (vl-catch-all-apply 'setvar (list "CMDECHO" 0))
          (tu-undo-begin)
          (setq maxLayerLen 10)
          (foreach grp data
            (setq layerStr (nth 1 grp))
            (if (> (strlen layerStr) maxLayerLen)
              (setq maxLayerLen (strlen layerStr))))
          ;; Добавляем запас для русских символов и пробелов
          (setq maxLayerLen (+ maxLayerLen 2))
          ;; Итоги и автоподгон — один проход по слоям
          (setq total-cnt 0 total-area 0.0 maxNumLen 3.5)
          (setq layerGroups (cl-group-blocks-by-layer data))
          (setq layerIdx 1)
          (foreach lg layerGroups
            (setq layCnt 0 layArea 0.0)
            (foreach r (cdr lg)
              (setq layCnt (+ layCnt (nth 5 r)))
              (setq layArea (+ layArea (cl-round2 (nth 6 r)))))
            (setq total-cnt (+ total-cnt layCnt))
            (setq total-area (+ total-area (cl-round2 layArea)))
            (setq numStr (strcat (itoa layerIdx) "." (itoa layCnt)))
            (if (> (strlen numStr) maxNumLen)
              (setq maxNumLen (strlen numStr)))
            (setq layerIdx (1+ layerIdx)))
          (setq col0Width (max 15.0 (* (+ maxNumLen 1) 3.5)))
          (setq units (cl-build-block-units data *TU-IDEAL-ROWS*))
          (setq total-chunks (length units))
          (setq chunk-idx 0 itemNum 0 nCols 6)
          (setq lastLayerIdx -1 rowInLayer 0)
          (foreach chunk units
            (setq is-last (tu-is-last-chunk chunk-idx total-chunks))
            (setq nRows (+ 2 (car chunk)))
            (if is-last (setq nRows (1+ nRows)))
            (setq items (cdr chunk))
            (setq tbl (vl-catch-all-apply 'vla-addtable
              (list space (vlax-3d-point pt_wcs) nRows nCols 10.0 50.0)))
            (if (vl-catch-all-error-p tbl)
              (princ (strcat "\nОшибка создания таблицы: "
                             (vl-catch-all-error-message tbl)))
              (progn
                (vla-SetColumnWidth tbl 0 col0Width)
                (vla-SetColumnWidth tbl 1 (* maxLayerLen 3.5))
                (vla-SetColumnWidth tbl 2 30.0)
                (vla-SetColumnWidth tbl 3 30.0)
                (vla-SetColumnWidth tbl 4 25.0)
                (vla-SetColumnWidth tbl 5 30.0)
                (ts-ac-title tbl 0 "Облицовка (полилинии)" 6)
                (ts-ac-header tbl 1
                  '("№" "Слой" "Высота, мм" "Ширина, мм" "Кол-во, шт." "Площадь, м2"))
                (setq row 2)
                (foreach item items
                  (if (eq (car item) 'data)
                    (progn
                      (setq layerIdx (cadr item))
                      (setq grp (cddr item))
                      (if (/= layerIdx lastLayerIdx)
                        (progn
                          (setq lastLayerIdx layerIdx)
                          (setq rowInLayer 0)))
                      (setq rowInLayer (1+ rowInLayer))
                      (vla-SetText tbl row 0
                        (strcat (itoa (1+ layerIdx)) "." (itoa rowInLayer)))
                      (vla-SetText tbl row 1 (nth 1 grp))
                      (vla-SetText tbl row 2 (itoa (nth 3 grp)))
                      (vla-SetText tbl row 3 (itoa (nth 4 grp)))
                      (vla-SetText tbl row 4 (itoa (nth 5 grp)))
                      (vla-SetText tbl row 5
                        (cl-format-area (cl-round2 (nth 6 grp))))
                      (vla-SetCellAlignment tbl row 0 5)
                      (vla-SetCellAlignment tbl row 1 4)
                      (vla-SetCellAlignment tbl row 2 5)
                      (vla-SetCellAlignment tbl row 3 5)
                      (vla-SetCellAlignment tbl row 4 5)
                      (vla-SetCellAlignment tbl row 5 5)
                      (setq row (1+ row)))
                    (progn
                      (setq layerIdx (nth 1 item))
                      (setq layer (nth 2 item))
                      (setq subCnt (nth 3 item))
                      (setq subArea (nth 4 item))
                      (ts-ac-subtotal tbl row (1+ layerIdx)
                        (strcat "   Итого: " layer) 1 3)
                      (vla-SetText tbl row 4 (itoa subCnt))
                      (vla-SetCellAlignment tbl row 4 5)
                      (vla-SetText tbl row 5
                        (cl-format-area (cl-round2 subArea)))
                      (vla-SetCellAlignment tbl row 5 5)
                      (setq row (1+ row)))))
                (if is-last
                  (progn
                    (ts-ac-total tbl row "           {\\LИтого по всем позициям:}" 0 3 4)
                    (vla-SetText tbl row 4 (itoa total-cnt))
                    (vla-SetCellAlignment tbl row 4 5)
                    (vla-SetText tbl row 5
                      (cl-format-area (cl-round2 total-area)))
                    (vla-SetCellAlignment tbl row 5 5)))
                (vla-update tbl)
                (princ (strcat "\nТаблица " (itoa (1+ chunk-idx)) " создана."))
                (setq pt_wcs (tu-next-table-point pt_wcs nRows 10.0 20.0))))
            (setq chunk-idx (1+ chunk-idx)))
          (tu-undo-end doc)
          (vl-catch-all-apply 'setvar (list "CMDECHO" oldEcho))
          (princ (strcat "\nВсего создано таблиц: " (itoa total-chunks)))
          T
        )
      )
    )
  )
)

;; ============================================================
;; К3: ЭКСПОРТ ПОЛИЛИНИЙ В XLS/CSV
;; ============================================================
(defun cl-write-xls (data report-mode fname / f brd rec total-cnt total-area
                          groups grp grpName grpRows grpCnt grpArea detail itemNum
                          row-num layer-start-row layer-end-row subtotal-row
                          subtotal-rows qsum asum r)
  (setq f (open fname "w"))
  (if (null f)
    nil
    (progn
      (setq detail (= (strcase report-mode) "DETAIL"))
      (setq brd (strcat "<Borders>"
        "<Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>"
        "<Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>"
        "<Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>"
        "<Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>"
        "</Borders>"))
      (eu-doc-begin f "BASIC")
(write-line " <Styles>" f)
      (write-line "  <Style ss:ID=\"Default\"><Alignment ss:Vertical=\"Center\"/></Style>" f)
      (write-line (strcat "  <Style ss:ID=\"Title\"><Font ss:Bold=\"1\" ss:Size=\"12\" ss:Underline=\"Single\"/>"
                "<Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>"
                "<Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" brd "</Style>") f)
      (write-line (strcat "  <Style ss:ID=\"Header\"><Font ss:Bold=\"1\"/>"
                "<Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>"
                "<Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" brd "</Style>") f)
      (write-line (strcat "  <Style ss:ID=\"Data\"><Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" brd "</Style>") f)
      (write-line (strcat "  <Style ss:ID=\"DataLeft\"><Alignment ss:Horizontal=\"Left\" ss:Vertical=\"Center\"/>" brd "</Style>") f)
      (write-line (strcat "  <Style ss:ID=\"Cut\"><Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>"
                "<Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" brd "</Style>") f)
      (write-line (strcat "  <Style ss:ID=\"Total\"><Font ss:Bold=\"1\"/>"
                "<Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>"
                "<Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" brd "</Style>") f)
      (write-line (strcat "  <Style ss:ID=\"Num\"><NumberFormat ss:Format=\"0.00\"/>"
                "<Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" brd "</Style>") f)
      (write-line (strcat "  <Style ss:ID=\"TotalNum\"><Font ss:Bold=\"1\"/>"
                "<Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>"
                "<NumberFormat ss:Format=\"0.00\"/>"
                "<Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" brd "</Style>") f)
      (write-line " </Styles>" f)

      (eu-worksheet f "Облицовка")

      (if detail
        ;; ========== ПОДРОБНЫЙ ==========
        (progn
          (eu-column f "30" "0")
          (eu-column f "120" "0")
          (eu-column f "40" "0")
          (eu-column f "40" "0")
          (eu-column f "40" "0")
          (eu-column f "40" "0")
          (eu-row-begin f "ss:Height=\"20\"")
          (eu-cell f "Title" "String" "Облицовка (полилинии)" "ss:MergeAcross=\"5\"")
          (eu-row-end f)
          (eu-row-begin f "")
          (eu-cell f "Header" "String" "№" "")
          (eu-cell f "Header" "String" "Слой" "")
          (eu-cell f "Header" "String" "Высота, мм" "")
          (eu-cell f "Header" "String" "Ширина, мм" "")
          (eu-cell f "Header" "String" "Кол-во, шт" "")
          (eu-cell f "Header" "String" "Площадь, м2" "")
          (eu-row-end f)
          (setq total-cnt 0 total-area 0.0 itemNum 0 row-num 3 subtotal-rows '())
          (setq groups (cl-group-blocks-by-layer data))
          (foreach grp groups
            (setq grpName (car grp))
            (setq grpRows (cdr grp))
            (setq layer-start-row row-num)
            (setq grpCnt 0 grpArea 0.0)
            (foreach rec grpRows
              (setq grpCnt (+ grpCnt (nth 5 rec)))
              (setq grpArea (+ grpArea (cl-round2 (nth 6 rec))))
              (setq total-cnt (+ total-cnt (nth 5 rec)))
              (setq total-area (+ total-area (cl-round2 (nth 6 rec))))
              (setq itemNum (1+ itemNum))
              (eu-row-begin f "")
              (eu-cell f "Data" "Number" (itoa itemNum) "")
              (eu-cell f "DataLeft" "String" (nth 1 rec) "")
              ;; Высота и ширина: для _НЕПРЯМОУГ_ со звездочкой и серый
              (if (= (caddr rec) "_НЕПРЯМОУГ_")
                (progn
                  (eu-cell f "Cut" "String" (strcat (itoa (nth 3 rec)) "*") "")
                  (eu-cell f "Cut" "String" (strcat (itoa (nth 4 rec)) "*") ""))
                (progn
                  (eu-cell f "Data" "Number" (itoa (nth 3 rec)) "")
                  (eu-cell f "Data" "Number" (itoa (nth 4 rec)) "")))
              (eu-cell f "Data" "Number" (itoa (nth 5 rec)) "")
              ;; Площадь: для _НЕПРЯМОУГ_ без формулы серый, для обычных формула
              (if (= (caddr rec) "_НЕПРЯМОУГ_")
                (eu-cell f "Num" "Number" (rtos (cl-round2 (nth 6 rec)) 2 2) "")
                (eu-cell f "Num" "Number" (rtos (cl-round2 (nth 6 rec)) 2 2)
                         (strcat "ss:Formula=\"=ROUND(R" (itoa row-num) "C3*R"
                                 (itoa row-num) "C4/1000000*R" (itoa row-num) "C5,2)\"")))
              (eu-row-end f)
              (setq row-num (1+ row-num)))
            (setq layer-end-row (1- row-num))
            (setq subtotal-row row-num)
            (setq subtotal-rows (append subtotal-rows (list subtotal-row)))
            (eu-row-begin f "")
            (eu-cell f "Total" "String" (strcat "  " grpName) "ss:MergeAcross=\"3\"")
            (eu-cell f "Total" "Number" (itoa grpCnt)
                     (strcat "ss:Formula=\"=SUM(R" (itoa layer-start-row) "C5:R"
                             (itoa layer-end-row) "C5)\""))
            (eu-cell f "TotalNum" "Number" (rtos (cl-round2 grpArea) 2 2)
                     (strcat "ss:Formula=\"=ROUND(SUM(R" (itoa layer-start-row) "C6:R"
                             (itoa layer-end-row) "C6),2)\""))
            (eu-row-end f)
            (setq row-num (1+ row-num)))
          (setq qsum "=SUM(")
          (setq asum "=ROUND(SUM(")
          (foreach r subtotal-rows
            (setq qsum (strcat qsum "R" (itoa r) "C5,"))
            (setq asum (strcat asum "R" (itoa r) "C6,")))
          (setq qsum (strcat (substr qsum 1 (1- (strlen qsum))) ")"))
          (setq asum (strcat (substr asum 1 (1- (strlen asum))) "),2)"))
          (eu-row-begin f "")
          (eu-cell f "Total" "String" "Итого по всем позициям:" "ss:MergeAcross=\"3\"")
          (eu-cell f "Total" "Number" (itoa total-cnt) (strcat "ss:Formula=\"" qsum "\""))
          (eu-cell f "TotalNum" "Number" (rtos (cl-round2 total-area) 2 2) (strcat "ss:Formula=\"" asum "\""))
          (eu-row-end f))
        ;; ========== КРАТКИЙ ==========
        (progn
          (eu-column f "30" "0")
          (eu-column f "140" "0")
          (eu-column f "40" "0")
          (eu-column f "40" "0")
          (eu-row-begin f "ss:Height=\"20\"")
          (eu-cell f "Title" "String" "Облицовка (полилинии)" "ss:MergeAcross=\"3\"")
          (eu-row-end f)
          (eu-row-begin f "")
          (eu-cell f "Header" "String" "№" "")
          (eu-cell f "Header" "String" "Слой" "")
          (eu-cell f "Header" "String" "Кол-во, шт" "")
          (eu-cell f "Header" "String" "Площадь, м2" "")
          (eu-row-end f)
          (setq total-cnt 0 total-area 0.0 itemNum 0)
          (foreach rec data
            (setq total-cnt (+ total-cnt (nth 5 rec)))
            (setq total-area (+ total-area (cl-round2 (nth 6 rec))))
            (setq itemNum (1+ itemNum))
            (eu-row-begin f "")
            (eu-cell f "Data" "Number" (itoa itemNum) "")
            (eu-cell f "DataLeft" "String" (cadr rec) "")
            (eu-cell f "Data" "Number" (itoa (nth 5 rec)) "")
            (eu-cell f "Data" "Number" (rtos (cl-round2 (nth 6 rec)) 2 2) "")
            (eu-row-end f))
          (eu-row-begin f "")
          (eu-cell f "Total" "String" "Итого по всем позициям:" "ss:MergeAcross=\"1\"")
          (eu-cell f "Total" "Number" (itoa total-cnt) "")
          (eu-cell f "Total" "Number" (rtos (cl-round2 total-area) 2 2) "")
          (eu-row-end f)))

      (eu-worksheet-end f)
      (eu-doc-end f)
      (close f)
      T
    )
  )
)

(defun cl-write-csv (data report-mode fname / f rec detail total-cnt total-area
                          groups grp grpName grpRows grpCnt grpArea)
  (setq f (open fname "w"))
  (if (null f)
    nil
    (progn
      (setq detail (= (strcase report-mode) "DETAIL"))
      (setq total-cnt 0 total-area 0.0)
      (if detail
        (write-line "Слой;Высота, мм;Ширина, мм;Кол-во, шт;Площадь, м2" f)
        (write-line "Слой;Кол-во, шт;Площадь, м2" f)
      )
      (if detail
        (progn
          (setq groups (cl-group-blocks-by-layer data))
          (foreach grp groups
            (setq grpName (car grp))
            (setq grpRows (cdr grp))
            (setq grpCnt 0 grpArea 0.0)
            (foreach rec grpRows
              (setq grpCnt (+ grpCnt (nth 5 rec)))
              (setq grpArea (+ grpArea (nth 6 rec)))
              (setq total-cnt (+ total-cnt (nth 5 rec)))
              (setq total-area (+ total-area (nth 6 rec)))
              (write-line (strcat (nth 1 rec) ";"
                                  (itoa (nth 3 rec)) ";"
                                  (itoa (nth 4 rec)) ";"
                                  (itoa (nth 5 rec)) ";"
                                  (cl-format-area (cl-round2 (nth 6 rec)))) f)
            )
            (write-line (strcat "Итого: " grpName ";;;"
                                (itoa grpCnt) ";"
                                (cl-format-area (cl-round2 grpArea))) f)
          )
        )
        (progn
          (foreach rec data
            (setq total-cnt (+ total-cnt (nth 5 rec)))
            (setq total-area (+ total-area (nth 6 rec)))
            (write-line (strcat (cadr rec) ";"
                                (itoa (nth 5 rec)) ";"
                                (cl-format-area (cl-round2 (nth 6 rec)))) f)
          )
        )
      )
      (if detail
        (write-line (strcat "Итого по всем позициям:;;;"
                            (itoa total-cnt) ";"
                            (cl-format-area (cl-round2 total-area))) f)
        (write-line (strcat "Итого;" (itoa total-cnt) ";"
                            (cl-format-area (cl-round2 total-area))) f)
      )
      (close f)
      T
    )
  )
)

(princ "\nCLADDING.LSP загружен (К1-К3, Б1-Б4, ред. 30: U2-примитивы). Команды: CLADDING / ОБЛИЦОВКА, CLBLOCKS")
(princ)