;;; ============================================================
;;; CUTSHEET.LSP — модуль двумерного раскроя листа
;;; AutoExtraction / AutoCAD 2016+
;;;
;;; РЕДАКЦИЯ 4 (исправления):
;;;   1. Площадь в агрегации — фактическая из записи
;;;   2. Убрано дублирование в диалоге
;;;   3. Вынесена константа *CUTSHEET-MIN-TEXT-AREA*
;;;   4. Уникальные имена блоков
;;;   5. Относительные координаты в summary
;;;   6. Расширены стили XLS
;;;   7. Явная проверка размеров
;;;   8. Динамическая ширина таблицы
;;;   9. Заливка SOLID вместо HATCH (совместимость с cutline)
;;;  10. Таблица: количество деталей напротив типов
;;;  11. Компактная таблица, шкала по низу листа
;;;  12. МаксRects с контактным скорингом
;;; ============================================================

(vl-load-com)

;; ================= ПАРАМЕТРЫ =================
(setq *CUTSHEET-MIN-SIZE*        50.0)
(setq *CUTSHEET-MAX-SIZE*     500000.0)
(setq *CUTSHEET-DEFAULT-WIDTH* 3000.0)
(setq *CUTSHEET-DEFAULT-HEIGHT* 1500.0)
(setq *CUTSHEET-DEFAULT-KERF*   3.0)
(setq *CUTSHEET-DEFAULT-ROTATE* T)

;; Графика
(setq *CUTSHEET-PART-TRANSPARENCY* 0.70)
(setq *CUTSHEET-WASTE-TRANSPARENCY* 0.88)
(setq *CUTSHEET-PALETTE* '(1 2 3 4 5 6 30 210 140 90 40 120))
(setq *CUTSHEET-TITLE-H* 100.0)
(setq *CUTSHEET-TEXT-H*  60.0)
(setq *CUTSHEET-SHEET-GAP* 190.0)   ; видимый вертикальный зазор = GAP+HEADER(260): 450 (требование пользователя)
(setq *CUTSHEET-SHEET-HEADER* 260.0)
(setq *CUTSHEET-GRID-COLS* 3)
(setq *CUTSHEET-SUMMARY-W* 2000.0)
(setq *CUTSHEET-SUMMARY-GAP* 500.0)
(setq *CUTSHEET-OUTLINE-COLOR* 7)
(setq *CUTSHEET-TITLE-COLOR* 5)
(setq *CUTSHEET-HEADER-COLOR* 3)
(setq *CUTSHEET-VALUE-COLOR* 7)
(setq *CUTSHEET-KPD-COLOR* 1)
(setq *CUTSHEET-WASTE-COLOR* 8)
(setq *CUTSHEET-PART-TEXT-COLOR* 2)
(setq *CUTSHEET-TEXT-COLOR* 7)
(setq *CUTSHEET-FRAME-LAYER* "Невидимые")
(setq *CUTSHEET-FRAME-PAD-LEFT* 200.0)
(setq *CUTSHEET-FRAME-PAD-RIGHT* 200.0)
(setq *CUTSHEET-FRAME-PAD-TOP* 250.0)
(setq *CUTSHEET-FRAME-PAD-BOTTOM* 200.0)
(setq *CUTSHEET-MIN-TEXT-AREA* 50000.0)

;; Состояние диалога
(if (not (boundp '*cs-tmp-choice*))    (setq *cs-tmp-choice* 'ALL))
(if (not (boundp '*cs-tmp-sheet-w*))   (setq *cs-tmp-sheet-w* *CUTSHEET-DEFAULT-WIDTH*))
(if (not (boundp '*cs-tmp-sheet-h*))   (setq *cs-tmp-sheet-h* *CUTSHEET-DEFAULT-HEIGHT*))
(if (not (boundp '*cs-tmp-kerf*))      (setq *cs-tmp-kerf* *CUTSHEET-DEFAULT-KERF*))
(if (not (boundp '*cs-tmp-rotate*))    (setq *cs-tmp-rotate* *CUTSHEET-DEFAULT-ROTATE*))
(if (not (boundp '*cs-tmp-xls*))       (setq *cs-tmp-xls* T))
(if (not (boundp '*cs-tmp-acad*))      (setq *cs-tmp-acad* T))
(if (not (boundp '*cs-tmp-dyn-type*))  (setq *cs-tmp-dyn-type* ""))
(if (not (boundp '*cs-dyn-types*))     (setq *cs-dyn-types* '()))
(if (not (boundp '*CUTSHEET-LAST-WIDTH*))  (setq *CUTSHEET-LAST-WIDTH* *CUTSHEET-DEFAULT-WIDTH*))
(if (not (boundp '*CUTSHEET-LAST-HEIGHT*)) (setq *CUTSHEET-LAST-HEIGHT* *CUTSHEET-DEFAULT-HEIGHT*))
(if (not (boundp '*CUTSHEET-LAST-KERF*))   (setq *CUTSHEET-LAST-KERF* *CUTSHEET-DEFAULT-KERF*))
(if (not (boundp '*CUTSHEET-LAST-ROTATE*)) (setq *CUTSHEET-LAST-ROTATE* *CUTSHEET-DEFAULT-ROTATE*))
(if (not (boundp '*CUTSHEET-LAST-XLS*))    (setq *CUTSHEET-LAST-XLS* T))
(if (not (boundp '*CUTSHEET-LAST-ACAD*))   (setq *CUTSHEET-LAST-ACAD* T))

;; ================= СТРОКИ / ЧИСЛА =================
(defun cs-split-string (str delim / pos out item)
  (setq out '())
  (while (setq pos (vl-string-search delim str))
    (setq item (vl-string-trim " \t\r\n" (substr str 1 pos)))
    (if (> (strlen item) 0) (setq out (cons item out)))
    (setq str (substr str (+ pos 2))))
  (setq item (vl-string-trim " \t\r\n" str))
  (if (> (strlen item) 0) (setq out (cons item out)))
  (reverse out))

(defun cs-format-num (x digits)
  (vl-string-translate "." "," (rtos x 2 digits)))

(defun cs-xls-num (x digits)
  (rtos x 2 digits))

(defun cs-itoa-safe (x)
  (itoa (fix (+ x 0.5))))

;; ================= ДИНАМИЧЕСКИЕ БЛОКИ =================
(defun cs-value-to-number (value / x s)
  (cond
    ((numberp value) (float value))
    ((= (type value) 'VARIANT)
     (setq x (vl-catch-all-apply 'vlax-variant-value (list value)))
     (cond
       ((vl-catch-all-error-p x) nil)
       ((numberp x) (float x))
       ((= (type x) 'STR) (cs-value-to-number x))
       (T nil)))
    ((= (type value) 'STR)
     (setq s (vl-string-trim " \t\r\n" value))
     (if (> (strlen s) 0)
       (progn
         (setq s (vl-string-translate "," "." s))
         (while (vl-string-search " " s)
           (setq s (vl-string-subst "" " " s)))
         (setq x (vl-catch-all-apply 'atof (list s)))
         (if (vl-catch-all-error-p x) nil x))
       nil))
    (T nil)))

(defun cs-block-all-props (obj / dyn prop pname pval out)
  (setq out '())
  (setq dyn (vl-catch-all-apply 'vlax-invoke (list obj 'GetDynamicBlockProperties)))
  (if (not (vl-catch-all-error-p dyn))
    (foreach prop dyn
      (setq pname (vl-catch-all-apply 'vla-get-PropertyName (list prop)))
      (if (and (not (vl-catch-all-error-p pname)) (= (type pname) 'STR))
        (progn
          (setq pname (vl-string-trim " \t\r\n" pname))
          (setq pval (vl-catch-all-apply 'vla-get-Value (list prop)))
          (if (not (vl-catch-all-error-p pval))
            (setq out (cons (cons pname pval) out)))))))
  (reverse out))

(defun cs-prop-value (props wanted / p)
  (setq p nil)
  (foreach x props
    (if (and (null p) (= (strcase (car x)) (strcase wanted)))
      (setq p (cdr x))))
  (if p (cs-value-to-number p) nil))

(defun cs-get-effective-name-safe (obj / r)
  (setq r (vl-catch-all-apply 'vla-get-EffectiveName (list obj)))
  (if (vl-catch-all-error-p r)
    (progn
      (setq r (vl-catch-all-apply 'vla-get-Name (list obj)))
      (if (vl-catch-all-error-p r) nil r))
    r))

(defun cs-get-visibility-safe (obj / dyn prop pname val result)
  (setq dyn (vl-catch-all-apply 'vlax-invoke (list obj 'GetDynamicBlockProperties)))
  (if (vl-catch-all-error-p dyn)
    nil
    (progn
      (setq result nil)
      (foreach prop dyn
        (if (null result)
          (progn
            (setq pname (vl-catch-all-apply 'vla-get-PropertyName (list prop)))
            (if (and (not (vl-catch-all-error-p pname))
                     (= (type pname) 'STR)
                     (or (vl-string-search "VISIBILITY" (strcase pname))
                         (vl-string-search "ВИДИМОСТЬ" (strcase pname))))
              (progn
                (setq val (vl-catch-all-apply 'vla-get-Value (list prop)))
                (if (not (vl-catch-all-error-p val))
                  (progn
                    (if (= (type val) 'VARIANT) (setq val (vlax-variant-value val)))
                    (if val (setq result (vl-princ-to-string val))))))))))
      result)))

(defun cs-get-dyn-type-name (ent / obj vis name)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
  (if (vl-catch-all-error-p obj)
    "Без имени"
    (progn
      (setq name (cs-get-effective-name-safe obj))
      (if (and name (= (type name) 'STR) (> (strlen (vl-string-trim " \t\r\n" name)) 0))
        (vl-string-trim " \t\r\n" name)
        (progn
          (setq vis (cs-get-visibility-safe obj))
          (if (and vis (> (strlen (vl-string-trim " \t\r\n" vis)) 0))
            (vl-string-trim " \t\r\n" vis)
            "Без имени"))))))

(defun cs-get-bbox (ent / obj mn mx r p1 p2)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
  (if (vl-catch-all-error-p obj)
    nil
    (progn
      (setq r (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'mn 'mx)))
      (if (vl-catch-all-error-p r)
        nil
        (progn
          (setq p1 (vlax-safearray->list mn) p2 (vlax-safearray->list mx))
          (list p1 p2))))))

(defun cs-bbox-w-h (ent / bb p1 p2)
  (setq bb (cs-get-bbox ent))
  (if bb
    (progn
      (setq p1 (car bb) p2 (cadr bb))
      (list (abs (- (car p2) (car p1))) (abs (- (cadr p2) (cadr p1)))))
    nil))

;; ================= ПОЛИЛИНИИ =================
(defun cs-poly-closed-p (ent / f)
  (setq f (cdr (assoc 70 (entget ent))))
  (if f (= 1 (logand 1 f)) nil))

(defun cs-poly-has-arcs (ent / found)
  (setq found nil)
  (foreach g (entget ent)
    (if (and (= (car g) 42) (> (abs (cdr g)) 1e-8)) (setq found T)))
  found)

(defun cs-poly-area (ent / obj a)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
  (setq a (if (vl-catch-all-error-p obj) nil (vl-catch-all-apply 'vla-get-Area (list obj))))
  (if (and a (not (vl-catch-all-error-p a)) (numberp a) (> a 0.0))
    (/ a 1000000.0)
    0.0))

;; ================= ИСТОЧНИК ОБЪЕКТОВ =================
(defun cs-layer-ok-p (layer layers)
  (if (or (null layers) (= (length layers) 0)) T (su-layer-match-any layer layers)))

(defun cs-build-filter-ss (layers / ss out i ent typ lay)
  (setq ss nil)
  (setq ss (su-take-preselection))
  (if (null ss)
    (setq ss (ssget "_I" '((0 . "LWPOLYLINE,INSERT")))))
  (if (null ss) (setq ss (ssget "_X" '((0 . "LWPOLYLINE,INSERT")))))
  (setq out (ssadd) i 0)
  (if ss
    (repeat (sslength ss)
      (setq ent (ssname ss i)
            typ (cdr (assoc 0 (entget ent)))
            lay (cdr (assoc 8 (entget ent))))
      (if (and (or (= typ "LWPOLYLINE") (= typ "INSERT")) (cs-layer-ok-p lay layers) (not (su-map-entity-p ent)))
        (ssadd ent out))
      (setq i (1+ i))))
  (if (> (sslength out) 0) out nil))

;; ================= ЗАПИСЬ ЧАСТЕЙ =================
(defun cs-poly-record (ent id / ed layer bb wh area arc type nominal)
  (setq ed (entget ent) layer (cdr (assoc 8 ed)) bb (cs-get-bbox ent))
  (if (and (cs-poly-closed-p ent) bb)
    (progn
      (setq wh (cs-bbox-w-h ent) area (cs-poly-area ent) arc (cs-poly-has-arcs ent))
      (if (and wh (> (car wh) 0.0) (> (cadr wh) 0.0) (> area 0.0))
        (progn
          (setq type (if arc "Полилиния (дуги)" "Полилиния"))
          (setq nominal (strcat (cs-itoa-safe (car wh)) "x" (cs-itoa-safe (cadr wh))))
          (list id "POLY" layer type (car wh) (cadr wh) area nominal T ent))
        nil))
    nil))

(defun cs-block-record (ent id / obj ed layer props typName wh w h pW pH area nominal source)
  (setq ed (entget ent) layer (cdr (assoc 8 ed))
        obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
  (if (vl-catch-all-error-p obj)
    nil
    (progn
      (setq props (cs-block-all-props obj) typName (cs-get-dyn-type-name ent))
      (setq pW (cs-prop-value props "Ширина") pH (cs-prop-value props "Высота"))
      (if (not (numberp pW)) (setq pW nil))
      (if (not (numberp pH)) (setq pH nil))
      (if (and pW pH (> pW 0.0) (> pH 0.0))
        (setq w pW h pH source "Свойства")
        (progn
          (setq wh (cs-bbox-w-h ent))
          (if (and wh (numberp (car wh)) (numberp (cadr wh)) (> (car wh) 0.0) (> (cadr wh) 0.0))
            (setq w (car wh) h (cadr wh) source "BoundingBox")
            (setq w nil h nil source nil))))
      (if (and w h (numberp w) (numberp h) (> w 0.0) (> h 0.0))
        (progn
          (setq area (/ (* w h) 1000000.0))
          (setq nominal (strcat (cs-itoa-safe w) "x" (cs-itoa-safe h)))
          (list id "DYN" layer typName w h area nominal source ent))
        nil))))

(defun cs-collect-records (ss choice dynType / i ent typ rec out id)
  (setq out '() i 0 id 0)
  (if ss
    (repeat (sslength ss)
      (setq ent (ssname ss i) typ (cdr (assoc 0 (entget ent))) rec nil)
      (cond
        ((and (= typ "LWPOLYLINE") (or (eq choice 'ALL) (eq choice 'POLY)))
         (setq id (1+ id)) (setq rec (cs-poly-record ent id)))
        ((and (= typ "INSERT") (or (eq choice 'ALL) (eq choice 'DYN)))
         (cond
           ((eq choice 'ALL) (setq id (1+ id)) (setq rec (cs-block-record ent id)))
           ((or (null dynType) (= dynType "") (= dynType "Все типы блоков"))
            (setq id (1+ id)) (setq rec (cs-block-record ent id)))
           ((= (cs-get-dyn-type-name ent) dynType)
            (setq id (1+ id)) (setq rec (cs-block-record ent id))))))
      (if rec (setq out (cons rec out)))
      (setq i (1+ i))))
  (reverse out))

(defun cs-collect-dyn-types (ss / i ent typ nm out)
  (setq i 0 out '())
  (if ss
    (repeat (sslength ss)
      (setq ent (ssname ss i) typ (cdr (assoc 0 (entget ent))))
      (if (= typ "INSERT")
        (progn
          (setq nm (cs-get-dyn-type-name ent))
          (if (and nm (not (member nm out))) (setq out (cons nm out)))))
      (setq i (1+ i))))
  (vl-sort out '(lambda (a b) (< (strcase a) (strcase b)))))

(defun cs-count-type (ss kind / i ent typ obj cnt wh)
  (setq i 0 cnt 0)
  (if ss
    (repeat (sslength ss)
      (setq ent (ssname ss i) typ (cdr (assoc 0 (entget ent))))
      (cond
        ((and (eq kind 'POLY) (= typ "LWPOLYLINE"))
         (if (cs-poly-closed-p ent) (setq cnt (1+ cnt))))
        ((and (eq kind 'DYN) (= typ "INSERT"))
         (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
         (if (not (vl-catch-all-error-p obj))
           (progn
             (setq wh (cs-bbox-w-h ent))
             (if (and wh (> (car wh) 0.0) (> (cadr wh) 0.0)) (setq cnt (1+ cnt))))))
        ((eq kind 'ALL)
         (if (or (= typ "LWPOLYLINE") (= typ "INSERT")) (setq cnt (1+ cnt)))))
      (setq i (1+ i))))
  cnt)

;; ================= DCL =================
(defun cs-safe-set-tile (key val) (vl-catch-all-apply 'set_tile (list key val)))
(defun cs-safe-mode-tile (key mode) (vl-catch-all-apply 'mode_tile (list key mode)))

;; Этап 2 (V7): помещается ли деталь на лист хотя бы в одной из
;; допустимых ориентаций (рез учитывает солвер; здесь - геометрия).
(defun cs-part-fits-sheet-p (w h sheetW sheetH rotateFlag)
  (or (and (<= w sheetW) (<= h sheetH))
      (and rotateFlag (<= h sheetW) (<= w sheetH))))

(defun cs-cut-sizes-valid-p (w h kerf)
  (and (numberp w) (numberp h) (> w *CUTSHEET-MIN-SIZE*) (> h *CUTSHEET-MIN-SIZE*)
       (< w *CUTSHEET-MAX-SIZE*) (< h *CUTSHEET-MAX-SIZE*)
       (numberp kerf) (>= kerf 0.0) (< kerf (min w h))))

;; ============================================================
;; Ручное управление радиокнопками (аналог n1-select-radio из cutline.lsp)
;; ============================================================
(defun cs-select-radio (selected / keys k)
  (setq keys '("rb_all" "rb_poly" "rb_dyn"))
  (foreach k keys
    (cs-safe-set-tile k (if (= k selected) "1" "0"))
  )
  (cond
    ((= selected "rb_all")  (setq *cs-tmp-choice* 'ALL)  (cs-safe-mode-tile "popup_dyn_type" 1))
    ((= selected "rb_poly") (setq *cs-tmp-choice* 'POLY) (cs-safe-mode-tile "popup_dyn_type" 1))
    ((= selected "rb_dyn")  (setq *cs-tmp-choice* 'DYN)  (cs-safe-mode-tile "popup_dyn_type" 0))
  )
)

(defun cs-dialog (polyCnt dynCnt dynTypes ss defaultW defaultH defaultKerf defaultRotate defaultXls defaultAcad / dcl-file dcl-id result)
  (setq dcl-file (findfile "cutsheet_filter.dcl"))
  (if (null dcl-file)
    (progn (princ "\n[CUTSHEET] Не найден cutsheet_filter.dcl.") nil)
    (progn
      (setq dcl-id (load_dialog dcl-file))
      (if (< dcl-id 0)
        (progn (princ "\n[CUTSHEET] Ошибка load_dialog.") nil)
        (progn
          (setq *cs-tmp-choice* 'ALL
                *cs-tmp-sheet-w* defaultW
                *cs-tmp-sheet-h* defaultH
                *cs-tmp-kerf* defaultKerf
                *cs-tmp-rotate* defaultRotate
                *cs-tmp-xls* defaultXls
                *cs-tmp-acad* defaultAcad
                *cs-tmp-dyn-type* "")
          (if (not (new_dialog "cutsheet_filter_dialog" dcl-id))
            (progn (vl-catch-all-apply 'unload_dialog (list dcl-id)) nil)
            (progn
              (cs-safe-set-tile "txt_poly_count" (strcat (itoa polyCnt) " шт."))
              (cs-safe-set-tile "txt_dyn_count" (strcat (itoa dynCnt) " шт."))
              (cs-safe-set-tile "txt_all_count" (strcat (itoa (+ polyCnt dynCnt)) " шт."))
              (setq *cs-dyn-types* dynTypes)
              (start_list "popup_dyn_type")
              (add_list "Все типы блоков")
              (foreach x dynTypes (add_list x))
              (end_list)
              (set_tile "popup_dyn_type" "0")
              (cs-safe-mode-tile "popup_dyn_type" 1)
              (if (<= polyCnt 0) (cs-safe-mode-tile "rb_poly" 1))
              (if (<= dynCnt 0) (cs-safe-mode-tile "rb_dyn" 1))
              (if (<= (+ polyCnt dynCnt) 0) (cs-safe-mode-tile "rb_all" 1))
              (cs-safe-set-tile "rb_all" "1")
              (cs-safe-set-tile "rb_poly" "0")
              (cs-safe-set-tile "rb_dyn" "0")
              (cs-safe-set-tile "edt_sheet_w" (rtos defaultW 2 0))
              (cs-safe-set-tile "edt_sheet_h" (rtos defaultH 2 0))
              (cs-safe-set-tile "edt_kerf" (rtos defaultKerf 2 2))
              (cs-safe-set-tile "chk_rotate" (if defaultRotate "1" "0"))
              (cs-safe-set-tile "chk_xls" (if defaultXls "1" "0"))
              (cs-safe-set-tile "chk_acad" (if defaultAcad "1" "0"))
              (action_tile "rb_all"  "(cs-select-radio \"rb_all\")")
              (action_tile "rb_poly" "(cs-select-radio \"rb_poly\")")
              (action_tile "rb_dyn"  "(cs-select-radio \"rb_dyn\")")
              (action_tile "popup_dyn_type"
                "(if (= (atoi $value) 0) (setq *cs-tmp-dyn-type* \"\") (setq *cs-tmp-dyn-type* (nth (1- (atoi $value)) *cs-dyn-types*))) (setq *cs-tmp-choice* 'DYN) (mode_tile \"popup_dyn_type\" 0)")
              (action_tile "edt_sheet_w" "(setq *cs-tmp-sheet-w* (tu-parse-number $value))")
              (action_tile "edt_sheet_h" "(setq *cs-tmp-sheet-h* (tu-parse-number $value))")
              (action_tile "edt_kerf"    "(setq *cs-tmp-kerf* (tu-parse-number $value))")
              (action_tile "chk_rotate" "(setq *cs-tmp-rotate* (= $value \"1\"))")
              (action_tile "chk_xls"    "(setq *cs-tmp-xls* (= $value \"1\"))")
              (action_tile "chk_acad"   "(setq *cs-tmp-acad* (= $value \"1\"))")
              (action_tile "btn_ok"     "(if (cs-cut-sizes-valid-p *cs-tmp-sheet-w* *cs-tmp-sheet-h* *cs-tmp-kerf*) (done_dialog 1) (alert \"[AutoExtraction][CUTSHEET][VALIDATION] Проверьте размеры листа и ширину реза (рез неотрицательный и меньше сторон листа).\"))")
              (action_tile "btn_cancel" "(done_dialog 0)")
              (setq result (start_dialog))
              (vl-catch-all-apply 'unload_dialog (list dcl-id))
              (if (= result 1)
                (list *cs-tmp-choice* *cs-tmp-sheet-w* *cs-tmp-sheet-h* *cs-tmp-kerf*
                      *cs-tmp-rotate* *cs-tmp-xls* *cs-tmp-acad* *cs-tmp-dyn-type*)
                nil))))))))

;; ================= АГРЕГАЦИЯ =================
(defun cs-part-key (r)
  (strcat (nth 1 r) "|" (nth 3 r) "|" (cs-itoa-safe (nth 4 r)) "x" (cs-itoa-safe (nth 5 r))))

(defun cs-part-label (r)
  (strcat (cs-itoa-safe (nth 4 r)) "x" (cs-itoa-safe (nth 5 r))))

(defun cs-aggregate (records / acc r key f out)
  (setq acc '())
  (foreach r records
    (setq key (cs-part-key r) f (assoc key acc))
    (if f
      (setq acc (subst (list key (nth 1 r) (nth 2 r) (nth 3 r) (nth 4 r) (nth 5 r)
                             (1+ (nth 6 f)) (+ (nth 7 f) (* (nth 4 r) (nth 5 r) (/ 1.0 1000000.0))) (+ (nth 8 f) (nth 6 r))) f acc))
      (setq acc (cons (list key (nth 1 r) (nth 2 r) (nth 3 r) (nth 4 r) (nth 5 r) 1
                            (* (nth 4 r) (nth 5 r) (/ 1.0 1000000.0)) (nth 6 r)) acc))))
  (setq out (vl-sort acc '(lambda (a b)
    (cond
      ((> (* (nth 4 a) (nth 5 a)) (* (nth 4 b) (nth 5 b))) T)
      ((< (* (nth 4 a) (nth 5 a)) (* (nth 4 b) (nth 5 b))) nil)
      ((< (strcase (nth 3 a)) (strcase (nth 3 b))) T)
      ((> (strcase (nth 3 a)) (strcase (nth 3 b))) nil)
      (T nil)))))
  out)

;; ================= SORT / NESTING (MaxRects с контактным скорингом) =================
(defun cs-merge-eps () 0.001)

;; Скоринг размещения с контактами и прижимом к началу
(defun cs-score-rect (rw rh fx fy fw fh usedParts curSheet sheetW sheetH / areaFit shortFit longFit fragmentation contact alignBonus distPenalty score p px py pw ph overlap)
  (setq areaFit (- (* fw fh) (* rw rh)))
  (setq shortFit (if (< (- fw rw) (- fh rh)) (- fw rw) (- fh rh)))
  (setq longFit (if (> (- fw rw) (- fh rh)) (- fw rw) (- fh rh)))
  (setq fragmentation (abs (- (- fw rw) (- fh rh))))
  
  (setq contact 0.0)
  (if (<= fx (cs-merge-eps)) (setq contact (+ contact (* rh 15.0))))
  (if (<= fy (cs-merge-eps)) (setq contact (+ contact (* rw 15.0))))
  (if (>= (+ fx rw) (- sheetW (cs-merge-eps))) (setq contact (+ contact (* rh 15.0))))
  (if (>= (+ fy rh) (- sheetH (cs-merge-eps))) (setq contact (+ contact (* rw 15.0))))
  (if (< (- fw rw) (cs-merge-eps)) (setq contact (+ contact (* rh 5.0))))
  (if (< (- fh rh) (cs-merge-eps)) (setq contact (+ contact (* rw 5.0))))
  
  (setq alignBonus 0.0)
  (foreach p usedParts
    (if (= (car p) curSheet)
      (progn
        (setq px (nth 1 p) py (nth 2 p) pw (nth 3 p) ph (nth 4 p))
        (if (or (< (abs (- px (+ fx rw))) (cs-merge-eps))
                (< (abs (- (+ px pw) fx)) (cs-merge-eps)))
          (progn
            (setq overlap (- (min (+ py ph) (+ fy rh)) (max py fy)))
            (if (> overlap 0) (setq contact (+ contact (* overlap 20.0))))))
        (if (or (< (abs (- py (+ fy rh))) (cs-merge-eps))
                (< (abs (- (+ py ph) fy)) (cs-merge-eps)))
          (progn
            (setq overlap (- (min (+ px pw) (+ fx rw)) (max px fx)))
            (if (> overlap 0) (setq contact (+ contact (* overlap 20.0))))))
        (if (< (abs (- fx px)) (cs-merge-eps)) (setq alignBonus (+ alignBonus 80.0)))
        (if (< (abs (- fy py)) (cs-merge-eps)) (setq alignBonus (+ alignBonus 80.0)))
        (if (< (abs (- (+ fx rw) (+ px pw))) (cs-merge-eps)) (setq alignBonus (+ alignBonus 50.0)))
        (if (< (abs (- (+ fy rh) (+ py ph))) (cs-merge-eps)) (setq alignBonus (+ alignBonus 50.0))))))
  
  (setq distPenalty (+ (* fx 0.3) (* fy 0.3)))
  (setq score (+ (* areaFit 0.001) (* shortFit 5.0) (* longFit 1.0) (* fragmentation 0.5)
                 (- (* contact 12.0)) (- alignBonus) distPenalty))
  score)

(defun cs-subtract-rect (freeRects rx ry rw rh kerf / newFree fr fx fy fw fh)
  (setq newFree '())
  (foreach fr freeRects
    (setq fx (nth 0 fr) fy (nth 1 fr) fw (nth 2 fr) fh (nth 3 fr))
    (if (or (>= rx (+ fx fw)) (<= (+ rx rw) fx) (>= ry (+ fy fh)) (<= (+ ry rh) fy))
      (setq newFree (cons fr newFree))
      (progn
        (if (> (- ry fy) kerf) (setq newFree (cons (list fx fy fw (- ry fy kerf)) newFree)))
        (if (> (- (+ fy fh) (+ ry rh)) kerf) (setq newFree (cons (list fx (+ ry rh kerf) fw (- (+ fy fh) (+ ry rh) kerf)) newFree)))
        (if (> (- rx fx) kerf) (setq newFree (cons (list fx fy (- rx fx kerf) fh) newFree)))
        (if (> (- (+ fx fw) (+ rx rw)) kerf) (setq newFree (cons (list (+ rx rw kerf) fy (- (+ fx fw) (+ rx rw) kerf) fh) newFree))))))
  (cs-prune-free-small newFree))

(defun cs-prune-free-small (freeRects / out fr)
  (setq out '())
  (foreach fr freeRects
    (if (and (> (nth 2 fr) 1.0) (> (nth 3 fr) 1.0)) (setq out (cons fr out))))
  (reverse out))

(defun cs-get-used-parts (sheet / out p)
  (setq out '())
  (foreach p (cadr sheet)
    (setq out (cons (list 0 (nth 1 p) (nth 2 p) (nth 3 p) (nth 4 p)) out)))
  (reverse out))

(defun cs-pack-rects (parts sheetW sheetH kerf rotateFlag / sheets oversized freeRects part pw ph bestScore bestX bestY bestW bestH bestRot bestSheetIdx found fr fx fy fw fh score sheetIdx sheet)
  (setq sheets '() oversized '())
  (setq sheets (list (list (list (list 0.0 0.0 sheetW sheetH)) '() 0.0 0.0)))
  
  (foreach part parts
    (setq pw (nth 4 part) ph (nth 5 part))
    (setq bestScore 1e15 bestX nil bestY nil bestW nil bestH nil bestRot 0 bestSheetIdx nil found nil)
    (setq sheetIdx 0)
    
    (foreach sheet sheets
      (setq freeRects (car sheet))
      (foreach fr freeRects
        (setq fx (nth 0 fr) fy (nth 1 fr) fw (nth 2 fr) fh (nth 3 fr))
        (if (and (<= pw fw) (<= ph fh))
          (progn
            (setq score (cs-score-rect pw ph fx fy fw fh (cs-get-used-parts sheet) sheetIdx sheetW sheetH))
            (if (< score bestScore)
              (setq bestScore score bestX fx bestY fy bestW pw bestH ph bestRot 0 bestSheetIdx sheetIdx found T))))
        (if (and rotateFlag (<= ph fw) (<= pw fh))
          (progn
            (setq score (cs-score-rect ph pw fx fy fw fh (cs-get-used-parts sheet) sheetIdx sheetW sheetH))
            (if (< score bestScore)
              (setq bestScore score bestX fx bestY fy bestW ph bestH pw bestRot 1 bestSheetIdx sheetIdx found T)))))
      (setq sheetIdx (1+ sheetIdx)))
    
    (if (not found)
      (progn
        (setq sheets (append sheets (list (list (list (list 0.0 0.0 sheetW sheetH)) '() 0.0 0.0))))
        (setq sheetIdx (1- (length sheets)))
        (setq freeRects (list (list 0.0 0.0 sheetW sheetH)))
        (foreach fr freeRects
          (setq fx (nth 0 fr) fy (nth 1 fr) fw (nth 2 fr) fh (nth 3 fr))
          (if (and (<= pw fw) (<= ph fh))
            (progn
              (setq score (cs-score-rect pw ph fx fy fw fh '() sheetIdx sheetW sheetH))
              (if (< score bestScore)
                (setq bestScore score bestX fx bestY fy bestW pw bestH ph bestRot 0 bestSheetIdx sheetIdx found T))))
          (if (and rotateFlag (<= ph fw) (<= pw fh))
            (progn
              (setq score (cs-score-rect ph pw fx fy fw fh '() sheetIdx sheetW sheetH))
              (if (< score bestScore)
                (setq bestScore score bestX fx bestY fy bestW ph bestH pw bestRot 1 bestSheetIdx sheetIdx found T)))))))
    
    (if found
      (progn
        (setq sheet (nth bestSheetIdx sheets))
        (setq sheet (list
                      (cs-subtract-rect (car sheet) bestX bestY bestW bestH kerf)
                      (cons (list part bestX bestY bestW bestH bestRot) (cadr sheet))
                      (+ (nth 2 sheet) (* bestW bestH (/ 1.0 1000000.0)))
                      (+ (nth 3 sheet) (nth 6 part))))
        (setq sheets (subst sheet (nth bestSheetIdx sheets) sheets)))
      (setq oversized (cons part oversized))))
  
  (list sheets (reverse oversized)))

(defun cs-sort-by-area (records)
  (vl-sort records '(lambda (a b) (> (* (nth 4 a) (nth 5 a)) (* (nth 4 b) (nth 5 b))))))

(defun cs-sort-by-max-side (records)
  (vl-sort records '(lambda (a b) (> (max (nth 4 a) (nth 5 a)) (max (nth 4 b) (nth 5 b))))))

(defun cs-sort-by-min-side (records)
  (vl-sort records '(lambda (a b) (> (min (nth 4 a) (nth 5 a)) (min (nth 4 b) (nth 5 b))))))

(defun cs-calc-utilization (sheets sheetW sheetH / used)
  (setq used 0.0)
  (foreach sheet sheets (setq used (+ used (nth 2 sheet))))
  (if (> (length sheets) 0)
    (/ (* used 100.0) (* (length sheets) sheetW sheetH (/ 1.0 1000000.0)))
    0.0))

(defun cs-nest (parts sheetW sheetH kerf rotateFlag / result1 result2 result3 bestResult bestSheets bestUtil sheets util)
  (setq result1 (cs-pack-rects (cs-sort-by-area parts) sheetW sheetH kerf rotateFlag))
  (setq sheets (car result1) util (cs-calc-utilization sheets sheetW sheetH))
  (setq bestResult result1 bestSheets (length sheets) bestUtil util)
  
  (setq result2 (cs-pack-rects (cs-sort-by-max-side parts) sheetW sheetH kerf rotateFlag))
  (setq sheets (car result2) util (cs-calc-utilization sheets sheetW sheetH))
  (if (or (< (length sheets) bestSheets) (and (= (length sheets) bestSheets) (> util bestUtil)))
    (setq bestResult result2 bestSheets (length sheets) bestUtil util))
  
  (setq result3 (cs-pack-rects (cs-sort-by-min-side parts) sheetW sheetH kerf rotateFlag))
  (setq sheets (car result3) util (cs-calc-utilization sheets sheetW sheetH))
  (if (or (< (length sheets) bestSheets) (and (= (length sheets) bestSheets) (> util bestUtil)))
    (setq bestResult result3 bestSheets (length sheets) bestUtil util))
  
  (setq bestResult (list (cs-optimize-last-sheet (car bestResult) sheetW sheetH kerf rotateFlag) (cadr bestResult)))
  bestResult)

(defun cs-optimize-last-sheet (sheets sheetW sheetH kerf rotateFlag / lastSheet lastParts result1 result2 result3 bestResult bestScore score newSheets)
  (if (or (null sheets) (<= (length sheets) 1))
    sheets
    (progn
      (setq lastSheet (last sheets))
      (if (or (null lastSheet) (not (listp lastSheet)))
        sheets
        (progn
          (setq lastParts (mapcar '(lambda (p) (car p)) (cadr lastSheet)))
          (if (or (null lastParts) (<= (length lastParts) 1))
            sheets
            (progn
              (setq result1 (cs-pack-rects (cs-sort-by-area lastParts) sheetW sheetH kerf rotateFlag))
              (setq bestResult result1 bestScore (cs-calc-utilization (car result1) sheetW sheetH))
              (setq result2 (cs-pack-rects (reverse (cs-sort-by-area lastParts)) sheetW sheetH kerf rotateFlag))
              (setq score (cs-calc-utilization (car result2) sheetW sheetH))
              (if (> score bestScore) (setq bestResult result2 bestScore score))
              (setq result3 (cs-pack-rects (cs-sort-by-min-side lastParts) sheetW sheetH kerf rotateFlag))
              (setq score (cs-calc-utilization (car result3) sheetW sheetH))
              (if (> score bestScore) (setq bestResult result3 bestScore score))
              (setq newSheets (append (reverse (cdr (reverse sheets))) (car bestResult)))
              newSheets)))))))

;; ================= СТАТИСТИКА =================
(defun cs-total-actual-area-records (records / a r) (setq a 0.0) (foreach r records (setq a (+ a (nth 6 r)))) a)
(defun cs-total-bbox-area-records (records / a r) (setq a 0.0) (foreach r records (setq a (+ a (* (nth 4 r) (nth 5 r) (/ 1.0 1000000.0))))) a)

;; ================= ГРАФИКА =================
(defun cs-ensure-italic-style (/ result)
  (if (tblsearch "STYLE" "Раскрой Italic") T
    (progn
      (setq result (entmake (list '(0 . "STYLE") '(100 . "AcDbSymbolTableRecord")
                                  '(100 . "AcDbTextStyleTableRecord")
                                  '(2 . "Раскрой Italic") '(70 . 0)
                                  '(40 . 0.0) '(41 . 1.0) '(50 . 0.26)
                                  '(71 . 0) '(42 . 2.5) '(3 . "Arial") '(4 . ""))))
      (if result (tblsearch "STYLE" "Раскрой Italic") nil))))

(defun cs-ensure-bold-style (/ result)
  (if (tblsearch "STYLE" "Основной стиль (надписи без наклона)") T
    (progn
      (setq result (entmake (list '(0 . "STYLE") '(100 . "AcDbSymbolTableRecord")
                                  '(100 . "AcDbTextStyleTableRecord")
                                  '(2 . "Основной стиль (надписи без наклона)")
                                  '(70 . 0) '(40 . 0.0) '(41 . 1.0) '(50 . 0.0)
                                  '(71 . 0) '(42 . 2.5) '(3 . "arialbd.ttf") '(4 . ""))))
      (if result (tblsearch "STYLE" "Основной стиль (надписи без наклона)") nil))))

(defun cs-draw-line (p1 p2 color)
  (entmake (list '(0 . "LINE") '(100 . "AcDbEntity")
                 (cons 62 color)
                 (cons 10 (list (car p1) (cadr p1) 0.0))
                 (cons 11 (list (car p2) (cadr p2) 0.0)))))

(defun cs-draw-rect (p1 p2 color)
  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity")
                 (cons 62 color) '(100 . "AcDbPolyline")
                 '(90 . 4) '(70 . 1)
                 (cons 10 (list (car p1) (cadr p1)))
                 (cons 10 (list (car p2) (cadr p1)))
                 (cons 10 (list (car p2) (cadr p2)))
                 (cons 10 (list (car p1) (cadr p2))))))

;; Прозрачность через ActiveX
(defun cs-apply-transparency (ent val90 / obj r)
  (if ent
    (progn
      (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
      (if (not (vl-catch-all-error-p obj))
        (progn
          (setq r (vl-catch-all-apply 'vlax-put-property (list obj 'EntityTransparency val90)))
          (if (vl-catch-all-error-p r)
            (vl-catch-all-apply 'vlax-put-property (list obj 'Transparency val90)))))))
  ent)

;; transparency: 0.0..1.0 (где 1.0 = 90% прозрачности)
;; ЗАЛИВКА ЧЕРЕЗ SOLID (цвет через entmod)
(defun cs-draw-filled-rect (p1 p2 color transparency / x1 y1 x2 y2 ent res aci edata)
  (setq x1 (car p1) y1 (cadr p1) x2 (car p2) y2 (cadr p2))
  ;; Преобразуем прозрачность 0.0..1.0 в 0..90
  (setq aci (fix (+ 0.5 (* transparency 90.0))))
  (if (> aci 90) (setq aci 90))
  (if (< aci 0) (setq aci 0))
  
  ;; Создаем SOLID БЕЗ кода 62
  (setq res
    (entmake
      (list
        '(0 . "SOLID")
        '(100 . "AcDbEntity")
        '(100 . "AcDbTrace")
        (cons 10 (list x1 y1 0.0))
        (cons 11 (list x2 y1 0.0))
        (cons 12 (list x1 y2 0.0))
        (cons 13 (list x2 y2 0.0))
        '(39 . 0.0)
      )
    )
  )
  
  (if res
    (progn
      (setq ent (entlast))
      ;; Устанавливаем цвет через entmod
      (setq edata (entget ent))
      (entmod (append edata (list (cons 62 color))))
      ;; Устанавливаем прозрачность через ActiveX
      (cs-apply-transparency ent aci)
      ent)
    nil))

(defun cs-draw-text (pt h txt color / style)
  (setq style (if (tblsearch "STYLE" "Раскрой Italic") "Раскрой Italic" (getvar "TEXTSTYLE")))
  (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity")
                 (cons 62 color) (cons 7 style)
                 (cons 10 (list (car pt) (cadr pt) 0.0))
                 (cons 40 h) (cons 1 txt) '(50 . 0.0))))

(defun cs-draw-text-bold (pt h txt color / style)
  (setq style (if (tblsearch "STYLE" "Основной стиль (надписи без наклона)")
                "Основной стиль (надписи без наклона)" (getvar "TEXTSTYLE")))
  (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity")
                 (cons 62 color) (cons 7 style)
                 (cons 10 (list (car pt) (cadr pt) 0.0))
                 (cons 40 h) (cons 1 txt) '(50 . 0.0))))

(defun cs-draw-text-center (pt h txt color / style)
  (setq style (if (tblsearch "STYLE" "Раскрой Italic") "Раскрой Italic" (getvar "TEXTSTYLE")))
  (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity")
                 (cons 62 color) (cons 7 style)
                 (cons 10 (list (car pt) (cadr pt) 0.0))
                 (cons 11 (list (car pt) (cadr pt) 0.0))
                 (cons 40 h) (cons 1 txt) '(50 . 0.0)
                 '(72 . 1) '(73 . 2))))

(defun cs-build-color-map (groups / out i g)
  (setq out '() i 0)
  (foreach g groups
    (setq out (cons (cons (car g) (nth (rem i (length *CUTSHEET-PALETTE*)) *CUTSHEET-PALETTE*)) out) i (1+ i)))
  out)

(defun cs-color-for-part (r colorMap / a)
  (setq a (assoc (cs-part-key r) colorMap))
  (if a (cdr a) *CUTSHEET-PART-TEXT-COLOR*))

(defun cs-draw-sheet-header (x y sheetW sheetH n / h textY)
  (setq h *CUTSHEET-SHEET-HEADER*)
  (cs-draw-text-bold (list x (+ y sheetH (* h 0.5))) *CUTSHEET-TEXT-H*
                     (strcat "ЛИСТ " (itoa n)) *CUTSHEET-HEADER-COLOR*)
  (cs-draw-text (list x (+ y sheetH (* h 0.15))) (* *CUTSHEET-TEXT-H* 0.75)
                (cs-itoa-safe sheetH) *CUTSHEET-VALUE-COLOR*)
  (cs-draw-line (list x y) (list (+ x sheetW) y) *CUTSHEET-OUTLINE-COLOR*)
  (setq textY (- y (* h 0.35)))
  (cs-draw-text (list x textY) (* *CUTSHEET-TEXT-H* 0.75) "0" *CUTSHEET-VALUE-COLOR*)
  (cs-draw-text (list (+ x sheetW -140.0) textY) (* *CUTSHEET-TEXT-H* 0.75)
                (cs-itoa-safe sheetW) *CUTSHEET-VALUE-COLOR*))

;; ОТРИСОВКА ИЗДЕЛИЯ: Заливка SOLID + Обводка + Подпись
(defun cs-draw-placement (pl x0 y0 colorMap / r x y w h rot col)
  (setq r (car pl)
        x (+ x0 (cadr pl))
        y (+ y0 (caddr pl))
        w (nth 3 pl)
        h (nth 4 pl)
        rot (nth 5 pl)
        col (cs-color-for-part r colorMap))
  ;; 1. Заливка (под обводкой) — SOLID с прозрачностью
  (cs-draw-filled-rect (list x y) (list (+ x w) (+ y h)) col *CUTSHEET-PART-TRANSPARENCY*)
  ;; 2. Обводка (поверх заливки)
  (cs-draw-rect (list x y) (list (+ x w) (+ y h)) *CUTSHEET-OUTLINE-COLOR*)
  ;; 3. Подпись
  (if (> (* w h) *CUTSHEET-MIN-TEXT-AREA*)
    (cs-draw-text-center (list (+ x (* w 0.5)) (+ y (* h 0.53)))
                         (min *CUTSHEET-TEXT-H* (* 0.12 (min w h)))
                         (cs-part-label r) *CUTSHEET-PART-TEXT-COLOR*)))

(defun cs-draw-summary (groups sheets oversized sheetW sheetH rotateFlag insPt / left top width rowH rows y totalCnt actualArea bboxArea sheetArea kpdFact kpdBox waste colorMap maxLabelLen col i sortedGroups sizeStr skipGroups g)
  (setq left (car insPt) top (cadr insPt) rowH 160.0 totalCnt 0 actualArea 0.0 bboxArea 0.0
        sheetArea (* (length sheets) sheetW sheetH (/ 1.0 1000000.0)))
  (foreach rec groups (setq totalCnt (+ totalCnt (nth 6 rec)) bboxArea (+ bboxArea (nth 7 rec))
                            actualArea (+ actualArea (nth 8 rec))))
  (setq kpdFact (if (> sheetArea 0.0) (* 100.0 (/ actualArea sheetArea)) 0.0)
        kpdBox (if (> sheetArea 0.0) (* 100.0 (/ bboxArea sheetArea)) 0.0)
        waste (max 0.0 (- sheetArea actualArea)))
  (setq rows (length groups) maxLabelLen 10)
  (foreach rec groups (setq col (strcat (cs-itoa-safe (nth 4 rec)) "x" (cs-itoa-safe (nth 5 rec))))
    (if (> (strlen col) maxLabelLen) (setq maxLabelLen (strlen col))))
  (setq width (max *CUTSHEET-SUMMARY-W* (+ 600.0 (* maxLabelLen 40.0))))
  (cs-draw-rect (list left (- top (* rowH (+ rows 13 (if oversized 1 0)))))
                (list (+ left width) top) *CUTSHEET-OUTLINE-COLOR*)
  
  (setq y (- top (* rowH 0.72)))
  (cs-draw-text-bold (list (+ left 50.0) y) (* *CUTSHEET-TEXT-H* 1.45) "РАСКРОЙ ЛИСТА" *CUTSHEET-TITLE-COLOR*)
  (setq y (- y (* rowH 1.28)))
  (cs-draw-text (list (+ left 50.0) y) *CUTSHEET-TEXT-H*
                (strcat "Лист: " (cs-itoa-safe sheetW) " x " (cs-itoa-safe sheetH) " мм") *CUTSHEET-HEADER-COLOR*)
  (setq y (- y rowH))
  (cs-draw-text (list (+ left 50.0) y) *CUTSHEET-TEXT-H*
                (strcat "Листов: " (itoa (length sheets))) *CUTSHEET-VALUE-COLOR*)
  (setq y (- y rowH))
  (cs-draw-text (list (+ left 50.0) y) *CUTSHEET-TEXT-H*
                (strcat "Изделий: " (itoa totalCnt) " шт.") *CUTSHEET-VALUE-COLOR*)
  (setq y (- y rowH))
  (cs-draw-text (list (+ left 50.0) y) *CUTSHEET-TEXT-H*
                (if rotateFlag "Поворот деталей разрешен" "Поворот деталей запрещен") *CUTSHEET-VALUE-COLOR*)
  (setq y (- y rowH))
  (cs-draw-text (list (+ left 50.0) y) *CUTSHEET-TEXT-H*
                (strcat "Фактическая площадь: " (cs-format-num actualArea 2) " м2") *CUTSHEET-VALUE-COLOR*)
  (setq y (- y rowH))
  (cs-draw-text (list (+ left 50.0) y) *CUTSHEET-TEXT-H*
                (strcat "Площадь габаритов: " (cs-format-num bboxArea 2) " м2") *CUTSHEET-VALUE-COLOR*)
  (setq y (- y rowH))
  (cs-draw-text-bold (list (+ left 50.0) y) *CUTSHEET-TEXT-H*
                     (strcat "ПОЛЕЗНЫЙ ВЫХОД: " (cs-format-num kpdFact 1) "%") *CUTSHEET-KPD-COLOR*)
  (setq y (- y rowH))
  (cs-draw-text (list (+ left 50.0) y) *CUTSHEET-TEXT-H*
                (strcat "Габаритный выход: " (cs-format-num kpdBox 1) "%") *CUTSHEET-VALUE-COLOR*)
  (setq y (- y rowH))
  (cs-draw-text (list (+ left 50.0) y) *CUTSHEET-TEXT-H*
                (strcat "Потери: " (cs-format-num waste 2) " м2") *CUTSHEET-WASTE-COLOR*)
  (setq y (- y (* 1.2 rowH)))
  (cs-draw-text-bold (list (+ left 50.0) y) *CUTSHEET-TEXT-H* "ИЗДЕЛИЯ (ВхШ)" *CUTSHEET-TITLE-COLOR*)
  (setq y (- y rowH))
  (cs-draw-text (list (+ left 50.0) y) (* *CUTSHEET-TEXT-H* 0.82) "Размер" *CUTSHEET-HEADER-COLOR*)
  (cs-draw-text (list (+ left (* width 0.45)) y) (* *CUTSHEET-TEXT-H* 0.82) "Кол-во" *CUTSHEET-HEADER-COLOR*)
  (cs-draw-text (list (+ left (* width 0.7)) y) (* *CUTSHEET-TEXT-H* 0.82) "Площадь" *CUTSHEET-HEADER-COLOR*)
  (setq y (- y rowH))
  
  (if rotateFlag
    (setq sortedGroups (vl-sort groups
      '(lambda (a b)
         (cond
           ((< (min (nth 4 a) (nth 5 a)) (min (nth 4 b) (nth 5 b))) T)
           ((> (min (nth 4 a) (nth 5 a)) (min (nth 4 b) (nth 5 b))) nil)
           ((< (max (nth 4 a) (nth 5 a)) (max (nth 4 b) (nth 5 b))) T)
           (T nil)))))
    (setq sortedGroups (vl-sort groups
      '(lambda (a b)
         (cond
           ((< (nth 5 a) (nth 5 b)) T)
           ((> (nth 5 a) (nth 5 b)) nil)
           ((< (nth 4 a) (nth 4 b)) T)
           (T nil))))))
  
  (setq colorMap (cs-build-color-map groups))
  (setq i 0)
  (foreach rec sortedGroups
    (setq col (nth (rem i (length *CUTSHEET-PALETTE*)) *CUTSHEET-PALETTE*))
    (if rotateFlag
      (setq sizeStr (strcat (cs-itoa-safe (min (nth 4 rec) (nth 5 rec))) "x"
                            (cs-itoa-safe (max (nth 4 rec) (nth 5 rec)))))
      (setq sizeStr (strcat (cs-itoa-safe (nth 5 rec)) "x" (cs-itoa-safe (nth 4 rec)))))
    (cs-draw-text (list (+ left 50.0) y) (* *CUTSHEET-TEXT-H* 0.82) sizeStr col)
    (cs-draw-text (list (+ left (* width 0.45)) y) (* *CUTSHEET-TEXT-H* 0.82)
                  (itoa (nth 6 rec)) *CUTSHEET-VALUE-COLOR*)
    (cs-draw-text (list (+ left (* width 0.7)) y) (* *CUTSHEET-TEXT-H* 0.82)
                  (cs-format-num (nth 8 rec) 2) *CUTSHEET-VALUE-COLOR*)
    (setq y (- y rowH))
    (setq i (1+ i)))
  
  ;; Этап 2: секция неразмещенных - агрегировано по габариту:
  ;; заголовок «НЕРАЗМЕЩЕНО (ВxШ)», колонки «габариты | количество».
  (setq skipGroups nil)
  (if oversized
    (progn
      (foreach r oversized
        (setq g (assoc (cs-part-label r) skipGroups))
        (if g
          (setq skipGroups
                (subst (list (car g) (cadr g) (caddr g) (1+ (cadddr g))) g skipGroups))
          (setq skipGroups
                (cons (list (cs-part-label r) (nth 4 r) (nth 5 r) 1) skipGroups))))
      (setq skipGroups
        (vl-sort skipGroups
          '(lambda (a b)
             (cond ((> (* (cadr a) (caddr a)) (* (cadr b) (caddr b))) T)
                   ((< (* (cadr a) (caddr a)) (* (cadr b) (caddr b))) nil)
                   ((vl-string< (car a) (car b)) T)
                   (T nil)))))
      (setq y (- y rowH))
      (cs-draw-text-bold (list (+ left 50.0) y) *CUTSHEET-TEXT-H*
                         "НЕРАЗМЕЩЕНО (ВxШ)" *CUTSHEET-TITLE-COLOR*)
      (setq y (- y rowH))
      (cs-draw-text (list (+ left 50.0) y) (* *CUTSHEET-TEXT-H* 0.82)
                    "Размер" *CUTSHEET-HEADER-COLOR*)
      (cs-draw-text (list (+ left (* width 0.45)) y) (* *CUTSHEET-TEXT-H* 0.82)
                    "Кол-во" *CUTSHEET-HEADER-COLOR*)
      (foreach g skipGroups
        (setq y (- y rowH))
        (cs-draw-text (list (+ left 50.0) y) (* *CUTSHEET-TEXT-H* 0.82)
                      (car g) *CUTSHEET-WASTE-COLOR*)
        (cs-draw-text (list (+ left (* width 0.45)) y) (* *CUTSHEET-TEXT-H* 0.82)
                      (itoa (nth 3 g)) *CUTSHEET-WASTE-COLOR*))))
  
  ;; Рамка - по фактической нижней линии текста (запас 0.8 строки):
  ;; секция неразмещенных конструктивно не может быть зачеркнута.
  (list (list left (- y (* rowH 0.8))) (list (+ left width) top)))

;; Фактический объемлющий прямоугольник всех примитивов набора.
;; Рамка результата строится по реальным границам (GetBoundingBox),
;; а не по расчетным координатам - наложение кромки исключено.
;; SAFEARRAY превращается в список точек независимо от того,
;; как отдал параметры AutoCAD: SAFEARRAY напрямую или VARIANT(safearray).
(defun cs-point-array->list (v)
  (cond ((= (type v) 'SAFEARRAY) (vlax-safearray->list v))
        ((= (type v) 'VARIANT)
         (if (= (type (vlax-variant-value v)) 'SAFEARRAY)
           (vlax-safearray->list (vlax-variant-value v))))
        ((= (type v) 'LIST) v)
        (T nil)))

(defun cs-entities-bbox (ss / i e o mn mx x1 y1 x2 y2)
  (setq x1 nil)
  (setq i 0)
  (repeat (sslength ss)
    (setq e (ssname ss i))
    (setq o (vl-catch-all-apply 'vlax-ename->vla-object (list e)))
    (if (and (not (vl-catch-all-error-p o)) o)
      (progn
        ;; Принудительное обновление примитива: без него AutoCAD может отдать
        ;; устаревший/усеченный bbox для только что созданных текстов,
        ;; и рамка по факт-bbox обрезала бы последние строки.
        (vl-catch-all-apply 'vla-update (list o))
        (setq mn nil)
        (setq mx nil)
        (vl-catch-all-apply 'vla-GetBoundingBox (list o 'mn 'mx))
        (setq mn (cs-point-array->list mn))
        (setq mx (cs-point-array->list mx))
        (if (and mn mx)
          (progn
            (if (or (null x1) (< (car mn) x1)) (setq x1 (car mn)))
            (if (or (null y1) (< (cadr mn) y1)) (setq y1 (cadr mn)))
            (if (or (null x2) (> (car mx) x2)) (setq x2 (car mx)))
            (if (or (null y2) (> (cadr mx) y2)) (setq y2 (cadr mx)))
          )
        )
      )
    )
    (setq i (1+ i))
  )
  (if x1 (list (list x1 y1) (list x2 y2)) nil)
)

(defun cs-draw-frame (bbox / x1 y1 x2 y2)
  (if (not (tblsearch "LAYER" *CUTSHEET-FRAME-LAYER*))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord")
                   (cons 2 *CUTSHEET-FRAME-LAYER*) '(70 . 0)
                   '(62 . 7) '(6 . "Continuous") '(370 . -3))))
  (setq x1 (- (car (car bbox)) *CUTSHEET-FRAME-PAD-LEFT*)
        y1 (- (cadr (car bbox)) *CUTSHEET-FRAME-PAD-BOTTOM*)
        x2 (+ (car (cadr bbox)) *CUTSHEET-FRAME-PAD-RIGHT*)
        y2 (+ (cadr (cadr bbox)) *CUTSHEET-FRAME-PAD-TOP*))
  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity")
                 (cons 8 *CUTSHEET-FRAME-LAYER*) '(100 . "AcDbPolyline")
                 '(90 . 4) '(70 . 1)
                 (cons 10 (list x1 y1)) (cons 10 (list x2 y1))
                 (cons 10 (list x2 y2)) (cons 10 (list x1 y2))))
  (list (list x1 y1) (list x2 y2)))

(defun cs-draw-layout (sheets sheetW sheetH insPt colorMap / n sh col row x y p)
  (setq n 0)
  (foreach sh sheets
    (setq n (1+ n) col (rem (1- n) *CUTSHEET-GRID-COLS*)
          row (fix (/ (1- n) *CUTSHEET-GRID-COLS*))
          x (+ (car insPt) (* col (+ sheetW *CUTSHEET-SHEET-GAP*)))
          y (- (cadr insPt) (* row (+ sheetH *CUTSHEET-SHEET-GAP* *CUTSHEET-SHEET-HEADER*))))
    (cs-draw-sheet-header x y sheetW sheetH n)
    (cs-draw-rect (list x y) (list (+ x sheetW) (+ y sheetH)) *CUTSHEET-OUTLINE-COLOR*)
    (foreach p (cadr sh) (cs-draw-placement p x y colorMap)))
  (list (list (car insPt) (- (cadr insPt) (* (max 0 (fix (/ (max 0 (1- n)) *CUTSHEET-GRID-COLS*)))
                                              (+ sheetH *CUTSHEET-SHEET-GAP* *CUTSHEET-SHEET-HEADER*))))
        (list (+ (car insPt) (* (max 0 (1- (min n *CUTSHEET-GRID-COLS*))) (+ sheetW *CUTSHEET-SHEET-GAP*)) sheetW)
              (+ (cadr insPt) sheetH))))

;; ================= XLS / CSV =================
(defun cs-xml-escape (s)
  (setq s (vl-string-subst "&amp;" "&" s))
  (setq s (vl-string-subst "&lt;" "<" s))
  (setq s (vl-string-subst "&gt;" ">" s))
  (setq s (vl-string-subst "&quot;" "\"" s))
  s)

(defun cs-write-csv (sheets oversized sheetW sheetH kerf / fname f n sh p r)
  (setq fname (strcat (getvar "DWGPREFIX") (vl-filename-base (getvar "DWGNAME")) " Раскрой листа.csv"))
  (setq f (open fname "w"))
  (if f
    (progn
      (write-line "Раскрой листа" f)
      (write-line (strcat "Лист;" (cs-itoa-safe sheetW) "x" (cs-itoa-safe sheetH) ";Пропил;" (cs-format-num kerf 2)) f)
      (write-line "№ листа;№ детали;Тип;Размер;X;Y;Поворот;Площадь, м2" f)
      (setq n 0)
      (foreach sh sheets
        (setq n (1+ n))
        (foreach p (cadr sh)
          (setq r (car p))
          (write-line (strcat (itoa n) ";" (itoa (car r)) ";" (nth 3 r) ";" (cs-part-label r) ";"
                              (cs-format-num (nth 1 p) 1) ";" (cs-format-num (nth 2 p) 1) ";"
                              (if (= (nth 5 p) 1) "90" "0") ";" (cs-format-num (nth 6 r) 4)) f)))
      (if oversized
        (progn
          (write-line "" f)
          (write-line "НЕРАЗМЕЩЕННЫЕ ДЕТАЛИ" f)
          (foreach r oversized
            (write-line (strcat (itoa (car r)) ";" (nth 3 r) ";" (cs-part-label r) ";"
                                (cs-format-num (nth 6 r) 4)) f))))
      (close f)
      (princ (strcat "\nCSV сохранен: " fname))
      T)
    nil))

(defun cs-write-xls (groups sheets oversized sheetW sheetH kerf / fname f n sh p r rec totalCnt actualArea bboxArea sheetArea kpdFact kpdBox)
  (setq fname (strcat (getvar "DWGPREFIX") (vl-filename-base (getvar "DWGNAME")) " Раскрой листа.xls"))
  (setq f (open fname "w"))
  (if f
    (progn
      (setq totalCnt 0 actualArea 0.0 bboxArea 0.0)
      (foreach rec groups (setq totalCnt (+ totalCnt (nth 6 rec)) bboxArea (+ bboxArea (nth 7 rec))
                                actualArea (+ actualArea (nth 8 rec))))
      (setq sheetArea (* (length sheets) sheetW sheetH (/ 1.0 1000000.0))
            kpdFact (if (> sheetArea 0.0) (* 100.0 (/ actualArea sheetArea)) 0.0)
            kpdBox  (if (> sheetArea 0.0) (* 100.0 (/ bboxArea sheetArea)) 0.0))
      (write-line "<?xml version=\"1.0\" encoding=\"windows-1251\"?>" f)
      (write-line "<?mso-application progid=\"Excel.Sheet\"?>" f)
      (write-line "<Workbook xmlns=\"urn:schemas-microsoft-com:office:spreadsheet\"" f)
      (write-line " xmlns:o=\"urn:schemas-microsoft-com:office:office\"" f)
      (write-line " xmlns:x=\"urn:schemas-microsoft-com:office:excel\"" f)
      (write-line " xmlns:ss=\"urn:schemas-microsoft-com:office:spreadsheet\">" f)
      (write-line "<Styles>" f)
      (write-line "<Style ss:ID=\"Default\"><Alignment ss:Vertical=\"Center\"/></Style>" f)
      (write-line "<Style ss:ID=\"T\"><Font ss:Bold=\"1\" ss:Size=\"13\"/><Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/><Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/><Borders><Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/><Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/><Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/><Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/></Borders></Style>" f)
      (write-line "<Style ss:ID=\"H\"><Font ss:Bold=\"1\"/><Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/><Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/><Borders><Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/><Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/><Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/><Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/></Borders></Style>" f)
      (write-line "<Style ss:ID=\"D\"><Alignment ss:Vertical=\"Center\"/><Borders><Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/><Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/><Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/><Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/></Borders></Style>" f)
      (write-line "<Style ss:ID=\"N\"><NumberFormat ss:Format=\"0.00\"/><Alignment ss:Vertical=\"Center\"/><Borders><Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/><Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/><Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/><Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/></Borders></Style>" f)
      (write-line "</Styles>" f)
      (write-line "<Worksheet ss:Name=\"Итоги\"><Table>" f)
      (write-line "<Column ss:Width=\"220\"/><Column ss:Width=\"110\"/><Column ss:Width=\"110\"/><Column ss:Width=\"120\"/>" f)
      (write-line "<Row><Cell ss:StyleID=\"T\" ss:MergeAcross=\"3\"><Data ss:Type=\"String\">Раскрой листа</Data></Cell></Row>" f)
      (write-line (strcat "<Row><Cell ss:StyleID=\"D\"><Data ss:Type=\"String\">Формат листа</Data></Cell><Cell ss:StyleID=\"D\"><Data ss:Type=\"String\">" (cs-itoa-safe sheetW) "x" (cs-itoa-safe sheetH) " мм</Data></Cell></Row>") f)
      (write-line (strcat "<Row><Cell ss:StyleID=\"D\"><Data ss:Type=\"String\">Пропил</Data></Cell><Cell ss:StyleID=\"N\"><Data ss:Type=\"Number\">" (cs-xls-num kerf 2) "</Data></Cell></Row>") f)
      (write-line (strcat "<Row><Cell ss:StyleID=\"D\"><Data ss:Type=\"String\">Листов</Data></Cell><Cell ss:StyleID=\"D\"><Data ss:Type=\"Number\">" (itoa (length sheets)) "</Data></Cell></Row>") f)
      (write-line (strcat "<Row><Cell ss:StyleID=\"D\"><Data ss:Type=\"String\">Изделий</Data></Cell><Cell ss:StyleID=\"D\"><Data ss:Type=\"Number\">" (itoa totalCnt) "</Data></Cell></Row>") f)
      (write-line (strcat "<Row><Cell ss:StyleID=\"D\"><Data ss:Type=\"String\">Фактическая площадь, м2</Data></Cell><Cell ss:StyleID=\"N\"><Data ss:Type=\"Number\">" (cs-xls-num actualArea 4) "</Data></Cell></Row>") f)
      (write-line (strcat "<Row><Cell ss:StyleID=\"D\"><Data ss:Type=\"String\">Площадь габаритов, м2</Data></Cell><Cell ss:StyleID=\"N\"><Data ss:Type=\"Number\">" (cs-xls-num bboxArea 4) "</Data></Cell></Row>") f)
      (write-line (strcat "<Row><Cell ss:StyleID=\"H\"><Data ss:Type=\"String\">Полезный выход, %</Data></Cell><Cell ss:StyleID=\"H\"><Data ss:Type=\"Number\">" (cs-xls-num kpdFact 1) "</Data></Cell></Row>") f)
      (write-line (strcat "<Row><Cell ss:StyleID=\"D\"><Data ss:Type=\"String\">Габаритный выход, %</Data></Cell><Cell ss:StyleID=\"N\"><Data ss:Type=\"Number\">" (cs-xls-num kpdBox 1) "</Data></Cell></Row>") f)
      (write-line "<Row></Row>" f)
      (write-line "<Row><Cell ss:StyleID=\"H\"><Data ss:Type=\"String\">Размер</Data></Cell><Cell ss:StyleID=\"H\"><Data ss:Type=\"String\">Тип</Data></Cell><Cell ss:StyleID=\"H\"><Data ss:Type=\"String\">Кол-во</Data></Cell><Cell ss:StyleID=\"H\"><Data ss:Type=\"String\">Площадь, м2</Data></Cell></Row>" f)
      (foreach rec groups
        (write-line (strcat "<Row><Cell ss:StyleID=\"D\"><Data ss:Type=\"String\">" (cs-itoa-safe (nth 4 rec)) "x" (cs-itoa-safe (nth 5 rec)) "</Data></Cell><Cell ss:StyleID=\"D\"><Data ss:Type=\"String\">" (cs-xml-escape (nth 3 rec)) "</Data></Cell><Cell ss:StyleID=\"D\"><Data ss:Type=\"Number\">" (itoa (nth 6 rec)) "</Data></Cell><Cell ss:StyleID=\"N\"><Data ss:Type=\"Number\">" (cs-xls-num (nth 8 rec) 4) "</Data></Cell></Row>") f))
      (write-line "</Table></Worksheet>" f)
      (write-line "<Worksheet ss:Name=\"Размещение\"><Table>" f)
      (write-line "<Row><Cell ss:StyleID=\"T\" ss:MergeAcross=\"7\"><Data ss:Type=\"String\">Размещение деталей</Data></Cell></Row>" f)
      (write-line "<Row><Cell ss:StyleID=\"H\"><Data ss:Type=\"String\">Лист</Data></Cell><Cell ss:StyleID=\"H\"><Data ss:Type=\"String\">№</Data></Cell><Cell ss:StyleID=\"H\"><Data ss:Type=\"String\">Тип</Data></Cell><Cell ss:StyleID=\"H\"><Data ss:Type=\"String\">Размер</Data></Cell><Cell ss:StyleID=\"H\"><Data ss:Type=\"String\">X</Data></Cell><Cell ss:StyleID=\"H\"><Data ss:Type=\"String\">Y</Data></Cell><Cell ss:StyleID=\"H\"><Data ss:Type=\"String\">Поворот</Data></Cell><Cell ss:StyleID=\"H\"><Data ss:Type=\"String\">Площадь</Data></Cell></Row>" f)
      (setq n 0)
      (foreach sh sheets
        (setq n (1+ n))
        (foreach p (cadr sh)
          (setq r (car p))
          (write-line (strcat "<Row><Cell ss:StyleID=\"D\"><Data ss:Type=\"Number\">" (itoa n) "</Data></Cell><Cell ss:StyleID=\"D\"><Data ss:Type=\"Number\">" (itoa (car r)) "</Data></Cell><Cell ss:StyleID=\"D\"><Data ss:Type=\"String\">" (cs-xml-escape (nth 3 r)) "</Data></Cell><Cell ss:StyleID=\"D\"><Data ss:Type=\"String\">" (cs-part-label r) "</Data></Cell><Cell ss:StyleID=\"N\"><Data ss:Type=\"Number\">" (cs-xls-num (nth 1 p) 1) "</Data></Cell><Cell ss:StyleID=\"N\"><Data ss:Type=\"Number\">" (cs-xls-num (nth 2 p) 1) "</Data></Cell><Cell ss:StyleID=\"D\"><Data ss:Type=\"Number\">" (if (= (nth 5 p) 1) "90" "0") "</Data></Cell><Cell ss:StyleID=\"N\"><Data ss:Type=\"Number\">" (cs-xls-num (nth 6 r) 4) "</Data></Cell></Row>") f)))
      (write-line "</Table></Worksheet>" f)
      (if oversized
        (progn
          (write-line "<Worksheet ss:Name=\"Неразмещенные\"><Table>" f)
          (write-line "<Row><Cell ss:StyleID=\"T\" ss:MergeAcross=\"3\"><Data ss:Type=\"String\">Неразмещенные детали</Data></Cell></Row>" f)
          (foreach r oversized
            (write-line (strcat "<Row><Cell ss:StyleID=\"D\"><Data ss:Type=\"Number\">" (itoa (car r)) "</Data></Cell><Cell ss:StyleID=\"D\"><Data ss:Type=\"String\">" (cs-xml-escape (nth 3 r)) "</Data></Cell><Cell ss:StyleID=\"D\"><Data ss:Type=\"String\">" (cs-part-label r) "</Data></Cell><Cell ss:StyleID=\"N\"><Data ss:Type=\"Number\">" (cs-xls-num (nth 6 r) 4) "</Data></Cell></Row>") f))
          (write-line "</Table></Worksheet>" f)))
      (write-line "</Workbook>" f)
      (close f)
      (princ (strcat "\nXLS сохранен: " fname))
      T)
    nil))

;; ================= СЛУЖЕБНЫЕ ГРАФИЧЕСКИЕ ПРОЦЕДУРЫ =================
(defun cs-combine-bbox (a b)
  (list (list (min (car (car a)) (car (car b))) (min (cadr (car a)) (cadr (car b))))
        (list (max (car (cadr a)) (car (cadr b))) (max (cadr (cadr a)) (cadr (cadr b))))))

(defun cs-zoom-bbox (bbox / app p1 p2)
  (setq app (vl-catch-all-apply 'vlax-get-acad-object '()))
  (if (and app bbox)
    (progn
      (setq p1 (vlax-3d-point (list (car (car bbox)) (cadr (car bbox)) 0.0))
            p2 (vlax-3d-point (list (car (cadr bbox)) (cadr (cadr bbox)) 0.0)))
      (vl-catch-all-apply 'vla-ZoomWindow (list app p1 p2)))))

(defun cs-setvar-transparency-display ()
  (vl-catch-all-apply 'setvar (list "TRANSPARENCYDISPLAY" 1)))

(defun cs-unique-block-name (base / name n)
  (setq n 0 name (strcat base " " (itoa n)))
  (while (tblsearch "BLOCK" name) (setq n (1+ n) name (strcat base " " (itoa n))))
  name)

(defun cs-wrap-to-block (blockName basePt ss / oldEcho oldOsmode oldCmddia oldFiledia ok refs r oldRefs si e retained ins-result finalRefs basePtStr insertPt3 acad doc ms result)
  (if (or (null ss) (<= (sslength ss) 0))
    (progn (princ "\n[wrap] Нет объектов для блока.") nil)
    (progn
      (setq oldEcho (getvar "CMDECHO")
            oldOsmode (getvar "OSMODE")
            oldCmddia (getvar "CMDDIA")
            oldFiledia (getvar "FILEDIA"))
      
      (setq basePtStr (strcat (rtos (car basePt) 2 6) "," (rtos (cadr basePt) 2 6) ",0"))
      
      (setvar "CMDECHO" 1)
      (setvar "OSMODE" 0)
      (setvar "CMDDIA" 0)
      (setvar "FILEDIA" 0)
      
      ;; Число INSERT имени blockName ДО -BLOCK (защита от двойного INSERT, Шаг 4):
      ;; в версиях AutoCAD, где -BLOCK спрашивает [Преобразовать/Удалить], ENTER
      ;; выбирает «Преобразовать» и вхождение создается самим -BLOCK.
      (setq r (ssget "_X" (list '(0 . "INSERT") (cons 2 blockName))))
      (setq oldRefs (if r (sslength r) 0))
      
      ;; Создаем блок (текстовый -BLOCK; опции Преобразовать/Удалить есть не во всех версиях)
      (setq result (vl-catch-all-apply 'vl-cmdf (list "_.-BLOCK" blockName basePtStr ss "")))
      (cond
        ((vl-catch-all-error-p result)
         (princ (strcat "\n[wrap] Ошибка: " (vl-catch-all-error-message result)))
         (setq ok nil))
        ((not (tblsearch "BLOCK" blockName))
         (princ "\n[wrap] Блок не создан")
         (setq ok nil))
        (T
         (setq ok T)
         (setq r (ssget "_X" (list '(0 . "INSERT") (cons 2 blockName))))
         (setq refs (if r (sslength r) 0))
         (princ (strcat "\n[wrap] Блок создан: да, ссылок: " (itoa refs)))
(cond
           ((= refs (1+ oldRefs))
            ;; Ровно один новый INSERT — его создал сам -BLOCK (режим «Преобразовать»).
            ;; Второй не добавляем (двойное вхождение, Шаг 4).
            (princ "\n[wrap] INSERT создан самим -BLOCK (ровно 1 новый, режим Преобразовать) — повторная вставка не требуется."))
           ((> refs (1+ oldRefs))
            ;; Аномалия: новых INSERT больше одного — ничего не добавляем, только сигнал.
            (princ (strcat "\n[wrap] ВНИМАНИЕ: новых INSERT после -BLOCK: " (itoa (- refs oldRefs))
                           " (ожидался 1) — повторная вставка отменена.")))
           (T
(progn
             ;; INSERT не создан. Если оригиналы пережили -BLOCK (режим «оставить»),
             ;; стираем их, чтобы под INSERT не осталась дублирующая геометрия.
             (setq si 0 retained 0)
             (repeat (sslength ss)
               (setq e (ssname ss si))
               (if (entget e) (progn (entdel e) (setq retained (1+ retained))))
               (setq si (1+ si)))
             (if (> retained 0)
               (princ (strcat "\n[wrap] Удалено оригиналов, оставшихся после -BLOCK: " (itoa retained))))
             ;; Вставляем ровно один INSERT обратно в ту же точку (базовая точка = точка вставки)
             (setq acad (vlax-get-acad-object)
                   doc (vla-get-ActiveDocument acad)
                   ms (vla-get-ModelSpace doc)
                   insertPt3 (vlax-3d-point (list (car basePt) (cadr basePt) 0.0)))
             (setq ins-result (vl-catch-all-apply 'vla-InsertBlock (list ms insertPt3 blockName 1.0 1.0 1.0 0.0)))
             (if (vl-catch-all-error-p ins-result)
               (princ (strcat "\n[wrap] ОШИБКА вставки INSERT: " (vl-catch-all-error-message ins-result)))
               (princ (strcat "\n[wrap] Блок вставлен в базовую точку: " (rtos (car basePt) 2 2) "," (rtos (cadr basePt) 2 2))))
             ;; Контроль (Шаг 4): после упаковки в чертеже ровно один новый INSERT
             (setq r (ssget "_X" (list '(0 . "INSERT") (cons 2 blockName))))
             (setq finalRefs (if r (sslength r) 0))
             (princ (strcat "\n[wrap] Проверка: ссылок до -BLOCK: " (itoa oldRefs)
                            ", после вставки: " (itoa finalRefs)
                            (if (= finalRefs (1+ oldRefs)) " (OK: ровно 1 новый)" " (ВНИМАНИЕ: прирост не равен 1!)"))))))))
      (setvar "FILEDIA" oldFiledia)
      (setvar "CMDDIA" oldCmddia)
      (setvar "OSMODE" oldOsmode)
      (setvar "CMDECHO" oldEcho)
      ok)))

;; ================= ГЛАВНАЯ ФУНКЦИЯ =================
(defun cutsheet-main (layers-from-caller / *error* ss polyCnt dynCnt dynTypes
                      defaultW defaultH defaultKerf defaultRotate defaultXls defaultAcad
                      r choice sheetW sheetH kerf rotateFlag exportXls exportAcad dynType
                      records groups parts nested sheets oversized fitSrc impossible
                      insPt colorMap bbox1 bbox2 bbox3 bbox
                      doc oldEcho lastEnt ssNew ent blockName baseName uMark
                      totalCnt actualArea bboxArea sheetArea kpdFact kpdBox
                      blockBasePt oldOsmode oldCmddia oldFiledia bboxFact bboxCalc
                       csDbgMin csDbgWho csDbgTxt csDbgI csDbgEnt csDbgObj csDbgMn csDbgMx)
  
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*,*EXIT*")))
      (princ (strcat "\n[CUTSHEET ERROR] " msg)))
    (if (and uMark doc) (vl-catch-all-apply 'vla-EndUndoMark (list doc)))
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (if oldOsmode (setvar "OSMODE" oldOsmode))
    (if oldCmddia (setvar "CMDDIA" oldCmddia))
    (if oldFiledia (setvar "FILEDIA" oldFiledia))
    (setq *cs-tmp-choice* 'ALL *cs-tmp-dyn-type* "")
    (princ))
  
  (setq oldOsmode (getvar "OSMODE")
        oldCmddia (getvar "CMDDIA")
        oldFiledia (getvar "FILEDIA")
        oldEcho (getvar "CMDECHO"))
  
  (princ "\n=== РАСКРОЙ ЛИСТА ===")
  
  (if (eq layers-from-caller 'ASK)
    (progn
      (setq layers-from-caller (getstring T "\nСлои через запятую (Enter — все слои): "))
      (if (= layers-from-caller "")
        (setq layers-from-caller nil)
        (setq layers-from-caller
          (mapcar '(lambda (x) (vl-string-trim " \t" x))
                  (cs-split-string layers-from-caller ","))))))
  
  (setq ss (cs-build-filter-ss layers-from-caller))
  (if (null ss)
    (progn (princ "\nНе выбрано подходящих LWPOLYLINE/INSERT.") (princ) (exit)))
  
  (setq polyCnt (cs-count-type ss 'POLY)
        dynCnt (cs-count-type ss 'DYN)
        dynTypes (cs-collect-dyn-types ss))
  
  (princ (strcat "\nПолилиний: " (itoa polyCnt) ", динамических блоков: " (itoa dynCnt)))
  
  (if (= (+ polyCnt dynCnt) 0)
    (progn (princ "\nПодходящих исходных объектов нет.") (princ) (exit)))
  
  (setq defaultW *CUTSHEET-LAST-WIDTH*
        defaultH *CUTSHEET-LAST-HEIGHT*
        defaultKerf *CUTSHEET-LAST-KERF*
        defaultRotate *CUTSHEET-LAST-ROTATE*
        defaultXls *CUTSHEET-LAST-XLS*
        defaultAcad *CUTSHEET-LAST-ACAD*)
  
  (if (boundp '*CUTSHEET-CREATE-XLS*) (setq defaultXls *CUTSHEET-CREATE-XLS*))
  (if (boundp '*CUTSHEET-CREATE-TABLE*) (setq defaultAcad *CUTSHEET-CREATE-TABLE*))
  
  (setq r (cs-dialog polyCnt dynCnt dynTypes ss defaultW defaultH defaultKerf defaultRotate defaultXls defaultAcad))
  (if (null r) (progn (princ "\nРаскрой листа отменен.") (princ) (exit)))
  
  (setq choice (nth 0 r) sheetW (nth 1 r) sheetH (nth 2 r) kerf (nth 3 r)
        rotateFlag (nth 4 r) exportXls (nth 5 r) exportAcad (nth 6 r) dynType (nth 7 r))
  
  (setq *CUTSHEET-LAST-WIDTH* sheetW *CUTSHEET-LAST-HEIGHT* sheetH
        *CUTSHEET-LAST-KERF* kerf *CUTSHEET-LAST-ROTATE* rotateFlag
        *CUTSHEET-LAST-XLS* exportXls *CUTSHEET-LAST-ACAD* exportAcad)
  
  (setq records (cs-collect-records ss choice dynType))
  (if (null records)
    (progn (princ "\nПосле фильтрации не осталось деталей с определенными габаритами.")
           (princ) (exit)))
  
  ;; Этап 2 (V7): детали, не помещающиеся на лист ни в одной ориентации,
  ;; отсекаются ДО солвера - не попадают ни в статистику, ни в таблицы,
  ;; только в секции "НЕРАЗМЕЩЕНО" консоли/XLS/CSV.
  (setq fitSrc nil impossible nil)
  (foreach r records
    (if (cs-part-fits-sheet-p (nth 4 r) (nth 5 r) sheetW sheetH rotateFlag)
      (setq fitSrc (cons r fitSrc))
      (setq impossible (cons r impossible))))
  (setq fitSrc (reverse fitSrc) impossible (reverse impossible))
  
  (if (null fitSrc)
    (progn
      (princ (strcat "\nНЕРАЗМЕЩЕНО: " (itoa (length impossible)) " шт."))
      (foreach r impossible
        (princ (strcat "\n  " (cs-part-label r) " | " (nth 3 r))))
      (princ "\nНи одна деталь не помещается на лист. Раскрой невозможен.")
      (princ) (exit)))
  
  (princ (strcat "\nВ раскрой принято деталей: " (itoa (length fitSrc))))
  
  (setq groups (cs-aggregate fitSrc))
  (setq nested (cs-nest fitSrc sheetW sheetH kerf rotateFlag)
        sheets (car nested)
        oversized (cadr nested))
  (setq oversized (append impossible oversized))
  
  (setq totalCnt (length fitSrc)
        actualArea (cs-total-actual-area-records fitSrc)
        bboxArea (cs-total-bbox-area-records fitSrc)
        sheetArea (* (length sheets) sheetW sheetH (/ 1.0 1000000.0))
        kpdFact (if (> sheetArea 0.0) (* 100.0 (/ actualArea sheetArea)) 0.0)
        kpdBox (if (> sheetArea 0.0) (* 100.0 (/ bboxArea sheetArea)) 0.0))
  
  (princ (strcat "\nЛистов: " (itoa (length sheets)) " | деталей: " (itoa totalCnt)
                 " | полезный выход: " (cs-format-num kpdFact 1) "%"
                 " | габаритный выход: " (cs-format-num kpdBox 1) "%"))
  
  (if oversized
    (progn
      (princ (strcat "\nНЕРАЗМЕЩЕНО: " (itoa (length oversized)) " шт."))
      (foreach r oversized
        (princ (strcat "\n  " (cs-part-label r) " | " (nth 3 r))))))
  
  (if exportXls
    (if (not (cs-write-xls groups sheets oversized sheetW sheetH kerf))
      (progn
        (princ "\nXLS не создан — выполняется fallback CSV.")
        (cs-write-csv sheets oversized sheetW sheetH kerf))))
  
  (if exportAcad
    (progn
      (setq insPt (getpoint "\nУкажите точку вставки карты раскроя: "))
      (if insPt
        (progn
          (cs-ensure-italic-style)
          (cs-ensure-bold-style)
          (cs-setvar-transparency-display)
          (setq colorMap (cs-build-color-map groups))
          (setq doc (vl-catch-all-apply 'vla-get-ActiveDocument (list (vlax-get-acad-object))))
          (if (and (not (vl-catch-all-error-p doc)) doc)
            (progn (vl-catch-all-apply 'vla-StartUndoMark (list doc)) (setq uMark T)))
          
          (setq lastEnt (entlast))
          
          (setq bbox1 (cs-draw-layout sheets sheetW sheetH insPt colorMap))
          (setq bbox2 (cs-draw-summary groups sheets oversized sheetW sheetH rotateFlag
                    (list (+ (car (cadr bbox1)) *CUTSHEET-SUMMARY-GAP*) (cadr (cadr bbox1)))))
          
          ;; Собрать созданные примитивы для фактического bbox
          (setq ssNew (ssadd) ent (if lastEnt (entnext lastEnt) (entnext)))
          (while ent (ssadd ent ssNew) (setq ent (entnext ent)))
          
          ;; Этап 2: рамка - по ФАКТИЧЕСКИМ границам результата (GetBoundingBox
          ;; каждого примитива), не по расчетным координатам выноски.
          ;; Рамка = объединение фактических и расчетных границ:
          ;; не может зачеркнуть ни одну отрисованную строку.
          (setq bboxFact (cs-entities-bbox ssNew))
          (setq bboxCalc (cs-combine-bbox bbox1 bbox2))
          ;; ДИАГНОСТИКА (этап отладки рамки): низы источников отдельно
          (princ (strcat "\n[CUTSHEET] bbox источников: факт-низ "
                          (if bboxFact (rtos (cadr (car bboxFact)) 2 1) "НЕДОСТУПЕН")
                          " | расчет-низ " (rtos (cadr (car bboxCalc)) 2 1)))
          (if bboxFact
            (setq bbox (cs-combine-bbox bboxFact bboxCalc))
            (progn
              (princ "\n[CUTSHEET][GUARD] фактический bbox недоступен - рамка по расчетным границам.")
              (setq bbox bboxCalc)))
          (setq bbox3 (cs-draw-frame bbox))
          ;; ДИАГНОСТИКА (этап отладки рамки): углы рамки в мировых координатах
          (princ (strcat "\n[CUTSHEET] Рамка: Y-верх " (rtos (cadr (cadr bbox3)) 2 1)
                          " Y-низ " (rtos (cadr (car bbox3)) 2 1)
                          " X-лево " (rtos (car (car bbox3)) 2 1)
                          " X-право " (rtos (car (cadr bbox3)) 2 1)))
          (setq bbox (cs-combine-bbox bbox bbox3))
          
          ;; Пересобрать набор с учетом рамки для обертки в блок
          (setq ssNew (ssadd) ent (if lastEnt (entnext lastEnt) (entnext)))
          (while ent (ssadd ent ssNew) (setq ent (entnext ent)))

          ;; ДИАГНОСТИКА (этап отладки рамки): самый низкий примитив карты и его владелец
          (setq csDbgMin nil)
          (setq csDbgWho "")
          (setq csDbgTxt "")
          (setq csDbgI 0)
          (while (< csDbgI (sslength ssNew))
            (setq csDbgEnt (ssname ssNew csDbgI))
            (setq csDbgObj (vl-catch-all-apply (quote vlax-ename->vla-object) (list csDbgEnt)))
            (if (not (vl-catch-all-error-p csDbgObj))
              (progn
                (vl-catch-all-apply (quote vla-update) (list csDbgObj))
                (setq csDbgMn nil)
                (setq csDbgMx nil)
                (vl-catch-all-apply (quote vla-GetBoundingBox) (list csDbgObj (quote csDbgMn) (quote csDbgMx)))
                (setq csDbgMn (cs-point-array->list csDbgMn))
                (if csDbgMn
                  (if (or (null csDbgMin) (< (cadr csDbgMn) csDbgMin))
                    (progn
                      (setq csDbgMin (cadr csDbgMn))
                      (setq csDbgWho (cdr (assoc 0 (entget csDbgEnt))))
                      (setq csDbgTxt "")
                      (if (member csDbgWho (quote ("TEXT" "MTEXT")))
                        (setq csDbgTxt (substr (cdr (assoc 1 (entget csDbgEnt))) 1 24))))))))
            (setq csDbgI (1+ csDbgI)))
          (princ (strcat "\n[CUTSHEET] Самый низкий примитив: " csDbgWho " " csDbgTxt
                          " | его низ Y=" (if csDbgMin (rtos csDbgMin 2 1) "-")))
          
          (if (> (sslength ssNew) 0)
            (progn
              (setq baseName (vl-filename-base (getvar "DWGNAME"))
                    blockName (cs-unique-block-name (strcat "Раскрой листа " baseName)))
              ;; Базовая точка блока = точка вставки = верхний левый угол рамки Невидимые
              (setq blockBasePt (list (car (car bbox3)) (cadr (cadr bbox3))))
              (cs-wrap-to-block blockName blockBasePt ssNew)))
          
          (if (and doc uMark) (progn (vla-EndUndoMark doc) (setq uMark nil)))
          (cs-zoom-bbox bbox))
        (princ "\nКарта AutoCAD не построена."))))
  
  (princ))

;; ================= КОМАНДЫ =================
(defun c:CUTSHEET () (cutsheet-main 'ASK))
(defun c:РАСКРОЙЛИСТА () (cutsheet-main 'ASK))

(princ "\nCUTSHEET.LSP загружен (ред. 5: update bbox, диагностика рамки). Команды: CUTSHEET, РАСКРОЙЛИСТА")
(princ)