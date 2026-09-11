;;; ============================================================
;;; CUTLINE.LSP — модуль линейного раскроя мерного материала
;;; Команда: CUTLINE / РАСКРОЙХЛЫСТА
;;; Объекты: LINE, MLINE, динамические блоки с свойством "Длина"
;;; Алгоритм: First-Fit Decreasing (FFD)
;;
;;; ПОДДЕРЖИВАЕМЫЕ ТИПЫ:
;;;   LINE      — обычные линии
;;;   MLINE     — мультилинии (длина по осевой трассе)
;;;   DYNBLOCK  — динамические блоки со свойством "Длина"
;;;               (блоки со свойством "Ширина" или "Высота" отсеиваются)
;;
;;; ЭТАПЫ ВНЕДРЕНИЯ ДИНАМИЧЕСКИХ БЛОКОВ:
;;;   Этап 2.1: Функции анализа блоков (в select-utils.lsp)
;;;   Этап 2.2: Выбор блоков (в select-utils.lsp)
;;;   Этап 2.3: Обработка блоков в алгоритме раскроя
;;;   Этап 2.4: Диалог с 4 радиокнопками
;;;   Этап 3.1: Выпадающий список типов блоков
;;;   Этап 3.2: Динамическое обновление количества блоков
;;;             и сортировка неразмещённых деталей
;;;   Этап 3.3: Косметические исправления
;;
;;; ИСПРАВЛЕНИЯ (Этап Р1 — Ремонт кода):
;;;   Р1.2: добавлен третий аргумент в vl-catch-all-apply
;;;         для заполнения списка слоёв в диалоге
;;;   Р1.3: исправлена проверка размещения детали с учётом реза
;;;         (было (> p stock), стало (> (+ p kerf) stock))
;;
;;; ИСПРАВЛЕНИЯ (Этап Р2 — Ремонт кода):
;;;   Р2.1: параметр разделителя в n1-list-to-str.
;;;         Для CSV используется ";" вместо пробела, чтобы
;;;         Excel не интерпретировал "2000 2000" как число.
;;;   Р2.4: обработка десятичной запятой в полях ввода.
;;;         В русской локали пользователь вводит "3,5",
;;;         а atof ожидает "3.5".
;;
;;; ТИПЫ РАСКРОЯ (диалог):
;;;   Только линии
;;;   Только мультилинии
;;;   Динамические блоки
;;;   Все типы
;;; ============================================================
(vl-load-com)

;; ================= Константы =================
(setq *CUTLINE-MIN-LENGTH*   100.0)
(setq *CUTLINE-MAX-LENGTH* 500000.0)
(setq *CUTLINE-DEFAULT-TOL*    1.0)
(setq *CUTLINE-DEFAULT-STOCK* 6000.0)
(setq *CUTLINE-DEFAULT-KERF*   0.0)

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
;; =============================================

(if (not (boundp '*n1-tmp-choice*))
  (setq *n1-tmp-choice* 'ALL))

(if (not (boundp '*n1-tmp-stock*))   (setq *n1-tmp-stock* *CUTLINE-DEFAULT-STOCK*))
(if (not (boundp '*n1-tmp-kerf*))    (setq *n1-tmp-kerf*  *CUTLINE-DEFAULT-KERF*))
(if (not (boundp '*n1-tmp-chk-xls*)) (setq *n1-tmp-chk-xls* T))
(if (not (boundp '*n1-tmp-chk-acad*)) (setq *n1-tmp-chk-acad* T))

;; ДОБАВЛЕНО (Этап 3.1): выбранный тип динамического блока
;; "" означает "Все типы блоков"
(if (not (boundp '*n1-tmp-dynblock-type*))
  (setq *n1-tmp-dynblock-type* "")
)

;; ДОБАВЛЕНО (Этап 3.1): список уникальных типов динамических блоков
(if (not (boundp '*n1-dynblock-types-list*))
  (setq *n1-dynblock-types-list* '())
)

;; ДОБАВЛЕНО (Этап 3.2): сохранение набора для пересчёта при выборе типа
(if (not (boundp '*n1-cutline-ss*))
  (setq *n1-cutline-ss* nil)
)

(if (not (boundp '*CUTLINE-LAST-STOCK*)) (setq *CUTLINE-LAST-STOCK* *CUTLINE-DEFAULT-STOCK*))
(if (not (boundp '*CUTLINE-LAST-KERF*))  (setq *CUTLINE-LAST-KERF*  *CUTLINE-DEFAULT-KERF*))
(if (not (boundp '*CUTLINE-LAST-XLS*))   (setq *CUTLINE-LAST-XLS*   T))
(if (not (boundp '*CUTLINE-LAST-ACAD*))  (setq *CUTLINE-LAST-ACAD*  T))

;; ---------- Утилиты ----------
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

(defun n1-trans-value (percent)
  (fix (* 255.0 (/ (- 100.0 (float percent)) 100.0)))
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

;; ============================================================
;; ИСПРАВЛЕНО: защита от nil-цвета
;; ============================================================
(defun n1-draw-text (pt h str color / style c)
  (setq style (if (and *NEST-TEXT-STYLE* (/= *NEST-TEXT-STYLE* ""))
                *NEST-TEXT-STYLE* (getvar "TEXTSTYLE")))
  ;; ЗАЩИТА: если color nil или не число — используем 7
  (setq c (if (and color (numberp color)) color 7))
  (entmake (list (cons 0 "TEXT") (cons 62 c) (cons 7 style)
                 (cons 10 (list (car pt) (cadr pt) 0.0))
                 (cons 40 h) (cons 1 str) (cons 50 0.0)))
)

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

(defun n1-draw-hatch (p1 p2 color trans / x1 y1 x2 y2)
  (setq x1 (car p1) y1 (cadr p1) x2 (car p2) y2 (cadr p2))
  (entmake (list (cons 0 "HATCH") (cons 100 "AcDbEntity")
                 (cons 62 color) (cons 440 trans) (cons 100 "AcDbHatch")
                 (cons 10 (list 0.0 0.0 0.0)) (cons 210 (list 0.0 0.0 1.0))
                 (cons 2 "SOLID") (cons 70 1) (cons 71 0) (cons 91 1) (cons 92 2)
                 (cons 72 0) (cons 73 1) (cons 93 4)
                 (cons 10 (list x1 y1)) (cons 10 (list x2 y1))
                 (cons 10 (list x2 y2)) (cons 10 (list x1 y2))
                 (cons 75 0) (cons 76 1) (cons 47 1.0) (cons 78 0) (cons 98 0)))
)

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
;; Алгоритм раскроя First-Fit Decreasing (FFD)
;; ИСПРАВЛЕНО (Р1.3): проверка размещения детали с учётом реза
;; ============================================================
(defun n1-ffd (sorted-pieces stock kerf / bars p placed j bar newbar skip)
  (setq bars '())
  (setq skip 0)
  (foreach p sorted-pieces
    ;; ИСПРАВЛЕНО (Р1.3): проверяем с учётом реза
    (if (> (+ p kerf) stock)
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

(defun n1-mline-length (ent) (su-mline-length ent))

(defun n1-add-group (groups key / found)
  (setq found (assoc key groups))
  (if found
    (mapcar '(lambda (x) (if (= (car x) key) (cons (car x) (1+ (cdr x))) x)) groups)
    (cons (cons key 1) groups)
  )
)

;; ============================================================
;; Подсчёт объектов по типам
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
;; ДОБАВЛЕНО (Этап 3.1)
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
;; Подсчёт динамических блоков конкретного типа
;; ДОБАВЛЕНО (Этап 3.2)
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
;; Отображение списка слоёв в диалоге и консоли
;; ИСПРАВЛЕНО: Корректная логика отображения.
;; Текст "Групповой фильтр..." показывается ТОЛЬКО если
;; сработал автоматический выбор (пользователь не выбирал
;; слои вручную в списке).
;; ============================================================
(defun n1-layer-display-list (layers / fname suffix)
  (setq fname (n1-filter-name-str))
  (setq suffix "")

  (cond
    ;; 1. Если сработал автоматический выбор по групповым фильтрам
    ;; (пользователь не выделял слои руками в списке)
    ((and (boundp '*CUTLINE-IS-AUTO-FILTER*) 
          *CUTLINE-IS-AUTO-FILTER* 
          fname)
     (setq suffix " (за исключением слоя 0)")
     (list (strcat "Групповой фильтр " fname suffix))
    )
    
    ;; 2. Если слои не выбраны вообще
    ((or (null layers) (not (listp layers)) (= (length layers) 0))
     (list "Все слои")
    )
    
    ;; 3. Если пользователь выбрал слои вручную 
    ;; (показываем сам список слоёв, даже если галки на фильтрах стоят)
    (T layers)
  )
)

;; Безопасные обёртки
(defun n1-safe-set-tile (key value)
  (vl-catch-all-apply 'set_tile (list key value))
)
(defun n1-safe-action-tile (key action)
  (vl-catch-all-apply 'action_tile (list key action))
)
(defun n1-safe-mode-tile (key mode)
  (vl-catch-all-apply 'mode_tile (list key mode))
)
(defun n1-safe-get-tile (key / r)
  (setq r (vl-catch-all-apply 'get_tile (list key)))
  (if (vl-catch-all-error-p r) nil r)
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
;; Диалог параметров раскроя
;; ИСПРАВЛЕНО (Р1.2): добавлен третий аргумент в
;; vl-catch-all-apply для заполнения списка слоёв
;; ИСПРАВЛЕНО (Р2.4): обработка десятичной запятой
;; ============================================================
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

              ;; ---- Слои ----
              (setq base-layers (n1-layer-display-list layers))
              ;; ИСПРАВЛЕНО (Р1.2): добавлен третий аргумент
              (vl-catch-all-apply
                '(lambda ()
                   (start_list "lst_layers")
                   (foreach l base-layers (add_list l))
                   (end_list))
                nil)

              ;; ---- Радиокнопки: начальное состояние ----
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

              ;; ---- Параметры ----
              (n1-safe-set-tile "edt_stock" (rtos default-stock 2 0))
              (n1-safe-set-tile "edt_kerf"  (rtos default-kerf 2 0))

              ;; ---- Экспорт ----
              (n1-safe-set-tile "chk_xls"  (if default-xls  "1" "0"))
              (n1-safe-set-tile "chk_acad" (if default-acad "1" "0"))

              ;; ---- Обработчики радиокнопок ----
              (n1-safe-action-tile "rb_line"     "(n1-select-radio \"rb_line\")")
              (n1-safe-action-tile "rb_mline"    "(n1-select-radio \"rb_mline\")")
              (n1-safe-action-tile "rb_dynblock" "(n1-select-radio \"rb_dynblock\")")
              (n1-safe-action-tile "rb_both"     "(n1-select-radio \"rb_both\")")

              ;; ---- Обработчик выпадающего списка типов блоков ----
              (n1-safe-action-tile "popup_dynblock_type"
                "(n1-on-dynblock-type-changed $value)")

              ;; ИСПРАВЛЕНО (Р2.4): обработка десятичной запятой
              ;; В русской локали пользователь вводит "3,5",
              ;; а atof ожидает "3.5"
              (n1-safe-action-tile "edt_stock"
                "(setq *n1-tmp-stock* (atof (vl-string-translate \",\" \".\" $value)))")
              (n1-safe-action-tile "edt_kerf"
                "(setq *n1-tmp-kerf* (atof (vl-string-translate \",\" \".\" $value)))")

              (n1-safe-action-tile "chk_xls"
                "(setq *n1-tmp-chk-xls* (= $value \"1\"))")
              (n1-safe-action-tile "chk_acad"
                "(setq *n1-tmp-chk-acad* (= $value \"1\"))")

              (n1-safe-action-tile "btn_ok"     "(done_dialog 1)")
              (n1-safe-action-tile "btn_cancel" "(done_dialog 0)")

              (setq result (start_dialog))

              (vl-catch-all-apply 'unload_dialog (list dcl-id))

              (if (= result 1)
                (list
                  *n1-tmp-choice*
                  default-tol
                  (if (<= *n1-tmp-stock* 0.0) default-stock *n1-tmp-stock*)
                  (if (< *n1-tmp-kerf* 0.0) default-kerf *n1-tmp-kerf*)
                  *n1-tmp-chk-xls*
                  *n1-tmp-chk-acad*
                  *n1-tmp-dynblock-type*
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
                            i ent typ len key pieces total
                            measured skipped skipped-short skipped-long obj)
  (setq pieces '() i 0 total (sslength ss)
        measured 0 skipped 0 skipped-short 0 skipped-long 0)
  (repeat total
    (setq ent (ssname ss i))
    (setq typ (cdr (assoc 0 (entget ent))))

    (cond
      ((= typ "MLINE")
       (setq len (n1-mline-length ent))
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
;; Вывод неразмещённых деталей в консоль
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
;; ИСПРАВЛЕНО (Р2.1): добавлен параметр разделителя
;;
;; Использование:
;;   (n1-list-to-str pieces " ")  — для консоли и таблицы
;;   (n1-list-to-str pieces ";")  — для CSV
;;   (n1-list-to-str pieces ", ") — для XLS
;; ============================================================
(defun n1-list-to-str (lst sep / s x)
  (if (null sep) (setq sep " "))
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
    barHeight gap txtH x0 y0 maxy miny i bar pieces waste used util
    curx p halfw str col labelX labelY1 labelY2)
  (setq barHeight (/ stock 30.0) gap (* barHeight 0.7) txtH (* barHeight 0.30))
  (setq x0 (car insPt) y0 (cadr insPt) maxy (+ y0 barHeight) miny y0 i 0)
  (foreach bar bars
    (setq i (1+ i))
    (setq pieces (cdr bar) waste (car bar) used (- stock waste)
          util (* 100.0 (/ used stock)) miny y0)
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

    (setq labelX (- x0 (* barHeight 2.75)))
    (setq labelY1 (+ y0 (* barHeight 0.65)))
    (setq labelY2 (+ y0 (* barHeight 0.20)))
    (n1-draw-text (list labelX labelY1) txtH
                  (strcat "Хлыст " (itoa i))
                  *NEST-COLOR-LABEL*)
    (n1-draw-text (list labelX labelY2) txtH
                  (strcat "[" (rtos util 2 1) "%]")
                  *NEST-COLOR-LABEL*)

    (setq curx x0)
    (foreach p pieces
      (setq str (itoa (fix p)) halfw (* (strlen str) txtH 0.4)
            col (n1-get-color color-map p))
      (n1-draw-text (list (+ curx (* p 0.5) (- halfw)) (+ y0 (* barHeight 0.35)))
                    txtH str col)
      (setq curx (+ curx p kerf))
    )
    (if (> waste 0.0)
      (progn
        (setq str (strcat "Отход " (itoa (fix waste)))
              halfw (* (strlen str) txtH 0.4))
        (n1-draw-text (list (+ (- (+ x0 stock) waste) (* waste 0.5) (- halfw))
                            (+ y0 (* barHeight 0.35)))
                      txtH str *NEST-COLOR-WASTE*)
      )
    )
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
;; Таблица неразмещённых деталей
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
;; Консольный отчёт по хлыстам
;; ИСПРАВЛЕНО (Р2.1): параметр разделителя
;; ============================================================
(defun n1-report (bars stock kerf / i bar pieces waste used util)
  (princ (strcat "\nКоличество хлыстов: " (itoa (length bars))))
  (setq i 0)
  (foreach bar bars
    (setq i (1+ i))
    (setq pieces (cdr bar) waste (car bar) used (- stock waste)
          util (* 100.0 (/ used stock)))
    ;; ИСПРАВЛЕНО (Р2.1): разделитель " " для консоли
    (princ (strcat "\nХлыст " (itoa i) ": " (n1-list-to-str pieces " ")
                   " | исп. " (rtos used 2 1) " | Отход " (rtos waste 2 1)
                   " | " (rtos util 2 1) "%"))
  )
  (princ)
)

;; ============================================================
;; XLS-экспорт
;; ИСПРАВЛЕНО (Р2.1): параметр разделителя
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

      (write-line "  <Style ss:ID=\"Bold\">" f)
      (write-line "   <Font ss:Bold=\"1\" ss:Underline=\"Single\"/>" f)
      (write-line "   <Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)

      (write-line "  <Style ss:ID=\"Label\">" f)
      (write-line "   <Font ss:Bold=\"1\"/>" f)
      (write-line "   <Alignment ss:Vertical=\"Center\"/>" f)
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

      ;; ЛИСТ — Раскрой
      (write-line " <Worksheet ss:Name=\"Раскрой\">" f)
      (write-line "  <Table>" f)

      (write-line "   <Column ss:Index=\"1\" ss:AutoFitWidth=\"0\" ss:Width=\"60\"/>" f)
      (write-line "   <Column ss:Index=\"2\" ss:AutoFitWidth=\"1\" ss:Width=\"200\"/>" f)
      (write-line "   <Column ss:Index=\"3\" ss:AutoFitWidth=\"0\" ss:Width=\"120\"/>" f)
      (write-line "   <Column ss:Index=\"4\" ss:AutoFitWidth=\"0\" ss:Width=\"100\"/>" f)
      (write-line "   <Column ss:Index=\"5\" ss:AutoFitWidth=\"0\" ss:Width=\"120\"/>" f)

      ;; Заголовок
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
      ;; ИСПРАВЛЕНО (Р2.1): разделитель " " для XLS
      (setq i 0)
      (foreach bar bars
        (setq i (1+ i))
        (setq pieces (cdr bar) waste (car bar) used (- stock waste)
              util (* 100.0 (/ used stock)))
        (write-line "   <Row>" f)
        (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (itoa i) "</Data></Cell>") f)
        (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"String\">" (n1-list-to-str pieces " ") "</Data></Cell>") f)
        (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (rtos used 2 1) "</Data></Cell>") f)
        (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (rtos waste 2 1) "</Data></Cell>") f)
        (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (rtos util 2 1) "</Data></Cell>") f)
        (write-line "   </Row>" f)
      )

      ;; Пустая строка
      (write-line "   <Row>" f)
      (write-line "    <Cell><Data ss:Type=\"String\"></Data></Cell>" f)
      (write-line "   </Row>" f)

      ;; Блок отчёта (столбцы B-D)
      (write-line "   <Row>" f)
      (write-line "    <Cell ss:Index=\"2\" ss:StyleID=\"ReportTitle\" ss:MergeAcross=\"2\"><Data ss:Type=\"String\">ОТЧЁТ</Data></Cell>" f)
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

      ;; Секция неразмещённых (столбцы B-D)
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
;; ИСПРАВЛЕНО (Р2.1): разделитель ";" для CSV
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
                ;; ИСПРАВЛЕНО (Р2.1): пробел как разделитель деталей,
        ;; ИСПРАВЛЕНО: запятая как разделитель деталей внутри колонки.
        ;; Без кавычек. Корректно обрабатывается Excel при разделителе ";"
        (write-line (strcat (itoa i) ";" (n1-list-to-str pieces ", ") ";"
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
(defun cutline-main (layers-from-caller / ss tol stock kerf insPt
                       pieces pieces-ok pieces-oversized split
                       sorted bars
                       bbox1 bbox2 bbox3 bbox p1 p2 color-map
                       barHeight sumInsPt num-bars stock-total-mm
                       total-cnt total-product-mm kpd rec blockName baseName
                       lastEnt ssNew ent oldEcho doc uMark
                       layers layers-str total-input type-counts
                       user-filter line-cnt mline-cnt dynblock-cnt dynblock-type
                       export-xls export-acad
                       default-xls default-acad
                       default-stock default-kerf
                       dialog-result r xls-ok)

  (princ "\n=== Линейный раскрой мерного материала ===")

  ;; 1. Слои
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

  ;; ОБНОВЛЕНО: передаём T для отображения суффикса
  ;; "(за исключением слоя 0)"
  (princ (strcat "\n" (car (n1-layer-display-list layers))))

  ;; 2. Выбор объектов
  (princ "\nВыберите объекты — исходные детали:")
  (princ "\n(принимаются LINE, MLINE и динамические блоки с длиной)")
  (princ "\n(если объекты уже выделены — Enter)")

  (setq ss (su-select-cutline-objects layers))

  (if (null ss)
    (progn (princ "\nНичего не выбрано.") (princ) (exit)))

  (setq total-input (sslength ss))
  (princ (strcat "\nВыбрано объектов: " (itoa total-input)))

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

  ;; 2б. Диалог
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
     (princ "\nРаскрой отменён.")
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
        dynblock-type (nth 6 dialog-result))

  (setq *CUTLINE-LAST-STOCK* stock
        *CUTLINE-LAST-KERF*  kerf
        *CUTLINE-LAST-XLS*   export-xls
        *CUTLINE-LAST-ACAD*  export-acad)

  (princ "\nПараметры приняты из окна диалога.")

  ;; 2в. Фильтрация
  (cond
    ((eq user-filter 'LINE)
     (setq ss (n1-filter-ss-by-type ss "LINE"))
     (princ "\nОставлены только линии (LINE)."))
    ((eq user-filter 'MLINE)
     (setq ss (n1-filter-ss-by-type ss "MLINE"))
     (princ "\nОставлены только мультилинии (MLINE)."))
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

  ;; ОБНОВЛЕНО: показываем дробную часть для реза
  (princ (strcat "\nПараметры: допуск " (rtos tol 2 2)
                 " мм, хлыст " (rtos stock 2 0)
                 " мм, рез " (rtos kerf 2 2) " мм."))
  (princ (strcat "\nЭкспорт: "
                 (if export-xls  ".xls" "без .xls") ", "
                 (if export-acad "таблица AutoCAD" "без таблицы")))

  ;; 3. Извлечение
  (setq pieces (n1-extract-pieces ss tol
                                  *CUTLINE-MIN-LENGTH* *CUTLINE-MAX-LENGTH*))
  (if (null pieces)
    (progn (princ "\nНе удалось извлечь длины.") (princ) (exit)))

  (setq total-cnt 0)
  (foreach rec pieces (setq total-cnt (+ total-cnt (cadr rec))))
  (princ (strcat "\nВсего деталей: " (itoa (length pieces))
                 ", общее количество: " (itoa total-cnt)))

  ;; 4. Разделение
  (setq split (n1-split-by-stock pieces stock))
  (setq pieces-ok (car split) pieces-oversized (cadr split))

  (n1-report-oversized pieces-oversized stock)

  (if (null pieces-ok)
    (progn
      (princ "\nВсе детали превышают длину хлыста. Раскрой невозможен.")
      (princ) (exit)
    )
  )

  ;; 5. Раскрой
  (setq sorted (n1-expand pieces-ok))
  (setq bars (n1-ffd sorted stock kerf))
  (n1-report bars stock kerf)

  ;; 6. Сводка
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

  ;; 7. Экспорт файла
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
    (princ "\nГалочка .xls снята — файл не создаётся.")
  )

  ;; 8. Раскладка AutoCAD
  (if export-acad
    (progn
      (setq insPt (getpoint "\nУкажите точку вставки раскладки: "))
      (if insPt
        (progn
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

          (setq bbox1 (n1-draw-layout bars stock kerf insPt color-map))
          (setq barHeight (/ stock 30.0))
          (setq sumInsPt (list (+ (car (cadr bbox1)) (* barHeight 2.0))
                               (cadr (cadr bbox1))))
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
              (command "._-BLOCK" blockName insPt ssNew "")
              (setvar "CMDECHO" oldEcho)
              (if (tblsearch "BLOCK" blockName)
                (progn (n1-block-insert blockName insPt)
                       (princ (strcat "\nСоздан блок с раскладкой: " blockName)))
                (princ "\nНе удалось создать блок."))
            )
            (princ "\nНет объектов для создания блока.")
          )

          (setq bbox (n1-combine-bbox bbox1
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

(defun c:CUTSHEET ()
  (princ "\nCUTSHEET: модуль в разработке.")
  (princ)
)

(princ "\nCUTLINE.LSP загружен. Команды: CUTLINE, РАСКРОЙХЛЫСТА")
(princ)