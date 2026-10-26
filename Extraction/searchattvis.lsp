;;; ============================================================
;;; Extraction/searchattvis.lsp — поиск по атрибутам и видимостям блоков
;;; Команды: ПОИСКАТР (режим «Атрибуты»), ПОИСКВИД (режим «Видимость блока»),
;;; ПОИСКСПИСОК (без окна: перечень в командной строке)
;;; Окно: Extraction/searchattvis.dcl — одно на обе команды.
;;;
;;; Область: предварительное выделение (если есть) или весь чертёж.
;;; Учитываются вставки INSERT в пространстве модели (анонимные — тоже).
;;; Замороженные, заблокированные и выключенные слои в набор не попадают.
;;; ============================================================
(vl-load-com)

(setq *ae-sv-mode* "all")      ; "all" | "attr" | "vis"
(setq *ae-sv-inserts* nil)      ; записи: (ename имя видимость атрибуты)
(setq *ae-sv-all-inserts* nil)  ; записи всего чертежа для «Выбрать подобное»
(setq *ae-sv-scope-pre* nil)    ; T — окно открыто по предварительно выбранному образцу
(setq *ae-sv-rows* nil)         ; строки списка: (текст . имена вставок)
(setq *ae-sv-scope-text* "")    ; текст области для строки статуса
(setq *ae-sv-result* nil)       ; (набор строк выбрано пропущено) после «Выбрать»
(setq *ae-sv-entries* nil)     ; записи окна: (блок лейбл en)
(setq *ae-sv-rows1* nil)       ; список блоков: (блок . вставки)
(setq *ae-sv-rows2* nil)       ; список атрибутов/видимостей: (лейбл . вставки)

;; ============================================================
;; Строки
;; ============================================================

(defun ae-sv-str (x)
  (if (= (type x) 'STR) x "")
)

(defun ae-sv-trim (s)
  (vl-string-trim (strcat " " (chr 9)) (ae-sv-str s))
)

(defun ae-sv-nz (s)
  (if (= s "") "?" s)
)

(defun ae-sv-has-mask-p (s)
  (or (vl-string-search "*" s)
      (vl-string-search "?" s)
      (vl-string-search "#" s)
      (vl-string-search "@" s))
)

(defun ae-sv-split (s / out pos)
  (setq out '())
  (while (setq pos (vl-string-search "," s))
    (setq out (cons (substr s 1 pos) out))
    (setq s (substr s (+ pos 2))))
  (reverse (cons s out))
)

(defun ae-sv-join (lst / s)
  (setq s "")
  (foreach x lst
    (setq s (if (= s "") x (strcat s "," x))))
  s
)

;; Запрос -> шаблон wcmatch. Пусто -> "" (условия нет).
;; Часть без масок оборачивается в *часть*; запятая — ИЛИ.
(defun ae-sv-norm (q / out p)
  (setq q (ae-sv-trim q))
  (setq out '())
  (if (/= q "")
    (foreach p (ae-sv-split q)
      (setq p (ae-sv-trim p))
      (if (/= p "")
        (setq out (cons (if (ae-sv-has-mask-p p) p (strcat "*" p "*")) out)))))
  (ae-sv-join (reverse out))
)

;; name — строка; pat — уже нормализованный шаблон ("" = любое значение)
(defun ae-sv-match (name pat)
  (if (= pat "")
    T
    (wcmatch (strcase (ae-sv-str name)) (strcase pat)))
)

;; Есть ли среди атрибутов (тег . значение) хотя бы одно значение, подходящее под pat
(defun ae-sv-any-attr-p (attrs pat / found)
  (setq found nil)
  (foreach a attrs
    (if (and (not found) (ae-sv-match (cdr a) pat))
      (setq found T)))
  found
)

(defun ae-sv-unique-ens (lst / seen out)
  (setq seen (ssadd) out '())
  (foreach en lst
    (if (not (ssmemb en seen))
      (progn
        (ssadd en seen)
        (setq out (cons en out)))))
  (reverse out)
)

(defun ae-sv-unique (lst / out)
  (setq out '())
  (foreach x lst
    (if (not (member x out)) (setq out (cons x out))))
  (reverse out)
)

;; ============================================================
;; Выборка вставок и слоёв
;; ============================================================

(defun ae-sv-model-insert-p (en / ed)
  (setq ed (entget en))
  (and (= (cdr (assoc 0 ed)) "INSERT")
       (not (= 1 (cdr (assoc 67 ed)))))
)

(defun ae-sv-model-inserts (ss / i en out)
  (setq out '())
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq en (ssname ss i))
        (if (ae-sv-model-insert-p en) (setq out (cons en out)))
        (setq i (1+ i)))))
  (reverse out)
)

;; Замороженный, заблокированный или выключенный слой (по имени) — вставка не выбирается
(defun ae-sv-layer-name-skip-p (lname / ld flags color)
  (setq ld (if (and (= (type lname) 'STR) (/= lname "")) (tblsearch "LAYER" lname) nil))
  (if (null ld)
    nil
    (progn
      (setq flags (cdr (assoc 70 ld)))
      (setq color (cdr (assoc 62 ld)))
      (if (not (numberp flags)) (setq flags 0))
      (or (= 1 (logand flags 1))
          (= 4 (logand flags 4))
          (and (numberp color) (< color 0)))))
)

;; Вставка на замороженном, заблокированном или выключенном слое
(defun ae-sv-layer-skip-p (en)
  (ae-sv-layer-name-skip-p (cdr (assoc 8 (entget en))))
)

;; Атрибуты вставки: список (тег . значение), пустые теги отброшены
(defun ae-sv-read-attrs (vla / attrs out a tag val)
  (setq out '())
  (setq attrs (vl-catch-all-apply 'vlax-invoke (list vla 'GetAttributes)))
  (if (and (not (vl-catch-all-error-p attrs)) (listp attrs))
    (foreach a attrs
      (setq tag (vl-catch-all-apply 'vla-get-TagString (list a)))
      (setq val (vl-catch-all-apply 'vla-get-TextString (list a)))
      (if (and (not (vl-catch-all-error-p tag)) (/= (ae-sv-trim tag) ""))
        (setq out (cons (cons (ae-sv-trim tag) (ae-sv-trim val)) out)))))
  (reverse out)
)

;; Имя блока (эффективное для динамического блока). Своя копия логики
;; из common/select-utils.lsp: модуль не должен зависеть от загрузки общих модулей.
(defun ae-sv-effname (vla / r)
  (setq r (vl-catch-all-apply 'vla-get-EffectiveName (list vla)))
  (if (vl-catch-all-error-p r)
    (progn
      (setq r (vl-catch-all-apply 'vla-get-Name (list vla)))
      (if (vl-catch-all-error-p r) "" (ae-sv-str r)))
    (ae-sv-str r))
)

;; Значение параметра видимости динамического блока (строка) или nil
(defun ae-sv-vis-name-p (pname)
  (or (vl-string-search "VISIBILITY" (strcase pname))
      (vl-string-search "ВИДИМОСТЬ" (strcase pname)))
)

;; Значение динамического свойства строкой
(defun ae-sv-prop-str (val / r)
  (setq r val)
  (if (= (type val) 'VARIANT)
    (setq r (vl-catch-all-apply 'vlax-variant-value (list val))))
  (if (or (null r) (vl-catch-all-error-p r))
    ""
    (progn
      (setq r (vl-catch-all-apply 'vl-princ-to-string (list r)))
      (if (vl-catch-all-error-p r) "" (ae-sv-trim r))))
)

;; Динамические свойства блока: (значения параметров видимости . ((имя . значение) ...))
(defun ae-sv-dyn-props (vla / props vis others prop pname val)
  (setq vis '() others '())
  (setq props (vl-catch-all-apply 'vlax-invoke (list vla 'GetDynamicBlockProperties)))
  (if (and (not (vl-catch-all-error-p props)) (listp props))
    (foreach prop props
      (setq pname (vl-catch-all-apply 'vla-get-PropertyName (list prop)))
      (setq val (vl-catch-all-apply 'vla-get-Value (list prop)))
      (if (and (not (vl-catch-all-error-p pname))
               (= (type pname) 'STR)
               (not (vl-catch-all-error-p val)))
        (if (ae-sv-vis-name-p pname)
          (setq vis (cons (ae-sv-prop-str val) vis))
          (setq others (cons (cons pname (ae-sv-prop-str val)) others))))))
  (cons (reverse vis) (reverse others))
)

;; Запись на вставку: (ename имя видимость атрибуты значения_видимостей свойства).
;; Поле 3 — первое значение видимости (для старой ПОИСКСПИСОК-выборки).
;; Читается один раз; дальше фильтрация идёт по памяти.
(defun ae-sv-collect (ens / out en vla name dyn)
  (setq out '())
  (foreach en ens
    (setq vla (vl-catch-all-apply 'vlax-ename->vla-object (list en)))
    (if (not (vl-catch-all-error-p vla))
      (progn
        (setq name (ae-sv-effname vla))
        (if (= name "")
          (setq name (ae-sv-str (cdr (assoc 2 (entget en))))))
        (setq dyn (ae-sv-dyn-props vla))
        (setq out (cons (list en name
                              (if (car dyn) (car (car dyn)) "")
                              (ae-sv-read-attrs vla)
                              (car dyn)
                              (cdr dyn)
                              (ae-sv-str (cdr (assoc 8 (entget en)))))
                        out)))))
  (reverse out)
)

;; ============================================================
;; Группировка строк списка
;; items: ((текст ename) ...) -> ((текст имена...) ...), по алфавиту
;; ============================================================

(defun ae-sv-group (items / sorted out cur ens seen it)
  (setq sorted
    (mapcar 'cdr
      (vl-sort (mapcar '(lambda (it) (cons (strcase (car it)) it)) items)
               '(lambda (a b) (< (car a) (car b))))))
  (setq out '() cur nil ens '() seen nil)
  (foreach it sorted
    (if (and cur (= (car it) cur))
      (if (not (ssmemb (cadr it) seen))
        (progn
          (ssadd (cadr it) seen)
          (setq ens (cons (cadr it) ens))))
      (progn
        (if cur (setq out (cons (cons cur (reverse ens)) out)))
        (setq cur (car it) ens (list (cadr it)) seen (ssadd (cadr it))))))
  (if cur (setq out (cons (cons cur (reverse ens)) out)))
  (reverse out)
)

(defun ae-sv-attr-label (tag val blk)
  (strcat tag " = " (if (= val "") "(пусто)" val) " | " (ae-sv-nz blk))
)

;; Режим «Атрибуты»: строка на (тег, значение, блок)
(defun ae-sv-rows-attr (qtag qval / items rec a)
  (setq items '())
  (foreach rec *ae-sv-inserts*
    (foreach a (nth 3 rec)
      (if (and (ae-sv-match (car a) qtag) (ae-sv-match (cdr a) qval))
        (setq items
          (cons (list (ae-sv-attr-label (car a) (cdr a) (nth 1 rec)) (car rec))
                items)))))
  (ae-sv-group items)
)

;; Режим «Видимость блока»: строка на (блок, видимость).
;; Значение — условие на атрибуты той же вставки (любой тег).
(defun ae-sv-rows-vis (qvis qval / items rec)
  (setq items '())
  (foreach rec *ae-sv-inserts*
    (if (and (nth 2 rec)
             (/= (nth 2 rec) "")
             (ae-sv-match (nth 2 rec) qvis)
             (or (= qval "") (ae-sv-any-attr-p (nth 3 rec) qval)))
      (setq items
        (cons (list (strcat (ae-sv-nz (nth 1 rec)) " | " (nth 2 rec)) (car rec))
              items))))
  (ae-sv-group items)
)

;; ============================================================
;; Окно: обновление списка и статуса
;; ============================================================

;; Подсказки внутри полей (как в панелях «Блоки» и «Слои»)
(defun ae-sv-hints ()
  (cond
    ((= *ae-sv-mode* "attr")
     (list "Блок (по Enter):" "Атрибут (по Enter):" "Значение (по Enter):"))
    ((= *ae-sv-mode* "vis")
     (list "Блок (по Enter):" "Видимость (по Enter):" "Значение (по Enter):"))
    (T
     (list "Блок (по Enter):" "" "Значение (по Enter):")))
)

(defun ae-sv-set-hints (/ h)
  (setq h (ae-sv-hints))
  (if (= (ae-sv-trim (ae-sv-clean (ae-sv-str (get_tile "edt_name")) (car h))) "")
    (set_tile "edt_name" (car h)))
  (set_tile "edt_query" (cadr h))
  (set_tile "edt_value" (caddr h))
  (mode_tile "edt_query" (if (= *ae-sv-mode* "all") 1 0))
  (mode_tile "edt_value" 0)
)

;; Радиокнопки режима: отмечена только текущая
(defun ae-sv-set-radios ()
  (set_tile "rb_mode_all" (if (= *ae-sv-mode* "all") "1" "0"))
  (set_tile "rb_mode_attr" (if (= *ae-sv-mode* "attr") "1" "0"))
  (set_tile "rb_mode_vis" (if (= *ae-sv-mode* "vis") "1" "0"))
)

;; Текст поля без подсказки: подсказка вырезается из любой позиции,
;; набранный до неё или после неё текст сохраняется
(defun ae-sv-clean (v hint / p)
  (cond
    ((= hint "") v)
    ((= v hint) "")
    ((setq p (vl-string-search hint v))
     (strcat (substr v 1 p) (substr v (+ p (strlen hint) 1))))
    (T v))
)

;; Значение поля с подсказкой; пустое поле снова показывает подсказку
(defun ae-sv-field (key hint / raw clean)
  (setq raw (ae-sv-str (get_tile key)))
  (setq clean (ae-sv-clean raw hint))
  (if (or (/= clean raw) (= clean ""))
    (set_tile key (if (= clean "") hint clean)))
  clean
)

;; Вывод найденного в командную строку (после закрытия окна)
(defun ae-sv-print-results (/ ens)
  (if (null *ae-sv-rows*)
    (princ "\nНичего не найдено.")
    (progn
      (setq ens (ae-sv-unique (apply 'append (mapcar 'cdr *ae-sv-rows*))))
      (princ (strcat "\nНайдено: " (itoa (length *ae-sv-rows*)) " строк, "
                     (itoa (length ens)) " вставок. Область: " *ae-sv-scope-text*))
      (foreach r *ae-sv-rows*
        (princ (strcat "\n  " (car r) " | " (itoa (length (cdr r))) " шт.")))))
  (princ)
)

;; Записи окна: (блок лейбл en)
;; Режим «Атрибуты»: лейбл — атрибут или «атрибут | значение» (если значение введено)
(defun ae-sv-attr-entries (rec qtag qval / out a)
  (setq out '())
  (foreach a (nth 3 rec)
    (if (and (ae-sv-match (car a) qtag) (ae-sv-match (cdr a) qval))
      (setq out (cons (list (nth 1 rec)
                            (if (= qval "")
                              (car a)
                              (strcat (car a) " | " (if (= (cdr a) "") "(пусто)" (cdr a))))
                            (car rec))
                      out))))
  out
)

;; Режим «Видимость»: поле «Видимость» — значения параметров видимости (любой параметр),
;; поле «Значение» — значения остальных динамических свойств (любое свойство).
;; Лейбл — состояние или «состояние | значение (имя свойства)» (если значение введено).
(defun ae-sv-vis-entries (rec qvis qval / out states dvals s d)
  (setq out '())
  (setq states (vl-remove-if-not '(lambda (v) (ae-sv-match v qvis)) (nth 4 rec)))
  (setq dvals
    (if (= qval "")
      '()
      (vl-remove-if-not '(lambda (o) (ae-sv-match (cdr o) qval)) (nth 5 rec))))
  (if (and states (or (= qval "") dvals))
    (if (= qval "")
      (foreach s states
        (setq out (cons (list (nth 1 rec) s (car rec)) out)))
      (foreach s states
        (foreach d dvals
          (setq out (cons (list (nth 1 rec)
                                (strcat s " | " (cdr d) " (" (car d) ")")
                                (car rec))
                          out))))))
  out
)

(defun ae-sv-len-match (rec qval / hit a v)
  ;; «Все блоки»: значение сравнивается только со свойством или атрибутом «Длина»
  (setq hit nil)
  (setq v (ae-sv-prop-get (ae-sv-prop-up rec) "Длина"))
  (if (and v (ae-sv-match v qval)) (setq hit T))
  (foreach a (nth 3 rec)
    (if (and (not hit)
             (= (strcase (ae-sv-trim (car a))) (strcase "Длина"))
             (ae-sv-match (cdr a) qval))
      (setq hit T)))
  hit
)

(defun ae-sv-all-entries (rec qval / name)
  ;; «Все блоки»: одна строка на вставку; пункты (атрибуты, видимости) не выводятся
  (setq name (nth 1 rec))
  (if (or (= qval "") (ae-sv-len-match rec qval))
    (list (list name name (car rec)))
    '()))

(defun ae-sv-entries-from (recs qn qa qb / acc rec part)
  (setq acc '())
  (foreach rec recs
    (if (ae-sv-match (nth 1 rec) qn)
      (progn
        (setq part
          (cond
            ((= *ae-sv-mode* "all") (ae-sv-all-entries rec qb))
            ((= *ae-sv-mode* "attr") (ae-sv-attr-entries rec qa qb))
            (T (ae-sv-vis-entries rec qa qb))))
        (setq acc (append (reverse part) acc)))))
  (reverse acc)
)

(defun ae-sv-entries (qn qa qb)
  (ae-sv-entries-from *ae-sv-inserts* qn qa qb)
)

;; Список 1 (блоки): если выбраны пункты списка 2 — только блоки с ними
(defun ae-sv-list1 (entries sel-i / items)
  (setq items
    (vl-remove-if-not
      '(lambda (e) (or (null sel-i) (member (cadr e) sel-i)))
      entries))
  (ae-sv-group (mapcar '(lambda (e) (list (car e) (caddr e))) items))
)

;; Список 2 (атрибуты или видимости): если выбраны блоки — только их
(defun ae-sv-list2 (entries sel-b / items)
  (setq items
    (vl-remove-if-not
      '(lambda (e) (or (null sel-b) (member (car e) sel-b)))
      entries))
  (ae-sv-group (mapcar '(lambda (e) (list (cadr e) (caddr e))) items))
)

;; Итог «Выбрать»: вставки, которые проходят и по выбранным блокам, и по выбранным пунктам
(defun ae-sv-final (entries sel-b sel-i / ens seen)
  (setq ens '() seen (ssadd))
  (foreach e entries
    (if (and (or (null sel-b) (member (car e) sel-b))
             (or (null sel-i) (member (cadr e) sel-i))
             (not (ssmemb (caddr e) seen)))
      (progn
        (ssadd (caddr e) seen)
        (setq ens (cons (caddr e) ens)))))
  ens
)

;; Индексы выбранных строк из строки list_box («0 2 3»)
(defun ae-sv-indices (s / r)
  (if (and (= (type s) 'STR) (/= s ""))
    (progn
      (setq r (vl-catch-all-apply 'read (list (strcat "(" s ")"))))
      (if (and (not (vl-catch-all-error-p r)) (listp r))
        (vl-remove-if-not 'numberp r)
        nil))
    nil)
)

;; Тексты выбранных пунктов списка (без счётчика)
(defun ae-sv-selected (key rows / out)
  (setq out '())
  (foreach i (ae-sv-indices (get_tile key))
    (if (and (>= i 0) (< i (length rows)))
      (setq out (cons (car (nth i rows)) out))))
  out
)

;; Выбор, который остался в обновлённом списке
(defun ae-sv-keep (sel rows)
  (vl-remove-if-not '(lambda (s) (assoc s rows)) sel)
)

;; Имя атрибута или видимости из строки второго списка (до « | »)
(defun ae-sv-label-base (s / p)
  (setq p (vl-string-search " | " s))
  (if p (substr s 1 p) s)
)

;; Перенос выбора второго списка при вводе или очистке поля «Значение»:
;; «МАРКА» <-> «МАРКА | М1», «Ригель» <-> «Ригель | 1500 (Длина)»
(defun ae-sv-adapt-sel-i (sel-i entries / labels out s base e)
  (if (null sel-i)
    nil
    (progn
      (setq labels (mapcar 'cadr entries) out '())
      (foreach s sel-i
        (if (member s labels)
          (if (not (member s out)) (setq out (cons s out)))
          (progn
            (setq base (ae-sv-label-base s))
            (foreach e entries
              (if (and (= (ae-sv-label-base (cadr e)) base)
                       (not (member (cadr e) out)))
                (setq out (cons (cadr e) out)))))))
      (reverse out)))
)

(defun ae-sv-join-sp (lst / s)
  (setq s "")
  (foreach x lst
    (setq s (if (= s "") x (strcat s " " x))))
  s
)

;; Заполнить список строками «пункт (N)»; выбор восстанавливается по тексту пункта.
;; Список перерисовывается, только если его строки изменились (меньше мигания при обновлении).
(setq *ae-sv-entries-key* nil *ae-sv-entries-cache* nil)  ; кэш записей окна: (режим запрос1 запрос2)
(setq *ae-sv-shown* nil)         ; строки, показанные в списках: ((ключ . строки) ...)

(defun ae-sv-fill (key rows sel / i idx texts txt)
  (setq texts (mapcar '(lambda (r) (strcat (car r) " (" (itoa (length (cdr r))) ")")) rows))
  (if (not (equal texts (cdr (assoc key *ae-sv-shown*))))
    (progn
      (start_list key)
      (foreach txt texts (add_list txt))
      (end_list)
      (setq *ae-sv-shown*
        (cons (cons key texts)
              (vl-remove-if '(lambda (x) (= (car x) key)) *ae-sv-shown*)))))
  (setq idx '() i 0)
  (foreach r rows
    (if (member (car r) sel) (setq idx (cons (itoa i) idx)))
    (setq i (1+ i)))
  (set_tile key (ae-sv-join-sp (reverse idx)))
)

;; Обновление обоих списков и статуса. Каждый список сужает другой.
;; Записи окна по режиму и запросам — один раз; выбор строк их не меняет
(defun ae-sv-entries-cached (qn qa qb / key)
  (setq key (list *ae-sv-mode* qn qa qb))
  (if (equal key *ae-sv-entries-key*)
    *ae-sv-entries-cache*
    (progn
      (setq *ae-sv-entries-cache* (ae-sv-entries qn qa qb))
      (setq *ae-sv-entries-key* key)
      *ae-sv-entries-cache*)))

(defun ae-sv-refresh (/ h qn qa qb sel-b sel-i entries rows1 rows2)
  (setq h (ae-sv-hints))
  (setq qn (ae-sv-norm (ae-sv-field "edt_name" (car h))))
  (setq qa (ae-sv-norm (ae-sv-field "edt_query" (cadr h))))
  (setq qb (ae-sv-norm (ae-sv-field "edt_value" (caddr h))))
  (if *ae-sv-pend*
    (progn
      (setq sel-b (car *ae-sv-pend*) sel-i (cadr *ae-sv-pend*))
      (setq *ae-sv-pend* nil))
    (progn
      (setq sel-b (ae-sv-selected "lst_blocks" *ae-sv-rows1*))
      (setq sel-i (ae-sv-selected "lst_items" *ae-sv-rows2*))))
  (setq entries (ae-sv-entries-cached qn qa qb))
  (setq sel-i (ae-sv-adapt-sel-i sel-i entries))
  (setq rows1 (ae-sv-list1 entries sel-i))
  (setq rows2 (if (= *ae-sv-mode* "all") nil (ae-sv-list2 entries sel-b)))
  (setq sel-b (ae-sv-keep sel-b rows1))
  (setq sel-i (ae-sv-keep sel-i rows2))
  (setq *ae-sv-entries* entries)
  (setq *ae-sv-rows1* rows1)
  (setq *ae-sv-rows2* rows2)
  (set_tile "txt_items_label"
    (cond ((= *ae-sv-mode* "attr") "Атрибуты")
          ((= *ae-sv-mode* "vis") "Видимости")
          (T "")))
  (ae-sv-fill "lst_blocks" rows1 sel-b)
  (ae-sv-fill "lst_items" rows2 sel-i)
  (set_tile "txt_status"
    (if (or sel-b sel-i)
      (ae-sv-status-text sel-b sel-i rows1 rows2)
      (strcat "Блоков: " (itoa (length rows1))
              (cond ((= *ae-sv-mode* "attr") ", атрибутов: " )
                    ((= *ae-sv-mode* "vis") ", видимостей: ")
                    (T ", "))
              (if (= *ae-sv-mode* "all") "" (itoa (length rows2)))
              (if (= *ae-sv-mode* "all") "" ", ")
              "вставок: "
              (itoa (length (ae-sv-unique-ens (mapcar 'caddr entries))))
              ". Область: " *ae-sv-scope-text*)))
  T
)

;; Вывод в командную строку после закрытия окна: оба списка
(defun ae-sv-print-lists ()
  (if (null *ae-sv-entries*)
    (princ "\nНичего не найдено.")
    (progn
      (princ (strcat "\nБлоки (" (itoa (length *ae-sv-rows1*)) "):"))
      (foreach r *ae-sv-rows1*
        (princ (strcat "\n  " (car r) " | " (itoa (length (cdr r))) " шт.")))
      (if (/= *ae-sv-mode* "all")
        (progn
          (princ (strcat "\n" (if (= *ae-sv-mode* "attr") "Атрибуты" "Видимости")
                         " (" (itoa (length *ae-sv-rows2*)) "):"))
          (foreach r *ae-sv-rows2*
            (princ (strcat "\n  " (car r) " | " (itoa (length (cdr r))) " шт.")))))
      (princ (strcat "\nВставок: " (itoa (length (ae-sv-unique-ens (mapcar 'caddr *ae-sv-entries*))))
                     ". Область: " *ae-sv-scope-text*))))
  (princ)
)

(defun ae-sv-on-mode (m)
  ;; смена режима очищает «Видимость» и «Значение»; «Блок» сохраняется
  (setq *ae-sv-mode* m)
  (ae-sv-set-radios)
  (set_tile "edt_query" "")
  (set_tile "edt_value" "")
  (setq *ae-sv-rows1* nil *ae-sv-rows2* nil)
  (ae-sv-set-hints)
  (ae-sv-refresh)
)

;; Завершение отбора вставок: фильтр слоёв и закрытие окна с выделением
(defun ae-sv-finish-pick (ens / ss sel-n skipped en)
  (if (null ens)
    (set_tile "txt_status" "По выбранным строкам вставок нет.")
    (progn
      (setq ss (ssadd))
      (setq sel-n 0 skipped 0)
      (foreach en ens
        (if (ae-sv-layer-skip-p en)
          (setq skipped (1+ skipped))
          (progn
            (ssadd en ss)
            (setq sel-n (1+ sel-n)))))
      (if (= sel-n 0)
        (set_tile "txt_status"
          "Все вставки выбранных строк на замороженных, заблокированных или выключенных слоях.")
        (progn
          (setq *ae-sv-result* (list ss sel-n skipped))
          (done_dialog 1)))))
)

;; Кнопка «Выбрать»: отмечены блоки и/или пункты второго списка
(defun ae-sv-on-pick (/ sel-b sel-i ens)
  (ae-sv-refresh)
  (setq sel-b (ae-sv-selected "lst_blocks" *ae-sv-rows1*))
  (setq sel-i (ae-sv-selected "lst_items" *ae-sv-rows2*))
  (if (and (null sel-b) (null sel-i))
    (set_tile "txt_status" "Не выбрано ни одной строки. Отметьте блоки или пункты второго списка.")
    (progn
      (setq ens (ae-sv-final *ae-sv-entries* sel-b sel-i))
      (ae-sv-finish-pick ens)))
)

;; Все вставки пространства модели (для «Выбрать подобное» — поиск всегда по всему чертежу)
(defun ae-sv-all-records ()
  (if *ae-sv-all-inserts*
    *ae-sv-all-inserts*
    (setq *ae-sv-all-inserts* (ae-sv-collect (ae-sv-all-model-inserts))))
)

;; Целевой выбор (sel-b sel-i) для «Выбрать подобное»: если строки в списках не отмечены,
;; берутся строки, отобранные полями ввода или образцом
(defun ae-sv-similar-targets (qn qa qb sel-b sel-i rows1 rows2 scope-pre / b i)
  (setq b sel-b i sel-i)
  (if (and (null b) (null i))
    (cond
      ((= *ae-sv-mode* "all")
       (if (or (/= qn "") (/= qb "") scope-pre)
         (setq b (mapcar 'car rows1))))
      ((or (/= qa "") (/= qb ""))
       (setq i (mapcar 'car rows2))
       (if (/= qn "") (setq b (mapcar 'car rows1))))
      ((/= qn "")
       (setq b (mapcar 'car rows1)))
      (scope-pre
       (setq i (mapcar 'car rows2))
       (if (null i) (setq b (mapcar 'car rows1)))))
    (if (and b (null i) (/= *ae-sv-mode* "all") (or (/= qa "") (/= qb "")))
      (setq i (mapcar 'car rows2))))
  (list b i)
)

;; Поиск подобных вставок по записям всего чертежа:
;; по Блоку, Видимости или Атрибуту (без Значения или со Значением, если оно введено)
(defun ae-sv-similar-ens (all-recs qn qa qb sel-b sel-i / ens seen r all-entries)
  (if (and sel-b (null sel-i) (= qa "") (= qb ""))
    (progn
      (setq ens '() seen (ssadd))
      (foreach r all-recs
        (if (and (ae-sv-match (nth 1 r) qn)
                 (member (nth 1 r) sel-b)
                 (not (ssmemb (car r) seen)))
          (progn
            (ssadd (car r) seen)
            (setq ens (cons (car r) ens)))))
      (reverse ens))
    (progn
      (setq all-entries (ae-sv-entries-from all-recs qn qa qb))
      (reverse (ae-sv-final all-entries sel-b sel-i))))
)

;; Кнопка «Выбрать подобное»: выбор аналогичных элементов по всему чертежу
(defun ae-sv-on-similar (/ h qn qa qb sel-b sel-i tg ens)
  (ae-sv-refresh)
  (setq h (ae-sv-hints))
  (setq qn (ae-sv-norm (ae-sv-field "edt_name" (car h))))
  (setq qa (ae-sv-norm (ae-sv-field "edt_query" (cadr h))))
  (setq qb (ae-sv-norm (ae-sv-field "edt_value" (caddr h))))
  (setq sel-b (ae-sv-selected "lst_blocks" *ae-sv-rows1*))
  (setq sel-i (ae-sv-selected "lst_items" *ae-sv-rows2*))
  (setq tg (ae-sv-similar-targets qn qa qb sel-b sel-i *ae-sv-rows1* *ae-sv-rows2* *ae-sv-scope-pre*))
  (setq sel-b (car tg) sel-i (cadr tg))
  (if (and (null sel-b) (null sel-i))
    (if (or (/= qn "") (/= qa "") (/= qb ""))
      (set_tile "txt_status" "По выбранным строкам вставок нет.")
      (set_tile "txt_status" "Не выбрано ни одной строки. Отметьте блоки или пункты второго списка."))
    (progn
      (setq ens (ae-sv-similar-ens (ae-sv-all-records) qn qa qb sel-b sel-i))
      (ae-sv-finish-pick ens)))
)

(defun ae-sv-init (/ r)
  (setq *ae-sv-shown* nil)
  (ae-sv-set-radios)
  (set_tile "edt_name" "")
  (ae-sv-set-hints)
  (if *ae-sv-restore*
    (progn
      (setq r *ae-sv-restore*)
      (if (/= (caddr r) "") (set_tile "edt_name" (caddr r)))
      (if (/= (cadddr r) "") (set_tile "edt_query" (cadddr r)))
      (if (/= (nth 4 r) "") (set_tile "edt_value" (nth 4 r)))
      (setq *ae-sv-pend* (list (car r) (cadr r)))
      (setq *ae-sv-restore* nil)))
  (ae-sv-refresh)
)

;; Сброс выбора в обоих списках (кнопка и Esc); окно не закрывается
(defun ae-sv-reset ()
  (set_tile "lst_blocks" "")
  (set_tile "lst_items" "")
  (ae-sv-refresh)
)

(defun ae-sv-wire ()
  (action_tile "rb_mode_all" "(ae-sv-on-mode \"all\")")
  (action_tile "rb_mode_attr" "(ae-sv-on-mode \"attr\")")
  (action_tile "rb_mode_vis" "(ae-sv-on-mode \"vis\")")
  (action_tile "edt_name" "(ae-sv-refresh)")
  (action_tile "edt_query" "(ae-sv-refresh)")
  (action_tile "edt_value" "(ae-sv-refresh)")
  (action_tile "lst_blocks" "(ae-sv-refresh)")
  (action_tile "lst_items" "(ae-sv-refresh)")
  (action_tile "btn_select" "(ae-sv-on-pick)")
  (action_tile "btn_similar" "(ae-sv-on-similar)")
  (action_tile "btn_count" "(ae-sv-on-count)")
  (action_tile "btn_reset" "(ae-sv-reset)")
  (action_tile "btn_cancel" "(done_dialog 0)")
)

;; Открывает окно. T — диалог отработал, nil — окно не открылось.
(defun ae-sv-dcl-file (/ root dcl-file)
  (setq root (if (= (type ae-settings-root) 'SUBR) (ae-settings-root) nil))
  ;; До регистрации в reload.lsp можно указать папку вручную:
  ;; (setq *ae-sv-dcl-dir* "D:\\autoextraction\\Extraction\\")
  (setq dcl-file nil)
  (if (and (boundp '*ae-sv-dcl-dir*) (= (type *ae-sv-dcl-dir*) 'STR))
    (setq dcl-file (findfile (strcat *ae-sv-dcl-dir* "searchattvis.dcl"))))
  (if (null dcl-file)
    (setq dcl-file (findfile "searchattvis.dcl")))
  (if (null dcl-file)
    (setq dcl-file (findfile "Extraction\\searchattvis.dcl")))
  (if (and (null dcl-file) root)
    (setq dcl-file (findfile (strcat root "\\Extraction\\searchattvis.dcl"))))
  dcl-file
)

(defun ae-sv-open (/ dcl-file dcl-id)
  (setq dcl-file (ae-sv-dcl-file))
  (if (null dcl-file)
    (progn
      (alert "Не найден файл Extraction\\searchattvis.dcl.")
      nil)
    (progn
      (setq dcl-id (load_dialog dcl-file))
      (if (< dcl-id 0)
        (progn
          (alert "Не удалось загрузить окно поиска.")
          nil)
        (if (new_dialog "searchattvis_dialog" dcl-id)
          (progn
            (ae-sv-init)
            (ae-sv-wire)
            (start_dialog)
            (unload_dialog dcl-id)
            (if (equal *ae-sv-next* "count")
              (progn
                (setq *ae-sv-next* nil)
                (ae-sv-count-flow)))
            T)
          (progn
            (unload_dialog dcl-id)
            (alert "Не удалось открыть окно поиска.")
            nil))))))

;; ============================================================
;; Подсчёт: погонаж (м.п.) и площадь (м2) — отдельное окно «Подсчёт»
;; Погонаж — свойство «Длина» (без номера). Площадь — «Высота в свету» и «Ширина в свету»,
;; затем «Высота» и «Ширина» (без номера); если «Ширины» нет, берётся «Длина».
;; Блоки с именем по маскам «Заполнени*», «Стекло*» получают припуск на раму (настройка).
;; Регистр не важен.
;; Размеры округляются до миллиметров; группы — по марке, затем по размерам.
;; ============================================================

(setq *ae-sv-next* nil)         ; "count" — открыть «Подсчёт»; "table" — таблица; "close" — назад
(setq *ae-sv-restore* nil)      ; (блоки пункты запрос1 запрос2) — восстановить после закрытия
(setq *ae-sv-pend* nil)         ; (блоки пункты) — применить при следующем обновлении
(setq *ae-sv-finished* nil)    ; T — таблица или копирование выполнены: все окна закрыты
(setq *ae-sv-count-sel* nil)    ; (блоки пункты) — выбор для окна «Подсчёт»
(setq *ae-sv-cnt-secs* nil)     ; разделы: (заголовок строки)
(setq *ae-sv-cnt-rows* nil)     ; строки списка окна «Подсчёт» (списки полей)
(setq *ae-sv-count-all-title* nil)     ; заголовок «Все блоки: N шт.» для подсчёта без выбора

;; Свойства вставки (ИМЯ-ПРОПИСНЫМИ . значение) — имена очищаются один раз на вставку
(defun ae-sv-prop-up (rec / out)
  (setq out '())
  (foreach p (nth 5 rec)
    (setq out (cons (cons (strcase (ae-sv-trim (car p))) (cdr p)) out)))
  (reverse out)
)

;; Значение свойства по имени (точное совпадение, регистр не важен); первое совпадение
(defun ae-sv-prop-get (up name)
  (cdr (assoc (strcase name) up))
)

;; Значение атрибута «Марка» (без учёта регистра) или ""
(defun ae-sv-marka (rec / hit)
  (setq hit nil)
  (foreach a (nth 3 rec)
    (if (and (null hit) (= (strcase (ae-sv-trim (car a))) (strcase "Марка")))
      (setq hit (cdr a))))
  (if hit (ae-sv-trim hit) "")
)

;; Число из строки или nil
;; Первое число в строке: «1500», «1500.0», «1500,5», «1500 мм» (знак минус в начале)
(defun ae-sv-num-prefix (s / out i a)
  (setq s (ae-sv-trim s) out "" i 1)
  (while (and (<= i (strlen s))
              (progn
                (setq a (ascii (substr s i 1)))
                (or (and (>= a 48) (<= a 57)) (= a 46) (= a 44) (= a 45))))
    (setq out (strcat out (substr s i 1)) i (1+ i)))
  out
)

(defun ae-sv-num (s / p r)
  (if (and (= (type s) 'STR) (/= s ""))
    (progn
      (setq p (ae-sv-num-prefix s))
      (setq r (distof p 2))
      (if (null r) (setq r (distof (vl-string-translate ".," ",." p) 2)))
      r)
    nil)
)

;; Округление до миллиметров; 0, отрицательные и nil — nil
(defun ae-sv-mm (v / r)
  (if v
    (progn
      (setq r (fix (+ v 0.5)))
      (if (> r 0) r nil))
    nil)
)

;; Ключи размеров для площади — цепочка ЗАПОЛНЕНИЯ (zapolnenie.lsp и раскрой): сначала размер «в свету»,
;; затем обычные параметры. Для каждого ключа — точное совпадение имени (регистр не важен),
;; затем вхождение в имени БЕЗ цифр («Высота двери» берётся, «Высота1» — нет).
(setq *ae-sv-height-keys* (quote ("Высота в свету" "Высота")))
(setq *ae-sv-width-keys*  (quote ("Ширина в свету" "Ширина" "Длина")))
(setq *ae-sv-allowance-default* 26)

;; Припуск на раму: настройка task.ZAPOLNENIE / input.frame.allowance, иначе 26
(defun ae-sv-allowance (/ v)
  (setq v (if (= (type ae-settings-frame-allowance) (quote SUBR))
            (ae-settings-frame-allowance)
            nil))
  (if (and (numberp v) (>= v 0)) v *ae-sv-allowance-default*)
)

;; Маски припуска — те же, что у раскроя (cs-fill-masks): task.ZAPOLNENIE / input.block.
;; Пустой список — припуск выключен. Читается один раз на подсчёт (см. ae-sv-calc).
(defun ae-sv-frame-masks (/ m)
  (setq m nil)
  (if (= (type ae-settings-task-blocks) 'SUBR)
    (setq m (ae-settings-task-blocks 'ZAPOLNENIE)))
  (if (and m (listp m)) m nil)
)

;; Имя блока (EffectiveName) или первое значение видимости подходит под одну из масок (регистр не важен)
(defun ae-sv-frame-p (rec masks / hit vals)
  (setq hit nil)
  (if masks
    (progn
      (setq vals (list (nth 1 rec) (nth 2 rec)))
      (foreach v vals
        (if (and (= (type v) 'STR) (/= v ""))
          (foreach m masks
            (if (and (= (type m) 'STR) (wcmatch (strcase v) (strcase m)))
              (setq hit T)))))))
  hit
)

;; В строке есть цифра (маска «#» — одна цифра)
(defun ae-sv-has-digit-p (s / i hit)
  (setq hit nil i 1)
  (while (and (null hit) (<= i (strlen s)))
    (if (wcmatch (substr s i 1) "#") (setq hit T))
    (setq i (1+ i)))
  hit
)

;; Размер по ключу: точное имя, затем вхождение в имени без цифр. up — (имя в верхнем регистре . значение)
(defun ae-sv-dim-key (up k / hit p)
  (setq hit (ae-sv-mm (ae-sv-num (ae-sv-prop-get up k))))
  (if (null hit)
    (foreach p up
      (if (and (null hit)
               (not (ae-sv-has-digit-p (car p)))
               (vl-string-search (strcase k) (car p)))
        (setq hit (ae-sv-mm (ae-sv-num (cdr p)))))))
  hit
)

;; Первый найденный размер по списку ключей (порядок ключей важен), либо nil
(defun ae-sv-dim-of (up keys / hit k)
  (setq hit nil)
  (foreach k keys
    (if (null hit) (setq hit (ae-sv-dim-key up k))))
  hit
)

;; Размер вставки: ("A" высота ширина) — площадь; ("L" длина nil) — погонаж; nil — размера нет.
;; Припуск (allow) добавляется к высоте и ширине блоков с маской раскроя, только для площади.
;; Погонаж — только точное «Длина» (без номера и без подстроки).
(defun ae-sv-size-of (rec allow masks / h w l up)
  (setq up (ae-sv-prop-up rec))
  (setq h (ae-sv-dim-of up *ae-sv-height-keys*)
        w (ae-sv-dim-of up *ae-sv-width-keys*)
        l (ae-sv-mm (ae-sv-num (ae-sv-prop-get up "Длина"))))
  (if (and h (ae-sv-frame-p rec masks))
    (progn
      (setq h (+ h allow))
      (if w (setq w (+ w allow)))))
  (cond
    ((and h w) (list "A" h w))
    (l (list "L" l nil))
    (T nil))
)

;; Группы: ((вид марка размер1 размер2) . количество); nosize — вставки без размера
(defun ae-sv-calc (recs / groups nosize sz key hit allow masks)
  (setq groups '() nosize 0 allow (ae-sv-allowance) masks (ae-sv-frame-masks))
  (foreach r recs
    (setq sz (ae-sv-size-of r allow masks))
    (if (null sz)
      (setq nosize (1+ nosize))
      (progn
        (setq key (list (car sz) (ae-sv-marka r) (cadr sz) (caddr sz)))
        (setq hit (assoc key groups))
        (if hit
          (setq groups (subst (cons key (1+ (cdr hit))) hit groups))
          (setq groups (cons (cons key 1) groups))))))
  (list groups nosize)
)

;; Лексикографическое сравнение строк по кодам символов (AutoLISP сравнивает числа)
(defun ae-sv-str-less (a b / i na nb res ca cb)
  (setq na (strlen a) nb (strlen b) i 1 res nil)
  (while (and (null res) (<= i na) (<= i nb))
    (setq ca (ascii (substr a i 1)) cb (ascii (substr b i 1)))
    (cond
      ((< ca cb) (setq res (quote lt)))
      ((> ca cb) (setq res (quote gt))))
    (setq i (1+ i)))
  (if res (= res (quote lt)) (< na nb))
)

;; Ключ марки для естественного порядка: цифровые участки дополняются нулями,
;; регистр не важен, поэтому «М2» идёт раньше «М10»
(defun ae-sv-pad-digits (run)
  (if (= run "")
    ""
    (strcat (substr "0000000000" 1 (max 0 (- 10 (strlen run)))) run))
)

(defun ae-sv-nat-key (s / out run ch i)
  (setq s (strcase (ae-sv-str s)) out "" run "" i 1)
  (while (<= i (strlen s))
    (setq ch (substr s i 1))
    (if (and (>= (ascii ch) 48) (<= (ascii ch) 57))
      (setq run (strcat run ch))
      (progn
        (setq out (strcat out (ae-sv-pad-digits run) ch))
        (setq run "")))
    (setq i (1+ i)))
  (strcat out (ae-sv-pad-digits run))
)

;; Группы одного вида: по марке (естественный порядок), затем по высоте и ширине
;; (по возрастанию; для погонажа — по длине). Ключ группы: (вид марка размер1 размер2)
(defun ae-sv-sorted (groups kind)
  (vl-sort
    (vl-remove-if-not '(lambda (g) (= (car (car g)) kind)) groups)
    '(lambda (a b / ka kb wa wb)
       (setq ka (ae-sv-nat-key (cadr (car a))) kb (ae-sv-nat-key (cadr (car b))))
       (setq wa (if (cadddr (car a)) (cadddr (car a)) 0)
             wb (if (cadddr (car b)) (cadddr (car b)) 0))
       (cond
         ((not (= ka kb)) (ae-sv-str-less ka kb))
         ((/= (caddr (car a)) (caddr (car b))) (< (caddr (car a)) (caddr (car b))))
         ((/= wa wb) (< wa wb))
         (T (ae-sv-str-less (cadr (car a)) (cadr (car b))))))
  )
)

(defun ae-sv-blanks (n / out)
  (setq out '())
  (repeat n (setq out (cons "" out)))
  out
)

;; Строка «Всего:»: подпись в первой колонке, количество и сумма — в последних двух
(defun ae-sv-total-row (cols cnt sum)
  (append (list "Всего:") (ae-sv-blanks (- (length cols) 3))
          (list (itoa cnt) (rtos sum 2 2)))
)

;; Раздел: (заголовок строки). Строки: заголовок колонок, позиции, «Всего» последней.
;; Колонки: № | [Марка] | Длина, мм | Кол-во | Сумма, м.п.  или  № | [Марка] | Высота | Ширина | Кол-во | Сумма, м2
(defun ae-sv-section (title sorted kind / mk cols rows i cnt sum g n d1 d2 m)
  (setq mk nil cnt 0 sum 0.0 i 0 rows '())
  (foreach g sorted
    (if (/= (cadr (car g)) "") (setq mk T))
    (setq n (cdr g) d1 (caddr (car g)) d2 (cadddr (car g)))
    (setq cnt (+ cnt n))
    (setq sum (+ sum (if (= kind "A") (/ (* d1 d2 n) 1000000.0) (/ (* d1 n) 1000.0)))))
  (setq cols
    (if (= kind "A")
      (if mk
        (list "№" "Марка" "Высота" "Ширина" "Кол-во" "Сумма, м2")
        (list "№" "Высота" "Ширина" "Кол-во" "Сумма, м2"))
      (if mk
        (list "№" "Марка" "Длина, мм" "Кол-во" "Сумма, м.п.")
        (list "№" "Длина, мм" "Кол-во" "Сумма, м.п."))))
  (setq rows (list cols))
  (foreach g sorted
    (setq i (1+ i) m (cadr (car g)) d1 (caddr (car g)) d2 (cadddr (car g)) n (cdr g))
    (setq rows
      (append rows
        (list
          (if (= kind "A")
            (append (list (itoa i)) (if mk (list m) '())
                    (list (itoa d1) (itoa d2) (itoa n) (rtos (/ (* d1 d2 n) 1000000.0) 2 2)))
            (append (list (itoa i)) (if mk (list m) '())
                    (list (itoa d1) (itoa n) (rtos (/ (* d1 n) 1000.0) 2 2))))))))
  (setq rows (append rows (list (ae-sv-total-row cols cnt sum))))
  (list title rows)
)

;; Разделы по данным вставок: сначала погонаж, затем площадь
(defun ae-sv-build-sections (recs / c groups lin area secs)
  (setq c (ae-sv-calc recs) groups (car c) secs '())
  (setq lin (ae-sv-sorted groups "L") area (ae-sv-sorted groups "A"))
  (if lin (setq secs (cons (ae-sv-section "м.п." lin "L") secs)))
  (if area (setq secs (cons (ae-sv-section "м2" area "A") secs)))
  (reverse secs)
)

;; Вставки выбора: пересечение выбранных блоков и пунктов; замороженные/выключенные слои — нет.
;; Слой проверяется один раз на имя слоя (кэш), а не на каждую вставку.
(defun ae-sv-count-recs (sel-b sel-i / ens ss cache out lay)
  (setq ens (if (or sel-b sel-i) (ae-sv-final *ae-sv-entries* sel-b sel-i) nil))
  (setq ss (ssadd))
  (foreach en ens (ssadd en ss))
  (setq cache '() out '())
  (foreach r *ae-sv-inserts*
    (if (ssmemb (car r) ss)
      (progn
        (setq lay (nth 6 r))
        (if (null (assoc lay cache))
          (setq cache (cons (cons lay (ae-sv-layer-name-skip-p lay)) cache)))
        (if (null (cdr (assoc lay cache)))
          (setq out (cons r out))))))
  (reverse out)
)

(defun ae-sv-join-sep (lst sep / s first)
  (setq s "" first T)
  (foreach x lst
    (if first
      (setq s x first nil)
      (setq s (strcat s sep x))))
  s
)

;; Полный текст выбранных строк (с количеством) для строки состояния окна поиска
(defun ae-sv-status-text (sel-b sel-i rows1 rows2 / out)
  (setq out '())
  (foreach s sel-b
    (setq out (cons (strcat s " (" (itoa (length (cdr (assoc s rows1)))) ")") out)))
  (foreach s sel-i
    (setq out (cons (strcat s " (" (itoa (length (cdr (assoc s rows2)))) ")") out)))
  (ae-sv-join-sep (reverse out) "; ")
)

;; Строка таблицы подсчёта: поля через табуляцию (колонки списка lst_count)
(defun ae-sv-row-tab (fields)
  (ae-sv-join-sep fields (chr 9))
)

;; Данные окна «Подсчёт» по выбранным блокам (и пунктам): строки списка и разделы
;; Текст диагностики, если размеров нет: сколько вставок выбрано, сколько отброшено по слоям
;; и какие свойства прочитаны у первой вставки
(defun ae-sv-prop-list-text (props / out n)
  (setq out '() n 0)
  (foreach p props
    (if (< n 8)
      (setq out (cons (strcat (car p) "=" (cdr p)) out) n (1+ n))))
  (if out (ae-sv-join-sep (reverse out) "; ") "нет")
)

(defun ae-sv-nosize-text (sel-b sel-i recs / ens-n skip head)
  (setq ens-n (length (ae-sv-final *ae-sv-entries* sel-b sel-i)))
  (setq skip (- ens-n (length recs)))
  (setq head (if (null sel-b) "У выбранных строк нет погонажа или площади" (if (= (length sel-b) 1)
               (strcat "У блока «" (car sel-b) "» нет погонажа или площади")
               "У выбранных блоков нет погонажа или площади")))
  (strcat head ". Вставок: " (itoa ens-n)
          ", на замороженных, заблокированных или выключенных слоях: " (itoa skip)
          ". Свойства: "
          (if recs (ae-sv-prop-list-text (nth 5 (car recs))) "нет"))
)

;; Раздел окна: (заголовок таблица названия) — названия идут в заголовок таблицы
(defun ae-sv-tag-secs (secs names / out)
  (setq out '())
  (foreach s secs
    (setq out (cons (append s (list names)) out)))
  (reverse out)
)

(defun ae-sv-cnt-data (sel-b sel-i / rows secs recs)
  (setq rows '() secs '())
  (if (and (null sel-b) (null sel-i))
    (setq rows (list (list "Выберите блок в списке слева")))
    (progn
      ;; выбрано две и более видимости — отдельная таблица на каждую видимость
      (if (and (equal *ae-sv-mode* "vis") (> (length sel-i) 1))
        (foreach st sel-i
          (setq secs (append secs
                             (ae-sv-tag-secs (ae-sv-build-sections (ae-sv-count-recs sel-b (list st)))
                                             (list st)))))
        (setq secs (ae-sv-tag-secs (ae-sv-build-sections (ae-sv-count-recs sel-b sel-i)) (ae-sv-title-names sel-b sel-i))))
      (if (null secs)
        (setq rows (list (list (ae-sv-nosize-text sel-b sel-i (ae-sv-count-recs sel-b sel-i)))))
        (foreach sec secs
          (setq rows (append rows
                             (if rows (list (list "")) '())
                             (list (list (ae-sv-block-title (caddr sec))))
                             (cadr sec)))))))
  (setq *ae-sv-cnt-secs* secs)
  (setq *ae-sv-cnt-rows* rows)
)

;; Строка состояния окна «Подсчёт»: итоги разделов — «Всего: N шт. S м.п.» или «... S м2»;
;; при двух разделах — через «; ». Без данных — «Всего: 0 шт.»
(defun ae-sv-totals-text (secs / out tot)
  (if (null secs)
    "Всего: 0 шт."
    (progn
      (setq out "")
      (foreach sec secs
        (setq tot (last (cadr sec)))
        (setq out (strcat out (if (= out "") "Всего: " "; ")
                          (nth (- (length tot) 2) tot) " шт. "
                          (last tot) " "
                          (if (vl-string-search "м.п." (car sec)) "м.п." "м2"))))
      out)))

;; Запомнить выбор и запрос перед закрытием окна поиска
(defun ae-sv-snapshot ()
  ;; (блоки, пункты, имя, видимость/атрибут, значение)
  (list (ae-sv-selected "lst_blocks" *ae-sv-rows1*)
        (ae-sv-selected "lst_items" *ae-sv-rows2*)
        (ae-sv-field "edt_name" (car (ae-sv-hints)))
        (ae-sv-field "edt_query" (cadr (ae-sv-hints)))
        (ae-sv-field "edt_value" (caddr (ae-sv-hints))))
)

(defun ae-sv-title-names (sel-b sel-i)
  ;; заголовок таблицы: «Все блоки: N шт.» или выбранные имена
  (cond (*ae-sv-count-all-title* (list *ae-sv-count-all-title*))
        (sel-b sel-b)
        (T sel-i))
)

;; Кнопка «Подсчёт» в окне поиска: закрыть поиск и открыть окно «Подсчёт»
(defun ae-sv-on-count (/ names total)
  (ae-sv-refresh)
  (setq *ae-sv-count-all-title* nil)
  (setq *ae-sv-restore* (ae-sv-snapshot))
  (setq *ae-sv-count-sel* (list (car *ae-sv-restore*) (cadr *ae-sv-restore*)))
  (setq names (mapcar 'car *ae-sv-rows1*))
  (cond
    ((and (= *ae-sv-mode* "all") (null (car *ae-sv-count-sel*)) (null names))
     (setq *ae-sv-restore* nil)
     (set_tile "txt_status" "Блоков по фильтру нет."))
    ((and (= *ae-sv-mode* "all") (null (car *ae-sv-count-sel*)))
     ;; без выбора в «Все блоки» — все блоки, прошедшие фильтр
     (setq total (length (ae-sv-final *ae-sv-entries* names nil)))
     (setq *ae-sv-count-sel* (list names nil))
     (setq *ae-sv-count-all-title* (strcat "Все блоки: " (itoa total) " шт."))
     (setq *ae-sv-next* "count")
     (done_dialog 4))
    (T
     (setq *ae-sv-next* "count")
     (done_dialog 4)))
)

;; Текст строк для буфера обмена: строки через CRLF, поля через табуляцию
(defun ae-sv-cnt-text (rows / out)
  (setq out "")
  (foreach r rows
    (setq out (strcat out (ae-sv-row-tab r) (chr 13) (chr 10))))
  out
)

;; Запись в буфер обмена через htmlfile (COM, без внешних программ)
(defun ae-sv-clip (txt / ie win)
  (setq ie (vlax-create-object "htmlfile"))
  (setq win (vlax-get ie 'parentWindow))
  (vlax-invoke (vlax-get win 'clipboardData) 'setData "Text" txt)
  T
)

(defun ae-sv-cnt-copy (/ r)
  (if (null *ae-sv-cnt-secs*)
    (princ "\nПодсчёт: нет данных для копирования.")
    (progn
      (setq r (vl-catch-all-apply 'ae-sv-clip (list (ae-sv-cnt-text *ae-sv-cnt-rows*))))
      (if (and (not (vl-catch-all-error-p r)) r)
        (progn
          (princ "\nПодсчёт: скопировано в буфер обмена (вставьте в Excel: Ctrl+V).")
          (setq *ae-sv-next* "done")
          (done_dialog 0))
        (princ "\nПодсчёт: буфер обмена недоступен."))))
  (princ)
)

;; ============================================================
;; Окно «Подсчёт»: таблица с рамкой (вид как в примере пользователя).
;;   площадь: | № | [Марка] | Высота | Ширина | Кол-во | Всего |
;;   погонаж без площади: | № | [Марка] | Длина | Кол-во | Всего | (ключи ln / lm)
;;   смешанный выбор: площадь и погонаж — два отдельных списка (диалоги _xn / _xm)
;; Ширина колонок W, символов: по ним DCL задаёт табуляции и ширину списка
;; (проверка — tests/static_check.py, check_search_count_window). Вариант — по данным.
;; Строки: линия над шапкой, шапка, линия, позиции, линия, «Всего:», линия.
;; ============================================================
(setq *ae-sv-cnt-widths*
  (quote (("n" 3 7 7 6 9)
          ("m" 3 20 7 7 6 9)
          ("ln" 3 7 6 9)
          ("lm" 3 20 7 6 9))))

(defun ae-sv-spaces (n / s)
  (setq s "")
  (repeat (max 0 n) (setq s (strcat s " ")))
  s
)

(defun ae-sv-dashes (n / s)
  (setq s "")
  (repeat (max 0 n) (setq s (strcat s "-")))
  s
)

;; Значение колонки по заголовку (пусто, если колонки нет)
(defun ae-sv-cnt-cell (cols row name / i)
  (setq i (vl-position name cols))
  (if i (nth i row) "")
)

;; Единица раздела по заголовку суммы: м.п. или м2
(defun ae-sv-cnt-unit (cols)
  (if (vl-string-search "м.п." (last cols)) "м.п." "м2")
)

(defun ae-sv-cnt-marks-p (secs / hit)
  (setq hit nil)
  (foreach sec secs
    (if (member "Марка" (car (cadr sec))) (setq hit T)))
  hit
)

;; Размер: погонаж — длина; площадь — высота x ширина
(defun ae-sv-cnt-size-cells (cols row wide)
  ;; площадь — высота и ширина отдельными колонками; погонаж — одна колонка длины
  (if (vl-position "Высота" cols)
    (list (ae-sv-cnt-cell cols row "Высота") (ae-sv-cnt-cell cols row "Ширина"))
    (if wide
      (list (ae-sv-cnt-cell cols row "Длина, мм") "")
      (list (ae-sv-cnt-cell cols row "Длина, мм"))))
)

;; Есть ли в выборе площадь (колонки «Высота» и «Ширина»)
(defun ae-sv-cnt-area-p (secs / hit)
  (setq hit nil)
  (foreach sec secs
    (if (member "Высота" (car (cadr sec))) (setq hit T)))
  hit
)

;; Ключ варианта окна: n, m — общая сетка с площадью; ln, lm — только погонаж
(defun ae-sv-cnt-key (marks wide)
  (strcat (if wide "" "l") (if marks "m" "n"))
)

;; Ячейки позиции (порядок колонок таблицы)
(defun ae-sv-cnt-item-cells (cols row marks wide)
  (append (list (ae-sv-cnt-cell cols row "№"))
          (if marks (list (ae-sv-cnt-cell cols row "Марка")) '())
          (ae-sv-cnt-size-cells cols row wide)
          (list (strcat (nth (- (length row) 2) row) " шт.")
                (strcat (last row) " " (ae-sv-cnt-unit cols)))))

;; Ячейки итога раздела: подпись «Всего:» в первой колонке размеров
(defun ae-sv-cnt-total-cells (cols row marks wide)
  (append (list "")
          (if marks (list "") '())
          (list "Всего:")
          (if wide (list "") '())
          (list (strcat (nth (- (length row) 2) row) " шт.")
                (strcat (last row) " " (ae-sv-cnt-unit cols)))))

;; Заголовки колонок раздела: площадь — Высота | Ширина; погонаж — Длина
(defun ae-sv-cnt-heads (marks area wide)
  (if area
    (if marks
      (list "№" "Марка" "Высота" "Ширина" "Кол-во" "Всего")
      (list "№" "Высота" "Ширина" "Кол-во" "Всего"))
    (if wide
      (if marks
        (list "№" "Марка" "Длина" "" "Кол-во" "Всего")
        (list "№" "Длина" "" "Кол-во" "Всего"))
      (if marks
        (list "№" "Марка" "Длина" "Кол-во" "Всего")
        (list "№" "Длина" "Кол-во" "Всего"))))
)

;; Выравнивание: l — влево, c — по центру, r — вправо
(defun ae-sv-cnt-aligns (marks wide)
  (append (list "l") (if marks (list "l") '()) (list "c") (if wide (list "c") '()) (list "c" "r")))

(defun ae-sv-cnt-head-aligns (marks wide)
  (append (list "c") (if marks (list "c") '()) (list "c") (if wide (list "c") '()) (list "c" "r")))

;; Текст в ячейке ширины w с выравниванием пробелами слева
(defun ae-sv-cnt-pad (text w al / n pad)
  (setq n (strlen text))
  (setq pad (cond ((= al "c") (/ (- w n) 2)) ((= al "r") (- w n)) (T 0)))
  (strcat (ae-sv-spaces pad) text)
)

;; Строка таблицы: «| ячейка» на каждой позиции табуляции; справа — закрывающая «|» на правой границе таблицы
(defun ae-sv-cnt-row (cells aligns widths / parts)
  (setq parts
    (mapcar '(lambda (c al w) (strcat "| " (ae-sv-cnt-pad c w al))) cells aligns widths))
  (strcat " " (ae-sv-join-sep (append parts (list "|")) (chr 9)) " ")
)

;; Единица ширины списка по символам (как в табуляциях DCL): 1 символ примерно 1,2 единицы
(defun ae-sv-cnt-units (c) (/ (+ (* 6 c) 2) 5))

;; Позиции табуляций колонок: начало колонки k — units(1 + накопленная ширина W + 2);
;; последняя позиция — правый край последней колонки
(defun ae-sv-cnt-bounds (widths / acc out)
  (setq acc 0 out '())
  (foreach w widths
    (setq out (cons (ae-sv-cnt-units (1+ acc)) out)
          acc (+ acc w 2)))
  (reverse (cons (ae-sv-cnt-units (1+ acc)) out))
)

;; Линия таблицы: между «+» на позициях табуляций — дефисы. Дефис примерно 0,5 единицы, «+» примерно 1 единица,
;; поэтому между двумя табуляциями (G единиц) ставится 2G-3 дефисов: линия доходит почти до
;; следующего «+» и не перескакивает табуляцию (запас 3 дефиса). Справа — закрывающий «+» на правой границе.
(defun ae-sv-cnt-line (widths / bd)
  (setq bd (ae-sv-cnt-bounds widths))
  (strcat " "
    (ae-sv-join-sep
      (append
        (mapcar '(lambda (x0 x1) (strcat "+" (ae-sv-dashes (- (* 2 (- x1 x0)) 3))))
                (reverse (cdr (reverse bd)))
                (cdr bd))
        (list "+"))
      (chr 9))
    " ")
)

;; Ширины по ключу варианта (n, m, ln, lm)
(defun ae-sv-cnt-widths-of (key)
  (cdr (assoc key *ae-sv-cnt-widths*))
)

;; Слова строки (разделитель — пробел)
(defun ae-sv-words (text / out cur i n c)
  (setq out '() cur "" i 1 n (strlen text))
  (while (<= i n)
    (setq c (substr text i 1))
    (if (= c " ")
      (progn
        (if (/= cur "") (setq out (cons cur out)))
        (setq cur ""))
      (setq cur (strcat cur c)))
    (setq i (1+ i)))
  (if (/= cur "") (setq out (cons cur out)))
  (reverse out)
)

;; Перенос текста по словам в строки не длиннее maxw (слово длиннее maxw — отдельной строкой)
(defun ae-sv-wrap (text maxw / lines line w)
  (setq lines '() line "")
  (foreach w (ae-sv-words text)
    (cond
      ((= line "") (setq line w))
      ((<= (+ (strlen line) 1 (strlen w)) maxw) (setq line (strcat line " " w)))
      (T (setq lines (cons line lines) line w))))
  (if (/= line "") (setq lines (cons line lines)))
  (reverse lines)
)

;; Заголовок таблицы «Подсчёт»: название каждого выбранного блока — своей строкой, по центру таблицы.
;; total — ширина таблицы в символах (сумма ячеек W + 2); центрирование пробелами слева.
(defun ae-sv-cnt-title-rows (names total / out pad)
  (setq out '())
  (foreach n names
    (foreach ln (ae-sv-wrap n (- total 2))
      (setq pad (/ (- total (strlen ln)) 2))
      (setq out (append out (list (strcat " " (ae-sv-spaces pad) ln))))))
  out
)

;; Строки списка «Подсчёт»: разделы подряд, между разделами — пустая строка; над каждым разделом — заголовок
(defun ae-sv-cnt-lines (secs marks / wide widths out cols body items total)
  (if (null secs)
    nil
    (progn
      (setq wide (ae-sv-cnt-area-p secs)
            widths (ae-sv-cnt-widths-of (ae-sv-cnt-key marks wide))
            total (apply '+ (mapcar '(lambda (w) (+ w 2)) widths))
            out '())
      (foreach sec secs
        (setq cols (car (cadr sec))
              body (cdr (cadr sec))
              items (reverse (cdr (reverse body))))
        (if out (setq out (append out (list ""))))
        (setq out (append out (ae-sv-cnt-title-rows (caddr sec) total)))
        (setq out (append out
                          (list (ae-sv-cnt-line widths)
                                (ae-sv-cnt-row (ae-sv-cnt-heads marks (vl-position "Высота" cols) wide)
                                               (ae-sv-cnt-head-aligns marks wide) widths)
                                (ae-sv-cnt-line widths))))
        (foreach row items
          (setq out (append out
                            (list (ae-sv-cnt-row (ae-sv-cnt-item-cells cols row marks wide)
                                                 (ae-sv-cnt-aligns marks wide) widths)))))
        (setq out (append out
                          (list (ae-sv-cnt-line widths)
                                (ae-sv-cnt-row (ae-sv-cnt-total-cells cols (last body) marks wide)
                                               (ae-sv-cnt-aligns marks wide) widths)
                                (ae-sv-cnt-line widths)))))
      out))
)

;; Вставки одного вида: площадь (колонки «Высота» и «Ширина») и погонаж (колонка «Длина»)
(defun ae-sv-cnt-area-secs (secs)
  (vl-remove-if-not '(lambda (s) (member "Высота" (car (cadr s)))) secs)
)

(defun ae-sv-cnt-lin-secs (secs)
  (vl-remove-if '(lambda (s) (member "Высота" (car (cadr s)))) secs)
)

;; Смешанный выбор: есть и площадь, и погонаж — два отдельных списка (диалоги _xn / _xm)
(defun ae-sv-cnt-mixed-p (secs)
  (if (and (ae-sv-cnt-area-secs secs) (ae-sv-cnt-lin-secs secs)) T nil)
)

;; Строки окна для одного диалога (n, m, ln, lm) — все разделы
(defun ae-sv-cnt-display ()
  (if (null *ae-sv-cnt-secs*)
    (mapcar 'car *ae-sv-cnt-rows*)
    (ae-sv-cnt-lines *ae-sv-cnt-secs* (ae-sv-cnt-marks-p *ae-sv-cnt-secs*)))
)

;; Имя диалога: searchattvis_count_n / _m / _ln / _lm; при смешанном выборе — _xn / _xm
(defun ae-sv-cnt-dialog-name (/ marks)
  (setq marks (ae-sv-cnt-marks-p *ae-sv-cnt-secs*))
  (cond
    ((null *ae-sv-cnt-secs*) "searchattvis_count_n")
    ((ae-sv-cnt-mixed-p *ae-sv-cnt-secs*) (strcat "searchattvis_count_x" (if marks "m" "n")))
    (T (strcat "searchattvis_count_" (ae-sv-cnt-key marks (ae-sv-cnt-area-p *ae-sv-cnt-secs*)))))
)

;; Окно «Подсчёт». Результат — *ae-sv-next*: "table" (таблица) или "close"
(defun ae-sv-count-dialog (/ dcl-file dcl-id t0)
  (setq t0 (getvar "MILLISECS"))
  (ae-sv-cnt-data (car *ae-sv-count-sel*) (cadr *ae-sv-count-sel*))
  (princ (strcat "\nПодсчёт: расчёт " (itoa (- (getvar "MILLISECS") t0)) " мс."))
  (setq *ae-sv-next* "close")
  (setq dcl-file (ae-sv-dcl-file))
  (if (null dcl-file)
    (alert "Не найден файл Extraction\\searchattvis.dcl.")
    (progn
      (setq dcl-id (load_dialog dcl-file))
      (if (< dcl-id 0)
        (alert "Не удалось загрузить окно подсчёта.")
        (if (new_dialog (ae-sv-cnt-dialog-name) dcl-id)
          (progn
            (if (ae-sv-cnt-mixed-p *ae-sv-cnt-secs*)
              (progn
                (start_list "lst_count")
                (foreach s (ae-sv-cnt-lines (ae-sv-cnt-area-secs *ae-sv-cnt-secs*) (ae-sv-cnt-marks-p *ae-sv-cnt-secs*))
                  (add_list s))
                (end_list)
                (start_list "lst_count2")
                (foreach s (ae-sv-cnt-lines (ae-sv-cnt-lin-secs *ae-sv-cnt-secs*) (ae-sv-cnt-marks-p *ae-sv-cnt-secs*))
                  (add_list s))
                (end_list))
              (progn
                (start_list "lst_count")
                (foreach s (ae-sv-cnt-display)
                  (add_list s))
                (end_list)))
            (set_tile "txt_count_status" (ae-sv-totals-text *ae-sv-cnt-secs*))
            (action_tile "btn_copy" "(ae-sv-cnt-copy)")
            (action_tile "btn_acad" "(progn (setq *ae-sv-next* \"table\") (done_dialog 3))")
            (action_tile "btn_count_close" "(progn (setq *ae-sv-next* \"close\") (done_dialog 0))")
            (start_dialog)
            (unload_dialog dcl-id))
          (progn
            (unload_dialog dcl-id)
            (alert "Не удалось открыть окно подсчёта."))))))
  T
)

;; Ширина колонки по самой длинной строке
;; Название выбранных динамических блоков для заголовка таблицы: через «, »
(defun ae-sv-block-title (names / out)
  (setq out "")
  (foreach n names
    (setq out (strcat out (if (= out "") "" ", ") n)))
  out
)

;; Ширина колонки по самой длинной строке, с запасом (текст не переносится)
(defun ae-sv-colw (rows c / w)
  (setq w 0)
  (foreach r rows
    (if (and (nth c r) (> (strlen (nth c r)) w)) (setq w (strlen (nth c r)))))
  (+ 8.0 (* 2.8 w))
)

;; Выравнивание колонки по тексту заголовка: числа — по центру (5), сумма — вправо (6).
;; nil — колонка остаётся как есть (№, Марка)
(defun ae-sv-col-align (head)
  (cond
    ((wcmatch head "Сумма*") 6)
    ((member head (quote ("Высота" "Ширина" "Кол-во" "Длина, мм"))) 5)
    (T nil))
)

;; Таблица AutoCAD: разделы друг под другом, точка вставки — первая.
;; Строка 0 — название блока (жирным), строка 1 — шапка колонок,
;; далее позиции и «Всего:» последней строкой
(defun ae-sv-acad-tables (/ pt doc space sec rows nr nc tbl k c tot made h al r)
  (if (null *ae-sv-cnt-secs*)
    (alert "Нет данных для таблицы: выберите блоки с погонажом или площадью.")
    (progn
      (setq pt (getpoint "\nТочка вставки таблицы подсчёта: "))
      (if pt
        (progn
          (vl-load-com)
          (setq doc (vla-get-activedocument (vlax-get-acad-object)))
          (setq space (vla-get-modelspace doc))
          (vla-startundomark doc)
          (foreach sec *ae-sv-cnt-secs*
            (if pt
              (progn
                (setq rows (cadr sec)
                      nr (1+ (length rows))
                      nc (length (car rows)))
                (setq tbl (vl-catch-all-apply 'vla-addtable
                            (list space (vlax-3d-point (trans pt 1 0)) nr nc 10.0 50.0)))
                (if (vl-catch-all-error-p tbl)
                  (progn
                    (princ (strcat "\nОшибка создания таблицы: " (vl-catch-all-error-message tbl)))
                    (setq pt nil))
                  (progn
                    (ts-ac-fast-begin tbl)
                    (ts-ac-title tbl 0
                      (strcat "{\\fArial|b1|i0|c0|p34;" (ae-sv-block-title (caddr sec)) "}")
                      nc)
                    (ts-ac-header tbl 1 (car rows))
                    (setq tot (last rows))
                    (setq k 2)
                    (foreach r (reverse (cdr (reverse (cdr rows))))
                      (setq c 0)
                      (foreach f r
                        (vla-SetText tbl k c f)
                        (setq c (1+ c)))
                      (setq k (1+ k)))
                    (ts-ac-total tbl (1- nr) (car tot) 0 (- nc 3) 4)
                    (vla-SetText tbl (1- nr) (- nc 2) (nth (- nc 2) tot))
                    (vla-SetText tbl (1- nr) (- nc 1) (nth (- nc 1) tot))
                    (setq c 0)
                    (repeat nc
                      (vla-SetColumnWidth tbl c (ae-sv-colw rows c))
                      (setq c (1+ c)))
                    (ts-ac-fast-end tbl)
                    ;; выравнивание — уже после снятия подавления перерисовки, по строкам 1..n
                    ;; (шапка и позиции); в «Всего:» объединены все, кроме последних двух ячеек
                    (setq c 0)
                    (foreach h (car rows)
                      (setq al (ae-sv-col-align h))
                      (if al
                        (progn
                          (setq r 1)
                          (repeat (1- nr)
                            (if (or (< r (1- nr)) (>= c (- nc 2)))
                              (vla-SetCellAlignment tbl r c al))
                            (setq r (1+ r)))))
                      (setq c (1+ c)))
                    (setq made T)
                    (setq pt (tu-next-table-point pt nr 10.0 10.0)))))))
          (vla-endundomark doc)
          (if made (setq *ae-sv-next* "done"))
          (princ "\nТаблица подсчёта создана.")))))
  (princ)
)

;; Цикл окна «Подсчёт»: «Таблица AutoCAD» строит таблицу и снова показывает окно;
;; «Закрыть» или Esc возвращают окно поиска с сохранённым выбором
(defun ae-sv-count-flow ()
  (setq *ae-sv-next* "table")
  (while (equal *ae-sv-next* "table")
    (ae-sv-count-dialog)
    (if (equal *ae-sv-next* "table") (ae-sv-acad-tables)))
  (if (equal *ae-sv-next* "done")
    (progn
      ;; Таблица или копирование выполнены: окна «Подсчёт» и «Поиск» не открываются снова
      (setq *ae-sv-next* nil *ae-sv-restore* nil *ae-sv-pend* nil *ae-sv-count-sel* nil)
      (setq *ae-sv-count-all-title* nil)
      (setq *ae-sv-finished* T))
    (progn
      (setq *ae-sv-next* nil)
      (ae-sv-open)))
)

;; ============================================================
;; Команда
;; ============================================================

;; Вставки пространства модели: обход цепочки объектов от блока *Model_Space*.
;; Без ssget "_X": тот читает весь чертёж (включая определения блоков)
;; и на больших чертежах занимает десятки секунд.
(defun ae-sv-all-model-inserts ( / blk e out)
  (setq blk (tblobjname "BLOCK" "*Model_Space"))
  (if (null blk)
    ;; Запасной путь: блок не найден — обычный фильтр (медленнее)
    (ae-sv-model-inserts (ssget "_X" '((0 . "INSERT") (410 . "Model"))))
    (progn
      (setq out '())
      (setq e (entnext blk))
      (while e
        (if (ae-sv-model-insert-p e) (setq out (cons e out)))
        (setq e (entnext e)))
      (reverse out)))
)

(defun ae-sv-load-scope (/ pre ens t0)
  (setq pre (ssget "_I"))
  (if (and (null pre) (= (type ex-take-preselected) 'SUBR))
    (setq pre (ex-take-preselected)))
  (if (and (boundp '*extraction-preselected-set*) *extraction-preselected-set*)
    (setq *extraction-preselected-set* nil))
  (setq ens (if pre (ae-sv-model-inserts pre) (ae-sv-all-model-inserts)))
  (cond
    ((and pre (null ens))
     (princ "\nВ предварительном выделении нет вставок блоков. Поиск не запущен.")
     nil)
    ((null ens)
     (princ "\nВ чертеже нет вставок блоков. Поиск не запущен.")
     nil)
    (T
     (setq *ae-sv-scope-pre* (if pre T nil))
     (setq *ae-sv-scope-text*
       (strcat (if pre "выделенные" "весь чертёж")
               " (" (itoa (length ens)) " вставок)"))
     (setq t0 (getvar "MILLISECS"))
     (setq *ae-sv-inserts* (ae-sv-collect ens))
     (setq *ae-sv-all-inserts* (if pre nil *ae-sv-inserts*))
     (setq *ae-sv-entries-key* nil *ae-sv-entries-cache* nil)
     (princ (strcat "\nПоиск: вставок " (itoa (length ens))
                    ", сбор данных " (itoa (- (getvar "MILLISECS") t0)) " мс."))
     T))
)

(defun ae-sv-run (mode / )
  (setq *ae-sv-mode* mode)
  (setq *ae-sv-result* nil)
  (setq *ae-sv-finished* nil)
  (if (and (ae-sv-load-scope) (ae-sv-open))
    (progn
      (if (not *ae-sv-finished*) (ae-sv-print-lists))
      (if *ae-sv-result*
        (progn
          (sssetfirst nil (nth 0 *ae-sv-result*))
          (princ (strcat "\nВыбрано вставок: " (itoa (nth 1 *ae-sv-result*))
                         ", пропущено (слои): " (itoa (nth 2 *ae-sv-result*))))))))
  (princ)
)

;; Без окна: условия спрашиваются в командной строке, перечень печатается туда же
(defun ae-sv-list-run (mode qa qb / rows)
  (setq *ae-sv-mode* mode)
  (if (ae-sv-load-scope)
    (progn
      (setq rows
        (if (= mode "attr")
          (ae-sv-rows-attr (ae-sv-norm qa) (ae-sv-norm qb))
          (ae-sv-rows-vis (ae-sv-norm qa) (ae-sv-norm qb))))
      (setq *ae-sv-rows* rows)
      (ae-sv-print-results)))
  (princ)
)

(defun c:ПОИСКАТР ()
  (ae-sv-run "attr")
)

(defun c:SEARCHATTR () (c:ПОИСКАТР))

(defun c:ПОИСКВИД ()
  (ae-sv-run "vis")
)

(defun c:ПОИСКБЛОК ()
  (ae-sv-run "all")
)

(defun c:SEARCHBLOCK () (c:ПОИСКБЛОК))

(defun c:SEARCHVIS () (c:ПОИСКВИД))

;; ПОИСКСПИСОК: режим, затем условия. Пустое условие — без ограничения.
(defun c:ПОИСКСПИСОК (/ kw qa qb)
  (initget "Атрибуты Видимость")
  (setq kw (getkword "\nРежим [Атрибуты/Видимость] <Атрибуты>: "))
  (if (= kw "Видимость")
    (progn
      (setq qa (getstring T "\nВидимость (пусто — любая): "))
      (setq qb (getstring T "\nЗначение (пусто — без условия): "))
      (ae-sv-list-run "vis" qa qb))
    (progn
      (setq qa (getstring T "\nАтрибут (пусто — любой): "))
      (setq qb (getstring T "\nЗначение (пусто — любое): "))
      (ae-sv-list-run "attr" qa qb)))
  (princ)
)

;; Таблица команд: при ручной загрузке печатается сразу; при RELOAD
;; (тихая загрузка) строки регистрируются, а таблицу печатает RELOAD.
(if (member (ae-reload-sym-type 'ae-banner-add) '(SUBR USUBR))
  (progn
    (ae-banner-add "SEARCHATTVIS" '(
        ("SEARCHBLOCK / ПОИСКБЛОК" "Поиск блоков: все блоки или выборка (Блок, Значение)")
        ("SEARCHATTR / ПОИСКАТР" "поиск блоков по атрибутам (окно)")
        ("SEARCHVIS / ПОИСКВИД" "поиск блоков по видимостям (окно)")))
    (if (not (and (boundp '*ae-reload-quiet*) *ae-reload-quiet*))
      (progn
        (setq *ae-banner-items* (list (assoc "SEARCHATTVIS" *ae-banner-items*)))
        (princ "\nAutoExtraction — модульная система AutoLISP для AutoCAD: извлечение данных из чертежей, подсчёт материалов, раскрой хлыстов и листов.")
        (ae-banner-print)))))
(princ)

;; SEARCHATTVIS.LSP — ред. 3: поиск блоков, атрибутов и видимостей, выбор подобного. Команды: ПОИСКБЛОК, ПОИСКАТР, ПОИСКВИД, ПОИСКСПИСОК.
