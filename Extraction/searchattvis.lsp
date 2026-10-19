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

(setq *ae-sv-mode* "attr")      ; "attr" | "vis"
(setq *ae-sv-inserts* nil)      ; записи: (ename имя видимость атрибуты)
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

;; Замороженный, заблокированный или выключенный слой — вставка не выбирается
(defun ae-sv-layer-skip-p (en / ld flags color)
  (setq ld (tblsearch "LAYER" (cdr (assoc 8 (entget en)))))
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
                              (cdr dyn))
                        out)))))
  (reverse out)
)

;; ============================================================
;; Группировка строк списка
;; items: ((текст ename) ...) -> ((текст имена...) ...), по алфавиту
;; ============================================================

(defun ae-sv-group (items / sorted out cur ens it)
  (setq sorted
    (vl-sort items '(lambda (a b) (< (strcase (car a)) (strcase (car b))))))
  (setq out '() cur nil ens '())
  (foreach it sorted
    (if (and cur (= (car it) cur))
      (if (not (member (cadr it) ens)) (setq ens (cons (cadr it) ens)))
      (progn
        (if cur (setq out (cons (cons cur (reverse ens)) out)))
        (setq cur (car it) ens (list (cadr it))))))
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
  (if (= *ae-sv-mode* "attr")
    (list "Атрибут (по Enter):" "Значение (по Enter):")
    (list "Видимость (по Enter):" "Значение (по Enter):"))
)

(defun ae-sv-set-hints (/ h)
  (setq h (ae-sv-hints))
  (set_tile "edt_query" (car h))
  (set_tile "edt_value" (cadr h))
)

;; Текст поля без подсказки: подсказка вырезается из любой позиции,
;; набранный до неё или после неё текст сохраняется
(defun ae-sv-clean (v hint / p)
  (cond
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
;; Лейбл — состояние или «состояние | значение свойства» (если значение введено).
(defun ae-sv-vis-entries (rec qvis qval / out states dvals s d)
  (setq out '())
  (setq states (vl-remove-if-not '(lambda (v) (ae-sv-match v qvis)) (nth 4 rec)))
  (setq dvals
    (if (= qval "")
      '()
      (mapcar 'cdr (vl-remove-if-not '(lambda (o) (ae-sv-match (cdr o) qval)) (nth 5 rec)))))
  (if (and states (or (= qval "") dvals))
    (if (= qval "")
      (foreach s states
        (setq out (cons (list (nth 1 rec) s (car rec)) out)))
      (foreach s states
        (foreach d dvals
          (setq out (cons (list (nth 1 rec) (strcat s " | " d) (car rec)) out))))))
  out
)

(defun ae-sv-entries (qa qb / out rec)
  (setq out '())
  (foreach rec *ae-sv-inserts*
    (setq out
      (append out
        (if (= *ae-sv-mode* "attr")
          (ae-sv-attr-entries rec qa qb)
          (ae-sv-vis-entries rec qa qb)))))
  out
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
(defun ae-sv-final (entries sel-b sel-i / ens)
  (setq ens '())
  (foreach e entries
    (if (and (or (null sel-b) (member (car e) sel-b))
             (or (null sel-i) (member (cadr e) sel-i))
             (not (member (caddr e) ens)))
      (setq ens (cons (caddr e) ens))))
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

(defun ae-sv-join-sp (lst / s)
  (setq s "")
  (foreach x lst
    (setq s (if (= s "") x (strcat s " " x))))
  s
)

;; Заполнить список строками «пункт (N)»; выбор восстанавливается по тексту пункта
(defun ae-sv-fill (key rows sel / i idx)
  (start_list key)
  (foreach r rows
    (add_list (strcat (car r) " (" (itoa (length (cdr r))) ")")))
  (end_list)
  (setq idx '() i 0)
  (foreach r rows
    (if (member (car r) sel) (setq idx (cons (itoa i) idx)))
    (setq i (1+ i)))
  (set_tile key (ae-sv-join-sp (reverse idx)))
)

;; Обновление обоих списков и статуса. Каждый список сужает другой.
(defun ae-sv-refresh (/ qa qb sel-b sel-i entries rows1 rows2)
  (setq qa (ae-sv-norm (ae-sv-field "edt_query" (car (ae-sv-hints)))))
  (setq qb (ae-sv-norm (ae-sv-field "edt_value" (cadr (ae-sv-hints)))))
  (setq sel-b (ae-sv-selected "lst_blocks" *ae-sv-rows1*))
  (setq sel-i (ae-sv-selected "lst_items" *ae-sv-rows2*))
  (setq entries (ae-sv-entries qa qb))
  (setq rows1 (ae-sv-list1 entries sel-i))
  (setq rows2 (ae-sv-list2 entries sel-b))
  (setq sel-b (ae-sv-keep sel-b rows1))
  (setq sel-i (ae-sv-keep sel-i rows2))
  (setq *ae-sv-entries* entries)
  (setq *ae-sv-rows1* rows1)
  (setq *ae-sv-rows2* rows2)
  (set_tile "txt_items_label" (if (= *ae-sv-mode* "attr") "Атрибуты" "Видимости"))
  (ae-sv-fill "lst_blocks" rows1 sel-b)
  (ae-sv-fill "lst_items" rows2 sel-i)
  (set_tile "txt_status"
    (strcat "Блоков: " (itoa (length rows1)) ", "
            (if (= *ae-sv-mode* "attr") "атрибутов: " "видимостей: ")
            (itoa (length rows2)) ", вставок: "
            (itoa (length (ae-sv-unique (mapcar 'caddr entries))))
            ". Область: " *ae-sv-scope-text*))
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
      (princ (strcat "\n" (if (= *ae-sv-mode* "attr") "Атрибуты" "Видимости")
                     " (" (itoa (length *ae-sv-rows2*)) "):"))
      (foreach r *ae-sv-rows2*
        (princ (strcat "\n  " (car r) " | " (itoa (length (cdr r))) " шт.")))
      (princ (strcat "\nВставок: " (itoa (length (ae-sv-unique (mapcar 'caddr *ae-sv-entries*))))
                     ". Область: " *ae-sv-scope-text*))))
  (princ)
)

(defun ae-sv-on-mode (m)
  (setq *ae-sv-mode* m)
  (setq *ae-sv-rows1* nil *ae-sv-rows2* nil)
  (ae-sv-set-hints)
  (ae-sv-refresh)
)

;; Кнопка «Выбрать»: отмечены блоки и/или пункты второго списка
(defun ae-sv-on-pick (/ sel-b sel-i ens ss sel-n skipped en)
  (setq sel-b (ae-sv-selected "lst_blocks" *ae-sv-rows1*))
  (setq sel-i (ae-sv-selected "lst_items" *ae-sv-rows2*))
  (if (and (null sel-b) (null sel-i))
    (set_tile "txt_status" "Не выбрано ни одной строки. Отметьте блоки или пункты второго списка.")
    (progn
      (setq ens (ae-sv-final *ae-sv-entries* sel-b sel-i))
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
              (done_dialog 1))))))))

(defun ae-sv-init ()
  (set_tile "rb_mode_attr" (if (= *ae-sv-mode* "attr") "1" "0"))
  (set_tile "rb_mode_vis" (if (= *ae-sv-mode* "attr") "0" "1"))
  (ae-sv-set-hints)
  (ae-sv-refresh)
)

;; Сброс выбора в обоих списках (кнопка и Esc); окно не закрывается
(defun ae-sv-reset ()
  (set_tile "lst_blocks" "")
  (set_tile "lst_items" "")
  (ae-sv-refresh)
)

(defun ae-sv-wire ()
  (action_tile "rb_mode_attr" "(ae-sv-on-mode \"attr\")")
  (action_tile "rb_mode_vis" "(ae-sv-on-mode \"vis\")")
  (action_tile "edt_query" "(ae-sv-refresh)")
  (action_tile "edt_value" "(ae-sv-refresh)")
  (action_tile "lst_blocks" "(ae-sv-refresh)")
  (action_tile "lst_items" "(ae-sv-refresh)")
  (action_tile "btn_select" "(ae-sv-on-pick)")
  (action_tile "btn_reset" "(ae-sv-reset)")
  (action_tile "btn_cancel" "(done_dialog 0)")
)

;; Открывает окно. T — диалог отработал, nil — окно не открылось.
(defun ae-sv-open (/ root dcl-file dcl-id)
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
            T)
          (progn
            (unload_dialog dcl-id)
            (alert "Не удалось открыть окно поиска.")
            nil))))))

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

(defun ae-sv-load-scope (/ pre ens)
  (setq pre (ssget "_I"))
  (setq ens (if pre (ae-sv-model-inserts pre) (ae-sv-all-model-inserts)))
  (cond
    ((and pre (null ens))
     (princ "\nВ предварительном выделении нет вставок блоков. Поиск не запущен.")
     nil)
    ((null ens)
     (princ "\nВ чертеже нет вставок блоков. Поиск не запущен.")
     nil)
    (T
     (setq *ae-sv-scope-text*
       (strcat (if pre "выделенные" "весь чертёж")
               " (" (itoa (length ens)) " вставок)"))
     (setq *ae-sv-inserts* (ae-sv-collect ens))
     T))
)

(defun ae-sv-run (mode / )
  (setq *ae-sv-mode* mode)
  (setq *ae-sv-result* nil)
  (if (and (ae-sv-load-scope) (ae-sv-open))
    (progn
      (ae-sv-print-lists)
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

(defun c:ПОИСКВИД ()
  (ae-sv-run "vis")
)

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

;; SEARCHATTVIS.LSP — ред. 1: поиск по атрибутам и видимостям. Команды: ПОИСКАТР, ПОИСКВИД.
