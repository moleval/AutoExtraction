;;; ============================================================
;;; Extraction/searchattvis.lsp — поиск по атрибутам и видимостям блоков
;;; Команды: ПОИСКАТР (режим «Атрибуты»), ПОИСКВИД (режим «Видимость блока»)
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

;; Запись на вставку: (ename имя видимость атрибуты). Читается один раз.
(defun ae-sv-collect (ens / out en vla name vis)
  (setq out '())
  (foreach en ens
    (setq vla (vl-catch-all-apply 'vlax-ename->vla-object (list en)))
    (if (not (vl-catch-all-error-p vla))
      (progn
        (setq name (ae-sv-str (vl-catch-all-apply 'su-get-effective-name (list vla))))
        (if (= name "")
          (setq name (ae-sv-str (cdr (assoc 2 (entget en))))))
        (setq vis (vl-catch-all-apply 'su-get-visibility (list vla)))
        (if (/= (type vis) 'STR) (setq vis nil))
        (setq out (cons (list en name vis (ae-sv-read-attrs vla)) out)))))
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

(defun ae-sv-set-labels ()
  (if (= *ae-sv-mode* "attr")
    (progn
      (set_tile "txt_query_label" "Поиск (тег, по Enter):")
      (set_tile "txt_value_label" "Значение (по Enter):"))
    (progn
      (set_tile "txt_query_label" "Поиск (видимость, по Enter):")
      (set_tile "txt_value_label" "Значение атрибута (по Enter):"))
  )
)

(defun ae-sv-refresh (/ rows labels qtag qval ens)
  (setq qtag (ae-sv-norm (get_tile "edt_query")))
  (setq qval (ae-sv-norm (get_tile "edt_value")))
  (if (= *ae-sv-mode* "attr")
    (setq rows (ae-sv-rows-attr qtag qval))
    (setq rows (ae-sv-rows-vis qtag qval)))
  (setq *ae-sv-rows* rows)
  (setq labels
    (mapcar '(lambda (r) (strcat (car r) " | " (itoa (length (cdr r))) " шт."))
            rows))
  (start_list "lst_result")
  (mapcar 'add_list labels)
  (end_list)
  (set_tile "lst_result" "")
  (setq ens (ae-sv-unique (apply 'append (mapcar 'cdr rows))))
  (set_tile "txt_status"
    (strcat "Найдено: " (itoa (length rows)) " строк, " (itoa (length ens))
            " вставок. Область: " *ae-sv-scope-text*))
  T
)

(defun ae-sv-on-mode (m)
  (setq *ae-sv-mode* m)
  (set_tile "edt_query" "")
  (set_tile "edt_value" "")
  (ae-sv-set-labels)
  (ae-sv-refresh)
)

;; Индексы выбранных строк из строки list_box ("0 2 3")
(defun ae-sv-indices (s / r)
  (if (and (= (type s) 'STR) (/= s ""))
    (progn
      (setq r (vl-catch-all-apply 'read (list (strcat "(" s ")"))))
      (if (and (not (vl-catch-all-error-p r)) (listp r))
        (vl-remove-if-not 'numberp r)
        nil))
    nil)
)

;; Кнопка «Выбрать»: ничего не выбрано — окно остаётся, ничего не выбирается.
;; Все вставки на замороженных/заблокированных слоях — окно остаётся.
(defun ae-sv-on-pick (/ idxs ens ss sel-n skipped en)
  (setq idxs (ae-sv-indices (get_tile "lst_result")))
  (if (null idxs)
    (set_tile "txt_status" "Не выбрано ни одной строки. Отметьте строки в списке.")
    (progn
      (setq ens '())
      (foreach i idxs
        (if (and (>= i 0) (< i (length *ae-sv-rows*)))
          (foreach en (cdr (nth i *ae-sv-rows*))
            (if (not (member en ens)) (setq ens (cons en ens))))))
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
          (setq *ae-sv-result* (list ss (length idxs) sel-n skipped))
          (done_dialog 1))))))

(defun ae-sv-init ()
  (set_tile "rb_mode_attr" (if (= *ae-sv-mode* "attr") "1" "0"))
  (set_tile "rb_mode_vis" (if (= *ae-sv-mode* "attr") "0" "1"))
  (ae-sv-set-labels)
  (set_tile "edt_query" "")
  (set_tile "edt_value" "")
  (ae-sv-refresh)
)

(defun ae-sv-wire ()
  (action_tile "rb_mode_attr" "(ae-sv-on-mode \"attr\")")
  (action_tile "rb_mode_vis" "(ae-sv-on-mode \"vis\")")
  (action_tile "edt_query" "(ae-sv-refresh)")
  (action_tile "edt_value" "(ae-sv-refresh)")
  (action_tile "btn_select" "(ae-sv-on-pick)")
  (action_tile "btn_cancel" "(done_dialog 0)")
)

;; Открывает окно. T — диалог отработал, nil — окно не открылось.
(defun ae-sv-open (/ root dcl-file dcl-id)
  (setq root (if (= (type ae-settings-root) 'SUBR) (ae-settings-root) nil))
  (setq dcl-file (findfile "searchattvis.dcl"))
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
  (setq out '())
  (setq blk (tblobjname "BLOCK" "*Model_Space*"))
  (setq e (if blk (entnext blk) nil))
  (while e
    (if (ae-sv-model-insert-p e) (setq out (cons e out)))
    (setq e (entnext e)))
  (reverse out)
)

(defun ae-sv-run (mode / pre ens t0)
  (setq t0 (getvar "millisecs"))
  (setq *ae-sv-mode* mode)
  (setq *ae-sv-result* nil)
  (setq pre (ssget "_I"))
  (setq ens
    (if pre
      (ae-sv-model-inserts pre)
      (ae-sv-all-model-inserts)))
  (cond
    ((and pre (null ens))
     (princ "\nВ предварительном выделении нет вставок блоков. Поиск не запущен."))
    ((null ens)
     (princ "\nВ чертеже нет вставок блоков. Поиск не запущен."))
    (T
     (setq *ae-sv-scope-text*
       (strcat (if pre "выделенные" "весь чертёж")
               " (" (itoa (length ens)) " вставок)"))
     (setq *ae-sv-inserts* (ae-sv-collect ens))
     (if (and (ae-sv-open) *ae-sv-result*)
       (progn
         (sssetfirst nil (nth 0 *ae-sv-result*))
         (princ (strcat "\nВыбрано вставок: " (itoa (nth 2 *ae-sv-result*))
                        " (строк: " (itoa (nth 1 *ae-sv-result*))
                        "), пропущено: " (itoa (nth 3 *ae-sv-result*))))))))
  (princ)
)

(defun c:ПОИСКАТР ()
  (ae-sv-run "attr")
)

(defun c:ПОИСКВИД ()
  (ae-sv-run "vis")
)

;; SEARCHATTVIS.LSP — ред. 1: поиск по атрибутам и видимостям. Команды: ПОИСКАТР, ПОИСКВИД.
