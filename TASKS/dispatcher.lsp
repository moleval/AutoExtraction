;;; ============================================================
;;; dispatcher.lsp  (самодостаточная версия, без fboundp)
;;; Команды: TASKDISPATCHER, TASKS
;;; ============================================================
(vl-load-com)

;; ---------- Глобальное состояние ----------
(setq *DISPATCHER-DCL-ID* nil)
(setq *DISPATCHER-ALL-LAYERS* nil)
(setq *DISPATCHER-VISIBLE-LAYERS* nil)
(setq *DISPATCHER-SELECTED-LAYERS* nil)
(setq *DISPATCHER-SELECTED-INDICES* nil)
(setq *DISPATCHER-GROUP-FILTER* nil)
(setq *DISPATCHER-TASK-ID* 'FASONKA)
(setq *DISPATCHER-REPORT-MODE* "DETAIL")
(setq *DISPATCHER-EXPORT-EXCEL* T)
(setq *DISPATCHER-EXPORT-TXT* nil)
(setq *DISPATCHER-CREATE-TABLE* T)
(setq *DISPATCHER-ACTION* 'CANCEL)

;; ---------- Безопасное получение списка слоёв ----------
(defun dsp-unique-ci (lst / out x key)
  (setq out '())
  (if (listp lst)
    (foreach x lst
      (if (= (type x) 'STR)
        (progn
          (setq key (strcase x))
          (if (not (vl-some '(lambda (y) (= (strcase y) key)) out))
            (setq out (append out (list x)))
          )
        )
      )
    )
  )
  out
)

(defun dsp-layer-names ( / acad doc layers out name)
  (setq out '())
  (setq acad (vl-catch-all-apply 'vlax-get-acad-object '()))
  (if (and (not (vl-catch-all-error-p acad)) acad)
    (progn
      (setq doc (vl-catch-all-apply 'vla-get-ActiveDocument (list acad)))
      (if (and (not (vl-catch-all-error-p doc)) doc)
        (progn
          (setq layers (vl-catch-all-apply 'vla-get-Layers (list doc)))
          (if (and (not (vl-catch-all-error-p layers)) layers)
            (vlax-for lay layers
              (setq name (vl-catch-all-apply 'vla-get-Name (list lay)))
              (if (and (not (vl-catch-all-error-p name))
                       (= (type name) 'STR)
                       (> (strlen name) 0))
                (setq out (cons name out))
              )
            )
          )
        )
      )
    )
  )
  (setq out (vl-remove-if-not '(lambda (x) (= (type x) 'STR)) out))
  (setq out (vl-sort out '(lambda (a b) (< (strcase a) (strcase b)))))
  (dsp-unique-ci out)
)

;; ---------- Загрузка модулей ----------
(defun dispatcher-project-root ( / dsp)
  (setq dsp (findfile "dispatcher.lsp"))
  (if dsp (vl-filename-directory (vl-filename-directory dsp)) nil)
)
(defun dispatcher-tasks-dir ( / dsp)
  (setq dsp (findfile "dispatcher.lsp"))
  (if dsp (vl-filename-directory dsp) nil)
)
(defun dispatcher-load-all ( / root common f path)
  (setq root (dispatcher-project-root))
  (if root
    (progn
      (setq common (strcat root "\\common\\"))
      (foreach f '("task-utils.lsp" "layer-utils.lsp" "excel-utils.lsp" "table-utils.lsp")
        (setq path (strcat common f))
        (if (findfile path) (load path)
          (princ (strcat "\n[DISPATCHER] Не найден: " path)))
      )
      (setq path (strcat root "\\TASKS\\fasonka.lsp"))
      (if (findfile path) (load path)
        (princ (strcat "\n[DISPATCHER] Не найден: " path)))
    )
    (progn
      (princ "\n[DISPATCHER] Корень не найден, загрузка по имени...")
      (foreach f '("task-utils.lsp" "layer-utils.lsp" "excel-utils.lsp" "table-utils.lsp" "fasonka.lsp")
        (setq path (findfile f))
        (if path (load path) (princ (strcat "\n[DISPATCHER] Не найден: " f)))
      )
    )
  )
  T
)

;; ---------- Список слоёв в диалоге ----------
(defun dispatcher-parse-indices (s / x)
  (if (and (= (type s) 'STR) (/= s ""))
    (progn
      (setq x (read (strcat "(" s ")")))
      (if (= (type x) 'LIST) x nil)
    )
    nil
  )
)

(defun dispatcher-selected-names ( / s indices out i n)
  (setq s (get_tile "lst_layers"))
  (if (or (null s) (/= (type s) 'STR) (= s ""))
    nil
    (progn
      (setq indices (dispatcher-parse-indices s))
      (setq out '())
      (setq n (length *DISPATCHER-VISIBLE-LAYERS*))
      (foreach i indices
        (if (and (numberp i) (>= i 0) (< i n))
          (setq out (cons (nth i *DISPATCHER-VISIBLE-LAYERS*) out))
        )
      )
      (dsp-unique-ci out)
    )
  )
)

(defun dispatcher-rebuild-layer-list ( / restore i selected item vis)
  (setq restore (dispatcher-selected-names))
  (setq *DISPATCHER-SELECTED-INDICES* '())

  (if *DISPATCHER-GROUP-FILTER*
    (progn
      ;; без fboundp: пробуем вызвать и ловим ошибку
      (setq vis (vl-catch-all-apply 'tu-filtered-layer-names '()))
      (if (or (vl-catch-all-error-p vis) (null vis) (/= (type vis) 'LIST))
        (setq vis *DISPATCHER-ALL-LAYERS*)
      )
      (setq *DISPATCHER-VISIBLE-LAYERS* vis)
    )
    (setq *DISPATCHER-VISIBLE-LAYERS* *DISPATCHER-ALL-LAYERS*)
  )

  (setq *DISPATCHER-VISIBLE-LAYERS*
    (vl-remove-if-not '(lambda (x) (= (type x) 'STR))
                      (if (listp *DISPATCHER-VISIBLE-LAYERS*) *DISPATCHER-VISIBLE-LAYERS* '()))
  )

  (start_list "lst_layers")
  (mapcar 'add_list *DISPATCHER-VISIBLE-LAYERS*)
  (end_list)

  (setq i 0)
  (foreach item *DISPATCHER-VISIBLE-LAYERS*
    (if (vl-some '(lambda (x) (and (= (type x) 'STR) (= (type item) 'STR)
                                   (= (strcase x) (strcase item)))) restore)
      (setq *DISPATCHER-SELECTED-INDICES* (cons i *DISPATCHER-SELECTED-INDICES*))
    )
    (setq i (1+ i))
  )

  (setq selected "")
  (foreach i (reverse *DISPATCHER-SELECTED-INDICES*)
    (setq selected (if (= selected "") (itoa i) (strcat selected " " (itoa i))))
  )
  (set_tile "lst_layers" selected)
)

;; ---------- Чтение параметров ----------
(defun dispatcher-read-params ( / selected)
  (setq selected (dispatcher-selected-names))
  (setq *DISPATCHER-SELECTED-LAYERS* (if selected selected nil))
  (setq *DISPATCHER-REPORT-MODE* (if (= (get_tile "rb_summary") "1") "SUMMARY" "DETAIL"))
  (setq *DISPATCHER-EXPORT-EXCEL* (= (get_tile "chk_xls") "1"))
  (setq *DISPATCHER-EXPORT-TXT* (= (get_tile "chk_txt") "1"))
  (setq *DISPATCHER-CREATE-TABLE* (= (get_tile "chk_acad") "1"))
  (cond
    ((= (get_tile "rb_task_fasonka") "1")      (setq *DISPATCHER-TASK-ID* 'FASONKA))
    ((= (get_tile "rb_task_subsystem") "1")    (setq *DISPATCHER-TASK-ID* 'SUBSYSTEM))
    ((= (get_tile "rb_task_cladding") "1")     (setq *DISPATCHER-TASK-ID* 'CLADDING))
    ((= (get_tile "rb_task_vitrazh") "1")      (setq *DISPATCHER-TASK-ID* 'VITRAZH))
    ((= (get_tile "rb_task_zapolnenie") "1")   (setq *DISPATCHER-TASK-ID* 'ZAPOLNENIE))
    (t (setq *DISPATCHER-TASK-ID* 'FASONKA))
  )
  T
)

;; ---------- Диспетчер (без fboundp) ----------
(defun run-task (task-id layers report-mode export-excel export-txt create-table save-base / r)
  (cond
    ((eq task-id 'FASONKA)
     (setq r (vl-catch-all-apply 'fasonka-main
               (list layers report-mode export-excel export-txt create-table save-base)))
     (if (vl-catch-all-error-p r)
       (princ "\nМодуль Фасонка не загружен или ошибка выполнения.")))
    ((eq task-id 'SUBSYSTEM)
     (setq r (vl-catch-all-apply 'subsystem-main
               (list layers report-mode export-excel export-txt create-table save-base)))
     (if (vl-catch-all-error-p r) (princ "\nМодуль Подсистема не загружен.")))
    ((eq task-id 'CLADDING)
     (setq r (vl-catch-all-apply 'cladding-main
               (list layers report-mode export-excel export-txt create-table save-base)))
     (if (vl-catch-all-error-p r) (princ "\nМодуль Облицовка не загружен.")))
    ((eq task-id 'VITRAZH)
     (setq r (vl-catch-all-apply 'vitrazh-main
               (list layers report-mode export-excel export-txt create-table save-base)))
     (if (vl-catch-all-error-p r) (princ "\nМодуль Витраж не загружен.")))
    ((eq task-id 'ZAPOLNENIE)
     (setq r (vl-catch-all-apply 'zapolnenie-main
               (list layers report-mode export-excel export-txt create-table save-base)))
     (if (vl-catch-all-error-p r) (princ "\nМодуль Заполнение не загружен.")))
    (t (princ "\nНеизвестная задача."))
  )
)

;; ---------- Обработчики ----------
(defun dispatcher-help ()
  (alert (strcat "Окно запуска задач.\n\n"
                 "Выбор одной задачи, слоёв, режима отчёта и форматов вывода.\n"
                 "Кнопки Раскрой хлыста / Раскрой листа запускают модули раскроя.\n\n"
                 "Сохранить - имя файла по умолчанию.\n"
                 "Сохранить как... - выбор пути и имени.\n"
                 "Если слои не выбраны - поиск по всем слоям."))
)
(defun dispatcher-save   () (dispatcher-read-params) (setq *DISPATCHER-ACTION* 'SAVE)   (done_dialog 1))
(defun dispatcher-saveas () (dispatcher-read-params) (setq *DISPATCHER-ACTION* 'SAVEAS) (done_dialog 1))
(defun dispatcher-close  () (setq *DISPATCHER-ACTION* 'CANCEL) (done_dialog 0))
(defun dispatcher-nest1d () (setq *DISPATCHER-ACTION* 'NEST1D) (done_dialog 1))
(defun dispatcher-nest2d () (setq *DISPATCHER-ACTION* 'NEST2D) (done_dialog 1))
(defun dispatcher-group-filter ()
  (setq *DISPATCHER-SELECTED-LAYERS* (dispatcher-selected-names))
  (setq *DISPATCHER-GROUP-FILTER* (= (get_tile "chk_group_filter") "1"))
  (dispatcher-rebuild-layer-list)
)
(defun dispatcher-layer-selection ()
  (setq *DISPATCHER-SELECTED-LAYERS* (dispatcher-selected-names))
)

;; ---------- Основная команда ----------
(defun c:taskdispatcher ( / dcl-file save-base tdir r)
  (vl-load-com)
  (dispatcher-load-all)

  (setq dcl-file nil)
  (setq tdir (dispatcher-tasks-dir))
  (if tdir (setq dcl-file (findfile (strcat tdir "\\dispatcher.dcl"))))
  (if (null dcl-file) (setq dcl-file (findfile "dispatcher.dcl")))

  (if (null dcl-file)
    (progn (alert "Не найден файл dispatcher.dcl.") (princ))
    (progn
      (setq *DISPATCHER-ALL-LAYERS* (dsp-layer-names))
      (setq *DISPATCHER-VISIBLE-LAYERS* *DISPATCHER-ALL-LAYERS*)
      (setq *DISPATCHER-SELECTED-LAYERS* nil)
      (setq *DISPATCHER-GROUP-FILTER* nil)
      (setq *DISPATCHER-TASK-ID* 'FASONKA)
      (setq *DISPATCHER-REPORT-MODE* "DETAIL")
      (setq *DISPATCHER-EXPORT-EXCEL* T)
      (setq *DISPATCHER-EXPORT-TXT* nil)
      (setq *DISPATCHER-CREATE-TABLE* T)
      (setq *DISPATCHER-ACTION* 'CANCEL)

      (setq *DISPATCHER-DCL-ID* (load_dialog dcl-file))
      (if (< *DISPATCHER-DCL-ID* 0)
        (alert "Не удалось загрузить dispatcher.dcl.")
        (progn
          (if (new_dialog "task_dispatcher" *DISPATCHER-DCL-ID*)
            (progn
              (set_tile "rb_detail" "1")
              (set_tile "rb_summary" "0")
              (set_tile "chk_xls" "1")
              (set_tile "chk_txt" "0")
              (set_tile "chk_acad" "1")
              (set_tile "chk_group_filter" "0")
              (set_tile "rb_task_fasonka" "1")
              (mode_tile "rb_task_subsystem" 1)
              (mode_tile "rb_task_cladding" 1)
              (mode_tile "rb_task_vitrazh" 1)
              (mode_tile "rb_task_zapolnenie" 1)
              (mode_tile "lst_blocks" 1)

              (dispatcher-rebuild-layer-list)

              (action_tile "btn_help"   "(dispatcher-help)")
              (action_tile "btn_save"   "(dispatcher-save)")
              (action_tile "btn_saveas" "(dispatcher-saveas)")
              (action_tile "btn_close"  "(dispatcher-close)")
              (action_tile "btn_nest1d" "(dispatcher-nest1d)")
              (action_tile "btn_nest2d" "(dispatcher-nest2d)")
              (action_tile "chk_group_filter" "(dispatcher-group-filter)")
              (action_tile "lst_layers" "(dispatcher-layer-selection)")
              (action_tile "rb_detail"  "(setq *DISPATCHER-REPORT-MODE* \"DETAIL\")")
              (action_tile "rb_summary" "(setq *DISPATCHER-REPORT-MODE* \"SUMMARY\")")

              (start_dialog)

              (cond
                ((eq *DISPATCHER-ACTION* 'SAVE)
                 (run-task *DISPATCHER-TASK-ID* *DISPATCHER-SELECTED-LAYERS*
                           *DISPATCHER-REPORT-MODE* *DISPATCHER-EXPORT-EXCEL*
                           *DISPATCHER-EXPORT-TXT* *DISPATCHER-CREATE-TABLE* nil))
                ((eq *DISPATCHER-ACTION* 'SAVEAS)
                 (setq save-base (vl-catch-all-apply 'tu-get-save-base (list *DISPATCHER-TASK-ID*)))
                 (if (vl-catch-all-error-p save-base) (setq save-base nil))
                 (run-task *DISPATCHER-TASK-ID* *DISPATCHER-SELECTED-LAYERS*
                           *DISPATCHER-REPORT-MODE* *DISPATCHER-EXPORT-EXCEL*
                           *DISPATCHER-EXPORT-TXT* *DISPATCHER-CREATE-TABLE* save-base))
                ((eq *DISPATCHER-ACTION* 'NEST1D)
                 (setq r (vl-catch-all-apply 'c:NEST1DS '()))
                 (if (vl-catch-all-error-p r)
                   (princ "\nNEST1DS не загружен. Загрузите nest1ds.lsp.")))
                ((eq *DISPATCHER-ACTION* 'NEST2D)
                 (setq r (vl-catch-all-apply 'c:NEST2DS '()))
                 (if (vl-catch-all-error-p r)
                   (princ "\nРаскрой листа: модуль в разработке (NEST2DS).")))
                (t nil))
            )
            (alert "Не удалось создать диалог task_dispatcher.")
          )
          (unload_dialog *DISPATCHER-DCL-ID*)
          (setq *DISPATCHER-DCL-ID* nil)
        )
      )
    )
  )
  (princ)
)

(defun c:tasks () (c:taskdispatcher))

(princ "\nDISPATCHER.LSP загружен. Команды: TASKDISPATCHER, TASKS")
(princ)