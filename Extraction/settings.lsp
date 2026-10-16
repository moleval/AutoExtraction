;;; ============================================================
;;; Extraction/settings.lsp — окно постоянных настроек
;;; ============================================================
(vl-load-com)

(setq *AE-SETTINGS-UI-TASKS*
  '(FASONKA SUBSYSTEM CLADDING VITRAZH ZAPOLNENIE CUTLINE CUTSHEET))
(setq *AE-SETTINGS-UI-LABELS*
  '("Фасонка" "Подсистема" "Облицовка" "Витраж"
    "Заполнение" "Раскрой хлыста" "Раскрой листа"))
(setq *AE-SETTINGS-UI-TASK* 'FASONKA)
(setq *AE-SETTINGS-UI-ACTION* 'CANCEL)
(setq *AE-SETTINGS-UI-PLUGINS* '())          ; буфер реестра плагинов: ((имя путь) ...)
(setq *AE-SETTINGS-UI-PLUGIN-SEL* nil)       ; индекс выбранного плагина или nil
(setq *AE-SETTINGS-UI-SNAPSHOT* nil)         ; память настроек на момент открытия

(defun ae-settings-ui-index-of (task / i item result)
  (setq i 0 result 0)
  (foreach item *AE-SETTINGS-UI-TASKS*
    (if (eq item task) (setq result i))
    (setq i (1+ i))
  )
  result
)

(defun ae-settings-ui-task-at (index)
  (if (and (numberp index)
           (>= index 0)
           (< index (length *AE-SETTINGS-UI-TASKS*)))
    (nth index *AE-SETTINGS-UI-TASKS*)
    'FASONKA
  )
)

(defun ae-settings-ui-set (key value)
  (set_tile key (if (= (type value) 'STR) value ""))
)

(defun ae-settings-ui-list-text (values)
  (ae-settings-join-list values)
)

(defun ae-settings-ui-default-layers (task)
  (cond
    ((eq task 'FASONKA) '("Фасонка*" "Железо*"))
    ((eq task 'SUBSYSTEM)
     '("Подсистема" "Подсистема алюминиевая" "Подсистема оцинкованная"))
    ((eq task 'VITRAZH) '("Витражи" "Стойк*" "Ригел*"))
    ((eq task 'ZAPOLNENIE) '("Заполнение" "Стекло" "Обозначение ст-т"))
    (T '("*"))
  )
)

(defun ae-settings-ui-default-blocks (task)
  (cond
    ((eq task 'CLADDING) '("*КАССЕТА*" "*ПАНЕЛЬ*"))
    ;; Заполнение: «Заполнение», «Стекло», «Сэндвич» в любом регистре
    ((eq task 'ZAPOLNENIE) '("*аполнение*" "*текл*" "*эндвич*"))
    (T '("*"))
  )
)

(defun ae-settings-ui-default-polyline-layers ()
  '("*Облицовка*" "*Кассет*" "*Керамогранит*")
)

(defun ae-settings-ui-default-block-layers ()
  '("*Облицовка*" "*Кассет*" "*Керамогранит*")
)

(defun ae-settings-ui-load-task (/ task section layers blocks)
  (setq task *AE-SETTINGS-UI-TASK*
        section (strcat "task." (strcase (vl-princ-to-string task))))
  (if (= (strcase (vl-princ-to-string task)) "CLADDING")
    (progn
      (ae-settings-ui-set "edt_layers" "")
      (ae-settings-ui-set "edt_blocks" "")
      (ae-settings-ui-set "edt_poly_layers"
        (ae-settings-ui-list-text
          (ae-settings-task-polyline-layers 'CLADDING)))
      (ae-settings-ui-set "edt_block_layers"
        (ae-settings-ui-list-text
          (ae-settings-task-block-layers 'CLADDING)))
      (ae-settings-ui-set "edt_block_names"
        (ae-settings-ui-list-text
          (ae-settings-task-blocks 'CLADDING)))
    )
    (progn
      (setq layers
        (ae-settings-list section "input.layer"
          (ae-settings-ui-default-layers task)))
      (setq blocks
        (ae-settings-list section "input.block"
          (ae-settings-ui-default-blocks task)))
      (ae-settings-ui-set "edt_layers"
        (ae-settings-ui-list-text layers))
      (ae-settings-ui-set "edt_blocks"
        (ae-settings-ui-list-text blocks))
      (ae-settings-ui-set "edt_poly_layers" "")
      (ae-settings-ui-set "edt_block_layers" "")
      (ae-settings-ui-set "edt_block_names" "")
    )
  )

  ;; Припуск на раму — только у ЗАПОЛНЕНИЯ, у прочих задач поле пустое
  (ae-settings-ui-set "edt_frame_allowance"
    (if (eq task 'ZAPOLNENIE) (itoa (ae-settings-frame-allowance)) ""))

  (if (member task '(FASONKA SUBSYSTEM CLADDING VITRAZH ZAPOLNENIE))
    (ae-settings-ui-set "edt_table_layer"
      (ae-settings-output task "output.table.layer" "CURRENT"))
    (ae-settings-ui-set "edt_table_layer" "")
  )

  (if (member task '(CUTLINE CUTSHEET))
    (progn
      (ae-settings-ui-set "edt_block_template"
        (ae-settings-output task "output.block.template"
          (if (eq task 'CUTSHEET) "Раскрой листа {DWG}" "Раскрой {DWG}")))
      (ae-settings-ui-set "edt_insert_layer"
        (ae-settings-output task "output.insert.layer" "CURRENT"))
      (ae-settings-ui-set "edt_frame_layer"
        (ae-settings-output task "output.frame.layer" "Невидимые"))
    )
    (progn
      (ae-settings-ui-set "edt_block_template" "")
      (ae-settings-ui-set "edt_insert_layer" "")
      (ae-settings-ui-set "edt_frame_layer" "")
    )
  )

  ;; Сначала отключаем нерелевантные поля, чтобы окно не создавало
  ;; ложного впечатления, что эти параметры применяются к каждой задаче.
  (mode_tile "edt_layers" (if (eq task 'CLADDING) 1 0))
  (mode_tile "edt_blocks" (if (eq task 'CLADDING) 1 0))
  (mode_tile "edt_poly_layers" (if (eq task 'CLADDING) 0 1))
  (mode_tile "edt_block_layers" (if (eq task 'CLADDING) 0 1))
  (mode_tile "edt_block_names" (if (eq task 'CLADDING) 0 1))
  (mode_tile "edt_frame_allowance" (if (eq task 'ZAPOLNENIE) 0 1))
  (mode_tile "edt_table_layer"
    (if (member task '(FASONKA SUBSYSTEM CLADDING VITRAZH ZAPOLNENIE)) 0 1))
  (mode_tile "edt_block_template" (if (member task '(CUTLINE CUTSHEET)) 0 1))
  (mode_tile "edt_insert_layer" (if (member task '(CUTLINE CUTSHEET)) 0 1))
  (mode_tile "edt_frame_layer" (if (member task '(CUTLINE CUTSHEET)) 0 1))
)

(defun ae-settings-ui-task-changed (value / index)
  (setq index (atoi value))
  (setq *AE-SETTINGS-UI-TASK* (ae-settings-ui-task-at index))
  (ae-settings-ui-load-task)
)

(defun ae-settings-ui-reset-task (/ task section)
  (setq task *AE-SETTINGS-UI-TASK*
        section (strcat "task." (strcase (vl-princ-to-string task))))
  (cond
    ((eq task 'CLADDING)
     (ae-settings-set-list section "input.polyline.layer"
       (ae-settings-ui-default-polyline-layers))
     (ae-settings-set-list section "input.block.layer"
       (ae-settings-ui-default-block-layers))
     (ae-settings-set-list section "input.block.name"
       (ae-settings-ui-default-blocks 'CLADDING))
     (ae-settings-set section "output.table.layer" "CURRENT"))
    ((member task '(FASONKA SUBSYSTEM VITRAZH ZAPOLNENIE))
     (ae-settings-set-list section "input.layer"
       (ae-settings-ui-default-layers task))
     (ae-settings-set-list section "input.block"
       (ae-settings-ui-default-blocks task))
     (ae-settings-set section "output.table.layer" "CURRENT"))
    ((eq task 'CUTLINE)
     (ae-settings-set-list section "input.layer" '("*"))
     (ae-settings-set-list section "input.block" '("*"))
     (ae-settings-set section "output.block.template" "Раскрой {DWG}")
     (ae-settings-set section "output.insert.layer" "CURRENT")
     (ae-settings-set section "output.frame.layer" "Невидимые"))
    ((eq task 'CUTSHEET)
     (ae-settings-set-list section "input.layer" '("*"))
     (ae-settings-set-list section "input.block" '("*"))
     (ae-settings-set section "output.block.template" "Раскрой листа {DWG}")
     (ae-settings-set section "output.insert.layer" "CURRENT")
     (ae-settings-set section "output.frame.layer" "Невидимые"))
  )
  (ae-settings-ui-load-task)
)

(defun ae-settings-ui-template-valid-p (value / text forbidden invalid item)
  (setq text (ae-settings-trim value) invalid nil)
  (setq forbidden
    (list "<" ">" "/" (chr 92) (chr 34) ":" ";" "?" "*" "|" "," "="))
  (foreach item forbidden
    (if (vl-string-search item text) (setq invalid T))
  )
  (and (> (strlen text) 0)
       (/= (substr text 1 1) "*")
       (not invalid))
)

(defun ae-settings-ui-save-task (/ task section values ok)
  (setq task *AE-SETTINGS-UI-TASK*
        section (strcat "task." (strcase (vl-princ-to-string task)))
        ok T)
  (cond
    ((eq task 'CLADDING)
     (setq values (ae-settings-split-list (get_tile "edt_poly_layers")))
     (if (null values) (setq ok nil)
       (ae-settings-set-list section "input.polyline.layer" values))
     (setq values (ae-settings-split-list (get_tile "edt_block_layers")))
     (if (null values) (setq ok nil)
       (ae-settings-set-list section "input.block.layer" values))
     (setq values (ae-settings-split-list (get_tile "edt_block_names")))
     (if (null values) (setq ok nil)
       (ae-settings-set-list section "input.block.name" values))
     (ae-settings-set section "output.table.layer"
       (ae-settings-trim (get_tile "edt_table_layer")))
    )
    ((member task '(FASONKA SUBSYSTEM VITRAZH ZAPOLNENIE CUTLINE CUTSHEET))
     (setq values (ae-settings-split-list (get_tile "edt_layers")))
     (if (null values) (setq ok nil)
       (ae-settings-set-list section "input.layer" values))
     (setq values (ae-settings-split-list (get_tile "edt_blocks")))
     (if (null values) (setq ok nil)
       (ae-settings-set-list section "input.block" values))
     ;; Припуск: целое неотрицательное. Мусор или минус — отказ
     ;; сохранения, иначе заготовка окажется меньше проёма.
     (if (eq task 'ZAPOLNENIE)
       (progn
         (setq values (ae-settings-trim (get_tile "edt_frame_allowance")))
         (if (and (= (type values) 'STR)
                  (/= values "")
                  (= values (itoa (atoi values)))
                  (>= (atoi values) 0))
           (ae-settings-set section "input.frame.allowance" values)
           (progn
             (setq ok nil)
             (alert "[AutoExtraction][SETTINGS][VALIDATION] Припуск на раму: нужно целое число не меньше нуля."))))
     )
     (if (member task '(FASONKA SUBSYSTEM VITRAZH ZAPOLNENIE))
       (ae-settings-set section "output.table.layer"
         (ae-settings-trim (get_tile "edt_table_layer")))
     )
     (if (member task '(CUTLINE CUTSHEET))
       (progn
         (ae-settings-set section "output.block.template"
           (ae-settings-trim (get_tile "edt_block_template")))
         (ae-settings-set section "output.insert.layer"
           (ae-settings-trim (get_tile "edt_insert_layer")))
         (ae-settings-set section "output.frame.layer"
           (ae-settings-trim (get_tile "edt_frame_layer")))
       )
     )
    )
  )
  (if (or (= (ae-settings-trim (get_tile "edt_table_layer")) "")
          (= (ae-settings-trim (get_tile "edt_insert_layer")) ""))
    ;; Пустое поле означает прежнее безопасное поведение.
    (progn
      (if (member task '(FASONKA SUBSYSTEM CLADDING VITRAZH ZAPOLNENIE))
        (ae-settings-set section "output.table.layer" "CURRENT"))
      (if (member task '(CUTLINE CUTSHEET))
        (ae-settings-set section "output.insert.layer" "CURRENT"))
    )
  )
  (if (= (ae-settings-trim (get_tile "edt_frame_layer")) "")
    (if (member task '(CUTLINE CUTSHEET))
      (ae-settings-set section "output.frame.layer" "Невидимые"))
  )
  (if (= (ae-settings-trim (get_tile "edt_block_template")) "")
    (if (eq task 'CUTLINE)
      (ae-settings-set section "output.block.template" "Раскрой {DWG}")
      (if (eq task 'CUTSHEET)
        (ae-settings-set section "output.block.template" "Раскрой листа {DWG}")))
  )
  (if (and (member task '(CUTLINE CUTSHEET))
           (not (ae-settings-ui-template-valid-p
                  (get_tile "edt_block_template"))))
    (setq ok nil)
  )
  ok
)

(defun ae-settings-ui-save ()
  (if (ae-settings-ui-save-task)
    (if (ae-settings-write)
      (progn
        (setq *AE-SETTINGS-UI-ACTION* 'SAVE)
        (done_dialog 1))
      (alert "Не удалось записать AutoExtraction\\settings.ini.")
    )
    (alert "Нужно задать хотя бы одну маску в каждом активном поле.")
  )
)

(defun ae-settings-ui-cancel ()
  (setq *AE-SETTINGS-UI-ACTION* 'CANCEL)
  (done_dialog 0)
)

(defun ae-settings-ui-save-plugin-rec (name path)
  (list name path)
)

(defun ae-settings-ui-plugin-name (path)
  ;; Имя файла плагина: база пути + .lsp (фильтр выбора только lsp).
  (strcat (vl-filename-base path) ".lsp")
)

(defun ae-settings-ui-plugin-index-of (name / i found)
  (setq i 0 found nil)
  (foreach rec *AE-SETTINGS-UI-PLUGINS*
    (if (and (null found) (= (strcase (car rec)) (strcase name)))
      (setq found i))
    (setq i (1+ i))
  )
  found
)

(defun ae-settings-ui-plugin-found-p (rec)
  ;; Тот же резолвер, что у RELOAD: путь из реестра или Plugins\<имя>.
  (if (ae-reload-plugin-resolve (car rec) (cadr rec)) T nil)
)

(defun ae-settings-ui-plugin-line (rec)
  (strcat (car rec) "  -  "
          (if (> (strlen (cadr rec)) 0) (cadr rec) "(путь не задан, Plugins\\)")
          (if (ae-settings-ui-plugin-found-p rec) "" "   [НЕ НАЙДЕН]"))
)

(defun ae-settings-ui-plugin-status ( / rec n missing)
  (setq n (length *AE-SETTINGS-UI-PLUGINS*))
  (setq missing 0)
  (foreach rec *AE-SETTINGS-UI-PLUGINS*
    (if (not (ae-settings-ui-plugin-found-p rec))
      (setq missing (1+ missing))))
  (set_tile "txt_plugin_status"
    (cond
      ((= n 0) "Плагинов нет. Кнопка «Добавить…» выбирает файл .lsp.")
      ((null *AE-SETTINGS-UI-PLUGIN-SEL*)
       (strcat "Плагинов: " (itoa n) ". Не найдено: " (itoa missing) ". Выберите строку."))
      (T
       (setq rec (nth *AE-SETTINGS-UI-PLUGIN-SEL* *AE-SETTINGS-UI-PLUGINS*))
       (if (ae-settings-ui-plugin-found-p rec)
         (strcat "Статус: найден. Файл: " (cadr rec))
         (strcat "Статус: НЕ НАЙДЕН по пути " (cadr rec)
                 ". Замените путь кнопкой «Изменить путь…».")))))
)

(defun ae-settings-ui-plugin-fill (/ rec)
  (start_list "lst_plugins")
  (foreach rec *AE-SETTINGS-UI-PLUGINS*
    (add_list (ae-settings-ui-plugin-line rec)))
  (end_list)
  (if (and *AE-SETTINGS-UI-PLUGIN-SEL*
           (< *AE-SETTINGS-UI-PLUGIN-SEL* (length *AE-SETTINGS-UI-PLUGINS*)))
    (set_tile "lst_plugins" (itoa *AE-SETTINGS-UI-PLUGIN-SEL*))
  )
  (ae-settings-ui-plugin-status)
)

(defun ae-settings-ui-plugin-select (value)
  (setq *AE-SETTINGS-UI-PLUGIN-SEL*
    (if (and (= (type value) 'STR) (/= value ""))
      (atoi value)
      nil))
  (ae-settings-ui-plugin-status)
)

(defun ae-settings-ui-plugin-add (/ path name idx rec)
  (setq path (getfiled "Выбор файла плагина" "" "lsp" 0))
  (if path
    (progn
      (setq name (ae-settings-ui-plugin-name path))
      (setq idx (ae-settings-ui-plugin-index-of name))
      (setq rec (ae-settings-ui-save-plugin-rec name path))
      (if idx
        ;; Одно имя - одна запись: второй файл с тем же именем заменяет путь.
        (setq *AE-SETTINGS-UI-PLUGINS*
          (ae-settings-ui-plugin-replace-at idx rec))
        (setq *AE-SETTINGS-UI-PLUGINS*
          (append *AE-SETTINGS-UI-PLUGINS* (list rec))))
      (setq *AE-SETTINGS-UI-PLUGIN-SEL*
        (ae-settings-ui-plugin-index-of name))
      (ae-settings-ui-plugin-fill)
    )
  )
)

(defun ae-settings-ui-plugin-replace-at (idx rec / i out)
  (setq i 0 out '())
  (foreach item *AE-SETTINGS-UI-PLUGINS*
    (setq out (cons (if (= i idx) rec item) out))
    (setq i (1+ i)))
  (reverse out)
)

(defun ae-settings-ui-plugin-change (/ path name cur out i)
  (setq cur *AE-SETTINGS-UI-PLUGIN-SEL*)
  (if (null cur)
    (alert "Выберите плагин в списке.")
    (progn
      (setq path (getfiled "Выбор файла плагина" "" "lsp" 0))
      (if path
        (progn
          (setq name (ae-settings-ui-plugin-name path))
          ;; Одно имя - одна запись: чужая запись с тем же именем уходит,
          ;; текущая получает новое имя и путь.
          (setq out '() i 0)
          (foreach item *AE-SETTINGS-UI-PLUGINS*
            (cond
              ((= i cur) (setq out (cons (ae-settings-ui-save-plugin-rec name path) out)))
              ((= (strcase (car item)) (strcase name)) nil)
              (T (setq out (cons item out))))
            (setq i (1+ i)))
          (setq *AE-SETTINGS-UI-PLUGINS* (reverse out))
          (setq *AE-SETTINGS-UI-PLUGIN-SEL* (ae-settings-ui-plugin-index-of name))
          (ae-settings-ui-plugin-fill)
        )
      )
    )
  )
)

(defun ae-settings-ui-plugin-remove-at (idx / i out)
  (setq i 0 out '())
  (foreach item *AE-SETTINGS-UI-PLUGINS*
    (if (/= i idx) (setq out (cons item out)))
    (setq i (1+ i)))
  (reverse out)
)

(defun ae-settings-ui-plugin-remove ()
  (if (null *AE-SETTINGS-UI-PLUGIN-SEL*)
    (alert "Выберите плагин в списке.")
    (progn
      (setq *AE-SETTINGS-UI-PLUGINS*
        (ae-settings-ui-plugin-remove-at *AE-SETTINGS-UI-PLUGIN-SEL*))
      (setq *AE-SETTINGS-UI-PLUGIN-SEL* nil)
      (ae-settings-ui-plugin-fill)
    )
  )
)

(defun ae-settings-ui-run-plugins (dcl-id / snap result)
  ;; Диалог плагинов: ОК переносит буфер в настройки, Отмена откатывает буфер.
  (setq snap *AE-SETTINGS-UI-PLUGINS*)
  (setq *AE-SETTINGS-UI-PLUGIN-SEL* nil)
  (if (new_dialog "plugins_dialog" dcl-id)
    (progn
      (ae-settings-ui-plugin-fill)
      (action_tile "lst_plugins" "(ae-settings-ui-plugin-select $value)")
      (action_tile "btn_plugin_add" "(ae-settings-ui-plugin-add)")
      (action_tile "btn_plugin_change" "(ae-settings-ui-plugin-change)")
      (action_tile "btn_plugin_remove" "(ae-settings-ui-plugin-remove)")
      (action_tile "btn_plugins_ok" "(done_dialog 1)")
      (action_tile "btn_plugins_cancel" "(done_dialog 0)")
      (setq result (start_dialog))
      (if (/= result 1)
        (setq *AE-SETTINGS-UI-PLUGINS* snap))
    )
    (progn
      (alert "Не удалось открыть plugins_dialog.")
      (setq *AE-SETTINGS-UI-PLUGINS* snap)
    )
  )
  result
)

(defun ae-settings-ui-run-settings (dcl-id / result)
  (if (new_dialog "settings_dialog" dcl-id)
    (progn
      (start_list "popup_task")
      (foreach item *AE-SETTINGS-UI-LABELS* (add_list item))
      (end_list)
      (set_tile "popup_task"
        (itoa (ae-settings-ui-index-of *AE-SETTINGS-UI-TASK*)))
      (ae-settings-ui-load-task)
      (action_tile "popup_task" "(ae-settings-ui-task-changed $value)")
      (action_tile "btn_defaults" "(ae-settings-ui-reset-task)")
      (action_tile "btn_plugins" "(ae-settings-ui-goto-plugins)")
      (action_tile "btn_save" "(ae-settings-ui-save)")
      (action_tile "btn_cancel" "(ae-settings-ui-cancel)")
      (setq result (start_dialog))
      result
    )
    (progn
      (alert "Не удалось открыть settings_dialog.")
      0
    )
  )
)

(defun ae-settings-ui-goto-plugins ()
  ;; Правки полей переносятся в память, затем окно закрывается с кодом 2.
  (if (ae-settings-ui-save-task)
    (progn
      (setq *AE-SETTINGS-UI-ACTION* 'PLUGINS)
      (done_dialog 2))
    (alert "Нужно задать хотя бы одну маску в каждом активном поле.")
  )
)

(defun ae-settings-ui-open (/ root dcl-file dcl-id result again)
  (if (not *AE-SETTINGS-LOADED*) (ae-settings-load))
  (setq *AE-SETTINGS-UI-ACTION* 'CANCEL)
  (setq *AE-SETTINGS-UI-SNAPSHOT* *AE-SETTINGS-DATA*)
  (setq *AE-SETTINGS-UI-PLUGINS* (ae-settings-plugin-list))
  (setq root (ae-settings-root))
  (setq dcl-file
    (if root (findfile (strcat root "\\Extraction\\settings.dcl")) nil))
  (if (null dcl-file) (setq dcl-file (findfile "settings.dcl")))
  (if (null dcl-file)
    (progn
      (alert "Не найден Extraction\\settings.dcl.")
      nil
    )
    (progn
      (setq dcl-id (load_dialog dcl-file))
      (if (< dcl-id 0)
        (progn (alert "Не удалось загрузить settings.dcl.") nil)
        (progn
          ;; Цикл: окно настроек <-> диалог плагинов (вложенные диалоги
          ;; AutoCAD не поддерживает, поэтому окна открываются по очереди).
          (setq again T)
          (while again
            (setq result (ae-settings-ui-run-settings dcl-id))
            (if (= result 2)
              (ae-settings-ui-run-plugins dcl-id)
              (setq again nil)
            )
          )
          (unload_dialog dcl-id)
          (if (/= *AE-SETTINGS-UI-ACTION* 'SAVE)
            (setq *AE-SETTINGS-DATA* *AE-SETTINGS-UI-SNAPSHOT*))
          (= *AE-SETTINGS-UI-ACTION* 'SAVE)
        )
      )
    )
  )
)
(princ "\nSETTINGS.LSP загружен.")
(princ)
