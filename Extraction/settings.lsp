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
  (if (eq task 'CLADDING)
    '("*КАССЕТА*" "*ПАНЕЛЬ*")
    '("*")
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

(defun ae-settings-ui-open (/ root dcl-file dcl-id i)
  (if (not *AE-SETTINGS-LOADED*) (ae-settings-load))
  (setq *AE-SETTINGS-UI-ACTION* 'CANCEL)
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
              (action_tile "btn_save" "(ae-settings-ui-save)")
              (action_tile "btn_cancel" "(ae-settings-ui-cancel)")
              (start_dialog)
              (unload_dialog dcl-id)
              (= *AE-SETTINGS-UI-ACTION* 'SAVE)
            )
            (progn
              (unload_dialog dcl-id)
              (alert "Не удалось открыть settings_dialog.")
              nil
            )
          )
        )
      )
    )
  )
)

(princ "\nSETTINGS.LSP загружен.")
(princ)
