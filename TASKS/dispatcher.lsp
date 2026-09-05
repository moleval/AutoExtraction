;;; ============================================================
;;; dispatcher.lsp
;;; Окно запуска задач + единый диспетчер run-task
;;; ============================================================

(vl-load-com)

(setq *DISPATCHER-DCL-ID* nil)
(setq *DISPATCHER-ALL-LAYERS* nil)
(setq *DISPATCHER-VISIBLE-LAYERS* nil)
(setq *DISPATCHER-SELECTED-LAYERS* nil)
(setq *DISPATCHER-GROUP-FILTER* nil)
(setq *DISPATCHER-TASK-ID* 'FASONKA)
(setq *DISPATCHER-REPORT-MODE* "DETAIL")
(setq *DISPATCHER-EXPORT-EXCEL* T)
(setq *DISPATCHER-EXPORT-TXT* nil)
(setq *DISPATCHER-CREATE-TABLE* T)
(setq *DISPATCHER-ACTION* 'CANCEL)

;; ------------------------------------------------------------
;; Загрузка модулей из каталога dispatcher.lsp
;; ------------------------------------------------------------

(defun dispatcher-load-module (path)
  (if (and path (findfile path))
    (load path)
    nil
  )
)

(defun dispatcher-load-all ( / dispatcher-file tasks-root project-root files f)
  ;; dispatcher.lsp находится в TASKS, а common — соседний каталог
  ;; на уровне корня проекта.
  (setq dispatcher-file (findfile "dispatcher.lsp"))

  (if dispatcher-file
    (progn
      (setq tasks-root (vl-filename-directory dispatcher-file))
      (setq project-root (vl-filename-directory tasks-root))

      (setq files
        (list
          (strcat project-root "\\common\\task-utils.lsp")
          (strcat project-root "\\common\\layer-utils.lsp")
          (strcat project-root "\\common\\excel-utils.lsp")
          (strcat project-root "\\common\\table-utils.lsp")
          (strcat tasks-root "\\fasonka.lsp")
        )
      )

      (foreach f files
        (dispatcher-load-module f)
      )
    )
  )
)

;; ------------------------------------------------------------
;; Преобразование строки индексов DCL list_box в список индексов
;; ------------------------------------------------------------

(defun dispatcher-parse-indices (s / x)
  (if (and s (/= s ""))
    (progn
      (setq x (read (strcat "(" s ")")))
      (if (= (type x) 'LIST)
        x
        nil
      )
    )
    nil
  )
)

(defun dispatcher-selected-names ( / s indices out i)
  (setq s (get_tile "lst_layers"))
  (setq indices (dispatcher-parse-indices s))
  (setq out '())

  (foreach i indices
    (if (and (>= i 0)
             (< i (length *DISPATCHER-VISIBLE-LAYERS*)))
      (setq out
        (cons
          (nth i *DISPATCHER-VISIBLE-LAYERS*)
          out
        )
      )
    )
  )

  (tu-sort-strings-ci
    (tu-list-unique-ci out)
  )
)

;; ------------------------------------------------------------
;; Заполнение list_box
;; ------------------------------------------------------------

(defun dispatcher-fill-layer-list
       (layers restore-names / i item selected)
  (setq *DISPATCHER-VISIBLE-LAYERS* layers)

  (start_list "lst_layers")
  (mapcar 'add_list layers)
  (end_list)

  ;; Восстанавливаем выбор по именам.
  (setq i 0)
  (foreach item layers
    (setq selected nil)

    (foreach name restore-names
      (if (= (strcase name) (strcase item))
        (setq selected T)
      )
    )

    (if selected
      (setq
        ;; DCL set_tile для list_box с multiple_select принимает
        ;; строку индексов. Соберём её ниже.
        *DISPATCHER-SELECTED-INDICES*
        (cons i *DISPATCHER-SELECTED-INDICES*)
      )
    )

    (setq i (1+ i))
  )

  (if (boundp '*DISPATCHER-SELECTED-INDICES*)
    (progn
      (setq selected "")
      (foreach i (reverse *DISPATCHER-SELECTED-INDICES*)
        (setq selected
          (if (= selected "")
            (itoa i)
            (strcat selected " " (itoa i))
          )
        )
      )
      (set_tile "lst_layers" selected)
    )
  )
)

;; ------------------------------------------------------------
;; Более надёжная версия заполнения списка.
;; ------------------------------------------------------------

(defun dispatcher-rebuild-layer-list ( / restore i selected indices)
  (setq restore
    (dispatcher-selected-names)
  )

  (setq *DISPATCHER-SELECTED-INDICES* '())

  (if *DISPATCHER-GROUP-FILTER*
    (setq *DISPATCHER-VISIBLE-LAYERS*
      (tu-filtered-layer-names)
    )
    (setq *DISPATCHER-VISIBLE-LAYERS*
      *DISPATCHER-ALL-LAYERS*
    )
  )

  (if (and *DISPATCHER-GROUP-FILTER*
           (null *DISPATCHER-VISIBLE-LAYERS*))
    (progn
      (setq *DISPATCHER-VISIBLE-LAYERS*
        *DISPATCHER-ALL-LAYERS*
      )
      (alert
        "Слои по выбранному фильтру не найдены.\nПоказаны все слои."
      )
    )
  )

  (start_list "lst_layers")
  (mapcar 'add_list *DISPATCHER-VISIBLE-LAYERS*)
  (end_list)

  (setq i 0)
  (foreach item *DISPATCHER-VISIBLE-LAYERS*
    (if
      (vl-some
        '(lambda (x)
           (= (strcase x) (strcase item))
         )
        restore
      )
      (setq *DISPATCHER-SELECTED-INDICES*
        (cons i *DISPATCHER-SELECTED-INDICES*)
      )
    )
    (setq i (1+ i))
  )

  (setq selected "")
  (foreach i (reverse *DISPATCHER-SELECTED-INDICES*)
    (setq selected
      (if (= selected "")
        (itoa i)
        (strcat selected " " (itoa i))
      )
    )
  )
  (set_tile "lst_layers" selected)
)

;; ------------------------------------------------------------
;; Чтение всех параметров окна
;; ------------------------------------------------------------

(defun dispatcher-read-params ( / selected)
  (setq selected (dispatcher-selected-names))

  (setq *DISPATCHER-SELECTED-LAYERS*
    (if selected selected nil)
  )

  (setq *DISPATCHER-REPORT-MODE*
    (if (= (get_tile "rb_summary") "1")
      "SUMMARY"
      "DETAIL"
    )
  )

  (setq *DISPATCHER-EXPORT-EXCEL*
    (= (get_tile "chk_xls") "1")
  )

  (setq *DISPATCHER-EXPORT-TXT*
    (= (get_tile "chk_txt") "1")
  )

  (setq *DISPATCHER-CREATE-TABLE*
    (= (get_tile "chk_acad") "1")
  )

  (cond
    ((= (get_tile "rb_task_fasonka") "1")
     (setq *DISPATCHER-TASK-ID* 'FASONKA))

    ((= (get_tile "rb_task_subsystem") "1")
     (setq *DISPATCHER-TASK-ID* 'SUBSYSTEM))

    ((= (get_tile "rb_task_cladding") "1")
     (setq *DISPATCHER-TASK-ID* 'CLADDING))

    ((= (get_tile "rb_task_vitrazh") "1")
     (setq *DISPATCHER-TASK-ID* 'VITRAZH))

    ((= (get_tile "rb_task_steklopakety") "1")
     (setq *DISPATCHER-TASK-ID* 'STEKLOPAKETY))

    (t
     (setq *DISPATCHER-TASK-ID* 'FASONKA)
    )
  )

  T
)

;; ------------------------------------------------------------
;; Единый диспетчер
;; ------------------------------------------------------------

(defun run-task
       (task-id layers report-mode export-excel export-txt create-table save-base)

  (cond
    ((eq task-id 'FASONKA)
     (if (fboundp 'fasonka-main)
       (fasonka-main
         layers
         report-mode
         export-excel
         export-txt
         create-table
         save-base
       )
       (princ "\nМодуль Фасонка не загружен.")
     )
    )

    ((eq task-id 'SUBSYSTEM)
     (if (fboundp 'subsystem-main)
       (subsystem-main
         layers report-mode export-excel export-txt create-table save-base
       )
       (princ "\nМодуль Подсистема не загружен.")
     )
    )

    ((eq task-id 'CLADDING)
     (if (fboundp 'cladding-main)
       (cladding-main
         layers report-mode export-excel export-txt create-table save-base
       )
       (princ "\nМодуль Облицовка не загружен.")
     )
    )

    ((eq task-id 'VITRAZH)
     (if (fboundp 'vitrazh-main)
       (vitrazh-main
         layers report-mode export-excel export-txt create-table save-base
       )
       (princ "\nМодуль Витраж не загружен.")
     )
    )

    ((eq task-id 'STEKLOPAKETY)
     (if (fboundp 'steklopakety-main)
       (steklopakety-main
         layers report-mode export-excel export-txt create-table save-base
       )
       (princ "\nМодуль Стеклопакеты не загружен.")
     )
    )

    (t
     (princ "\nНеизвестная задача.")
    )
  )
)

;; ------------------------------------------------------------
;; Обработчики кнопок
;; ------------------------------------------------------------

(defun dispatcher-help ()
  (alert
    (strcat
      "Окно запуска задач.\n\n"
      "Позволяет выбрать одну задачу, слои, режим отчёта "
      "и форматы вывода.\n\n"
      "Сохранить — запуск с именем файла по умолчанию.\n"
      "Сохранить как... — выбор пути и имени.\n\n"
      "Если слои не выбраны — поиск выполняется по всем слоям."
    )
  )
)

(defun dispatcher-save ()
  (dispatcher-read-params)
  (setq *DISPATCHER-ACTION* 'SAVE)
  (done_dialog 1)
)

(defun dispatcher-saveas ()
  (dispatcher-read-params)
  (setq *DISPATCHER-ACTION* 'SAVEAS)
  (done_dialog 1)
)

(defun dispatcher-close ()
  (setq *DISPATCHER-ACTION* 'CANCEL)
  (done_dialog 0)
)

(defun dispatcher-group-filter ()
  ;; Сначала фиксируем выбор в текущем видимом списке,
  ;; затем меняем фильтр.
  (setq *DISPATCHER-SELECTED-LAYERS*
    (dispatcher-selected-names)
  )

  (setq *DISPATCHER-GROUP-FILTER*
    (= (get_tile "chk_group_filter") "1")
  )

  (dispatcher-rebuild-layer-list)
)

(defun dispatcher-layer-selection ()
  ;; Сохраняем выбор по именам, а не по индексам.
  (setq *DISPATCHER-SELECTED-LAYERS*
    (dispatcher-selected-names)
  )
)

;; ------------------------------------------------------------
;; Основная команда окна
;; ------------------------------------------------------------

(defun c:taskdispatcher
       ( / dcl-file action save-base result)

  (vl-load-com)
  (dispatcher-load-all)

  (setq dcl-file
    (findfile
      (strcat
        (vl-filename-directory
          (or (findfile "dispatcher.lsp") "")
        )
        "\\dispatcher.dcl"
      )
    )
  )

  (if (null dcl-file)
    (progn
      (alert "Не найден файл dispatcher.dcl.")
      (princ)
    )
    (progn
      ;; Начальные значения.
      (setq *DISPATCHER-ALL-LAYERS*
        (tu-layer-names)
      )
      (setq *DISPATCHER-VISIBLE-LAYERS*
        *DISPATCHER-ALL-LAYERS*
      )
      (setq *DISPATCHER-SELECTED-LAYERS* nil)
      (setq *DISPATCHER-GROUP-FILTER* nil)
      (setq *DISPATCHER-TASK-ID* 'FASONKA)
      (setq *DISPATCHER-REPORT-MODE* "DETAIL")
      (setq *DISPATCHER-EXPORT-EXCEL* T)
      (setq *DISPATCHER-EXPORT-TXT* nil)
      (setq *DISPATCHER-CREATE-TABLE* T)
      (setq *DISPATCHER-ACTION* 'CANCEL)

      (setq *DISPATCHER-DCL-ID*
        (load_dialog dcl-file)
      )

      (if (< *DISPATCHER-DCL-ID* 0)
        (alert "Не удалось загрузить dispatcher.dcl.")
        (progn
          (if (new_dialog "task_dispatcher" *DISPATCHER-DCL-ID*)
            (progn
              ;; Начальные значения.
              (set_tile "rb_detail" "1")
              (set_tile "rb_summary" "0")

              (set_tile "chk_xls" "1")
              (set_tile "chk_txt" "0")
              (set_tile "chk_acad" "1")
              (set_tile "chk_group_filter" "0")

              (set_tile "rb_task_fasonka" "1")

              ;; Пока реализована только Фасонка.
              (mode_tile "rb_task_subsystem" 1)
              (mode_tile "rb_task_cladding" 1)
              (mode_tile "rb_task_vitrazh" 1)
              (mode_tile "rb_task_steklopakety" 1)

              ;; Список блоков — заглушка.
              (mode_tile "lst_blocks" 1)

              (dispatcher-rebuild-layer-list)

              ;; Actions.
              (action_tile
                "btn_help"
                "(dispatcher-help)"
              )

              (action_tile
                "btn_save"
                "(dispatcher-save)"
              )

              (action_tile
                "btn_saveas"
                "(dispatcher-saveas)"
              )

              (action_tile
                "btn_close"
                "(dispatcher-close)"
              )

              (action_tile
                "chk_group_filter"
                "(dispatcher-group-filter)"
              )

              (action_tile
                "lst_layers"
                "(dispatcher-layer-selection)"
              )

              (action_tile
                "rb_detail"
                "(setq *DISPATCHER-REPORT-MODE* \"DETAIL\")"
              )

              (action_tile
                "rb_summary"
                "(setq *DISPATCHER-REPORT-MODE* \"SUMMARY\")"
              )

              (start_dialog)

              ;; DCL уже закрыт. Только теперь разрешены
              ;; файловый диалог и запуск задачи.
              (cond
                ((eq *DISPATCHER-ACTION* 'SAVE)
                 (run-task
                   *DISPATCHER-TASK-ID*
                   *DISPATCHER-SELECTED-LAYERS*
                   *DISPATCHER-REPORT-MODE*
                   *DISPATCHER-EXPORT-EXCEL*
                   *DISPATCHER-EXPORT-TXT*
                   *DISPATCHER-CREATE-TABLE*
                   nil
                 )
                )

                ((eq *DISPATCHER-ACTION* 'SAVEAS)
                 (setq save-base
                   (tu-get-save-base
                     *DISPATCHER-TASK-ID*
                   )
                 )
                 (if save-base
                   (run-task
                     *DISPATCHER-TASK-ID*
                     *DISPATCHER-SELECTED-LAYERS*
                     *DISPATCHER-REPORT-MODE*
                     *DISPATCHER-EXPORT-EXCEL*
                     *DISPATCHER-EXPORT-TXT*
                     *DISPATCHER-CREATE-TABLE*
                     save-base
                   )
                   (princ "\nСохранение отменено.")
                 )
                )

                (t nil)
              )
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

;; Удобный короткий псевдоним.
(defun c:tasks ()
  (c:taskdispatcher)
)

(princ "\nDISPATCHER.LSP загружен. Команды: TASKDISPATCHER, TASKS")
(princ)
