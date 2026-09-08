;;; ============================================================
;;; reload.lsp
;;; Команда RELOAD для перезагрузки всех модулей AutoExtraction
;;; ============================================================

(defun c:RELOAD ( / root common tasks f)
  ;; Определяем корень проекта: на два уровня выше от fasonka.lsp
  (setq root
    (vl-filename-directory
      (vl-filename-directory (findfile "fasonka.lsp"))
    )
  )

  (if root
    (progn
      (setq common (strcat root "\\common\\"))
      (setq tasks (strcat root "\\TASKS\\"))

      ;; Загружаем общие библиотеки
      (foreach f
        '(
          "task-utils.lsp"
          "layer-utils.lsp"
          "excel-utils.lsp"
          "table-utils.lsp"
          "txt-utils.lsp"
        )
        (if (findfile (strcat common f))
          (progn
            (load (strcat common f))
            (princ (strcat "\nЗагружен: " common f))
          )
          (princ (strcat "\nНЕ НАЙДЕН: " common f))
        )
      )

      ;; Загружаем модули задач
      (foreach f
        '(
          "fasonka.lsp"
          "dispatcher.lsp"
          "cutline.lsp"
          "cutsheet.lsp"
        )
        (if (findfile (strcat tasks f))
          (progn
            (load (strcat tasks f))
            (princ (strcat "\nЗагружен: " tasks f))
          )
          (princ (strcat "\nНЕ НАЙДЕН: " tasks f))
        )
      )

      (princ "\nВсе модули AutoExtraction перезагружены.")
    )
    (princ "\nОшибка: не удалось определить корень проекта (fasonka.lsp не найден).")
  )

  (princ)
)

(princ "\nRELOAD.LSP загружен. Команда: RELOAD")
(princ)