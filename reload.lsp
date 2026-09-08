;;; ============================================================
;;; reload.lsp
;;; Команда RELOAD для перезагрузки всех модулей AutoExtraction
;;; ============================================================

(defun c:RELOAD ( / root common extraction-dir f safe-load)
  ;; Функция безопасной загрузки файла
  (defun safe-load (path / res)
    (setq res (vl-catch-all-apply 'load (list path)))
    (if (vl-catch-all-error-p res)
      (princ (strcat "\nОшибка загрузки: " path " -> " (vl-catch-all-error-message res)))
      (princ (strcat "\nЗагружен: " path))
    )
  )

  (setq root
    (if (findfile "fasonka.lsp")
      (vl-filename-directory
        (vl-filename-directory (findfile "fasonka.lsp"))
      )
      nil
    )
  )

  (if root
    (progn
      (setq common (strcat root "\\common\\"))
      (setq extraction-dir (strcat root "\\Extraction\\"))

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
          (safe-load (strcat common f))
          (princ (strcat "\nНЕ НАЙДЕН: " common f))
        )
      )

      ;; Загружаем модули задач
      (foreach f
        '(
          "fasonka.lsp"
          "extraction.lsp"
          "cutline.lsp"
          "cutsheet.lsp"
        )
        (if (findfile (strcat extraction-dir f))
          (safe-load (strcat extraction-dir f))
          (princ (strcat "\nНЕ НАЙДЕН: " extraction-dir f))
        )
      )

      (princ "\nВсе модули AutoExtraction перезагружены.")
    )
    (princ "\nОшибка: fasonka.lsp не найден в путях поддержки AutoCAD.")
  )

  (princ)
)

(princ "\nRELOAD.LSP загружен. Команда: RELOAD")
(princ)