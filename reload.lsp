;;; ============================================================
;;; reload.lsp
;;; Команда RELOAD для перезагрузки всех модулей AutoExtraction
;;; ============================================================

(defun c:RELOAD ( / root extraction-dir common-dir f)
  ;; Определяем корень проекта через extraction.lsp
  (setq root
    (if (findfile "extraction.lsp")
      (vl-filename-directory
        (vl-filename-directory (findfile "extraction.lsp"))
      )
      nil
    )
  )

  (if root
    (progn
      (setq common-dir (strcat root "\\common\\"))
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
        (if (findfile (strcat common-dir f))
          (progn
            (load (strcat common-dir f))
            (princ (strcat "\nЗагружен: " common-dir f))
          )
          (princ (strcat "\nНЕ НАЙДЕН: " common-dir f))
        )
      )

      ;; Загружаем модули задач (из Extraction)
      (foreach f
        '(
          "fasonka.lsp"
          "extraction.lsp"
          "cutline.lsp"
          "cutsheet.lsp"
        )
        (if (findfile (strcat extraction-dir f))
          (progn
            (load (strcat extraction-dir f))
            (princ (strcat "\nЗагружен: " extraction-dir f))
          )
          (princ (strcat "\nНЕ НАЙДЕН: " extraction-dir f))
        )
      )

      (princ "\nВсе модули AutoExtraction перезагружены.")
    )
    (princ "\nОшибка: extraction.lsp не найден в путях поддержки AutoCAD.")
  )

  (princ)
)

(princ "\nRELOAD.LSP загружен. Команда: RELOAD")
(princ)