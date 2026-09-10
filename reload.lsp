;;; ============================================================
;;; reload.lsp
;;; Команда RELOAD для перезагрузки всех модулей AutoExtraction
;;
;;; ИСПРАВЛЕНИЯ (аудит Этап 4.4):
;;;   D10: Добавлены поясняющие комментарии о структуре
;;;       загрузочной цепочки и роли каждого файла.
;;; ============================================================

(defun c:RELOAD ( / root common extraction-dir f)
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
      (setq common (strcat root "\\common\\"))
      (setq extraction-dir (strcat root "\\Extraction\\"))

      ;; ========================================================
      ;; Общие библиотеки (загружаются первыми)
      ;; Эти файлы содержат утилиты, используемые всеми задачами.
      ;; Порядок важен: сначала утилиты, затем задачи.
      ;; ========================================================
      (foreach f
        '(
          "task-utils.lsp"
          "layer-utils.lsp"
          "select-utils.lsp"
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

      ;; ========================================================
      ;; Задачи
      ;; Каждая задача — самостоятельный модуль с автономной
      ;; командой (например, FASONKA) и функцией -main для
      ;; запуска из диспетчера.
      ;;
      ;; Примечание (аудит Этап 4.4, пункт D10):
      ;; extraction.lsp загружается для обновления, так как
      ;; является диспетчером. Он уже находится в памяти в момент
      ;; вызова RELOAD, но перезагрузка гарантирует актуальность
      ;; кода после изменений.
      ;; ========================================================
      (foreach f
        '(
          "fasonka.lsp"
          "subsystem.lsp"
          "zapolnenie.lsp"
          "extraction.lsp"   ; диспетчер, перезагружается для обновления
          "cutline.lsp"
          "cutsheet.lsp"
          "blockrename.lsp"
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