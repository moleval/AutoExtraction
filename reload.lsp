;;; ============================================================
;;; RELOAD.LSP
;;; Автоматическая перезагрузка AutoExtraction
;;; AutoCAD 2016+
;;;
;;; Особенности:
;;;   - определяет корень проекта по extraction.lsp;
;;;   - загружает common-модули в правильном порядке;
;;;   - загружает Extraction-модули в правильном порядке;
;;;   - проверяет наличие каждого файла;
;;;   - перехватывает ошибки загрузки;
;;;   - точно сообщает имя файла, вызвавшего ошибку;
;;;   - продолжает загрузку следующих модулей;
;;; ============================================================


(defun ae-reload-load-file
       (fullpath / result)

  ;; ------------------------------------------------------------
  ;; Загрузка одного LSP-файла с перехватом ошибки
  ;; ------------------------------------------------------------

  (if (not (findfile fullpath))
    (progn
      (princ
        (strcat
          "\nНЕ НАЙДЕН: "
          fullpath
        )
      )
      nil
    )
    (progn

      (setq result
        (vl-catch-all-apply
          'load
          (list fullpath)
        )
      )

      (if (vl-catch-all-error-p result)
        (progn
          (princ
            (strcat
              "\nОШИБКА ЗАГРУЗКИ: "
              fullpath
              "\n  Причина: "
              (vl-catch-all-error-message result)
            )
          )
          nil
        )
        (progn
          (princ
            (strcat
              "\nЗагружен: "
              fullpath
            )
          )
          T
        )
      )
    )
  )
)


(defun c:RELOAD
       ( / root
           common
           extraction-dir
           f
           fullpath
           common-files
           extraction-files
           ok
           errors
           missing)

  ;; ------------------------------------------------------------
  ;; Заголовок
  ;; ------------------------------------------------------------

  (princ
    "\n============================================="
  )
  (princ
    "\n AutoExtraction RELOAD"
  )
  (princ
    "\n============================================="
  )


  ;; ------------------------------------------------------------
  ;; Определяем корень проекта
  ;;
  ;; extraction.lsp находится:
  ;;   <root>\Extraction\extraction.lsp
  ;;
  ;; Поэтому два раза поднимаемся от файла:
  ;;   dirname(extraction.lsp) -> Extraction
  ;;   dirname(Extraction)     -> root
  ;; ------------------------------------------------------------

  (setq root
    (if (findfile "extraction.lsp")
      (vl-filename-directory
        (vl-filename-directory
          (findfile "extraction.lsp")
        )
      )
      nil
    )
  )


  ;; ------------------------------------------------------------
  ;; Если extraction.lsp не найден
  ;; ------------------------------------------------------------

  (if (not root)
    (progn
      (princ
        "\nОШИБКА: не найден extraction.lsp."
      )
      (princ
        "\nRELOAD прерван."
      )
      (princ)
    )

    (progn

      ;; --------------------------------------------------------
      ;; Пути
      ;; --------------------------------------------------------

      (setq common
        (strcat root "\\common\\")
      )

      (setq extraction-dir
        (strcat root "\\Extraction\\")
      )


      (princ
        (strcat
          "\nКорень проекта: "
          root
        )
      )

      (princ
        (strcat
          "\nCommon: "
          common
        )
      )

      (princ
        (strcat
          "\nExtraction: "
          extraction-dir
        )
      )


      ;; --------------------------------------------------------
      ;; Счётчики
      ;; --------------------------------------------------------

      (setq ok 0)
      (setq errors 0)
      (setq missing 0)


      ;; --------------------------------------------------------
      ;; COMMON
      ;;
      ;; Порядок загрузки важен:
      ;;
      ;; task-utils
      ;; layer-utils
      ;; select-utils
      ;; excel-utils
      ;; table-utils
      ;; txt-utils
      ;; --------------------------------------------------------

      (setq common-files
        '(
          "task-utils.lsp"
          "layer-utils.lsp"
          "select-utils.lsp"
          "excel-utils.lsp"
          "table-utils.lsp"
          "txt-utils.lsp"
        )
      )


      (princ
        "\n"
      )
      (princ
        "\n--- COMMON ---"
      )


      (foreach f common-files

        (setq fullpath
          (strcat common f)
        )

        (if (ae-reload-load-file fullpath)
          (setq ok (1+ ok))
          (if (findfile fullpath)
            (setq errors (1+ errors))
            (setq missing (1+ missing))
          )
        )
      )


      ;; --------------------------------------------------------
      ;; EXTRACTION
      ;;
      ;; Порядок:
      ;;
      ;; FASONKA
      ;; SUBSYSTEM
      ;; CLADDING
      ;; ZAPOLNENIE
      ;; EXTRACTION
      ;; CUTLINE
      ;; CUTSHEET
      ;; BLOCKRENAME
      ;;
      ;; CLADDING должен быть загружен ДО extraction.lsp,
      ;; поскольку dispatcher extraction.lsp вызывает
      ;; cladding-main.
      ;; --------------------------------------------------------

      (setq extraction-files
        '(
          "fasonka.lsp"
          "subsystem.lsp"
          "cladding.lsp"
          "zapolnenie.lsp"
          "extraction.lsp"
          "cutline.lsp"
          "cutsheet.lsp"
          "blockrename.lsp"
        )
      )


      (princ
        "\n"
      )
      (princ
        "\n--- EXTRACTION ---"
      )


      (foreach f extraction-files

        (setq fullpath
          (strcat extraction-dir f)
        )

        (if (ae-reload-load-file fullpath)
          (setq ok (1+ ok))
          (if (findfile fullpath)
            (setq errors (1+ errors))
            (setq missing (1+ missing))
          )
        )
      )


      ;; --------------------------------------------------------
      ;; Итог
      ;; --------------------------------------------------------

      (princ
        "\n"
      )
      (princ
        "\n============================================="
      )

      (princ
        (strcat
          "\n RELOAD завершён."
        )
      )

      (princ
        (strcat
          "\n Успешно загружено: "
          (itoa ok)
        )
      )

      (princ
        (strcat
          "\n Ошибок загрузки: "
          (itoa errors)
        )
      )

      (princ
        (strcat
          "\n Не найдено файлов: "
          (itoa missing)
        )
      )

      (princ
        "\n============================================="
      )


      ;; --------------------------------------------------------
      ;; Дополнительное предупреждение
      ;; --------------------------------------------------------

      (if (> errors 0)
        (princ
          "\nВНИМАНИЕ: имеются ошибки загрузки. См. строки выше."
        )
      )

      (if (> missing 0)
        (princ
          "\nВНИМАНИЕ: некоторые файлы проекта не найдены."
        )
      )
    )
  )

  (princ)
)


(princ
  "\nRELOAD.LSP загружен. Команда: RELOAD"
)

(princ)