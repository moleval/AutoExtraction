;;; ============================================================
;;; common/txt-utils.lsp
;;; Экспорт в GAL (формат для станка с ЧПУ)
;;; Функции для Фасонки, Подсистемы и Заполнения
;;; ============================================================

(vl-load-com)

;; ------------------------------------------------------------
;; Экспорт в GAL для линейных элементов (Фасонка, Подсистема)
;; Формат: Otr=имя номер/количество/длина/
;; ------------------------------------------------------------
(defun tx-export-gal (report-type report-data base-name / galfile fgal name recs itemNum rec len count)
  (setq galfile (strcat base-name ".gal"))
  (setq fgal (open galfile "w"))
  (if fgal
    (progn
      (write-line "Длина=6000" fgal)
      (write-line "ML=0" fgal)
      (write-line "MR=0" fgal)
      (write-line "Pil=10" fgal)

      (if (= report-type "DETAIL")
        (foreach ig report-data
          (setq name (cadr ig)
                recs (caddr ig)
                itemNum 0)
          (foreach rec recs
            (setq itemNum (1+ itemNum)
                  len (cadr rec)
                  count (caddr rec))
            (write-line
              (strcat "Otr=" name " " (itoa itemNum) "/" (itoa count) "/" (rtos len 2 0) "/")
              fgal
            )
          )
        )
        (foreach rec report-data
          (write-line
            (strcat "Otr=" (car rec) "/" (itoa (cadr rec)) "/0/")
            fgal
          )
        )
      )

      (close fgal)
      (princ (strcat "\nGAL сохранён: " galfile))
      T
    )
    (progn
      (princ "\nНе удалось создать файл GAL.")
      nil
    )
  )
)

;; ------------------------------------------------------------
;; Экспорт в GAL для панелей (Заполнение, в будущем — Облицовка)
;; Формат: №_Тип/высота/ширина/количество/
;;
;; Параметры:
;;   data        - список (тип высота-мм ширина-мм количество)
;;   base-name   - базовое имя файла без расширения
;;   sheet-size  - размер листа (строка, напр. "3210?2250")
;;   rotate-flag - флаг вращения (строка, напр. "вращать")
;;
;; Параметры листа и вращения передаются извне для
;; универсальности (Облицовка может использовать другие значения).
;;
;; Возврат: T при успехе, nil при недоступности файла
;; ------------------------------------------------------------
(defun tx-export-gal-zapolnenie (data base-name sheet-size rotate-flag / galfile fgal rec num tip h w cnt)
  (setq galfile (strcat base-name ".gal"))
  (setq fgal (open galfile "w"))
  (if fgal
    (progn
      ;; Заголовочные строки
      (write-line (strcat "Размер листа=" sheet-size) fgal)
      (write-line rotate-flag fgal)

      ;; Строки панелей с последовательной нумерацией
      (setq num 0)
      (foreach rec data
        (setq num (1+ num)
              tip (car rec)
              h   (cadr rec)
              w   (caddr rec)
              cnt (cadddr rec))
        (write-line
          (strcat (itoa num) "_" tip "/" (itoa h) "/" (itoa w) "/" (itoa cnt) "/")
          fgal
        )
      )

      (close fgal)
      (princ (strcat "\nGAL сохранён: " galfile))
      T
    )
    (progn
      (princ "\nНе удалось создать файл GAL.")
      nil
    )
  )
)

(princ "\nTXT-UTILS.LSP загружен.")
(princ)