;;; ============================================================
;;;  TXT-UTILS.LSP
;;;  Экспорт отчёта Фасонки в GAL (TXT)
;;; ============================================================

(defun tx-export-gal (report-type report-data base-name
                      / galfile fgal ig name recs itemNum len count rec)
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
                  len      (cadr rec)
                  count    (caddr rec))

            (write-line
              (strcat
                "Otr="
                name
                " "
                (itoa itemNum)
                "/"
                (itoa count)
                "/"
                (rtos len 2 0)
                "/")
              fgal)
          )
        )

        ;; SUMMARY (не предусмотрено, но выводим агрегаты)
        (foreach rec report-data
          (write-line
            (strcat
              "Otr="
              (car rec)
              "/"
              (itoa (cadr rec))
              "/0/")
            fgal)
        )
      )

      (close fgal)
      (princ (strcat "\nGAL сохранён: " galfile))
      T
    )

    (progn
      (princ "\nНе удалось сохранить GAL.")
      nil
    )
  )
)

(princ)