(defun c:CUTSHEET ()
  (princ "\nCUTSHEET: модуль в разработке.")
  (princ)
)

;; ============================================================
;; Обёртка для запуска из диспетчера EXTRACTION
;; ============================================================
(defun cutsheet-main ()
  (c:cutsheet)
)

(princ "\nCUTSHEET.LSP загружен. Команда: CUTSHEET")
(princ)