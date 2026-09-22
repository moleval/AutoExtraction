;;; ============================================================
;;; VALIDATION-UTILS.LSP - Этап 2 (V1 + V2 дорожной карты)
;;; Единый парсер чисел и предикаты валидации входных данных.
;;; Слои: Нормализация -> Валидация (HARD) -> Sanity (SOFT).
;;; Алгоритмы модулей этот файл не меняет.
;;; ============================================================

;; ---------- Защитные лимиты (именованные; при необходимости менять) ----------
(if (not (boundp '*n1-max-bar-length*))
  (setq *n1-max-bar-length* 500000.0))  ; макс. длина хлыста/детали, мм
(if (not (boundp '*n1-max-kerf*))
  (setq *n1-max-kerf* 100.0))           ; макс. ширина реза, мм
(if (not (boundp '*tu-max-dim*))
  (setq *tu-max-dim* 500000.0))         ; общий технический потолок размера, мм

;; ---------- V2. Единый numeric/VARIANT parser ----------
;; Принимает STR (запятая и пробелы-разрядки допускаются), INT, REAL, VARIANT.
;; Возвращает REAL либо nil - "не число"/ошибка VARIANT.
(defun tu-parse-number (s / x)
  (cond
    ((null s) nil)
    ((member (type s) '(REAL INT)) (float s))
    ((= (type s) 'VARIANT)
     (setq x (vl-catch-all-apply 'vlax-variant-value (list s)))
     (if (vl-catch-all-error-p x)
       nil
       (if (member (type x) '(REAL INT)) (float x) nil)))
    ((= (type s) 'STR)
     (setq x (vl-string-trim " \t\r\n" s))
     (if (= x "")
       nil
       (progn
         (setq x (vl-string-translate "," "." x))
         (while (vl-string-search " " x)
           (setq x (vl-string-subst "" " " x)))
         (setq x (vl-catch-all-apply 'distof (list x 2)))
         (if (vl-catch-all-error-p x) nil x))))
    (T nil)
  )
)

;; "1 3 5" -> '(1 3 5) | nil; каждый элемент обязан быть INT.
;; Обертка над read: мусор вместо списка дает nil, а не ошибку.
(defun tu-parse-int-list (s / x)
  (if (and (= (type s) 'STR) (/= s ""))
    (progn
      (setq x (vl-catch-all-apply 'read (list (strcat "(" s ")"))))
      (if (or (vl-catch-all-error-p x) (/= (type x) 'LIST))
        nil
        (if (vl-every '(lambda (e) (= (type e) 'INT)) x) x nil)
      )
    )
    nil
  )
)

;; ---------- V1. Предикаты ----------
(defun tu-positive-number-p    (x) (and (numberp x) (>  x 0)))
(defun tu-nonnegative-number-p (x) (and (numberp x) (>= x 0)))
(defun tu-positive-integer-p   (x) (and (= (type x) 'INT) (> x 0)))
(defun tu-number-in-range-p    (x lo hi)
  (and (numberp x) (>= x lo) (<= x hi)))
(defun tu-string-nonempty-p    (x) (and (= (type x) 'STR) (> (strlen x) 0)))

;; Точка '(x y)/(x y z): список из 2-3 чисел
(defun tu-valid-point-p (x)
  (and (= (type x) 'LIST)
       (member (length x) '(2 3))
       (vl-every 'numberp x)))

;; Пара размеров > 0
(defun tu-valid-size-p (w h)
  (and (tu-positive-number-p w) (tu-positive-number-p h)))

;; Длина хлыста/детали: > 0 и не выше защитного лимита
(defun tu-valid-bar-length-p (x)
  (and (tu-positive-number-p x) (<= x *n1-max-bar-length*)))

;; Ширина реза: >= 0 и не выше защитного лимита (связь "рез < длины" - у вызывающего)
(defun tu-valid-kerf-p (x)
  (tu-number-in-range-p x 0.0 *n1-max-kerf*))

;; Габарит листа: обе стороны > 0, в технических пределах
(defun tu-valid-sheet-size-p (w h)
  (and (tu-valid-size-p w h) (<= w *tu-max-dim*) (<= h *tu-max-dim*)))

(princ "\nVALIDATION-UTILS.LSP загружен.")
(princ)
