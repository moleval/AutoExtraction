;;; ============================================================
;;; common/select-utils.lsp
;;; Выбор вхождений блоков и извлечение динамических свойств
;;; ============================================================
(vl-load-com)

;; Проверка, принадлежит ли слой выбранному списку
(defun su-layer-selected-p (layer layers / x)
  (if (null layers)
    T
    (progn
      (setq x nil)
      (foreach name layers
        (if (= (strcase name) (strcase layer))
          (setq x T)
        )
      )
      x
    )
  )
)

;; Преобразование значения в число (точная копия fasonka-value-to-number)
(defun su-value-to-number (value / x s)
  (cond
    ((numberp value)
     (float value)
    )
    ((= (type value) 'VARIANT)
     (setq x (vl-catch-all-apply 'vlax-variant-value (list value)))
     (if (vl-catch-all-error-p x)
       nil
       (su-value-to-number x)
     )
    )
    ((= (type value) 'STR)
     (setq s (vl-string-trim " \t\r\n" value))
     (if (> (strlen s) 0)
       (progn
         (setq s (vl-string-translate "," "." s))
         (while (vl-string-search " " s)
           (setq s (vl-string-subst "" " " s))
         )
         (setq x (vl-catch-all-apply 'atof (list s)))
         (if (vl-catch-all-error-p x)
           nil
           x
         )
       )
       nil
     )
    )
    (t nil)
  )
)

;; Выбор вхождений блоков (INSERT) с учётом предварительного выбора
(defun su-select-inserts (layers / ss i ent data layer out)
  (setq out '())
  (setq ss (ssget "_I"))
  (if (null ss)
    (progn
      (cond
        ((null layers)
         (setq ss (ssget "_X" '((0 . "INSERT")))))
        (t
         (setq layer-filter (list '(0 . "INSERT")))
         (foreach lay layers
           (setq layer-filter (append layer-filter (list (cons 8 lay)))))
         (setq ss (ssget "_X" layer-filter))
        )
      )
    )
  )

  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq data (entget ent))
        (setq layer (cdr (assoc 8 data)))
        (if (su-layer-selected-p layer layers)
          (setq out (cons ent out))
        )
        (setq i (1+ i))
      )
    )
  )

  (reverse out)
)

;; Защищённое получение EffectiveName
(defun su-get-effective-name (obj / r name)
  (setq r
    (vl-catch-all-apply
      'vla-get-EffectiveName
      (list obj)
    )
  )
  (if (vl-catch-all-error-p r)
    (progn
      (setq r
        (vl-catch-all-apply
          'vla-get-Name
          (list obj)
        )
      )
      (if (vl-catch-all-error-p r)
        nil
        r
      )
    )
    r
  )
)

;; Получение длины из динамических свойств (точная копия fasonka-get-length)
(defun su-get-length (obj / dynprops prop pname value result)
  (setq dynprops
    (vl-catch-all-apply
      'vlax-invoke
      (list obj 'GetDynamicBlockProperties)
    )
  )

  (if (vl-catch-all-error-p dynprops)
    nil
    (progn
      (setq result nil)
      (foreach prop dynprops
        (if (null result)
          (progn
            (setq pname
              (vl-catch-all-apply
                'vla-get-PropertyName
                (list prop)
              )
            )
            (if (and
                  (not (vl-catch-all-error-p pname))
                  pname
                  (= (type pname) 'STR)
                  (= (strcase pname) "ДЛИНА")
                )
              (progn
                (setq value
                  (vl-catch-all-apply
                    'vla-get-Value
                    (list prop)
                  )
                )
                (if (not (vl-catch-all-error-p value))
                  (setq result
                    (su-value-to-number value)
                  )
                )
              )
            )
          )
        )
      )
      ;; Исходное преобразование в целое число
      (if (numberp result)
        (atoi (rtos result 2 0))
        nil
      )
    )
  )
)

(princ "\nSELECT-UTILS.LSP загружен.")
(princ)