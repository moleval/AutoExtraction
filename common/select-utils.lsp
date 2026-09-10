;;; ============================================================
;;; common/select-utils.lsp
;;; Выбор вхождений блоков и извлечение динамических свойств
;;
;;; ИСПРАВЛЕНИЕ (аудит Этап 3, пункт B4):
;;; su-get-length использует двухступенчатый поиск:
;;;   1. Сначала точное совпадение "ДЛИНА"
;;;   2. Если не найдено — поиск по подстроке "ДЛИНА"
;;; Это защищает от ложных срабатываний и находит свойства типа
;;; "Длина_уплотнителя", "Длина в свету" и т.д.
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

;; Преобразование значения в число
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
(defun su-select-inserts (layers / ss i ent data layer out layer-name)
  (setq out '())
  
  ;; Проверяем сохранённый предварительный выбор от диспетчера
  (if (and (boundp '*extraction-preselected-set*) *extraction-preselected-set*)
    (progn
      (setq ss *extraction-preselected-set*)
      (setq *extraction-preselected-set* nil)  ; очищаем после использования
    )
    (setq ss (ssget "_I"))
  )
  
  ;; Если предварительного выбора нет — выбираем по слоям или все
  (if (null ss)
    (progn
      (if (null layers)
        (setq ss (ssget "_X" '((0 . "INSERT"))))
        (progn
          ;; Формируем строку слоёв через запятую
          (setq layer-name
            (apply 'strcat
              (mapcar '(lambda (x) (strcat x ",")) layers)
            )
          )
          (setq layer-name (substr layer-name 1 (1- (strlen layer-name))))
          (setq ss (ssget "_X" (list '(0 . "INSERT") (cons 8 layer-name))))
        )
      )
    )
  )
  
  ;; Фильтрация по слоям (даже для предварительного выбора)
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

;; Получение строкового значения видимости
(defun su-get-visibility (obj / dynprops prop pname val result s)
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
                  (or
                    (vl-string-search "VISIBILITY" (strcase pname))
                    (vl-string-search "ВИДИМОСТЬ" (strcase pname))
                  )
                )
              (progn
                (setq val
                  (vl-catch-all-apply
                    'vla-get-Value
                    (list prop)
                  )
                )
                (if (not (vl-catch-all-error-p val))
                  (progn
                    (if (= (type val) 'VARIANT)
                      (setq val (vlax-variant-value val))
                    )
                    (if val
                      (setq result (vl-princ-to-string val))
                    )
                  )
                )
              )
            )
          )
        )
      )
      result
    )
  )
)

;; ============================================================
;; Получение длины из динамических свойств
;; ИСПРАВЛЕНО (аудит Этап 3, пункт B4):
;; Двухступенчатый поиск: сначала точное совпадение, затем подстрока.
;; Это защищает от ложных срабатываний и находит свойства типа
;; "Длина_уплотнителя", "Длина в свету" и т.д.
;; Результат округляется до целого числа (мм).
;; ============================================================
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

      ;; Шаг 1: точное совпадение "ДЛИНА"
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
                  (setq result (su-value-to-number value))
                )
              )
            )
          )
        )
      )

      ;; Шаг 2: поиск по подстроке "ДЛИНА" (если точное не найдено)
      (if (null result)
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
                    (vl-string-search "ДЛИНА" (strcase pname))
                  )
                (progn
                  (setq value
                    (vl-catch-all-apply
                      'vla-get-Value
                      (list prop)
                    )
                  )
                  (if (not (vl-catch-all-error-p value))
                    (setq result (su-value-to-number value))
                  )
                )
              )
            )
          )
        )
      )

      ;; Округление до целого числа (мм) — ожидаемое поведение для Фасонки
      (if (numberp result)
        (atoi (rtos result 2 0))
        nil
      )
    )
  )
)

(princ "\nSELECT-UTILS.LSP загружен.")
(princ)