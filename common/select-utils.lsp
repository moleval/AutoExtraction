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

  (if (and (boundp '*extraction-preselected-set*) *extraction-preselected-set*)
    (progn
      (setq ss *extraction-preselected-set*)
      (setq *extraction-preselected-set* nil)
    )
    (setq ss (ssget "_I"))
  )

  (if (null ss)
    (progn
      (if (null layers)
        (setq ss (ssget "_X" '((0 . "INSERT"))))
        (progn
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

;; Получение длины из динамических свойств
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
      (if (numberp result)
        (atoi (rtos result 2 0))
        nil
      )
    )
  )
)


;; ============================================================
;; Извлечение габаритной длины MLINE
;; ============================================================
;; vla-Explode для MLINE не поддерживается ActiveX.
;; Длина считается напрямую по вершинам осевой линии
;; (DXF-код 11).
;; ============================================================

(defun su-mline-length (ent / data verts i total p1 p2)
  (setq data (entget ent))
  (setq verts '())

  (foreach pair data
    (if (= (car pair) 11)
      (setq verts (cons (cdr pair) verts))
    )
  )

  (setq verts (reverse verts))

  (setq total 0.0)
  (setq i 0)
  (while (< i (1- (length verts)))
    (setq p1 (nth i verts))
    (setq p2 (nth (1+ i) verts))
    (setq total (+ total (distance p1 p2)))
    (setq i (1+ i))
  )

  total
)


;; ============================================================
;; Проверка соответствия слоя списку (с масками)
;; ============================================================

(defun su-layer-match-any (layer layers / found)
  (if (or (null layers) (not (listp layers)) (= (length layers) 0))
    T
    (progn
      (setq found nil)
      (foreach l layers
        (if (wcmatch (strcase layer) (strcase l))
          (setq found T)
        )
      )
      found
    )
  )
)


;; ============================================================
;; Типы объектов, допустимые в CUTLINE
;; ============================================================
;; Раскрой хлыстов принимает ТОЛЬКО:
;;   LINE  — отрезок;
;;   MLINE — мультилиния.
;; Дуги, эллипсы, сплайны, полилинии — не принимаются.
;; ============================================================

(setq *su-cutline-types* '("LINE" "MLINE"))


;; ============================================================
;; Ручная фильтрация selection set по типу и слоям
;; ============================================================

(defun su-filter-ss-cutline (ss types layers / i ent data typ lay new-ss)
  (if (null ss)
    nil
    (progn
      (setq new-ss (ssadd))
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq data (entget ent))
        (setq typ (cdr (assoc 0 data)))
        (setq lay (cdr (assoc 8 data)))
        (if (and (member typ types)
                 (su-layer-match-any lay layers))
          (ssadd ent new-ss)
        )
        (setq i (1+ i))
      )
      (if (> (sslength new-ss) 0) new-ss nil)
    )
  )
)


;; ============================================================
;; Построение ssget-фильтра для CUTLINE
;; ============================================================

(defun su-build-cutline-ssfilter (layers / base layer-mask)
  (setq base
    (list '(0 . "LINE,MLINE")))

  (if (and layers (listp layers) (> (length layers) 0))
    (progn
      (setq layer-mask
        (apply 'strcat
          (mapcar '(lambda (x) (strcat x ",")) layers)))
      (setq layer-mask (substr layer-mask 1 (1- (strlen layer-mask))))
      (append base (list (cons 8 layer-mask)))
    )
    base
  )
)


;; ============================================================
;; Извлечение исходного набора объектов для CUTLINE
;; ============================================================
;; Приоритет источников:
;;   1. *extraction-preselected-set*
;;   2. ssget "_I"
;;   3. интерактивный ssget
;; ============================================================

(defun su-select-cutline-objects (layers / ss ssfilter)
  (setq ss nil)
  (setq ssfilter (su-build-cutline-ssfilter layers))

  (if (and (boundp '*extraction-preselected-set*)
           *extraction-preselected-set*)
    (progn
      (setq ss
        (su-filter-ss-cutline
          *extraction-preselected-set*
          *su-cutline-types*
          layers))
      (setq *extraction-preselected-set* nil)
    )
  )

  (if (null ss)
    (setq ss (ssget "_I" ssfilter))
  )

  (if (null ss)
    (setq ss (ssget ssfilter))
  )

  ss
)


(princ "\nSELECT-UTILS.LSP загружен.")
(princ)