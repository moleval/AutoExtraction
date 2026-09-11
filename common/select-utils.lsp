;;; ============================================================
;;; common/select-utils.lsp
;;; Выбор вхождений блоков и извлечение динамических свойств
;;
;;; ИСПРАВЛЕНИЯ (аудит Этап 3, пункт B4):
;;; su-get-length использует строгий поиск:
;;;   Только точное совпадение "ДЛИНА" (Вариант А)
;;
;;; ДОБАВЛЕНО (Этап 2.1 — анализ динамических блоков):
;;;   su-has-length-property  — точное совпадение "ДЛИНА"
;;;   su-has-width-property   — подстрока "ШИРИНА"
;;;   su-has-height-property  — подстрока "ВЫСОТА"
;;;   su-is-valid-stock-block — комплексная проверка пригодности
;;
;;; ДОБАВЛЕНО (Этап 2.2 — выбор динамических блоков):
;;;   su-filter-dynblocks     — пост-фильтрация непригодных блоков
;;;   *su-cutline-types*      — расширен: добавлен "INSERT"
;;;   su-build-cutline-ssfilter — добавлен INSERT в фильтр
;;;   su-select-cutline-objects — вызов пост-фильтрации
;;
;;; ДОБАВЛЕНО (Этап 3.1 — унификация с Подсистемой):
;;;   su-get-dynblock-type-name  — имя типа блока (Видимость или имя)
;;;   su-collect-dynblock-types  — сбор уникальных типов
;;
;;; ИСПРАВЛЕНО (Этап Р2 — Ремонт кода):
;;;   Р2.2: обнуление *extraction-preselected-set* перенесено
;;;         после проверки результата
;;
;;; ИСПРАВЛЕНО (Этап Р3 — Ремонт кода, технический долг):
;;;   Р3.2: двухступенчатый поиск длины в su-get-length:
;;;         ступень 1 — точное "ДЛИНА",
;;;         ступень 2 — подстрока "ДЛИНА" без "ШИРИНА"/"ВЫСОТА".
;;;         Для CUTLINE поведение не меняется (блоки предварительно
;;;         фильтруются по точному совпадению).
;;;   Р3.3: кэш свойств динамических блоков по ename.
;;;         Свойства читаются ОДИН раз за запуск и за ОДИН проход
;;;         по GetDynamicBlockProperties (было до 9 перечислений
;;;         на блок). Кэш сбрасывается в начале каждого выбора.
;;
;;; ПРАВИЛА ОТСЕИВАНИЯ (зафиксированы):
;;;   Блок принимается в раскрой хлыстов, если:
;;;     ? Есть свойство "ДЛИНА" (точное совпадение)
;;;     ? НЕТ свойства с подстрокой "ШИРИНА"
;;;     ? НЕТ свойства с подстрокой "ВЫСОТА"
;;;   Иначе блок отсеивается.
;;; ============================================================
(vl-load-com)

;; ============================================================
;; Проверка, принадлежит ли слой выбранному списку
;; ============================================================
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

;; ============================================================
;; Преобразование значения в число
;; ============================================================
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


;; ============================================================
;; КЭШ СВОЙСТВ ДИНАМИЧЕСКИХ БЛОКОВ
;; ДОБАВЛЕНО (Р3.3): устранение технического долга
;;
;; Проблема: каждый блок проверялся до 3 раз за запуск,
;; и каждая проверка делала до 3 проходов по COM-свойствам
;; (GetDynamicBlockProperties). Итого до 9 перечислений на блок.
;;
;; Решение:
;;   - свойства читаются ОДИН раз за запуск (кэш по ename);
;;   - чтение выполняется за ОДИН проход по свойствам;
;;   - кэш сбрасывается в начале каждого выбора объектов.
;;
;; Формат записи кэша:
;;   (ename . (has-len has-wid has-hei len-exact len-soft))
;; ============================================================

(if (not (boundp '*su-block-props-cache*))
  (setq *su-block-props-cache* '())
)

;; Сброс кэша. Вызывается в начале каждого выбора объектов,
;; чтобы изменения свойств блоков в чертеже не давали устаревших данных
(defun su-block-props-cache-clear ()
  (setq *su-block-props-cache* '())
)

;; ============================================================
;; Чтение свойств блока за ОДИН проход, с кэшированием по ename
;; ДОБАВЛЕНО (Р3.3)
;;
;; Возвращает список:
;;   (has-len has-wid has-hei len-exact len-soft)
;; где:
;;   has-len   — есть свойство с точным именем "ДЛИНА"
;;   has-wid   — есть свойство с подстрокой "ШИРИНА"
;;   has-hei   — есть свойство с подстрокой "ВЫСОТА"
;;   len-exact — значение свойства "ДЛИНА" (точное совпадение)
;;   len-soft  — значение первого свойства, содержащего "ДЛИНА",
;;               но не содержащего "ШИРИНА"/"ВЫСОТА"
;;               (ступень 2 для Фасонки/Подсистемы, Р3.2)
;; ============================================================
(defun su-get-block-props (obj / ent cached dynprops prop pname pval
                               has-len has-wid has-hei len-exact len-soft entry)
  (setq ent (vl-catch-all-apply 'vlax-vla-object->ename (list obj)))

  (if (vl-catch-all-error-p ent)
    '(nil nil nil nil nil)
    (progn
      (setq cached (assoc ent *su-block-props-cache*))
      (if cached
        (cdr cached)
        (progn
          (setq has-len nil has-wid nil has-hei nil
                len-exact nil len-soft nil)

          (setq dynprops
            (vl-catch-all-apply 'vlax-invoke
              (list obj 'GetDynamicBlockProperties)))

          (if (not (vl-catch-all-error-p dynprops))
            (foreach prop dynprops
              (setq pname
                (vl-catch-all-apply 'vla-get-PropertyName (list prop)))
              (if (and (not (vl-catch-all-error-p pname))
                       pname
                       (= (type pname) 'STR))
                (progn
                  (setq pname (strcase (vl-string-trim " \t\r\n" pname)))
                  (cond
                    ;; Ступень 1: точное совпадение "ДЛИНА"
                    ((= pname "ДЛИНА")
                     (setq has-len T)
                     (setq pval (vl-catch-all-apply 'vla-get-Value (list prop)))
                     (if (not (vl-catch-all-error-p pval))
                       (setq len-exact (su-value-to-number pval)))
                    )
                    ;; Ступень 2 (Р3.2): подстрока "ДЛИНА",
                    ;; исключаем свойства вида "Ширина_Длина_зазора"
                    ((and (vl-string-search "ДЛИНА" pname)
                          (not (vl-string-search "ШИРИНА" pname))
                          (not (vl-string-search "ВЫСОТА" pname)))
                     (if (null len-soft)
                       (progn
                         (setq pval (vl-catch-all-apply 'vla-get-Value (list prop)))
                         (if (not (vl-catch-all-error-p pval))
                           (setq len-soft (su-value-to-number pval)))
                       )
                     )
                    )
                  )
                  (if (vl-string-search "ШИРИНА" pname) (setq has-wid T))
                  (if (vl-string-search "ВЫСОТА" pname) (setq has-hei T))
                )
              )
            )
          )

          (setq entry (list has-len has-wid has-hei len-exact len-soft))
          (setq *su-block-props-cache*
            (cons (cons ent entry) *su-block-props-cache*))
          entry
        )
      )
    )
  )
)


;; ============================================================
;; Выбор вхождений блоков (INSERT) с учётом предварительного выбора
;; ИСПРАВЛЕНО (Р2.2): предвыбор обнуляется только при успехе
;; ДОБАВЛЕНО (Р3.3): сброс кэша свойств в начале выбора
;; ============================================================
(defun su-select-inserts (layers / ss i ent data layer out layer-name)
  (su-block-props-cache-clear)   ;; ДОБАВЛЕНО (Р3.3)
  (setq out '())

  (if (and (boundp '*extraction-preselected-set*) *extraction-preselected-set*)
    (progn
      (setq ss *extraction-preselected-set*)
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

  ;; ИСПРАВЛЕНО (Р2.2): обнуляем предварительный выбор
  ;; только если результат непустой
  (if (and (boundp '*extraction-preselected-set*)
           *extraction-preselected-set*)
    (if out
      (setq *extraction-preselected-set* nil)
    )
  )

  (reverse out)
)

;; ============================================================
;; Защищённое получение EffectiveName
;; ============================================================
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

;; ============================================================
;; Получение строкового значения видимости
;; ============================================================
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
;; Проверки свойств через кэш
;; ОБНОВЛЕНО (Р3.3): ранее каждая функция делала отдельный
;; проход по GetDynamicBlockProperties
;; ============================================================
(defun su-has-length-property (obj)
  (car (su-get-block-props obj))
)

(defun su-has-width-property (obj)
  (cadr (su-get-block-props obj))
)

(defun su-has-height-property (obj)
  (caddr (su-get-block-props obj))
)

;; ============================================================
;; Проверка пригодности динамического блока для раскроя хлыстов
;; ЛОГИКА БЕЗ ИЗМЕНЕНИЙ, но через кэш (Р3.3):
;;   ? Есть свойство "ДЛИНА" (точное совпадение)
;;   ? НЕТ свойства с подстрокой "ШИРИНА"
;;   ? НЕТ свойства с подстрокой "ВЫСОТА"
;; ============================================================
(defun su-is-valid-stock-block (obj / p)
  (setq p (su-get-block-props obj))
  (and (car p) (not (cadr p)) (not (caddr p)))
)

;; ============================================================
;; Получение имени типа динамического блока
;; ДОБАВЛЕНО (Этап 3.1): вынесено из cutline.lsp для унификации
;;
;; Логика:
;;   1. Если есть свойство "Видимость" с непустым значением
;;      ? возвращаем значение видимости
;;   2. Иначе ? возвращаем EffectiveName блока
;;   3. Если оба недоступны ? "Без имени"
;; ============================================================
(defun su-get-dynblock-type-name (ent / obj vis name)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
  (if (vl-catch-all-error-p obj)
    "Без имени"
    (progn
      (setq vis (vl-catch-all-apply 'su-get-visibility (list obj)))
      (if (or (vl-catch-all-error-p vis) (null vis) (not (= (type vis) 'STR)))
        (setq vis nil)
      )
      (if (and vis (> (strlen (vl-string-trim " \t\r\n" vis)) 0))
        (vl-string-trim " \t\r\n" vis)
        (progn
          (setq name (vl-catch-all-apply 'su-get-effective-name (list obj)))
          (if (or (vl-catch-all-error-p name) (null name) (not (= (type name) 'STR)))
            "Без имени"
            (if (> (strlen (vl-string-trim " \t\r\n" name)) 0)
              (vl-string-trim " \t\r\n" name)
              "Без имени"
            )
          )
        )
      )
    )
  )
)

;; ============================================================
;; Сбор уникальных типов динамических блоков из набора
;; ДОБАВЛЕНО (Этап 3.1)
;;
;; Параметры:
;;   ss        — selection set
;;   filter-fn — функция проверки пригодности блока (или nil для всех)
;;
;; Возвращает: отсортированный список строк — имена типов
;; ============================================================
(defun su-collect-dynblock-types (ss filter-fn / i ent typ obj types type-name)
  (setq types '() i 0)
  (if (null ss)
    '()
    (progn
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq typ (cdr (assoc 0 (entget ent))))
        (if (= typ "INSERT")
          (progn
            (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
            (if (not (vl-catch-all-error-p obj))
              (if (or (null filter-fn)
                      (apply filter-fn (list obj)))
                (progn
                  (setq type-name (su-get-dynblock-type-name ent))
                  (if (not (member type-name types))
                    (setq types (cons type-name types))
                  )
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
      (vl-sort types '(lambda (a b) (< (strcase a) (strcase b))))
    )
  )
)

;; ============================================================
;; Получение длины из динамических свойств
;; ОБНОВЛЕНО (Р3.2): двухступенчатый поиск
;;   Ступень 1: точное совпадение "ДЛИНА"
;;   Ступень 2: подстрока "ДЛИНА" (без "ШИРИНА"/"ВЫСОТА")
;; ОБНОВЛЕНО (Р3.3): значение берётся из кэша
;;
;; Для CUTLINE поведение не меняется: блоки предварительно
;; фильтруются по точному совпадению, поэтому ступень 2
;; в раскрое никогда не срабатывает.
;; Для Фасонки и Подсистемы ступень 2 спасает блоки
;; с именами свойств вида "Длина профиля".
;;
;; Возвращает: длина в мм (целое) или nil
;; ============================================================
(defun su-get-length (obj / p len)
  (setq p (su-get-block-props obj))
  (setq len (if (car p) (nth 3 p) (nth 4 p)))
  (if (numberp len)
    (atoi (rtos len 2 0))
    nil
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

(setq *su-cutline-types* '("LINE" "MLINE" "INSERT"))


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
;; Пост-фильтрация динамических блоков для раскроя
;; ДОБАВЛЕНО (Этап 2.2)
;; ============================================================
(defun su-filter-dynblocks (ss / i ent typ obj new-ss)
  (if (null ss)
    nil
    (progn
      (setq new-ss (ssadd))
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq typ (cdr (assoc 0 (entget ent))))

        (cond
          ((or (= typ "LINE") (= typ "MLINE"))
           (ssadd ent new-ss)
          )

          ((= typ "INSERT")
           (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
           (if (and
                 (not (vl-catch-all-error-p obj))
                 (su-is-valid-stock-block obj))
             (ssadd ent new-ss)
           )
          )
        )

        (setq i (1+ i))
      )

      (if (> (sslength new-ss) 0) new-ss nil)
    )
  )
)


;; ============================================================
;; Построение ssget-фильтра для CUTLINE
;; ОБНОВЛЕНО (Этап 2.2): добавлен INSERT
;; ============================================================

(defun su-build-cutline-ssfilter (layers / base layer-mask)
  (setq base
    (list '(0 . "LINE,MLINE,INSERT")))

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
;; ИСПРАВЛЕНО (Р2.2): предвыбор НЕ обнуляется при пустом
;; результате фильтрации
;; ДОБАВЛЕНО (Р3.3): сброс кэша свойств в начале выбора
;; ============================================================
(defun su-select-cutline-objects (layers / ss ssfilter raw-count pre-ss)
  (su-block-props-cache-clear)   ;; ДОБАВЛЕНО (Р3.3)
  (setq ss nil)
  (setq ssfilter (su-build-cutline-ssfilter layers))

  ;; Шаг 1: Предварительный выбор из диспетчера
  (if (and (boundp '*extraction-preselected-set*)
           *extraction-preselected-set*)
    (progn
      (setq pre-ss *extraction-preselected-set*)
      (setq ss (su-filter-ss-cutline pre-ss *su-cutline-types* layers))

      (if (and ss (> (sslength ss) 0))
        ;; Успех — обнуляем предвыбор
        (setq *extraction-preselected-set* nil)
        ;; Пустой результат — сохраняем предвыбор для повторной попытки
        (setq ss nil)
      )
    )
  )

  ;; Шаг 2: Если предвыбор не дал результата — текущий выбор в чертеже
  (if (null ss)
    (setq ss (ssget "_I" ssfilter))
  )

  ;; Шаг 3: Интерактивный выбор рамкой
  (if (null ss)
    (setq ss (ssget ssfilter))
  )

  ;; Шаг 4: Пост-фильтрация динамических блоков
  (if ss
    (progn
      (setq raw-count (sslength ss))
      (setq ss (su-filter-dynblocks ss))
      (if (and ss raw-count (< (sslength ss) raw-count))
        (princ (strcat "\n  После пост-фильтрации отсеяно: "
                       (itoa (- raw-count (sslength ss)))
                       " непригодных объектов (динамические блоки без свойства 'Длина' или с 'Ширина'/'Высота')"))
      )
    )
  )

  ss
)


(princ "\nSELECT-UTILS.LSP загружен.")
(princ)