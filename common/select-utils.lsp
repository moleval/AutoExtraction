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
;;;         после проверки результата. Если фильтрация вернула
;;;         пустой результат, предварительный выбор сохраняется
;;;         для повторной попытки.
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
;; Выбор вхождений блоков (INSERT) с учётом предварительного выбора
;; ============================================================
(defun su-select-inserts (layers / ss i ent data layer out layer-name)
  (setq out '())

  (if (and (boundp '*extraction-preselected-set*) *extraction-preselected-set*)
    (progn
      (setq ss *extraction-preselected-set*)
      ;; ИСПРАВЛЕНО (Р2.2): обнуляем только если результат непустой
      ;; (см. проверку после фильтрации ниже)
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
;; Проверка наличия свойства "ДЛИНА" у динамического блока
;; ДОБАВЛЕНО (Этап 2.1)
;; СТРОГИЙ ПОИСК (Вариант А): точное совпадение "ДЛИНА"
;; (регистронезависимо через strcase)
;; Возвращает: T если свойство найдено, иначе nil
;; ============================================================
(defun su-has-length-property (obj / dynprops prop pname result)
  (setq result nil)
  (setq dynprops
    (vl-catch-all-apply
      'vlax-invoke
      (list obj 'GetDynamicBlockProperties)
    )
  )
  (if (not (vl-catch-all-error-p dynprops))
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
            (setq result T)
          )
        )
      )
    )
  )
  result
)

;; ============================================================
;; Проверка наличия свойства "ШИРИНА" у динамического блока
;; ДОБАВЛЕНО (Этап 2.1)
;; ПОИСК ПО ПОДСТРОКЕ: любое свойство, содержащее "ШИРИНА"
;; (регистронезависимо)
;; Возвращает: T если свойство найдено, иначе nil
;; ============================================================
(defun su-has-width-property (obj / dynprops prop pname result)
  (setq result nil)
  (setq dynprops
    (vl-catch-all-apply
      'vlax-invoke
      (list obj 'GetDynamicBlockProperties)
    )
  )
  (if (not (vl-catch-all-error-p dynprops))
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
                (vl-string-search "ШИРИНА" (strcase pname))
              )
            (setq result T)
          )
        )
      )
    )
  )
  result
)

;; ============================================================
;; Проверка наличия свойства "ВЫСОТА" у динамического блока
;; ДОБАВЛЕНО (Этап 2.1)
;; ПОИСК ПО ПОДСТРОКЕ: любое свойство, содержащее "ВЫСОТА"
;; (регистронезависимо)
;; Возвращает: T если свойство найдено, иначе nil
;; ============================================================
(defun su-has-height-property (obj / dynprops prop pname result)
  (setq result nil)
  (setq dynprops
    (vl-catch-all-apply
      'vlax-invoke
      (list obj 'GetDynamicBlockProperties)
    )
  )
  (if (not (vl-catch-all-error-p dynprops))
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
                (vl-string-search "ВЫСОТА" (strcase pname))
              )
            (setq result T)
          )
        )
      )
    )
  )
  result
)

;; ============================================================
;; Проверка пригодности динамического блока для раскроя хлыстов
;; ДОБАВЛЕНО (Этап 2.1)
;;
;; БЛОК ПРИГОДЕН, ЕСЛИ:
;;   ? Есть свойство "ДЛИНА" (точное совпадение)
;;   ? НЕТ свойства с подстрокой "ШИРИНА"
;;   ? НЕТ свойства с подстрокой "ВЫСОТА"
;;
;; Возвращает: T если блок пригоден, иначе nil
;; ============================================================
(defun su-is-valid-stock-block (obj)
  (and
    ;; Есть точное свойство "ДЛИНА"
    (su-has-length-property obj)
    ;; НЕТ "ШИРИНА"
    (not (su-has-width-property obj))
    ;; НЕТ "ВЫСОТА"
    (not (su-has-height-property obj))
  )
)

;; ============================================================
;; Получение имени типа динамического блока
;; ДОБАВЛЕНО (Этап 3.1): вынесено из cutline.lsp для унификации
;;
;; Используется в:
;;   - CUTLINE (выпадающий список типов блоков)
;;   - SUBSYSTEM (группировка по типам в отчёте)
;;   - CUTSHEET (будущий модуль раскроя листа)
;;
;; Логика:
;;   1. Если есть свойство "Видимость" с непустым значением
;;      ? возвращаем значение видимости
;;   2. Иначе ? возвращаем EffectiveName блока
;;   3. Если оба недоступны ? "Без имени"
;;
;; Возвращает: строка — имя типа блока
;; ============================================================
(defun su-get-dynblock-type-name (ent / obj vis name)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
  (if (vl-catch-all-error-p obj)
    "Без имени"
    (progn
      ;; Пробуем получить видимость
      (setq vis (vl-catch-all-apply 'su-get-visibility (list obj)))
      (if (or (vl-catch-all-error-p vis) (null vis) (not (= (type vis) 'STR)))
        (setq vis nil)
      )
      ;; Если видимость есть и непустая — используем её
      (if (and vis (> (strlen (vl-string-trim " \t\r\n" vis)) 0))
        (vl-string-trim " \t\r\n" vis)
        ;; Иначе — EffectiveName
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
;; ДОБАВЛЕНО (Этап 3.1): вынесено из cutline.lsp для унификации
;;
;; Параметры:
;;   ss        — selection set
;;   filter-fn — функция проверки пригодности блока (или nil для всех)
;;
;; Используется в:
;;   - CUTLINE: (su-collect-dynblock-types ss 'su-is-valid-stock-block)
;;   - SUBSYSTEM: (su-collect-dynblock-types ss nil)
;;   - CUTSHEET: (su-collect-dynblock-types ss 'su-is-valid-sheet-block)
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
              ;; Проверяем пригодность блока (если задан фильтр)
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
      ;; Сортируем по алфавиту
      (vl-sort types '(lambda (a b) (< (strcase a) (strcase b))))
    )
  )
)

;; ============================================================
;; Получение длины из динамических свойств
;; ОБНОВЛЕНО (Этап 2.1): защита от ошибок через vl-catch-all-apply
;;
;; СТРОГИЙ ПОИСК (Вариант А):
;;   Только точное совпадение "ДЛИНА" (регистронезависимо)
;;
;; Это защищает от ложных срабатываний на свойства типа:
;;   "Длина общая", "Полная длина", "Ширина_Длина_зазора"
;;
;; Возвращает: длина в мм (целое число) или nil
;; ============================================================
(defun su-get-length (obj / dynprops prop pname value result)
  (setq result nil)
  (setq dynprops
    (vl-catch-all-apply
      'vlax-invoke
      (list obj 'GetDynamicBlockProperties)
    )
  )

  (if (not (vl-catch-all-error-p dynprops))
    (progn
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

      ;; Округление до целого числа (мм)
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
;; Раскрой хлыстов принимает:
;;   LINE   — отрезок;
;;   MLINE  — мультилиния;
;;   INSERT — динамический блок (с отсевом через
;;            su-is-valid-stock-block на этапе пост-фильтрации).
;;
;; Дуги, эллипсы, сплайны, полилинии — не принимаются.
;;
;; ДОБАВЛЕНО (Этап 2.2):
;;   "INSERT" добавлен для поддержки динамических блоков.
;;   Отсеивание непригодных блоков выполняется в
;;   su-select-cutline-objects через su-filter-dynblocks.
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
;;
;; Для каждого INSERT проверяется пригодность через
;; su-is-valid-stock-block:
;;   ? Есть свойство "ДЛИНА" (точное совпадение)
;;   ? НЕТ свойства с подстрокой "ШИРИНА"
;;   ? НЕТ свойства с подстрокой "ВЫСОТА"
;;
;; Непригодные блоки отсеиваются.
;; LINE и MLINE принимаются без проверки.
;;
;; Возвращает: отфильтрованный набор или nil
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
          ;; LINE и MLINE — принимаем без проверки
          ((or (= typ "LINE") (= typ "MLINE"))
           (ssadd ent new-ss)
          )

          ;; INSERT — проверяем пригодность
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
;; результате фильтрации — сохраняется для повторной попытки
;; ============================================================
(defun su-select-cutline-objects (layers / ss ssfilter raw-count pre-ss)
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