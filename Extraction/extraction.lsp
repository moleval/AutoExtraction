;;; ============================================================
;;; extraction.lsp — Диспетчер EXTRACTION / ЭКСТРАКЦИЯ
;;;
;;; ДОБАВЛЕНО (Б4): в ветке CLADDING функции run-task передается
;;; седьмым аргументом T — диспетчер собирает и полилинии, и блоки.
;;;
;;; АРХИТЕКТУРА ЗАЩИТЫ:
;;; Два флага:
;;;   *extraction-in-dialog*    — lifecycle DCL (нужен для *error*)
;;;   *extraction-updating-ui*  — единая защита от re-entrancy
;;;
;;; Принцип: когда программа модифицирует DCL (set_tile, start_list,
;;; mode_tile), устанавливается *extraction-updating-ui* = T.
;;; Callback'и, возникающие во время этого, не запускают бизнес-логику.
;;; ============================================================

(vl-load-com)

;; ============================================================
;; ПОСЛЕДНИЕ НАСТРОЙКИ
;; ============================================================

(if (not (boundp '*EXTRACTION-LAST-TASK*))
  (setq *EXTRACTION-LAST-TASK* 'FASONKA)
)

(if (not (boundp '*EXTRACTION-LAST-REPORT-MODE*))
  (setq *EXTRACTION-LAST-REPORT-MODE* "DETAIL")
)

(if (not (boundp '*EXTRACTION-LAST-EXPORT-EXCEL*))
  (setq *EXTRACTION-LAST-EXPORT-EXCEL* nil)
)

(if (not (boundp '*EXTRACTION-LAST-EXPORT-TXT*))
  (setq *EXTRACTION-LAST-EXPORT-TXT* nil)
)

(if (not (boundp '*EXTRACTION-LAST-CREATE-TABLE*))
  (setq *EXTRACTION-LAST-CREATE-TABLE* T)
)

(if (not (boundp '*EXTRACTION-LAST-FASONKA-LAYERS*))
  (setq *EXTRACTION-LAST-FASONKA-LAYERS* nil)
)

(if (not (boundp '*EXTRACTION-LAST-SUBSYSTEM-LAYERS*))
  (setq
    *EXTRACTION-LAST-SUBSYSTEM-LAYERS*
    '("Подсистема"
      "Подсистема алюминиевая"
      "Подсистема оцинкованная")
  )
)

(if (not (boundp '*EXTRACTION-LAST-SUBSYSTEM-CHECKS*))
  (setq *EXTRACTION-LAST-SUBSYSTEM-CHECKS* '(T T T))
)

;; Встроенные фильтры слоев диспетчера (ред. 2): Мои / Фасады / Витражи / Окна.
;; Состояние чекбоксов между открытиями окна — список ключей либо nil.
(if (not (boundp '*EXTRACTION-LAST-LAYER-FILTERS*))
  (setq *EXTRACTION-LAST-LAYER-FILTERS* nil)
)

(if (not (boundp '*extraction-preselected-set*))
  (setq *extraction-preselected-set* nil)
)

;; Единственная операция с предвыделением вне жизненного цикла диспетчера —
;; «взять и погасить» (consume-once). Библиотечные выборки (select-utils,
;; cs-build-filter-ss) не изменяют *extraction-preselected-set* напрямую,
;; а делегируют изъятие сюда (владелец состояния — диспетчер, Шаг 7).
(defun ex-take-preselected (/ v)
  (setq v *extraction-preselected-set*)
  (setq *extraction-preselected-set* nil)
  v)

(if (not (boundp '*EXTRACTION-MODULES-LOADED*))
  (setq *EXTRACTION-MODULES-LOADED* nil)
)

(if (not (boundp '*extraction-list-open*))
  (setq *extraction-list-open* nil)
)

(if (not (boundp '*extraction-in-dialog*))
  (setq *extraction-in-dialog* nil)
)

(if (not (boundp '*extraction-updating-ui*))
  (setq *extraction-updating-ui* nil)
)

;; ============================================================
;; Слои по умолчанию для каждой задачи
;; ============================================================

(if (not (boundp '*EXTRACTION-LAST-FASONKA-LAYERS*))
  (setq *EXTRACTION-LAST-FASONKA-LAYERS* '("Фасонка*" "Железо*"))
)

(if (not (boundp '*EXTRACTION-LAST-ZAPOLNENIE-LAYERS*))
  (setq *EXTRACTION-LAST-ZAPOLNENIE-LAYERS* '("Заполнение" "Стекло" "Обозначение ст-т"))
)

(if (not (boundp '*EXTRACTION-LAST-CLADDING-LAYERS*))
  (setq *EXTRACTION-LAST-CLADDING-LAYERS* '("*Облицовка*" "*Кассет*" "*Керамогранит*"))
)

(if (not (boundp '*EXTRACTION-LAST-VITRAZH-LAYERS*))
  (setq *EXTRACTION-LAST-VITRAZH-LAYERS* '("Витражи" "Стойк*" "Ригел*"))
)

(if (not (boundp '*EXTRACTION-LAST-CUTLINE-LAYERS*))
  (setq *EXTRACTION-LAST-CUTLINE-LAYERS* nil)
)

;; ============================================================
;; НАСТРОЙКИ ИЗ AutoExtraction\settings.ini
;; ============================================================
(defun extraction-apply-settings-defaults (/)
  (if (= (type ae-settings-load) 'SUBR)
    (ae-settings-load)
  )
  (if (= (type ae-settings-task-layers) 'SUBR)
    (progn
      (setq *EXTRACTION-LAST-FASONKA-LAYERS*
        (ae-settings-task-layers 'FASONKA))
      (setq *EXTRACTION-LAST-SUBSYSTEM-LAYERS*
        (ae-settings-task-layers 'SUBSYSTEM))
      (setq *EXTRACTION-LAST-CLADDING-LAYERS*
        (ae-settings-task-layers 'CLADDING))
      (setq *EXTRACTION-LAST-VITRAZH-LAYERS*
        (ae-settings-task-layers 'VITRAZH))
      (setq *EXTRACTION-LAST-ZAPOLNENIE-LAYERS*
        (ae-settings-task-layers 'ZAPOLNENIE))
    )
  )
  T
)


;; ============================================================
;; РАБОЧЕЕ СОСТОЯНИЕ
;; ============================================================

(setq *EXTRACTION-DCL-ID* nil)
(setq *EXTRACTION-ALL-LAYERS* nil)
(setq *EXTRACTION-VISIBLE-LAYERS* nil)
(setq *EXTRACTION-SELECTED-LAYERS* nil)
(setq *EXTRACTION-SELECTED-INDICES* nil)

;; Поиск слоя. Поведение один в один с поиском блока (blockrename):
;; подсказка-плейсхолдер видна, пока поле пусто; срабатывание по Enter;
;; запрос без символов маски превращается в поиск подстроки.
(if (not (boundp '*EXTRACTION-LAYER-SEARCH-HINT*))
  (setq *EXTRACTION-LAYER-SEARCH-HINT* "Поиск (по Enter):")
)
(setq *EXTRACTION-LAYER-SEARCH-PATTERN* "")

;; Активные фильтры слоев — список ключей (MY FACADES VITRAZH WINDOWS)
(setq *EXTRACTION-LAYER-FILTERS* nil)

;; Источник слоев последнего запуска: 'FILTER (групповые фильтры), 'MANUAL, nil
(setq *EXTRACTION-LAYERS-SOURCE* nil)

;; Ключи, DCL-плитки и подписи встроенных фильтров слоев
(setq *EXTRACTION-LAYER-FILTER-KEYS* '(MY FACADES VITRAZH WINDOWS))
(setq *EXTRACTION-LAYER-FILTER-TILES*
  '((MY . "chk_filter_my")
    (FACADES . "chk_filter_facades")
    (VITRAZH . "chk_filter_vitrazh")
    (WINDOWS . "chk_filter_windows")))
(setq *EXTRACTION-LAYER-FILTER-LABELS*
  '((MY . "Мои")
    (FACADES . "Фасады")
    (VITRAZH . "Витражи")
    (WINDOWS . "Окна")))

(setq *EXTRACTION-TASK-ID* *EXTRACTION-LAST-TASK*)
(setq *EXTRACTION-REPORT-MODE* *EXTRACTION-LAST-REPORT-MODE*)
(setq *EXTRACTION-EXPORT-EXCEL* *EXTRACTION-LAST-EXPORT-EXCEL*)
(setq *EXTRACTION-EXPORT-TXT* *EXTRACTION-LAST-EXPORT-TXT*)
(setq *EXTRACTION-CREATE-TABLE* *EXTRACTION-LAST-CREATE-TABLE*)

(setq *EXTRACTION-ACTION* 'CANCEL)


;; ============================================================
;; УНИКАЛЬНЫЕ СТРОКИ
;; ============================================================

(defun extraction-unique-ci (lst / out x key)
  (setq out '())
  (if (listp lst)
    (foreach x lst
      (if (= (type x) 'STR)
        (progn
          (setq key (strcase x))
          (if (not (vl-some '(lambda (y) (= (strcase y) key)) out))
            (setq out (cons x out))
          )
        )
      )
    )
  )
  (reverse out)
)

;; ============================================================
;; БЕЗОПАСНОЕ ЗАПОЛНЕНИЕ СПИСКА
;; Гарантия end_list обеспечивается локальным *error*
;; через флаг *extraction-list-open*.
;; ============================================================

(defun extraction-safe-fill-list (tile_name items / item)
  (setq *extraction-list-open* T)
  (start_list tile_name)
  (if (listp items)
    (foreach item items
      (if (= (type item) 'STR)
        (add_list item)
      )
    )
  )
  (end_list)
  (setq *extraction-list-open* nil)
)

;; ============================================================
;; ОЧИСТКА СПИСКА СЛОЕВ ДЛЯ РАСКРОЯ
;; ============================================================

(defun extraction-cut-clean-filter-layers (layers / out x sx)
  (setq out '())

  (if (listp layers)
    (foreach x layers
      (if (= (type x) 'STR)
        (progn
          (setq sx (strcase x))
          (if (and
                (/= sx "0")
                (/= sx "DEFPOINTS"))
            (setq out (cons x out))
          )
        )
      )
    )
  )

  (extraction-unique-ci (reverse out))
)


;; ============================================================
;; ОБРАБОТКА РЕЗУЛЬТАТА ВЫЗОВА МОДУЛЯ
;; ============================================================
(defun extraction-handle-module-result (r module-name / errMsg errMsgUp)
  (if (vl-catch-all-error-p r)
    (progn
      (setq errMsg (vl-catch-all-error-message r))
      (setq errMsgUp (strcase errMsg))

      (if (or
            (vl-string-search "QUIT" errMsgUp)
            (vl-string-search "CANCEL" errMsgUp)
            (vl-string-search "ОТМЕН" errMsgUp)
            (vl-string-search "ЗАВЕРШИТЬ" errMsgUp)
            (vl-string-search "ПРЕРВАТЬ" errMsgUp)
            (vl-string-search "ВЫЙТИ" errMsgUp))
        nil
        (princ (strcat "\nМодуль " module-name
                       " не загружен или ошибка выполнения: "
                       errMsg))
      )
    )
  )
)


;; ============================================================
;; ПОЛУЧЕНИЕ ВСЕХ СЛОЕВ ЧЕРТЕЖА
;; ============================================================

(defun extraction-layer-names ( / acad doc layers out name)
  (setq out '())
  (setq acad (vl-catch-all-apply 'vlax-get-acad-object '()))

  (if (and (not (vl-catch-all-error-p acad)) acad)
    (progn
      (setq doc
        (vl-catch-all-apply 'vla-get-ActiveDocument (list acad)))

      (if (and (not (vl-catch-all-error-p doc)) doc)
        (progn
          (setq layers
            (vl-catch-all-apply 'vla-get-Layers (list doc)))

          (if (and (not (vl-catch-all-error-p layers)) layers)
            (vlax-for lay layers
              (setq name
                (vl-catch-all-apply 'vla-get-Name (list lay)))

              (if (and
                    (not (vl-catch-all-error-p name))
                    (= (type name) 'STR)
                    (> (strlen name) 0))
                (setq out (cons name out))
              )
            )
          )
        )
      )
    )
  )

  (setq out
    (vl-remove-if-not '(lambda (x) (= (type x) 'STR)) out))

  (setq out
    (vl-sort out '(lambda (a b) (< (strcase a) (strcase b)))))

  (extraction-unique-ci out)
)


;; ============================================================
;; ЗАГРУЗКА МОДУЛЕЙ
;; ============================================================

(defun extraction-project-root ( / dsp)
  (setq dsp (findfile "extraction.lsp"))
  (if dsp
    (vl-filename-directory (vl-filename-directory dsp))
    nil
  )
)


(defun extraction-modules-dir ( / dsp)
  (setq dsp (findfile "extraction.lsp"))
  (if dsp
    (vl-filename-directory dsp)
    nil
  )
)


(defun extraction-load-all ( / root common f path)
  (setq root (extraction-project-root))

  (if root
    (progn

      (setq common (strcat root "\\common\\"))

      (foreach f
        '("task-utils.lsp"
          "settings-utils.lsp"
          "layer-utils.lsp"
          "select-utils.lsp"
          "excel-utils.lsp"
          "table-utils.lsp"
          "txt-utils.lsp"
          "validation-utils.lsp")

        (setq path (strcat common f))

        (if (findfile path)
          (load path)
          (princ (strcat "\n[EXTRACTION] Не найден: " path))
        )
      )

      (setq path (strcat root "\\Extraction\\fasonka.lsp"))
      (if (findfile path)
        (load path)
        (princ (strcat "\n[EXTRACTION] Не найден: " path)))

      (setq path (strcat root "\\Extraction\\subsystem.lsp"))
      (if (findfile path)
        (load path)
        (princ (strcat "\n[EXTRACTION] Не найден: " path)))

      (setq path (strcat root "\\Extraction\\cladding.lsp"))
      (if (findfile path)
        (load path)
        (princ (strcat "\n[EXTRACTION] Не найден: " path)))

      (setq path (strcat root "\\Extraction\\cutline.lsp"))
      (if (findfile path)
        (load path)
        (princ (strcat "\n[EXTRACTION] Не найден: " path)))

      (setq path (strcat root "\\Extraction\\cutsheet.lsp"))
      (if (findfile path)
        (load path)
        (princ (strcat "\n[EXTRACTION] Не найден: " path)))

      (setq path (strcat root "\\Extraction\\zapolnenie.lsp"))
      (if (findfile path)
        (load path)
        (princ (strcat "\n[EXTRACTION] Не найден: " path)))

      (setq path (strcat root "\\Extraction\\settings.lsp"))
      (if (findfile path)
        (load path)
        (princ (strcat "\n[EXTRACTION] Не найден: " path)))

      (setq path (strcat root "\\Extraction\\help.lsp"))
      (if (findfile path)
        (load path)
        (princ (strcat "\n[EXTRACTION] help.lsp not found: " path)))

      (setq path (strcat root "\\Extraction\\blockrename.lsp"))
      (if (findfile path)
        (load path)
        (princ (strcat "\n[EXTRACTION] Не найден: " path)))
    )

    (princ "\n[EXTRACTION] Корень проекта не найден.")
  )

  T
)


;; ============================================================
;; РАЗБОР ИНДЕКСОВ DCL
;; ============================================================

;; Этап 2 (V2): единый защищенный парсер списка индексов.
(defun extraction-parse-indices (s)
  (tu-parse-int-list s)
)


;; ============================================================
;; ПОЛУЧЕНИЕ ВЫБРАННЫХ СЛОЕВ
;; ============================================================

(defun extraction-selected-names ( / s indices out i n)
  (setq s (get_tile "lst_layers"))

  (if (or (null s) (/= (type s) 'STR) (= s ""))
    nil
    (progn
      (setq indices (extraction-parse-indices s))
      (setq out '())
      (setq n (length *EXTRACTION-VISIBLE-LAYERS*))

      (foreach i indices
        (if (and (numberp i) (>= i 0) (< i n))
          (setq out (cons (nth i *EXTRACTION-VISIBLE-LAYERS*) out))
        )
      )

      (extraction-unique-ci out)
    )
  )
)


;; ============================================================
;; ВСТРОЕННЫЕ ФИЛЬТРЫ СЛОЕВ (групповые фильтры AutoCAD)
;; Имена/маски фильтров — в common/layer-utils.lsp (ред. 1)
;; ============================================================

;; DCL-плитка чекбокса фильтра
(defun extraction-layer-filter-tile (key)
  (cdr (assoc key *EXTRACTION-LAYER-FILTER-TILES*))
)

;; Подпись фильтра: «Мои», «Фасады», «Витражи», «Окна»
(defun extraction-layer-filter-label (key)
  (cdr (assoc key *EXTRACTION-LAYER-FILTER-LABELS*))
)

;; Подписи активных фильтров одной строкой: «Мои, Окна»
(defun extraction-layer-filter-list-str (keys / s key lab)
  (setq s "")
  (foreach key keys
    (setq lab (extraction-layer-filter-label key))
    (if lab
      (setq s (if (= s "") lab (strcat s ", " lab))))
  )
  s
)

;; Заголовок источника слоев: «по фильтру: Мои» / «по фильтрам: Фасады, Окна»
(defun extraction-layer-filter-title (keys / s)
  (setq s (extraction-layer-filter-list-str keys))
  (if (= s "")
    nil
    (strcat (if (= (length keys) 1) "по фильтру: " "по фильтрам: ") s)
  )
)

;; Подпись источника слоев текущего запуска (CUTLINE/CUTSHEET, отчеты)
(defun extraction-layer-filter-source-title ()
  (if (eq *EXTRACTION-LAYERS-SOURCE* 'FILTER)
    (extraction-layer-filter-title *EXTRACTION-LAYER-FILTERS*)
    nil
  )
)

;; Есть ли активные фильтры слоев
(defun extraction-layer-filter-active-p ()
  (if *EXTRACTION-LAYER-FILTERS* T nil)
)

;; Состояние одного чекбокса
(defun extraction-layer-filter-tile-on-p (key / tile)
  (setq tile (extraction-layer-filter-tile key))
  (if tile (= (get_tile tile) "1") nil)
)

;; Считать активные фильтры из чекбоксов окна
(defun extraction-layer-filter-read-tiles ( / out key)
  (setq out '())
  (foreach key *EXTRACTION-LAYER-FILTER-KEYS*
    (if (extraction-layer-filter-tile-on-p key)
      (setq out (append out (list key))))
  )
  out
)

;; Программно выставить чекбоксы по списку ключей (внутри updating-ui)
(defun extraction-layer-filter-set-tiles (keys / key)
  (foreach key *EXTRACTION-LAYER-FILTER-KEYS*
    (set_tile (extraction-layer-filter-tile key)
      (if (member key keys) "1" "0"))
  )
)

;; Слои по активным фильтрам (через common/layer-utils.lsp)
(defun extraction-layer-filter-layers (keys / r)
  (setq r (vl-catch-all-apply 'tu-layer-filter-layers-for-keys (list keys)))
  (if (vl-catch-all-error-p r) nil r)
)

;; Есть ли в чертеже хотя бы один групповой фильтр (пустой словарь = нет)
(defun extraction-layer-filter-available-p ( / r)
  (setq r (vl-catch-all-apply 'tu-layer-filter-available-p '()))
  (if (vl-catch-all-error-p r) nil r)
)

;; Диагностика пустого результата фильтра (единый формат сообщений, командная строка)
(defun extraction-layer-filter-fail-msg (keys)
  (if (extraction-layer-filter-available-p)
    (strcat "[EXTRACTION][LAYER-UTILS] Групповые фильтры по маскам не найдены: "
            (extraction-layer-filter-list-str keys) ".")
    "[EXTRACTION][LAYER-UTILS] Групповой фильтр отсутствует в чертеже."
  )
)

;; Текст проблемы фильтра для окна: короткий, без служебных меток
;; (строка состояния обрезается по ширине окна).
;; no-dict = T — в чертеже нет ни одного группового фильтра.
(defun extraction-layer-filter-problem-text (keys no-dict / title)
  (if no-dict
    "Групповой фильтр отсутствует"
    (progn
      (setq title (extraction-layer-filter-title keys))
      (if title
        (strcat "Нет слоев " title)
        "Нет слоев по фильтру"))
  )
)

;; Короткая подсказка в строку состояния окна. nil — фильтры дали слои.
(defun extraction-layer-filter-hint-msg (keys)
  (if (extraction-layer-filter-layers keys)
    nil
    (extraction-layer-filter-problem-text
      keys (not (extraction-layer-filter-available-p)))
  )
)

;; Слои для запуска задачи.
;; Возвращает (СЛОИ ИСТОЧНИК) либо nil, если запуск запрещен:
;; фильтр активен, но не дал ни одного слоя — молча брать все слои нельзя.
(defun extraction-resolve-layers (clean / manual layers)
  (setq manual (extraction-selected-names))

  (cond
    ((and manual (> (length manual) 0))
     (list manual 'MANUAL))

    ((extraction-layer-filter-active-p)
     (if *EXTRACTION-VISIBLE-LAYERS*
       (progn
         (setq layers *EXTRACTION-VISIBLE-LAYERS*)
         (if clean
           (setq layers (extraction-cut-clean-filter-layers layers)))
         (list layers 'FILTER))
       (progn
         (alert
           (strcat (extraction-layer-filter-problem-text
                     *EXTRACTION-LAYER-FILTERS*
                     (not (extraction-layer-filter-available-p)))
                   ".\nЗадача не запущена — обработка всех слоев вместо фильтра"
                   " запрещена.\nПроверьте групповые фильтры слоев в чертеже."))
         (princ (strcat "\n"
                        (extraction-layer-filter-fail-msg *EXTRACTION-LAYER-FILTERS*)))
         nil)))

    (T (list nil nil)))
)


;; ============================================================
;; ПОИСК СЛОЯ
;; Зеркало поиска блока: подсказка-плейсхолдер, маска wcmatch,
;; срабатывание по Enter. Поиск СУЖАЕТ результат групповых фильтров,
;; а не подменяет его, и не меняет правило «слои не выбраны — все слои».
;; ============================================================

;; Значение поля без подсказки (вырезается из любой позиции:
;; текст, набранный ДО подсказки, сохраняется)
(defun extraction-layer-search-value ( / v p)
  (setq v (get_tile "edt_layer_search"))
  (if (= (type v) 'STR)
    (progn
      (cond
        ((= v *EXTRACTION-LAYER-SEARCH-HINT*)
         (setq v ""))
        ((setq p (vl-string-search *EXTRACTION-LAYER-SEARCH-HINT* v))
         (setq v (strcat
                   (substr v 1 p)
                   (substr v (+ p (strlen *EXTRACTION-LAYER-SEARCH-HINT*) 1)))))
      )
      v)
    "")
)

;; Символы маски — те же, что у поиска блока
(defun extraction-layer-search-has-mask-p (text)
  (and
    (= (type text) 'STR)
    (or
      (vl-string-search "*" text)
      (vl-string-search "?" text)
      (vl-string-search "#" text)
      (vl-string-search "@" text))
  )
)

;; Запрос без маски = поиск подстроки
(defun extraction-layer-search-normalize (text)
  (if (or (/= (type text) 'STR) (= text ""))
    ""
    (if (extraction-layer-search-has-mask-p text)
      text
      (strcat "*" text "*"))
  )
)

(defun extraction-layer-search-match-p (name pattern / normalized)
  (if (or (/= (type name) 'STR) (/= (type pattern) 'STR) (= pattern ""))
    T
    (progn
      (setq normalized (extraction-layer-search-normalize pattern))
      (if (= normalized "")
        T
        (wcmatch (strcase name) (strcase normalized)))
    )
  )
)

;; Отбор по запросу. Пустой запрос список не меняет.
(defun extraction-layer-search-apply (layers / pattern)
  (setq pattern *EXTRACTION-LAYER-SEARCH-PATTERN*)
  (if (or (/= (type pattern) 'STR) (= pattern "") (not (listp layers)))
    layers
    (vl-remove-if-not
      '(lambda (x) (extraction-layer-search-match-p x pattern))
      layers)
  )
)

;; Строка состояния: фильтры + результат поиска. Сообщение об отсутствии
;; группового фильтра имеет приоритет — поиск дописывается только тогда,
;; когда базовый список непустой.
(defun extraction-layer-status-text (base-hint keys base-count found-count)
  (if (or (/= (type *EXTRACTION-LAYER-SEARCH-PATTERN*) 'STR)
          (= *EXTRACTION-LAYER-SEARCH-PATTERN* "")
          (<= base-count 0))
    base-hint
    (if keys
      (strcat "Фильтр " (extraction-layer-filter-list-str keys)
              ": " (itoa base-count) " | найдено " (itoa found-count))
      (strcat "Найдено слоев: " (itoa found-count) " из " (itoa base-count)))
  )
)

;; Обработчик поля поиска. Выбор начинается заново: при смене запроса
;; выделение сбрасывается (решение по UI, как у поиска блока).
(defun extraction-layer-search-changed ( / v tile)
  (if *extraction-updating-ui*
    nil
    (progn
      (setq v    (extraction-layer-search-value)
            tile (get_tile "edt_layer_search"))
      ;; нормализуем текст поля: подсказка срезана; пусто = снова подсказка
      (if (/= v tile)
        (progn
          (setq *extraction-updating-ui* T)
          (set_tile "edt_layer_search"
            (if (= v "") *EXTRACTION-LAYER-SEARCH-HINT* v))
          (setq *extraction-updating-ui* nil)
        )
      )
      (setq *EXTRACTION-LAYER-SEARCH-PATTERN* v)
      (setq *EXTRACTION-SELECTED-LAYERS* nil)
      (extraction-rebuild-layer-list)
    )
  )
  T
)


;; ============================================================
;; ОБНОВЛЕНИЕ АКТИВНОСТИ КНОПОК ВЫБОРА
;; Защищено флагом *extraction-updating-ui*
;; ============================================================

(defun extraction-update-select-buttons ()
  (setq *extraction-updating-ui* T)
  (if (extraction-all-layers-selected-p)
    (progn
      (mode_tile "btn_select_all" 1)
      (mode_tile "btn_clear_all" 0)
    )
    (progn
      (mode_tile "btn_select_all" 0)
      (mode_tile "btn_clear_all" 1)
    )
  )
  (setq *extraction-updating-ui* nil)
)


;; ============================================================
;; ПЕРЕСТРОЕНИЕ СПИСКА СЛОЕВ
;; Защищено флагом *extraction-updating-ui*
;; ============================================================

(defun extraction-rebuild-layer-list ( / keys layers hint msg base-count)

  (setq *extraction-updating-ui* T)

  (setq *EXTRACTION-SELECTED-INDICES* '())

  (setq keys *EXTRACTION-LAYER-FILTERS*)

  (if keys
    (progn
      ;; Слои фильтров строит common/layer-utils.lsp
      (setq layers (extraction-layer-filter-layers keys))

      (if layers
        (setq hint
          (strcat "Фильтр " (extraction-layer-filter-list-str keys)
                  ": слоев " (itoa (length layers))))
        (progn
          ;; Причины разные: нет групповых фильтров в чертеже либо
          ;; фильтры есть, но слоев по ним нет. В окно — короткий текст,
          ;; в командную строку — сообщение с меткой (fail-msg).
          (setq msg (extraction-layer-filter-fail-msg keys))
          (setq hint (extraction-layer-filter-hint-msg keys)))
      )

      ;; Пустой результат фильтра НЕ подменяем всеми слоями чертежа
      (if msg (princ (strcat "\n" msg)))
      (setq *EXTRACTION-VISIBLE-LAYERS* (if (listp layers) layers '())))
    (progn
      (setq *EXTRACTION-VISIBLE-LAYERS* *EXTRACTION-ALL-LAYERS*)
      (setq hint "Если слои не выбраны — поиск по всем слоям"))
  )

  (setq *EXTRACTION-VISIBLE-LAYERS*
    (vl-sort
      (vl-remove-if-not
        '(lambda (x) (= (type x) 'STR))
        (if (listp *EXTRACTION-VISIBLE-LAYERS*)
          *EXTRACTION-VISIBLE-LAYERS*
          '()))
      '(lambda (a b) (< (strcase a) (strcase b)))))

  (setq *EXTRACTION-VISIBLE-LAYERS*
    (extraction-unique-ci *EXTRACTION-VISIBLE-LAYERS*))

  ;; Поиск слоя сужает результат фильтров (а не заменяет его)
  (setq base-count (length *EXTRACTION-VISIBLE-LAYERS*))
  (setq *EXTRACTION-VISIBLE-LAYERS*
    (extraction-layer-search-apply *EXTRACTION-VISIBLE-LAYERS*))
  (setq hint
    (extraction-layer-status-text
      hint keys base-count (length *EXTRACTION-VISIBLE-LAYERS*)))

  (extraction-safe-fill-list "lst_layers" *EXTRACTION-VISIBLE-LAYERS*)
  (set_tile "lst_layers" "")
  (set_tile "txt_layers_hint" hint)
  (extraction-update-select-buttons)

  (setq *extraction-updating-ui* nil)
)


;; ============================================================
;; СПЕЦИАЛЬНЫЙ СПИСОК ПОДСИСТЕМЫ
;; ============================================================


;; ============================================================
;; ОЧИСТКА ВЫБОРА
;; ============================================================


;; ============================================================
;; СИНХРОНИЗАЦИЯ ЧЕКБОКСОВ ПОДСИСТЕМЫ
;; Защищено флагом *extraction-updating-ui*
;; ============================================================

(defun extraction-sync-checks-from-layers ( / selected)
  (setq *extraction-updating-ui* T)

  (setq selected (extraction-selected-names))

  (set_tile "chk_subsystem_1"
    (if (member "Подсистема" selected) "1" "0"))

  (set_tile "chk_subsystem_2"
    (if (member "Подсистема алюминиевая" selected) "1" "0"))

  (set_tile "chk_subsystem_3"
    (if (member "Подсистема оцинкованная" selected) "1" "0"))

  (setq *extraction-updating-ui* nil)
)


;; ============================================================
;; УСТАНОВКА ВЫДЕЛЕНИЯ СЛОЕВ
;; Защищено флагом *extraction-updating-ui*
;; ============================================================
(defun extraction-select-layers-in-list
       (layers-to-select / i item selected str after-set)

  (setq *extraction-updating-ui* T)

  (setq selected '())
  (setq i 0)

  (foreach item *EXTRACTION-VISIBLE-LAYERS*
    (if (vl-some
          '(lambda (x)
             (or
               (= (strcase x) (strcase item))
               (wcmatch (strcase item) (strcase x))
             )
           )
          layers-to-select)
      (setq selected (cons i selected))
    )
    (setq i (1+ i))
  )

  (setq selected (reverse selected))
  (setq str "")

  (foreach i selected
    (setq str
      (if (= str "")
        (itoa i)
        (strcat str " " (itoa i))))
  )

  (set_tile "lst_layers" str)
  (setq after-set (get_tile "lst_layers"))

  (if (/= str after-set)
    (progn
      (extraction-safe-fill-list "lst_layers" *EXTRACTION-VISIBLE-LAYERS*)
      (set_tile "lst_layers" str)
      (setq after-set (get_tile "lst_layers"))
    )
  )

  ;; layer-selection-changed проверяет updating-ui и пропустит бизнес-логику
  (extraction-layer-selection-changed)

  (extraction-update-select-buttons)

  (setq *extraction-updating-ui* nil)
)


;; ============================================================
;; ПРОВЕРКА, ВСЕ ЛИ ВИДИМЫЕ СЛОИ ВЫБРАНЫ
;; ============================================================

(defun extraction-all-layers-selected-p ( / selected total)
  (setq selected (extraction-selected-names))
  (setq total (length *EXTRACTION-VISIBLE-LAYERS*))
  (and selected (= (length selected) total))
)


;; ============================================================
;; ВЫБРАТЬ ВСЕ ВИДИМЫЕ СЛОИ
;; ============================================================

(defun extraction-select-all-layers ( / str i)
  (setq str "")
  (setq i 0)

  (foreach item *EXTRACTION-VISIBLE-LAYERS*
    (setq str
      (if (= str "")
        (itoa i)
        (strcat str " " (itoa i))))
    (setq i (1+ i))
  )

  (extraction-select-layers-in-list *EXTRACTION-VISIBLE-LAYERS*)
)


;; ============================================================
;; СНЯТЬ ВЫДЕЛЕНИЕ СО ВСЕХ СЛОЕВ
;; ============================================================

(defun extraction-clear-all-layers ()
  (set_tile "lst_layers" "")
  (extraction-layer-selection-changed)
  (extraction-update-select-buttons)
)


;; ============================================================
;; ИЗМЕНЕНИЕ ЧЕКБОКСА ПОДСИСТЕМЫ
;; Пользовательский callback: проверяет *extraction-updating-ui*
;; ============================================================

(defun extraction-subsystem-check-changed
       (key / val layers-to-select current-selection)

  ;; Если это программное обновление UI — пропускаем
  (if *extraction-updating-ui*
    nil
    (progn
      (setq val
        (= (get_tile
             (strcat "chk_subsystem_" (itoa key))) "1"))

      (setq *EXTRACTION-LAST-SUBSYSTEM-CHECKS*
        (list
          (if (= key 1) val (car *EXTRACTION-LAST-SUBSYSTEM-CHECKS*))
          (if (= key 2) val (cadr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*))
          (if (= key 3) val (caddr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*))))

      (setq layers-to-select '())

      (if (car *EXTRACTION-LAST-SUBSYSTEM-CHECKS*)
        (setq layers-to-select (cons "Подсистема" layers-to-select)))

      (if (cadr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*)
        (setq layers-to-select (cons "Подсистема алюминиевая" layers-to-select)))

      (if (caddr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*)
        (setq layers-to-select (cons "Подсистема оцинкованная" layers-to-select)))

      (setq current-selection (extraction-selected-names))

      (setq current-selection
        (vl-remove-if
          '(lambda (x)
             (or (= (strcase x) "ПОДСИСТЕМА")
                 (= (strcase x) "ПОДСИСТЕМА АЛЮМИНИЕВАЯ")
                 (= (strcase x) "ПОДСИСТЕМА ОЦИНКОВАННАЯ")))
          current-selection))

      (setq current-selection
        (append current-selection layers-to-select))

      (extraction-select-layers-in-list current-selection)
    )
  )
)


;; ============================================================
;; ПЕРЕКЛЮЧЕНИЕ ЗАДАЧИ И ВОССТАНОВЛЕНИЕ СЛОЕВ
;; Пользовательский callback: проверяет *extraction-updating-ui*
;; ============================================================
(defun extraction-toggle-subsystem-layers ( / layers-to-select)

  ;; Если это программное обновление UI — пропускаем
  (if *extraction-updating-ui*
    nil
    (progn
      (setq *extraction-updating-ui* T)

      ;; Включение/выключение блока подсистемы
      (cond
        ((= (get_tile "rb_task_subsystem") "1")
         (mode_tile "box_subsystem_layers" 0)
         (mode_tile "chk_subsystem_1" 0)
         (mode_tile "chk_subsystem_2" 0)
         (mode_tile "chk_subsystem_3" 0)
        )
        (T
         (mode_tile "box_subsystem_layers" 1)
         (mode_tile "chk_subsystem_1" 1)
         (mode_tile "chk_subsystem_2" 1)
         (mode_tile "chk_subsystem_3" 1)
        )
      )

      (setq *extraction-updating-ui* nil)

      ;; Перестроение списка и восстановление выбора
      (cond
        ((= (get_tile "rb_task_subsystem") "1")
         (extraction-rebuild-layer-list)

         (setq layers-to-select *EXTRACTION-LAST-SUBSYSTEM-LAYERS*)

         (if (null layers-to-select)
           (progn
             (setq layers-to-select '())
             (if (car *EXTRACTION-LAST-SUBSYSTEM-CHECKS*)
               (setq layers-to-select (cons "Подсистема" layers-to-select)))
             (if (cadr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*)
               (setq layers-to-select (cons "Подсистема алюминиевая" layers-to-select)))
             (if (caddr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*)
               (setq layers-to-select (cons "Подсистема оцинкованная" layers-to-select)))
           )
         )

         (extraction-select-layers-in-list layers-to-select)
         (extraction-sync-checks-from-layers)
        )

        ((= (get_tile "rb_task_fasonka") "1")
         (extraction-rebuild-layer-list)
         (if *EXTRACTION-LAST-FASONKA-LAYERS*
           (extraction-select-layers-in-list *EXTRACTION-LAST-FASONKA-LAYERS*)
         )
        )

        ((= (get_tile "rb_task_zapolnenie") "1")
         (extraction-rebuild-layer-list)
         (if *EXTRACTION-LAST-ZAPOLNENIE-LAYERS*
           (extraction-select-layers-in-list *EXTRACTION-LAST-ZAPOLNENIE-LAYERS*)
         )
        )

        ((= (get_tile "rb_task_cladding") "1")
         (extraction-rebuild-layer-list)
         (if *EXTRACTION-LAST-CLADDING-LAYERS*
           (extraction-select-layers-in-list *EXTRACTION-LAST-CLADDING-LAYERS*)
         )
        )

        ((= (get_tile "rb_task_vitrazh") "1")
         (extraction-rebuild-layer-list)
         (if *EXTRACTION-LAST-VITRAZH-LAYERS*
           (extraction-select-layers-in-list *EXTRACTION-LAST-VITRAZH-LAYERS*)
         )
        )

        (T
         (extraction-rebuild-layer-list)
        )
      )
    )
  )
)


;; ============================================================
;; ИЗМЕНЕНИЕ ВЫБОРА В СПИСКЕ
;; Пользовательский callback: проверяет *extraction-updating-ui*
;; ============================================================
(defun extraction-layer-selection-changed ( / selected)

  ;; Если это программное обновление UI — пропускаем бизнес-логику
  (if *extraction-updating-ui*
    nil
    (progn
      (setq selected (extraction-selected-names))
      (setq *EXTRACTION-SELECTED-LAYERS* selected)

      (cond
        ((eq *EXTRACTION-TASK-ID* 'SUBSYSTEM)
         (setq *EXTRACTION-LAST-SUBSYSTEM-LAYERS* selected)
         (extraction-sync-checks-from-layers))

        ((eq *EXTRACTION-TASK-ID* 'FASONKA)
         (setq *EXTRACTION-LAST-FASONKA-LAYERS* selected))

        ((eq *EXTRACTION-TASK-ID* 'ZAPOLNENIE)
         (setq *EXTRACTION-LAST-ZAPOLNENIE-LAYERS* selected))

        ((eq *EXTRACTION-TASK-ID* 'CLADDING)
         (setq *EXTRACTION-LAST-CLADDING-LAYERS* selected))

        ((eq *EXTRACTION-TASK-ID* 'VITRAZH)
         (setq *EXTRACTION-LAST-VITRAZH-LAYERS* selected))
      )

      (extraction-update-select-buttons)
    )
  )
)


(defun extraction-layer-selection ()
  (extraction-layer-selection-changed)
)


;; ============================================================
;; ЧТЕНИЕ ПАРАМЕТРОВ
;; ============================================================

(defun extraction-read-params ( / selected r ok)

  (cond
    ((= (get_tile "rb_task_fasonka") "1")
     (setq *EXTRACTION-TASK-ID* 'FASONKA))

    ((= (get_tile "rb_task_subsystem") "1")
     (setq *EXTRACTION-TASK-ID* 'SUBSYSTEM))

    ((= (get_tile "rb_task_cladding") "1")
     (setq *EXTRACTION-TASK-ID* 'CLADDING))

    ((= (get_tile "rb_task_vitrazh") "1")
     (setq *EXTRACTION-TASK-ID* 'VITRAZH))

    ((= (get_tile "rb_task_zapolnenie") "1")
     (setq *EXTRACTION-TASK-ID* 'ZAPOLNENIE))

    (T
     (setq *EXTRACTION-TASK-ID* 'FASONKA))
  )

  (setq *EXTRACTION-LAST-TASK* *EXTRACTION-TASK-ID*)

  ;; Слои задачи (ред. 2): ручной выбор -> слои фильтров -> все слои (nil).
  ;; Фильтр активен, но слоёв нет — запуск запрещён (extraction-resolve-layers).
  (setq r (extraction-resolve-layers nil))

  (if r
    (progn
      (setq selected (car r))
      (setq *EXTRACTION-LAYERS-SOURCE* (cadr r))
    )
    (setq selected '())
  )

  (setq *EXTRACTION-SELECTED-LAYERS* (if r selected nil))
  (setq ok (if r T nil))

  (if ok
    (cond
      ((eq *EXTRACTION-TASK-ID* 'FASONKA)
       (setq *EXTRACTION-LAST-FASONKA-LAYERS* selected)
      )

      ((eq *EXTRACTION-TASK-ID* 'SUBSYSTEM)
       (setq *EXTRACTION-LAST-SUBSYSTEM-LAYERS* selected)
       (if (null selected)
         (progn
           (alert "Не выбрано ни одного слоя подсистемы.")
           (setq ok nil)
         )
       )
      )

      ((eq *EXTRACTION-TASK-ID* 'CLADDING)
       (setq *EXTRACTION-LAST-CLADDING-LAYERS* selected)
      )

      ((eq *EXTRACTION-TASK-ID* 'VITRAZH)
       (setq *EXTRACTION-LAST-VITRAZH-LAYERS* selected)
      )

      ((eq *EXTRACTION-TASK-ID* 'ZAPOLNENIE)
       (setq *EXTRACTION-LAST-ZAPOLNENIE-LAYERS* selected)
      )
    )
  )

  (setq *EXTRACTION-REPORT-MODE*
    (if (= (get_tile "rb_summary") "1") "SUMMARY" "DETAIL"))

  (setq *EXTRACTION-LAST-REPORT-MODE* *EXTRACTION-REPORT-MODE*)

  (setq *EXTRACTION-EXPORT-EXCEL*
    (= (get_tile "chk_xls") "1"))

  (setq *EXTRACTION-LAST-EXPORT-EXCEL* *EXTRACTION-EXPORT-EXCEL*)

  (setq *EXTRACTION-EXPORT-TXT*
    (= (get_tile "chk_txt") "1"))

  (setq *EXTRACTION-LAST-EXPORT-TXT* *EXTRACTION-EXPORT-TXT*)

  (setq *EXTRACTION-CREATE-TABLE*
    (= (get_tile "chk_acad") "1"))

  (setq *EXTRACTION-LAST-CREATE-TABLE* *EXTRACTION-CREATE-TABLE*)

  ;; Фильтр активен, но слоёв нет — запуск запрещён (диалог остаётся открытым)
  (if ok T nil)
)


;; ============================================================
;; ЗАПУСК ЗАДАЧИ
;; ДОБАВЛЕНО (Б4): в ветке CLADDING передается T седьмым
;; аргументом — диспетчер собирает и полилинии, и блоки.
;; ============================================================
(defun run-task
       (task-id layers report-mode export-excel export-txt
                create-table save-base / r uDoc)

  ;; V10: Undo-группа - весь прогон задачи откатывается одним U
  (setq uDoc (tu-undo-begin))
  (setq *AE-SETTINGS-TABLE-TASK* task-id)

  (cond
    ((eq task-id 'FASONKA)
     (setq *EXTRACTION-LAST-FASONKA-LAYERS* layers))
    ((eq task-id 'SUBSYSTEM)
     (setq *EXTRACTION-LAST-SUBSYSTEM-LAYERS* layers))
    ((eq task-id 'ZAPOLNENIE)
     (setq *EXTRACTION-LAST-ZAPOLNENIE-LAYERS* layers))
    ((eq task-id 'CLADDING)
     (setq *EXTRACTION-LAST-CLADDING-LAYERS* layers))
    ((eq task-id 'VITRAZH)
     (setq *EXTRACTION-LAST-VITRAZH-LAYERS* layers))
  )

  (cond
    ((eq task-id 'FASONKA)
     (setq r
       (vl-catch-all-apply 'fasonka-main
         (list layers report-mode export-excel export-txt
               create-table save-base)))
     (extraction-handle-module-result r "Фасонка")
    )

    ((eq task-id 'SUBSYSTEM)
     (setq r
       (vl-catch-all-apply 'subsystem-main
         (list layers report-mode export-excel export-txt
               create-table save-base)))
     (extraction-handle-module-result r "Подсистема")
    )

    ;; ДОБАВЛЕНО (Б4): T = делать блоки вслед за полилиниями
    ((eq task-id 'CLADDING)
     (setq r
       (vl-catch-all-apply 'cladding-main
         (list layers report-mode export-excel export-txt
               create-table save-base T)))
     (extraction-handle-module-result r "Облицовка")
    )

    ((eq task-id 'VITRAZH)
     (setq r
       (vl-catch-all-apply 'vitrazh-main
         (list layers report-mode export-excel export-txt
               create-table save-base)))
     (extraction-handle-module-result r "Витраж")
    )

    ((eq task-id 'ZAPOLNENIE)
     (setq r
       (vl-catch-all-apply 'zapolnenie-main
         (list layers report-mode export-excel export-txt
               create-table save-base)))
     (extraction-handle-module-result r "Заполнение")
    )

    (T
     (princ "\nНеизвестная задача."))
  )
  ;; V13: «всё или ничего» - обломки неудачного прогона откатываются автоматически
  (if (vl-catch-all-error-p r)
    (progn
      (tu-undo-cancel uDoc)
      (princ "\n[EX] Незавершённая операция откачена (обломков не осталось)."))
    (tu-undo-end uDoc))
)


;; ============================================================
;; ПОМОЩЬ
;; ============================================================

(defun extraction-help ()
  (setq *EXTRACTION-ACTION* 'HELP)
  (done_dialog 1)
)


(defun extraction-settings ()
  (setq *EXTRACTION-ACTION* 'SETTINGS)
  (done_dialog 1)
)


;; ============================================================
;; КНОПКИ
;; ============================================================

(defun extraction-save ()
  (if (extraction-read-params)
    (progn
      (setq *EXTRACTION-ACTION* 'SAVE)
      (done_dialog 1)
    )
  )
)


(defun extraction-saveas ()
  (if (extraction-read-params)
    (progn
      (setq *EXTRACTION-ACTION* 'SAVEAS)
      (done_dialog 1)
    )
  )
)


(defun extraction-close ()
  (setq *EXTRACTION-ACTION* 'CANCEL)
  (done_dialog 0)
)


;; ============================================================
;; КНОПКА "РАСКРОЙ ХЛЫСТА"
;; ============================================================
(defun extraction-cutline ( / r selected)

  (setq *CUTLINE-CREATE-TABLE* (= (get_tile "chk_acad") "1"))
  (setq *CUTLINE-CREATE-XLS*  (= (get_tile "chk_xls") "1"))

  (setq *CUTLINE-IS-AUTO-FILTER* nil)

  ;; Слои: ручной выбор -> слои фильтров (без слоя 0/DEFPOINTS) -> все слои
  (setq r (extraction-resolve-layers T))

  (if (null r)
    nil
    (progn
      (setq selected (car r))
      (setq *EXTRACTION-LAYERS-SOURCE* (cadr r))

      (if (eq *EXTRACTION-LAYERS-SOURCE* 'FILTER)
        (setq *CUTLINE-IS-AUTO-FILTER* T)
      )

      (if selected
        (setq selected (vl-sort selected '(lambda (a b) (< (strcase a) (strcase b)))))
      )

      (setq *EXTRACTION-SELECTED-LAYERS* selected)
      (setq *EXTRACTION-ACTION* 'CUTLINE)
      (done_dialog 1)
    )
  )
)


;; ============================================================
;; КНОПКА "РАСКРОЙ ЛИСТА"
;; ============================================================
(defun extraction-cutsheet ( / r selected)
  (setq *CUTSHEET-CREATE-TABLE*
    (= (get_tile "chk_acad") "1"))

  (setq *CUTSHEET-CREATE-XLS*
    (= (get_tile "chk_xls") "1"))

  ;; Слои: ручной выбор -> слои фильтров (без слоя 0/DEFPOINTS) -> все слои
  (setq r (extraction-resolve-layers T))

  (if r
    (progn
      (setq selected (car r))
      (setq *EXTRACTION-LAYERS-SOURCE* (cadr r))

      (setq *EXTRACTION-SELECTED-LAYERS* selected)
      (setq *EXTRACTION-ACTION* 'CUTSHEET)
      (done_dialog 1)
    )
  )
)


;; ============================================================
;; ФИЛЬТРЫ СЛОЁВ — ЕДИНЫЙ ОБРАБОТЧИК ЧЕКБОКСОВ
;; Пользовательский callback: проверяет *extraction-updating-ui*
;; ============================================================

(defun extraction-layer-filter-changed ( / keys keep)

  ;; Если это программное обновление UI — пропускаем
  (if *extraction-updating-ui*
    nil
    (progn
      ;; 1-2) состояние чекбоксов -> список активных фильтров
      (setq keys (extraction-layer-filter-read-tiles))
      (setq *EXTRACTION-LAYER-FILTERS* keys)

      ;; 3) состояние запоминается между открытиями окна
      (setq *EXTRACTION-LAST-LAYER-FILTERS* keys)

      ;; Ручной выбор до перестройки списка
      (setq keep (extraction-selected-names))

      ;; 4-7) новый список слоёв: фильтры, дубликаты, сортировка
      (extraction-rebuild-layer-list)

      ;; 8-9) сохраняем только тот ручной выбор, который остался видимым
      (setq keep (extraction-visible-only keep))
      (extraction-select-layers-in-list keep)
    )
  )
)


;; Оставить из списка только слои, видимые в текущем списке окна
(defun extraction-visible-only (layers / out item)
  (setq out '())
  (foreach item layers
    (if (vl-some '(lambda (x) (= (strcase x) (strcase item)))
                 *EXTRACTION-VISIBLE-LAYERS*)
      (setq out (cons item out))
    )
  )
  (reverse out)
)


;; ============================================================
;; ОСНОВНАЯ КОМАНДА
;; ============================================================

(defun c:extraction
       ( / *error* dcl-file save-base modules-dir r uDoc)

  (vl-load-com)

  ;; ----------------------------------------------------------
  ;; Локальный обработчик ошибок
  ;; ----------------------------------------------------------
  (defun *error* (msg)

    ;; --- Если список слоев остался открытым — закрыть ---
    (if *extraction-list-open*
      (progn
        (vl-catch-all-apply 'end_list '())
        (setq *extraction-list-open* nil)
      )
    )

    ;; --- Сброс всех флагов — ВСЕГДА ---
    (setq *extraction-updating-ui* nil)

    ;; --- Очистка предвыделения при любой ошибке (единая точка: диспетчер) ---
    (setq *extraction-preselected-set* nil)

    ;; --- Выгрузка DCL ---
    ;; Если мы ВНУТРИ start_dialog (в callback) — НЕ выгружаем,
    ;; иначе уничтожим диалог изнутри его цикла обработки событий.
    ;; Если мы ВНЕ диалога — выгружаем штатно.
    (if (not *extraction-in-dialog*)
      (if (and *EXTRACTION-DCL-ID*
               (numberp *EXTRACTION-DCL-ID*)
               (>= *EXTRACTION-DCL-ID* 0))
        (progn
          (unload_dialog *EXTRACTION-DCL-ID*)
          (setq *EXTRACTION-DCL-ID* nil)
        )
      )
    )

    ;; --- Сбрасываем флаг после проверки выгрузки ---
    (setq *extraction-in-dialog* nil)

    ;; --- Сообщение об ошибке ---
    (if (and msg
             (not (wcmatch (strcase msg)
                    "*BREAK*,*CANCEL*,*QUIT*,*EXIT*,*ПРЕРВА*,*ОТМЕН*")))
      (princ (strcat "\n[EX ERROR] " msg))
    )
    (princ)
  )

  ;; ----------------------------------------------------------
  ;; Загрузка модулей только при первом запуске за сессию
  ;; ----------------------------------------------------------
  (if (not *EXTRACTION-MODULES-LOADED*)
    (progn
      (extraction-load-all)
      (setq *EXTRACTION-MODULES-LOADED* T)
    )
  )

  ;; Постоянные маски читаются при каждом открытии EXTRACTION.
  (extraction-apply-settings-defaults)

  (setq *extraction-preselected-set* (ssget "_I"))

  (setq dcl-file nil)
  (setq modules-dir (extraction-modules-dir))

  (if modules-dir
    (setq dcl-file
      (findfile (strcat modules-dir "\\extraction.dcl"))))

  (if (null dcl-file)
    (setq dcl-file (findfile "extraction.dcl")))

  (if (null dcl-file)
    (progn
      (alert "Не найден файл extraction.dcl.")
      (princ)
    )
    (progn

      (setq *EXTRACTION-ALL-LAYERS* (extraction-layer-names))
      (setq *EXTRACTION-SELECTED-LAYERS* nil)

      ;; Встроенные фильтры слоёв: состояние из прошлого открытия окна
      (setq *EXTRACTION-LAYER-FILTERS* *EXTRACTION-LAST-LAYER-FILTERS*)
      (setq *EXTRACTION-LAYERS-SOURCE* nil)

      (setq *EXTRACTION-TASK-ID*      *EXTRACTION-LAST-TASK*)
      (setq *EXTRACTION-REPORT-MODE*  *EXTRACTION-LAST-REPORT-MODE*)
      (setq *EXTRACTION-EXPORT-EXCEL* *EXTRACTION-LAST-EXPORT-EXCEL*)
      (setq *EXTRACTION-EXPORT-TXT*   *EXTRACTION-LAST-EXPORT-TXT*)
      (setq *EXTRACTION-CREATE-TABLE* *EXTRACTION-LAST-CREATE-TABLE*)
      (setq *EXTRACTION-ACTION* 'CANCEL)

      (setq *EXTRACTION-DCL-ID* (load_dialog dcl-file))

      (if (< *EXTRACTION-DCL-ID* 0)
        (alert "Не удалось загрузить extraction.dcl.")
        (progn

          (if (new_dialog "extraction_dialog" *EXTRACTION-DCL-ID*)
            (progn

              ;; --- Начальная инициализация UI ---
              ;; Оборачиваем в updating-ui, чтобы set_tile
              ;; не вызывали callback'и
              (setq *extraction-updating-ui* T)

              (set_tile "rb_detail"
                (if (= *EXTRACTION-LAST-REPORT-MODE* "DETAIL") "1" "0"))
              (set_tile "rb_summary"
                (if (= *EXTRACTION-LAST-REPORT-MODE* "SUMMARY") "1" "0"))

              (set_tile "chk_xls"
                (if *EXTRACTION-LAST-EXPORT-EXCEL* "1" "0"))
              (set_tile "chk_txt"
                (if *EXTRACTION-LAST-EXPORT-TXT* "1" "0"))
              (set_tile "chk_acad"
                (if *EXTRACTION-LAST-CREATE-TABLE* "1" "0"))

              (extraction-layer-filter-set-tiles
                *EXTRACTION-LAST-LAYER-FILTERS*)

              (set_tile "rb_task_fasonka"
                (if (eq *EXTRACTION-LAST-TASK* 'FASONKA) "1" "0"))
              (set_tile "rb_task_subsystem"
                (if (eq *EXTRACTION-LAST-TASK* 'SUBSYSTEM) "1" "0"))
              (set_tile "rb_task_cladding"
                (if (eq *EXTRACTION-LAST-TASK* 'CLADDING) "1" "0"))
              (set_tile "rb_task_vitrazh"
                (if (eq *EXTRACTION-LAST-TASK* 'VITRAZH) "1" "0"))
              (set_tile "rb_task_zapolnenie"
                (if (eq *EXTRACTION-LAST-TASK* 'ZAPOLNENIE) "1" "0"))

              (set_tile "chk_subsystem_1"
                (if (car *EXTRACTION-LAST-SUBSYSTEM-CHECKS*) "1" "0"))
              (set_tile "chk_subsystem_2"
                (if (cadr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*) "1" "0"))
              (set_tile "chk_subsystem_3"
                (if (caddr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*) "1" "0"))

              (if (= (type blockrename-init) 'SUBR)
                (blockrename-init)
              )

              (if (eq *EXTRACTION-LAST-TASK* 'SUBSYSTEM)
                (progn
                  (mode_tile "box_subsystem_layers" 0)
                  (mode_tile "chk_subsystem_1" 0)
                  (mode_tile "chk_subsystem_2" 0)
                  (mode_tile "chk_subsystem_3" 0)
                )
                (progn
                  (mode_tile "box_subsystem_layers" 1)
                  (mode_tile "chk_subsystem_1" 1)
                  (mode_tile "chk_subsystem_2" 1)
                  (mode_tile "chk_subsystem_3" 1)
                )
              )

              (setq *extraction-updating-ui* nil)

              ;; Поле поиска слоя: новое открытие окна — запрос пустой
              (setq *EXTRACTION-LAYER-SEARCH-PATTERN* "")
              (set_tile "edt_layer_search" *EXTRACTION-LAYER-SEARCH-HINT*)

              ;; --- Заполнение списка слоев и восстановление выбора ---
              (extraction-rebuild-layer-list)

              (extraction-select-layers-in-list
                (cond
                  ((eq *EXTRACTION-LAST-TASK* 'SUBSYSTEM)
                   *EXTRACTION-LAST-SUBSYSTEM-LAYERS*)
                  ((eq *EXTRACTION-LAST-TASK* 'CLADDING)
                   *EXTRACTION-LAST-CLADDING-LAYERS*)
                  ((eq *EXTRACTION-LAST-TASK* 'VITRAZH)
                   *EXTRACTION-LAST-VITRAZH-LAYERS*)
                  ((eq *EXTRACTION-LAST-TASK* 'ZAPOLNENIE)
                   *EXTRACTION-LAST-ZAPOLNENIE-LAYERS*)
                  (T *EXTRACTION-LAST-FASONKA-LAYERS*)))

              (extraction-sync-checks-from-layers)

              (action_tile "btn_settings" "(extraction-settings)")
              (action_tile "btn_help"   "(extraction-help)")
              (action_tile "btn_save"   "(extraction-save)")
              (action_tile "btn_saveas" "(extraction-saveas)")
              (action_tile "btn_close"  "(extraction-close)")

              (action_tile "btn_cutline"  "(extraction-cutline)")
              (action_tile "btn_cutsheet" "(extraction-cutsheet)")

              (action_tile "edt_layer_search"    "(extraction-layer-search-changed)")
              (action_tile "btn_select_all"      "(extraction-select-all-layers)")
              (action_tile "btn_clear_all"       "(extraction-clear-all-layers)")
              (action_tile "chk_filter_my"       "(extraction-layer-filter-changed)")
              (action_tile "chk_filter_facades"  "(extraction-layer-filter-changed)")
              (action_tile "chk_filter_vitrazh"  "(extraction-layer-filter-changed)")
              (action_tile "chk_filter_windows"  "(extraction-layer-filter-changed)")
              (action_tile "lst_layers"          "(extraction-layer-selection)")

              (action_tile "rb_detail"
                "(setq *EXTRACTION-REPORT-MODE* \"DETAIL\")(setq *EXTRACTION-LAST-REPORT-MODE* \"DETAIL\")")
              (action_tile "rb_summary"
                "(setq *EXTRACTION-REPORT-MODE* \"SUMMARY\")(setq *EXTRACTION-LAST-REPORT-MODE* \"SUMMARY\")")

              (action_tile "rb_task_fasonka"
                "(setq *EXTRACTION-TASK-ID* 'FASONKA)(setq *EXTRACTION-LAST-TASK* 'FASONKA)(extraction-toggle-subsystem-layers)")
              (action_tile "rb_task_subsystem"
                "(setq *EXTRACTION-TASK-ID* 'SUBSYSTEM)(setq *EXTRACTION-LAST-TASK* 'SUBSYSTEM)(extraction-toggle-subsystem-layers)")
              (action_tile "rb_task_cladding"
                "(setq *EXTRACTION-TASK-ID* 'CLADDING)(setq *EXTRACTION-LAST-TASK* 'CLADDING)(extraction-toggle-subsystem-layers)")
              (action_tile "rb_task_vitrazh"
                "(setq *EXTRACTION-TASK-ID* 'VITRAZH)(setq *EXTRACTION-LAST-TASK* 'VITRAZH)(extraction-toggle-subsystem-layers)")
              (action_tile "rb_task_zapolnenie"
                "(setq *EXTRACTION-TASK-ID* 'ZAPOLNENIE)(setq *EXTRACTION-LAST-TASK* 'ZAPOLNENIE)(extraction-toggle-subsystem-layers)")

              (action_tile "chk_subsystem_1" "(extraction-subsystem-check-changed 1)")
              (action_tile "chk_subsystem_2" "(extraction-subsystem-check-changed 2)")
              (action_tile "chk_subsystem_3" "(extraction-subsystem-check-changed 3)")

              (action_tile "chk_filter_anonymous" "(blockrename-filter-anonymous)")
              (action_tile "edt_block_search"     "(blockrename-search-changed)")
              (action_tile "lst_blocks"           "(blockrename-selected)")
              (action_tile "btn_block_copy"       "(blockrename-copy-handler)")
              (action_tile "btn_block_insert"     "(blockrename-insert-handler)")
              (action_tile "btn_block_rename"     "(blockrename-rename)")

              ;; --- Запуск модального диалога ---
              (setq *extraction-in-dialog* T)
              (start_dialog)
              (setq *extraction-in-dialog* nil)

              ;; --- Обработка результата ---
              (cond

                ((eq *EXTRACTION-ACTION* 'SETTINGS)
                 ;; Главное окно закрывается перед вторым модальным DCL.
                 ;; После настроек оно открывается заново с перечитанными масками.
                 (if (= (type ae-settings-ui-open) 'SUBR)
                   (ae-settings-ui-open)
                 )
                 (extraction-apply-settings-defaults)
                 (c:extraction)
                )

                ((eq *EXTRACTION-ACTION* 'HELP)
                 (if (= (type ae-help-ui-open) 'SUBR)
                   (ae-help-ui-open)
                   (alert "Окно справки недоступно."))
                 (c:extraction)
                )

                ((eq *EXTRACTION-ACTION* 'SAVE)
                 (run-task
                   *EXTRACTION-TASK-ID*
                   *EXTRACTION-SELECTED-LAYERS*
                   *EXTRACTION-REPORT-MODE*
                   *EXTRACTION-EXPORT-EXCEL*
                   *EXTRACTION-EXPORT-TXT*
                   *EXTRACTION-CREATE-TABLE*
                   nil)
                )

                ((eq *EXTRACTION-ACTION* 'COPYBLOCK)
                 (if (and (boundp '*BLOCKRENAME-SELECTED*)
                          *BLOCKRENAME-SELECTED*)
                   (blockrename-copy-block *BLOCKRENAME-SELECTED*)
                   (princ "\nБлок для копирования не выбран.")
                 )
                )

                ((eq *EXTRACTION-ACTION* 'INSERTBLOCK)
                 (if (and (boundp '*BLOCKRENAME-SELECTED*)
                          *BLOCKRENAME-SELECTED*)
                   (blockrename-insert-block *BLOCKRENAME-SELECTED*)
                   (princ "\nБлок для вставки не выбран.")
                 )
                )

                ((eq *EXTRACTION-ACTION* 'SAVEAS)
                 (setq save-base
                   (vl-catch-all-apply 'tu-get-save-base
                     (list *EXTRACTION-TASK-ID*)))

                 (if (vl-catch-all-error-p save-base)
                   (princ "\nОшибка выбора файла.")
                   (if save-base
                     (run-task
                       *EXTRACTION-TASK-ID*
                       *EXTRACTION-SELECTED-LAYERS*
                       *EXTRACTION-REPORT-MODE*
                       *EXTRACTION-EXPORT-EXCEL*
                       *EXTRACTION-EXPORT-TXT*
                       *EXTRACTION-CREATE-TABLE*
                       save-base)
                   )
                 )
                )

                ((eq *EXTRACTION-ACTION* 'CUTLINE)
                 ;; V13: откат недоделанной раскладки при падении в середине
                 (setq uDoc (tu-undo-begin))
                 (setq r
                   (vl-catch-all-apply 'cutline-main
                     (list *EXTRACTION-SELECTED-LAYERS*)))

                 (extraction-handle-module-result r "CUTLINE")
                 (if (vl-catch-all-error-p r)
                   (progn
                     (tu-undo-cancel uDoc)
                     (princ "\n[EX][GUARD] Прервано посреди создания. Если недоделанное уже было в чертеже - оно откачено автоматически."))
                   (tu-undo-end uDoc))
                )

                ((eq *EXTRACTION-ACTION* 'CUTSHEET)
                 ;; V13: откат недоделанной карты при падении в середине
                 (setq uDoc (tu-undo-begin))
                 (setq r
                   (vl-catch-all-apply 'cutsheet-main
                     (list *EXTRACTION-SELECTED-LAYERS*)))

                 (extraction-handle-module-result r "CUTSHEET")
                 (if (vl-catch-all-error-p r)
                   (progn
                     (tu-undo-cancel uDoc)
                     (princ "\n[EX][GUARD] Прервано посреди создания. Если недоделанное уже было в чертеже - оно откачено автоматически."))
                   (tu-undo-end uDoc))
                )
              )

              ;; --- Гарантированная выгрузка на штатном пути ---
              (if (and *EXTRACTION-DCL-ID*
                       (numberp *EXTRACTION-DCL-ID*)
                       (>= *EXTRACTION-DCL-ID* 0))
                (progn
                  (unload_dialog *EXTRACTION-DCL-ID*)
                  (setq *EXTRACTION-DCL-ID* nil)
                )
              )
            )
            ;; --- Конец progn успешной ветки new_dialog ---

            (progn
              (if (and *EXTRACTION-DCL-ID*
                       (numberp *EXTRACTION-DCL-ID*)
                       (>= *EXTRACTION-DCL-ID* 0))
                (progn
                  (unload_dialog *EXTRACTION-DCL-ID*)
                  (setq *EXTRACTION-DCL-ID* nil)
                )
              )
              (alert "Не удалось открыть диалог EXTRACTION.")
            )
            ;; --- Конец progn ветки new_dialog failed ---
          )
          ;; --- Конец if new_dialog ---
        )
        ;; --- Конец progn load_dialog OK ---
      )
      ;; --- Конец if load_dialog ---
    )
    ;; --- Конец progn dcl-file found ---
  )
  ;; --- Конец if dcl-file ---

  ;; --- Очистка предвыделения между запусками (штатный выход диспетчера) ---
  (setq *extraction-preselected-set* nil)

  (princ)
)


;; ============================================================
;; РУССКАЯ КОМАНДА
;; ============================================================

(defun c:ЭКСТРАКЦИЯ ()
  (c:extraction)
)


(princ "\nEXTRACTION.LSP загружен (ред. 29: раскладка окна — отступы сведены к трём прототипам DCL; поиск слоя в панели Слои; фильтры Мои/Фасады/Витражи/Окна по групповым фильтрам AutoCAD).")

;; ============================================================
;; ЗАГЛУШКИ
;; ============================================================

(defun vitrazh-main (layers report-mode export-excel
                     export-txt create-table save-base / )
  (princ "\nВИТРАЖ: модуль в разработке.")
  (princ "\nВыбранные слои: ")
  (if layers
    (foreach l layers (princ (strcat l " ")))
    (princ "все")
  )
  (princ)
  T
)

(princ)