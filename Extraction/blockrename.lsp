;;; ============================================================
;;; BLOCKRENAME.LSP
;;; Переименование пользовательских блоков и PASTEBLOCK-блоков
;;;
;;; AutoCAD 2016+
;;; Visual LISP / ActiveX / DCL
;;;
;;; Особенности:
;;;   - фильтр «Анонимные блоки» (только A$C...);
;;;   - поиск по маске;
;;;   - переименование через ActiveX;
;;;   - «прилипающий» список: переименованный блок
;;;     остается видимым до закрытия диспетчера
;;;     или ручного изменения фильтра/поиска;
;;;   - прилипание работает при обоих фильтрах:
;;;     и «Анонимные блоки», и «Поиск».
;;;
;;; Без обработчика Enter в поле «Новое имя».
;;; Переименование — только по кнопке.
;;; ============================================================

(vl-load-com)


;; ============================================================
;; ГЛОБАЛЬНОЕ СОСТОЯНИЕ
;; ============================================================

(if (not (boundp '*BLOCKRENAME-ALL*))
  (setq *BLOCKRENAME-ALL* nil)
)

(if (not (boundp '*BLOCKRENAME-FILTERED*))
  (setq *BLOCKRENAME-FILTERED* nil)
)

(if (not (boundp '*BLOCKRENAME-FILTER-ANONYMOUS*))
  (setq *BLOCKRENAME-FILTER-ANONYMOUS* nil)
)

(if (not (boundp '*BLOCKRENAME-SCOPE*))
  (setq *BLOCKRENAME-SCOPE* nil)
)

(if (not (boundp '*BLOCKRENAME-SEARCH-PATTERN*))
  (setq *BLOCKRENAME-SEARCH-PATTERN* "")
)

(if (not (boundp '*BLOCKRENAME-SELECTED*))
  (setq *BLOCKRENAME-SELECTED* nil)
)

(if (not (boundp '*BLOCKRENAME-STICKY*))
  (setq *BLOCKRENAME-STICKY* nil)
)


;; ============================================================
;; ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
;; ============================================================

(defun blockrename-string-p (value)
  (= (type value) 'STR)
)


(defun blockrename-string-empty-p (value)
  (or
    (not (blockrename-string-p value))
    (= (strlen value) 0)
  )
)


;; ------------------------------------------------------------
;; Программная установка поля «Новое имя».
;; ------------------------------------------------------------

(defun blockrename-set-rename-field (value / v)
  (setq v (if (blockrename-string-p value) value ""))
  ;; пустое поле показывает подсказку
  (if (= v "") (setq v *BLOCKRENAME-NAME-HINT*))
  (set_tile "edt_block_rename" v)
  T
)


;; ============================================================
;; СТИКИ-СПИСОК
;; ============================================================

(defun blockrename-sticky-add (name)
  (if (and
        (blockrename-string-p name)
        (> (strlen name) 0))
    (progn
      (if (not
            (vl-some
              '(lambda (x) (= (strcase x) (strcase name)))
              *BLOCKRENAME-STICKY*))
        (setq *BLOCKRENAME-STICKY*
          (cons name *BLOCKRENAME-STICKY*)))
      T
    )
    nil
  )
)


(defun blockrename-sticky-clear ()
  (setq *BLOCKRENAME-STICKY* nil)
)


(defun blockrename-sticky-member-p (name)
  (and
    (blockrename-string-p name)
    (vl-some
      '(lambda (x) (= (strcase x) (strcase name)))
      *BLOCKRENAME-STICKY*)
  )
)


;; ============================================================
;; КЛАССИФИКАЦИЯ БЛОКОВ
;; ============================================================

(defun blockrename-system-p (name)
  (and
    (blockrename-string-p name)
    (or
      (wcmatch (strcase name) "*MODEL_SPACE*")
      (wcmatch (strcase name) "*PAPER_SPACE*"))
  )
)


(defun blockrename-anonymous-p (name)
  (and
    (blockrename-string-p name)
    (> (strlen name) 0)
    (not (blockrename-system-p name))
    (= (substr name 1 1) "*")
  )
)


(defun blockrename-pasteblock-p (name)
  (and
    (blockrename-string-p name)
    (> (strlen name) 3)
    (= (strcase (substr name 1 3)) "A$C")
  )
)


(defun blockrename-visible-p (name)
  (and
    (blockrename-string-p name)
    (> (strlen name) 0)
    (not (blockrename-system-p name))
    (not (blockrename-anonymous-p name))
  )
)


(defun blockrename-renamable-p (name)
  (blockrename-visible-p name)
)

;; ============================================================
;; ГЕНЕРАЦИЯ УНИКАЛЬНОГО ИМЕНИ БЛОКА
;; Формат: <baseName>_1, <baseName>_2, ...
;; ============================================================

(defun blockrename-generate-unique-name (baseName / n testName)
  (setq n 1)
  (setq testName (strcat baseName "_" (itoa n)))
  (while (tblsearch "BLOCK" testName)
    (setq n (1+ n))
    (setq testName (strcat baseName "_" (itoa n)))
  )
  testName
)

;; ============================================================
;; ПОЛУЧЕНИЕ ВСЕХ ОПРЕДЕЛЕНИЙ БЛОКОВ
;; ============================================================

(defun blockrename-all-names ( / acad doc blocks out item name result)

  (setq out '())

  (setq acad
    (vl-catch-all-apply 'vlax-get-acad-object '()))

  (if (and
        (not (vl-catch-all-error-p acad))
        acad)
    (progn
      (setq doc
        (vl-catch-all-apply
          'vla-get-ActiveDocument
          (list acad)))

      (if (and
            (not (vl-catch-all-error-p doc))
            doc)
        (progn
          (setq blocks
            (vl-catch-all-apply
              'vla-get-Blocks
              (list doc)))

          (if (and
                (not (vl-catch-all-error-p blocks))
                blocks)
            (progn
              (vlax-for item blocks
                (setq name
                  (vl-catch-all-apply
                    'vla-get-Name
                    (list item)))

                (if (and
                      (not (vl-catch-all-error-p name))
                      (blockrename-visible-p name))
                  (setq out (cons name out))
                )
              )
            )
          )
        )
      )
    )
  )

  (setq result
    (vl-sort
      out
      '(lambda (a b) (< (strcase a) (strcase b)))))

  result
)


;; ============================================================
;; ПОИСК
;; ============================================================

(defun blockrename-search-has-mask-p (text)
  (and
    (blockrename-string-p text)
    (or
      (vl-string-search "*" text)
      (vl-string-search "?" text)
      (vl-string-search "#" text)
      (vl-string-search "@" text))
  )
)


(defun blockrename-normalize-pattern (text)
  (if (not (blockrename-string-p text))
    ""
    (if (= text "")
      ""
      (if (blockrename-search-has-mask-p text)
        text
        (strcat "*" text "*"))
    )
  )
)


(defun blockrename-match-p (name pattern / normalized)
  (if (or
        (not (blockrename-string-p name))
        (not (blockrename-string-p pattern))
        (= pattern ""))
    T
    (progn
      (setq normalized
        (blockrename-normalize-pattern pattern))
      (if (= normalized "")
        T
        (wcmatch
          (strcase name)
          (strcase normalized))
      )
    )
  )
)


;; ============================================================
;; ФИЛЬТРАЦИЯ
;; ============================================================
;; Прилипающие имена показываются всегда.
;; Остальные — с учетом чекбокса и поиска.
;; ============================================================

;; ============================================================
;; Область списка: блоки, выбранные до запуска EXTRACTION
;; ============================================================
;; Если при запуске были выбраны вставки блоков, список показывает только
;; их имена (имя определения блока). Без выбора (или если выбор без вставок
;; блоков) список полный. Сами блоки чертежа не затрагиваются.

(defun blockrename-scope-from-set (ss / i en nm cands all names)
  (setq cands '() i 0)
  (if ss
    (repeat (sslength ss)
      (setq en (ssname ss i))
      (if (= (cdr (assoc 0 (entget en))) "INSERT")
        (foreach prop '(vla-get-Name vla-get-EffectiveName)
          (setq nm (vl-catch-all-apply prop (list (vlax-ename->vla-object en))))
          (if (and (not (vl-catch-all-error-p nm))
                   (blockrename-string-p nm)
                   (> (strlen nm) 0))
            (setq cands (cons (strcase nm) cands)))))
      (setq i (1+ i))))
  ;; в область попадают только имена, которые есть в списке блоков;
  ;; если таких нет — область пустая, список полный
  (setq all (mapcar 'strcase (blockrename-all-names)))
  (setq names
    (if cands
      (vl-remove-if-not '(lambda (n) (member n all)) cands)
      nil))
  (setq *BLOCKRENAME-SCOPE* names)
  (if names T nil)
)

(defun blockrename-in-scope-p (name)
  (if (null *BLOCKRENAME-SCOPE*)
    T
    (if (and (blockrename-string-p name)
             (member (strcase name) *BLOCKRENAME-SCOPE*))
      T
      nil))
)


(defun blockrename-filter-list ( / result name search)

  (setq result '())

  (setq search
    (if (blockrename-string-p *BLOCKRENAME-SEARCH-PATTERN*)
      *BLOCKRENAME-SEARCH-PATTERN*
      ""
    )
  )

  (foreach name *BLOCKRENAME-ALL*

    (if (blockrename-sticky-member-p name)

      ;; «Прилипающие» — показываются всегда.
      (setq result (cons name result))

      ;; Обычная логика фильтрации.
      (if
        (and
          (blockrename-in-scope-p name)
          (or
            (not *BLOCKRENAME-FILTER-ANONYMOUS*)
            (blockrename-pasteblock-p name))
          (blockrename-match-p name search))
        (setq result (cons name result))
      )
    )
  )

  (vl-sort
    (reverse result)
    '(lambda (a b) (< (strcase a) (strcase b))))
)


;; ============================================================
;; ЗАПОЛНЕНИЕ LIST_BOX
;; ============================================================

(defun blockrename-populate-list ()
  (start_list "lst_blocks")
  (mapcar 'add_list *BLOCKRENAME-FILTERED*)
  (end_list)
  (set_tile "lst_blocks" "")
)


;; ============================================================
;; ВЫБОР СТРОКИ ПО ИМЕНИ
;; ============================================================

(defun blockrename-select-in-list (name / i found)
  (setq i 0)
  (setq found nil)

  (foreach item *BLOCKRENAME-FILTERED*
    (if (and
          (not found)
          (= (strcase item) (strcase name)))
      (setq found i))
    (setq i (1+ i))
  )

  (if (numberp found)
    (set_tile "lst_blocks" (itoa found))
    (set_tile "lst_blocks" "")
  )

  found
)


;; ============================================================
;; ПЕРЕСТРОЕНИЕ СПИСКА
;; ============================================================

(defun blockrename-rebuild ( / old-selected)
  (setq old-selected *BLOCKRENAME-SELECTED*)

  (setq *BLOCKRENAME-FILTERED*
    (blockrename-filter-list))

  (blockrename-populate-list)

  (if
    (and
      (blockrename-string-p old-selected)
      (vl-some
        '(lambda (x) (= (strcase x) (strcase old-selected)))
        *BLOCKRENAME-FILTERED*))
    (progn
      (blockrename-select-in-list old-selected)
      (blockrename-set-rename-field old-selected)
    )
    (progn
      (setq *BLOCKRENAME-SELECTED* nil)
      (set_tile "lst_blocks" "")
      (blockrename-set-rename-field "")
    )
  )

  T
)


;; ============================================================
;; ИНИЦИАЛИЗАЦИЯ
;; ============================================================

(defun blockrename-init ()
  (setq *BLOCKRENAME-FILTER-ANONYMOUS* nil)
  (setq *BLOCKRENAME-SEARCH-PATTERN* "")
  (setq *BLOCKRENAME-SELECTED* nil)

  (blockrename-sticky-clear)

  (setq *BLOCKRENAME-ALL*
    (blockrename-all-names))

  (set_tile "chk_filter_anonymous" "0")
  (set_tile "edt_block_search" *BLOCKRENAME-SEARCH-HINT*)
  (blockrename-set-rename-field "")

  (blockrename-rebuild)

  T
)


;; ============================================================
;; ОБРАБОТЧИК: ЧЕКБОКС «АНОНИМНЫЕ БЛОКИ»
;; ============================================================

(defun blockrename-filter-anonymous ()
  (setq *BLOCKRENAME-FILTER-ANONYMOUS*
    (= (get_tile "chk_filter_anonymous") "1"))

  (blockrename-sticky-clear)

  (setq *BLOCKRENAME-SELECTED* nil)
  (blockrename-set-rename-field "")

  (blockrename-rebuild)

  T
)


;; ============================================================
;; ПОЛЕ ПОИСКА: ПОДСКАЗКА-ПЛЕЙСХОЛДЕР
;; Пока пользователь не ввел значение, поле показывает подсказку.
;; Подсказка не считается поисковым запросом; при вводе текста после
;; подсказки она срезается и поле показывает только запрос.
;; ============================================================

(if (not (boundp '*BLOCKRENAME-SEARCH-HINT*))
  (setq *BLOCKRENAME-SEARCH-HINT* "Поиск (по Enter):")
)

;; Подсказка поля нового имени: обработчика Enter у поля нет,
;; переименование - только по кнопке
(if (not (boundp '*BLOCKRENAME-NAME-HINT*))
  (setq *BLOCKRENAME-NAME-HINT* "Введите новое имя:")
)

;; Значение поля поиска без подсказки. Подсказка вырезается из любой
;; позиции: набранный ДО нее текст сохраняется
(defun blockrename-search-value (/ v p)
  (setq v (get_tile "edt_block_search"))
  (if (= (type v) 'STR)
    (progn
      (cond
        ((= v *BLOCKRENAME-SEARCH-HINT*)
         (setq v ""))
        ((setq p (vl-string-search *BLOCKRENAME-SEARCH-HINT* v))
         (setq v (strcat
                   (substr v 1 p)
                   (substr v (+ p (strlen *BLOCKRENAME-SEARCH-HINT*) 1)))))
      )
      v)
    "")
)

;; Значение поля нового имени без подсказки (вырезается из любой позиции)
(defun blockrename-name-value (/ v p)
  (setq v (get_tile "edt_block_rename"))
  (if (= (type v) 'STR)
    (progn
      (cond
        ((= v *BLOCKRENAME-NAME-HINT*)
         (setq v ""))
        ((setq p (vl-string-search *BLOCKRENAME-NAME-HINT* v))
         (setq v (strcat
                   (substr v 1 p)
                   (substr v (+ p (strlen *BLOCKRENAME-NAME-HINT*) 1)))))
      )
      v)
    "")
)

;; ============================================================
;; ОБРАБОТЧИК: ПОИСК
;; ============================================================

(defun blockrename-search-changed (/ v tile)
  (setq v (blockrename-search-value)
        tile (get_tile "edt_block_search"))
  ;; нормализуем текст поля: подсказка срезана; пусто = снова подсказка
  (if (/= v tile)
    (set_tile "edt_block_search"
      (if (= v "") *BLOCKRENAME-SEARCH-HINT* v)))

  (setq *BLOCKRENAME-SEARCH-PATTERN* v)

  (blockrename-sticky-clear)

  (blockrename-rebuild)

  T
)


;; ============================================================
;; РАЗБОР ИНДЕКСА LIST_BOX
;; ============================================================

(defun blockrename-parse-index (s / x)
  (if (and
        (blockrename-string-p s)
        (/= s ""))
    (progn
      (setq x (read s))
      (if (numberp x) x nil))
    nil
  )
)


;; ============================================================
;; ОБРАБОТЧИК: ВЫБОР БЛОКА
;; ============================================================

(defun blockrename-selected ( / s index name)
  (setq s (get_tile "lst_blocks"))

  (if (or
        (not (blockrename-string-p s))
        (= s ""))
    (progn
      (setq *BLOCKRENAME-SELECTED* nil)
      (blockrename-set-rename-field "")
    )
    (progn
      (setq index (blockrename-parse-index s))

      (if (and
            (numberp index)
            (>= index 0)
            (< index (length *BLOCKRENAME-FILTERED*)))
        (progn
          (setq name (nth index *BLOCKRENAME-FILTERED*))
          (setq *BLOCKRENAME-SELECTED* name)
          (blockrename-set-rename-field name)
        )
      )
    )
  )

  T
)


;; ============================================================
;; ПОЛУЧЕНИЕ ОБЪЕКТА ОПРЕДЕЛЕНИЯ БЛОКА
;; ============================================================

(defun blockrename-get-block-object (name / acad doc blocks obj result)
  (setq result nil)

  (if (blockrename-string-p name)
    (progn
      (setq acad
        (vl-catch-all-apply 'vlax-get-acad-object '()))

      (if (and
            (not (vl-catch-all-error-p acad))
            acad)
        (progn
          (setq doc
            (vl-catch-all-apply
              'vla-get-ActiveDocument
              (list acad)))

          (if (and
                (not (vl-catch-all-error-p doc))
                doc)
            (progn
              (setq blocks
                (vl-catch-all-apply
                  'vla-get-Blocks
                  (list doc)))

              (if (and
                    (not (vl-catch-all-error-p blocks))
                    blocks)
                (progn
                  (setq obj
                    (vl-catch-all-apply
                      'vla-Item
                      (list blocks name)))

                  (if (and
                        (not (vl-catch-all-error-p obj))
                        obj)
                    (setq result obj)
                  )
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


;; ============================================================
;; ПРОВЕРКА РЕЗУЛЬТАТА ПЕРЕИМЕНОВАНИЯ
;; ============================================================

(defun blockrename-rename-verified-p (old-name new-name)
  (and
    (not (tblsearch "BLOCK" old-name))
    (tblsearch "BLOCK" new-name))
)


;; ============================================================
;; ПЕРЕИМЕНОВАНИЕ ЧЕРЕЗ ACTIVEX
;; ============================================================

(defun blockrename-rename-activex (old-name new-name / obj result)
  (setq result nil)
  (setq obj (blockrename-get-block-object old-name))

  (if obj
    (progn
      (setq result
        (vl-catch-all-apply
          'vla-put-Name
          (list obj new-name)))

      (if (not (vl-catch-all-error-p result))
        T
        nil)
    )
    nil
  )
)

;; ============================================================
;; КОПИРОВАНИЕ БЛОКА С СОХРАНЕНИЕМ ДИНАМИЧЕСКИХ СВОЙСТВ
;;
;; Копируется ОПРЕДЕЛЕНИЕ блока, а не вставка. Способ: определение
;; выгружается в промежуточный документ ObjectDBX, там получает новое
;; имя и возвращается в чертёж. Так дублируются параметры и операции
;; динамического блока, а исходное определение не затрагивается.
;;
;; Почему не WBLOCK (ред. 3):
;;   - WBLOCK динамического блока пишет в файл ВСТАВКУ, а не
;;     определение: INSERT такого файла создаёт обёртку, внутри которой
;;     лежит исходный блок, - «муляж» без своих ручек;
;;   - если имя совпадает с уже имеющимся в чертеже, AutoCAD подставляет
;;     СУЩЕСТВУЮЩЕЕ определение: копия становится ссылкой на исходник;
;;   - vla-get-EffectiveName у вставки динамического блока возвращает имя
;;     ДИНАМИЧЕСКОГО блока (а не анонимного *U...), поэтому переименование
;;     по EffectiveName уводило имя у ИСХОДНОГО определения - блок
;;     «исчезал» из чертежа. Теперь имя копии задаётся только в
;;     промежуточном документе, до возврата в чертёж.
;; ============================================================

;; Промежуточный документ ObjectDBX. nil - интерфейс недоступен
;; (LT, урезанная установка): тогда копию определения не сделать.
(defun blockrename-objectdbx ( / app vrs progid dbx)
  (setq app (vlax-get-acad-object))
  (setq vrs (atoi (getvar "ACADVER")))
  (setq progid
    (if (< vrs 16)
      "objectdbx.axdbdocument"
      (strcat "objectdbx.axdbdocument." (itoa vrs))))
  (setq dbx
    (vl-catch-all-apply 'vla-getinterfaceobject (list app progid)))
  (if (or (null dbx)
          (vl-catch-all-error-p dbx)
          (/= (type dbx) 'VLA-OBJECT))
    nil
    dbx)
)

;; Копия определения oldName под именем newName.
;; Возвращает (OK newName) | (ERROR "причина").
(defun blockrename-copy-definition
       (oldName newName / acad doc blocks src dbx dbxBlocks dbxDef
        cp rn res)

  (cond
    ((not (tblsearch "BLOCK" oldName))
     (list 'ERROR (strcat "определение \"" oldName "\" не найдено")))

    ((tblsearch "BLOCK" newName)
     (list 'ERROR (strcat "имя \"" newName "\" уже занято")))

    ((null (setq dbx (blockrename-objectdbx)))
     (list 'ERROR
       "ObjectDBX недоступен: копия определения невозможна"))

    (T
     (setq acad (vlax-get-acad-object))
     (setq doc (vla-get-ActiveDocument acad))
     (setq blocks (vla-get-Blocks doc))
     (setq dbxBlocks (vla-get-Blocks dbx))
     (setq res (list 'ERROR "копирование определения не удалось"))

     (setq src (vl-catch-all-apply 'vla-Item (list blocks oldName)))

     (cond

       ((or (null src) (vl-catch-all-error-p src))
        (setq res (list 'ERROR
                    (strcat "не открыто определение \"" oldName "\""))))

       (T
        ;; 1. Определение -> промежуточный документ.
        (setq cp
          (vl-catch-all-apply 'vlax-invoke
            (list doc 'CopyObjects (list src) dbxBlocks)))

        (cond

          ((vl-catch-all-error-p cp)
           (setq res (list 'ERROR
                       (vl-catch-all-error-message cp))))

          (T
           (setq dbxDef
             (vl-catch-all-apply 'vla-Item (list dbxBlocks oldName)))

           (cond

             ((or (null dbxDef) (vl-catch-all-error-p dbxDef))
              (setq res (list 'ERROR
                          "копия определения не появилась в промежуточном документе")))

             (T
              ;; 2. Переименование ТОЛЬКО в промежуточном документе:
              ;;    определение чертежа ещё не тронуто.
              (setq rn
                (vl-catch-all-apply 'vla-put-Name (list dbxDef newName)))

              (cond

                ((vl-catch-all-error-p rn)
                 (setq res
                   (list 'ERROR
                     (strcat "переименование копии: "
                             (vl-catch-all-error-message rn)))))

                (T
                 ;; 3. Обратно в чертёж - уже под новым именем.
                 (setq cp
                   (vl-catch-all-apply 'vlax-invoke
                     (list dbx 'CopyObjects (list dbxDef) blocks)))

                 (cond

                   ((vl-catch-all-error-p cp)
                    (setq res (list 'ERROR
                                (vl-catch-all-error-message cp))))

                   ((not (tblsearch "BLOCK" newName))
                    (setq res (list 'ERROR
                                "новое определение не появилось в чертеже")))

                   (T
                    (setq res (list 'OK newName))))
                )
              )
             )
           )
          )
        )
       )
     )

     (if (= (type dbx) 'VLA-OBJECT)
       (vl-catch-all-apply 'vlax-release-object (list dbx)))

     res)
  )
)

;; Вставка ссылки на определение в точку.
;; Возвращает (OK вставка динамическая) | (ERROR "причина").
(defun blockrename-insert-reference
       (defName insPt / acad doc ms cell obj dyn)

  (setq acad (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acad))
  (setq ms (vla-get-ModelSpace doc))

  (setq cell
    (ex-safe-call 'vla-InsertBlock
      (list ms (vlax-3d-point insPt) defName 1.0 1.0 1.0 0.0)))

  (cond
    ((not (ex-safe-ok-p cell))
     (list 'ERROR (strcat "вставка не удалась: " (ex-safe-message cell))))

    (T
     (setq obj (ex-safe-value cell))
     (setq dyn (vl-catch-all-apply 'vla-get-IsDynamicBlock (list obj)))
     (if (vl-catch-all-error-p dyn) (setq dyn nil))
     (list 'OK obj (if dyn T nil)))
  )
)

(defun blockrename-copy-block
       (old-name / new-name insPt uDoc res cell dyn)

  (cond

    ((blockrename-string-empty-p old-name)
     (alert "Блок не выбран в списке.")
     nil)

    ((not (blockrename-renamable-p old-name))
     (alert
       (strcat "Блок \"" old-name
               "\" является системным или анонимным\n"
               "и не может быть скопирован."))
     nil)

    (T
     (setq new-name (blockrename-generate-unique-name old-name))
     (setq insPt (getpoint (strcat "\nУкажите точку вставки копии \""
                                   new-name "\": ")))

     (cond

       ((null insPt)
        (princ "\nКопирование отменено.")
        nil)

       (T
        (setq uDoc (tu-undo-begin))

        ;; 1. Копия определения под новым именем.
        (setq res (blockrename-copy-definition old-name new-name))

        (cond

          ((not (ex-safe-ok-p res))
           (princ (strcat "\n[BLOCKCOPY] " (ex-safe-message res)))
           (alert (strcat "Копия блока \"" old-name "\" не создана.\n"
                          (ex-safe-message res)))
           (tu-undo-end uDoc)
           nil)

          (T
           ;; 2. Ссылка на копию в указанной точке.
           (setq cell (blockrename-insert-reference new-name insPt))

           (cond

             ((not (ex-safe-ok-p cell))
              (princ (strcat "\n[BLOCKCOPY] " (ex-safe-message cell)))
              (alert (strcat "Копия блока \"" old-name "\" не вставлена.\n"
                             (ex-safe-message cell)))
              (tu-undo-end uDoc)
              nil)

             (T
              (setq dyn (caddr cell))

              ;; 3. Исходное определение обязано остаться на месте.
              (if (tblsearch "BLOCK" old-name)
                (progn
                  (setq *BLOCKRENAME-ALL* (blockrename-all-names))
                  (blockrename-sticky-add new-name)
                  (setq *BLOCKRENAME-SELECTED* new-name)
                  (princ (strcat "\nСоздана копия: \"" old-name "\" -> \""
                                 new-name "\" (" (if dyn "динамический" "статический")
                                 ")"))
                  (if (not dyn)
                    (princ (strcat "\n[BLOCKCOPY] внимание: определение "
                                   "скопировано без параметров динамики")))
                  (setq res T))
                (progn
                  (princ (strcat "\n[BLOCKCOPY] ИСХОДНОЕ определение \""
                                 old-name "\" исчезло из чертежа"))
                  (alert (strcat "Исходный блок \"" old-name
                                 "\" пропал из чертежа.\nОтмените операцию: U"))
                  (setq res nil)))

              (tu-undo-end uDoc)
              res)
           )
          )
        )
       )
     )
    )
  )
)

;; ============================================================
;; АВТОНОМНАЯ КОМАНДА КОПИРОВАНИЯ БЛОКА
;; ============================================================

(defun c:blockcopy ( / *error* svSaved uDoc name)
  ;; V8: guard сиспеременных - обрыв копирования вернёт CMDECHO/FILEDIA/EXPERT
  (defun *error* (msg)
    (tu-sysvar-restore svSaved)
    ;; V10: обрыв посреди копирования - закрыть группу (одно U откатит всё)
    (tu-undo-end uDoc)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\n[BLOCKRENAME][ERROR] " msg)))
    (princ))
  (setq svSaved (tu-sysvar-save '("CMDECHO" "FILEDIA" "EXPERT")))
  (setq uDoc (tu-undo-begin))
  (princ "\n--- Копирование блока ---")
  (setq name (getstring T "\nИмя блока для копирования: "))
  (cond
    ((blockrename-string-empty-p name)
     (princ "\nИмя блока не задано."))
    ((not (tblsearch "BLOCK" name))
     (princ (strcat "\nБлок \"" name "\" не найден.")))
    (T
     (blockrename-copy-block name)))
  (tu-undo-end uDoc)
  (princ)
)

(defun c:КОПИЯБЛОКА () (c:blockcopy))

;; ============================================================
;; ВСТАВКА ВЫБРАННОГО БЛОКА (кнопка "Вставить" панели БЛОКИ)
;; Вставляет ссылку выделенного блока в указанную точку
;; (масштаб 1, поворот 0). Определение блока не меняется.
;; ============================================================

(defun blockrename-insert-block (name / *error* svSaved uDoc acad doc ms pt obj)
  ;; guard сиспеременных; обрыв - закрытие группы UNDO
  (defun *error* (msg)
    (tu-sysvar-restore svSaved)
    (tu-undo-end uDoc)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\n[BLOCKRENAME][ERROR] " msg)))
    (princ))
  (cond
    ((blockrename-string-empty-p name)
     (alert "Блок не выбран в списке.")
     nil)

    ((not (tblsearch "BLOCK" name))
     (alert (strcat "Блок \"" name "\" не найден."))
     nil)

    (T
     (setq pt (getpoint (strcat "\nУкажите точку вставки блока \"" name "\": ")))
     (if (null pt)
       (progn (princ "\nВставка отменена.") nil)
       (progn
         (setq svSaved (tu-sysvar-save '("CMDECHO")))
         (setq uDoc (tu-undo-begin))
         (setq acad (vlax-get-acad-object))
         (setq doc (vla-get-ActiveDocument acad))
         (setq ms (vla-get-ModelSpace doc))
         (setq obj (ex-safe-call 'vla-InsertBlock
                     (list ms (vlax-3d-point pt) name 1.0 1.0 1.0 0.0)))
         (if (ex-safe-ok-p obj)
           (progn
             (setq obj (ex-safe-value obj))
             (princ (strcat "\nВставлен блок: \"" name "\"")))
           (progn
             (princ "\nНе удалось вставить блок.")
             (setq obj nil)))
         (tu-undo-end uDoc)
         (tu-sysvar-restore svSaved)
         (if obj T nil))))
  )
)

(defun blockrename-insert-handler ()
  (setq *EXTRACTION-ACTION* 'INSERTBLOCK)
  (done_dialog 1)
)

;; ============================================================
;; ПЕРЕИМЕНОВАНИЕ В ДИАЛОГЕ
;; ============================================================
;; Только ActiveX. Никаких вызовов command / command-s.
;; ============================================================

(defun blockrename-rename
       ( / old-name new-name success active-result)

  (setq old-name *BLOCKRENAME-SELECTED*)
  (setq new-name (blockrename-name-value))

  (cond

    ((blockrename-string-empty-p old-name)
     (alert "Блок не выбран в списке.")
     nil)

    ((blockrename-string-empty-p new-name)
     (alert "Не задано новое имя блока.")
     nil)

    ((= (strcase old-name) (strcase new-name))
     T)

    ((tblsearch "BLOCK" new-name)
     (alert
       (strcat "Блок с именем \"" new-name "\" уже существует."))
     nil)

    ((not (blockrename-renamable-p old-name))
     (alert
       (strcat "Блок \"" old-name
               "\" является системным или анонимным\n"
               "и не может быть переименован."))
     nil)

    (T
     (setq success nil)

     (setq active-result
       (blockrename-rename-activex old-name new-name))

     (if (and active-result
              (blockrename-rename-verified-p old-name new-name))
       (setq success T))

     (if success
       (progn
         (setq *BLOCKRENAME-ALL*
           (blockrename-all-names))

         (blockrename-sticky-add new-name)

         (setq *BLOCKRENAME-SELECTED* new-name)

         (setq *BLOCKRENAME-FILTERED*
           (blockrename-filter-list))

         (blockrename-populate-list)

         (if (vl-some
               '(lambda (x) (= (strcase x) (strcase new-name)))
               *BLOCKRENAME-FILTERED*)
           (progn
             (blockrename-select-in-list new-name)
             (blockrename-set-rename-field "")
           )
           (progn
             (setq *BLOCKRENAME-SELECTED* nil)
             (set_tile "lst_blocks" "")
             (blockrename-set-rename-field "")
           )
         )

         T
       )

       (progn
         (alert
           (strcat "Не удалось переименовать блок \""
                   old-name "\" в \"" new-name "\"."))
         nil)
     )
    )
  )
)

;; ============================================================
;; ОБРАБОТЧИК КНОПКИ "КОПИЯ"
;; Устанавливает глобальный флаг и закрывает диалог
;; ============================================================

(if (not (boundp '*BLOCKRENAME-COPY-REQUESTED*))
  (setq *BLOCKRENAME-COPY-REQUESTED* nil)
)

(defun blockrename-copy-handler ()
  (setq *EXTRACTION-ACTION* 'COPYBLOCK)
  (done_dialog 1)
)

;; ============================================================
;; АВТОНОМНАЯ КОМАНДА
;; ============================================================

(defun c:blockrename ( / *error* svSaved uDoc old-name new-name success)
  ;; V8: guard сиспеременных - обрыв переименования вернёт CMDECHO/FILEDIA/EXPERT
  (defun *error* (msg)
    (tu-sysvar-restore svSaved)
    ;; V10: обрыв посреди переименования - закрыть группу (одно U откатит всё)
    (tu-undo-end uDoc)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\n[BLOCKRENAME][ERROR] " msg)))
    (princ))
  (setq svSaved (tu-sysvar-save '("CMDECHO" "FILEDIA" "EXPERT")))
  (setq uDoc (tu-undo-begin))
  (princ "\n--- Переименование определения блока ---")

  (setq old-name
    (getstring T "\nСтарое имя блока: "))

  (cond

    ((blockrename-string-empty-p old-name)
     (princ "\nИмя блока не задано."))

    ((not (tblsearch "BLOCK" old-name))
     (princ (strcat "\nБлок \"" old-name "\" не найден.")))

    ((not (blockrename-renamable-p old-name))
     (princ (strcat "\nБлок \"" old-name
                    "\" является системным или анонимным\n"
                    "и не может быть переименован.")))

    (T
     (setq new-name
       (getstring T "\nНовое имя блока: "))

     (cond

       ((blockrename-string-empty-p new-name)
        (princ "\nНовое имя не задано."))

       ((tblsearch "BLOCK" new-name)
        (princ (strcat "\nБлок \"" new-name
                       "\" уже существует.")))

       (T
        (setq success nil)

        (if (blockrename-rename-activex old-name new-name)
          (if (blockrename-rename-verified-p old-name new-name)
            (setq success T)))

        (if (not success)
          (progn
            (setvar "CMDECHO" 0)
            (vl-catch-all-apply
              '(lambda ()
                 (command "_.-RENAME" "_Block" old-name new-name))
              '())
            (setvar "CMDECHO" 1)

            (if (blockrename-rename-verified-p old-name new-name)
              (setq success T))
          )
        )

        (if success
          (princ (strcat "\nБлок успешно переименован: "
                         old-name " -> " new-name))
          (princ "\nНе удалось переименовать блок."))
       )
     )
    )
  )

  (tu-undo-end uDoc)
  (princ)
)


;; ============================================================
;; РУССКАЯ КОМАНДА
;; ============================================================

(defun c:ПЕРЕИМЕНОВАТЬ ()
  (c:blockrename)
)


;; ============================================================
;; ЗАВЕРШЕНИЕ
;; ============================================================

;; BLOCKRENAME.LSP загружен (ред. 4: копия определения через ObjectDBX, без WBLOCK). Команды: BLOCKRENAME, ПЕРЕИМЕНОВАТЬ
(if (member (ae-reload-sym-type 'ae-banner-add) '(SUBR USUBR))
  (ae-banner-add "BLOCKRENAME" '(
      ("BLOCKRENAME / ПЕРЕИМЕНОВАТЬ" "копия и переименование блока" "4"))))
(princ)