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
;;;     остаётся видимым до закрытия диспетчера
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

(defun blockrename-set-rename-field (value)
  (set_tile "edt_block_rename"
            (if (blockrename-string-p value) value ""))
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
;; Остальные — с учётом чекбокса и поиска.
;; ============================================================

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
  (set_tile "edt_block_search" "")
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
;; ОБРАБОТЧИК: ПОИСК
;; ============================================================

(defun blockrename-search-changed ()
  (setq *BLOCKRENAME-SEARCH-PATTERN*
    (get_tile "edt_block_search"))

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
;; ПЕРЕИМЕНОВАНИЕ В ДИАЛОГЕ
;; ============================================================
;; Только ActiveX. Никаких вызовов command / command-s.
;; ============================================================

(defun blockrename-rename
       ( / old-name new-name success active-result)

  (setq old-name *BLOCKRENAME-SELECTED*)
  (setq new-name (get_tile "edt_block_rename"))

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
;; АВТОНОМНАЯ КОМАНДА
;; ============================================================

(defun c:blockrename ( / old-name new-name success)
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

(princ "\nBLOCKRENAME.LSP загружен. Команды: BLOCKRENAME, ПЕРЕИМЕНОВАТЬ")
(princ)