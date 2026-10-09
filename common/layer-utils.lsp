;;; ============================================================
;;; common/layer-utils.lsp
;;; Слои чертежа и ГРУППОВЫЕ ФИЛЬТРЫ СЛОЕВ AutoCAD
;;;
;;; РЕД. 1 (2026-10-07): дерево групповых фильтров с уровнями,
;;;   прямые и рекурсивные (вложенные любых уровней) слои,
;;;   поиск по маскам имен, встроенные фильтры диспетчера
;;;   (Мои / Фасады / Витражи / Окна), защита от циклов,
;;;   корректное поведение без словаря групповых фильтров.
;;;
;;; Структура словаря (чертеж -> таблица слоев -> расширенный словарь):
;;;   ключ ACLYDICTIONARY — словарь групповых фильтров;
;;;   запись фильтра (XRECORD): 1 = "AcLyLayerGroup", 300 = имя,
;;;                             330 = прямые слои (объекты LAYER),
;;;                             расширенный словарь -> ACLYDICTIONARY ->
;;;                             вложенные фильтры следующего уровня.
;;; Обход выполняется по парам 350 (записи словаря); ссылки на объекты
;;; принимаются как entity name или как handle (handent).
;;; ============================================================
(vl-load-com)

;; ============================================================
;; ВСТРОЕННЫЕ ФИЛЬТРЫ ДИСПЕТЧЕРА — именованные переменные с дефолтами
;; (править можно здесь, без правок кода)
;; ============================================================

;; «Мои»: рекурсивно весь групповой фильтр + явные имена + тематические маски.
;; Явное включение «Отключенные» — на случай переноса фильтра за пределы
;; «Стройплэкс»: он должен попадать в «Мои» в любом случае.
(setq *tu-layer-filter-mine-group* "Стройплэкс")
(setq *tu-layer-filter-mine-extra* '("Отключенные"))
(setq *tu-layer-filter-mine-masks* '("Фасад*" "Витраж*" "Фонар*" "Окн*"))

;; Тематические фильтры (маски имен групповых фильтров)
(setq *tu-layer-filter-facades-masks* '("Фасад*"))
(setq *tu-layer-filter-vitrazh-masks* '("Витраж*" "Фонар*"))
(setq *tu-layer-filter-windows-masks* '("Окн*"))

;; Тематические маски ищутся на уровнях 1 и 2
;; (уровень 0 — корневой фильтр «Все», структурный корень)
(setq *tu-layer-filter-mask-levels* '(1 2))

;; Имя корневого фильтра (AutoCAD локализует: «Все» / «All»)
(setq *tu-layer-filter-root-names* '("Все" "All"))


;; ============================================================
;; БАЗОВЫЕ УТИЛИТЫ
;; ============================================================

;; Нормализация для сравнения имен: обрезка пробелов + верхний регистр
(defun tu-layer-filter-norm (s)
  (if (= (type s) 'STR)
    (strcase (vl-string-trim " \t" s))
    nil
  )
)

;; Правила сопоставления имен (без учета регистра, с обрезкой пробелов):
;;   маска с «*» — шаблон wcmatch; без «*» — точное имя.
(defun tu-layer-filter-name-match-p (name mask / n m)
  (setq n (tu-layer-filter-norm name))
  (setq m (tu-layer-filter-norm mask))
  (if (and n m)
    (if (vl-string-search "*" m)
      (wcmatch n m)
      (= n m)
    )
    nil
  )
)

;; Ссылка из данных словаря/записи -> entity name (ename или handle)
(defun tu-layer-filter-object-ename (ref / e)
  (cond
    ((= (type ref) 'ENAME) ref)
    ((= (type ref) 'STR) (if (setq e (handent ref)) e nil))
    (T nil)
  )
)

;; Данные объекта по ссылке (nil, если объект недоступен)
(defun tu-layer-filter-object-data (ref / e d)
  (setq e (tu-layer-filter-object-ename ref))
  (if e
    (progn
      (setq d (vl-catch-all-apply 'entget (list e)))
      (if (vl-catch-all-error-p d) nil d)
    )
    nil
  )
)

;; Прямые слои записи фильтра: ссылки на объекты LAYER (коды 330/340/350/360;
;; посторонние ссылки отсекаются проверкой типа объекта)
(defun tu-layer-filter-data-layers (data / out pair code objdef lname)
  (setq out '())
  (foreach pair data
    (setq code (car pair))
    (if (or (= code 330) (= code 340) (= code 350) (= code 360))
      (progn
        (setq objdef (tu-layer-filter-object-data (cdr pair)))
        (if (and objdef (= (strcase (cdr (assoc 0 objdef))) "LAYER"))
          (progn
            (setq lname (cdr (assoc 2 objdef)))
            (if (and (= (type lname) 'STR) (/= lname ""))
              (setq out (cons lname out))
            )
          )
        )
      )
    )
  )
  (reverse out)
)

;; Расширенный словарь объекта: 102 "{ACAD_XDICTIONARY" + 360
;; (fallback — первый 360 в данных)
(defun tu-layer-filter-extdict (data / tail ref)
  (setq ref nil)
  (if (setq tail (member '(102 . "{ACAD_XDICTIONARY") data))
    (setq ref (cdr (assoc 360 tail)))
  )
  (if (null ref)
    (setq ref (cdr (assoc 360 data)))
  )
  (tu-layer-filter-object-ename ref)
)

;; Данные записи словаря по имени (dictsearch) либо nil
(defun tu-layer-filter-dict-entry (dict ref-name / e d)
  (setq d nil)
  (if (setq e (tu-layer-filter-object-ename dict))
    (progn
      (setq d (vl-catch-all-apply 'dictsearch (list e ref-name)))
      (if (vl-catch-all-error-p d) (setq d nil))
    )
  )
  d
)

;; Словарь вложенных фильтров записи: расширенный словарь -> ACLYDICTIONARY.
;; Если ACLYDICTIONARY нет — используются записи самого расширенного словаря.
(defun tu-layer-filter-container (data / ext d)
  (setq d nil)
  (if (setq ext (tu-layer-filter-extdict data))
    (progn
      (setq d (tu-layer-filter-dict-entry ext "ACLYDICTIONARY"))
      (if (null d)
        (setq d (tu-layer-filter-object-data ext))
      )
    )
  )
  d
)

;; Корневой фильтр («Все» / «All»)?
(defun tu-layer-filter-root-p (name / n)
  (setq n (tu-layer-filter-norm name))
  (if n
    (vl-some '(lambda (x) (= (tu-layer-filter-norm x) n)) *tu-layer-filter-root-names*)
    nil
  )
)


;; ============================================================
;; ДЕРЕВО ГРУППОВЫХ ФИЛЬТРОВ
;; ============================================================

;; Обход словаря: список узлов (имя уровень родитель дочерние прямые-слои).
;; visited — список объектов для защиты от циклических ссылок.
(defun tu-layer-filter-walk (data level parent visited
                             / out pair ref objdef name lvl child lname)
  (setq out '())
  (if (listp data)
    (foreach pair data
      (if (= (car pair) 350)
        (progn
          (setq ref (cdr pair))
          (if (not (member ref visited))
            (progn
              (setq objdef (tu-layer-filter-object-data ref))

              (cond
                ;; --- Запись группового фильтра: есть имя (код 300) ---
                ((and objdef
                      (setq lname (cdr (assoc 300 objdef)))
                      (= (type lname) 'STR))
                 (setq lname (vl-string-trim " \t" lname))
                 (setq lvl (if (tu-layer-filter-root-p lname) 0 level))

                 (setq child
                   (tu-layer-filter-walk
                     (tu-layer-filter-container objdef)
                     (if (= lvl 0) 1 (1+ lvl))
                     lname
                     (cons ref visited)))

                 (setq out
                   (append out
                     (list (list lname lvl parent
                                 (mapcar 'car child)
                                 (tu-layer-filter-data-layers objdef))))
                   )
                 (setq out (append out child))
                )

                ;; --- Вложенный словарь: прозрачный спуск ---
                ((and objdef (= (strcase (cdr (assoc 0 objdef))) "DICTIONARY"))
                 (setq child
                   (tu-layer-filter-walk objdef level parent (cons ref visited)))
                 (setq out (append out child))
                )
              )
            )
          )
        )
      )
    )
  )
  out
)

;; Данные словаря групповых фильтров чертежа (ACLYDICTIONARY таблицы слоев)
(defun tu-layer-filter-dictionary-data ( / layer0 table-ext ext d)
  (setq d nil)
  (if (setq layer0 (tblobjname "LAYER" "0"))
    (progn
      (setq table-ext
        (tu-layer-filter-object-ename (cdr (assoc 330 (entget layer0)))))
      (if table-ext
        (progn
          (setq ext (tu-layer-filter-extdict (entget table-ext)))
          (if ext
            (progn
              (setq d (tu-layer-filter-dict-entry ext "ACLYDICTIONARY"))
              (if (null d)
                (setq d (tu-layer-filter-object-data ext))
              )
            )
          )
        )
      )
    )
  )
  d
)

;; Есть ли в чертеже хотя бы один групповой фильтр.
;; Пустой словарь (или его отсутствие) = фильтров нет: диспетчер пишет
;; «Групповой фильтр отсутствует», а не «совпадений по маскам не найдено».
(defun tu-layer-filter-available-p ()
  (if (tu-layer-filter-tree) T nil)
)

;; Дерево групповых фильтров: список узлов (имя уровень родитель дочерние прямые)
(defun tu-layer-filter-tree ()
  (tu-layer-filter-walk (tu-layer-filter-dictionary-data) 1 nil '())
)

;; Узел по точному имени (без учета регистра)
(defun tu-layer-filter-node-by-name (name tree / n found)
  (if (null tree) (setq tree (tu-layer-filter-tree)))
  (setq n (tu-layer-filter-norm name))
  (setq found nil)
  (foreach node tree
    (if (and (null found) (= (tu-layer-filter-norm (nth 0 node)) n))
      (setq found node)
    )
  )
  found
)

;; Прямые слои фильтра с точным именем
(defun tu-layer-filter-direct-layers (name tree / node)
  (setq node (tu-layer-filter-node-by-name name tree))
  (if node (nth 4 node) nil)
)

;; Узлы, чьи имена совпали с масками на заданных уровнях
(defun tu-layer-filter-nodes-by-masks (masks levels tree / out node lv hit)
  (if (null tree) (setq tree (tu-layer-filter-tree)))
  (setq out '())
  (foreach node tree
    (setq lv (nth 1 node))
    (if (member lv levels)
      (progn
        (setq hit nil)
        (foreach mask masks
          (if (and (null hit) (tu-layer-filter-name-match-p (nth 0 node) mask))
            (setq hit T)
          )
        )
        (if hit (setq out (cons node out)))
      )
    )
  )
  (reverse out)
)

;; Слои фильтров, найденных по маскам (объединение прямых слоев)
(defun tu-layer-filter-layers-by-masks (masks levels tree / out)
  (setq out '())
  (foreach node (tu-layer-filter-nodes-by-masks masks levels tree)
    (setq out (append out (nth 4 node)))
  )
  (tu-sort-strings-ci (tu-list-unique-ci out))
)

;; Слои фильтров с точными именами (любой уровень)
(defun tu-layer-filter-layers-by-names (names tree / out)
  (setq out '())
  (foreach name names
    (setq out (append out (tu-layer-filter-direct-layers name tree)))
  )
  (tu-sort-strings-ci (tu-list-unique-ci out))
)

;; Все слои фильтра: прямые + слои вложенных групп любых уровней.
;; visited — имена по пути (защита от циклов).
(defun tu-layer-filter-collect-all (name tree visited / node out child key)
  (setq out '())
  (setq node (tu-layer-filter-node-by-name name tree))
  (if node
    (progn
      (setq out (append (nth 4 node) out))
      (foreach child (nth 3 node)
        (setq key (tu-layer-filter-norm child))
        (if (not (member key visited))
          (setq out
            (append
              (tu-layer-filter-collect-all child tree (cons key visited))
              out))
        )
      )
    )
  )
  out
)

;; Рекурсивные слои фильтра по имени (все уровни вложенности)
(defun tu-layer-filter-all-layers (name tree / out)
  (if (null tree) (setq tree (tu-layer-filter-tree)))
  (setq out
    (tu-layer-filter-collect-all name tree (list (tu-layer-filter-norm name))))
  (tu-sort-strings-ci (tu-list-unique-ci out))
)


;; ============================================================
;; ВСТРОЕННЫЕ ФИЛЬТРЫ ДИСПЕТЧЕРА
;; Ключи: MY / FACADES / VITRAZH / WINDOWS
;; ============================================================

;; Слои одного встроенного фильтра
(defun tu-layer-filter-layers-for-key (key tree / out)
  (if (null tree) (setq tree (tu-layer-filter-tree)))
  (setq out '())
  (cond
    ;; «Мои» = весь «Стройплэкс» (рекурсивно) + «Отключенные» явно + тематические маски
    ((eq key 'MY)
     (setq out
       (append
         (tu-layer-filter-all-layers *tu-layer-filter-mine-group* tree)
         (tu-layer-filter-layers-by-names *tu-layer-filter-mine-extra* tree)
         (tu-layer-filter-layers-by-masks *tu-layer-filter-mine-masks*
                                          *tu-layer-filter-mask-levels* tree))))

    ((eq key 'FACADES)
     (setq out (tu-layer-filter-layers-by-masks *tu-layer-filter-facades-masks*
                                                *tu-layer-filter-mask-levels* tree)))

    ((eq key 'VITRAZH)
     (setq out (tu-layer-filter-layers-by-masks *tu-layer-filter-vitrazh-masks*
                                                *tu-layer-filter-mask-levels* tree)))

    ((eq key 'WINDOWS)
     (setq out (tu-layer-filter-layers-by-masks *tu-layer-filter-windows-masks*
                                                *tu-layer-filter-mask-levels* tree)))
  )
  (tu-sort-strings-ci (tu-list-unique-ci out))
)

;; Объединение слоев по списку ключей (несколько чекбоксов — «или», без дублей)
(defun tu-layer-filter-layers-for-keys (keys / out key tree)
  (setq out '())
  (setq tree (tu-layer-filter-tree))
  (foreach key keys
    (setq out (append out (tu-layer-filter-layers-for-key key tree)))
  )
  (tu-sort-strings-ci (tu-list-unique-ci out))
)

(princ "\nLAYER-UTILS.LSP загружен (ред. 2: групповые фильтры — дерево, уровни, маски, встроенные фильтры Мои/Фасады/Витражи/Окна).")
(princ)
