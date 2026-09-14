;;; ============================================================
;;; CLADDING-TEST.LSP — тестовые помощники для Облицовки
;;; РЕДАКЦИЯ 8: добавлен сканер Б0 (SCANBLOCKS) с локальной
;;; проверкой слоёв (без зависимости от su-layer-match-any)
;;
;;; Команды:
;;;   MKTEST        — создать тестовый набор (8 полилиний, 2 слоя)
;;;   RMTEST        — удалить набор и слои
;;;   CLCOUNTERS    — показать счётчики предупреждений
;;;   TEST-FALLBACK — сверка AutoCAD Area и Green на фигуре
;;;   SCANBLOCKS    — сканер Б0: динамические блоки облицовки
;;; ============================================================

(vl-load-com)

;; ============================================================
;; СЛУЖЕБНЫЕ: слой и полилиния
;; ============================================================

;; Создание слоя через entmake (без команды .-LAYER)
(defun cl-test-ensure-layer (name)
  (if (not (tblsearch "LAYER" name))
    (if (null
          (entmake (list (cons 0 "LAYER")
                         (cons 100 "AcDbSymbolTableRecord")
                         (cons 100 "AcDbLayerTableRecord")
                         (cons 2 name)
                         (cons 70 0)
                         (cons 62 7))))
      (princ (strcat "\n[TEST] ВНИМАНИЕ: не создан слой " name))
    )
  )
)

;; Удаление слоя через ActiveX с защитой
(defun cl-test-delete-layer (name / doc layers lay r)
  (setq doc (vl-catch-all-apply 'vla-get-ActiveDocument
                                (list (vlax-get-acad-object))))
  (if (not (vl-catch-all-error-p doc))
    (progn
      (setq layers (vla-get-Layers doc))
      (setq lay (vl-catch-all-apply 'vla-item (list layers name)))
      (if (not (vl-catch-all-error-p lay))
        (progn
          (setq r (vl-catch-all-apply 'vla-delete (list lay)))
          (if (vl-catch-all-error-p r)
            (princ (strcat "\n[TEST] слой не удалён (занят?): " name))
          )
        )
      )
    )
  )
)

;; LWPOLYLINE: точки строго 2D; булж добавляется только ненулевой
(defun cl-test-lw (layer closed pts blgs / i e bv res)
  (setq e (list (cons 0 "LWPOLYLINE")
                (cons 100 "AcDbEntity")
                (cons 8 layer)
                (cons 100 "AcDbPolyline")
                (cons 90 (length pts))
                (cons 70 (if closed 1 0))))
  (setq i 0)
  (foreach p pts
    (setq e (append e (list (cons 10 (list (car p) (cadr p))))))
    (setq bv (nth i blgs))
    (if (and bv (/= bv 0.0))
      (setq e (append e (list (cons 42 bv))))
    )
    (setq i (1+ i))
  )
  (setq res (entmake e))
  (if (null res)
    (princ (strcat "\n[TEST] ВНИМАНИЕ: entmake не создал полилинию на слое " layer))
  )
  res
)

;; ============================================================
;; ГЕНЕРАТОРЫ ФИГУР
;; ============================================================

(defun cl-test-rect (layer x y w h)
  (cl-test-lw layer T
    (list (list x y) (list (+ x w) y) (list (+ x w) (+ y h)) (list x (+ y h)))
    (list 0 0 0 0))
)

(defun cl-test-rounded (layer x y w h r / b)
  (setq b 0.414214)  ; tan(90/4 град) — скругление 90 град
  (cl-test-lw layer T
    (list (list (+ x r) y) (list (- (+ x w) r) y)
          (list (+ x w) (+ y r)) (list (+ x w) (- (+ y h) r))
          (list (- (+ x w) r) (+ y h)) (list (+ x r) (+ y h))
          (list x (- (+ y h) r)) (list x (+ y r)))
    (list 0 b 0 b 0 b 0 b))
)

(defun cl-test-arc (layer x y w h b)
  (cl-test-lw layer T
    (list (list x y) (list (+ x w) y) (list (+ x w) (+ y h)) (list x (+ y h)))
    (list 0 0 b 0))
)

(defun cl-test-rot (layer x y w h ang / a ca sa pts rot)
  (setq a (* ang (/ pi 180.0)) ca (cos a) sa (sin a))
  (setq rot '(lambda (p)
               (list (+ x (- (* (car p) ca) (* (cadr p) sa)))
                     (+ y (+ (* (car p) sa) (* (cadr p) ca))))))
  (setq pts (mapcar rot (list (list 0 0) (list w 0) (list w h) (list 0 h))))
  (cl-test-lw layer T pts (list 0 0 0 0))
)

(defun cl-test-open (layer x y)
  (cl-test-lw layer nil
    (list (list x y) (list (+ x 500) y) (list (+ x 500) (+ y 300)))
    (list 0 0 0))
)

;; Тест A: незамкнутая + bulge
(defun cl-test-open-bulge (layer x y)
  (cl-test-lw layer nil
    (list (list x y) (list (+ x 500) y) (list (+ x 500) (+ y 300)))
    (list 0.3 0 0))
)

;; ============================================================
;; MKTEST: создать тестовый набор
;; ============================================================
(defun c:mktest ()
  (vl-load-com)
  (cl-test-ensure-layer "ТЕСТ_КЕРАМОГРАНИТ")
  (cl-test-ensure-layer "ТЕСТ_КАССЕТЫ")

  (cl-test-rect       "ТЕСТ_КЕРАМОГРАНИТ"    0.0    0.0 600.0 600.0)
  (cl-test-rounded    "ТЕСТ_КЕРАМОГРАНИТ" 1000.0   0.0 600.0 600.0 10.0)
  (cl-test-arc        "ТЕСТ_КЕРАМОГРАНИТ" 2000.0   0.0 1000.0 500.0 -0.3)
  (cl-test-rot        "ТЕСТ_КЕРАМОГРАНИТ" 4000.0   0.0 800.0 400.0 30.0)
  (cl-test-open       "ТЕСТ_КЕРАМОГРАНИТ" 5000.0   0.0)
  (cl-test-open-bulge "ТЕСТ_КЕРАМОГРАНИТ" 6000.0   0.0)
  (cl-test-rect       "ТЕСТ_КАССЕТЫ"         0.0 2000.0 1200.0 600.0)
  (cl-test-rect       "ТЕСТ_КАССЕТЫ"      2000.0 2000.0 1200.0 600.0)

  (princ "\nТестовый набор создан: 8 полилиний на 2 слоях.")
  (princ)
)

;; ============================================================
;; RMTEST: удалить тестовый набор
;; ============================================================
(defun c:rmtest ( / ss lay)
  (foreach lay '("ТЕСТ_КЕРАМОГРАНИТ" "ТЕСТ_КАССЕТЫ")
    (setq ss (ssget "_X" (list (cons 8 lay))))
    (if ss (command "._ERASE" ss ""))
  )
  (foreach lay '("ТЕСТ_КЕРАМОГРАНИТ" "ТЕСТ_КАССЕТЫ")
    (cl-test-delete-layer lay)
  )
  (princ "\nТестовый набор удалён.")
  (princ)
)

;; ============================================================
;; CLCOUNTERS: показать счётчики предупреждений
;; ============================================================
(defun c:clcounters ()
  (princ (strcat "\nopen="     (itoa *cladding-skipped-open*)
                 "  zero="    (itoa *cladding-skipped-zero*)
                 "  arcs="    (itoa *cladding-with-arcs*)
                 "  mismatch=" (itoa *cladding-check-mismatch*)))
  (princ)
)

;; ============================================================
;; TEST-FALLBACK: сверка AutoCAD Area и Green (тест B)
;; ============================================================
(defun c:test-fallback ( / ent obj a-cad a-green diff)
  (princ "\n=== Тест B: сверка AutoCAD Area и Green ===")
  (setq ent (car (entsel "\nВыберите полилинию: ")))
  (if ent
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (setq a-cad (vlax-get obj 'Area))
      (setq a-green (cl-green-area ent))
      (setq diff (abs (- a-cad a-green)))
      (princ (strcat "\nAutoCAD Area: " (rtos a-cad 2 2) " мм2"))
      (princ (strcat "\nGreen Area:   " (rtos a-green 2 2) " мм2"))
      (princ (strcat "\nРасхождение:  " (rtos diff 2 2) " мм2"))
      (if (<= diff 1.0)
        (princ "\nСовпадение в пределах 1 мм2")
        (princ "\nВНИМАНИЕ: расхождение > 1 мм2 — требуется анализ")
      )
    )
    (princ "\nОтмена.")
  )
  (princ)
)

;; ============================================================
;; СКАНЕР Б0: диагностика динамических блоков облицовки
;; Выводит имя, видимость, слои, количество и полный список
;; свойств каждого уникального блока. Результат — в консоль
;; (кратко) и в файл "<чертёж> Сканер блоков.txt" (подробно).
;; ============================================================

;; Безопасное преобразование значения свойства в строку
(defun cl-scan-prop-value (pval / r vt)
  (if (= (type pval) 'VARIANT)
    (progn
      (setq r (vl-catch-all-apply 'vlax-variant-value (list pval)))
      (if (vl-catch-all-error-p r)
        "<вариант>"
        (cl-scan-prop-value r)
      )
    )
    (progn
      (setq vt (type pval))
      (cond
        ((= vt 'INT)  (itoa pval))
        ((= vt 'REAL) (rtos pval 2 4))
        ((= vt 'STR)  pval)
        ((null pval)  "<nil>")
        (T
         (setq r (vl-catch-all-apply 'vl-princ-to-string (list pval)))
         (if (vl-catch-all-error-p r)
           (strcat "<" (vl-princ-to-string vt) ">")
           r)
        )
      )
    )
  )
)

;; Все динамические свойства блока: список (имя . значение)
(defun cl-scan-all-props (obj / dynprops prop pname pval out)
  (setq out '())
  (setq dynprops
    (vl-catch-all-apply 'vlax-invoke
      (list obj 'GetDynamicBlockProperties)))
  (if (not (vl-catch-all-error-p dynprops))
    (foreach prop dynprops
      (setq pname
        (vl-catch-all-apply 'vla-get-PropertyName (list prop)))
      (if (and (not (vl-catch-all-error-p pname))
               pname (= (type pname) 'STR))
        (progn
          (setq pval
            (vl-catch-all-apply 'vla-get-Value (list prop)))
          (if (vl-catch-all-error-p pval)
            (setq out (cons (cons pname "<ошибка>") out))
            (setq out (cons (cons pname (cl-scan-prop-value pval)) out))
          )
        )
      )
    )
  )
  (reverse out)
)

;; Склеить список строк через разделитель
(defun cl-scan-join (lst sep / out)
  (setq out "")
  (foreach x lst
    (setq out (if (= out "") x (strcat out sep x)))
  )
  out
)

;; Локальный разбор строки слоёв (не зависит от порядка загрузки)
(defun cl-scan-split (str delim / pos result item)
  (setq result '())
  (while (setq pos (vl-string-search delim str))
    (setq item (vl-string-trim " " (substr str 1 pos)))
    (setq result (cons item result))
    (setq str (substr str (+ pos 2)))
  )
  (setq item (vl-string-trim " " str))
  (if (> (strlen item) 0)
    (setq result (cons item result))
  )
  (reverse result)
)

;; Добавить/обновить группу в списке групп
(defun cl-scan-add-group (groups key eff vis layer props / grp new-entry)
  (setq grp (assoc key groups))
  (if grp
    (progn
      (setq new-entry
        (list key eff vis
              (1+ (nth 3 grp))
              (if (member layer (nth 4 grp))
                (nth 4 grp)
                (cons layer (nth 4 grp)))
              (nth 5 grp)))
      (subst new-entry grp groups)
    )
    (cons (list key eff vis 1 (list layer) props) groups)
  )
)

;; Локальная проверка слоя по списку масок (без зависимости от
;; реализации в select-utils; маски поддерживают *)
(defun cl-scan-layer-match (layer layers / found)
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
;; КОМАНДА SCANBLOCKS
;; ============================================================
(defun c:scanblocks ( / layers-str layers filter-str layer-mask ss i ent
                        data layer obj eff vis key props groups grp
                        total-cnt fname f cnt e-layers e-props)
  (vl-load-com)

  (setq layers-str
    (getstring T "\nСлои через запятую (Enter - все слои): "))
  (if (= layers-str "")
    (setq layers nil)
    (setq layers
      (mapcar '(lambda (x) (strcase (vl-string-trim " " x)))
              (cl-scan-split layers-str ",")))
  )

  (setq filter-str
    (getstring T "\nФильтр по имени (подстрока, Enter - все блоки): "))
  (setq filter-str (strcase (vl-string-trim " " filter-str)))

  ;; Выбор INSERT
  (if (null layers)
    (setq ss (ssget "_X" '((0 . "INSERT"))))
    (progn
      (setq layer-mask
        (apply 'strcat (mapcar '(lambda (x) (strcat x ",")) layers)))
      (setq layer-mask (substr layer-mask 1 (1- (strlen layer-mask))))
      (setq ss (ssget "_X" (list '(0 . "INSERT") (cons 8 layer-mask))))
    )
  )

  (if (null ss)
    (progn (princ "\nБлоки не найдены.") (princ) nil)
    (progn
      (setq total-cnt (sslength ss))
      (setq groups '())
      (setq i 0)
      (repeat total-cnt
        (setq ent (ssname ss i))
        (setq data (entget ent))
        (setq layer (cdr (assoc 8 data)))

        ;; Фильтр по слоям (с масками; если слои не заданы - все)
        (if (cl-scan-layer-match layer layers)
          (progn
            (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
            (if (not (vl-catch-all-error-p obj))
              (progn
                (setq eff (su-get-effective-name obj))
                (if (or (null eff) (vl-catch-all-error-p eff))
                  (setq eff "<нет имени>"))
                (setq vis (vl-catch-all-apply 'su-get-visibility (list obj)))
                (if (or (vl-catch-all-error-p vis) (null vis))
                  (setq vis ""))

                ;; Фильтр по подстроке имени/видимости
                (if (or (= filter-str "")
                        (vl-string-search filter-str (strcase eff))
                        (vl-string-search filter-str (strcase vis)))
                  (progn
                    (setq props (cl-scan-all-props obj))
                    (setq key (strcat eff " || " vis))
                    (setq groups (cl-scan-add-group groups key eff vis layer props))
                  )
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )

      ;; Сортировка групп по имени
      (setq groups
        (vl-sort groups
          '(lambda (a b) (< (strcase (car a)) (strcase (car b))))))

      ;; Файл подробного отчёта
      (setq fname
        (strcat (getvar "DWGPREFIX")
                (vl-filename-base (getvar "DWGNAME"))
                " Сканер блоков.txt"))
      (setq f (open fname "w"))

      (princ (strcat "\nВсего блоков в выборке: " (itoa total-cnt)))
      (princ (strcat "\nУникальных блоков (имя+видимость): " (itoa (length groups))))

      (foreach grp groups
        (setq eff (nth 1 grp)
              vis (nth 2 grp)
              cnt (nth 3 grp)
              e-layers (vl-sort (nth 4 grp)
                         '(lambda (a b) (< (strcase a) (strcase b))))
              e-props (vl-sort (nth 5 grp)
                        '(lambda (a b) (< (strcase (car a)) (strcase (car b))))))

        ;; Кратко в консоль
        (princ (strcat "\n  " eff
                       (if (= vis "") "" (strcat " / " vis))
                       "  x" (itoa cnt)
                       "  [" (cl-scan-join e-layers ", ") "]"))

        ;; Подробно в файл
        (if f
          (progn
            (write-line "" f)
            (write-line (strcat "=== Блок: " eff " ===") f)
            (write-line (strcat "Видимость: " (if (= vis "") "<нет>" vis)) f)
            (write-line (strcat "Вхождений: " (itoa cnt)) f)
            (write-line (strcat "Слои (" (itoa (length e-layers)) "): "
                                (cl-scan-join e-layers ", ")) f)
            (if e-props
              (progn
                (write-line "Свойства:" f)
                (foreach p e-props
                  (write-line (strcat "  " (car p) " = " (cdr p)) f)
                )
              )
              (write-line "Свойства: <нет динамических свойств>" f)
            )
          )
        )
      )

      (if f (close f))
      (princ (strcat "\nПодробный результат: " fname))
      (princ)
    )
  )
)

(princ "\nCLADDING-TEST.LSP загружен (ред. 8).")
(princ "\nКоманды: MKTEST, RMTEST, CLCOUNTERS, TEST-FALLBACK, SCANBLOCKS")
(princ)