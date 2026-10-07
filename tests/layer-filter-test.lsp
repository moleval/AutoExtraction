;;; ============================================================
;;; tests/layer-filter-test.lsp — приёмочный тест фильтров слоёв
;;; Ред. 1 (2026-10-07)
;;;
;;; Запуск в AutoCAD (файл лежит в папке tests):
;;;   (load "layer-filter-test.lsp")
;;;   LAYERFILTERTEST
;;;
;;; Печатает дерево групповых фильтров и отчет PASS/FAIL по пунктам
;;; приёмки встроенных фильтров: Мои / Фасады / Витражи / Окна.
;;; Требуется загруженный common/layer-utils.lsp (RELOAD или EXTRACTION).
;;; ============================================================
(vl-load-com)

;; ------------------------------------------------------------
;; Мини-фреймворк отчета
;; ------------------------------------------------------------

(setq *lft-pass* 0)
(setq *lft-fail* 0)
(setq *lft-skip* 0)

(defun lft-check (name ok)
  (if ok
    (progn
      (setq *lft-pass* (1+ *lft-pass*))
      (princ (strcat "\n[LAYER-FILTER-TEST][PASS] " name)))
    (progn
      (setq *lft-fail* (1+ *lft-fail*))
      (princ (strcat "\n[LAYER-FILTER-TEST][FAIL] " name)))
  )
  (princ)
)

(defun lft-skip (name)
  (setq *lft-skip* (1+ *lft-skip*))
  (princ (strcat "\n[LAYER-FILTER-TEST][SKIP] " name))
  (princ)
)

;; ------------------------------------------------------------
;; Вспомогательные предикаты (строки сравниваются без регистра)
;; ------------------------------------------------------------

(defun lft-count (lst)
  (if (listp lst) (length lst) 0)
)

(defun lft-in-p (x lst)
  (vl-some '(lambda (y) (= (strcase y) (strcase x))) lst)
)

(defun lft-subset-p (sub set)
  (if (listp sub)
    (vl-every '(lambda (x) (lft-in-p x set)) sub)
    T
  )
)

(defun lft-same-set-p (a b)
  (and (lft-subset-p a b) (lft-subset-p b a))
)

(defun lft-inter (a b / out)
  (setq out '())
  (foreach x a
    (if (lft-in-p x b) (setq out (cons x out)))
  )
  (reverse out)
)

(defun lft-diff (a b / out)
  (setq out '())
  (foreach x a
    (if (not (lft-in-p x b)) (setq out (cons x out)))
  )
  (reverse out)
)

(defun lft-unique-p (lst / seen ok)
  (setq seen '() ok T)
  (foreach x lst
    (if (lft-in-p x seen)
      (setq ok nil)
      (setq seen (cons x seen))
    )
  )
  ok
)

(defun lft-indent (n / s i)
  (setq s "" i 0)
  (while (< i n)
    (setq s (strcat s "   "))
    (setq i (1+ i))
  )
  s
)

;; Имена групповых фильтров, совпавших с масками
(defun lft-names-by-masks (masks tree)
  (mapcar 'car (tu-layer-filter-nodes-by-masks
                 masks *tu-layer-filter-mask-levels* tree))
)

(defun lft-overlap-p (a b)
  (if (lft-inter a b) T nil)
)

;; ------------------------------------------------------------
;; Основная проверка
;; ------------------------------------------------------------

(defun c:LAYERFILTERTEST ( / tree layers-my layers-fac layers-vit layers-win
                             my-group my-extra extra-layers themed
                             names-fac names-vit names-win names-other
                             only-off node)

  (setq *lft-pass* 0)
  (setq *lft-fail* 0)
  (setq *lft-skip* 0)

  (princ "\n=== ТЕСТ ФИЛЬТРОВ СЛОЁВ AutoExtraction (ред. 1) ===")

  ;; --- 1. Словарь и дерево ---
  (setq tree (tu-layer-filter-tree))

  (lft-check "Словарь групповых фильтров найден"
             (tu-layer-filter-available-p))
  (lft-check "Дерево фильтров не пустое"
             (and (listp tree) (> (length tree) 0)))

  (princ "\n\n--- Дерево групповых фильтров ---")
  (foreach node tree
    (princ (strcat "\n" (lft-indent (nth 1 node))
                   (nth 0 node)
                   "  [уровень " (itoa (nth 1 node)) "]"
                   "  родитель: " (if (nth 2 node) (nth 2 node) "-")
                   "  прямых слоёв: " (itoa (lft-count (nth 4 node)))))
  )
  (princ)

  ;; --- 2. Встроенные фильтры ---
  (setq layers-my  (tu-layer-filter-layers-for-key 'MY tree))
  (setq layers-fac (tu-layer-filter-layers-for-key 'FACADES tree))
  (setq layers-vit (tu-layer-filter-layers-for-key 'VITRAZH tree))
  (setq layers-win (tu-layer-filter-layers-for-key 'WINDOWS tree))

  (princ "\n\n--- Слои встроенных фильтров ---")
  (princ (strcat "\nМои:     " (itoa (lft-count layers-my)) " сл."))
  (princ (strcat "\nФасады:  " (itoa (lft-count layers-fac)) " сл."))
  (princ (strcat "\nВитражи: " (itoa (lft-count layers-vit)) " сл."))
  (princ (strcat "\nОкна:    " (itoa (lft-count layers-win)) " сл."))
  (princ (strcat "\nМаски: Мои " (lft-list-str *tu-layer-filter-mine-masks*)
                 " | Фасады " (lft-list-str *tu-layer-filter-facades-masks*)
                 " | Витражи " (lft-list-str *tu-layer-filter-vitrazh-masks*)
                 " | Окна " (lft-list-str *tu-layer-filter-windows-masks*)))
  (princ (strcat "\nУровни поиска тематических масок: "
                 (lft-list-str *tu-layer-filter-mask-levels*)))

  ;; --- 3. Состав «Мои» ---
  (setq my-group *tu-layer-filter-mine-group*)
  (setq my-extra *tu-layer-filter-mine-extra*)
  (setq extra-layers (tu-layer-filter-layers-by-names my-extra tree))
  (setq themed (tu-layer-filter-layers-by-masks
                 *tu-layer-filter-mine-masks* *tu-layer-filter-mask-levels* tree))

  (lft-check (strcat "Групповой фильтр \"" my-group "\" найден")
             (if (tu-layer-filter-node-by-name my-group tree) T nil))

  (lft-check (strcat "Слои \"" my-group "\" и всех вложенных групп входят в «Мои»")
             (lft-subset-p (tu-layer-filter-all-layers my-group tree) layers-my))

  (if (tu-layer-filter-node-by-name (car my-extra) tree)
    (lft-check (strcat "Слои \"" (car my-extra) "\" входят в «Мои» (явное включение)")
               (lft-subset-p extra-layers layers-my))
    (lft-skip (strcat "Групповой фильтр \"" (car my-extra) "\" в чертеже не найден"))
  )

  (lft-check "Тематические фильтры (Фасад*/Витраж*/Фонар*/Окн*) входят в «Мои»"
             (lft-subset-p themed layers-my))

  (lft-check "В «Мои» нет слоёв сверх «Стройплэкс» + «Отключенные» + тематика"
             (lft-subset-p layers-my
                           (append (tu-layer-filter-all-layers my-group tree)
                                   extra-layers themed)))

  ;; --- 4. Дубликаты ---
  (lft-check "«Мои»: дубликатов нет" (lft-unique-p layers-my))
  (lft-check "«Фасады»: дубликатов нет" (lft-unique-p layers-fac))
  (lft-check "«Витражи»: дубликатов нет" (lft-unique-p layers-vit))
  (lft-check "«Окна»: дубликатов нет" (lft-unique-p layers-win))

  ;; --- 5. Разделение тематических фильтров по именам групп ---
  (setq names-fac (lft-names-by-masks *tu-layer-filter-facades-masks* tree))
  (setq names-vit (lft-names-by-masks *tu-layer-filter-vitrazh-masks* tree))
  (setq names-win (lft-names-by-masks *tu-layer-filter-windows-masks* tree))

  (lft-check "«Фасады» не включают группу «Отключенные»"
             (null (lft-inter names-fac my-extra)))
  (lft-check "«Фасады» не включают группы «Витраж*/Фонар*/Окн*»"
             (null (lft-inter names-fac
                              (append (lft-names-by-masks
                                        *tu-layer-filter-vitrazh-masks* tree)
                                      (lft-names-by-masks
                                        *tu-layer-filter-windows-masks* tree)))))
  (lft-check "«Витражи» не включают группы «Фасад*/Окн*»"
             (null (lft-inter names-vit
                              (append names-fac names-win))))
  (lft-check "«Окна» не включают группы «Фасад*/Витраж*/Фонар*»"
             (null (lft-inter names-win
                              (append names-fac names-vit))))

  ;; --- 6. Критическое условие приёмки ---
  ;; Слой, входящий ТОЛЬКО в «Отключенные», виден в «Мои» и скрыт
  ;; в «Фасады»/«Витражи»/«Окна».
  (setq only-off
    (lft-diff extra-layers (append themed
                                   (tu-layer-filter-all-layers my-group tree))))
  (if only-off
    (lft-check "КРИТИЧНО: слой только из «Отключенные» — виден в «Мои», скрыт в Фасады/Витражи/Окна"
               (and (lft-subset-p only-off layers-my)
                    (null (lft-inter only-off layers-fac))
                    (null (lft-inter only-off layers-vit))
                    (null (lft-inter only-off layers-win))))
    (lft-skip "Слоёв, входящих только в «Отключенные», не найдено")
  )

  ;; --- 7. Общая контрактация диспетчера ---
  (lft-check "Без выбранных фильтров результат пустой (= все слои чертежа)"
             (null (tu-layer-filter-layers-for-keys nil)))
  (lft-check "Объединение фильтров = «или» без дублей"
             (lft-unique-p
               (tu-layer-filter-layers-for-keys '(FACADES WINDOWS))))
  (lft-check "«Мои» + «Фасады» не дают дублей"
             (lft-unique-p (tu-layer-filter-layers-for-keys '(MY FACADES))))

  ;; --- Итог ---
  (princ (strcat "\n\nИтог: PASS " (itoa *lft-pass*)
                 ", FAIL " (itoa *lft-fail*)
                 ", SKIP " (itoa *lft-skip*)))
  (if (= *lft-fail* 0)
    (princ "\n[LAYER-FILTER-TEST][OK]")
    (princ "\n[LAYER-FILTER-TEST][ERRORS]")
  )
  (princ)
)

;; Список значений одной строкой
(defun lft-list-str (lst / s)
  (setq s "")
  (foreach x lst
    (setq s (strcat s (if (= s "") "" ", ")
                    (if (= (type x) 'STR) x (vl-princ-to-string x))))
  )
  s
)

(princ "\nLAYER-FILTER-TEST.LSP загружен (ред. 1). Команда: LAYERFILTERTEST")
(princ)
