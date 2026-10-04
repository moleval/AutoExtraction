;;; ============================================================
;;; ZAPOLNENIE2.LSP - ТЕСТОВЫЙ СТЕНД доработки "Заполнение":
;;; колонка "Марка" после колонки "Тип" (только DETAIL-вывод).
;;; Рабочий ZAPOLNENIE.LSP не использует и не переопределяет -
;;; вся логика под префиксом z2-. После подтверждения правильности
;;; логика переносится в рабочий модуль, стенд удаляется.
;;; ред. 2: ошибка на отдельном блоке не прерывает команду - блок
;;; пропускается с сообщением [Z2] (номер, имя, текст ошибки).
;;;
;;; КОМАНДА: ZAPOLNENIE2 (алиас ЗАПОЛНЕНИЕ2)
;;; Требует общие модули (загружаются штатным RELOAD):
;;;   select-utils (su-*), excel-utils (eu-round2/XmlStyles/XmlEscape/
;;;   CsvQuote/FormatAreaCsv), table-utils (tc-partition-flat, ts-ac-*,
;;;   tu-next-table-point, tu-is-last-chunk, *TU-IDEAL-ROWS*),
;;;   task-utils (tu-sysvar-save/restore).
;;;
;;; ЛОГИКА КОЛОНКИ "МАРКА" (утверждено 2026-09-23):
;;;   Атрибут ВИТРАЖ - флаг (значение не печатается), атрибут МАРКА -
;;;   печатается ее значение. Регистр атрибутов не учитывается.
;;;     ВИТРАЖ есть, МАРКА есть   -> "Витраж <значение МАРКА>"
;;;     ВИТРАЖ есть, МАРКИ нет    -> "Витраж"
;;;     ВИТРАЖА нет, МАРКА есть   -> "<значение МАРКА>"
;;;     оба отсутствуют/пустые    -> пустая ячейка
;;;   Колонка добавляется ТОЛЬКО если хотя бы у одного принятого блока
;;;   выборки заполнен хотя бы один из атрибутов. Иначе вывод 1:1 со
;;;   старым форматом (без колонки).
;;;   Один Тип/размеры + разные Марки -> РАЗНЫЕ строки (Марка в ключе
;;;   группировки, 4-й критерий сортировки).
;;;   SUMMARY (таблицы/XLS/CSV) в стенде не строится - меняется только DETAIL.
;;; ============================================================

(vl-load-com)

;; ---------- КОНСТАНТЫ (как в рабочем модуле) ----------
(setq *Z2-FRAME-ALLOWANCE* 26)
(setq *Z2-HEIGHT-KEYWORDS* '("ВЫСОТА В СВЕТУ" "ВЫСОТА"))
(setq *Z2-WIDTH-KEYWORDS*  '("ШИРИНА В СВЕТУ" "ШИРИНА" "ДЛИНА"))

(setq *z2-skipped-no-height* 0)
(setq *z2-skipped-no-width* 0)
;; T, если хотя бы у одного принятого блока есть Витраж/Марка
(setq *z2-has-marks* nil)
;; диагностика ред. 2: индекс текущего блока (-1 = вне цикла), счетчик ошибок
(setq *z2-progress* -1)
(setq *z2-errors* 0)

;; ---------- ОКРУГЛЕНИЕ/ФОРМАТ (копии логики рабочего модуля) ----------
(defun z2-round2 (x)
  (/ (fix (+ (* x 100.0) 0.5)) 100.0))

(defun z2-format-area (area / int-part frac)
  (setq int-part (fix area))
  (setq frac (fix (+ (* (- area int-part) 100.0) 0.5)))
  (cond
    ((= frac 0) (itoa int-part))
    ((= (rem frac 10) 0) (strcat (itoa int-part) "," (itoa (/ frac 10))))
    ((< frac 10) (strcat (itoa int-part) ",0" (itoa frac)))
    (T (strcat (itoa int-part) "," (itoa frac)))))

;; ---------- УМНАЯ СОРТИРОВКА СТРОК (копия Р3.1) ----------
(defun z2-leading-number (s / n ch)
  (if (/= (type s) 'STR)
    nil
    (progn
      (setq s (vl-string-trim " \t" s) n "")
      (while (and (> (strlen s) 0)
                  (setq ch (substr s 1 1))
                  (>= (ascii ch) 48)
                  (<= (ascii ch) 57))
        (setq n (strcat n ch))
        (setq s (substr s 2)))
      (if (= n "") nil (atoi n)))))

(defun z2-str-smart-less (a b / na nb)
  (setq na (z2-leading-number a) nb (z2-leading-number b))
  (if (and na nb)
    (if (= na nb) (< (strcase a) (strcase b)) (< na nb))
    (< (strcase a) (strcase b))))

;; ---------- ИМЯ ЭЛЕМЕНТА (ТИП) - как в рабочем модуле ----------
(defun z2-element-name (obj / effname vis name)
  (setq effname (su-get-effective-name obj))
  (setq vis (su-get-visibility obj))
  (setq name
    (if (and vis (= (type vis) 'STR) (/= vis ""))
      vis
      (if (= (type effname) 'STR) effname "Без имени")))
  ;; ред. 2: имя обязано быть строкой - иначе сообщаем и подменяем
  (if (/= (type name) 'STR)
    (progn
      (princ (strcat "\n[Z2] ИМЯ ЭЛЕМЕНТА не строка (тип "
                     (vl-princ-to-string (type name))
                     ", значение " (vl-princ-to-string name)
                     ") - беру \"Без имени\"."))
      (setq name "Без имени")))
  name)

;; ---------- АТРИБУТЫ (НОВОЕ) ----------
;; Значение атрибута по тегу (регистр не учитывается); пустое -> nil.
(defun z2-get-attr (obj tag / attrs a tagname value result)
  (setq attrs (vl-catch-all-apply 'vlax-invoke (list obj 'GetAttributes)))
  (if (or (vl-catch-all-error-p attrs) (not (listp attrs)))
    nil
    (progn
      (foreach a attrs
        (if (null result)
          (progn
            (setq tagname (vl-catch-all-apply 'vla-get-TagString (list a)))
            (if (and (not (vl-catch-all-error-p tagname))
                     (= (type tagname) 'STR)
                     (= (strcase (vl-string-trim " " tagname)) (strcase tag)))
              (progn
                (setq value (vl-catch-all-apply 'vla-get-TextString (list a)))
                (if (and (not (vl-catch-all-error-p value))
                         (= (type value) 'STR))
                  (setq value (vl-string-trim " \t" value))
                  (setq value nil))
                (if (and value (/= value "")) (setq result value)))))))
      result)))

;; Комбинированное значение колонки "Марка" (правило утверждено):
(defun z2-mark-line (obj / vit mar)
  (setq vit (z2-get-attr obj "ВИТРАЖ")
        mar (z2-get-attr obj "МАРКА"))
  (cond
    ((and vit mar) (strcat "Витраж " mar))
    (vit "Витраж")
    (mar mar)
    (T nil)))

;; ---------- ПОИСК ДИН-СВОЙСТВА (копия рабочего модуля) ----------
(defun z2-find-property (obj keyword / dynprops prop pname value result)
  (setq dynprops
    (vl-catch-all-apply 'vlax-invoke
      (list obj 'GetDynamicBlockProperties)))
  (if (vl-catch-all-error-p dynprops)
    nil
    (progn
      (setq result nil)
      (foreach prop dynprops
        (if (null result)
          (progn
            (setq pname
              (vl-catch-all-apply 'vla-get-PropertyName (list prop)))
            (if (and (not (vl-catch-all-error-p pname)) pname
                     (= (type pname) 'STR)
                     (= (strcase pname) (strcase keyword)))
              (progn
                (setq value
                  (vl-catch-all-apply 'vla-get-Value (list prop)))
                (if (not (vl-catch-all-error-p value))
                  (setq result (su-value-to-number value))))))))
      (if (null result)
        (foreach prop dynprops
          (if (null result)
            (progn
              (setq pname
                (vl-catch-all-apply 'vla-get-PropertyName (list prop)))
              (if (and (not (vl-catch-all-error-p pname)) pname
                       (= (type pname) 'STR)
                       (vl-string-search (strcase keyword) (strcase pname)))
                (progn
                  (setq value
                    (vl-catch-all-apply 'vla-get-Value (list prop)))
                  (if (not (vl-catch-all-error-p value))
                    (setq result (su-value-to-number value)))))))))
      result)))

(defun z2-get-dimension (obj keywords-priority / result kw)
  (setq result nil)
  (foreach kw keywords-priority
    (if (null result)
      (setq result (z2-find-property obj kw))))
  result)

;; ---------- РАЗМЕРЫ С ПРИПУСКОМ (копия) ----------
(defun z2-compute-dims (obj / raw-h raw-w h w)
  (setq raw-h (z2-get-dimension obj *Z2-HEIGHT-KEYWORDS*))
  (setq raw-w (z2-get-dimension obj *Z2-WIDTH-KEYWORDS*))
  (cond
    ((or (null raw-h) (<= raw-h 0.0))
     (setq *z2-skipped-no-height* (1+ *z2-skipped-no-height*))
     nil)
    ((or (null raw-w) (<= raw-w 0.0))
     (setq *z2-skipped-no-width* (1+ *z2-skipped-no-width*))
     nil)
    (T
     (setq h (fix (+ raw-h *Z2-FRAME-ALLOWANCE*)))
     (setq w (fix (+ raw-w *Z2-FRAME-ALLOWANCE*)))
     (list h w))))

;; ---------- СОРТИРОВКА DETAIL (копия + 4-й критерий: Марка) ----------
(defun z2-sort-less (a b / ta tb ha hb wa wb ma mb)
  (setq ta (strcase (car a)) tb (strcase (car b))
        ha (cadr a) hb (cadr b)
        wa (caddr a) wb (caddr b)
        ma (if (null (nth 4 a)) "" (strcase (nth 4 a)))
        mb (if (null (nth 4 b)) "" (strcase (nth 4 b))))
  (cond
    ((z2-str-smart-less ta tb) T)
    ((z2-str-smart-less tb ta) nil)
    ((< ha hb) T)
    ((> ha hb) nil)
    ((< wa wb) T)
    ((> wa wb) nil)
    ((z2-str-smart-less ma mb) T)
    ((z2-str-smart-less mb ma) nil)
    (T nil)))

;; ---------- ДИАГНОСТИКА БЛОКА (ред. 2) ----------
(defun z2-block-diag (i obj err / nm)
  (setq nm (if obj (vl-catch-all-apply 'vla-get-EffectiveName (list obj)) "?"))
  (if (vl-catch-all-error-p nm) (setq nm "?"))
  (if (/= (type nm) 'STR) (setq nm (vl-princ-to-string nm)))
  (princ (strcat "\n[Z2] Блок #" (itoa (1+ i)) " (" nm "): "
                 (vl-catch-all-error-message err)
                 " - БЛОК ПРОПУЩЕН.")))

;; ---------- АГРЕГАЦИЯ (Марка в ключе и в записи; ред. 2 - с диагностикой) ----------
;; Внутренняя структура: (key name h w count mark)
;; Возвращаемая:         (name h w count mark)  - mark nil | "строка"
(defun z2-aggregate (inserts /
    i ent obj name dims h w mark key found acc display-names r sorted)
  (setq acc '() display-names '() i 0)
  (setq *z2-skipped-no-height* 0 *z2-skipped-no-width* 0)
  (setq *z2-has-marks* nil)
  (setq *z2-errors* 0)

  (repeat (length inserts)
    (setq ent (nth i inserts) obj nil)
    (setq *z2-progress* i)
    (setq r
      (vl-catch-all-apply
        '(lambda ()
           (setq obj (vlax-ename->vla-object ent))
           (setq name (z2-element-name obj))
           (if (/= (type name) 'STR)
             (progn
               (princ (strcat "\n[Z2] Блок #" (itoa (1+ i))
                              ": ИМЯ не строка (тип " (vl-princ-to-string (type name))
                              ", значение " (vl-princ-to-string name)
                              ") - беру \"Без имени\"."))
               (setq name "Без имени")))
           (setq dims (z2-compute-dims obj))
           (if dims
             (progn
               (setq h (car dims) w (cadr dims))
               (setq mark (z2-mark-line obj))
               (if (and mark (/= (type mark) 'STR))
                 (progn
                   (princ (strcat "\n[Z2] Блок #" (itoa (1+ i))
                                  ": МАРКА не строка (тип " (vl-princ-to-string (type mark))
                                  ", значение " (vl-princ-to-string mark)
                                  ") - перевожу в строку."))
                   (setq mark (vl-princ-to-string mark))))
               (if mark (setq *z2-has-marks* T))
               (setq key (strcat (strcase name) "|" (itoa h) "|" (itoa w)
                                 "|" (if mark (strcase mark) "-")))
               (setq found (assoc key acc))
               (if found
                 (setq acc
                   (subst
                     (list key
                           (cadr found)
                           (caddr found)
                           (cadddr found)
                           (1+ (car (cddddr found)))
                           (nth 5 found))
                     found acc))
                 (progn
                   (if (not (assoc (strcase name) display-names))
                     (setq display-names
                       (cons (cons (strcase name) name) display-names)))
                   (setq acc (cons (list key name h w 1 mark) acc)))))))
        (list)))
    (if (vl-catch-all-error-p r)
      (progn
        (setq *z2-errors* (1+ *z2-errors*))
        (if (<= *z2-errors* 20)
          (z2-block-diag i obj r)
          (if (= *z2-errors* 21)
            (princ "\n[Z2] Дальнейшие сообщения об ошибках подавлены.")))))
    (setq i (1+ i)))

  (setq sorted (vl-catch-all-apply 'vl-sort (list acc 'z2-sort-less)))
  (if (vl-catch-all-error-p sorted)
    (progn
      (princ (strcat "\n[Z2] Ошибка СОРТИРОВКИ: " (vl-catch-all-error-message sorted)
                     " - использую данные без сортировки."))
      (setq *z2-errors* (1+ *z2-errors*) sorted acc)))

  (mapcar
    '(lambda (rec)
       (list (cadr rec) (caddr rec) (cadddr rec)
             (car (cddddr rec)) (nth 5 rec)))
    sorted))

;; ---------- КУСКОВАНИЕ: ПЛОСКИЙ СПИСОК (копия, rec из 5 полей) ----------
(defun z2-build-flat-items (data / items groups grp grpName grpRows
                                   rec h w cnt area totalCnt totalArea groupIndex)
  (setq items '() groupIndex 0)
  (setq groups '())
  (foreach rec data
    (setq grpName (car rec))
    (setq grp (assoc grpName groups))
    (if grp
      (setq groups (subst (append grp (list (list rec))) grp groups))
      (setq groups (append groups (list (list grpName (list rec)))))))
  (foreach grp groups
    (setq groupIndex (1+ groupIndex))
    (setq grpName (car grp))
    (setq grpRows (cdr grp))
    (setq totalCnt 0 totalArea 0.0)
    (foreach rec grpRows
      (setq rec (car rec))
      (setq items (append items (list (cons 'data (cons groupIndex rec)))))
      (setq h (cadr rec) w (caddr rec) cnt (cadddr rec))
      (setq area (z2-round2 (/ (* h w cnt) 1000000.0)))
      (setq totalCnt  (+ totalCnt cnt))
      (setq totalArea (+ totalArea area)))
    (setq items
      (append items
        (list (list 'subtotal groupIndex grpName totalCnt totalArea)))))
  items)

(defun z2-build-units (data idealRows / items chunks ch result)
  (setq items (z2-build-flat-items data))
  (setq chunks (tc-partition-flat items idealRows))
  (setq result '())
  (foreach ch chunks
    (setq result (append result (list (cons (length ch) ch)))))
  result)

;; ---------- ТАБЛИЦА DETAIL (колонка Марка после Типа, если есть) ----------
(defun z2-create-table-detail (data /
    pt pt_wcs units total-chunks chunk-idx is-last chunk items item
    nCols nRows space tbl row oldEcho doc
    lastGroupIdx rowInGroup maxNameLen nameStr maxMarkLen markStr
    tip h w cnt mark area total-cnt total-area
    grpName grpCnt grpArea
    maxNumLen zpGroups grpEntry grpIdx numStr col0Width col1Width)

  (if (null data)
    (progn (princ "\nНет данных для таблицы Заполнение2.") nil)
    (progn
      (setq pt (getpoint "\nУкажите точку вставки таблицы: "))
      (if (null pt)
        (progn (princ "\nТаблица пропущена.") nil)
        (progn
          (setq doc (vlax-get-acad-object))
          (setq doc (vla-get-activedocument doc))
          (setq space (vla-get-modelspace doc))
          (setq pt_wcs (trans pt 1 0))
          (setq oldEcho (getvar "CMDECHO"))
          (vl-catch-all-apply 'setvar (list "CMDECHO" 0))
          (vla-startundomark doc)

          (setq maxNameLen 10)
          (foreach item data
            (setq nameStr (car item))
            (if (> (strlen nameStr) maxNameLen)
              (setq maxNameLen (strlen nameStr))))
          (setq maxMarkLen 10)
          (foreach item data
            (setq markStr (if (nth 4 item) (nth 4 item) ""))
            (if (> (strlen markStr) maxMarkLen)
              (setq maxMarkLen (strlen markStr))))

          (setq maxNumLen 3)
          (setq zpGroups '())
          (foreach item data
            (setq grpName (car item))
            (if (not (assoc grpName zpGroups))
              (setq zpGroups (cons (list grpName 0) zpGroups))))
          (foreach item data
            (setq grpName (car item))
            (setq grpEntry (assoc grpName zpGroups))
            (if grpEntry
              (setq zpGroups
                (subst (list grpName (1+ (cadr grpEntry)))
                       grpEntry zpGroups))))
          (setq grpIdx 1)
          (foreach grpEntry (reverse zpGroups)
            (setq numStr (strcat (itoa grpIdx) "." (itoa (cadr grpEntry))))
            (if (> (strlen numStr) maxNumLen)
              (setq maxNumLen (strlen numStr)))
            (setq grpIdx (1+ grpIdx)))
          (setq col0Width (max 15.0 (* (+ maxNumLen 1) 3.5)))
          (setq col1Width (max 75.0 (* maxNameLen 3.0)))

          (setq total-cnt 0 total-area 0.0)
          (foreach item data
            (setq total-cnt (+ total-cnt (cadddr item)))
            (setq total-area (+ total-area
              (z2-round2
                (/ (* (cadr item) (caddr item) (cadddr item)) 1000000.0)))))

          (setq units (z2-build-units data *TU-IDEAL-ROWS*))
          (setq total-chunks (length units))
          ;; Колонка Марка - только если в выборке реально есть Витраж/Марка
          (setq nCols (if *z2-has-marks* 7 6))
          (setq chunk-idx 0)

          (setq lastGroupIdx -1 rowInGroup 0)

          (foreach chunk units
            (setq is-last (tu-is-last-chunk chunk-idx total-chunks))
            (setq nRows (+ 2 (car chunk)))
            (if is-last (setq nRows (1+ nRows)))
            (setq items (cdr chunk))
            (setq tbl (vl-catch-all-apply 'vla-addtable
              (list space (vlax-3d-point pt_wcs) nRows nCols 10.0 50.0)))

            (if (vl-catch-all-error-p tbl)
              (princ (strcat "\nОшибка создания таблицы Заполнение2: "
                             (vl-catch-all-error-message tbl)))
              (progn
                (vla-SetColumnWidth tbl 0 col0Width)
                (vla-SetColumnWidth tbl 1 col1Width)
                (if *z2-has-marks*
                  (progn
                    (vla-SetColumnWidth tbl 2 (max 40.0 (* maxMarkLen 3.0)))
                    (vla-SetColumnWidth tbl 3 30.0)
                    (vla-SetColumnWidth tbl 4 30.0)
                    (vla-SetColumnWidth tbl 5 30.0)
                    (vla-SetColumnWidth tbl 6 35.0))
                  (progn
                    (vla-SetColumnWidth tbl 2 30.0)
                    (vla-SetColumnWidth tbl 3 30.0)
                    (vla-SetColumnWidth tbl 4 30.0)
                    (vla-SetColumnWidth tbl 5 35.0)))

                (ts-ac-title tbl 0 "Заполнение (тест Марка)" nCols)

                (if *z2-has-marks*
                  (ts-ac-header tbl 1
                    '("№" "Тип" "Марка" "Высота, мм" "Ширина, мм" "Кол-во, шт." "Площадь, м2"))
                  (ts-ac-header tbl 1
                    '("№" "Тип" "Высота, мм" "Ширина, мм" "Кол-во, шт." "Площадь, м2")))

                (setq row 2)

                (foreach item items
                  (if (eq (car item) 'data)
                    (progn
                      (setq groupIdx (cadr item))
                      (setq tip (caddr item))
                      (setq h   (cadddr item))
                      (setq w   (caddr (cddr item)))
                      (setq cnt (cadddr (cddr item)))
                      (setq mark (nth 6 item))
                      (setq area
                        (z2-round2 (/ (* h w cnt) 1000000.0)))

                      (if (/= groupIdx lastGroupIdx)
                        (progn
                          (setq lastGroupIdx groupIdx)
                          (setq rowInGroup 0)))
                      (setq rowInGroup (1+ rowInGroup))

                      (vla-SetText tbl row 0
                        (strcat (itoa groupIdx) "." (itoa rowInGroup)))
                      (vla-SetText tbl row 1 tip)
                      (if *z2-has-marks*
                        (progn
                          (vla-SetText tbl row 2 (if mark mark ""))
                          (vla-SetText tbl row 3 (itoa h))
                          (vla-SetText tbl row 4 (itoa w))
                          (vla-SetText tbl row 5 (itoa cnt))
                          (vla-SetText tbl row 6 (z2-format-area area)))
                        (progn
                          (vla-SetText tbl row 2 (itoa h))
                          (vla-SetText tbl row 3 (itoa w))
                          (vla-SetText tbl row 4 (itoa cnt))
                          (vla-SetText tbl row 5 (z2-format-area area))))

                      (vla-SetCellAlignment tbl row 0 5)
                      (vla-SetCellAlignment tbl row 1 4)
                      (if *z2-has-marks*
                        (progn
                          (vla-SetCellAlignment tbl row 2 4)
                          (vla-SetCellAlignment tbl row 3 5)
                          (vla-SetCellAlignment tbl row 4 5)
                          (vla-SetCellAlignment tbl row 5 5)
                          (vla-SetCellAlignment tbl row 6 5))
                        (progn
                          (vla-SetCellAlignment tbl row 2 5)
                          (vla-SetCellAlignment tbl row 3 5)
                          (vla-SetCellAlignment tbl row 4 5)
                          (vla-SetCellAlignment tbl row 5 5)))

                      (setq row (1+ row)))

                    (progn
                      (setq grpName (nth 2 item))
                      (setq grpCnt  (nth 3 item))
                      (setq grpArea (nth 4 item))

                      (ts-ac-subtotal tbl row groupIdx
                        (strcat "   {\\L" grpName "}") 1 (if *z2-has-marks* 3 3))
                      (if *z2-has-marks*
                        (progn
                          (vla-SetText tbl row 5 (itoa grpCnt))
                          (vla-SetText tbl row 6
                            (z2-format-area grpArea))
                          (vla-SetCellAlignment tbl row 5 5)
                          (vla-SetCellAlignment tbl row 6 5))
                        (progn
                          (vla-SetText tbl row 4 (itoa grpCnt))
                          (vla-SetText tbl row 5
                            (z2-format-area grpArea))
                          (vla-SetCellAlignment tbl row 4 5)
                          (vla-SetCellAlignment tbl row 5 5)))

                      (setq row (1+ row)))))

                (if is-last
                  (progn
                    (ts-ac-total tbl row
                      "           {\\LИтого по всем позициям:}" 0 3 (if *z2-has-marks* 5 4))
                    (if *z2-has-marks*
                      (progn
                        (vla-SetText tbl row 5 (itoa total-cnt))
                        (vla-SetText tbl row 6
                          (z2-format-area total-area))
                        (vla-SetCellAlignment tbl row 5 5)
                        (vla-SetCellAlignment tbl row 6 5))
                      (progn
                        (vla-SetText tbl row 4 (itoa total-cnt))
                        (vla-SetText tbl row 5
                          (z2-format-area total-area))
                        (vla-SetCellAlignment tbl row 4 5)
                        (vla-SetCellAlignment tbl row 5 5)))))

                (vla-update tbl)
                (princ (strcat "\nТаблица Заполнение2 "
                               (itoa (1+ chunk-idx)) " создана."))
                (setq pt_wcs
                  (tu-next-table-point pt_wcs nRows 10.0 20.0))))

            (setq chunk-idx (1+ chunk-idx)))

          (vla-endundomark doc)
          (vl-catch-all-apply 'setvar (list "CMDECHO" oldEcho))
          (princ (strcat "\nВсего создано таблиц Заполнение2: "
                         (itoa total-chunks)))
          T)))))

;; ---------- XLS DETAIL (Марка после Типа; формулы как в рабочем) ----------
(defun z2-export-xls-detail (data xlsfile / f rec tip h w cnt mark area
                                     total-cnt total-area
                                     groups grp grpName grpRows grpCnt grpArea
                                     startRow endRow itemNum
                                     subtotal-rows formula-cnt formula-area r
                                     cCnt cArea cMarkMerge)
  (setq cCnt  (if *z2-has-marks* 6 5)
        cArea (if *z2-has-marks* 7 6)
        cMarkMerge (if *z2-has-marks* 4 3))
  (setq f (open xlsfile "w"))
  (if (null f)
    nil
    (progn
      (write-line "<?xml version=\"1.0\" encoding=\"windows-1251\"?>" f)
      (write-line "<?mso-application progid=\"Excel.Sheet\"?>" f)
      (write-line "<Workbook xmlns=\"urn:schemas-microsoft-com:office:spreadsheet\"" f)
      (write-line " xmlns:o=\"urn:schemas-microsoft-com:office:office\"" f)
      (write-line " xmlns:x=\"urn:schemas-microsoft-com:office:excel\"" f)
      (write-line " xmlns:ss=\"urn:schemas-microsoft-com:office:spreadsheet\"" f)
      (write-line " xmlns:html=\"http://www.w3.org/TR/REC-html40\">" f)
      (write-line (eu-xml-styles "0.##") f)

      (write-line " <Worksheet ss:Name=\"Zapolnenie2Detail\">" f)
      (write-line "  <Table>" f)
      (write-line "   <Column ss:Width=\"30\"/>" f)
      (write-line "   <Column ss:Width=\"125\"/>" f)
      (if *z2-has-marks*
        (progn
          (write-line "   <Column ss:Width=\"90\"/>" f)
          (write-line "   <Column ss:Width=\"80\"/>" f)
          (write-line "   <Column ss:Width=\"80\"/>" f)
          (write-line "   <Column ss:Width=\"80\"/>" f)
          (write-line "   <Column ss:Width=\"80\"/>" f))
        (progn
          (write-line "   <Column ss:Width=\"80\"/>" f)
          (write-line "   <Column ss:Width=\"80\"/>" f)
          (write-line "   <Column ss:Width=\"80\"/>" f)
          (write-line "   <Column ss:Width=\"80\"/>" f)))

      (write-line "   <Row ss:Height=\"20\">" f)
      (write-line (strcat "    <Cell ss:StyleID=\"Header\" ss:MergeAcross=\""
                          (itoa (1- (if *z2-has-marks* 7 6)))
                          "\"><Data ss:Type=\"String\">Заполнение (тест Марка)</Data></Cell>") f)
      (write-line "   </Row>" f)

      (write-line "   <Row>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">№</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Тип</Data></Cell>" f)
      (if *z2-has-marks*
        (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Марка</Data></Cell>" f))
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Высота, мм</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Ширина, мм</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Кол-во, шт.</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Площадь, м2</Data></Cell>" f)
      (write-line "   </Row>" f)

      (setq groups '())
      (foreach rec data
        (setq tip (car rec))
        (setq grp (assoc tip groups))
        (if grp
          (setq groups (subst (append grp (list (list rec))) grp groups))
          (setq groups (append groups (list (list tip (list rec)))))))

      (setq rowNum 3 itemNum 0 total-cnt 0 total-area 0.0
            subtotal-rows '())

      (foreach grp groups
        (setq grpName (car grp)
              grpRows (cdr grp)
              grpCnt  0
              grpArea 0.0
              startRow rowNum)

        (foreach rec grpRows
          (setq rec (car rec))
          (setq itemNum (1+ itemNum)
                h   (cadr rec)
                w   (caddr rec)
                cnt (cadddr rec)
                mark (nth 4 rec)
                area (eu-round2 (/ (* h w cnt) 1000000.0)))
          (setq grpCnt (+ grpCnt cnt)
                grpArea (+ grpArea area)
                total-cnt (+ total-cnt cnt)
                total-area (+ total-area area))

          (write-line "   <Row>" f)
          (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (itoa itemNum) "</Data></Cell>") f)
          (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"String\">" (eu-xml-escape grpName) "</Data></Cell>") f)
          (if *z2-has-marks*
            (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"String\">" (if mark (eu-xml-escape mark) "") "</Data></Cell>") f))
          (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (itoa h) "</Data></Cell>") f)
          (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (itoa w) "</Data></Cell>") f)
          (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (itoa cnt) "</Data></Cell>") f)
          (write-line (strcat "    <Cell ss:StyleID=\"Num\" ss:Formula=\"=ROUND(RC[-3]*RC[-2]*RC[-1]/1000000,2)\"><Data ss:Type=\"Number\">" (rtos area 2 2) "</Data></Cell>") f)
          (write-line "   </Row>" f)

          (setq rowNum (1+ rowNum)))

        (setq endRow (1- rowNum))

        (write-line "   <Row>" f)
        (write-line "    <Cell ss:StyleID=\"BoldUnderline\"><Data ss:Type=\"String\"></Data></Cell>" f)
        (write-line (strcat "    <Cell ss:StyleID=\"BoldUnderline\"><Data ss:Type=\"String\">" (eu-xml-escape grpName) "</Data></Cell>") f)
        (if *z2-has-marks*
          (write-line "    <Cell ss:StyleID=\"BoldUnderline\"><Data ss:Type=\"String\"></Data></Cell>" f))
        (write-line "    <Cell ss:StyleID=\"BoldUnderline\"><Data ss:Type=\"String\"></Data></Cell>" f)
        (write-line "    <Cell ss:StyleID=\"BoldUnderline\"><Data ss:Type=\"String\"></Data></Cell>" f)
        (write-line (strcat "    <Cell ss:StyleID=\"BoldUnderline\" ss:Formula=\"=SUM(R" (itoa startRow) "C" (itoa cCnt) ":R" (itoa endRow) "C" (itoa cCnt) ")\"><Data ss:Type=\"Number\">" (itoa grpCnt) "</Data></Cell>") f)
        (write-line (strcat "    <Cell ss:StyleID=\"BoldUnderlineNum\" ss:Formula=\"=SUM(R" (itoa startRow) "C" (itoa cArea) ":R" (itoa endRow) "C" (itoa cArea) ")\"><Data ss:Type=\"Number\">" (rtos grpArea 2 2) "</Data></Cell>") f)
        (write-line "   </Row>" f)
        (setq rowNum (1+ rowNum))
        (setq subtotal-rows (append subtotal-rows (list (1- rowNum))))
      )

      (setq formula-cnt "" formula-area "")
      (foreach r subtotal-rows
        (if (= formula-cnt "")
          (setq formula-cnt (strcat "=R" (itoa r) "C" (itoa cCnt)))
          (setq formula-cnt (strcat formula-cnt "+R" (itoa r) "C" (itoa cCnt))))
        (if (= formula-area "")
          (setq formula-area (strcat "=R" (itoa r) "C" (itoa cArea)))
          (setq formula-area (strcat formula-area "+R" (itoa r) "C" (itoa cArea)))))

      (write-line "   <Row>" f)
      (write-line (strcat "    <Cell ss:StyleID=\"BoldUnderline\" ss:MergeAcross=\"" (itoa cMarkMerge) "\"><Data ss:Type=\"String\">Итого</Data></Cell>") f)
      (write-line (strcat "    <Cell ss:StyleID=\"BoldUnderline\" ss:Formula=\"" formula-cnt "\"><Data ss:Type=\"Number\">" (itoa total-cnt) "</Data></Cell>") f)
      (write-line (strcat "    <Cell ss:StyleID=\"BoldUnderlineNum\" ss:Formula=\"" formula-area "\"><Data ss:Type=\"Number\">" (rtos total-area 2 2) "</Data></Cell>") f)
      (write-line "   </Row>" f)

      (write-line "  </Table>" f)
      (write-line " </Worksheet>" f)
      (write-line "</Workbook>" f)
      (close f)
      T)))

;; ---------- CSV DETAIL (Марка после Типа) ----------
(defun z2-export-csv-detail (data csvfile / f rec tip h w cnt mark area
                                    total-cnt total-area
                                    groups grp grpName grpRows grpCnt grpArea itemNum)
  (setq f (open csvfile "w"))
  (if f
    (progn
      (write-line "Заполнение (тест Марка)" f)
      (if *z2-has-marks*
        (write-line "№;Тип;Марка;Высота, мм;Ширина, мм;Кол-во, шт.;Площадь, м2" f)
        (write-line "№;Тип;Высота, мм;Ширина, мм;Кол-во, шт.;Площадь, м2" f))
      (setq total-cnt 0 total-area 0.0 itemNum 0)

      (setq groups '())
      (foreach rec data
        (setq tip (car rec))
        (setq grp (assoc tip groups))
        (if grp
          (setq groups (subst (append grp (list (list rec))) grp groups))
          (setq groups (append groups (list (list tip (list rec)))))))

      (foreach grp groups
        (setq grpName (car grp)
              grpRows (cdr grp)
              grpCnt  0
              grpArea 0.0)

        (foreach rec grpRows
          (setq rec (car rec))
          (setq itemNum (1+ itemNum)
                h   (cadr rec)
                w   (caddr rec)
                cnt (cadddr rec)
                mark (nth 4 rec)
                area (eu-round2 (/ (* h w cnt) 1000000.0)))
          (setq grpCnt (+ grpCnt cnt)
                grpArea (+ grpArea area)
                total-cnt (+ total-cnt cnt)
                total-area (+ total-area area))
          (if *z2-has-marks*
            (write-line
              (strcat (itoa itemNum) ";"
                      (eu-csv-quote grpName) ";"
                      (eu-csv-quote (if mark mark "")) ";"
                      (itoa h) ";"
                      (itoa w) ";"
                      (itoa cnt) ";"
                      "\"" (eu-format-area-csv area) "\"")
              f)
            (write-line
              (strcat (itoa itemNum) ";"
                      (eu-csv-quote grpName) ";"
                      (itoa h) ";"
                      (itoa w) ";"
                      (itoa cnt) ";"
                      "\"" (eu-format-area-csv area) "\"")
              f)))

        (if *z2-has-marks*
          (write-line
            (strcat ";" (eu-csv-quote grpName) ";;;;"
                    (itoa grpCnt) ";"
                    "\"" (eu-format-area-csv grpArea) "\"")
            f)
          (write-line
            (strcat ";" (eu-csv-quote grpName) ";;;"
                    (itoa grpCnt) ";"
                    "\"" (eu-format-area-csv grpArea) "\"")
            f)))

      (if *z2-has-marks*
        (write-line
          (strcat "Итого;;;;"
                  (itoa total-cnt) ";"
                  "\"" (eu-format-area-csv total-area) "\"")
          f)
        (write-line
          (strcat "Итого;;;"
                  (itoa total-cnt) ";"
                  "\"" (eu-format-area-csv total-area) "\"")
          f))
      (close f)
      T)
    nil))

;; ---------- MAIN ----------
(defun zapolnenie2-main (layers create-table / *error* inserts data
                              xlsfile csvfile xls-ok svSaved)
  (vl-load-com)
  (sssetfirst nil nil)

  (defun *error* (msg)
    (if (and msg
             (not (wcmatch (strcase msg)
                    "*BREAK*,*CANCEL*,*QUIT*,*EXIT*")))
      (progn
        (princ (strcat "\nОшибка: " msg))
        (if (>= *z2-progress* 0)
          (princ (strcat "\n[Z2] Сбой возник при обработке блока #"
                         (itoa (1+ *z2-progress*)) " (нумерация с 1).")))))
    (tu-sysvar-restore svSaved)
    (princ))

  (setq svSaved (tu-sysvar-save (list "CMDECHO")))

  (setq inserts (su-select-inserts layers))
  (if (or (null inserts) (= (length inserts) 0))
    (progn (princ "\nБлоки не найдены.")
           (tu-sysvar-restore svSaved) (princ))
    (progn
      (princ (strcat "\nБлоков принято к извлечению: " (itoa (length inserts))))
      (setq data (z2-aggregate inserts))
(setq *z2-progress* -1)
(if (> *z2-errors* 0)
  (princ (strcat "\n[Z2] Итого блоков пропущено из-за ошибок: " (itoa *z2-errors*)
                 " - подробности выше; пришлите этот лог разработчику.")))
      (princ (strcat "\nБлоков пропущено (нет Высоты): " (itoa *z2-skipped-no-height*)
                     ", (нет Ширины): " (itoa *z2-skipped-no-width*)))
      (if (null data)
        (progn (princ "\nНет данных для Заполнение2.")
               (tu-sysvar-restore svSaved) (princ))
        (progn
          (princ (strcat "\nПозиций после группировки: " (itoa (length data))))
          (princ (if *z2-has-marks*
                   "\nКолонка МАРКА: ДА (найдены атрибуты Витраж/Марка)"
                   "\nКолонка МАРКА: НЕТ (атрибуты Витраж/Марка не найдены - формат как раньше)"))
          (setq xlsfile (strcat (getvar "DWGPREFIX")
                                (vl-filename-base (getvar "DWGNAME"))
                                " Заполнение2.xls"))
          (setq xls-ok (z2-export-xls-detail data xlsfile))
          (if xls-ok
            (princ (strcat "\nXLS сохранен: " xlsfile))
            (progn
              (setq csvfile (strcat (getvar "DWGPREFIX")
                                    (vl-filename-base (getvar "DWGNAME"))
                                    " Заполнение2.csv"))
              (if (z2-export-csv-detail data csvfile)
                (princ (strcat "\nНе удалось сохранить XLS. Сохраняю CSV: " csvfile))
                (princ "\nНе удалось сохранить ни XLS, ни CSV (файл занят?)"))))
          (if create-table
            (z2-create-table-detail data)
            (princ "\nТаблица AutoCAD пропущена (по вашему выбору)."))
          (tu-sysvar-restore svSaved)
          (princ))))))

;; ---------- КОМАНДА ----------
(defun c:zapolnenie2 ( / layers-str layers create-table)
  (if (not (and (type 'su-select-inserts)
                (type 'tc-partition-flat)
                (type 'ts-ac-header)
                (type 'eu-xml-styles)
                (type 'tu-sysvar-save)))
    (progn
      (princ "\nНе загружены общие модули. Запустите RELOAD.")
      (princ)
      (exit)))

  (setq layers-str
    (getstring T "\nВведите слои через запятую (Enter — все слои): "))
  (if (= layers-str "")
    (setq layers nil)
    (setq layers
      (mapcar
        '(lambda (x) (strcase (vl-string-trim " " x)))
        (z2-split-string layers-str ","))))

  (initget "Y N")
  (setq create-table
    (getkword "\nСоздать таблицу AutoCAD? [Да(Y)/Нет(N)] <Y>: "))
  (if (or (null create-table) (= create-table "Y"))
    (setq create-table T)
    (setq create-table nil))

  (zapolnenie2-main layers create-table)
  (princ))

(defun c:ЗАПОЛНЕНИЕ2 () (c:zapolnenie2))

;; ---------- РАЗБИЕНИЕ СТРОКИ (копия логики n1-split-string) ----------
(defun z2-split-string (str delim / pos result item)
  (setq result '())
  (while (setq pos (vl-string-search delim str))
    (setq item (vl-string-trim " " (substr str 1 pos)))
    (if (/= item "")
      (setq result (append result (list item))))
    (setq str (substr str (+ pos 1 (strlen delim)))))
  (if (/= (vl-string-trim " " str) "")
    (setq result (append result (list (vl-string-trim " " str)))))
  result)

(princ "\nZAPOLNENIE2.LSP загружен (ред. 2: тест колонки Марка + диагностика ошибок агрегации). Команда: ZAPOLNENIE2 / ЗАПОЛНЕНИЕ2")
(princ)
