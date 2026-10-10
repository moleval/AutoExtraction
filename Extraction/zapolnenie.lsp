;;; ============================================================
;;; ZAPOLNENIE.LSP — Заполнение проемов
;;; (стеклопакеты, сэндвич-панели, глухие панели, витражное стекло)
;;;
;;; РЕДАКЦИЯ 30. История редакций — HANDOFF_TZ.md.
;;;
;;; ТЕХНИЧЕСКИЕ ПРАВИЛА (Этап 0):
;;;   - Видимость отсутствует -> EffectiveName, не пропускать
;;;   - Высота: "ВЫСОТА В СВЕТУ" -> "ВЫСОТА"
;;;   - Ширина: "ШИРИНА В СВЕТУ" -> "ШИРИНА" -> "ДЛИНА"
;;;   - Поиск свойства: точное совпадение, затем подстрока
;;;   - Припуск: +26 мм; округление размеров: fix (целые мм)
;;;   - Нулевой размер: пропуск (раздельные счетчики)
;;;   - Площадь: мм2 -> м2, округление до 2 знаков ДО суммирования
;;;   - Отображение площади: 3,00 -> 3
;;;   - DETAIL ключ: UPPERCASE(ТИП) | ВЫСОТА | ШИРИНА
;;;   - SUMMARY строится из DETAIL (один проход)
;;;   - Сортировка DETAIL: Тип -> Высота -> Ширина
;;;   - Сортировка SUMMARY: Тип (алфавит)
;;;
;;; Р3.1: числовая сортировка артикулов ("526" < "1226") через
;;;       zp-leading-number и zp-str-smart-less.
;;; ============================================================

(vl-load-com)

;; ---------- КОНСТАНТЫ ----------
;; Припуск на раму по умолчанию. Рабочее значение берётся из настроек
;; (task.ZAPOLNENIE / input.frame.allowance), эта константа — запасной
;; вариант, если модуль настроек не загружен.
(setq *ZAPOLNENIE-FRAME-ALLOWANCE* 26)

;; Действующий припуск: настройка, иначе константа выше
(defun zapolnenie-frame-allowance ( / v)
  (if (= (type ae-settings-frame-allowance) 'SUBR)
    (progn
      (setq v (ae-settings-frame-allowance))
      (if (and (numberp v) (>= v 0)) v *ZAPOLNENIE-FRAME-ALLOWANCE*))
    *ZAPOLNENIE-FRAME-ALLOWANCE*)
)
(setq *ZAPOLNENIE-SHEET-SIZE* "3210x2250")
(setq *ZAPOLNENIE-ROTATE* "вращать")

(setq *ZAPOLNENIE-HEIGHT-KEYWORDS* '("ВЫСОТА В СВЕТУ" "ВЫСОТА"))
(setq *ZAPOLNENIE-WIDTH-KEYWORDS*  '("ШИРИНА В СВЕТУ" "ШИРИНА" "ДЛИНА"))
;; Марка (ред. 29): T, если хотя бы у одного принятого блока выборки
;; заполнен атрибут ВИТРАЖ или МАРКА
(setq *zapolnenie-has-marks* nil)
;; Диагностика (ред. 29): индекс текущего блока (-1 = вне цикла), счетчик ошибок
(setq *zapolnenie-progress* -1)
(setq *zapolnenie-errors* 0)

(setq *zapolnenie-skipped-no-height* 0)
(setq *zapolnenie-skipped-no-width* 0)

;; ---------- ОКРУГЛЕНИЕ И ФОРМАТИРОВАНИЕ ----------
(defun zapolnenie-round2 (x)
  (/ (fix (+ (* x 100.0) 0.5)) 100.0))

(defun zapolnenie-format-area (area / int-part frac)
  (setq int-part (fix area))
  (setq frac (fix (+ (* (- area int-part) 100.0) 0.5)))
  (cond
    ((= frac 0) (itoa int-part))
    ((= (rem frac 10) 0) (strcat (itoa int-part) "," (itoa (/ frac 10))))
    ((< frac 10) (strcat (itoa int-part) ",0" (itoa frac)))
    (T (strcat (itoa int-part) "," (itoa frac)))))

;; ---------- УМНАЯ СОРТИРОВКА СТРОК (Р3.1) ----------
(defun zp-leading-number (s / n ch)
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

(defun zp-str-smart-less (a b / na nb)
  (setq na (zp-leading-number a) nb (zp-leading-number b))
  (if (and na nb)
    (if (= na nb) (< (strcase a) (strcase b)) (< na nb))
    (< (strcase a) (strcase b))))

;; ---------- ИМЯ ЭЛЕМЕНТА (ТИП) ----------
(defun zapolnenie-element-name (obj / effname vis)
  (setq effname (su-get-effective-name obj))
  (setq vis (su-get-visibility obj))
  (if (and vis (/= vis ""))
    vis
    (if effname effname "Без имени")))

;; ---------- ПОИСК СВОЙСТВА (точное совпадение -> подстрока) ----------
(defun zapolnenie-find-property (obj keyword / dynprops prop pname value result)
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

(defun zapolnenie-get-dimension (obj keywords-priority / result kw)
  (setq result nil)
  (foreach kw keywords-priority
    (if (null result)
      (setq result (zapolnenie-find-property obj kw))))
  result)

;; ---------- РАЗМЕРЫ С ПРИПУСКОМ ----------
(defun zapolnenie-compute-dims (obj / raw-h raw-w h w allow)
  (setq raw-h (zapolnenie-get-dimension obj *ZAPOLNENIE-HEIGHT-KEYWORDS*))
  (setq raw-w (zapolnenie-get-dimension obj *ZAPOLNENIE-WIDTH-KEYWORDS*))

  (cond
    ((or (null raw-h) (<= raw-h 0.0))
     (setq *zapolnenie-skipped-no-height*
           (1+ *zapolnenie-skipped-no-height*))
     nil)

    ((or (null raw-w) (<= raw-w 0.0))
     (setq *zapolnenie-skipped-no-width*
           (1+ *zapolnenie-skipped-no-width*))
     nil)

    (T
     (setq allow (zapolnenie-frame-allowance))
     (setq h (fix (+ raw-h allow)))
     (setq w (fix (+ raw-w allow)))
     (list h w))))

(defun zapolnenie-get-attr (obj tag / attrs a tagname value result)
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

(defun zapolnenie-mark-line (obj / vit mar)
  (setq vit (zapolnenie-get-attr obj "ВИТРАЖ")
        mar (zapolnenie-get-attr obj "МАРКА"))
  (cond
    ((and vit mar) (strcat "Витраж " mar))
    (vit "Витраж")
    (mar mar)
    (T nil)))

(defun zapolnenie-block-diag (i obj err / nm)
  (setq nm (if obj (vl-catch-all-apply 'vla-get-EffectiveName (list obj)) "?"))
  (if (vl-catch-all-error-p nm) (setq nm "?"))
  (if (/= (type nm) 'STR) (setq nm (vl-princ-to-string nm)))
  (princ (strcat "\n[ZAPOLNENIE] Блок #" (itoa (1+ i)) " (" nm "): "
                 (vl-catch-all-error-message err)
                 " - БЛОК ПРОПУЩЕН.")))

;; ---------- СОРТИРОВКА DETAIL ----------
(defun zapolnenie-sort-less (a b / ta tb ma mb ha hb wa wb)
  (setq ta (if (= (type (cadr a)) 'STR) (strcase (cadr a)) "")
        tb (if (= (type (cadr b)) 'STR) (strcase (cadr b)) "")
        ma (if (= (type (nth 5 a)) 'STR) (strcase (nth 5 a)) "")
        mb (if (= (type (nth 5 b)) 'STR) (strcase (nth 5 b)) "")
        ha (if (numberp (caddr a)) (caddr a) 0)
        hb (if (numberp (caddr b)) (caddr b) 0)
        wa (if (numberp (cadddr a)) (cadddr a) 0)
        wb (if (numberp (cadddr b)) (cadddr b) 0))
  (cond
    ((zp-str-smart-less ta tb) T)
    ((zp-str-smart-less tb ta) nil)
    ((zp-str-smart-less ma mb) T)
    ((zp-str-smart-less mb ma) nil)
    ((< ha hb) T)
    ((> ha hb) nil)
    ((< wa wb) T)
    ((> wa wb) nil)
    (T nil)))

;; ---------- АГРЕГАЦИЯ ----------
;; Внутренняя структура: (key name h w count)
;; Возвращаемая:         (name h w count)
(defun zapolnenie-aggregate (inserts /
    i ent obj name dims h w mark key found acc display-names r sorted)
  (setq acc '() display-names '() i 0)
  (setq *zapolnenie-skipped-no-height* 0 *zapolnenie-skipped-no-width* 0)
  (setq *zapolnenie-has-marks* nil)
  (setq *zapolnenie-errors* 0)

  (repeat (length inserts)
    (setq ent (nth i inserts) obj nil)
    (setq *zapolnenie-progress* i)
    (setq r
      (vl-catch-all-apply
        '(lambda ()
           (setq obj (vlax-ename->vla-object ent))
           (setq name (zapolnenie-element-name obj))
           (if (/= (type name) 'STR)
             (progn
               (princ (strcat "\n[ZAPOLNENIE] Блок #" (itoa (1+ i))
                              ": ИМЯ не строка (тип " (vl-princ-to-string (type name))
                              ", значение " (vl-princ-to-string name)
                              ") - беру \"Без имени\"."))
               (setq name "Без имени")))
           (setq dims (zapolnenie-compute-dims obj))
           (if dims
             (progn
               (setq h (car dims) w (cadr dims))
               (setq mark (zapolnenie-mark-line obj))
               (if (and mark (/= (type mark) 'STR))
                 (progn
                   (princ (strcat "\n[ZAPOLNENIE] Блок #" (itoa (1+ i))
                                  ": МАРКА не строка (тип " (vl-princ-to-string (type mark))
                                  ", значение " (vl-princ-to-string mark)
                                  ") - перевожу в строку."))
                   (setq mark (vl-princ-to-string mark))))
               (if mark (setq *zapolnenie-has-marks* T))
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
        (setq *zapolnenie-errors* (1+ *zapolnenie-errors*))
        (if (<= *zapolnenie-errors* 20)
          (zapolnenie-block-diag i obj r)
          (if (= *zapolnenie-errors* 21)
            (princ "\n[ZAPOLNENIE] Дальнейшие сообщения об ошибках подавлены.")))))
    (setq i (1+ i)))

  (setq sorted (vl-catch-all-apply 'vl-sort (list acc 'zapolnenie-sort-less)))
  (if (vl-catch-all-error-p sorted)
    (progn
      (princ (strcat "\n[ZAPOLNENIE] Ошибка СОРТИРОВКИ: " (vl-catch-all-error-message sorted)
                     " - использую данные без сортировки."))
      (setq *zapolnenie-errors* (1+ *zapolnenie-errors*) sorted acc)))

  (mapcar
    '(lambda (rec)
       (list (cadr rec) (caddr rec) (cadddr rec)
             (car (cddddr rec)) (nth 5 rec)))
    sorted))

;; ---------- SUMMARY ИЗ DETAIL ----------
(defun zapolnenie-build-summary (data /
    summary rec name h w cnt area key found)
  (setq summary '())

  (foreach rec data
    (setq name (car rec)
          h    (cadr rec)
          w    (caddr rec)
          cnt  (cadddr rec)
          area (/ (* h w cnt 1.0) 1000000.0)
          key  (strcase name))
    (setq found (assoc key summary))

    (if found
      (setq summary
        (subst
          (list key (cadr found)
                (+ (caddr found) cnt)
                (+ (cadddr found) area))
          found summary))
      (setq summary
        (cons (list key name cnt area) summary))))

  (setq summary
    (vl-sort summary
      '(lambda (a b) (zp-str-smart-less (car a) (car b)))))

  ;; ред. 29: площади накоплены точно - округляем один раз при выдаче
  (mapcar
    '(lambda (r) (list (cadr r) (caddr r) (zapolnenie-round2 (cadddr r))))
    summary))

;; ---------- КУСКОВАНИЕ: ПЛОСКИЙ СПИСОК ----------
;; Группы по типу, каждая с подитогом.
(defun zp-build-flat-items (data / items groups grp grpName grpRows
                                   rec h w cnt totalCnt totalArea groupIndex)
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
      (setq totalCnt  (+ totalCnt cnt))
      (setq totalArea (+ totalArea (/ (* h w cnt 1.0) 1000000.0))))

    (setq items
      (append items
        (list (list 'subtotal groupIndex grpName totalCnt totalArea)))))

  items)

(defun zp-build-units (data idealRows / items chunks ch result)
  (setq items (zp-build-flat-items data))
  (setq chunks (tc-partition-flat items idealRows))
  (setq result '())
  (foreach ch chunks
    (setq result (append result (list (cons (length ch) ch)))))
  result)

;; ---------- ТАБЛИЦА DETAIL (кускованная) ----------
(defun zapolnenie-create-table-detail (data /
    pt pt_wcs units total-chunks chunk-idx is-last chunk items item
    nCols nRows space tbl row oldEcho doc
    lastGroupIdx rowInGroup maxNameLen nameStr maxMarkLen markStr
    tip h w cnt mark area total-cnt total-area
    grpName grpCnt grpArea
    maxNumLen zpGroups grpEntry grpIdx numStr col0Width col1Width
    tms0 tms1)

  (if (null data)
    (progn (princ "\nНет данных для таблицы Заполнения.") nil)
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
            (setq total-area (+ total-area (/ (* (cadr item) (caddr item) (cadddr item) 1.0) 1000000.0))))

          (setq units (zp-build-units data *TU-IDEAL-ROWS*))
          (setq total-chunks (length units))
          ;; Колонка Марка - только если в выборке реально есть Витраж/Марка
          (setq nCols (if *zapolnenie-has-marks* 7 6))
          (setq chunk-idx 0)

          (setq lastGroupIdx -1 rowInGroup 0)

          (foreach chunk units
            (setq is-last (tu-is-last-chunk chunk-idx total-chunks))
            (setq nRows (+ 2 (car chunk)))
            (if is-last (setq nRows (1+ nRows)))
            (setq items (cdr chunk))
            (setq tms0 (getvar "MILLISECS"))
            (setq tbl (vl-catch-all-apply 'vla-addtable
              (list space (vlax-3d-point pt_wcs) nRows nCols 10.0 50.0)))

            (if (vl-catch-all-error-p tbl)
              (princ (strcat "\nОшибка создания таблицы Заполнения: "
                             (vl-catch-all-error-message tbl)))
              (progn
                ;; ред. 5: подавление пересборки на все заполнение
                (vl-catch-all-apply 'vla-put-RegenerateTableSuppressed
                  (list tbl :vlax-true))
                (vla-SetColumnWidth tbl 0 col0Width)
                (vla-SetColumnWidth tbl 1 col1Width)
                (if *zapolnenie-has-marks*
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

                (ts-ac-title tbl 0 "Заполнение" nCols)

                (if *zapolnenie-has-marks*
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
                        (zapolnenie-round2 (/ (* h w cnt) 1000000.0)))

                      (if (/= groupIdx lastGroupIdx)
                        (progn
                          (setq lastGroupIdx groupIdx)
                          (setq rowInGroup 0)))
                      (setq rowInGroup (1+ rowInGroup))

                      (vla-SetText tbl row 0
                        (strcat (itoa groupIdx) "." (itoa rowInGroup)))
                      (vla-SetText tbl row 1 tip)
                      (if *zapolnenie-has-marks*
                        (progn
                          (vla-SetText tbl row 2 (if mark mark ""))
                          (vla-SetText tbl row 3 (itoa h))
                          (vla-SetText tbl row 4 (itoa w))
                          (vla-SetText tbl row 5 (itoa cnt))
                          (vla-SetText tbl row 6 (zapolnenie-format-area area)))
                        (progn
                          (vla-SetText tbl row 2 (itoa h))
                          (vla-SetText tbl row 3 (itoa w))
                          (vla-SetText tbl row 4 (itoa cnt))
                          (vla-SetText tbl row 5 (zapolnenie-format-area area))))

                      (vla-SetCellAlignment tbl row 0 5)
                      (vla-SetCellAlignment tbl row 1 4)
                      (if *zapolnenie-has-marks*
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
                        (strcat "   {\\L" grpName "}") 1 (if *zapolnenie-has-marks* 3 3))
                      (if *zapolnenie-has-marks*
                        (progn
                          (vla-SetText tbl row 5 (itoa grpCnt))
                          (vla-SetText tbl row 6
                            (zapolnenie-format-area grpArea))
                          (vla-SetCellAlignment tbl row 5 5)
                          (vla-SetCellAlignment tbl row 6 5))
                        (progn
                          (vla-SetText tbl row 4 (itoa grpCnt))
                          (vla-SetText tbl row 5
                            (zapolnenie-format-area grpArea))
                          (vla-SetCellAlignment tbl row 4 5)
                          (vla-SetCellAlignment tbl row 5 5)))

                      (setq row (1+ row)))))

                (if is-last
                  (progn
                    (ts-ac-total tbl row
                      "           {\\LИтого по всем позициям:}" 0 3 (if *zapolnenie-has-marks* 5 4))
                    (if *zapolnenie-has-marks*
                      (progn
                        (vla-SetText tbl row 5 (itoa total-cnt))
                        (vla-SetText tbl row 6
                          (zapolnenie-format-area total-area))
                        (vla-SetCellAlignment tbl row 5 5)
                        (vla-SetCellAlignment tbl row 6 5))
                      (progn
                        (vla-SetText tbl row 4 (itoa total-cnt))
                        (vla-SetText tbl row 5
                          (zapolnenie-format-area total-area))
                        (vla-SetCellAlignment tbl row 4 5)
                        (vla-SetCellAlignment tbl row 5 5)))))

                ;; ред. 5: снятие подавления = единственная полная сборка
                ;; (vla-Update после этого дал бы вторую сборку - убран)
                (vl-catch-all-apply 'vla-put-RegenerateTableSuppressed
                  (list tbl :vlax-false))
                (setq tms1 (getvar "MILLISECS"))
                (princ (strcat "\nТаблица Заполнения "
                               (itoa (1+ chunk-idx)) " создана за "
                               (rtos (/ (- tms1 tms0) 1000.0) 2 2) " с."))
                (setq pt_wcs
                  (tu-next-table-point pt_wcs nRows 10.0 20.0))))

            (setq chunk-idx (1+ chunk-idx)))

          (vla-endundomark doc)
          (vl-catch-all-apply 'setvar (list "CMDECHO" oldEcho))
          (princ (strcat "\nВсего создано таблиц Заполнения: "
                         (itoa total-chunks)))
          T)))))

;; ---------- ТАБЛИЦА SUMMARY ----------
(defun zapolnenie-create-table-summary (data /
    pt tbl row nRows nCols space rec tip cnt area total-cnt total-area
    tms0 tms1)

  (setq pt (getpoint "\nУкажите точку вставки таблицы: "))

  (if pt
    (progn
      (setvar "CMDECHO" 0)
      (setq nCols 4)
      (setq nRows (+ 3 (length data)))
      (setq space
        (vla-get-modelspace
          (vla-get-activedocument (vlax-get-acad-object))))
      (setq tms0 (getvar "MILLISECS"))
      (setq tbl (vl-catch-all-apply 'vla-addtable
        (list space (vlax-3d-point pt) nRows nCols 10.0 50.0)))
      (vl-catch-all-apply 'vla-put-RegenerateTableSuppressed
        (list tbl :vlax-true))

      (vla-SetColumnWidth tbl 0 15.0)
      (vla-SetColumnWidth tbl 1 75.0)
      (vla-SetColumnWidth tbl 2 30.0)
      (vla-SetColumnWidth tbl 3 35.0)

      (ts-ac-title tbl 0 "Заполнение" 4)

      (ts-ac-header tbl 1 '("№" "Тип" "Кол-во, шт." "Площадь, м2"))

      (setq row 2 total-cnt 0 total-area 0.0)

      (foreach rec data
        (setq tip  (car rec))
        (setq cnt  (cadr rec))
        (setq area (caddr rec))

        (if (null cnt)  (setq cnt 0))
        (if (null area) (setq area 0.0))

        (setq total-cnt  (+ total-cnt cnt))
        (setq total-area (+ total-area area))

        (vla-SetText tbl row 0 (itoa (1+ (- row 2))))
        (vla-SetText tbl row 1 tip)
        (vla-SetText tbl row 2 (itoa cnt))
        (vla-SetText tbl row 3 (zapolnenie-format-area area))

        (vla-SetCellAlignment tbl row 0 5)
        (vla-SetCellAlignment tbl row 1 4)
        (vla-SetCellAlignment tbl row 2 5)
        (vla-SetCellAlignment tbl row 3 5)

        (setq row (1+ row)))

      (ts-ac-total tbl row "{   \\LИтого по всем позициям:}" 0 1 5)
      (vla-SetText tbl row 2 (itoa total-cnt))
      (vla-SetText tbl row 3 (zapolnenie-format-area total-area))

      (vla-SetCellAlignment tbl row 0 5)
      (vla-SetCellAlignment tbl row 2 5)
      (vla-SetCellAlignment tbl row 3 5)

      (vl-catch-all-apply 'vla-put-RegenerateTableSuppressed
        (list tbl :vlax-false))
      (setq tms1 (getvar "MILLISECS"))
      (princ (strcat "\nТаблица Заполнения (краткая) создана за "
                     (rtos (/ (- tms1 tms0) 1000.0) 2 2) " с."))
      (setvar "CMDECHO" 1)
      tbl)))

(defun zapolnenie-export-xls-detail (data xlsfile / f rec tip h w cnt mark area
                                     total-cnt total-area
                                     groups grp grpName grpRows grpCnt grpArea
                                     startRow endRow itemNum
                                     subtotal-rows formula-cnt formula-area r
                                     cCnt cArea cMarkMerge groupRanges gr grS grE)
  (setq cCnt  (if *zapolnenie-has-marks* 6 5)
        cArea (if *zapolnenie-has-marks* 7 6)
        cMarkMerge (if *zapolnenie-has-marks* 4 3))
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

      (write-line " <Worksheet ss:Name=\"ZapolnenieDetail\">" f)
      (write-line "  <Table>" f)
      (write-line "   <Column ss:Width=\"30\"/>" f)
      (write-line "   <Column ss:Width=\"125\"/>" f)
      (if *zapolnenie-has-marks*
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
                          (itoa (1- (if *zapolnenie-has-marks* 7 6)))
                          "\"><Data ss:Type=\"String\">Заполнение</Data></Cell>") f)
      (write-line "   </Row>" f)

      (write-line "   <Row>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">№</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Тип</Data></Cell>" f)
      (if *zapolnenie-has-marks*
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
            subtotal-rows '() groupRanges '())

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
                grpArea (+ grpArea (/ (* h w cnt 1.0) 1000000.0))
                total-cnt (+ total-cnt cnt)
                total-area (+ total-area (/ (* h w cnt 1.0) 1000000.0)))

          (write-line "   <Row>" f)
          (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (itoa itemNum) "</Data></Cell>") f)
          (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"String\">" (eu-xml-escape grpName) "</Data></Cell>") f)
          (if *zapolnenie-has-marks*
            (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"String\">" (if mark (eu-xml-escape mark) "") "</Data></Cell>") f))
          (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (itoa h) "</Data></Cell>") f)
          (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (itoa w) "</Data></Cell>") f)
          (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (itoa cnt) "</Data></Cell>") f)
          ;; ред. 6: в ячейке строки ТОЧНАЯ площадь (формула без ROUND),
          ;; стиль Num отображает 2 знака; итоги - по этой же колонке
          (write-line (strcat "    <Cell ss:StyleID=\"Num\" ss:Formula=\"=RC[-3]*RC[-2]*RC[-1]/1000000\"><Data ss:Type=\"Number\">" (rtos (/ (* h w cnt 1.0) 1000000.0) 2 6) "</Data></Cell>") f)
          (write-line "   </Row>" f)

          (setq rowNum (1+ rowNum)))

        (setq endRow (1- rowNum))
        ;; ред. 7: диапазон ТОЛЬКО строк данных группы - для итого
        (setq groupRanges (append groupRanges (list (list startRow endRow))))

        (write-line "   <Row>" f)
        (write-line "    <Cell ss:StyleID=\"BoldUnderline\"><Data ss:Type=\"String\"></Data></Cell>" f)
        (write-line (strcat "    <Cell ss:StyleID=\"BoldUnderline\"><Data ss:Type=\"String\">" (eu-xml-escape grpName) "</Data></Cell>") f)
        (if *zapolnenie-has-marks*
          (write-line "    <Cell ss:StyleID=\"BoldUnderline\"><Data ss:Type=\"String\"></Data></Cell>" f))
        (write-line "    <Cell ss:StyleID=\"BoldUnderline\"><Data ss:Type=\"String\"></Data></Cell>" f)
        (write-line "    <Cell ss:StyleID=\"BoldUnderline\"><Data ss:Type=\"String\"></Data></Cell>" f)
        (write-line (strcat "    <Cell ss:StyleID=\"BoldUnderline\" ss:Formula=\"=SUM(R" (itoa startRow) "C" (itoa cCnt) ":R" (itoa endRow) "C" (itoa cCnt) ")\"><Data ss:Type=\"Number\">" (itoa grpCnt) "</Data></Cell>") f)
        (write-line (strcat "    <Cell ss:StyleID=\"BoldUnderlineNum\" ss:Formula=\"=ROUND(SUM(R" (itoa startRow) "C" (itoa cArea) ":R" (itoa endRow) "C" (itoa cArea) "),2)\"><Data ss:Type=\"Number\">" (rtos grpArea 2 2) "</Data></Cell>") f)
        (write-line "   </Row>" f)
        (setq rowNum (1+ rowNum))
        (setq subtotal-rows (append subtotal-rows (list (1- rowNum))))
      )

      (setq formula-cnt "")
      (foreach r subtotal-rows
        (if (= formula-cnt "")
          (setq formula-cnt (strcat "=R" (itoa r) "C" (itoa cCnt)))
          (setq formula-cnt (strcat formula-cnt "+R" (itoa r) "C" (itoa cCnt)))))
      ;; ред. 7: итого - округление ТОЧНОЙ суммы строк данных по всем
      ;; группам (диапазоны данных, БЕЗ строк подытогов - иначе задвоение)
      (setq formula-area "")
      (foreach gr groupRanges
        (setq grS (car gr) grE (cadr gr))
        (if (= formula-area "")
          (setq formula-area
            (strcat "R" (itoa grS) "C" (itoa cArea) ":R" (itoa grE) "C" (itoa cArea)))
          (setq formula-area
            (strcat formula-area ",R" (itoa grS) "C" (itoa cArea) ":R" (itoa grE) "C" (itoa cArea)))))
      (setq formula-area (strcat "=ROUND(SUM(" formula-area "),2)"))

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

(defun zapolnenie-export-csv-detail (data csvfile / f rec tip h w cnt mark area
                                    total-cnt total-area
                                    groups grp grpName grpRows grpCnt grpArea itemNum)
  (setq f (open csvfile "w"))
  (if f
    (progn
      (if *zapolnenie-has-marks*
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
                grpArea (+ grpArea (/ (* h w cnt 1.0) 1000000.0))
                total-cnt (+ total-cnt cnt)
                total-area (+ total-area (/ (* h w cnt 1.0) 1000000.0)))
          (if *zapolnenie-has-marks*
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

        (if *zapolnenie-has-marks*
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

      (if *zapolnenie-has-marks*
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

;; ---------- ОСНОВНАЯ ФУНКЦИЯ ----------
(defun zapolnenie-main (layers report-mode export-excel export-txt
                        create-table save-base
                        / *error* svSaved inserts data summary-data
                          base-name xlsfile csvfile
                          total-count total-area skipped-total rec)

  (vl-load-com)
  (setq *AE-SETTINGS-TABLE-TASK* 'ZAPOLNENIE)

  (sssetfirst nil nil)

  (defun *error* (msg)
    (if (and msg
             (not (wcmatch (strcase msg)
                    "*BREAK*,*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\nОшибка: " msg)))
    (tu-sysvar-restore svSaved)
    (princ))

  ;; V8: guard - обрыв вернет CMDECHO
  (setq svSaved (tu-sysvar-save '("CMDECHO")))
  (if (and (null layers) (= (type ae-settings-task-layers) 'SUBR))
    (setq layers (ae-settings-task-layers 'ZAPOLNENIE)))

  (setq inserts (su-select-inserts layers))
  (if (= (type ae-settings-task-blocks) 'SUBR)
    (setq inserts
      (su-filter-inserts-by-name-masks
        inserts (ae-settings-task-blocks 'ZAPOLNENIE)))
  )

  (if inserts
    (progn
      (setq data (zapolnenie-aggregate inserts))
      (setq *zapolnenie-progress* -1)
      (if (> *zapolnenie-errors* 0)
        (princ (strcat "\n[ZAPOLNENIE] Итого блоков пропущено из-за ошибок: "
                       (itoa *zapolnenie-errors*) " - подробности выше.")))
      (princ (if *zapolnenie-has-marks*
        "\nКолонка МАРКА: ДА (найдены атрибуты Витраж/Марка)"
        "\nКолонка МАРКА: НЕТ (атрибуты не найдены - формат как раньше)"))

      (if data
        (progn
          (if (null save-base)
            (setq base-name
              (strcat (getvar "dwgprefix")
                      (vl-filename-base (getvar "dwgname"))
                      " Заполнение "
                      (if (= (strcase report-mode) "DETAIL")
                        "подробный" "краткий")))
            (setq base-name
              (strcat save-base " "
                      (if (= (strcase report-mode) "DETAIL")
                        "подробный" "краткий"))))

          (setq summary-data (zapolnenie-build-summary data))

          ;; --- Экспорт XLS / CSV ---
          (if export-excel
            (progn
              (setq xlsfile (strcat base-name ".xls"))
              (if (= (strcase report-mode) "DETAIL")
                (if (zapolnenie-export-xls-detail data xlsfile)
                  (princ (strcat "\nXLS сохранен: " xlsfile))
                  (progn
                    (princ "\nНе удалось сохранить XLS. Сохраняю CSV...")
                    (setq csvfile (strcat base-name ".csv"))
                    (if (zapolnenie-export-csv-detail data csvfile)
                      (princ (strcat "\nCSV сохранен: " csvfile))
                      (princ "\nНе удалось создать CSV."))))
                (if (eu-export-zapolnenie-summary summary-data xlsfile)
                  (princ (strcat "\nXLS сохранен: " xlsfile))
                  (progn
                    (princ "\nНе удалось сохранить XLS. Сохраняю CSV...")
                    (setq csvfile (strcat base-name ".csv"))
                    (if (eu-export-zapolnenie-csv-summary summary-data csvfile)
                      (princ (strcat "\nCSV сохранен: " csvfile))
                      (princ "\nНе удалось создать CSV.")))))))

          ;; --- Экспорт GAL ---
          (if export-txt
            (tx-export-gal-zapolnenie data base-name
              *ZAPOLNENIE-SHEET-SIZE* *ZAPOLNENIE-ROTATE*))

          ;; --- Таблица AutoCAD ---
          (if create-table
            (if (= (strcase report-mode) "DETAIL")
              (zapolnenie-create-table-detail data)
              (zapolnenie-create-table-summary summary-data)))

          ;; --- Итоговое сообщение ---
          (setq total-count 0 total-area 0.0)

          (foreach rec data
            (setq total-count (+ total-count (cadddr rec)))
            (setq total-area
              (+ total-area
                 (/ (* (cadr rec) (caddr rec) (cadddr rec) 1.0) 1000000.0))))

          (setq skipped-total
            (+ *zapolnenie-skipped-no-height*
               *zapolnenie-skipped-no-width*))

          (if (> skipped-total 0)
            (progn
              (princ
                (strcat "\nЗаполнение: обработано " (itoa total-count)
                        " блоков, общая площадь "
                        (zapolnenie-format-area total-area)
                        " м2, пропущено " (itoa skipped-total) "."))
              (princ
                (strcat "\n  Без высоты: "
                        (itoa *zapolnenie-skipped-no-height*)
                        ", без ширины: "
                        (itoa *zapolnenie-skipped-no-width*))))
            (princ
              (strcat "\nЗаполнение: обработано " (itoa total-count)
                      " блоков, общая площадь "
                      (zapolnenie-format-area total-area) " м2"))))

        (princ "\nНет данных для отчета.")))

    (princ "\nБлоки заполнения не найдены."))

  (princ))

;; ---------- АВТОНОМНАЯ КОМАНДА ----------
(defun c:zapolnenie ( / layers-str layers report-mode
                        export-excel export-txt create-table
                        use-default save-base)

  (if (not (and (type 'su-select-inserts)
                (type 'tc-partition-flat)
                (type 'ts-ac-header)
                (type 'eu-xml-styles)
                (type 'tx-export-gal-zapolnenie)))
    (progn
      (princ "\nНе загружены зависимости. Запустите EXTRACTION или RELOAD.")
      (princ)
      (exit)))

  (setq layers-str
    (getstring T "\nВведите слои через запятую (Enter — все слои): "))

  (if (= layers-str "")
    (setq layers nil)
    (setq layers
      (mapcar
        '(lambda (x) (strcase (vl-string-trim " " x)))
        (split-string layers-str ","))))

  (initget "D S")
  (setq report-mode
    (getkword "\nРежим отчета [Подробный(D)/Краткий(S)] <D>: "))
  (if (null report-mode) (setq report-mode "D"))
  (setq report-mode (if (= report-mode "D") "DETAIL" "SUMMARY"))

  (initget "Y N")
  (setq export-excel
    (getkword "\nЭкспорт в Excel? [Да(Y)/Нет(N)] <N>: "))
  (if (or (null export-excel) (= export-excel "N"))
    (setq export-excel nil)
    (setq export-excel T))

  (initget "Y N")
  (setq export-txt
    (getkword "\nЭкспорт в TXT (GAL)? [Да(Y)/Нет(N)] <N>: "))
  (if (or (null export-txt) (= export-txt "N"))
    (setq export-txt nil)
    (setq export-txt T))

  (initget "Y N")
  (setq create-table
    (getkword "\nСоздать таблицу AutoCAD? [Да(Y)/Нет(N)] <Y>: "))
  (if (or (null create-table) (= create-table "Y"))
    (setq create-table T)
    (setq create-table nil))

  (initget "Y N")
  (setq use-default
    (getkword "\nИспользовать путь по умолчанию? [Да(Y)/Нет(N)] <Y>: "))
  (if (or (null use-default) (= use-default "Y"))
    (setq save-base nil)
    (setq save-base
      (getstring T "\nБазовое имя файла (без расширения): ")))

  (zapolnenie-main layers report-mode
    export-excel export-txt create-table save-base)
  (princ))

(defun c:ЗАПОЛНЕНИЕ () (c:zapolnenie))

(princ "\nZAPOLNENIE.LSP загружен (ред. 30: припуск на раму из настроек; колонка Марка, точные суммы, быстрые таблицы). Команды: ZAPOLNENIE, ЗАПОЛНЕНИЕ")
(princ)