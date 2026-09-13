;;; ============================================================
;;; CLADDING.LSP — Облицовка, часть 1: сбор полилиний
;;; (керамогранит, кассеты и т.п., изображённые полилиниями)
;;;
;;; Часть 2 (динамические блоки облицовки) — заглушка,
;;; отдельное под-ТЗ позже. Точка слияния: cl-collect-blocks.
;;;
;;; ПРАВИЛА (ТЗ редакция 3):
;;;   - площадь: ОСНОВНОЙ источник vla-get-Area; fallback —
;;;     cl-green-area (теорема Грина) при отказе COM;
;;;   - сверка методов по флагу *CLADDING-CHECK-AREA*;
;;;   - незамкнутые — исключить + предупреждение;
;;;   - без площади — исключить + предупреждение;
;;;   - со скруглениями (дугами) — включить + информ. строка;
;;;   - округление элемента: 3 знака м2 (внутренне);
;;;     итоги и отображение: 2 знака (сотые);
;;;   - агрегация: SUMMARY по слоям, DETAIL слой + номинал;
;;;     повёрнутые и непрямоугольные -> группа _НЕПРЯМОУГ_
;;;     (критерий: |S - dx*dy| > max(1 мм2, 1% dx*dy) по AABB).
;;;
;;; К2: таблицы AutoCAD SUMMARY и DETAIL; глобалки последнего
;;; прогона *CLADDING-LAST-RECORDS/DATA/MODE* (в т.ч. для
;;; будущей задачи "Раскрой листа").
;;; К3: экспорт XLS (SpreadsheetML, windows-1251) и CSV
;;; (разделитель ";", площадь строкой с запятой); fallback
;;; XLS -> CSV; имя файла save-base или "<dwg> Облицовка
;;; подробный|краткий".
;;;
;;; РЕДАКЦИЯ 9: оформление титула с серой заливкой и контуром
;;; по периметру; выравнивание итоговой строки DETAIL
;;; (убрана пустая ячейка между слиянием и количеством).
;;; РЕДАКЦИЯ 8: баланс скобок в cl-write-xls; № — сквозной.
;;; РЕДАКЦИЯ 6: vla-MergeCells в порядке (Row1 Row2 Col1 Col2);
;;; *error* распознаёт *ПРЕРВА*.
;;; ============================================================

(vl-load-com)

;; Флаг сверки площади двумя методами
(if (not (boundp '*CLADDING-CHECK-AREA*))
  (setq *CLADDING-CHECK-AREA* nil)
)

;; Счётчики (сбрасываются в cl-collect)
(setq *cladding-skipped-open*   0)
(setq *cladding-skipped-zero*   0)
(setq *cladding-with-arcs*      0)
(setq *cladding-check-mismatch* 0)

;; ============================================================
;; ОКРУГЛЕНИЯ И ФОРМАТ
;; ============================================================

;; Внутреннее округление элемента: 3 знака (тысячные м2)
(defun cl-round3 (x)
  (/ (fix (+ (* x 1000.0) 0.5)) 1000.0)
)

;; Округление итогов: 2 знака (сотые)
(defun cl-round2 (x)
  (/ (fix (+ (* x 100.0) 0.5)) 100.0)
)

;; Отображение площади: 2 знака с подавлением лишних нулей
(defun cl-format-area (area / int-part frac-hundredths)
  (setq int-part (fix area))
  (setq frac-hundredths (fix (+ (* (- area int-part) 100.0) 0.5)))
  (cond
    ((= frac-hundredths 0)
     (itoa int-part)
    )
    ((= (rem frac-hundredths 10) 0)
     (strcat (itoa int-part) "," (itoa (/ frac-hundredths 10)))
    )
    (T
     (if (< frac-hundredths 10)
       (strcat (itoa int-part) ",0" (itoa frac-hundredths))
       (strcat (itoa int-part) "," (itoa frac-hundredths))
     )
    )
  )
)

;; ============================================================
;; СОРТИРОВКА И СЕРВИС
;; ============================================================

;; Извлечение ведущего числа из строки
(defun cl-leading-number (s / n ch)
  (if (/= (type s) 'STR)
    nil
    (progn
      (setq s (vl-string-trim " \t" s)
            n "")
      (while (and (> (strlen s) 0)
                  (setq ch (substr s 1 1))
                  (>= (ascii ch) 48)
                  (<= (ascii ch) 57))
        (setq n (strcat n ch))
        (setq s (substr s 2))
      )
      (if (= n "") nil (atoi n))
    )
  )
)

;; Строковое сравнение с числовым приоритетом
(defun cl-str-smart-less (a b / na nb)
  (setq na (cl-leading-number a)
        nb (cl-leading-number b))
  (if (and na nb)
    (if (= na nb)
      (< (strcase a) (strcase b))
      (< na nb)
    )
    (< (strcase a) (strcase b))
  )
)

;; Локальный разбор строки слоёв
(defun cl-split-string (str delim / pos result item)
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

;; ============================================================
;; ГЕОМЕТРИЯ ПОЛИЛИНИИ
;; ============================================================

;; Вершины с прогибами: список ((x y z) . bulge)
;; DXF 42 заменяет булж последней вершины, точка = (caar pts)
(defun cl-poly-vertices (ent / pts)
  (setq pts '())
  (foreach g (entget ent)
    (cond
      ((= (car g) 10)
       (setq pts (cons (cons (cdr g) 0.0) pts))
      )
      ((= (car g) 42)
       (if pts
         (setq pts (cons (cons (caar pts) (cdr g)) (cdr pts)))
       )
      )
    )
  )
  (reverse pts)
)

;; Замкнутость: DXF 70, бит 1
(defun cl-poly-closed-p (ent / f)
  (setq f (cdr (assoc 70 (entget ent))))
  (if f (= 1 (logand 1 f)) nil)
)

;; Есть ли дуговые сегменты (|bulge| > 0)
(defun cl-poly-has-arcs (vb)
  (vl-some '(lambda (v) (> (abs (cdr v)) 1e-8)) vb)
)

;; ============================================================
;; FALLBACK-ПЛОЩАДЬ: теорема Грина (линии + дуги)
;; Сегмент с ненулевым bulge — это ДУГА: вклад хорды для него
;; НЕ добавляется, граница контура проходит по дуге
;; ============================================================
(defun cl-green-area (ent / vb n i s v1 v2 p1 p2 b c r u nx ny
                            mx my d cx cy a1 a2 dl)
  (setq vb (cl-poly-vertices ent))
  (setq n (length vb))
  (if (< n 3)
    0.0
    (progn
      (setq s 0.0 i 0)
      (while (< i n)
        (setq v1 (nth i vb)
              v2 (nth (rem (1+ i) n) vb))
        (setq p1 (car v1) p2 (car v2) b (cdr v1))
        (if (> (abs b) 1e-8)
          ;; Сегмент — ДУГА: только интеграл по дуге
          (progn
            (setq c (distance p1 p2))
            (setq r (/ (* c (+ 1.0 (* b b))) (* 4.0 (abs b))))
            (setq u (list (/ (- (car p2) (car p1)) c)
                          (/ (- (cadr p2) (cadr p1)) c)))
            (setq nx (- (cadr u)) ny (car u))
            (setq mx (/ (+ (car p1) (car p2)) 2.0)
                  my (/ (+ (cadr p1) (cadr p2)) 2.0))
            (setq d (/ (* c (- 1.0 (* b b))) (* 4.0 b)))
            (setq cx (+ mx (* nx d))
                  cy (+ my (* ny d)))
            (setq a1 (atan (- (cadr p1) cy) (- (car p1) cx)))
            (setq a2 (atan (- (cadr p2) cy) (- (car p2) cx)))
            (setq dl (- a2 a1))
            (if (> b 0)
              (while (<= dl 0.0) (setq dl (+ dl (* 2.0 pi))))
              (while (>= dl 0.0) (setq dl (- dl (* 2.0 pi))))
            )
            (setq s (+ s (* 0.5 (+ (* r r dl)
                                   (* cx r (- (sin a2) (sin a1)))
                                   (* cy r (- (cos a1) (cos a2)))))))
          )
          ;; Сегмент — прямая: вклад хорды
          (setq s (+ s (/ (- (* (car p1) (cadr p2))
                             (* (car p2) (cadr p1)))
                          2.0)))
        )
        (setq i (1+ i))
      )
      (abs s)
    )
  )
)

;; ============================================================
;; ПЛОЩАДЬ: основной путь — AutoCAD, fallback — Грин
;; ============================================================
(defun cl-poly-area (ent / obj a)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
  (setq a
    (if (vl-catch-all-error-p obj)
      nil
      (vl-catch-all-apply 'vla-get-Area (list obj))
    )
  )
  (cond
    ((and a (not (vl-catch-all-error-p a)) (numberp a) (> a 0.0))
     a
    )
    (T
     (cl-green-area ent)
    )
  )
)

;; Сверка двух методов (по флагу): расхождение > 1 мм2 -> счётчик
(defun cl-check-area (ent a / g)
  (if *CLADDING-CHECK-AREA*
    (progn
      (setq g (cl-green-area ent))
      (if (> (abs (- a g)) 1.0)
        (setq *cladding-check-mismatch* (1+ *cladding-check-mismatch*))
      )
    )
  )
)

;; ============================================================
;; ЗАПИСЬ ОБ ЭЛЕМЕНТЕ
;; Возвращает: (layer nominal area_m2) или nil (исключён)
;; ============================================================
(defun cl-poly-record (ent / vb layer area xs ys dx dy nominal
                             minx maxx miny maxy)
  (setq layer (cdr (assoc 8 (entget ent))))

  ;; Незамкнутая — исключить + предупреждение
  (if (not (cl-poly-closed-p ent))
    (progn
      (setq *cladding-skipped-open* (1+ *cladding-skipped-open*))
      nil
    )
    (progn
      (setq vb (cl-poly-vertices ent))
      (if (cl-poly-has-arcs vb)
        (setq *cladding-with-arcs* (1+ *cladding-with-arcs*))
      )
      (setq area (cl-poly-area ent))
      (cl-check-area ent area)
      (if (or (null area) (<= area 0.0))
        (progn
          (setq *cladding-skipped-zero* (1+ *cladding-skipped-zero*))
          nil
        )
        (progn
          ;; Габарит (axis-aligned bounding box) и номинал
          (setq xs (mapcar '(lambda (v) (car (car v))) vb)
                ys (mapcar '(lambda (v) (cadr (car v))) vb))
          (setq minx (apply 'min xs) maxx (apply 'max xs)
                miny (apply 'min ys) maxy (apply 'max ys))
          (setq dx (- maxx minx) dy (- maxy miny))

          ;; Контроль поворота/непрямоугольности:
          ;; сравнение реальной площади с площадью AABB
          (if (> (abs (- area (* dx dy)))
                 (max 1.0 (* 0.01 dx dy)))
            (setq nominal "_НЕПРЯМОУГ_")
            (setq nominal
              (strcat (itoa (fix (+ dx 0.5))) "x"
                      (itoa (fix (+ dy 0.5)))))
          )
          (list layer nominal (cl-round3 (/ area 1e6)))
        )
      )
    )
  )
)

;; ============================================================
;; СБОР ДАННЫХ
;; records: список (layer nominal area_m2)
;; ============================================================
(defun cl-collect (layers / inserts rec records)
  (setq *cladding-skipped-open*    0
        *cladding-skipped-zero*    0
        *cladding-with-arcs*       0
        *cladding-check-mismatch*  0)

  (setq inserts (su-select-lwpolylines layers))
  (setq records '())
  (foreach ent inserts
    (setq rec (cl-poly-record ent))
    (if rec
      (setq records (cons rec records))
    )
  )
  (reverse records)
)

;; ============================================================
;; АГРЕГАЦИЯ
;; key: SUMMARY — слой; DETAIL — слой|номинал
;; Возвращает: список (key layer nominal count area)
;; ============================================================
(defun cl-aggregate (records detail / acc rec key found out)
  (setq acc '())
  (foreach rec records
    (setq key
      (if detail
        (strcat (car rec) "|" (cadr rec))
        (car rec)
      )
    )
    (setq found (assoc key acc))
    (if found
      (setq acc
        (subst
          (list key (car rec) (cadr rec)
                (1+ (nth 3 found))
                (+ (nth 4 found) (caddr rec)))
          found
          acc)
      )
      (setq acc
        (cons
          (list key (car rec) (cadr rec) 1 (caddr rec))
          acc)
      )
    )
  )
  (setq out
    (vl-sort acc
      '(lambda (a b)
         (if (= (cadr a) (cadr b))
           (cl-str-smart-less (caddr a) (caddr b))
           (cl-str-smart-less (cadr a) (cadr b))
         )
       )
    )
  )
  out
)

;; ============================================================
;; КОНСОЛЬНЫЙ ОТЧЁТ
;; ============================================================
(defun cl-report (data report-mode / total-cnt total-area rec cur-layer)
  (setq total-cnt 0 total-area 0.0)
  (setq cur-layer nil)
  (foreach rec data
    (setq total-cnt (+ total-cnt (nth 3 rec)))
    (setq total-area (+ total-area (nth 4 rec)))
    (if (= (strcase report-mode) "DETAIL")
      (progn
        (if (not (equal cur-layer (cadr rec)))
          (progn
            (setq cur-layer (cadr rec))
            (princ (strcat "\n  Слой: " cur-layer))
          )
        )
        (princ (strcat "\n    " (caddr rec)
                       ": " (itoa (nth 3 rec)) " шт, "
                       (cl-format-area (cl-round2 (nth 4 rec))) " м2"))
      )
      (princ (strcat "\n  " (cadr rec)
                     ": " (itoa (nth 3 rec)) " шт, "
                     (cl-format-area (cl-round2 (nth 4 rec))) " м2"))
    )
  )
  (princ (strcat "\nИтого: " (itoa total-cnt) " шт, "
                 (cl-format-area (cl-round2 total-area)) " м2"))
)

;; ============================================================
;; ПРЕДУПРЕЖДЕНИЯ
;; ============================================================
(defun cl-warnings ()
  (if (> *cladding-skipped-open* 0)
    (princ (strcat "\nПредупреждение: исключено незамкнутых полилиний: "
                   (itoa *cladding-skipped-open*)))
  )
  (if (> *cladding-skipped-zero* 0)
    (princ (strcat "\nПредупреждение: исключено полилиний без площади: "
                   (itoa *cladding-skipped-zero*)))
  )
  (if (> *cladding-with-arcs* 0)
    (princ (strcat "\nПредупреждение: полилиний со скруглениями (дугами): "
                   (itoa *cladding-with-arcs*)
                   " - учтены с точным расчётом площади"))
  )
  (if (> *cladding-check-mismatch* 0)
    (princ (strcat "\nВНИМАНИЕ: расхождение площади AutoCAD и контрольного расчёта > 1 мм2: "
                   (itoa *cladding-check-mismatch*) " эл."))
  )
)

;; ============================================================
;; ЧАСТЬ 2 (ЗАГЛУШКА): динамические блоки облицовки
;; ============================================================
(defun cl-collect-blocks (layers)
  nil
)

;; ============================================================
;; ГЛОБАЛКИ ПОСЛЕДНЕГО ПРОГОНА (К2)
;; ============================================================
(if (not (boundp '*CLADDING-LAST-RECORDS*))
  (setq *CLADDING-LAST-RECORDS* nil)
)
(if (not (boundp '*CLADDING-LAST-DATA*))
  (setq *CLADDING-LAST-DATA* nil)
)
(if (not (boundp '*CLADDING-LAST-MODE*))
  (setq *CLADDING-LAST-MODE* nil)
)

;; ============================================================
;; ОСНОВНАЯ ФУНКЦИЯ
;; Сигнатура ожидается диспетчером EXTRACTION
;; ============================================================
(defun cladding-main (layers report-mode export-excel export-txt
                      create-table save-base
                      / *error* records data xls-base xlsfile csvfile)

  (defun *error* (msg)
    (if (and msg
             (not (wcmatch (strcase msg)
                           "*BREAK*,*CANCEL*,*QUIT*,*EXIT*,*ПРЕРВА*")))
      (princ (strcat "\nОшибка: " msg))
    )
    (princ)
  )

  (princ "\n=== Облицовка: сбор полилиний ===")

  (setq records (cl-collect layers))

  (if records
    (progn
      (setq data (cl-aggregate records
                               (= (strcase report-mode) "DETAIL")))

      ;; Сохраняем последние результаты (К2)
      (setq *CLADDING-LAST-RECORDS* records)
      (setq *CLADDING-LAST-DATA* data)
      (setq *CLADDING-LAST-MODE* report-mode)

      (cl-report data report-mode)
      (cl-warnings)

      ;; Экспорт XLS/CSV (К3)
      (if export-excel
        (progn
          (setq xls-base
            (if save-base
              save-base
              (strcat (getvar "DWGPREFIX")
                      (vl-filename-base (getvar "DWGNAME"))
                      " Облицовка "
                      (if (= (strcase report-mode) "DETAIL")
                        "подробный"
                        "краткий")))
          )
          (setq xlsfile (strcat xls-base ".xls"))
          (if (cl-write-xls data report-mode xlsfile)
            (princ (strcat "\nXLS сохранен: " xlsfile))
            (progn
              (princ "\nНе удалось сохранить XLS. Сохраняю CSV...")
              (setq csvfile (strcat xls-base ".csv"))
              (if (cl-write-csv data report-mode csvfile)
                (princ (strcat "\nCSV сохранен: " csvfile))
                (princ "\nНе удалось создать CSV.")
              )
            )
          )
        )
      )
      (if export-txt
        (princ "\nTXT: для облицовки не предусмотрен.")
      )

      ;; Таблица AutoCAD (К2)
      (if create-table
        (if (= (strcase report-mode) "DETAIL")
          (cl-create-table-detail data)
          (cl-create-table-summary data)
        )
      )
    )
    (progn
      (princ "\nДанных по полилиниям нет.")
      (cl-warnings)
    )
  )

  (princ)
)

;; ============================================================
;; АВТОНОМНАЯ КОМАНДА
;; ============================================================
(defun c:cladding ( / layers-str layers report-mode)
  (vl-load-com)
  (setq layers-str
    (getstring T "\nСлои через запятую (Enter — все слои): "))
  (if (= layers-str "")
    (setq layers nil)
    (setq layers
      (mapcar '(lambda (x) (strcase (vl-string-trim " " x)))
              (cl-split-string layers-str ",")))
  )
  (initget "D S")
  (setq report-mode
    (getkword "\nРежим отчёта [Подробный(D)/Краткий(S)] <S>: "))
  (if (null report-mode)
    (setq report-mode "S")
  )
  (setq report-mode (if (= report-mode "D") "DETAIL" "SUMMARY"))
  (cladding-main layers report-mode nil nil nil nil)
  (princ)
)

;; ============================================================
;; ТАБЛИЦА AUTOCAD — SUMMARY (К2)
;; Слияния: (Row1 Row2 Col1 Col2) по факту окружения
;; ============================================================
(defun cl-create-table-summary (data / pt tbl row nRows nCols space
                                    rec total-cnt total-area)
  (setq pt (getpoint "\nУкажите точку вставки таблицы: "))
  (if (null pt)
    (progn (princ "\nТаблица пропущена.") nil)
    (progn
      (setvar "CMDECHO" 0)
      (setq nCols 4)
      (setq nRows (+ 3 (length data)))   ; титул + шапка + строки + итог
      (setq space (vla-get-modelspace
                    (vla-get-activedocument (vlax-get-acad-object))))
      (setq tbl (vla-addtable space (vlax-3d-point pt) nRows nCols 10.0 50.0))

      (vla-SetColumnWidth tbl 0 15.0)
      (vla-SetColumnWidth tbl 1 90.0)
      (vla-SetColumnWidth tbl 2 30.0)
      (vla-SetColumnWidth tbl 3 35.0)

      (vla-MergeCells tbl 0 0 0 3)
      (vla-SetText tbl 0 0 "{\\LОблицовка}")

      (vla-SetText tbl 1 0 "№")
      (vla-SetText tbl 1 1 "Слой")
      (vla-SetText tbl 1 2 "Кол-во, шт.")
      (vla-SetText tbl 1 3 "Площадь, м2")
      (vla-SetCellAlignment tbl 1 0 5)
      (vla-SetCellAlignment tbl 1 1 5)
      (vla-SetCellAlignment tbl 1 2 5)
      (vla-SetCellAlignment tbl 1 3 5)

      (setq row 2 total-cnt 0 total-area 0.0)
      (foreach rec data
        (setq total-cnt (+ total-cnt (nth 3 rec)))
        (setq total-area (+ total-area (nth 4 rec)))
        (vla-SetText tbl row 0 (itoa (1+ (- row 2))))
        (vla-SetText tbl row 1 (cadr rec))
        (vla-SetText tbl row 2 (itoa (nth 3 rec)))
        (vla-SetText tbl row 3 (cl-format-area (cl-round2 (nth 4 rec))))
        (vla-SetCellAlignment tbl row 0 5)
        (vla-SetCellAlignment tbl row 1 4)
        (vla-SetCellAlignment tbl row 2 5)
        (vla-SetCellAlignment tbl row 3 5)
        (setq row (1+ row))
      )

      ;; Итог: слияние (row row 0 1)
      (vla-MergeCells tbl row row 0 1)
      (vla-SetText tbl row 0 "{\\LИтого}")
      (vla-SetText tbl row 2 (itoa total-cnt))
      (vla-SetText tbl row 3 (cl-format-area (cl-round2 total-area)))
      (vla-SetCellAlignment tbl row 0 5)
      (vla-SetCellAlignment tbl row 2 5)
      (vla-SetCellAlignment tbl row 3 5)

      (vla-update tbl)
      (setvar "CMDECHO" 1)
      tbl
    )
  )
)

;; ============================================================
;; ТАБЛИЦА AUTOCAD — DETAIL (К2)
;; Группировка по слоям с подитогом, общий итог внизу
;; ============================================================
(defun cl-create-table-detail (data / pt tbl row nRows nCols space
                                   groups grp rec grpName grpRows
                                   grpCnt grpArea total-cnt total-area
                                   itemNum)
  (setq pt (getpoint "\nУкажите точку вставки таблицы: "))
  (if (null pt)
    (progn (princ "\nТаблица пропущена.") nil)
    (progn
      (setvar "CMDECHO" 0)

      ;; Группировка по слоям с сохранением порядка сортировки
      (setq groups '())
      (foreach rec data
        (setq grp (assoc (cadr rec) groups))
        (if grp
          (setq groups (subst (append grp (list (list rec))) grp groups))
          (setq groups (append groups (list (list (cadr rec) (list rec)))))
        )
      )

      (setq nCols 5)
      ;; титул + шапка + строки + подитоги + общий итог
      (setq nRows (+ 3 (length data) (length groups)))
      (setq space (vla-get-modelspace
                    (vla-get-activedocument (vlax-get-acad-object))))
      (setq tbl (vla-addtable space (vlax-3d-point pt) nRows nCols 10.0 50.0))

      (vla-SetColumnWidth tbl 0 15.0)
      (vla-SetColumnWidth tbl 1 60.0)
      (vla-SetColumnWidth tbl 2 30.0)
      (vla-SetColumnWidth tbl 3 30.0)
      (vla-SetColumnWidth tbl 4 35.0)

      (vla-MergeCells tbl 0 0 0 4)
      (vla-SetText tbl 0 0 "{\\LОблицовка}")

      (vla-SetText tbl 1 0 "№")
      (vla-SetText tbl 1 1 "Слой")
      (vla-SetText tbl 1 2 "Номинал")
      (vla-SetText tbl 1 3 "Кол-во, шт.")
      (vla-SetText tbl 1 4 "Площадь, м2")
      (vla-SetCellAlignment tbl 1 0 5)
      (vla-SetCellAlignment tbl 1 1 5)
      (vla-SetCellAlignment tbl 1 2 5)
      (vla-SetCellAlignment tbl 1 3 5)
      (vla-SetCellAlignment tbl 1 4 5)

      (setq row 2 itemNum 0 total-cnt 0 total-area 0.0)
      (foreach grp groups
        (setq grpName (car grp)
              grpRows (cdr grp)
              grpCnt 0
              grpArea 0.0)

        (foreach rec grpRows
          (setq rec (car rec))
          (setq itemNum (1+ itemNum))
          (setq grpCnt (+ grpCnt (nth 3 rec)))
          (setq grpArea (+ grpArea (nth 4 rec)))
          (setq total-cnt (+ total-cnt (nth 3 rec)))
          (setq total-area (+ total-area (nth 4 rec)))

          (vla-SetText tbl row 0 (itoa itemNum))
          (vla-SetText tbl row 1 grpName)
          (vla-SetText tbl row 2 (caddr rec))
          (vla-SetText tbl row 3 (itoa (nth 3 rec)))
          (vla-SetText tbl row 4 (cl-format-area (cl-round2 (nth 4 rec))))
          (vla-SetCellAlignment tbl row 0 5)
          (vla-SetCellAlignment tbl row 1 4)
          (vla-SetCellAlignment tbl row 2 5)
          (vla-SetCellAlignment tbl row 3 5)
          (vla-SetCellAlignment tbl row 4 5)
          (setq row (1+ row))
        )

        ;; Подитог слоя: слияние (row row 1 2)
        (vla-MergeCells tbl row row 1 2)
        (vla-SetText tbl row 0 "")
        (vla-SetText tbl row 1 (strcat "   {\\L" grpName "}"))
        (vla-SetText tbl row 3 (itoa grpCnt))
        (vla-SetText tbl row 4 (cl-format-area (cl-round2 grpArea)))
        (vla-SetCellAlignment tbl row 0 5)
        (vla-SetCellAlignment tbl row 1 4)
        (vla-SetCellAlignment tbl row 3 5)
        (vla-SetCellAlignment tbl row 4 5)
        (setq row (1+ row))
      )

      ;; Общий итог: слияние (row row 0 2)
      (vla-MergeCells tbl row row 0 2)
      (vla-SetText tbl row 0 "{\\LИтого}")
      (vla-SetText tbl row 3 (itoa total-cnt))
      (vla-SetText tbl row 4 (cl-format-area (cl-round2 total-area)))
      (vla-SetCellAlignment tbl row 0 5)
      (vla-SetCellAlignment tbl row 3 5)
      (vla-SetCellAlignment tbl row 4 5)

      (vla-update tbl)
      (setvar "CMDECHO" 1)
      tbl
    )
  )
)

;; ============================================================
;; ЭКСПОРТ XLS (К3): SpreadsheetML, windows-1251
;; РЕДАКЦИЯ 9: титул с серой заливкой и контуром по периметру;
;; итоговая строка DETAIL выровнена под заголовками
;; ============================================================
(defun cl-write-xls (data report-mode fname / f brd rec total-cnt total-area
                          groups grp grpName grpRows grpCnt grpArea detail
                          itemNum)
  (setq f (open fname "w"))
  (if (null f)
    nil
    (progn
      (setq detail (= (strcase report-mode) "DETAIL"))
      (setq brd
        (strcat
          "<Borders>"
          "<Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>"
          "<Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>"
          "<Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>"
          "<Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>"
          "</Borders>"))

      (write-line "<?xml version=\"1.0\" encoding=\"windows-1251\"?>" f)
      (write-line "<?mso-application progid=\"Excel.Sheet\"?>" f)
      (write-line "<Workbook xmlns=\"urn:schemas-microsoft-com:office:spreadsheet\"" f)
      (write-line " xmlns:o=\"urn:schemas-microsoft-com:office:office\"" f)
      (write-line " xmlns:x=\"urn:schemas-microsoft-com:office:excel\"" f)
      (write-line " xmlns:ss=\"urn:schemas-microsoft-com:office:spreadsheet\">" f)
      (write-line " <Styles>" f)
      (write-line "  <Style ss:ID=\"Default\"><Alignment ss:Vertical=\"Center\"/></Style>" f)
      (write-line
        (strcat "  <Style ss:ID=\"Title\"><Font ss:Bold=\"1\" ss:Size=\"12\" ss:Underline=\"Single\"/>"
                "<Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>"
                "<Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>"
                brd "</Style>")
        f)
      (write-line
        (strcat "  <Style ss:ID=\"Header\"><Font ss:Bold=\"1\"/>"
                "<Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>"
                "<Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>"
                brd "</Style>")
        f)
      (write-line
        (strcat "  <Style ss:ID=\"Data\">"
                "<Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>"
                brd "</Style>")
        f)
      (write-line
        (strcat "  <Style ss:ID=\"DataLeft\">"
                "<Alignment ss:Horizontal=\"Left\" ss:Vertical=\"Center\"/>"
                brd "</Style>")
        f)
      (write-line
        (strcat "  <Style ss:ID=\"Total\"><Font ss:Bold=\"1\"/>"
                "<Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>"
                "<Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>"
                brd "</Style>")
        f)
      (write-line " </Styles>" f)
      (write-line " <Worksheet ss:Name=\"Облицовка\">" f)
      (write-line "  <Table>" f)

      (if detail
        (progn
          (write-line "   <Column ss:Index=\"1\" ss:AutoFitWidth=\"0\" ss:Width=\"30\"/>" f)
          (write-line "   <Column ss:Index=\"2\" ss:AutoFitWidth=\"0\" ss:Width=\"120\"/>" f)
          (write-line "   <Column ss:Index=\"3\" ss:AutoFitWidth=\"0\" ss:Width=\"60\"/>" f)
          (write-line "   <Column ss:Index=\"4\" ss:AutoFitWidth=\"0\" ss:Width=\"40\"/>" f)
          (write-line "   <Column ss:Index=\"5\" ss:AutoFitWidth=\"0\" ss:Width=\"40\"/>" f)
        )
        (progn
          (write-line "   <Column ss:Index=\"1\" ss:AutoFitWidth=\"0\" ss:Width=\"30\"/>" f)
          (write-line "   <Column ss:Index=\"2\" ss:AutoFitWidth=\"0\" ss:Width=\"140\"/>" f)
          (write-line "   <Column ss:Index=\"3\" ss:AutoFitWidth=\"0\" ss:Width=\"40\"/>" f)
          (write-line "   <Column ss:Index=\"4\" ss:AutoFitWidth=\"0\" ss:Width=\"40\"/>" f)
        )
      )

      ;; Титул
      (write-line "   <Row ss:Height=\"20\">" f)
      (write-line
        (strcat "    <Cell ss:StyleID=\"Title\" ss:MergeAcross=\""
                (if detail "4" "3")
                "\"><Data ss:Type=\"String\">Облицовка</Data></Cell>")
        f)
      (write-line "   </Row>" f)

      ;; Шапка
      (write-line "   <Row>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">№</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Слой</Data></Cell>" f)
      (if detail
        (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Номинал</Data></Cell>" f)
      )
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Кол-во, шт</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Площадь, м2</Data></Cell>" f)
      (write-line "   </Row>" f)

      (setq total-cnt 0 total-area 0.0 itemNum 0)

      (if detail
        (progn
          ;; Группировка по слоям для подитогов
          (setq groups '())
          (foreach rec data
            (setq grp (assoc (cadr rec) groups))
            (if grp
              (setq groups (subst (append grp (list (list rec))) grp groups))
              (setq groups (append groups (list (list (cadr rec) (list rec)))))
            )
          )
          (foreach grp groups
            (setq grpName (car grp)
                  grpRows (cdr grp)
                  grpCnt 0
                  grpArea 0.0)
            (foreach rec grpRows
              (setq rec (car rec))
              (setq grpCnt (+ grpCnt (nth 3 rec)))
              (setq grpArea (+ grpArea (nth 4 rec)))
              (setq total-cnt (+ total-cnt (nth 3 rec)))
              (setq total-area (+ total-area (nth 4 rec)))
              (setq itemNum (1+ itemNum))
              (write-line "   <Row>" f)
              (write-line
                (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">"
                        (itoa itemNum) "</Data></Cell>")
                f)
              (write-line
                (strcat "    <Cell ss:StyleID=\"DataLeft\"><Data ss:Type=\"String\">"
                        (cadr rec) "</Data></Cell>")
                f)
              (write-line
                (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"String\">"
                        (caddr rec) "</Data></Cell>")
                f)
              (write-line
                (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">"
                        (itoa (nth 3 rec)) "</Data></Cell>")
                f)
              (write-line
                (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">"
                        (rtos (cl-round2 (nth 4 rec)) 2 2) "</Data></Cell>")
                f)
              (write-line "   </Row>" f)
            )
            ;; Подитог слоя
            (write-line "   <Row>" f)
            (write-line
              (strcat "    <Cell ss:StyleID=\"Total\" ss:MergeAcross=\"1\"><Data ss:Type=\"String\">  "
                      grpName "</Data></Cell>")
              f)
            (write-line "    <Cell ss:StyleID=\"Total\"><Data ss:Type=\"String\"></Data></Cell>" f)
            (write-line
              (strcat "    <Cell ss:StyleID=\"Total\"><Data ss:Type=\"Number\">"
                      (itoa grpCnt) "</Data></Cell>")
              f)
            (write-line
              (strcat "    <Cell ss:StyleID=\"Total\"><Data ss:Type=\"Number\">"
                      (rtos (cl-round2 grpArea) 2 2) "</Data></Cell>")
              f)
            (write-line "   </Row>" f)
          )
        )
        (progn
          (foreach rec data
            (setq total-cnt (+ total-cnt (nth 3 rec)))
            (setq total-area (+ total-area (nth 4 rec)))
            (setq itemNum (1+ itemNum))
            (write-line "   <Row>" f)
            (write-line
              (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">"
                      (itoa itemNum) "</Data></Cell>")
              f)
            (write-line
              (strcat "    <Cell ss:StyleID=\"DataLeft\"><Data ss:Type=\"String\">"
                      (cadr rec) "</Data></Cell>")
              f)
            (write-line
              (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">"
                      (itoa (nth 3 rec)) "</Data></Cell>")
              f)
            (write-line
              (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">"
                      (rtos (cl-round2 (nth 4 rec)) 2 2) "</Data></Cell>")
              f)
            (write-line "   </Row>" f)
          )
        )
      )

      ;; Итого (РЕДАКЦИЯ 9: убрана пустая ячейка в DETAIL)
      (write-line "   <Row>" f)
      (write-line
        (strcat "    <Cell ss:StyleID=\"Total\" ss:MergeAcross=\""
                (if detail "2" "1")
                "\"><Data ss:Type=\"String\">Итого</Data></Cell>")
        f)
      (write-line
        (strcat "    <Cell ss:StyleID=\"Total\"><Data ss:Type=\"Number\">"
                (itoa total-cnt) "</Data></Cell>")
        f)
      (write-line
        (strcat "    <Cell ss:StyleID=\"Total\"><Data ss:Type=\"Number\">"
                (rtos (cl-round2 total-area) 2 2) "</Data></Cell>")
        f)
      (write-line "   </Row>" f)

      (write-line "  </Table>" f)
      (write-line " </Worksheet>" f)
      (write-line "</Workbook>" f)
      (close f)
      T
    )
  )
)

;; ============================================================
;; ЭКСПОРТ CSV (К3): fallback и самостоятельный формат
;; Разделитель ";", площадь строкой с запятой.
;; DETAIL — плоский список без подитогов + строка "Итого"
;; ============================================================
(defun cl-write-csv (data report-mode fname / f rec detail
                          total-cnt total-area)
  (setq f (open fname "w"))
  (if (null f)
    nil
    (progn
      (setq detail (= (strcase report-mode) "DETAIL"))
      (setq total-cnt 0 total-area 0.0)
      (if detail
        (write-line "Слой;Номинал;Кол-во, шт;Площадь, м2" f)
        (write-line "Слой;Кол-во, шт;Площадь, м2" f)
      )
      (foreach rec data
        (setq total-cnt (+ total-cnt (nth 3 rec)))
        (setq total-area (+ total-area (nth 4 rec)))
        (if detail
          (write-line (strcat (cadr rec) ";" (caddr rec) ";"
                              (itoa (nth 3 rec)) ";"
                              (cl-format-area (cl-round2 (nth 4 rec)))) f)
          (write-line (strcat (cadr rec) ";"
                              (itoa (nth 3 rec)) ";"
                              (cl-format-area (cl-round2 (nth 4 rec)))) f)
        )
      )
      (if detail
        (write-line (strcat "Итого;;" (itoa total-cnt) ";"
                            (cl-format-area (cl-round2 total-area))) f)
        (write-line (strcat "Итого;" (itoa total-cnt) ";"
                            (cl-format-area (cl-round2 total-area))) f)
      )
      (close f)
      T
    )
  )
)

(princ "\nCLADDING.LSP загружен (К1-К3, ред. 9). Команда: CLADDING")
(princ)