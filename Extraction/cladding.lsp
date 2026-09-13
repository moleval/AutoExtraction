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
;;;     повернутые -> группа _ПОВЕРН_.
;;;
;;; РЕДАКЦИЯ 4: исправлен cl-poly-vertices — при чтении DXF 42
;;; вершина заменяется парой (точка . булж): (caar pts),
;;; а не (cdar pts). Ошибка давала "consp 0.0".
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

(defun cl-round3 (x)
  (/ (fix (+ (* x 1000.0) 0.5)) 1000.0)
)

(defun cl-round2 (x)
  (/ (fix (+ (* x 100.0) 0.5)) 100.0)
)

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
;; ИСПРАВЛЕНО (ред. 4): DXF 42 заменяет булж последней вершины,
;; точка берётся как (caar pts)
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
;; ИСПРАВЛЕНО (ред. 5): сегмент с ненулевым bulge — это дуга,
;; а не хорда плюс дуга. Вклад хорды для такого сегмента
;; НЕ добавляется: граница контура проходит по дуге.
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
          ;; Сегмент — ДУГА: добавляем только интеграл по дуге
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
          ;; Сегмент — прямая: добавляем вклад хорды
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

          ;; Контроль поворота: сравнение реальной площади
          ;; с площадью AABB (dx * dy).
          ;; Скругления: дефект ~0.86*R?; поворот: сотни тысяч мм?.
          ;; Порог max(1 мм?, 1% от dx*dy) разделяет классы.
          (if (> (abs (- area (* dx dy)))
                 (max 1.0 (* 0.01 dx dy)))
            (setq nominal "_НЕПРЯМОУГ_")   ; повёрнутые ИЛИ непрямоугольные
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
;; ОСНОВНАЯ ФУНКЦИЯ
;; ============================================================
(defun cladding-main (layers report-mode export-excel export-txt
                      create-table save-base
                      / *error* records data)

  (defun *error* (msg)
    (if (and msg
             (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*QUIT*,*EXIT*")))
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
      (cl-report data report-mode)
      (cl-warnings)
      (if export-excel
        (princ "\nXLS/CSV: будет реализовано на этапе К3.")
      )
      (if export-txt
        (princ "\nTXT: для облицовки не предусмотрен.")
      )
      (if create-table
        (princ "\nТаблица AutoCAD: будет реализована на этапе К2.")
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

(princ "\nCLADDING.LSP загружен (К1, ред. 4). Команда: CLADDING")
(princ)