;;; ============================================================
;;; SEARCH-SAME-SELFTEST.LSP — самопроверка ПОИСКИДВИД / ПОИСКИДАТР
;;; Команда: ПОИСКТЕСТ (SEARCHSAMETEST)
;;;
;;; Строит временные блоки, проверяет пять правил и убирает за собой.
;;; Запускать в любом чертеже, лучше в пустом:
;;;   (load "D:/AutoExtraction/tests/search_same_selftest.lsp")
;;;   ПОИСКТЕСТ
;;;
;;; Проверки:
;;;   1. те же атрибуты (регистр и пробелы не важны) — найдены;
;;;   2. другое значение — не найдено, возвращается только образец;
;;;   3. блок без атрибутов — сообщение и nil;
;;;   4. вставка на замороженном слое отброшена, образец остался;
;;;   5. ПОИСКИДВИД на динамическом блоке — только то же состояние
;;;      видимости (если в чертеже есть такой блок, иначе SKIP).
;;;
;;; Нужен загруженный модуль Extraction/searchattvis.lsp (RELOAD).
;;; ============================================================

(vl-load-com)

;; ---------- служебное: слой ----------

(defun sst-layer (name)
  (if (not (tblsearch "LAYER" name))
    (entmake (list '(0 . "LAYER")
                   '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord")
                   (cons 2 name) '(70 . 0) '(62 . 7) '(6 . "Continuous"))))
  (if (tblsearch "LAYER" name) T nil)
)

;; Заморозить (on = T) или разморозить слой, без команды .-LAYER
(defun sst-freeze (name on / ld pair flags)
  (setq ld (entget (tblobjname "LAYER" name)))
  (if ld
    (progn
      (setq pair (assoc 70 ld))
      (setq flags (cdr pair))
      (if on
        (setq flags (logior flags 1))
        (if (= 1 (logand flags 1)) (setq flags (- flags 1))))
      (entmod (subst (cons 70 flags) pair ld))
      T))
)

;; ---------- служебное: блоки и вставки ----------

;; Определение блока: отрезок + описания атрибутов (тег подсказка значение)
(defun sst-mkblock (name layer defs / made d)
  (setq made
    (entmake (list '(0 . "BLOCK") (cons 8 layer) (cons 2 name)
                   '(70 . 0) '(10 0.0 0.0 0.0))))
  (if made
    (progn
      (entmake (list '(0 . "LINE") (cons 8 layer)
                     '(10 0.0 0.0 0.0) '(11 10.0 0.0 0.0)))
      (foreach d defs
        (entmake (list '(0 . "ATTDEF") (cons 8 layer) '(10 0.0 0.0 0.0)
                       '(40 . 2.5)
                       (cons 1 (caddr d)) (cons 2 (car d))
                       (cons 3 (cadr d)) '(70 . 0))))
      (setq made (entmake (list '(0 . "ENDBLK") (cons 8 layer))))))
  (if made T nil)
)

;; Вставка блока со значениями атрибутов: vals = ((тег . значение) ...)
(defun sst-insert (name layer pt vals / en a)
  (setq en nil)
  (if (entmake (append (list '(0 . "INSERT") (cons 8 layer)
                             (cons 2 name) (cons 10 pt))
                       (if vals (list '(66 . 1)))))
    (progn
      (setq en (entlast))
      (foreach a vals
        (entmake (list '(0 . "ATTRIB") (cons 8 layer) (cons 10 pt) '(40 . 2.5)
                       (cons 1 (cdr a)) (cons 2 (car a)) '(70 . 0))))
      (if vals (entmake (list '(0 . "SEQEND") (cons 8 layer))))))
  en
)

;; ---------- служебное: запуск команды и проверки ----------

;; Образец кладётся в подразумеваемое выделение, команда вызывается как функция.
;; Результат берём из *ae-sv-same-result*: команды заканчиваются (princ), чтобы
;; AutoCAD не печатал <Selection set: ...> в командной строке, и потому сами
;; набор не возвращают. Возврат функции оставлен запасным путём для старой загрузки.
(defun sst-run (cmd en / res out)
  (setq *ae-sv-same-result* nil)
  (sssetfirst nil (ssadd en))
  (setq res (vl-catch-all-apply cmd '()))
  (setq out (if (= (type res) 'PICKSET) res *ae-sv-same-result*))
  (sssetfirst nil nil)
  (if (vl-catch-all-error-p res) nil out)
)

(defun sst-count (ss)
  (if (and ss (= (type ss) 'PICKSET)) (sslength ss) 0)
)

(defun sst-has (ss en / i hit)
  (setq hit nil i 0)
  (if (and ss (= (type ss) 'PICKSET))
    (repeat (sslength ss)
      (if (eq (ssname ss i) en) (setq hit T))
      (setq i (1+ i))))
  hit
)

;; Уборка: удалить вставки, разморозить слой, вычистить определения и слои
(defun sst-cleanup (made blks layer frz)
  (foreach en made
    (if (and en (= (type en) 'ENAME) (entget en)) (entdel en)))
  (if (and frz (tblsearch "LAYER" frz)) (sst-freeze frz nil))
  (foreach b blks
    (vl-catch-all-apply 'vl-cmdf (list "_.-PURGE" "_B" b "_N")))
  (foreach l (list frz layer)
    (if (and l (tblsearch "LAYER" l))
      (vl-catch-all-apply 'vl-cmdf (list "_.-PURGE" "_LA" l "_N"))))
  nil
)

;; ============================================================
;; САМОПРОВЕРКА
;; ============================================================

(defun c:ПОИСКТЕСТ ( / *error* suf layer frz blkA blkB blkP
                       enA enB enC enD enP enF made blks ok ss n pass fail skip
                       dynEn ref rec good en)

  (defun *error* (msg)
    ;; В обработчике ошибки только безопасное: удаление сущностей.
    ;; Команды (.-PURGE) здесь не запускаются - состояние ввода не то.
    (foreach en made
      (if (and en (= (type en) 'ENAME) (entget en)) (entdel en)))
    (setq made '())
    (sssetfirst nil nil)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\n[ПОИСКТЕСТ][ERROR] " msg)))
    (princ))

  (setq pass 0 fail 0 skip 0 made '() blks '())
  (princ "\n[ПОИСКТЕСТ] самопроверка ПОИСКИДВИД / ПОИСКИДАТР")

  (cond

    ((not (member (type c:ПОИСКИДАТР) '(SUBR USUBR)))
     (princ "\n[ПОИСКТЕСТ] Модуль не загружен. Нужен RELOAD AutoExtraction."))

    (T
     (setq suf (itoa (fix (getvar "MILLISECS"))))
     (setq layer (strcat "AE-SV-TEST-" suf))
     (setq frz (strcat "AE-SV-TEST-FRZ-" suf))
     (setq blkA (strcat "AE-SV-ATTR-" suf))
     (setq blkB (strcat "AE-SV-ATTR2-" suf))
     (setq blkP (strcat "AE-SV-PLAIN-" suf))

     (setq ok
       (and
         (sst-layer layer)
         (sst-layer frz)
         (sst-mkblock blkA layer
           '(("МАРКА" "Марка" "") ("ПРИМЕЧАНИЕ" "Примечание" "")))
         (sst-mkblock blkB layer
           '(("МАРКА" "Марка" "") ("ПРИМЕЧАНИЕ" "Примечание" "")))
         (sst-mkblock blkP layer nil)))

     (if (not ok)
       (princ "\n[ПОИСКТЕСТ] Не удалось создать служебные слои или блоки.")
       (progn
         (setq blks (list blkA blkB blkP))

         ;; Сцена: A и B совпадают по атрибутам (регистр и пробелы разные),
         ;; C отличается значением, D — другой блок с теми же значениями,
         ;; P — блок без атрибутов, F совпадает, но лежит на замороженном слое.
         (setq enA (sst-insert blkA layer '(0.0 0.0 0.0)
                     '(("МАРКА" . "Кз-2") ("ПРИМЕЧАНИЕ" . "нет"))))
         (setq enB (sst-insert blkA layer '(100.0 0.0 0.0)
                     '(("МАРКА" . " кз-2 ") ("ПРИМЕЧАНИЕ" . "НЕТ "))))
         (setq enC (sst-insert blkA layer '(200.0 0.0 0.0)
                     '(("МАРКА" . "Кз-3") ("ПРИМЕЧАНИЕ" . "нет"))))
         (setq enD (sst-insert blkB layer '(300.0 0.0 0.0)
                     '(("МАРКА" . "Кз-2") ("ПРИМЕЧАНИЕ" . "нет"))))
         (setq enP (sst-insert blkP layer '(400.0 0.0 0.0) nil))
         (setq enF (sst-insert blkA frz '(500.0 0.0 0.0)
                     '(("МАРКА" . "Кз-2") ("ПРИМЕЧАНИЕ" . "нет"))))
         (sst-freeze frz T)

         (setq made (list enA enB enC enD enP enF))

         (cond
           ((not (and enA enB enC enD enP enF))
            (princ (strcat "\n[ПОИСКТЕСТ] Не удалось создать вставки"
                           " (A/B/C/D/P/F), проверка не выполнена.")))

           (T
            ;; --- 1. Те же атрибуты: A + B, чужие C/D и F на замороженном слое отброшены
            (setq ss (sst-run (quote c:ПОИСКИДАТР) enA))
            (setq n (sst-count ss))
            (if (and (= n 2) (sst-has ss enA) (sst-has ss enB))
              (setq pass (1+ pass))
              (setq fail (1+ fail)))
            (princ (strcat "\n[ПОИСКТЕСТ] 1) те же атрибуты"
                           (if (and (= n 2) (sst-has ss enA) (sst-has ss enB))
                             " — OK" " — FAIL")
                           " (ждали 2: образец + копия; получили " (itoa n) ")"))

            ;; --- 2. Другое значение: только образец
            (setq ss (sst-run (quote c:ПОИСКИДАТР) enC))
            (setq n (sst-count ss))
            (if (and (= n 1) (sst-has ss enC))
              (setq pass (1+ pass))
              (setq fail (1+ fail)))
            (princ (strcat "\n[ПОИСКТЕСТ] 2) другое значение"
                           (if (and (= n 1) (sst-has ss enC))
                             " — OK" " — FAIL")
                           " (ждали 1: только образец; получили " (itoa n) ")"))

            ;; --- 3. Блок без атрибутов: nil и сообщение
            (setq ss (sst-run (quote c:ПОИСКИДАТР) enP))
            (setq n (sst-count ss))
            (if (= n 0) (setq pass (1+ pass)) (setq fail (1+ fail)))
            (princ (strcat "\n[ПОИСКТЕСТ] 3) блок без атрибутов"
                           (if (= n 0) " — OK" " — FAIL")
                           " (ждали 0 и сообщение; получили " (itoa n) ")"))

            ;; --- 4. Замороженный слой: F не попал, но при разморозке попадает
            (sst-freeze frz nil)
            (setq ss (sst-run (quote c:ПОИСКИДАТР) enA))
            (setq n (sst-count ss))
            (sst-freeze frz T)
            (if (and (= n 3) (sst-has ss enF))
              (setq pass (1+ pass))
              (setq fail (1+ fail)))
            (princ (strcat "\n[ПОИСКТЕСТ] 4) замороженный слой"
                           (if (and (= n 3) (sst-has ss enF))
                             " — OK" " — FAIL")
                           " (при заморозке F отброшен — в п.1 было 2;"
                           " после разморозки найдено " (itoa n) " из 3, ждали 3)"))

            ;; --- 5. Видимость: нужен динамический блок с параметром видимости
            (setq dynEn nil)
            (foreach en (ae-sv-all-model-inserts)
              (if (null dynEn)
                (progn
                  (setq ref (vl-catch-all-apply 'vlax-ename->vla-object (list en)))
                  (if (and ref (not (vl-catch-all-error-p ref)))
                    (if (car (ae-sv-dyn-props ref))
                      (setq dynEn en))))))

            (cond
              ((null dynEn)
               (setq skip (1+ skip))
               (princ "\n[ПОИСКТЕСТ] 5) видимость — SKIP: в чертеже нет динамического блока с параметром видимости"))

              (T
               (setq ss (sst-run (quote c:ПОИСКИДВИД) dynEn))
               (setq n (sst-count ss))
               (setq ref (car (ae-sv-collect (list dynEn))))
               (setq good (and (> n 0) (sst-has ss dynEn)))
               ;; все, кого вернули, обязаны иметь тот же список видимости
               (foreach rec (ae-sv-collect (ae-sv-model-inserts ss))
                 (if (not (ae-sv-same-vis-p rec ref))
                   (setq good nil)))
               (if good (setq pass (1+ pass)) (setq fail (1+ fail)))
               (princ (strcat "\n[ПОИСКТЕСТ] 5) видимость"
                              (if good " — OK" " — FAIL")
                              " (найдено " (itoa n)
                              ", все с тем же состоянием: "
                              (if good "да" "нет") ")"))))
           )
         )
       )
     )
    )
  )

  ;; уборка и итог
  (sst-cleanup made blks layer frz)
  (setq made '())
  (sssetfirst nil nil)

  (princ (strcat "\n[ПОИСКТЕСТ] ИТОГ: PASS " (itoa pass)
                 ", FAIL " (itoa fail)
                 (if (> skip 0) (strcat ", SKIP " (itoa skip)) "")
                 (if (and (= fail 0) (> pass 0)) " — проверки пройдены" "")))
  (princ)
)

(defun c:SEARCHSAMETEST () (c:ПОИСКТЕСТ))

(princ "\nSEARCH-SAME-SELFTEST.LSP загружен. Команда: ПОИСКТЕСТ (SEARCHSAMETEST)")
(princ)
