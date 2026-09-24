;;; ============================================================
;;; tests/validation-test.lsp - харнесс V1/V2 (Этап 2: парсер + предикаты)
;;; Закрывает блокер ревью V1/V2: "предикаты покрыты харнессом" (roadmap V1).
;;; Запуск: после RELOAD (или отдельной загрузки common/validation-utils.lsp):
;;;   (load "D:/AutoExtraction/tests/validation-test.lsp")
;;; Ожидание: "[V1][OK] все проверки пройдены." без строк [V1][FAIL].
;;; Зафиксировано поведение (замечание P1 ревью): пробелы в числах - разрядка,
;;; поэтому "1 2" -> 12. Пересмотр семантики - на V3, не сейчас.
;;; Кодировка: cp1251 (ANSI), как все .lsp проекта.
;;; ============================================================

(vl-load-com)

(if (not (boundp 'tu-parse-number))
  (princ "\n[V1][FAIL] tu-parse-number не определен - сначала RELOAD или (load \".../common/validation-utils.lsp\")")
  (progn
    (setq *v1-pass* 0 *v1-fail* 0)

    (defun v1-report (name ok)
      (if ok
        (setq *v1-pass* (1+ *v1-pass*))
        (progn
          (setq *v1-fail* (1+ *v1-fail*))
          (princ (strcat "\n[V1][FAIL] " name)))))

    ;; численное сравнение с допуском; expected=nil => actual обязан быть nil
    (defun v1-num (name actual expected)
      (v1-report name
        (if expected
          (and (numberp actual) (equal (float actual) expected 1e-9))
          (null actual))))

    ;; логическое: T/nil (любое не-nil значение считается T)
    (defun v1-bool (name actual expected)
      (v1-report name (eq (null actual) (null expected))))

    ;; структурное равенство (списки)
    (defun v1-eq (name actual expected)
      (v1-report name (equal actual expected)))

    (princ "\n=== V1/V2 selftest: tu-parse-number ===")
    (v1-num "число INT 1500 -> 1500.0"        (tu-parse-number 1500)        1500.0)
    (v1-num "число REAL 1500.5 -> 1500.5"     (tu-parse-number 1500.5)      1500.5)
    (v1-num "строка \"1500\" -> 1500.0"       (tu-parse-number "1500")      1500.0)
    (v1-num "запятая \"1500,5\" -> 1500.5"    (tu-parse-number "1500,5")    1500.5)
    (v1-num "точка \"1500.5\" -> 1500.5"      (tu-parse-number "1500.5")    1500.5)
    (v1-num "разрядка \" 1 500,5 \" -> 1500.5" (tu-parse-number " 1 500,5 ") 1500.5)
    (v1-num "пустая строка -> nil"            (tu-parse-number "")          nil)
    (v1-num "только пробелы -> nil"           (tu-parse-number "   ")       nil)
    (v1-num "мусор \"abc\" -> nil"            (tu-parse-number "abc")       nil)
    (v1-num "отрицательное \"-500\" -> -500.0" (tu-parse-number "-500")     -500.0)
    (v1-num "nil -> nil"                      (tu-parse-number nil)         nil)
    (v1-num "ЗАФИКСИРОВАНО: \"1 2\" -> 12.0 (пробелы = разрядка, пересмотр на V3)"
              (tu-parse-number "1 2")         12.0)

    (princ "\n=== V1/V2 selftest: tu-parse-number VARIANT ===")
    (v1-num "VARIANT(INT 1500) -> 1500.0"
              (tu-parse-number (vlax-make-variant 1500))       1500.0)
    (v1-num "VARIANT(REAL 2.5) -> 2.5"
              (tu-parse-number (vlax-make-variant 2.5))        2.5)
    (v1-num "VARIANT(STR \"1500\") -> nil (в VARIANT допускаются только REAL/INT)"
              (tu-parse-number (vlax-make-variant "1500"))     nil)
    ;; error-VARIANT: конструкция может не поддерживаться версией - тогда SKIP
    (setq verr (vl-catch-all-apply 'vlax-make-variant (list 0 vlax-vbError)))
    (if (vl-catch-all-error-p verr)
      (princ "\n[V1][SKIP] error-VARIANT не конструируется - ветка catch-all tu-parse-number не проверена здесь")
      (v1-num "VARIANT(error) -> nil" (tu-parse-number verr) nil))

    (princ "\n=== V1/V2 selftest: tu-parse-int-list ===")
    (v1-eq "\"1 3 5\" -> (1 3 5)"    (tu-parse-int-list "1 3 5")  '(1 3 5))
    (v1-eq "\"1 abc\" -> nil"        (tu-parse-int-list "1 abc")  nil)
    (v1-eq "пустая строка -> nil"    (tu-parse-int-list "")       nil)
    (v1-eq "REAL в списке -> nil"    (tu-parse-int-list "1.5 2")  nil)
    (v1-eq "не-строка -> nil"        (tu-parse-int-list nil)      nil)

    (princ "\n=== V1/V2 selftest: предикаты ===")
    (v1-bool "positive(0) -> nil"      (tu-positive-number-p 0)      nil)
    (v1-bool "positive(-1) -> nil"     (tu-positive-number-p -1)     nil)
    (v1-bool "positive(1) -> T"        (tu-positive-number-p 1)      T)
    (v1-bool "positive(\"1\") -> nil (строку предикат не парсит)"
              (tu-positive-number-p "1")   nil)
    (v1-bool "nonnegative(0) -> T"     (tu-nonnegative-number-p 0)   T)
    (v1-bool "nonnegative(-0.1) -> nil" (tu-nonnegative-number-p -0.1) nil)
    (v1-bool "posint(3) -> T"          (tu-positive-integer-p 3)     T)
    (v1-bool "posint(3.5) -> nil"      (tu-positive-integer-p 3.5)   nil)
    (v1-bool "posint(-3) -> nil"       (tu-positive-integer-p -3)    nil)
    (v1-bool "range(5 1 10) -> T"      (tu-number-in-range-p 5 1 10)  T)
    (v1-bool "range(10 1 10) -> T (границы включены)" (tu-number-in-range-p 10 1 10) T)
    (v1-bool "range(0 1 10) -> nil"    (tu-number-in-range-p 0 1 10)  nil)
    (v1-bool "string-nonempty(\"a\") -> T" (tu-string-nonempty-p "a") T)
    (v1-bool "string-nonempty(\"\") -> nil" (tu-string-nonempty-p "") nil)
    (v1-bool "point(1 2) -> T"         (tu-valid-point-p '(1 2))     T)
    (v1-bool "point(1 2 3) -> T"       (tu-valid-point-p '(1 2 3))   T)
    (v1-bool "point(1) -> nil"         (tu-valid-point-p '(1))       nil)
    (v1-bool "point(1 2 3 4) -> nil"   (tu-valid-point-p '(1 2 3 4)) nil)
    (v1-bool "point(1 \"a\") -> nil"   (tu-valid-point-p '(1 "a"))   nil)
    (v1-bool "size(100 200) -> T"      (tu-valid-size-p 100 200)     T)
    (v1-bool "size(0 200) -> nil"      (tu-valid-size-p 0 200)       nil)
    (v1-bool "bar(6000) -> T"          (tu-valid-bar-length-p 6000)  T)
    (v1-bool "bar(0) -> nil"           (tu-valid-bar-length-p 0)     nil)
    (v1-bool "bar(-100) -> nil"        (tu-valid-bar-length-p -100)  nil)
    (v1-bool "bar(600000) -> nil (лимит *n1-max-bar-length*)"
              (tu-valid-bar-length-p 600000) nil)
    (v1-bool "kerf(0) -> T"            (tu-valid-kerf-p 0)           T)
    (v1-bool "kerf(-1) -> nil"         (tu-valid-kerf-p -1)          nil)
    (v1-bool "kerf(100) -> T (граница включена)" (tu-valid-kerf-p 100) T)
    (v1-bool "kerf(101) -> nil (лимит *n1-max-kerf*)" (tu-valid-kerf-p 101) nil)
    (v1-bool "sheet(1500 3000) -> T"   (tu-valid-sheet-size-p 1500 3000) T)
    (v1-bool "sheet(600000 100) -> nil (лимит *tu-max-dim*)"
              (tu-valid-sheet-size-p 600000 100) nil)

    (princ (strcat "\n=== V1/V2 selftest: PASS " (itoa *v1-pass*)
                   ", FAIL " (itoa *v1-fail*) " ==="))
    (if (= *v1-fail* 0)
      (princ "\n[V1][OK] все проверки пройдены.")
      (princ (strcat "\n[V1][FAIL] провалено проверок: " (itoa *v1-fail*))))))

(princ)
