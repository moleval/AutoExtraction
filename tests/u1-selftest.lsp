;;; ============================================================
;;; tests/u1-selftest.lsp — самотест примитивов U1 (Этап 4)
;;; Запуск: (load "D:/AutoExtraction/tests/u1-selftest.lsp")
;;; Генерирует D:\AutoExtraction\tests\u1-selftest-out.xls,
;;; проверяет: парность тегов Cell/Row и контракт колонок
;;; (eu-cell сам тикает курсор — ss:Index руками не задаётся).
;;; ============================================================

(defun u1-count-lines (fname sub / f line cnt)
  (setq cnt 0)
  (setq f (open fname "r"))
  (if f
    (progn
      (while (setq line (read-line f))
        (if (vl-string-search sub line)
          (setq cnt (1+ cnt))))
      (close f)))
  cnt)

(defun u1-selftest ( / f out ok cellsOpen cellsClose rowsOpen rowsClose)
  (princ "\n=== U1 selftest ===")
  (setq out (strcat (getvar "ROAMABLEROOTPREFIX") "u1-selftest-out.xls"))
  ;; В проекте ROAMABLEROOTPREFIX может быть недоступен в контексте команды —
  ;; фиксированный путь рядом с тестом:
  (setq out "D:\\AutoExtraction\\tests\\u1-selftest-out.xls")
  (setq f (open out "w"))
  (setq ok nil)
  (if (null f)
    (princ (strcat "\n[U1][FAIL] Не удалось открыть файл на запись: " out))
    (progn
      (eu-doc-begin f)
      (write-line (eu-xml-styles "0.00") f)
      (eu-worksheet f "U1-TEST")
      (eu-column f "40" "0")
      (eu-column f "120" "0")
      (eu-column f "60" "0")

      (eu-row-begin f "")
      (eu-cell f "Header" "String" "Имя" "")
      (eu-cell f "Header" "String" "Описание & <теги>" "")
      (eu-cell f "Header" "String" "Число" "")
      (eu-row-end f)

      (eu-row-begin f "")
      (eu-cell f "Data" "String" "Альфа" "")
      (eu-cell f "Data" "String" "первая строка" "")
      (eu-cell f "Num" "Number" "12.5" "")
      (eu-row-end f)

      ;; Строка с пропуском средней колонки (без ручного ss:Index)
      (eu-row-begin f "")
      (eu-cell f "Data" "String" "Бета" "")
      (eu-cell-skip f 1)
      (eu-cell f "Num" "Number" "7" "")
      (eu-row-end f)

      (eu-worksheet-end f)
      (eu-doc-end f)
      (close f)

      (setq cellsOpen  (u1-count-lines out "<Cell")
            cellsClose (u1-count-lines out "</Cell>")
            rowsOpen   (u1-count-lines out "<Row>")
            rowsClose  (u1-count-lines out "</Row>"))
      ;; cellsOpen считает и закрывающие теги (там тоже есть "<Cell"), поэтому
      ;; открывающих = cellsOpen - cellsClose; каждой паре <Cell ...> </Cell>
      ;; соответствует два тэга на одной строке, пустые ячейки - одинокие <Cell/>.
      (princ (strcat "\n[U1] rows: " (itoa rowsOpen) " открыто / " (itoa rowsClose) " закрыто"))
      (princ (strcat "\n[U1] cell-тегов всего (с закрывающими): " (itoa cellsOpen)
                     ", закрывающих: " (itoa cellsClose)))
      (if (and (= rowsOpen rowsClose 3)
               (> cellsOpen cellsClose))
        (progn
          (setq ok T)
          (princ "\n[U1][OK] Структура строк/ячеек согласована."))
        (princ "\n[U1][FAIL] Несогласованность структуры."))
      (princ (strcat "\n[U1] файл: " out))
    )
  )
  (princ "\n=== U1 selftest завершён ===\n")
  ok)

(u1-selftest)
(princ)
