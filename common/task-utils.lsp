;;; ============================================================
;;; common/task-utils.lsp
;;; Универсальные функции для задач AutoExtraction
;;
;;; ИСПРАВЛЕНИЯ (аудит Этап 4.3):
;;;   D3: Удалена функция tu-parse-comma-list.
;;;       Единственной функцией разбора строк является
;;;       универсальная функция split-string.
;;;       Поиск по проекту показал, что tu-parse-comma-list
;;;       нигде не вызывается, удаление безопасно.
;;
;;;   D9: Унификация суффиксов имен файлов.
;;;       Добавлены суффиксы для всех задач:
;;;       Фасонка, Подсистема, Заполнение, Облицовка, Витраж.
;;
;;; ПРОШЛЫЕ ИСПРАВЛЕНИЯ:
;;;   Этап 4.1 (D7): добавлена переменная pos в /-список
;;;       tu-strip-extension
;;;   Этап 4.2 (D4): оптимизация циклов — append заменен
;;;       на cons/reverse в функциях:
;;;       - tu-list-unique-ci
;;;       - split-string
;;;   Этап 4.2 (D14): исправление утечки файлового дескриптора
;;;       в функции tu-file-writable-p
;;; ============================================================

(vl-load-com)

;; ============================================================
;; Уникализация списка строк без учета регистра
;; ИСПРАВЛЕНО (аудит Этап 4.2, пункт D4):
;; Оптимизировано: cons вместо append (O(1) вместо O(N))
;; Порядок элементов сохраняется через reverse в конце.
;; ============================================================
(defun tu-list-unique-ci (lst / out x key)
  (setq out '())
  (if (listp lst)
    (foreach x lst
      (if (= (type x) 'STR)
        (progn
          (setq key (strcase x))
          (if (not (vl-some '(lambda (y) (= (strcase y) key)) out))
            (setq out (cons x out))
          )
        )
      )
    )
  )
  (reverse out)
)

;; Сортировка строк без учета регистра
(defun tu-sort-strings-ci (lst)
  (vl-sort lst '(lambda (a b) (< (strcase a) (strcase b))))
)

;; Удаление расширения из пути
;; Исправлено на Этапе 4.1: добавлена pos в /-список
(defun tu-strip-extension (path / pos)
  (if (setq pos (vl-string-position 46 (vl-string-left-trim "\\/" path) 0 T nil))
    (substr path 1 pos)
    path
  )
)

;; ============================================================
;; Формирование базового имени файла по умолчанию
;; ИСПРАВЛЕНО (аудит Этап 4.3, пункт D9):
;; Добавлены суффиксы для всех задач:
;; Фасонка, Подсистема, Заполнение, Облицовка, Витраж.
;; Ранее суффикс был только для Фасонки.
;; ============================================================
(defun tu-default-save-base (task-id)
  (strcat
    (getvar "dwgprefix")
    (vl-filename-base (getvar "dwgname"))
    (cond
      ((eq task-id 'FASONKA)    " Фасонка")
      ((eq task-id 'SUBSYSTEM)  " Подсистема")
      ((eq task-id 'ZAPOLNENIE) " Заполнение")
      ((eq task-id 'CLADDING)   " Облицовка")
      ((eq task-id 'VITRAZH)    " Витраж")
      (t "")
    )
  )
)

;; Запрос базового имени для сохранения
(defun tu-get-save-base (task-id / default)
  (setq default (tu-default-save-base task-id))
  (getfiled "Сохранить как" default "xls" 1)
)

;; ============================================================
;; Проверка возможности записи файла
;; ИСПРАВЛЕНО (аудит Этап 4.2, пункт D14):
;; Файловый дескриптор теперь закрывается после проверки.
;; ============================================================
(defun tu-file-writable-p (path / f)
  (setq f (open path "a"))
  (if f
    (progn
      (close f)
      T
    )
    nil
  )
)

;; ============================================================
;; РАЗБОР СТРОК
;; ИСПРАВЛЕНО (аудит Этап 4.3, пункт D3):
;; Функция tu-parse-comma-list удалена.
;; Единственной функцией разбора строк является
;; универсальная функция split-string.
;; Поиск по проекту показал, что tu-parse-comma-list
;; нигде не вызывается, удаление безопасно.
;; ============================================================

;; ============================================================
;; Разбиение строки на список по произвольному разделителю
;; Используется для разбора строки слоев, введенной через запятую
;; С обрезкой пробелов по краям каждого элемента.
;; Это гарантирует корректную работу при вводе слоев
;; вида "Заполнение, Стекло" (с пробелом после запятой).
;; Пример: (split-string "a, b ,c" ",") ? ("a" "b" "c")
;; ИСПРАВЛЕНО (аудит Этап 4.2, пункт D4):
;; Оптимизировано: cons вместо append (O(1) вместо O(N))
;; Порядок элементов сохраняется через reverse в конце.
;; ============================================================
(defun split-string (str delim / pos result item)
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

(princ "\nTASK-UTILS.LSP загружен.")
(princ)

;; ============================================================
;; Этап 2 (V11): единый диагностический протокол.
;; Тумблер *EXTRACTION-DIAGNOSTIC*: nil (по умолчанию) - тишина;
;; T - этапные строки [SCAN]/[FILTER]/[VALIDATION]/[PACK]/[DRAW]/[BLOCK].
;; Включается из командной строки AutoCAD: (setq *EXTRACTION-DIAGNOSTIC* T).
;; ============================================================
(setq *EXTRACTION-DIAGNOSTIC* nil)

(defun tu-diag (tag text)
  (if *EXTRACTION-DIAGNOSTIC*
    (princ (strcat "\n[" tag "] " text))))

;; ============================================================
;; Этап 3 (V8): единый guard системных переменных.
;; Паттерн SAVE -> MODIFY -> WORK -> RESTORE только для реально
;; изменяемых переменных. Любой обрыв (ESC/ошибка) обязан вернуть
;; исходные значения - возврат выполняет локальный *error* модуля.
;; ============================================================

;; Сохранить значения списка системных переменных.
;; Возвращает assoc-список (("NAME" . старое-значение) ...).
(defun tu-sysvar-save (names)
  (mapcar (function (lambda (n) (cons n (getvar n)))) names))

;; Вернуть значения, сохранённые tu-sysvar-save.
;; Сбой отдельного setvar не мешает восстановлению остальных.
(defun tu-sysvar-restore (saved)
  (foreach p saved
    (vl-catch-all-apply 'setvar (list (car p) (cdr p))))
  nil)

;; ============================================================
;; Этап 3 (V10): единый Undo-guard.
;; tu-undo-begin  — открыть Undo-группу (возвращает doc или nil).
;; tu-undo-end    — закрыть группу: всё внутри откатывается одним U.
;; tu-undo-cancel — закрыть и сразу откатить (для мелких команд).
;; ============================================================
(defun tu-undo-begin ( / doc)
  (setq doc (vl-catch-all-apply
              'vla-get-ActiveDocument (list (vlax-get-acad-object))))
  (if (and doc (not (vl-catch-all-error-p doc)))
    (progn
      (vl-catch-all-apply 'vla-StartUndoMark (list doc))
      doc)))

(defun tu-undo-end (doc)
  (if (and doc (not (vl-catch-all-error-p doc)))
    (vl-catch-all-apply 'vla-EndUndoMark (list doc)))
  nil)

(defun tu-undo-cancel (doc)
  (if (and doc (not (vl-catch-all-error-p doc)))
    (progn
      (vl-catch-all-apply 'vla-EndUndoMark (list doc))
      (vl-catch-all-apply 'vla-Undo (list doc))))
  nil)

