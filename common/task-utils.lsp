;;; ============================================================
;;; common/task-utils.lsp
;;; Универсальные функции для задач AutoExtraction
;;; ============================================================

(vl-load-com)

;; Безопасный вызов функции с перехватом ошибок
(defun tu-safe-call (func args / result)
  (setq result (vl-catch-all-apply func args))
  (if (vl-catch-all-error-p result)
    nil
    result
  )
)

;; Возвращает T, если вызов завершился успешно
(defun tu-safe-call-result (func args / result)
  (setq result (vl-catch-all-apply func args))
  (not (vl-catch-all-error-p result))
)

;; Проверка пустой строки
(defun tu-string-empty-p (str)
  (or (null str) (= str "") (not (= (type str) 'STR)))
)

;; Уникализация списка строк без учёта регистра
(defun tu-list-unique-ci (lst / out x key)
  (setq out '())
  (if (listp lst)
    (foreach x lst
      (if (= (type x) 'STR)
        (progn
          (setq key (strcase x))
          (if (not (vl-some '(lambda (y) (= (strcase y) key)) out))
            (setq out (append out (list x)))
          )
        )
      )
    )
  )
  out
)

;; Сортировка строк без учёта регистра
(defun tu-sort-strings-ci (lst)
  (vl-sort lst '(lambda (a b) (< (strcase a) (strcase b))))
)

;; Преобразование списка строк в строку с разделителем запятая
(defun tu-list-to-comma-string (lst)
  (if lst
    (apply 'strcat
      (cons (car lst)
        (mapcar '(lambda (x) (strcat "," x)) (cdr lst))
      )
    )
    ""
  )
)

;; Удаление расширения из пути
(defun tu-strip-extension (path)
  (if (setq pos (vl-string-position 46 (vl-string-left-trim "\\/" path) 0 T nil))
    (substr path 1 pos)
    path
  )
)

;; Формирование базового имени файла по умолчанию
(defun tu-default-save-base (task-id)
  (strcat
    (getvar "dwgprefix")
    (vl-filename-base (getvar "dwgname"))
    (cond
      ((eq task-id 'FASONKA) " Фасонка")
      (t "")
    )
  )
)

;; Запрос базового имени для сохранения
(defun tu-get-save-base (task-id / default)
  (setq default (tu-default-save-base task-id))
  (getfiled "Сохранить как" default "xls" 1)
)

;; Безопасный вывод числа с фиксированной точностью
(defun tu-safe-rtos (num prec)
  (if (numberp num)
    (rtos num 2 prec)
    "0"
  )
)

;; Проверка возможности записи файла
(defun tu-file-writable-p (path)
  (not (null (open path "a")))
)

;; Проверка существования каталога
(defun tu-ensure-directory-exists-p (dir)
  (if (findfile dir)
    T
    nil
  )
)

;; Разбор строки со списком, разделённым запятыми
(defun tu-parse-comma-list (str / pos result)
  (setq result '())
  (while (setq pos (vl-string-search "," str))
    (setq result (append result (list (vl-string-trim " " (substr str 1 pos)))))
    (setq str (substr str (+ pos 2)))
  )
  (if (> (strlen (vl-string-trim " " str)) 0)
    (setq result (append result (list (vl-string-trim " " str))))
  )
  result
)

;; ------------------------------------------------------------
;; Разбиение строки на список по произвольному разделителю
;; Используется для разбора строки слоёв, введённой через запятую
;; Пример: (split-string "a,b,c" ",") ? ("a" "b" "c")
;; ------------------------------------------------------------
(defun split-string (str delim / pos result)
  (setq result '())
  (while (setq pos (vl-string-search delim str))
    (setq result (append result (list (substr str 1 pos))))
    (setq str (substr str (+ pos 2)))
  )
  (setq result (append result (list str)))
  result
)

(princ "\nTASK-UTILS.LSP загружен.")
(princ)