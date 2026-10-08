;;; ============================================================
;;; common/settings-utils.lsp
;;; Постоянные настройки AutoExtraction.
;;;
;;; Формат: AutoExtraction\settings.ini рядом с extraction.lsp.
;;; Настройки читаются как INI и хранятся в памяти в виде списка
;;; (SECTION KEY VALUE). Значения списков хранятся нумерованными
;;; ключами: input.layer.count / input.layer.1 / ...
;;; ============================================================
(vl-load-com)

(setq *AE-SETTINGS-DATA* nil)
(setq *AE-SETTINGS-PATH* nil)
(setq *AE-SETTINGS-LOADED* nil)

(defun ae-settings-trim (s)
  (if (= (type s) 'STR)
    (vl-string-trim " \t\r\n" s)
    ""
  )
)

(defun ae-settings-ci-eq (a b)
  (= (strcase (ae-settings-trim a)) (strcase (ae-settings-trim b)))
)

(defun ae-settings-root (/ p)
  (setq p (findfile "extraction.lsp"))
  (if p
    (vl-filename-directory (vl-filename-directory p))
    nil
  )
)

(defun ae-settings-path (/ root)
  (setq root (ae-settings-root))
  (if root (strcat root "\\settings.ini") nil)
)

(defun ae-settings-entry (section key value)
  (list section key value)
)

(defun ae-settings-find (section key / found item)
  (setq found nil)
  (foreach item *AE-SETTINGS-DATA*
    (if (and (ae-settings-ci-eq (car item) section)
             (ae-settings-ci-eq (cadr item) key))
      (setq found item)
    )
  )
  found
)

(defun ae-settings-get (section key default / item)
  (setq item (ae-settings-find section key))
  (if item (caddr item) default)
)

(defun ae-settings-remove (section key / out item)
  (setq out '())
  (foreach item *AE-SETTINGS-DATA*
    (if (not (and (ae-settings-ci-eq (car item) section)
                  (ae-settings-ci-eq (cadr item) key)))
      (setq out (cons item out))
    )
  )
  (setq *AE-SETTINGS-DATA* (reverse out))
)

(defun ae-settings-set (section key value)
  (ae-settings-remove section key)
  (setq *AE-SETTINGS-DATA*
    (cons (ae-settings-entry section key (if (= (type value) 'STR) value ""))
          *AE-SETTINGS-DATA*))
  value
)

(defun ae-settings-int (section key default / s n)
  (setq s (ae-settings-get section key nil))
  (if (and s (= (type s) 'STR) (> (strlen (ae-settings-trim s)) 0))
    (progn
      (setq n (atoi (ae-settings-trim s)))
      (if (>= n 0) n default)
    )
    default
  )
)

(defun ae-settings-list (section prefix default / n i value out)
  (setq n (ae-settings-int section (strcat prefix ".count") -1))
  (if (< n 0)
    default
    (progn
      (setq out '() i 1)
      (while (<= i n)
        (setq value (ae-settings-get section (strcat prefix "." (itoa i)) ""))
        (if (> (strlen (ae-settings-trim value)) 0)
          (setq out (cons (ae-settings-trim value) out))
        )
        (setq i (1+ i))
      )
      (reverse out)
    )
  )
)

(defun ae-settings-prefix-p (key prefix)
  (and (= (type key) 'STR)
       (= (type prefix) 'STR)
       (>= (strlen key) (strlen prefix))
       (= (strcase (substr key 1 (strlen prefix))) (strcase prefix)))
)

(defun ae-settings-remove-prefix (section prefix / out item key)
  (setq out '())
  (foreach item *AE-SETTINGS-DATA*
    (setq key (cadr item))
    (if (not (and (ae-settings-ci-eq (car item) section)
                  (ae-settings-prefix-p key (strcat prefix "."))))
      (setq out (cons item out))
    )
  )
  (setq *AE-SETTINGS-DATA* (reverse out))
)

(defun ae-settings-set-list (section prefix values / i)
  (ae-settings-remove-prefix section prefix)
  (ae-settings-set section (strcat prefix ".count") (itoa (length values)))
  (setq i 1)
  (foreach value values
    (ae-settings-set section (strcat prefix "." (itoa i)) value)
    (setq i (1+ i))
  )
  values
)

(defun ae-settings-split-list (text / pos item out)
  (setq text (ae-settings-trim text) out '())
  (while (setq pos (vl-string-search ";" text))
    (setq item (ae-settings-trim (substr text 1 pos)))
    (if (> (strlen item) 0) (setq out (cons item out)))
    (setq text (substr text (+ pos 2)))
  )
  (setq item (ae-settings-trim text))
  (if (> (strlen item) 0) (setq out (cons item out)))
  (reverse out)
)

(defun ae-settings-join-list (values / out)
  (setq out "")
  (foreach value values
    (if (= out "")
      (setq out value)
      (setq out (strcat out ";" value))
    )
  )
  out
)

(defun ae-settings-read-line (line section / p raw key value)
  (setq line (ae-settings-trim line))
  (cond
    ((= line "") (list section nil nil))
    ((or (= (substr line 1 1) ";") (= (substr line 1 1) "#"))
     (list section nil nil))
    ((= (substr line 1 1) "[")
     (setq p (vl-string-search "]" line))
     (if p
       (list (ae-settings-trim (substr line 2 (1- p))) nil nil)
       (list section nil nil)
     )
    )
    (T
     (setq p (vl-string-search "=" line))
     (if p
       (progn
         (setq key (ae-settings-trim (substr line 1 p)))
         (setq value (ae-settings-trim (substr line (+ p 2))))
         (list section key value)
       )
       (list section nil nil)
    ))
  )
)

(defun ae-settings-load (/ path f line section parsed)
  (setq *AE-SETTINGS-DATA* '())
  (setq path (ae-settings-path))
  (setq *AE-SETTINGS-PATH* path)
  (if (and path (setq f (open path "r")))
    (progn
      (setq section "")
      (while (setq line (read-line f))
        (setq parsed (ae-settings-read-line line section))
        (setq section (car parsed))
        (if (and (cadr parsed) (> (strlen (cadr parsed)) 0))
          (setq *AE-SETTINGS-DATA*
            (cons (ae-settings-entry section (cadr parsed) (caddr parsed))
                  *AE-SETTINGS-DATA*))
        )
      )
      (close f)
      (setq *AE-SETTINGS-DATA* (reverse *AE-SETTINGS-DATA*))
    )
  )
  (ae-settings-seed-defaults)
  (setq *AE-SETTINGS-LOADED* T)
  *AE-SETTINGS-DATA*
)

(defun ae-settings-ensure-list (section prefix defaults / values)
  (setq values (ae-settings-list section prefix nil))
  (if (null values)
    (ae-settings-set-list section prefix defaults)
    values
  )
)

(defun ae-settings-ensure-value (section key default / item)
  (setq item (ae-settings-find section key))
  (if (or (null item)
          (and (= (type (caddr item)) 'STR)
               (= (ae-settings-trim (caddr item)) "")))
    (ae-settings-set section key default)
  )
  (ae-settings-get section key default)
)

(defun ae-settings-seed-defaults (/)
  (ae-settings-ensure-value "meta" "version" "1")

  (ae-settings-ensure-list "task.FASONKA" "input.layer"
    '("Фасонка*" "Железо*"))
  (ae-settings-ensure-list "task.FASONKA" "input.block" '("*"))
  (ae-settings-ensure-value "task.FASONKA" "output.table.layer" "CURRENT")

  (ae-settings-ensure-list "task.SUBSYSTEM" "input.layer"
    '("Подсистема" "Подсистема алюминиевая" "Подсистема оцинкованная"))
  (ae-settings-ensure-list "task.SUBSYSTEM" "input.block" '("*"))
  (ae-settings-ensure-value "task.SUBSYSTEM" "output.table.layer" "CURRENT")

  (ae-settings-ensure-list "task.CLADDING" "input.polyline.layer"
    '("*Облицовка*" "*Кассет*" "*Керамогранит*"))
  (ae-settings-ensure-list "task.CLADDING" "input.block.layer"
    '("*Облицовка*" "*Кассет*" "*Керамогранит*"))
  (ae-settings-ensure-list "task.CLADDING" "input.block.name"
    '("*КАССЕТА*" "*ПАНЕЛЬ*"))
  (ae-settings-ensure-value "task.CLADDING" "output.table.layer" "CURRENT")

  (ae-settings-ensure-list "task.VITRAZH" "input.layer"
    '("Витражи" "Стойк*" "Ригел*"))
  (ae-settings-ensure-list "task.VITRAZH" "input.block" '("*"))
  (ae-settings-ensure-value "task.VITRAZH" "output.table.layer" "CURRENT")

  (ae-settings-ensure-list "task.ZAPOLNENIE" "input.layer"
    '("Заполнение" "Стекло" "Обозначение ст-т"))
  (ae-settings-ensure-list "task.ZAPOLNENIE" "input.block" '("*"))
  ;; Припуск на раму, мм. Прибавляется к размеру «в свету» и даёт размер
  ;; заготовки. Используют ЗАПОЛНЕНИЕ (отчёт) и CUTSHEET (раскрой листа).
  (ae-settings-ensure-value "task.ZAPOLNENIE" "input.frame.allowance" "26")
  (ae-settings-ensure-value "task.ZAPOLNENIE" "output.table.layer" "CURRENT")

  (ae-settings-ensure-list "task.CUTLINE" "input.layer" '("*"))
  (ae-settings-ensure-list "task.CUTLINE" "input.block" '("*"))
  (ae-settings-ensure-value "task.CUTLINE" "output.block.template" "Раскрой {DWG}")
  (ae-settings-ensure-value "task.CUTLINE" "output.insert.layer" "CURRENT")
  (ae-settings-ensure-value "task.CUTLINE" "output.frame.layer" "Невидимые")

  (ae-settings-ensure-list "task.CUTSHEET" "input.layer" '("*"))
  (ae-settings-ensure-list "task.CUTSHEET" "input.block" '("*"))
  (ae-settings-ensure-value "task.CUTSHEET" "output.block.template" "Раскрой листа {DWG}")
  (ae-settings-ensure-value "task.CUTSHEET" "output.insert.layer" "CURRENT")
  (ae-settings-ensure-value "task.CUTSHEET" "output.frame.layer" "Невидимые")
)

(defun ae-settings-task-layers (task / section defaults)
  (setq section (strcat "task." (strcase (vl-princ-to-string task))))
  (cond
    ((= (strcase (vl-princ-to-string task)) "CLADDING")
     (append
       (ae-settings-list section "input.polyline.layer"
         '("*Облицовка*" "*Кассет*" "*Керамогранит*"))
       (ae-settings-list section "input.block.layer"
         '("*Облицовка*" "*Кассет*" "*Керамогранит*"))))
    ((= (strcase (vl-princ-to-string task)) "FASONKA")
     (ae-settings-list section "input.layer" '("Фасонка*" "Железо*")))
    ((= (strcase (vl-princ-to-string task)) "SUBSYSTEM")
     (ae-settings-list section "input.layer"
       '("Подсистема" "Подсистема алюминиевая" "Подсистема оцинкованная")))
    ((= (strcase (vl-princ-to-string task)) "VITRAZH")
     (ae-settings-list section "input.layer" '("Витражи" "Стойк*" "Ригел*")))
    ((= (strcase (vl-princ-to-string task)) "ZAPOLNENIE")
     (ae-settings-list section "input.layer"
       '("Заполнение" "Стекло" "Обозначение ст-т")))
    (T
     (ae-settings-list section "input.layer" '("*")))
  )
)

;; Припуск на раму, мм: размер «в свету» + припуск = размер заготовки.
;; Единый источник для ЗАПОЛНЕНИЯ и раскроя листа. Отрицательное или
;; нечисловое значение заменяется умолчанием — припуск не может быть
;; меньше нуля, иначе заготовка окажется меньше проёма.
(defun ae-settings-frame-allowance ( / v)
  (setq v (ae-settings-int "task.ZAPOLNENIE" "input.frame.allowance" 26))
  (if (and (numberp v) (>= v 0)) v 26)
)

(defun ae-settings-task-blocks (task / section tsk)
  (setq tsk (strcase (vl-princ-to-string task))
        section (strcat "task." tsk))
  (if (= tsk "CLADDING")
    (ae-settings-list section "input.block.name" '("*КАССЕТА*" "*ПАНЕЛЬ*"))
    (ae-settings-list section "input.block" '("*")))
)

(defun ae-settings-task-polyline-layers (task)
  (ae-settings-list
    (strcat "task." (strcase (vl-princ-to-string task)))
    "input.polyline.layer"
    '("*Облицовка*" "*Кассет*" "*Керамогранит*"))
)

(defun ae-settings-task-block-layers (task)
  (ae-settings-list
    (strcat "task." (strcase (vl-princ-to-string task)))
    "input.block.layer"
    '("*Облицовка*" "*Кассет*" "*Керамогранит*"))
)

(defun ae-settings-output (task key default)
  (ae-settings-get
    (strcat "task." (strcase (vl-princ-to-string task)))
    key default)
)

(defun ae-settings-output-layer (task / value)
  (setq value
    (ae-settings-output task "output.insert.layer" "CURRENT"))
  (if (or (null value) (= (strcase (vl-string-trim " \t" value)) "")
          (= (strcase (vl-string-trim " \t" value)) "CURRENT"))
    nil
    (vl-string-trim " \t" value))
)

(defun ae-settings-ensure-layer (layer)
  (if (and layer (not (tblsearch "LAYER" layer)))
    (entmake
      (list '(0 . "LAYER")
            '(100 . "AcDbSymbolTableRecord")
            '(100 . "AcDbLayerTableRecord")
            (cons 2 layer) '(70 . 0) '(62 . 7)
            '(6 . "Continuous") '(370 . -3)))
  )
  layer
)

(defun ae-settings-apply-entity-layer (ent layer)
  (if (and ent layer (entget ent))
    (progn
      (ae-settings-ensure-layer layer)
      (entmod (subst (cons 8 layer) (assoc 8 (entget ent)) (entget ent)))
      (entupd ent)
    )
  )
  ent
)

(defun ae-settings-apply-vla-layer (obj layer / result)
  (if (and obj layer)
    (progn
      (ae-settings-ensure-layer layer)
      (setq result
        (vl-catch-all-apply 'vla-put-Layer (list obj layer)))
      (if (vl-catch-all-error-p result) nil obj)
    )
    obj
  )
)

(defun ae-settings-write-entry (f key value)
  (write-line (strcat key "=" (if value value "")) f)
)

(defun ae-settings-write-list (f section prefix values / i)
  (ae-settings-write-entry f (strcat prefix ".count") (itoa (length values)))
  (setq i 1)
  (foreach value values
    (ae-settings-write-entry f (strcat prefix "." (itoa i)) value)
    (setq i (1+ i))
  )
)

(defun ae-settings-write-section (f section / key value)
  (write-line (strcat "[" section "]") f)
  (cond
    ((= section "meta")
     (ae-settings-write-entry f "version" (ae-settings-get section "version" "1")))
    ((= section "task.FASONKA")
     (ae-settings-write-list f section "input.layer" (ae-settings-list section "input.layer" '("Фасонка*" "Железо*")))
     (ae-settings-write-list f section "input.block" (ae-settings-list section "input.block" '("*")))
     (ae-settings-write-entry f "output.table.layer" (ae-settings-output 'FASONKA "output.table.layer" "CURRENT")))
    ((= section "task.SUBSYSTEM")
     (ae-settings-write-list f section "input.layer" (ae-settings-list section "input.layer" '("Подсистема" "Подсистема алюминиевая" "Подсистема оцинкованная")))
     (ae-settings-write-list f section "input.block" (ae-settings-list section "input.block" '("*")))
     (ae-settings-write-entry f "output.table.layer" (ae-settings-output 'SUBSYSTEM "output.table.layer" "CURRENT")))
    ((= section "task.CLADDING")
     (ae-settings-write-list f section "input.polyline.layer" (ae-settings-task-polyline-layers 'CLADDING))
     (ae-settings-write-list f section "input.block.layer" (ae-settings-task-block-layers 'CLADDING))
     (ae-settings-write-list f section "input.block.name" (ae-settings-task-blocks 'CLADDING))
     (ae-settings-write-entry f "output.table.layer" (ae-settings-output 'CLADDING "output.table.layer" "CURRENT")))
    ((= section "task.VITRAZH")
     (ae-settings-write-list f section "input.layer" (ae-settings-list section "input.layer" '("Витражи" "Стойк*" "Ригел*")))
     (ae-settings-write-list f section "input.block" (ae-settings-list section "input.block" '("*")))
     (ae-settings-write-entry f "output.table.layer" (ae-settings-output 'VITRAZH "output.table.layer" "CURRENT")))
    ((= section "task.ZAPOLNENIE")
     (ae-settings-write-list f section "input.layer" (ae-settings-list section "input.layer" '("Заполнение" "Стекло" "Обозначение ст-т")))
     (ae-settings-write-list f section "input.block" (ae-settings-list section "input.block" '("*")))
     (ae-settings-write-entry f "input.frame.allowance"
       (itoa (ae-settings-frame-allowance)))
     (ae-settings-write-entry f "output.table.layer" (ae-settings-output 'ZAPOLNENIE "output.table.layer" "CURRENT")))
    ((= section "task.CUTLINE")
     (ae-settings-write-list f section "input.layer" (ae-settings-list section "input.layer" '("*")))
     (ae-settings-write-list f section "input.block" (ae-settings-list section "input.block" '("*")))
     (ae-settings-write-entry f "output.block.template" (ae-settings-output 'CUTLINE "output.block.template" "Раскрой {DWG}"))
     (ae-settings-write-entry f "output.insert.layer" (ae-settings-output 'CUTLINE "output.insert.layer" "CURRENT"))
     (ae-settings-write-entry f "output.frame.layer" (ae-settings-output 'CUTLINE "output.frame.layer" "Невидимые")))
    ((= section "task.CUTSHEET")
     (ae-settings-write-list f section "input.layer" (ae-settings-list section "input.layer" '("*")))
     (ae-settings-write-list f section "input.block" (ae-settings-list section "input.block" '("*")))
     (ae-settings-write-entry f "output.block.template" (ae-settings-output 'CUTSHEET "output.block.template" "Раскрой листа {DWG}"))
     (ae-settings-write-entry f "output.insert.layer" (ae-settings-output 'CUTSHEET "output.insert.layer" "CURRENT"))
     (ae-settings-write-entry f "output.frame.layer" (ae-settings-output 'CUTSHEET "output.frame.layer" "Невидимые")))
  )
  (write-line "" f)
)

(defun ae-settings-write (/ path f tmp result)
  (setq path (ae-settings-path))
  (if (null path)
    nil
    (progn
      (setq tmp (strcat path ".tmp"))
      (setq f (open tmp "w"))
      (if (null f)
        nil
        (progn
          (write-line "; AutoExtraction settings" f)
          (write-line "; version 1; masks use AutoCAD wcmatch syntax" f)
          (write-line "" f)
          (ae-settings-write-section f "meta")
          (ae-settings-write-section f "task.FASONKA")
          (ae-settings-write-section f "task.SUBSYSTEM")
          (ae-settings-write-section f "task.CLADDING")
          (ae-settings-write-section f "task.VITRAZH")
          (ae-settings-write-section f "task.ZAPOLNENIE")
          (ae-settings-write-section f "task.CUTLINE")
          (ae-settings-write-section f "task.CUTSHEET")
          (close f)
          (if (findfile path) (vl-file-delete path))
          (setq result
            (vl-catch-all-apply 'vl-file-rename (list tmp path)))
          (if (vl-catch-all-error-p result)
            (progn
              (if (findfile tmp) (vl-file-delete tmp))
              nil)
            T)
        )
      )
    )
  )
)

(defun ae-settings-expand-output-template (task / template dwg)
  (setq template
    (ae-settings-output task "output.block.template"
      (if (= (strcase (vl-princ-to-string task)) "CUTSHEET")
        "Раскрой листа {DWG}"
        "Раскрой {DWG}")))
  (setq dwg (vl-filename-base (getvar "DWGNAME")))
  (vl-string-subst dwg "{DWG}" template)
)

(princ "\nSETTINGS-UTILS.LSP загружен.")
(princ)
