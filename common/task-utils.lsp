;;; ============================================================
;;; common/task-utils.lsp
;;; Общие утилиты для системы задач
;;; ============================================================

(vl-load-com)

(defun tu-safe-call (fn args / r)
  (setq r (vl-catch-all-apply fn args))
  (if (vl-catch-all-error-p r)
    nil
    r
  )
)

(defun tu-safe-call-result (fn args / r)
  (setq r (vl-catch-all-apply fn args))
  (if (vl-catch-all-error-p r)
    (list nil (vl-catch-all-error-message r))
    (list r nil)
  )
)

(defun tu-string-empty-p (s)
  (or (null s) (= (vl-string-trim " \t\r\n" s) ""))
)

(defun tu-list-unique-ci (lst / out x key)
  (setq out '())
  (foreach x lst
    (if (and (= 'STR (type x))
             (> (strlen x) 0))
      (progn
        (setq key (strcase x))
        (if (not
              (vl-some
                '(lambda (y) (= (strcase y) key))
                out
              )
            )
          (setq out (append out (list x)))
        )
      )
    )
  )
  out
)

(defun tu-sort-strings-ci (lst)
  (vl-sort lst
    '(lambda (a b)
       (< (strcase a) (strcase b))
     )
  )
)

(defun tu-list-to-comma-string (lst / s)
  (if lst
    (progn
      (setq s "")
      (foreach x lst
        (setq s
          (if (= s "")
            x
            (strcat s "," x)
          )
        )
      )
      s
    )
    ""
  )
)

(defun tu-strip-extension (path / p name)
  (if path
    (progn
      (setq p (vl-filename-directory path))
      (setq name (vl-filename-base path))
      (if p
        (strcat p "\\" name)
        name
      )
    )
  )
)

(defun tu-default-save-base (task-id / prefix dwg base)
  (setq prefix (getvar "DWGPREFIX"))
  (setq dwg    (getvar "DWGNAME"))
  (setq base   (vl-filename-base dwg))
  (if (or (null base) (= base ""))
    (setq base "Untitled")
  )
  (if (or (null prefix) (= prefix ""))
    (setq prefix (getvar "TEMPPREFIX"))
  )
  (strcat prefix base "_" (strcase (vl-symbol-name task-id)))
)

(defun tu-get-save-base (task-id / default result)
  (setq default (strcat (tu-default-save-base task-id) ".xls"))
  (setq result
    (getfiled
      "Сохранить отчёт"
      default
      "xls"
      1
    )
  )
  (if result
    (tu-strip-extension result)
  )
)

(defun tu-parse-comma-list (s / pos token out rest)
  (setq out '())
  (setq rest (if s (vl-string-trim " \t\r\n" s) ""))

  (while (> (strlen rest) 0)
    (setq pos (vl-string-search "," rest))

    (if pos
      (progn
        (setq token (substr rest 1 pos))
        (setq rest  (substr rest (+ pos 2)))
      )
      (progn
        (setq token rest)
        (setq rest "")
      )
    )

    (setq token (vl-string-trim " \t\r\n" token))

    (if (/= token "")
      (setq out (append out (list token)))
    )
  )

  (tu-list-unique-ci out)
)

(defun tu-safe-rtos (value mode prec)
  (if (numberp value)
    (rtos value mode prec)
    ""
  )
)

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

(defun tu-ensure-directory-exists-p (path)
  (and path
       (/= path "")
       (vl-file-directory-p path))
)

(princ)
