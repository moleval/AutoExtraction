;;; ============================================================
;;; CHKPARENS.LSP - универсальная проверка баланса скобок LSP
;;; Команды: CHKALL (все lsp проекта), CHKFILE (один файл по пути)
;;; Функция: (chk-parens-scan path) -> печатает отчёт, возвращает
;;;          число проблем (0 = файл цел)
;;; Правила: "(" -> +1, ")" -> -1; игнорируются строки в кавычках
;;;          (включая экранированные \") и комментарии до конца строки
;;; ============================================================

(defun chk-parens-scan (path / f ln line bal i ch instr incomm skip tr top problems)
  (setq f (open path "r"))
  (if (null f)
    (progn (princ (strcat "\n[CHK] не открыт: " path)) 1)
    (progn
      (setq ln 0 bal 0 problems 0)
      (while (setq line (read-line f))
        (setq ln (1+ ln))
        (setq tr (vl-string-trim " \t" line))
        (setq top (= (substr line 1 1) "("))
        (if (and top (= (substr tr 1 6) "(defun") (/= bal 0))
          (progn
            (princ (strcat "\n[CHK] " path " БАЛАНС " (itoa bal)
                           " ПЕРЕД строкой " (itoa ln) ": " tr))
            (setq problems (1+ problems))
          )
        )
        (setq i 1 instr nil incomm nil skip nil)
        (while (<= i (strlen line))
          (setq ch (substr line i 1))
          (cond
            (skip (setq skip nil))
            (incomm nil)
            ((and instr (= ch "\\")) (setq skip T))
            (instr (if (= ch "\"") (setq instr nil)))
            ((= ch ";") (setq incomm T))
            ((= ch "\"") (setq instr T))
            ((= ch "(") (setq bal (1+ bal)))
            ((= ch ")") (setq bal (1- bal)))
          )
          (setq i (1+ i))
        )
        (if (< bal 0)
          (progn
            (princ (strcat "\n[CHK] " path
                           " ЛИШНЯЯ ЗАКРЫВАЮЩАЯ строка " (itoa ln)))
            (setq bal 0)
            (setq problems (1+ problems))
          )
        )
      )
      (close f)
      (if (/= bal 0)
        (progn
          (princ (strcat "\n[CHK] " path " ИТОГ: " (itoa bal) " (не закрыто)"))
          (setq problems (1+ problems))
        )
        (princ (strcat "\n[CHK] " path " ИТОГ: 0 (OK)"))
      )
      problems
    )
  )
)

(defun c:chkfile ( / p)
  (setq p (getstring T "\nПуть к lsp-файлу: "))
  (if (/= p "") (chk-parens-scan p))
  (princ)
)

(defun c:chkall ( / root d files f total)
  (setq root
    (if (findfile "extraction.lsp")
      (vl-filename-directory
        (vl-filename-directory (findfile "extraction.lsp")))
      nil))
  (if (null root)
    (princ "\n[CHK] не найден корень проекта (extraction.lsp).")
    (progn
      (setq total 0)
      (foreach d (list (strcat root "\\common\\")
                       (strcat root "\\Extraction\\"))
        (setq files (vl-directory-files d "*.lsp" 1))
        (if files
          (foreach f files
            (setq total (+ total (chk-parens-scan (strcat d f))))
          )
        )
      )
      (setq files (vl-directory-files root "*.lsp" 1))
      (if files
        (foreach f files
          (setq total (+ total (chk-parens-scan (strcat root "\\" f))))
        )
      )
      (princ (strcat "\n[CHK] всего проблем: " (itoa total)))
    )
  )
  (princ)
)

(princ "\nCHKPARENS.LSP загружен. Команды: CHKALL, CHKFILE")
(princ)