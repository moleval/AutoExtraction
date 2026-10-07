;;; ============================================================
;;; CHKPARENS.LSP - универсальна€ проверка баланса скобок LSP
;;;  оманды: CHKALL (все lsp проекта), CHKFILE (один файл по пути)
;;; ‘ункци€: (chk-parens-scan path) -> печатает отчЄт, возвращает
;;;          число проблем (0 = файл цел)
;;; ѕравила: "(" -> +1, ")" -> -1; игнорируютс€ строки в кавычках
;;;          (включа€ экранированные \") и комментарии до конца строки
;;; ============================================================

(defun chk-parens-scan (path / f ln line bal i ch instr incomm skip tr top problems
                        size sum bytes bom ctrlline hbline hbany code tok tokpos)
  ;; –асширено (2026-10-07): кроме баланса скобок провер€ютс€ причины
  ;; Ђсинтаксической ошибкиї, при которой баланс скобок цел:
  ;;   - BOM (UTF-8) в начале файла Ч AutoCAD читает .lsp как ANSI;
  ;;   - управл€ющие байты; не-ANSI символы вне строк/комментариев
  ;;     (кроме имЄн команд вида c:»ћя);
  ;;   - незакрыта€ строкова€ константа в строке;
  ;;   - скрытые байты (NUL, смешанные переводы строк): размер против
  ;;     числа прочитанных символов.
  ;; ѕечатает размер файла Ч по нему свер€етс€, та ли верси€ файла загружена.
  (setq f (open path "r"))
  (if (null f)
    (progn (princ (strcat "\n[CHK] не открыт: " path)) 1)
    (progn
      (setq size (vl-file-size path))
      (setq ln 0 bal 0 problems 0 sum 0 ctrlline 0 hbline 0 bom nil)
      (while (setq line (read-line f))
        (setq ln (1+ ln))
        (setq sum (+ sum (strlen line)))
        (if (and (= ln 1) (>= (strlen line) 1) (= (ascii (substr line 1 1)) 239))
          (progn
            (princ (strcat "\n[CHK] " path " —“–ќ ј 1: BOM (байт 239) Ч AutoCAD читает файл как ANSI,"
                           " пересохранить без BOM"))
            (setq bom T)
            (setq problems (1+ problems))
          )
        )
        (setq tr (vl-string-trim " \t" line))
        (setq top (= (substr line 1 1) "("))
        (if (and top (= (substr tr 1 6) "(defun") (/= bal 0))
          (progn
            (princ (strcat "\n[CHK] " path " ЅјЋјЌ— " (itoa bal)
                           " ѕ≈–≈ƒ строкой " (itoa ln) ": " tr))
            (setq problems (1+ problems))
          )
        )
        (setq i 1 instr nil incomm nil skip nil)
        (while (<= i (strlen line))
          (setq ch (substr line i 1))
          (setq code (ascii ch))
          (cond
            (skip (setq skip nil))
            (incomm nil)
            ((and instr (= ch "\\")) (setq skip T))
            (instr (if (= ch "\"") (setq instr nil)))
            ((= ch ";") (setq incomm T))
            ((= ch "\"") (setq instr T))
            ((= ch "(") (setq bal (1+ bal)))
            ((= ch ")") (setq bal (1- bal)))
            ((and (< code 32) (/= code 9))
             (if (/= ctrlline ln)
               (progn
                 (princ (strcat "\n[CHK] " path " строка " (itoa ln)
                                ": управл€ющий символ (код " (itoa code) ")"))
                 (setq ctrlline ln problems (1+ problems))
               )
             )
            )
            ((> code 127)
             (setq tokpos i)
             (while (and (> tokpos 1)
                         (not (member (substr line (1- tokpos) 1)
                                      '(" " "\t" "(" ")" "\"" ";"))))
               (setq tokpos (1- tokpos))
             )
             (setq tok (substr line tokpos (- (1+ i) tokpos)))
             (if (= hbline ln) (setq hbany T) (setq hbany nil))
             (if (and (not hbany) (not (wcmatch (strcase tok) "C:*")))
               (progn
                 (princ (strcat "\n[CHK] " path " строка " (itoa ln)
                                ": не-ANSI символ вне строки/комментари€ (слово \"" tok "\")"))
                 (setq hbline ln problems (1+ problems))
               )
             )
            )
          )
          (setq i (1+ i))
        )
        (if instr
          (progn
            (princ (strcat "\n[CHK] " path " строка " (itoa ln)
                           ": строкова€ константа не закрыта (нет закрывающей кавычки)"))
            (setq problems (1+ problems))
          )
        )
        (if (< bal 0)
          (progn
            (princ (strcat "\n[CHK] " path
                           " Ћ»ЎЌяя «ј –џ¬јёўјя строка " (itoa ln)))
            (setq bal 0)
            (setq problems (1+ problems))
          )
        )
      )
      (close f)
      (setq bytes (+ sum ln))
      (if (and size (not bom)
               (/= size bytes) (/= size (1- bytes)) (/= size (+ sum ln ln)))
        (progn
          (princ (strcat "\n[CHK] " path " размер " (itoa size)
                         " байт, прочитано символов " (itoa sum)
                         " Ч есть скрытые байты (NUL или смешанные переводы строк)"))
          (setq problems (1+ problems))
        )
      )
      (princ (strcat "\n[CHK] " path " размер " (itoa size) " байт"))
      (if (/= bal 0)
        (progn
          (princ (strcat "\n[CHK] " path " »“ќ√: " (itoa bal) " (не закрыто)"))
          (setq problems (1+ problems))
        )
        (if (= problems 0)
          (princ (strcat "\n[CHK] " path " »“ќ√: 0 (OK)"))
          (princ (strcat "\n[CHK] " path " проблем: " (itoa problems)))
        )
      )
      problems
    )
  )
)

(defun c:chkfile ( / p)
  (setq p (getstring T "\nѕуть к lsp-файлу: "))
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

(princ "\nCHKPARENS.LSP загружен.  оманды: CHKALL, CHKFILE")
(princ)