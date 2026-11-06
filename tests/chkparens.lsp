;;; ============================================================
;;; CHKPARENS.LSP - универсальная проверка баланса скобок LSP
;;; Команды: CHKALL (все lsp проекта), CHKALLFULL (подробно),
;;;          CHKFILE (один файл по пути)
;;; Функция: (chk-parens-scan path quiet) -> печатает отчёт,
;;;          возвращает число проблем (0 = файл цел)
;;;
;;; ТИХИЙ РЕЖИМ (2026-10-30). CHKALL зовут автоматически при загрузке
;;; плагина MARKZ (ред. 48.36+: TEST 00 -> c:CHKALL), поэтому полный
;;; отчёт печатался на каждый RELOAD - 27 файлов по две строки.
;;; Теперь печатаются только файлы с проблемами и общий итог.
;;; Подробный отчёт: CHKALLFULL или (setq *ae-chk-quiet* nil).
;;; Переменная      : *ae-chk-quiet* T - кратко, nil - подробно
;;; Правила: "(" -> +1, ")" -> -1; игнорируются строки в кавычках
;;;          (включая экранированные \") и комментарии до конца строки
;;; ============================================================

(if (not (boundp '*ae-chk-quiet*))
  (setq *ae-chk-quiet* T))

(defun chk-parens-scan (path quiet / f ln line bal i ch instr incomm skip tr top problems
                        size sum bytes bom ctrlline hbline hbany code tok tokpos)
  ;; Расширено (2026-10-07): кроме баланса скобок проверяются причины
  ;; «синтаксической ошибки», при которой баланс скобок цел:
  ;;   - BOM (UTF-8) в начале файла — AutoCAD читает .lsp как ANSI;
  ;;   - управляющие байты; не-ANSI символы вне строк/комментариев
  ;;     (кроме имён команд вида c:ИМЯ);
  ;;   - незакрытая строковая константа в строке;
  ;;   - скрытые байты (NUL, смешанные переводы строк): размер против
  ;;     числа прочитанных символов.
  ;; Печатает размер файла — по нему сверяется, та ли версия файла загружена.
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
            (princ (strcat "\n[CHK] " path " СТРОКА 1: BOM (байт 239) — AutoCAD читает файл как ANSI,"
                           " пересохранить без BOM"))
            (setq bom T)
            (setq problems (1+ problems))
          )
        )
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
                                ": управляющий символ (код " (itoa code) ")"))
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
                                ": не-ANSI символ вне строки/комментария (слово \"" tok "\")"))
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
                           ": строковая константа не закрыта (нет закрывающей кавычки)"))
            (setq problems (1+ problems))
          )
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
      ;; Размер файла должен укладываться в окно для LF/CRLF:
      ;;   минимум sum+ln-1 (LF, последняя строка без перевода),
      ;;   максимум sum+2*ln (CRLF, перевод после последней строки).
      ;; Внутри окна «скрытых байт» нет: CR в конце строк законен.
      (setq bytes (+ sum ln))
      (if (and size (not bom)
               (or (< size (1- bytes)) (> size (+ bytes ln))))
        (progn
          (princ (strcat "\n[CHK] " path " размер " (itoa size)
                         " байт, прочитано символов " (itoa sum)
                         " — размер не сходится с содержимым (посторонние байты)"))
          (setq problems (1+ problems))
        )
      )
      (if (not quiet)
        (princ (strcat "\n[CHK] " path " размер " (itoa size) " байт")))
      (if (/= bal 0)
        (progn
          (princ (strcat "\n[CHK] " path " ИТОГ: " (itoa bal) " (не закрыто)"))
          (setq problems (1+ problems))
        )
        (cond
          ((/= problems 0)
            (princ (strcat "\n[CHK] " path " проблем: " (itoa problems))))
          ((not quiet)
            (princ (strcat "\n[CHK] " path " ИТОГ: 0 (OK)")))
        )
      )
      problems
    )
  )
)

(defun c:chkfile ( / p)
  (setq p (getstring T "\nПуть к lsp-файлу: "))
  (if (/= p "") (chk-parens-scan p nil))
  (princ)
)

(defun chk-all-run (quiet / root d files f total p rec)
  (setq root
    (if (findfile "extraction.lsp")
      (vl-filename-directory
        (vl-filename-directory (findfile "extraction.lsp")))
      nil))
  (if (null root)
    (princ "\n[CHK] не найден корень проекта (extraction.lsp).")
    (progn
      (if (not quiet)
        (princ (strcat "\n[CHK] корень проекта: " root)))
      (setq total 0)
      (foreach d (list (strcat root "\\common\\")
                       (strcat root "\\Extraction\\"))
        (setq files (vl-directory-files d "*.lsp" 1))
        (if files
          (foreach f files
            (setq total (+ total (chk-parens-scan (strcat d f) quiet)))
          )
        )
      )
      ;; Плагины из реестра [plugins] settings.ini: тот же резолвер, что у
      ;; RELOAD (ae-reload-plugin-resolve), - проверяется ровно тот файл,
      ;; который будет загружен. Файла нет - пропуск, проверка не падает.
      (if (and (member (type ae-reload-plugin-registry) '(SUBR USUBR))
               (member (type ae-reload-plugin-resolve) '(SUBR USUBR)))
        (foreach rec (ae-reload-plugin-registry)
          (setq p (ae-reload-plugin-resolve (car rec) (cadr rec)))
          (if p
            (setq total (+ total (chk-parens-scan p quiet))))
        )
      )
      (setq files (vl-directory-files root "*.lsp" 1))
      (if files
        (foreach f files
          (setq total (+ total (chk-parens-scan (strcat root "\\" f) quiet)))
        )
      )
      (princ (strcat "\n[CHK] всего проблем: " (itoa total)))
      total
    )
  )
)

;; CHKALL - краткий отчёт (по умолчанию): печатаются только проблемные
;; файлы и общий итог. Полный отчёт - CHKALLFULL.
(defun c:chkall ( / total)
  (setq total (chk-all-run (if (boundp '*ae-chk-quiet*) *ae-chk-quiet* T)))
  (princ)
)

(defun c:chkallfull ( / total)
  (setq total (chk-all-run nil))
  (princ)
)

(defun c:chkload ( / p)
  (setq p (getstring T "\nПуть к lsp-файлу: "))
  (if (/= p "") (chk-load-find p))
  (princ)
)

(princ "\nCHKPARENS.LSP загружен. Команды: CHKALL (кратко), CHKALLFULL (подробно), CHKFILE, CHKLOAD (поиск места сбоя загрузки)")
(princ)