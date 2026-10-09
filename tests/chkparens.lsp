;;; ============================================================
;;; CHKPARENS.LSP - универсальная проверка баланса скобок LSP
;;; Команды: CHKALL (все lsp проекта), CHKFILE (один файл по пути)
;;; Функция: (chk-parens-scan path) -> печатает отчёт, возвращает
;;;          число проблем (0 = файл цел)
;;; Правила: "(" -> +1, ")" -> -1; игнорируются строки в кавычках
;;;          (включая экранированные \") и комментарии до конца строки
;;; ============================================================

(defun chk-parens-scan (path / f ln line bal i ch instr incomm skip tr top problems
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
      (princ (strcat "\n[CHK] " path " размер " (itoa size) " байт"))
      (if (/= bal 0)
        (progn
          (princ (strcat "\n[CHK] " path " ИТОГ: " (itoa bal) " (не закрыто)"))
          (setq problems (1+ problems))
        )
        (if (= problems 0)
          (princ (strcat "\n[CHK] " path " ИТОГ: 0 (OK)"))
          (princ (strcat "\n[CHK] " path " проблем: " (itoa problems)))
        )
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
      ;; Plugins\ - сторонние модули (раздел плагинов RELOAD).
      ;; Если папки нет, vl-directory-files вернёт nil - проверка
      ;; ниже просто пропустит каталог.
      (foreach d (list (strcat root "\\common\\")
                       (strcat root "\\Extraction\\")
                       (strcat root "\\Plugins\\"))
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

;; ============================================================
;; ПОИСК МЕСТА СБОЯ ЗАГРУЗКИ (CHKLOAD), 2026-10-07
;; Баланс скобок цел, а (load) падает с «синтаксической ошибкой»: файл
;; режется на префиксы по границам верхнеуровневых форм (строка начинается
;; с "(" при нулевом балансе) и каждый префикс пишется во временный файл и
;; загружается. Первый падающий префикс указывает форму со сбоем.
;; Загрузка префиксов печатает сообщения модуля — это не загрузка модуля.
;; ============================================================

;; Баланс скобок ПОСЛЕ каждой строки (строки в кавычках и комментарии не считаются)
(defun chk-line-balances (lines / out bal line i ch instr incomm skip)
  (setq out '() bal 0)
  (foreach line lines
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
    (setq out (cons bal out))
  )
  (reverse out)
)

;; Записать первые k-1 строк во временный файл и загрузить его.
;; T = префикс падает, то есть форма со сбоем внутри него.
;; Записать первые k-1 строк во временный файл (LF, перевод после каждой
;; строки) и загрузить. nil = префикс загрузился; иначе — текст ошибки.
(defun chk-load-try (lines k tmp / i f res)
  (setq i 0)
  (setq f (open tmp "w"))
  (if (null f)
    "не открыт временный файл"
    (progn
      (while (< i (1- k))
        (write-line (nth i lines) f)
        (setq i (1+ i))
      )
      (close f)
      (setq res (vl-catch-all-apply 'load (list tmp)))
      (vl-file-delete tmp)
      (if (vl-catch-all-error-p res)
        (vl-catch-all-error-message res)
        nil)
    )
  )
)

(defun chk-temp-name (path / tmp)
  (setq tmp (vl-catch-all-apply 'vl-filename-mktemp
                                (list "chkload" (vl-filename-directory path) ".lsp")))
  (if (or (vl-catch-all-error-p tmp) (null tmp))
    (setq tmp (vl-catch-all-apply 'vl-filename-mktemp '())))
  (if (or (vl-catch-all-error-p tmp) (null tmp))
    (setq tmp (strcat (vl-filename-directory path) "\\chkload_tmp.lsp")))
  tmp
)

(defun chk-load-find (path / f line lines bals bounds i lo hi mid tmp found n prev cur msg tot bad)
  (setq f (open path "r"))
  (if (null f)
    (progn (princ (strcat "\n[CHKLOAD] не открыт: " path)) nil)
    (progn
      (setq lines '())
      (while (setq line (read-line f)) (setq lines (cons line lines)))
      (close f)
      (setq lines (reverse lines) n (length lines))
      (setq bals (chk-line-balances lines))
      (setq bounds '() prev 0 i 0)
      (foreach line lines
        (if (and (= (substr line 1 1) "(") (= prev 0))
          (setq bounds (cons (1+ i) bounds)))
        (setq prev (nth i bals))
        (setq i (1+ i))
      )
      (setq bounds (reverse bounds))
      (cond
        ((null bounds)
         (princ (strcat "\n[CHKLOAD] " path
                        ": границы форм не найдены (нет строк, начинающихся с \"(\")")))
        (T
         (setq tmp (chk-temp-name path))
         (setq tot 0)
         (foreach line lines (setq tot (+ tot (strlen line))))
         (princ (strcat "\n[CHKLOAD] " path ", строк " (itoa n)
                        ", символов " (itoa tot)
                        ", форм верхнего уровня " (itoa (length bounds))))
         (princ "\n[CHKLOAD] грузятся префиксы — сообщения модуля ниже к загрузке не относятся")
         (setq msg (chk-load-try lines (1+ n) tmp))
         (if (null msg)
           (princ (strcat "\n[CHKLOAD] копия файла (LF) грузится, а сам файл — нет"
                          " (дело не в содержимом форм)"))
           (progn
             (princ (strcat "\n[CHKLOAD] загрузка всего файла: " msg))
             (setq lo 0 hi (1- (length bounds)) found nil)
             (while (<= lo hi)
               (setq mid (fix (/ (+ lo hi) 2.0)))
               (if (chk-load-try lines (nth mid bounds) tmp)
                 (progn (setq found mid hi (1- mid)))
                 (setq lo (1+ mid))
               )
             )
             ;; Префикс перед формой k падает, префикс перед формой k-1 грузится
             ;; -> дефект именно в форме k-1. Если падает только целый файл,
             ;; значит дефект в ПОСЛЕДНЕЙ форме (её префикс равен целому файлу).
             (setq bad (if found (if (> found 0) (1- found) 0) (1- (length bounds))))
             (setq cur (nth bad bounds))
             (setq msg (chk-load-try lines
                         (if found (nth found bounds) (1+ n)) tmp))
             (princ (strcat "\n[CHKLOAD] СБОЙ В ФОРМЕ №" (itoa (1+ bad))
                            ", строки " (itoa cur) ".."
                            (itoa (if (< (1+ bad) (length bounds))
                                    (1- (nth (1+ bad) bounds))
                                    n))
                            " | ошибка: " (if msg msg "нет")))
             (setq i 0)
             (foreach line lines
               (setq i (1+ i))
               (if (and (>= i cur) (<= i (+ cur 11)))
                 (princ (strcat "\n[CHKLOAD]   " (itoa i) ": " (substr line 1 160)))
               )
             )
             )
           )
         )
        )
      (princ)
      )
    )
  )

(defun c:chkload ( / p)
  (setq p (getstring T "\nПуть к lsp-файлу: "))
  (if (/= p "") (chk-load-find p))
  (princ)
)

(princ "\nCHKPARENS.LSP загружен. Команды: CHKALL, CHKFILE, CHKLOAD (поиск места сбоя загрузки)")
(princ)