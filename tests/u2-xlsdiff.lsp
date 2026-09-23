;;; ============================================================
;;; tests/u2-xlsdiff.lsp - семантический diff двух XLS (SpreadsheetML)
;;; —канер тегов: обрабатывает Ќ≈— ќЋ№ ќ €чеек на одной строке файла
;;;   (формат CUTSHEET с "<Row><Cell..>...</Row>" тоже парситс€).
;;; »гнорирует <Styles>, ширины колонок и способ задани€ ss:Index:
;;;   сравниваетс€ поток €чеек (колонка | стиль | merge | значение).
;;; »спользование:
;;;   (load "D:/AutoExtraction/tests/u2-xlsdiff.lsp")
;;;   (u2-diff "D:/masters/old.xls" "D:/path/new.xls")
;;; ============================================================

(defun u2-attr (txt name / pat p q)
  (setq pat (strcat name "=\""))
  (setq p (vl-string-search pat txt))
  (if p
    (progn
      (setq p (+ p (strlen pat)))
      (setq q (vl-string-search "\"" txt p))
      (if q (substr txt (1+ p) (- q p)) ""))
    nil))

;; ѕарс одной €чейки по точному фрагменту; col - курсор колонки.
;; ¬озвращает (“ќ ≈Ќ | nil, NEW-COL): пуста€ €чейка-пропуск в поток не идЄт.
(defun u2-parse-cell (txt col / idx style merge q r e val)
  (setq idx (u2-attr txt "ss:Index"))
  (if idx (setq col (atoi idx)) (setq col (1+ col)))
  (setq style (u2-attr txt "ss:StyleID"))
  (if (null style) (setq style ""))
  (setq merge (u2-attr txt "ss:MergeAcross"))
  (if (null merge) (setq merge ""))
  (if (not (vl-string-search "<Data" txt))
    (setq val "")
    (progn
      (setq q (vl-string-search "<Data" txt))
      (setq r (vl-string-search ">" txt q))
      (setq e (vl-string-search "</Data>" txt))
      (setq val (if (and r e (>= (- e r 1) 0))
                  (substr txt (+ r 2) (- e r 1)) ""))))
  (if (and (= style "") (= merge "") (= val ""))
    (list nil col)
    (list (list "C" col style merge val) col)))

;; —канер одной строки файла: все событи€ "<Row" / "</Row>" / "<Cell".
(defun u2-scan-line (line col tokens / i a b p m ev c1 c2 txt pair L)
  (setq i 0 L (strlen line))
  (while (< i L)
    (setq a (vl-string-search "<Cell" line i))
    (setq b (vl-string-search "</Row>" line i))
    (setq p (vl-string-search "<Row" line i))
    (setq ev nil m L)
    (if (and a (< a m)) (setq ev 'cell m a))
    (if (and b (< b m)) (setq ev 'rowend m b))
    (if (and p (< p m)) (setq ev 'row m p))
    (if (null ev)
      (setq i (1+ L))
      (cond
        ((= ev 'row)
         (setq col 0 i (+ m 4)))
        ((= ev 'rowend)
         (setq tokens (cons (list "R") tokens)) (setq i (+ m 6)))
        (t
         ;; спан €чейки: до "</Cell>" (c1), либо самозакрыта€ "/>" (c2)
         (setq c1 (vl-string-search "</Cell>" line m))
         (setq c2 (vl-string-search "/>" line m))
         (if (and c2 (or (null c1) (< c2 c1)))
           (progn
             (setq txt (substr line (1+ m) (- (+ c2 2) m)))
             (setq i (+ c2 2)))
           (if c1
             (progn
               (setq txt (substr line (1+ m) (- (+ c1 7) m)))
               (setq i (+ c1 7)))
             (progn
               (setq txt (substr line (1+ m) (min 500 (- L m))))
               (setq i L))))
         (setq pair (u2-parse-cell txt col))
         (if (car pair) (setq tokens (cons (car pair) tokens)))
         (setq col (cadr pair))))))
  (list tokens col))

(defun u2-parse (fname / f line tokens col res)
  (setq tokens '() col 0)
  (setq f (open fname "r"))
  (if (null f)
    (progn
      (princ (strcat "\n[U2][FAIL] ‘айл не открываетс€ (нет такого файла?): " fname))
      (setq tokens (list (list "OPEN-FAIL" fname))))
    nil)
  (if f
    (progn
      (while (setq line (read-line f))
        (setq res (u2-scan-line line col tokens))
        (setq tokens (car res) col (cadr res)))
      (close f)))
  (reverse tokens))

(defun u2-compare (a b / i n mismatch)
  (setq i 0 n (length a) mismatch nil)
  (if (/= n (length b))
    (setq mismatch (list "LEN" n (length b)))
    (while (and (< i n) (not mismatch))
      (if (not (equal (nth i a) (nth i b)))
        (setq mismatch (list "TOK" i (nth i a) (nth i b))))
      (setq i (1+ i))))
  mismatch)

(defun u2-diff (f-old f-new / ta tb mis rows-a rows-b cells-a cells-b)
  (princ "\n=== U2 xlsdiff ===")
  (princ (strcat "\n[U2] старый: " f-old))
  (princ (strcat "\n[U2] новый:  " f-new))
  (setq ta (u2-parse f-old) tb (u2-parse f-new))
  (if (or (assoc "OPEN-FAIL" ta) (assoc "OPEN-FAIL" tb))
    (progn
      (princ "\n[U2][FAIL] diff невозможен: проверьте пути к файлам.")
      (princ "\n==========================\n")
      (exit)))
  
  (setq rows-a  (length (vl-remove-if '(lambda (x) (= (car x) "C")) ta))
        rows-b  (length (vl-remove-if '(lambda (x) (= (car x) "C")) tb))
        cells-a (length (vl-remove-if-not '(lambda (x) (= (car x) "C")) ta))
        cells-b (length (vl-remove-if-not '(lambda (x) (= (car x) "C")) tb)))
  (princ (strcat "\n[U2] строк: " (itoa rows-a) " -> " (itoa rows-b)
                 " | €чеек: " (itoa cells-a) " -> " (itoa cells-b)))
  (setq mis (u2-compare ta tb))
  (if (null mis)
    (princ "\n[U2][OK] —одержимое совпадает 1:1 по €чейкам.")
    (if (= (car mis) "LEN")
      (princ (strcat "\n[U2][FAIL] разна€ длина потока токенов: "
                     (itoa (cadr mis)) " vs " (itoa (caddr mis))))
      (progn
        (princ (strcat "\n[U2][FAIL] расхождение в токене #" (itoa (1+ (cadr mis)))))
        (princ (strcat "\n[U2] старое: " (vl-princ-to-string (caddr mis))))
        (princ (strcat "\n[U2] новое: " (vl-princ-to-string (cadddr mis)))))))
  (princ "\n==========================\n")
  (null mis))

(princ "\nU2-XLSDIFF.LSP загружен (редакци€ сканера: много €чеек на строку).")
(princ)
