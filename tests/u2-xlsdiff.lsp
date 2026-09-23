;;; ============================================================
;;; tests/u2-xlsdiff.lsp - семантический diff двух XLS (SpreadsheetML)
;;; »гнорирует <Styles>, ширины колонок и —ѕќ—ќЅ задани€ ss:Index:
;;;   сравниваетс€ поток €чеек (колонка | стиль | merge | значение)
;;;   по пор€дку следовани€ в файле.
;;; »спользование:
;;;   (load "D:/AutoExtraction/tests/u2-xlsdiff.lsp")
;;;   (u2-diff "D:/masters/old.xls" "D:/path/new.xls")
;;; ============================================================

(defun u2-attr (line name / pat p q)
  (setq pat (strcat name "=\""))
  (setq p (vl-string-search pat line))
  (if p
    (progn
      (setq p (+ p (strlen pat)))
      (setq q (vl-string-search "\"" line p))
      (if q (substr line (1+ p) (- q p)) ""))
    nil))

(defun u2-parse-cell (line col / idx style merge q r e val)
  ;; возвращает (“ќ ≈Ќ, NEW-COL)
  (setq idx (u2-attr line "ss:Index"))
  (if idx (setq col (atoi idx)) (setq col (1+ col)))
  (setq style (u2-attr line "ss:StyleID"))
  (if (null style) (setq style ""))
  (setq merge (u2-attr line "ss:MergeAcross"))
  (if (null merge) (setq merge ""))
  (if (or (not (vl-string-search "<Data" line))
          (and (vl-string-search "/>" line)
               (< (vl-string-search "/>" line)
                  (vl-string-search "<Data" line))))
    (setq val "")   ; пуста€ €чейка <Cell/> или <Cell ... />
    (progn
      (setq q (vl-string-search "<Data" line))
      (setq r (vl-string-search ">" line q))
      (setq e (vl-string-search "</Data>" line))
      (setq val (if (and r e (>= (- e r 1) 0))
                  (substr line (+ r 2) (- e r 1))
                  ""))))
  ;; пуста€ €чейка-пропуск (без стил€/merge/значени€) в поток токенов не идЄт -
  ;; она лишь двигает курсор колонки (замена ss:Index)
  (if (and (= style "") (= merge "") (= val ""))
    (list nil col)
    (list (list "C" col style merge val) col)))

(defun u2-parse (fname / f line tokens col pair)
  (setq tokens '() col 0)
  (setq f (open fname "r"))
  (if f
    (progn
      (while (setq line (read-line f))
        (cond
          ((vl-string-search "</Row>" line)
           (setq tokens (cons (list "R") tokens)))
          ((vl-string-search "<Row" line)
           (setq col 0))
          ((vl-string-search "<Cell" line)
           (setq pair (u2-parse-cell line col))
           (if (car pair)
             (setq tokens (cons (car pair) tokens)))
           (setq col (cadr pair)))))
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

(princ "\nU2-XLSDIFF.LSP загружен. »спользование: (u2-diff old-file new-file)")
(princ)
