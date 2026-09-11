;; AutoExtraction — lightweight unit/regression tests
;; AutoCAD: APPLOAD -> tests/unit-tests.lsp -> AE_TEST

(defun ae-test-assert (condition name /)
  (if condition
    (progn (setq *AE-TEST-PASS* (1+ *AE-TEST-PASS*)) (princ (strcat "\n  PASS  " name)) T)
    (progn (setq *AE-TEST-FAIL* (1+ *AE-TEST-FAIL*)) (princ (strcat "\n  FAIL  " name)) nil)))

(defun ae-test-eq (actual expected name /)
  (ae-test-assert (equal actual expected 1e-8)
    (strcat name " | expected=" (vl-princ-to-string expected)
            " actual=" (vl-princ-to-string actual))))

(defun ae-test-ffd (/ result)
  (if (not (fboundp 'n1-ffd))
    (progn (princ "\n  SKIP  n1-ffd not loaded") T)
    (progn
      (setq result (n1-ffd '(3000.0 3000.0) 6000.0 0.0))
      (ae-test-eq (length result) 1 "FFD exact fit / zero kerf")
      (setq result (n1-ffd '(3000.0 3000.0) 6000.0 5.0))
      (ae-test-eq (length result) 2 "FFD kerf prevents overflow")
      (setq result (n1-ffd '(6000.0) 6000.0 5.0))
      (ae-test-eq (length result) 1 "FFD exact stock length")
      (if result
        (ae-test-eq (car (car result)) 0.0 "FFD exact stock leaves zero waste"))
      (setq result (n1-ffd '(6000.1) 6000.0 5.0))
      (ae-test-eq (length result) 0 "FFD oversize skipped"))))

(defun c:AE_TEST (/)
  (setq *AE-TEST-PASS* 0 *AE-TEST-FAIL* 0)
  (if (not (fboundp 'n1-ffd))
    (if (findfile "Extraction/cutline.lsp") (load "Extraction/cutline.lsp")))
  (princ "\n========================================")
  (princ "\nAutoExtraction UNIT / REGRESSION TESTS")
  (princ "\n========================================")
  (ae-test-ffd)
  (princ (strcat "\nPASS: " (itoa *AE-TEST-PASS*)
                 "\nFAIL: " (itoa *AE-TEST-FAIL*)))
  (princ (if (= *AE-TEST-FAIL* 0) "\nRESULT: PASS" "\nRESULT: FAIL"))
  (princ))
