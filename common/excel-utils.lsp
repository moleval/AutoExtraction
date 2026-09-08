;;; ============================================================
;;; common/excel-utils.lsp
;;; Экспорт отчётов Фасонки в Excel (XML Spreadsheet) и CSV
;;; ============================================================

(vl-load-com)

;; ------------------------------------------------------------
;; Экранирование XML-спецсимволов
;; ------------------------------------------------------------
(defun eu-xml-escape (str / result ch)
  (setq result "")
  (while (/= str "")
    (setq ch (substr str 1 1))
    (cond
      ((= ch "&") (setq result (strcat result "&amp;")))
      ((= ch "<") (setq result (strcat result "&lt;")))
      ((= ch ">") (setq result (strcat result "&gt;")))
      ((= ch "\"") (setq result (strcat result "&quot;")))
      ((= ch "'") (setq result (strcat result "&apos;")))
      (t (setq result (strcat result ch)))
    )
    (setq str (substr str 2))
  )
  result
)

;; ------------------------------------------------------------
;; Экспорт DETAIL в XLS
;; ------------------------------------------------------------
(defun eu-export-xls-detail (data xlsfile / f rowNum groupIndex name recs rec itemNum len count startRow endRow sum totalSum)
  (setq f (open xlsfile "w"))
  (if (null f)
    nil
    (progn
      ;; Заголовок XML и стили — без изменений
      (write-line "<?xml version=\"1.0\" encoding=\"windows-1251\"?>" f)
      (write-line "<?mso-application progid=\"Excel.Sheet\"?>" f)
      (write-line "<Workbook xmlns=\"urn:schemas-microsoft-com:office:spreadsheet\"" f)
      (write-line " xmlns:o=\"urn:schemas-microsoft-com:office:office\"" f)
      (write-line " xmlns:x=\"urn:schemas-microsoft-com:office:excel\"" f)
      (write-line " xmlns:ss=\"urn:schemas-microsoft-com:office:spreadsheet\"" f)
      (write-line " xmlns:html=\"http://www.w3.org/TR/REC-html40\">" f)
      (write-line " <Styles>" f)
      (write-line "  <Style ss:ID=\"Default\" ss:Name=\"Normal\">" f)
      (write-line "   <Alignment ss:Vertical=\"Center\"/>" f)
      (write-line "  </Style>" f)
      (write-line "  <Style ss:ID=\"Data\">" f)
      (write-line "   <Alignment ss:Vertical=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)
      (write-line "  <Style ss:ID=\"Header\">" f)
      (write-line "   <Font ss:Bold=\"1\" ss:Size=\"12\" ss:Underline=\"Single\"/>" f)
      (write-line "   <Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)
      (write-line "  <Style ss:ID=\"BoldUnderline\">" f)
      (write-line "   <Font ss:Bold=\"1\" ss:Underline=\"Single\"/>" f)
      (write-line "   <Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)
      (write-line "  <Style ss:ID=\"BoldUnderlineNum\">" f)
      (write-line "   <Font ss:Bold=\"1\" ss:Underline=\"Single\"/>" f)
      (write-line "   <Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>" f)
      (write-line "   <NumberFormat ss:Format=\"0.00\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)
      (write-line "  <Style ss:ID=\"Num\">" f)
      (write-line "   <NumberFormat ss:Format=\"0.00\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)
      (write-line " </Styles>" f)

      (write-line " <Worksheet ss:Name=\"Lengths\">" f)
      (write-line "  <Table>" f)
      (write-line "   <Column ss:Width=\"30\"/>" f)
      (write-line "   <Column ss:Width=\"200\"/>" f)
      (write-line "   <Column ss:Width=\"75\"/>" f)
      (write-line "   <Column ss:Width=\"75\"/>" f)
      (write-line "   <Column ss:Width=\"75\"/>" f)

      (write-line "   <Row ss:Height=\"20\">" f)
      (write-line "    <Cell ss:StyleID=\"Header\" ss:MergeAcross=\"4\"><Data ss:Type=\"String\">Фасонное железо</Data></Cell>" f)
      (write-line "   </Row>" f)

      (write-line "   <Row>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">№</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Тип фасонки</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Длина, мм</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Кол-во, шт.</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Сумма, м.п.</Data></Cell>" f)
      (write-line "   </Row>" f)

      (setq rowNum 3)
      (foreach ig data
        (setq groupIndex (car ig)
              name       (cadr ig)
              recs       (caddr ig)
              startRow   rowNum
              itemNum    0
              totalSum   0.0)

        (foreach rec recs
          (setq itemNum (1+ itemNum)
                len     (cadr rec)
                count   (caddr rec)
                sum     (/ (* len count) 1000.0)
                totalSum (+ totalSum sum))

          (write-line "   <Row>" f)
          (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"String\">" (itoa groupIndex) "." (itoa itemNum) "</Data></Cell>") f)
          (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"String\">" (eu-xml-escape name) "</Data></Cell>") f)
          (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (rtos len 2 0) "</Data></Cell>") f)
          (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (itoa count) "</Data></Cell>") f)
          (write-line (strcat "    <Cell ss:StyleID=\"Num\" ss:Formula=\"=RC[-2]*RC[-1]/1000\"><Data ss:Type=\"Number\">" (rtos sum 2 2) "</Data></Cell>") f)
          (write-line "   </Row>" f)

          (setq rowNum (1+ rowNum))
        )

        (setq endRow (1- rowNum))
        (write-line "   <Row>" f)
        (write-line (strcat "    <Cell ss:StyleID=\"BoldUnderline\"><Data ss:Type=\"String\">" (itoa groupIndex) "</Data></Cell>") f)
        (write-line (strcat "    <Cell ss:StyleID=\"BoldUnderline\"><Data ss:Type=\"String\">" (eu-xml-escape name) "</Data></Cell>") f)
        (write-line "    <Cell ss:StyleID=\"BoldUnderline\"><Data ss:Type=\"String\"></Data></Cell>" f)
        (write-line "    <Cell ss:StyleID=\"BoldUnderline\"><Data ss:Type=\"String\"></Data></Cell>" f)
        (write-line (strcat "    <Cell ss:StyleID=\"BoldUnderlineNum\" ss:Formula=\"=SUM(R" (itoa startRow) "C5:R" (itoa endRow) "C5)\"><Data ss:Type=\"Number\">" (rtos totalSum 2 2) "</Data></Cell>") f)
        (write-line "   </Row>" f)

        (setq rowNum (1+ rowNum))
      )

      (write-line "  </Table>" f)
      (write-line " </Worksheet>" f)
      (write-line "</Workbook>" f)
      (close f)
      T
    )
  )
)

;; ------------------------------------------------------------
;; Экспорт SUMMARY в XLS
;; ------------------------------------------------------------
(defun eu-export-xls-summary (data xlsfile / f rowNum i totalCount totalSum)
  (setq f (open xlsfile "w"))
  (if (null f)
    nil
    (progn
      (write-line "<?xml version=\"1.0\" encoding=\"windows-1251\"?>" f)
      (write-line "<?mso-application progid=\"Excel.Sheet\"?>" f)
      (write-line "<Workbook xmlns=\"urn:schemas-microsoft-com:office:spreadsheet\"" f)
      (write-line " xmlns:o=\"urn:schemas-microsoft-com:office:office\"" f)
      (write-line " xmlns:x=\"urn:schemas-microsoft-com:office:excel\"" f)
      (write-line " xmlns:ss=\"urn:schemas-microsoft-com:office:spreadsheet\"" f)
      (write-line " xmlns:html=\"http://www.w3.org/TR/REC-html40\">" f)
      (write-line " <Styles>" f)
      (write-line "  <Style ss:ID=\"Default\" ss:Name=\"Normal\">" f)
      (write-line "   <Alignment ss:Vertical=\"Center\"/>" f)
      (write-line "  </Style>" f)
      (write-line "  <Style ss:ID=\"Data\">" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)
      (write-line "  <Style ss:ID=\"Header\">" f)
      (write-line "   <Font ss:Bold=\"1\" ss:Size=\"12\" ss:Underline=\"Single\"/>" f)
      (write-line "   <Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)
      (write-line "  <Style ss:ID=\"BoldUnderline\">" f)
      (write-line "   <Font ss:Bold=\"1\" ss:Underline=\"Single\"/>" f)
      (write-line "   <Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)
      (write-line " </Styles>" f)
      (write-line " <Worksheet ss:Name=\"Summary\">" f)
      (write-line "  <Table>" f)
      (write-line "   <Column ss:Width=\"30\"/>" f)
      (write-line "   <Column ss:Width=\"200\"/>" f)
      (write-line "   <Column ss:Width=\"75\"/>" f)
      (write-line "   <Column ss:Width=\"75\"/>" f)
      (write-line "   <Row ss:Height=\"20\">" f)
      (write-line "    <Cell ss:StyleID=\"Header\" ss:MergeAcross=\"3\"><Data ss:Type=\"String\">Фасонное железо</Data></Cell>" f)
      (write-line "   </Row>" f)
      (write-line "   <Row>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">№</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Тип фасонки</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Кол-во, шт.</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Сумма, м.п.</Data></Cell>" f)
      (write-line "   </Row>" f)

      (setq rowNum 3
            i 0)
      (foreach rec data
        (setq i (1+ i)
              name  (car rec)
              count (cadr rec)
              sum   (caddr rec))
        (write-line "   <Row>" f)
        (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (itoa i) "</Data></Cell>") f)
        (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"String\">" (eu-xml-escape name) "</Data></Cell>") f)
        (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (itoa count) "</Data></Cell>") f)
        (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (rtos sum 2 2) "</Data></Cell>") f)
        (write-line "   </Row>" f)
        (setq rowNum (1+ rowNum))
      )

      ;; Итоговая строка
      (setq totalCount 0
            totalSum   0.0)
      (foreach rec data
        (setq totalCount (+ totalCount (cadr rec))
              totalSum   (+ totalSum (caddr rec)))
      )
      (write-line "   <Row>" f)
      (write-line "    <Cell ss:StyleID=\"BoldUnderline\" ss:MergeAcross=\"1\"><Data ss:Type=\"String\">Итого</Data></Cell>" f)
      (write-line (strcat "    <Cell ss:StyleID=\"BoldUnderline\"><Data ss:Type=\"Number\">" (itoa totalCount) "</Data></Cell>") f)
      (write-line (strcat "    <Cell ss:StyleID=\"BoldUnderline\"><Data ss:Type=\"Number\">" (rtos totalSum 2 2) "</Data></Cell>") f)
      (write-line "   </Row>" f)

      (write-line "  </Table>" f)
      (write-line " </Worksheet>" f)
      (write-line "</Workbook>" f)
      (close f)
      T
    )
  )
)

;; ------------------------------------------------------------
;; Экспорт DETAIL в CSV (fallback)
;; ------------------------------------------------------------
(defun eu-export-csv-detail (data csvfile / f excelRow groupIndex name recs startRow itemNum totalSum len count sum endRow)
  (setq f (open csvfile "w"))
  (if (null f)
    nil
    (progn
      (write-line "Фасонное железо" f)
      (write-line "№;Тип фасонки;Длина, мм;Кол-во, шт.;Сумма, м.п." f)

      (setq excelRow 3) ; данные начинаются со строки 3

      (foreach ig data
        (setq groupIndex (car ig)
              name       (cadr ig)
              recs       (caddr ig)
              startRow   excelRow
              itemNum    0
              totalSum   0.0)

        (foreach rec recs
          (setq itemNum (1+ itemNum)
                len     (cadr rec)
                count   (caddr rec)
                sum     (/ (* len count) 1000.0)
                totalSum (+ totalSum sum))

          (write-line
            (strcat (itoa groupIndex) "_" (itoa itemNum) ";"
                    name ";"
                    (rtos len 2 0) ";"
                    (itoa count) ";"
                    "=C" (itoa excelRow) "*D" (itoa excelRow) "/1000")
            f)

          (setq excelRow (1+ excelRow))
        )

        (setq endRow (1- excelRow))
        ;; Итоговая строка группы
        (write-line
          (strcat (itoa groupIndex) ";"
                  name ";"
                  ";" ; Length пусто
                  "=СУММ(D" (itoa startRow) ":D" (itoa endRow) ");"
                  "=СУММ(E" (itoa startRow) ":E" (itoa endRow) ")")
          f)

        (setq excelRow (1+ excelRow))
      )

      (close f)
      T
    )
  )
)

;; ------------------------------------------------------------
;; Экспорт SUMMARY в CSV (fallback)
;; ------------------------------------------------------------
(defun eu-export-csv-summary (data csvfile / f i name count sum totalCount totalSum)
  (setq f (open csvfile "w"))
  (if (null f)
    nil
    (progn
      (write-line "Фасонное железо" f)
      (write-line "№;Тип фасонки;Кол-во, шт.;Сумма, м.п." f)

      (setq i 0)
      (foreach rec data
        (setq i (1+ i)
              name (car rec)
              count (cadr rec)
              sum (caddr rec))
        (write-line
          (strcat (itoa i) ";"
                  name ";"
                  (itoa count) ";"
                  (vl-string-translate "." "," (rtos sum 2 2)))
          f)
      )

      (setq totalCount 0 totalSum 0.0)
      (foreach rec data
        (setq totalCount (+ totalCount (cadr rec))
              totalSum (+ totalSum (caddr rec)))
      )
      (write-line
        (strcat "Итого;;"
                (itoa totalCount) ";"
                (vl-string-translate "." "," (rtos totalSum 2 2)))
        f)

      (close f)
      T
    )
  )
)

(princ "\nEXCEL-UTILS.LSP загружен.")
(princ)