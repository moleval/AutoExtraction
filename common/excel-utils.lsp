;;; ============================================================
;;; common/excel-utils.lsp
;;; Экспорт отчётов в Excel (XML Spreadsheet) и CSV
;;; Функции для Фасонки, Подсистемы и Заполнения
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
;; Экранирование текстового значения для CSV
;; Заключает в кавычки, если есть точка с запятой, кавычка или перевод строки
;; Внутренние кавычки удваиваются
;; ------------------------------------------------------------
(defun eu-csv-quote (str / result)
  (if (null str)
    ""
    (if (or (vl-string-search ";" str)
            (vl-string-search "\"" str)
            (vl-string-search "\n" str))
      (progn
        (setq result (vl-string-subst "\"\"" "\"" str))
        (strcat "\"" result "\"")
      )
      str
    )
  )
)

;; ============================================================
;; ФУНКЦИИ ДЛЯ ФАСОНКИ (восстановлены полностью)
;; ============================================================

(defun eu-export-xls-detail (data xlsfile / f rowNum groupIndex name recs rec itemNum len count startRow endRow sum totalSum)
  (setq f (open xlsfile "w"))
  (if (null f)
    nil
    (progn
      ;; Заголовок XML и стили
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
;; Экспорт Фасонка DETAIL в CSV
;; Разделитель: точка с запятой (единообразно с Подсистемой)
;; Десятичный разделитель: запятая
;; data: список (номер-группы имя-группы список-записей)
;;   где запись: (номер длина количество)
;; ------------------------------------------------------------
(defun eu-export-csv-detail (data csvfile / f groupIndex name recs rec itemNum len count sum)
  (setq f (open csvfile "w"))
  (if f
    (progn
      (write-line "Фасонное железо" f)
      (write-line "№;Тип фасонки;Длина, мм;Кол-во, шт.;Сумма, м.п." f)

      (foreach ig data
        (setq groupIndex (car ig)
              name       (cadr ig)
              recs       (caddr ig)
              itemNum    0)

        (foreach rec recs
          (setq itemNum (1+ itemNum)
                len     (cadr rec)
                count   (caddr rec)
                sum     (/ (* len count) 1000.0))

          (write-line
            (strcat (itoa groupIndex) "-" (itoa itemNum) ";"   ; ? дефис здесь
                    (eu-csv-quote name) ";"
                    (rtos len 2 0) ";"
                    (itoa count) ";"
                    "\"" (vl-string-translate "." "," (rtos sum 2 2)) "\"")
            f)
        )
      )

      (close f)
      T
    )
    nil
  )
)

;; ------------------------------------------------------------
;; Экспорт Фасонка SUMMARY в CSV
;; Разделитель: точка с запятой (единообразно с Подсистемой)
;; Десятичный разделитель: запятая
;; data: список (имя количество сумма)
;; ------------------------------------------------------------
(defun eu-export-csv-summary (data csvfile / f i name count sum totalCount totalSum)
  (setq f (open csvfile "w"))
  (if f
    (progn
      (write-line "Фасонное железо" f)
      (write-line "№;Тип фасонки;Кол-во, шт.;Сумма, м.п." f)

      (setq i 0 totalCount 0 totalSum 0.0)
      (foreach rec data
        (setq i (1+ i)
              name  (car rec)
              count (cadr rec)
              sum   (caddr rec))
        (setq totalCount (+ totalCount count)
              totalSum   (+ totalSum sum))

        (write-line
          (strcat (itoa i) ";"
                  (eu-csv-quote name) ";"
                  (itoa count) ";"
                  "\"" (vl-string-translate "." "," (rtos sum 2 2)) "\"")
            f)
      )

      ;; Итоговая строка
      (write-line
        (strcat "Итого;;"
                (itoa totalCount) ";"
                "\"" (vl-string-translate "." "," (rtos totalSum 2 2)) "\"")
        f)

      (close f)
      T
    )
    nil
  )
)

;; ============================================================
;; ФУНКЦИИ ДЛЯ ПОДСИСТЕМЫ
;; ============================================================

(defun eu-export-subsystem-detail (data xlsfile / f rowNum i name len cnt sum
                                    totalSum groupIndex itemNum groupName groupRows startRow endRow)
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

      (write-line " <Worksheet ss:Name=\"SubsystemDetail\">" f)
      (write-line "  <Table>" f)
      (write-line "   <Column ss:Width=\"30\"/>" f)
      (write-line "   <Column ss:Width=\"250\"/>" f)
      (write-line "   <Column ss:Width=\"75\"/>" f)
      (write-line "   <Column ss:Width=\"75\"/>" f)
      (write-line "   <Column ss:Width=\"75\"/>" f)

      (write-line "   <Row ss:Height=\"20\">" f)
      (write-line "    <Cell ss:StyleID=\"Header\" ss:MergeAcross=\"4\"><Data ss:Type=\"String\">Подсистема</Data></Cell>" f)
      (write-line "   </Row>" f)

      (write-line "   <Row>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">№</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Наименование</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Длина, мм</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Кол-во, шт.</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Сумма, м.п.</Data></Cell>" f)
      (write-line "   </Row>" f)

      (setq rowNum 3
            itemNum 0)

      ;; Штучные блоки
      (foreach rec data
        (setq name (car rec)
              len  (cadr rec)
              cnt  (caddr rec))
        (if (null len)
          (progn
            (setq itemNum (1+ itemNum))
            (write-line "   <Row>" f)
            (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (itoa itemNum) "</Data></Cell>") f)
            (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"String\">" (eu-xml-escape name) "</Data></Cell>") f)
            (write-line "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"String\"></Data></Cell>" f)
            (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (itoa cnt) "</Data></Cell>") f)
            (write-line "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"String\"></Data></Cell>" f)
            (write-line "   </Row>" f)
            (setq rowNum (1+ rowNum))
          )
        )
      )

      ;; Мерные блоки: группировка по наименованию
      (setq groups '())
      (foreach rec data
        (if (cadr rec)
          (if (not (assoc (car rec) groups))
            (setq groups (cons (list (car rec)) groups))
          )
        )
      )
      (setq groups (vl-sort groups '(lambda (a b) (< (car a) (car b)))))

      (foreach grp groups
        (setq groupName (car grp)
              groupRows '()
              totalSum 0.0
              startRow rowNum)
        (foreach rec data
          (if (and (= (car rec) groupName) (cadr rec))
            (setq groupRows (append groupRows (list rec)))
          )
        )
        (foreach rec groupRows
          (setq len (cadr rec)
                cnt (caddr rec)
                sum (/ (* len cnt) 1000.0)
                totalSum (+ totalSum sum))
          (setq itemNum (1+ itemNum))
          (write-line "   <Row>" f)
          (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (itoa itemNum) "</Data></Cell>") f)
          (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"String\">" (eu-xml-escape groupName) "</Data></Cell>") f)
          (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (rtos len 2 0) "</Data></Cell>") f)
          (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (itoa cnt) "</Data></Cell>") f)
          (write-line (strcat "    <Cell ss:StyleID=\"Num\" ss:Formula=\"=RC[-2]*RC[-1]/1000\"><Data ss:Type=\"Number\">" (rtos sum 2 2) "</Data></Cell>") f)
          (write-line "   </Row>" f)
          (setq rowNum (1+ rowNum))
        )
        (setq endRow (1- rowNum))
        (write-line "   <Row>" f)
        (write-line "    <Cell ss:StyleID=\"BoldUnderline\"><Data ss:Type=\"String\"></Data></Cell>" f)
        (write-line (strcat "    <Cell ss:StyleID=\"BoldUnderline\"><Data ss:Type=\"String\">" (eu-xml-escape groupName) "</Data></Cell>") f)
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

(defun eu-export-subsystem-summary (data xlsfile / f rowNum i name cnt sum totalCount totalSum)
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
      (write-line " </Styles>" f)
      (write-line " <Worksheet ss:Name=\"SubsystemSummary\">" f)
      (write-line "  <Table>" f)
      (write-line "   <Column ss:Width=\"30\"/>" f)
      (write-line "   <Column ss:Width=\"250\"/>" f)
      (write-line "   <Column ss:Width=\"75\"/>" f)
      (write-line "   <Column ss:Width=\"75\"/>" f)
      (write-line "   <Row ss:Height=\"20\">" f)
      (write-line "    <Cell ss:StyleID=\"Header\" ss:MergeAcross=\"3\"><Data ss:Type=\"String\">Подсистема</Data></Cell>" f)
      (write-line "   </Row>" f)
      (write-line "   <Row>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">№</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Наименование</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Кол-во, шт.</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Сумма, м.п.</Data></Cell>" f)
      (write-line "   </Row>" f)

      (setq rowNum 3
            i 0)
      (foreach rec data
        (setq i (1+ i)
              name (car rec)
              cnt  (cadr rec)
              sum  (caddr rec))
        (write-line "   <Row>" f)
        (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (itoa i) "</Data></Cell>") f)
        (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"String\">" (eu-xml-escape name) "</Data></Cell>") f)
        (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (itoa cnt) "</Data></Cell>") f)
        (if sum
          (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (rtos sum 2 2) "</Data></Cell>") f)
          (write-line "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"String\"></Data></Cell>" f)
        )
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

;; CSV для Подсистемы с десятичной запятой
(defun eu-export-subsystem-csv-detail (data csvfile / f rowNum i name len cnt sum totalSum groupName groupRows startRow endRow itemNum)
  (setq f (open csvfile "w"))
  (if f
    (progn
      (write-line "Подсистема" f)
      (write-line "№;Наименование;Длина, мм;Кол-во, шт.;Сумма, м.п." f)
      (setq rowNum 3
            itemNum 0)
      ;; Штучные
      (foreach rec data
        (if (null (cadr rec))
          (progn
            (setq itemNum (1+ itemNum))
            (write-line (strcat (itoa itemNum) ";" (car rec) ";;" (itoa (caddr rec)) ";") f)
          )
        )
      )
      ;; Мерные с подитогами
      (setq groups '())
      (foreach rec data
        (if (cadr rec)
          (if (not (assoc (car rec) groups))
            (setq groups (cons (list (car rec)) groups))
          )
        )
      )
      (setq groups (vl-sort groups '(lambda (a b) (< (car a) (car b)))))
      (foreach grp groups
        (setq groupName (car grp)
              totalSum 0.0)
        (foreach rec data
          (if (and (= (car rec) groupName) (cadr rec))
            (progn
              (setq len (cadr rec)
                    cnt (caddr rec)
                    sum (/ (* len cnt) 1000.0)
                    totalSum (+ totalSum sum))
              (setq itemNum (1+ itemNum))
              (write-line (strcat (itoa itemNum) ";" groupName ";" (rtos len 2 0) ";" (itoa cnt) ";" (vl-string-translate "." "," (rtos sum 2 2))) f)
            )
          )
        )
        ;; Подитог
        (write-line (strcat ";" groupName ";;;" (vl-string-translate "." "," (rtos totalSum 2 2))) f)
      )
      (close f)
      T
    )
    nil
  )
)

(defun eu-export-subsystem-csv-summary (data csvfile / f i name cnt sum)
  (setq f (open csvfile "w"))
  (if f
    (progn
      (write-line "Подсистема" f)
      (write-line "№;Наименование;Кол-во, шт.;Сумма, м.п." f)
      (setq i 0)
      (foreach rec data
        (setq i (1+ i)
              name (car rec)
              cnt  (cadr rec)
              sum  (caddr rec))
        (write-line
          (strcat (itoa i) ";" name ";" (itoa cnt) ";"
                  (if sum (vl-string-translate "." "," (rtos sum 2 2)) "")) f)
      )
      (close f)
      T
    )
    nil
  )
)

;; ============================================================
;; ФУНКЦИИ ДЛЯ ЗАПОЛНЕНИЯ
;; ============================================================

;; ------------------------------------------------------------
;; Округление до двух знаков после запятой (для Заполнения)
;; Применяется ДО суммирования, чтобы подитоги и итоги
;; точно соответствовали сумме отображаемых значений
;; (не терялась 0,01 при накоплении погрешностей).
;; ------------------------------------------------------------
(defun eu-round2 (x)
  (/ (fix (+ (* x 100.0) 0.5)) 100.0)
)

;; ------------------------------------------------------------
;; Форматирование площади для CSV с подавлением лишних нулей
;; 3,00 -> 3 ; 3,10 -> 3,1 ; 3,01 -> 3,01 ; 3,15 -> 3,15
;; ------------------------------------------------------------
(defun eu-format-area-csv (area / int-part frac-hundredths)
  (setq int-part (fix area))
  (setq frac-hundredths (fix (+ (* (- area int-part) 100.0) 0.5)))
  (cond
    ;; Нет дробной части: 3,00 -> 3
    ((= frac-hundredths 0)
     (itoa int-part))
    ;; Сотые нулевые, десятые ненулевые: 3,10 -> 3,1
    ((= (rem frac-hundredths 10) 0)
     (strcat (itoa int-part) "," (itoa (/ frac-hundredths 10))))
    ;; Обе цифры значимые: 3,01 -> 3,01 ; 3,15 -> 3,15
    (T
     (if (< frac-hundredths 10)
       (strcat (itoa int-part) ",0" (itoa frac-hundredths))
       (strcat (itoa int-part) "," (itoa frac-hundredths))
     )
    )
  )
)

;; ------------------------------------------------------------
;; Экспорт Заполнение DETAIL в XLS (XML Spreadsheet)
;; data: список (тип высота-мм ширина-мм количество)
;; Формулы: Площадь = Высота ? Ширина ? Кол-во / 1000000
;; Группировка по типам с подитогами
;; Общий итог суммирует только строки подитогов групп
;; Формат площади: 0.## (подавление лишних нулей)
;; ------------------------------------------------------------
(defun eu-export-zapolnenie-detail (data xlsfile / f rowNum i rec tip h w cnt area
                                     total-cnt total-area
                                     groups grp grpName grpRows grpCnt grpArea
                                     startRow endRow itemNum
                                     subtotal-rows formula-cnt formula-area r)
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
      (write-line "  <Style ss:ID=\"Num\">" f)
      (write-line "   <NumberFormat ss:Format=\"0.##\"/>" f)
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
      (write-line "   <NumberFormat ss:Format=\"0.##\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)
      (write-line " </Styles>" f)

      (write-line " <Worksheet ss:Name=\"ZapolnenieDetail\">" f)
      (write-line "  <Table>" f)
      (write-line "   <Column ss:Width=\"30\"/>" f)
      (write-line "   <Column ss:Width=\"125\"/>" f)
      (write-line "   <Column ss:Width=\"80\"/>" f)
      (write-line "   <Column ss:Width=\"80\"/>" f)
      (write-line "   <Column ss:Width=\"80\"/>" f)
      (write-line "   <Column ss:Width=\"80\"/>" f)

      (write-line "   <Row ss:Height=\"20\">" f)
      (write-line "    <Cell ss:StyleID=\"Header\" ss:MergeAcross=\"5\"><Data ss:Type=\"String\">Заполнение</Data></Cell>" f)
      (write-line "   </Row>" f)

      (write-line "   <Row>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">№</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Тип</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Высота, мм</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Ширина, мм</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Кол-во, шт.</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Площадь, м2</Data></Cell>" f)
      (write-line "   </Row>" f)

      ;; Группировка по типам (данные уже отсортированы по типу)
      (setq groups '())
      (foreach rec data
        (setq tip (car rec))
        (setq grp (assoc tip groups))
        (if grp
          (setq groups (subst (append grp (list (list rec))) grp groups))
          (setq groups (append groups (list (list tip (list rec)))))
        )
      )

      (setq rowNum 3 itemNum 0 total-cnt 0 total-area 0.0
            subtotal-rows '())

      ;; Вывод по группам с подитогами
      (foreach grp groups
        (setq grpName (car grp)
              grpRows (cdr grp)
              grpCnt  0
              grpArea 0.0
              startRow rowNum)

        ;; Строки группы
        (foreach rec grpRows
          (setq rec (car rec))
          (setq itemNum (1+ itemNum)
                h   (cadr rec)
                w   (caddr rec)
                cnt (cadddr rec)
                ;; Округление ДО суммирования — чтобы подитоги были точными
                area (eu-round2 (/ (* h w cnt) 1000000.0)))
          (setq grpCnt (+ grpCnt cnt)
                grpArea (+ grpArea area)
                total-cnt (+ total-cnt cnt)
                total-area (+ total-area area))

          (write-line "   <Row>" f)
          (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (itoa itemNum) "</Data></Cell>") f)
          (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"String\">" (eu-xml-escape grpName) "</Data></Cell>") f)
          (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (itoa h) "</Data></Cell>") f)
          (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (itoa w) "</Data></Cell>") f)
          (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (itoa cnt) "</Data></Cell>") f)
          ;; Площадь с формулой округления
          (write-line (strcat "    <Cell ss:StyleID=\"Num\" ss:Formula=\"=ROUND(RC[-3]*RC[-2]*RC[-1]/1000000,2)\"><Data ss:Type=\"Number\">" (rtos area 2 2) "</Data></Cell>") f)
          (write-line "   </Row>" f)

          (setq rowNum (1+ rowNum))
        )

        (setq endRow (1- rowNum))

        ;; Подитог группы с формулами
        (write-line "   <Row>" f)
        (write-line "    <Cell ss:StyleID=\"BoldUnderline\"><Data ss:Type=\"String\"></Data></Cell>" f)
        (write-line (strcat "    <Cell ss:StyleID=\"BoldUnderline\"><Data ss:Type=\"String\">" (eu-xml-escape grpName) "</Data></Cell>") f)
        (write-line "    <Cell ss:StyleID=\"BoldUnderline\"><Data ss:Type=\"String\"></Data></Cell>" f)
        (write-line "    <Cell ss:StyleID=\"BoldUnderline\"><Data ss:Type=\"String\"></Data></Cell>" f)
        (write-line (strcat "    <Cell ss:StyleID=\"BoldUnderline\" ss:Formula=\"=SUM(R" (itoa startRow) "C5:R" (itoa endRow) "C5)\"><Data ss:Type=\"Number\">" (itoa grpCnt) "</Data></Cell>") f)
        (write-line (strcat "    <Cell ss:StyleID=\"BoldUnderlineNum\" ss:Formula=\"=SUM(R" (itoa startRow) "C6:R" (itoa endRow) "C6)\"><Data ss:Type=\"Number\">" (rtos grpArea 2 2) "</Data></Cell>") f)
        (write-line "   </Row>" f)
        (setq rowNum (1+ rowNum))

        ;; Запоминаем номер строки подитога для формулы общего итога
        (setq subtotal-rows (append subtotal-rows (list (1- rowNum))))
      )

      ;; Построение формул общего итога: суммируем только строки подитогов групп
      (setq formula-cnt "" formula-area "")
      (foreach r subtotal-rows
        (if (= formula-cnt "")
          (setq formula-cnt (strcat "=R" (itoa r) "C5"))
          (setq formula-cnt (strcat formula-cnt "+R" (itoa r) "C5"))
        )
        (if (= formula-area "")
          (setq formula-area (strcat "=R" (itoa r) "C6"))
          (setq formula-area (strcat formula-area "+R" (itoa r) "C6"))
        )
      )

      ;; Общий итог с формулами, суммирующими только подитоги групп
      (write-line "   <Row>" f)
      (write-line "    <Cell ss:StyleID=\"BoldUnderline\" ss:MergeAcross=\"3\"><Data ss:Type=\"String\">Итого</Data></Cell>" f)
      (write-line (strcat "    <Cell ss:StyleID=\"BoldUnderline\" ss:Formula=\"" formula-cnt "\"><Data ss:Type=\"Number\">" (itoa total-cnt) "</Data></Cell>") f)
      (write-line (strcat "    <Cell ss:StyleID=\"BoldUnderlineNum\" ss:Formula=\"" formula-area "\"><Data ss:Type=\"Number\">" (rtos total-area 2 2) "</Data></Cell>") f)
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
;; Экспорт Заполнение SUMMARY в XLS (XML Spreadsheet)
;; data: список (тип количество площадь-м2)
;; Формат площади: 0.## (подавление лишних нулей)
;; ------------------------------------------------------------
(defun eu-export-zapolnenie-summary (data xlsfile / f rowNum i rec tip cnt area total-cnt total-area startRow endRow)
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
      (write-line "  <Style ss:ID=\"Num\">" f)
      (write-line "   <NumberFormat ss:Format=\"0.##\"/>" f)
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
      (write-line "   <NumberFormat ss:Format=\"0.##\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Center\" ss:Vertical=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)
      (write-line " </Styles>" f)

      (write-line " <Worksheet ss:Name=\"ZapolnenieSummary\">" f)
      (write-line "  <Table>" f)
      (write-line "   <Column ss:Width=\"30\"/>" f)
      (write-line "   <Column ss:Width=\"125\"/>" f)
      (write-line "   <Column ss:Width=\"80\"/>" f)
      (write-line "   <Column ss:Width=\"80\"/>" f)

      (write-line "   <Row ss:Height=\"20\">" f)
      (write-line "    <Cell ss:StyleID=\"Header\" ss:MergeAcross=\"3\"><Data ss:Type=\"String\">Заполнение</Data></Cell>" f)
      (write-line "   </Row>" f)

      (write-line "   <Row>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">№</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Тип</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Кол-во, шт.</Data></Cell>" f)
      (write-line "    <Cell ss:StyleID=\"Header\"><Data ss:Type=\"String\">Площадь, м2</Data></Cell>" f)
      (write-line "   </Row>" f)

      (setq rowNum 3 i 0 total-cnt 0 total-area 0.0 startRow 3)
      (foreach rec data
        (setq i (1+ i)
              tip (car rec)
              cnt (cadr rec)
              area (caddr rec))
        (setq total-cnt (+ total-cnt cnt)
              total-area (+ total-area area))

        (write-line "   <Row>" f)
        (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (itoa i) "</Data></Cell>") f)
        (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"String\">" (eu-xml-escape tip) "</Data></Cell>") f)
        (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (itoa cnt) "</Data></Cell>") f)
        (write-line (strcat "    <Cell ss:StyleID=\"Num\"><Data ss:Type=\"Number\">" (rtos area 2 2) "</Data></Cell>") f)
        (write-line "   </Row>" f)

        (setq rowNum (1+ rowNum))
      )

      (setq endRow (1- rowNum))

      ;; Итоговая строка с формулами (корректно, т.к. нет промежуточных подитогов)
      (write-line "   <Row>" f)
      (write-line "    <Cell ss:StyleID=\"BoldUnderline\" ss:MergeAcross=\"1\"><Data ss:Type=\"String\">Итого</Data></Cell>" f)
      (write-line (strcat "    <Cell ss:StyleID=\"BoldUnderline\" ss:Formula=\"=SUM(R" (itoa startRow) "C3:R" (itoa endRow) "C3)\"><Data ss:Type=\"Number\">" (itoa total-cnt) "</Data></Cell>") f)
      (write-line (strcat "    <Cell ss:StyleID=\"BoldUnderlineNum\" ss:Formula=\"=SUM(R" (itoa startRow) "C4:R" (itoa endRow) "C4)\"><Data ss:Type=\"Number\">" (rtos total-area 2 2) "</Data></Cell>") f)
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
;; Экспорт Заполнение DETAIL в CSV
;; Разделитель: точка с запятой (единообразно с Подсистемой)
;; Десятичный разделитель: запятая
;; Площадь в кавычках, с подавлением лишних нулей
;; data: список (тип высота-мм ширина-мм количество)
;; Группировка по типам с подитогами
;; ------------------------------------------------------------
(defun eu-export-zapolnenie-csv-detail (data csvfile / f i rec tip h w cnt area
                                         total-cnt total-area
                                         groups grp grpName grpRows grpCnt grpArea itemNum)
  (setq f (open csvfile "w"))
  (if f
    (progn
      (write-line "Заполнение" f)
      (write-line "№;Тип;Высота, мм;Ширина, мм;Кол-во, шт.;Площадь, м2" f)
      (setq i 0 total-cnt 0 total-area 0.0 itemNum 0)

      ;; Группировка по типам (данные уже отсортированы по типу)
      (setq groups '())
      (foreach rec data
        (setq tip (car rec))
        (setq grp (assoc tip groups))
        (if grp
          (setq groups (subst (append grp (list (list rec))) grp groups))
          (setq groups (append groups (list (list tip (list rec)))))
        )
      )

      ;; Вывод по группам с подитогами
      (foreach grp groups
        (setq grpName (car grp)
              grpRows (cdr grp)
              grpCnt  0
              grpArea 0.0)

        ;; Строки группы
        (foreach rec grpRows
          (setq rec (car rec))
          (setq itemNum (1+ itemNum)
                h   (cadr rec)
                w   (caddr rec)
                cnt (cadddr rec)
                ;; Округление ДО суммирования — чтобы подитоги были точными
                area (eu-round2 (/ (* h w cnt) 1000000.0)))
          (setq grpCnt (+ grpCnt cnt)
                grpArea (+ grpArea area)
                total-cnt (+ total-cnt cnt)
                total-area (+ total-area area))
          (write-line
            (strcat (itoa itemNum) ";"
                    (eu-csv-quote grpName) ";"
                    (itoa h) ";"
                    (itoa w) ";"
                    (itoa cnt) ";"
                    "\"" (eu-format-area-csv area) "\"")
            f)
        )

        ;; Подитог группы
        (write-line
          (strcat ";" (eu-csv-quote grpName) ";;;"
                  (itoa grpCnt) ";"
                  "\"" (eu-format-area-csv grpArea) "\"")
          f)
      )

      ;; Общий итог (3 точки с запятой: данные в колонках 5 и 6)
      (write-line
        (strcat "Итого;;;"
                (itoa total-cnt) ";"
                "\"" (eu-format-area-csv total-area) "\"")
        f)
      (close f)
      T
    )
    nil
  )
)

;; ------------------------------------------------------------
;; Экспорт Заполнение SUMMARY в CSV
;; Разделитель: точка с запятой (единообразно с Подсистемой)
;; Десятичный разделитель: запятая
;; Площадь в кавычках, с подавлением лишних нулей
;; data: список (тип количество площадь-м2)
;; ------------------------------------------------------------
(defun eu-export-zapolnenie-csv-summary (data csvfile / f i rec tip cnt area total-cnt total-area)
  (setq f (open csvfile "w"))
  (if f
    (progn
      (write-line "Заполнение" f)
      (write-line "№;Тип;Кол-во, шт.;Площадь, м2" f)
      (setq i 0 total-cnt 0 total-area 0.0)
      (foreach rec data
        (setq i (1+ i)
              tip (car rec)
              cnt (cadr rec)
              area (caddr rec))
        (setq total-cnt (+ total-cnt cnt)
              total-area (+ total-area area))
        (write-line
          (strcat (itoa i) ";"
                  (eu-csv-quote tip) ";"
                  (itoa cnt) ";"
                  "\"" (eu-format-area-csv area) "\"")
          f)
      )
      ;; Итоговая строка (2 точки с запятой: данные в колонках 3 и 4)
      (write-line
        (strcat "Итого;;"
                (itoa total-cnt) ";"
                "\"" (eu-format-area-csv total-area) "\"")
        f)
      (close f)
      T
    )
    nil
  )
)

(princ "\nEXCEL-UTILS.LSP загружен.")
(princ)