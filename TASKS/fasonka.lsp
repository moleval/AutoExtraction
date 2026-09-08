;;; ============================================================
;;;  FASONKA.LSP
;;;  Извлечение данных о фасонном железе
;;;  Поддержка режимов DETAIL и SUMMARY
;;;  Экспорт в XLS (XML Spreadsheet), CSV, GAL (TXT)
;;;  Создание таблиц AutoCAD (вынесено в common/table-utils.lsp)
;;; ============================================================

(defun c:fasonka () (fasonka-main))
(defun c:Фасонка () (fasonka-main))

(defun fasonka-main (layers report-mode export-excel export-txt create-table save-base
                     / *error* csvfile f
                     val name len acc rec found
                     ss i ent obj effname dynprops prop
                     layer-filter lay
                     detail-groups curName curRecs
                     indexed-detail groupIndex
                     summary-groups totalCount totalSum
                     report-data report-type
                     total-blocks total-pos total-types total-sum
                     base-name xlsfile
                     save-xls save-xls-summary
                     txt-utils-path table-utils-path)

  (vl-load-com)

  ;; Загрузка txt-utils.lsp для GAL-экспорта
  (setq txt-utils-path
    (strcat
      (vl-filename-directory
        (vl-filename-directory (findfile "fasonka.lsp"))
      )
      "\\common\\txt-utils.lsp"
    )
  )
  (if (findfile txt-utils-path)
    (load txt-utils-path)
    (princ "\nПредупреждение: txt-utils.lsp не найден по пути " txt-utils-path)
  )

  ;; Загрузка table-utils.lsp для создания таблиц
  (setq table-utils-path
    (strcat
      (vl-filename-directory
        (vl-filename-directory (findfile "fasonka.lsp"))
      )
      "\\common\\table-utils.lsp"
    )
  )
  (if (findfile table-utils-path)
    (load table-utils-path)
    (princ "\nПредупреждение: table-utils.lsp не найден по пути " table-utils-path)
  )

  ;; Обработчик ошибок
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\nОшибка: " msg))
    )
    (princ)
  )

  ;; Очистка имени блока
  (defun clean-name (str / tmp first rest)
    (if (and str (> (strlen str) 7))
      (progn
        (setq tmp (substr str 8))
        (if (> (strlen tmp) 0)
          (setq first (strcase (substr tmp 1 1))
                rest  (substr tmp 2)
                tmp   (strcat first rest))
        )
        tmp
      )
      str
    )
  )

  ;; Экранирование XML
  (defun xml-escape (str / result ch)
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

  ;; ============ Экспорт в XLS ============
  ;; DETAIL XLS
  (defun save-xls (data xlsfile / f rowNum groupIndex name recs rec itemNum len count startRow endRow sum totalSum)
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
            (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"String\">" (xml-escape name) "</Data></Cell>") f)
            (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (rtos len 2 0) "</Data></Cell>") f)
            (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"Number\">" (itoa count) "</Data></Cell>") f)
            (write-line (strcat "    <Cell ss:StyleID=\"Num\" ss:Formula=\"=RC[-2]*RC[-1]/1000\"><Data ss:Type=\"Number\">" (rtos sum 2 2) "</Data></Cell>") f)
            (write-line "   </Row>" f)

            (setq rowNum (1+ rowNum))
          )

          (setq endRow (1- rowNum))
          (write-line "   <Row>" f)
          (write-line (strcat "    <Cell ss:StyleID=\"BoldUnderline\"><Data ss:Type=\"String\">" (itoa groupIndex) "</Data></Cell>") f)
          (write-line (strcat "    <Cell ss:StyleID=\"BoldUnderline\"><Data ss:Type=\"String\">" (xml-escape name) "</Data></Cell>") f)
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

  ;; SUMMARY XLS (4 колонки)
  (defun save-xls-summary (data xlsfile / f rowNum i totalCount totalSum)
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
          (write-line (strcat "    <Cell ss:StyleID=\"Data\"><Data ss:Type=\"String\">" (xml-escape name) "</Data></Cell>") f)
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

  ;; ==================== Формирование выборки блоков ====================
  (setq ss (ssget "_I"))
  (if (null ss)
    (progn
      (cond
        ((null layers)
         (setq ss (ssget "_X" '((0 . "INSERT")))))
        (t
         (setq layer-filter (list '(0 . "INSERT")))
         (foreach lay layers
           (setq layer-filter (append layer-filter (list (cons 8 lay)))))
         (setq ss (ssget "_X" layer-filter))
        )
      )
    )
  )

  (if ss
    (progn
      (setq i 0 acc '())
      (repeat (sslength ss)
        (setq ent (ssname ss i)
              obj (vlax-ename->vla-object ent)
              effname (vla-get-effectivename obj)
              dynprops (vlax-invoke obj 'GetDynamicBlockProperties)
              val nil)
        (foreach prop dynprops
          (if (= (strcase (vla-get-propertyname prop)) "ДЛИНА")
            (setq val (vlax-get prop 'Value))
          )
        )
        (if val
          (progn
            (setq name (clean-name effname)
                  len  (atoi (rtos val 2 0))
                  found nil)
            (foreach rec acc
              (if (and (= (car rec) name) (= (cadr rec) len))
                (setq found rec)
              )
            )
            (if found
              (setq acc (subst (list name len (1+ (caddr found))) found acc))
              (setq acc (cons (list name len 1) acc))
            )
          )
        )
        (setq i (1+ i))
      )

      (if acc
        (progn
          ;; Сортировка по имени и длине
          (setq acc (vl-sort acc '(lambda (a b) (if (= (car a) (car b)) (< (cadr a) (cadr b)) (< (car a) (car b))))))

          ;; Группировка для DETAIL
          (setq detail-groups '() curName nil curRecs '())
          (foreach rec acc
            (setq name (car rec))
            (if (not (equal name curName))
              (progn (if curName (setq detail-groups (cons (cons curName (reverse curRecs)) detail-groups))) (setq curName name curRecs (list rec)))
              (setq curRecs (cons rec curRecs))
            )
          )
          (if curName (setq detail-groups (cons (cons curName (reverse curRecs)) detail-groups)))
          (setq detail-groups (reverse detail-groups))

          ;; Нумерация групп для DETAIL
          (setq indexed-detail '() groupIndex 0)
          (foreach grp detail-groups
            (setq groupIndex (1+ groupIndex))
            (setq indexed-detail (append indexed-detail (list (list groupIndex (car grp) (cdr grp)))))
          )

          ;; Группировка для SUMMARY (по имени)
          (setq summary-groups '())
          (foreach grp detail-groups
            (setq name (car grp)
                  recs (cdr grp)
                  totalCount 0
                  totalSum 0.0)
            (foreach rec recs
              (setq totalCount (+ totalCount (caddr rec))
                    totalSum   (+ totalSum (/ (* (cadr rec) (caddr rec)) 1000.0)))
            )
            (setq summary-groups (append summary-groups (list (list name totalCount totalSum))))
          )

          ;; Определяем данные для отчёта
          (if (= (strcase report-mode) "SUMMARY")
            (setq report-data summary-groups
                  report-type "SUMMARY")
            (setq report-data indexed-detail
                  report-type "DETAIL")
          )

          ;; Вычисляем сводку
          (setq total-blocks 0)
          (foreach rec acc (setq total-blocks (+ total-blocks (caddr rec))))

          (if (= report-type "DETAIL")
            (progn
              (setq total-pos (length acc))
              (setq total-types (length summary-groups))
              (setq total-sum 0.0)
              (foreach rec acc (setq total-sum (+ total-sum (/ (* (cadr rec) (caddr rec)) 1000.0))))
              (princ (strcat "\nФасонка: найдено блоков " (itoa total-blocks)
                             ", позиций " (itoa total-pos)
                             ", общий погонаж " (rtos total-sum 2 2) " м.п."))
            )
            (progn
              (setq total-types (length summary-groups))
              (setq total-sum 0.0)
              (foreach g summary-groups (setq total-sum (+ total-sum (caddr g))))
              (princ (strcat "\nФасонка: найдено блоков " (itoa total-blocks)
                             ", типов " (itoa total-types)
                             ", общий погонаж " (rtos total-sum 2 2) " м.п."))
            )
          )

          ;; ==================== Формирование имён файлов ====================
          (if (null save-base)
            (setq base-name (strcat (getvar "dwgprefix")
                                    (vl-filename-base (getvar "dwgname"))
                                    " Фасонка"))
            (setq base-name save-base)
          )

          ;; ==================== Экспорт в Excel / CSV ====================
          (if export-excel
            (progn
              (setq xlsfile (strcat base-name ".xls"))
              (if (= report-type "DETAIL")
                (if (save-xls report-data xlsfile)
                  (princ (strcat "\nXLS сохранён: " xlsfile))
                  ;; CSV fallback для DETAIL
                  (progn
                    (princ "\nНе удалось сохранить XLS. Сохраняю CSV...")
                    (setq csvfile (strcat base-name ".csv"))
                    (setq f (open csvfile "w"))
                    (if f
                      (progn
                        (write-line "Фасонное железо" f)
                        (write-line "№;Тип фасонки;Длина, мм;Кол-во, шт.;Сумма, м.п." f)
                        (foreach ig report-data
                          (setq groupIndex (car ig) name (cadr ig) recs (caddr ig) itemNum 0 totalSum 0.0)
                          (foreach rec recs
                            (setq itemNum (1+ itemNum)
                                  len (cadr rec)
                                  count (caddr rec)
                                  sum (/ (* len count) 1000.0)
                                  totalSum (+ totalSum sum))
                            (write-line (strcat (itoa groupIndex) "." (itoa itemNum) ";" name ";" (rtos len 2 0) ";" (itoa count) ";" (rtos sum 2 2)) f)
                          )
                          (write-line (strcat (itoa groupIndex) ";" name ";;;" (rtos totalSum 2 2)) f)
                        )
                        (close f)
                        (princ (strcat "\nCSV сохранён: " csvfile))
                      )
                      (princ "\nНе удалось открыть CSV-файл.")
                    )
                  )
                )
                ;; SUMMARY
                (if (save-xls-summary report-data xlsfile)
                  (princ (strcat "\nXLS сохранён: " xlsfile))
                  ;; CSV fallback для SUMMARY
                  (progn
                    (princ "\nНе удалось сохранить XLS. Сохраняю CSV...")
                    (setq csvfile (strcat base-name ".csv"))
                    (setq f (open csvfile "w"))
                    (if f
                      (progn
                        (write-line "Фасонное железо" f)
                        (write-line "№;Тип фасонки;Кол-во, шт.;Сумма, м.п." f)
                        (setq i 0)
                        (foreach rec report-data
                          (setq i (1+ i)
                                name (car rec)
                                count (cadr rec)
                                sum (caddr rec))
                          (write-line (strcat (itoa i) ";" name ";" (itoa count) ";" (rtos sum 2 2)) f)
                        )
                        (setq totalCount 0 totalSum 0.0)
                        (foreach rec report-data (setq totalCount (+ totalCount (cadr rec)) totalSum (+ totalSum (caddr rec))))
                        (write-line (strcat "Итого;;" (itoa totalCount) ";" (rtos totalSum 2 2)) f)
                        (close f)
                        (princ (strcat "\nCSV сохранён: " csvfile))
                      )
                      (princ "\nНе удалось открыть CSV-файл.")
                    )
                  )
                )
              )
            )
          )

          ;; ==================== Экспорт в GAL (TXT) ====================
          (if export-txt
            (tx-export-gal report-type report-data base-name)
          )

          ;; ==================== Создание таблиц AutoCAD ====================
          (if create-table
            (tbl-create-report report-type report-data)
          )
        )
        (princ "\nБлоки со свойством 'Длина' не найдены.")
      )
    )
    (princ "\nОбъекты не найдены.")
  )
  (princ)
)

;; ==================== Интерактивные команды ====================
(defun c:fasonka ()
  (setq layers-str (getstring "\nВведите слои через запятую (Enter — все слои): "))
  (if (= layers-str "")
    (setq layers nil)
    (setq layers (mapcar 'strcase (split-string layers-str ",")))
  )

  (initget "D S")
  (setq report-mode (getkword "\nРежим отчёта [Подробный(D)/Краткий(S)] <D>: "))
  (if (null report-mode) (setq report-mode "D"))
  (setq report-mode (if (= report-mode "D") "DETAIL" "SUMMARY"))

  (initget "Y N")
  (setq export-excel (getkword "\nЭкспорт в Excel? [Да(Y)/Нет(N)] <N>: "))
  (if (or (null export-excel) (= export-excel "N")) (setq export-excel nil) (setq export-excel T))

  (initget "Y N")
  (setq export-txt (getkword "\nЭкспорт в TXT (GAL)? [Да(Y)/Нет(N)] <N>: "))
  (if (or (null export-txt) (= export-txt "N")) (setq export-txt nil) (setq export-txt T))

  (initget "Y N")
  (setq create-table (getkword "\nСоздать таблицу AutoCAD? [Да(Y)/Нет(N)] <Y>: "))
  (if (or (null create-table) (= create-table "Y")) (setq create-table T) (setq create-table nil))

  (initget "Y N")
  (setq use-default (getkword "\nИспользовать путь по умолчанию? [Да(Y)/Нет(N)] <Y>: "))
  (if (or (null use-default) (= use-default "Y"))
    (setq save-base nil)
    (setq save-base (getstring "\nБазовое имя файла (без расширения): "))
  )

  (fasonka-main layers report-mode export-excel export-txt create-table save-base)
  (princ)
)

(defun c:Фасонка ()
  (c:fasonka)
)

;; Вспомогательная функция разбиения строки
(defun split-string (str delim / pos result)
  (setq result '())
  (while (setq pos (vl-string-search delim str))
    (setq result (append result (list (substr str 1 pos))))
    (setq str (substr str (+ pos 2)))
  )
  (setq result (append result (list str)))
  result
)

(princ "\nКоманды: FASONKA, ФАСОНКА")
(princ)