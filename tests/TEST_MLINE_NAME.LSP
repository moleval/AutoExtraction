;;; ================================================================
;;; TEST_MLINE_NAME.LSP  (rev 2)
;;; Диагностика названия MLINE и её MLINESTYLE
;;; AutoCAD 2016+ / BricsCAD / nanoCAD
;;;
;;; Команда: TESTMLINENAME
;;; ================================================================

(vl-load-com)


;;; ------------------------------------------------
;;; Безопасный вызов
;;; ------------------------------------------------
(defun test-ml-safe (func args / r)
  (setq r (vl-catch-all-apply func args))
  (if (vl-catch-all-error-p r) nil r)
)


;;; ------------------------------------------------
;;; Печать значения
;;; ------------------------------------------------
(defun test-ml-print-value (label value)
  (princ
    (strcat
      "\n" label ": "
      (cond
        ((null value) "<нет>")
        ((= (type value) 'VLA-OBJECT)
          (strcat "#<VLA-OBJECT " (vl-princ-to-string value) ">"))
        (T (vl-princ-to-string value))
      )
    )
  )
)


;;; ------------------------------------------------
;;; MLINESTYLE через Named Object Dictionary
;;; ------------------------------------------------
(defun test-ml-style-via-dict (name / dict dict-ename entry)
  (princ "\n--- через ACAD_MLINESTYLE dictionary ---")

  (setq dict (test-ml-safe 'dictsearch (list (namedobjdict) "ACAD_MLINESTYLE")))
  (if (null dict)
    (princ "\n  Словарь ACAD_MLINESTYLE не найден.")
    (progn
      (setq dict-ename (cdr (assoc -1 dict)))
      (setq entry (test-ml-safe 'dictsearch (list dict-ename name)))
      (if (null entry)
        (princ (strcat "\n  Стиль \"" name "\" в словаре не найден."))
        (progn
          (princ "\n  Запись найдена:")
          (foreach item entry
            (princ
              (strcat
                "\n    DXF "
                (vl-princ-to-string (car item))
                " : "
                (vl-princ-to-string (cdr item))
              )
            )
          )
          ;; Переход к самому объекту стиля через 350 (ename)
          (setq entry-ename (cdr (assoc 350 entry)))
          (if entry-ename
            (progn
              (princ "\n  --- DXF объекта MLINESTYLE ---")
              (foreach item (entget entry-ename)
                (princ
                  (strcat
                    "\n    DXF "
                    (vl-princ-to-string (car item))
                    " : "
                    (vl-princ-to-string (cdr item))
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)


;;; ------------------------------------------------
;;; MLINESTYLE через ActiveX (MLineStyles collection)
;;; ------------------------------------------------
(defun test-ml-style-via-activex (name / doc mstyles ms)
  (princ "\n--- через ActiveX (MLineStyles) ---")

  (setq doc (test-ml-safe 'vla-get-ActiveDocument
                          (list (vlax-get-acad-object))))
  (if (null doc)
    (princ "\n  Нет ActiveDocument.")
    (progn
      (setq mstyles (test-ml-safe 'vla-get-MLineStyles (list doc)))
      (if (null mstyles)
        (princ "\n  Коллекция MLineStyles недоступна.")
        (progn
          (setq ms (test-ml-safe 'vla-item (list mstyles name)))
          (if (null ms)
            (princ (strcat "\n  Стиль \"" name "\" не найден."))
            (progn
              (princ "\n  Свойства MLINESTYLE:")
              (foreach prop
                '("Name" "Description" "Style"
                  "StartSquareCap" "StartRoundCap" "StartInnerArcs"
                  "EndSquareCap" "EndRoundCap" "EndInnerArcs"
                  "FillOn" "FillColor" "ShowMiters")
                (test-ml-print-value
                  (strcat "  " prop)
                  (test-ml-safe 'vlax-get-property (list ms prop))
                )
              )
            )
          )
        )
      )
    )
  )
)


;;; ------------------------------------------------
;;; Разбор MLINESTYLE (обе ветки)
;;; ------------------------------------------------
(defun test-ml-print-mlinestyle (style-name)
  (princ "\n")
  (princ "\n==============================================")
  (princ "\nMLINESTYLE")
  (princ "\n==============================================")

  (if (not style-name)
    (princ "\n<имя стиля отсутствует>")
    (progn
      (test-ml-print-value "Имя стиля (DXF 2)" style-name)
      (test-ml-style-via-dict style-name)
      (test-ml-style-via-activex style-name)
    )
  )
)


;;; ------------------------------------------------
;;; Dump всех ActiveX-свойств MLINE
;;; ------------------------------------------------
(defun test-ml-dump-mline (obj)
  (princ "\n")
  (princ "\n==============================================")
  (princ "\nВСЕ ACTIVEX-СВОЙСТВА MLINE (vlax-dump-object)")
  (princ "\n==============================================")
  (princ "\n")
  (vlax-dump-object obj T)
)


;;; ------------------------------------------------
;;; DXF примитива
;;; ------------------------------------------------
(defun test-ml-print-dxf (ent / data)
  (setq data (entget ent))
  (princ "\n")
  (princ "\n==============================================")
  (princ "\nDXF-ДАННЫЕ ПРИМИТИВА")
  (princ "\n==============================================")
  (foreach item data
    (princ
      (strcat "\n"
        (vl-princ-to-string (car item)) " : "
        (vl-princ-to-string (cdr item))))
  )
)


;;; ------------------------------------------------
;;; Строковые DXF-значения
;;; ------------------------------------------------
(defun test-ml-print-strings (ent / data)
  (setq data (entget ent))
  (princ "\n")
  (princ "\n==============================================")
  (princ "\nСТРОКОВЫЕ DXF-ЗНАЧЕНИЯ")
  (princ "\n==============================================")
  (foreach item data
    (if (eq (type (cdr item)) 'STR)
      (princ (strcat "\nDXF "
        (vl-princ-to-string (car item)) " = \""
        (cdr item) "\""))
    )
  )
)


;;; ------------------------------------------------
;;; XData
;;; ------------------------------------------------
(defun test-ml-print-xdata (ent / all item)
  (princ "\n")
  (princ "\n==============================================")
  (princ "\nXDATA")
  (princ "\n==============================================")
  (setq all (entget ent '("*")))
  (if (null all)
    (princ "\n<нет>")
    (progn
      (foreach item all
        (if (= (car item) -3)
          (princ (strcat "\nAppName: " (cdr item))))
      )
    )
  )
)


;;; ------------------------------------------------
;;; Основная команда
;;; ------------------------------------------------
(defun c:TESTMLINENAME (/ ent obj dxf2)
  (princ "\n")
  (princ "\n==============================================")
  (princ "\nТЕСТ НАЗВАНИЯ MLINE")
  (princ "\n==============================================")

  (setq ent (car (entsel "\nВыберите MLINE: ")))

  (cond
    ((null ent)
      (princ "\nОтмена."))

    ((/= (cdr (assoc 0 (entget ent))) "MLINE")
      (princ (strcat "\nНе MLINE: "
        (cdr (assoc 0 (entget ent))))))

    (T
      (setq obj (vlax-ename->vla-object ent))
      (setq dxf2 (cdr (assoc 2 (entget ent))))

      (princ "\n----------------------------------------------")
      (princ "\nОСНОВНОЕ")
      (princ "\n----------------------------------------------")
      (test-ml-print-value "Handle" (cdr (assoc 5 (entget ent))))
      (test-ml-print-value "Layer"  (cdr (assoc 8 (entget ent))))
      (test-ml-print-value "ObjectName"
        (test-ml-safe 'vlax-get-property (list obj "ObjectName")))
      (test-ml-print-value "MLINESTYLE (DXF 2) <- ИМЯ" dxf2)
      (test-ml-print-value "Linetype"
        (test-ml-safe 'vlax-get-property (list obj "Linetype")))

      (test-ml-print-mlinestyle dxf2)
      (test-ml-dump-mline obj)
      (test-ml-print-dxf ent)
      (test-ml-print-strings ent)
      (test-ml-print-xdata ent)

      (princ "\n")
      (princ "\n==============================================")
      (princ "\nТЕСТ ЗАВЕРШЕН")
      (princ "\n==============================================")
      (princ "\n")
    )
  )
  (princ)
)


(princ "\nTEST_MLINE_NAME.LSP (rev 2) загружен.")
(princ "\nКоманда: TESTMLINENAME")
(princ)