;;; ============================================================
;;; ZAPOLNENIE.LSP
;;; Извлечение данных о заполнении проёмов
;;; (стеклопакеты, сэндвич-панели, глухие панели, витражное стекло)
;;;
;;; Основная величина: ПЛОЩАДЬ (м2), количество (шт.)
;;; ТИП = состояние видимости блока, иначе — EffectiveName
;;; Высота/Ширина — из динамических свойств блока
;;;
;;; Режимы: DETAIL / SUMMARY
;;; Экспорт: XLS/CSV (common/excel-utils.lsp),
;;;          GAL (common/txt-utils.lsp),
;;;          таблица AutoCAD
;;;
;;
;;; ТЕХНИЧЕСКИЕ ПРАВИЛА (согласованы на Этапе 0):
;;;   - Видимость отсутствует -> EffectiveName, НЕ пропускать
;;;   - Высота: приоритет "ВЫСОТА В СВЕТУ" -> "ВЫСОТА"
;;;   - Ширина: приоритет "ШИРИНА В СВЕТУ" -> "ШИРИНА" -> "ДЛИНА"
;;;   - Поиск свойства: сначала точное совпадение, затем подстрока
;;;   - Припуск: +26 мм (константа)
;;;   - Округление размеров: fix (до целых мм)
;;;   - Нулевой размер: пропуск
;;;   - Площадь: мм2 -> м2, НЕ округлять до агрегации
;;;   - Округление площади: только при выводе (rtos 2 2)
;;;   - DETAIL ключ: UPPERCASE(ТИП) + ВЫСОТА + ШИРИНА
;;;   - Отображаемое имя: из первой записи группы
;;;   - SUMMARY: строить из DETAIL (один проход)
;;;   - Сортировка DETAIL: Тип -> Высота -> Ширина
;;;   - Сортировка SUMMARY: Тип (алфавит)
;;; ============================================================

(vl-load-com)

;; ============================================================
;; КОНСТАНТЫ МОДУЛЯ
;; ============================================================

;; Припуск на рамку/профиль, мм (применяется к высоте и ширине)
(setq *ZAPOLNENIE-FRAME-ALLOWANCE* 26)

;; Размер листа для экспорта в GAL
;; Используется латинская 'x' для корректной записи в файл
(setq *ZAPOLNENIE-SHEET-SIZE* "3210x2250")

;; Флаг вращения панелей при раскрое (для GAL)
(setq *ZAPOLNENIE-ROTATE* "вращать")

;; Приоритетные списки ключевых слов для поиска свойств
;; Высота: сначала "ВЫСОТА В СВЕТУ", затем "ВЫСОТА"
(setq *ZAPOLNENIE-HEIGHT-KEYWORDS* '("ВЫСОТА В СВЕТУ" "ВЫСОТА"))
;; Ширина: сначала "ШИРИНА В СВЕТУ", затем "ШИРИНА", затем "ДЛИНА"
(setq *ZAPOLNENIE-WIDTH-KEYWORDS* '("ШИРИНА В СВЕТУ" "ШИРИНА" "ДЛИНА"))


;; ============================================================
;; ИМЯ ЭЛЕМЕНТА (ТИП)
;; ============================================================
;; Если есть видимость — используем её.
;; Если видимости нет — используем EffectiveName (НЕ пропускаем).
;; ============================================================
(defun zapolnenie-element-name (obj / effname vis)
  (setq effname (su-get-effective-name obj))
  (setq vis (su-get-visibility obj))
  (if (and vis (/= vis ""))
    vis
    (if effname
      effname
      "Без имени"
    )
  )
)


;; ============================================================
;; ПОИСК СВОЙСТВА ПО КЛЮЧЕВОМУ СЛОВУ
;; ============================================================
;; Двухступенчатый поиск:
;;   1. Сначала точное совпадение имени свойства
;;   2. Если не найдено — поиск по подстроке
;; Это защищает от ложных срабатываний
;; (например, "Высота в свету 2", "Высота рамки").
;; ============================================================
(defun zapolnenie-find-property (obj keyword / dynprops prop pname value result)
  (setq dynprops (vl-catch-all-apply 'vlax-invoke (list obj 'GetDynamicBlockProperties)))
  (if (vl-catch-all-error-p dynprops)
    nil
    (progn
      (setq result nil)

      ;; Шаг 1: точное совпадение
      (foreach prop dynprops
        (if (null result)
          (progn
            (setq pname (vl-catch-all-apply 'vla-get-PropertyName (list prop)))
            (if (and (not (vl-catch-all-error-p pname))
                     pname
                     (= (type pname) 'STR)
                     (= (strcase pname) (strcase keyword)))
              (progn
                (setq value (vl-catch-all-apply 'vla-get-Value (list prop)))
                (if (not (vl-catch-all-error-p value))
                  (setq result (su-value-to-number value))
                )
              )
            )
          )
        )
      )

      ;; Шаг 2: поиск по подстроке (если точное совпадение не найдено)
      (if (null result)
        (foreach prop dynprops
          (if (null result)
            (progn
              (setq pname (vl-catch-all-apply 'vla-get-PropertyName (list prop)))
              (if (and (not (vl-catch-all-error-p pname))
                       pname
                       (= (type pname) 'STR)
                       (vl-string-search (strcase keyword) (strcase pname)))
                (progn
                  (setq value (vl-catch-all-apply 'vla-get-Value (list prop)))
                  (if (not (vl-catch-all-error-p value))
                    (setq result (su-value-to-number value))
                  )
                )
              )
            )
          )
        )
      )

      result
    )
  )
)


;; ============================================================
;; ИЗВЛЕЧЕНИЕ РАЗМЕРА ПО ПРИОРИТЕТНОМУ СПИСКУ КЛЮЧЕВЫХ СЛОВ
;; ============================================================
;; Ищет первое совпадение в порядке приоритета.
;; Например, для ширины: "ШИРИНА В СВЕТУ" -> "ШИРИНА" -> "ДЛИНА"
;; ============================================================
(defun zapolnenie-get-dimension (obj keywords-priority / result kw)
  (setq result nil)
  (foreach kw keywords-priority
    (if (null result)
      (setq result (zapolnenie-find-property obj kw))
    )
  )
  result
)


;; ============================================================
;; ВЫЧИСЛЕНИЕ ВЫСОТЫ И ШИРИНЫ С ПРИПУСКОМ
;; ============================================================
;; Последовательность: свойство -> числовое значение -> +26 -> fix
;; Возвращает (высота-мм ширина-мм) или nil, если размеры не найдены
;; или равны нулю.
;; ============================================================
(defun zapolnenie-compute-dims (obj / raw-h raw-w h w)
  ;; Извлекаем высоту по приоритетному списку
  (setq raw-h (zapolnenie-get-dimension obj *ZAPOLNENIE-HEIGHT-KEYWORDS*))
  ;; Извлекаем ширину по приоритетному списку (включая "ДЛИНА")
  (setq raw-w (zapolnenie-get-dimension obj *ZAPOLNENIE-WIDTH-KEYWORDS*))

  ;; Проверяем наличие и положительность обоих размеров
  (if (and raw-h raw-w (> raw-h 0.0) (> raw-w 0.0))
    (progn
      ;; Применяем припуск и округляем до целых мм
      (setq h (fix (+ raw-h *ZAPOLNENIE-FRAME-ALLOWANCE*)))
      (setq w (fix (+ raw-w *ZAPOLNENIE-FRAME-ALLOWANCE*)))
      (list h w)
    )
    nil
  )
)


;; ============================================================
;; СОРТИРОВКА
;; ============================================================
;; DETAIL: Тип -> Высота -> Ширина
;; Сравнение без учёта регистра для типа.
;; ============================================================
(defun zapolnenie-sort-less (a b / ta tb ha hb wa wb)
  (setq ta (strcase (car a))
        tb (strcase (car b))
        ha (cadr a)
        hb (cadr b)
        wa (caddr a)
        wb (caddr b))
  (cond
    ((< ta tb) T)
    ((> ta tb) nil)
    ;; Типы равны — сравниваем высоту
    ((< ha hb) T)
    ((> ha hb) nil)
    ;; Высота равна — сравниваем ширину
    ((< wa wb) T)
    ((> wa wb) nil)
    (T nil)
  )
)


;; ============================================================
;; АГРЕГАЦИЯ
;; ============================================================
;; Объединяет блоки с одинаковыми (ТИП, ВЫСОТА, ШИРИНА).
;; Ключ агрегации: UPPERCASE(ТИП) + ВЫСОТА + ШИРИНА
;; Отображаемое имя сохраняется из первой записи группы.
;;
;; Возвращает отсортированный список:
;;   (тип высота-мм ширина-мм количество)
;; ============================================================
(defun zapolnenie-aggregate (inserts / i ent obj name dims h w key rec found acc display-names)
  (setq acc '()
        display-names '()
        i 0)

  (repeat (length inserts)
    (setq ent (nth i inserts)
          obj (vlax-ename->vla-object ent))

    ;; Получаем имя (видимость или EffectiveName)
    (setq name (zapolnenie-element-name obj))

    ;; Извлекаем размеры с припуском
    (setq dims (zapolnenie-compute-dims obj))

    (if dims
      (progn
        (setq h (car dims)
              w (cadr dims))

        ;; Ключ агрегации: нормализованное имя + размеры
        (setq key (strcat (strcase name) "|" (itoa h) "|" (itoa w)))

        ;; Ищем существующую запись
        (setq found (assoc key acc))

        (if found
          ;; Увеличиваем количество
          (setq acc (subst
                      (list key (cadr found) (caddr found) (cadddr found) (1+ (cadddr found)))
                      found
                      acc))
          ;; Создаём новую запись
          ;; Сохраняем отображаемое имя из первой записи
          (progn
            (if (not (assoc (strcase name) display-names))
              (setq display-names (cons (cons (strcase name) name) display-names))
            )
            (setq acc (cons (list key name h w 1) acc))
          )
        )
      )
    )

    (setq i (1+ i))
  )

  ;; Сортировка: Тип -> Высота -> Ширина
  (setq acc (vl-sort acc 'zapolnenie-sort-less))

  ;; Возвращаем список без ключа: (тип высота ширина количество)
  (mapcar '(lambda (r) (list (cadr r) (caddr r) (cadddr r) (cadddr (cdr r)))) acc)
)


;; ============================================================
;; SUMMARY ИЗ DETAIL (один проход)
;; ============================================================
;; Агрегирует DETAIL-данные по типу.
;; Площадь считается как сумма (кол-во ? площадь_панели) по позициям.
;;
;; Вход:  (тип высота-мм ширина-мм количество)
;; Выход: (тип количество площадь-м2)
;; ============================================================
(defun zapolnenie-build-summary (data / summary rec name h w cnt area key found)
  (setq summary '())

  (foreach rec data
    (setq name (car rec)
          h    (cadr rec)
          w    (caddr rec)
          cnt  (cadddr rec)
          ;; Площадь панели в м2 (не округляется до агрегации)
          area (/ (* h w cnt) 1000000.0)
          key  (strcase name))

    ;; Ищем существующую запись по нормализованному имени
    (setq found (assoc key summary))

    (if found
      ;; Увеличиваем количество И площадь
      (setq summary
        (subst
          (list key (cadr found) (+ (caddr found) cnt) (+ (cadddr found) area))
          found
          summary))
      ;; Создаём новую запись (отображаемое имя из первой записи)
      (setq summary (cons (list key name cnt area) summary))
    )
  )

  ;; Сортировка по типу (алфавит)
  (setq summary (vl-sort summary '(lambda (a b) (< (car a) (car b)))))

  ;; Возвращаем список без ключа: (тип количество площадь-м2)
  (mapcar '(lambda (r) (list (cadr r) (caddr r) (cadddr r))) summary)
)


;; ============================================================
;; ТАБЛИЦА AUTOCAD — DETAIL
;; ============================================================
;; С группировкой по типам и подитогами групп.
;; Наименования подитогов — с подчёркиванием (формат {\L...}),
;; как в Подсистеме.
;; Конкретная реализация для Заполнения (без преждевременной
;; генерализации, согласно решению Этапа 0).
;; ============================================================
(defun zapolnenie-create-table-detail (data / pt tbl row nRows nCols space
                                        rec tip h w cnt area itemNum
                                        total-cnt total-area
                                        groups grp grpName grpRows grpCnt grpArea
                                        startRow endRow)
  (setq pt (getpoint "\nУкажите точку вставки таблицы: "))

  (if pt
    (progn
      (setvar "CMDECHO" 0)

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

      (setq nCols 6)
      ;; Заголовок + шапка + данные + подитоги групп + общий итог
      (setq nRows (+ 3 (length data) (length groups)))

      (setq space (vla-get-modelspace
                    (vla-get-activedocument (vlax-get-acad-object))))

      (setq tbl (vla-addtable space (vlax-3d-point pt) nRows nCols 10.0 50.0))

      ;; Ширины колонок
      (vla-SetColumnWidth tbl 0 15.0)   ; №
      (vla-SetColumnWidth tbl 1 75.0)  ; Тип
      (vla-SetColumnWidth tbl 2 30.0)   ; Высота
      (vla-SetColumnWidth tbl 3 30.0)   ; Ширина
      (vla-SetColumnWidth tbl 4 30.0)   ; Кол-во
      (vla-SetColumnWidth tbl 5 35.0)   ; Площадь

      ;; Заголовок таблицы
      (vla-MergeCells tbl 0 0 0 5)
      (vla-SetText tbl 0 0 "{\\LЗаполнение}")

      ;; Шапка колонок
      (vla-SetText tbl 1 0 "№")
      (vla-SetText tbl 1 1 "Тип")
      (vla-SetText tbl 1 2 "Высота, мм")
      (vla-SetText tbl 1 3 "Ширина, мм")
      (vla-SetText tbl 1 4 "Кол-во, шт.")
      (vla-SetText tbl 1 5 "Площадь, м2")

      ;; Выравнивание шапки
      (vla-SetCellAlignment tbl 1 0 5)
      (vla-SetCellAlignment tbl 1 1 5)
      (vla-SetCellAlignment tbl 1 2 5)
      (vla-SetCellAlignment tbl 1 3 5)
      (vla-SetCellAlignment tbl 1 4 5)
      (vla-SetCellAlignment tbl 1 5 5)

      ;; Вывод по группам с подитогами
      (setq row 2 itemNum 0 total-cnt 0 total-area 0.0)

      (foreach grp groups
        (setq grpName (car grp)
              grpRows (cdr grp)
              grpCnt  0
              grpArea 0.0)

        ;; Строки группы
        (foreach rec grpRows
          (setq rec (car rec))
          (setq itemNum (1+ itemNum)
                tip (car rec)
                h   (cadr rec)
                w   (caddr rec)
                cnt (cadddr rec)
                area (/ (* h w cnt) 1000000.0))
          (setq grpCnt (+ grpCnt cnt)
                grpArea (+ grpArea area)
                total-cnt (+ total-cnt cnt)
                total-area (+ total-area area))

          (vla-SetText tbl row 0 (itoa itemNum))
          (vla-SetText tbl row 1 tip)
          (vla-SetText tbl row 2 (itoa h))
          (vla-SetText tbl row 3 (itoa w))
          (vla-SetText tbl row 4 (itoa cnt))
          (vla-SetText tbl row 5 (rtos area 2 2))

          ;; Выравнивание: Тип — влево, остальное — по центру
          (vla-SetCellAlignment tbl row 0 5)
          (vla-SetCellAlignment tbl row 1 4)
          (vla-SetCellAlignment tbl row 2 5)
          (vla-SetCellAlignment tbl row 3 5)
          (vla-SetCellAlignment tbl row 4 5)
          (vla-SetCellAlignment tbl row 5 5)

          (setq row (1+ row))
        )

        ;; Подитог группы (с подчёркиванием наименования, как в Подсистеме)
        (vla-MergeCells tbl row row 1 3)
        (vla-SetText tbl row 0 "")
        (vla-SetText tbl row 1 (strcat "   {\\L" grpName "}"))
        (vla-SetText tbl row 4 (itoa grpCnt))
        (vla-SetText tbl row 5 (rtos grpArea 2 2))
        (vla-SetCellAlignment tbl row 0 5)
        (vla-SetCellAlignment tbl row 1 4)
        (vla-SetCellAlignment tbl row 4 5)
        (vla-SetCellAlignment tbl row 5 5)

        (setq row (1+ row))
      )

      ;; Общий итог (с подчёркиванием)
      (vla-MergeCells tbl row row 0 3)
      (vla-SetText tbl row 0 "{\\LИтого}")
      (vla-SetText tbl row 4 (itoa total-cnt))
      (vla-SetText tbl row 5 (rtos total-area 2 2))
      (vla-SetCellAlignment tbl row 0 5)
      (vla-SetCellAlignment tbl row 4 5)
      (vla-SetCellAlignment tbl row 5 5)

      (vla-update tbl)
      (setvar "CMDECHO" 1)

      tbl
    )
  )
)


;; ============================================================
;; ТАБЛИЦА AUTOCAD — SUMMARY
;; ============================================================
(defun zapolnenie-create-table-summary (data / pt tbl row nRows nCols space
                                         rec tip cnt area
                                         total-cnt total-area)
  (setq pt (getpoint "\nУкажите точку вставки таблицы: "))

  (if pt
    (progn
      (setvar "CMDECHO" 0)

      (setq nCols 4)
      ;; Заголовок + шапка + количество записей + итог
      (setq nRows (+ 3 (length data)))

      (setq space (vla-get-modelspace
                    (vla-get-activedocument (vlax-get-acad-object))))

      (setq tbl (vla-addtable space (vlax-3d-point pt) nRows nCols 10.0 50.0))

      ;; Ширины колонок
      (vla-SetColumnWidth tbl 0 15.0)   ; №
      (vla-SetColumnWidth tbl 1 75.0)  ; Тип
      (vla-SetColumnWidth tbl 2 30.0)   ; Кол-во
      (vla-SetColumnWidth tbl 3 35.0)   ; Площадь

      ;; Заголовок таблицы
      (vla-MergeCells tbl 0 0 0 3)
      (vla-SetText tbl 0 0 "{\\LЗаполнение}")

      ;; Шапка колонок
      (vla-SetText tbl 1 0 "№")
      (vla-SetText tbl 1 1 "Тип")
      (vla-SetText tbl 1 2 "Кол-во, шт.")
      (vla-SetText tbl 1 3 "Площадь, м2")

      ;; Выравнивание шапки
      (vla-SetCellAlignment tbl 1 0 5)
      (vla-SetCellAlignment tbl 1 1 5)
      (vla-SetCellAlignment tbl 1 2 5)
      (vla-SetCellAlignment tbl 1 3 5)

      ;; Данные
      (setq row 2 total-cnt 0 total-area 0.0)
      (foreach rec data
        (setq tip (car rec)
              cnt (cadr rec)
              area (caddr rec))

        ;; Защита от nil (на случай некорректных данных)
        (if (null cnt) (setq cnt 0))
        (if (null area) (setq area 0.0))

        (setq total-cnt (+ total-cnt cnt)
              total-area (+ total-area area))

        (vla-SetText tbl row 0 (itoa (1+ (- row 2))))
        (vla-SetText tbl row 1 tip)
        (vla-SetText tbl row 2 (itoa cnt))
        (vla-SetText tbl row 3 (rtos area 2 2))

        ;; Выравнивание: Тип — влево, остальное — по центру
        (vla-SetCellAlignment tbl row 0 5)
        (vla-SetCellAlignment tbl row 1 4)
        (vla-SetCellAlignment tbl row 2 5)
        (vla-SetCellAlignment tbl row 3 5)

        (setq row (1+ row))
      )

      ;; Итоговая строка (с подчёркиванием)
      (vla-MergeCells tbl row row 0 1)
      (vla-SetText tbl row 0 "{\\LИтого}")
      (vla-SetText tbl row 2 (itoa total-cnt))
      (vla-SetText tbl row 3 (rtos total-area 2 2))
      (vla-SetCellAlignment tbl row 0 5)
      (vla-SetCellAlignment tbl row 2 5)
      (vla-SetCellAlignment tbl row 3 5)

      (vla-update tbl)
      (setvar "CMDECHO" 1)

      tbl
    )
  )
)


;; ============================================================
;; ОСНОВНАЯ ФУНКЦИЯ
;; ============================================================
(defun zapolnenie-main (layers report-mode export-excel export-txt create-table save-base
                        / *error*
                          inserts data summary-data
                          base-name xlsfile csvfile
                          total-count total-area
                          rec)
  (vl-load-com)
  (sssetfirst nil nil)

  ;; Обработчик ошибок
  (defun *error* (msg)
    (if (and msg
             (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\nОшибка: " msg))
    )
    (princ)
  )

  ;; Выбор блоков через общую утилиту (учитывает предварительный выбор и слои)
  (setq inserts (su-select-inserts layers))

  (if inserts
    (progn
      ;; Агрегация данных
      (setq data (zapolnenie-aggregate inserts))

      (if data
        (progn
          ;; Базовое имя файла
          (if (null save-base)
            (setq base-name
              (strcat
                (getvar "dwgprefix")
                (vl-filename-base (getvar "dwgname"))
                " Заполнение "
                (if (= (strcase report-mode) "DETAIL") "подробный" "краткий")))
            (setq base-name
              (strcat
                save-base " "
                (if (= (strcase report-mode) "DETAIL") "подробный" "краткий")))
          )

          ;; Построение SUMMARY из DETAIL (один проход)
          (setq summary-data (zapolnenie-build-summary data))

          ;; ================================================
          ;; ЭКСПОРТ В EXCEL / CSV (с переключением при недоступности)
          ;; ================================================
          (if export-excel
            (progn
              (setq xlsfile (strcat base-name ".xls"))

              (if (= (strcase report-mode) "DETAIL")
                ;; DETAIL
                (if (eu-export-zapolnenie-detail data xlsfile)
                  (princ (strcat "\nXLS сохранён: " xlsfile))
                  (progn
                    (princ "\nНе удалось сохранить XLS. Сохраняю CSV...")
                    (setq csvfile (strcat base-name ".csv"))
                    (if (eu-export-zapolnenie-csv-detail data csvfile)
                      (princ (strcat "\nCSV сохранён: " csvfile))
                      (princ "\nНе удалось создать CSV.")
                    )
                  )
                )
                ;; SUMMARY
                (if (eu-export-zapolnenie-summary summary-data xlsfile)
                  (princ (strcat "\nXLS сохранён: " xlsfile))
                  (progn
                    (princ "\nНе удалось сохранить XLS. Сохраняю CSV...")
                    (setq csvfile (strcat base-name ".csv"))
                    (if (eu-export-zapolnenie-csv-summary summary-data csvfile)
                      (princ (strcat "\nCSV сохранён: " csvfile))
                      (princ "\nНе удалось создать CSV.")
                    )
                  )
                )
              )
            )
          )

          ;; ================================================
          ;; ЭКСПОРТ В GAL
          ;; ================================================
          (if export-txt
            (tx-export-gal-zapolnenie data base-name
                                      *ZAPOLNENIE-SHEET-SIZE*
                                      *ZAPOLNENIE-ROTATE*)
          )

          ;; ================================================
          ;; ТАБЛИЦА AUTOCAD
          ;; ================================================
          (if create-table
            (if (= (strcase report-mode) "DETAIL")
              (zapolnenie-create-table-detail data)
              (zapolnenie-create-table-summary summary-data)
            )
          )

          ;; ================================================
          ;; ИТОГОВОЕ СООБЩЕНИЕ
          ;; ================================================
          (setq total-count 0 total-area 0.0)
          (foreach rec data
            (setq total-count (+ total-count (cadddr rec))
                  total-area (+ total-area (/ (* (cadr rec) (caddr rec) (cadddr rec)) 1000000.0)))
          )

          (princ (strcat
                   "\nЗаполнение: элементов " (itoa total-count)
                   ", общая площадь " (rtos total-area 2 2) " м2"))
        )
        (princ "\nНет данных для отчёта.")
      )
    )
    (princ "\nБлоки заполнения не найдены.")
  )

  (princ)
)


;; ============================================================
;; АВТОНОМНАЯ КОМАНДА
;; ============================================================
(defun c:zapolnenie ( / layers-str layers report-mode export-excel export-txt create-table use-default save-base)
  ;; Проверка наличия зависимостей
  (if (not (and (type 'su-select-inserts)
                (type 'eu-export-zapolnenie-detail)
                (type 'tx-export-gal-zapolnenie)))
    (progn
      (princ "\nНе загружены зависимости. Запустите EXTRACTION или RELOAD.")
      (princ)
      (exit)
    )
  )

  ;; Запрос слоёв (T разрешает пробелы)
  (setq layers-str (getstring T "\nВведите слои через запятую (Enter — все слои): "))
  (if (= layers-str "")
    (setq layers nil)
    (setq layers (mapcar 'strcase (split-string layers-str ",")))
  )

  ;; Режим отчёта
  (initget "D S")
  (setq report-mode (getkword "\nРежим отчёта [Подробный(D)/Краткий(S)] <D>: "))
  (if (null report-mode) (setq report-mode "D"))
  (setq report-mode (if (= report-mode "D") "DETAIL" "SUMMARY"))

  ;; Экспорт в Excel
  (initget "Y N")
  (setq export-excel (getkword "\nЭкспорт в Excel? [Да(Y)/Нет(N)] <N>: "))
  (if (or (null export-excel) (= export-excel "N")) (setq export-excel nil) (setq export-excel T))

  ;; Экспорт в GAL
  (initget "Y N")
  (setq export-txt (getkword "\nЭкспорт в TXT (GAL)? [Да(Y)/Нет(N)] <N>: "))
  (if (or (null export-txt) (= export-txt "N")) (setq export-txt nil) (setq export-txt T))

  ;; Таблица AutoCAD
  (initget "Y N")
  (setq create-table (getkword "\nСоздать таблицу AutoCAD? [Да(Y)/Нет(N)] <Y>: "))
  (if (or (null create-table) (= create-table "Y")) (setq create-table T) (setq create-table nil))

  ;; Путь сохранения
  (initget "Y N")
  (setq use-default (getkword "\nИспользовать путь по умолчанию? [Да(Y)/Нет(N)] <Y>: "))
  (if (or (null use-default) (= use-default "Y"))
    (setq save-base nil)
    (setq save-base (getstring T "\nБазовое имя файла (без расширения): "))
  )

  (zapolnenie-main layers report-mode export-excel export-txt create-table save-base)
  (princ)
)

;; Русская команда
(defun c:ЗАПОЛНЕНИЕ ()
  (c:zapolnenie)
)

(princ "\nZAPOLNENIE.LSP загружен. Команды: ZAPOLNENIE, ЗАПОЛНЕНИЕ")
(princ)