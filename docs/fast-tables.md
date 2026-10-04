# Быстрое создание таблиц AutoCAD из AutoLISP

Методика построения таблиц AutoCAD через ActiveX так, чтобы заполнение
таблицы любого размера занимало доли секунды вместо десятков секунд.
Адаптировано в AutoExtraction по материалам FAST_TABLES (проект MarkZ,
ред. 48.32/48.34) и Autodesk AutoCAD ActiveX Reference.

## 1. Почему таблица строится медленно

Объект AcDbTable пересобирается целиком при каждом изменении. Документация
Autodesk (свойство `RegenerateTableSuppressed`): все методы, меняющие
таблицу, работают по схеме «Open write → Modify → Close», и закрытие
пересчитывает таблицу; пересчёт больших таблиц «consumes a lot of time and
memory because the Table object is reconstructed from scratch».

Следствие: заполнение по-ячейночному (`SetText` + выравнивания + высоты
строк + ширины колонок + объединения) стоит N полных пересборок таблицы,
каждая дороже предыдущей — время растёт квадратично. Ведомость на 57
позиций — это около 846 ActiveX-вызовов, т.е. ~846 полных пересборок.

## 2. Правило

1. Одна вставка сразу конечного размера:
   `(vla-AddTable space pt nRows nCols colW rowH)`.
   Никаких поштучных `InsertRows` в цикле.
2. Сразу после `AddTable` включить подавление пересборки:
   `(vl-catch-all-apply 'vla-put-RegenerateTableSuppressed (list tbl :vlax-true))`.
3. Заполнить всё (ширины колонок, тексты, выравнивания, объединения
   `MergeCells`, слой, высоту текста) — под подавлением каждый вызов
   дешёвый.
4. Выключить подавление:
   `(vl-catch-all-apply 'vla-put-RegenerateTableSuppressed (list tbl :vlax-false))`
   — это единственный полный пересчёт.
5. **НЕ вызывать** `vla-Update` следом: включение регенерации само
   пересобирает таблицовый блок; `Update` делал бы пересчёт второй раз
   (двойная «сборка»). Откат при проблемах отображения — одна строка (§6).
6. Оба переключения — через `vl-catch-all-apply`: на версии AutoCAD без
   этого свойства программа молча продолжит старым медленным, но
   корректным путём.
7. Замер: `(getvar "MILLISECS")` до/после этапов, вывод `(rtos ... 2 2)` —
   секунды с сотыми. В логе программы удобно держать INFO со временем.

## 3. Шаблон (копировать в свою программу)

```lisp
(vl-load-com)

;; Быстрая таблица: data = список строк, строка = список текстов ячеек.
;; Пример: (("№" "Наименование" "Кол-во") ("1" "Стойка" "12") ...)
;; pt — точка вставки в WCS.
(defun my:fast-table (data pt / doc space tbl t0 t1 t2 t3
                        nrows ncols r c row cell)
  (setq doc   (vla-get-ActiveDocument (vlax-get-acad-object))
        space (vla-get-ModelSpace doc)   ; или (vla-get-PaperSpace doc)
        nrows (+ 1 (length data))        ; шапка + данные (итоги добавьте в data)
        ncols (length (car data))
        t0    (getvar "MILLISECS"))
  ;; 1. одна вставка сразу конечного размера
  (setq tbl (vla-AddTable space (vlax-3d-point pt) nrows ncols 10.0 30.0))
  ;; 2. подавить пересборку на всё время заполнения
  (vl-catch-all-apply 'vla-put-RegenerateTableSuppressed
                      (list tbl :vlax-true))
  (setq t1 (getvar "MILLISECS"))
  ;; 3. заполнение — теперь вызовы не пересоздают таблицу
  (setq c 0)
  (repeat ncols                         ; ширины колонок
    (vla-SetColumnWidth tbl c 40.0)
    (setq c (1+ c)))
  (setq r 0)
  (foreach row data                     ; тексты + выравнивание
    (setq c 0)
    (foreach cell row
      (vla-SetText tbl r c cell)
      (vla-SetCellAlignment tbl r c acMiddleCenter) ; 5; acMiddleLeft = 4
      (setq c (1+ c)))
    (setq r (1+ r)))
  (setq t2 (getvar "MILLISECS"))
  ;; 4. единственный полный пересчёт — выключением подавления
  (vl-catch-all-apply 'vla-put-RegenerateTableSuppressed
                      (list tbl :vlax-false))
  (setq t3 (getvar "MILLISECS"))
  (princ (strcat "\nТаблица: строк " (itoa nrows) ", время "
                 (rtos (/ (- t3 t0) 1000.0) 2 2) " с (создание "
                 (rtos (/ (- t1 t0) 1000.0) 2 2) ", заполнение "
                 (rtos (/ (- t2 t1) 1000.0) 2 2) ", сборка "
                 (rtos (/ (- t3 t2) 1000.0) 2 2) ")"))
  tbl)
```

Дополнения, работающие внутри того же окна подавления:

- объединение ячеек: `(vla-MergeCells tbl minRow maxRow minCol maxCol)`;
- высота текста: `(vla-SetCellTextHeight tbl row col 3.0)`;
- высота строки: `(vl-catch-all-apply 'vla-SetRowHeight (list tbl row 8.0))`;
- слой: назначить таблице до/во время заполнения
  (`(vla-put-Layer tbl "Размеры")`);
- всю сборку обернуть в `CMDECHO 0` + `StartUndoMark`/`EndUndoMark`
  документа — один шаг Undo на всю таблицу.

## 4. Реализация в AutoExtraction

- `Extraction/zapolnenie.lsp` (ред. 29): `zapolnenie-create-table-detail`
  и `zapolnenie-create-table-summary`. Схема: число строк считается до
  вставки (`nRows = 2 + строки чанка (+ 1 итог)`), один `vla-AddTable`
  конечного размера → подавление → заполнение (ширины, шапка, данные,
  подытоги `MergeCells`, итог) → снятие подавления → замер в лог
  («создана за X.XX с.»). `vla-Update` после снятия подавления нет.
- Впервые схема внедрена в тестовом стенде `zapolnenie2.lsp` (ред. 5),
  после приёмки перенесена в рабочий модуль (в т.ч. в краткую таблицу);
  стенд удалён. До этого в проекте применялась только первая половина
  схемы (одна вставка конечного размера, без `InsertRows`), но каждый
  вызов при заполнении пересобирал таблицу целиком. Живой прогон:
  таблицы создаются «моментально».
- Особенность проекта: длинные ведомости нарезаются на несколько таблиц
  (`tc-partition-flat`, идеал `*TU-IDEAL-ROWS*`), каждая вставляется
  столбиком ниже предыдущей (`tu-next-table-point`). Окно подавления и
  замер — у каждого чанка свои.
- Легаси-генератор `tbl-create-summary` в `common/table-utils.lsp`
  (4 колонки, `vla-Update`, без подавления) остаётся на старой схеме —
  унификация всех вставок таблиц проекта на эту методику отдельная задача.

## 5. Числа из практики (MarkZ, ведомость заполнений)

Таблица на 57 позиций (~60 строк × 7 колонок с шапкой и итогами,
~846 ActiveX-вызовов).

- Было (каждый вызов = полная пересборка таблицы): субъективно десятки
  секунд.
- Стало (подавление + отказ от двойного пересчёта): **0.08 с** на всё —
  создание 0.02, заполнение 0.05, сборка 0.02 (этапы округляются по
  отдельности). Подтверждено контрольным прогоном пользователя:
  «таблица создалась мгновенно».

## 6. Типичные ошибки

- Забыли выключить подавление (`:vlax-false`) → таблица на экране не
  обновится до `REGEN`. Обрыв программы в середине заполнения лечится
  просто `REGEN`.
- `vla-Update` после `:vlax-false` → двойная сборка, потеря половины
  выигрыша на финальном этапе. Не нужен.
- `InsertRows` в цикле вместо `AddTable` конечного размера.
- `vl-catch-all-apply` принимает список аргументов:
  `(vl-catch-all-apply 'vla-put-RegenerateTableSuppressed (list tbl :vlax-true))`.
- В AutoLISP нет `defvar` — глобальные переменные инициализируются `setq`,
  иначе `LOAD` падает с «no function definition: DEFVAR».
- Булевы значения COM — `:vlax-true` / `:vlax-false` (не `T` / `nil`).

## 7. Совместимость и откат

Свойство `RegenerateTableSuppressed` есть во всех актуальных версиях
AutoCAD (справка ActiveX, проверено по документации 2016+).
`vl-catch-all-apply` гарантирует тихую деградацию, если свойства нет.

Если на какой-то версии таблица после `:vlax-false` выглядит несвежей —
вернуть одну строку сразу после включения регенерации:

```lisp
(vl-catch-all-apply 'vla-Update (list tbl))
```

Равнозначная альтернатива снятию подавления (упомянута Autodesk):
`(vla-RecomputeTableBlock tbl :vlax-true)`.

## 8. Источники

- Autodesk, AutoCAD ActiveX and VBA Reference, Table object, свойство
  `RegenerateTableSuppressed`: описание механики пересборки
  («reconstructed from scratch») и официальный пример — AddTable →
  включить подавление → заполнить циклами → выключить подавление,
  без `vla-Update`.
- Проект MarkZ (moleval/MarkZ), FAST_TABLES, ред. 48.32/48.34.
