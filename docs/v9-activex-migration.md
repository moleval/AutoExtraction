# V9 — единый контракт safe-call для ActiveX

**Этап 3 (P1) · точки миграции ActiveX/API**

## Контракт

Один API вместо разнобоя `vl-catch-all-apply` (common/task-utils.lsp):

```lisp
(setq r (ex-safe-call 'vla-<метод> arg1 arg2 ...))
;; r = (OK значение) | (ERROR "текст ошибки")
(ex-safe-ok-p r)      ; T | nil
(ex-safe-value r)     ; значение при OK
(ex-safe-message r)   ; текст при ERROR
```

Принцип: **оборачиваем только внешние вызовы AutoCAD/ActiveX** (InsertBlock,
addtable, BoundingBox, свойства динблоков, файловые/API-операции).
Не оборачиваем каждую строку расчётного кода и не трогаем entmake-геометрию
(у неё нет исключений — нет контракта).

## Инвентаризация точек

| № | Точка | Где | Статус |
|---|-------|-----|--------|
| 1 | `vla-InsertBlock` — вставка карты в wrap-блок | cutsheet `cs-wrap-to-block` | ✅ мигрировано (V9-1) |
| 2 | `vla-InsertBlock` — копия блока через WBLOCK | blockrename `blockrename-copy-block` | ✅ мигрировано (V9-1) |
| 3 | `vla-addtable` (10 вызовов) + семейство `vla-SetText/SetCellAlignment/SetColumnWidth/MergeCells` | cladding, table-utils | следующая волна — вместе с U1/U2: оборачивать «создать таблицу» целиком, не ячейки |
| 4 | `vla-GetBoundingBox` (2) | cutsheet (bb-телеметрия) | следующая волна |
| 5 | `vla-Item` + `vla-put-Name` (Blocks) | blockrename | следующая волна |
| 6 | `vla-ZoomWindow` | cutsheet `cs-zoom-bbox` | следующая волна |
| 7 | геттеры дин. свойств блоков (`vla-get-DynamicBlockProperties`) | загрузчики модулей | следующая волна |
| 8 | `vla-get-ActiveDocument` / `vlax-get-acad-object` | — | уже централизовано в `tu-undo-begin` (V10) |

## Ограничения (зафиксировано)

- Не заменяем массовые `vl-catch-all-apply` механически — только по списку.
- `command`/`vl-cmdf` (`-BLOCK`, `-WBLOCK`, `-RENAME`) — не ActiveX-контракт;
  их целостность обеспечивает V13 (undo-откат), здесь не трогаем.
- `entmake`-примитивы (cutline: `n1-block-insert`, тексты, рамки) — без исключений, не кандидаты.
