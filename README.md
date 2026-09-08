# AutoExtraction

Модульная система AutoLISP для AutoCAD.

## Текущая структура

```text
D:\AutoExtraction\
├── TASKS\
│   ├── dispatcher.lsp
│   ├── dispatcher.dcl
│   └── fasonka.lsp
├── common\
│   ├── task-utils.lsp
│   ├── excel-utils.lsp
│   ├── table-utils.lsp
│   └── layer-utils.lsp
├── docs\
├── .vscode\
│   └── settings.json
├── .gitignore
└── README.md
```

## Архитектура

`TASKS/dispatcher.lsp` — единая точка запуска. Он открывает DCL-окно, собирает параметры и передаёт их в `run-task`.

`TASKS/fasonka.lsp` — первая реально реализованная задача FASONKA.

`common/` — общие библиотеки, которые не должны содержать бизнес-логику конкретной задачи.

Будущие задачи:

- `SUBSYSTEM`
- `CLADDING`
- `VITRAZH`
- `STEKLOPAKETY`

Пока соответствующие модули намеренно не добавлены.

## Запуск

1. В AutoCAD один раз настройте пути проекта.
2. Загрузите `TASKS/dispatcher.lsp` через `APPLOAD`.
3. Выполните команду:

```text
Extraction
```

или:

```text
Экстракция
```

Для прямого теста FASONKA:

```lisp
(fasonka-main nil "DETAIL" T nil T nil)
```

## Важное правило разработки

Диспетчер не должен выполнять расчёты конкретной задачи. Его задача — UI и маршрутизация. Вся предметная логика FASONKA находится в `fasonka.lsp`, а общие операции — в `common/`.

## Кодировка

Проект рассчитан на современный AutoCAD с Unicode-поддержкой AutoLISP. Файлы сохраняются в UTF-8 согласно настройке VS Code.

Не переводите проект обратно в Windows-1251 без необходимости: при `LISPSYS=1` AutoCAD использует кодировку, заданную в VS Code, а AutoLISP полноценно поддерживает Unicode.
