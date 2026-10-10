# Структура проекта AutoExtraction

Актуально на редакцию `ee8fb89` (PR #5). Номера редакций модулей — в шапке каждого файла и в сообщении загрузки `RELOAD`.

```text
AutoExtraction/
|-- reload.lsp                  # Команда RELOAD: загрузка всех модулей, раздел плагинов, маркер редакции
|-- settings.ini                # Настройки задач (маски слоёв и блоков, слои и имена выходных блоков, припуск)
|-- AutoExtraction.prj          # Файл проекта Visual LISP (неполный: см. HANDOFF_TZ.md, раздел 9)
|-- export_code.py              # Выгрузка кода проекта в текстовые файлы
|-- README.md                   # Краткое описание проекта и порядок установки
|-- HANDOFF_TZ.md               # ТЗ и журнал передачи контекста между сессиями разработки
|-- roadmap.md                  # Дорожная карта (этапы защиты, унификации, производительности)
|-- techdebt.md                 # Журнал технического долга и закрытых дефектов
|-- stage8_cleanup_audit.md     # Аудит неиспользуемого кода (шаг 8, 2026-09-21)
|-- .gitattributes              # Plugins\*.lsp без преобразования переводов строк
|-- .gitignore                  # Игнорирование временных и служебных файлов
|-- .vscode/
|   `-- settings.json           # VS Code: кодировка windows1251 для .lsp и .dcl
|-- common/                     # Общие модули (загружаются первыми)
|   |-- task-utils.lsp         # Общие утилиты задач: строки, файлы, марка элемента (*AE-MARK-ATTR*)
|   |-- settings-utils.lsp     # Чтение и запись settings.ini, значения по умолчанию
|   |-- perf-utils.lsp         # Замеры производительности метками (*ae-perf-on*, по умолчанию выключено)
|   |-- layer-utils.lsp        # Слои и групповые фильтры AutoCAD: Мои / Фасады / Витражи / Окна
|   |-- select-utils.lsp       # Выбор блоков и извлечение динамических свойств
|   |-- excel-utils.lsp        # Экспорт в Excel (XML Spreadsheet) и CSV
|   |-- table-utils.lsp        # Таблицы AutoCAD (ts-ac-*, быстрые таблицы tc-partition-flat)
|   |-- txt-utils.lsp          # Экспорт в текстовый формат GAL
|   `-- validation-utils.lsp   # Проверка входных данных (V1/V2)
|-- Extraction/                 # Диспетчер и модули задач
|   |-- extraction.lsp         # Диспетчер EXTRACTION / ЭКСТРАКЦИЯ, главное окно, запуск задач
|   |-- extraction.dcl         # Диалог диспетчера
|   |-- settings.lsp           # Окно «Настройки» (правила задач, припуск на раму)
|   |-- settings.dcl           # Диалог окна «Настройки»
|   |-- help.lsp               # Расширенная справка (кнопка «?»)
|   |-- help.dcl               # Диалог справки
|   |-- fasonka.lsp            # Задача «Фасонка» (FASONKA / ФАСОНКА)
|   |-- subsystem.lsp          # Задача «Подсистема» (SUBSYSTEM / ПОДСИСТЕМА)
|   |-- cladding.lsp           # Задача «Облицовка» (CLADDING / ОБЛИЦОВКА, CLBLOCKS)
|   |-- zapolnenie.lsp         # Задача «Заполнение» (ZAPOLNENIE / ЗАПОЛНЕНИЕ), колонка «Марка»
|   |-- cutline.lsp            # Раскрой хлыста (CUTLINE / РАСКРОЙХЛЫСТА)
|   |-- cutline_filter.dcl     # Диалог параметров раскроя хлыстов (CUTLINE)
|   |-- cutsheet.lsp           # Раскрой листа (CUTSHEET / РАСКРОЙЛИСТА)
|   |-- cutsheet_filter.dcl    # Диалог параметров раскроя листа (CUTSHEET)
|   `-- blockrename.lsp        # Копия, вставка и переименование блоков (BLOCKCOPY, BLOCKRENAME)
|-- Plugins/                    # Принятые сторонние плагины (папка может отсутствовать)
|-- docs/                       # Документация
|   |-- START.md               # Быстрый старт
|   |-- requirements.md        # Функциональные требования
|   |-- requirements-dev.md    # Требования к разработке и окружению
|   |-- promt.md               # Исходное техническое задание
|   |-- structure.md           # Этот файл
|   |-- search-masks.md        # Маски поиска в полях «Поиск»
|   |-- marks-in-maps.md       # Марка элемента в картах раскроя и XLS/CSV
|   |-- fill-allowance.md      # Блоки заполнения: размер в свету и припуск на раму
|   |-- zapolnenie.md          # Описание задачи «Заполнение»
|   |-- fast-tables.md         # Быстрые таблицы AutoCAD (FAST_TABLES)
|   |-- block-attributes.md    # Атрибуты блоков
|   |-- nesting-quality.md     # Сверка качества раскроя
|   |-- acceptance-*.md        # Протоколы приёмки живых прогонов
|   |-- review-pr4-2026-10-09.md  # Аудит PR #4
|   |-- next-tasks.md          # Исходный план задач (историческая справка)
|   |-- extraction-settings-design-draft.md  # Проект окна «Настройки»
|   |-- v9-activex-migration.md  # Контракт безопасных вызовов ActiveX (V9)
|   `-- GIT.md                 # Заметки по git
|-- scripts/
|   `-- test.ps1               # Запуск всех автоматических проверок (Windows PowerShell)
|-- tests/                      # Тесты и диагностика
|   |-- README.md              # Описание тестов и порядок запуска
|   |-- static_check.py        # Статические проверки кода (без AutoCAD)
|   |-- layer_filter_logic_test.py  # Логика фильтров слоёв (без AutoCAD)
|   |-- mark_logic_test.py     # Логика марок и колонок XLS/CSV (без AutoCAD)
|   |-- fill_allowance_test.py # Признак заполнения и припуск (без AutoCAD)
|   |-- nesting_benchmark.py   # Сравнение алгоритмов раскроя (без AutoCAD)
|   |-- chkparens.lsp          # Диагностика баланса скобок в AutoCAD (CHKALL, CHKFILE, CHKLOAD)
|   |-- unit-tests.lsp         # Модульные проверки в AutoCAD (AE_TEST)
|   |-- layer-filter-test.lsp  # Фильтры слоёв на живом чертеже (LAYERFILTERTEST)
|   |-- validation-test.lsp    # Проверки V1/V2 в AutoCAD
|   |-- u1-selftest.lsp        # Самопроверка примитивов XLS
|   |-- u2-xlsdiff.lsp         # Семантическое сравнение XLS с эталонами
|   |-- cladding-test.lsp      # Тестовые помощники для облицовки (MKTEST, RMTEST, SCANBLOCKS)
|   |-- CHKPARENS.md           # Описание chkparens.lsp
|   |-- TEST_CASES.md          # Регрессионные тестовые сценарии
|   |-- TEST_MLINE_NAME.LSP    # Тест имени блока MLINE
|   `-- run-tests.scr.example  # Пример скрипта запуска тестов
`-- export/
    `-- zapolnenie-bundle.txt  # Выгрузка модуля «Заполнение» для передачи (собирается вручную)
```
