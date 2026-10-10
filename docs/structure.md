# Структура проекта AutoExtraction

```text
AutoExtraction/
├── reload.lsp                 # Команда RELOAD: загрузка всех модулей, секция PLUGINS
├── settings.ini               # Пользовательские настройки (cp1251)
├── AutoExtraction.prj         # Файл проекта Visual LISP (неполный, см. techdebt.md)
├── export_code.py             # Выгрузка кода проекта в текстовые файлы
├── Extraction/                # Команды задач и их диалоги
│   ├── extraction.lsp         # Диспетчер EXTRACTION / ЭКСТРАКЦИЯ, Витраж (заглушка)
│   ├── extraction.dcl         # Диалог диспетчера
│   ├── settings.lsp           # Окно «Настройки» (ключи settings.ini)
│   ├── settings.dcl           # Диалог «Настройки»
│   ├── fasonka.lsp            # Задача «Фасонка» (FASONKA)
│   ├── subsystem.lsp          # Задача «Подсистема» (SUBSYSTEM)
│   ├── cladding.lsp           # Задача «Облицовка» (CLADDING, CLBLOCKS)
│   ├── zapolnenie.lsp         # Задача «Заполнение» (ZAPOLNENIE)
│   ├── cutline.lsp            # Раскрой хлыста (CUTLINE)
│   ├── cutline_filter.dcl     # Диалог раскроя хлыста
│   ├── cutsheet.lsp           # Раскрой листа (CUTSHEET)
│   ├── cutsheet_filter.dcl    # Диалог раскроя листа
│   ├── blockrename.lsp        # Панель «Блоки»: копия, вставка, переименование (BLOCKCOPY, BLOCKRENAME)
│   ├── help.lsp               # Окно справки (кнопка «?»)
│   └── help.dcl               # Диалог справки
├── common/                    # Общие библиотеки (префиксы функций в скобках)
│   ├── task-utils.lsp         # Строки, файлы, UNDO, безопасные ActiveX-вызовы (tu-, ex-)
│   ├── validation-utils.lsp   # Разбор и проверка чисел (tu-parse-number, tu-valid-*)
│   ├── layer-utils.lsp        # Слои и групповые фильтры AutoCAD (Мои / Фасады / Витражи / Окна)
│   ├── select-utils.lsp       # Выборка блоков и чтение динамических свойств (su-)
│   ├── settings-utils.lsp     # Постоянные настройки: чтение и хранение settings.ini
│   ├── excel-utils.lsp        # XLS (XML Spreadsheet) и CSV без COM (eu-)
│   ├── txt-utils.lsp          # GAL для станка с ЧПУ (tx-)
│   ├── table-utils.lsp        # Таблицы AutoCAD, разбиение строк (tc-, ts-ac-)
│   └── perf-utils.lsp         # Замеры производительности (подключён, в задачах не используется)
├── Plugins/                   # Принятые плагины проекта (папки в репозитории нет;
│                              #   плагины в разработке грузятся из *ae-reload-plugin-dev-dirs*)
├── tests/                     # Тесты: Python без AutoCAD и LSP для AutoCAD
│   ├── static_check.py        # Статические проверки исходников (Python)
│   ├── mark_logic_test.py     # Логика марок (Python)
│   ├── layer_filter_logic_test.py  # Логика фильтров слоёв (Python)
│   ├── fill_allowance_test.py # Признак блока заполнения и припуск (Python)
│   ├── nesting_benchmark.py   # Сверка качества раскроя с независимым раскладчиком (Python)
│   ├── unit-tests.lsp         # Unit-тесты в AutoCAD (AE_TEST)
│   ├── validation-test.lsp    # Парсер и предикаты валидации (V1/V2)
│   ├── u1-selftest.lsp        # Примитивы XLS (U1)
│   ├── u2-xlsdiff.lsp         # Семантический diff XLS против эталонов
│   ├── layer-filter-test.lsp  # Фильтры слоёв на живом чертеже (LAYERFILTERTEST)
│   ├── cladding-test.lsp      # Тестовые помощники облицовки (MKTEST, RMTEST, SCANBLOCKS и др.)
│   ├── chkparens.lsp          # Баланс скобок и кодировка (CHKALL, CHKFILE, CHKLOAD)
│   ├── CHKPARENS.md           # Описание проверки баланса скобок
│   ├── TEST_CASES.md          # Регрессионные случаи CUTLINE
│   ├── TEST_MLINE_NAME.LSP    # Диагностика имени MLINE и её MLINESTYLE (TESTMLINENAME)
│   ├── run-tests.scr.example  # Пример скрипта AutoCAD: загрузка unit-tests.lsp и AE_TEST
│   └── README.md              # Описание тестов и порядок запуска
├── scripts/
│   └── test.ps1               # Запуск static_check и Python-тестов одной командой (PowerShell);
│                              #   ключ -AutoCAD пока только ищет acad.exe (заглушка)
├── export/
│   └── zapolnenie-bundle.txt  # Выгрузка ZAPOLNENIE для передачи (ред. 28, устарел; перевыпуск не сделан)
├── docs/                      # Документация
│   ├── START.md               # Быстрый старт
│   ├── requirements.md        # Функциональные требования (текущее поведение)
│   ├── requirements-dev.md    # Требования к разработке и окружению
│   ├── promt.md               # Техническое задание (промт)
│   ├── structure.md           # Этот файл
│   ├── next-tasks.md          # План задач (Витраж — не реализован)
│   ├── zapolnenie.md          # Спецификация задачи «Заполнение»
│   ├── marks-in-maps.md       # Марка детали в картах раскроя
│   ├── fill-allowance.md      # Блоки заполнения: размер «в свету» + припуск
│   ├── search-masks.md        # Маски поиска
│   ├── block-attributes.md    # Извлечение атрибутов блоков
│   ├── fast-tables.md         # Быстрое создание таблиц AutoCAD
│   ├── nesting-quality.md     # Качество раскроя CUTSHEET
│   ├── v9-activex-migration.md  # Контракт safe-call для ActiveX
│   ├── acceptance-v3-v5-v6.md # Протокол живой приёмки V3/V5/V6
│   ├── extraction-settings-design-draft.md  # Черновик проектирования настроек
│   └── GIT.md                 # Работа с Git и GitHub
├── roadmap.md                 # Дорожная карта (этапы 2–5)
├── techdebt.md                # Технический долг
├── HANDOFF_TZ.md              # Журнал передачи контекста между сессиями
├── README.md                  # Краткое описание проекта
├── export_code.py             # (см. выше)
├── .vscode/settings.json      # Настройки VS Code (кодировка LISP-файлов)
└── .gitignore                 # Игнорирование временных файлов
```

Кодировки: `.lsp`, `.dcl`, `settings.ini` — Windows-1251; `.md` — UTF-8 без BOM.
