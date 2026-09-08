# Структура проекта AutoExtraction

```text
AutoExtraction/
├── common/
│   ├── task-utils.lsp           # Универсальные функции: парсинг списков, сортировка, уникализация, пути
│   ├── layer-utils.lsp          # Работа со слоями и групповыми фильтрами (ACLYDICTIONARY)
│   ├── excel-utils.lsp          # Экспорт в Excel (XML) с формулами
│   ├── txt-utils.lsp            # Экспорт в TXT (.gal для передачи в раскрой)
│   └── table-utils.lsp          # Создание таблиц AutoCAD (ActiveX)
│
├── Extraction/
│   ├── extraction.lsp           # Диспетчер: загрузка модулей, окно выбора, вызов run-task
│   ├── extraction.dcl           # Описание диалогового окна
│   ├── fasonka.lsp              # Задача «Фасонка»
│   ├── cutline.lsp              # Утилита линейного раскроя (раскрой хлыста)
│   └── cutsheet.lsp             # Утилита листового раскроя (в разработке)
│
├── docs/
│   ├── promt.md                 # Промт/техническое задание
│   ├── requirements.md          # Функциональные требования
│   ├── requirements-dev.md      # Требования к разработке
│   ├── structure.md             # Этот файл
│   └── licenses.md              # Лицензии и правовая информация
│
├── .gitignore
├── README.md
├── reload.lsp
└── exportcode.py                # Скрипт экспорта кода в единый текстовый файл