## README.md

# AutoExtraction

Модульная система AutoLISP для AutoCAD, предназначенная для автоматизации извлечения данных из чертежей, подсчёта материалов и раскроя.

## Возможности

- Запуск задач через диалоговое окно (`EXTRACTION`) или из командной строки (`FASONKA`, `SUBSYSTEM`).
- Поддержка режимов отчёта:
  - **DETAIL** — детальный перечень с промежуточными итогами по группам;
  - **SUMMARY** — сводный отчёт с общими итогами.
- Экспорт результатов:
  - в Excel (`.xls`) — XML Spreadsheet с формулами;
  - в CSV (`.csv`) — запасной вариант при недоступности Excel;
  - в текстовый файл (`.gal`) — для передачи в системы раскроя;
  - в таблицу AutoCAD.
- Модуль линейного раскроя (`CUTLINE`) — оптимизация размещения деталей на хлыстах.

## Структура проекта

См. [docs/structure.md](docs/structure.md).

## Документация

- [Техническое задание](docs/promt.md)
- [Функциональные требования](docs/requirements.md)
- [Требования к разработке](docs/requirements-dev.md)
- [Быстрый старт](docs/START.md)

## Установка

1. Скопируйте папку проекта на диск.
2. Добавьте пути `AutoExtraction`, `AutoExtraction\Extraction`, `AutoExtraction\common` в пути поддержки AutoCAD.
3. Загрузите `reload.lsp` или `extraction.lsp`.
4. Выполните команду `EXTRACTION`.

Подробнее см. [docs/START.md](docs/START.md).

## Лицензия

Использование

Обычный режим (один файл)
bash
python export_code.py .
Разделение по каталогам
Создаст в папке export_split файлы вида AutoExtraction_Extraction.txt, AutoExtraction_common.txt, AutoExtraction_docs.txt и т.д.

bash
python export_code.py . --split-dirs
Можно указать свою выходную папку:

bash
python export_code.py . --split-dirs --out-dir my_export
Разделение по отдельным файлам
Каждый .lsp, .dcl, .prj, .md будет сохранён как отдельный .txt с тем же именем (например, fasonka.lsp.txt).

bash
python export_code.py . --split-files
Включить документацию
Добавьте --include-docs в любую команду:

bash
python export_code.py . --split-dirs --include-docs
Исключить каталоги
bash
python export_code.py . --split-dirs --exclude .git backups