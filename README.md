# AutoExtraction

Модульная система AutoLISP для AutoCAD, предназначенная для автоматизации извлечения данных из чертежей, подсчёта материалов и раскроя.

## Возможности

- Запуск задач через диалоговое окно (`EXTRACTION`) или из командной строки (`FASONKA`, `SUBSYSTEM`, `ZAPOLNENIE`, `CLADDING`, `CLBLOCKS`).
- Поддержка режимов отчёта:
  - **DETAIL** — детальный перечень с промежуточными итогами по группам;
  - **SUMMARY** — сводный отчёт с общими итогами.
- Экспорт результатов:
  - в Excel (`.xls`) — XML Spreadsheet с формулами;
  - в CSV (`.csv`) — запасной вариант при недоступности Excel;
  - в текстовый файл (`.gal`) — для передачи в системы раскроя;
  - в таблицу AutoCAD.
- Модуль линейного раскроя (`CUTLINE`) — оптимизация размещения деталей на хлыстах.

## Архитектура

### Кускование таблиц

Все модули используют единый эталонный алгоритм `tc-partition-flat` из `common/table-utils.lsp`. Алгоритм равномерно распределяет строки по таблицам с инвариантом: подитог группы никогда не отрывается от своих данных.

| Модуль | Подготовка строк | Рендер |
|---|---|---|
| Облицовка | `cl-build-flat-items` | `cl-create-table-detail` |
| Подсистема | `su-build-flat-items` | `subsystem-create-table-detail` |
| Заполнение | `zp-build-flat-items` | `zapolnenie-create-table-detail` |
| Фасонка | `fs-build-flat-items` | `fasonka-create-table-detail` |

### Оформление таблиц

Единый визуальный стандарт реализован функциями `ts-ac-*` в `common/table-utils.lsp`:

- `ts-ac-title` — заголовок таблицы (объединение + подчёркивание);
- `ts-ac-header` — шапка (заполнение + центрирование);
- `ts-ac-subtotal` — подитог группы (жирный номер + текст);
- `ts-ac-total` — общий итог (объединение + текст).

### Оформление Excel

Стили XML Spreadsheet централизованы функцией `eu-xml-styles` в `common/excel-utils.lsp`. Все модули экспорта используют единый набор стилей: `Default`, `Title`, `Header`, `Data`, `DataLeft`, `Cut`, `Num`, `Total`, `TotalNum`.

### Допущения

- Подробный XLS блоков может отличаться от прочих выводов на 0.01 м? на слой из-за порядка округления (формулы округляют каждую строку, остальные выводы округляют подитоги).
- Полилинии с номиналом `_НЕПРЯМОУГ_` в XLS выводятся с высотой/шириной описанного прямоугольника со звёздочкой (справочные значения), площадь без формулы.

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

## Экспорт кода

Утилита `export_code.py` позволяет выгрузить весь код проекта в текстовые файлы.

### Обычный режим (один файл)

```bash
python export_code.py .
Разделение по каталогам
Создаст в папке export_split файлы вида AutoExtraction_Extraction.txt, AutoExtraction_common.txt, AutoExtraction_docs.txt и т.д.
python export_code.py . --split-dirs
Можно указать свою выходную папку:
python export_code.py . --split-dirs --out-dir my_export
Разделение по отдельным файлам
Каждый .lsp, .dcl, .prj, .md будет сохранён как отдельный .txt с тем же именем (например, fasonka.lsp.txt).
python export_code.py . --split-files
Включить документацию
Добавьте --include-docs в любую команду:
python export_code.py . --split-dirs --include-docs
Исключить каталоги
python export_code.py . --split-dirs --exclude .git backups
Лицензия
Использование