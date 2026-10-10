# AutoExtraction

Модульная система AutoLISP для AutoCAD, предназначенная для автоматизации извлечения данных из чертежей, подсчёта материалов и раскроя.

## Возможности

- Запуск задач через диалоговое окно диспетчера (`EXTRACTION` / `ЭКСТРАКЦИЯ`) или отдельными командами (см. [docs/START.md](docs/START.md)).
- Задачи отчётов в диспетчере:
  - **Фасонка**, **Подсистема**, **Облицовка**, **Заполнение** — отчёты DETAIL и SUMMARY;
  - **Витраж** — в разработке: пока заглушка, отчёта нет.
- Режимы отчёта:
  - **DETAIL** — детальный перечень с промежуточными итогами по группам;
  - **SUMMARY** — сводный отчёт с общими итогами.
- Вывод результатов:
  - в Excel (`.xls`) — XML Spreadsheet с формулами; если файл не удалось записать (например, он открыт в Excel), сохраняется CSV с тем же базовым именем;
  - в текстовый файл (`.gal`) — формат GAL для станка с ЧПУ;
  - в таблицу AutoCAD.
- Раскрой:
  - **Раскрой хлыста** (`CUTLINE`) — линейный раскрой на хлыстах; карта раскроя вставляется блоком;
  - **Раскрой листа** (`CUTSHEET`) — раскрой деталей на листах; карта раскроя вставляется блоком;
  - марка детали (атрибут «МАРКА») подписывается на карте раскроя; в XLS и CSV колонка «Марка» появляется, если в выборке есть хотя бы одна марка;
  - в XLS и CSV «Раскроя листа» колонка «Тип» показывает состояние видимости блока, а не его имя;
  - блоки заполнения в «Раскрое листа» кроятся по размеру «в свету» плюс припуск на раму (`input.frame.allowance`, по умолчанию 26 мм) — см. [docs/fill-allowance.md](docs/fill-allowance.md).
- Групповые фильтры слоёв: **Мои**, **Фасады**, **Витражи**, **Окна** — см. [docs/requirements.md](docs/requirements.md), раздел 1.1.
- Поиск по спискам слоёв и блоков с масками `*`, `?`, `#`, `@` — см. [docs/search-masks.md](docs/search-masks.md).
- Раздел плагинов: сторонние модули загружаются тем же `RELOAD` (секция `--- PLUGINS ---`) — из каталога разработки (`*ae-reload-plugin-dev-dirs*`) или из папки `Plugins\`. Подключён `PlotFrameToPDF` — экспорт области, выделенной рамкой, в PDF; команды `ЭКСВПДФ` / `EXPTPDF`, `ПФП*`.

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

- Подробный XLS блоков может отличаться от прочих выводов на 0.01 м2 на слой из-за порядка округления (формулы округляют каждую строку, остальные выводы округляют подитоги).
- Полилинии с номиналом `_НЕПРЯМОУГ_` в XLS выводятся с высотой/шириной описанного прямоугольника со звёздочкой (справочные значения), площадь без формулы.

## Структура проекта

См. [docs/structure.md](docs/structure.md).

## Документация

- [Техническое задание](docs/promt.md)
- [Функциональные требования](docs/requirements.md)
- [Требования к разработке](docs/requirements-dev.md)
- [Структура проекта](docs/structure.md)
- [Быстрый старт](docs/START.md)
- [Марка элемента в картах раскроя](docs/marks-in-maps.md)
- [Блоки заполнения: размер в свету и припуск](docs/fill-allowance.md)
- [Маски поиска](docs/search-masks.md)
- [Автоматические тесты](tests/README.md)

## Установка

1. Скопируйте папку проекта на диск.
2. Добавьте пути `AutoExtraction`, `AutoExtraction\Extraction`, `AutoExtraction\common` в пути поддержки AutoCAD.
3. Загрузите `reload.lsp` или `extraction.lsp`.
4. Выполните команду `EXTRACTION`.

Подробнее см. [docs/START.md](docs/START.md).

## Тесты

Из корня репозитория (Windows PowerShell):

```powershell
.\scripts\test.ps1
```

Запускаются статические проверки `tests/static_check.py` (баланс скобок, инварианты кода, DCL, арность вызовов, плагины) и три логических теста без AutoCAD: `layer_filter_logic_test.py`, `mark_logic_test.py`, `fill_allowance_test.py`. Подробности — [tests/README.md](tests/README.md).

## Экспорт кода

Утилита `export_code.py` выгружает код проекта в текстовые файлы.

```bash
# один файл AutoExtraction_export.txt
python export_code.py .

# по каталогам: AutoExtraction_Extraction.txt, AutoExtraction_common.txt и т.д. в папке export_split
python export_code.py . --split-dirs

# свой каталог вывода
python export_code.py . --split-dirs --out-dir my_export

# отдельный .txt на каждый файл (например, fasonka.lsp.txt)
python export_code.py . --split-files

# добавить документацию (*.md) в выгрузку
python export_code.py . --split-dirs --include-docs

# исключить каталоги
python export_code.py . --split-dirs --exclude .git backups
```
