# AutoExtraction

Модульная система AutoLISP для AutoCAD: извлечение данных из чертежей, подсчёт
материалов, раскрой хлыстов и листов.

## Возможности

- Запуск задач через диалог `EXTRACTION` / `ЭКСТРАКЦИЯ` или отдельными командами.
- Задачи:
  - **Фасонка** (`FASONKA`) — погонаж фасонного железа;
  - **Подсистема** (`SUBSYSTEM`) — мерные и штучные элементы подсистемы;
  - **Облицовка** (`CLADDING`, `CLBLOCKS`) — полилинии и блоки облицовки;
  - **Заполнение** (`ZAPOLNENIE`) — стеклопакеты и панели по размеру «в свету» с припуском на раму;
  - **Витраж** — пока заглушка, данные не извлекаются.
- Раскрой:
  - **Раскрой хлыста** (`CUTLINE`) — размещение деталей на хлыстах заданной длины с учётом реза;
  - **Раскрой листа** (`CUTSHEET`) — размещение деталей на листе заданного размера, поворот на 90° по желанию;
  - марка детали подписывается на карте раскроя и попадает в перечень (см. [docs/marks-in-maps.md](docs/marks-in-maps.md)).
- Режимы отчёта:
  - **DETAIL** — подробный перечень с промежуточными итогами по группам;
  - **SUMMARY** — сводный отчёт с общими итогами.
- Экспорт:
  - **Excel `.xls`** — XML Spreadsheet с формулами, без Microsoft Excel и COM;
  - **CSV `.csv`** — запасной вариант, если XLS не удалось записать (например, файл открыт в Excel);
  - **GAL `.gal`** (чекбокс `.txt` в диалоге) — текстовый формат для станка с ЧПУ;
  - **таблица или карта AutoCAD** в чертеже.
- Групповые фильтры слоёв: **Мои**, **Фасады**, **Витражи**, **Окна** — см. [docs/requirements.md](docs/requirements.md), раздел 1.1.
- Поиск по спискам слоёв и блоков с масками `*`, `?`, `#`, `@` — см. [docs/search-masks.md](docs/search-masks.md).
- Панель «Блоки»: копия, вставка, переименование блоков.
- Плагины: сторонние модули загружаются тем же `RELOAD` (секция `--- PLUGINS ---`) — из каталога разработки (`*ae-reload-plugin-dev-dirs*`) или из папки `Plugins\`. Подключён `PlotFrameToPDF` — экспорт области, выделенной рамкой, в PDF (`ЭКСВПДФ` / `EXPTPDF`, `ПФП*`).

## Архитектура

### Разбиение таблиц по страницам

Все модули используют общий алгоритм `tc-partition-flat` из `common/table-utils.lsp`.
Он равномерно распределяет строки по таблицам и не отрывает подитог группы от своих строк.

| Модуль | Подготовка строк | Рендер |
|---|---|---|
| Облицовка | `cl-build-flat-items` | `cl-create-table-detail` |
| Подсистема | `su-build-flat-items` | `subsystem-create-table-detail` |
| Заполнение | `zp-build-flat-items` | `zapolnenie-create-table-detail` |
| Фасонка | `fs-build-flat-items` | `fasonka-create-table-detail` |

### Оформление таблиц AutoCAD

Единый стандарт в `common/table-utils.lsp` (функции `ts-ac-*`):

- `ts-ac-title` — заголовок таблицы (объединение и подчёркивание);
- `ts-ac-header` — шапка (заполнение и центрирование);
- `ts-ac-subtotal` — подитог группы (жирный номер и текст);
- `ts-ac-total` — общий итог (объединение и текст).

### Оформление Excel

Стили XML Spreadsheet задаются функцией `eu-xml-styles` в `common/excel-utils.lsp`.
Все модули экспорта используют один набор: `Default`, `Title`, `Header`, `Data`, `DataLeft`, `Cut`, `Num`, `Total`, `TotalNum`.

### Допущения

- Подробный XLS блоков может отличаться от других выводов на 0,01 м² на слой из-за порядка округления: формулы округляют каждую строку, остальные выводы округляют подитоги.
- Полилинии с номиналом `_НЕПРЯМОУГ_` в XLS выводятся с высотой и шириной описанного прямоугольника со звёздочкой (справочные значения), площадь без формулы.

## Структура проекта

См. [docs/structure.md](docs/structure.md).

## Документация

- [Быстрый старт](docs/START.md)
- [Функциональные требования](docs/requirements.md)
- [Техническое задание](docs/promt.md)
- [Требования к разработке](docs/requirements-dev.md)
- [Структура проекта](docs/structure.md)
- [План будущих задач](docs/next-tasks.md)
- [Дорожная карта](roadmap.md) и [технический долг](techdebt.md)

## Установка

1. Скопируйте папку проекта на диск, например `C:\AutoExtraction`.
2. Добавьте пути `AutoExtraction`, `AutoExtraction\Extraction`, `AutoExtraction\common` в пути поддержки AutoCAD.
3. Загрузите `reload.lsp` или `Extraction\extraction.lsp`.
4. Выполните команду `EXTRACTION`.

Подробнее — [docs/START.md](docs/START.md).

## Экспорт кода

Утилита `export_code.py` выгружает код проекта в текстовые файлы.

По умолчанию создаётся один файл `AutoExtraction_export.txt`:

```bash
python export_code.py .
```

Разделение по каталогам — файлы `AutoExtraction_<каталог>.txt` в папке `export_split`:

```bash
python export_code.py . --split-dirs
python export_code.py . --split-dirs --out-dir my_export
```

Разделение по отдельным файлам — каждый `.lsp`, `.dcl`, `.prj` сохраняется отдельным `.txt`
с тем же именем (например, `fasonka.lsp.txt`):

```bash
python export_code.py . --split-files
```

Документация включается флагом `--include-docs`, исключение каталогов — `--exclude`:

```bash
python export_code.py . --split-dirs --include-docs
python export_code.py . --split-dirs --exclude .git backups
```

## Лицензия

Лицензия не указана. Уточнить у владельца репозитория.
