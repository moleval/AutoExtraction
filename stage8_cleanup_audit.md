# Шаг 8 — аудит мусора в коде (перед удалением)

Дата: 2026-09-21. Ветка: `arena/01a08717-autoextraction` (от main @ 739da15 + шаги 1–7).

## Метод построения таблицы ссылок (для новичка)

Идея простая: функция мёртвая, если её имя нигде не вызывается. Но искать надо хитро:

1. Взяты все 15 рабочих `.lsp` (9 в `Extraction/`, 6 в `common/`, `reload.lsp`) + 3 файла тестов (`tests/*.lsp`) только как источник ссылок.
2. Из каждого файла убраны строки (в кавычках) и комментарии (после `;`) в «маске» — чтобы вызов внутри строки
   не считался ложным совпадением, но учитывался отдельно (обработчики DCL вызываются ИЗ строк `action_tile`).
3. Найдено **374 определения `(defun …)`**. Дубликатов имён нет, кроме `*error*` — это локальные обработчики
   ошибок внутри `cutsheet-main` и `c:extraction`, так задумано.
4. Для каждого имени посчитаны ссылки: в коде (все файлы, кроме строки самого определения), в строках,
   в комментариях. Сравнение регистронезависимое (AutoLISP безразличен к регистру).
5. Отдельно проверены ссылки вне `.lsp`: `*.dcl`, `docs/`, `scripts/`, `*.prj`, `README.md`,
   `export_code.py`, `tests/*.md`, `tests/*.py` — **обращений к кандидатам нет**.
6. Динамическая диспетчеризация: `(read …)` в коде два места (`blockrename.lsp:472`,
   `extraction.lsp:375`) — оба разбирают ДАННЫЕ (значения полей диалога), не имена функций.

Из «неиспользуемых» заранее исключены: команды `c:*` (точки входа), `*error*`, и 23 функции,
вызываемые только из строк `action_tile`/диспетчера (`extraction-help`, `extraction-save`,
`blockrename-rename` и т.д.) — они живые, их DCL дёргает по имени.

## Категория A: закомментированный код — НЕ НАЙДЕН (0 строк)

Двумя независимыми проверками (строгий шаблон `;; (defun|setq|princ|…)` и свободный поиск
любых `(setq`/ `(defun` внутри `;`-комментариев) — 0 совпадений.

## Категория B: отладочные princ — НЕ НАЙДЕНЫ (0)

Нет «голых» дампов переменных `(princ x)` без строки формата; нет princ с маркерами
debug/trace/`>>>`/`!!!` и т.п. Рабочая диагностика пользователя (`[wrap] …`, баннеры, итоги раскроя)
в категорию «отладочные» НЕ входит и сохраняется.

## Категория C (В): неиспользуемые функции — 24 кандидата

Ссылки: 0 в коде всех `.lsp` (включая тесты), 0 в строках, 0 вне `.lsp`.

| Функция | Файл:строка | Размер тела |
|---|---|---|
| cl-str-smart-less | Extraction/cladding.lsp:57 | 6 |
| cl-partition-flat | Extraction/cladding.lsp:474 | 19 |
| n1-draw-cut-line | Extraction/cutline.lsp:288 | 5 |
| n1-mline-length | Extraction/cutline.lsp:555 | 1 |
| n1-safe-get-tile | Extraction/cutline.lsp:792 | 4 |
| cs-round2 | Extraction/cutsheet.lsp:83 | 2 |
| cs-safe-number | Extraction/cutsheet.lsp:95 | 2 |
| cs-block-area-from-props | Extraction/cutsheet.lsp:206 | 2 |
| cs-count-dyn-type | Extraction/cutsheet.lsp:338 | 9 |
| cs-butlast | Extraction/cutsheet.lsp:617 | 2 |
| cs-total-count | Extraction/cutsheet.lsp:645 | 1 |
| extraction-subsystem-layer-list | Extraction/extraction.lsp:514 | 14 |
| extraction-clear-layer-selection | Extraction/extraction.lsp:534 | 4 |
| tu-layer-names | common/layer-utils.lsp:6 | 24 |
| tu-filtered-layer-names | common/layer-utils.lsp:117 | 16 |
| su-has-length-property | common/select-utils.lsp:261 | 3 |
| su-has-width-property | common/select-utils.lsp:264 | 3 |
| su-has-height-property | common/select-utils.lsp:268 | 3 |
| tu-safe-call | common/task-utils.lsp:30 | 7 |
| tu-safe-call-result | common/task-utils.lsp:39 | 4 |
| tu-string-empty-p | common/task-utils.lsp:45 | 3 |
| tu-list-to-comma-string | common/task-utils.lsp:78 | 10 |
| tu-safe-rtos | common/task-utils.lsp:127 | 6 |
| tu-ensure-directory-exists-p | common/task-utils.lsp:151 | 6 |

Итого ≈174 строки мёртвого кода. Это «библиотечные» заготовки (su-*/tu-*/cs-*/cl-*/n1-*),
оставшиеся после рефакторингов: реальных вызывателей не осталось ни в одном модуле.

## План удаления

- Одна категория = один коммит: «Стабилизация кода Шаг 8: Очистка кода от неактуальных фрагментов».
- Удаление строго по балансу скобок (форма `(defun …)` целиком + прилипший описательный комментарий
  прямо над ней, если он не декоративный заголовок секции).
- После удаления: сканер скобок по всем затронутым файлам + defun 374 → 350.
- Затем у пользователя: `CHKALL` + `RELOAD` (+ прогоновый тест).

## Что осознанно НЕ трогаем

- Команды `c:*`, локальные `*error*`, 23 строково-вызываемые DCL-функции.
- Рабочие `princ` (диагностика `[wrap]`, баннеры, статистика) — они не отладочные.
- Любые алгоритмы раскроя (принципы стабилизации).
