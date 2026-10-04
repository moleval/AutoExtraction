# AutoExtraction — автоматические тесты

Быстрый запуск из корня репозитория:

```powershell
.\scripts\test.ps1
```

Проверяется структура проекта, баланс скобок LISP, дублирующиеся `defun`, DCL keys и базовые регрессионные проверки.

Каждая найденная ошибка должна становиться отдельным regression test.

## Self-тесты, запускаемые вручную в AutoCAD

| Файл | Что проверяет | Запуск |
|---|---|---|
| `tests/u1-selftest.lsp` | примитивы XLS (U1) | `(load "D:/AutoExtraction/tests/u1-selftest.lsp")` |
| `tests/validation-test.lsp` | парсер и предикаты V1/V2 (Этап 2): `tu-parse-number` (числа/строки/запятая/разрядка/VARIANT), `tu-parse-int-list`, все `tu-*-p` — около 50 проверок; ожидание `[V1][OK]` | после RELOAD: `(load "D:/AutoExtraction/tests/validation-test.lsp")` |
| `tests/u2-xlsdiff.lsp` | семантический diff XLS против эталонов | см. заголовок файла |

Зафиксированное поведение (ревью V1/V2, P1): пробелы в числовых строках — разрядка, поэтому `"1 2"` → `12`. Пересмотр семантики возможен только на V3.
