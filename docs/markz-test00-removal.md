# MARKZ.lsp — убрать TEST 00: пошаговое задание

**Кому:** разработка `moleval/MarkZ`.
**Редакция-основание:** Ред. 48.38 / 48.39.
**Зачем:** `TEST 00` запускает проверку кода на **каждой** загрузке модуля и, если загружен
AutoExtraction, зовёт `c:CHKALL` — полный обход всех модулей проекта и всех плагинов.
Проверки кода принадлежат AutoExtraction, модуль их не дублирует.

Основание и разбор — `docs/tz-markz-checks.md` в репозитории AutoExtraction.

---

## Шаг 0. Что должно остаться

| Остаётся | Удаляется |
|---|---|
| `TEST 01` … `TEST 16` — проверки **данных чертежа** | `TEST 00` — проверка **кода** (скобки, кавычки) |
| `mark:find-source`, `mark:file-open?`, `*mark:source-candidates*`, `*mark:source-file*` | `mark:test-parens`, `mark:scan-parens`, `mark:has-command?` |
| `mark:diagnostics` (без строки TEST 00), `mark:safe`, `mark:note-error`, `*mark:errors*` | вызов `(mark:test-parens)` при загрузке |

`mark:find-source` трогать **нельзя**: его использует `mark:snapshot-path` (снимок геометрии
кладётся рядом с исходником).

---

## Шаг 1. Правка скриптом (рекомендуется)

Скрипт делает все правки разом, сам определяет кодировку (cp1251 или utf-8) и переводы строк
(LF или CRLF), пишет файл в том же виде, в конце печатает самопроверку.
Повторный запуск безопасен.

```python
# -*- coding: utf-8 -*-
"""MARKZ.lsp: убрать TEST 00 (проверка скобок) — правка для репозитория MarkZ.

Кодировку и переводы строк определяет сам: читает и пишет в том же виде
(cp1251 или utf-8, LF или CRLF). Повторный запуск безопасен.

Запуск:  python patch.py [путь_к_MARKZ.lsp]
"""
import pathlib
import sys

path = pathlib.Path(sys.argv[1] if len(sys.argv) > 1 else 'MARKZ.lsp')
raw = path.read_bytes()

try:
    text = raw.decode('utf-8')
    enc = 'utf-8'
except UnicodeDecodeError:
    text = raw.decode('cp1251')
    enc = 'cp1251'

crlf = b'\r\n' in raw
if crlf:
    text = text.replace('\r\n', '\n')

orig_len = len(text)


def cut(start, ends, what):
    """Удалить от start до ближайшего из ends. Нет start — уже удалено."""
    global text
    i = text.find(start)
    if i < 0:
        print('  уже нет: %s' % what)
        return
    cand = [(text.find(e, i), e) for e in ends]
    cand = [(j, e) for j, e in cand if j > i]
    if not cand:
        sys.exit('ОСТАНОВ: для «%s» не найден конец блока.\n'
                 'Похоже, редакция файла отличается — пришлите файл, '
                 'поправим анкеры вручную.' % what)
    j = min(cand)[0]
    print('  удалено %d символов: %s' % (j - i, what))
    text = text[:i] + text[j:]


def swap(old, new, what):
    global text
    if old in text:
        text = text.replace(old, new, 1)
        print('  заменено: %s' % what)
    else:
        print('  уже нет: %s' % what)


print('файл: %s (%s, %s)' % (path, enc, 'CRLF' if crlf else 'LF'))

NEXT_TEST = ';;;--------------------- TEST 01'

# 1. сканер скобок (+ комментарий над ним)
cut(';; Сканер: баланс () вне строк и комментариев.\n',
    ['(defun mark:has-command?', '(defun mark:test-parens', NEXT_TEST],
    'mark:scan-parens (+ комментарий)')

# 2. mark:has-command? (появился в ред. 48.36, в старых редакциях его нет)
cut('(defun mark:has-command?',
    ['(defun mark:test-parens', NEXT_TEST],
    'mark:has-command?')

# 3. сам TEST 00
cut('(defun mark:test-parens (/ path res)', [NEXT_TEST], 'mark:test-parens')
text = text.rstrip('\n') + '\n\n'

# 4. вызов при загрузке — главная причина прогонов на каждом RELOAD.
# Строка стоит отдельно (в нулевой колонке) перед баннером загрузки.
call = '\n(mark:test-parens)\n'
if text.count(call) == 1:
    text = text.replace(call, '\n', 1)
    print('  убран вызов (mark:test-parens) в хвосте')
elif text.startswith('(mark:test-parens)\n'):
    text = text[len('(mark:test-parens)\n'):]
    print('  убран вызов (mark:test-parens) в начале файла')
else:
    print('  уже нет: вызов в хвосте')

# 5. строка в mark:diagnostics
old = "  (mark:safe 'mark:test-parens             \"TEST 00\")\n"
if old in text:
    text = text.replace(old, '', 1)
    print('  убрана строка TEST 00 из mark:diagnostics')
else:
    print('  уже нет: строка TEST 00 в mark:diagnostics')

# 6. заголовок секции, где остаются *mark:source-candidates* и mark:find-source
swap(';;;--------------------- TEST 00 — прогон скобок исходника ------------',
     ';;;--------------------- Исходник MARKZ.lsp (путь для снимка) ----------',
     'заголовок секции исходника')

# 7. заголовок хвоста файла
swap(';;; ---- TEST 00 + сообщение загрузки ------------------------------------',
     ';;; ---- сообщение загрузки ----------------------------------------------',
     'заголовок хвоста файла')

# 8. шапка файла
swap(';;;  Режим самодиагностики TEST 00 ... TEST 14.\n'
     ';;;  TEST 00 — прогон скобок исходника MARKZ.lsp.\n',
     ';;;  Режим самодиагностики TEST 01 ... TEST 16.\n'
     ';;;  Проверка кода — только через AutoExtraction (CHKALL / CHKALLFULL /\n'
     ';;;  CHKFILE / CHKLOAD); своего сканера в модуле нет.\n',
     'шапка файла')

if crlf:
    text = text.replace('\n', '\r\n')
path.write_bytes(text.encode(enc))
print('готово: было %d, стало %d символов' % (orig_len, len(text)))

# ---------------------------------------------------------------
# Самопроверка результата
# ---------------------------------------------------------------
out = path.read_bytes()
try:
    res = out.decode('utf-8')
except UnicodeDecodeError:
    res = out.decode('cp1251')
if b'\r\n' in out:
    res = res.replace('\r\n', '\n')

must_go = ['(mark:test-parens)', "(mark:safe 'mark:test-parens",
           '(defun mark:test-parens', '(defun mark:scan-parens',
           'mark:has-command?', 'mark:scan-parens', 'TEST 00']
must_stay = ['(defun mark:find-source', '(defun mark:file-open?',
             '(defun mark:snapshot-path']

bal = 0
instr = incom = esc = False
for ch in res:
    if ch == '\n':
        incom = esc = False
        continue
    if incom:
        continue
    if instr:
        if esc:
            esc = False
        elif ch == '\\':
            esc = True
        elif ch == '"':
            instr = False
        continue
    if ch == ';':
        incom = True
    elif ch == '"':
        instr = True
    elif ch == '(':
        bal += 1
    elif ch == ')':
        bal -= 1

print()
ok = True
for k in must_go:
    n = res.count(k)
    ok &= n == 0
    print('  %s  нет «%s» (найдено %d)' % ('OK  ' if n == 0 else 'FAIL', k, n))
for k in must_stay:
    n = res.count(k)
    ok &= n == 1
    print('  %s  остался «%s» (найдено %d)' % ('OK  ' if n == 1 else 'FAIL', k, n))
print('  %s  баланс скобок: %d' % ('OK  ' if bal == 0 else 'FAIL', bal))
print('  %s  кодировка %s, переводы строк %s' %
      ('OK  ', enc, 'CRLF' if crlf else 'LF'))
print('ИТОГ:', 'PASS' if ok and bal == 0 else 'FAIL')
```

Запуск:

```powershell
cd D:\MarkZ
python patch.py MARKZ.lsp
```

Ожидаемый конец вывода:

```
  OK    нет «(mark:test-parens)» (найдено 0)
  OK    нет «TEST 00» (найдено 0)
  OK    остался «(defun mark:find-source» (найдено 1)
  OK    остался «(defun mark:snapshot-path» (найдено 1)
  OK    баланс скобок: 0
ИТОГ: PASS
```

Если скрипт напечатал `ОСТАНОВ: ... не найден конец блока` — редакция файла отличается от
48.38/48.39, файл **не изменён**. Пришлите файл, анкеры поправим.

---

## Шаг 2. То же руками (если скрипт запускать негде)

### 2.1. Удалить вызов при загрузке — это главное

Найти в конце файла (перед баннером загрузки):

```lisp
(mark:test-parens)
(mark:emit (strcat "\n[MARKZ] " (if (mark:strp *mark:rev*) *mark:rev* "Ред. неизвестна")
```

Строку `(mark:test-parens)` удалить. В ред. 48.35 там же был `(princ (strcat "\n[MARKZ] "` —
принцип тот же: удаляется только строка вызова.

### 2.2. Убрать строку из `mark:diagnostics`

```lisp
(defun mark:diagnostics ()
  (mark:safe 'mark:test-parens             "TEST 00")   ; <-- удалить
  (mark:safe 'mark:test-fillings            "TEST 01")
  ...
```

### 2.3. Удалить три функции

| Функция | Где | Что вместе с ней |
|---|---|---|
| `mark:scan-parens` | секция «TEST 00 — прогон скобок исходника» | комментарий над ней: `;; Сканер: баланс () вне строк и комментариев.` |
| `mark:has-command?` | там же | комментарии про `atoms-family` над ней |
| `mark:test-parens` | там же | — |

Границы: от комментария перед `mark:scan-parens` до строки `;;;--------------------- TEST 01`.
В этом диапазоне **оставить**:

```lisp
(setq *mark:source-candidates* ...)      ; пути поиска исходника
(setq *mark:source-file* nil)
(defun mark:file-open? ...)              ; нужен mark:find-source
(defun mark:find-source ...)             ; нужен mark:snapshot-path
```

### 2.4. Тексты

- заголовок секции: `;;;--------------------- TEST 00 — прогон скобок исходника ------------`
  → `;;;--------------------- Исходник MARKZ.lsp (путь для снимка) ----------`;
- заголовок хвоста: `;;; ---- TEST 00 + сообщение загрузки ---...` → `;;; ---- сообщение загрузки ---...`;
- шапка файла:

```lisp
;;;  Режим самодиагностики TEST 00 ... TEST 14.
;;;  TEST 00 — прогон скобок исходника MARKZ.lsp.
```

заменить на:

```lisp
;;;  Режим самодиагностики TEST 01 ... TEST 16.
;;;  Проверка кода — только через AutoExtraction (CHKALL / CHKALLFULL /
;;;  CHKFILE / CHKLOAD); своего сканера в модуле нет.
```

Тесты **не перенумеровывать**: `TEST 01`…`TEST 16` остаются на своих номерах, иначе поедут
ссылки в `README.md`, `TZ.md` и в выводе `[INFO]`.

### 2.5. Документация

- `AGENTS.md`, п. 2: убрать «Модульные проверки — только страховка на случай, когда
  AutoExtraction не загружен: `TEST 00` (MarkZ, печатается при загрузке)». Вместо:
  «Проверка кода — только через AutoExtraction: `CHKALL` (кратко), `CHKALLFULL` (подробно),
  `CHKFILE` (один файл), `CHKLOAD` (место сбоя). Своего сканера в модуле нет».
- `README.md`, `TZ.md`: убрать `TEST 00` из перечней самодиагностики; в разделе загрузки
  указать, что штатная загрузка — через `RELOAD` AutoExtraction.

---

## Шаг 3. Приёмка

1. `RELOAD` в AutoCAD. В логе **не должно быть**:
   - `[TEST 00] Прогон скобок — CHKALL (AutoExtraction)`;
   - `[TEST 00] Отчёт CHKALL — выше в логе.`;
   - простыни `[CHK] <файл> размер N байт` / `[CHK] <файл> ИТОГ: 0 (OK)`.
   Должно быть: `[MARKZ] Ред. … загружена.` и таблица команд.
2. `CHKALL` — краткий отчёт: только проблемные файлы и `[CHK] всего проблем: N`.
   `CHKALLFULL` — подробный, в списке есть `D:\MarkZ\MARKZ.lsp`.
3. Сломанный `MARKZ.lsp` (снять одну `)`): `RELOAD` печатает `ОШИБКА ЗАГРУЗКИ`,
   затем `--- диагностика файла: скобки, строки, скрытые байты ---`,
   `--- поиск формы со сбоем (CHKLOAD) ---` и в конце
   `--- CHKALL: проверка всех файлов после сбоя загрузки ---`. Файл и место сбоя названы.
4. `МАРКАЗ` на рабочем чертеже: `TEST 01`…`TEST 16` печатаются как раньше, марки строятся.
5. Смена билда: правка не меняет поведения на приёмке — по `AGENTS.md` п. 3 меняется только
   `*mark:build*`. Рекомендуемое значение по умолчанию, если заказчик не задал своё.

---

## Шаг 4. Побочное, но важное: кодировка файла в репозитории

Ветки MarkZ расходятся по кодировке:

| Ветка | Кодировка |
|---|---|
| `arena/13d4d469-markz` (Ред. 48.39) | **UTF-8** |
| `arena/01a0e5ee`, `01a0d3b2`, `01a0e328`, `01a0e6a0`, `main` | cp1251 |

AutoCAD читает `.lsp` как ANSI (для русской локали — cp1251). Файл в UTF-8 даст кракозябры
во всех русских строках, включая баннер и сообщения `[TEST nn]`. Установленная у заказчика
копия — cp1251 (`D:\MarkZ\MARKZ.lsp`, 358 539 байт).

Перед выдачей сборки убедиться: **cp1251, без BOM**. Скрипт из шага 1 сам сохраняет ту
кодировку, в которой файл был прочитан, поэтому UTF-8 он не исправит — перекодировать надо
отдельно:

```python
import pathlib
p = pathlib.Path('MARKZ.lsp')
b = p.read_bytes()
if b[:3] == b'\xef\xbb\xbf':
    b = b[3:]
p.write_bytes(b.decode('utf-8').encode('cp1251'))
```

---

## Что НЕ входит в это задание

`МАРКАВСКОБКИ` в `MarkV.lsp` устроена так же: зовёт `CHKALL`, иначе работает свой сканер.
Правится отдельным этапом по тому же принципу. В текущем задании `MarkV.lsp` не трогать.

---

## Почему удаление безопасно

1. **Проверка на загрузке не может сработать.** С битым синтаксисом файл не загрузится —
   `(mark:test-parens)` не выполнится. Реально `TEST 00` ловил только «проверяю не тот
   файл»: `mark:find-source` ищет по путям поддержки и кандидатам, а грузится
   `D:\MarkZ\MARKZ.lsp`.
2. **`MARKZ.lsp` уже в реестре плагинов** AutoExtraction (`settings.ini`, `[plugins]`).
   `CHKALL` берёт путь тем же резолвером, что `RELOAD`, — проверяется ровно загружаемый файл.
3. **Наш доктор строже**: кроме скобок и кавычек он проверяет BOM, управляющие байты,
   не-ANSI символы вне строк, расхождение размера файла с содержимым; `CHKLOAD` локализует
   сломанную форму бисекцией. У `TEST 00` этого нет.
