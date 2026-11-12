#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Порт сравнений ПОИСКИДВИД / ПОИСКИДАТР (Extraction/searchattvis.lsp).

Команды выделяют вставки того же блока:
- ПОИСКИДВИД / SEARCHSAMEVIS — с теми же значениями видимости;
- ПОИСКИДАТР  / SEARCHSAMEATTR — с теми же атрибутами (тег + значение).

Порт повторяет ae-sv-same-vis-p и ae-sv-same-attr-p:

- строки сравниваются после trim, без учёта регистра;
- видимость: длины списков обязаны совпасть, порядок значим
  (у одного определения порядок свойств стабилен);
- атрибуты: тот же набор тегов, порядок не важен, лишний или
  недостающий тег — не совпадение.

Держится за исходник: наличие полей записи ae-sv-collect (nth 4 —
значения видимости, nth 3 — атрибуты), четырёх команд и их алиасов
проверяется чтением .lsp, чтобы тест не разошёлся с кодом.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

PASS = 0
FAIL = 0

try:
    sys.stdout.reconfigure(errors="replace")
except (AttributeError, ValueError):
    pass


def check(name, ok, detail=""):
    global PASS, FAIL
    if ok:
        PASS += 1
    else:
        FAIL += 1
        print(f"FAIL: {name}" + (f" — {detail}" if detail else ""))


def norm(s):
    """Повторение ae-sv-same-norm: trim + без регистра."""
    return str(s or "").strip().casefold()


def same_vis(a, b):
    """Повторение ae-sv-same-vis-p."""
    if a is None and b is None:
        return True
    if a is None or b is None:
        return False
    if len(a) != len(b):
        return False
    return all(norm(x) == norm(y) for x, y in zip(a, b))


def same_attr(a, b):
    """Повторение ae-sv-same-attr-p (a, b — списки пар «тег, значение»)."""
    if a is None and b is None:
        return True
    if a is None or b is None:
        return False
    if len(a) != len(b):
        return False
    left = {norm(tag): norm(val) for tag, val in a}
    right = {norm(tag): norm(val) for tag, val in b}
    if len(left) != len(a) or len(right) != len(b):
        return False                    # повтор тега — набор не определён
    return left == right


# ---------------------------------------------------------------
# Видимость
# ---------------------------------------------------------------
check("одинаковые значения видимости", same_vis(["П1"], ["П1"]))
check("регистр не важен", same_vis(["П1"], ["п1"]))
check("пробелы по краям не важны", same_vis(["  П1 "], ["П1"]))
check("два параметра, порядок значим",
      same_vis(["П1", "Л"], ["П1", "Л"]))
check("перестановка параметров — не совпадение",
      not same_vis(["П1", "Л"], ["Л", "П1"]))
check("разное состояние — не совпадение", not same_vis(["П1"], ["П2"]))
check("разная длина списка — не совпадение",
      not same_vis(["П1"], ["П1", "Л"]))
check("нет видимости у обоих", same_vis(None, None))
check("нет видимости у одного", not same_vis(None, ["П1"]))
check("пустой список — не совпадение с nil", not same_vis([], None))

# ---------------------------------------------------------------
# Атрибуты
# ---------------------------------------------------------------
A = [("МАРКА", "Кз-2"), ("ПРИМЕЧАНИЕ", "")]
check("те же теги и значения", same_attr(A, list(A)))
check("порядок тегов не важен",
      same_attr(A, [("ПРИМЕЧАНИЕ", ""), ("МАРКА", "Кз-2")]))
check("регистр тегов и значений не важен",
      same_attr(A, [("марка", "кз-2"), ("примечание", "")]))
check("trim по краям",
      same_attr([(" МАРКА ", " Кз-2 ")], [("МАРКА", "Кз-2")]))
check("другое значение — не совпадение",
      not same_attr(A, [("МАРКА", "Кз-3"), ("ПРИМЕЧАНИЕ", "")]))
check("лишний тег — не совпадение",
      not same_attr(A, [("МАРКА", "Кз-2"), ("ПРИМЕЧАНИЕ", ""), ("ЭТАЖ", "2")]))
check("недостающий тег — не совпадение",
      not same_attr(A, [("МАРКА", "Кз-2")]))
check("пустые атрибуты у обоих", same_attr([], []))
check("пустые против непустых", not same_attr([], A))
check("nil против списка", not same_attr(None, A))

# ---------------------------------------------------------------
# Имя определения (ae-sv-same-name-p)
# ---------------------------------------------------------------
def same_name(a, b):
    return norm(a) == norm(b)


check("имя блока без учёта регистра", same_name("Козырёк Кз-2", "козырёк кз-2"))
check("разные блоки — не совпадение",
      not same_name("Козырёк Кз-2", "Козырёк Кз-3"))
check("анонимное имя сравнивается как есть",
      not same_name("*U12", "Козырёк Кз-2"))

# ---------------------------------------------------------------
# Связь порта с исходником
# ---------------------------------------------------------------
ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT / "Extraction" / "searchattvis.lsp"
raw = SOURCE.read_bytes()
try:
    text = raw.decode("utf-8")
except UnicodeDecodeError:
    text = raw.decode("cp1251")

def has_defun(name: str) -> bool:
    """Точное определение, а не префикс: ae-sv-same-vis-p != ae-sv-same-vis-p-TMP."""
    return re.search(r"\(defun\s+" + re.escape(name) + r"[\s(]", text) is not None


for name in ("ae-sv-same-norm", "ae-sv-same-source", "ae-sv-same-quick-name",
             "ae-sv-same-name-p", "ae-sv-same-vis-p", "ae-sv-same-attr-p",
             "ae-sv-same-attr-norm", "ae-sv-same-ens", "ae-sv-same-run"):
    check(f"есть функция {name}", has_defun(name))

for cmd, alias in (("ПОИСКИДВИД", "SEARCHSAMEVIS"),
                   ("ПОИСКИДАТР", "SEARCHSAMEATTR")):
    check(f"есть команда {cmd}", has_defun("c:" + cmd))
    check(f"есть алиас {alias}", has_defun("c:" + alias))

def defun_body(name: str) -> str:
    """Тело (defun name ...) до закрывающей скобки формы."""
    found = re.search(r"\(defun\s+" + re.escape(name) + r"[\s(]", text)
    if not found:
        return ""
    start = found.start()
    depth = 0
    in_string = False
    escaped = False
    in_comment = False
    for pos in range(start, len(text)):
        ch = text[pos]
        if in_comment:
            if ch == "\n":
                in_comment = False
            continue
        if in_string:
            if escaped:
                escaped = False
            elif ch == "\\":
                escaped = True
            elif ch == '"':
                in_string = False
            continue
        if ch == ";":
            in_comment = True
        elif ch == '"':
            in_string = True
        elif ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
            if depth == 0:
                return text[start:pos + 1]
    return text[start:]


vis_body = defun_body("ae-sv-same-vis-p")
check("видимость сравнивается ПОЗИЦИОННО, а не по вхождению",
      "(car va)" in vis_body and "(car vb)" in vis_body,
      "нет парного сравнения car va / car vb")
check("в сравнении видимости нет поиска по вхождению",
      "member" not in vis_body and "assoc" not in vis_body,
      "порядок значений перестал быть значимым")
check("длины списков видимости сверяются",
      "(length va)" in vis_body and "(length vb)" in vis_body,
      "нет проверки равенства длин")

attr_body = defun_body("ae-sv-same-attr-p")
check("атрибуты сверяются по тегу (assoc), а не по позиции",
      "assoc" in attr_body,
      "нет поиска пары по тегу")
check("лишние/недостающие теги ловятся равенством длин",
      "(length na)" in attr_body and "(length nb)" in attr_body,
      "нет сверки количества атрибутов")

ens_body = defun_body("ae-sv-same-ens")
check("тяжёлое чтение идёт после отбора по имени",
      ens_body.find("ae-sv-same-quick-name") < ens_body.find("ae-sv-collect"),
      "ae-sv-collect вызван раньше быстрого отбора")

run_body = defun_body("ae-sv-same-run")
check("образец попадает в результат при любом слое",
      "eq ename en" in run_body,
      "нет исключения для исходного блока")
check("команды возвращают набор, а не глушат результат princ-ом",
      run_body.rstrip().endswith("ss))))))") or "ss))" in run_body[-80:],
      "хвост ae-sv-same-run не возвращает ss")

src_body = defun_body("ae-sv-same-source")
check("образец берётся из ssget _I", '(ssget "_I")' in src_body)
check("gripped-набор читается через ssgetfirst",
      "ssgetfirst" in src_body.lower())
check("при пустом выделении запрашивается блок",
      '(ssget "\\nВыберите блок: " \'((0 . "INSERT")))' in text)
check("слои фильтруются общим ae-sv-layer-skip-p",
      "(ae-sv-layer-skip-p ename)" in text)
check("кандидаты собираются обходом модели, а не ssget _X по чертежу",
      "ae-sv-all-model-inserts" in text)
check("быстрый отбор по имени до тяжёлого чтения",
      "ae-sv-same-quick-name" in text and
      text.index("ae-sv-same-quick-name en") <
      text.index("(ae-sv-collect (reverse names))"))
check("выделение через sssetfirst", "sssetfirst" in text)
check("нет префиксов чужого проекта",
      "zp-" not in text.lower() and "zaporno" not in text.lower())
check("окно существующих команд не тронуто",
      '(defun c:ПОИСКАТР' in text and '(defun c:ПОИСКВИД' in text and
      "ae-sv-open" in text)
check("в баннере есть строка ПОИСКИДВИД",
      '"SEARCHSAMEVIS / ПОИСКИДВИД"' in text)
check("в баннере есть строка ПОИСКИДАТР",
      '"SEARCHSAMEATTR / ПОИСКИДАТР"' in text)
check("ревизия модуля 5 в итоговом комментарии",
      "ред. 5" in text)

for msg, what in (("У блока нет параметра видимости", "нет видимости"),
                  ("У блока нет атрибутов", "нет атрибутов"),
                  ("Похожих блоков нет", "только исходный"),
                  ("Совпадений нет", "нет совпадений")):
    check(f"сообщение: {what}", msg in run_body)

check("при отказе возвращается nil, а не набор",
      run_body.count("nil)") >= 4,
      "ветки отказов не возвращают nil")

# ---------------------------------------------------------------
print()
if FAIL:
    print(f"PASS: {PASS}, FAIL: {FAIL}")
    raise SystemExit(1)

print("видимость: порядок значим, длины равны; атрибуты: набор, порядок не важен")
print(f"PASS: {PASS}, FAIL: {FAIL}")
print("[SEARCH-SAME][OK]")
