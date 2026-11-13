#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Порт сравнения ПОИСКИДМЛИН / SEARCHSAMEMLINE (Extraction/searchattvis.lsp).

Команда выделяет мультилинии с тем же стилем, что у выбранной.
Сравнение — по имени стиля (DXF 2 мультилинии, у стиля — ключ
в словаре ACAD_MLINESTYLE): trim и без учёта регистра.

Порт повторяет ae-sv-ml-run:
- кандидаты — только MLINE пространства модели;
- образец попадает в результат всегда, даже если обход модели его
  не вернул или он лежит на пропущенном слое;
- остальные отсекаются общим ae-sv-layer-skip-p;
- набор собирается один раз, число «найдено» = выделено + пропущено.

Держится за исходник: имена функций, фильтр MLINE, способ получения
образца и формат итоговых строк читаются из .lsp, чтобы тест не
разошёлся с кодом.
"""

from __future__ import annotations

import sys
from pathlib import Path

PASS = 0
FAIL = 0

try:
    sys.stdout.reconfigure(errors="replace")
except (AttributeError, ValueError):
    pass

ROOT = Path(__file__).resolve().parent.parent
LSP = ROOT / "Extraction" / "searchattvis.lsp"


def check(name, ok, detail=""):
    global PASS, FAIL
    if ok:
        PASS += 1
    else:
        FAIL += 1
        print(f"FAIL: {name}" + (f" — {detail}" if detail else ""))


def norm(s):
    """Повторение ae-sv-same-norm: trim + без учёта регистра."""
    return str(s or "").strip().casefold()


def same_style(a, b):
    """Повторение сравнения в ae-sv-ml-run: пустое имя — не совпадение."""
    if a is None or b is None or a == "" or b == "":
        return False
    return norm(a) == norm(b)


def collect(sample, mlines, skipped_layers, sample_in_walk=True):
    """Повторение ae-sv-ml-run: (набор, выделено, пропущено)."""
    picked, skipped = [], 0
    for en in mlines:
        if not same_style(en.get("style"), sample.get("style")):
            continue
        if en["name"] == sample["name"]:
            picked.append(en)
        elif en["layer"] in skipped_layers:
            skipped += 1
        else:
            picked.append(en)
    if sample_in_walk or sample["name"] not in [e["name"] for e in picked]:
        if sample["name"] not in [e["name"] for e in picked]:
            picked.append(sample)
    return picked, len(picked), skipped


def ml(name, style, layer="0"):
    return {"name": name, "style": style, "layer": layer}


def defun_body(text, name):
    """Тело (defun name ...) с балансом скобок, без учёта комментариев."""
    start = text.find(f"(defun {name} ")
    if start < 0:
        start = text.find(f"(defun {name}\n")
    if start < 0:
        return None
    depth, i, n = 0, start, len(text)
    in_str, in_com = False, False
    while i < n:
        ch = text[i]
        if in_com:
            if ch == "\n":
                in_com = False
        elif in_str:
            if ch == "\\":
                i += 2
                continue
            if ch == '"':
                in_str = False
        else:
            if ch == ";":
                in_com = True
            elif ch == '"':
                in_str = True
            elif ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0:
                    return text[start:i + 1]
        i += 1
    return None


def main():
    text = LSP.read_bytes().decode("cp1251", errors="replace")

    # --- 1. Сравнение имён стилей -------------------------------------
    check("то же имя стиля", same_style("Стойка", "Стойка"))
    check("регистр не важен", same_style("Стойка", "СТОЙКА"))
    check("пробелы по краям не важны", same_style(" Стойка ", "Стойка"))
    check("разные стили не совпадают", not same_style("Стойка", "Стойка_1"))
    check("пустое имя не совпадает с именем", not same_style("", "Стойка"))
    check("два пустых имени — не совпадение", not same_style("", ""))
    check("None не совпадает с именем", not same_style(None, "Стойка"))
    check("Стандарт не равен СтОЙКА", not same_style("Стандарт", "Стойка"))

    # --- 2. Сбор набора -----------------------------------------------
    s = ml("A", "Стойка")
    picked, sel, skip = collect(s, [
        ml("A", "Стойка"),
        ml("B", "Стойка"),
        ml("C", "Стойка_1"),
        ml("D", "стойка"),
        ml("E", "Другая"),
    ], skipped_layers=set())
    check("найдены все линии того же стиля",
          [e["name"] for e in picked] == ["A", "B", "D"],
          f"получили {[e['name'] for e in picked]}")
    check("другие стили не попали", "C" not in [e["name"] for e in picked])
    check("счётчик выделенных совпал с набором", sel == 3 and skip == 0)

    # --- 3. Замороженный слой: отброшен, но посчитан -------------------
    picked, sel, skip = collect(s, [
        ml("A", "Стойка"),
        ml("B", "Стойка", layer="FRZ"),
        ml("C", "Стойка"),
    ], skipped_layers={"FRZ"})
    check("вставка на пропущенном слое не выделена",
          [e["name"] for e in picked] == ["A", "C"])
    check("пропущенная посчитана отдельно", skip == 1)
    check("итог «найдено» = выделено + пропущено", sel + skip == 3)

    # --- 4. Образец всегда в результате --------------------------------
    sample = ml("A", "Стойка", layer="FRZ")
    picked, sel, skip = collect(sample, [
        ml("B", "Стойка"),
        ml("C", "Стойка"),
    ], skipped_layers={"FRZ"}, sample_in_walk=False)
    check("образец на пропущенном слое остаётся в наборе",
          "A" in [e["name"] for e in picked])
    check("образец не считается пропущенным", skip == 0)
    check("в наборе три линии: две найденные и образец", sel == 3)

    # --- 5. Никого, кроме образца --------------------------------------
    picked, sel, skip = collect(s, [ml("A", "Стойка"), ml("Z", "Другая")],
                                skipped_layers=set())
    check("один образец — набор из одной линии", sel == 1)

    # --- 6. Связь с исходником -----------------------------------------
    check("модуль на месте", LSP.exists())
    for name in ("c:ПОИСКИДМЛИН", "c:SEARCHSAMEMLINE", "ae-sv-ml-run",
                 "ae-sv-ml-source", "ae-sv-ml-style", "ae-sv-all-model-mlines",
                 "ae-sv-ml-sample", "ae-sv-ml-elements", "ae-sv-rtos1"):
        check(f"определена функция {name}", defun_body(text, name) is not None)

    run = defun_body(text, "ae-sv-ml-run") or ""
    check("сравнение через общий нормировщик ae-sv-same-norm",
          "ae-sv-same-norm" in run)
    check("слой фильтруется общим ae-sv-layer-skip-p",
          "ae-sv-layer-skip-p" in run)
    check("образец добавляется принудительно (ssmemb)",
          "ssmemb" in run)
    check("набор кладётся в *ae-sv-ml-result*", "*ae-sv-ml-result*" in run)
    check("команда печатает две строки: образец и итог",
          "ae-sv-ml-sample" in run and "мультилиний с этим стилем" in run)

    src = defun_body(text, "ae-sv-ml-source") or ""
    check("образец из подразумеваемого выделения",
          '(ssget "_I")' in src and "ssgetfirst" in src)
    check("фильтр по типу MLINE", '"MLINE"' in src)
    check("лист отсекается", "assoc 67" in src)

    walker = defun_body(text, "ae-sv-all-model-mlines") or ""
    check("обход блока *Model_Space", "*Model_Space" in walker)
    check("обход только мультилиний модели", "ae-sv-ml-model-p" in walker)

    style = defun_body(text, "ae-sv-ml-style") or ""
    check("имя стиля — DXF 2", "assoc 2" in style)
    check("используется общий su-mline-style-name", "su-mline-style-name" in style)

    for cmd in ("c:ПОИСКИДМЛИН", "c:SEARCHSAMEMLINE"):
        body = (defun_body(text, cmd) or "").rstrip()
        inner = body[:-1].rstrip() if body.endswith(")") else body
        check(f"{cmd} заканчивается (princ)", inner.endswith("(princ)"))

    check("в таблице команд одна строка модуля SEARCHATTVIS",
          '"SEARCHATTVIS" "поиск и выбор блоков, атрибутов, видимостей, млиний"' in text)
    check("строка-команда в таблицу не добавлена",
          '"SEARCHSAMEMLINE / ПОИСКИДМЛИН"' not in text)
    check("ревизия модуля 6", "ред. 6" in text)
    check("нет префиксов чужого проекта",
          "zp-" not in text.lower() and "zaporno" not in text.lower())
    check("окно существующих команд не тронуто",
          "(defun c:ПОИСКАТР" in text and "(defun c:ПОИСКВИД" in text)

    print()
    print("стиль мультилинии сравнивается по имени, trim и без учёта регистра;"
          " образец всегда в наборе")
    print(f"PASS: {PASS}, FAIL: {FAIL}")
    if FAIL:
        return 1
    print("[SEARCH-MLINE][OK]")
    return 0


if __name__ == "__main__":
    sys.exit(main())
