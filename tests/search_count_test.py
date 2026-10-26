#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Спецификационный тест размеров в «Подсчёте» окна поиска (ПОИСКАТР / ПОИСКВИД).

Модель НЕ запускает AutoCAD: проверяется ПОРТ логики из
`Extraction/searchattvis.lsp` (ae-sv-size-of и помощники) и её совпадение
с цепочкой размеров ЗАПОЛНЕНИЯ (`Extraction/zapolnenie.lsp`).

Правила:
- площадь: сначала размер «в свету» («Высота в свету», «Ширина в свету»), затем «Высота» и «Ширина»,
  для ширины после них — «Длина»; по каждому ключу сначала точное имя (регистр не важен),
  затем вхождение в имени БЕЗ цифр: «Высота двери» берётся, «Высота1» — нет;
- если «Высоты» нет — блок в площадь не попадает;
- припуск на раму (input.frame.allowance, по умолчанию 26) добавляется к высоте и ширине блоков,
  у которых имя или первое значение видимости подходит под маски задачи ЗАПОЛНЕНИЕ (input.block);
  пустой список масок — припуска нет (как в раскрое); у остальных блоков припуска нет;
- размеры округляются до миллиметров ДО припуска;
- погонаж («Длина», только точное имя) припуска не получает.

Только stdlib. Запуск: python3 tests/search_count_test.py
"""

from __future__ import annotations

import fnmatch
import re
import sys

PASS = 0
FAIL = 0

HEIGHT_KEYS = ["Высота в свету", "Высота"]
WIDTH_KEYS = ["Ширина в свету", "Ширина", "Длина"]
FRAME_MASKS = ["*Заполнение*", "*Стекло*"]   # пример настроек task.ZAPOLNENIE / input.block
DEFAULT_ALLOWANCE = 26


def check(name, ok):
    global PASS, FAIL
    if ok:
        PASS += 1
        print("[SEARCH][PASS] %s" % name)
    else:
        FAIL += 1
        print("[SEARCH][FAIL] %s" % name)


# ---------------------------------------------------------------
# Порт: свойства, числа, размеры
# ---------------------------------------------------------------

def prop_exact(props, wanted):
    for name, value in props:
        if name.strip().lower() == wanted.lower():
            return value
    return None


def mm(value):
    """ae-sv-mm: округление до миллиметров, 0 и меньше — None."""
    if value is None:
        return None
    r = int(value + 0.5)
    return r if r > 0 else None


def num(text):
    """ae-sv-num: строка -> число, пустая строка -> None."""
    if text is None or text == "":
        return None
    return float(text.replace(",", "."))


def has_digit(text):
    return any(ch.isdigit() for ch in text)


def dim_key(props, key):
    """ae-sv-dim-key: точное имя, затем вхождение в имени без цифр."""
    v = mm(num(prop_exact(props, key)))
    if v is None:
        for name, value in props:
            if key.lower() in name.lower() and not has_digit(name):
                v = mm(num(value))
                if v is not None:
                    break
    return v


def dim_of(props, keys):
    """ae-sv-dim-of: первый найденный размер по списку ключей (порядок важен)."""
    for k in keys:
        v = dim_key(props, k)
        if v is not None:
            return v
    return None


def is_frame(name, vis, masks):
    """ae-sv-frame-p: имя или первое значение видимости подходит под маску (регистр не важен); пустой список — нет."""
    if not masks:
        return False
    values = [v for v in (name, vis) if v]
    return any(fnmatch.fnmatch(v.lower(), m.lower()) for v in values for m in masks)


def size_of(name, props, allowance=DEFAULT_ALLOWANCE, vis="", masks=FRAME_MASKS):
    """ae-sv-size-of: ('A', h, w) — площадь; ('L', l, None) — погонаж; None."""
    h = dim_of(props, HEIGHT_KEYS)
    w = dim_of(props, WIDTH_KEYS)
    l = mm(num(prop_exact(props, "Длина")))
    if h is not None and is_frame(name, vis, masks):
        h += allowance
        if w is not None:
            w += allowance
    if h is not None and w is not None:
        return ("A", h, w)
    if l is not None:
        return ("L", l, None)
    return None


# ---------------------------------------------------------------
# Сценарии
# ---------------------------------------------------------------

def scenario_inch_glass():
    """Заполнение в витраж: «в свету» + припуск, как в ЗАПОЛНЕНИИ."""
    print("\n--- Заполнение в витраж: размер «в свету» ---")
    props = [("Высота в свету", "750"), ("Ширина в свету", "974")]
    check("«в свету» даёт площадь",
          size_of("Заполнение в витраж", props) == ("A", 776, 1000))
    check("припуск 26 мм на обе стороны (как у ЗАПОЛНЕНИЯ)",
          size_of("Заполнение в витраж", props, 26) == ("A", 776, 1000))
    check("припуск из настроек (30 мм)",
          size_of("Заполнение в витраж", props, 30) == ("A", 780, 1004))


def scenario_priority():
    print("\n--- Приоритет: «в свету» раньше обычных имён ---")
    props = [("Высота", "1000"), ("Высота в свету", "750"),
             ("Ширина", "600"), ("Ширина в свету", "700")]
    check("«Высота в свету» важнее «Высота»",
          size_of("Заполнение в витраж", props) == ("A", 776, 726))


def scenario_fallback_length():
    print("\n--- Ширина: запасной вариант «Длина» ---")
    props = [("Высота в свету", "750"), ("Длина", "900")]
    check("без «Ширины» берётся «Длина», припуск применяется",
          size_of("Заполнение в витраж", props) == ("A", 776, 926))


def scenario_numbered_names():
    print("\n--- Точное совпадение: номера не берутся ---")
    props = [("Высота1", "1200"), ("Ширина2", "600")]
    check("«Высота1» и «Ширина2» не дают площадь",
          size_of("Окно", props) is None)


def scenario_no_height():
    print("\n--- Без высоты блок не попадает в площадь ---")
    props = [("Ширина в свету", "974"), ("Длина", "1500")]
    check("без высоты — только погонаж (без припуска)",
          size_of("Заполнение в витраж", props) == ("L", 1500, None))
    check("без высоты и длины — не считается",
          size_of("Заполнение в витраж", [("Ширина в свету", "974")]) is None)


def scenario_no_mask_no_allowance():
    print("\n--- Припуск только по маскам ---")
    props = [("Высота", "1200"), ("Ширина", "600")]
    check("блок «Окно» — без припуска",
          size_of("Окно", props) == ("A", 1200, 600))
    check("блок «Заполнение» — с припуском",
          size_of("Заполнение", props) == ("A", 1226, 626))


def scenario_mask_case():
    print("\n--- Маски: регистр не важен, «Стекло*» ловит «Стеклопакет» ---")
    props = [("Высота", "1000"), ("Ширина", "500")]
    check("«стеклопакет 4мм» — с припуском",
          size_of("стеклопакет 4мм", props) == ("A", 1026, 526))
    check("«СТЕКЛО» — с припуском",
          size_of("СТЕКЛО", props) == ("A", 1026, 526))


def scenario_door_names():
    print("\n--- Подсчёт: «Высота двери» и «Ширина двери» (подстрока без цифр) ---")
    props = [("Высота двери", "2100"), ("Ширина двери", "900")]
    check("«Высота двери» и «Ширина двери» дают площадь",
          size_of("Дверь", props) == ("A", 2100, 900))
    check("«Высота двери 2» (с цифрой) не берётся",
          size_of("Дверь", [("Высота двери 2", "2100"), ("Ширина двери", "900")]) is None)
    check("точное «Ширина» важнее «Ширина двери»",
          size_of("Дверь", [("Высота", "2100"), ("Ширина двери", "900"), ("Ширина", "800")]) == ("A", 2100, 800))
    check("«Длина двери» — запасная ширина",
          size_of("Дверь", [("Высота", "2100"), ("Длина двери", "900")]) == ("A", 2100, 900))
    check("погонаж — только точное «Длина»: «Длина двери» без высоты не даёт погонажа",
          size_of("Дверь", [("Длина двери", "900")]) is None)


def scenario_frame_masks():
    print("\n--- Маски припуска: как у раскроя (task.ZAPOLNENIE / input.block) ---")
    props = [("Высота", "1000"), ("Ширина", "500")]
    check("по видимости: «Окно» с видимостью «Заполнение 1» — с припуском",
          size_of("Окно", props, vis="Заполнение 1") == ("A", 1026, 526))
    check("без совпадения имени и видимости — без припуска",
          size_of("Окно", props, vis="Открыто") == ("A", 1000, 500))
    check("пустая маска — припуска нет (как в раскрое)",
          size_of("Заполнение", props, masks=[]) == ("A", 1000, 500))
    check("маска из настроек, например «*Витраж*», работает",
          size_of("Витраж 3", props, masks=["*Витраж*"]) == ("A", 1026, 526))


def scenario_rounding():
    print("\n--- Округление до мм ДО припуска ---")
    props = [("Высота в свету", "750,4"), ("Ширина в свету", "974,6")]
    check("750,4 -> 750, 974,6 -> 975, затем +26",
          size_of("Заполнение в витраж", props) == ("A", 776, 1001))


# ---------------------------------------------------------------
# Порт: раздел таблицы («Всего» последней строкой) и строка состояния
# ---------------------------------------------------------------

def section_rows(title, positions):
    """ae-sv-section: [заголовок, позиции..., «Всего»]; positions — (марка, d1, d2, n)."""
    area = "м2" in title
    cnt = sum(p[3] for p in positions)
    if area:
        total_sum = sum(p[1] * p[2] * p[3] / 1000000.0 for p in positions)
        head = ["№", "Высота", "Ширина", "Кол-во", "Сумма, м2"]
    else:
        total_sum = sum(p[1] * p[3] / 1000.0 for p in positions)
        head = ["№", "Длина, мм", "Кол-во", "Сумма, м.п."]
    rows = [head]
    for i, p in enumerate(positions, 1):
        if area:
            rows.append([str(i), str(p[1]), str(p[2]), str(p[3]), "%.2f" % (p[1] * p[2] * p[3] / 1000000.0)])
        else:
            rows.append([str(i), str(p[1]), str(p[3]), "%.2f" % (p[1] * p[3] / 1000.0)])
    total = ["Всего:"] + [""] * (len(head) - 3) + [str(cnt), "%.2f" % total_sum]
    rows.append(total)
    return rows


def totals_text(sections):
    """ae-sv-totals-text: «Всего: N шт. S м.п.» / «... S м2», разделы через «; »."""
    if not sections:
        return "Всего: 0 шт."
    out = ""
    for title, rows in sections:
        tot = rows[-1]
        unit = "м.п." if "м.п." in title else "м2"
        out += ("Всего: " if out == "" else "; ") + tot[-2] + " шт. " + tot[-1] + " " + unit
    return out


def scenario_layout():
    print("\n--- Подсчёт: «Всего» внизу, строка состояния — итоги ---")
    lin = section_rows("м.п.", [("", 1500, 0, 6), ("", 900, 0, 8)])
    check("«Всего:» — последняя строка раздела",
          lin[-1][0] == "Всего:" and lin[0][0] == "№")
    check("позиции нумеруются с 1, «Всего» без номера",
          [r[0] for r in lin[1:-1]] == ["1", "2"] and lin[-1][0] == "Всего:")
    area = section_rows("м2", [("", 1200, 600, 5)])
    check("площадь: «Всего» последней, сумма 3.60",
          area[-1][-1] == "3.60" and area[-1][-2] == "5")
    check("строка состояния: итоги погонажа",
          totals_text([("м.п.", lin)]) == "Всего: 14 шт. 16.20 м.п.")
    check("строка состояния: погонаж и площадь через «; »",
          totals_text([("м.п.", lin), ("м2", area)])
          == "Всего: 14 шт. 16.20 м.п.; 5 шт. 3.60 м2")
    check("строка состояния без данных — «Всего: 0 шт.»",
          totals_text([]) == "Всего: 0 шт.")


# ---------------------------------------------------------------
# Порт: сортировка групп (марка естественно, затем высота и ширина)
# ---------------------------------------------------------------

def nat_key(mark):
    """ae-sv-nat-key: цифровые участки дополняются нулями, регистр не важен."""
    out, run = "", ""
    for ch in mark.lower():
        if ch.isdigit():
            run += ch
        else:
            out += (run.zfill(10) if run else "") + ch
            run = ""
    return out + (run.zfill(10) if run else "")


def sort_area(groups):
    """ae-sv-sorted для площади: (марка, высота, ширина) по возрастанию."""
    return sorted(groups, key=lambda g: (nat_key(g[0]), g[1], g[2]))


def sort_lin(groups):
    """ae-sv-sorted для погонажа: (марка, длина) по возрастанию."""
    return sorted(groups, key=lambda g: (nat_key(g[0]), g[1]))


def scenario_sort():
    print("\n--- Сортировка: марка, затем высота и ширина ---")
    area = [("М10", 1200, 600), ("М2", 900, 800), ("М2", 900, 500),
            ("М2", 600, 900), ("", 1000, 1000), ("М1", 2000, 300)]
    check("площадь: М1, М2 (по высоте, затем ширине), М10; без марки — первой",
          sort_area(area) == [("", 1000, 1000), ("М1", 2000, 300),
                              ("М2", 600, 900), ("М2", 900, 500),
                              ("М2", 900, 800), ("М10", 1200, 600)])
    lin = [("М10", 1500), ("М2", 900), ("М2", 600)]
    check("погонаж: М2 (по длине), затем М10",
          sort_lin(lin) == [("М2", 600), ("М2", 900), ("М10", 1500)])
    check("«М2» идёт раньше «М10» (не лексикографически)",
          nat_key("М2") < nat_key("М10"))


# ----------------------------------------------------------------------
# Таблица окна «Подсчёт» (порт ae-sv-cnt-* из searchattvis.lsp)
# ----------------------------------------------------------------------

WIDTHS = {"n": [3, 7, 7, 6, 9], "m": [3, 20, 7, 7, 6, 9],
          "ln": [3, 7, 6, 9], "lm": [3, 20, 7, 6, 9]}   # *ae-sv-cnt-widths* в lsp


def cnt_wide(secs):
    # есть площадь: общая сетка с колонками «Высота» и «Ширина»
    return any("Высота" in cols for cols, _ in secs)


def cnt_key(secs):
    return ("" if cnt_wide(secs) else "l") + ("m" if cnt_marks(secs) else "n")


def cnt_aligns(marks, wide):
    return ["l"] + (["l"] if marks else []) + ["c"] + (["c"] if wide else []) + ["c", "r"]


def cnt_head_aligns(marks, wide):
    return ["c"] + (["c"] if marks else []) + ["c"] + (["c"] if wide else []) + ["c", "r"]


def cnt_heads(marks, area, wide):
    if area:
        return ["№", "Марка", "Высота", "Ширина", "Кол-во", "Всего"] if marks else ["№", "Высота", "Ширина", "Кол-во", "Всего"]
    if wide:
        return ["№", "Марка", "Длина", "", "Кол-во", "Всего"] if marks else ["№", "Длина", "", "Кол-во", "Всего"]
    return ["№", "Марка", "Длина", "Кол-во", "Всего"] if marks else ["№", "Длина", "Кол-во", "Всего"]


def cnt_cell(cols, row, name):
    return row[cols.index(name)] if name in cols else ""


def cnt_unit(cols):
    return "м.п." if "м.п." in cols[-1] else "м2"


def cnt_size_cells(cols, row, wide):
    if "Высота" in cols:
        return [cnt_cell(cols, row, "Высота"), cnt_cell(cols, row, "Ширина")]
    if wide:
        return [cnt_cell(cols, row, "Длина, мм"), ""]
    return [cnt_cell(cols, row, "Длина, мм")]


def cnt_item(cols, row, marks, wide):
    cells = [cnt_cell(cols, row, "№")]
    if marks:
        cells.append(cnt_cell(cols, row, "Марка"))
    cells += cnt_size_cells(cols, row, wide) + [row[-2] + " шт.", row[-1] + " " + cnt_unit(cols)]
    return cells


def cnt_total(cols, row, marks, wide):
    return ([""] + ([""] if marks else []) + ["Всего:"] + ([""] if wide else [])
            + [row[-2] + " шт.", row[-1] + " " + cnt_unit(cols)])


def cnt_pad(text, w, al):
    n = len(text)
    pad = (w - n) // 2 if al == "c" else (w - n if al == "r" else 0)
    return " " * max(0, pad) + text


def cnt_units(chars):
    return (6 * chars + 2) // 5   # 1 символ примерно 1,2 единицы (как в табуляциях DCL)


def cnt_bounds(widths):
    acc, out = 0, []
    for w in widths:
        out.append(cnt_units(1 + acc))
        acc += w + 2
    out.append(cnt_units(1 + acc))
    return out


def cnt_row(cells, aligns, widths):
    parts = ["| " + cnt_pad(c, w, al) for c, al, w in zip(cells, aligns, widths)]
    return " " + "\t".join(parts + ["|"]) + " "


def cnt_dashes(gap):
    return 2 * gap - 3   # дефис 0,5 единицы, «+» 1 единица, запас 3 дефиса


def cnt_line(widths):
    bd = cnt_bounds(widths)
    return " " + "\t".join(["+" + "-" * cnt_dashes(bd[k + 1] - bd[k]) for k in range(len(widths))] + ["+"]) + " "


def cnt_marks(secs):
    return any("Марка" in cols for cols, _ in secs)


def cnt_wrap(text, maxw):
    lines, line = [], ""
    for w in text.split():
        if not line:
            line = w
        elif len(line) + 1 + len(w) <= maxw:
            line += " " + w
        else:
            lines.append(line)
            line = w
    if line:
        lines.append(line)
    return lines


def cnt_title_rows(names, total):
    out = []
    for n in names:
        for ln in cnt_wrap(n, total - 2):
            pad = (total - len(ln)) // 2
            out.append(" " + " " * max(0, pad) + ln)
    return out


def cnt_display(secs, names=()):
    marks = cnt_marks(secs)
    wide = cnt_wide(secs)
    widths = WIDTHS[cnt_key(secs)]
    total = sum(w + 2 for w in widths)
    out = []
    for idx, (cols, rows) in enumerate(secs):
        if idx:
            out.append("")
        out += cnt_title_rows(names, total)
        out.append(cnt_line(widths))
        body = rows[1:]
        items, tot = body[:-1], body[-1]
        out.append(cnt_row(cnt_heads(marks, "Высота" in cols, wide), cnt_head_aligns(marks, wide), widths))
        out.append(cnt_line(widths))
        for row in items:
            out.append(cnt_row(cnt_item(cols, row, marks, wide), cnt_aligns(marks, wide), widths))
        out.append(cnt_line(widths))
        out.append(cnt_row(cnt_total(cols, tot, marks, wide), cnt_aligns(marks, wide), widths))
        out.append(cnt_line(widths))
    return out


def scenario_table():
    print("\n-- Таблица «Подсчёт»: рамка, колонки, итог, размеры окна --")
    area_cols = ["№", "Марка", "Высота", "Ширина", "Кол-во", "Сумма, м2"]
    lin_cols = ["№", "Длина, мм", "Кол-во", "Сумма, м.п."]
    lin_m_cols = ["№", "Марка", "Длина, мм", "Кол-во", "Сумма, м.п."]
    area = (area_cols, [area_cols,
                        ["1", "М1", "2100", "900", "2", "3.78"],
                        ["2", "М122", "2100", "900", "10", "123.78"],
                        ["Всего:", "", "", "", "12", "127.56"]])
    lin = (lin_cols, [lin_cols,
                      ["1", "1500", "2", "3.00"],
                      ["Всего:", "2", "3.00"]])
    lin_m = (lin_m_cols, [lin_m_cols,
                          ["1", "М2", "1500", "2", "3.00"],
                          ["Всего:", "", "2", "3.00"]])

    def cells(line):
        return [c.strip() for c in line.split("\t")]

    def inner(line):
        return [c.lstrip("| ").strip() for c in cells(line)[:-1]]

    lines = cnt_display([area])
    row_m1 = [ln for ln in lines if "М1" in ln][0]
    check("позиция М1: «1 | М1 | 2100 | 900 | 2 шт. | 3.78 м2» (высота и ширина — отдельно)",
          inner(row_m1) == ["1", "М1", "2100", "900", "2 шт.", "3.78 м2"])
    total = [ln for ln in lines if "Всего:" in ln][0]
    check("итог: «Всего:», 12 шт., 127.56 м2 (сумма групп)",
          "12 шт." in total and "127.56 м2" in total)
    dash_lines = [ln for ln in lines if ln.strip() and set(ln.replace("\t", "").strip()) <= set("+-")]
    check("рамка: линия над шапкой, после шапки, после позиций, после итога (4 линии)",
          len(dash_lines) == 4 and lines[0] == dash_lines[0] and lines[-1] == dash_lines[0])
    check("последняя строка раздела — «Всего:», перед ней линия",
          "Всего:" in lines[-2] and lines[-3] == dash_lines[0])

    lin_lines = cnt_display([lin])
    row_1 = [ln for ln in lin_lines if "1500" in ln][0]
    check("погонаж без площади: «1 | 1500 | 2 шт. | 3.00 м.п.» — без пустой колонки",
          inner(row_1) == ["1", "1500", "2 шт.", "3.00 м.п."])
    check("погонаж без площади: «Всего:» есть, колонки марки нет, вариант ln — 4 колонки",
          any("Всего:" in ln for ln in lin_lines) and len(cells(row_1)) - 1 == len(WIDTHS["ln"]) == 4)
    lin_m_lines = cnt_display([lin_m])
    row_m2 = [ln for ln in lin_m_lines if "М2" in ln][0]
    check("погонаж с марками: «1 | М2 | 1500 | 2 шт. | 3.00 м.п.», вариант lm — 5 колонок",
          inner(row_m2) == ["1", "М2", "1500", "2 шт.", "3.00 м.п."] and len(WIDTHS["lm"]) == 5)
    check("ключи вариантов: площадь — n / m, погонаж — ln / lm",
          cnt_key([area]) == "m" and cnt_key([(["№", "Высота", "Ширина", "Кол-во", "Сумма, м2"], [])]) == "n"
          and cnt_key([lin]) == "ln" and cnt_key([lin_m]) == "lm")
    check("без марок — вариант n: 5 колонок; с марками — m: 6 колонок",
          len(WIDTHS["n"]) == 5 and len(WIDTHS["m"]) == 6)

    for name, secs in (("ln", [lin]), ("lm", [lin_m]), ("m", [area]), ("m+n", [area, lin])):
        key = cnt_key(secs)
        widths = WIDTHS[key]
        out = cnt_display(secs)
        rows = [ln for ln in out if "\t" in ln]
        check("%s: каждая строка — %d ячеек и закрывающий «|» или «+» справа" % (name, len(widths)),
              all(len(ln.split("\t")) == len(widths) + 1 and ln.rstrip()[-1] in "|+" for ln in rows))
        bd = cnt_bounds(widths)
        check("%s: дефисы не доходят до следующей табуляции (1 + N/2 < ширины ячейки)" % name,
              all(1 + cnt_dashes(bd[k + 1] - bd[k]) / 2 < bd[k + 1] - bd[k] for k in range(len(widths))))
        fits = True
        for ln in [r for r in rows if "|" in r]:   # линии с дефисами шире ячеек по замыслу
            for part, w in zip(ln.split("\t"), widths):
                if len(part.strip()) - 2 > w:
                    fits = False
        check("%s: все ячейки помещаются в ширину колонок" % name, fits)
    check("раздел отделён одной пустой строкой (погонаж + площадь)",
          cnt_display([area, lin]).count("") == 1)
    head = [ln for ln in lines if "Высота" in ln][0]
    check("шапка с «Всего» — подпись прижата вправо; колонки «Высота» и «Ширина» раздельно",
          cells(head)[-2].endswith("Всего")
          and "Высота" in [c.lstrip("| ").strip() for c in cells(head)]
          and "Ширина" in [c.lstrip("| ").strip() for c in cells(head)])
    widths_m = max(cnt_units(sum(w + 2 for w in WIDTHS["m"]) + 2) + 6, 48 + 2)
    widths_n = max(cnt_units(sum(w + 2 for w in WIDTHS["n"]) + 2) + 6, 48 + 2)
    widths_lm = max(cnt_units(sum(w + 2 for w in WIDTHS["lm"]) + 2) + 6, 48 + 2)
    widths_ln = max(cnt_units(sum(w + 2 for w in WIDTHS["ln"]) + 2) + 6, 48 + 2)
    check("ширина окна: площадь без марок %d, с марками %d; погонаж без марок %d, с марками %d единиц"
          % (widths_n, widths_m, widths_ln, widths_lm),
          widths_n == 59 and widths_m == 85 and widths_ln == 50 and widths_lm == 74)
    check("колонка «Марка» 20 символов: «В-1 А-1» и марки до 20 символов помещаются",
          all(len(m) <= 20 for m in ["В-1 А-1", "М122", "МАРКА-ДЛИННАЯ-20сим"]))
    tot_m = sum(w + 2 for w in WIDTHS["m"])
    one = cnt_display([area], ("ВН-2 Б6",))
    head_line = one[0]
    check("заголовок: название блока — первая строка таблицы, по центру (пробелы слева = (ширина − длина) / 2 + 1)",
          head_line.strip() == "ВН-2 Б6" and len(head_line) - len(head_line.lstrip(" ")) == (tot_m - len("ВН-2 Б6")) // 2 + 1)


# ============================================================
# «Выбрать подобное» и подсказка «Блок (по Enter):» (порт из searchattvis.lsp)
# ============================================================

def sv_label_base(s):
    p = s.find(" | ")
    return s[:p] if p >= 0 else s


def sv_adapt_sel_i(sel_i, entries):
    if not sel_i:
        return []
    labels = [e[1] for e in entries]
    out = []
    for s in sel_i:
        if s in labels:
            if s not in out:
                out.append(s)
        else:
            base = sv_label_base(s)
            for e in entries:
                if sv_label_base(e[1]) == base and e[1] not in out:
                    out.append(e[1])
    return out


def sv_match(name, pat):
    if not pat:
        return True
    s = (name or "").upper()
    return any(fnmatch.fnmatchcase(s, p.strip().upper()) for p in pat.split(",") if p.strip())


def sv_entries_from(recs, mode, qn="", qa="", qb=""):
    acc = []
    for rec in recs:
        en, name, _vis0, attrs, vis_list, props, _lay = rec
        if not sv_match(name, qn):
            continue
        if mode == "all":
            if qb == "":
                acc.append((name, name, en))
            else:
                v = prop_exact(props, "Длина")
                hit = bool(v and sv_match(str(v), qb))
                for k, av in attrs:
                    if not hit and k.strip().upper() == "ДЛИНА" and sv_match(av, qb):
                        hit = True
                if hit:
                    acc.append((name, name, en))
        elif mode == "attr":
            for k, av in attrs:
                if sv_match(k, qa) and sv_match(av, qb):
                    lbl = k if qb == "" else ("%s | %s" % (k, "(пусто)" if av == "" else av))
                    acc.append((name, lbl, en))
        else:
            states = [v for v in vis_list if sv_match(v, qa)]
            dvals = [] if qb == "" else [(pk, pv) for pk, pv in props if sv_match(pv, qb)]
            if states and (qb == "" or dvals):
                if qb == "":
                    for s in states:
                        acc.append((name, s, en))
                else:
                    for s in states:
                        for pk, pv in dvals:
                            acc.append((name, "%s | %s (%s)" % (s, pv, pk), en))
    return acc


def sv_similar_targets(mode, qn, qa, qb, sel_b, sel_i, rows1, rows2, scope_pre):
    b = list(sel_b)
    i = list(sel_i)
    if not b and not i:
        if mode == "all":
            if qn != "" or qb != "" or scope_pre:
                b = [r[0] for r in rows1]
        elif qa != "" or qb != "":
            i = [r[0] for r in rows2]
            if qn != "":
                b = [r[0] for r in rows1]
        elif qn != "":
            b = [r[0] for r in rows1]
        elif scope_pre:
            i = [r[0] for r in rows2]
            if not i:
                b = [r[0] for r in rows1]
    elif b and not i and mode != "all" and (qa != "" or qb != ""):
        i = [r[0] for r in rows2]
    return b, i


def sv_similar_ens(all_recs, mode, qn, qa, qb, sel_b, sel_i):
    if sel_b and not sel_i and qa == "" and qb == "":
        out = []
        for r in all_recs:
            if sv_match(r[1], qn) and r[1] in sel_b and r[0] not in out:
                out.append(r[0])
        return out
    all_entries = sv_entries_from(all_recs, mode, qn, qa, qb)
    out = []
    for blk, lbl, en in all_entries:
        if (not sel_b or blk in sel_b) and (not sel_i or lbl in sel_i) and en not in out:
            out.append(en)
    return out


def scenario_similar():
    print("\n--- Сценарий 11: «Выбрать подобное» по образцу (Блок / Видимость / Атрибут, без значения и со значением) ---")
    # Чертёж: 5 вставок в модели, образец — en1
    r1 = ("en1", "Профиль", "Ригель", [("МАРКА", "М1"), ("ЭТАЖ", "1")], ["Ригель"], [("Длина", "1500")], "0")
    r2 = ("en2", "Профиль", "Ригель", [("МАРКА", "М1"), ("ЭТАЖ", "2")], ["Ригель"], [("Длина", "1500")], "0")
    r3 = ("en3", "Профиль", "Ригель", [("МАРКА", "М2"), ("ЭТАЖ", "1")], ["Ригель"], [("Длина", "2000")], "0")
    r4 = ("en4", "Профиль", "Стойка", [("МАРКА", "М1"), ("ЭТАЖ", "1")], ["Стойка"], [("Длина", "1500")], "0")
    r5 = ("en5", "Кронштейн", "Кр-1", [("МАРКА", "М1")], ["Кр-1"], [("Длина", "1500")], "0")
    all_recs = [r1, r2, r3, r4, r5]

    # 1. Выбран Блок без значения -> все вставки блока «Профиль» в чертеже
    ens = sv_similar_ens(all_recs, "all", "", "", "", ["Профиль"], [])
    check("подобное по Блоку без значения: все 4 вставки блока «Профиль»",
          ens == ["en1", "en2", "en3", "en4"])

    # 2. Выбран Блок со значением (Длина = 1500) -> вставки «Профиль» с длиной 1500
    ens = sv_similar_ens(all_recs, "all", "", "", "*1500*", ["Профиль"], [])
    check("подобное по Блоку со значением 1500: вставки «Профиль» длиной 1500",
          ens == ["en1", "en2", "en4"])

    # 3. Выбрана Видимость «Ригель» без значения -> все вставки с видимостью «Ригель»
    ens = sv_similar_ens(all_recs, "vis", "", "", "", [], ["Ригель"])
    check("подобное по Видимости без значения: все 3 вставки с видимостью «Ригель»",
          ens == ["en1", "en2", "en3"])

    # 4. Выбрана Видимость «Ригель» со значением 1500 -> вставки с видимостью «Ригель» и длиной 1500
    entries_vis_val = sv_entries_from([r1], "vis", "", "", "*1500*")
    adapted_vis = sv_adapt_sel_i(["Ригель"], entries_vis_val)
    check("перенос выбора видимости при вводе значения: «Ригель» -> «Ригель | 1500 (Длина)»",
          adapted_vis == ["Ригель | 1500 (Длина)"])
    ens = sv_similar_ens(all_recs, "vis", "", "", "*1500*", [], adapted_vis)
    check("подобное по Видимости со значением 1500: только «Ригель» длиной 1500 (en1, en2)",
          ens == ["en1", "en2"])

    # 5. Выбран Атрибут «МАРКА» без значения -> все вставки с атрибутом «МАРКА» (а при выборе блока — только этого блока)
    ens_all_attr = sv_similar_ens(all_recs, "attr", "", "", "", [], ["МАРКА"])
    ens_blk_attr = sv_similar_ens(all_recs, "attr", "", "", "", ["Профиль"], ["МАРКА"])
    check("подобное по Атрибуту без значения: 5 вставок по чертежу, 4 вставки при выбранном блоке «Профиль»",
          ens_all_attr == ["en1", "en2", "en3", "en4", "en5"] and ens_blk_attr == ["en1", "en2", "en3", "en4"])

    # 6. Выбран Атрибут «МАРКА» со значением «М1» -> вставки с МАРКА = М1
    entries_attr_val = sv_entries_from([r1], "attr", "", "", "*М1*")
    adapted_attr = sv_adapt_sel_i(["МАРКА"], entries_attr_val)
    check("перенос выбора атрибута при вводе значения: «МАРКА» -> «МАРКА | М1»",
          adapted_attr == ["МАРКА | М1"])
    ens = sv_similar_ens(all_recs, "attr", "", "", "*М1*", ["Профиль"], adapted_attr)
    check("подобное по Атрибуту со значением «М1» и блоку «Профиль»: en1, en2, en4",
          ens == ["en1", "en2", "en4"])

    # 7. Возврат выбора при очистке поля «Значение»
    entries_attr_plain = sv_entries_from([r1], "attr", "", "", "")
    check("возврат выбора атрибута при очистке значения: «МАРКА | М1» -> «МАРКА»",
          sv_adapt_sel_i(adapted_attr, entries_attr_plain) == ["МАРКА"])

    # 8. Автоподстановка цели по предвыбранному образцу (scope_pre=True), когда строки в списках не кликнуты
    b, i = sv_similar_targets("vis", "", "", "", [], [], [("Профиль", ["en1"])], [("Ригель", ["en1"])], True)
    check("образец в режиме «Видимости» без клика в списке берёт видимость образца",
          b == [] and i == ["Ригель"])


def main():
    print("=== Тест размеров «Подсчёта» (поиск) ===")
    scenario_inch_glass()
    scenario_priority()
    scenario_fallback_length()
    scenario_numbered_names()
    scenario_no_height()
    scenario_no_mask_no_allowance()
    scenario_mask_case()
    scenario_rounding()
    scenario_door_names()
    scenario_frame_masks()
    scenario_layout()
    scenario_sort()
    scenario_table()
    scenario_similar()
    print("\nИтог: PASS %d, FAIL %d" % (PASS, FAIL))
    if FAIL == 0:
        print("[SEARCH][OK]")
        return 0
    print("[SEARCH][ERRORS]")
    return 1


if __name__ == "__main__":
    sys.exit(main())
