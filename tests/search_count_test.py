#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Спецификационный тест размеров в «Подсчёте» окна поиска (ПОИСКАТР / ПОИСКВИД).

Модель НЕ запускает AutoCAD: проверяется ПОРТ логики из
`Extraction/searchattvis.lsp` (ae-sv-size-of и помощники) и её совпадение
с цепочкой размеров ЗАПОЛНЕНИЯ (`Extraction/zapolnenie.lsp`).

Правила:
- площадь: сначала размер «в свету» («Высота в свету», «Ширина в свету»),
  затем «Высота» и «Ширина» без номера; если «Ширины» нет — «Длина»;
- совпадение точное (регистр не важен): «Высота1» не берётся;
- припуск на раму (настройка input.frame.allowance, по умолчанию 26)
  добавляется к высоте и ширине блоков с именем по маскам «Заполнени*»
  и «Стекло*»; у остальных блоков припуска нет;
- размеры округляются до миллиметров ДО припуска;
- без высоты блок в площадь не попадает; погонаж («Длина») припуска не получает.

Только stdlib. Запуск: python3 tests/search_count_test.py
"""

from __future__ import annotations

import fnmatch
import sys

PASS = 0
FAIL = 0

HEIGHT_KEYS = ["Высота в свету", "Высота"]
WIDTH_KEYS = ["Ширина в свету", "Ширина", "Длина"]
FRAME_MASKS = ["Заполнени*", "Стекло*"]
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
# Порт: точное совпадение имени свойства (регистр не важен)
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


def dim_of(props, keys):
    for k in keys:
        v = mm(num(prop_exact(props, k)))
        if v is not None:
            return v
    return None


def is_frame(name):
    """ae-sv-frame-p: имя подходит под одну из масок (регистр не важен)."""
    return any(fnmatch.fnmatch(name.lower(), m.lower()) for m in FRAME_MASKS)


def size_of(name, props, allowance=DEFAULT_ALLOWANCE):
    """ae-sv-size-of: ('A', h, w) — площадь; ('L', l, None) — погонаж; None."""
    h = dim_of(props, HEIGHT_KEYS)
    w = dim_of(props, WIDTH_KEYS)
    l = mm(num(prop_exact(props, "Длина")))
    if h is not None and is_frame(name):
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
    lin = section_rows("Погонаж, м.п.", [("", 1500, 0, 6), ("", 900, 0, 8)])
    check("«Всего:» — последняя строка раздела",
          lin[-1][0] == "Всего:" and lin[0][0] == "№")
    check("позиции нумеруются с 1, «Всего» без номера",
          [r[0] for r in lin[1:-1]] == ["1", "2"] and lin[-1][0] == "Всего:")
    area = section_rows("Площадь, м2", [("", 1200, 600, 5)])
    check("площадь: «Всего» последней, сумма 3.60",
          area[-1][-1] == "3.60" and area[-1][-2] == "5")
    check("строка состояния: итоги погонажа",
          totals_text([("Погонаж, м.п.", lin)]) == "Всего: 14 шт. 16.20 м.п.")
    check("строка состояния: погонаж и площадь через «; »",
          totals_text([("Погонаж, м.п.", lin), ("Площадь, м2", area)])
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
    scenario_layout()
    scenario_sort()
    print("\nИтог: PASS %d, FAIL %d" % (PASS, FAIL))
    if FAIL == 0:
        print("[SEARCH][OK]")
        return 0
    print("[SEARCH][ERRORS]")
    return 1


if __name__ == "__main__":
    sys.exit(main())
