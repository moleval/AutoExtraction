#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Спецификационный тест ветки «блоки заполнения» в раскрое листа.

Модель НЕ запускает AutoCAD: проверяется ПОРТ логики из
`Extraction/cutsheet.lsp` (cs-fill-*) и правил, общих с ЗАПОЛНЕНИЕМ.

Суть правила: у блока заполнения параметры задают размер ПРОЁМА
(«в свету»), а резать надо заготовку — проём плюс припуск на раму.
Габарит блока (BoundingBox) для таких блоков не годится вовсе: он
включает раму и даёт размер в разы больше, поэтому при нехватке
параметра деталь отклоняется с причиной, а не достраивается габаритом.

Только stdlib. Запуск: python3 tests/fill_allowance_test.py
"""

from __future__ import annotations

import sys

PASS = 0
FAIL = 0

HEIGHT_KEYS = ["ВЫСОТА В СВЕТУ", "ВЫСОТА"]
WIDTH_KEYS = ["ШИРИНА В СВЕТУ", "ШИРИНА", "ДЛИНА"]
DEFAULT_ALLOWANCE = 26


def check(name, ok):
    global PASS, FAIL
    if ok:
        PASS += 1
        print("[FILL][PASS] %s" % name)
    else:
        FAIL += 1
        print("[FILL][FAIL] %s" % name)


# ---------------------------------------------------------------
# Порт: поиск параметра (cs-prop-value-like)
# ---------------------------------------------------------------

def prop_value_like(props, wanted):
    """Точное совпадение имени, затем вхождение подстроки."""
    for name, value in props:
        if name.upper() == wanted.upper():
            return value
    for name, value in props:
        if wanted.upper() in name.upper():
            return value
    return None


def fill_dimension(props, keys):
    """cs-fill-dimension: первый непустой положительный по приоритету."""
    for k in keys:
        v = prop_value_like(props, k)
        if isinstance(v, (int, float)) and v > 0:
            return v
    return None


# ---------------------------------------------------------------
# Порт: запись детали заполнения (ветка в cs-block-record)
# ---------------------------------------------------------------

def fill_record(props, mark=None, allowance=DEFAULT_ALLOWANCE, type_name="Блок"):
    """Возвращает (w, h, source, typeName) либо ('reject', причина)."""
    raw_h = fill_dimension(props, HEIGHT_KEYS)
    raw_w = fill_dimension(props, WIDTH_KEYS)
    if raw_h is None:
        return ("reject", "заполнение: не найдена высота (в свету)")
    if raw_w is None:
        return ("reject", "заполнение: не найдена ширина (в свету)")
    w = float(int(raw_w + allowance))
    h = float(int(raw_h + allowance))
    name = mark if (isinstance(mark, str) and mark != "") else type_name
    return (w, h, "Заполнение (в свету + припуск)", name)


def is_fill_block(name, masks):
    """cs-fill-block-p: пустой список масок = признак выключен."""
    if not masks:
        return False
    import re
    for m in masks:
        rx = "".join(".*" if c == "*" else ("." if c == "?" else re.escape(c))
                     for c in m)
        if re.fullmatch(rx, name, re.UNICODE | re.IGNORECASE):
            return True
    return False


# ---------------------------------------------------------------
# Сценарии
# ---------------------------------------------------------------

def scenario_props():
    """Поиск параметра: точное совпадение приоритетнее подстроки."""
    props = [("Высота в свету, мм", 1200.0), ("ВЫСОТА", 1300.0)]
    check("параметр: точное совпадение имеет приоритет",
          prop_value_like(props, "ВЫСОТА") == 1300.0)
    check("параметр: подстрока срабатывает, когда точного нет",
          prop_value_like([("Высота в свету, мм", 1200.0)], "ВЫСОТА В СВЕТУ") == 1200.0)
    check("параметр: не найден = None",
          prop_value_like(props, "ТОЛЩИНА") is None)


def scenario_priority():
    """Цепочка приоритетов повторяет ЗАПОЛНЕНИЕ."""
    check("высота: «в свету» приоритетнее обычной",
          fill_dimension([("ВЫСОТА В СВЕТУ", 1200.0), ("ВЫСОТА", 1300.0)],
                         HEIGHT_KEYS) == 1200.0)
    check("ширина: «в свету» -> «ШИРИНА» -> «ДЛИНА»",
          fill_dimension([("ДЛИНА", 500.0), ("ШИРИНА", 600.0)], WIDTH_KEYS) == 600.0)
    check("ширина: «ДЛИНА» как последний запасной вариант",
          fill_dimension([("ДЛИНА", 500.0)], WIDTH_KEYS) == 500.0)
    check("размер <= 0 не принимается",
          fill_dimension([("ШИРИНА", 0.0), ("ДЛИНА", 500.0)], WIDTH_KEYS) == 500.0)


def scenario_allowance():
    """Припуск прибавляется к каждой стороне, размер — целые мм."""
    props = [("ВЫСОТА В СВЕТУ", 1200.0), ("ШИРИНА В СВЕТУ", 600.0)]
    w, h, src, name = fill_record(props)
    check("припуск: 600 в свету -> 626 заготовка", w == 626.0)
    check("припуск: 1200 в свету -> 1226 заготовка", h == 1226.0)
    check("источник размера помечен", src == "Заполнение (в свету + припуск)")

    w2, h2, _, _ = fill_record(props, allowance=0)
    check("припуск 0: размер равен проёму", (w2, h2) == (600.0, 1200.0))

    w3, h3, _, _ = fill_record([("ВЫСОТА В СВЕТУ", 1200.4),
                                ("ШИРИНА В СВЕТУ", 600.7)], allowance=26)
    check("дробный проём отсекается до целых мм", (w3, h3) == (626.0, 1226.0))


def scenario_reject():
    """Нет размера — отказ с причиной, а не габарит блока."""
    r = fill_record([("ШИРИНА В СВЕТУ", 600.0)])
    check("нет высоты: отказ", r[0] == "reject")
    check("нет высоты: причина названа", "высота" in r[1])
    r = fill_record([("ВЫСОТА В СВЕТУ", 1200.0)])
    check("нет ширины: отказ", r[0] == "reject")
    check("нет ширины: причина названа", "ширина" in r[1])
    check("отказ не подменяется габаритом блока", r[0] == "reject")


def scenario_name():
    """Имя элемента — марка, если она есть."""
    props = [("ВЫСОТА В СВЕТУ", 1200.0), ("ШИРИНА В СВЕТУ", 600.0)]
    check("имя: марка вытесняет имя блока",
          fill_record(props, mark="М-12")[3] == "М-12")
    check("имя: без марки остаётся имя блока",
          fill_record(props, mark=None, type_name="Заполнение КП50")[3]
          == "Заполнение КП50")
    check("имя: пустая марка не вытесняет имя блока",
          fill_record(props, mark="", type_name="Заполнение КП50")[3]
          == "Заполнение КП50")


def scenario_masks():
    """Признак блока заполнения — маски задачи ЗАПОЛНЕНИЕ."""
    check("маска: точное имя", is_fill_block("Заполнение КП50", ["Заполнение*"]))
    check("маска: не подходит", not is_fill_block("Ригель", ["Заполнение*"]))
    check("маска: регистр не важен", is_fill_block("заполнение кп50", ["Заполнение*"]))
    check("маска «*» ловит всё — отсюда предупреждение в логе",
          is_fill_block("Ригель", ["*"]))
    check("пустой список масок выключает признак",
          not is_fill_block("Заполнение КП50", []))
    check("список масок отсутствует — признак выключен",
          not is_fill_block("Заполнение КП50", None))


def main():
    print("=== Тест ветки «блоки заполнения» в раскрое листа ===")
    scenario_props()
    scenario_priority()
    scenario_allowance()
    scenario_reject()
    scenario_name()
    scenario_masks()
    print("\nИтог: PASS %d, FAIL %d" % (PASS, FAIL))
    if FAIL == 0:
        print("[FILL][OK]")
        return 0
    print("[FILL][ERRORS]")
    return 1


if __name__ == "__main__":
    sys.exit(main())
