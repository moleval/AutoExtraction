#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Порт нумерации DETAIL-таблицы Подсистемы.

Модель повторяет `su-build-flat-items` и разметку колонки «№» из
`subsystem-create-table-detail` (`Extraction/subsystem.lsp`):

- штучные позиции (без длины) — простые номера 1, 2, 3...;
- мерные группы — номер «группа.строка»: 14.1, 14.2, 15.1...

Нумерация сквозная по строкам: штучные занимают номера 1..N, первая
мерная группа получает N+1. На рабочем чертеже с 13 штучными позициями
группы обязаны идти 14, 15, 16 — прежняя схема давала скачок «13 -> 2.1».
Если штучных нет, первая мерная группа — 1.
"""

from __future__ import annotations

import sys

PASS = 0
FAIL = 0


# Вывод в PowerShell идёт через пайп, поэтому Python кодирует текст в
# ANSI-кодировку системы (cp1251). Любой символ вне неё ронял прогон
# UnicodeEncodeError прямо посреди проверок. Основное лечение —
# PYTHONIOENCODING=utf-8 в scripts/test.ps1; это страховка для прямого запуска.
try:
    sys.stdout.reconfigure(errors="replace")
except (AttributeError, ValueError):
    pass


def check(name, ok):
    global PASS, FAIL
    if ok:
        PASS += 1
        print("[SUBSYSTEM][PASS] %s" % name)
    else:
        FAIL += 1
        print("[SUBSYSTEM][FAIL] %s" % name)


# ---------------------------------------------------------------
# Порт: плоский список строк (su-build-flat-items)
# ---------------------------------------------------------------

def has_pieces(data):
    return any(rec[1] is None for rec in data)


def build_flat_items(data):
    """data: [(name, length|None, count)]

    -> [('data', idx, name, len, cnt) | ('subtotal', idx, name, sum)]
    """
    items = []

    # Штучные позиции — простые номера 1..N (внутри группы 1).
    pieces = has_pieces(data)
    pieces_count = 0
    for rec in data:
        if rec[1] is None:
            pieces_count += 1
            items.append(("data", 1, rec[0], rec[1], rec[2]))

    # Мерные группы продолжают счёт: 13 штучных -> первая группа 14.
    group_index = pieces_count

    # Мерные группы — по одной на имя, с подитогом.
    names = []
    for rec in data:
        if rec[1] is not None and rec[0] not in names:
            names.append(rec[0])
    names.sort()

    for name in names:
        group_index += 1
        rows = [r for r in data if r[0] == name and r[1] is not None]
        total = 0.0
        for r in rows:
            total += (r[1] * r[2]) / 1000.0
            items.append(("data", group_index, r[0], r[1], r[2]))
        items.append(("subtotal", group_index, name, total))

    return items


def number_column(items, pieces):
    """Разметка колонки «№» из subsystem-create-table-detail.

    Возвращает список текстов номеров строк данных.
    """
    out = []
    last_group = None
    row_in_group = 0
    for item in items:
        if item[0] != "data":
            continue
        group_idx = item[1]
        if group_idx != last_group:
            last_group = group_idx
            row_in_group = 0
        row_in_group += 1
        if pieces and group_idx == 1:
            # Группа штучных: простой номер.
            out.append(str(row_in_group))
        else:
            out.append("%d.%d" % (group_idx, row_in_group))
    return out


def subtotal_numbers(items):
    return [item[1] for item in items if item[0] == "subtotal"]


# ---------------------------------------------------------------
# Сценарии
# ---------------------------------------------------------------

def scenario_only_measured():
    """Типичный случай Подсистемы: у всех блоков есть длина, штучных нет."""
    print("\n--- Только мерные позиции (штучных нет) ---")
    data = [
        ("Профиль П-1", 2500, 3),
        ("Профиль П-1", 3000, 1),
        ("Уголок У-2", 1200, 4),
    ]
    items = build_flat_items(data)
    texts = number_column(items, has_pieces(data))

    check("первая строка таблицы нумеруется 1.1, а не 2.1", texts[0] == "1.1")
    check("нумерация начинается с единицы: %s" % ", ".join(texts),
          texts == ["1.1", "1.2", "2.1"])
    check("номера групп начинаются с 1: %s" % subtotal_numbers(items),
          subtotal_numbers(items) == [1, 2])


def scenario_with_pieces():
    """Есть штучные позиции: они занимают группу 1, мерные идут с 2."""
    print("\n--- Мерные + штучные ---")
    data = [
        ("Кронштейн К-1", None, 10),
        ("Профиль П-1", 2500, 3),
        ("Кронштейн К-2", None, 2),
    ]
    items = build_flat_items(data)
    texts = number_column(items, has_pieces(data))

    check("штучные — простые номера 1, 2", texts[:2] == ["1", "2"])
    check("после двух штучных первая мерная группа — 3.1", texts[2] == "3.1")
    check("подитог первой мерной группы — 3", subtotal_numbers(items) == [3])


def scenario_three_pieces():
    """Штучные — ОДНА группа, сколько бы строк в ней ни было.

    Их номера 1, 2, 3 не сдвигают счёт групп: первая мерная группа всё
    равно 2, а не 4. Номер группы — идентификатор группы (он же стоит в
    строке подитога), а не продолжение сквозной нумерации строк.
    """
    print("\n--- Три штучные + мерные: группа считается по группам ---")
    data = [
        ("Кронштейн К-1", None, 10),
        ("Кронштейн К-2", None, 2),
        ("Кронштейн К-3", None, 5),
        ("Профиль П-1", 2500, 3),
        ("Профиль П-1", 3000, 1),
        ("Уголок У-2", 1200, 4),
    ]
    items = build_flat_items(data)
    texts = number_column(items, has_pieces(data))

    check("штучные: 1, 2, 3", texts[:3] == ["1", "2", "3"])
    check("после трёх штучных первая мерная группа — 4.1: %s" % texts[3],
          texts[3] == "4.1")
    check("полная колонка №: %s" % " | ".join(texts),
          texts == ["1", "2", "3", "4.1", "4.2", "5.1"])
    check("подитоги групп: %s" % subtotal_numbers(items),
          subtotal_numbers(items) == [4, 5])


def scenario_screenshot_13_pieces():
    """Рабочий чертёж пользователя: 13 штучных + три мерные группы.

    Было: 1..13, затем 2.1 (скачок назад, читается как ошибка).
    Должно быть: 1..13, затем 14.1..14.16, 15.1..15.23, 16.1..16.7.
    """
    print("\n--- 13 штучных + три группы (рабочий чертёж) ---")
    data = [("Штучная %d" % i, None, 1) for i in range(1, 14)]
    data += [("Несущая", 1430 + i, 1) for i in range(16)]
    data += [("Т-обр.", 275 + i, 1) for i in range(23)]
    data += [("Шляпный", 210 + i, 1) for i in range(7)]

    items = build_flat_items(data)
    texts = number_column(items, has_pieces(data))

    check("штучные: 13 строк с номерами 1..13",
          texts[:13] == [str(i) for i in range(1, 14)])
    check("первая мерная группа — 14.1, а не 2.1", texts[13] == "14.1")
    check("группа 1: 14.1..14.16",
          texts[13:29] == ["14.%d" % i for i in range(1, 17)])
    check("группа 2: 15.1..15.23",
          texts[29:52] == ["15.%d" % i for i in range(1, 24)])
    check("группа 3: 16.1..16.7",
          texts[52:59] == ["16.%d" % i for i in range(1, 8)])
    check("подитоги: 14, 15, 16", subtotal_numbers(items) == [14, 15, 16])


def scenario_only_pieces():
    """Только штучные: мерных групп нет, подитогов нет."""
    print("\n--- Только штучные позиции ---")
    data = [("Кронштейн К-1", None, 10), ("Кронштейн К-2", None, 2)]
    items = build_flat_items(data)
    texts = number_column(items, has_pieces(data))

    check("штучные нумеруются 1, 2", texts == ["1", "2"])
    check("подитогов нет", subtotal_numbers(items) == [])


def scenario_three_groups_no_pieces():
    """Три мерные группы без штучных: 1.1 ... 3.1, подитоги 1, 2, 3."""
    print("\n--- Три группы без штучных ---")
    data = [
        ("А", 1000, 1),
        ("Б", 2000, 2),
        ("В", 3000, 3),
    ]
    items = build_flat_items(data)
    texts = number_column(items, has_pieces(data))
    check("номера строк: %s" % ", ".join(texts), texts == ["1.1", "2.1", "3.1"])
    check("подитоги: %s" % subtotal_numbers(items), subtotal_numbers(items) == [1, 2, 3])


def main():
    print("=== Тест нумерации таблицы Подсистемы ===")
    scenario_only_measured()
    scenario_with_pieces()
    scenario_three_pieces()
    scenario_screenshot_13_pieces()
    scenario_only_pieces()
    scenario_three_groups_no_pieces()

    print("\nPASS: %d, FAIL: %d" % (PASS, FAIL))
    if FAIL:
        return 1
    print("[SUBSYSTEM][OK]")
    return 0


if __name__ == "__main__":
    sys.exit(main())
