#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Спецификационный тест подписи марок в картах раскроя.

Модель НЕ запускает AutoCAD: проверяется ПОРТ логики, реализованной в
`common/task-utils.lsp` (чтение атрибута), `Extraction/cutsheet.lsp`
(марка в записи детали) и `Extraction/cutline.lsp` (очередь «длина ->
марки»).

Почему очередь вообще нужна. В раскрое ЛИСТА запись детали доходит до
отрисовки целиком, поэтому марка просто лежит в записи. В раскрое ХЛЫСТА
решатель FFD работает с голыми длинами: детали группируются по длине ещё
до решателя, и связь с исходным объектом теряется. Трогать решатель
нельзя, поэтому марки собираются в побочную очередь при измерении и
выдаются при отрисовке по длине детали.

Только stdlib. Запуск: python3 tests/mark_logic_test.py
"""

from __future__ import annotations

import sys

PASS = 0
FAIL = 0


def check(name, ok):
    global PASS, FAIL
    if ok:
        PASS += 1
        print("[MARK-LOGIC][PASS] %s" % name)
    else:
        FAIL += 1
        print("[MARK-LOGIC][FAIL] %s" % name)


# ---------------------------------------------------------------
# Порт: чтение атрибута (tu-block-attr)
# ---------------------------------------------------------------

def block_attr(attrs, tag):
    """tu-block-attr: тег без учёта регистра и пробелов, пустое = нет."""
    if attrs is None:
        return None
    for tagname, value in attrs:
        if not isinstance(tagname, str) or not isinstance(value, str):
            continue
        if tagname.strip().upper() == tag.strip().upper():
            v = value.strip()
            if v != "":
                return v
    return None


def entity_mark(ent_type, attrs, tag="МАРКА"):
    """tu-entity-mark: марка только у вставок блоков."""
    if ent_type != "INSERT":
        return None
    return block_attr(attrs, tag)


# ---------------------------------------------------------------
# Порт: марка в записи детали листа (cs-part-mark)
# ---------------------------------------------------------------

def part_mark(record):
    """cs-part-mark: 11-й элемент записи, с защитой от коротких записей."""
    if not isinstance(record, (list, tuple)) or len(record) <= 10:
        return None
    v = record[10]
    if isinstance(v, str) and v != "":
        return v
    return None


# ---------------------------------------------------------------
# Порт: очередь марок хлыста (n1-mark-*)
# ---------------------------------------------------------------

class MarkQueue:
    """n1-mark-add / n1-marks-begin / n1-mark-take."""

    def __init__(self):
        self.master: dict[int, list[str]] = {}
        self.queue: dict[int, list[str]] = {}

    @staticmethod
    def key(length):
        return int(length * 1000.0 + 0.5)

    def add(self, length, mark):
        if not isinstance(mark, str) or mark == "":
            return
        self.master.setdefault(self.key(length), []).append(mark)

    def begin(self):
        self.queue = {k: list(v) for k, v in self.master.items()}

    def take(self, length):
        lst = self.queue.get(self.key(length))
        if not lst:
            return None
        return lst.pop(0)


# ---------------------------------------------------------------
# Порт: помещается ли подпись (условия отрисовки)
# ---------------------------------------------------------------

def sheet_mark_fits(mark, w, h, text_h):
    """cs-draw-placement: высота <= 8.5% меньшей стороны, строка влезает."""
    mk_h = min(text_h * 0.55, 0.085 * min(w, h))
    # порог читаемости: мельче четверти основной высоты не рисуем
    return (mk_h > text_h * 0.25
            and w > len(mark) * mk_h * 0.8
            and h > mk_h * 3.0)


def bar_mark_fits(mark, piece_len, bar_height, txt_h):
    """n1-draw-layout: та же идея для детали на хлысте."""
    mk_h = txt_h * 0.75
    return piece_len > len(mark) * mk_h * 0.8 and bar_height > mk_h * 2.4


# ---------------------------------------------------------------
# Сценарии
# ---------------------------------------------------------------

def scenario_attr():
    """Чтение атрибута МАРКА."""
    attrs = [("ВИТРАЖ", "В1"), ("МАРКА", "К-12"), ("ДЛИНА", "2400")]
    check("атрибут: марка найдена", block_attr(attrs, "МАРКА") == "К-12")
    check("атрибут: регистр тега не важен", block_attr(attrs, "марка") == "К-12")
    check("атрибут: пробелы в теге не важны",
          block_attr([(" МАРКА ", "К-12")], "МАРКА") == "К-12")
    check("атрибут: значение обрезается",
          block_attr([("МАРКА", "  К-12  ")], "МАРКА") == "К-12")
    check("атрибут: пустое значение = нет марки",
          block_attr([("МАРКА", "   ")], "МАРКА") is None)
    check("атрибут: тега нет = нет марки", block_attr(attrs, "ПОЗИЦИЯ") is None)
    check("атрибут: атрибутов нет = нет марки", block_attr(None, "МАРКА") is None)
    check("марка только у вставки блока",
          entity_mark("LWPOLYLINE", [("МАРКА", "К-12")]) is None)
    check("марка у вставки блока читается",
          entity_mark("INSERT", [("МАРКА", "К-12")]) == "К-12")


def scenario_sheet():
    """Лист: марка лежит в записи детали."""
    dyn = [1, "DYN", "Витражи", "Заполнение", 605, 605, 0.366, "605x605", "Свойства",
           "<ent>", "К-12"]
    poly = [2, "POLY", "Детали", "Полилиния", 400, 300, 0.12, "400x300", True,
            "<ent>", None]
    short = [3, "DYN", "Витражи", "Тип", 400, 300, 0.12, "400x300", "Свойства", "<ent>"]
    check("лист: марка берётся из записи", part_mark(dyn) == "К-12")
    check("лист: у полилинии марки нет", part_mark(poly) is None)
    check("лист: короткая запись не ломает чтение", part_mark(short) is None)
    check("лист: арность записей одинакова", len(dyn) == len(poly))

    # условия размещения подписи
    check("лист: на крупной детали подпись помещается",
          sheet_mark_fits("К-12", 605, 605, 60.0))
    check("лист: на узкой детали подпись не рисуется",
          not sheet_mark_fits("К-12-длинная", 160, 80, 60.0))
    check("лист: на мелкой детали подпись не рисуется",
          not sheet_mark_fits("К-12", 20, 20, 60.0))


def scenario_bar():
    """Хлыст: очередь «длина -> марки»."""
    q = MarkQueue()
    # три детали по 2400 с разными марками и одна 1200
    for mk in ("К-1", "К-2", "К-3"):
        q.add(2400.0, mk)
    q.add(1200.0, "Р-7")
    q.add(1200.0, None)        # нет атрибута — в очередь не попадает
    q.add(1200.0, "")          # пустой — тоже
    q.begin()

    check("хлыст: марки выдаются по порядку",
          [q.take(2400.0) for _ in range(3)] == ["К-1", "К-2", "К-3"])
    check("хлыст: очередь исчерпана = nil", q.take(2400.0) is None)
    check("хлыст: марки не смешиваются между длинами", q.take(1200.0) == "Р-7")
    check("хлыст: пустые марки в очередь не попадают", q.take(1200.0) is None)
    check("хлыст: длина без марок = nil", q.take(999.0) is None)

    # повторная отрисовка получает марки заново
    q.begin()
    check("хлыст: повторная отрисовка снова видит марки",
          q.take(2400.0) == "К-1")

    # ключ устойчив к float-арифметике
    check("хлыст: ключ длины устойчив к float",
          MarkQueue.key(2400.0) == MarkQueue.key(1200.0 * 2.0))
    check("хлыст: разные длины дают разные ключи",
          MarkQueue.key(2400.0) != MarkQueue.key(2400.001))

    # условия размещения подписи
    check("хлыст: на длинной детали подпись помещается",
          bar_mark_fits("К-1", 2400.0, 133.0, 60.0))
    check("хлыст: на короткой детали подпись не рисуется",
          not bar_mark_fits("К-1-длинная", 120.0, 133.0, 60.0))
    check("хлыст: на тонком хлысте подпись не рисуется",
          not bar_mark_fits("К-1", 2400.0, 40.0, 60.0))


def scenario_interchange():
    """Документированное следствие: детали одной длины взаимозаменяемы."""
    q = MarkQueue()
    q.add(2400.0, "К-1")
    q.add(2400.0, "К-2")
    q.begin()
    got = {q.take(2400.0), q.take(2400.0)}
    check("хлыст: обе марки одной длины выданы ровно по разу",
          got == {"К-1", "К-2"})
    check("хлыст: порядок внутри длины совпадает с порядком измерения",
          True)  # зафиксировано в scenario_bar


def main():
    print("=== Тест подписи марок в картах раскроя ===")
    scenario_attr()
    scenario_sheet()
    scenario_bar()
    scenario_interchange()
    print("\nИтог: PASS %d, FAIL %d" % (PASS, FAIL))
    if FAIL == 0:
        print("[MARK-LOGIC][OK]")
        return 0
    print("[MARK-LOGIC][ERRORS]")
    return 1


if __name__ == "__main__":
    sys.exit(main())
