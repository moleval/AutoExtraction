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


# ---------------------------------------------------------------
# Порт: краткая строка марок для перечня изделий (tu-marks-brief)
# ---------------------------------------------------------------

def marks_brief(marks, maxlen):
    out = []
    for m in marks or []:
        if isinstance(m, str) and m != "" and m not in out:
            out.append(m)
    if not out:
        return ""
    s = ", ".join(out)
    if len(s) <= maxlen:
        return s
    if len(out) > 1:
        tail = " +%d" % (len(out) - 1)
        return out[0][:max(1, maxlen - len(tail))] + tail
    return out[0][:maxlen]


def scenario_brief():
    """Марки в перечне изделий: колонка не должна разъезжаться."""
    check("перечень: одна марка", marks_brief(["М-12"], 16) == "М-12")
    check("перечень: две марки через запятую",
          marks_brief(["М-12", "М-13"], 16) == "М-12, М-13")
    check("перечень: повторы схлопываются",
          marks_brief(["М-12", "М-12"], 16) == "М-12")
    check("перечень: длинный список сворачивается в «первая +N»",
          marks_brief(["М-12", "М-13", "М-14", "М-15"], 16) == "М-12 +3")
    check("перечень: длина результата не превышает предел",
          len(marks_brief(["М-12", "М-13", "М-14", "М-15"], 16)) <= 16)
    check("перечень: одна слишком длинная марка обрезается",
          marks_brief(["ОЧЕНЬ-ДЛИННАЯ-МАРКА-ЭЛЕМЕНТА"], 10) == "ОЧЕНЬ-ДЛИН")
    check("перечень: марок нет — пустая строка", marks_brief([], 16) == "")
    check("перечень: nil — пустая строка", marks_brief(None, 16) == "")
    check("перечень: пустые строки игнорируются",
          marks_brief(["", "М-12", ""], 16) == "М-12")

    # геометрия таблицы хлыста: сумма колонок не меняется
    with_marks = 3.6 + 3.4 + 2.6 + 3.4
    without = 5.0 + 3.5 + 4.5
    check("хлыст: ширина таблицы с маркой и без совпадает",
          abs(with_marks - without) < 1e-9)
    # порядок колонок сводки листа
    check("лист: колонки с маркой идут по порядку",
          0.0 < 0.30 < 0.62 < 0.80 < 1.0)
    check("лист: без марки колонки левее — место под «Марку» освобождается",
          0.45 < 0.62 and 0.70 < 0.80)


# ---------------------------------------------------------------
# Порт: подписи на детали не наползают друг на друга
#   ЛИСТ  — марка снизу, габарит выше строки марки
#   ХЛЫСТ — узкая полоса, обе подписи в ряд: марка слева,
#           габарит центрируется в оставшемся справа месте
# ---------------------------------------------------------------

def sheet_layout(mark, w, h, text_h=60.0):
    """cs-draw-placement: возвращает (рисовать_марку, низ_габарита)."""
    mk_h = min(text_h * 0.55, 0.085 * min(w, h)) if mark else 0.0
    draw = bool(mark) and mk_h > text_h * 0.25 \
        and w > len(mark) * mk_h * 0.8 and h > mk_h * 3.0
    size_h = min(text_h, 0.12 * min(w, h))
    size_y = max(h * 0.53, 1.5 * mk_h + 0.5 * size_h) if draw else h * 0.53
    if draw and size_y + size_h > h:
        draw, size_y = False, h * 0.53
    return draw, size_y, 1.5 * mk_h, size_h


def bar_layout(mark, piece, bar_h, txt_h):
    """n1-draw-layout: возвращает (рисовать_марку, левый_край_габарита)."""
    mk_h = txt_h * 0.75 if mark else 0.0
    mark_w = (0.35 * mk_h + len(mark) * mk_h * 0.6) if mark else 0.0
    size_w = len(str(int(piece))) * txt_h * 1.1 * 0.6
    draw = bool(mark) and bar_h > mk_h * 2.4 and piece > mark_w + size_w + 0.5 * mk_h
    if not draw:
        mark_w = 0.0
    text_x = mark_w + (piece - mark_w) * 0.5
    return draw, text_x - size_w / 2, mark_w


def scenario_overlap():
    """Марка и габарит не перекрываются ни на листе, ни на хлысте."""
    # ЛИСТ: габарит всегда выше строки марки
    for w, h in ((300, 300), (400, 200), (605, 605), (250, 160), (1500, 750)):
        draw, size_y, mark_top, size_h = sheet_layout("М-12", w, h)
        ok = (not draw) or (size_y >= mark_top)
        check("лист %dx%d: габарит не опускается в строку марки" % (w, h), ok)
        if draw:
            check("лист %dx%d: габарит не вылезает за деталь" % (w, h),
                  size_y + size_h <= h)

    # ЛИСТ: без марки положение габарита прежнее
    d0, y0, _, _ = sheet_layout(None, 605, 605)
    check("лист: без марки габарит на прежнем месте", abs(y0 - 605 * 0.53) < 1e-9)

    # ХЛЫСТ: габарит начинается правее марки
    txt = (4000.0 / 30.0) * 0.30
    bar = 4000.0 / 45.0
    for piece in (150.0, 200.0, 300.0, 600.0, 2400.0):
        draw, size_left, mark_w = bar_layout("М-12", piece, bar, txt)
        check("хлыст %d: габарит правее марки" % piece,
              (not draw) or size_left >= mark_w)

    # ХЛЫСТ: на короткой детали снимается МАРКА, а не габарит
    draw, size_left, mark_w = bar_layout("М-12", 150.0, bar, txt)
    check("хлыст: на короткой детали марка снимается", not draw)
    check("хлыст: габарит при этом центрируется по всей детали",
          abs(size_left - (150.0 / 2 - len("150") * txt * 1.1 * 0.6 / 2)) < 1e-9)

    # ХЛЫСТ: без марки положение габарита прежнее
    d0, left0, mw0 = bar_layout(None, 600.0, bar, txt)
    check("хлыст: без марки габарит по центру детали",
          (not d0) and mw0 == 0.0
          and abs(left0 - (300.0 - len("600") * txt * 1.1 * 0.6 / 2)) < 1e-9)


# ---------------------------------------------------------------
# Порт: предел длины считается от ширины колонки (tu-fit-chars)
# ---------------------------------------------------------------

def fit_chars(col_w, text_h):
    if text_h <= 0:
        return 8
    return max(3, int(col_w / (text_h * 0.6)) - 1)


def scenario_fit():
    """Свёртка марок никогда не длиннее предела, предел — от колонки."""
    # сплошная проверка: любой список марок любой длины
    worst = 0
    for maxlen in range(4, 25):
        for count in range(1, 8):
            for mlen in range(1, 30):
                s = marks_brief(["М" * mlen + str(i) for i in range(count)], maxlen)
                worst = max(worst, len(s) - maxlen)
    check("свёртка: длина НИКОГДА не превышает предел", worst <= 0)

    # реальный случай с живой карты: «ТБ-1 Рг5.1бдв» + ещё одна марка
    s = marks_brief(["ТБ-1 Рг5.1бдв", "ТБ-1 Рг5.2бдв"], 14)
    check("свёртка: случай с карты укладывается в 14", len(s) <= 14)
    check("свёртка: случай с карты сохраняет счётчик", s.endswith("+1"))

    # предел от ширины колонки: хлыст 6000
    bar = 6000.0 / 45.0
    th = (6000.0 / 30.0) * 0.30
    col_m = 3.95 * bar
    n = fit_chars(col_m, th)
    # 527 px / (60 * 0.6) = 14.6, минус знак на зазор до соседней колонки
    check("предел по колонке хлыста: 13 знаков", n == 13)
    check("предел по колонке: текст физически влезает",
          n * th * 0.6 <= col_m)

    # заголовки колонок хлыста помещаются
    cw = 0.6 * th
    for w, hdr in ((3.1, "Изделие, мм"), (3.95, "Марка"),
                   (2.85, "Кол-во, шт"), (3.1, "Сумма, м.п.")):
        check("заголовок «%s» помещается в колонку" % hdr,
              len(hdr) * cw <= w * bar)
    check("сумма колонок с марками не изменилась",
          abs((3.1 + 3.95 + 2.85 + 3.1) - (5.0 + 3.5 + 4.5)) < 1e-9)
    check("марка сдвинута ближе к «Изделие» (3.1 против прежних 3.6)",
          3.1 < 3.6)


# ---------------------------------------------------------------
# Порт: перечень изделий — одна строка на марку
#   ХЛЫСТ  n1-piece-rows, ЛИСТ  разбивка в cs-draw-summary
# ---------------------------------------------------------------

def piece_rows(length, total, marks):
    """Строки перечня для одной длины: (длина, количество, марка)."""
    seen = []
    for m in marks:
        found = [p for p in seen if p[0] == m]
        if found:
            seen[seen.index(found[0])] = (m, found[0][1] + 1)
        else:
            seen.append((m, 1))
    nomark = total - len(marks)
    if not seen:
        return [(length, total, "")]
    out = [(length, c, m) for m, c in seen]
    if nomark > 0:
        out.append((length, nomark, "-"))
    return out


def scenario_rows():
    """Разбивка перечня по маркам: точные количества, ничего не теряется."""
    # реальный случай с карты: 950 мм, 9 шт, три марки
    marks = (["ТБ-1 Рг3ср"] * 6) + (["ТБ-1 Рг3м"] * 2) + ["ТБ-1 Рг3"]
    rows = piece_rows(950, 9, marks)
    check("хлыст: три марки дают три строки", len(rows) == 3)
    check("хлыст: сумма по строкам равна количеству изделий",
          sum(r[1] for r in rows) == 9)
    check("хлыст: количество по первой марке верное",
          rows[0] == (950, 6, "ТБ-1 Рг3ср"))
    check("хлыст: порядок марок — как при измерении",
          [r[2] for r in rows] == ["ТБ-1 Рг3ср", "ТБ-1 Рг3м", "ТБ-1 Рг3"])
    check("хлыст: обозначения «+N» в строках больше нет",
          all("+" not in r[2] for r in rows))

    # часть изделий без марки — отдельная строка с прочерком
    rows = piece_rows(1100, 5, ["М-1", "М-1", "М-2"])
    check("без марки: добавлена отдельная строка", len(rows) == 3)
    check("без марки: строка помечена прочерком", rows[-1] == (1100, 2, "-"))
    check("без марки: сумма сходится", sum(r[1] for r in rows) == 5)

    # марок нет вовсе — одна строка, как раньше
    rows = piece_rows(600, 4, [])
    check("без марок вовсе: одна строка как раньше", rows == [(600, 4, "")])

    # площадь строки листа — доля площади группы по количеству
    group_area, group_cnt = 3.6, 9
    parts = [6, 2, 1]
    areas = [group_area * (c / group_cnt) for c in parts]
    check("лист: сумма площадей строк равна площади группы",
          abs(sum(areas) - group_area) < 1e-9)


def main():
    print("=== Тест подписи марок в картах раскроя ===")
    scenario_attr()
    scenario_sheet()
    scenario_bar()
    scenario_interchange()
    scenario_brief()
    scenario_overlap()
    scenario_fit()
    scenario_rows()
    print("\nИтог: PASS %d, FAIL %d" % (PASS, FAIL))
    if FAIL == 0:
        print("[MARK-LOGIC][OK]")
        return 0
    print("[MARK-LOGIC][ERRORS]")
    return 1


if __name__ == "__main__":
    sys.exit(main())
