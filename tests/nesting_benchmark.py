#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Сверка качества раскроя CUTSHEET с независимым раскладчиком.

Задача-эталон (живой прогон 2026-10-08): лист 1500x3000, рез 10 мм,
поворот разрешён, 268 деталей. CUTSHEET дал 35 листов, полезный выход 77,1%.

Скрипт отвечает на вопрос «есть ли алгоритмы лучше»:
  1) прогоняет 50 комбинаций (5 правил размещения x 5 порядков x 2 стратегии
     по листам) независимой реализации MaxRects;
  2) считает НИЖНЮЮ ГРАНИЦУ числа листов и доказывает оптимальность;
  3) показывает чувствительность к резу и формату листа.

Только stdlib. Запуск: python3 tests/nesting_benchmark.py
"""
from __future__ import annotations

import math

# Эталонный заказ: (ширина, высота, количество)
PARTS = [
    (1500, 750, 18), (1050, 750, 7), (750, 1050, 14), (950, 750, 7),
    (750, 950, 14), (750, 850, 7), (850, 750, 7), (750, 750, 30),
    (750, 500, 14), (605, 605, 60), (605, 605, 1), (445, 605, 36),
    (605, 330, 10), (445, 330, 3), (208, 605, 12), (300, 400, 7),
    (160, 605, 12), (300, 300, 7), (208, 330, 1), (160, 330, 1),
]
SHEET_W, SHEET_H, KERF = 1500, 3000, 10
CUTSHEET_RESULT = 35          # что дал модуль в живом прогоне

# Рез моделируется раздуванием: деталь (w+k)x(h+k) в листе (W+k)x(H+k).
# Это в точности «зазор k между соседями, у кромки листа зазора нет».


def build(kerf: int):
    out = []
    for w, h, n in PARTS:
        out += [(w + kerf, h + kerf, w, h)] * n
    return out


class Sheet:
    __slots__ = ("free", "used", "bw", "bh")

    def __init__(self, bw: int, bh: int):
        self.bw, self.bh = bw, bh
        self.free = [(0, 0, bw, bh)]
        self.used = []

    def score(self, w, h, heur):
        best = None
        for (fx, fy, fw, fh) in self.free:
            for (rw, rh) in ((w, h), (h, w)):
                if rw <= fw and rh <= fh:
                    lx, ly = fw - rw, fh - rh
                    if heur == "BSSF":
                        s = (min(lx, ly), max(lx, ly))
                    elif heur == "BLSF":
                        s = (max(lx, ly), min(lx, ly))
                    elif heur == "BAF":
                        s = (fw * fh - rw * rh, min(lx, ly))
                    elif heur == "BL":
                        s = (fy + rh, fx)
                    else:                       # CP — контактный счёт
                        s = (-self._contact(fx, fy, rw, rh), fy + rh)
                    if best is None or s < best[0]:
                        best = (s, fx, fy, rw, rh)
        return best

    def _contact(self, x, y, w, h):
        c = 0
        if x == 0 or x + w == self.bw:
            c += h
        if y == 0 or y + h == self.bh:
            c += w
        for (ux, uy, uw, uh) in self.used:
            if ux == x + w or ux + uw == x:
                c += max(0, min(uy + uh, y + h) - max(uy, y))
            if uy == y + h or uy + uh == y:
                c += max(0, min(ux + uw, x + w) - max(ux, x))
        return c

    def place(self, x, y, w, h):
        self.used.append((x, y, w, h))
        nf = []
        for (fx, fy, fw, fh) in self.free:
            if x >= fx + fw or x + w <= fx or y >= fy + fh or y + h <= fy:
                nf.append((fx, fy, fw, fh))
                continue
            if x > fx:
                nf.append((fx, fy, x - fx, fh))
            if x + w < fx + fw:
                nf.append((x + w, fy, fx + fw - (x + w), fh))
            if y > fy:
                nf.append((fx, fy, fw, y - fy))
            if y + h < fy + fh:
                nf.append((fx, y + h, fw, fy + fh - (y + h)))
        out = []
        for i, a in enumerate(nf):
            inside = any(
                i != j and b[0] <= a[0] and b[1] <= a[1]
                and b[0] + b[2] >= a[0] + a[2] and b[1] + b[3] >= a[1] + a[3]
                for j, b in enumerate(nf)
            )
            if not inside:
                out.append(a)
        self.free = out


SORTS = {
    "площадь":      lambda it: sorted(it, key=lambda r: (-r[0] * r[1], -max(r[0], r[1]))),
    "макс.сторона": lambda it: sorted(it, key=lambda r: (-max(r[0], r[1]), -r[0] * r[1])),
    "мин.сторона":  lambda it: sorted(it, key=lambda r: (-min(r[0], r[1]), -r[0] * r[1])),
    "периметр":     lambda it: sorted(it, key=lambda r: -(r[0] + r[1])),
    "высота":       lambda it: sorted(it, key=lambda r: (-r[1], -r[0])),
}
HEURS = ["BSSF", "BLSF", "BAF", "BL", "CP"]


def pack_open(items, heur, bw, bh):
    """Деталь кладётся в лучшее место среди ВСЕХ открытых листов."""
    sheets = [Sheet(bw, bh)]
    for (w, h, _ow, _oh) in items:
        best = None
        for s in sheets:
            sc = s.score(w, h, heur)
            if sc and (best is None or sc[0] < best[0][0]):
                best = (sc, s)
        if best is None:
            s = Sheet(bw, bh)
            sheets.append(s)
            best = (s.score(w, h, heur), s)
        (sc, fx, fy, rw, rh), s = best
        s.place(fx, fy, rw, rh)
    return sheets


def pack_fill(items, heur, bw, bh):
    """Лист добивается до отказа: лучшая пара (деталь, место) на каждом шаге."""
    rest, sheets = list(items), []
    while rest:
        s = Sheet(bw, bh)
        sheets.append(s)
        while True:
            best = None
            for i, (w, h, _ow, _oh) in enumerate(rest):
                sc = s.score(w, h, heur)
                if sc is None:
                    continue
                key = (sc[0], -(w * h))
                if best is None or key < best[0]:
                    best = (key, sc, i)
            if best is None:
                break
            (_, (_sc, fx, fy, rw, rh), i) = best
            s.place(fx, fy, rw, rh)
            rest.pop(i)
    return sheets


def lower_bound(kerf: int, sheet_w: int, sheet_h: int):
    """Строгая нижняя граница: детали, которые не встают рядом по ширине.

    Если у детали ОБЕ стороны + рез больше половины ширины листа, то две такие
    детали не помещаются рядом ни при каком повороте — значит на листе они идут
    строго друг над другом и их число ограничено высотой листа.
    """
    bw, bh = sheet_w + kerf, sheet_h + kerf
    blockers = [(w, h, n) for w, h, n in PARTS if 2 * (min(w, h) + kerf) > bw]
    count = sum(n for _, _, n in blockers)
    if not count:
        return None, 0, 0
    step = min(min(w, h) + kerf for w, h, _ in blockers)
    per_sheet = bh // step
    if per_sheet <= 0:
        return None, count, 0
    return math.ceil(count / per_sheet), count, per_sheet


def main() -> int:
    area = sum(w * h * n for w, h, n in PARTS)
    total = sum(n for _, _, n in PARTS)
    bw, bh = SHEET_W + KERF, SHEET_H + KERF
    items = build(KERF)

    print("=" * 72)
    print(f"Задача: лист {SHEET_W}x{SHEET_H}, рез {KERF} мм, поворот разрешён")
    print(f"Деталей: {total}, их площадь {area / 1e6:.3f} м2, лист {SHEET_W * SHEET_H / 1e6:.3f} м2")
    print(f"CUTSHEET в живом прогоне: {CUTSHEET_RESULT} листов, "
          f"полезный выход {100 * area / (CUTSHEET_RESULT * SHEET_W * SHEET_H):.2f}%")
    print("=" * 72)

    best = None
    results = {}
    for sname, sf in SORTS.items():
        for heur in HEURS:
            for mode, fn in (("все листы", pack_open), ("добивание", pack_fill)):
                n = len(fn(sf(items), heur, bw, bh))
                results[(mode, heur, sname)] = n
                if best is None or n < best[0]:
                    best = (n, f"{mode}/{heur}/{sname}")
    worst = max(results.values())
    print(f"\n50 комбинаций независимого MaxRects: лучший {best[0]}, худший {worst}")
    print(f"  лучший вариант: {best[1]}")
    print(f"  комбинаций, давших {best[0]} листов: "
          f"{sum(1 for v in results.values() if v == best[0])} из {len(results)}")

    lb, cnt, per = lower_bound(KERF, SHEET_W, SHEET_H)
    print(f"\nНижняя граница (доказательство):")
    print(f"  деталей, у которых обе стороны >= {bw // 2 - KERF + 1} мм: {cnt}")
    print(f"  две такие рядом: {2 * (750 + KERF)} > {bw} -> на одной высоте не стоят")
    print(f"  на лист их влезает не больше: {bh} // {750 + KERF} = {per}")
    print(f"  => не меньше ceil({cnt}/{per}) = {lb} листов")

    verdict = "ОПТИМУМ ДОСТИГНУТ" if CUTSHEET_RESULT <= lb else "есть запас"
    print(f"\nВЕРДИКТ: CUTSHEET {CUTSHEET_RESULT} листов, теоретический минимум {lb} -> {verdict}")

    print("\nЧувствительность (тот же раскладчик, другие параметры):")
    print(f"  {'рез':>4} {'лист':>12} {'листов':>7} {'выход':>8}")
    for (k, w, h) in [(10, 1500, 3000), (5, 1500, 3000), (1, 1500, 3000), (0, 1500, 3000),
                      (10, 1510, 3010), (10, 1520, 3020), (10, 1550, 3050)]:
        it = build(k)
        n = len(pack_open(SORTS["площадь"](it), "BSSF", w + k, h + k))
        print(f"  {k:4} {f'{w}x{h}':>12} {n:7} {100 * area / (n * w * h):7.2f}%")

    ok = best[0] >= CUTSHEET_RESULT and CUTSHEET_RESULT <= lb
    print(f"\nRESULT: {'PASS' if ok else 'FAIL'} "
          f"(CUTSHEET не хуже независимого раскладчика и не хуже нижней границы)")
    return 0 if ok else 1


if __name__ == "__main__":
    raise SystemExit(main())
