#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Порт выбора имени элемента Подсистемы.

Модель повторяет `subsystem-element-name` из `Extraction/subsystem.lsp`
и поиск родителя `su-anon-parent-name` из `common/select-utils.lsp`:

- есть свойство видимости — берётся его значение;
- иначе EffectiveName, если он не анонимный;
- анонимное вхождение `*U5385` — это изменённый динамический блок,
  имя берётся у родителя (Block Representation / XData);
- если родителя нет, блок в спецификацию не попадает.

Первая попытка (ред. 6) отсеивала имена без букв и из-за кодировки
`ascii` выбросила все русские наименования. Здесь такого правила нет:
рабочие имена не трогаются вообще, решается только судьба `*U`.
"""

from __future__ import annotations

import sys

PASS = 0
FAIL = 0


# Вывод в PowerShell идёт через пайп, поэтому Python кодирует текст в
# ANSI-кодировку системы (cp1251). Основное лечение — PYTHONIOENCODING=utf-8
# в scripts/test.ps1; это страховка для прямого запуска.
try:
    sys.stdout.reconfigure(errors="replace")
except (AttributeError, ValueError):
    pass


def check(name, ok):
    global PASS, FAIL
    if ok:
        PASS += 1
        print("[SUBSYSTEM-NAMES][PASS] %s" % name)
    else:
        FAIL += 1
        print("[SUBSYSTEM-NAMES][FAIL] %s" % name)


def is_anonymous(name):
    return isinstance(name, str) and name[:1] == "*"


def parent_name_by_handle(raw):
    """Порт su-parent-name-by-handle: анонимное имя родителем не считается."""
    if not isinstance(raw, str) or raw == "" or is_anonymous(raw):
        return None
    return raw


def element_name(effname, vis, parent):
    """Порт subsystem-element-name. None на выходе = блок отбрасывается."""
    if vis and vis != "":
        return vis
    if effname is None:
        return "Без имени"
    if not is_anonymous(effname):
        return effname
    return parent


def aggregate_names(rows):
    """Отсев в subsystem-aggregate: строки без имени не попадают в данные."""
    return [r for r in rows if r[0] is not None]


# ---------------------------------------------------------------
# Сценарии
# ---------------------------------------------------------------

def scenario_visibility_first():
    print("\n--- Видимость важнее имени блока ---")
    check("видимость «Несущий» побеждает",
          element_name("Профиль П-1", "Несущий", None) == "Несущий")
    check("видимость «Опорный» побеждает",
          element_name("Профиль П-1", "Опорный", None) == "Опорный")


def scenario_plain_names():
    print("\n--- Обычные блоки не трогаются ---")
    check("«Профиль направляющий» остаётся",
          element_name("Профиль направляющий", None, None) == "Профиль направляющий")
    check("«Т-обр. 467» остаётся",
          element_name("Т-обр. 467", None, None) == "Т-обр. 467")
    check("«AISI 304» остаётся",
          element_name("AISI 304", None, None) == "AISI 304")
    check("«№5.385» остаётся (правило по буквам откачено)",
          element_name("№5.385", None, None) == "№5.385")
    check("блок без имени — «Без имени»",
          element_name(None, None, None) == "Без имени")


def scenario_anonymous_parent():
    """Блоки со скриншота: *U5385 и т.п. получают имя родителя."""
    print("\n--- Анонимные *U-блоки берут имя родителя ---")
    check("*U4566 -> «Кронштейн ригеля»",
          element_name("*U4566", None, "Кронштейн ригеля") == "Кронштейн ригеля")
    check("*U5385 -> «Профиль направляющий»",
          element_name("*U5385", None, "Профиль направляющий") == "Профиль направляющий")

    rows = [
        (element_name("*U4566", None, "Кронштейн ригеля"), None, 1),
        (element_name("*U5385", None, "Профиль направляющий"), 1430, 1),
        (element_name("*U5387", None, "Профиль направляющий"), 1450, 1),
        (element_name("*U5389", None, "Профиль направляющий"), 1460, 1),
        (element_name("*U5391", None, "Профиль направляющий"), 1470, 1),
    ]
    kept = aggregate_names(rows)
    check("все 5 блоков остались, получив имя родителя: %d" % len(kept),
          len(kept) == 5)
    check("анонимных имён в данных нет",
          all(not is_anonymous(r[0]) for r in kept))
    check("родители различимы: %d уникальных имён"
          % len({r[0] for r in kept}),
          len({r[0] for r in kept}) == 2)


def scenario_anonymous_without_parent():
    """Родителя нет — блок не элемент подсистемы."""
    print("\n--- Анонимный блок без родителя ---")
    check("*U9999 без родителя -> None",
          element_name("*U9999", None, None) is None)
    check("анонимное имя родителем не считается",
          parent_name_by_handle("*U1234") is None)
    check("пустой handle -> None", parent_name_by_handle("") is None)
    check("нормальный handle -> имя",
          parent_name_by_handle("Кронштейн ригеля") == "Кронштейн ригеля")

    rows = [
        (element_name("*U9999", None, None), None, 1),
        (element_name("Шляпный профиль", None, None), 210, 1),
    ]
    kept = aggregate_names(rows)
    check("в данные попал только рабочий блок: %d" % len(kept), len(kept) == 1)
    check("это «Шляпный профиль»", kept[0][0] == "Шляпный профиль")


def main():
    print("=== Тест имён элементов Подсистемы ===")
    scenario_visibility_first()
    scenario_plain_names()
    scenario_anonymous_parent()
    scenario_anonymous_without_parent()

    print("\nPASS: %d, FAIL: %d" % (PASS, FAIL))
    if FAIL:
        return 1
    print("[SUBSYSTEM-NAMES][OK]")
    return 0


if __name__ == "__main__":
    sys.exit(main())
