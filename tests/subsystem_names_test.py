#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Порт отсева служебных блоков Подсистемы.

Модель повторяет `su-name-has-letters-p` и `su-service-name-p` из
`Extraction/subsystem.lsp` и отсев в `subsystem-aggregate`:

- у блока без свойства видимости в наименование попадает EffectiveName;
- у служебных блоков оно выглядит как «№5.385» — букв нет, только цифры
  и знаки. Такой блок не является элементом подсистемы;
- пустое имя и заглушка «Без имени» — тоже служебные.

Правило: наименование без единой буквы (латиница или кириллица) в
спецификацию не попадает. Наименования с буквами остаются, даже если
в них есть цифры: «Т-обр. 467», «AISI 304».
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


# ---------------------------------------------------------------
# Порт: есть ли в строке буквы (su-name-has-letters-p)
#
# В LISP перебираются коды символов, потому что wcmatch-диапазоны
# для кириллицы зависят от кодовой страницы. Здесь тот же перебор.
# ---------------------------------------------------------------

LETTER_CODES = (
    (65, 90),        # A-Z
    (97, 122),       # a-z
    (1024, 1103),    # А-я
    1025,            # Ё
    1105,            # ё
)


def name_has_letters(s):
    if not isinstance(s, str):
        return False
    for ch in s:
        code = ord(ch)
        for rng in LETTER_CODES:
            if isinstance(rng, tuple):
                if rng[0] <= code <= rng[1]:
                    return True
            elif code == rng:
                return True
    return False


def service_name(s):
    """Порт su-service-name-p."""
    return s is None or s == "" or s == "Без имени" or not name_has_letters(s)


def filter_records(records):
    """Отсев в subsystem-aggregate: записи (name, len, cnt)."""
    return [r for r in records if not service_name(r[0])]


# ---------------------------------------------------------------
# Сценарии
# ---------------------------------------------------------------

def scenario_service_names():
    """Имена со скриншота и их родня — служебные."""
    print("\n--- Служебные наименования ---")
    cases = ["№5.385", "№5.387", "№5.389", "№5.391",
             "5.385", "123", "№ 12.3", "1.2.3", "-", "№"]
    for s in cases:
        check("«%s» — служебное" % s, service_name(s))

    check("пустое имя — служебное", service_name(""))
    check("None — служебное", service_name(None))
    check("заглушка «Без имени» — служебная", service_name("Без имени"))


def scenario_real_names():
    """Осмысленные наименования остаются."""
    print("\n--- Настоящие наименования ---")
    cases = [
        "Профиль направляющий Г-обр. 180 мм КПС 842",
        "Кронштейн основной Т-обр. 205 мм",
        "Т-обр. 467",
        "AISI 304",
        "Шляпный профиль",
        "Ёлка",
    ]
    for s in cases:
        check("«%s» — не служебное" % s, not service_name(s))


def scenario_filter_records():
    """Выборка со скриншота: четыре служебных блока уходят из таблицы."""
    print("\n--- Отсев в выборке ---")
    records = [
        ("№5.385", None, 1),
        ("№5.387", None, 1),
        ("№5.389", None, 1),
        ("№5.391", None, 1),
        ("Профиль направляющий Г-обр. 180 мм", 1430, 1),
        ("Профиль направляющий Г-обр. 180 мм", 1450, 1),
        ("Шляпный профиль", 210, 1),
    ]
    kept = filter_records(records)
    check("осталось 3 позиции из 7: %d" % len(kept), len(kept) == 3)
    check("служебные имена ушли",
          all(name_has_letters(r[0]) for r in kept))
    check("мерные позиции сохранились",
          [r[0] for r in kept].count("Профиль направляющий Г-обр. 180 мм") == 2)
    check("штучная позиция сохранилась",
          [r[0] for r in kept].count("Шляпный профиль") == 1)


def scenario_all_service():
    """Одна служебная выборка — на выходе пусто (таблица не создаётся)."""
    print("\n--- Только служебные блоки ---")
    records = [("№5.385", None, 1), ("№5.387", None, 1)]
    check("после отсева данных нет", filter_records(records) == [])


def main():
    print("=== Тест отсева служебных блоков Подсистемы ===")
    scenario_service_names()
    scenario_real_names()
    scenario_filter_records()
    scenario_all_service()

    print("\nPASS: %d, FAIL: %d" % (PASS, FAIL))
    if FAIL:
        return 1
    print("[SUBSYSTEM-NAMES][OK]")
    return 0


if __name__ == "__main__":
    sys.exit(main())
