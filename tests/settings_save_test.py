#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Порт сохранения окна настроек: слой таблицы и слой вставки.

Проверяется то, что пользователь делает руками: задал слой таблицы ->
«Сохранить» -> переключил задачу -> вернулся, слой на месте.

Модель повторяет `ae-settings-ui-save-task` и `ae-settings-ui-load-task`
из `Extraction/settings.lsp` (только поля вывода), поэтому её падение
означает дефект в диалоге, а не в тесте.
"""

from __future__ import annotations

import sys

# Вывод в PowerShell идёт через пайп, поэтому Python кодирует текст в
# ANSI-кодировку системы (cp1251). Любой символ вне неё ронял прогон
# UnicodeEncodeError прямо посреди проверок. Заменяем непечатаемое на '?',
# чтобы тест доходил до итога. Основное лечение — PYTHONIOENCODING=utf-8
# в scripts/test.ps1; это страховка на случай прямого запуска.
try:
    sys.stdout.reconfigure(errors="replace")
except (AttributeError, ValueError):
    pass

PASS = 0
FAIL = 0


def check(name, ok):
    global PASS, FAIL
    if ok:
        PASS += 1
        print("[SETTINGS][PASS] %s" % name)
    else:
        FAIL += 1
        print("[SETTINGS][FAIL] %s" % name)


TABLE_TASKS = ("FASONKA", "SUBSYSTEM", "CLADDING", "VITRAZH", "ZAPOLNENIE")
CUT_TASKS = ("CUTLINE", "CUTSHEET")


def trim(value):
    return (value or "").strip()


class Store:
    """*AE-SETTINGS-DATA*: плоский список (section, key, value)."""

    def __init__(self):
        self.data = []

    def set(self, section, key, value):
        self.data = [e for e in self.data if not (e[0] == section and e[1] == key)]
        self.data.append((section, key, value if isinstance(value, str) else ""))

    def get(self, section, key, default=None):
        for e in self.data:
            if e[0] == section and e[1] == key:
                return e[2]
        return default

    def output(self, task, key, default):
        return self.get("task." + task.upper(), key, default)


def save_task(store, task, tiles):
    """Порт ae-settings-ui-save-task: только поля вывода.

    tiles - значения полей диалога. Поля, неактивные для задачи, диалог
    очищает при загрузке (ae-settings-ui-load-task), поэтому в модели они
    пустые - ровно как в жизни.
    """
    section = "task." + task.upper()

    table_layer = trim(tiles.get("edt_table_layer", ""))
    insert_layer = trim(tiles.get("edt_insert_layer", ""))
    frame_layer = trim(tiles.get("edt_frame_layer", ""))
    template = trim(tiles.get("edt_block_template", ""))

    if task in TABLE_TASKS:
        store.set(section, "output.table.layer", table_layer)
    if task in CUT_TASKS:
        store.set(section, "output.block.template", template)
        store.set(section, "output.insert.layer", insert_layer)
        store.set(section, "output.frame.layer", frame_layer)

    # Пустое поле означает прежнее безопасное поведение.
    if task in TABLE_TASKS and table_layer == "":
        store.set(section, "output.table.layer", "CURRENT")
    if task in CUT_TASKS and insert_layer == "":
        store.set(section, "output.insert.layer", "CURRENT")
    if task in CUT_TASKS and frame_layer == "":
        store.set(section, "output.frame.layer", "Невидимые")
    if task in CUT_TASKS and template == "":
        store.set(section, "output.block.template",
                  "Раскрой {DWG}" if task == "CUTLINE" else "Раскрой листа {DWG}")
    return True


def load_task(store, task):
    """Порт ae-settings-ui-load-task: что попадёт в поле при открытии задачи."""
    tiles = {
        "edt_table_layer": "",
        "edt_insert_layer": "",
        "edt_frame_layer": "",
        "edt_block_template": "",
    }
    if task in TABLE_TASKS:
        tiles["edt_table_layer"] = store.output(task, "output.table.layer", "CURRENT")
    if task in CUT_TASKS:
        tiles["edt_block_template"] = store.output(
            task, "output.block.template",
            "Раскрой листа {DWG}" if task == "CUTSHEET" else "Раскрой {DWG}")
        tiles["edt_insert_layer"] = store.output(task, "output.insert.layer", "CURRENT")
        tiles["edt_frame_layer"] = store.output(task, "output.frame.layer", "Невидимые")
    return tiles


def scenario_table_layer():
    """Слой таблицы: задал -> сохранил -> переключил задачу -> вернулся."""
    print("\n--- Слой таблицы: сохранение и переключение задач ---")
    store = Store()

    for task in TABLE_TASKS:
        layer = "ТАБЛИЦА_" + task
        # Диалог: поле слоя вставки для этих задач пустое (оно им не нужно).
        save_task(store, task, {"edt_table_layer": layer, "edt_insert_layer": ""})
        got = load_task(store, task)["edt_table_layer"]
        check("%s: слой таблицы '%s' сохранился" % (task, layer), got == layer)

    # Переключение по задачам в обе стороны ничего не затирает.
    tiles = load_task(store, "FASONKA")
    check("переключение: у FASONKA после обхода задач всё ещё свой слой",
          tiles["edt_table_layer"] == "ТАБЛИЦА_FASONKA")
    tiles = load_task(store, "ZAPOLNENIE")
    check("переключение: у ZAPOLNENIE свой слой, а не соседский",
          tiles["edt_table_layer"] == "ТАБЛИЦА_ZAPOLNENIE")

    # Слои задач не пересекаются.
    check("слои таблиц разных задач независимы",
          len({load_task(store, t)["edt_table_layer"] for t in TABLE_TASKS}) == len(TABLE_TASKS))


def scenario_insert_layer():
    """Слой вставки карты (CUTLINE / CUTSHEET) - тот же класс дефекта."""
    print("\n--- Слой вставки карты ---")
    store = Store()

    for task in CUT_TASKS:
        layer = "ВСТАВКА_" + task
        # Поле слоя таблицы у этих задач пустое и отключено.
        save_task(store, task, {"edt_table_layer": "", "edt_insert_layer": layer})
        got = load_task(store, task)["edt_insert_layer"]
        check("%s: слой вставки '%s' сохранился" % (task, layer), got == layer)


def scenario_empty_fallback():
    """Пустое поле по-прежнему означает безопасное умолчание."""
    print("\n--- Пустое поле: умолчания ---")
    store = Store()

    save_task(store, "FASONKA", {"edt_table_layer": "", "edt_insert_layer": ""})
    check("FASONKA: пустой слой таблицы -> CURRENT",
          store.output("FASONKA", "output.table.layer", "") == "CURRENT")

    save_task(store, "CUTLINE", {"edt_table_layer": "", "edt_insert_layer": "",
                                 "edt_frame_layer": "", "edt_block_template": ""})
    check("CUTLINE: пустой слой вставки -> CURRENT",
          store.output("CUTLINE", "output.insert.layer", "") == "CURRENT")
    check("CUTLINE: пустой слой рамки -> Невидимые",
          store.output("CUTLINE", "output.frame.layer", "") == "Невидимые")
    check("CUTLINE: пустой шаблон -> Раскрой {DWG}",
          store.output("CUTLINE", "output.block.template", "") == "Раскрой {DWG}")
    save_task(store, "CUTSHEET", {"edt_block_template": ""})
    check("CUTSHEET: пустой шаблон -> Раскрой листа {DWG}",
          store.output("CUTSHEET", "output.block.template", "") == "Раскрой листа {DWG}")

    # Явно заданный CURRENT тоже сохраняется как CURRENT (не путать с пустым).
    save_task(store, "SUBSYSTEM", {"edt_table_layer": "CURRENT", "edt_insert_layer": ""})
    check("SUBSYSTEM: заданный вручную CURRENT сохраняется",
          store.output("SUBSYSTEM", "output.table.layer", "x") == "CURRENT")


def scenario_cross_task_isolation():
    """Сохранение одной задачи не трогает секции других."""
    print("\n--- Изоляция секций ---")
    store = Store()
    for task in TABLE_TASKS:
        save_task(store, task, {"edt_table_layer": "СЛОЙ_" + task})
    save_task(store, "CUTLINE", {"edt_insert_layer": "ВСТАВКА_CUTLINE"})

    intact = all(store.output(t, "output.table.layer", "") == "СЛОЙ_" + t
                 for t in TABLE_TASKS)
    check("CUTLINE не затёр слои таблиц остальных задач", intact)
    check("у CUTLINE нет ключа output.table.layer",
          store.output("CUTLINE", "output.table.layer", None) is None)


def main():
    print("=== Тест настроек: слой таблицы и слой вставки ===")
    scenario_table_layer()
    scenario_insert_layer()
    scenario_empty_fallback()
    scenario_cross_task_isolation()

    print("\nPASS: %d, FAIL: %d" % (PASS, FAIL))
    if FAIL:
        return 1
    print("[SETTINGS][OK]")
    return 0


if __name__ == "__main__":
    sys.exit(main())
