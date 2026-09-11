#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
run_stage5_tests.py — Этап 5: автоматизированное тестирование модуля «Заполнение»
и регрессии диспетчера EXTRACTION без AutoCAD (на mock-окружении).

Сценарии (по плану):
  Группа 6 (выполняется ПЕРВОЙ): регрессия — задачи «Фасонка» и «Подсистема»
        через диспетчер (run-task) — убеждаемся, что существующие модули не сломаны.
  Группа 1: одиночные блоки / граничные случаи «Заполнения»
            (включая регрессии трёх исправленных дефектов Этапа 5:
             1.3 — счётчик количества; 1.4 — числовая сортировка; 1.6 — trim слоёв).
  Группа 2/3: множественный выбор, несколько слоёв, предвыбор (pickfirst).
  Группа 4: диспетчер EXTRACTION — отмена, несуществующая задача, отсутствующие
            модули, полный путь «Заполнения», кнопки «Раскрой хлыста»/
            «Раскрой листа» (регрессия дефекта с CUTLINE-MAIN).
  Группа 5: экспорт XLS/GAL, путь по умолчанию, занятый файл, c:zapolnenie.

Запуск:  python3 tests/run_stage5_tests.py
"""

import os
import re
import sys
import glob
import shutil
import traceback

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from autolisp import (  # noqa: E402
    Interpreter, Sym, LispError, LispExit, Variant, Pair, read_one,
)
from acad_mocks import AcadContext, SelectionSet, DynProp, prop  # noqa: E402

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
EXTRACTION_DIR = os.path.join(REPO_ROOT, "Extraction")
TMP_ROOT = os.path.join(REPO_ROOT, "tests", "tmp_stage5")

RESULTS = []


def S(x):
    return Sym(x)


# ------------------------------------------------------------------
# Канва тестового прогона
# ------------------------------------------------------------------

def load_all(interp):
    files = []
    files.extend(glob.glob(os.path.join(REPO_ROOT, "common", "*.lsp")))
    files.extend(glob.glob(os.path.join(EXTRACTION_DIR, "*.lsp")))
    files = [f for f in files if "test" not in os.path.basename(f).lower()]
    files.sort(key=lambda f: (0 if "common" in f else 1, f))
    for f in files:
        interp.load_file(f)
    return files


def new_ctx(tag):
    tmp = os.path.join(TMP_ROOT, tag)
    shutil.rmtree(tmp, ignore_errors=True)
    os.makedirs(tmp, exist_ok=True)
    interp = Interpreter()
    ctx = AcadContext(interp, tmp)
    ctx.fs.search_dirs = (EXTRACTION_DIR, REPO_ROOT, os.path.join(REPO_ROOT, "common"))
    load_all(interp)
    return interp, ctx


def run_case(name, fn):
    rec = {"name": name, "status": "PASS", "notes": []}
    try:
        fn(rec)
    except AssertionError as e:
        rec["status"] = "FAIL"
        rec["error"] = str(e)
    except Exception as e:
        rec["status"] = "ERROR"
        rec["error"] = "%s: %s" % (type(e).__name__, e)
        rec["trace"] = traceback.format_exc()
    RESULTS.append(rec)
    tag = "OK " if rec["status"] == "PASS" else "!!!"
    print("[%s] %s — %s" % (tag, name, rec["status"]))
    if rec["status"] != "PASS":
        print("     %s" % rec.get("error", ""))
        if "--verbose" in sys.argv and rec.get("trace"):
            print(rec["trace"])


def eval_eval(interp, form):
    """eval с эмуляцией *error*: если внутри функции ошибка не перехвачена,
    вызываем глобальный *error* (как делает AutoCAD)."""
    try:
        return interp.eval(form)
    except LispError as e:
        handler = interp.functions.get("*ERROR*")
        if handler:
            interp.call_user(handler, [str(e)])
            return None
        raise


def rt(*args):
    """Форма (run-task 'TASK layers mode xls gal table base)."""
    return [S("RUN-TASK"), [S("QUOTE"), S(args[0])] if isinstance(args[0], str) else args[0]] + list(args[1:])


def run_layers_arg(names):
    return [Pair(S("QUOTE"), None)]  # placeholder, не используется


def quoted_list(items):
    return [S("QUOTE"), [str(x) for x in items]]


# ------------------------------------------------------------------
# Фикстуры чертежа
# ------------------------------------------------------------------

def _mk_block(ctx, layer, effname, props):
    """INSERT на слое с дин.свойствами props = dict(имя -> значение)."""
    ename = ctx.db.add_entity("INSERT", [prop(8, layer), prop(2, effname)])
    obj = ctx.db.ename_to_vla(ename)
    obj.effectivename = str(effname)
    obj.name = str(effname)
    storage = dict(props)

    def gdp():
        return [DynProp(obj, k, v) for k, v in storage.items()]

    obj.getdynamicblockproperties = gdp
    return ename


def add_zap_block(ctx, layer, name, h=None, w=None, vis=None):
    """Блок заполнения: h/w — значения свойств «ВЫСОТА В СВЕТУ»/«ШИРИНА В СВЕТУ» (мм,
    без припуска). vis — значение свойства видимости (ТИП элемента)."""
    props = {}
    if vis is not None:
        props["Видимость"] = vis
    if h is not None:
        props["Высота в свету"] = float(h)
    if w is not None:
        props["Ширина в свету"] = float(w)
    return _mk_block(ctx, layer, name, props)


def add_len_block(ctx, layer, name, length, vis=None):
    props = {"ДЛИНА": float(length)}
    if vis is not None:
        props["Видимость"] = vis
    return _mk_block(ctx, layer, name, props)


def agg(interp, enames):
    """(zapolnenie-aggregate (list e1..en)) — юнит-уровень."""
    form = [S("ZAPOLNENIE-AGGREGATE"), [S("QUOTE"), list(enames)]]
    return interp.eval(form)


def zap_main_form(layers=None, mode="DETAIL", xls=False, gal=False, table=False, base=None):
    return [S("ZAPOLNENIE-MAIN"),
            [S("QUOTE"), [str(x) for x in layers]] if layers else None,
            mode, S("T") if xls else None, S("T") if gal else None,
            S("T") if table else None, base]


def expect_area(enames_dims):
    """Сумма площадей с припуском +26 и fix(), как в модуле."""
    tot = 0.0
    for (h, w) in enames_dims:
        H, W = int(h + 26), int(w + 26)
        tot += H * W / 1000000.0
    return tot


def find_total(msg_rows, text):
    m = re.search(r"Заполнение: обработано (\d+) блоков, общая площадь ([\d.,]+) м2", text)
    return (int(m.group(1)), float(m.group(2).replace(",", "."))) if m else (None, None)


# ==================================================================
# ГРУППА 6 — РЕГРЕССИЯ (ПЕРВАЯ)
# ==================================================================

def t6_1_fasonka(rec):
    interp, ctx = new_ctx("6_1")
    ctx.add_layer("фасонка")
    add_len_block(ctx, "фасонка", "ПЛАНКА_А", 1000)
    add_len_block(ctx, "фасонка", "ПЛАНКА_А", 1000)
    add_len_block(ctx, "фасонка", "ПЛАНКА_Б", 500)
    add_len_block(ctx, "фасонка", "ПЛАНКА_Б", 500)
    eval_eval(interp, rt("FASONKA", None, "SUMMARY", None, None, None, None))
    out = ctx.out.text
    assert "не загружен" not in out.lower() or "Фасонка:" in out, out[-300:]
    m = re.search(r"Фасонка: найдено блоков (\d+)", out)
    assert m, "нет итогового сообщения: %s" % out[-300:]
    assert int(m.group(1)) == 4, "блоков=%s (ожидалось 4)" % m.group(1)
    rec["notes"].append(m.group(0))


def t6_2_subsystem(rec):
    interp, ctx = new_ctx("6_2")
    ctx.add_layer("подсистема")
    add_len_block(ctx, "подсистема", "СТОЙКА_1", 1000)
    add_len_block(ctx, "подсистема", "РИГЕЛЬ_1", 2000)
    eval_eval(interp, rt("SUBSYSTEM", None, "SUMMARY", None, None, None, None))
    out = ctx.out.text
    assert "не загружен" not in out.lower() or "Подсистема:" in out, out[-300:]
    m = re.search(r"Подсистема: элементов (\d+), общая длина ([\d.]+) м", out)
    assert m, "нет итогового сообщения: %s" % out[-400:]
    assert int(m.group(1)) == 2, "элементов=%s (ожидалось 2)" % m.group(1)
    assert abs(float(m.group(2)) - 3.0) < 0.01, "длина=%s (ожидалось 3.00)" % m.group(2)
    rec["notes"].append(m.group(0))


# ==================================================================
# ГРУППА 1 — одиночные блоки / границы
# ==================================================================

def t1_1_empty(rec):
    interp, ctx = new_ctx("1_1")
    eval_eval(interp, zap_main_form())
    out = ctx.out.text
    assert "Блоки заполнения не найдены" in out, out[-300:]


def t1_2_single(rec):
    """Один блок 1400×1200 (+26) → 1426×1226, 1 шт, 1.75 м2."""
    interp, ctx = new_ctx("1_2")
    ctx.add_layer("заполнение")
    add_zap_block(ctx, "заполнение", "ЗАПОЛНЕНИЕ_ОКНО", h=1400, w=1200, vis="Окно")
    eval_eval(interp, zap_main_form())
    out = ctx.out.text
    n, area = find_total(None, out)
    assert n == 1, "элементов=%s (ожидалось 1): %s" % (n, out[-300:])
    assert abs(area - 1426 * 1226 / 1e6) < 0.005, "площадь=%s" % area
    rec["notes"].append("1426x1226, %.2f м2" % area)


def t1_3_counter_regression(rec):
    """Регрессия бага №1 (счётчик): два ОДИНАКОВЫХ блока → количество = 2,
    а не (ширина + 1)."""
    interp, ctx = new_ctx("1_3")
    ctx.add_layer("заполнение")
    e1 = add_zap_block(ctx, "заполнение", "ЗАПОЛНЕНИЕ_ОКНО", h=1400, w=1200, vis="Окно")
    e2 = add_zap_block(ctx, "заполнение", "ЗАПОЛНЕНИЕ_ОКНО", h=1400, w=1200, vis="Окно")
    rows = agg(interp, [e1, e2])
    assert rows is not None and len(rows) == 1, "строк агрегации=%s" % rows
    cnt = rows[0][3]
    assert cnt == 2, "количество=%s (регрессия: было ширина+1)" % cnt
    eval_eval(interp, zap_main_form())
    n, area = find_total(None, ctx.out.text)
    assert n == 2, "элементов=%s" % n
    rec["notes"].append("2 одинаковых блока → cnt=2")


def t1_4_numeric_sort_regression(rec):
    """Регрессия бага №2 (сортировка): ширины 526 и 1226 должны идти
    числом (526 < 1226), а не строкой («1226» < «526»)."""
    interp, ctx = new_ctx("1_4")
    ctx.add_layer("заполнение")
    e1 = add_zap_block(ctx, "заполнение", "ЗАПОЛНЕНИЕ_ОКНО", h=1000, w=1200, vis="Окно")
    e2 = add_zap_block(ctx, "заполнение", "ЗАПОЛНЕНИЕ_ОКНО", h=1000, w=500, vis="Окно")
    rows = agg(interp, [e1, e2])
    assert len(rows) == 2, rows
    w_first, w_second = rows[0][2], rows[1][2]
    assert (w_first, w_second) == (526, 1226), \
        "порядок ширин %s (регрессия строковой сортировки)" % [(w_first, w_second)]
    rec["notes"].append("526 < 1226")


def t1_5_zero_dim_skipped(rec):
    """Нулевой размер → позиция пропускается; если таких все — «Нет данных»."""
    interp, ctx = new_ctx("1_5")
    ctx.add_layer("заполнение")
    add_zap_block(ctx, "заполнение", "ЗАПОЛНЕНИЕ_ОКНО", h=0, w=1200, vis="Окно")
    eval_eval(interp, zap_main_form())
    out = ctx.out.text
    assert "Нет данных для отчёта" in out, out[-300:]


def t1_6_c_zapolnenie_trim_regression(rec):
    """Регрессия бага №3 (пробел после запятой): команда ZAPOLNENIE с вводом
    'заполнение, Заполнение витражей' находит блоки обоих слоёв."""
    interp, ctx = new_ctx("1_6")
    ctx.add_layer("заполнение")
    ctx.add_layer("Заполнение витражей")
    add_zap_block(ctx, "заполнение", "ЗАПОЛНЕНИЕ_ОКНО", h=1400, w=1200, vis="Окно")
    add_zap_block(ctx, "Заполнение витражей", "ЗАПОЛНЕНИЕ_ВИТРАЖ", h=2100, w=900, vis="Витраж")
    ctx.set_inputs(
        ("string", "заполнение, Заполнение витражей"),
        ("kword", "D"), ("kword", "N"), ("kword", "N"),
        ("kword", "N"), ("kword", "Y"),
    )
    eval_eval(interp, [S("C:ZAPOLNENIE")])
    out = ctx.out.text
    assert "Блоки заполнения не найдены" not in out, \
        "слои не разобраны (регрессия trim): %s" % out[-400:]
    n, area = find_total(None, out)
    assert n == 2, "элементов=%s (ожидалось 2): %s" % (n, out[-400:])
    rec["notes"].append("2 слоя с пробелом после запятой")


# ==================================================================
# ГРУППА 2/3 — множественный выбор / несколько слоёв / предвыбор
# ==================================================================

def t2_1_multi_sizes(rec):
    """Несколько размеров на одном слое: агрегация и итог."""
    interp, ctx = new_ctx("2_1")
    ctx.add_layer("заполнение")
    add_zap_block(ctx, "заполнение", "ЗАПОЛНЕНИЕ_ОКНО", h=1400, w=1200, vis="Окно")
    add_zap_block(ctx, "заполнение", "ЗАПОЛНЕНИЕ_ОКНО", h=1500, w=1200, vis="Окно")
    add_zap_block(ctx, "заполнение", "ЗАПОЛНЕНИЕ_ОКНО", h=1500, w=1200, vis="Окно")
    eval_eval(interp, zap_main_form())
    n, area = find_total(None, ctx.out.text)
    exp = expect_area([(1400, 1200), (1500, 1200), (1500, 1200)])
    assert n == 3, "элементов=%s" % n
    assert abs(area - exp) < 0.01, "площадь=%s ожидалось %.2f" % (area, exp)
    rec["notes"].append("3 эл, %.2f м2" % area)


def t3_1_cross_layer_aggregate(rec):
    """Одинаковые блоки на РАЗНЫХ слоях склеиваются в одну позицию
    (ключ — тип+размеры, слой не входит)."""
    interp, ctx = new_ctx("3_1")
    ctx.add_layer("заполнение")
    ctx.add_layer("заполнение витражей")
    e1 = add_zap_block(ctx, "заполнение", "ЗАПОЛНЕНИЕ_ОКНО", h=1400, w=1200, vis="Стекло")
    e2 = add_zap_block(ctx, "заполнение витражей", "ЗАПОЛНЕНИЕ_ВИТРАЖ", h=1400, w=1200, vis="Стекло")
    rows = agg(interp, [e1, e2])
    assert len(rows) == 1, "строк=%s" % rows
    assert rows[0][3] == 2, "cnt=%s" % rows[0][3]
    rec["notes"].append("2 слоя → 1 позиция, cnt=2")


def t3_2_layer_filter(rec):
    """Передан список из одного слоя — берутся только его блоки."""
    interp, ctx = new_ctx("3_2")
    ctx.add_layer("заполнение")
    ctx.add_layer("заполнение витражей")
    add_zap_block(ctx, "заполнение", "ЗАПОЛНЕНИЕ_ОКНО", h=1400, w=1200, vis="Окно")
    add_zap_block(ctx, "заполнение витражей", "ЗАПОЛНЕНИЕ_ВИТРАЖ", h=2100, w=900, vis="Витраж")
    eval_eval(interp, zap_main_form(layers=["заполнение"]))
    n, area = find_total(None, ctx.out.text)
    assert n == 1, "элементов=%s (ожидался 1 со слоя «заполнение»)" % n
    rec["notes"].append("фильтр по слою: 1 эл")


def t3_3_pickfirst(rec):
    """Предвыбор (*extraction-preselected-set*): используется и очищается."""
    interp, ctx = new_ctx("3_3")
    ctx.add_layer("заполнение")
    e1 = add_zap_block(ctx, "заполнение", "ЗАПОЛНЕНИЕ_ОКНО", h=1400, w=1200, vis="Окно")
    add_zap_block(ctx, "заполнение", "ЗАПОЛНЕНИЕ_ОКНО", h=2100, w=900, vis="Окно")
    interp.assign(S("*EXTRACTION-PRESELECTED-SET*"), SelectionSet(ctx, [e1]))
    eval_eval(interp, zap_main_form())
    n, area = find_total(None, ctx.out.text)
    assert n == 1, "элементов=%s (предвыбран 1 блок из 2)" % n
    assert interp.globals.get("*EXTRACTION-PRESELECTED-SET*") is None, \
        "предвыбор не очищен"
    rec["notes"].append("предвыбор: 1 эл, набор очищен")


# ==================================================================
# ГРУППА 4 — диспетчер EXTRACTION
# ==================================================================

def t4_1_cancel(rec):
    interp, ctx = new_ctx("4_1")
    ctx.add_layer("0")
    ctx.dialog_script()  # пустой сценарий → done_dialog(0)
    eval_eval(interp, [S("C:EXTRACTION")])
    out = ctx.out.text
    assert interp.globals.get("*EXTRACTION-ACTION*") == S("CANCEL"), \
        "ACTION=%s" % interp.globals.get("*EXTRACTION-ACTION*")
    assert "Фасонка:" not in out and "Заполнение: обработано" not in out, \
        "при отмене задача не должна запускаться"


def t4_2_unknown_task(rec):
    interp, ctx = new_ctx("4_2")
    eval_eval(interp, rt("FLYING", None, "SUMMARY", None, None, None, None))
    assert "Неизвестная задача" in ctx.out.text, ctx.out.text[-200:]


def t4_3_missing_module(rec):
    interp, ctx = new_ctx("4_3")
    interp.functions.pop("ZAPOLNENIE-MAIN", None)
    eval_eval(interp, rt("ZAPOLNENIE", None, "DETAIL", None, None, None, None))
    out = ctx.out.text
    assert "Модуль Заполнение не загружен или ошибка выполнения" in out, out[-300:]


def t4_4_cladding(rec):
    interp, ctx = new_ctx("4_4")
    eval_eval(interp, rt("CLADDING", None, "SUMMARY", None, None, None, None))
    out = ctx.out.text
    assert "Модуль Облицовка не загружен или ошибка выполнения" in out, out[-300:]


def t4_5_btn_fill_full_path(rec):
    """Полный путь: диалог → задача «Заполнение» → btn_save → run-task."""
    interp, ctx = new_ctx("4_5")
    ctx.add_layer("заполнение")
    add_zap_block(ctx, "заполнение", "ЗАПОЛНЕНИЕ_ОКНО", h=1400, w=1200, vis="Окно")
    # пользователь ранее выключил создание таблицы AutoCAD (LAST-настройки персистентны)
    interp.assign(S("*EXTRACTION-LAST-CREATE-TABLE*"), None)
    # в реальном DCL клик по радиокнопке выставляет её в "1", а остальные группы в "0"
    ctx.dialog_script(("set", "rb_task_fasonka", "0"),
                      ("set", "rb_task_subsystem", "0"),
                      ("set", "rb_task_cladding", "0"),
                      ("set", "rb_task_vitrazh", "0"),
                      ("set", "rb_task_zapolnenie", "1"),
                      ("click", "rb_task_zapolnenie"),
                      ("set", "rb_detail", "1"),
                      ("set", "rb_summary", "0"),
                      ("click", "rb_detail"),
                      ("click", "btn_save"))
    eval_eval(interp, [S("C:EXTRACTION")])
    out = ctx.out.text
    assert "Заполнение: обработано 1 блоков" in out, "модуль не выполнился: %s" % out[-500:]


def t4_6_btn_cutline(rec):
    """Регрессия дефекта «неверная функция: CUTLINE-MAIN»: управление из
    диспетчера должно достигать команды c:cutline (её заголовок в консоли)."""
    interp, ctx = new_ctx("4_6")
    ctx.add_layer("0")
    ctx.dialog_script(("click", "btn_cutline"))
    eval_eval(interp, [S("C:EXTRACTION")])
    out = ctx.out.text
    assert getattr(interp.functions, "__contains__", lambda k: False)("C:CUTLINE"), \
        "c:cutline не загружена"
    assert "Линейный раскрой мерного материала" in out, \
        "точка входа CUTLINE не получила управление: %s" % out[-500:]


def t4_7_btn_cutsheet(rec):
    """Аналогично для «Раскрой листа»: вход — команда c:cutsheet."""
    interp, ctx = new_ctx("4_7")
    ctx.add_layer("0")
    ctx.dialog_script(("click", "btn_cutsheet"))
    eval_eval(interp, [S("C:EXTRACTION")])
    out = ctx.out.text
    assert "CUTSHEET" in out, "c:cutsheet не получила управление: %s" % out[-500:]
    assert "не загружен или ошибка выполнения" not in out, \
        "ложное сообщение об ошибке: %s" % out[-300:]


# ==================================================================
# ГРУППА 5 — экспорт / сохранение
# ==================================================================

def t5_1_export_detail_default_path(rec):
    """export-excel=T, путь по умолчанию → <DWGPREFIX>Test Заполнение подробный.xls."""
    interp, ctx = new_ctx("5_1")
    ctx.add_layer("заполнение")
    add_zap_block(ctx, "заполнение", "ЗАПОЛНЕНИЕ_ОКНО", h=1400, w=1200, vis="Окно")
    eval_eval(interp, zap_main_form(xls=True))
    out = ctx.out.text
    assert "XLS сохранён" in out, out[-400:]
    fname = "Test Заполнение подробный.xls"
    text = ctx.fs.read_text(fname)
    assert "Workbook" in text and "Окно" in text, text[:300]
    rec["notes"].append(fname)


def t5_2_export_summary(rec):
    """SUMMARY-режим: агрегация по типу, файл «…краткий.xls»."""
    interp, ctx = new_ctx("5_2")
    ctx.add_layer("заполнение")
    add_zap_block(ctx, "заполнение", "ЗАПОЛНЕНИЕ_ОКНО", h=1400, w=1200, vis="Окно")
    add_zap_block(ctx, "заполнение", "ЗАПОЛНЕНИЕ_ОКНО", h=1500, w=1200, vis="Окно")
    eval_eval(interp, zap_main_form(mode="SUMMARY", xls=True))
    out = ctx.out.text
    assert "XLS сохранён" in out, out[-400:]
    text = ctx.fs.read_text("Test Заполнение краткий.xls")
    assert "Окно" in text and "2" in text, text[:400]
    rec["notes"].append("краткий.xls: 1 тип, 2 шт")


def t5_3_export_gal(rec):
    """export-txt=T → <base>.gal с размером листа и строками позиций."""
    interp, ctx = new_ctx("5_3")
    ctx.add_layer("заполнение")
    add_zap_block(ctx, "заполнение", "ЗАПОЛНЕНИЕ_ОКНО", h=1400, w=1200, vis="Окно")
    eval_eval(interp, zap_main_form(gal=True))
    out = ctx.out.text
    assert "GAL сохранён" in out, out[-400:]
    text = ctx.fs.read_text("Test Заполнение подробный.gal")
    assert "Размер листа=3210x2250" in text, text[:120]
    assert "1_Окно/1426/1226/1/" in text, text


def t5_4_locked_then_retry(rec):
    """Занятый XLS: open возвращает nil → модуль штатно переходит на CSV
    («Не удалось сохранить XLS. Сохраняю CSV...» → «CSV сохранён»);
    после освобождения повторное сохранение XLS успешно."""
    interp, ctx = new_ctx("5_4")
    ctx.add_layer("заполнение")
    add_zap_block(ctx, "заполнение", "ЗАПОЛНЕНИЕ_ОКНО", h=1400, w=1200, vis="Окно")
    fname = "Test Заполнение подробный.xls"
    ctx.lock_file(fname)
    eval_eval(interp, zap_main_form(xls=True))
    out1 = ctx.out.text
    assert "Не удалось сохранить XLS" in out1, "ожидался переход на CSV: %s" % out1[-400:]
    assert "CSV сохранён" in out1, out1[-400:]
    ctx.unlock_file(fname)
    mark = len(ctx.out.parts)
    eval_eval(interp, zap_main_form(xls=True))
    out2 = "".join(ctx.out.parts[mark:])
    assert "XLS сохранён" in out2, out2[-300:]
    text = ctx.fs.read_text(fname)
    assert "Окно" in text


def t5_5_c_zapolnenie_full_command(rec):
    """Команда ZAPOLNENIE целиком: все вопросы, экспорт согласован, файл создан."""
    interp, ctx = new_ctx("5_5")
    ctx.add_layer("заполнение")
    add_zap_block(ctx, "заполнение", "ЗАПОЛНЕНИЕ_ОКНО", h=1400, w=1200, vis="Окно")
    ctx.set_inputs(
        ("enter", None),          # слои: Enter — все
        ("kword", "D"),           # режим
        ("kword", "Y"),           # Excel
        ("kword", "N"),           # GAL
        ("kword", "N"),           # таблица AutoCAD
        ("kword", "Y"),           # путь по умолчанию
    )
    eval_eval(interp, [S("C:ZAPOLNENIE")])
    out = ctx.out.text
    assert "XLS сохранён" in out, out[-400:]
    n, area = find_total(None, out)
    assert n == 1, "элементов=%s" % n
    rec["notes"].append("интерактивная команда, 6 ответов")


# ==================================================================
# Регрессии аудита (после Этапа 5)
# ==================================================================

def t1_7_c_fasonka_trim(rec):
    """c:fasonka: ввод слоёв с пробелом после запятой находит оба слоя."""
    interp, ctx = new_ctx("1_7")
    ctx.add_layer("фасонка")
    ctx.add_layer("фасонка несущая")
    add_len_block(ctx, "фасонка", "ПЛАНКА_А", 1000)
    add_len_block(ctx, "фасонка несущая", "ПЛАНКА_Б", 500)
    ctx.set_inputs(
        ("string", "фасонка, фасонка несущая"),
        ("kword", "S"), ("kword", "N"), ("kword", "N"),
        ("kword", "N"), ("kword", "Y"),
    )
    eval_eval(interp, [S("C:FASONKA")])
    m = re.search(r"Фасонка: найдено блоков (\d+)", ctx.out.text)
    assert m and int(m.group(1)) == 2, "найдено=%s (ожидалось 2)" % (m and m.group(1))
    rec["notes"].append("trim в fasonka")


def t1_8_c_subsystem_trim(rec):
    """c:subsystem: то же — пробел после запятой."""
    interp, ctx = new_ctx("1_8")
    ctx.add_layer("подсистема")
    ctx.add_layer("подсистема оцинкованная")
    add_len_block(ctx, "подсистема", "СТОЙКА_1", 1000)
    add_len_block(ctx, "подсистема оцинкованная", "СТОЙКА_2", 2000)
    ctx.set_inputs(
        ("string", "подсистема, подсистема оцинкованная"),
        ("kword", "S"), ("kword", "N"), ("kword", "N"),
        ("kword", "N"), ("kword", "Y"),
    )
    eval_eval(interp, [S("C:SUBSYSTEM")])
    m = re.search(r"Подсистема: элементов (\d+)", ctx.out.text)
    assert m and int(m.group(1)) == 2, "элементов=%s (ожидалось 2)" % (m and m.group(1))


def t3_4_preselect_kept_on_error(rec):
    """Предвыбор НЕ очищается, если фильтрация упала с ошибкой
    (повторный запуск не теряет набор)."""
    interp, ctx = new_ctx("3_4")
    ctx.add_layer("заполнение")
    bad = ctx.db.add_entity("INSERT", [prop(2, "СЛОМАННЫЙ_БЕЗ_СЛОЯ")])  # без пары (8 . layer)
    interp.assign(S("*EXTRACTION-PRESELECTED-SET*"), SelectionSet(ctx, [bad]))
    try:
        interp.eval([S("SU-SELECT-INSERTS"), [S("QUOTE"), S("заполнение")]])
        rec["status"] = "FAIL"
        rec["error"] = "ожидалась ошибка на битом примитиве"
        return
    except LispError:
        pass
    kept = interp.globals.get("*EXTRACTION-PRESELECTED-SET*")
    assert kept is not None, "предвыбор потерян после ошибки"
    rec["notes"].append("набор сохранён")


def t5_6_fasonka_csv_fallback(rec):
    """Фасонка: занятый XLS → fallback-CSV реально создаётся (было:
    'no function definition: EU-EXPORT-CSV-DETAIL')."""
    interp, ctx = new_ctx("5_6")
    ctx.add_layer("фасонка")
    add_len_block(ctx, "фасонка", "ПЛАНКА_А", 1000)
    ctx.lock_file("Test Фасонка подробный.xls")
    eval_eval(interp, zap_main := [S("FASONKA-MAIN"), None, "DETAIL", S("T"), None, None, None])
    out = ctx.out.text
    assert "Не удалось сохранить XLS" in out, out[-300:]
    assert "no function definition" not in out, out[-300:]
    assert "CSV сохранён" in out, out[-300:]
    text = ctx.fs.read_text("Test Фасонка подробный.csv")
    assert "Фасонное железо" in text and "ПЛАНКА_А" in text, text[:300]
    rec["notes"].append("CSV fallback работает")


def t5_7_reload_loads_zapolnenie(rec):
    """RELOAD перезагружает zapolnenie.lsp (ранее отсутствовал в списке)."""
    interp, ctx = new_ctx("5_7")
    interp.load_file(os.path.join(REPO_ROOT, "reload.lsp"))
    eval_eval(interp, [S("C:RELOAD")])
    out = ctx.out.text
    assert "zapolnenie.lsp" in out, "zapolnenie.lsp не в выводе RELOAD"
    assert "НЕ НАЙДЕН" not in out, out[-400:]
    assert "Все модули AutoExtraction перезагружены" in out, out[-200:]


def t7_1_ffd_oversized_skipped(rec):
    """n1-ffd: деталь длиннее хлыста не размещается; kerf-граница
    (p + kerf > stock) тоже не даёт отрицательного отхода."""
    interp, ctx = new_ctx("7_1")
    r = interp.eval(read_one("(n1-ffd (quote (7000.0 2000.0)) 6000.0 0.0)"))
    assert r is not None and len(r) == 1, "хлыстов=%s" % r
    bar = r[0]
    assert bar[0] >= 0 and 2000.0 in bar[1:], bar
    r = interp.eval(read_one("(n1-ffd (quote (6000.0 2000.0)) 6000.0 5.0)"))
    assert r is not None and all(b[0] >= 0 for b in r), r


def t7_2_c_cutline_mixed(rec):
    """c:cutline end-to-end (main): 2x2000 + 7000 (LINE), диалог ОК ->
    1 хлыст, неразмещённые в консоли, в XLS и именование файла."""
    interp, ctx = new_ctx("7_2")
    ctx.add_layer("0")
    ents = [ctx.db.add_entity("LINE", [prop(10, [0.0, 0.0, 0.0]), prop(11, [2000.0, 0.0, 0.0])]),
            ctx.db.add_entity("LINE", [prop(10, [0.0, 0.0, 0.0]), prop(11, [2000.0, 0.0, 0.0])]),
            ctx.db.add_entity("LINE", [prop(10, [0.0, 0.0, 0.0]), prop(11, [7000.0, 0.0, 0.0])])]
    ctx.scripted_selection = SelectionSet(ctx, ents)
    ctx.dialog_script(("click", "btn_ok"))
    ctx.set_inputs(("enter", None),   # слои: Enter — все
                   ("enter", None))   # точка вставки: отказ
    interp.eval([S("C:CUTLINE")])
    out = ctx.out.text
    assert "НЕРАЗМЕЩЕННЫЕ ДЕТАЛИ" in out, out[-500:]
    assert "Длина 7000 мм, кол-во 1 шт." in out, out[-500:]
    assert "Всего неразмещенных: 1 шт." in out, out[-500:]
    assert "Хлыст 1:" in out, out[-400:]
    assert "Всего изделий: 2 шт" in out, out[-300:]
    assert "Раскладка пропущена" in out, out[-200:]
    text = ctx.fs.read_text("Test Раскрой хлыстов.xls")
    assert "НЕРАЗМЕЩЕННЫЕ ДЕТАЛИ" in text and ">7000<" in text, text[-800:]
    rec["notes"].append("диалог + LINE + XLS")


def t7_2b_c_cutline_all_oversize(rec):
    """Все детали длиннее хлыста: main выходит с пояснением,
    не размещая ни одного хлыста."""
    interp, ctx = new_ctx("7_2b")
    ctx.add_layer("0")
    e = ctx.db.add_entity("LINE", [prop(10, [0.0, 0.0, 0.0]), prop(11, [7000.0, 0.0, 0.0])])
    ctx.scripted_selection = SelectionSet(ctx, [e])
    ctx.dialog_script(("click", "btn_ok"))
    ctx.set_inputs(("enter", None))
    try:
        interp.eval([S("C:CUTLINE")])
    except LispExit:
        pass
    out = ctx.out.text
    assert "Все детали превышают длину хлыста" in out, out[-400:]
    assert "НЕРАЗМЕЩЕННЫЕ ДЕТАЛИ" in out, out[-400:]


def t7_3_cutline_xls_csv(rec):
    """Сохранение cutline по контракту main: имя «<чертёж> Раскрой
    хлыстов.xls», занят XLS -> nil, CSV-паритет."""
    interp, ctx = new_ctx("7_3")
    bars = "((1000.0 5000.0 1000.0))"
    overs = "((7000.0 1))"
    r = interp.eval(read_one("(n1-write-xls (quote %s) 6000.0 0.0 (quote %s) 1 6000.0 6000.0 100.0)" % (bars, overs)))
    assert r is not None, "n1-write-xls вернул nil"
    text = ctx.fs.read_text("Test Раскрой хлыстов.xls")
    assert "НЕРАЗМЕЩЕННЫЕ ДЕТАЛИ" in text and ">7000<" in text, text[-800:]
    ctx.lock_file("Test Раскрой хлыстов.xls")
    r = interp.eval(read_one("(n1-write-xls (quote %s) 6000.0 0.0 (quote %s) 1 6000.0 6000.0 100.0)" % (bars, overs)))
    assert r is None, "при занятом файле ожидался nil"
    r = interp.eval(read_one("(n1-write-csv (quote %s) 6000.0 0.0 (quote %s))" % (bars, overs)))
    assert r is not None, "n1-write-csv вернул nil"
    text = ctx.fs.read_text("Test Раскрой хлыстов.csv")
    assert "НЕРАЗМЕЩЕННЫЕ ДЕТАЛИ" in text and "7000" in text, text[:400]


def t7_4_mline_axis_length(rec):
    """su-mline-length (main): длина = сумма отрезков по вершинам
    осевой линии (DXF 11) — explode не требуется."""
    interp, ctx = new_ctx("7_4")
    e = ctx.db.add_entity("MLINE", [prop(71, 2), prop(72, 2),
                                    prop(11, [0.0, 0.0, 0.0]),
                                    prop(11, [1500.0, 0.0, 0.0])])
    r = interp.eval([S("N1-MLINE-LENGTH"), [S("QUOTE"), e]])
    assert abs(float(r) - 1500.0) < 0.01, "длина=%s (ожидалось 1500)" % r


def t7_5_su_get_length_substring(rec):
    """su-get-length: сначала точное «ДЛИНА», иначе — по подстроке
    («Длина профиля», «ДЛИНА1»)."""
    interp, ctx = new_ctx("7_5")
    e1 = _mk_block(ctx, "фасонка", "БЛОК_А", {"Длина профиля": 2500.0})
    r = interp.eval([S("SU-GET-LENGTH"),
                     [S("VLAX-ENAME->VLA-OBJECT"), [S("QUOTE"), e1]]])
    assert r == 2500, "по подстроке: %r (ожидалось 2500)" % r
    e2 = _mk_block(ctx, "фасонка", "БЛОК_Б", {"ДЛИНА": 1000.0, "Длина профиля": 2500.0})
    r = interp.eval([S("SU-GET-LENGTH"),
                     [S("VLAX-ENAME->VLA-OBJECT"), [S("QUOTE"), e2]]])
    assert r == 1000, "точное совпадение приоритетно: %r" % r


def t7_6_clean_name_prefix(rec):
    """clean-name: снимает ТОЛЬКО префикс «Железо »; произвольные имена
    не обрезаются."""
    interp, ctx = new_ctx("7_6")
    ctx.add_layer("фасонка")
    add_len_block(ctx, "фасонка", "ПЛАНКА_А", 1000)
    eval_eval(interp, [S("FASONKA-MAIN"), None, "SUMMARY", None, None, None, None])
    r1 = interp.eval([S("CLEAN-NAME"), "Железо Кронштейн"])
    assert r1 == "Кронштейн", repr(r1)
    r2 = interp.eval([S("CLEAN-NAME"), "ФАСОННЫЙ_ЭЛЕМЕНТ"])
    assert r2 == "ФАСОННЫЙ_ЭЛЕМЕНТ", repr(r2)


# ==================================================================
# main
# ==================================================================

def t7_7_no_yo_in_cutline(rec):
    """Буквы Ё/ё не должны встречаться в cutline.lsp (шрифт таблицы
    AutoCAD их не отображает) - заменены на Е/е."""
    raw = open(os.path.join(REPO_ROOT, "Extraction", "cutline.lsp"), "rb").read().decode("cp1251")
    assert "Ё" not in raw and "ё" not in raw


def main():
    os.makedirs(TMP_ROOT, exist_ok=True)
    print("=" * 60)
    print("ЭТАП 5 — автоматизированное тестирование «Заполнения»")
    print("база: Этап 4 (коммит 73a360f)")
    print("=" * 60)

    # Группа 6 — регрессия ПЕРВОЙ (по требованию)
    run_case("6.1 Регрессия: Фасонка через диспетчер", t6_1_fasonka)
    run_case("6.2 Регрессия: Подсистема через диспетчер", t6_2_subsystem)

    run_case("1.1 Пустой чертёж", t1_1_empty)
    run_case("1.2 Один блок, размеры с припуском", t1_2_single)
    run_case("1.3 Регрессия счётчика (2 одинаковых блока)", t1_3_counter_regression)
    run_case("1.4 Регрессия сортировки (526 обязано быть перед 1226)", t1_4_numeric_sort_regression)
    run_case("1.5 Нулевой размер пропускается", t1_5_zero_dim_skipped)
    run_case("1.6 Регрессия trim слоёв (пробел после запятой)", t1_6_c_zapolnenie_trim_regression)

    run_case("2.1 Несколько размеров на одном слое", t2_1_multi_sizes)
    run_case("3.1 Агрегация через слои (одинаковые блоки)", t3_1_cross_layer_aggregate)
    run_case("3.2 Фильтр по заданному слою", t3_2_layer_filter)
    run_case("3.3 Предвыбор (pickfirst): использован и очищен", t3_3_pickfirst)

    run_case("4.1 Отмена диалога EXTRACTION", t4_1_cancel)
    run_case("4.2 Несуществующая задача", t4_2_unknown_task)
    run_case("4.3 Отсутствующий модуль Заполнения", t4_3_missing_module)
    run_case("4.4 Незагруженный модуль (Облицовка)", t4_4_cladding)
    run_case("4.5 Полный путь: диалог → Заполнение → запуск", t4_5_btn_fill_full_path)
    run_case("4.6 Кнопка «Раскрой хлыста»: вход c:cutline разрешается", t4_6_btn_cutline)
    run_case("4.7 Кнопка «Раскрой листа»: вход c:cutsheet разрешается", t4_7_btn_cutsheet)

    run_case("5.1 Экспорт XLS (подробный, путь по умолчанию)", t5_1_export_detail_default_path)
    run_case("5.2 Экспорт XLS (краткий)", t5_2_export_summary)
    run_case("5.3 Экспорт GAL", t5_3_export_gal)
    run_case("5.4 Занятый файл → ошибка, повтор после освобождения", t5_4_locked_then_retry)
    run_case("5.5 Команда ZAPOLNENIE целиком", t5_5_c_zapolnenie_full_command)
    run_case("1.7 Регрессия trim слоёв: c:fasonka", t1_7_c_fasonka_trim)
    run_case("1.8 Регрессия trim слоёв: c:subsystem", t1_8_c_subsystem_trim)
    run_case("3.4 Предвыбор сохраняется при ошибке фильтрации", t3_4_preselect_kept_on_error)
    run_case("5.6 Фасонка: занятый XLS → fallback CSV", t5_6_fasonka_csv_fallback)
    run_case("5.7 RELOAD перезагружает zapolnenie.lsp", t5_7_reload_loads_zapolnenie)
    run_case("7.1 FFD: оверсайз и kerf-граница", t7_1_ffd_oversized_skipped)
    run_case("7.2 c:cutline: смешанный набор end-to-end (диалог+XLS)", t7_2_c_cutline_mixed)
    run_case("7.2b c:cutline: все детали длиннее хлыста", t7_2b_c_cutline_all_oversize)
    run_case("7.3 CUTLINE: имя XLS, занят XLS -> nil, CSV-паритет", t7_3_cutline_xls_csv)
    run_case("7.4 MLINE: длина по осям DXF 11", t7_4_mline_axis_length)
    run_case("7.5 su-get-length: точное→подстрока", t7_5_su_get_length_substring)
    run_case("7.6 clean-name: только префикс «Железо »", t7_6_clean_name_prefix)
    run_case("7.7 В cutline.lsp нет буквы Ё", t7_7_no_yo_in_cutline)

    npass = sum(1 for r in RESULTS if r["status"] == "PASS")
    nfail = sum(1 for r in RESULTS if r["status"] != "PASS")
    print("=" * 60)
    print("ИТОГ: %d/%d PASS, %d FAIL/ERROR" % (npass, len(RESULTS), nfail))
    print("=" * 60)
    return 0 if nfail == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
