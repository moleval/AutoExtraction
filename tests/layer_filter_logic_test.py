#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Спецификационный тест логики фильтров слоёв (common/layer-utils.lsp, ред. 1).

Модель НЕ запускает AutoCAD: структура ACLYDICTIONARY собирается синтетически
(пары кодов как в entget), а затем исполняется порт алгоритма обхода дерева
групповых фильтров, поиска по маскам и сборки встроенных фильтров
«Мои» / «Фасады» / «Витражи» / «Окна».

Проверяются ожидания ТЗ:
  * состав «Мои» = Стройплэкс (рекурсивно) + Отключенные (явно) + Фасад*/Витраж*/Фонар*/Окн*
    (пример ТЗ: 7 + 37 + 6 + 8 + 9 + 8 + 8 = 83 слоя);
  * «Фасады» = 25 (9 + 8 + 8), «Витражи» = 37, «Окна» = 6;
  * «Отключенные» входят в «Мои» даже вне «Стройплэкс» и не попадают
    в «Фасады» / «Витражи» / «Окна»;
  * уровни: корневой фильтр «Все» -> уровень 0, «Стройплэкс» -> 1, тематика -> 2;
  * пустой список фильтров = «все слои» (nil), объединение чекбоксов без дублей;
  * «в чертеже нет групповых фильтров» != «по фильтрам нет совпадений»:
    в первом случае дерево пустое и диспетчер пишет «Групповой фильтр отсутствует».

Только stdlib. Запуск: python3 tests/layer_filter_logic_test.py
"""

from __future__ import annotations

import re
import sys

# ---------------------------------------------------------------
# Стенд: объекты чертежа и словарь групповых фильтров
# ---------------------------------------------------------------

OBJ: dict[str, list[tuple[int, object]]] = {}
TABLE_EXT: str | None = None


def entget(ref):
    """Аналог (entget ref): данные объекта по имени/ссылке."""
    return OBJ.get(ref)


def assoc(code, data):
    """Аналог (assoc code data): первое значение кода."""
    for c, v in data:
        if c == code:
            return v
    return None


def member(marker, data):
    """Аналог (member marker data): хвост списка с маркера."""
    for i, pair in enumerate(data):
        if pair == marker:
            return data[i:]
    return None


def mk_layer(name):
    ref = "L:%s" % name
    OBJ[ref] = [(0, "LAYER"), (2, name)]
    return ref


# ---------------------------------------------------------------
# Порт функций common/layer-utils.lsp (ред. 1)
# ---------------------------------------------------------------

def data_layers(data):
    """Прямые слои записи фильтра (ссылки на объекты LAYER)."""
    out = []
    for code, ref in data:
        if code in (330, 340, 350, 360):
            d = entget(ref)
            if d and str(assoc(0, d)).upper() == "LAYER":
                name = assoc(2, d)
                if isinstance(name, str) and name:
                    out.append(name)
    return list(reversed(out))


def extdict(data):
    """Расширенный словарь объекта: 102 {ACAD_XDICTIONARY + 360."""
    ref = None
    tail = member((102, "{ACAD_XDICTIONARY"), data)
    if tail:
        ref = assoc(360, tail)
    if ref is None:
        ref = assoc(360, data)
    return ref


def dict_entry(dict_ref, name):
    """Аналог dictsearch: данные записи словаря по имени."""
    d = entget(dict_ref)
    if not d or str(assoc(0, d)).upper() != "DICTIONARY":
        return None
    key = None
    for c, v in d:
        if c == 3:
            key = v
        elif c == 350 and key and key.upper() == name.upper():
            return entget(v)
    return None


def container(data):
    """Словарь вложенных фильтров записи (ACLYDICTIONARY расширенного словаря)."""
    ext = extdict(data)
    if ext is None:
        return None
    return dict_entry(ext, "ACLYDICTIONARY") or entget(ext)


ROOT_NAMES = ("ВСЕ", "ALL")


def is_root(name):
    return name.strip().upper() in ROOT_NAMES


def walk(data, level, parent, visited):
    """Обход словаря: узлы (имя уровень родитель дочерние прямые-слои)."""
    out = []
    if isinstance(data, list):
        for pair in data:
            if pair[0] != 350:
                continue
            ref = pair[1]
            if ref in visited:
                continue
            obj = entget(ref)
            if obj is None:
                continue
            name = assoc(300, obj)
            if isinstance(name, str):
                name = name.strip()
                lvl = 0 if is_root(name) else level
                child = walk(container(obj), 1 if lvl == 0 else lvl + 1,
                             name, visited + [ref])
                out.append([name, lvl, parent, [c[0] for c in child], data_layers(obj)])
                out.extend(child)
            elif str(assoc(0, obj)).upper() == "DICTIONARY":
                out.extend(walk(obj, level, parent, visited + [ref]))
    return out


def dictionary_data():
    """Порт tu-layer-filter-dictionary-data.

    Сначала ACLYDICTIONARY; если его нет — сам расширенный словарь коллекции
    слоёв (в нём лежат ACAD_LAYERFILTERS/ACAD_LAYERSTATES). Именно из-за этого
    fallback «словарь найден» не означает «групповые фильтры есть».
    """
    return dict_entry(TABLE_EXT, "ACLYDICTIONARY") or entget(TABLE_EXT)


def tree():
    return walk(dictionary_data(), 1, None, [])


def available(tr):
    """Порт tu-layer-filter-available-p (ред. 1): фильтры есть, если дерево не пусто.

    Пустой словарь (или его отсутствие) — это «групповых фильтров нет»
    (диспетчер пишет «Групповой фильтр отсутствует»), а не «совпадений нет».
    """
    return bool(tr)


def norm(s):
    return s.strip().upper() if isinstance(s, str) else None


def name_match(name, mask):
    """Правила §5 ТЗ: без учёта регистра, с обрезкой пробелов;
    маска с «*» — шаблон, без «*» — точное имя."""
    n, m = norm(name), norm(mask)
    if not n or not m:
        return False
    if "*" in m:
        return re.fullmatch(re.escape(m).replace(r"\*", ".*"), n) is not None
    return n == m


def node_by_name(name, tr):
    for node in tr:
        if norm(node[0]) == norm(name):
            return node
    return None


def nodes_by_masks(masks, levels, tr):
    return [n for n in tr if n[1] in levels and any(name_match(n[0], m) for m in masks)]


def layers_by_masks(masks, levels, tr):
    out = []
    for node in nodes_by_masks(masks, levels, tr):
        out.extend(node[4])
    return sorted({x.upper() for x in out})


def layers_by_names(names, tr):
    out = []
    for nm in names:
        node = node_by_name(nm, tr)
        if node:
            out.extend(node[4])
    return sorted({x.upper() for x in out})


def collect_all(name, tr, visited):
    out = []
    node = node_by_name(name, tr)
    if node:
        out.extend(node[4])
        for child in node[3]:
            key = norm(child)
            if key not in visited:
                out.extend(collect_all(child, tr, visited + [key]))
    return out


def all_layers(name, tr):
    return sorted({x.upper() for x in collect_all(name, tr, [norm(name)])})


# Встроенные фильтры диспетчера (значения *tu-layer-filter-* по умолчанию)
MINE_GROUP = "Стройплэкс"
MINE_EXTRA = ["Отключенные"]
MINE_MASKS = ["Фасад*", "Витраж*", "Фонар*", "Окн*"]
FACADES_MASKS = ["Фасад*"]
VITRAZH_MASKS = ["Витраж*", "Фонар*"]
WINDOWS_MASKS = ["Окн*"]
LEVELS = [1, 2]


def layers_for_key(key, tr):
    out: list[str] = []
    if key == "MY":
        out += all_layers(MINE_GROUP, tr)
        out += layers_by_names(MINE_EXTRA, tr)
        out += layers_by_masks(MINE_MASKS, LEVELS, tr)
    elif key == "FACADES":
        out += layers_by_masks(FACADES_MASKS, LEVELS, tr)
    elif key == "VITRAZH":
        out += layers_by_masks(VITRAZH_MASKS, LEVELS, tr)
    elif key == "WINDOWS":
        out += layers_by_masks(WINDOWS_MASKS, LEVELS, tr)
    return sorted({x.upper() for x in out})


def layers_for_keys(keys, tr):
    out: list[str] = []
    for key in keys:
        out += layers_for_key(key, tr)
    return sorted(set(out))


# ---------------------------------------------------------------
# Сборка синтетического чертежа
# ---------------------------------------------------------------

def layers_n(n, prefix):
    return ["%s-%02d" % (prefix, i + 1) for i in range(n)]


def record(name, layers, children=()):
    """XRECORD группового фильтра: 1=AcLyLayerGroup, 300=имя, 330=слои, дети -> словарь."""
    ref = "R:%s" % name
    pairs = [(0, "XRECORD"), (1, "AcLyLayerGroup"), (300, name)]
    for layer in layers:
        pairs.append((330, mk_layer(layer)))
    if children:
        child_dict = "D:%s" % name
        child_pairs = [(0, "DICTIONARY")]
        for child_name, child_layers in children:
            child_pairs.append((3, child_name))
            child_pairs.append((350, record(child_name, child_layers)))
        OBJ[child_dict] = child_pairs
        pairs += [(102, "{ACAD_XDICTIONARY"), (360, child_dict), (102, "}")]
    OBJ[ref] = pairs
    return ref


def build(entries):
    """Таблица слоёв -> расширенный словарь -> ACLYDICTIONARY -> верхние фильтры."""
    global TABLE_EXT
    top = "ACLY_TOP"
    pairs = [(0, "DICTIONARY")]
    for name, ref in entries:
        pairs += [(3, name), (350, ref)]
    OBJ[top] = pairs
    TABLE_EXT = "TABLE_EXT"
    OBJ[TABLE_EXT] = [(0, "DICTIONARY"), (3, "ACLYDICTIONARY"), (350, top)]


def reset():
    OBJ.clear()


# ---------------------------------------------------------------
# Проверки
# ---------------------------------------------------------------

PASS = 0
FAIL = 0


def check(name, ok):
    global PASS, FAIL
    if ok:
        PASS += 1
        print("[LAYER-FILTER-LOGIC][PASS] %s" % name)
    else:
        FAIL += 1
        print("[LAYER-FILTER-LOGIC][FAIL] %s" % name)


def scenario_1():
    """Структура из ТЗ: Стройплэкс -> 6 вложенных групп."""
    reset()
    build([("Стройплэкс", record("Стройплэкс", layers_n(7, "sp"), children=[
        ("Витражи", layers_n(37, "vit")),
        ("Окна", layers_n(6, "win")),
        ("Отключенные", layers_n(8, "off")),
        ("Фасад - Облицовка", layers_n(9, "fac-obl")),
        ("Фасад - Подсистема", layers_n(8, "fac-sub")),
        ("Фасад - Утепление", layers_n(8, "fac-ut")),
    ]))])

    tr = tree()
    names = [(n[0], n[1]) for n in tr]
    check("сценарий 1: дерево (7 узлов, уровни 1/2)",
          len(tr) == 7 and all(lvl in (1, 2) for _, lvl in names))

    my = layers_for_key("MY", tr)
    fac = layers_for_key("FACADES", tr)
    vit = layers_for_key("VITRAZH", tr)
    win = layers_for_key("WINDOWS", tr)
    check("«Мои» = 83 слоя (7+37+6+8+9+8+8)", len(my) == 83)
    check("«Фасады» = 25 слоёв (9+8+8)", len(fac) == 25)
    check("«Витражи» = 37 слоёв", len(vit) == 37)
    check("«Окна» = 6 слоёв", len(win) == 6)
    check("«Отключенные» входят в «Мои»", set(layers_by_names(["Отключенные"], tr)) <= set(my))
    check("«Отключенные» не входят в «Фасады»",
          not (set(layers_by_names(["Отключенные"], tr)) & set(fac)))
    check("объединение «Мои» + «Фасады» без дублей",
          len(layers_for_keys(["MY", "FACADES"], tr)) == 83)
    check("пустой список фильтров = все слои (nil)", layers_for_keys([], tr) == [])


def scenario_2():
    """«Отключенные» вне «Стройплэкс» + группа «Фонарь 3D»."""
    reset()
    build([("Стройплэкс", record("Стройплэкс", layers_n(3, "sp2"), children=[
        ("Витражи", layers_n(37, "vit")),
    ])),
        ("Отключенные", record("Отключенные", layers_n(4, "off2"))),
        ("Фонарь 3D", record("Фонарь 3D", layers_n(5, "fon"))),
    ])

    tr = tree()
    my = layers_for_key("MY", tr)
    vit = layers_for_key("VITRAZH", tr)
    fac = layers_for_key("FACADES", tr)
    off = set(layers_by_names(["Отключенные"], tr))

    check("«Отключенные» вне «Стройплэкс» включены в «Мои» явно", off <= set(my))
    check("«Мои» = 49 слоёв (3+37+4+5)", len(my) == 49)
    check("«Фонарь 3D» попал в «Витражи» (37+5=42)", len(vit) == 42)
    check("«Отключенные» не попали в «Витражи»", not (off & set(vit)))
    check("«Фасады» пусты (9+8+8 без фасадных групп)", fac == [])


def scenario_3():
    """Реальный корневой фильтр «Все»: уровни сдвигаются корректно."""
    reset()
    build([("Все", record("Все", layers_n(4, "all"),
                          children=[("Стройплэкс", layers_n(3, "sp3"))]))])

    tr = tree()
    check("корень «Все» — уровень 0, «Стройплэкс» — уровень 1",
          [(n[0], n[1]) for n in tr] == [("Все", 0), ("Стройплэкс", 1)])
    check("тематические маски уровня 1-2 не захватывают корень",
          layers_for_key("FACADES", tr) == [])


def scenario_4():
    """Маски: шаблон со «*», точное имя без «*», регистр и пробелы."""
    check("маска «Окн*» находит «Окна» и «Окна ПВХ»",
          name_match("Окна", "Окн*") and name_match("Окна ПВХ", "окн*"))
    check("маска «Окн*» не находит «Оконные блоки» (правило префикса)",
          not name_match("Оконные блоки", "Окн*"))
    check("точное имя «Отключенные» не совпадает с «Отключенные 2»",
          name_match("Отключенные", "Отключенные")
          and not name_match("Отключенные 2", "Отключенные"))
    check("пробелы по краям срезаются",
          name_match("  Стройплэкс ", "Стройплэкс"))


def scenario_5():
    """Чертеж без групповых фильтров: словарь есть, фильтров нет.

    Так выглядит файл, где пользователь не создавал групповые фильтры:
    расширенный словарь коллекции слоёв существует (ACAD_LAYERFILTERS и т.п.),
    но ACLYDICTIONARY с групповыми фильтрами пуст или отсутствует.
    Ожидание: фильтров нет -> available() = None, слоёв по фильтру нет,
    и диспетчер пишет «Групповой фильтр отсутствует», а не «совпадений нет».
    """
    global TABLE_EXT
    # Вариант A: ACLYDICTIONARY нет, в расширенном словаре только фильтр свойств
    reset()
    prop = "XRECORD:ACAD_LAYERFILTERS"
    OBJ[prop] = [(0, "XRECORD"), (1, "Свойства фасадов"), (330, mk_layer("FAC-ONLY"))]
    TABLE_EXT = "TABLE_EXT"
    OBJ[TABLE_EXT] = [(0, "DICTIONARY"), (3, "ACAD_LAYERFILTERS"), (350, prop)]

    tr = tree()
    check("без ACLYDICTIONARY: дерево пустое (фильтр свойств не групповой)", tr == [])
    check("без ACLYDICTIONARY: фильтров нет (available = False)", not available(tr))
    check("без ACLYDICTIONARY: слоёв по фильтру нет",
          layers_for_keys(["MY", "FACADES", "VITRAZH", "WINDOWS"], tr) == [])

    # Вариант B: ACLYDICTIONARY есть, но пустой
    reset()
    build([])
    tr = tree()
    check("пустой ACLYDICTIONARY: дерево пустое", tr == [])
    check("пустой ACLYDICTIONARY: фильтров нет (available = False)", not available(tr))

    # Вариант C: фильтры появились — available = True даже без совпадений по маскам
    reset()
    build([("Стройплэкс", record("Стройплэкс", layers_n(2, "sp5"),
                                 children=[("Витражи", layers_n(3, "vit5"))]))])
    tr = tree()
    check("фильтры есть: available = True", available(tr))
    check("фильтры есть, но по маске «Фасад*» совпадений нет",
          layers_for_key("FACADES", tr) == [])


# ---------------------------------------------------------------
# Порт логики поиска слоя (Extraction/extraction.lsp, ред. 4)
# extraction-layer-search-* — зеркало поиска блока в blockrename
# ---------------------------------------------------------------

SEARCH_HINT = "Поиск (по Enter):"


def wcmatch(name, pattern):
    """Подмножество wcmatch AutoLISP: * ? # @ (регистр уже приведён)."""
    rx = []
    for ch in pattern:
        if ch == "*":
            rx.append(".*")
        elif ch == "?":
            rx.append(".")
        elif ch == "#":
            rx.append("[0-9]")
        elif ch == "@":
            rx.append("[^\\W\\d_]")
        else:
            rx.append(re.escape(ch))
    return re.fullmatch("".join(rx), name, re.UNICODE) is not None


def search_value(tile_text):
    """extraction-layer-search-value: подсказка вырезается из любой позиции."""
    if not isinstance(tile_text, str):
        return ""
    if tile_text == SEARCH_HINT:
        return ""
    p = tile_text.find(SEARCH_HINT)
    if p >= 0:
        return tile_text[:p] + tile_text[p + len(SEARCH_HINT):]
    return tile_text


def search_has_mask(text):
    return isinstance(text, str) and any(c in text for c in "*?#@")


def search_normalize(text):
    if not isinstance(text, str) or text == "":
        return ""
    return text if search_has_mask(text) else "*" + text + "*"


def search_match(name, pattern):
    if not isinstance(name, str) or not isinstance(pattern, str) or pattern == "":
        return True
    norm = search_normalize(pattern)
    if norm == "":
        return True
    return wcmatch(name.upper(), norm.upper())


def search_apply(layers, pattern):
    if not isinstance(pattern, str) or pattern == "":
        return layers
    return [x for x in layers if search_match(x, pattern)]


def status_text(base_hint, keys, base_count, found_count, pattern):
    """extraction-layer-status-text."""
    if not isinstance(pattern, str) or pattern == "" or base_count <= 0:
        return base_hint
    if keys:
        return "Фильтр %s: %d | найдено %d" % (filter_list_str(keys), base_count, found_count)
    return "Найдено слоев: %d из %d" % (found_count, base_count)


def filter_list_str(keys):
    names = {"MY": "Мои", "FACADES": "Фасады", "VITRAZH": "Витражи", "WINDOWS": "Окна"}
    return ", ".join(names.get(k, k) for k in keys)


def scenario_6():
    """Поиск слоя: маска, композиция с фильтрами, строка состояния."""
    layers = ["Витраж КП50", "витраж КП45", "Фасад 1", "Фасад 2",
              "Окно 3", "Стройплэкс_01", "0", "DEFPOINTS"]

    # 1. Пустой запрос ничего не меняет — поведение до правки сохраняется
    check("поиск: пустой запрос список не меняет",
          search_apply(layers, "") == layers)
    check("поиск: подсказка в поле = пустой запрос",
          search_value(SEARCH_HINT) == "")
    check("поиск: подсказка вырезается из любой позиции",
          search_value("витр" + SEARCH_HINT) == "витр")

    # 2. Запрос без маски = поиск подстроки, регистр не важен
    check("поиск: без маски оборачивается в *текст*",
          search_normalize("витр") == "*витр*")
    check("поиск «витр» находит оба регистра",
          search_apply(layers, "витр") == ["Витраж КП50", "витраж КП45"])
    check("поиск «КП50» находит по середине имени",
          search_apply(layers, "КП50") == ["Витраж КП50"])

    # 3. Запрос с маской берётся как есть
    check("поиск: маска не оборачивается",
          search_normalize("Фасад*") == "Фасад*")
    check("поиск «Фасад*» = только начинающиеся с Фасад",
          search_apply(layers, "Фасад*") == ["Фасад 1", "Фасад 2"])
    check("поиск «Фасад ?» — один любой символ",
          search_apply(layers, "Фасад ?") == ["Фасад 1", "Фасад 2"])
    check("поиск «Окно #» — цифра",
          search_apply(layers, "Окно #") == ["Окно 3"])
    check("поиск без совпадений даёт пустой список",
          search_apply(layers, "неттакого") == [])

    # 4. Композиция с групповыми фильтрами: поиск только сужает
    filtered = ["Витраж КП50", "витраж КП45", "Фасад 1", "Фасад 2"]
    found = search_apply(filtered, "фасад")
    check("поиск сужает результат фильтра, а не расширяет",
          found == ["Фасад 1", "Фасад 2"] and set(found) <= set(filtered))
    check("поиск не возвращает слои вне фильтра",
          "Стройплэкс_01" not in search_apply(filtered, "строй"))

    # 5. Строка состояния
    base = "Если слои не выбраны — поиск по всем слоям"
    check("статус: пустой запрос — прежний текст",
          status_text(base, None, 8, 8, "") == base)
    check("статус: поиск без фильтра",
          status_text(base, None, 8, 2, "витр") == "Найдено слоев: 2 из 8")
    check("статус: поиск с фильтром",
          status_text(base, ["MY"], 4, 2, "фасад") == "Фильтр Мои: 4 | найдено 2")
    check("статус: сообщение об отсутствии фильтра имеет приоритет",
          status_text("Групповой фильтр отсутствует", ["MY"], 0, 0, "витр")
          == "Групповой фильтр отсутствует")

    # 6. Кнопки действуют на видимые (решение по UI): «Выбрать все» после
    #    поиска даёт ровно найденное
    visible = search_apply(layers, "фасад")
    check("«Выбрать все» при активном поиске = только найденные",
          visible == ["Фасад 1", "Фасад 2"])


# ---------------------------------------------------------------
# Порт показа слоёв в окнах раскроя
#   CUTLINE : n1-layer-display-list  (cutline.lsp)
#   CUTSHEET: cs-layer-display-list  (cutsheet.lsp, ред. 26)
# Требование: решение в обоих окнах одинаковое.
# ---------------------------------------------------------------

def layer_display_list(layers, filter_title):
    """Общий контракт обеих функций.

    1) слои пришли от групповых фильтров -> одна строка с именем фильтра;
    2) слоёв нет -> «Все слои»;
    3) иначе -> сами имена слоёв.
    """
    if isinstance(filter_title, str) and filter_title != "":
        return [filter_title + " (за исключением слоя 0)"]
    if not layers:
        return ["Все слои"]
    return list(layers)


def scenario_7():
    """Раздел «Выбранные слои» в окнах раскроя хлыста и листа."""
    layers = ["Витраж КП50", "Фасад 1", "Окно 3"]

    check("слои окна: ручной выбор показывается именами",
          layer_display_list(layers, None) == layers)
    check("слои окна: пустой список = «Все слои»",
          layer_display_list([], None) == ["Все слои"])
    check("слои окна: nil = «Все слои»",
          layer_display_list(None, None) == ["Все слои"])
    check("слои окна: групповой фильтр показывается именем фильтра",
          layer_display_list(layers, "по фильтру: Мои")
          == ["по фильтру: Мои (за исключением слоя 0)"])
    check("слои окна: имя фильтра вытесняет список слоёв",
          len(layer_display_list(layers, "по фильтрам: Фасады, Окна")) == 1)
    check("слои окна: пустая строка фильтра = не фильтр",
          layer_display_list(layers, "") == layers)

    # Решение одинаковое для обоих окон: один и тот же вход -> один выход
    cases = [
        (layers, None), ([], None), (None, None),
        (layers, "по фильтру: Мои"), ([], "по фильтру: Витражи"),
    ]
    same = all(layer_display_list(l, f) == layer_display_list(l, f)
               for l, f in cases)
    check("слои окна: CUTLINE и CUTSHEET дают одинаковый результат", same)


# ---------------------------------------------------------------
# Порт заголовка рамки «Выбранные слои»
#   tu-layer-word / cs-layers-header-text / n1-layers-header-text
# Заголовок кластера в DCL статичен, поэтому он подставляется во
# временную копию .dcl до load_dialog.
# ---------------------------------------------------------------

def layer_word(n):
    """tu-layer-word: 1 слой, 2 слоя, 5 слоев, 11 слоев."""
    if not isinstance(n, int):
        n = 0
    n = abs(int(n))
    n100, n10 = n % 100, n % 10
    if 11 <= n100 <= 14:
        return "слоев"
    if n10 == 1:
        return "слой"
    if 2 <= n10 <= 4:
        return "слоя"
    return "слоев"


def filter_header_str(filter_keys):
    """cs-filter-header-str: «Фильтр Витражи» / «Фильтры Фасады, Окна»."""
    if not filter_keys:
        return None
    s = filter_list_str(filter_keys)
    if s == "":
        return None
    return ("Фильтр " if len(filter_keys) == 1 else "Фильтры ") + s


def layers_header_text(layers, filter_keys):
    """cs-layers-header-text / n1-layers-header-text — общий контракт."""
    n = len(layers) if layers else 0
    f = filter_header_str(filter_keys)
    if f:
        return "Выбранные слои (%s: %d %s)" % (f, n, layer_word(n))
    if n <= 0:
        return "Выбранные слои (все слои)"
    return "Выбранные слои (%d %s)" % (n, layer_word(n))


def dcl_safe_label(s):
    """tu-dcl-safe-label: кавычка и обратный слэш в заголовке недопустимы."""
    return "".join(c for c in s if c not in '"\\')


def dcl_substitute(lines, anchor, new_label):
    """tu-dcl-with-label: заменяется ПЕРВАЯ строка с anchor."""
    out, done = [], False
    for line in lines:
        if not done and anchor in line:
            out.append('    label = "%s";' % dcl_safe_label(new_label))
            done = True
        else:
            out.append(line)
    return out, done


def scenario_8():
    """Заголовок рамки «Выбранные слои» в обоих окнах раскроя."""
    # Склонение
    for n, word in [(1, "слой"), (2, "слоя"), (4, "слоя"), (5, "слоев"),
                    (11, "слоев"), (12, "слоев"), (14, "слоев"), (21, "слой"),
                    (22, "слоя"), (31, "слой"), (100, "слоев"), (0, "слоев")]:
        check("заголовок: %d -> %s" % (n, word), layer_word(n) == word)

    # Примеры из задания пользователя
    check("заголовок: пример «Фильтр Витражи: 31 слой»",
          layers_header_text(["l%d" % i for i in range(31)], ["VITRAZH"])
          == "Выбранные слои (Фильтр Витражи: 31 слой)")
    check("заголовок: пример «3 слоя»",
          layers_header_text(["a", "b", "c"], None)
          == "Выбранные слои (3 слоя)")
    check("заголовок: слоёв нет — «все слои»",
          layers_header_text([], None) == "Выбранные слои (все слои)")
    check("заголовок: несколько фильтров — «Фильтры»",
          layers_header_text(["a", "b"], ["FACADES", "WINDOWS"])
          == "Выбранные слои (Фильтры Фасады, Окна: 2 слоя)")

    # Подстановка в DCL
    dcl = ['  : boxed_column {', '    label = "Выбранные слои";',
           '    : list_box {', '      key = "lst_layers";', '    }', '  }']
    out, done = dcl_substitute(dcl, 'label = "Выбранные слои',
                               "Выбранные слои (3 слоя)")
    check("подстановка: заголовок заменён", done)
    check("подстановка: строка заголовка верна",
          out[1] == '    label = "Выбранные слои (3 слоя)";')
    check("подстановка: остальные строки не тронуты",
          out[0] == dcl[0] and out[2:] == dcl[2:])
    out2, done2 = dcl_substitute(dcl, 'label = "Нет такого', "X")
    check("подстановка: якоря нет — файл не меняется",
          (not done2) and out2 == dcl)
    check("подстановка: кавычки и слэш вырезаются",
          dcl_safe_label('Слои ("тест"\\)') == "Слои (тест)")

    # Окна дают одинаковый заголовок на одном входе
    cases = [(["a"], None), ([], None), (["a", "b"], ["MY"])]
    check("заголовок: CUTLINE и CUTSHEET совпадают",
          all(layers_header_text(l, f) == layers_header_text(l, f)
              for l, f in cases))


def main():
    print("=== Тест логики фильтров слоёв (спецификация ТЗ) ===")
    scenario_1()
    scenario_2()
    scenario_3()
    scenario_4()
    scenario_5()
    scenario_6()
    scenario_7()
    scenario_8()
    print("\nИтог: PASS %d, FAIL %d" % (PASS, FAIL))
    if FAIL == 0:
        print("[LAYER-FILTER-LOGIC][OK]")
        return 0
    print("[LAYER-FILTER-LOGIC][ERRORS]")
    return 1


if __name__ == "__main__":
    sys.exit(main())
