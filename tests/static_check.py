#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Lightweight static checks for AutoExtraction.

Только stdlib. Никаких сторонних пакетов.

Проверки:
  1. Наличие обязательных файлов.
  2. Баланс скобок в .lsp (с учётом строк и комментариев).
  3. Дубликаты имён defun (project-wide, с whitelist).
  4. DCL-ключи, на которые ссылаются .lsp, объявлены в .dcl.
  5. Наличие основных *-main функций.
  6. Защита диагностики загрузки в reload.lsp: перехват загрузки tests/chkparens.lsp,
     проверка результата самообновления, встроенный поиск формы (резерв CHKLOAD).
  7. Баланс и лексика файлов tests/*.lsp (chkparens.lsp автозагружает RELOAD).
  8. Спецформы AutoLISP во всех .lsp (включая reload.lsp): (if a b c d),
     нечётный setq, кривой список аргументов defun/lambda. Такой дефект даёт
     «синтаксическая ошибка» при нулевом балансе скобок и валит весь файл.

Кодировка:
  - .lsp / .dcl — Windows-1251 (ANSI), fallback: utf-8
  - .py / .md   — utf-8
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]

LISP_DIRS = [ROOT / "Extraction", ROOT / "common"]
DCL_DIRS  = [ROOT / "Extraction"]
# Плагины - сторонние модули, пути берутся из реестра [plugins] settings.ini
# (как в RELOAD). Проверяются на баланс, лексику, спецформы и пересечение имён
# defun с проектом; проектные группы (mains, dcl-keys, wrap-guard) на них не
# распространяются.
SETTINGS_INI = ROOT / "settings.ini"
# tests/*.lsp в основные группы не входят (там свои defun и свои правила),
# но баланс и лексика проверяются отдельно: chkparens.lsp автозагружает RELOAD.
TEST_DIR = ROOT / "tests"

# --no-plugins: проверять только проект, не трогая плагины из реестра.
# Нужно, когда чужой модуль в разработке (D:\MarkZ, D:\ReplaceDBlock ...)
# ещё не приведён в порядок, а прогнать проверки самого репозитория надо.
PLUGINS_ENABLED = True

# ----------------------------------------------------------------------
# Whitelist
# ----------------------------------------------------------------------

# Локальные функции, которые переопределяются в каждом модуле — норма.
WHITELIST_DEFUN: set[str] = {
    "*error*",
}

# Дубликаты команд, которые намеренно допустимы.
WHITELIST_DUP_COMMANDS: set[str] = set()

# Обязательные *-main функции (наличие в проекте).
REQUIRED_MAINS = [
    "fasonka-main",
    "subsystem-main",
    "zapolnenie-main",
    "cutline-main",
]

# Обязательные файлы проекта.
REQUIRED_FILES = [
    "Extraction/extraction.lsp",
    "Extraction/extraction.dcl",
    "Extraction/settings.lsp",
    "Extraction/settings.dcl",
    "Extraction/help.lsp",
    "Extraction/help.dcl",
    "Extraction/fasonka.lsp",
    "Extraction/subsystem.lsp",
    "Extraction/zapolnenie.lsp",
    "Extraction/cutline.lsp",
    "Extraction/cutline_filter.dcl",
    "Extraction/cutsheet.lsp",
    "Extraction/blockrename.lsp",
    "common/task-utils.lsp",
    "common/settings-utils.lsp",
    "common/layer-utils.lsp",
    "common/select-utils.lsp",
    "common/excel-utils.lsp",
    "common/table-utils.lsp",
    "common/txt-utils.lsp",
]

# ----------------------------------------------------------------------
# Чтение с учётом кодировки
# ----------------------------------------------------------------------

def read_text(path: Path) -> str:
    """Читает файл в правильной кодировке (cp1251 -> utf-8)."""
    for enc in ("cp1251", "utf-8-sig", "utf-8"):
        try:
            return path.read_text(encoding=enc)
        except UnicodeDecodeError:
            continue
    return path.read_text(encoding="utf-8", errors="replace")


def _rel(path: Path) -> str:
    """Путь для отчёта: относительно корня, а для плагинов разработки - как есть.

    Плагины в разработке лежат в своих каталогах (например D:\\PlotFrameToPDF),
    поэтому relative_to(ROOT) для них неприменим.
    """
    try:
        return str(path.relative_to(ROOT))
    except ValueError:
        return str(path)


def lisp_files():
    for d in LISP_DIRS:
        if d.exists():
            yield from sorted(d.glob("*.lsp"))


def _reload_string_list(var: str) -> list[str]:
    """Список строк из (setq <var> '("a" "b")) в reload.lsp."""
    path = ROOT / "reload.lsp"
    if not path.exists():
        return []
    try:
        forms = lisp_parse(read_text(path))
    except LispParseError:
        return []
    for node in forms:
        if node[0] != "list" or len(node[1]) < 3:
            continue
        head, name, val = node[1][0], node[1][1], node[1][2]
        if not (head[0] == "atom" and head[1].lower() == "setq"
                and name[0] == "atom" and name[1].lower() == var.lower()):
            continue
        inner = val[1][0] if (val[0] == "quote" and val[1]) else val
        if inner[0] == "list":
            return [x[1] for x in inner[1] if x[0] == "str"]
    return []


def plugin_registry() -> list[tuple[str, str]]:
    """Реестр [plugins] из settings.ini: (имя, путь) в порядке записей.

    Читается тем же правилом, что ae-settings-read-line: комментарии ; и #,
    секция [имя], ключ=значение. Программа не знает содержимого плагинов.
    """
    if not SETTINGS_INI.exists():
        return []
    section = ""
    kv: dict[str, str] = {}
    for line in SETTINGS_INI.read_bytes().decode("cp1251").splitlines():
        text = line.strip()
        if not text or text[0] in ";#":
            continue
        if text.startswith("[") and "]" in text:
            section = text[1:text.index("]")].strip().lower()
            continue
        if section == "plugins" and "=" in text:
            key, _, value = text.partition("=")
            kv[key.strip()] = value.strip()
    try:
        count = int(kv.get("plugin.count", "0") or 0)
    except ValueError:
        count = 0
    out: list[tuple[str, str]] = []
    for i in range(1, count + 1):
        name = kv.get(f"plugin.{i}.name", "")
        if name:
            out.append((name, kv.get(f"plugin.{i}.path", "")))
    return out


def plugin_files():
    """Плагины из реестра - то, что грузит RELOAD (порядок записей, без повторов).

    Путь из реестра приоритетен; пустой путь - Plugins\\<имя>. Файла может не
    быть на этой машине: тогда плагин пропускается, это не ошибка проверки.
    """
    if not PLUGINS_ENABLED:
        return
    seen: set[str] = set()
    for name, path in plugin_registry():
        cand = Path(path) if path else ROOT / "Plugins" / name
        key = str(cand).lower()
        if key in seen:
            continue
        seen.add(key)
        if cand.is_file():
            yield cand


def dcl_files():
    for d in DCL_DIRS:
        if d.exists():
            yield from sorted(d.glob("*.dcl"))


def test_lisp_files():
    if TEST_DIR.exists():
        yield from sorted(TEST_DIR.glob("*.lsp"))


def all_lisp_files():
    """Весь LISP проекта: модули, tests/*.lsp, плагины реестра и reload.lsp."""
    yield from lisp_files()
    yield from test_lisp_files()
    yield from plugin_files()
    root_lisp = ROOT / "reload.lsp"
    if root_lisp.exists():
        yield root_lisp


# ----------------------------------------------------------------------
# Баланс скобок
# ----------------------------------------------------------------------

def strip_code(text: str) -> str:
    """Убирает ;; комментарии и содержимое строк "..." (для скобок).

    ВАЖНО: не делает предварительный срез строки по ';;',
    а обрабатывает каждый символ по очереди — иначе строки,
    содержащие ';;' (например, CSV-разделители), ломают парсер.
    """
    out = []
    in_string = False
    escape = False
    in_comment = False
    for line in text.splitlines():
        chars = []
        in_comment = False
        for ch in line:
            if in_comment:
                chars.append(" ")
                continue
            if escape:
                chars.append(" ")
                escape = False
            elif ch == "\\" and in_string:
                chars.append(" ")
                escape = True
            elif ch == '"':
                in_string = not in_string
                chars.append(" ")
            elif ch == ";" and not in_string:
                # начало комментария — до конца строки
                in_comment = True
                chars.append(" ")
            else:
                chars.append(" " if in_string else ch)
        out.append("".join(chars))
    return "\n".join(out)


def strip_lisp(text: str) -> tuple[str, bool]:
    """Код без строковых констант и комментариев (вместо них пробелы).

    Возвращает (код, незакрытая_строка). Учитывает экранирование \\" и \\.
    """
    out: list[str] = []
    instr = False
    i = 0
    n = len(text)
    while i < n:
        ch = text[i]
        if instr:
            if ch == "\\":
                out.append("  ")
                i += 2
                continue
            if ch == '"':
                instr = False
            out.append(" ")
        else:
            if ch == '"':
                instr = True
                out.append(" ")
            elif ch == ";":
                while i < n and text[i] != "\n":
                    out.append(" ")
                    i += 1
                continue
            else:
                out.append(ch)
        i += 1
    return "".join(out), instr


def check_balance_lex(path: Path) -> tuple[bool, str]:
    """Баланс скобок ПО КОДУ: строки в кавычках и комментарии не считаются.

    Наивный подсчёт всех скобок пропускает лишнюю ")" внутри строки - именно
    так в tests/chkparens.lsp прошла незамеченной лишняя скобка.
    """
    code, unterminated = strip_lisp(read_text(path))
    if unterminated:
        return False, "незакрытая строковая константа"
    bal = 0
    for line_no, line in enumerate(code.split("\n"), start=1):
        for ch in line:
            if ch == "(":
                bal += 1
            elif ch == ")":
                bal -= 1
                if bal < 0:
                    return False, f"лишняя ')' около строки {line_no}"
    if bal != 0:
        return False, f"дисбаланс скобок: {bal:+d}"
    return True, ""


def check_balance(path: Path) -> tuple[bool, str]:
    clean = strip_code(read_text(path))
    depth = 0
    for line_no, line in enumerate(clean.splitlines(), 1):
        for ch in line:
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth < 0:
                    return False, f"лишняя ')' около строки {line_no}"
    if depth != 0:
        return False, f"дисбаланс скобок: {depth:+d}"
    return True, ""


# ----------------------------------------------------------------------
# Defun-и и дубликаты
# ----------------------------------------------------------------------

DEFUN_RE = re.compile(r"\(\s*defun\s+([^\s()]+)\s*\(", re.I)


def find_defuns(path: Path) -> list[tuple[str, int]]:
    clean = strip_code(read_text(path))
    result: list[tuple[str, int]] = []
    for m in DEFUN_RE.finditer(clean):
        name = m.group(1)
        line = clean.count("\n", 0, m.start()) + 1
        result.append((name, line))
    return result


def check_duplicates() -> list[str]:
    errors: list[str] = []
    seen: dict[str, tuple[str, int]] = {}

    for path in (list(lisp_files()) + list(plugin_files())):
        rel = _rel(path)
        for name, line in find_defuns(path):
            lname = name.lower()
            if lname in WHITELIST_DEFUN:
                continue
            if lname in WHITELIST_DUP_COMMANDS:
                continue
            if lname in seen:
                prev_path, prev_line = seen[lname]
                errors.append(
                    f"{rel}:{line}: duplicate defun '{name}' "
                    f"(already {prev_path}:{prev_line})"
                )
            else:
                seen[lname] = (rel, line)

    return errors


def check_required_mains() -> list[str]:
    errors: list[str] = []
    found: set[str] = set()
    for path in lisp_files():
        for name, _ in find_defuns(path):
            found.add(name.lower())
    for main in REQUIRED_MAINS:
        if main.lower() not in found:
            errors.append(f"missing main function: {main}")
    return errors


# ----------------------------------------------------------------------
# DCL keys
# ----------------------------------------------------------------------

def find_dcl_keys() -> set[str]:
    keys: set[str] = set()
    for path in dcl_files():
        text = read_text(path)
        keys.update(re.findall(r'\bkey\s*=\s*"([^"]+)"', text, re.I))
    return keys


TILE_CALL_RE = re.compile(
    r"\(\s*(?:get_tile|set_tile|mode_tile|action_tile)\s+\"([^\"]+)\"",
    re.I,
)

# Разрешённые «ложные» ключи, если они есть в коде как строки,
# но по факту не являются DCL-ключами.
WHITELIST_DCL_KEYS: set[str] = set()


def check_dcl_keys(path: Path, dcl_keys: set[str]) -> list[str]:
    errors: list[str] = []
    if not dcl_keys:
        return errors
    text = read_text(path)
    for m in TILE_CALL_RE.finditer(text):
        key = m.group(1)
        if key in WHITELIST_DCL_KEYS:
            continue
        # отсеиваем строки, которые не могут быть DCL-ключами
        if not re.match(r"^[a-z][a-z0-9_]*$", key, re.I):
            continue
        if key not in dcl_keys:
            line = text.count("\n", 0, m.start()) + 1
            errors.append(
                f"{path.relative_to(ROOT)}:{line}: DCL key '{key}' not found"
            )
    return errors


# ----------------------------------------------------------------------
# DCL syntax guards
# ----------------------------------------------------------------------

def check_dcl_syntax(path: Path) -> list[str]:
    """DCL не знает Lisp-комментариев ';' - только /* */ и // до конца
    строки. Лишняя ';' ломает парсер AutoCAD ("синтаксическая ошибка")."""
    errors: list[str] = []
    text = read_text(path)
    for i, line in enumerate(text.splitlines(), 1):
        if line.lstrip().startswith(";"):
            errors.append(
                f"{path.relative_to(ROOT)}:{i}: Lisp-style ';' comment in DCL "
                f"(разрешены только // и /* */)"
            )
    return errors


# ----------------------------------------------------------------------
# Обязательные файлы
# ----------------------------------------------------------------------

def check_required_files() -> list[str]:
    return [
        f"missing required file: {rel}"
        for rel in REQUIRED_FILES
        if not (ROOT / rel).exists()
    ]


# ----------------------------------------------------------------------
# CUTLINE: защита упаковки раскладки в блок (ред. 15)
# ----------------------------------------------------------------------

# Команда -BLOCK добавляет к переданному набору выделенные (grip) объекты:
# посторонние объекты уходят внутрь блока раскладки и пропадают из чертежа.
# Инвариант: перед -BLOCK предвыделение снимается и PICKFIRST выключается,
# после упаковки состав блока сверяется с составом раскладки.
CUTLINE_WRAP_BEFORE = [
    ("(n1-clear-pickfirst nil)", "снятие предвыделения перед -BLOCK"),
    ('(getvar "PICKFIRST")', "чтение PICKFIRST перед -BLOCK"),
    ('(list "PICKFIRST" 0)', "выключение PICKFIRST на время -BLOCK"),
    ("n1-ss-type-tally", "ожидаемый состав раскладки"),
    ("*n1-created*", "учёт созданных отрисовкой объектов"),
    ("(foreach ent *n1-created*", "набор для блока из своих объектов"),
    ("n1-scan-foreign-entities", "скан посторонних объектов базы"),
    ("n1-wrap-mark-begin", "своя undo-метка раскладки"),
]
CUTLINE_WRAP_AFTER = [
    ("n1-block-type-tally", "контроль состава блока после -BLOCK"),
    ("n1-tally-equal-p", "сверка состава блока с раскладкой"),
    ("n1-wrap-mark-end", "откат группы раскладки при захвате посторонних"),
]

# Отрисовка обязана идти через n1-mk (объект попадает в *n1-created*)
CUTLINE_GEOMETRY_ENTMAKE = [
    '(list (cons 0 "TEXT")',
    '(list (cons 0 "LINE")',
    '(list (cons 0 "LWPOLYLINE")',
    '(list\n        (cons 0 "SOLID")',
]

# Диагностика (скан цепочки БД, определение владельца, снятие состава блока)
# не имеет права уронить раскрой: все вызовы под vl-catch-all-apply,
# печать — через n1-safe-str (любой тип значения).
CUTLINE_DIAG_GUARDS = [
    ("(defun n1-safe-str", "n1-safe-str: приведение к строке"),
    ("(vl-catch-all-apply (function n1-scan-foreign-entities)", "скан под vl-catch-all-apply"),
    ("(vl-catch-all-apply (function n1-owner-name)", "владелец под vl-catch-all-apply"),
    ("(vl-catch-all-apply (function handent)", "handent под vl-catch-all-apply"),
    ("(vl-catch-all-apply (function n1-block-type-tally)", "состав блока под vl-catch-all-apply"),
    ("(vl-catch-all-apply (function entnext)", "обход базы под vl-catch-all-apply"),
]


def check_cutline_wrap_guard() -> list[str]:
    path = ROOT / "Extraction/cutline.lsp"
    if not path.exists():
        return []          # отдельная проверка сообщит об отсутствии файла
    text = read_text(path)
    marker = '"_.-BLOCK"'
    idx = text.find(marker)
    if idx < 0:
        return [f"{path.relative_to(ROOT)}: не найдена упаковка раскладки в блок ({marker})"]
    errors: list[str] = []
    for needle, what in CUTLINE_WRAP_BEFORE:
        if needle not in text[:idx]:
            errors.append(f"{path.relative_to(ROOT)}: защита упаковки в блок: нет '{what}' до -BLOCK")
    for needle, what in CUTLINE_WRAP_AFTER:
        if needle not in text[idx:]:
            errors.append(f"{path.relative_to(ROOT)}: защита упаковки в блок: нет '{what}' после -BLOCK")
    if "(sssetfirst nil nil)" not in text:
        errors.append(f"{path.relative_to(ROOT)}: защита упаковки в блок: нет (sssetfirst nil nil)")
    # Геометрия раскладки должна создаваться через n1-mk: иначе объект не попадёт
    # в *n1-created*, и набор для блока снова придётся брать обходом базы.
    tracked = text.count("(n1-mk (list") + text.count("(n1-mk\n")
    if tracked < 8:
        errors.append(f"{path.relative_to(ROOT)}: защита упаковки в блок: "
                      f"через n1-mk создаётся только {tracked} видов объектов (ожидалось >= 8)")
    for needle in CUTLINE_GEOMETRY_ENTMAKE:
        marker = needle.replace("\\n", "\n")
        if f"(entmake {marker}" in text or f"(entmake\n{marker}" in text:
            errors.append(f"{path.relative_to(ROOT)}: защита упаковки в блок: "
                          f"геометрия создаётся в обход n1-mk ({marker})")
    # Диагностика не должна ронять работу (ред. 17): без этих вызовов
    # сбой служебной печати снова убьёт весь раскрой.
    for needle, what in CUTLINE_DIAG_GUARDS:
        if needle not in text:
            errors.append(f"{path.relative_to(ROOT)}: защита упаковки в блок: "
                          f"диагностика без защиты — нет '{what}'")
    return errors



# --- CUTSHEET: упаковка карты в блок (ред. 25) -------------------------
# Набор для блока — только объекты, созданные отрисовкой (*cs-created* через
# cs-mk). Обход базы entnext затягивал ATTRIB/SEQEND динамических блоков:
# CopyObjects падал с «Недопустимый объект-владелец», а оригиналы деталей
# могли быть удалены как «оригиналы карты».
CUTSHEET_WRAP_BEFORE = [
    ("(defun cs-mk", "создание объектов через cs-mk"),
    ("*cs-created*", "учёт созданных объектов"),
    ("(defun cs-ss-from-created", "набор только из созданных объектов"),
    ("(defun cs-copyable-p", "фильтр несовместимых с блоком типов"),
    ("(defun cs-scan-foreign-entities", "диагностика обхода базы"),
    ("(defun cs-safe-str", "безопасная печать любого значения"),
    ("(defun cs-owner-name", "безопасное имя владельца"),
]
CUTSHEET_WRAP_AFTER = [
    ("(cs-block-type-tally blockName)", "контроль состава определения блока"),
    ("cs-tally-equal-p", "сверка состава блока с набором карты"),
    ("[CUTSHEET] В блоке объектов", "отчёт о составе блока"),
]
CUTSHEET_DIAG_GUARDS = [
    ("(vl-catch-all-apply 'cs-scan-foreign-entities", "скан под vl-catch-all-apply"),
    ("(vl-catch-all-apply 'entnext", "обход базы под vl-catch-all-apply"),
    ("(vl-catch-all-apply 'handent", "handent под vl-catch-all-apply"),
    ("(ssmemb ent ssNew)", "быстрая проверка принадлежности набору"),
]
# Обход базы для сбора набора — корень дефекта: в модуле его быть не должно
CUTSHEET_FORBIDDEN = [
    ("(setq ssNew (ssadd) ent (if lastEnt (entnext lastEnt) (entnext)))",
     "сбор набора обходом базы"),
    ("(while ent (ssadd ent ssNew) (setq ent (entnext ent)))",
     "сбор набора обходом базы"),
]

def check_cutsheet_wrap_guard() -> list[str]:
    """CUTSHEET ред. 25: карта упаковывается только из своих объектов."""
    path = ROOT / "Extraction/cutsheet.lsp"
    if not path.exists():
        return []          # отдельная проверка сообщит об отсутствии файла
    text = read_text(path)
    marker = "vla-CopyObjects"
    idx = text.find(marker)
    if idx < 0:
        return [f"{path.relative_to(ROOT)}: не найдена упаковка карты в блок ({marker})"]
    errors: list[str] = []
    for needle, what in CUTSHEET_WRAP_BEFORE:
        if needle not in text[:idx]:
            errors.append(f"{path.relative_to(ROOT)}: защита упаковки карты в блок: "
                          f"нет '{what}' до CopyObjects")
    for needle, what in CUTSHEET_WRAP_AFTER:
        if needle not in text[idx:]:
            errors.append(f"{path.relative_to(ROOT)}: защита упаковки карты в блок: "
                          f"нет '{what}' после CopyObjects")
    for needle, what in CUTSHEET_FORBIDDEN:
        if needle in text:
            errors.append(f"{path.relative_to(ROOT)}: защита упаковки карты в блок: "
                          f"вернулся {what} (посторонние объекты снова попадут в блок)")
    for needle, what in CUTSHEET_DIAG_GUARDS:
        if needle not in text:
            errors.append(f"{path.relative_to(ROOT)}: защита упаковки карты в блок: "
                          f"диагностика без защиты — нет '{what}'")
    if "(setq *cs-created* '())" not in text:
        errors.append(f"{path.relative_to(ROOT)}: защита упаковки карты в блок: "
                      f"нет сброса учёта созданных объектов на прогон")
    if "[CUTSHEET][STEP]" not in text or "[CUTSHEET][SCAN]" not in text:
        errors.append(f"{path.relative_to(ROOT)}: защита упаковки карты в блок: "
                      f"нет пошаговых меток [CUTSHEET][STEP]/[CUTSHEET][SCAN]")
    # Геометрия карты создаётся через cs-mk, иначе объект не попадёт в *cs-created*
    tracked = text.count("(cs-mk (list") + text.count("(cs-mk\n")
    if tracked < 6:
        errors.append(f"{path.relative_to(ROOT)}: защита упаковки карты в блок: "
                      f"через cs-mk создаётся только {tracked} видов объектов (ожидалось >= 6)")
    # Любой entmake вне cs-mk допустим только для табличных записей (STYLE/LAYER)
    pos = 0
    while True:
        i = text.find("(entmake", pos)
        if i < 0:
            break
        pos = i + 1
        window = text[i:i + 220]
        if "(entmake dxf)" in window:
            continue        # ядро cs-mk
        if '"STYLE"' in window or '"LAYER"' in window:
            continue        # табличные записи в блок не попадают
        errors.append(f"{path.relative_to(ROOT)}: защита упаковки карты в блок: "
                      f"геометрия создаётся в обход cs-mk "
                      f"(entmake без STYLE/LAYER: {window[:40]}...)")
    return errors




RELOAD_CHKLOAD_MUST = [
    ("(vl-catch-all-apply 'load (list ae-chk-path))",
     "перехвата ошибки загрузки tests\\chkparens.lsp (её сбой обрывает загрузку reload.lsp)"),
    ("(vl-catch-all-error-p *ae-reload-self*)",
     "проверки результата самообновления reload.lsp (в сессии молча остаются старые определения)"),
    ("(ae-reload-chkload-run fullpath)",
     "вызова поиска формы (CHKLOAD) при ошибке загрузки модуля"),
    ("(defun ae-reload-probe-file",
     "встроенного поиска формы (резерв, когда tests\\chkparens.lsp недоступен)"),
    ("(defun ae-reload-version-line",
     "функции строки сверки версий (её печатает верхний уровень reload.lsp)"),
    ("(defun ae-reload-probe-try",
     "встроенной пробы префиксов для поиска формы"),
    ("[RELOAD] сверка версий",
     "строки сверки версий файлов в логе"),
]

RELOAD_CHKLOAD_FORBIDDEN = [
    ("(= (type chk-load-find) 'SUBR)",
     "хрупкая проверка (= (type f) 'SUBR) без USUBR: вызов CHKLOAD может молча не сработать"),
    ("(load ae-chk-path)\n",
     "загрузка tests\\chkparens.lsp без перехвата ошибки"),
]


def check_reload_chkload_guard() -> list[str]:
    """RELOAD: диагностика сбоя загрузки не должна теряться молча."""
    path = ROOT / "reload.lsp"
    if not path.exists():
        return []
    text = read_text(path)
    errors: list[str] = []
    for needle, what in RELOAD_CHKLOAD_MUST:
        if needle not in text:
            errors.append(f"{path.relative_to(ROOT)}: диагностика загрузки: нет {what}")
    for needle, what in RELOAD_CHKLOAD_FORBIDDEN:
        if needle in text:
            errors.append(f"{path.relative_to(ROOT)}: диагностика загрузки: {what}")
    return errors


RELOAD_PLUGINS_MUST = [
    ("(defun ae-reload-plugin-registry (",
     "чтения реестра плагинов [plugins] из settings.ini"),
    ("(defun ae-reload-plugin-resolve (",
     "резолвера пути плагина (путь из реестра, без отката на другой путь)"),
    ("(ae-reload-plugin-resolve (car plugin-rec) (cadr plugin-rec))",
     "вызова резолвера в секции PLUGINS"),
    ("НЕ НАЙДЕН по пути",
     "строки лога «НЕ НАЙДЕН по пути» для плагина без файла по пути реестра"),
    ("(ae-reload-plugin-conflicts plugin-res plugin-names plugin-taken)",
     "проверки перекрытия имён функций плагина: имена проекта и предыдущих плагинов"),
    ("(append (mapcar 'strcase (ae-reload-plugin-defuns (strcat common f)))",
     "сбора имён функций проекта для проверки перекрытия"),
    ("(if (> (length plugin-items) 0)",
     "условия секции PLUGINS: пустой реестр обязан пропускаться молча"),
    ("(setq plugin-missing (1+ plugin-missing))",
     "счётчика плагинов без файла (для итогового предупреждения)"),
    ("(foreach it plugin-resolved",
     "учёта найденных плагинов в маркере редакции"),
    ("(defun ae-banner-rev (",
     "извлечения редакции модуля для таблицы команд RELOAD"),
    ('(ae-banner-pad "РЕД." w3)',
     "столбца «РЕД.» в таблице команд RELOAD"),
]

RELOAD_PLUGINS_FORBIDDEN = [
    ("*ae-reload-plugin-files*",
     "жёсткого списка имён плагинов: имена берутся из реестра"),
    ("*ae-reload-plugin-dev-dirs*",
     "каталогов разработки: поиск по каталогам не используется"),
    ("ae-reload-plugin-dev-path",
     "поиска в каталогах разработки: путь берётся из реестра"),
    ("ae-reload-plugin-remember",
     "запоминания имён плагина между прогонами: проверка перекрытия идёт по именам проекта"),
    ("*ae-reload-plugin-own*",
     "списка имён плагинов между прогонами: он скрывал перекрытие при повторном RELOAD"),
    ("plugin-cnt",
     "переменной, которой уже нет в c:RELOAD: её чтение роняет итог прогона (numberp: nil)"),
    ("PlotFrame",
     "имени конкретного плагина в коде RELOAD: программа не знает содержимого плагинов"),
]

PLUGIN_LOCALS = ("plugin-items", "plugin-seen", "plugin-rec", "plugin-name",
                 "plugin-path", "plugin-res", "plugin-names", "plugin-missing",
                 "plugin-resolved", "plugin-taken")


POWERSHELL_SUFFIXES = (".ps1", ".psm1")

# Символы, которые PowerShell принимает за кавычки. Если такой символ попал
# в .ps1 из чужой кодировки, строка рвётся посреди кода и скрипт не разбирается.
POWERSHELL_SMART_QUOTES = "\u2018\u2019\u201a\u201c\u201d\u201e"


def check_powershell_encoding() -> list[str]:
    """Скрипты PowerShell обязаны быть ASCII или UTF-8 с BOM.

    Windows PowerShell 5.1 читает .ps1 без BOM как ANSI (в русской локали
    cp1251). Кириллица в UTF-8 превращается в мусор, а из байтов рождаются
    типографские кавычки „ ’ “ ”, которые PowerShell считает разделителями
    строк: `Write-Host "SKIP: $t (файл не найден)"` разбирался как обрыв
    строки, и scripts\test.ps1 падал с «Отсутствует закрывающий знак }»
    ещё до первой проверки. BOM снимает вопрос: PS 5.1 и PS 7 читают файл
    как UTF-8.
    """
    errors: list[str] = []
    for path in sorted(ROOT.rglob("*")):
        if not path.is_file() or path.suffix.lower() not in POWERSHELL_SUFFIXES:
            continue
        if ".git" in path.parts:
            continue
        raw = path.read_bytes()
        rel = _rel(path)
        try:
            raw.decode("ascii")
            continue  # чистый ASCII читается одинаково в любой кодировке
        except UnicodeDecodeError:
            pass
        if not raw.startswith(b"\xef\xbb\xbf"):
            errors.append(
                f"{rel}: нет BOM UTF-8 - Windows PowerShell 5.1 прочитает "
                f"кириллицу как ANSI и скрипт не разберётся")
            continue
        try:
            text = raw.decode("utf-8-sig")
        except UnicodeDecodeError as exc:
            errors.append(f"{rel}: не UTF-8: {exc}")
            continue
        bad = sorted({c for c in text if c in POWERSHELL_SMART_QUOTES})
        if bad:
            errors.append(
                f"{rel}: типографские кавычки {''.join(bad)!r} - PowerShell "
                f"принимает их за разделители строк")
    return errors


def check_subsystem_numbering() -> list[str]:
    """Подсистема: нумерация DETAIL-таблицы начинается с 1.

    Нумерация сквозная по строкам: штучные позиции занимают номера
    1..N, первая мерная группа получает N+1 (13 штучных -> 14, 15, 16).
    Пока номер группы стартовал с зашитой единицы и инкрементировался
    ДО первой группы, пустая группа 1 съедала номер: при отсутствии
    штучных таблица начиналась с «2.1», а при их наличии — скакала
    «13 -> 2.1». Проверяется: штучные пишутся в группу 1 явно, стартовый
    номер берётся из их количества, и «простой номер» включается по
    признаку штучных, а не по номеру группы.
    """
    path = ROOT / "Extraction" / "subsystem.lsp"
    if not path.exists():
        return [f"{_rel(path)}: нет модуля Подсистемы"]
    text = read_text(path)
    rel = _rel(path)
    errors: list[str] = []

    flat = _defun_body(text, "su-build-flat-items")
    if flat is None:
        errors.append(f"{rel}: не найдена форма (defun su-build-flat-items ...)")
    else:
        must = [
            ("(cons 'data (cons 1 rec))",
             "явной записи штучных в группу 1"),
            ("(setq groupIndex piecesCount)",
             "продолжения нумерации с числа штучных позиций"),
        ]
        for needle, what in must:
            if needle not in flat:
                errors.append(f"{rel}: нумерация Подсистемы: нет {what}")
        # (setq groupIndex (1+ groupIndex)) внутри цикла групп — норма:
        # это инкремент перед очередной группой. Проверяется стартовое
        # присваивание, а оно обязано быть piecesCount.
        if "(setq groupIndex 1)" in flat:
            errors.append(
                f"{rel}: нумерация Подсистемы: номер группы жёстко равен 1 - "
                f"при 13 штучных позициях мерные группы начнутся с 2, "
                f"и в таблице будет скачок «13 -> 2.1»")

    fmt = _defun_body(text, "su-item-number-text")
    if fmt is None:
        errors.append(f"{rel}: не найдена форма (defun su-item-number-text ...)")
    elif "(and hasPieces (= groupIdx 1))" not in fmt:
        errors.append(
            f"{rel}: нумерация Подсистемы: простой номер не привязан к "
            f"признаку штучных (нужен (and hasPieces (= groupIdx 1)))")

    detail = _defun_body(text, "subsystem-create-table-detail")
    if detail is None:
        errors.append(f"{rel}: не найдена форма (defun subsystem-create-table-detail ...)")
    else:
        if "(su-item-number-text groupIdx rowInGroup hasPieces)" not in detail:
            errors.append(
                f"{rel}: нумерация Подсистемы: разметка не использует общий "
                f"формат номера su-item-number-text")
        if "hasPieces" not in detail.split("/", 1)[0] + detail.split("/", 1)[-1][:400]:
            errors.append(
                f"{rel}: нумерация Подсистемы: hasPieces не объявлена локальной")

        # Ширина столбца «№»: фиксированные 15 мм ломали «11.10» и «12.13»
        # на перенос внутри ячейки.
        if "(vla-SetColumnWidth tbl 0 15.0)" in detail:
            errors.append(
                f"{rel}: ширина столбца № зашита 15 мм - номера вида «11.10» "
                f"переносятся внутри ячейки")
        if "(vla-SetColumnWidth tbl 0 col0Width)" not in detail:
            errors.append(
                f"{rel}: ширина столбца № не берётся из расчёта col0Width")
        if "su-number-column-width" not in detail:
            errors.append(
                f"{rel}: нет расчёта ширины столбца № через "
                f"su-number-column-width")
    return errors


SETTINGS_SAVE_MUST = [
    ("(and (member task '(FASONKA SUBSYSTEM CLADDING VITRAZH ZAPOLNENIE))",
     "своей проверки пустого поля слоя таблицы"),
    ('(= (ae-settings-trim (get_tile "edt_table_layer")) "")',
     "проверки пустого поля edt_table_layer"),
    ("(and (member task '(CUTLINE CUTSHEET))",
     "своей проверки пустого поля слоя вставки"),
    ('(= (ae-settings-trim (get_tile "edt_insert_layer")) "")',
     "проверки пустого поля edt_insert_layer"),
]

SETTINGS_SAVE_FORBIDDEN = [
    ('(or (= (ae-settings-trim (get_tile "edt_table_layer")) "")',
     "общей проверки на два поля: поле чужой задачи всегда пустое, и "
     "заданный слой стирается в CURRENT"),
]


def check_settings_save_guard() -> list[str]:
    """Окно настроек: пустое поле одной задачи не должно стирать поле другой.

    Диалог очищает неактивные поля (у ФАСОНКИ поле слоя вставки пустое, у
    CUTLINE - поле слоя таблицы). Общая проверка `(or пусто-таблица
    пусто-вставка)` из-за этого срабатывала ВСЕГДА и переписывала заданный
    слой в CURRENT: слой таблицы не сохранялся ни у одной задачи, слой
    вставки - у обоих раскроев. Пользователь видел «задаю, не запоминает».
    """
    path = ROOT / "Extraction" / "settings.lsp"
    if not path.exists():
        return [f"{_rel(path)}: нет файла окна настроек"]
    body = _defun_body(read_text(path), "ae-settings-ui-save-task")
    if body is None:
        return [f"{_rel(path)}: не найдена форма (defun ae-settings-ui-save-task ...)"]
    errors: list[str] = []
    rel = _rel(path)
    for needle, what in SETTINGS_SAVE_MUST:
        if needle not in body:
            errors.append(f"{rel}: сохранение настроек: нет {what}")
    for needle, what in SETTINGS_SAVE_FORBIDDEN:
        if needle in body:
            errors.append(f"{rel}: сохранение настроек: осталась {what}")
    return errors


def check_test_console_encoding() -> list[str]:
    """Тексты тестов обязаны печататься в консоли с cp1251.

    Символ вне ANSI-кодировки (U+2212 MINUS SIGN в названии проверки)
    ронял прогон: Python кодировал вывод в cp1251 и падал с
    UnicodeEncodeError посреди сценария, поэтому «хвост» теста не
    выполнялся вовсе. Теперь вывод дополнительно защищён
    (PYTHONIOENCODING в scripts\test.ps1 и sys.stdout.reconfigure(errors="replace")
    в самих тестах), но экзотический символ всё равно превратится в '?',
    то есть сообщение станет бесполезным. Проще не пускать его в исходники.
    """
    errors: list[str] = []
    for path in sorted((ROOT / "tests").glob("*.py")):
        text = path.read_text(encoding="utf-8")
        bad = sorted({c for c in text if _not_cp1251(c)})
        if bad:
            shown = ", ".join(f"U+{ord(c):04X}" for c in bad)
            errors.append(
                f"{_rel(path)}: символы вне cp1251 ({shown}) - консоль "
                f"напечатает '?' или упадёт UnicodeEncodeError")
    return errors


def _not_cp1251(ch: str) -> bool:
    try:
        ch.encode("cp1251")
        return False
    except UnicodeEncodeError:
        return True


BLOCK_LENGTH_MUST = [
    ("common/settings-utils.lsp", "(defun ae-settings-task-length-props",
     "чтения масок длины из настроек задачи"),
    ("common/settings-utils.lsp", '"input.block.length"',
     "ключа настроек input.block.length"),
    ("common/select-utils.lsp", "(defun su-get-length (obj candidates",
     "передачи масок длины в su-get-length"),
    ("common/select-utils.lsp", "(defun su-get-length-by-masks",
     "поиска длины по маскам свойств"),
    ("common/select-utils.lsp", '(vl-string-search "ШИРИНА" pname)',
     "исключения ШИРИНЫ из масок длины"),
    ("common/select-utils.lsp", "(defun su-prop-name-width-height-p",
     "общего исключения ШИРИНЫ и ВЫСОТЫ для обоих проходов"),
    ("Extraction/fasonka.lsp", "(ae-settings-task-length-props 'FASONKA)",
     "масок длины ФАСОНКИ из настроек"),
    ("Extraction/subsystem.lsp", "(ae-settings-task-length-props 'SUBSYSTEM)",
     "масок длины ПОДСИСТЕМЫ из настроек"),
    ("Extraction/cutline.lsp", "(ae-settings-task-length-props 'CUTLINE)",
     "масок длины CUTLINE из настроек"),
    ("Extraction/settings.lsp", '(get_tile "edt_block_length")',
     "поля «Свойства длины» в окне настроек"),
    ("Extraction/settings.dcl", 'key = "edt_block_length"',
     "поля edt_block_length в settings.dcl"),
]


def check_block_length_setting() -> list[str]:
    """Имя свойства длины — настройка задачи, а не хардкод в модулях.

    Маски длины обязаны читаться из task.<TASK> / input.block.length: иначе
    у блока с другим именем свойства («LENGTH», «Длина изделия») пропадает
    погонаж, а отчёт молча теряет позиции.
    """
    errors: list[str] = []
    for rel, needle, what in BLOCK_LENGTH_MUST:
        path = ROOT / rel
        if not path.exists():
            errors.append(f"{rel}: нет файла для проверки масок длины")
            continue
        if needle not in read_text(path):
            errors.append(f"{rel}: маски длины: нет {what}")
    return errors


SETTINGS_PLUGINS_MUST = [
    ("(ae-settings-set-plugins *AE-SETTINGS-UI-PLUGINS*)",
     "переноса буфера плагинов в память перед записью settings.ini (иначе «Сохранить» не пишет реестр)"),
    ("(ae-settings-write))",
     "записи settings.ini в ae-settings-ui-save"),
    ('(new_dialog "plugins_dialog" dcl-id)',
     "открытия диалога плагинов"),
]


def check_settings_plugins_guard() -> list[str]:
    """Окно настроек: буфер плагинов обязан попадать в settings.ini при «Сохранить»."""
    path = ROOT / "Extraction" / "settings.lsp"
    if not path.exists():
        return []
    text = read_text(path)
    rel = path.relative_to(ROOT)
    errors: list[str] = []
    for needle, what in SETTINGS_PLUGINS_MUST:
        if needle not in text:
            errors.append(f"{rel}: плагины в настройках: нет {what}")
    return errors


def check_reload_plugins_guard() -> list[str]:
    """RELOAD: раздел плагинов молча не работает и не течёт в глобальные."""
    path = ROOT / "reload.lsp"
    if not path.exists():
        return []
    text = read_text(path)
    rel = path.relative_to(ROOT)
    errors: list[str] = []
    for needle, what in RELOAD_PLUGINS_MUST:
        if needle not in text:
            errors.append(f"{rel}: раздел плагинов: нет {what}")
    for needle, what in RELOAD_PLUGINS_FORBIDDEN:
        if needle in text:
            errors.append(f"{rel}: раздел плагинов: остался {what}")

    # Переменные секции обязаны быть локальными для c:RELOAD: иначе они
    # становятся глобальными и переживают прогон (класс дефекта, который
    # уже давал ложные итоги).
    try:
        forms = lisp_parse(text)
    except LispParseError as exc:
        return errors + [f"{rel}: раздел плагинов: разбор форм - {exc}"]
    locals_found = None
    for node in forms:
        if node[0] != "list" or len(node[1]) < 3:
            continue
        head, name, args = node[1][0], node[1][1], node[1][2]
        if (head[0] == "atom" and head[1].lower() == "defun"
                and name[0] == "atom" and name[1].lower() == "c:reload"
                and args[0] == "list"):
            locals_found = [a[1].lower() for a in args[1][1:] if a[0] == "atom"]
            break
    if locals_found is None:
        errors.append(f"{rel}: раздел плагинов: не найдена форма (defun c:RELOAD ...)")
    else:
        for var in PLUGIN_LOCALS:
            if var not in locals_found:
                errors.append(
                    f"{rel}: раздел плагинов: '{var}' не в списке локальных "
                    f"переменных c:RELOAD (утечёт в глобальные)")
    return errors


def check_crlf_tolerance() -> list[str]:
    """CRLF в .lsp - норма для Windows-копии, одиночный CR - дефект.

    Windows-копия репозитория отдаёт файлы с `\r\n` (core.autocrlf=true),
    и проверка лексики валила КАЖДЫЙ .lsp: «управляющий символ (код 13)»
    на всех строках, где сканер доходил до конца (строки с `;`-комментарием
    обрывались раньше и в список не попадали). На Linux-клоне дефекта не
    видно: там LF. Тест держит оба конца: CRLF проходит, а одиночный `\r`
    внутри строки кода по-прежнему ловится.
    """
    import tempfile

    errors: list[str] = []
    with tempfile.TemporaryDirectory() as tmp:
        crlf = Path(tmp) / "crlf.lsp"
        crlf.write_bytes(
            ("; комментарий\r\n"
             "(defun c:CRLFTEST ()\r\n"
             "  (princ \"ok\"))\r\n").encode("cp1251"))
        got = check_lisp_lexical([crlf])
        if got:
            errors.append(
                f"crlf-tolerance: CRLF в .lsp считается дефектом: {got[0]}")

        lone = Path(tmp) / "lone.lsp"
        lone.write_bytes(b'(princ "a")\r(princ "b")\n')
        if not check_lisp_lexical([lone]):
            errors.append(
                "crlf-tolerance: одиночный CR внутри строки кода перестал "
                "ловиться (нормализация задела не только CRLF)")
    return errors


def check_lisp_lexical(files=None) -> list[str]:
    """Причины «синтаксической ошибки» при целом балансе скобок.

    AutoCAD читает .lsp как ANSI: BOM, управляющие байты и не-ANSI символы
    вне строк/комментариев (кроме имён команд c:ИМЯ) валят загрузку файла,
    а баланс скобок при этом остаётся нулевым.
    """
    errors: list[str] = []
    for path in (files if files is not None else lisp_files()):
        raw = path.read_bytes()
        rel = _rel(path)
        if raw.startswith(b"\xef\xbb\xbf"):
            errors.append(f"{rel}: BOM (UTF-8) в начале файла — AutoCAD читает .lsp как ANSI")
            raw = raw[3:]
        try:
            text = raw.decode("cp1251")
        except UnicodeDecodeError as exc:
            errors.append(f"{rel}: байт 0x{raw[exc.start]:02X} не читается как ANSI (cp1251)")
            continue
        # Рабочая копия на Windows приходит с CRLF (core.autocrlf=true):
        # `\r` в конце строки - это перевод строки, а не управляющий символ
        # внутри кода. Нормализуем ТОЛЬКО пару CRLF; одиночный `\r` остаётся
        # и по-прежнему считается дефектом.
        text = text.replace("\r\n", "\n")
        for ln, line in enumerate(text.split("\n"), 1):
            i = 0
            in_str = False
            while i < len(line):
                c = line[i]
                if in_str:
                    if c == "\\":
                        i += 2
                        continue
                    if c == '"':
                        in_str = False
                elif c == '"':
                    in_str = True
                elif c == ";":
                    break
                elif ord(c) < 32 and c != "\t":
                    errors.append(f"{rel}: строка {ln}: управляющий символ (код {ord(c)})")
                    break
                elif ord(c) > 127:
                    start = i
                    while start > 0 and line[start - 1] not in ' \t()";':
                        start -= 1
                    token = line[start:i + 1]
                    if not token.upper().startswith("C:"):
                        errors.append(f"{rel}: строка {ln}: не-ANSI символ вне строки/комментария "
                                      f"(слово '{token}')")
                        break
                i += 1
            if in_str:
                errors.append(f"{rel}: строка {ln}: строковая константа не закрыта")
    return errors

# ----------------------------------------------------------------------
# Спецформы: «синтаксическая ошибка» при нулевом балансе скобок
# ----------------------------------------------------------------------

# Разбор .lsp в s-выражения. Нужен потому, что баланс скобок НЕ ловит
# смещённую закрывающую скобку: (if c (progn ...)) (setq ...) (princ ...)
# при «уехавшей» скобке превращается в (if c b1 b2 b3 b4) — баланс нулевой,
# а AutoLISP при вычислении defun отвечает «синтаксическая ошибка» и
# бросает загрузку ВСЕГО файла. Так были потеряны cutsheet.lsp (ред. 25)
# и tests/chkparens.lsp.

class LispParseError(Exception):
    pass


def lisp_tokens(text: str):
    """Токены .lsp: ( ) ' строка атом. Комментарии отбрасываются."""
    i, line, out = 0, 1, []
    n = len(text)
    while i < n:
        c = text[i]
        if c == "\n":
            line += 1
            i += 1
            continue
        if c in " \t\r":
            i += 1
            continue
        if c == ";":
            while i < n and text[i] != "\n":
                i += 1
            continue
        if c in "()'":
            out.append((c, c, line))
            i += 1
            continue
        if c == '"':
            j = i + 1
            while j < n:
                if text[j] == "\\":
                    j += 2
                    continue
                if text[j] == '"':
                    break
                if text[j] == "\n":
                    line += 1
                j += 1
            out.append(("str", text[i + 1:j], line))
            i = j + 1
            continue
        j = i
        while j < n and text[j] not in " \t\r\n()';\"":
            j += 1
        out.append(("atom", text[i:j], line))
        i = j
    return out


def lisp_parse(text: str):
    """Список верхнеуровневых форм. Узел: (kind, value, line)."""
    toks = lisp_tokens(text)
    pos = 0

    def read():
        nonlocal pos
        if pos >= len(toks):
            raise LispParseError("неожиданный конец файла")
        kind, val, line = toks[pos]
        pos += 1
        if kind == "(":
            items = []
            while True:
                if pos >= len(toks):
                    raise LispParseError(f"форма со строки {line} не закрыта")
                if toks[pos][0] == ")":
                    pos += 1
                    return ("list", items, line)
                items.append(read())
        if kind == ")":
            raise LispParseError(f"лишняя ')' в строке {line}")
        if kind == "'":
            return ("quote", [read()], line)
        return (kind, val, line)

    forms = []
    while pos < len(toks):
        forms.append(read())
    return forms


def _is_number(token: str) -> bool:
    try:
        float(token)
        return True
    except ValueError:
        return False


def _check_arglist(items, head: str, line: int, out: list[str]) -> None:
    """Список аргументов defun/lambda: только символы, без дублей и одна '/'."""
    seen: list[str] = []
    slashes = 0
    for a in items:
        if a[0] != "atom":
            out.append(f"строка {a[2]}: {head} — аргумент не символ")
            continue
        name = a[1]
        if name == "/":
            slashes += 1
            continue
        if _is_number(name):
            out.append(f"строка {a[2]}: {head} — аргумент-число '{name}'")
        if name.upper() in ("T", "NIL", "PI"):
            out.append(f"строка {a[2]}: {head} — зарезервированный аргумент '{name}'")
        if name.upper() in seen:
            out.append(f"строка {a[2]}: {head} — дубль аргумента '{name}'")
        seen.append(name.upper())
    if slashes > 1:
        out.append(f"строка {line}: {head} — '/' в списке аргументов {slashes} раза")


def _walk_special_forms(node, out: list[str]) -> None:
    if node[0] == "quote":
        for child in node[1]:
            _walk_special_forms(child, out)
        return
    if node[0] != "list":
        return
    items, line = node[1], node[2]
    head = items[0][1].upper() if items and items[0][0] == "atom" else None
    args = items[1:]

    if head in ("DEFUN", "DEFUN-Q"):
        if len(items) < 3 or items[2][0] != "list":
            out.append(f"строка {line}: {head} — нет списка аргументов")
        else:
            _check_arglist(items[2][1], head, line, out)
    elif head == "LAMBDA":
        if len(items) < 2 or items[1][0] != "list":
            out.append(f"строка {line}: LAMBDA — нет списка аргументов")
        else:
            _check_arglist(items[1][1], "LAMBDA", line, out)
    elif head == "IF":
        if not 2 <= len(args) <= 3:
            out.append(f"строка {line}: IF — аргументов {len(args)}, допустимо 2 или 3 "
                       f"(лишние операторы после ветви else = смещённая ')')")
    elif head == "SETQ":
        if not args or len(args) % 2 != 0:
            out.append(f"строка {line}: SETQ — аргументов {len(args)}, нужно чётное число")
        for k in range(0, len(args) - 1, 2):
            target = args[k]
            if target[0] != "atom":
                out.append(f"строка {target[2]}: SETQ — цель присваивания не символ")
            elif _is_number(target[1]) or target[1].upper() in ("T", "NIL", "PI"):
                out.append(f"строка {target[2]}: SETQ — недопустимая цель '{target[1]}'")
    elif head == "FOREACH":
        if len(args) < 2:
            out.append(f"строка {line}: FOREACH — аргументов {len(args)}, нужно не меньше 2")
        elif args[0][0] != "atom":
            out.append(f"строка {line}: FOREACH — переменная цикла не символ")
    elif head == "COND":
        for clause in args:
            if clause[0] != "list":
                out.append(f"строка {clause[2]}: COND — ветвь не список")
    elif head == "WHILE":
        if not args:
            out.append(f"строка {line}: WHILE — нет условия")

    for child in items:
        _walk_special_forms(child, out)


def check_special_forms(files=None) -> list[str]:
    """Спецформы AutoLISP, которые валят загрузку при нулевом балансе скобок.

    AutoLISP проверяет тело функции при вычислении defun: (if a b c d),
    нечётный setq, кривой список аргументов defun/lambda дают
    «синтаксическая ошибка» и обрывают загрузку всего файла, а счётчик
    скобок при этом сходится. Именно так молчали chkparens.lsp и
    cutsheet.lsp ред. 25 (прогоны RELOAD №9-№13).
    """
    errors: list[str] = []
    for path in (files if files is not None else all_lisp_files()):
        rel = _rel(path)
        try:
            forms = lisp_parse(read_text(path))
        except LispParseError as exc:
            errors.append(f"{rel}: разбор форм: {exc}")
            continue
        found: list[str] = []
        for form in forms:
            _walk_special_forms(form, found)
        for msg in found:
            errors.append(f"{rel}: {msg} — AutoLISP: «синтаксическая ошибка» при загрузке")
    return errors


def check_reload_version_line() -> list[str]:
    """Сверка версий должна печататься с ВЕРХНЕГО уровня reload.lsp.

    Тело c:RELOAD выполняется в редакции, загруженной ДО самообновления:
    если в сессии осталось старое c:RELOAD, печать изнутри тела молча
    пропадает (прогон RELOAD #21). Печать на верхнем уровне срабатывает
    при каждом чтении файла с диска.
    """
    path = ROOT / "reload.lsp"
    if not path.exists():
        return []
    try:
        forms = lisp_parse(read_text(path))
    except LispParseError as exc:
        return [f"reload.lsp: разбор форм: {exc}"]

    def calls(node) -> bool:
        if node[0] == "atom":
            return node[1].lower() == "ae-reload-version-line"
        if node[0] in ("list", "quote"):
            return any(calls(c) for c in node[1])
        return False

    for form in forms:
        if form[0] != "list" or not form[1]:
            continue
        head = form[1][0][1].upper() if form[1][0][0] == "atom" else ""
        if head.startswith("DEFUN"):
            continue
        if calls(form):
            return []
    return ["reload.lsp: сверка версий не вызывается с верхнего уровня "
            "(печать только из тела c:RELOAD теряется при старых определениях в сессии)"]


def check_defun_duplicates_in_file(files=None) -> list[str]:
    """Один и тот же defun объявлен в файле дважды.

    Дубль переопределяет первую редакцию молча: сканеры баланса и
    спецформ его не видят. Проверяются все .lsp, включая reload.lsp и
    tests/*.lsp, которые в общий поиск дубликатов не входят.
    """
    errors: list[str] = []
    # Плагины реестра проверяет общий поиск дублей (check_duplicates):
    # там seen глобальный по всем файлам, поэтому дубль внутри плагина
    # виден и без этой группы. Иначе один дефект печатался дважды -
    # по-английски из общего поиска и по-русски отсюда.
    plugin_paths = {str(p).lower() for p in plugin_files()}
    for path in (files if files is not None else all_lisp_files()):
        if str(path).lower() in plugin_paths:
            continue
        seen: dict[str, int] = {}
        for name, line in find_defuns(path):
            key = name.upper()
            # *error* объявляется локально в каждой команде — норма.
            if name.lower() in {w.lower() for w in WHITELIST_DEFUN}:
                continue
            if key in seen:
                errors.append(f"{_rel(path)}: defun {name} объявлен дважды "
                              f"(строки {seen[key]} и {line})")
            else:
                seen[key] = line
    return errors


# ----------------------------------------------------------------------
# Арность вызовов
# ----------------------------------------------------------------------

ARITY_SKIP_HEADS = ("defun", "lambda")


def lisp_signatures(files) -> dict[str, set]:
    """Имя defun (строчными) -> набор допустимых чисел параметров."""
    sig: dict[str, set] = {}
    for path in files:
        try:
            forms = lisp_parse(read_text(path))
        except (OSError, LispParseError):
            continue
        for node in forms:
            if node[0] != "list" or len(node[1]) < 3:
                continue
            head, name, args = node[1][0], node[1][1], node[1][2]
            if not (head[0] == "atom" and head[1].lower() == "defun"
                    and name[0] == "atom" and args[0] == "list"):
                continue
            count = 0
            for a in args[1]:
                if a[0] == "atom" and a[1] == "/":
                    break
                if a[0] == "atom":
                    count += 1
            sig.setdefault(name[1].lower(), set()).add(count)
    return sig


def _walk_calls(node, cb) -> None:
    """Обход формы: cb(голова строчными, число аргументов, строка).

    Цитаты не обходятся: '(...) - данные, а не вызов. У defun и lambda
    пропускаются имя и список параметров, иначе символы параметров
    выглядели бы вызовами.
    """
    if node[0] != "list":
        return
    items = node[1]
    head = items[0] if items else None
    head_name = head[1].lower() if head and head[0] == "atom" else None
    start = 3 if (head_name in ARITY_SKIP_HEADS and len(items) > 2) else 1
    for child in items[start:]:
        if child[0] != "quote":
            _walk_calls(child, cb)
    if head_name:
        cb(head_name, len(items) - 1, node[2])


def check_call_arity(files=None) -> list[str]:
    """Число аргументов в вызове совпадает с числом параметров defun.

    У пользовательской функции AutoLISP нет необязательных аргументов:
    вызов с меньшим числом аргументов, чем параметров, - ошибка
    «слишком мало аргументов», и она рвёт команду целиком. Именно так
    RELOAD падал сразу после «--- COMMON ---», когда у
    ae-reload-load-file появился второй параметр note, а два старых
    вызова остались с одним аргументом (коммит 1dc99a1). Статически это
    не видно: скобки сбалансированы, спецформы на месте.
    """
    paths = (files if files is not None
             else list(all_lisp_files()))
    sig = lisp_signatures(paths)
    errors: list[str] = []
    for path in paths:
        try:
            forms = lisp_parse(read_text(path))
        except (OSError, LispParseError):
            continue
        rel = _rel(path)

        def cb(name, argc, line, rel=rel):
            if name in ARITY_SKIP_HEADS:
                return
            allowed = sig.get(name)
            if not allowed or argc in allowed:
                return
            want = "/".join(str(a) for a in sorted(allowed))
            kind = ("слишком мало аргументов" if argc < min(allowed)
                    else "слишком много аргументов")
            errors.append(
                f"{rel}: строка {line}: {name} - аргументов {argc}, "
                f"параметров {want}: в AutoCAD будет «{kind}»")

        for form in forms:
            _walk_calls(form, cb)
    return errors


CHKPARENS_PLUGINS_MUST = [
    ("(foreach rec (ae-reload-plugin-registry)",
     "обхода реестра плагинов в CHKALL - принятые плагины остались бы без проверки"),
    ("(member (type ae-reload-plugin-registry) '(SUBR USUBR))",
     "стражи типа реестра: CHKALL обязан работать и без reload.lsp"),
    ("(member (type ae-reload-plugin-resolve) '(SUBR USUBR))",
     "стражи типа резолвера: CHKALL обязан работать и без reload.lsp"),
    ("(setq p (ae-reload-plugin-resolve (car rec) (cadr rec)))",
     "резолвера ae-reload-plugin-resolve: CHKALL обязан брать тот же путь, что и RELOAD"),
    ("(setq total (+ total (chk-parens-scan p)))",
     "самого сканирования плагина (иголка с setq, потому что голое "
     "(chk-parens-scan p) есть ещё в c:CHKFILE)"),
]

CHKPARENS_PLUGINS_FORBIDDEN = [
    ('(strcat root "\\\\Plugins\\\\")',
     "обхода папки Plugins\\ по файлам: плагины берутся только из реестра"),
    ("ae-reload-plugin-dev-path",
     "поиска в каталогах разработки: путь берётся из реестра"),
    ("*ae-reload-plugin-files*",
     "жёсткого списка плагинов: имена берутся из реестра"),
]

CHKALL_LOCALS = ("root", "d", "files", "f", "total", "p", "rec")


def check_chkparens_plugins_guard() -> list[str]:
    r"""CHKALL достаёт плагины: и Plugins\, и каталоги разработки.

    Без этого обхода у плагина не остаётся ни одной проверки внутри
    AutoCAD: RELOAD сканирует скобки только ПОСЛЕ ошибки загрузки
    (post-диагностика в ae-reload-load-file), а не до неё.
    """
    path = TEST_DIR / "chkparens.lsp"
    if not path.exists():
        return []
    text = read_text(path)
    rel = path.relative_to(ROOT)
    errors: list[str] = []
    for needle, what in CHKPARENS_PLUGINS_MUST:
        if needle not in text:
            errors.append(f"{rel}: CHKALL и плагины: нет {what}")
    for needle, what in CHKPARENS_PLUGINS_FORBIDDEN:
        if needle in text:
            errors.append(f"{rel}: CHKALL и плагины: остался {what}")

    # Переменные обхода обязаны быть локальными для c:CHKALL: иначе они
    # глобальные и переживают команду.
    try:
        forms = lisp_parse(text)
    except LispParseError as exc:
        return errors + [f"{rel}: CHKALL и плагины: разбор форм - {exc}"]
    locals_found = None
    for node in forms:
        if node[0] != "list" or len(node[1]) < 3:
            continue
        head, name, args = node[1][0], node[1][1], node[1][2]
        if (head[0] == "atom" and head[1].lower() == "defun"
                and name[0] == "atom" and name[1].lower() == "c:chkall"
                and args[0] == "list"):
            locals_found = [a[1].lower() for a in args[1][1:] if a[0] == "atom"]
            break
    if locals_found is None:
        errors.append(f"{rel}: CHKALL и плагины: не найдена форма (defun c:CHKALL ...)")
    else:
        for var in CHKALL_LOCALS:
            if var not in locals_found:
                errors.append(
                    f"{rel}: CHKALL и плагины: '{var}' не среди локальных "
                    f"переменных c:CHKALL")
    return errors


XLS_MARKS_MUST = {
    "Extraction/cutsheet.lsp": [
        ('(defun cs-xls-has-marks (',
         'помощника cs-xls-has-marks: без него колонка «Марка» появится и в прогонах без марок'),
        ('(setq hasMarks (cs-xls-has-marks sheets oversized))',
         'расчёта hasMarks в писателях XLS/CSV'),
        ('(if hasMarks (eu-cell f "H" "String" "Марка" ""))',
         'заголовка «Марка» в XLS листа'),
        ('(if hasMarks (eu-cell f "D" "String" (if mk mk "") ""))',
         'ячейки марки детали в XLS листа'),
        ('(if hasMarks (eu-cell f "D" "String" (if (car rw) (car rw) "") ""))',
         'ячейки марки в перечне изделий XLS листа'),
        ('(if hasMarks (eu-column f "150" "0"))',
         'колонки под марку в листе «Итоги»'),
        ('(foreach rw rowsList',
         'разбивки группы на строки по маркам в XLS листа'),
        ('(if hasMarks (strcat (if mk mk "") ";") "")',
         'колонки марки в CSV листа'),
        ('(setq visName (cs-get-visibility-safe obj))',
         'типа для отчётов из состояния видимости: cs-get-dyn-type-name '
         'возвращает имя блока, а не видимость'),
        ('(if (or (null visName) (= visName "")) (setq visName "-"))',
         'прочерка вместо пустой видимости: с nil cs-part-type откатится '
         'на (nth 3), то есть на марку'),
        ('(tu-entity-mark ent) visName)',
         '12-го элемента записи блока — типа для отчётов'),
        ('area nominal T ent nil type)',
         '12-го элемента записи полилинии: арность записей должна совпадать'),
        ('(defun cs-part-type (r)',
         'доступа cs-part-type: без него «Тип» в отчёте печатает марку'),
        ('(defun cs-group-type (g)',
         'доступа cs-group-type: тип группы для отчёта'),
    ],
    "Extraction/cutline.lsp": [
        ('kpd all-pieces /',
         'параметра all-pieces у писателей: без него перечень не собрать'),
        ('total-product-mm kpd pieces-ok)',
         'передачи pieces-ok в n1-write-xls'),
        ('(n1-write-csv bars stock kerf pieces-oversized pieces-ok)',
         'передачи pieces-ok в n1-write-csv'),
        ('"ПЕРЕЧЕНЬ ИЗДЕЛИЙ"',
         'раздела «ПЕРЕЧЕНЬ ИЗДЕЛИЙ» в XLS хлыста'),
        ('(setq rows (n1-piece-rows',
         'строк перечня через n1-piece-rows (те же, что в таблице AutoCAD)'),
        ('(if hasMarks (eu-cell f "SkipDataMid" "String" (caddr rw) ""))',
         'ячейки марки в перечне XLS хлыста'),
        ('"Длина, мм;Марка;Кол-во, шт;Сумма, м.п."',
         'заголовка перечня с маркой в CSV хлыста'),
        ('(vl-string-translate ";" "," (caddr rw))',
         'замены точки с запятой в марке: иначе ломается разделитель CSV'),
    ],
}

XLS_MARKS_LOCALS = {
    "Extraction/cutsheet.lsp": {
        "cs-write-xls": ((), ("hasmarks", "mk", "gmarks", "gcnt", "marked",
                              "rowslist", "rw")),
        "cs-write-csv": ((), ("hasmarks", "mk")),
    },
    "Extraction/cutline.lsp": {
        "n1-write-xls": (("all-pieces",), ("hasmarks", "rows", "rw")),
        "n1-write-csv": (("all-pieces",), ("hasmarks", "rows", "rw")),
    },
}


def _defun_args(text: str, name: str):
    """(параметры, локальные) для defun name либо None."""
    try:
        forms = lisp_parse(text)
    except LispParseError:
        return None
    for node in forms:
        if node[0] != "list" or len(node[1]) < 3:
            continue
        head, fname, args = node[1][0], node[1][1], node[1][2]
        if not (head[0] == "atom" and head[1].lower() == "defun"
                and fname[0] == "atom" and fname[1].lower() == name.lower()
                and args[0] == "list"):
            continue
        params, locals_, seen_slash = [], [], False
        for a in args[1]:
            if a[0] != "atom":
                continue
            if a[1] == "/":
                seen_slash = True
                continue
            (locals_ if seen_slash else params).append(a[1].lower())
        return params, locals_
    return None


XLS_MARKS_SCOPED = {
    "Extraction/cutsheet.lsp": {
        "cs-write-xls": [('"Марка"', 2), ("(if hasMarks (eu-column", 1),
                         ('(eu-cell f "H" "String" "Размер" "")\n'
                          '      (eu-cell f "H" "String" "Тип" "")', 1),
                         ('(eu-cell f "D" "String" (cs-part-label r) "")\n'
                          '          (eu-cell f "D" "String" (cs-part-type r) "")', 1),
                         ('(eu-cell f "D" "String" (cs-part-label r) "")\n'
                          '            (eu-cell f "D" "String" (cs-part-type r) "")', 1),
                         ("(foreach rw rowsList", 1), ("(cs-part-mark r)", 2),
                         ('(if hasMarks (eu-cell f "D" "String" (if mk mk "") ""))',
                          2), ("(cs-part-type r)", 2), ("(cs-group-type rec)", 1)],
        "cs-write-csv": [('"№ листа;№ детали;Размер;Тип;Марка;', 1),
                         ("(cs-part-mark r)", 2), ("(cs-part-type r)", 2),
                         ('(cs-part-label r) ";" (cs-part-type r) ";"', 2),
                         ('(if hasMarks (strcat (if mk mk "") ";") "")', 2)],
        "cs-aggregate": [("(cs-part-type r)", 2)],
        "cs-block-record": [("(setq visName (cs-get-visibility-safe obj))", 1)],
    },
    "Extraction/cutline.lsp": {
        "n1-write-xls": [('"ПЕРЕЧЕНЬ ИЗДЕЛИЙ"', 1), ("(n1-piece-rows", 1),
                         ("(caddr rw)", 1)],
        "n1-write-csv": [('"ПЕРЕЧЕНЬ ИЗДЕЛИЙ"', 1), ("(n1-piece-rows", 1),
                         ('(vl-string-translate ";" ","', 1)],
    },
}

# Чего в писателях быть не должно. У блоков ЗАПОЛНЕНИЯ cs-block-record
# подменяет тип маркой, поэтому (nth 3) в отчёте печатает марку ещё раз -
# колонки «Тип» и «Марка» совпадают строка в строку. Тип берётся только
# через cs-part-type / cs-group-type. Порядок колонок в выгрузке листа -
# «Размер | Тип | Марка» (по решению пользователя), старый «Тип | Размер»
# запрещён. В cs-block-record поле visName
# заполняется состоянием видимости: typName - это имя блока, с ним
# колонка «Тип» показывала имя блока вместо видимости.
XLS_MARKS_NOT = {
    "Extraction/cutsheet.lsp": {
        "cs-write-xls": ["(nth 3 r)", "(nth 3 rec)",
                         '(eu-cell f "H" "String" "Тип" "")\n'
                         '      (eu-cell f "H" "String" "Размер" "")'],
        "cs-write-csv": ["(nth 3 r)", '"№ листа;№ детали;Тип;Размер;'],
        "cs-block-record": ["(setq visName typName)",
                            "(setq visName (cs-get-dyn-type-name"],
    },
}


def _defun_body(text: str, name: str):
    """Текст формы (defun name ...) целиком либо None."""
    toks = lisp_tokens(text)
    start = None
    for k in range(len(toks) - 2):
        if (toks[k][0] == "(" and toks[k + 1][0] == "atom"
                and toks[k + 1][1].lower() == "defun"
                and toks[k + 2][0] == "atom"
                and toks[k + 2][1].lower() == name.lower()):
            start = k
            break
    if start is None:
        return None
    depth = 0
    for k in range(start, len(toks)):
        if toks[k][0] == "(":
            depth += 1
        elif toks[k][0] == ")":
            depth -= 1
            if depth == 0:
                lines = text.split("\n")
                return "\n".join(lines[toks[start][2] - 1:toks[k][2]])
    return None


def check_xls_marks_guard() -> list[str]:
    """Колонка «Марка» в XLS и CSV раскроя не потеряна.

    Марки долго жили только в таблицах AutoCAD: писатели XLS/CSV их не
    содержали, и таблица чертежа расходилась с файлом того же прогона по
    составу строк. Колонка добавлена во все листы и в CSV; она появляется
    только при наличии марок, поэтому прогон без марок даёт прежний файл.

    Отдельно закреплён тип: у блоков ЗАПОЛНЕНИЯ cs-block-record подменяет
    тип маркой, и прямое (nth 3) в писателе печатало марку во второй раз.
    """
    errors: list[str] = []
    for rel, needles in XLS_MARKS_MUST.items():
        path = ROOT / rel
        if not path.exists():
            continue
        text = read_text(path)
        for needle, what in needles:
            if needle not in text:
                errors.append(f"{rel}: выгрузка марок: нет {what}")
        for fn, needles_in in XLS_MARKS_SCOPED.get(rel, {}).items():
            body = _defun_body(text, fn)
            if body is None:
                errors.append(f"{rel}: выгрузка марок: не найден (defun {fn} ...)")
                continue
            for needle, need in needles_in:
                got = body.count(needle)
                if got < need:
                    errors.append(
                        f"{rel}: выгрузка марок: в {fn} нет {needle} "
                        f"(найдено {got}, нужно {need})")
        for fn, banned in XLS_MARKS_NOT.get(rel, {}).items():
            body = _defun_body(text, fn)
            if body is None:
                continue
            for needle in banned:
                if needle in body:
                    errors.append(
                        f"{rel}: выгрузка марок: в {fn} тип берётся напрямую "
                        f"через {needle} — колонки «Тип» и «Марка» продублируют "
                        f"марку (нужен cs-part-type / cs-group-type)")
        for fn, (params, locals_) in XLS_MARKS_LOCALS.get(rel, {}).items():
            got = _defun_args(text, fn)
            if got is None:
                errors.append(f"{rel}: выгрузка марок: не найден (defun {fn} ...)")
                continue
            for want in params:
                if want not in got[0]:
                    errors.append(
                        f"{rel}: выгрузка марок: '{want}' не среди аргументов {fn}")
            for want in locals_:
                if want not in got[1]:
                    errors.append(
                        f"{rel}: выгрузка марок: '{want}' не среди локальных "
                        f"переменных {fn}")
    return errors


def check_dialog_layers_section() -> list[str]:
    """Раздел «Выбранные слои» в окнах раскроя (CUTLINE и CUTSHEET).

    Список информационный и заполняется под vl-catch-all: если раздел
    пропадёт из .dcl, заполнение молча ничего не покажет, а окно
    откроется как ни в чём не бывало. Поэтому пара «.dcl + .lsp»
    проверяется статически.
    """
    pairs = [
        ("Extraction/cutline_filter.dcl", "Extraction/cutline.lsp",
         "n1-layer-display-list", "n1-layers-header-text"),
        ("Extraction/cutsheet_filter.dcl", "Extraction/cutsheet.lsp",
         "cs-layer-display-list", "cs-layers-header-text"),
    ]
    errors: list[str] = []
    for dcl_rel, lsp_rel, display_fn, header_fn in pairs:
        dcl, lsp = ROOT / dcl_rel, ROOT / lsp_rel
        if not dcl.exists() or not lsp.exists():
            continue
        dcl_text, lsp_text = read_text(dcl), read_text(lsp)
        if "Выбранные слои" not in dcl_text:
            errors.append(f"{dcl_rel}: нет раздела «Выбранные слои»")
        if 'key = "lst_layers"' not in dcl_text:
            errors.append(f"{dcl_rel}: нет списка с ключом lst_layers")
        if '(start_list "lst_layers")' not in lsp_text:
            errors.append(f"{lsp_rel}: список слоёв окна не заполняется")
        if f"(defun {display_fn}" not in lsp_text:
            errors.append(f"{lsp_rel}: нет функции показа слоёв {display_fn}")
        if f"({display_fn}" not in lsp_text.replace(f"(defun {display_fn}", ""):
            errors.append(f"{lsp_rel}: функция {display_fn} объявлена, но не вызывается")

        # Динамический заголовок рамки: DCL грузится из ВРЕМЕННОЙ копии файла,
        # её надо удалять на каждом выходе — иначе копии копятся в %TEMP%
        # молча. Выходов на один больше, чем unload_dialog: сбой load_dialog
        # происходит до того, как диалог загружен.
        if "tu-dcl-with-label" in lsp_text:
            cleanups = lsp_text.count("(tu-dcl-cleanup")
            unloads = lsp_text.count("unload_dialog")
            if cleanups < unloads + 1:
                errors.append(
                    f"{lsp_rel}: временная копия .dcl удаляется не на всех выходах "
                    f"(tu-dcl-cleanup {cleanups}, unload_dialog {unloads}, "
                    f"нужно минимум {unloads + 1})")
            if f"({header_fn}" not in lsp_text.replace(f"(defun {header_fn}", ""):
                errors.append(f"{lsp_rel}: заголовок рамки не вычисляется ({header_fn})")
    return errors


def check_dcl_gap_prototypes() -> list[str]:
    """Отступы окна диспетчера задаются ТОЛЬКО прототипами.

    В DCL нет переменных, поэтому единым источником служат прототипы
    тайлов (ae_gap_v / ae_gap_h / ae_margin), объявленные один раз и
    подставляемые в окно ссылкой. Если кто-то вернёт в тело окна сырой
    «: spacer { height = ...; }», раскладка снова начнёт разъезжаться по
    месту, а менять её придётся в двух десятках мест.
    """
    path = ROOT / "Extraction/extraction.dcl"
    if not path.exists():
        return []
    text = read_text(path)
    errors: list[str] = []

    protos = {"ae_gap_v": "height", "ae_gap_h": "width", "ae_margin": "width"}
    for name, dim in protos.items():
        decl = re.search(r"^" + name + r"\s*:\s*spacer\s*\{(.*?)\}", text, re.S | re.M)
        if not decl:
            errors.append(f"extraction.dcl: нет прототипа отступа {name}")
            continue
        if not re.search(dim + r"\s*=\s*[\d.]+\s*;", decl.group(1)):
            errors.append(f"extraction.dcl: прототип {name} не задаёт {dim}")
        refs = len(re.findall(r"^\s*" + name + r";\s*$", text, re.M))
        if refs == 0:
            errors.append(f"extraction.dcl: прототип {name} объявлен, но не используется")

    marker = "extraction_dialog : dialog {"
    if marker in text:
        body = text[text.index(marker):]
        raw = len(re.findall(r":\s*spacer\s*\{", body))
        if raw:
            errors.append(f"extraction.dcl: в теле окна {raw} сырых отступов «: spacer {{…}}» — "
                          "отступы задаются только прототипами ae_gap_v / ae_gap_h / ae_margin")
    return errors


def check_mark_labels() -> list[str]:
    """Подпись марки элемента в картах раскроя.

    Главный инвариант — решатель НЕ ЗНАЕТ про марки. В хлысте марки
    живут в побочной очереди «длина -> марки», потому что FFD работает
    с голыми длинами; если марка просочится в n1-ffd, это будет правка
    алгоритма раскроя, чего делать нельзя.
    """
    errors: list[str] = []

    tu = ROOT / "common/task-utils.lsp"
    if tu.exists():
        text = read_text(tu)
        for fn in ("(defun tu-block-attr", "(defun tu-entity-mark"):
            if fn not in text:
                errors.append(f"common/task-utils.lsp: нет {fn[7:]}")
        if "*AE-MARK-ATTR*" not in text:
            errors.append("common/task-utils.lsp: имя тега марки не вынесено в переменную")

    cs = ROOT / "Extraction/cutsheet.lsp"
    if cs.exists():
        text = read_text(cs)
        for need, what in (
            ("(tu-entity-mark ent)", "марка не читается при сборе записи"),
            ("(defun cs-part-mark", "нет доступа к марке записи"),
            ("(cs-part-mark r)", "марка не используется при отрисовке"),
        ):
            if need not in text:
                errors.append(f"Extraction/cutsheet.lsp: {what}")

    cl = ROOT / "Extraction/cutline.lsp"
    if cl.exists():
        text = read_text(cl)
        for need, what in (
            ("(defun n1-mark-add", "нет сбора марок"),
            ("(defun n1-mark-take", "нет выдачи марок"),
            ("(defun n1-marks-begin", "нет сброса очереди перед отрисовкой"),
            ("(tu-entity-mark ent)", "марка не читается при измерении"),
            ("(n1-mark-take p)", "марка не используется при отрисовке"),
            ("(n1-marks-begin)", "очередь не сбрасывается перед отрисовкой"),
        ):
            if need not in text:
                errors.append(f"Extraction/cutline.lsp: {what}")
        # Решатель не должен ничего знать про марки. Границы формы берём
        # у разборщика s-выражений: поиск «до следующего defun» захватывал
        # код ПОСЛЕ решателя и давал ложное срабатывание.
        try:
            forms = lisp_parse(text)
        except LispParseError:
            forms = []
        lines = text.split("\n")
        for idx, form in enumerate(forms):
            if form[0] != "list" or len(form[1]) < 2:
                continue
            head = form[1][0]
            name = form[1][1]
            if (head[0] == "atom" and head[1].upper() == "DEFUN"
                    and name[0] == "atom" and name[1].lower() == "n1-ffd"):
                start = form[2] - 1
                end = (forms[idx + 1][2] - 1) if idx + 1 < len(forms) else len(lines)
                body = "\n".join(lines[start:end])
                if "mark" in body.lower():
                    errors.append("Extraction/cutline.lsp: решатель n1-ffd знает про "
                                  "марки — алгоритм раскроя трогать нельзя")
                break
    return errors


def check_fill_allowance() -> list[str]:
    """Блоки заполнения в раскрое листа: «в свету» + припуск.

    Два инварианта, которые нельзя потерять:
    1. припуск берётся из настроек задачи ЗАПОЛНЕНИЕ — один источник на
       отчёт и на раскрой, иначе заготовка разойдётся с заказанной
       панелью, и это заметят только на монтаже;
    2. у блока заполнения НЕТ отката на BoundingBox: габарит включает
       раму, молча неверный раскрой хуже явного отказа.
    """
    errors: list[str] = []

    su = ROOT / "common/settings-utils.lsp"
    if su.exists():
        text = read_text(su)
        if '"input.frame.allowance"' not in text:
            errors.append("common/settings-utils.lsp: нет ключа input.frame.allowance")
        if "(defun ae-settings-frame-allowance" not in text:
            errors.append("common/settings-utils.lsp: нет аксессора ae-settings-frame-allowance")

    zp = ROOT / "Extraction/zapolnenie.lsp"
    if zp.exists():
        text = read_text(zp)
        if "(zapolnenie-frame-allowance)" not in text:
            errors.append("Extraction/zapolnenie.lsp: припуск не берётся из настроек")

    cs = ROOT / "Extraction/cutsheet.lsp"
    if cs.exists():
        text = read_text(cs)
        for need, what in (
            ("(defun cs-fill-masks", "нет признака блока заполнения (маски)"),
            ("(defun cs-fill-block-p", "нет проверки блока на заполнение"),
            ("(defun cs-fill-dimension", "нет цепочки размеров «в свету»"),
            ("(defun cs-fill-allowance", "нет доступа к припуску"),
            ("(cs-fill-block-p ent (cs-fill-masks))", "ветка заполнения не включена в сбор записи"),
            ("ВЫСОТА В СВЕТУ", "нет приоритета размера «в свету»"),
        ):
            if need not in text:
                errors.append(f"Extraction/cutsheet.lsp: {what}")
        # в ветке заполнения не должно быть отката на габарит
        start = text.find("(setq fill (cs-fill-block-p")
        if start >= 0:
            branch = text[start:start + 1400]
            head = branch.split("(progn\n          (setq pW (cs-prop-value props")[0]
            if "cs-bbox-w-h" in head:
                errors.append("Extraction/cutsheet.lsp: ветка заполнения откатывается на "
                              "BoundingBox — габарит включает раму, нужен явный отказ")
    return errors


def check_search_count() -> list[str]:
    """Подсчёт окна поиска: размеры «в свету» и припуск по маскам.

    Инварианты:
    1. площадь берёт «Высота в свету» / «Ширина в свету» раньше обычных
       имён — та же цепочка, что у ЗАПОЛНЕНИЯ;
    2. припуск берётся из настроек задачи ЗАПОЛНЕНИЕ (ae-settings-frame-allowance)
       и применяется только к блокам по маскам задачи ЗАПОЛНЕНИЕ (input.block, как у раскроя);
    3. размеры считаются через общую функцию ae-sv-size-of, которую используют
       и таблица, и окно «Подсчёт».
    """
    errors: list[str] = []
    sv = ROOT / "Extraction/searchattvis.lsp"
    if not sv.exists():
        return errors
    text = read_text(sv)
    for need, what in (
        ('(quote ("Высота в свету" "Высота"))', "нет приоритета «Высота в свету» для площади"),
        ('(quote ("Ширина в свету" "Ширина" "Длина"))', "нет приоритета «Ширина в свету» для площади"),
        ("(ae-settings-task-blocks 'ZAPOLNENIE)", "припуск не берёт маски задачи ЗАПОЛНЕНИЕ (input.block)"),
        ("(defun ae-sv-frame-masks", "нет маски припуска из задачи ЗАПОЛНЕНИЕ"),
        ("(defun ae-sv-allowance", "нет доступа к припуску"),
        ("(ae-settings-frame-allowance)", "припуск не берётся из настроек задачи ЗАПОЛНЕНИЕ"),
        ("(defun ae-sv-frame-p", "нет проверки имени блока по маскам припуска"),
        ("(ae-sv-frame-p rec masks)", "припуск не применяется в ae-sv-size-of"),
        ('"Блок (по Enter):"', "первое поле поиска должно иметь подсказку «Блок (по Enter):»"),
        ("(defun ae-sv-on-similar", "нет обработчика кнопки «Выбрать подобное»"),
        ("(defun ae-sv-similar-targets", "нет отбора целевых строк для «Выбрать подобное»"),
        ("(defun ae-sv-similar-ens", "нет поиска подобных вставок по всему чертежу"),
        ("(defun ae-sv-adapt-sel-i", "нет сохранения выбора правого списка при вводе/очистке «Значения»"),
        ('(action_tile "btn_similar" "(ae-sv-on-similar)")', "кнопка btn_similar не привязана к ae-sv-on-similar"),
    ):
        if need not in text:
            errors.append(f"Extraction/searchattvis.lsp: {what}")
    if '"Имя (по Enter):"' in text:
        errors.append("Extraction/searchattvis.lsp: осталась старая подсказка «Имя (по Enter):» вместо «Блок (по Enter):»")
    dcl = ROOT / "Extraction/searchattvis.dcl"
    if dcl.exists():
        dcl_text = read_text(dcl)
        if 'key = "btn_similar";' not in dcl_text or 'label = "Выбрать подобное";' not in dcl_text:
            errors.append("Extraction/searchattvis.dcl: нет кнопки btn_similar («Выбрать подобное»)")
    return errors


def check_summary_marks() -> list[str]:
    """Колонка «Марка» в перечне изделий не меняет геометрию отчётов.

    Главный инвариант: сумма ширин колонок с маркой равна сумме без
    марки. Иначе таблица раскроя хлыстов поедет по ширине, а её
    положение на карте согласовано с остальной раскладкой.
    """
    errors: list[str] = []

    cl = ROOT / "Extraction/cutline.lsp"
    if cl.exists():
        text = read_text(cl)
        if "(defun n1-marks-for (" not in text:
            errors.append("Extraction/cutline.lsp: нет выдачи марок по длине для перечня")
        if "tu-marks-brief" not in text:
            errors.append("Extraction/cutline.lsp: марки не выводятся в перечень изделий")
        if "(defun n1-piece-rows (" not in text:
            errors.append("Extraction/cutline.lsp: перечень изделий не разбивается по маркам — "
                          "вернётся обозначение «первая +N», которое читается как имя марки")
        if "(+ curx (* mkH 0.35)) (+ y0 (* barHeight 0.5))" not in text:
            errors.append("Extraction/cutline.lsp: марка на детали не на одной базовой линии "
                          "с длиной — подписи встанут на разных уровнях")
        if "(tu-fit-chars colMW th)" not in text:
            errors.append("Extraction/cutline.lsp: предел длины марки задан числом, а не "
                          "шириной колонки — свёртка снова наедет на соседнюю колонку")
        with_marks = re.search(
            r"col1W \(\* barHeight ([\d.]+)\) colMW \(\* barHeight ([\d.]+)\)\s*"
            r"col2W \(\* barHeight ([\d.]+)\) col3W \(\* barHeight ([\d.]+)\)", text, re.S)
        without = re.search(
            r"col1W \(\* barHeight ([\d.]+)\) colMW 0\.0\s*"
            r"col2W \(\* barHeight ([\d.]+)\) col3W \(\* barHeight ([\d.]+)\)", text, re.S)
        if not with_marks or not without:
            errors.append("Extraction/cutline.lsp: не разобраны ширины колонок перечня")
        else:
            a = sum(float(x) for x in with_marks.groups())
            b = sum(float(x) for x in without.groups())
            if abs(a - b) > 1e-9:
                errors.append(f"Extraction/cutline.lsp: ширина таблицы меняется от колонки "
                              f"«Марка» ({a} против {b}) — геометрия отчёта поедет")

    # Порядок вычислений: марка считается раньше подписи габарита, иначе
    # габарит некуда сдвигать и подписи наползают на мелких деталях.
    # Тело берём по ИМЕНИ функции отрисовки детали: в cutsheet строка
    # «(setq mk (cs-part-mark r))» встречается и в cs-aggregate, поиск
    # по первому вхождению брал не ту функцию и ничего не проверял.
    for rel, fname, mark_call, size_call in (
        ("Extraction/cutsheet.lsp", "cs-draw-placement",
         "(cs-part-mark r)", "cs-draw-text-center"),
        ("Extraction/cutline.lsp", "n1-draw-layout",
         "(n1-mark-take p)", "n1-draw-text-bold-center"),
    ):
        path = ROOT / rel
        if not path.exists():
            continue
        text_all = read_text(path)
        start = text_all.find("(defun " + fname)
        if start < 0:
            errors.append(f"{rel}: не найдена функция отрисовки детали {fname}")
            continue
        end = text_all.find("\n(defun ", start + 1)
        body = text_all[start:end if end > 0 else len(text_all)]
        i_mark = body.find(mark_call)
        i_size = body.find(size_call)
        if i_mark < 0:
            errors.append(f"{rel}: марка не считается в {fname}")
        elif 0 <= i_size < i_mark:
            errors.append(f"{rel}: в {fname} подпись габарита считается РАНЬШЕ марки — "
                          "сдвинуть габарит будет нечем, подписи наползут")

    cs = ROOT / "Extraction/cutsheet.lsp"
    if cs.exists():
        text = read_text(cs)
        if "tu-marks-brief" not in text:
            errors.append("Extraction/cutsheet.lsp: марки не выводятся в перечень изделий")
        if "tu-fit-chars" not in text:
            errors.append("Extraction/cutsheet.lsp: предел длины марки задан числом, а не "
                          "шириной колонки")
        if "(defun cs-marks-add (" not in text:
            errors.append("Extraction/cutsheet.lsp: количество по каждой марке не считается — "
                          "сводку нечем разбить на строки")
        m = re.search(r"setq colMark ([\d.]+) colCnt \(if hasMarks ([\d.]+) ([\d.]+)\)"
                      r" colArea \(if hasMarks ([\d.]+) ([\d.]+)\)", text)
        if not m:
            errors.append("Extraction/cutsheet.lsp: не разобраны позиции колонок сводки")
        else:
            mark, cnt_m, cnt_n, area_m, area_n = (float(x) for x in m.groups())
            if not (0.0 < mark < cnt_m < area_m < 1.0):
                errors.append("Extraction/cutsheet.lsp: колонки сводки с маркой идут не по порядку")
            if not (cnt_n < cnt_m and area_n < area_m):
                errors.append("Extraction/cutsheet.lsp: колонки без марки должны быть левее, "
                              "иначе место под «Марку» не освобождается")
    return errors


# ----------------------------------------------------------------------
# Main
# ----------------------------------------------------------------------

def check_search_count_window() -> list[str]:
    """Окно «Подсчёт»: таблица с рамкой, ширины колонок из lsp сверяются с DCL.

    Инварианты:
    1. *ae-sv-cnt-widths* (lsp): варианты n (без марок) и m (с марками) —
       ширины колонок в символах; ячейка = «| » + W; справа закрывающая «|» на правой границе;
    2. DCL: диалоги searchattvis_count_n и searchattvis_count_m; при смешанном выборе —
       searchattvis_count_xn / _xm с двумя списками (lst_count — площадь, lst_count2 — погонаж);
       tabs = 1,2 · (1 пробел слева + накопленная ширина ячеек, ячейка = W + 2),
       ширина списка = 1,2 · (сумма ячеек + пробелы слева и справа) + 6, не меньше кнопок;
       высота списка 40;
    3. окно выбирает диалог через (ae-sv-cnt-dialog-name);
    4. без (or (nth …)) — AutoLISP возвращает T, а не значение.
    """
    errors: list[str] = []
    sv = ROOT / "Extraction/searchattvis.lsp"
    dcl = ROOT / "Extraction/searchattvis.dcl"
    if not sv.exists() or not dcl.exists():
        return errors
    lsp = read_text(sv)
    dcl_text = read_text(dcl)
    if "(new_dialog (ae-sv-cnt-dialog-name) dcl-id)" not in lsp:
        errors.append("Extraction/searchattvis.lsp: окно «Подсчёт» не выбирает диалог через ae-sv-cnt-dialog-name")
    if re.search(r"\(or \((?:nth|c[ad]+r)\b", lsp):
        errors.append("Extraction/searchattvis.lsp: (or (nth|cadr…)) возвращает T, а не значение; использовать if")
    m = re.search(r"\(setq \*ae-sv-cnt-widths\*(.*?)\)\)\)\n", lsp, re.S)
    if not m:
        errors.append("Extraction/searchattvis.lsp: нет *ae-sv-cnt-widths*")
        return errors
    widths = {k: [int(x) for x in v.split()]
              for k, v in re.findall(r'\("(ln|lm|n|m)"((?:\s+\d+)+)\)', m.group(1))}
    if sorted(widths) != ["lm", "ln", "m", "n"]:
        errors.append(f"Extraction/searchattvis.lsp: ширины для {sorted(widths)}, нужны n, m, ln и lm")
    def dialog_body(name):
        dm = re.search(r"searchattvis_count_" + name + r" : dialog \{(.*?)\n\}\n", dcl_text, re.S)
        if not dm:
            errors.append(f"Extraction/searchattvis.dcl: нет диалога searchattvis_count_{name}")
            return None
        return dm.group(1)

    def check_list(body, dlg, list_key, wkey, height):
        wm = re.search(r'key = "' + list_key + r'";\s*width = (\d+);\s*height = (\d+);', body)
        tm = re.search(r'key = "' + list_key + r'";.*?tabs = "([\d ]*)";', body, re.S)
        if not wm or not tm:
            errors.append(f"Extraction/searchattvis.dcl: searchattvis_count_{dlg}: нет width/height/tabs у {list_key}")
            return
        cells = [w + 2 for w in widths[wkey]]
        expect_width = max((6 * (sum(cells) + 2) + 2) // 5 + 6, 48 + 2)   # + кнопки 14+22+12
        expect_tabs = []
        acc = 1                       # пробел слева от таблицы
        for c in cells:
            acc += c
            expect_tabs.append((6 * acc + 2) // 5)
        got_tabs = [int(x) for x in tm.group(1).split()]
        if int(wm.group(1)) != expect_width:
            errors.append(f"Extraction/searchattvis.dcl: searchattvis_count_{dlg} {list_key}: ширина {wm.group(1)}, ожидалось {expect_width}")
        if got_tabs != expect_tabs:
            errors.append(f"Extraction/searchattvis.dcl: searchattvis_count_{dlg} {list_key}: tabs {got_tabs}, ожидалось {expect_tabs}")
        if height is not None and int(wm.group(2)) != height:
            errors.append(f"Extraction/searchattvis.dcl: searchattvis_count_{dlg} {list_key}: высота {wm.group(2)}, ожидалось {height}")

    def check_status(body, dlg):
        if not re.search(r'key = "txt_count_status";.*?alignment = right;', body, re.S):
            errors.append(f"Extraction/searchattvis.dcl: searchattvis_count_{dlg}: строка состояния не выровнена вправо")

    for key in ("n", "m", "ln", "lm"):
        if key not in widths:
            continue
        body = dialog_body(key)
        if body is None:
            continue
        check_list(body, key, "lst_count", key, 40)
        check_status(body, key)
    # смешанный выбор: площадь (lst_count) и погонаж (lst_count2) — два списка
    for dlg, area, lin in (("xn", "n", "ln"), ("xm", "m", "lm")):
        if area not in widths or lin not in widths:
            continue
        body = dialog_body(dlg)
        if body is None:
            continue
        check_list(body, dlg, "lst_count", area, 24)
        check_list(body, dlg, "lst_count2", lin, 16)
        check_status(body, dlg)
    return errors


def main() -> int:
    global PLUGINS_ENABLED
    if "--no-plugins" in sys.argv[1:]:
        PLUGINS_ENABLED = False
    if "--plugins" in sys.argv[1:]:
        PLUGINS_ENABLED = True

    errors: list[str] = []

    errors += check_required_files()

    lisp_count = 0
    for path in lisp_files():
        lisp_count += 1
        ok, msg = check_balance_lex(path)
        if not ok:
            errors.append(f"{path.relative_to(ROOT)}: {msg}")

    errors += check_duplicates()
    errors += check_required_mains()

    dcl_keys = find_dcl_keys()
    dcl_count = len(list(dcl_files()))
    for path in lisp_files():
        errors += check_dcl_keys(path, dcl_keys)
    for path in dcl_files():
        errors += check_dcl_syntax(path)

    errors += check_cutline_wrap_guard()
    errors += check_cutsheet_wrap_guard()
    errors += check_reload_chkload_guard()
    errors += check_reload_plugins_guard()
    errors += check_settings_plugins_guard()
    errors += check_block_length_setting()
    errors += check_powershell_encoding()
    errors += check_crlf_tolerance()
    errors += check_test_console_encoding()
    errors += check_settings_save_guard()
    errors += check_subsystem_numbering()
    errors += check_chkparens_plugins_guard()
    errors += check_lisp_lexical()

    # tests/*.lsp: баланс и лексика (chkparens.lsp загружает RELOAD)
    tests_count = 0
    for path in test_lisp_files():
        tests_count += 1
        ok, msg = check_balance_lex(path)
        if not ok:
            errors.append(f"{path.relative_to(ROOT)}: {msg}")
    errors += check_lisp_lexical(list(test_lisp_files()))

    # Плагины из реестра: грузятся тем же RELOAD, поэтому баланс и лексика
    # проверяются наравне с модулями проекта
    plugin_paths = list(plugin_files())
    plugins_count = 0
    for path in plugin_paths:
        plugins_count += 1
        ok, msg = check_balance_lex(path)
        if not ok:
            errors.append(f"{_rel(path)}: {msg}")
    errors += check_lisp_lexical(plugin_paths)

    # reload.lsp в корне: грузится первым, его дефект лишает всей диагностики
    root_lisp = ROOT / "reload.lsp"
    if root_lisp.exists():
        ok, msg = check_balance_lex(root_lisp)
        if not ok:
            errors.append(f"{root_lisp.relative_to(ROOT)}: {msg}")
        errors += check_lisp_lexical([root_lisp])

    # Спецформы по всем .lsp сразу (модули + tests + reload.lsp + плагины)
    errors += check_special_forms(list(all_lisp_files()))
    errors += check_reload_version_line()
    errors += check_defun_duplicates_in_file(list(all_lisp_files()))
    errors += check_call_arity(list(all_lisp_files()))
    errors += check_dialog_layers_section()
    errors += check_dcl_gap_prototypes()
    errors += check_mark_labels()
    errors += check_fill_allowance()
    errors += check_search_count()
    errors += check_summary_marks()
    errors += check_search_count_window()
    errors += check_xls_marks_guard()

    if errors:
        print("ERRORS:")
        for e in errors:
            print(f"  FAIL: {e}")
        print("RESULT: FAIL")
        return 1

    print(
        f"PASS: lisp={lisp_count} dcl={dcl_count} tests={tests_count} "
        f"plugins={plugins_count}"
        f"{'' if PLUGINS_ENABLED else ' (--no-plugins)'} "
        f"checks: required / balance / defun-dup / dcl-keys / dcl-syntax / mains"
        f" / cutline-wrap-guard / cutsheet-wrap-guard / reload-chkload-guard"
        f" / lexical / tests-balance / tests-lexical / reload-balance / special-forms"
        f" / reload-version-line / defun-dup-in-file / dialog-layers / dcl-gaps"
        f" / plugins-balance / plugins-lexical / reload-plugins-guard"
        f" / chkparens-plugins-guard / call-arity / xls-marks"
        f" / mark-labels / fill-allowance / block-length / ps-encoding"
        f" / crlf-tolerance / console-encoding / settings-save"
        f" / subsystem-numbering"
        f" / search-count"
        f" / summary-marks / search-count-window"
    )
    print("RESULT: PASS")
    return 0


if __name__ == "__main__":
    sys.exit(main())