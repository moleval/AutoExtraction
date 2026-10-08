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
# tests/*.lsp в основные группы не входят (там свои defun и свои правила),
# но баланс и лексика проверяются отдельно: chkparens.lsp автозагружает RELOAD.
TEST_DIR = ROOT / "tests"

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


def lisp_files():
    for d in LISP_DIRS:
        if d.exists():
            yield from sorted(d.glob("*.lsp"))


def dcl_files():
    for d in DCL_DIRS:
        if d.exists():
            yield from sorted(d.glob("*.dcl"))


def test_lisp_files():
    if TEST_DIR.exists():
        yield from sorted(TEST_DIR.glob("*.lsp"))


def all_lisp_files():
    """Весь LISP проекта: модули, tests/*.lsp и корневой reload.lsp."""
    yield from lisp_files()
    yield from test_lisp_files()
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

    for path in lisp_files():
        rel = str(path.relative_to(ROOT))
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


def check_lisp_lexical(files=None) -> list[str]:
    """Причины «синтаксической ошибки» при целом балансе скобок.

    AutoCAD читает .lsp как ANSI: BOM, управляющие байты и не-ANSI символы
    вне строк/комментариев (кроме имён команд c:ИМЯ) валят загрузку файла,
    а баланс скобок при этом остаётся нулевым.
    """
    errors: list[str] = []
    for path in (files if files is not None else lisp_files()):
        raw = path.read_bytes()
        rel = path.relative_to(ROOT)
        if raw.startswith(b"\xef\xbb\xbf"):
            errors.append(f"{rel}: BOM (UTF-8) в начале файла — AutoCAD читает .lsp как ANSI")
            raw = raw[3:]
        try:
            text = raw.decode("cp1251")
        except UnicodeDecodeError as exc:
            errors.append(f"{rel}: байт 0x{raw[exc.start]:02X} не читается как ANSI (cp1251)")
            continue
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
        rel = path.relative_to(ROOT)
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


def check_defun_duplicates_in_file() -> list[str]:
    """Один и тот же defun объявлен в файле дважды.

    Дубль переопределяет первую редакцию молча: сканеры баланса и
    спецформ его не видят. Проверяются все .lsp, включая reload.lsp и
    tests/*.lsp, которые в общий поиск дубликатов не входят.
    """
    errors: list[str] = []
    for path in all_lisp_files():
        seen: dict[str, int] = {}
        for name, line in find_defuns(path):
            key = name.upper()
            # *error* объявляется локально в каждой команде — норма.
            if name.lower() in {w.lower() for w in WHITELIST_DEFUN}:
                continue
            if key in seen:
                errors.append(f"{path.relative_to(ROOT)}: defun {name} объявлен дважды "
                              f"(строки {seen[key]} и {line})")
            else:
                seen[key] = line
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
        if "(defun n1-marks-for" not in text:
            errors.append("Extraction/cutline.lsp: нет выдачи марок по длине для перечня")
        if "tu-marks-brief" not in text:
            errors.append("Extraction/cutline.lsp: марки не выводятся в перечень изделий")
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

def main() -> int:
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
    errors += check_lisp_lexical()

    # tests/*.lsp: баланс и лексика (chkparens.lsp загружает RELOAD)
    tests_count = 0
    for path in test_lisp_files():
        tests_count += 1
        ok, msg = check_balance_lex(path)
        if not ok:
            errors.append(f"{path.relative_to(ROOT)}: {msg}")
    errors += check_lisp_lexical(list(test_lisp_files()))

    # reload.lsp в корне: грузится первым, его дефект лишает всей диагностики
    root_lisp = ROOT / "reload.lsp"
    if root_lisp.exists():
        ok, msg = check_balance_lex(root_lisp)
        if not ok:
            errors.append(f"{root_lisp.relative_to(ROOT)}: {msg}")
        errors += check_lisp_lexical([root_lisp])

    # Спецформы по всем .lsp сразу (модули + tests + reload.lsp)
    errors += check_special_forms()
    errors += check_reload_version_line()
    errors += check_defun_duplicates_in_file()
    errors += check_dialog_layers_section()
    errors += check_dcl_gap_prototypes()
    errors += check_mark_labels()
    errors += check_fill_allowance()
    errors += check_summary_marks()

    if errors:
        print("ERRORS:")
        for e in errors:
            print(f"  FAIL: {e}")
        print("RESULT: FAIL")
        return 1

    print(
        f"PASS: lisp={lisp_count} dcl={dcl_count} tests={tests_count} "
        f"checks: required / balance / defun-dup / dcl-keys / dcl-syntax / mains"
        f" / cutline-wrap-guard / cutsheet-wrap-guard / reload-chkload-guard"
        f" / lexical / tests-balance / tests-lexical / reload-balance / special-forms"
        f" / reload-version-line / defun-dup-in-file / dialog-layers / dcl-gaps"
        f" / mark-labels / fill-allowance / summary-marks"
    )
    print("RESULT: PASS")
    return 0


if __name__ == "__main__":
    sys.exit(main())