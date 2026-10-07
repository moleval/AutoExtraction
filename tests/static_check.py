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


# ----------------------------------------------------------------------
# Main
# ----------------------------------------------------------------------

def main() -> int:
    errors: list[str] = []

    errors += check_required_files()

    lisp_count = 0
    for path in lisp_files():
        lisp_count += 1
        ok, msg = check_balance(path)
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

    if errors:
        print("ERRORS:")
        for e in errors:
            print(f"  FAIL: {e}")
        print("RESULT: FAIL")
        return 1

    print(
        f"PASS: lisp={lisp_count} dcl={dcl_count} "
        f"checks: required / balance / defun-dup / dcl-keys / dcl-syntax / mains"
        f" / cutline-wrap-guard"
    )
    print("RESULT: PASS")
    return 0


if __name__ == "__main__":
    sys.exit(main())