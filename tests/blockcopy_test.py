# -*- coding: utf-8 -*-
"""BLOCKCOPY / КОПИЯБЛОКА: порт логики копирования блока.

Проверяет контракт ред. 4 (Extraction/blockrename.lsp):

 1. копируется ОПРЕДЕЛЕНИЕ блока через промежуточный документ ObjectDBX:
    переименование делается там, до возврата в чертёж;
 2. исходное определение не переименовывается и не удаляется;
 3. копия вставляется ссылкой на новое определение, а не файлом и не
    вложением исходного блока;
 4. WBLOCK не используется: он пишет вставку, а не определение, и даёт
    обёртку-«муляж»;
 5. результат сообщает, динамическая копия или нет.

Почему появился (ред. 4): пользователь получил копию, которая оказалась
вложенной (WBLOCK -> INSERT создал обёртку), а исходный блок при этом
потерял имя — переименование шло по vla-get-EffectiveName, который у
динамического блока возвращает имя динамического блока, то есть имя
ИСХОДНОГО определения.
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

if hasattr(sys.stdout, "reconfigure"):
    sys.stdout.reconfigure(errors="replace")

ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT / "Extraction" / "blockrename.lsp"

PASS = 0
FAIL = 0
FAILURES: list[str] = []


def expect(name: str, condition: bool, detail: str = "") -> None:
    global PASS, FAIL
    if condition:
        PASS += 1
    else:
        FAIL += 1
        FAILURES.append(f"{name}: {detail}" if detail else name)


def lisp_text(path: Path) -> str:
    raw = path.read_bytes()
    try:
        return raw.decode("utf-8")
    except UnicodeDecodeError:
        return raw.decode("cp1251")


text = lisp_text(SOURCE)


def strip_comments(src: str) -> str:
    """Код без построчных комментариев (строковые литералы не трогаем)."""
    out = []
    for line in src.split("\n"):
        kept = []
        in_string = False
        escaped = False
        for ch in line:
            if in_string:
                kept.append(ch)
                if escaped:
                    escaped = False
                elif ch == "\\":
                    escaped = True
                elif ch == '"':
                    in_string = False
                continue
            if ch == '"':
                in_string = True
                kept.append(ch)
            elif ch == ";":
                break
            else:
                kept.append(ch)
        out.append("".join(kept))
    return "\n".join(out)


code = strip_comments(text)


def defun_body(name: str) -> str:
    """Тело (defun name ...) до закрывающей скобки формы."""
    found = re.search(r"\(defun\s+" + re.escape(name) + r"[\s(]", text)
    if not found:
        return ""
    start = found.start()
    depth = 0
    in_string = False
    escaped = False
    in_comment = False
    for pos in range(start, len(text)):
        ch = text[pos]
        if in_comment:
            if ch == "\n":
                in_comment = False
            continue
        if in_string:
            if escaped:
                escaped = False
            elif ch == "\\":
                escaped = True
            elif ch == '"':
                in_string = False
            continue
        if ch == ";":
            in_comment = True
        elif ch == '"':
            in_string = True
        elif ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
            if depth == 0:
                return text[start:pos + 1]
    return text[start:]


copy_block = defun_body("blockrename-copy-block")
copy_def = defun_body("blockrename-copy-definition")
insert_ref = defun_body("blockrename-insert-reference")

# ---------------------------------------------------------------
# Порт 1: генерация имени копии
# ---------------------------------------------------------------
def unique_name(base: str, existing: set[str]) -> str:
    n = 1
    while f"{base}_{n}" in existing:
        n += 1
    return f"{base}_{n}"


expect("свежее имя - _1", unique_name("Козырёк Кз-2", set()) == "Козырёк Кз-2_1")
expect("занятое имя обходится",
       unique_name("Козырёк Кз-2", {"Козырёк Кз-2_1"}) == "Козырёк Кз-2_2")
expect("несколько занятых подряд",
       unique_name("Б", {"Б_1", "Б_2", "Б_3"}) == "Б_4")

# ---------------------------------------------------------------
# Порт 2: порядок операций копирования определения
# ---------------------------------------------------------------
def copy_steps(has_dbx: bool, src_ok: bool, rename_ok: bool,
               back_ok: bool) -> tuple[str, str]:
    if not has_dbx:
        return ("ERROR", "ObjectDBX недоступен")
    if not src_ok:
        return ("ERROR", "не открыто определение")
    if not rename_ok:
        return ("ERROR", "переименование копии")
    if not back_ok:
        return ("ERROR", "новое определение не появилось")
    return ("OK", "копия")


expect("полный успех", copy_steps(True, True, True, True)[0] == "OK")
expect("без ObjectDBX копии нет",
       copy_steps(False, True, True, True) ==
       ("ERROR", "ObjectDBX недоступен"))
expect("сбой переименования в промежуточном документе отменяет возврат",
       copy_steps(True, True, False, True)[0] == "ERROR")
expect("сбой возврата в чертёж - ошибка, а не молчание",
       copy_steps(True, True, True, False)[0] == "ERROR")

# ---------------------------------------------------------------
# Порт 3: опознание динамической копии
# ---------------------------------------------------------------
def report_kind(is_dynamic: bool) -> str:
    return "динамический" if is_dynamic else "статический"


expect("динамическая копия называется динамической",
       report_kind(True) == "динамический")
expect("статическая копия называется статической",
       report_kind(False) == "статический")

# ---------------------------------------------------------------
# Связь порта с исходником
# ---------------------------------------------------------------
expect("WBLOCK больше не используется",
       "WBLOCK" not in code.upper(),
       "в модуле остался WBLOCK - он пишет вставку, а не определение")
expect("временные файлы не создаются",
       "vl-filename-mktemp" not in code,
       "осталось создание временного файла")
copy_path = copy_block + copy_def + insert_ref
expect("в пути копирования нет EffectiveName",
       "EffectiveName" not in copy_path,
       "EffectiveName динамического блока = имя исходного определения")
expect("используется промежуточный документ ObjectDBX",
       "objectdbx.axdbdocument" in text,
       "нет строки ProgID ObjectDBX")
expect("ProgID собирается по версии AutoCAD",
       "(itoa vrs)" in text or "(itoa (atoi" in text,
       "ProgID не учитывает версию")

expect("определение копируется из Blocks чертежа",
       "(list doc 'CopyObjects (list src) dbxBlocks)" in copy_def,
       "нет копирования определения в промежуточный документ")
expect("переименование идёт в промежуточном документе",
       "(list dbxDef newName)" in copy_def,
       "переименование адресовано не копии в ObjectDBX")
expect("переименовывается только объект промежуточного документа",
       copy_def.count("vla-put-Name") == 1,
       "в копировании больше одного переименования")
expect("переименование стоит ДО возврата в чертёж",
       copy_def.find("(list dbxDef newName)") <
       copy_def.find("(list dbx 'CopyObjects (list dbxDef) blocks)"),
       "переименование после возврата - снова риск задеть исходник")
expect("определение возвращается в Blocks чертежа",
       "(list dbx 'CopyObjects (list dbxDef) blocks)" in copy_def,
       "нет возврата определения")
expect("проверяется, что новое имя появилось",
       "(tblsearch \"BLOCK\" newName)" in copy_def,
       "нет проверки результата копирования")
expect("промежуточный документ освобождается",
       "vlax-release-object" in copy_def,
       "утечка ObjectDBX-документа")

expect("ссылка вставляется по имени определения",
       "defName 1.0 1.0 1.0 0.0" in insert_ref,
       "вставка идёт не по имени определения")
expect("динамика вставки проверяется",
       "IsDynamicBlock" in insert_ref,
       "нет проверки vla-get-IsDynamicBlock")

expect("исходное определение проверяется после копии",
       "(tblsearch \"BLOCK\" old-name)" in copy_block,
       "нет проверки, что исходный блок остался")
expect("о потере исходного блока сообщается",
       "исчезло из чертежа" in copy_block,
       "нет сообщения о пропаже исходного определения")
expect("копия входит в одну группу UNDO",
       "tu-undo-begin" in copy_block and "tu-undo-end" in copy_block,
       "нет undo-группы")
expect("сбой копирования не оставляет молчания",
       "не создана" in copy_block,
       "нет сообщения о сбое")

# ---------------------------------------------------------------
if FAILURES:
    print("ОШИБКИ:")
    for failure in FAILURES:
        print("  -", failure)
    print(f"PASS: {PASS}, FAIL: {FAIL}")
    raise SystemExit(1)

print("копия: определение через ObjectDBX, переименование до возврата")
print("исходный блок не переименовывается; WBLOCK не используется")
print(f"PASS: {PASS}, FAIL: {FAIL}")
print("[BLOCKCOPY][OK]")
