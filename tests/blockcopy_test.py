# -*- coding: utf-8 -*-
"""BLOCKCOPY / КОПИЯБЛОКА: порт логики копирования блока.

Проверяет контракт blockrename-copy-block (Extraction/blockrename.lsp):

 1. каталог для временного файла выбирается по факту записи:
    TEMPPREFIX -> TEMP -> TMP -> папка чертежа -> Мои документы;
 2. WBLOCK считается успешным только при файле ненулевого размера;
 3. если WBLOCK не сработал, копия строится напрямую клоном определения,
    а причина печатается, а не пропадает;
 4. entmake-клон не переносит дескриптор, владельца, словари и XDATA.

Порт держится за исходник: список кодов для клона и точка отсечения
XDATA вычитываются прямо из .lsp, поэтому правка кода без правки теста
даёт расхождение.

Почему появился: пользователь получил «Не удалось создать временный файл
для блока "Козырёк Кз-2"» без причины - старый код игнорировал результат
-WBLOCK и проверял только findfile.
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


# ---------------------------------------------------------------
# Исходник: коды, которые вырезает клон
# ---------------------------------------------------------------
def lisp_text(path: Path) -> str:
    raw = path.read_bytes()
    try:
        return raw.decode("utf-8")
    except UnicodeDecodeError:
        return raw.decode("cp1251")


text = lisp_text(SOURCE)


def defun_body(name: str) -> str:
    """Тело (defun name ...) до следующей формы верхнего уровня."""
    pattern = re.compile(r"\(defun\s+" + re.escape(name) + r"[\s(]")
    found = pattern.search(text)
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


clone_body = defun_body("blockrename-clone-data")
match = re.search(r"'\(([\d\s]+)\)", clone_body)
dropped = {int(code) for code in match.group(1).split()} if match else set()
stop_match = re.search(r"\(= \(car pair\) (\d+)\)", clone_body)
stop_code = int(stop_match.group(1)) if stop_match else -1

expect("коды клона читаются из исходника", bool(dropped), "не найден список кодов")
expect("точка отсечения XDATA читается из исходника", stop_code > 0, "не найден код 1001")

# ---------------------------------------------------------------
# Порт 1: выбор каталога
# ---------------------------------------------------------------
def pick_dir(candidates: list[str], writable: set[str]) -> str | None:
    for candidate in candidates:
        if not candidate:
            continue
        if candidate in writable:
            return candidate
    return None


cands = ["", "Z:\\нет-такого\\", "C:\\Temp\\", "C:\\Чертёж\\"]
expect("пустой кандидат пропускается",
       pick_dir(cands, {"C:\\Temp\\", "C:\\Чертёж\\"}) == "C:\\Temp\\")
expect("нерабочий TEMPPREFIX пропускается",
       pick_dir(cands, {"C:\\Чертёж\\"}) == "C:\\Чертёж\\")
expect("нет ни одного рабочего каталога",
       pick_dir(cands, set()) is None)

# ---------------------------------------------------------------
# Порт 2: классификация результата WBLOCK
# ---------------------------------------------------------------
def wblock_result(exists: bool, size: int, error: str | None) -> tuple[str, str]:
    if exists and size > 0:
        return ("OK", "ok")
    if error:
        return ("ERROR", error)
    return ("ERROR", "файл не создан")


expect("WBLOCK с файлом ненулевого размера - успех",
       wblock_result(True, 2048, None)[0] == "OK")
expect("пустой файл после сбойной команды - не успех",
       wblock_result(True, 0, "Function cancelled")[0] == "ERROR")
expect("причина из команды доходит до вызывающего",
       wblock_result(False, 0, "Неверное имя файла")[1] == "Неверное имя файла")
expect("без ошибки команды причина называет файл",
       "файл не создан" in wblock_result(False, 0, None)[1])

# ---------------------------------------------------------------
# Порт 3: фильтр данных для entmake-клона
# ---------------------------------------------------------------
def clone_data(data: list[tuple[int, object]]) -> list[tuple[int, object]]:
    out: list[tuple[int, object]] = []
    stop = False
    for pair in data:
        code = pair[0]
        if stop:
            continue
        if code == stop_code:
            stop = True
            continue
        if code in dropped:
            continue
        out.append(pair)
    return out


sample = [
    (0, "INSERT"), (5, "2A7"), (330, "1F"), (100, "AcDbEntity"),
    (8, "0"), (100, "AcDbBlockReference"), (2, "Козырёк Кз-2"),
    (10, (0.0, 0.0, 0.0)), (360, "2B1"), (102, "{ACAD_XDICTIONARY"),
    (1001, "PLUGIN"), (1000, "данные"),
]
result = clone_data(sample)
codes = [code for code, _ in result]
expect("дескриптор объекта не переносится", 5 not in codes)
expect("владелец не переносится", 330 not in codes)
expect("словарь не переносится", 102 not in codes and 360 not in codes)
expect("XDATA обрезается целиком", 1001 not in codes and 1000 not in codes)
expect("тип и слой объекта сохранены", 0 in codes and 8 in codes)
expect("подклассы AcDb сохранены", codes.count(100) == 2)
expect("имя вложенного блока сохранено", (2, "Козырёк Кз-2") in result)

# ---------------------------------------------------------------
# Связь порта с кодом
# ---------------------------------------------------------------
copy_body = defun_body("blockrename-copy-block")

expect("каталог выбирается проверкой записи",
       "ae_probe_" in defun_body("blockrename-writable-dir"),
       "в blockrename-writable-dir нет пробного файла")
expect("WBLOCK идёт через command-s с резервом command",
       "command-s" in defun_body("blockrename-wblock"),
       "в blockrename-wblock нет command-s")
expect("размер файла проверяется, а не только findfile",
       "vl-file-size" in defun_body("blockrename-wblock"),
       "в blockrename-wblock нет vl-file-size")
expect("при сбое WBLOCK строится прямой клон",
       "blockrename-clone-block old-name new-name" in copy_body,
       "blockrename-copy-block не зовёт blockrename-clone-block")
expect("причина сбоя печатается в командную строку",
       "WBLOCK не сработал" in copy_body,
       "нет сообщения о причине сбоя WBLOCK")
expect("в диалоге сбоя есть причина",
       "(ex-safe-message res)" in copy_body,
       "alert не показывает причину")
expect("временный файл удаляется после копирования",
       "vl-file-delete tempFile" in copy_body,
       "нет удаления временного файла")
expect("копия входит в одну группу UNDO",
       "tu-undo-begin" in copy_body and "tu-undo-end" in copy_body,
       "нет undo-группы")

# ---------------------------------------------------------------
if FAILURES:
    print("ОШИБКИ:")
    for failure in FAILURES:
        print("  -", failure)
    print(f"PASS: {PASS}, FAIL: {FAIL}")
    raise SystemExit(1)

print("каталог: проверка записи; WBLOCK: command-s и размер файла")
print("сбой WBLOCK: прямой клон + причина в логе")
print(f"PASS: {PASS}, FAIL: {FAIL}")
print("[BLOCKCOPY][OK]")
