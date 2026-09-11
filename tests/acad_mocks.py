# -*- coding: utf-8 -*-
"""
acad_mocks.py — mock-объекты AutoCAD для тестирования модулей AutoExtraction
вне AutoCAD (Этап 5 — тестирование).

Предоставляет:
  AcadContext          — окружение: вывод, входной поток, диалоги, файлы
  EntityDatabase       — entget/entmake/entnext/ename<->vla
  MockDocument / MockModelSpace / MockLayers / MockBlocks — vlax-иерархия
  MockBlockRef         — dynamic block properties (GetDynamicBlockProperties)
  DclDriver            — выполнение DCL-диалогов по текстовому сценарию
  SelectionSet         — ssget/ssname/sslength
"""

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from autolisp import (  # noqa: E402
    Sym, Pair, Variant, LispError, FileHandle, read_all,
)


def prop(sym, val):
    s = str(sym)
    key = int(s) if s.lstrip("+-").isdigit() else Sym(s)
    return Pair(key, val)


class MockOutput:
    """Накопитель консольного вывода (princ/prompt/write-line)."""

    def __init__(self):
        self.parts = []

    def write(self, text):
        self.parts.append(text)

    @property
    def text(self):
        return "".join(self.parts)

    def lines(self):
        return self.text.split("\n")

    def contains(self, sub):
        return sub in self.text

    def find_line(self, sub):
        return [l for l in self.lines() if sub in l]


# ------------------------------------------------------------------
# vlax-объекты
# ------------------------------------------------------------------

class MockApp:
    lisp_type = "VLA-OBJECT"

    def __init__(self, doc):
        self.activedocument = doc


class MockDocument:
    lisp_type = "VLA-OBJECT"

    def __init__(self, ctx):
        self._ctx = ctx
        self.modelspace = MockModelSpace(ctx)
        self.layers_obj = MockLayers(ctx)
        self.blocks_obj = MockBlocks(ctx)

    def layers(self):
        return self.layers_obj

    def blocks(self):
        return self.blocks_obj

    @property
    def activeselectionset(self):
        return self._ctx.active_selection

    def getvariable(self, name):
        return self._ctx.getvar(str(name))

    def startundomark(self):
        self._ctx.undo_open += 1

    def endundomark(self):
        self._ctx.undo_open -= 1
        self._ctx.undo_closed += 1


class MockModelSpace:
    lisp_type = "VLA-OBJECT"

    def __init__(self, ctx):
        self._ctx = ctx

    def addmtext(self, pt, width, text):
        if isinstance(pt, Variant):
            pt = pt.value
        vals = pt if isinstance(pt, list) else list(pt)
        ent = self._ctx.db.add_entity("MTEXT", [
            prop(10, list(vals[:3]) + [0.0] * (3 - len(vals))),
            prop(1, text),
            prop(40, 2.5),
        ])
        return self._ctx.db.ename_to_vla(ent)

    def addline(self, p1, p2):
        ent = self._ctx.db.add_entity("LINE", [prop(10, p1), prop(11, p2)])
        return self._ctx.db.ename_to_vla(ent)

    def item(self, i):
        return self._ctx.db.vla_items()[int(i)]

    @property
    def count(self):
        return len(self._ctx.db.vla_items())

    def delete(self):
        pass


class LayerEntry:
    lisp_type = "VLA-OBJECT"

    def __init__(self, name):
        self.name = str(name)
        self.color = 7


class MockLayers:
    """Коллекция слоёв: doc.Layers().Item(...) и итерация vlax-for."""

    lisp_type = "VLA-OBJECT"

    def __init__(self, ctx):
        self._ctx = ctx
        self._extra = {}

    def item(self, name):
        lname = str(name)
        entry = LayerEntry(lname)
        self._ctx.db.layer_colors[lname] = entry
        self._extra[lname] = entry
        return entry

    def __iter__(self):
        names = list(self._ctx.layer_index)
        for n in self._extra:
            if n not in names:
                names.append(n)
        return iter([self.item(n) for n in names])


class BlockDef:
    lisp_type = "VLA-OBJECT"

    def __init__(self, name):
        self.name = name


class MockBlocks:
    lisp_type = "VLA-OBJECT"

    def __init__(self, ctx):
        self._ctx = ctx

    def item(self, name):
        return BlockDef(str(name))


class DynProp:
    """Запись динамического свойства блока (как элемент safearray из
    GetDynamicBlockProperties)."""

    def __init__(self, blk, name, value):
        self._blk = blk
        self.propertyname = name
        self.value = Variant(value)
        self.allowedvalues = Variant([])

    def setdynprop(self, v):
        self._blk.dynprops[self.propertyname] = v
        return None


class MockBlockRef:
    lisp_type = "VLA-OBJECT"

    def __init__(self, ctx, name, input_pt, ename=None):
        self._ctx = ctx
        self.effectivename = str(name)
        self.name = str(name)
        self.dynprops = {}
        self.ename = ename
        self.isdynamicblock = True
        ins = input_pt if isinstance(input_pt, list) else list(input_pt)
        while len(ins) < 3:
            ins.append(0.0)
        self._insert = ins[:3]

    @property
    def valuename(self):
        return self.name

    def insert(self, *args):
        return self

    def insertionpoint(self):
        return Variant(list(self._insert))

    def rotation(self):
        return 0.0

    def scalex(self):
        return 1.0

    def getdynamicblockproperties(self):
        """Возвращает safearray COM-объектов с PropertyName/Value."""
        names = self._ctx.default_dynprops(self.effectivename)
        arr = [DynProp(self, n, self.dynprops.get(n)) for n in names]
        return Variant(arr)

    def addmtext(self, pt, width, text):
        if isinstance(pt, Variant):
            pt = pt.value
        vals = pt if isinstance(pt, list) else list(pt)
        ent = self._ctx.db.add_entity("MTEXT", [
            prop(10, list(vals[:3]) + [0.0] * (3 - len(vals))),
            prop(1, text),
            prop(40, 2.5),
        ])
        return self._ctx.db.ename_to_vla(ent)


class FilletResult:
    """vla-объект, возвращаемый vla-cmdf '_.FILLET' если бы создавался."""

    lisp_type = "VLA-OBJECT"


# ------------------------------------------------------------------
# База примитивов (entget / entmake)
# ------------------------------------------------------------------

class EntityDatabase:
    def __init__(self, ctx):
        self._ctx = ctx
        self.entities = {}          # ename -> list[Pair]
        self.counter = 0
        self.order = []             # порядок создания
        self.layer_colors = {}      # layer -> LayerEntry (мок цветов)
        self._vla_cache = {}

    def _next_ename(self):
        self.counter += 1
        return Sym("<Entity name: %X>" % (0x60000000 + self.counter))

    def add_entity(self, typ, pairs):
        ename = self._next_ename()
        data = [prop(0, typ)]
        data += pairs
        self.entities[ename] = data
        self.order.append(ename)
        return ename

    def entget(self, ename):
        return self.entities.get(ename)

    def entlast(self):
        if self.order:
            return self.order[-1]
        return None

    def entnext(self, ename=None):
        if ename is None:
            return self.order[0] if self.order else None
        try:
            idx = self.order.index(ename)
        except ValueError:
            return None
        if idx + 1 < len(self.order):
            return self.order[idx + 1]
        return None

    def entmake(self, data, return_name=False):
        if not isinstance(data, list) or not data:
            return None
        typ = None
        for p in data:
            if isinstance(p, Pair) and isinstance(p.car, Sym) and int(str(p.car)) == 0:
                typ = p.cdr
        if not typ:
            return None
        ename = self._next_ename()
        self.entities[ename] = list(data)
        self.order.append(ename)
        return ename if return_name else Sym("T")

    def entdel(self, ename):
        self.entities.pop(ename, None)
        if ename in self.order:
            self.order.remove(ename)

    def ename_to_vla(self, ename):
        if ename not in self._vla_cache:
            self._vla_cache[ename] = MockEntityVla(self, ename)
        return self._vla_cache[ename]

    def vla_to_ename(self, obj):
        if isinstance(obj, MockEntityVla):
            return obj.ename
        return getattr(obj, "ename", None)

    def vla_items(self):
        return [self.ename_to_vla(e) for e in self.order]

    @property
    def layers(self):
        return [name for name in self._ctx.layer_index]


class MockEntityVla:
    lisp_type = "VLA-OBJECT"

    def __init__(self, db, ename):
        self._db = db
        self.ename = ename
        self.deleted = False

    def _get(self, code):
        data = self._db.entities.get(self.ename, [])
        for p in data:
            if isinstance(p, Pair) and int(str(p.car)) == code:
                return p.cdr
        return None

    def delete(self):
        self.deleted = True
        self._db.entdel(self.ename)

    def copy(self):
        return MockEntityVla(self._db, self.ename)

    def startpoint(self):
        v = self._get(10)
        return Variant(v if isinstance(v, list) else [0.0, 0.0, 0.0])

    def endpoint(self):
        v = self._get(11)
        return Variant(v if isinstance(v, list) else [0.0, 0.0, 0.0])

    @property
    def length(self):
        p1 = self.startpoint().value
        p2 = self.endpoint().value
        import math
        return math.sqrt(sum((b - a) ** 2 for a, b in zip(p1, p2)))


# ------------------------------------------------------------------
# Наборы выбора (ssget)
# ------------------------------------------------------------------

class SelectionSet:
    def __init__(self, ctx, enames=None):
        self._ctx = ctx
        self._enames = list(enames) if enames else []

    def length(self):
        return len(self._enames)

    def name(self, i):
        if 0 <= i < len(self._enames):
            return self._enames[i]
        return None

    def add(self, ename):
        self._enames.append(ename)
        return ename

    def append_name(self, ename):
        self._enames.append(ename)

    def delete(self):
        self._enames = []

    def remove(self, ename):
        if ename in self._enames:
            self._enames.remove(ename)

    def iter(self):
        return list(self._enames)


# ------------------------------------------------------------------
# DCL-движок
# ------------------------------------------------------------------

class DclDriver:
    """Проигрывание сценария диалога.

    ctx.dialog_script(): вызывается из start_dialog; пункты вида:
      ("click", "btn_clad")     — выполнить action-выражение тайла
      ("set", "key", "value")   — просто установить значение тайла
    Без entry ("" по умолчанию) — синхронно выполнить done_dialog(0).

    Хранит значения тайлов ($value) и исполняет lisp-выражения action_tile
    на уровне интерпретатора (с попаданием setq-переменных в глобалы).
    """

    def __init__(self, ctx):
        self.ctx = ctx
        self.tiles = {}
        self.actions = {}
        self.list_stack = []
        self.current_list = None
        self.log = []

    def action(self, key, expr):
        self.actions[key] = expr

    def set_tile(self, key, value):
        self.tiles[key] = value

    def get_tile(self, key):
        return self.tiles.get(key)

    def mode(self, key, m):
        self.log.append(("mode", key, m))

    def start_list(self, key):
        self.current_list = (key, [])
        self.list_stack.append(self.current_list)

    def add_list(self, item):
        if self.list_stack:
            self.list_stack[-1][1].append(item)
        self.set_tile("list_" + (self.current_list[0] if self.current_list else "?"),
                      ",".join(self.list_stack[-1][1]) if self.list_stack else "")

    def end_list(self):
        if self.list_stack:
            self.list_stack.pop()
        self.current_list = self.list_stack[-1] if self.list_stack else None

    def click(self, key):
        """Выполняет action-выражение тайла. LispExit/вложенный done_dialog
        пробрасываются. Синхронный done_dialog(status) перехватывается и
        возвращается как статус диалога."""
        expr = self.actions.get(key)
        self.log.append(("click", key))
        if expr is None:
            return None
        from autolisp import DoneDialog, LispExit
        interp = self.ctx.interp
        forms = read_all(expr)
        result = None
        try:
            for form in forms:
                result = interp.eval(form)
        except DoneDialog as dd:
            self.ctx._pending_dcl_status = dd.status
            return None
        except LispExit:
            raise
        return result


# ------------------------------------------------------------------
# Входной поток (getstring / getkword / getreal / getpoint)
# ------------------------------------------------------------------

class InputStream:
    """Очередь ответов пользователя. Каждая запись — кортеж вида:
    ("string", "7") | ("kword", "Э") | ("point", [x,y,z]) | ("real", 5.0)
    | ("enter", None)  -> nil
    Функции get* изымают следующую запись и проверяют тип.
    """

    def __init__(self):
        self.queue = []

    def push(self, *items):
        self.queue.extend(items)

    def next(self, kind):
        if not self.queue:
            raise LispError("input stream empty (ожидался %s)" % kind)
        k, v = self.queue.pop(0)
        if k == "enter":
            return None
        if k == kind:
            return v
        if kind == "real" and k == "point":
            raise LispError("в input-потоке point, а запрошен real")
        return v


# ------------------------------------------------------------------
# Виртуальная ФС
# ------------------------------------------------------------------

class VirtualFS:
    """Реальные файлы в изолированной директории (tmp), CP1251."""

    def __init__(self, root):
        self.root = root
        os.makedirs(root, exist_ok=True)

    def abs(self, path):
        p = str(path).replace("\\", "/")
        if os.path.isabs(p) or p.startswith(self.root):
            return p
        return os.path.join(self.root, p)

    def open(self, path, mode):
        real = self.abs(path)
        return FileHandle(real, mode.lower())

    search_dirs = ()

    def findfile(self, path):
        p = str(path).replace("\\", "/")
        candidates = [p, os.path.join(self.root, p)]
        if not os.path.isabs(p):
            base = os.path.basename(p)
            for d in getattr(self, "search_dirs", ()):
                candidates.append(os.path.join(d, p))
                candidates.append(os.path.join(d, base))
        for c in candidates:
            if os.path.isfile(c):
                return os.path.abspath(c)
        if os.path.isfile(str(path)):
            return os.path.abspath(str(path))
        return None

    def read_text(self, path):
        real = self.abs(path)
        with open(real, "rb") as fh:
            return fh.read().decode("cp1251")


# ------------------------------------------------------------------
# Главный контекст
# ------------------------------------------------------------------

class AcadContext:
    def __init__(self, interp, tmpdir):
        self.interp = interp
        interp.ctx = self
        self.out = MockOutput()
        self.input = InputStream()
        self.fs = VirtualFS(tmpdir)
        self.db = EntityDatabase(self)
        self.dcl = DclDriver(self)
        self.vars = {
            "FILEDIA": 1,
            "CMDECHO": 1,
            "DWGNAME": "Test.dwg",
            "DWGPREFIX": str(tmpdir).replace("\\", "/") + "/",
        }
        self.alerts = []
        self.commands = []
        self.doc = MockDocument(self)
        self.app = MockApp(self.doc)
        self.locked_files = []      # имена файлов, считающихся заблокированными
        self.active_selection = Sym("nil")
        self.layer_index = {}       # имя слоя (любой регистр) -> int
        self._pending_dcl_status = None
        self.dialog_steps = []      # сценарий текущего диалога
        self.getfiled_queue = []
        self.pickfirst = []
        self.undo_open = 0
        self.undo_closed = 0
        self.default_dynprops_map = {}
        self.command_log = []

    # ---------- вывод ----------
    def out_write(self, text):
        self.out.write(text)

    # ---------- ввод ----------
    def next_input_string(self, allow_spaces=False):
        v = self.input.next("string")
        # AutoCAD: getstring по пустому Enter возвращает "", а не nil
        return "" if v is None else v

    def next_input_kword(self):
        return self.input.next("kword")

    def next_input_point(self):
        return self.input.next("point")

    def next_input_real(self):
        return self.input.next("real")

    def initget(self, *args):
        pass

    def next_getfiled(self, default, ext):
        if self.getfiled_queue:
            return self.getfiled_queue.pop(0)
        base = os.path.splitext(default)[0] if default else "output"
        return os.path.join(self.fs.root, base + "." + ext)

    def alert(self, msg):
        self.alerts.append(msg)

    def command(self, *args):
        self.command_log.append(tuple(args))
        return None

    # ---------- переменные ----------
    def getvar(self, name):
        return self.vars.get(str(name).upper())

    def setvar(self, name, value):
        self.vars[str(name).upper()] = value
        return value

    # ---------- ssget ----------
    def ssget(self, *args):
        """ssget с фильтрами: режим "_P"/"_X"/"_I" или императивный список
        кодов. Возвращает SelectionSet (или None если пусто). В тестах набор
        задаётся через db-фикстуру/предвыбор."""
        mode = None
        filters = []
        for a in args:
            if a is None:
                continue
            if isinstance(a, str) and a.startswith("_"):
                mode = a.upper()
            elif isinstance(a, list):
                filters.append(a)
            elif isinstance(a, Sym):
                pass
        enames = []
        if mode == "_I":
            return SelectionSet(self, self.pickfirst) if self.pickfirst else None
        if mode == "_P":
            return SelectionSet(self, self.pickfirst) if self.pickfirst else None
        if mode == "_X":
            enames = list(self.db.order)
        elif mode is None and filters:
            # интерактивный ssget с фильтром — в тестах берётся getfiled-подобно
            # из очереди scripted selection, если заполнена
            if self.scripted_selection:
                return self.scripted_selection
            return None
        else:
            enames = list(self.db.order)
        enames = [e for e in self._filter(enames, filters)]
        return SelectionSet(self, enames) if enames else None

    def _filter(self, enames, filters):
        out = []
        for e in enames:
            data = self.db.entities.get(e) or []
            ok = True
            for f in filters:
                if not isinstance(f, Pair):
                    continue
                code = int(str(f.car))
                want = f.cdr
                have = None
                for p in data:
                    if isinstance(p, Pair) and int(str(p.car)) == code:
                        have = p.cdr
                        break
                if code == 0 and isinstance(want, str) and isinstance(have, str):
                    names = [s.strip().upper() for s in want.split(",")]
                    if have.upper() not in names:
                        ok = False
                elif code in (8, 410):
                    pass  # фильтр по слою/пространству в ssget не применяем строго
                else:
                    if isinstance(want, Sym) or isinstance(have, Sym):
                        if str(want) != str(have):
                            ok = False
                    elif isinstance(want, str) and isinstance(have, str):
                        from autolisp import _split_wcmatch, _wcmatch_regex
                        import re as _re
                        matched = False
                        for alt in _split_wcmatch(want):
                            if _re.fullmatch(_wcmatch_regex(alt), have, _re.IGNORECASE):
                                matched = True
                        if not matched:
                            ok = False
                    elif want != have:
                        ok = False
                if not ok:
                    break
            if ok:
                out.append(e)
        return out

    scripted_selection = None

    def curve_length(self, ent):
        """Длина примитива по данным entget (LINE / LWPOLYLINE)."""
        data = self.db.entities.get(ent) or []
        typ = None
        pts = []
        for p in data:
            if isinstance(p, Pair):
                code = int(str(p.car))
                if code == 0:
                    typ = p.cdr
                elif code == 10:
                    pts.append(list(p.cdr)[:2])
                elif code == 11:
                    pts.append(list(p.cdr)[:2])
        if typ == "LINE" and len(pts) >= 2:
            import math
            (x1, y1), (x2, y2) = pts[0], pts[1]
            return math.hypot(x2 - x1, y2 - y1)
        if typ in ("LWPOLYLINE", "POLYLINE") and len(pts) >= 2:
            import math
            return sum(math.hypot(b[0] - a[0], b[1] - a[1])
                       for a, b in zip(pts, pts[1:]))
        if pts:
            return 0.0
        return 0.0

    def ssadd(self, *args):
        """AutoLISP ssadd: (ssadd) -> пустой набор; (ssadd ent set) ->
        ДОБАВЛЯЕТ ent в set на месте и возвращает set."""
        target = None
        ents = []
        for a in args:
            if isinstance(a, SelectionSet):
                target = a
            elif a is not None:
                ents.append(a)
        if target is None:
            target = SelectionSet(self)
        for e in ents:
            target.append_name(e)
        return target

    def ename_to_vla(self, ename):
        return self.db.ename_to_vla(ename)

    def default_dynprops(self, name):
        return list(self.default_dynprops_map.get(str(name), []))

    # ---------- DCL ----------
    def dcl_load(self, path):
        return 1

    def dcl_new(self, name):
        return Sym("T")

    def dcl_start(self):
        from autolisp import DoneDialog, LispExit
        self._pending_dcl_status = 0
        try:
            for step in self.dialog_steps:
                kind = step[0]
                if kind == "click":
                    self.dcl.click(step[1])
                elif kind == "set":
                    self.dcl.set_tile(step[1], step[2])
        except LispExit:
            raise
        finally:
            self.dialog_steps = []
        status = self._pending_dcl_status
        self._pending_dcl_status = None
        return status

    def dcl_action(self, key, expr):
        self.dcl.action(key, expr)

    def dcl_set_tile(self, key, value):
        self.dcl.set_tile(key, value)

    def dcl_get_tile(self, key):
        return self.dcl.get_tile(key)

    def dcl_mode(self, key, mode):
        self.dcl.mode(key, mode)

    def dcl_start_list(self, key):
        self.dcl.start_list(key)

    def dcl_add_list(self, item):
        self.dcl.add_list(item)

    def dcl_end_list(self):
        self.dcl.end_list()

    # ---------- ФС ----------
    def fs_open(self, path, mode):
        # AutoLISP-семантика: при неудаче open возвращает nil, а не ошибку
        base = os.path.basename(str(path).replace("\\", "/"))
        if base in self.locked_files and mode.lower() in ("w", "a"):
            return None
        try:
            return self.fs.open(path, mode)
        except OSError:
            return None

    def fs_findfile(self, path):
        return self.fs.findfile(path)

    # ---------- хелперы для тестов ----------
    def add_layer(self, name):
        self.layer_index[str(name)] = len(self.layer_index)

    def set_inputs(self, *items):
        self.input.push(*items)

    def dialog_script(self, *steps):
        self.dialog_steps.extend(steps)

    def set_pickfirst(self, enames):
        self.pickfirst = list(enames)

    def lock_file(self, path):
        self.locked_files.append(os.path.basename(str(path).replace("\\", "/")))

    def unlock_file(self, path):
        base = os.path.basename(str(path).replace("\\", "/"))
        if base in self.locked_files:
            self.locked_files.remove(base)

    @property
    def selection_property(self):
        return self.scripted_selection

    @selection_property.setter
    def selection_property(self, v):
        self.scripted_selection = v
