# -*- coding: utf-8 -*-
"""
autolisp.py — минимальный интерпретатор диалекта AutoLISP (Visual LISP),
достаточный для загрузки и выполнения модулей AutoExtraction
(common/*.lsp, Extraction/*.lsp) вне AutoCAD.

Используется исключительно в тестовой среде (Этап 5 — тестирование).
AutoCAD-специфичные функции (ssget, vla-*, DCL и т.п.) делегируются
в mock-контекст (tests/acad_mocks.py).

Представление значений:
  nil        -> None
  T          -> Sym('T')
  символ     -> Sym (строка, interned, UPPERCASE)
  строка     -> str
  список     -> list
  dotted pair-> Pair
  числа      -> int / float (AutoLISP-семантика целочисленного деления)

Важно: в AutoLISP пустой список () тождествен nil — ридер и встроенные
функции нормализуют пустые списки в None.
"""

import re
import math
import os
from decimal import Decimal, ROUND_HALF_UP


# ------------------------------------------------------------------
# Базовые типы
# ------------------------------------------------------------------

class Sym(str):
    _cache = {}

    def __new__(cls, name):
        name = name.upper()
        if name not in cls._cache:
            cls._cache[name] = str.__new__(cls, name)
        return cls._cache[name]

    def __repr__(self):
        return str(self)


class Pair:
    __slots__ = ("car", "cdr")

    def __init__(self, car, cdr):
        self.car = car
        self.cdr = cdr

    def __repr__(self):
        return "(%s . %s)" % (lisp_prin1(self.car), lisp_prin1(self.cdr))


class Lambda:
    __slots__ = ("params", "locals", "body")

    def __init__(self, params, locals_, body):
        self.params = params
        self.locals = locals_
        self.body = body


class UserFunction:
    __slots__ = ("name", "params", "locals", "body")

    def __init__(self, name, params, locals_, body):
        self.name = name
        self.params = params
        self.locals = locals_
        self.body = body


class CatchAllError:
    """Аналог объекта ошибки vl-catch-all-apply."""

    def __init__(self, message):
        self.message = message

    def __repr__(self):
        return "#<catch-all-error: %s>" % self.message


class Variant:
    __slots__ = ("value",)
    lisp_type = "VARIANT"

    def __init__(self, value):
        self.value = value


class LispError(Exception):
    pass


class LispExit(Exception):
    pass


class DoneDialog(Exception):
    def __init__(self, status):
        self.status = status


# ------------------------------------------------------------------
# Печать значений
# ------------------------------------------------------------------

def lisp_prin1(x):
    if x is None:
        return "nil"
    if isinstance(x, Pair):
        return repr(x)
    if isinstance(x, Sym):
        return str(x)
    if isinstance(x, str):
        return '"%s"' % x.replace("\\", "\\\\").replace('"', '\\"')
    if isinstance(x, bool):
        return "T" if x else "nil"
    if isinstance(x, float):
        if x == int(x) and abs(x) < 1e15:
            return ("%f" % x)
        return repr(x)
    if isinstance(x, int):
        return str(x)
    if isinstance(x, list):
        return "(" + " ".join(lisp_prin1(i) for i in x) + ")"
    return repr(x)


def lisp_princ(x):
    if x is None:
        return ""
    if isinstance(x, Sym):
        return str(x)
    if isinstance(x, str):
        return x
    return lisp_prin1(x)


def truthy(x):
    return x is not None


def value_equal(a, b):
    if a == [] and b is None or b == [] and a is None:
        return True
    if a is None or b is None:
        return a is None and b is None
    if isinstance(a, Pair) and isinstance(b, Pair):
        return value_equal(a.car, b.car) and value_equal(a.cdr, b.cdr)
    if isinstance(a, list) and isinstance(b, list):
        return len(a) == len(b) and all(value_equal(x, y) for x, y in zip(a, b))
    if isinstance(a, Sym) and isinstance(b, Sym):
        return str(a) == str(b)
    if isinstance(a, (int, float)) and isinstance(b, (int, float)):
        return a == b
    if type(a) is str and type(b) is str:
        return a == b
    return a is b


# ------------------------------------------------------------------
# Ридер (токенайзер + парсер)
# ------------------------------------------------------------------

_TOKEN_RE = re.compile(r"""
    (?P<ws>[\s,]+)
  | (?P<comment>;[^\n]*)
  | (?P<string>"(?:[^"\\]|\\.)*")
  | (?P<quote>')
  | (?P<lpar>\()
  | (?P<rpar>\))
  | (?P<atom>[^\s()";]+)
""", re.VERBOSE)


_NUM_INT_RE = re.compile(r"^[+-]?\d+$")
_NUM_FLOAT_RE = re.compile(r"^[+-]?(\d+\.\d*|\.\d+|\d+)([eE][+-]?\d+)?$")
_NUM_FLOAT_FORCE = re.compile(r"[.eE]")


def tokenize(src):
    pos = 0
    tokens = []
    n = len(src)
    while pos < n:
        m = _TOKEN_RE.match(src, pos)
        if not m:
            raise LispError("reader: unexpected character %r at %d" % (src[pos], pos))
        pos = m.end()
        kind = m.lastgroup
        text = m.group(0)
        if kind in ("ws", "comment"):
            continue
        if kind == "string":
            body = text[1:-1]
            out = []
            i = 0
            while i < len(body):
                ch = body[i]
                if ch == "\\" and i + 1 < len(body):
                    nxt = body[i + 1]
                    mapping = {"n": "\n", "r": "\r", "t": "\t", "\\": "\\", '"': '"', "e": "\x1b"}
                    out.append(mapping.get(nxt, nxt))
                    i += 2
                else:
                    out.append(ch)
                    i += 1
            tokens.append(("str", "".join(out)))
        elif kind == "quote":
            tokens.append(("quote", None))
        elif kind == "lpar":
            tokens.append(("lpar", None))
        elif kind == "rpar":
            tokens.append(("rpar", None))
        else:
            tokens.append(("atom", text))
    return tokens


class _Reader:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0

    def read(self):
        if self.pos >= len(self.tokens):
            raise LispError("reader: unexpected EOF")
        kind, val = self.tokens[self.pos]
        self.pos += 1
        if kind == "quote":
            return [Sym("QUOTE"), self.read()]
        if kind == "str":
            return val
        if kind == "lpar":
            items = []
            while True:
                if self.pos >= len(self.tokens):
                    raise LispError("reader: missing )")
                k, v = self.tokens[self.pos]
                if k == "rpar":
                    self.pos += 1
                    # AutoLISP: пустой список () тождествен nil
                    return items if items else None
                if k == "atom" and v == ".":
                    # dotted pair
                    self.pos += 1
                    cdr = self.read()
                    if self.pos >= len(self.tokens) or self.tokens[self.pos][0] != "rpar":
                        raise LispError("reader: bad dotted pair")
                    self.pos += 1
                    if len(items) != 1:
                        raise LispError("reader: dotted pair with several cars")
                    return Pair(items[0], cdr)
                items.append(self.read())
        if kind == "atom":
            if _NUM_INT_RE.match(val):
                try:
                    return int(val)
                except ValueError:
                    pass
            if _NUM_FLOAT_RE.match(val) and _NUM_FLOAT_FORCE.search(val):
                try:
                    return float(val)
                except ValueError:
                    pass
            if val == "nil":
                return None
            return Sym(val)
        raise LispError("reader: unexpected token")


def read_all(src):
    rd = _Reader(tokenize(src))
    forms = []
    while rd.pos < len(rd.tokens):
        forms.append(rd.read())
    return forms


def read_one(s):
    rd = _Reader(tokenize(s))
    if rd.pos >= len(rd.tokens):
        return None
    return rd.read()


# ------------------------------------------------------------------
# Интерпретатор
# ------------------------------------------------------------------

class Interpreter:
    def __init__(self, ctx=None):
        self.globals = {}                 # имя -> значение
        self.global_bound = set()         # имена, которым когда-либо присваивали
        self.functions = {}               # имя -> UserFunction
        self.builtins = self._make_builtins()
        self.frames = []                  # стек dict (динамическая область)
        self.ctx = ctx                    # mock-контекст AutoCAD (acad_mocks)
        self.loaded_files = set()

    # ---------------- загрузка файлов ----------------

    def load_file_text(self, src, filename="<string>"):
        forms = read_all(src)
        result = None
        for form in forms:
            result = self.eval(form)
        return result

    def load_file(self, path):
        with open(path, "rb") as fh:
            raw = fh.read()
        src = raw.decode("cp1251")
        return self.load_file_text(src, path)

    # ---------------- области видимости ----------------

    def lookup(self, name):
        for frame in reversed(self.frames):
            if name in frame:
                return frame[name]
        if name in self.globals:
            return self.globals[name]
        # Как в AutoLISP: символ функции вычисляется в сам объект функции
        # (иначе (type blockrename-init) падал бы с "unbound variable")
        if name in self.functions:
            return self.functions[name]
        if name in self.builtins:
            return self.builtins[name]
        raise LispError("unbound variable: %s" % name)

    def assign(self, name, value):
        for frame in reversed(self.frames):
            if name in frame:
                frame[name] = value
                return value
        self.globals[name] = value
        self.global_bound.add(name)
        return value

    # ---------------- вычисление ----------------

    def eval(self, x):
        while True:
            if x is None or isinstance(x, (int, float, str)) and not isinstance(x, Sym):
                return x
            if isinstance(x, Sym):
                if str(x) == "T":
                    return x
                return self.lookup(x)
            if isinstance(x, Pair):
                raise LispError("cannot eval dotted pair")
            if not isinstance(x, list):
                return x
            if len(x) == 0:
                return None

            head = x[0]
            if isinstance(head, Sym):
                hname = str(head)
                handler = self.specials.get(hname)
                if handler is not None:
                    result = handler(self, x[1:])
                    if isinstance(result, _TailCall):
                        x = result.form
                        continue
                    return result
                if hname in self.builtins or hname.startswith("VLA-"):
                    args = [self.eval(a) for a in x[1:]]
                    return self.apply_builtin(hname, args)
                if hname in self.functions:
                    fn = self.functions[hname]
                    args = [self.eval(a) for a in x[1:]]
                    return self.call_user(fn, args)
                raise LispError("no function definition: %s" % hname)
            if isinstance(head, list) and head and isinstance(head[0], Sym) and str(head[0]) == "LAMBDA":
                lam = self._make_lambda(head)
                args = [self.eval(a) for a in x[1:]]
                return self.call_lambda(lam, args)
            raise LispError("bad function position: %s" % lisp_prin1(head))

    def call_user(self, fn, args):
        if len(args) != len(fn.params):
            raise LispError(
                "function %s expects %d args, got %d" % (fn.name, len(fn.params), len(args)))
        frame = {}
        for p, a in zip(fn.params, args):
            frame[str(p)] = a
        for p in fn.locals:
            frame[str(p)] = None
        self.frames.append(frame)
        try:
            result = None
            for form in fn.body:
                result = self.eval(form)
            return result
        finally:
            self.frames.pop()

    def call_lambda(self, lam, args):
        if len(args) != len(lam.params):
            raise LispError("lambda expects %d args, got %d" % (len(lam.params), len(args)))
        frame = {}
        for p, a in zip(lam.params, args):
            frame[str(p)] = a
        for p in lam.locals:
            frame[str(p)] = None
        self.frames.append(frame)
        try:
            result = None
            for form in lam.body:
                result = self.eval(form)
            return result
        finally:
            self.frames.pop()

    def apply_callable(self, fn, args):
        if isinstance(fn, Sym):
            name = str(fn)
            if name in self.builtins or name.startswith("VLA-"):
                return self.apply_builtin(name, args)
            if name in self.globals and isinstance(self.globals[name], (UserFunction, Lambda)):
                fn = self.globals[name]
            elif name in self.functions:
                return self.call_user(self.functions[name], args)
            else:
                raise LispError("apply: no function %s" % name)
        if isinstance(fn, UserFunction):
            return self.call_user(fn, args)
        if isinstance(fn, Lambda):
            return self.call_lambda(fn, args)
        if isinstance(fn, list) and fn and isinstance(fn[0], Sym) and str(fn[0]) == "LAMBDA":
            return self.call_lambda(self._make_lambda(fn), args)
        raise LispError("apply: not a function: %s" % lisp_prin1(fn))

    def _split_params(self, plist):
        params, locals_ = [], []
        target = params
        for p in (plist or []):
            if isinstance(p, Sym) and str(p) == "/":
                target = locals_
                continue
            target.append(p)
        return params, locals_

    def _make_lambda(self, form):
        params, locals_ = self._split_params(form[1])
        return Lambda(params, locals_, form[2:])

    def apply_builtin(self, name, args):
        fn = self.builtins.get(name)
        if fn is not None:
            return fn(self, *args)
        if name.startswith("VLA-"):
            return self._vla_dispatch(name, args)
        raise LispError("no builtin: %s" % name)

    def _vla_dispatch(self, name, args):
        """Динамическая диспетчеризация vla-get-X / vla-Method над mock-объектами."""
        if not args:
            raise LispError("%s: no object" % name)
        obj = args[0]
        key = name[4:].replace("-", "").lower()
        if key.startswith("get"):
            attr = key[3:]
            if attr:
                val = getattr(obj, attr, _MISSING)
                if val is not _MISSING:
                    return val() if callable(val) else val
                raise LispError("no property %s on %r" % (attr, obj))
        fn = getattr(obj, key, _MISSING)
        if fn is _MISSING or not callable(fn):
            raise LispError("no method %s on %r" % (key, obj))
        return fn(*args[1:])

    # ---------------- специальные формы ----------------

    def _sp_quote(self, args):
        return args[0] if args else None

    def _sp_defun(self, args):
        name = str(args[0])
        params, locals_ = self._split_params(args[1])
        body = args[2:]
        self.functions[name] = UserFunction(name, params, locals_, body)
        return Sym(name)

    def _sp_setq(self, args):
        if len(args) % 2 != 0:
            raise LispError("setq: odd number of arguments")
        result = None
        for i in range(0, len(args), 2):
            var = args[i]
            if not isinstance(var, Sym):
                raise LispError("setq: not a symbol")
            result = self.eval(args[i + 1])
            self.assign(str(var), result)
        return result

    def _sp_if(self, args):
        test = self.eval(args[0])
        if truthy(test):
            return _TailCall(args[1])
        if len(args) > 2:
            if len(args) == 3:
                return _TailCall(args[2])
            result = None
            for f in args[2:]:
                result = self.eval(f)
            return result
        return None

    def _sp_cond(self, args):
        for clause in args:
            if not isinstance(clause, list):
                raise LispError("cond: bad clause")
            if not clause:
                continue
            test = self.eval(clause[0])
            if truthy(test):
                if len(clause) == 1:
                    return test
                result = None
                for f in clause[1:]:
                    result = self.eval(f)
                return result
        return None

    def _sp_progn(self, args):
        result = None
        for f in args:
            result = self.eval(f)
        return result

    def _sp_and(self, args):
        for f in args:
            if not truthy(self.eval(f)):
                return None
        return Sym("T")

    def _sp_or(self, args):
        for f in args:
            if truthy(self.eval(f)):
                return Sym("T")
        return None

    def _sp_foreach(self, args):
        var = args[0]
        lst = self.eval(args[1])
        result = None
        if lst is not None:
            if isinstance(lst, Pair):
                raise LispError("foreach: improper list")
            if not isinstance(lst, list):
                raise LispError("foreach: not a list")
            for item in lst:
                self.assign(str(var), item)
                for f in args[2:]:
                    result = self.eval(f)
        return result

    def _sp_repeat(self, args):
        n = self.eval(args[0])
        result = None
        if isinstance(n, (int, float)):
            for _ in range(int(n)):
                for f in args[1:]:
                    result = self.eval(f)
        return result

    def _sp_while(self, args):
        result = None
        guard = 0
        while truthy(self.eval(args[0])):
            for f in args[1:]:
                result = self.eval(f)
            guard += 1
            if guard > 10_000_000:
                raise LispError("while: loop limit exceeded")
        return result

    def _sp_lambda(self, args):
        return [Sym("LAMBDA")] + list(args)

    def _sp_function(self, args):
        return self.eval(args[0])

    def _sp_vlax_for(self, args):
        var = args[0]
        coll = self.eval(args[1])
        result = None
        items = []
        if coll is not None:
            if isinstance(coll, list):
                items = coll
            elif hasattr(coll, "__iter__"):
                items = list(coll)
        for item in items:
            self.assign(str(var), item)
            for f in args[2:]:
                result = self.eval(f)
        return None

    def _sp_exit(self, args):
        raise LispExit()

    @property
    def specials(self):
        if not hasattr(self, "_specials"):
            self._specials = {
                "QUOTE": lambda interp, a: interp._sp_quote(a),
                "DEFUN": lambda interp, a: interp._sp_defun(a),
                "SETQ": lambda interp, a: interp._sp_setq(a),
                "IF": lambda interp, a: interp._sp_if(a),
                "COND": lambda interp, a: interp._sp_cond(a),
                "PROGN": lambda interp, a: interp._sp_progn(a),
                "AND": lambda interp, a: interp._sp_and(a),
                "OR": lambda interp, a: interp._sp_or(a),
                "FOREACH": lambda interp, a: interp._sp_foreach(a),
                "REPEAT": lambda interp, a: interp._sp_repeat(a),
                "WHILE": lambda interp, a: interp._sp_while(a),
                "LAMBDA": lambda interp, a: interp._sp_lambda(a),
                "FUNCTION": lambda interp, a: interp._sp_function(a),
                "VLAX-FOR": lambda interp, a: interp._sp_vlax_for(a),
                "EXIT": lambda interp, a: interp._sp_exit(a),
                "QUIT": lambda interp, a: interp._sp_exit(a),
            }
        return self._specials

    # ---------------- встроенные функции ----------------

    def _num(self, x):
        if isinstance(x, (int, float)) and not isinstance(x, bool):
            return x
        raise LispError("not a number: %s" % lisp_prin1(x))

    def _make_builtins(self):
        B = {}

        def reg(name):
            def deco(fn):
                B[name] = fn
                return fn
            return deco

        # ---------- арифметика ----------
        @reg("+")
        def b_add(interp, *args):
            acc = 0
            allint = True
            for x in args:
                v = interp._num(x)
                allint = allint and isinstance(v, int)
                acc += v
            return acc if allint else float(acc)

        @reg("-")
        def b_sub(interp, *args):
            if len(args) == 1:
                v = interp._num(args[0])
                return -v
            acc = interp._num(args[0])
            allint = isinstance(acc, int)
            for x in args[1:]:
                v = interp._num(x)
                allint = allint and isinstance(v, int)
                acc -= v
            return acc if allint else float(acc)

        @reg("*")
        def b_mul(interp, *args):
            acc = 1
            allint = True
            for x in args:
                v = interp._num(x)
                allint = allint and isinstance(v, int)
                acc *= v
            return acc if allint else float(acc)

        @reg("/")
        def b_div(interp, *args):
            acc = interp._num(args[0])
            allint = isinstance(acc, int)
            for x in args[1:]:
                v = interp._num(x)
                allint = allint and isinstance(v, int)
                if allint:
                    if v == 0:
                        raise LispError("divide by zero")
                    # AutoLISP: целочисленное деление с усечением к нулю
                    q = abs(acc) // abs(v)
                    if (acc < 0) != (v < 0):
                        q = -q
                    acc = q
                else:
                    acc = acc / v
            return acc if allint else float(acc)

        @reg("1+")
        def b_1p(interp, x):
            return interp._num(x) + 1

        @reg("1-")
        def b_1m(interp, x):
            return interp._num(x) - 1

        @reg("REM")
        def b_rem(interp, a, b, *rest):
            a = interp._num(a)
            b = interp._num(b)
            if isinstance(a, int) and isinstance(b, int):
                r = abs(a) % abs(b)
                acc = -r if a < 0 else r
            else:
                acc = math.fmod(float(a), float(b))
            for x in rest:
                acc = math.fmod(float(acc), interp._num(x))
            return acc

        @reg("FIX")
        def b_fix(interp, x):
            return int(interp._num(x))

        @reg("FLOAT")
        def b_float(interp, x):
            return float(interp._num(x))

        @reg("ABS")
        def b_abs(interp, x):
            return abs(interp._num(x))

        @reg("MAX")
        def b_max(interp, *args):
            vals = [interp._num(a) for a in args]
            r = max(vals)
            return r if all(isinstance(v, int) for v in vals) else float(r)

        @reg("MIN")
        def b_min(interp, *args):
            vals = [interp._num(a) for a in args]
            r = min(vals)
            return r if all(isinstance(v, int) for v in vals) else float(r)

        @reg("ZEROP")
        def b_zerop(interp, x):
            return Sym("T") if interp._num(x) == 0 else None

        @reg("MINUSP")
        def b_minusp(interp, x):
            return Sym("T") if interp._num(x) < 0 else None

        @reg("SQRT")
        def b_sqrt(interp, x):
            return math.sqrt(interp._num(x))

        @reg("EXPT")
        def b_expt(interp, a, b):
            a = interp._num(a)
            b = interp._num(b)
            r = a ** b
            if isinstance(a, int) and isinstance(b, int) and b >= 0:
                return r
            return float(r)

        def _cmp(args, op_num, op_str_same, op_str):
            if len(args) < 2:
                return Sym("T")
            for x, y in zip(args, args[1:]):
                if isinstance(x, (int, float)) and isinstance(y, (int, float)):
                    if not op_num(x, y):
                        return None
                elif isinstance(x, (str, Sym)) and isinstance(y, (str, Sym)):
                    xs, ys = str(x), str(y)
                    if xs == ys:
                        if not op_str_same:
                            return None
                    elif not op_str(xs, ys):
                        return None
                elif x is None and y is None:
                    continue
                else:
                    raise LispError("bad comparison %s %s" % (lisp_prin1(x), lisp_prin1(y)))
            return Sym("T")

        @reg("<")
        def b_lt(interp, *args):
            return _cmp(args, lambda a, b: a < b, False, lambda a, b: a < b)

        @reg("<=")
        def b_le(interp, *args):
            return _cmp(args, lambda a, b: a <= b, True, lambda a, b: a < b)

        @reg(">")
        def b_gt(interp, *args):
            return _cmp(args, lambda a, b: a > b, False, lambda a, b: a > b)

        @reg(">=")
        def b_ge(interp, *args):
            return _cmp(args, lambda a, b: a >= b, True, lambda a, b: a > b)

        @reg("=")
        def b_eq(interp, *args):
            if len(args) < 2:
                return Sym("T")
            for x, y in zip(args, args[1:]):
                ok = False
                if isinstance(x, bool) or isinstance(y, bool):
                    ok = (x is y)
                elif isinstance(x, (int, float)) and isinstance(y, (int, float)):
                    ok = (x == y)
                elif isinstance(x, Sym) and isinstance(y, Sym):
                    ok = str(x) == str(y)
                elif type(x) is str and type(y) is str:
                    ok = (x == y)
                elif x is None or y is None:
                    ok = (x is None and y is None)
                else:
                    ok = (x is y)
                if not ok:
                    return None
            return Sym("T")

        @reg("/=")
        def b_ne(interp, *args):
            r = b_eq(interp, *args)
            return None if truthy(r) else Sym("T")

        @reg("EQ")
        def b_eqs(interp, a, b):
            if isinstance(a, Sym) and isinstance(b, Sym):
                return Sym("T") if str(a) == str(b) else None
            if a is None and b is None:
                return Sym("T")
            return Sym("T") if a is b else None

        @reg("EQUAL")
        def b_equal(interp, a, b, fuzz=None):
            if value_equal(a, b):
                return Sym("T")
            if fuzz is not None and isinstance(a, (int, float)) and isinstance(b, (int, float)):
                if abs(a - b) <= fuzz:
                    return Sym("T")
            return None

        # ---------- предикаты ----------
        @reg("NULL")
        def b_null(interp, x):
            return Sym("T") if x is None else None

        @reg("NOT")
        def b_not(interp, x):
            return Sym("T") if x is None else None

        @reg("LISTP")
        def b_listp(interp, x):
            if x is None:
                return Sym("T")
            return Sym("T") if isinstance(x, (list, Pair)) else None

        @reg("ATOM")
        def b_atom(interp, x):
            if isinstance(x, (list, Pair)):
                return None
            return Sym("T")

        @reg("NUMBERP")
        def b_numberp(interp, x):
            return Sym("T") if isinstance(x, (int, float)) and not isinstance(x, bool) else None

        @reg("BOUNDP")
        def b_boundp(interp, sym):
            if not isinstance(sym, Sym):
                return None
            name = str(sym)
            if name == "T":
                return Sym("T")
            return Sym("T") if name in interp.global_bound or name in interp.globals else None

        @reg("FBOUNDP")
        def b_fboundp(interp, sym):
            if not isinstance(sym, Sym):
                return None
            name = str(sym)
            return Sym("T") if name in interp.functions or name in interp.builtins else None

        @reg("TYPE")
        def b_type(interp, x):
            if x is None:
                return None
            if isinstance(x, Sym):
                return Sym("SYM")
            if type(x) is str:
                return Sym("STR")
            if isinstance(x, bool):
                return Sym("SYM")
            if isinstance(x, int):
                return Sym("INT")
            if isinstance(x, float):
                return Sym("REAL")
            if isinstance(x, list) or isinstance(x, Pair):
                return Sym("LIST")
            t = getattr(x, "lisp_type", None)
            if t:
                return Sym(t)
            if isinstance(x, (UserFunction, Lambda)):
                return Sym("SUBR")
            if callable(x):
                return Sym("SUBR")
            if isinstance(x, CatchAllError):
                return None
            return Sym("USUBR")

        # ---------- списки ----------
        @reg("CAR")
        def b_car(interp, x):
            if x is None:
                return None
            if isinstance(x, Pair):
                return x.car
            if isinstance(x, list):
                return x[0] if x else None
            raise LispError("car: bad arg")

        @reg("CDR")
        def b_cdr(interp, x):
            if x is None:
                return None
            if isinstance(x, Pair):
                return x.cdr
            if isinstance(x, list):
                return x[1:] if len(x) > 1 else None
            raise LispError("cdr: bad arg")

        def _cxr(path):
            def fn(interp, x):
                for c in reversed(path):
                    x = b_car(interp, x) if c == "a" else b_cdr(interp, x)
                return x
            return fn

        for depth in (2, 3, 4):
            for mask in range(2 ** depth):
                mid = "".join("a" if (mask >> i) & 1 else "d" for i in range(depth))
                B["C" + mid.upper() + "R"] = _cxr(mid)

        @reg("CONS")
        def b_cons(interp, a, b):
            if b is None:
                return [a]
            if isinstance(b, list):
                return [a] + list(b)
            if isinstance(b, Pair):
                return [a, b]
            return Pair(a, b)

        @reg("LIST")
        def b_list(interp, *args):
            return list(args) if args else None

        @reg("APPEND")
        def b_append(interp, *args):
            out = []
            for a in args:
                if a is None:
                    continue
                if isinstance(a, list):
                    out.extend(a)
                elif isinstance(a, Pair):
                    out.append(a)
                else:
                    out.append(a)
            return out if out else None

        @reg("LENGTH")
        def b_length(interp, x):
            if x is None:
                return 0
            if isinstance(x, list):
                return len(x)
            raise LispError("length: bad arg")

        @reg("NTH")
        def b_nth(interp, n, x):
            n = interp._num(n)
            if x is None:
                return None
            if n < 0:
                return None
            if isinstance(x, list):
                return x[n] if n < len(x) else None
            raise LispError("nth: bad arg")

        @reg("LAST")
        def b_last(interp, x):
            if isinstance(x, list) and x:
                return x[-1]
            return None

        @reg("REVERSE")
        def b_reverse(interp, x):
            if isinstance(x, list):
                return list(reversed(x))
            return None

        def _proper_list(x):
            if x is None:
                return []
            if isinstance(x, list):
                return x
            raise LispError("improper list")

        @reg("MEMBER")
        def b_member(interp, item, lst):
            lst = _proper_list(lst)
            for i, x in enumerate(lst):
                if value_equal(item, x):
                    return lst[i:] if lst[i:] else None
            return None

        @reg("ASSOC")
        def b_assoc(interp, key, lst):
            for pair in _proper_list(lst):
                if isinstance(pair, Pair):
                    if value_equal(pair.car, key):
                        return pair
                elif isinstance(pair, list) and pair:
                    if value_equal(pair[0], key):
                        return pair
            return None

        @reg("SUBST")
        def b_subst(interp, new, old, lst):
            return [new if value_equal(x, old) else x for x in _proper_list(lst)]

        @reg("MAPCAR")
        def b_mapcar(interp, fn, *lists):
            lists = [_proper_list(l) for l in lists]
            if not lists:
                return None
            out = []
            for items in zip(*lists):
                out.append(interp.apply_callable(fn, list(items)))
            return out if out else None

        @reg("APPLY")
        def b_apply(interp, fn, *args):
            if args and isinstance(args[-1], list):
                arglist = list(args[:-1]) + list(args[-1])
            else:
                arglist = list(args)
            return interp.apply_callable(fn, arglist)

        @reg("EVAL")
        def b_eval(interp, x):
            return interp.eval(x)

        @reg("VL-SORT")
        def b_vl_sort(interp, lst, fn):
            lst = _proper_list(lst)
            import functools

            def cmp(a, b):
                if truthy(interp.apply_callable(fn, [a, b])):
                    return -1
                if truthy(interp.apply_callable(fn, [b, a])):
                    return 1
                return 0

            s = sorted(lst, key=functools.cmp_to_key(cmp))
            # vl-sort удаляет дубликаты
            out = []
            for x in s:
                if not out or not value_equal(out[-1], x):
                    out.append(x)
            return out if out else None

        @reg("VL-SORT-I")
        def b_vl_sort_i(interp, lst, fn):
            lst = _proper_list(lst)
            import functools

            def cmp(ai, bi):
                a, b = lst[ai], lst[bi]
                if truthy(interp.apply_callable(fn, [a, b])):
                    return -1
                if truthy(interp.apply_callable(fn, [b, a])):
                    return 1
                return 0

            idx = sorted(range(len(lst)), key=functools.cmp_to_key(cmp))
            return idx if idx else None

        @reg("VL-REMOVE-IF")
        def b_vl_remove_if(interp, fn, lst):
            out = [x for x in _proper_list(lst)
                   if not truthy(interp.apply_callable(fn, [x]))]
            return out if out else None

        @reg("VL-REMOVE-IF-NOT")
        def b_vl_remove_if_not(interp, fn, lst):
            out = [x for x in _proper_list(lst)
                   if truthy(interp.apply_callable(fn, [x]))]
            return out if out else None

        @reg("VL-SOME")
        def b_vl_some(interp, fn, lst, *rest):
            lists = [_proper_list(lst)] + [_proper_list(r) for r in rest]
            for items in zip(*lists):
                r = interp.apply_callable(fn, list(items))
                if truthy(r):
                    return r
            return None

        @reg("VL-EVERY")
        def b_vl_every(interp, fn, lst, *rest):
            lists = [_proper_list(lst)] + [_proper_list(r) for r in rest]
            for items in zip(*lists):
                if not truthy(interp.apply_callable(fn, list(items))):
                    return None
            return Sym("T")

        @reg("VL-MEMBER-IF")
        def b_vl_member_if(interp, fn, lst):
            lst = _proper_list(lst)
            for i, x in enumerate(lst):
                if truthy(interp.apply_callable(fn, [x])):
                    return lst[i:] if lst[i:] else None
            return None

        # ---------- строки ----------
        @reg("STRCASE")
        def b_strcase(interp, s, lower=None):
            if s is None:
                raise LispError("strcase: nil")
            s = str(s)
            return s.lower() if truthy(lower) else s.upper()

        @reg("STRCAT")
        def b_strcat(interp, *args):
            out = []
            for a in args:
                if a is None:
                    raise LispError("strcat: nil argument")
                if isinstance(a, Sym):
                    raise LispError("strcat: symbol argument")
                if isinstance(a, str):
                    out.append(a)
                else:
                    raise LispError("strcat: bad argument %s" % lisp_prin1(a))
            return "".join(out)

        @reg("STRLEN")
        def b_strlen(interp, *args):
            return sum(len(str(a)) for a in args)

        @reg("SUBSTR")
        def b_substr(interp, s, start, length=None):
            s = str(s)
            start = int(interp._num(start))
            if start < 1:
                start = 1
            begin = start - 1
            if begin >= len(s):
                return ""
            if length is None:
                return s[begin:]
            length = int(interp._num(length))
            return s[begin:begin + max(0, length)]

        @reg("ITOA")
        def b_itoa(interp, n):
            n = interp._num(n)
            if not isinstance(n, int):
                raise LispError("itoa: not an integer")
            return str(n)

        @reg("ATOI")
        def b_atoi(interp, s):
            m = re.match(r"^[ \t]*([+-]?\d+)", str(s))
            return int(m.group(1)) if m else 0

        @reg("ATOF")
        def b_atof(interp, s):
            m = re.match(r"^[ \t]*([+-]?(\d+\.\d*|\.\d+|\d+)([eE][+-]?\d+)?)", str(s))
            return float(m.group(1)) if m else 0.0

        @reg("RTOS")
        def b_rtos(interp, x, mode=None, prec=None):
            v = interp._num(x)
            mode = int(interp._num(mode)) if mode is not None else 2
            prec = int(interp._num(prec)) if prec is not None else 6
            if mode == 1:
                return ("%.*E" % (prec, v)).replace("E+0", "E+").replace("E-0", "E-")
            q = Decimal(str(float(v))).quantize(
                Decimal(1).scaleb(-prec), rounding=ROUND_HALF_UP)
            return "%.*f" % (prec, q)

        @reg("ANGTOS")
        def b_angtos(interp, x, mode=None, prec=None):
            return b_rtos(interp, x, 2, prec or 4)

        @reg("ASCII")
        def b_ascii(interp, s):
            s = str(s)
            return ord(s[0]) if s else 0

        @reg("CHR")
        def b_chr(interp, n):
            return chr(int(interp._num(n)))

        @reg("VL-STRING-ELT")
        def b_vl_string_elt(interp, s, pos):
            return ord(str(s)[int(interp._num(pos))])

        @reg("VL-STRING-SEARCH")
        def b_vl_string_search(interp, pat, s, start=None):
            s = str(s)
            pat = str(pat)
            start = int(interp._num(start)) if start is not None else 0
            idx = s.find(pat, start)
            return idx if idx >= 0 else None

        @reg("VL-STRING-SUBST")
        def b_vl_string_subst(interp, new, old, s, start=None):
            s = str(s)
            old = str(old)
            start = int(interp._num(start)) if start is not None else 0
            idx = s.find(old, start)
            if idx < 0:
                return s
            return s[:idx] + str(new) + s[idx + len(old):]

        @reg("VL-STRING-TRANSLATE")
        def b_vl_string_translate(interp, src, dst, s):
            tbl = {}
            for i, ch in enumerate(str(src)):
                tbl[ord(ch)] = str(dst)[i] if i < len(str(dst)) else ch
            return str(s).translate(tbl)

        @reg("VL-STRING-TRIM")
        def b_vl_string_trim(interp, bag, s):
            return str(s).strip(str(bag))

        @reg("VL-STRING-LEFT-TRIM")
        def b_vl_string_ltrim(interp, bag, s):
            return str(s).lstrip(str(bag))

        @reg("VL-STRING-RIGHT-TRIM")
        def b_vl_string_rtrim(interp, bag, s):
            return str(s).rstrip(str(bag))

        @reg("VL-STRING-POSITION")
        def b_vl_string_position(interp, code, s, start=None, from_end=None):
            s = str(s)
            ch = chr(int(interp._num(code)))
            if truthy(from_end):
                end = len(s) if start is None else int(interp._num(start)) + 1
                idx = s.rfind(ch, 0, end)
            else:
                idx = s.find(ch, int(interp._num(start)) if start is not None else 0)
            return idx if idx >= 0 else None

        @reg("VL-PRINC-TO-STRING")
        def b_vl_princ_to_string(interp, x):
            return lisp_princ(x) if not isinstance(x, str) else x

        @reg("VL-STRING->LIST")
        def b_str2list(interp, s):
            out = [ord(c) for c in str(s)]
            return out if out else None

        @reg("VL-LIST->STRING")
        def b_list2str(interp, lst):
            return "".join(chr(int(c)) for c in _proper_list(lst))

        @reg("READ")
        def b_read(interp, s):
            return read_one(str(s))

        @reg("WCMATCH")
        def b_wcmatch(interp, s, pattern):
            s = str(s)
            pattern = str(pattern)
            for alt in _split_wcmatch(pattern):
                if re.fullmatch(_wcmatch_regex(alt), s, re.IGNORECASE):
                    return Sym("T")
            return None

        # ---------- печать / файлы ----------
        def _emit(interp, text, fd):
            if fd is not None:
                fd.write_text(text)
            else:
                interp.ctx.out_write(text)

        @reg("PRINC")
        def b_princ(interp, x=None, fd=None):
            if isinstance(x, FileHandle):
                fd, x = x, None
            text = lisp_princ(x)
            _emit(interp, text, fd)
            return x if x is not None else ""

        @reg("PRIN1")
        def b_prin1(interp, x=None, fd=None):
            text = lisp_prin1(x)
            _emit(interp, text, fd)
            return x

        @reg("PRINT")
        def b_print(interp, x=None, fd=None):
            _emit(interp, "\n" + lisp_prin1(x) + " ", fd)
            return x

        @reg("TERPRI")
        def b_terpri(interp, fd=None):
            _emit(interp, "\n", fd)
            return None

        @reg("PROMPT")
        def b_prompt(interp, msg):
            interp.ctx.out_write(str(msg))
            return None

        @reg("WRITE-LINE")
        def b_write_line(interp, s, fd=None):
            _emit(interp, str(s) + "\n", fd)
            return s

        @reg("WRITE-CHAR")
        def b_write_char(interp, code, fd=None):
            _emit(interp, chr(int(interp._num(code))), fd)
            return code

        @reg("OPEN")
        def b_open(interp, path, mode):
            return interp.ctx.fs_open(str(path), str(mode))

        @reg("CLOSE")
        def b_close(interp, fd):
            if isinstance(fd, FileHandle):
                fd.close()
                return None
            return None

        @reg("LOAD")
        def b_load(interp, path, onfailure=None):
            full = interp.ctx.fs_findfile(str(path))
            if full:
                interp.load_file(full)
                return full
            if onfailure is not None:
                return onfailure
            raise LispError("load: cannot find %s" % path)

        @reg("FINDFILE")
        def b_findfile(interp, path):
            return interp.ctx.fs_findfile(str(path))

        @reg("VL-FILENAME-BASE")
        def b_vfn_base(interp, path):
            base = os.path.basename(str(path).replace("\\", "/"))
            if "." in base:
                base = base.rsplit(".", 1)[0]
            return base

        @reg("VL-FILENAME-DIRECTORY")
        def b_vfn_dir(interp, path):
            p = str(path).replace("\\", "/")
            d = os.path.dirname(p)
            return d if d else None

        @reg("VL-FILENAME-EXTENSION")
        def b_vfn_ext(interp, path):
            base = os.path.basename(str(path).replace("\\", "/"))
            if "." in base:
                return "." + base.rsplit(".", 1)[1]
            return None

        # ---------- AutoCAD системные (делегаты контекста) ----------
        @reg("GETVAR")
        def b_getvar(interp, name):
            return interp.ctx.getvar(str(name))

        @reg("SETVAR")
        def b_setvar(interp, name, value):
            return interp.ctx.setvar(str(name), value)

        @reg("GETSTRING")
        def b_getstring(interp, cr=None, prompt=None):
            if prompt is not None:
                interp.ctx.out_write(str(prompt))
            return interp.ctx.next_input_string(allow_spaces=truthy(cr))

        @reg("GETKWORD")
        def b_getkword(interp, prompt=None):
            if prompt is not None:
                interp.ctx.out_write(str(prompt))
            return interp.ctx.next_input_kword()

        @reg("INITGET")
        def b_initget(interp, *args):
            interp.ctx.initget(*[a for a in args if a is not None])
            return None

        @reg("GETPOINT")
        def b_getpoint(interp, pt=None, prompt=None):
            if prompt is not None:
                interp.ctx.out_write(str(prompt))
            return interp.ctx.next_input_point()

        @reg("GETREAL")
        def b_getreal(interp, prompt=None):
            if prompt is not None:
                interp.ctx.out_write(str(prompt))
            return interp.ctx.next_input_real()

        @reg("GETDIST")
        def b_getdist(interp, pt=None, prompt=None):
            if prompt is not None:
                interp.ctx.out_write(str(prompt))
            return interp.ctx.next_input_real()

        @reg("GETFILED")
        def b_getfiled(interp, title, default, ext, flags):
            interp.ctx.out_write(" [getfiled: %s] " % title)
            return interp.ctx.next_getfiled(str(default), str(ext))

        @reg("ALERT")
        def b_alert(interp, msg):
            interp.ctx.alert(str(msg))
            return None

        @reg("SSGET")
        def b_ssget(interp, *args):
            return interp.ctx.ssget(*[a for a in args if a is not None])

        @reg("SSLENGTH")
        def b_sslength(interp, ss):
            return ss.length()

        @reg("SSNAME")
        def b_ssname(interp, ss, i):
            return ss.name(int(interp._num(i)))

        @reg("SSADD")
        def b_ssadd(interp, *args):
            return interp.ctx.ssadd(*args)

        @reg("SSDEL")
        def b_ssdel(interp, ent, ss):
            ss.remove(ent)
            return ss

        @reg("SSSETFIRST")
        def b_sssetfirst(interp, gripset=None, pickset=None):
            return None

        @reg("ENTGET")
        def b_entget(interp, ename, applist=None):
            return interp.ctx.db.entget(ename)

        @reg("ENTLAST")
        def b_entlast(interp):
            return interp.ctx.db.entlast()

        @reg("ENTNEXT")
        def b_entnext(interp, ename=None):
            return interp.ctx.db.entnext(ename)

        @reg("ENTMAKE")
        def b_entmake(interp, data):
            return interp.ctx.db.entmake(data)

        @reg("ENTMAKEX")
        def b_entmakex(interp, data):
            return interp.ctx.db.entmake(data, return_name=True)

        @reg("ENTMOD")
        def b_entmod(interp, data):
            return data

        @reg("ENTUPD")
        def b_entupd(interp, ename):
            return ename

        @reg("ENTDEL")
        def b_entdel(interp, ename):
            interp.ctx.db.entdel(ename)
            return None

        @reg("ENTSEL")
        def b_entsel(interp, prompt=None):
            return interp.ctx.next_entsel()

        @reg("TBLOBJNAME")
        def b_tblobjname(interp, table, name):
            return None

        @reg("TBLSEARCH")
        def b_tblsearch(interp, table, name, setnext=None):
            return None

        @reg("TBLNEXT")
        def b_tblnext(interp, table, rewind=None):
            return None

        @reg("DICTSEARCH")
        def b_dictsearch(interp, ename, name, setnext=None):
            return None

        @reg("DICTNEXT")
        def b_dictnext(interp, ename, rewind=None):
            return None

        @reg("DICTADD")
        def b_dictadd(interp, owner, name, obj):
            return obj

        @reg("NAMEDOBJDICT")
        def b_namedobjdict(interp):
            return None

        @reg("SNVALID")
        def b_snvalid(interp, name, flag=None):
            return Sym("T")

        @reg("REGAPP")
        def b_regapp(interp, name):
            return Sym("T")

        @reg("DISTANCE")
        def b_distance(interp, p1, p2):
            import math
            a, b = list(p1), list(p2)
            return math.sqrt(sum((float(y) - float(x)) ** 2 for x, y in zip(a, b)))

        @reg("VLAX-CURVE-GETENDPARAM")
        def b_vlax_curve_end(interp, ent):
            return 1.0

        @reg("VLAX-CURVE-GETSTARTPARAM")
        def b_vlax_curve_start(interp, ent):
            return 0.0

        @reg("VLAX-CURVE-GETDISTATPARAM")
        def b_vlax_curve_dist(interp, ent, param):
            return interp.ctx.curve_length(ent)

        @reg("TRANS")
        def b_trans(interp, pt, fromcs, tocs, disp=None):
            p = _proper_list(pt)
            return [p[0], p[1], p[2] if len(p) > 2 else 0.0]

        @reg("COMMAND")
        def b_command(interp, *args):
            return interp.ctx.command(*args)

        @reg("VL-CMDF")
        def b_vl_cmdf(interp, *args):
            return interp.ctx.command(*args)

        @reg("GRAPHSCR")
        def b_graphscr(interp):
            return None

        @reg("TEXTSCR")
        def b_textscr(interp):
            return None

        @reg("TEXTPAGE")
        def b_textpage(interp):
            return None

        @reg("VL-LOAD-COM")
        def b_vl_load_com(interp):
            return None

        # ---------- vlax / vla (делегаты mock-объектов) ----------
        @reg("VLAX-GET-ACAD-OBJECT")
        def b_vlax_get_acad(interp):
            return interp.ctx.app

        @reg("VLAX-ENAME->VLA-OBJECT")
        def b_vlax_ename_vla(interp, ename):
            return interp.ctx.db.ename_to_vla(ename)

        @reg("VLAX-VLA-OBJECT->ENAME")
        def b_vlax_vla_ename(interp, obj):
            return interp.ctx.db.vla_to_ename(obj)

        @reg("VLAX-INVOKE")
        def b_vlax_invoke(interp, obj, method, *args):
            return vla_call(obj, str(method), list(args))

        @reg("VLAX-INVOKE-METHOD")
        def b_vlax_invoke_method(interp, obj, method, *args):
            return vla_call(obj, str(method), list(args))

        @reg("VLAX-PUT-PROPERTY")
        def b_vlax_put(interp, obj, prop, value):
            vla_set(obj, str(prop), value)
            return None

        @reg("VLAX-GET-PROPERTY")
        def b_vlax_get(interp, obj, prop):
            return vla_get(obj, str(prop))

        @reg("VLAX-VARIANT-VALUE")
        def b_vlax_variant_value(interp, v):
            if isinstance(v, Variant):
                return v.value
            return v

        @reg("VLAX-VARIANT-TYPE")
        def b_vlax_variant_type(interp, v):
            return 5

        @reg("VLAX-SAFEARRAY->LIST")
        def b_vlax_safearray(interp, sa):
            if isinstance(sa, Variant):
                sa = sa.value
            if sa is None:
                return None
            if isinstance(sa, list):
                return sa if sa else None
            return list(sa)

        @reg("VLAX-MAKE-SAFEARRAY")
        def b_vlax_make_safearray(interp, typ, *args):
            return []

        @reg("VLAX-SAFEARRAY-FILL")
        def b_vlax_safearray_fill(interp, sa, lst):
            return _proper_list(lst)

        @reg("VLAX-3D-POINT")
        def b_vlax_3d(interp, *args):
            vals = args
            if len(args) == 1 and isinstance(args[0], list):
                vals = tuple(args[0])
            vals = list(vals) + [0.0, 0.0, 0.0]
            return [float(vals[0]), float(vals[1]), float(vals[2])]

        @reg("VL-CATCH-ALL-APPLY")
        def b_vl_catch(interp, fn, arglist):
            if arglist is None:
                arglist = []
            try:
                return interp.apply_callable(fn, list(arglist))
            except DoneDialog:
                raise
            except LispExit:
                # в AutoLISP (exit)/(quit) перехватываются vl-catch-all-apply
                return CatchAllError("quit / exit abort")
            except Exception as e:
                return CatchAllError(str(e))

        @reg("VL-CATCH-ALL-ERROR-P")
        def b_vl_catch_p(interp, x):
            return Sym("T") if isinstance(x, CatchAllError) else None

        @reg("VL-CATCH-ALL-ERROR-MESSAGE")
        def b_vl_catch_msg(interp, x):
            if isinstance(x, CatchAllError):
                return x.message
            return ""

        # ---------- DCL (делегаты контекста) ----------
        @reg("LOAD_DIALOG")
        def b_load_dialog(interp, path):
            return interp.ctx.dcl_load(str(path))

        @reg("NEW_DIALOG")
        def b_new_dialog(interp, name, dcl_id, action=None, screen_pt=None):
            return interp.ctx.dcl_new(str(name))

        @reg("START_DIALOG")
        def b_start_dialog(interp):
            return interp.ctx.dcl_start()

        @reg("DONE_DIALOG")
        def b_done_dialog(interp, status=None):
            raise DoneDialog(int(interp._num(status)) if status is not None else 1)

        @reg("TERM_DIALOG")
        def b_term_dialog(interp):
            raise DoneDialog(0)

        @reg("UNLOAD_DIALOG")
        def b_unload_dialog(interp, dcl_id):
            return None

        @reg("ACTION_TILE")
        def b_action_tile(interp, key, expr):
            interp.ctx.dcl_action(str(key), str(expr))
            return None

        @reg("SET_TILE")
        def b_set_tile(interp, key, value):
            interp.ctx.dcl_set_tile(str(key), str(value))
            return value

        @reg("GET_TILE")
        def b_get_tile(interp, key):
            return interp.ctx.dcl_get_tile(str(key))

        @reg("MODE_TILE")
        def b_mode_tile(interp, key, mode):
            interp.ctx.dcl_mode(str(key), int(interp._num(mode)))
            return None

        @reg("START_LIST")
        def b_start_list(interp, key, op=None, idx=None):
            interp.ctx.dcl_start_list(str(key))
            return None

        @reg("ADD_LIST")
        def b_add_list(interp, item):
            interp.ctx.dcl_add_list(str(item))
            return None

        @reg("END_LIST")
        def b_end_list(interp):
            interp.ctx.dcl_end_list()
            return None

        @reg("DIMX_TILE")
        def b_dimx(interp, key):
            return 100

        @reg("DIMY_TILE")
        def b_dimy(interp, key):
            return 20

        return B


class FileHandle:
    lisp_type = "FILE"

    def __init__(self, path, mode):
        self.path = path
        self.mode = mode
        self.closed = False
        if mode in ("w", "a"):
            real_mode = "wb" if mode == "w" else "ab"
        else:
            real_mode = "rb"
        os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
        self._fh = open(path, real_mode)

    def write_text(self, text):
        if self.closed:
            raise LispError("file closed")
        self._fh.write(text.encode("cp1251", errors="replace"))

    def read_line(self):
        line = self._fh.readline()
        if not line:
            return None
        return line.decode("cp1251").rstrip("\r\n")

    def close(self):
        if not self.closed:
            self._fh.close()
            self.closed = True


def vla_call(obj, method, args):
    """Вызов метода (по имени вида GetDynamicBlockProperties) на mock-объекте."""
    name = method.replace("-", "").lower()
    fn = getattr(obj, name, None)
    if not callable(fn):
        raise LispError("no method %s on %r" % (method, obj))
    return fn(*args)


def vla_get(obj, prop):
    name = prop.replace("-", "").lower()
    if hasattr(obj, name):
        val = getattr(obj, name)
        return val() if callable(val) else val
    raise LispError("no property %s" % prop)


def vla_set(obj, prop, value):
    name = "set" + prop.replace("-", "").lower()
    fn = getattr(obj, name, None)
    if callable(fn):
        return fn(value)
    raise LispError("no property setter %s" % prop)


def _split_wcmatch(pattern):
    parts = []
    cur = []
    i = 0
    while i < len(pattern):
        ch = pattern[i]
        if ch == "`" and i + 1 < len(pattern):
            cur.append("`" + pattern[i + 1])
            i += 2
            continue
        if ch == ",":
            parts.append("".join(cur))
            cur = []
            i += 1
            continue
        cur.append(ch)
        i += 1
    parts.append("".join(cur))
    return parts


def _wcmatch_regex(pat):
    out = []
    i = 0
    while i < len(pat):
        ch = pat[i]
        if ch == "`" and i + 1 < len(pat):
            out.append(re.escape(pat[i + 1]))
            i += 2
        elif ch == "*":
            out.append(".*")
            i += 1
        elif ch == "?":
            out.append(".")
            i += 1
        elif ch == "[":
            j = pat.find("]", i)
            if j < 0:
                out.append(re.escape(ch))
                i += 1
            else:
                body = pat[i + 1:j]
                if body.startswith("~"):
                    body = "^" + body[1:]
                out.append("[" + body + "]")
                i = j + 1
        elif ch == "#":
            out.append(r"\d")
            i += 1
        elif ch == "@":
            out.append(r"[A-Za-zА-Яа-яЁё]")
            i += 1
        elif ch == ".":
            out.append(r"[^\w]")
            i += 1
        else:
            out.append(re.escape(ch))
            i += 1
    return "".join(out)


class _TailCall:
    def __init__(self, form):
        self.form = form


class _Missing:
    def __repr__(self):
        return "<missing>"


_MISSING = _Missing()
