from typing import List, Set, Tuple, Dict
from eval import Result

DEFAULT_CAP = "@cap"


class Binop:
    PLUS = "+"
    MINUS = "-"
    TIMES = "*"
    DIVIDE = "/"
    MOD = "%"
    AND = "and"
    OR = "or"
    GT = '>'
    LT = '<'
    GTE = '>='
    LTE = '<='
    NEQ = '!='
    EQ = '=='


class Unop:
    NOT = "not"
    NEG = "-"
    DEREF = "*"


class Serializable:
    def __init__(self):
        pass

    def jsonify(self):
        d = dict()
        d["class"] = self.__class__.__name__
        for a, v in self.__dict__.items():
            if not a.startswith("_") and a != "line" and a != "type" and a != "capability":
                d[a] = serialize(v)
        if len(d.keys()) == 1:
            return self.__class__.__name__
        return d


def serialize(v: Serializable):
    if hasattr(v.__class__, "jsonify") and callable(getattr(v.__class__, 'jsonify')):
        return v.jsonify()
    elif isinstance(v, tuple):
        return [serialize(x) for x in list(v)]
    elif isinstance(v, list):
        return [serialize(x) for x in v]
    elif isinstance(v, dict):
        for k in v:
            v[k] = serialize(v[k])
        return v
    else:
        return v


class Expr(Serializable):
    def __init__(self, line: int):
        super().__init__()
        self.line = line
        self.type = None

    def eval(self, visitor, k, focus):
        raise Exception("not implemented!")


class Value(Expr):
    def __init__(self, c: str):
        super().__init__(0)
        self.capability = c

    def getCap(self):
        return self.capability


class Qualifier(Serializable):
    def __init__(self):
        super().__init__()


class Type(Serializable):
    def __init__(self):
        super().__init__()


# QUALIFIER

class Q_a(Qualifier):
    def __init__(self):
        super().__init__()


class Q_b(Qualifier):
    def __init__(self):
        super().__init__()


class Q_u(Qualifier):
    def __init__(self):
        super().__init__()


# TYPES

class T_int(Type):
    def __init__(self):
        super().__init__()

    def __eq__(self, other):
        return isinstance(other, T_int)


class T_bool(Type):
    def __init__(self):
        super().__init__()

    def __eq__(self, other):
        return isinstance(other, T_bool)


class T_ref(Type):
    def __init__(self, t: Type):
        super().__init__()
        self.t = t

    def __eq__(self, other):
        return isinstance(other, T_ref) and other.t == self.t


class T_unit(Type):
    def __init__(self):
        super().__init__()

    def __eq__(self, other):
        return isinstance(other, T_unit)


class T_func(Type):
    def __init__(self, params: List[Type], output: Type):
        super().__init__()
        self.input = params
        self.output = output

    def __eq__(self, other):
        if not isinstance(other, T_func):
            return False
        return self.input == other.input and self.output == other.output


class T_obj(Type):
    # (id, qualifier, Type)
    def __init__(self, fields: List[Tuple[str, Qualifier, Type]]):
        super().__init__()
        fields.sort(key=lambda x: x[0])
        self.fields: List[Tuple[str, Qualifier, Type]] = fields

    def __eq__(self, other):
        return isinstance(other, T_obj) and self.fields == other.fields


class T_shared(Type):
    def __init__(self, cap: str, t: Type):
        super().__init__()
        self.cap = cap
        self.t = t

    def __eq__(self, other):
        return isinstance(other, T_shared) and self.t == other.t


# id, qualifier, type, mutable flag
Field = Tuple[str, Qualifier, Type, bool]


# EXPRESSIONS


class VarDecl(Expr):
    def __init__(self, var: str, e1: Expr, e2: Expr, line: int):
        super().__init__(line)
        self.cap = None
        self.var = var
        self.e1 = e1
        self.e2 = e2

    def eval(self, visitor, k, focus) -> Result:
        return visitor.evalVarDecl(self, k, focus)

    def accept(self, visitor):
        visitor.evalVarDecl(self)


class If(Expr):
    def __init__(self, cond: Expr, e1: Expr, e2: Expr, line: int):
        super().__init__(line)
        self.cond = cond
        self.e1 = e1
        self.e2 = e2

    def eval(self, visitor, k, focus) -> Result:
        return visitor.evalIf(self, k, focus)

    def accept(self, visitor):
        visitor.evalIf(self)


class While(Expr):
    def __init__(self, cond: Expr, e: Expr, line: int):
        super().__init__(line)
        self.cond = cond
        self.e = e

    def eval(self, visitor, k, focus) -> Result:
        return visitor.evalWhile(self, k, focus)

    def accept(self, visitor):
        visitor.evalWhile(self)


class Focus(Expr):
    def __init__(self, focused: Expr, e: Expr, line: int):
        super().__init__(line)
        self.focused = focused
        self.e = e

    def eval(self, visitor, k, focus) -> Result:
        return visitor.evalFocus(self, k, focus)

    def accept(self, visitor):
        visitor.evalFocus(self)


class Seq(Expr):
    def __init__(self, e1: Expr, e2: Expr, line: int):
        super().__init__(line)
        self.e1 = e1
        self.e2 = e2

    def eval(self, visitor, k, focus) -> Result:
        return visitor.evalSeq(self, k, focus)

    def accept(self, visitor):
        visitor.evalSeq(self)


class Destroy(Expr):
    def __init__(self, e: Expr, line: int):
        super().__init__(line)
        self.e = e

    def eval(self, visitor, k, focus) -> Result:
        return visitor.evalDestroy(self, k, focus)

    def accept(self, visitor):
        visitor.evalDestroy(self)


class Assign(Expr):
    def __init__(self, e1: Expr, e2: Expr, line: int):
        super().__init__(line)
        self.e1 = e1
        self.e2 = e2

    def eval(self, visitor, k, focus) -> Result:
        return visitor.evalAssign(self, k, focus)

    def accept(self, visitor):
        visitor.evalAssign(self)


# (id, qualifier, expr, mut)
FieldExpr = Tuple[str, str, Expr, bool]


class Obj(Expr):
    def __init__(self, fields: List[FieldExpr], line: int):
        super().__init__(line)
        self.fields = [list(f) for f in fields]

    def eval(self, visitor, k, focus) -> Result:
        return visitor.evalObj(self, k, focus)

    def accept(self, visitor):
        visitor.evalObj(self)


class Get(Expr):
    def __init__(self, e: Expr, name: str, line: int):
        super().__init__(line)
        self.e = e
        self.name = name

    def eval(self, visitor, k, focus) -> Result:
        return visitor.evalGet(self, k, focus)

    def accept(self, visitor):
        visitor.evalGet(self)


class FocusGet(Expr):
    def __init__(self, name: str, line: int):
        super().__init__(line)
        self.name = name

    def eval(self, visitor, k, focus) -> Result:
        return visitor.evalFocusGet(self, k, focus)

    def accept(self, visitor):
        visitor.evalFocusGet(self)


class Func(Expr):
    # TODO substitute argument names in closures
    # (id, qualifier, Type)
    def __init__(self, params: List[Tuple[str, str, Type]], t: Type, e: Expr, line: int):
        super().__init__(line)
        self.params = [list(p) for p in params]
        self.e = e
        self.out = t

    def eval(self, visitor, k, focus) -> Result:
        return visitor.evalFunc(self, k, focus)

    def accept(self, visitor):
        visitor.evalFunc(self)


class Unary(Expr):
    def __init__(self, op: Unop, e: Expr, line: int):
        super().__init__(line)
        self.op = op
        self.e = e

    def eval(self, visitor, k, focus) -> Result:
        return visitor.evalUnary(self, k, focus)

    def accept(self, visitor):
        visitor.evalUnary(self)


class Binary(Expr):
    def __init__(self, op: Binop, e1: Expr, e2: Expr, line: int):
        super().__init__(line)
        self.op = op
        self.e1 = e1
        self.e2 = e2

    def eval(self, visitor, k, focus) -> Result:
        return visitor.evalBinary(self, k, focus)

    def accept(self, visitor):
        visitor.evalBinary(self)


class Call(Expr):
    def __init__(self, name: str, args: List[Expr], line: int):
        super().__init__(line)
        self.name = name
        self.args = args

    def eval(self, visitor, k, focus) -> Result:
        return visitor.evalCall(self, k, focus)

    def accept(self, visitor):
        visitor.evalCall(self)


class Branch(Expr):
    def __init__(self, vars_: List[str], e: Expr, line: int):
        super().__init__(line)
        self.vars = vars_
        self.e = e

    def eval(self, visitor, k, focus) -> Result:
        return visitor.evalBranch(self, k, focus)

    def accept(self, visitor):
        visitor.evalBranch(self)


class Int(Expr):
    def __init__(self, val: int, line: int):
        super().__init__(line)
        self.val = val

    def eval(self, visitor, k, focus) -> Result:
        return visitor.evalInt(self, k, focus)

    def accept(self, visitor):
        visitor.evalInt(self)


class Bool(Expr):
    def __init__(self, val: bool, line: int):
        super().__init__(line)
        self.val = val

    def eval(self, visitor, k, focus) -> Result:
        return visitor.evalBool(self, k, focus)

    def accept(self, visitor):
        visitor.evalBool(self)


class Unit(Expr):
    def __init__(self, line: int):
        super().__init__(line)

    def eval(self, visitor, k, focus) -> Result:
        return visitor.evalUnit(self, k, focus)

    def accept(self, visitor):
        visitor.evalUnit(self)


class Print(Expr):
    def __init__(self, e: Expr, line: int):
        super().__init__(line)
        self.e = e

    def eval(self, visitor, k, focus) -> Result:
        return visitor.evalPrint(self, k, focus)

    def accept(self, visitor):
        visitor.evalPrint(self)


class Sleep(Expr):
    def __init__(self, e: Expr, line: int):
        super().__init__(line)
        self.e = e

    def eval(self, visitor, k, focus) -> Result:
        return visitor.evalSleep(self, k, focus)

    def accept(self, visitor):
        visitor.evalSleep(self)


class Var(Expr):
    def __init__(self, name: str, line: int):
        super().__init__(line)
        super().__init__(line)
        self.name = name

    def eval(self, visitor, k, focus) -> Result:
        return visitor.evalVar(self, k, focus)

    def accept(self, visitor):
        visitor.evalVar(self)
