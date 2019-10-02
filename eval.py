from ast import *
from typing import List, Set, Dict, Tuple
from symbol_table import SymbolTable, EntryStatus, StoreEntry
from threading import Thread, Lock, current_thread
import time

Result = Tuple[Value, Tuple[str, str], Set[str], Set[str]]
Focus = str  # placeholder TODO
FocusStack = List[Set[Focus]]


class Cap:
    NONE = "@cap_C_NONE"
    ANY = "@cap_C_ANY"


# read and write any
C_ANY = Cap.ANY, Cap.ANY
DEFAULT_cap = Cap.ANY


# essentially, if LHS write cap is NONE then we can't assign
# otherwise if RHS read cap is ANY or if the caps match then we can assign
def can_assign(write: str, read: str):
    if write == Cap.NONE:
        return False
    else:
        return read == Cap.ANY or read == write


# VALUES

class V_unit(Value):
    def __init__(self):
        super().__init__(DEFAULT_cap)
        self.type = T_unit()

    def __str__(self):
        return "()"

    def __eq__(self, other):
        return isinstance(other, V_unit)


class V_int(Value):
    def __init__(self, val: int):
        super().__init__(DEFAULT_cap)
        self.type = T_int()
        self.val = val

    def __str__(self):
        return str(self.val)

    def __eq__(self, other):
        return (self.val == other.val) if isinstance(other, V_int) else False


class V_bool(Value):
    def __init__(self, val: bool):
        super().__init__(DEFAULT_cap)
        self.type = T_bool()
        self.val = val

    def __str__(self):
        return str(self.val)

    def __eq__(self, other):
        return (self.val == other.val) if isinstance(other, V_bool) else False


class V_ref(Value):
    def __init__(self, loc: int, t: Type, c: str):
        super().__init__(c)
        self.type = T_ref(t)
        self.loc = loc

    def __str__(self):
        return "REF"

    def __eq__(self, other):
        return (self.loc == other.loc) if isinstance(other, V_ref) else False


class V_field(Value):
    def __init__(self, name: str, q: Qualifier, v: Value, m: bool, c: str):
        # TODO initialized with U/A cap
        super().__init__(c if isinstance(q, Q_a) else c + name)
        self.name = name
        self.q = q
        self.v = v
        self.mut = m

    def __str__(self):
        # TODO
        return "{}:{} {}".format(self.name, self.q, self.v) + (" MUT" if self.mut else "")

    def __eq__(self, other):
        return str(self) == str(other) if isinstance(other, V_field) else False


class V_obj(Value):
    def __init__(self, fields: List[Tuple[str, Qualifier, Value]], c: str):
        super().__init__(c)
        t_fields = []
        self.fields: Dict[str, Tuple[Value, Qualifier, EntryStatus]] = {}
        for f in fields:
            self.fields[f[0]] = (f[2], f[1], EntryStatus.NORMAL)  # Value, qualifier, status
            t_fields.append((f[0], f[1], f[2].type))
        self.type = T_obj(t_fields)

    def __str__(self):
        s = [k + ":" + str(self.fields[k]) for k in self.fields]
        s.sort()
        return "{" + ", ".join(s) + "}"

    def __eq__(self, other):
        return (str(self) == str(other)) if isinstance(other, V_obj) else False


class V_closure(Value):
    # (id, qualifier, Type)
    def __init__(self, params: List[Tuple[str, Qualifier, Type]], o: Type, e: Expr, store: SymbolTable, c: str):
        super().__init__(c)
        self.type = T_func([x[2] for x in params], o)
        self.params = params
        self.e = e
        self.store = store

    def __str__(self):
        return "CLOSURE"

    def __eq__(self, other):
        return False


class EvalVisitor:

    def __init__(self):
        t_name = current_thread().name
        self.st: Dict[str, SymbolTable] = {t_name: SymbolTable()}
        # self.focus: Dict[str, SymbolTable] = {t_name: SymbolTable()}
        # self.K: Dict[str, Set[str]] = {t_name: set()}  # input caps
        self.locations: Dict[int, Value] = {}  # refs, should be shared btwn threads
        self.lid = 0
        self.tid = 0
        self.capid = 0
        self.refLock = Lock()
        self.threadLock = Lock()
        self.storeLock = Lock()
        self.capLock = Lock()

    def tname(self):
        return current_thread().name

    def newscope(self):
        with self.storeLock:
            self.st[self.tname()] = self.st[self.tname()].newscope()

    def exitscope(self):
        with self.storeLock:
            self.st[self.tname()] = self.st[self.tname()].exitscope()

    def newloc(self) -> int:
        with self.refLock:
            self.lid += 1
            return self.lid

    def newthread(self) -> str:
        with self.threadLock:
            self.tid += 1
            return "@thread" + str(self.tid)

    def newcap(self) -> str:
        with self.capLock:
            self.capid += 1
            return "@cap" + str(self.capid)

    def store(self):
        return self.st[current_thread().name]

    # EVAL

    def evalVarDecl(self, node: VarDecl, K: Set[str], focus: FocusStack) -> Result:
        cap = self.newcap()
        self.store().addcap(cap)
        node.cap = cap
        # if not self.store().containscap(node.cap):
        #     raise NameError("capability {} not defined".format(node.cap))
        if self.store().containsvar(node.var):
            raise NameError("variable {} already exists".format(node.var))
        v = node.e1.eval(self)
        v.capability = node.cap
        self.newscope()
        self.store().addvar(node.var, v, node.cap)
        return node.e2.eval(self)

    def evalIf(self, node: If, K: Set[str], focus: FocusStack) -> Result:
        vcond, rwcond, kcond, pcond = node.cond.eval(self)
        if not isinstance(vcond, V_bool):
            raise TypeError("if condition needs to be bool")
        self.newscope()
        # TODO do we need to eval both branches of if?
        v, rw, k, p = None, None, None, None
        if vcond.val:
            v, rw, k, p = node.e1.eval(self)
        else:
            v, rw, k, p = node.e2.eval(self)
        self.exitscope()
        return v, (rw[0], Cap.NONE), k, p.union(kcond).union(pcond)

    def evalWhile(self, node: While, K: Set[str], focus: FocusStack) -> Result:
        vcond, rwcond, kcond, pcond = node.cond.eval(self)
        if not isinstance(vcond, V_bool):
            raise TypeError("if condition needs to be bool")
        self.newscope()
        v, rw, k, p = node.e.eval(self)
        self.exitscope()
        return V_unit(), C_ANY, set(), kcond.union(pcond).union(k).union(p)

    def evalSeq(self, node: Seq, K: Set[str], focus: FocusStack) -> Result:
        v1, rw1, k1, p1 = node.e1.eval(self)
        v2, rw2, k2, p2 = node.e2.eval(self)
        return v2, rw2, k2, k1.union(p1).union(p2)

    # def evalRef(self, node: Ref, K:Set[str], focus:FocusStack) -> Result:
    #     v = node.e.eval(self)
    #     l = self.newloc()
    #     self.locations[l] = v
    #     return V_ref(l, v.type, DEFAULT_cap)

    def evalDestroy(self, node: Destroy, K: Set[str], focus: FocusStack) -> Result:

        if isinstance(node.e, Var):
            v, rw, k, p = node.e.eval(self)
            self.store().destroy(node.e.name)
            return V_unit(), (Cap.NONE, Cap.NONE), k, p
        else:
            raise Exception("not implemented!")

    def evalAssign(self, node: Assign, K: Set[str], focus: FocusStack) -> Result:
        v1 = node.e1.eval(self)
        if not isinstance(v1, V_ref):
            raise TypeError("expected ref")
        v2 = node.e2.eval(self)
        if type(v2.type) != type(v1.type.t):
            raise TypeError("ref content type mismatch")
        self.locations[v1.loc] = v2
        return v2

    def evalObj(self, node: Obj, K: Set[str], focus: FocusStack) -> Result:
        # TODO
        v_fields = []
        for f in node.fields:
            name, qualifier, e = f
            v = e.eval(self)
            v_fields.append((name, qualifier, v))
        return V_obj(v_fields, DEFAULT_cap)

    def evalGet(self, node: Get, K: Set[str], focus: FocusStack) -> Result:
        # TODO
        v = node.e.eval(self)
        n = node.name
        if not isinstance(v, V_obj):
            raise TypeError("expected object")
        if n not in v.fields:
            raise NameError("field {} does not exist".format(n))
        return v.fields[n][0]

    def evalFunc(self, node: Func, K: Set[str], focus: FocusStack) -> Result:
        # TODO capture avoiding substitution, nothing is evaluated so I *think* k and p are empty?
        return V_closure(node.params, node.out, node.e, self.store().copy(), DEFAULT_cap), C_ANY, set(), set()

    def evalUnary(self, node: Unary, K: Set[str], focus: FocusStack) -> Result:
        v, rw, k, p = node.e.eval(self)
        if node.op == Unop.NOT:
            if not isinstance(v, V_bool):
                raise TypeError("expected bool")
            return V_bool(not v.val), rw, k, p
        if node.op == Unop.NEG:
            if not isinstance(v, V_int):
                raise TypeError("expected int")
            return V_int(-1 * v.val), rw, k, p
        # TODO should return RW be C_ANY?
        # if node.op == Unop.DEREF:
        #     if not isinstance(v, V_ref):
        #         raise TypeError("expected ref")
        #     return self.locations[v.loc]

    def evalBinary(self, node: Binary, K: Set[str], focus: FocusStack) -> Result:
        v1, rw1, k1, p1 = node.e1.eval(self, K, focus)
        v2, rw2, k2, p2 = node.e2.eval(self)
        res = None
        if node.op in [Binop.PLUS, Binop.MINUS, Binop.TIMES, Binop.DIVIDE, Binop.MOD]:
            if not isinstance(v1, V_int) or not isinstance(v2, V_int):
                raise TypeError("expected ints")
            if node.op == Binop.PLUS:
                res = V_int(v1.val + v2.val)
            elif node.op == Binop.MINUS:
                res = V_int(v1.val - v2.val)
            elif node.op == Binop.TIMES:
                res = V_int(v1.val * v2.val)
            elif node.op == Binop.DIVIDE:
                res = V_int(int(v1.val / v2.val))
            elif node.op == Binop.MOD:
                res = V_int(v1.val % v2.val)
        elif node.op in [Binop.AND, Binop.OR]:
            if not isinstance(v1, V_bool) or not isinstance(v2, V_bool):
                raise TypeError("expected bools")
            elif node.op == Binop.AND:
                res = V_bool(v1.val and v2.val)
            elif node.op == Binop.OR:
                res = V_bool(v1.val or v2.val)
        elif node.op in [Binop.EQ, Binop.NEQ]:
            if v1.type != v2.type:
                raise TypeError("expected same type")
            elif node.op == Binop.EQ:
                res = V_bool(v1 == v2)
            elif node.op == Binop.NEQ:
                res = V_bool(v1 != v2)
        elif node.op in [Binop.GT, Binop.LT, Binop.GTE, Binop.LTE]:
            if not isinstance(v1, V_int) or not isinstance(v2, V_int):
                raise TypeError("expected ints")
            elif node.op == Binop.GT:
                res = V_bool(v1.val > v2.val)
            elif node.op == Binop.LT:
                res = V_bool(v1.val < v2.val)
            elif node.op == Binop.GTE:
                res = V_bool(v1.val >= v2.val)
            elif node.op == Binop.LTE:
                res = V_bool(v1.val <= v2.val)
        if not res:
            raise TypeError("unsupported op")
        # no refs in result
        # TODO what to do with r/w caps of both sides?
        return res, C_ANY, k1.union(k2), p1.union(p2)

    def evalCall(self, node: Call, K: Set[str], focus: FocusStack) -> Result:
        cl = self.store().getvar(node.name).val
        if not isinstance(cl, V_closure):
            raise TypeError("expected closure")
        argvs = [e.eval(self) for e in node.args]
        callstore = cl.store.copy()
        if len(argvs) != len(cl.params):
            raise TypeError("expected same number of arguments")
        for i in range(len(argvs)):
            if argvs[i].type != cl.params[i][2]:
                raise TypeError("mismatched arg type")
            callstore.addVar(cl.params[i][0], argvs[i], argvs[i].capability)
        visitor = EvalVisitor()
        visitor.st = {self.tname(): callstore}
        visitor.lid = self.lid
        visitor.tid = self.tid
        visitor.refLock = self.refLock
        visitor.threadLock = self.threadLock
        visitor.storeLock = self.storeLock
        return cl.e.eval(visitor)

    def evalBranch(self, node: Branch, K: Set[str], focus: FocusStack) -> Result:
        for v in node.vars:
            pass
        tname = self.newthread()
        # branching symbol table
        with self.storeLock:
            self.st[tname] = SymbolTable(self.store())
            self.st[self.tname()] = SymbolTable(self.store())
        t = Thread(name=tname, target=(lambda: self.handleThread(node.e))).start()
        # TODO
        return V_unit(), C_ANY, set(), set()

    def evalPrint(self, node: Print, K: Set[str], focus: FocusStack) -> Result:
        v, rw, k, p = node.e.eval(self)
        print(str(v))
        return V_unit(), C_ANY, k, p

    def evalSleep(self, node: Print, K: Set[str], focus: FocusStack) -> Result:
        v, rw, k, p = node.e.eval(self)
        if not isinstance(v, V_int):
            raise TypeError("expected int")
        time.sleep(v.val)
        return V_unit(), C_ANY, k, p

    def evalInt(self, node: Int, K: Set[str], focus: FocusStack) -> Result:
        return V_int(node.val), C_ANY, set(), set()

    def evalBool(self, node: Bool, K: Set[str], focus: FocusStack) -> Result:
        return V_bool(node.val), C_ANY, set(), set()

    def evalUnit(self, _node: Unit, K: Set[str], focus: FocusStack) -> Result:
        return V_unit(), C_ANY, set(), set()

    def evalVar(self, node: Var, K: Set[str], focus: FocusStack) -> Result:
        entry = self.store().getvar(node.name)
        c = entry.capability
        return entry.val, (c, c), {c}, set()

    def evalFocusGet(self, node: FocusGet, K: Set[str], focus: FocusStack) -> Result:
        raise Exception("unimplemented")

    def evalFocus(self, node: Focus, K: Set[str], focus: FocusStack) -> Result:
        raise Exception("unimplemented")

    def handleThread(self, e: Expr, K:Set[str], focus:FocusStack):
        e.eval(self, K, focus)
        with self.storeLock:
            del self.st[self.tname()]
