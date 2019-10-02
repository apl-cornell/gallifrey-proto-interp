from ast import *
from typing import List, Set, Dict, Tuple
from symbol_table import SymbolTable, EntryStatus, StoreEntry
from threading import Thread, Lock, current_thread
import time

Result = Tuple[Value, Tuple[str, str], Set[str], Set[str]]


class Cap:
    NONE = "@cap_C_NONE"
    ANY = "@cap_C_ANY"

# read and write any
C_ANY = Cap.ANY, Cap.ANY
DEFAULT_CAP = Cap.ANY

# VALUES

class V_unit(Value):
    def __init__(self):
        super().__init__(DEFAULT_CAP)
        self.type = T_unit()

    def __str__(self):
        return "()"

    def __eq__(self, other):
        return isinstance(other, V_unit)


class V_int(Value):
    def __init__(self, val: int):
        super().__init__(DEFAULT_CAP)
        self.type = T_int()
        self.val = val

    def __str__(self):
        return str(self.val)

    def __eq__(self, other):
        return (self.val == other.val) if isinstance(other, V_int) else False


class V_bool(Value):
    def __init__(self, val: bool):
        super().__init__(DEFAULT_CAP)
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
        self.st = {t_name: SymbolTable()}
        self.locations: Dict[int, Value] = {}  # refs
        self.lid = 0
        self.tid = 0
        self.capid = 0
        self.refLock = Lock()
        self.threadLock = Lock()
        self.storeLock = Lock()
        self.capLock = Lock()

    def tname(self):
        return current_thread().name

    def newScope(self):
        with self.storeLock:
            self.st[self.tname()] = self.st[self.tname()].newScope()

    def exitScope(self):
        with self.storeLock:
            self.st[self.tname()] = self.st[self.tname()].exitScope()

    def newLoc(self) -> int:
        with self.refLock:
            self.lid += 1
            return self.lid

    def newThread(self) -> str:
        with self.threadLock:
            self.tid += 1
            return "@thread" + str(self.tid)

    def newCap(self) -> str:
        with self.capLock:
            self.capid += 1
            return "@cap" + str(self.capid)

    def store(self):
        return self.st[current_thread().name]

    # EVAL

    def evalVarDecl(self, node: VarDecl) -> Result:
        cap = self.newCap()
        self.store().addCap(cap)
        node.cap = cap
        # if not self.store().containsCap(node.cap):
        #     raise NameError("capability {} not defined".format(node.cap))
        if self.store().containsVar(node.var):
            raise NameError("variable {} already exists".format(node.var))
        v = node.e1.eval(self)
        v.capability = node.cap
        self.newScope()
        self.store().addVar(node.var, v, node.cap)
        return node.e2.eval(self)

    def evalIf(self, node: If) -> Result:
        vcond, rwcond, kcond, pcond = node.cond.eval(self)
        if not isinstance(vcond, V_bool):
            raise TypeError("if condition needs to be bool")
        self.newScope()
        # TODO do we need to eval both branches of if?
        v, rw, k, p = None, None, None, None
        if vcond.val:
            v, rw, k, p = node.e1.eval(self)
        else:
            v, rw, k, p = node.e2.eval(self)
        self.exitScope()
        return v, (rw[0], Cap.NONE), k, p.union(kcond).union(pcond)

    def evalWhile(self, node: While) -> Result:
        vcond, rwcond, kcond, pcond = node.cond.eval(self)
        if not isinstance(vcond, V_bool):
            raise TypeError("if condition needs to be bool")
        self.newScope()
        v, rw, k, p = node.e.eval(self)
        self.exitScope()
        return V_unit(), C_ANY, set(), kcond.union(pcond).union(k).union(p)

    def evalSeq(self, node: Seq) -> Result:
        v1, rw1, k1, p1 = node.e1.eval(self)
        v2, rw2, k2, p2 = node.e2.eval(self)
        return v2, rw2, k2, k1.union(p1).union(p2)

    # def evalRef(self, node: Ref) -> Result:
    #     v = node.e.eval(self)
    #     l = self.newLoc()
    #     self.locations[l] = v
    #     return V_ref(l, v.type, DEFAULT_CAP)

    def evalDestroy(self, node: Destroy) -> Result:
        if isinstance(node.e, Var):
            self.store().destroy(node.e.name)
        else:
            raise Exception("not implemented!")
        return V_unit(),

    def evalAssign(self, node: Assign) -> Result:
        v1 = node.e1.eval(self)
        if not isinstance(v1, V_ref):
            raise TypeError("expected ref")
        v2 = node.e2.eval(self)
        if type(v2.type) != type(v1.type.t):
            raise TypeError("ref content type mismatch")
        self.locations[v1.loc] = v2
        return v2

    def evalObj(self, node: Obj) -> Result:
        # TODO
        v_fields = []
        for f in node.fields:
            name, qualifier, e = f
            v = e.eval(self)
            v_fields.append((name, qualifier, v))
        return V_obj(v_fields, DEFAULT_CAP)

    def evalGet(self, node: Get) -> Result:
        # TODO
        v = node.e.eval(self)
        n = node.name
        if not isinstance(v, V_obj):
            raise TypeError("expected object")
        if n not in v.fields:
            raise NameError("field {} does not exist".format(n))
        return v.fields[n][0]

    def evalFunc(self, node: Func) -> Result:
        # TODO capture avoiding substitution, nothing is evaluated so I *think* k and p are empty?
        return V_closure(node.params, node.out, node.e, self.store().copy(), DEFAULT_CAP), C_ANY, set(), set()

    def evalUnary(self, node: Unary) -> Result:
        v, rw, k, p = node.e.eval(self)
        if node.op == Unop.NOT:
            if not isinstance(v, V_bool):
                raise TypeError("expected bool")
            return V_bool(not v.val), rw, set(), k.union(p)
        if node.op == Unop.NEG:
            if not isinstance(v, V_int):
                raise TypeError("expected int")
            return V_int(-1 * v.val), rw, set(), k.union(p)
        # TODO should return RW be C_ANY?
        # if node.op == Unop.DEREF:
        #     if not isinstance(v, V_ref):
        #         raise TypeError("expected ref")
        #     return self.locations[v.loc]

    def evalBinary(self, node: Binary) -> Result:
        v1, rw1, k1, p1 = node.e1.eval(self)
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
        return res, C_ANY, set(), k1.union(k2).union(p1).union(p2)

    def evalCall(self, node: Call) -> Result:
        cl = self.store().getVar(node.name).val
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

    def evalBranch(self, node: Branch) -> Result:
        for v in node.vars:
            pass
        tname = self.newThread()
        # branching symbol table
        with self.storeLock:
            self.st[tname] = SymbolTable(self.store())
            self.st[self.tname()] = SymbolTable(self.store())
        t = Thread(name=tname, target=(lambda: self.handleThread(node.e))).start()
        # TODO
        return V_unit(), C_ANY, set(), set()

    def evalPrint(self, node: Print) -> Result:
        v, rw, k, p = node.e.eval(self)
        print(str(v))
        return V_unit(), C_ANY, set(), k.union(p)

    def evalSleep(self, node: Print) -> Result:
        v, rw, k, p = node.e.eval(self)
        if not isinstance(v, V_int):
            raise TypeError("expected int")
        time.sleep(v.val)
        return V_unit(), C_ANY, set(), k.union(p)

    def evalInt(self, node: Int) -> Result:
        return V_int(node.val), C_ANY, set(), set()

    def evalBool(self, node: Bool) -> Result:
        return V_bool(node.val), C_ANY, set(), set()

    def evalUnit(self, _node: Unit) -> Result:
        return V_unit(), C_ANY, set(), set()

    def evalVar(self, node: Var) -> Result:
        return self.store().getVar(node.name).val

    def evalFocusGet(self, node: FocusGet) -> Result:
        raise Exception("unimplemented")

    def evalFocus(self, node: Focus) -> Result:
        raise Exception("unimplemented")

    def handleThread(self, e: Expr):
        e.eval(self)
        with self.storeLock:
            del self.st[self.tname()]
