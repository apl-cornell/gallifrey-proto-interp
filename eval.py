from ast import *
from typing import List, Set, Dict, Tuple
from symbol_table import SymbolTable, EntryStatus, StoreEntry
from threading import Thread, Lock, current_thread
import time


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
        tname = current_thread().name
        self.st = {tname: SymbolTable()}
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
    # def evalCapDecl(self, node: CapDecl) -> Value:
    #     if self.store().containsCap(node.cap):
    #         raise NameError("capability {} already defined".format(node.cap))
    #     self.newScope()
    #     self.store().addCap(node.cap)
    #     return node.e.eval(self)

    def evalVarDecl(self, node: VarDecl) -> Value:
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

    def evalIf(self, node: If) -> Value:
        vcond = node.cond.eval(self)
        if not isinstance(vcond, V_bool):
            raise TypeError("if condition needs to be bool")
        self.newScope()
        v = node.e1.eval(self) if vcond.val else node.e2.eval(self)
        self.exitScope()
        return v

    def evalWhile(self, node: While) -> Value:
        vcond = node.cond.eval(self)
        if not isinstance(vcond, V_bool):
            raise TypeError("if condition needs to be bool")
        self.newScope()
        node.e.eval(self)
        self.exitScope()
        return V_unit()

    def evalSeq(self, node: Seq) -> Value:
        node.e1.eval(self)
        return node.e2.eval(self)

    def evalRef(self, node: Ref) -> Value:
        v = node.e.eval(self)
        l = self.newLoc()
        self.locations[l] = v
        return V_ref(l, v.type, DEFAULT_CAP)

    def evalDestroy(self, node: Destroy) -> Value:
        if isinstance(node.e, Var):
            self.store().destroy(node.e.name)
        else:
            raise Exception("not implemented!")
        return V_unit()

    def evalAssign(self, node: Assign) -> Value:
        v1 = node.e1.eval(self)
        if not isinstance(v1, V_ref):
            raise TypeError("expected ref")
        v2 = node.e2.eval(self)
        if type(v2.type) != type(v1.type.t):
            raise TypeError("ref content type mismatch")
        self.locations[v1.loc] = v2
        return v2

    def evalObj(self, node: Obj) -> Value:
        v_fields = []
        for f in node.fields:
            name, qualifier, e = f
            v = e.eval(self)
            v_fields.append((name, qualifier, v))
        return V_obj(v_fields, DEFAULT_CAP)

    def evalGet(self, node: Get) -> Value:
        v = node.e.eval(self)
        n = node.name
        if not isinstance(v, V_obj):
            raise TypeError("expected object")
        if n not in v.fields:
            raise NameError("field {} does not exist".format(n))
        return v.fields[n][0]

    def evalFunc(self, node: Func) -> Value:
        # TODO capture avoiding substitution
        return V_closure(node.params, node.out, node.e, self.store().copy(), DEFAULT_CAP)

    def evalUnary(self, node: Unary) -> Value:
        v = node.e.eval(self)
        if node.op == Unop.NOT:
            if not isinstance(v, V_bool):
                raise TypeError("expected bool")
            return V_bool(not v.val)
        if node.op == Unop.NEG:
            if not isinstance(v, V_int):
                raise TypeError("expected int")
            return V_int(-1 * v.val)
        if node.op == Unop.DEREF:
            if not isinstance(v, V_ref):
                raise TypeError("expected ref")
            return self.locations[v.loc]

    def evalBinary(self, node: Binary) -> Value:
        v1 = node.e1.eval(self)
        v2 = node.e2.eval(self)
        if node.op in [Binop.PLUS, Binop.MINUS, Binop.TIMES, Binop.DIVIDE, Binop.MOD]:
            if not isinstance(v1, V_int) or not isinstance(v2, V_int):
                raise TypeError("expected ints")
            if node.op == Binop.PLUS:
                return V_int(v1.val + v2.val)
            elif node.op == Binop.MINUS:
                return V_int(v1.val - v2.val)
            elif node.op == Binop.TIMES:
                return V_int(v1.val * v2.val)
            elif node.op == Binop.DIVIDE:
                return V_int(int(v1.val / v2.val))
            elif node.op == Binop.MOD:
                return V_int(v1.val % v2.val)
        elif node.op in [Binop.AND, Binop.OR]:
            if not isinstance(v1, V_bool) or not isinstance(v2, V_bool):
                raise TypeError("expected bools")
            elif node.op == Binop.AND:
                return V_bool(v1.val and v2.val)
            elif node.op == Binop.OR:
                return V_bool(v1.val or v2.val)
        elif node.op in [Binop.EQ, Binop.NEQ]:
            if v1.type != v2.type:
                raise TypeError("expected same type")
            elif node.op == Binop.EQ:
                return V_bool(v1 == v2)
            elif node.op == Binop.NEQ:
                return V_bool(v1 != v2)
        elif node.op in [Binop.GT, Binop.LT, Binop.GTE, Binop.LTE]:
            if not isinstance(v1, V_int) or not isinstance(v2, V_int):
                raise TypeError("expected ints")
            elif node.op == Binop.GT:
                return V_bool(v1.val > v2.val)
            elif node.op == Binop.LT:
                return V_bool(v1.val < v2.val)
            elif node.op == Binop.GTE:
                return V_bool(v1.val >= v2.val)
            elif node.op == Binop.LTE:
                return V_bool(v1.val <= v2.val)
        raise TypeError("unsupported op")

    def evalCall(self, node: Call) -> Value:
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

    def evalBranch(self, node: Branch) -> Value:
        for v in node.vars:
            pass
        tname = self.newThread()
        # branching symbol table
        with self.storeLock:
            self.st[tname] = SymbolTable(self.store())
            self.st[self.tname()] = SymbolTable(self.store())
        t = Thread(name=tname, target=(lambda: self.handleThread(node.e))).start()
        return V_unit()

    def evalPrint(self, node: Print) -> Value:
        v = node.e.eval(self)
        print(str(v))
        return V_unit()

    def evalSleep(self, node: Print) -> Value:
        v = node.e.eval(self)
        if not isinstance(v, V_int):
            raise TypeError("expected int")
        time.sleep(v.val)

    def evalInt(self, node: Int) -> Value:
        return V_int(node.val)

    def evalBool(self, node: Bool) -> Value:
        return V_bool(node.val)

    def evalUnit(self, node: Unit) -> Value:
        return V_unit()

    def evalVar(self, node: Var) -> Value:
        return self.store().getVar(node.name).val

    def handleThread(self, e: Expr):
        e.eval(self)
        with self.storeLock:
            del self.st[self.tname()]
