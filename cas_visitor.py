from ast import *
from symbol_table import SymbolTable


class CasVisitor:

    def __init__(self, node: Expr, store: SymbolTable, counter: int = 0):
        if not isinstance(node, Func):
            raise Exception("cas can only be performed on closures")
        self.mappings = [{}]
        self.counter = counter
        self.evalFunc(node)
        self.store = store

    def newname(self, base: str):
        self.counter += 1
        return "@arg_{}_{}".format(base, self.counter)

    def evalVarDecl(self, node: VarDecl):
        node.e2.accept(self)

    def evalIf(self, node: If):
        node.cond.accept(self)
        node.e1.accept(self)
        node.e2.accept(self)

    def evalWhile(self, node: While):
        node.cond.accept(self)
        node.e.accept(self)

    def evalSeq(self, node: Seq):
        node.e1.accept(self)
        node.e2.accept(self)

    def evalDestroy(self, node: Destroy):
        node.e.accept(self)

    def evalAssign(self, node: Assign):
        node.e1.accept(self)
        node.e2.accept(self)

    def evalObj(self, node: Obj):
        for f in node.fields:
            f[3].accept(self)

    def evalGet(self, node: Get):
        node.e.accept(self)

    def evalFunc(self, node: Func):
        return  # TODO

    def evalUnary(self, node: Unary):
        node.e.accept(self)

    def evalBinary(self, node: Binary):
        node.e1.accept(self)
        node.e2.accept(self)

    def evalCall(self, node: Call):
        # TODO
        for e in node.args:
            e.accept(self)

    def evalBranch(self, node: Branch):
        node.vars = [self.mappings[v] if v in self.mappings else v for v in node.vars]
        node.e.accept(self)

    def evalPrint(self, node: Print):
        node.e.accept(self)

    def evalSleep(self, node: Print):
        node.e.accept(self)

    def evalInt(self, _node: Int):
        return

    def evalBool(self, _node: Bool):
        return

    def evalUnit(self, _node: Unit):
        return

    def evalVar(self, node: Var):
        if node.name in self.mappings:
            node.name = self.mappings[node.name]

    def evalFocusGet(self, _node: FocusGet):
        return

    def evalFocus(self, node: Focus):
        node.e.accept(self)
