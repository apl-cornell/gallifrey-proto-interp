from ast import *
from typing import Dict, List, Tuple, Any


class EntryStatus:
    NORMAL = "normal"
    UNDECLARED = "undeclared"
    SCOPE_ENDED = "scope ended"
    DESTROYED = "resource destroyed"


class StoreEntry:
    def __init__(self, val: Value, c: str, status: EntryStatus = EntryStatus.NORMAL):
        self.status = status
        self.val = val
        self.capability = c

    def copy(self):
        return StoreEntry(self.val, self.capability, self.status)


class SymbolTable:

    def __init__(self, prev=None):
        self.caps: Set[str] = set()
        self.vars: Dict[str, StoreEntry] = {}
        self.prev = prev

    def newscope(self):
        new = SymbolTable(self)
        return new

    def exitscope(self):
        for c in self.caps:
            self.prev.caps.remove(c)
        for v in self.vars:
            self.prev.addvar(v, self.vars[v], EntryStatus.SCOPE_ENDED)
        return self.prev

    def addvar(self, k: str, v: Value, c: str, status: EntryStatus = EntryStatus.NORMAL):
        if self.containsVar(k):
            raise NameError("key {} already exists".format(k))
        v.capability = c
        self.vars[k] = StoreEntry(v, c, status)

    def addcap(self, k: str):
        if self.containscap(k):
            raise NameError("capability {} already exists".format(k))
        self.caps.add(k)

    def removecap(self, k: str):
        self.caps.remove(k)
        if self.prev:
            self.prev.removecap(k)

    def updatevar(self, k: str, status: EntryStatus):
        if k in self.vars:
            self.vars[k].status = status
        elif self.prev:
            self.prev.updatevar(k, status)

    def getvar(self, k: str) -> StoreEntry:
        if k in self.vars:
            if self.vars[k].status != EntryStatus.NORMAL:
                raise Exception(self.vars[k].status)
            return self.vars[k]
        elif self.prev:
            return self.prev.getvar(k)
        else:
            raise NameError("key not found")

    def containscap(self, k: str) -> bool:
        if k in self.caps:
            return True
        elif self.prev:
            return self.prev.containscap(k)
        else:
            return False

    def containsvar(self, k: str) -> bool:
        if k in self.vars:
            return True
        elif self.prev:
            return self.prev.containsvar(k)
        else:
            return False

    def destroy(self, k: str):
        c = self.getcap(k)
        if c:
            self.destroycap(c)

    def getcap(self, k: str):
        if k in self.vars:
            return self.vars[k].capability
        elif self.prev:
            return self.prev.getcap(k)
        else:
            return None

    def destroycap(self, k: str):
        for v in self.vars:
            if self.vars[v].capability == k:
                self.vars[v].status = EntryStatus.DESTROYED
        if self.prev:
            self.prev.destroycap(k)

    def copy(self):
        # returns a new SymbolTable that is a copy of current
        # this is a shallow copy of the dict values
        c = self.caps.copy()
        vars = {}
        for k in self.vars:
            vars[k] = self.vars[k].copy()
        p = None
        if self.prev:
            p = self.prev.copy()
        new = SymbolTable(p)
        new.caps = c
        new.vars = vars
        return new
