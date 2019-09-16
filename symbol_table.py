from ast import *
from typing import Dict, List, Tuple, Any
from threading import Lock, current_thread


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

    def newScope(self):
        new = SymbolTable(self)
        return new

    def exitScope(self):
        for c in self.caps:
            self.prev.caps.remove(c)
        for v in self.vars:
            self.prev.addVar(v, self.vars[v], EntryStatus.SCOPE_ENDED)
        return self.prev

    def addVar(self, k: str, v: Value, c: str, status: EntryStatus = EntryStatus.NORMAL):
        if self.containsVar(k):
            raise NameError("key {} already exists".format(k))
        v.capability = c
        self.vars[k] = StoreEntry(v, c, status)

    def addCap(self, k: str):
        if self.containsCap(k):
            raise NameError("capability {} already exists".format(k))
        self.caps.add(k)

    def removeCap(self, k: str):
        self.caps.remove(k)
        if self.prev:
            self.prev.removeCap(k)

    def updateVar(self, k: str, status: EntryStatus):
        if k in self.vars:
            self.vars[k].status = status
        elif self.prev:
            self.prev.updateVar(k, status)

    def getVar(self, k: str) -> StoreEntry:
        if k in self.vars:
            if self.vars[k].status != EntryStatus.NORMAL:
                raise Exception(self.vars[k].status)
            return self.vars[k]
        elif self.prev:
            return self.prev.getVar(k)
        else:
            raise NameError("key not found")

    def containsCap(self, k: str) -> bool:
        if k in self.caps:
            return True
        elif self.prev:
            return self.prev.containsCap(k)
        else:
            return False

    def containsVar(self, k: str) -> bool:
        if k in self.vars:
            return True
        elif self.prev:
            return self.prev.containsVar(k)
        else:
            return False

    def destroy(self, k: str):
        c = self.getCap(k)
        if c:
            self.destroyCap(c)

    def getCap(self, k: str):
        if k in self.vars:
            return self.vars[k].capability
        elif self.prev:
            return self.prev.getCap(k)
        else:
            return None

    def destroyCap(self, k: str):
        for v in self.vars:
            if self.vars[v].capability == k:
                self.vars[v].status = EntryStatus.DESTROYED
        if self.prev:
            self.prev.destroyCap(k)

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
