from ply import lex

reserved = {
    'if': 'IF',
    'then': 'THEN',
    'else': 'ELSE',
    'while': 'WHILE',
    'do': 'DO',

    'and': 'AND',
    'or': 'OR',
    'not': 'NOT',

    'fun': 'FUN',
    'let': 'LET',
    'in': 'IN',
    'end': 'END',

    'int': 'INT',
    'bool': 'BOOL',
    'unit': 'UNIT',

    'ref': 'REF',
    'sleep': 'SLEEP',

    # 'shared': 'SHARED',
    # 'unique': 'UNIQUE',
    # 'local': 'LOCAL',
    # 'borrowed': 'BORROWED',
    'B': 'B',
    'A': 'A',
    'U': 'U',

    'destroy': 'DESTROY',
    'branch': 'BRANCH',
    'print': 'PRINT'
}

symbols = {
    '+': 'PLUS',
    '-': 'MINUS',
    '*': 'TIMES',
    '/': 'DIVIDE',
    '%': 'MOD',
    '=': 'ASSIGN',
    '==': 'EQ',
    '!=': 'NEQ',
    '>': 'GT',
    '>=': 'GTE',
    '<': 'LT',
    '<=': 'LTE',
    '->': 'ARROW',
    '.': 'DOT',
}

tokens = [
             'ID', 'INT_LITERAL', 'BOOL_LITERAL',
             'LPAREN', 'RPAREN',
             'LCURL', 'RCURL',
             'COMMA', 'COLON', 'SEMICOLON',
             'LBRAC', 'RBRAC',
         ] + list(reserved.values()) + list(symbols.values())

# Tokens
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRAC = r'\['
t_RBRAC = r'\]'
t_LCURL = r'\{'
t_RCURL = r'\}'
t_COMMA = r','
t_COLON = r':'
t_SEMICOLON = r';'


def t_NUMBER(t):
    r'\d+'
    t.type = "INT_LITERAL"
    t.value = int(t.value)
    return t


def t_BOOL(t):
    r'(true|false)'
    t.value = (t.value == "true")
    t.type = "BOOL_LITERAL"
    return t


def t_ID(t):
    r'[A-Za-z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'ID')
    return t


def t_SYMBOL(t):
    r'[\+\-\=\*\/\>\<\!\%]+'
    t.type = symbols.get(t.value, 'UNKNOWN')
    if t.type == 'UNKNOWN':
        raise Exception('Unknown symbol! ' + str(t.value))
    return t


# Ignored characters
t_ignore = " \t"


def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")


def t_error(t):
    raise SyntaxError("Illegal character '{}' on line {}".format(
        t.value[0], t.lexer.lineno))
