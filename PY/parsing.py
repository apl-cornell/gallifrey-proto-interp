from ast import *
from ply import yacc
from lexing import tokens

# Precedence rules
precedence = (
    ('left', 'COMMA', 'COLON', 'SEMICOLON', 'ASSIGN'),
    ('left', 'OR'),
    ('left', 'AND'),
    ('left', 'IN'),
    ('left', 'EQ', 'NEQ'),
    ('left', 'GT', 'LT', 'GTE', 'LTE'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE', 'MOD'),
    ('right', 'UMINUS', 'NOT'),
    ('left', 'LPAREN', 'RPAREN', 'DOT', 'LBRAC', 'RBRAC', 'LCURL', 'RCURL')
)


def p_program(p):
    '''program : expr'''
    p[0] = p[1]


# TYPES and QUALIFIERS

def p_type_int(p):
    '''simple_type : INT'''
    p[0] = T_int()


def p_type_bool(p):
    '''simple_type : BOOL'''
    p[0] = T_bool()


def p_type_unit(p):
    '''simple_type : UNIT'''
    p[0] = T_unit()


def p_type(p):
    '''type : simple_type'''
    p[0] = p[1]


def p_tlist(p):
    '''tlist : type COMMA type'''
    p[0] = [p[1], p[3]]


def p_tlist_multi(p):
    '''tlist : type COMMA tlist'''
    p[0] = [p[1]] + p[3]


# def p_type_shared(p):
#     '''type : SHARED LBRAC ID RBRAC simple_type'''
#     p[0] = T_shared(p[3], p[5])


def p_type_obj(p):
    '''type : LT paramlist GT'''
    field_names = [x[0] for x in p[2]]
    if len(field_names) != len(set(field_names)):
        raise SyntaxError("duplicate field names")
    p[0] = T_obj(p[2])


def p_type_obj_single(p):
    '''type : LT param GT'''
    p[0] = T_obj([p[2]])


def p_tfunc(p):
    '''type : LBRAC tlist ARROW type RBRAC'''
    p[0] = T_func(p[2], p[4])


def p_tfunc_single(p):
    '''type : LBRAC type ARROW type RBRAC'''
    line = p.lineno(1)
    p[0] = T_func([p[2]], p[4])


def p_tfunc_none(p):
    '''type : LBRAC ARROW type RBRAC'''
    line = p.lineno(1)
    p[0] = T_func([], p[3])


def p_qualifier_a(p):
    '''qualifier : A'''
    p[0] = Q_a()


def p_qualifier_b(p):
    '''qualifier : B'''
    p[0] = Q_b()


def p_qualifier_u(p):
    '''qualifier : U'''
    p[0] = Q_u()


# STATEMENTS (commands)


def p_var_decl(p):
    '''expr : LET ID ASSIGN expr IN expr'''
    line = p.lineno(1)
    p[0] = VarDecl(p[2], p[5], p[7], line)


def p_assign(p):
    '''expr : expr ASSIGN expr'''
    line = p.lineno(1)
    p[0] = Assign(p[1], p[3], line)


def p_if(p):
    '''expr : IF expr LCURL expr RCURL ELSE LCURL expr RCURL'''
    line = p.lineno(1)
    p[0] = If(p[2], p[4], p[6], line)


def p_while(p):
    '''expr : WHILE expr LCURL expr RCURL'''
    line = p.lineno(1)
    p[0] = While(p[2], p[4], line)


def p_focus(p):
    '''expr : FOCUS expr LCURL expr RCURL'''
    line = p.lineno(1)
    p[0] = Focus(p[2], p[4], line)


def p_seq(p):
    'expr : expr SEMICOLON expr'
    line = p.lineno(1)
    p[0] = Seq(p[1], p[3], line)


def p_print(p):
    'expr : PRINT LPAREN expr RPAREN'
    line = p.lineno(1)
    p[0] = Print(p[3], line)


def p_print(p):
    'expr : SLEEP LPAREN expr RPAREN'
    line = p.lineno(1)
    p[0] = Sleep(p[3], line)


def p_destroy(p):
    'expr : DESTROY LPAREN expr RPAREN'
    line = p.lineno(1)
    p[0] = Destroy(p[3], line)


def p_get(p):
    'expr : expr DOT ID'
    line = p.lineno(1)
    p[0] = Get(p[1], p[3], line)


def p_focus_get(p):
    'expr : THIS DOT ID'
    line = p.lineno(1)
    p[0] = FocusGet(p[3], line)


# OPERATORS and GROUPING


def p_binop(p):
    '''expr : expr PLUS expr
                  | expr MINUS expr
                  | expr TIMES expr
                  | expr DIVIDE expr
                  | expr AND expr
                  | expr OR expr
                  | expr EQ expr
                  | expr GT expr
                  | expr GTE expr
                  | expr LT expr
                  | expr LTE expr
                  | expr NEQ expr
                  | expr MOD expr
                  '''
    line = p.lineno(1)
    if p[2] == '+':
        p[0] = Binary(Binop.PLUS, p[1], p[3], line)
    elif p[2] == '-':
        p[0] = Binary(Binop.MINUS, p[1], p[3], line)
    elif p[2] == '*':
        p[0] = Binary(Binop.TIMES, p[1], p[3], line)
    elif p[2] == '/':
        p[0] = Binary(Binop.DIVIDE, p[1], p[3], line)
    elif p[2] == 'and':
        p[0] = Binary(Binop.AND, p[1], p[3], line)
    elif p[2] == 'or':
        p[0] = Binary(Binop.OR, p[1], p[3], line)
    elif p[2] == '>':
        p[0] = Binary(Binop.GT, p[1], p[3], line)
    elif p[2] == '>=':
        p[0] = Binary(Binop.GTE, p[1], p[3], line)
    elif p[2] == '<':
        p[0] = Binary(Binop.LT, p[1], p[3], line)
    elif p[2] == '<=':
        p[0] = Binary(Binop.LTE, p[1], p[3], line)
    elif p[2] == '==':
        p[0] = Binary(Binop.EQ, p[1], p[3], line)
    elif p[2] == '!=':
        p[0] = Binary(Binop.NEQ, p[1], p[3], line)
    elif p[2] == '%':
        p[0] = Binary(Binop.MOD, p[1], p[3], line)


def p_uminus(p):
    '''expr : MINUS expr %prec UMINUS'''
    line = p.lineno(1)
    p[0] = Unary(Unop.NEG, p[2], line)


def p_unot(p):
    '''expr : NOT expr'''
    line = p.lineno(1)
    p[0] = Unary(Unop.NOT, p[2], line)


def p_group(p):
    'expr : LPAREN expr RPAREN'
    p[0] = p[2]


# LITERALS and VARS


def p_unit(p):
    'expr : LPAREN RPAREN'
    line = p.lineno(1)
    p[0] = Unit(line)


def p_int(p):
    'expr : INT_LITERAL'
    line = p.lineno(1)
    p[0] = Int(p[1], line)


def p_boolean(p):
    'expr : BOOL_LITERAL'
    line = p.lineno(1)
    p[0] = Bool(p[1], line)


def p_var(p):
    'expr : ID'
    line = p.lineno(1)
    p[0] = Var(p[1], line)


def p_varlist(p):
    'varlist : ID COMMA ID'
    line = p.lineno(1)
    p[0] = [p[1], p[3]]


def p_varlist_multi(p):
    'varlist : ID COMMA varlist'
    line = p.lineno(1)
    p[0] = [p[1]] + p[3]


def p_branch(p):
    '''expr : BRANCH LPAREN varlist RPAREN LCURL expr RCURL'''
    line = p.lineno(1)
    p[0] = Branch(p[3], p[5], line)


def p_branch_single(p):
    '''expr : BRANCH LPAREN ID RPAREN LCURL expr RCURL'''
    line = p.lineno(1)
    p[0] = Branch([p[3]], p[5], line)


def p_branch_empty(p):
    '''expr : BRANCH LPAREN RPAREN LCURL expr RCURL'''
    line = p.lineno(1)
    p[0] = Branch([], p[4], line)


def p_field(p):
    '''field : ID ASSIGN qualifier expr'''
    p[0] = [p[1], p[3], p[4], False]


def p_field_mut(p):
    '''field : MUT ID ASSIGN qualifier expr'''
    p[0] = [p[1], p[3], p[4], True]


def p_fieldlist(p):
    '''fieldlist : field COMMA field'''
    p[0] = [p[1], p[3]]


def p_fieldlist_multi(p):
    '''fieldlist : field COMMA fieldlist'''
    p[0] = [p[1]] + p[3]


def p_object(p):
    '''expr : LCURL fieldlist RCURL'''
    # TODO check for qualifier
    line = p.lineno(1)
    field_names = [x[0] for x in p[2]]
    if len(field_names) != len(set(field_names)):
        raise SyntaxError("duplicate field names")
    p[0] = Obj(p[2], line)


def p_object_single(p):
    '''expr : LCURL field RCURL'''
    line = p.lineno(1)
    p[0] = Obj([p[2]], line)


# FUNCTIONS


def p_param(p):
    '''param : ID COLON qualifier type'''
    p[0] = [p[1], p[3], p[4], False]


# this is actually only possible in the case of object types, should throw an error in the case of function
def p_param_mut(p):
    '''param : MUT ID COLON qualifier type'''
    p[0] = [p[1], p[3], p[4], True]


def p_paramlist(p):
    '''paramlist : param COMMA param'''
    p[0] = [p[1], p[3]]


def p_paramlist_multi(p):
    '''paramlist : param COMMA paramlist'''
    p[0] = [p[1]] + p[3]


def p_func(p):
    '''expr : FUN LPAREN paramlist RPAREN ARROW type LCURL expr RCURL'''
    line = p.lineno(1)
    param_names = [x[0] for x in p[3]]
    if len(param_names) != len(set(param_names)):
        raise SyntaxError("duplicate param names")
    p[0] = Func(p[3], p[5], p[7], line)


def p_func_single(p):
    '''expr : FUN LPAREN param RPAREN ARROW type LCURL expr RCURL'''
    line = p.lineno(1)
    p[0] = Func([p[3]], p[5], p[7], line)


def p_func_none(p):
    '''expr : FUN LPAREN RPAREN ARROW type LCURL expr RCURL'''
    line = p.lineno(1)
    p[0] = Func([], p[4], p[6], line)


def p_exprlist(p):
    '''exprlist : expr COMMA expr'''
    p[0] = [p[1], p[3]]


def p_exprlist_multi(p):
    '''exprlist : expr COMMA exprlist'''
    p[0] = [p[1]] + p[3]


def p_call(p):
    '''expr : ID LPAREN exprlist RPAREN'''
    line = p.lineno(1)
    p[0] = Call(p[1], p[3], line)


def p_call_single(p):
    '''expr : ID LPAREN expr RPAREN'''
    line = p.lineno(1)
    p[0] = Call(p[1], [p[3]], line)


def p_call_none(p):
    '''expr : ID LPAREN RPAREN'''
    line = p.lineno(1)
    p[0] = Call(p[1], [], line)


def p_error(p):
    raise SyntaxError(
        "Syntax error at line {0}, '{1}'".format(p.lineno, p.value))
