from ply import lex
from ply import yacc
import lexing as l
import parsing as p
import os.path
import sys
from eval import *
from ast import *
import json

'''
This is the main file for the Tython executable.
Reads source code of input file, parses into AST, runs type-checker, then transpiles and writes Python code to output file
'''

lex.lex(module=l)
yacc.yacc(module=p)

AST_DUMP = "ast.json"

def process_file(filename: str):
    if not os.path.isfile(filename) or filename[-2:] != ".g":
        raise FileNotFoundError("Error: Invalid file {0}".format(filename))
    py_filename = filename[:-2] + ".g"
    dump_s = ""
    ast = None
    with open(filename) as f:
        s = "".join([line for line in f])
        ast = yacc.parse(s)
        dump_s = json.dumps(ast.jsonify())
    with open(AST_DUMP, "w") as f:
        print("Dumping AST to: "+AST_DUMP)
        f.write(dump_s)
    interpreter = EvalVisitor()
    print("Interpreting Program:")
    r = ast.eval(interpreter)
    print("Result:")
    print(json.dumps(r.jsonify()))

filename = sys.argv[-1]

if not os.path.isfile(filename) or filename[-2:] != ".g":
    raise FileNotFoundError("Error: Invalid file {0}".format(filename))

process_file(filename)
