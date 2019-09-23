
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'leftCOMMACOLONSEMICOLONASSIGNleftORleftANDleftINleftEQNEQleftGTLTGTELTEleftPLUSMINUSleftTIMESDIVIDEMODrightUMINUSNOTDEREFleftLPARENRPARENDOTA AND ARROW ASSIGN B BOOL BOOL_LITERAL BRANCH COLON COMMA DESTROY DIVIDE DO DOT ELSE END EQ FUN GT GTE ID IF IN INT INT_LITERAL LBRAC LCURL LET LPAREN LT LTE MINUS MOD NEQ NOT OR PLUS PRINT RBRAC RCURL REF RPAREN SEMICOLON SLEEP THEN TIMES U UNIT WHILEprogram : exprsimple_type : INTsimple_type : BOOLsimple_type : UNITtype : simple_typetlist : type COMMA typetlist : type COMMA tlisttype : LT paramlist GTtype : LT param GTtype : LBRAC tlist ARROW type RBRACtype : LBRAC type ARROW type RBRACtype : LBRAC ARROW type RBRACqualifier : Aqualifier : Bqualifier : Uexpr : LET ID IN exprexpr : LET ID ID ASSIGN expr IN exprexpr : expr ASSIGN exprexpr : IF expr THEN expr ELSE expr ENDexpr : WHILE expr DO expr ENDexpr : expr SEMICOLON exprexpr : PRINT LPAREN expr RPARENexpr : DESTROY LPAREN expr RPARENexpr : expr DOT IDexpr : expr PLUS expr\n                  | expr MINUS expr\n                  | expr TIMES expr\n                  | expr DIVIDE expr\n                  | expr AND expr\n                  | expr OR expr\n                  | expr EQ expr\n                  | expr GT expr\n                  | expr GTE expr\n                  | expr LT expr\n                  | expr LTE expr\n                  | expr NEQ expr\n                  | expr MOD expr\n                  expr : MINUS expr %prec UMINUSexpr : NOT exprexpr : TIMES expr %prec DEREFexpr : expr REFexpr : LPAREN expr RPARENexpr : LPAREN RPARENexpr : INT_LITERALexpr : BOOL_LITERALexpr : IDvarlist : ID COMMA IDvarlist : ID COMMA varlistexpr : BRANCH LPAREN varlist RPAREN expr ENDexpr : BRANCH LPAREN ID RPAREN expr ENDexpr : BRANCH LPAREN RPAREN expr ENDfield : ID ASSIGN qualifier exprfieldlist : field COMMA fieldfieldlist : field COMMA fieldlistexpr : LCURL fieldlist RCURLexpr : LCURL field RCURLparam : ID COLON qualifier typeparamlist : param COMMA paramparamlist : param COMMA paramlistexpr : FUN LPAREN paramlist RPAREN type ARROW expr ENDexpr : FUN LPAREN param RPAREN type ARROW expr ENDexpr : FUN LPAREN RPAREN type ARROW expr ENDexprlist : expr COMMA exprexprlist : expr COMMA exprlistexpr : ID LPAREN exprlist RPARENexpr : ID LPAREN expr RPARENexpr : ID LPAREN RPAREN'
    
_lr_action_items = {'LET':([0,5,6,8,10,11,12,18,19,21,22,23,24,25,26,27,28,29,30,31,32,33,36,39,42,68,72,73,78,88,92,97,99,103,104,105,106,121,130,140,144,152,],[3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,-13,-14,-15,3,3,3,3,3,]),'IF':([0,5,6,8,10,11,12,18,19,21,22,23,24,25,26,27,28,29,30,31,32,33,36,39,42,68,72,73,78,88,92,97,99,103,104,105,106,121,130,140,144,152,],[5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,-13,-14,-15,5,5,5,5,5,]),'WHILE':([0,5,6,8,10,11,12,18,19,21,22,23,24,25,26,27,28,29,30,31,32,33,36,39,42,68,72,73,78,88,92,97,99,103,104,105,106,121,130,140,144,152,],[6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,-13,-14,-15,6,6,6,6,6,]),'PRINT':([0,5,6,8,10,11,12,18,19,21,22,23,24,25,26,27,28,29,30,31,32,33,36,39,42,68,72,73,78,88,92,97,99,103,104,105,106,121,130,140,144,152,],[7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,-13,-14,-15,7,7,7,7,7,]),'DESTROY':([0,5,6,8,10,11,12,18,19,21,22,23,24,25,26,27,28,29,30,31,32,33,36,39,42,68,72,73,78,88,92,97,99,103,104,105,106,121,130,140,144,152,],[9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,-13,-14,-15,9,9,9,9,9,]),'MINUS':([0,2,4,5,6,8,10,11,12,13,14,18,19,21,22,23,24,25,26,27,28,29,30,31,32,33,34,36,37,38,39,40,41,42,43,44,45,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,68,70,71,72,73,74,75,76,78,80,81,88,89,90,91,92,93,94,95,96,97,98,99,103,104,105,106,118,119,121,122,123,124,125,128,130,140,141,142,143,144,145,152,154,155,156,157,163,164,167,],[10,22,-46,10,10,10,10,10,10,-44,-45,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,-41,10,22,22,10,22,-43,10,-38,-40,-39,22,22,-24,-25,-26,-27,-28,22,22,22,22,22,22,22,22,-37,10,-67,22,10,10,22,-42,22,10,-55,-56,10,22,-65,-66,10,22,22,-22,-23,10,22,10,10,-13,-14,-15,22,22,10,-20,22,-51,22,22,10,10,22,-49,-50,10,22,10,22,-19,22,-62,22,-60,-61,]),'NOT':([0,5,6,8,10,11,12,18,19,21,22,23,24,25,26,27,28,29,30,31,32,33,36,39,42,68,72,73,78,88,92,97,99,103,104,105,106,121,130,140,144,152,],[12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,-13,-14,-15,12,12,12,12,12,]),'TIMES':([0,2,4,5,6,8,10,11,12,13,14,18,19,21,22,23,24,25,26,27,28,29,30,31,32,33,34,36,37,38,39,40,41,42,43,44,45,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,68,70,71,72,73,74,75,76,78,80,81,88,89,90,91,92,93,94,95,96,97,98,99,103,104,105,106,118,119,121,122,123,124,125,128,130,140,141,142,143,144,145,152,154,155,156,157,163,164,167,],[11,23,-46,11,11,11,11,11,11,-44,-45,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,-41,11,23,23,11,23,-43,11,-38,-40,-39,23,23,-24,23,23,-27,-28,23,23,23,23,23,23,23,23,-37,11,-67,23,11,11,23,-42,23,11,-55,-56,11,23,-65,-66,11,23,23,-22,-23,11,23,11,11,-13,-14,-15,23,23,11,-20,23,-51,23,23,11,11,23,-49,-50,11,23,11,23,-19,23,-62,23,-60,-61,]),'LPAREN':([0,4,5,6,7,8,9,10,11,12,15,17,18,19,21,22,23,24,25,26,27,28,29,30,31,32,33,36,39,42,68,72,73,78,88,92,97,99,103,104,105,106,121,130,140,144,152,],[8,36,8,8,39,8,42,8,8,8,46,50,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,-13,-14,-15,8,8,8,8,8,]),'INT_LITERAL':([0,5,6,8,10,11,12,18,19,21,22,23,24,25,26,27,28,29,30,31,32,33,36,39,42,68,72,73,78,88,92,97,99,103,104,105,106,121,130,140,144,152,],[13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,-13,-14,-15,13,13,13,13,13,]),'BOOL_LITERAL':([0,5,6,8,10,11,12,18,19,21,22,23,24,25,26,27,28,29,30,31,32,33,36,39,42,68,72,73,78,88,92,97,99,103,104,105,106,121,130,140,144,152,],[14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,-13,-14,-15,14,14,14,14,14,]),'ID':([0,3,5,6,8,10,11,12,16,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,35,36,39,42,46,50,68,72,73,78,82,88,92,97,99,100,103,104,105,106,110,116,121,130,140,144,152,],[4,35,4,4,4,4,4,4,49,4,4,53,4,4,4,4,4,4,4,4,4,4,4,4,4,67,4,4,4,79,87,4,4,4,4,49,4,4,4,4,126,4,-13,-14,-15,87,87,4,4,4,4,4,]),'BRANCH':([0,5,6,8,10,11,12,18,19,21,22,23,24,25,26,27,28,29,30,31,32,33,36,39,42,68,72,73,78,88,92,97,99,103,104,105,106,121,130,140,144,152,],[15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,-13,-14,-15,15,15,15,15,15,]),'LCURL':([0,5,6,8,10,11,12,18,19,21,22,23,24,25,26,27,28,29,30,31,32,33,36,39,42,68,72,73,78,88,92,97,99,103,104,105,106,121,130,140,144,152,],[16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,-13,-14,-15,16,16,16,16,16,]),'FUN':([0,5,6,8,10,11,12,18,19,21,22,23,24,25,26,27,28,29,30,31,32,33,36,39,42,68,72,73,78,88,92,97,99,103,104,105,106,121,130,140,144,152,],[17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,-13,-14,-15,17,17,17,17,17,]),'$end':([1,2,4,13,14,34,41,43,44,45,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,70,75,80,81,89,90,91,95,96,122,124,142,143,154,155,157,164,167,],[0,-1,-46,-44,-45,-41,-43,-38,-40,-39,-18,-21,-24,-25,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-67,-42,-55,-56,-16,-65,-66,-22,-23,-20,-51,-49,-50,-17,-19,-62,-60,-61,]),'ASSIGN':([2,4,13,14,34,37,38,40,41,43,44,45,49,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,70,71,74,75,76,80,81,89,90,91,93,94,95,96,98,118,119,122,123,124,125,128,141,142,143,145,154,155,156,157,163,164,167,],[18,-46,-44,-45,-41,18,18,18,-43,-38,-40,-39,83,-18,-21,-24,-25,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,88,-67,18,18,-42,18,-55,-56,-16,-65,-66,18,18,-22,-23,18,18,18,-20,18,-51,18,18,18,-49,-50,18,-17,-19,18,-62,18,-60,-61,]),'SEMICOLON':([2,4,13,14,34,37,38,40,41,43,44,45,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,70,71,74,75,76,80,81,89,90,91,93,94,95,96,98,118,119,122,123,124,125,128,141,142,143,145,154,155,156,157,163,164,167,],[19,-46,-44,-45,-41,19,19,19,-43,-38,-40,-39,-18,-21,-24,-25,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-67,19,19,-42,19,-55,-56,-16,-65,-66,19,19,-22,-23,19,19,19,-20,19,-51,19,19,19,-49,-50,19,-17,-19,19,-62,19,-60,-61,]),'DOT':([2,4,13,14,34,37,38,40,41,43,44,45,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,70,71,74,75,76,80,81,89,90,91,93,94,95,96,98,118,119,122,123,124,125,128,141,142,143,145,154,155,156,157,163,164,167,],[20,-46,-44,-45,-41,20,20,20,-43,20,20,20,20,20,-24,20,20,20,20,20,20,20,20,20,20,20,20,20,-67,20,20,-42,20,-55,-56,20,-65,-66,20,20,-22,-23,20,20,20,-20,20,-51,20,20,20,-49,-50,20,20,-19,20,-62,20,-60,-61,]),'PLUS':([2,4,13,14,34,37,38,40,41,43,44,45,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,70,71,74,75,76,80,81,89,90,91,93,94,95,96,98,118,119,122,123,124,125,128,141,142,143,145,154,155,156,157,163,164,167,],[21,-46,-44,-45,-41,21,21,21,-43,-38,-40,-39,21,21,-24,-25,-26,-27,-28,21,21,21,21,21,21,21,21,-37,-67,21,21,-42,21,-55,-56,21,-65,-66,21,21,-22,-23,21,21,21,-20,21,-51,21,21,21,-49,-50,21,21,-19,21,-62,21,-60,-61,]),'DIVIDE':([2,4,13,14,34,37,38,40,41,43,44,45,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,70,71,74,75,76,80,81,89,90,91,93,94,95,96,98,118,119,122,123,124,125,128,141,142,143,145,154,155,156,157,163,164,167,],[24,-46,-44,-45,-41,24,24,24,-43,-38,-40,-39,24,24,-24,24,24,-27,-28,24,24,24,24,24,24,24,24,-37,-67,24,24,-42,24,-55,-56,24,-65,-66,24,24,-22,-23,24,24,24,-20,24,-51,24,24,24,-49,-50,24,24,-19,24,-62,24,-60,-61,]),'AND':([2,4,13,14,34,37,38,40,41,43,44,45,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,70,71,74,75,76,80,81,89,90,91,93,94,95,96,98,118,119,122,123,124,125,128,141,142,143,145,154,155,156,157,163,164,167,],[25,-46,-44,-45,-41,25,25,25,-43,-38,-40,-39,25,25,-24,-25,-26,-27,-28,-29,25,-31,-32,-33,-34,-35,-36,-37,-67,25,25,-42,25,-55,-56,-16,-65,-66,25,25,-22,-23,25,25,25,-20,25,-51,25,25,25,-49,-50,25,-17,-19,25,-62,25,-60,-61,]),'OR':([2,4,13,14,34,37,38,40,41,43,44,45,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,70,71,74,75,76,80,81,89,90,91,93,94,95,96,98,118,119,122,123,124,125,128,141,142,143,145,154,155,156,157,163,164,167,],[26,-46,-44,-45,-41,26,26,26,-43,-38,-40,-39,26,26,-24,-25,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-67,26,26,-42,26,-55,-56,-16,-65,-66,26,26,-22,-23,26,26,26,-20,26,-51,26,26,26,-49,-50,26,-17,-19,26,-62,26,-60,-61,]),'EQ':([2,4,13,14,34,37,38,40,41,43,44,45,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,70,71,74,75,76,80,81,89,90,91,93,94,95,96,98,118,119,122,123,124,125,128,141,142,143,145,154,155,156,157,163,164,167,],[27,-46,-44,-45,-41,27,27,27,-43,-38,-40,-39,27,27,-24,-25,-26,-27,-28,27,27,-31,-32,-33,-34,-35,-36,-37,-67,27,27,-42,27,-55,-56,27,-65,-66,27,27,-22,-23,27,27,27,-20,27,-51,27,27,27,-49,-50,27,27,-19,27,-62,27,-60,-61,]),'GT':([2,4,13,14,34,37,38,40,41,43,44,45,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,70,71,74,75,76,80,81,89,90,91,93,94,95,96,98,109,112,113,114,118,119,122,123,124,125,128,131,132,137,138,141,142,143,145,146,147,153,154,155,156,157,159,163,164,165,166,167,],[28,-46,-44,-45,-41,28,28,28,-43,-38,-40,-39,28,28,-24,-25,-26,-27,-28,28,28,28,-32,-33,-34,-35,28,-37,-67,28,28,-42,28,-55,-56,28,-65,-66,28,28,-22,-23,28,-5,-2,-3,-4,28,28,-20,28,-51,28,28,146,147,-58,-59,28,-49,-50,28,-8,-9,-57,28,-19,28,-62,-12,28,-60,-10,-11,-61,]),'GTE':([2,4,13,14,34,37,38,40,41,43,44,45,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,70,71,74,75,76,80,81,89,90,91,93,94,95,96,98,118,119,122,123,124,125,128,141,142,143,145,154,155,156,157,163,164,167,],[29,-46,-44,-45,-41,29,29,29,-43,-38,-40,-39,29,29,-24,-25,-26,-27,-28,29,29,29,-32,-33,-34,-35,29,-37,-67,29,29,-42,29,-55,-56,29,-65,-66,29,29,-22,-23,29,29,29,-20,29,-51,29,29,29,-49,-50,29,29,-19,29,-62,29,-60,-61,]),'LT':([2,4,13,14,34,37,38,40,41,43,44,45,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,70,71,74,75,76,80,81,85,89,90,91,93,94,95,96,98,104,105,106,107,111,115,118,119,122,123,124,125,128,134,139,141,142,143,145,148,150,151,154,155,156,157,163,164,167,],[30,-46,-44,-45,-41,30,30,30,-43,-38,-40,-39,30,30,-24,-25,-26,-27,-28,30,30,30,-32,-33,-34,-35,30,-37,-67,30,30,-42,30,-55,-56,110,30,-65,-66,30,30,-22,-23,30,-13,-14,-15,110,110,110,30,30,-20,30,-51,30,30,110,110,30,-49,-50,30,110,110,110,30,-19,30,-62,30,-60,-61,]),'LTE':([2,4,13,14,34,37,38,40,41,43,44,45,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,70,71,74,75,76,80,81,89,90,91,93,94,95,96,98,118,119,122,123,124,125,128,141,142,143,145,154,155,156,157,163,164,167,],[31,-46,-44,-45,-41,31,31,31,-43,-38,-40,-39,31,31,-24,-25,-26,-27,-28,31,31,31,-32,-33,-34,-35,31,-37,-67,31,31,-42,31,-55,-56,31,-65,-66,31,31,-22,-23,31,31,31,-20,31,-51,31,31,31,-49,-50,31,31,-19,31,-62,31,-60,-61,]),'NEQ':([2,4,13,14,34,37,38,40,41,43,44,45,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,70,71,74,75,76,80,81,89,90,91,93,94,95,96,98,118,119,122,123,124,125,128,141,142,143,145,154,155,156,157,163,164,167,],[32,-46,-44,-45,-41,32,32,32,-43,-38,-40,-39,32,32,-24,-25,-26,-27,-28,32,32,-31,-32,-33,-34,-35,-36,-37,-67,32,32,-42,32,-55,-56,32,-65,-66,32,32,-22,-23,32,32,32,-20,32,-51,32,32,32,-49,-50,32,32,-19,32,-62,32,-60,-61,]),'MOD':([2,4,13,14,34,37,38,40,41,43,44,45,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,70,71,74,75,76,80,81,89,90,91,93,94,95,96,98,118,119,122,123,124,125,128,141,142,143,145,154,155,156,157,163,164,167,],[33,-46,-44,-45,-41,33,33,33,-43,-38,-40,-39,33,33,-24,33,33,-27,-28,33,33,33,33,33,33,33,33,-37,-67,33,33,-42,33,-55,-56,33,-65,-66,33,33,-22,-23,33,33,33,-20,33,-51,33,33,33,-49,-50,33,33,-19,33,-62,33,-60,-61,]),'REF':([2,4,13,14,34,37,38,40,41,43,44,45,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,70,71,74,75,76,80,81,89,90,91,93,94,95,96,98,118,119,122,123,124,125,128,141,142,143,145,154,155,156,157,163,164,167,],[34,-46,-44,-45,-41,34,34,34,-43,-38,-40,-39,-18,-21,-24,-25,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-67,34,34,-42,34,-55,-56,-16,-65,-66,34,34,-22,-23,34,34,34,-20,34,-51,34,34,34,-49,-50,34,-17,-19,34,-62,34,-60,-61,]),'THEN':([4,13,14,34,37,41,43,44,45,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,70,75,80,81,89,90,91,95,96,122,124,142,143,154,155,157,164,167,],[-46,-44,-45,-41,72,-43,-38,-40,-39,-18,-21,-24,-25,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-67,-42,-55,-56,-16,-65,-66,-22,-23,-20,-51,-49,-50,-17,-19,-62,-60,-61,]),'DO':([4,13,14,34,38,41,43,44,45,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,70,75,80,81,89,90,91,95,96,122,124,142,143,154,155,157,164,167,],[-46,-44,-45,-41,73,-43,-38,-40,-39,-18,-21,-24,-25,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-67,-42,-55,-56,-16,-65,-66,-22,-23,-20,-51,-49,-50,-17,-19,-62,-60,-61,]),'RPAREN':([4,8,13,14,34,36,40,41,43,44,45,46,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,69,70,71,74,75,76,77,79,80,81,84,86,89,90,91,95,96,109,112,113,114,119,120,122,124,126,127,137,138,142,143,146,147,153,154,155,157,159,164,165,166,167,],[-46,41,-44,-45,-41,70,75,-43,-38,-40,-39,78,85,-18,-21,-24,-25,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,90,-67,91,95,-42,96,97,99,-55,-56,107,115,-16,-65,-66,-22,-23,-5,-2,-3,-4,-63,-64,-20,-51,-47,-48,-58,-59,-49,-50,-8,-9,-57,-17,-19,-62,-12,-60,-10,-11,-61,]),'COMMA':([4,13,14,34,41,43,44,45,48,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,70,71,75,79,80,81,86,89,90,91,95,96,101,109,112,113,114,119,122,124,126,128,132,135,137,142,143,146,147,153,154,155,157,159,161,164,165,166,167,],[-46,-44,-45,-41,-43,-38,-40,-39,82,-18,-21,-24,-25,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-67,92,-42,100,-55,-56,116,-16,-65,-66,-22,-23,82,-5,-2,-3,-4,92,-20,-51,100,-52,116,151,116,-49,-50,-8,-9,-57,-17,-19,-62,-12,151,-60,-10,-11,-61,]),'ELSE':([4,13,14,34,41,43,44,45,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,70,75,80,81,89,90,91,93,95,96,122,124,142,143,154,155,157,164,167,],[-46,-44,-45,-41,-43,-38,-40,-39,-18,-21,-24,-25,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-67,-42,-55,-56,-16,-65,-66,121,-22,-23,-20,-51,-49,-50,-17,-19,-62,-60,-61,]),'END':([4,13,14,34,41,43,44,45,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,70,75,80,81,89,90,91,94,95,96,98,122,123,124,125,141,142,143,145,154,155,156,157,163,164,167,],[-46,-44,-45,-41,-43,-38,-40,-39,-18,-21,-24,-25,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-67,-42,-55,-56,-16,-65,-66,122,-22,-23,124,-20,142,-51,143,155,-49,-50,157,-17,-19,164,-62,167,-60,-61,]),'IN':([4,13,14,34,35,41,43,44,45,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,70,75,80,81,89,90,91,95,96,118,122,124,142,143,154,155,157,164,167,],[-46,-44,-45,-41,68,-43,-38,-40,-39,-18,-21,-24,-25,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-67,-42,-55,-56,-16,-65,-66,-22,-23,140,-20,-51,-49,-50,-17,-19,-62,-60,-61,]),'RCURL':([4,13,14,34,41,43,44,45,47,48,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,70,75,80,81,89,90,91,95,96,101,102,122,124,128,142,143,154,155,157,164,167,],[-46,-44,-45,-41,-43,-38,-40,-39,80,81,-18,-21,-24,-25,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-67,-42,-55,-56,-16,-65,-66,-22,-23,-53,-54,-20,-51,-52,-49,-50,-17,-19,-62,-60,-61,]),'A':([83,117,],[104,104,]),'B':([83,117,],[105,105,]),'U':([83,117,],[106,106,]),'LBRAC':([85,104,105,106,107,111,115,134,139,148,150,151,],[111,-13,-14,-15,111,111,111,111,111,111,111,111,]),'INT':([85,104,105,106,107,111,115,134,139,148,150,151,],[112,-13,-14,-15,112,112,112,112,112,112,112,112,]),'BOOL':([85,104,105,106,107,111,115,134,139,148,150,151,],[113,-13,-14,-15,113,113,113,113,113,113,113,113,]),'UNIT':([85,104,105,106,107,111,115,134,139,148,150,151,],[114,-13,-14,-15,114,114,114,114,114,114,114,114,]),'COLON':([87,],[117,]),'ARROW':([108,109,111,112,113,114,129,133,135,136,146,147,159,161,162,165,166,],[130,-5,134,-2,-3,-4,144,148,150,152,-8,-9,-12,-6,-7,-10,-11,]),'RBRAC':([109,112,113,114,146,147,149,158,159,160,165,166,],[-5,-2,-3,-4,-8,-9,159,165,-12,166,-10,-11,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'program':([0,],[1,]),'expr':([0,5,6,8,10,11,12,18,19,21,22,23,24,25,26,27,28,29,30,31,32,33,36,39,42,68,72,73,78,88,92,97,99,103,121,130,140,144,152,],[2,37,38,40,43,44,45,51,52,54,55,56,57,58,59,60,61,62,63,64,65,66,71,74,76,89,93,94,98,118,119,123,125,128,141,145,154,156,163,]),'fieldlist':([16,82,],[47,102,]),'field':([16,82,],[48,101,]),'exprlist':([36,92,],[69,120,]),'varlist':([46,100,],[77,127,]),'paramlist':([50,110,116,],[84,131,138,]),'param':([50,110,116,],[86,132,137,]),'qualifier':([83,117,],[103,139,]),'type':([85,107,111,115,134,139,148,150,151,],[108,129,135,136,149,153,158,160,161,]),'simple_type':([85,107,111,115,134,139,148,150,151,],[109,109,109,109,109,109,109,109,109,]),'tlist':([111,151,],[133,162,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> program","S'",1,None,None,None),
  ('program -> expr','program',1,'p_program','parsing.py',21),
  ('simple_type -> INT','simple_type',1,'p_type_int','parsing.py',28),
  ('simple_type -> BOOL','simple_type',1,'p_type_bool','parsing.py',33),
  ('simple_type -> UNIT','simple_type',1,'p_type_unit','parsing.py',38),
  ('type -> simple_type','type',1,'p_type','parsing.py',43),
  ('tlist -> type COMMA type','tlist',3,'p_tlist','parsing.py',48),
  ('tlist -> type COMMA tlist','tlist',3,'p_tlist_multi','parsing.py',53),
  ('type -> LT paramlist GT','type',3,'p_type_obj','parsing.py',63),
  ('type -> LT param GT','type',3,'p_type_obj_single','parsing.py',72),
  ('type -> LBRAC tlist ARROW type RBRAC','type',5,'p_tfunc','parsing.py',77),
  ('type -> LBRAC type ARROW type RBRAC','type',5,'p_tfunc_single','parsing.py',82),
  ('type -> LBRAC ARROW type RBRAC','type',4,'p_tfunc_none','parsing.py',88),
  ('qualifier -> A','qualifier',1,'p_qualifier_a','parsing.py',108),
  ('qualifier -> B','qualifier',1,'p_qualifier_b','parsing.py',113),
  ('qualifier -> U','qualifier',1,'p_qualifier_u','parsing.py',118),
  ('expr -> LET ID IN expr','expr',4,'p_cap_decl','parsing.py',126),
  ('expr -> LET ID ID ASSIGN expr IN expr','expr',7,'p_var_decl','parsing.py',132),
  ('expr -> expr ASSIGN expr','expr',3,'p_assign','parsing.py',138),
  ('expr -> IF expr THEN expr ELSE expr END','expr',7,'p_if','parsing.py',144),
  ('expr -> WHILE expr DO expr END','expr',5,'p_while','parsing.py',150),
  ('expr -> expr SEMICOLON expr','expr',3,'p_seq','parsing.py',156),
  ('expr -> PRINT LPAREN expr RPAREN','expr',4,'p_print','parsing.py',162),
  ('expr -> DESTROY LPAREN expr RPAREN','expr',4,'p_destroy','parsing.py',168),
  ('expr -> expr DOT ID','expr',3,'p_get','parsing.py',174),
  ('expr -> expr PLUS expr','expr',3,'p_binop','parsing.py',183),
  ('expr -> expr MINUS expr','expr',3,'p_binop','parsing.py',184),
  ('expr -> expr TIMES expr','expr',3,'p_binop','parsing.py',185),
  ('expr -> expr DIVIDE expr','expr',3,'p_binop','parsing.py',186),
  ('expr -> expr AND expr','expr',3,'p_binop','parsing.py',187),
  ('expr -> expr OR expr','expr',3,'p_binop','parsing.py',188),
  ('expr -> expr EQ expr','expr',3,'p_binop','parsing.py',189),
  ('expr -> expr GT expr','expr',3,'p_binop','parsing.py',190),
  ('expr -> expr GTE expr','expr',3,'p_binop','parsing.py',191),
  ('expr -> expr LT expr','expr',3,'p_binop','parsing.py',192),
  ('expr -> expr LTE expr','expr',3,'p_binop','parsing.py',193),
  ('expr -> expr NEQ expr','expr',3,'p_binop','parsing.py',194),
  ('expr -> expr MOD expr','expr',3,'p_binop','parsing.py',195),
  ('expr -> MINUS expr','expr',2,'p_uminus','parsing.py',227),
  ('expr -> NOT expr','expr',2,'p_unot','parsing.py',233),
  ('expr -> TIMES expr','expr',2,'p_uderef','parsing.py',239),
  ('expr -> expr REF','expr',2,'p_ref','parsing.py',245),
  ('expr -> LPAREN expr RPAREN','expr',3,'p_group','parsing.py',251),
  ('expr -> LPAREN RPAREN','expr',2,'p_unit','parsing.py',259),
  ('expr -> INT_LITERAL','expr',1,'p_int','parsing.py',265),
  ('expr -> BOOL_LITERAL','expr',1,'p_boolean','parsing.py',271),
  ('expr -> ID','expr',1,'p_var','parsing.py',277),
  ('varlist -> ID COMMA ID','varlist',3,'p_varlist','parsing.py',283),
  ('varlist -> ID COMMA varlist','varlist',3,'p_varlist_multi','parsing.py',289),
  ('expr -> BRANCH LPAREN varlist RPAREN expr END','expr',6,'p_branch','parsing.py',295),
  ('expr -> BRANCH LPAREN ID RPAREN expr END','expr',6,'p_branch_single','parsing.py',301),
  ('expr -> BRANCH LPAREN RPAREN expr END','expr',5,'p_branch_empty','parsing.py',307),
  ('field -> ID ASSIGN qualifier expr','field',4,'p_field','parsing.py',313),
  ('fieldlist -> field COMMA field','fieldlist',3,'p_fieldlist','parsing.py',318),
  ('fieldlist -> field COMMA fieldlist','fieldlist',3,'p_fieldlist_multi','parsing.py',323),
  ('expr -> LCURL fieldlist RCURL','expr',3,'p_object','parsing.py',328),
  ('expr -> LCURL field RCURL','expr',3,'p_object_single','parsing.py',338),
  ('param -> ID COLON qualifier type','param',4,'p_param','parsing.py',347),
  ('paramlist -> param COMMA param','paramlist',3,'p_paramlist','parsing.py',352),
  ('paramlist -> param COMMA paramlist','paramlist',3,'p_paramlist_multi','parsing.py',357),
  ('expr -> FUN LPAREN paramlist RPAREN type ARROW expr END','expr',8,'p_func','parsing.py',362),
  ('expr -> FUN LPAREN param RPAREN type ARROW expr END','expr',8,'p_func_single','parsing.py',371),
  ('expr -> FUN LPAREN RPAREN type ARROW expr END','expr',7,'p_func_none','parsing.py',377),
  ('exprlist -> expr COMMA expr','exprlist',3,'p_exprlist','parsing.py',383),
  ('exprlist -> expr COMMA exprlist','exprlist',3,'p_exprlist_multi','parsing.py',388),
  ('expr -> ID LPAREN exprlist RPAREN','expr',4,'p_call','parsing.py',393),
  ('expr -> ID LPAREN expr RPAREN','expr',4,'p_call_single','parsing.py',399),
  ('expr -> ID LPAREN RPAREN','expr',3,'p_call_none','parsing.py',405),
]
