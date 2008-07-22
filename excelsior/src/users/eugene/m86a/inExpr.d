DEFINITION MODULE inExpr; (* Sem 04-Mar-91. (c) KRONOS *)

IMPORT  pc  : pcTab;
IMPORT  sym : inSym;
IMPORT  flw : inFlow;

VAR
  gen_call: PROCEDURE (pc.ref);

PROCEDURE gen_condition(l: pc.ref; then,else: flw.node);

PROCEDURE expression(l: pc.ref; VAR a: sym.access);

PROCEDURE bytes_i(l: pc.ref): INTEGER;

END inExpr.
