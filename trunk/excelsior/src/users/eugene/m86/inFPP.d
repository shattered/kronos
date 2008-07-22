DEFINITION MODULE inFPP; (* Sem 25-Mar-91. (c) KRONOS *)

IMPORT  sym : inSym;

TYPE
  cmd_cop = (c_mov,c_add,c_sub,c_mul,c_div,c_cmp);
  ucm_cop = (u_abs,u_neg);

VAR
  stk : INTEGER;
  top : sym.access;

PROCEDURE load         (c: cmd_cop; a: sym.access; sz: INTEGER);
PROCEDURE load_integer (c: cmd_cop; a: sym.access; sz: INTEGER);

PROCEDURE ucm          (c: ucm_cop);

PROCEDURE store        (a: sym.access; sz: INTEGER);
PROCEDURE store_integer(a: sym.access; sz: INTEGER);

END inFPP.
