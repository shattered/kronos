DEFINITION MODULE fcExpr;

FROM fcObj  IMPORT Types, Info;
FROM SYSTEM IMPORT WORD;
FROM fcDefs IMPORT maxpar;

TYPE Emode = (invmd,expr,cexpr,var,arrel,substr,funcall,arr,ext,star);
     Dexpr = RECORD
              tp   : Types;
              emd  : Emode;
              start: INTEGER;
              name : INTEGER;
              wd   : WORD;
             END;
     arrob = ARRAY[0..maxpar] OF Dexpr;
     bind  = RECORD len,val : INTEGER; END;

PROCEDURE Lexpr(VAR ob:Dexpr);

PROCEDURE Expr(VAR ob:Dexpr);

PROCEDURE lExpr(VAR ob:Dexpr);

PROCEDURE tExpr(VAR ob:Dexpr);

PROCEDURE aExpr(VAR ob:Dexpr);

PROCEDURE cExpr(VAR ob:Dexpr);

PROCEDURE icExpr(VAR ob:Dexpr);

PROCEDURE iExpr(VAR ob:Dexpr);
PROCEDURE bIndex(VAR bi:bind):BOOLEAN;
PROCEDURE SetFN(i:INTEGER);
END fcExpr.
