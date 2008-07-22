DEFINITION MODULE fcInd;

FROM fcExpr  IMPORT Dexpr;
FROM fcObj   IMPORT Info;


PROCEDURE ldSubstr(VAR I:Info; VAR ob:Dexpr);
PROCEDURE LoadInd(VAR I:Info;):INTEGER;
PROCEDURE EvalAdr(VAR ob:Dexpr);
PROCEDURE EvalVal(VAR I:Info; VAR ob:Dexpr);
PROCEDURE LeftAdr(VAR I:Info; VAR ob:Dexpr);
PROCEDURE StoreIn(VAR obl,obr:Dexpr);
PROCEDURE LoadVal(VAR I:Info;);
PROCEDURE LoadAdr(VAR I:Info;);
PROCEDURE StoreInVar(VAR I:Info;);
PROCEDURE gDovarA(VAR I:Info;);
PROCEDURE gDovarV(VAR I:Info;);
PROCEDURE gDovarF(VAR I:Info;);
PROCEDURE InsertC(VAR ob:Dexpr; cp0:INTEGER);
PROCEDURE LoadConst(VAR ob:Dexpr;);
PROCEDURE CompCstr(ofs0,ofs1:INTEGER;):INTEGER;
PROCEDURE cCat(L,R:Dexpr);
PROCEDURE genCxCmp(eq:BOOLEAN);
PROCEDURE genCxAdd(add:BOOLEAN);
PROCEDURE genCxMul(mul:BOOLEAN);


END fcInd.
