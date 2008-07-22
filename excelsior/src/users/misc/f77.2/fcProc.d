DEFINITION MODULE fcProc; (* 04-Nov-88.  *)

FROM fcObj    IMPORT Info, Idname, Types;
FROM fcExpr   IMPORT Dexpr;

PROCEDURE SubrCall(VAR I:Info);
PROCEDURE FuncCall(VAR ob:Dexpr);
PROCEDURE LookIntr(VAR Id:Idname):INTEGER;

END fcProc.
