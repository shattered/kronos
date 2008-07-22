DEFINITION MODULE fcStm; (* 03-Nov-88. *)

FROM fcExpr IMPORT Dexpr;

VAR EndUnit:BOOLEAN;

PROCEDURE Statement;
PROCEDURE AssTypeConv(VAR obl,obr:Dexpr):BOOLEAN;
PROCEDURE InitLabels;
PROCEDURE gLoclab():INTEGER;
PROCEDURE FinishCheck;
PROCEDURE InitStm;
PROCEDURE GetFmt(lab:INTEGER);
PROCEDURE SetFmt(lab,strofs:INTEGER);
PROCEDURE PutLabel(label,cp:INTEGER);
PROCEDURE Goto(cond,fixed:BOOLEAN; label:INTEGER);

END fcStm.
