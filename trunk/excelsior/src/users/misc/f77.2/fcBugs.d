DEFINITION MODULE fcBugs;

FROM fcScan     IMPORT Psymbol, Formsym;

PROCEDURE InitStr;
PROCEDURE AddStr(S:ARRAY OF CHAR);
PROCEDURE OutStr;
PROCEDURE VisPosKW(sy:Psymbol; VAR S:ARRAY OF CHAR);
PROCEDURE Visfsym(sy:Formsym; VAR S:ARRAY OF CHAR);

END fcBugs.
