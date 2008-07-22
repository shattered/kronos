IMPLEMENTATION MODULE fcBugs;

FROM StdIO      IMPORT Show;
FROM fcScan     IMPORT Psymbol, Formsym;
VAR Str: ARRAY [0..79] OF CHAR;
    pos:INTEGER;

PROCEDURE InitStr;
BEGIN
 pos:=0;
END InitStr;

PROCEDURE OutStr;
BEGIN
  IF pos=0 THEN RETURN
  ELSE Str[pos]:=0c; Show(Str); pos:=0;
  END;
END OutStr;

PROCEDURE AddStr(c:ARRAY OF CHAR);
VAR l,i:INTEGER;
BEGIN
  l:=HIGH(c);
  IF (pos+l) > 78 THEN OutStr END;
  i:=0;
  WHILE c[i]#0c DO
   Str[pos]:=c[i]; INC(pos); INC(i);
  END; Str[pos]:=40c; INC(pos);
END AddStr;

PROCEDURE VisPosKW(sym:Psymbol;VAR S:ARRAY OF CHAR);
BEGIN
  CASE sym OF
   access: S:="'ACCESS='";
  |blank:  S:="'BLANK='";
  |direct: S:="'DIRECT='";
  |err:    S:="'ERR='";
  |lend:   S:="'END='";
  |exist:  S:="'EXIST='";
  |formatted: S:="'FORMATTED='";
  |form:   S:="'FORM='";
  |file:   S:="'FILE='";
  |fmt:    S:="'FMT='";
  |iostat: S:="'IOSTAT='";
  |named:  S:="'NAMED='";
  |nextrec:S:="'NEXTREC='";
  |number: S:="'NUMBER='";
  |name:   S:="'NAME='";
  |opened: S:="'OPENED='";
  |recl:   S:="'RECL='";
  |rec:    S:="'REC='";
  |sequential: S:="'SEQUENTIAL='";
  |status: S:="'STATUS='";
  |unformatted:S:="'UNFORMATTED='";
  |unit:   S:="'UNIT='";
  ELSE     S:="'invPkw'";
  END;
END VisPosKW;

PROCEDURE Visfsym(fsym:Formsym; VAR s:ARRAY OF CHAR);
BEGIN
  CASE fsym OF
   invd: s:="'invd'";
  |dlpar: s:="'('" |dcomma: s:="','";
  |dplus: s:="'+'" |dslash: s:="'/'";
  |drpar: s:="')'";
  |dminus:s:="'-'";|dcolon: s:="':':";
  |dstring:s:="'Ccnst'";
  |dnum:   s:="'Icnst'";
  |dot:  s:="'.'"; |dA: s:="'A'";
  |dI: s:="'I'";   |dF: s:="'F'"; |dE: s:="'E'";
  |dG: s:="'G'";   |dD: s:="'D'"; |dL: s:="'L'";
  |dX: s:="'X'";   |dP: s:="'P'"; |dH: s:="'H'";
  |dTL:s:="'TL'";  |dTR:s:="'TR'";|dT: s:="'T'";
  |dSP:s:="'SP'";  |dSS:s:="'SS'";|dS: s:="'S'";
  |dBN:s:="'BN'";  |dBZ:s:="'BZ'";
  ELSE s:="'invd'";
  END; (* c a s e *)
END Visfsym;

BEGIN
END fcBugs.
