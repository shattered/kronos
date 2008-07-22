IMPLEMENTATION MODULE lkOpt; (* 24-Mar-89. *)

FROM SYSTEM   IMPORT ADDRESS;

FROM fcGen    IMPORT ProcCode, c2, cp, COD;
FROM fcHeap   IMPORT Give, Free, BackHeap;


TYPE Plabel = POINTER TO labelem;
     labelem= RECORD
                cplab : INTEGER;
                label : INTEGER;
                next  : Plabel;
              END;

     Pjump  = POINTER TO jumpelem;
     jumpelem= RECORD
                 cpjump : INTEGER;
                 plab   : Plabel;
                 stage  : INTEGER;
                 next   : Pjump;
               END;

     Pldform = POINTER TO ldform;
     ldform  = RECORD
                 cpldform : INTEGER;
                 formlab  : INTEGER;
                 next     : Pldform;
               END;

     Pformat = POINTER TO format;
     format  = RECORD
                 formlab  : INTEGER;
                 offs     : INTEGER;
                 next     : Pformat;
               END;

VAR labList : Plabel;
    jumpList: Pjump;
    ldfList : Pldform;
    formList: Pformat;

CONST JFLC = 18h; JFL = 19h; JBLC = 1Ch; JBL = 1Dh;
      LSTA = 0C2h;
      fix = 31;  short = 30;

PROCEDURE InitLabs;
BEGIN
  BackHeap;
  labList :=NIL;
  jumpList:=NIL;
  ldfList :=NIL;
  formList:=NIL;
END InitLabs;

PROCEDURE setFormat(label,offs:INTEGER);
  VAR pform:Pformat;
BEGIN
  Give(pform,SIZE(format));
  pform^.formlab:=label;
  pform^.offs:=offs;
  pform^.next:=formList;
  formList:=pform;
END setFormat;

PROCEDURE setldFormat(label:INTEGER);
  VAR pldform:Pldform;
BEGIN
  Give(pldform,SIZE(ldform));
  WITH pldform^ DO
    cpldform:=cp;
    formlab:=label;
    next:=ldfList;
  END;
  ldfList:=pldform;
  ProcCode; c2(LSTA,0,0);
END setldFormat;

PROCEDURE ldFormat;
  VAR pld:Pldform; pform:Pformat;
      label,cp0,offs:INTEGER;
BEGIN
  pld:=ldfList;
  WHILE pld#NIL DO
    cp0:=pld^.cpldform; label:=pld^.formlab;
    pform:=formList; offs:=0;
    WHILE (pform#NIL) AND (offs=0) DO
      IF pform^.formlab = label THEN
         offs:=pform^.offs;
      END;
      pform:=pform^.next;
    END;
  --  IF offs=0 THEN Error(xx) END;
    COD[cp0+1]:=CHAR(offs MOD 100h);
    COD[cp0+2]:=CHAR(offs DIV 100h);
    pld:=pld^.next;
  END;
END ldFormat;

PROCEDURE fLabel(label:INTEGER):ADDRESS;
  VAR plab:Plabel; notfind:BOOLEAN;
BEGIN
  plab:=labList; notfind:=TRUE;
  WHILE notfind AND (plab#NIL) DO
    IF plab^.label=label THEN notfind:=FALSE;
    ELSE plab:=plab^.next
    END;
  END;
  IF notfind THEN
    Give(plab,SIZE(labelem));
    plab^.cplab:=0;
    plab^.label:=label;
    plab^.next :=labList;
    labList:=plab;
  END;
  RETURN plab
END fLabel;

PROCEDURE setLabel(label:INTEGER);
  VAR plab:Plabel;
BEGIN
  plab:=fLabel(label);
  plab^.cplab:=cp;
END setLabel;

PROCEDURE setJump(label:INTEGER; fixed, cond:BOOLEAN);
  VAR pjump:Pjump; plab:Plabel;
      stage, code: INTEGER;
BEGIN
  Give(pjump,SIZE(jumpelem));
  plab:=fLabel(label);
  pjump^.cpjump:=cp;
  pjump^.plab:=plab;
  stage:=cp;
  IF fixed THEN stage:=INTEGER(BITSET(stage)+{fix}); END;
  pjump^.stage:=stage;
  pjump^.next:=jumpList;
  jumpList:=pjump;
  IF plab^.cplab>0 THEN
    IF cond THEN code:=JBLC ELSE code:=JBL END;
  ELSE
    IF cond THEN code:=JFLC ELSE code:=JFL END;
  END;
  ProcCode; c2(code,0,0);
END setJump;

PROCEDURE correctjump(pjump:Pjump);
  VAR plab:Plabel;
      jump:Pjump;
      cpjump:INTEGER;
BEGIN
  cpjump:=pjump^.stage;
  pjump^.stage:=INTEGER(BITSET(pjump^.stage)+{short});
  plab:=labList;
  WHILE plab#NIL DO
    IF plab^.cplab>cpjump THEN DEC(plab^.cplab); END;
    plab:=plab^.next;
  END;
  jump:=jumpList;
  WHILE jump>pjump DO
    DEC(jump^.stage);
    jump:=jump^.next;
  END;
END correctjump;

PROCEDURE optJump;
  VAR corrected:BOOLEAN;
      pjump:Pjump;
      cplab,offs, stage:INTEGER;
      notfixed,long:BOOLEAN;
BEGIN
   corrected:=TRUE;
   WHILE corrected  DO
     corrected:=FALSE;
     pjump:=jumpList;
     WHILE pjump#NIL  DO
       stage:=pjump^.stage;
       notfixed:=NOT(fix   IN BITSET(stage));
       long:=    NOT(short IN BITSET(stage));
       IF notfixed AND long THEN
         cplab:= pjump^.plab^.cplab;
         offs:= stage-cplab;
         IF offs<0 THEN offs:=2-offs ELSE offs:=offs+2; END;
         IF offs<=255 THEN
           corrected:=TRUE;
           correctjump(pjump);
         END;
       END;
       pjump:=pjump^.next;
     END;
   END;
END optJump;

PROCEDURE procJump;
  VAR pjump:Pjump;
      offs: INTEGER;
      stage,cpjump,i:INTEGER;
BEGIN
  pjump:=jumpList;
  WHILE pjump#NIL DO
    stage:=INTEGER(BITSET(pjump^.stage)*{0..29});
    offs:=stage-pjump^.plab^.cplab;
    cpjump:=pjump^.cpjump;
    IF short IN BITSET(pjump^.stage) THEN
       -- copy code
      FOR i:=cpjump+2 TO cp-1 DO
        COD[i-1]:=COD[i];
      END;
      cp:=cp-1;
      COD[cpjump]:=CHAR(INTEGER(COD[cpjump])+2);
      IF offs<0 THEN offs:=-2-offs ELSE offs:=offs+2; END;
      COD[cpjump+1]:=CHAR(offs);
    ELSE
      -- put offs
      IF offs<0 THEN offs:=-3-offs ELSE offs:=offs+3; END;
      COD[cpjump+1]:=CHAR(offs MOD 100h);
      COD[cpjump+2]:=CHAR(offs DIV 100h);
    END;
    pjump:=pjump^.next;
  END;
END procJump;

END lkOpt.
