IMPLEMENTATION MODULE pedChain; (* 08-Feb-87. (c) KRONOS *)

FROM SYSTEM    IMPORT   ADR, ADDRESS;
FROM StdIO     IMPORT   Open, Close, sRead, Show, Stream, Write, print,
                        WriteLn, WriteString, Confirm;
FROM Strings   IMPORT   Str1, Str0, InsCh;
FROM ASCII     IMPORT   NL;

FROM pedModel  IMPORT   board, cre_signal, cre_chip, ReadType, string,
                        signal, ctype, chip, cre_board, tie;

VAR Errors: ARRAY [0..7] OF RECORD
              Ln,Lm,Pz: INTEGER;
              Text,Mess: ARRAY [0..79] OF CHAR;
            END;
    ErrCnt: INTEGER;
    LineWithError: BOOLEAN;
    Line: ARRAY [0..255] OF CHAR;
    LineCnt,LineMCnt,Poz,SyPoz: INTEGER;

PROCEDURE Error(s: ARRAY OF CHAR);
BEGIN
  IF ErrCnt<=HIGH(Errors) THEN
    WITH Errors[ErrCnt] DO
      Str0(Text); Str1(Mess,s);
      Ln:=LineCnt; Pz:=SyPoz;
      Lm:=LineMCnt;
      LineWithError:=TRUE;
    END;
    INC(ErrCnt);
  END;
END Error;

PROCEDURE PrintErrors;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO ErrCnt-1 DO
    WriteLn;
    WITH Errors[i] DO
      print('  ***** Line %3d %2d. %s\n',Ln,Lm,Mess);
      IF Pz<0 THEN Pz:=0 END;
      InsCh(Text,Pz);
      Text[Pz]:='$';
      print('%s\n',Text);
    END;
  END;
END PrintErrors;

PROCEDURE Check(i: INTEGER): INTEGER;
BEGIN
  IF i>=0 THEN RETURN i END;
  print('Error reading command file.\n');
  HALT(1);
END Check;

VAR Macro: ARRAY [0..1000] OF CHAR;
    Parms: ARRAY [0..9] OF ARRAY [0..80] OF CHAR;
    p,ParmCnt,MacroCnt: INTEGER;
    InParm, InMacro: BOOLEAN;
    inp: Stream;
    Ch: CHAR;

PROCEDURE NextCh;
  VAR i: INTEGER;
BEGIN
  IF Ch=NL THEN
    IF LineWithError THEN
      Line[Poz]:=0c;
      FOR i:=0 TO ErrCnt-1 DO
        IF (Errors[i].Ln=LineCnt)&
           (Errors[i].Lm=LineMCnt)
        THEN Str1(Errors[i].Text,Line) END;
      END;
      LineWithError:=FALSE;
    END;
    IF InMacro THEN INC(LineMCnt) ELSE INC(LineCnt) END;
    Poz:=-1;
    IF LineCnt MOD 16 =0 THEN print('%4d\r',LineCnt) END;
  END;
  LOOP
    Ch:=0c;
    IF InParm THEN
      Ch:=Parms[p,ParmCnt]; INC(ParmCnt); InParm:=Ch#0c;
    ELSIF InMacro THEN
      Ch:=Macro[MacroCnt]; INC(MacroCnt);
      IF Ch=0c THEN InMacro:=FALSE; LineMCnt:=0 END;
      IF Ch='!' THEN
        NextCh; p:=0;
        IF (Ch>='0')&(Ch<='9') THEN p:=ORD(Ch)-ORD('0') END;
        Ch:=0c; ParmCnt:=0; InParm:=TRUE;
      END;
    ELSE
      IF Check(sRead(inp,ADR(Ch),1))<0 THEN END;
    END;
    IF Ch#0c THEN
      INC(Poz);
      Line[Poz]:=Ch;
      RETURN
    END;
  END;
END NextCh;

CONST ident=1c; number=2c;
VAR Sy    : CHAR;
    IdVal : string;
    NumVal: INTEGER;

PROCEDURE Next;
  VAR i: INTEGER;
BEGIN
  LOOP
    WHILE Ch=' ' DO NextCh END;
    SyPoz:=Poz;
    CASE Ch OF
       ',',';','-','$',':','=': Sy:=Ch; NextCh; RETURN;
      |'"': IF InMacro THEN Error('Nesting macro.') END;
            i:=0; NextCh;
            WHILE Ch#'"' DO Macro[i]:=Ch; INC(i); NextCh END;
            Macro[i]:=NL; Macro[i+1]:=0c; NextCh;
      |'#': IF InMacro THEN Error('Nesting macro.') END;
            p:=-1; NextCh;
            WHILE Ch=' ' DO NextCh END;
            WHILE Ch#'#' DO
              INC(p); i:=0;
              WHILE (Ch#' ')&(Ch#'#') DO Parms[p,i]:=Ch; INC(i); NextCh; END;
              Parms[p,i]:=0c;
              WHILE Ch=' ' DO NextCh END;
            END;
            FOR p:=p+1 TO 9 DO Parms[p,0]:=0c END;
            MacroCnt:=0; InMacro:=TRUE; NextCh;
    ELSE
      IF (Ch>='A')&(Ch<='Z')OR(Ch>='a')&(Ch<='z')OR
         (Ch>=200c)&(Ch<=376c)OR(Ch='.') THEN
        Sy:=ident; i:=0;
        REPEAT
          IF i<HIGH(IdVal) THEN
            IdVal[i]:=Ch; INC(i);
          END;
          NextCh;
        UNTIL NOT((Ch>='A')&(Ch<='Z')OR(Ch>='a')&(Ch<='z')OR(Ch='.')
                OR(Ch>='0')&(Ch<='9')OR(Ch>=200c)&(Ch<=376c));
        IdVal[i]:=0c; RETURN;
      ELSIF (Ch>='0')&(Ch<='9') THEN
        Sy:=number; NumVal:=0;
        REPEAT
          NumVal:=NumVal*10+(ORD(Ch)-ORD('0')); NextCh;
        UNTIL NOT((Ch>='0')&(Ch<='9')); RETURN;
      ELSIF Ch=NL THEN
        NextCh;
      ELSE
        Error('Unknown char.');
        NextCh;
      END;
    END;
  END;
END Next;

VAR mdl   : board;
    Header: BOOLEAN;

PROCEDURE FindSignal(nm: string): signal;
  VAR s: signal; i: INTEGER;
BEGIN
  FOR i:=0 TO mdl^.sno-1 DO
    IF mdl^.sigs^[i]^.name=nm THEN RETURN mdl^.sigs^[i] END;
  END;
  cre_signal(s,mdl); s^.name:=nm;
  print(' ***  Created new signal: %s\n',nm); Header:=FALSE;
  RETURN s;
END FindSignal;

PROCEDURE FindChipType(nm: string): ctype;
  VAR i: INTEGER; fnm: string; t: ctype;
BEGIN
  FOR i:=0 TO mdl^.tno-1 DO
    IF mdl^.typs^[i]^.name=nm THEN RETURN mdl^.typs^[i] END;
  END;
  Header:=FALSE;
  REPEAT
    print('Chip type %s',nm); Str1(fnm,nm);
    Confirm(' not found in model, enter file name: ',fnm);
    print('\n');
    t:=ReadType(fnm,nm,mdl);
  UNTIL t#NIL;
  RETURN t;
END FindChipType;

PROCEDURE FindChip(nm,tnm: string): chip;
  VAR t: ctype; c: chip; i: INTEGER;
BEGIN
  FOR i:=0 TO mdl^.cno-1 DO
    IF mdl^.chps^[i]^.name=nm THEN RETURN mdl^.chps^[i] END;
  END;
  IF tnm='' THEN
    Error('Chip type must be defined.');
    RETURN NIL;
  END;
  t:=FindChipType(tnm);
  cre_chip(c,mdl,t);
  c^.name:=nm;
  print(' ***  Created new chip: %s\n',nm); Header:=FALSE;
  RETURN c;
END FindChip;

PROCEDURE Chip;
  VAR Name,Signal,Type: string; ch: chip; sig: signal;
      n: INTEGER;
      X,Y,R,V: INTEGER;
BEGIN
  IF Sy#ident THEN Error('Illegal chip name.'); Next; RETURN END;
  Name:=IdVal; Next;
  Type[0]:=0c; X:=-1; Y:=-1; R:=-1; V:=-1; ch:=NIL;
  WHILE (Sy=':')OR(Sy='-')OR(Sy='=') DO
    IF Sy='-' THEN
      Next;
      IF Sy#ident THEN Error('Illegal chip type.') ELSE Type:=IdVal END;
      Next;
    ELSIF Sy=':' THEN
      Next;
      IF Sy#number
        THEN Error('Illegal co-odinates.') ELSE X:=NumVal*960 DIV 25000 END;
      Next;
      IF Sy#number
        THEN Error('Illegal co-odinates.') ELSE Y:=NumVal*960 DIV 25000 END;
      Next;
      IF Sy#number THEN Error('Illegal rotation.') ELSE R:=NumVal END;
      Next;
    ELSIF Sy='=' THEN
      Next;
      IF Sy#number THEN Error('Illegal value.') ELSE V:=NumVal END;
      Next;
    END;
  END;
  IF ErrCnt=0 THEN
    ch:=FindChip(Name,Type);
    IF ch#NIL THEN
      IF X>=0 THEN ch^.x:=X END;
      IF Y>=0 THEN ch^.y:=Y END;
      IF R>=0 THEN ch^.r:=R END;
  --  IF V>=0 THEN ch^.cValue:=V END;
    END;
  END;
  WHILE Sy#';' DO
    IF Sy=',' THEN Next END;
    IF Sy#number THEN Error('Must be pin number.'); n:=0 ELSE n:=NumVal END;
    Next;
    IF Sy#'-' THEN
      Error('Must be - .');
      IF Sy#ident THEN Next END;
    ELSE Next;
    END;
    IF Sy#ident THEN
      Error('Must be signal name.'); Signal[0]:=0c;
    ELSE
      Signal:=IdVal;
    END;
    Next;
    IF ErrCnt=0 THEN
      sig:=FindSignal(Signal);
      IF n<1 THEN
        Error('Illegal pin no.')
      ELSE
        IF NOT Header THEN
          print('     chip       pin   signal\n'); Header:=TRUE;
        END;
        print('%15s %3d %-15s\n',ch^.name,n,sig^.name);
        tie(sig,ch,n-1,mdl);
      END;
    END;
  END;
  Next;
END Chip;

PROCEDURE Execute(VAR b: board; Name: ARRAY OF CHAR);
BEGIN
  IF b=NIL THEN cre_board(b) END;
  mdl:=b;
  ParmCnt:=0; MacroCnt:=0;
  InMacro:=FALSE; InParm:=FALSE; Macro[0]:=0c;
  inp:=Check(Open(Name));
  LineCnt:=0; Ch:=0c; LineMCnt:=0;
  Poz:=-1; ErrCnt:=0;
  NextCh; Next;
  Header:=FALSE;
  IF Sy=':' THEN
    Next;
    IF Sy#number THEN
      Error('Illegal co-odinates.')
    ELSE
      mdl^.x:=NumVal*960 DIV 25000;
    END;
    Next;
    IF Sy#number THEN
      Error('Illegal co-odinates.')
    ELSE
      mdl^.y:=NumVal*960 DIV 25000;
    END;
    Next;
    IF Sy=';' THEN Next END;
  END;
  WHILE (Sy#'$')&((ErrCnt<=HIGH(Errors))OR(LineWithError)) DO Chip END;
  inp:=Check(Close(inp));
  PrintErrors;
END Execute;

END pedChain.
