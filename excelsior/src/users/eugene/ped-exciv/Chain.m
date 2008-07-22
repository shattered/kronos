IMPLEMENTATION MODULE Chain; (* 08-Feb-87. (c) KRONOS *)

FROM SYSTEM    IMPORT   ADR, ADDRESS;
FROM StdIO     IMPORT   Open, Close, sRead, Show, Stream, Write, print,
                        WriteLn, WriteString, Confirm;
FROM Model     IMPORT   String, Iterate, NewObject, Objects, Object,
                        Lget, Lset, Tie, List, KillList, InitList, UnTie;
FROM ModelPbl  IMPORT   Message, RaiseInMe, SyntaxError, Exception?,
                        IOerror, CrashInModel, Reaction, Exception,
                        KillReaction;
FROM Strings   IMPORT   Str1, Str0, InsCh;
FROM ModelIO   IMPORT   ReadModel;
FROM ASCII     IMPORT   NL;
IMPORT  mcd: mCodeMnem;

VAR Errors: ARRAY [0..7] OF RECORD
              Ln,Lm,Pz: INTEGER;
              Text,Mess: ARRAY [0..79] OF CHAR;
            END;
    ErrCnt: INTEGER;
    LineWithError: BOOLEAN;
    Line: ARRAY [0..255] OF CHAR;
    LineCnt,LineMCnt,Poz,SyPoz: INTEGER;

PROCEDURE Tag(o: Object): Objects;
CODE 0 mcd.lxb END Tag;

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
  Str1(Message,'Error reading command file.');
  RaiseInMe(IOerror);
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
VAR Sy: CHAR;
    IdVal: String;
    NumVal: INTEGER;

PROCEDURE Next;
  VAR i,ii: INTEGER;
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
            FOR ii:=p+1 TO 9 DO Parms[ii,0]:=0c END;
            p:=9;
            MacroCnt:=0; InMacro:=TRUE; NextCh;
    ELSE
      IF (Ch>='A')&(Ch<='Z')OR(Ch>='a')&(Ch<='z')OR(Ch='^')OR
         (Ch='_')OR(Ch>=200c)&(Ch<=376c)OR(Ch='.') THEN
        Sy:=ident; i:=0;
        REPEAT
          IF i<HIGH(IdVal) THEN
            IdVal[i]:=Ch; INC(i);
          END;
          NextCh;
        UNTIL NOT((Ch>='A')&(Ch<='Z')OR(Ch>='a')&(Ch<='z')OR(Ch='+')
                OR(Ch=')')OR(Ch='.')OR(Ch='_')OR(Ch='^')OR(Ch='/')OR(Ch='_')
                OR(Ch='(')OR(Ch>='0')&(Ch<='9')OR(Ch>=200c)&(Ch<=376c));
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

VAR Key: String;
    Res: Object;
    ChipList, SignalList, ChipTypesList: List;
    mdl: Object;
    Header: BOOLEAN;

PROCEDURE IterProc(o: Object; info: INTEGER);
BEGIN
  IF o^.Name=Key THEN
    IF Res#NIL THEN
      Str1(Message,'Dange: illegal data in model.');
      RaiseInMe(CrashInModel);
    END;
    Res:=o;
  END;
END IterProc;

PROCEDURE FindSignal(nm: String): Object;
BEGIN
  Key:=nm; Res:=NIL;
  Iterate(SignalList,IterProc,0);
  IF Res#NIL THEN RETURN Res END;
  Res:=NewObject(signal);
  Res^.Name:=nm; Tie(SignalList,Res); Tie(mdl^.All,Res);
  WriteString(' ***  Created new signal: '); Show(nm); Header:=FALSE;
  RETURN Res;
END FindSignal;

PROCEDURE FindChipType(nm: String): Object;
  VAR e: Reaction; c: Exception;
BEGIN
  Key:=nm; Res:=NIL;
  Iterate(ChipTypesList,IterProc,0);
  IF Res#NIL THEN RETURN Res END;
  WriteString('Chip type '); WriteString(nm);
  Confirm(' not found in model, enter file name: ',nm); WriteLn;
  LOOP
    c:=Exception(Exception?(e));
    IF BOOLEAN(c) THEN
      IF c#IOerror THEN RaiseInMe(c) END;
      Show(Message); Confirm('Enter file name: ',nm); WriteLn;
    ELSE
      Res:=ReadModel(nm);
      --Res:=ReadShortModel(nm);
      KillReaction(e); EXIT
    END;
  END;
  Tie(ChipTypesList,Res); Tie(mdl^.All,Res);
  RETURN Res;
END FindChipType;

VAR CreType,CreChip: Object;

PROCEDURE CrePin(o: Object; info: INTEGER);
  VAR pn: Object; sig: Object;
BEGIN
  pn:=Lget(CreChip^.Pins,o^.EPinNo);
  IF pn=NIL THEN
    pn:=NewObject(pin);
    pn^.No:=o^.EPinNo; pn^.Chip:=CreChip; Lset(CreChip^.Pins,o^.EPinNo,pn);
    Tie(mdl^.All,pn);
    sig:=FindSignal('..free..');
    pn^.Signal:=sig;
    Tie(sig^.TiedPins,pn);
  END;
END CrePin;

PROCEDURE FindChip(nm,tnm: String): Object;
  VAR o,o1: Object;
BEGIN
  Key:=nm; Res:=NIL;
  Iterate(ChipList,IterProc,0); o1:=Res;
  IF (o1#NIL)&((tnm[0]=0c)OR(o1^.ChipType^.Name=tnm)) THEN RETURN o1 END;
  IF tnm[0]#0c THEN o:=FindChipType(tnm) ELSE o:=NIL END;
  IF o1#NIL THEN
    IF o#NIL THEN o1^.ChipType:=o END;
    RETURN o1
  END;
  Res:=NewObject(chip);
  Res^.Name:=nm;
  Res^.ChipType:=o;
  Tie(ChipList,Res); Tie(mdl^.All,Res);
  CreType:=o; CreChip:=Res;
  IF CreType#NIL THEN Iterate(o^.ExternalPins,CrePin,0) END;
  WriteString(' ***  Created new chip: '); Show(nm); Header:=FALSE;
  RETURN CreChip;
END FindChip;

PROCEDURE Connect(sig,ch: Object; n: INTEGER);
  VAR pn,sg: Object;
BEGIN
  IF ch=NIL THEN RETURN END;
  pn:=Lget(ch^.Pins,n);
  IF pn=NIL THEN
    pn:=NewObject(pin);
    pn^.No:=n; pn^.Chip:=ch; Lset(ch^.Pins,n,pn);
    Tie(mdl^.All,pn);
  END;
  sg:=pn^.Signal;
  IF sg#NIL THEN
    IF sg=sig THEN RETURN END;
    UnTie(sg^.TiedPins,pn)
  END;
  Tie(sig^.TiedPins,pn); pn^.Signal:=sig;
  IF NOT Header THEN
    WriteLn; Show('Chip name       Pin  Signal name'); Header:=TRUE;
  END;
  print('%-15s %3d  %s\n',ch^.Name,pn^.No+1,sig^.Name);
END Connect;

PROCEDURE Chip;
  VAR Name,Signal,Type: String; ch,sig: Object; n: INTEGER;
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
        THEN Error('Illegal co-ordinates.') ELSE X:=NumVal*960 DIV 25000 END;
      Next;
      IF Sy#number
        THEN Error('Illegal co-ordinates.') ELSE Y:=NumVal*960 DIV 25000 END;
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
    IF X>=0  THEN ch^.XB:=X END;
    IF Y>=0  THEN ch^.YB:=Y END;
    IF R>=-1 THEN ch^.RB:=R END;
    IF V>=0  THEN ch^.cValue:=V END;
  END;
  WHILE Sy#';' DO
    IF Sy=',' THEN Next END;
    IF Sy#number THEN
      Error('Must be pin number.');
      Next; IF Sy#number THEN RETURN END;
    END;
    n:=NumVal; Next;
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
        Connect(sig,ch,n-1);
      END;
    END;
  END;
  Next;
END Chip;

PROCEDURE Sort(o: Object; info: INTEGER);
BEGIN
  CASE Tag(o) OF
    chip:     Tie(ChipList,o);
   |signal:   Tie(SignalList,o);
   |chiptype: Tie(ChipTypesList,o);
  ELSE
  END;
END Sort;

PROCEDURE Execute(model: Object; Name: ARRAY OF CHAR): INTEGER;
BEGIN
  mdl:=model;
  print('Model %s\n',model^.Name);
  InitList(ChipList);
  InitList(SignalList);
  InitList(ChipTypesList);
  Iterate(mdl^.All,Sort,0);
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
      mdl^.ctX:=NumVal*960 DIV 25000;
    END;
    Next;
    IF Sy#number THEN
      Error('Illegal co-odinates.')
    ELSE
      mdl^.ctY:=NumVal*960 DIV 25000;
    END;
    Next;
    IF Sy=';' THEN Next END;
  END;
  WHILE (Sy#'$')&((ErrCnt<=HIGH(Errors))OR(LineWithError)) DO Chip END;
  inp:=Check(Close(inp));
  KillList(ChipList);
  KillList(SignalList);
  KillList(ChipTypesList);
  PrintErrors;
  RETURN ErrCnt;
END Execute;

END Chain.
