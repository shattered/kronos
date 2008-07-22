IMPLEMENTATION MODULE Chain; (* 08-Feb-87. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  defCodes;
IMPORT  ASCII;
IMPORT  pcb: Model;
IMPORT  pbl: ModelPbl;
IMPORT  str: Strings;
IMPORT  std: StdIO;
IMPORT  mio: ModelIO;
IMPORT  bio: BIO;
IMPORT  lex: Lexicon;
IMPORT  sed: strEditor;

VAR Errors: ARRAY [0..7] OF RECORD
              Ln,Lm,Pz: INTEGER;
              Text,Mess: ARRAY [0..79] OF CHAR;
            END;
    ErrCnt: INTEGER;
    LineWithError: BOOLEAN;
    Line: ARRAY [0..255] OF CHAR;
    LineCnt,LineMCnt,Poz,SyPoz: INTEGER;

PROCEDURE Tag(o: pcb.Object): pcb.Objects;
CODE 0 defCodes.lxb END Tag;

PROCEDURE Error(s: ARRAY OF CHAR);
BEGIN
  IF ErrCnt<=HIGH(Errors) THEN
    WITH Errors[ErrCnt] DO
      Text:=''; str.copy(Mess,s);
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
    std.WriteLn;
    WITH Errors[i] DO
      std.print('  ***** Line %3d %2d. %s\n',Ln,Lm,Mess);
      IF Pz<0 THEN Pz:=0 END;
      str.insert(Text,Pz,1);
      Text[Pz]:='$';
      std.print('%s\n',Text);
    END;
  END;
END PrintErrors;

PROCEDURE Check(done: BOOLEAN; error: INTEGER);
BEGIN
  IF done THEN RETURN END;
  lex.perror(pbl.Message,error,'Error reading command file. %%s');
  pbl.RaiseInMe(pbl.IOerror);
END Check;

VAR Macro: ARRAY [0..1000] OF CHAR;
    Parms: ARRAY [0..9] OF ARRAY [0..80] OF CHAR;
    p,ParmCnt,MacroCnt: INTEGER;
    InParm, InMacro: BOOLEAN;
    inp: bio.FILE;
    Ch: CHAR;

PROCEDURE NextCh;
  VAR i: INTEGER;
BEGIN
  IF Ch=ASCII.NL THEN
    IF LineWithError THEN
      Line[Poz]:=0c;
      FOR i:=0 TO ErrCnt-1 DO
        IF (Errors[i].Ln=LineCnt)&
           (Errors[i].Lm=LineMCnt)
        THEN str.copy(Errors[i].Text,Line) END;
      END;
      LineWithError:=FALSE;
    END;
    IF InMacro THEN INC(LineMCnt) ELSE INC(LineCnt) END;
    Poz:=-1;
    IF LineCnt MOD 16 =0 THEN std.print('%4d\r',LineCnt) END;
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
      bio.getch(inp,Ch); Check(bio.done,bio.error)
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
    IdVal : pcb.String;
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
            Macro[i]:=ASCII.NL; Macro[i+1]:=0c; NextCh;
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
      ELSIF Ch=ASCII.NL THEN
        NextCh;
      ELSE
        Error('Unknown char.');
        NextCh;
      END;
    END;
  END;
END Next;

VAR Key: pcb.String;
    Res: pcb.Object;
    ChipList, SignalList, ChipTypesList: pcb.List;
    mdl: pcb.Object;
    Header: BOOLEAN;

PROCEDURE IterProc(o: pcb.Object; info: INTEGER);
BEGIN
  IF o^.Name=Key THEN
    IF Res#NIL THEN
      str.copy(pbl.Message,'Dange: illegal data in model.');
      pbl.RaiseInMe(pbl.CrashInModel)
    END;
    Res:=o;
  END;
END IterProc;

PROCEDURE FindSignal(nm: pcb.String): pcb.Object;
BEGIN
  Key:=nm; Res:=NIL;
  pcb.Iterate(SignalList,IterProc,0);
  IF Res#NIL THEN RETURN Res END;
  Res:=pcb.NewObject(pcb.signal);
  Res^.Name:=nm; pcb.Tie(SignalList,Res); pcb.Tie(mdl^.All,Res);
  std.WriteString(' ***  Created new signal: ');
  std.print('%s\n',nm);
  Header:=FALSE;
  RETURN Res;
END FindSignal;

PROCEDURE FindChipType(nm: pcb.String): pcb.Object;
  VAR e: pbl.Reaction; c: pbl.Exception; d: sed.descriptor;
BEGIN
  Key:=nm; Res:=NIL;
  pcb.Iterate(ChipTypesList,IterProc,0);
  IF Res#NIL THEN RETURN Res END;
  std.WriteString('Chip type '); std.WriteString(nm);
  sed.new(d,1);
  IF d=NIL THEN pbl.RaiseInMe(pbl.MemoryOverflow) END;
  d^.how:=sed.confirm;
  sed.read_str(' not found in model, enter file name: ',nm,d); std.WriteLn;
  LOOP
    c:=pbl.Exception(pbl.Exception?(e));
    IF BOOLEAN(c) THEN
      IF c#pbl.IOerror THEN pbl.RaiseInMe(c) END;
      std.print('%s\n',pbl.Message);
      sed.read_str('Enter file name: ',nm,d); std.WriteLn
    ELSE
      Res:=mio.ReadModel(nm);
      pbl.KillReaction(e);
      EXIT
    END
  END;
  sed.dispose(d);
  pcb.Tie(ChipTypesList,Res); pcb.Tie(mdl^.All,Res);
  RETURN Res
END FindChipType;

VAR CreType,CreChip: pcb.Object;

PROCEDURE CrePin(o: pcb.Object; info: INTEGER);
  VAR pn: pcb.Object; sig: pcb.Object;
BEGIN
  pn:=pcb.Lget(CreChip^.Pins,o^.EPinNo);
  IF pn=NIL THEN
    pn:=pcb.NewObject(pcb.pin);
    pn^.No:=o^.EPinNo; pn^.Chip:=CreChip; pcb.Lset(CreChip^.Pins,o^.EPinNo,pn);
    pcb.Tie(mdl^.All,pn);
    sig:=FindSignal('..free..');
    pn^.Signal:=sig;
    pcb.Tie(sig^.TiedPins,pn);
  END;
END CrePin;

PROCEDURE FindChip(nm,tnm: pcb.String): pcb.Object;
  VAR o,o1: pcb.Object;
BEGIN
  Key:=nm; Res:=NIL;
  pcb.Iterate(ChipList,IterProc,0); o1:=Res;
  IF (o1#NIL)&((tnm[0]=0c)OR(o1^.ChipType^.Name=tnm)) THEN RETURN o1 END;
  IF tnm[0]#0c THEN o:=FindChipType(tnm) ELSE o:=NIL END;
  IF o1#NIL THEN
    IF o#NIL THEN o1^.ChipType:=o END;
    RETURN o1
  END;
  Res:=pcb.NewObject(pcb.chip);
  Res^.Name:=nm;
  Res^.ChipType:=o;
  pcb.Tie(ChipList,Res); pcb.Tie(mdl^.All,Res);
  CreType:=o; CreChip:=Res;
  IF CreType#NIL THEN pcb.Iterate(o^.ExternalPins,CrePin,0) END;
  std.WriteString(' ***  Created new chip: ');
  std.print('%s\n',nm);
  Header:=FALSE;
  RETURN CreChip;
END FindChip;

PROCEDURE Connect(sig,ch: pcb.Object; n: INTEGER);
  VAR pn,sg: pcb.Object;
BEGIN
  IF ch=NIL THEN RETURN END;
  pn:=pcb.Lget(ch^.Pins,n);
  IF pn=NIL THEN
    pn:=pcb.NewObject(pcb.pin);
    pn^.No:=n; pn^.Chip:=ch; pcb.Lset(ch^.Pins,n,pn);
    pcb.Tie(mdl^.All,pn);
  END;
  sg:=pn^.Signal;
  IF sg#NIL THEN
    IF sg=sig THEN RETURN END;
    pcb.UnTie(sg^.TiedPins,pn)
  END;
  pcb.Tie(sig^.TiedPins,pn); pn^.Signal:=sig;
  IF NOT Header THEN
    std.WriteLn;
    std.print('%s\n','Chip name       Pin  Signal name'); Header:=TRUE;
  END;
  std.print('%-15s %3d  %s\n',ch^.Name,pn^.No+1,sig^.Name);
END Connect;

PROCEDURE Chip;
  VAR Name,Signal,Type: pcb.String; ch,sig: pcb.Object; n: INTEGER;
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

PROCEDURE Sort(o: pcb.Object; info: INTEGER);
BEGIN
  CASE Tag(o) OF
    pcb.chip:     pcb.Tie(ChipList,o);
   |pcb.signal:   pcb.Tie(SignalList,o);
   |pcb.chiptype: pcb.Tie(ChipTypesList,o);
  ELSE
  END;
END Sort;

PROCEDURE Execute(model: pcb.Object; Name: ARRAY OF CHAR): INTEGER;
BEGIN
  mdl:=model;
  std.print('Model %s\n',model^.Name);
  pcb.InitList(ChipList);
  pcb.InitList(SignalList);
  pcb.InitList(ChipTypesList);
  pcb.Iterate(mdl^.All,Sort,0);
  ParmCnt:=0; MacroCnt:=0;
  InMacro:=FALSE; InParm:=FALSE; Macro[0]:=0c;
  bio.open(inp,Name,'r'); Check(bio.done,bio.error);
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
  bio.close(inp); Check(bio.done,bio.error);
  pcb.KillList(ChipList);
  pcb.KillList(SignalList);
  pcb.KillList(ChipTypesList);
  PrintErrors;
  RETURN ErrCnt;
END Execute;

END Chain.
