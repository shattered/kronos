IMPLEMENTATION MODULE Chain; (* 08-Feb-87. (c) KRONOS *)

FROM SYSTEM    IMPORT   ADR, ADDRESS;
FROM ASCII     IMPORT   NL;
IMPORT  std: StdIO;
IMPORT  bio: BIO;
IMPORT  str: Strings;
IMPORT  pcb: pedModel;
IMPORT  tty: Terminal;
IMPORT  sed: strEditor;

VAR Errors: ARRAY [0..7] OF RECORD
              Ln,Lm,Pz: INTEGER;
              Text,Mess: ARRAY [0..79] OF CHAR;
            END;
    ErrCnt: INTEGER;
    LineWithError: BOOLEAN;
    Line: ARRAY [0..255] OF CHAR;
    LineCnt,LineMCnt,Poz,SyPoz: INTEGER;
    mdl: pcb.MDL;
    Header: BOOLEAN;

PROCEDURE Error(s: ARRAY OF CHAR);
BEGIN
  IF ErrCnt<=HIGH(Errors) THEN
    WITH Errors[ErrCnt] DO
      str.print(Mess,'%s',s);
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
    tty.WriteLn;
    WITH Errors[i] DO
      tty.print('  ***** Line %3d %2d. %s\n',Ln,Lm,Mess);
      IF Pz<0 THEN Pz:=0 END;
      str.insert(Text,Pz,1);
      Text[Pz]:='$';
      tty.print('%s\n',Text);
    END;
  END;
END PrintErrors;

PROCEDURE Check;
BEGIN
  IF NOT bio.done THEN
    tty.perror(bio.error,'Error reading command file:  %%s\n'); HALT(1);
  END;
END Check;

PROCEDURE check;
BEGIN
  IF NOT pcb.done THEN
    tty.perror(pcb.error,'Error in pedModel:  %%s\n'); HALT(1);
  END;
END check;

VAR Macro: ARRAY [0..1000] OF CHAR;
    Parms: ARRAY [0..9] OF ARRAY [0..80] OF CHAR;
    p,ParmCnt,MacroCnt: INTEGER;
    InParm, InMacro: BOOLEAN;
    inp: bio.FILE;
    Ch: CHAR;

PROCEDURE NextCh;
  VAR i: INTEGER;
BEGIN
  IF (Ch=NL)OR(Ch=12c)OR(Ch=15c) THEN
    IF LineWithError THEN
      Line[Poz]:=0c;
      FOR i:=0 TO ErrCnt-1 DO
        IF (Errors[i].Ln=LineCnt)&
           (Errors[i].Lm=LineMCnt)
        THEN str.print(Errors[i].Text,'%s',Line) END;
      END;
      LineWithError:=FALSE;
    END;
    IF InMacro THEN INC(LineMCnt) ELSE INC(LineCnt) END;
    Poz:=-1;
    IF LineCnt MOD 16 =0 THEN tty.print('%4d\r',LineCnt) END;
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
      bio.getch(inp,Ch); Check;
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
    IdVal: ARRAY [0..31] OF CHAR;
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
      ELSIF (Ch=NL)OR(Ch=12c)OR(Ch=15c) THEN
        NextCh;
      ELSE
        Error('Unknown char.');
        NextCh;
      END;
    END;
  END;
END Next;

PROCEDURE FindChipType(nm: ARRAY OF CHAR): pcb.MDL;
  VAR cht: pcb.MDL; d: sed.descriptor;
BEGIN
  pcb.find_mdl(cht,mdl,nm);
  IF cht#pcb.M_NIL THEN RETURN cht END;
  tty.WriteString('Chip type '); tty.WriteString(nm);
  sed.new(d,1); ASSERT(d#NIL,4Eh);
  d^.how:=sed.confirm;
  sed.read_str(' not found in model, enter file name: ',nm,d); tty.WriteLn;
  pcb.read_mdl(cht,nm); check;
  pcb.ins_mdl(mdl,cht); check;
  RETURN cht;
END FindChipType;

PROCEDURE FindChip(tnm,nm: ARRAY OF CHAR): pcb.MDL;
  VAR chp,cht: pcb.MDL; ln: STRING; i: INTEGER;
BEGIN
  chp:=pcb.M_NIL; cht:=pcb.M_NIL;
  pcb.find_chip(chp,mdl,nm);
  IF (chp#pcb.M_NIL) THEN
    pcb.chip_type(chp,cht);
    pcb.mdl_extract(cht,ln);
    IF ln=tnm THEN RETURN chp END;
  END;
  ASSERT(chp=pcb.M_NIL);
  cht:=FindChipType(tnm);
  pcb.new_chip(chp,cht,mdl,nm,''); check;
  tty.WriteString(' ***  Created new chip: '); tty.Show(nm); Header:=FALSE;
  RETURN chp
END FindChip;

PROCEDURE FindSignal(nm: ARRAY OF CHAR): pcb.SIGNAL;
  VAR sig: pcb.SIGNAL;
BEGIN
  pcb.find_sig(sig,mdl,nm);
  IF sig#pcb.S_NIL THEN RETURN sig END;
  pcb.new_sig(sig,mdl,TRUE,{},nm);
  tty.WriteString(' ***  Created new signal: '); tty.Show(nm); Header:=FALSE;
  RETURN sig;
END FindSignal;

PROCEDURE Chip;
  VAR Name,Signal,Type: ARRAY [0..31] OF CHAR;
      ch: pcb.MDL; sig: pcb.SIGNAL; n: INTEGER;
      X,Y,R,V: INTEGER; pnm: ARRAY [0..31] OF CHAR;
BEGIN
  IF Sy#ident THEN Error('Illegal chip name.'); Next; RETURN END;
  Name:=IdVal; Next;
  Type[0]:=0c; X:=-1; Y:=-1; R:=-1; V:=-1; ch:=pcb.M_NIL;
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
  IF ErrCnt=0 THEN ch:=FindChip(Type,Name) END;
  WHILE Sy#';' DO
    IF Sy=',' THEN Next END;
    IF Sy#number THEN
      Error('Must be pin number.');
      Next; IF Sy#number THEN RETURN END
    END;
    n:=NumVal; Next;
    IF Sy#'-' THEN
      Error('Must be - .');
      IF Sy#ident THEN Next END
    ELSE
      Next
    END;
    IF Sy#ident THEN
      Error('Must be signal name.'); Signal[0]:=0c;
    ELSE
      Signal:=IdVal;
    END;
    Next;
    IF ErrCnt=0 THEN
      sig:=FindSignal(Signal);
      IF n<1 THEN Error('Illegal pin no.')
      ELSE        str.print(pnm,'%d',n); pcb.connect(sig,ch,pnm) END
    END
  END;
  IF (X>=0) & (Y>=0) & (R>=-1) THEN pcb.move_chip(ch,X,Y,R) END;
  Next;
END Chip;

PROCEDURE Execute(model: pcb.MDL; Name: ARRAY OF CHAR): INTEGER;
  VAR ctX,ctY: INTEGER;
BEGIN
  mdl:=model;
  tty.print('Model %s\n',Name);
  ParmCnt:=0; MacroCnt:=0;
  InMacro:=FALSE; InParm:=FALSE; Macro[0]:=0c;
  bio.open(inp,Name,'r'); Check;
  LineCnt:=0; Ch:=0c; LineMCnt:=0;
  Poz:=-1; ErrCnt:=0;
  NextCh; Next;
  Header:=FALSE;
  IF Sy=':' THEN
    Next;
    IF Sy#number THEN
      Error('Illegal co-odinates.');
      ctX:=0;
    ELSE
      ctX:=NumVal*960 DIV 25000;
    END;
    Next;
    IF Sy#number THEN
      Error('Illegal co-odinates.');
      ctY:=0;
    ELSE
      ctY:=NumVal*960 DIV 25000;
    END;
    pcb.mdl_set_size(mdl,ctX,ctY);
    pcb.open(mdl,TRUE); check;
    Next;
    IF Sy=';' THEN Next END
  END;
  WHILE (Sy#'$')&((ErrCnt<=HIGH(Errors))OR(LineWithError)) DO Chip END;
  bio.close(inp); Check;
  PrintErrors;
  RETURN ErrCnt
END Execute;

END Chain.
