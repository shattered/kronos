MODULE vObj; (* 14-Feb-89.  *)

IMPORT SYSTEM, objFile, StdIO, Strings, Image, Args, FileNames;

FROM mCode      IMPORT VisCommand;

FROM objFile    IMPORT FILE,  SPOOL, EOF,
                       PROG,  FUNC,  SUBR,    SUBRA, BDATA, ENTRY, ENDP,
                       aRRAY, COMMON,EXTERNAL,CALL,  DCODE, PCODE, BCODE,
                       LABEL, ORJUMP,ANDJUMP, eNDLAB,JUMP,  JUMPC,
                       LDFORM,FORMAT,NEWSTM,  LPC,
                       gTag,    gxTag,  gName,
                       gProg,   gBData, gFunc, gSubr, gSubra,
                       gCommon, gArray, gCall, gLabel,gFormat,
                       gEndproc,
                       OpenObj, CloseObj;

VAR
    name: ARRAY [0..255] OF CHAR;
      fn: ARRAY [0..255] OF CHAR;
    line: ARRAY [0..255] OF CHAR;
     sou: StdIO.Stream;
     cmd: ARRAY [0..6] OF CHAR;
VAR proc, parno, noalt, tp, length, moffs:INTEGER;
    label:INTEGER; fixed:BOOLEAN;
    offset:INTEGER; cdlen, nocmd:INTEGER;

PROCEDURE initVis;
BEGIN Strings.Str1(line,'    ');  nocmd:=0;
END initVis;

PROCEDURE pr(VAL format:ARRAY OF CHAR; SEQ x: SYSTEM.WORD);
BEGIN Image.image(line,format,x);
END pr;

PROCEDURE nl;
BEGIN StdIO.Show(line); Strings.Str1(line,'    ');
      nocmd:=0;
END nl;

PROCEDURE pos0;
BEGIN Strings.Str0(line);
END pos0;

PROCEDURE Next():INTEGER;
BEGIN
 DEC(cdlen); RETURN gTag()
END Next;

PROCEDURE Next2():INTEGER;
BEGIN
  RETURN Next() + Next()*100h;
END Next2;

PROCEDURE Next4():INTEGER;
BEGIN
  RETURN Next2() + INTEGER(BITSET(Next2())<<16);
END Next4;

PROCEDURE addcmd;
BEGIN pr("%-7s",cmd); INC(nocmd);
  IF nocmd=4 THEN nl; END;
END addcmd;

PROCEDURE addcmd1;
BEGIN pr("%-7s",cmd); pr("%-3d",Next());
  INC(nocmd);
  IF nocmd=4 THEN nl; END;
END addcmd1;

PROCEDURE addcmd11;
BEGIN pr("%-7s",cmd);
pr("%-3d",Next()); pr("%-3d",Next());
  INC(nocmd);
  IF nocmd=4 THEN nl; END;
END addcmd11;

PROCEDURE addcmd12;
BEGIN pr("%-7s",cmd);
pr("%-3d",Next()); pr("%-6d",Next2());
  INC(nocmd);
  IF nocmd=4 THEN nl; END;
END addcmd12;

PROCEDURE addcmd2;
BEGIN pr("%-7s",cmd); pr("%-6d",Next2());
  INC(nocmd);
  IF nocmd=4 THEN nl; END;
END addcmd2;

PROCEDURE addcmd4;
BEGIN pr("%-7s",cmd); pr("%8$#h ",Next4());
  INC(nocmd);
  IF nocmd=4 THEN nl; END;
END addcmd4;

PROCEDURE VisCode;
  VAR cod: INTEGER;
BEGIN
  WHILE cdlen>0 DO
    cod:=gTag(); DEC(cdlen); VisCommand(cod,cmd);
    CASE cod OF
     0h..0Fh:addcmd;
    |10h: addcmd1;
    |11h: addcmd2;
    |12h: addcmd4;
    |13h: addcmd;
    |14h..16h: addcmd1;
    |17h: addcmd11;
    |18h..19h: addcmd2;
    |1Ah..1Bh: addcmd1;
    |1Ch..1Dh: addcmd2;
    |1Eh..21h: addcmd1;
    |22h: addcmd11;
    |23h: addcmd1;
    |24h..2Fh: addcmd;
    |30h..31h: addcmd1;
    |32h: addcmd11;
    |33h: addcmd1;
    |34h..9Eh: addcmd;
    |9Fh: addcmd1;
    |0A0h..0B5h: addcmd;
    |0B6h..0B7h: addcmd1;
    |0B8h..0B9h: addcmd12;
    |0BAh: addcmd2; -- ?? ENTC xxxx .....
    |0BBh: addcmd;
    |0BCh: addcmd2;
    |0BEh..0BFh: addcmd1;
    |0C0h: addcmd;
    |0C1h: addcmd1; -- ?? RDS len ...
    |0C2h: addcmd2;
    |0C3h..0C8h: addcmd;
    |0C9h: addcmd1;
    |0CAh..0CBh: addcmd;
    |0CCh: addcmd11;
    |0CDh: addcmd1;
    |0CEh: addcmd;
    |0CFh: addcmd1;
    |0D0h..0EAh: addcmd;
    |0EBh: addcmd2;
    |0ECh..0F0h: addcmd;
    |0F1h..0F3h: addcmd1;
    |0F4h..0FAh: addcmd;
    |0FBh..0FCh: addcmd1;
    |0FDh..0FFh: addcmd;
    ELSE
    END;
  END;
END VisCode;

PROCEDURE tag(VAL s:ARRAY OF CHAR);
BEGIN pr('%-10.8s',s)
END tag;

PROCEDURE vname;
BEGIN pr("'%-10s'",name)
END vname;

PROCEDURE id;
BEGIN objFile.gName(name)
END id;

PROCEDURE vx;
BEGIN  pr(' %-5d ',gxTag() );
END vx;

PROCEDURE vnum(val:INTEGER);
BEGIN
  pr(' %-8d ',val);
END vnum;

PROCEDURE scan;
  VAR byte:INTEGER;
      i,len,j,k:INTEGER;
BEGIN
  LOOP
    byte:=gTag();
    CASE byte  OF
      FILE:      tag('FILE');       id; vname; nl;

     |SPOOL :    tag('SPOOL');
                 len:=gxTag(); pr(' %5d ',len);  nl; k:=0;
                 FOR i:=0 TO len*4-1 DO
                   j:=gTag(); pr('%4$#h ',j); INC(k);
                   IF k=12 THEN nl; k:=0; END;
                 END; nl;

     |PROG:      tag('PROG');
                 gProg(name,proc); vname; vnum(proc); nl;

     |FUNC:      tag('FUNC');
                 gFunc(name,proc,parno,tp);
                 vname; vnum(proc); vnum(parno); vnum(tp); nl;

     |SUBR:      tag('SUBR');
                 gSubr(name,proc,parno);
                 vname; vnum(proc); vnum(parno); nl;

     |SUBRA:     tag('SUBRA');
                 gSubra(name,proc,parno,noalt);
                 vname; vnum(proc); vnum(parno); vnum(noalt); nl;

     |BDATA:     tag('BDATA');
                 gBData(name,proc); vname; vnum(proc); nl;

     |ENTRY:     tag('ENTRY');
                 gSubra(name,proc,parno,noalt);
                 vname; vnum(proc); vnum(parno); vnum(noalt); nl;

     |ENDP :     tag('ENDP');
                 gEndproc(name,moffs,length,noalt);
                 vname;  vnum(moffs);  vnum(length); vnum(noalt); nl;

     |aRRAY:     tag('ARRAY');
                 gArray(noalt,moffs,length);
                 vnum(noalt);  vnum(moffs);  vnum(length);  nl;

     |COMMON:    tag('COMMON');
                 gCommon(name,moffs,length);
                 vname; vnum(moffs); vnum(length); nl;

     |EXTERNAL:  tag('EXTERNAL');
                 id; vname; nl;

     |CALL:      tag('CALL');
                 gCall(name,parno,noalt);
                 vname; vnum(parno); vnum(noalt); nl;

     |DCODE :    tag('DCODE');
                 len:=gxTag(); pr(' %5d ',len);  nl;
                 cdlen:=len; VisCode; nl;
        --         FOR i:=0 TO len-1 DO j:=gTag(); pr('%4$#h',j) END; nl;

     |PCODE :    tag('PCODE');
                 len:=gxTag(); pr(' %5d ',len);  nl;
                 cdlen:=len; VisCode; nl;
         --        FOR i:=0 TO len-1 DO j:=gTag(); pr('%4$#h',j) END; nl;

     |BCODE :    tag('BCODE');
                 len:=gxTag(); pr(' %5d ',len);  nl;
                 cdlen:=len; VisCode; nl;
         --        FOR i:=0 TO len-1 DO j:=gTag(); pr('%4$#h',j) END; nl;

     |LABEL:     tag('LABEL');
                 gLabel(label); vnum(label); nl;

     |ORJUMP:    tag('ORJUMP');  nl;

     |ANDJUMP:   tag('ANDJUMP'); nl;

     |eNDLAB:    tag('ENDLAB');  nl;

     |JUMP:      tag('JUMP');
                 gLabel(label); fixed:=BOOLEAN(gTag());
                 vnum(label);   vnum(INTEGER(fixed));  nl;

     |JUMPC:     tag('JUMPC');
                 gLabel(label); fixed:=BOOLEAN(gTag());
                 vnum(label);   vnum(INTEGER(fixed)); nl;

     |LDFORM:    tag('LDFORM');
                 gLabel(label); vnum(label); nl;

     |FORMAT:    tag('FORMAT');
                 gFormat(label,offset); vnum(label); vnum(offset); nl

     |NEWSTM:    tag('NEWSTM'); nl;

     |LPC:       tag('LPC');
                 id; vname; nl;
     ELSE
       IF byte#EOF THEN
         StdIO.print('Некорректный тег: %#h',byte);
       END; RETURN
    END;
  END;
END scan;

PROCEDURE visobj(VAR nm:ARRAY OF CHAR);
BEGIN
  Image.image0(fn,'%s',nm);
  FileNames.ChangeExt(fn,'o');
  objFile.OpenObj(fn);
  initVis; scan;
END visobj;

VAR w: ARRAY [0..255] OF CHAR;

BEGIN Args.ScanFlags;
  Args.TakeWord(w); IF w[0]=0c THEN HALT END;
  visobj(w);

END vObj.
