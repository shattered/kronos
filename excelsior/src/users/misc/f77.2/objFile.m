IMPLEMENTATION MODULE objFile; (* 14-Feb-89. *)

IMPORT  StdIO, FileNames;

VAR objsou: StdIO.Stream;
    name,fn: ARRAY [0..79] OF CHAR;


PROCEDURE pTag(Tag:INTEGER);
BEGIN
  StdIO.PutC(objsou,CHAR(Tag));
END pTag;

PROCEDURE gTag():INTEGER;
BEGIN
  RETURN ORD(StdIO.GetC(objsou))
END gTag;

PROCEDURE g2Tag():INTEGER;
BEGIN
  RETURN gTag()+gTag()*100h
END g2Tag;

PROCEDURE g4Tag():INTEGER;
BEGIN
  RETURN INTEGER(BITSET(g2Tag())+BITSET(g2Tag()<<16))
END g4Tag;

PROCEDURE gxTag():INTEGER;
  VAR b:INTEGER;
BEGIN b:=gTag();
  IF    b>=8 THEN RETURN b-128
  ELSIF b=0  THEN RETURN gTag()
  ELSIF b=1  THEN RETURN -gTag()
  ELSIF b=2  THEN RETURN g2Tag()
  ELSIF b=3  THEN RETURN g4Tag()
  ELSE  ASSERT(FALSE)
  END
END gxTag;

PROCEDURE pxTag(val:INTEGER);
BEGIN
  IF val>=0 THEN
    IF    val<128     THEN pTag(val+128)
    ELSIF val<=255    THEN pTag(0); pTag(val)
    ELSIF val<=0FFFFh THEN pTag(2);
          pTag(val MOD 100h); pTag(val DIV 100h)
    ELSE  pTag(3);
          pTag(INTEGER(BITSET(val)*{0..7})); val:=(val>>8);
          pTag(INTEGER(BITSET(val)*{0..7})); val:=val>>8;
          pTag(INTEGER(BITSET(val)*{0..7})); val:=val>>8;
          pTag(INTEGER(BITSET(val)*{0..7}));
    END;
  ELSE
    IF     val>=-120 THEN pTag(val+128)
    ELSIF  val>=-255 THEN pTag(1); pTag(-val)
    ELSE   ASSERT(FALSE)
    END;
  END;
END pxTag;

PROCEDURE pName(VAR name:ARRAY OF CHAR);
  VAR i:INTEGER; c:CHAR;
BEGIN  i:=0; c:=name[i];
  WHILE (i<HIGH(name)) & (c#0c) DO
    StdIO.PutC(objsou,c); INC(i); c:=name[i];
  END;
  StdIO.PutC(objsou,0c);
END pName;

PROCEDURE gName(VAR name:ARRAY OF CHAR);
  VAR i:INTEGER; c:CHAR;
BEGIN  i:=0; c:=CHAR(gTag());
  WHILE (i<HIGH(name)) & (c#0c) DO
    name[i]:=c; c:=CHAR(gTag()); INC(i)
  END;
  ASSERT (c=0c);
  name[i]:=c;
END gName;

PROCEDURE CreateObj(VAR nm:ARRAY OF CHAR);
  VAR iend, i:INTEGER; ch:CHAR;
      ext:ARRAY [0..3] OF CHAR;
BEGIN
(*
  i:=0; ch:=nm[i];
  WHILE (ch#'.') AND (ch#0c)  DO
    fn[i]:=ch; INC(i); ch:=nm[i];
  END; iend:=i; fn[i]:='.'; INC(i);
  fn[i]:='o'; INC(i); fn[i]:='b'; INC(i); fn[i]:='j'; INC(i);
  fn[i]:=0c;
*)
  fn:=nm; ext:="o";
  FileNames.ChangeExt(fn,ext);
-- StdIO.print(' CreateObj: nm= %s fn= %s \n', nm, fn);
  objsou:=StdIO.Create(fn);
  IF objsou < 0 THEN
    StdIO.Why?(objsou,name);
    StdIO.print(' %s %s \n',name,fn); HALT;
  END;
--(*
  FileNames.LastPart(fn,name);
  FileNames.DelExt(name);
--*)
--  name:=fn; name[iend]:=0c;
  pTag(FILE); pName(name);
-- StdIO.print(' CreateObj: nm= %s fn= %s name=%s \n', nm, fn, name);
END CreateObj;

PROCEDURE OpenObj(VAR nm:ARRAY OF CHAR);
  VAR i:INTEGER; ch:CHAR;
      ext:ARRAY [0..3] OF CHAR;
BEGIN
--(*
  i:=0; ch:=nm[i];
  WHILE (ch#'.') AND (ch#0c)  DO
    fn[i]:=ch; INC(i); ch:=nm[i];
  END; fn[i]:='.'; INC(i);
  fn[i]:='o'; INC(i);
--  fn[i]:='b'; INC(i); fn[i]:='j'; INC(i);
  fn[i]:=0c;
--*)
(*
  fn:=nm; ext:="o";
  FileNames.ChangeExt(fn,ext);
    StdIO.print(' %s %s \n',nm,fn);
*)
  objsou:=StdIO.Open(fn);
  IF objsou < 0 THEN
    StdIO.Why?(objsou,name);
    StdIO.print(' %s %s \n',name,fn); HALT;
  END;
END OpenObj;

PROCEDURE CloseObj;
  VAR i:INTEGER;
BEGIN
--  pTag(EOF);
  i:=StdIO.Close(objsou);
  IF i < 0 THEN
    StdIO.Why?(i,name);
    StdIO.print(' %s %s \n',name,fn); HALT;
  END;
END CloseObj;

PROCEDURE pFunc(VAR nm:ARRAY OF CHAR; proc,parno,tp:INTEGER);
BEGIN
  pTag(FUNC); pName(nm); pxTag(proc);
  pTag(parno); pTag(tp);
END pFunc;

PROCEDURE pSubr(VAR nm:ARRAY OF CHAR; proc,parno:INTEGER);
BEGIN
  pTag(SUBR); pName(nm); pxTag(proc);
  pTag(parno);
END pSubr;

PROCEDURE pSubra(VAR nm:ARRAY OF CHAR; proc,parno,alt:INTEGER);
BEGIN
  pTag(SUBRA); pName(nm); pxTag(proc);
  pTag(parno); pTag(alt);
END pSubra;

PROCEDURE pProg(VAR nm:ARRAY OF CHAR; proc:INTEGER);
BEGIN
  pTag(PROG); pName(nm); pxTag(proc);
END pProg;

PROCEDURE pBData(VAR nm:ARRAY OF CHAR; proc:INTEGER);
BEGIN
  pTag(BDATA); pName(nm); pxTag(proc);
END pBData;

PROCEDURE pEntry(VAR nm:ARRAY OF CHAR; proc,parno,alt:INTEGER);
BEGIN
  pTag(ENTRY); pName(nm); pxTag(proc);
  pTag(parno); pTag(alt);
END pEntry;

PROCEDURE gFunc(VAR nm:ARRAY OF CHAR; VAR proc,parno,tp:INTEGER);
BEGIN
  gName(nm); proc:=gxTag();
  parno:=gTag(); tp:=gTag();
END gFunc;

PROCEDURE gSubr(VAR nm:ARRAY OF CHAR; VAR proc,parno:INTEGER);
BEGIN
  gName(nm); proc:=gxTag();
  parno:=gTag();
END gSubr;

PROCEDURE gSubra(VAR nm:ARRAY OF CHAR; VAR proc,parno,alt:INTEGER);
BEGIN
  gName(nm); proc:=gxTag();
  parno:=gTag(); alt:=gTag();
END gSubra;

PROCEDURE gProg(VAR nm:ARRAY OF CHAR; VAR proc:INTEGER);
BEGIN
  gName(nm); proc:=gxTag();
END gProg;

PROCEDURE gBData(VAR nm:ARRAY OF CHAR; VAR proc:INTEGER);
BEGIN
  gName(nm); proc:=gxTag();
END gBData;

PROCEDURE gEntry(VAR nm:ARRAY OF CHAR; VAR proc,parno,alt:INTEGER);
BEGIN
  gName(nm); proc:=gxTag();
  parno:=gTag(); alt:=gTag();
END gEntry;

PROCEDURE pCommon(VAR nm:ARRAY OF CHAR; moffs,len:INTEGER);
BEGIN
 pTag(COMMON); pName(nm); pTag(moffs); pxTag(len);
END pCommon;

PROCEDURE pExternal(VAR nm:ARRAY OF CHAR);
BEGIN
  pTag(EXTERNAL); pName(nm);
END pExternal;

PROCEDURE pLPC(VAR nm:ARRAY OF CHAR);
BEGIN
  pTag(LPC); pName(nm);
END pLPC;

PROCEDURE gCommon(VAR nm:ARRAY OF CHAR; VAR moffs,len:INTEGER);
BEGIN
 gName(nm); moffs:=gTag(); len:=gxTag();
END gCommon;

PROCEDURE gExternal(VAR nm:ARRAY OF CHAR);
BEGIN
  gName(nm);
END gExternal;

PROCEDURE gLPC(VAR nm:ARRAY OF CHAR);
BEGIN
  gName(nm);
END gLPC;

PROCEDURE pEndproc(VAR name:ARRAY OF CHAR;
                   maplen,locals,templen:INTEGER);
BEGIN
  pTag(ENDP);   pName(name);
  pTag(maplen); pxTag(locals); pTag(templen);
END pEndproc;

PROCEDURE gEndproc(VAR name:ARRAY OF CHAR;
                   VAR maplen,locals,templen:INTEGER);
BEGIN
  gName(name);
  maplen:=gTag(); locals:= gxTag(); templen:=gTag();
END gEndproc;

PROCEDURE pArray(bmoffs,moffs,offs:INTEGER);
BEGIN
  pTag(aRRAY); pTag(bmoffs); pTag(moffs); pxTag(offs);
END pArray;

PROCEDURE gArray(VAR bmoffs,moffs,offs:INTEGER);
BEGIN
  bmoffs:= gTag(); moffs:=gTag(); offs:=gxTag();
END gArray;

PROCEDURE pCall(VAR name:ARRAY OF CHAR; noparam,noaltr:INTEGER);
BEGIN
  pTag(CALL); pName(name); pTag(noparam); pTag(noaltr);
END pCall;

PROCEDURE gCall(VAR name:ARRAY OF CHAR; VAR noparam,noaltr:INTEGER);
BEGIN
  gName(name); noparam:=gTag(); noaltr:=gTag();
END gCall;

PROCEDURE pLabel(label:INTEGER);
BEGIN
  pTag(LABEL); pxTag(label);
END pLabel;

PROCEDURE gLabel(VAR label:INTEGER);
BEGIN
  label:=gxTag();
END gLabel;

PROCEDURE pORJump;
BEGIN
  pTag(ORJUMP);
END pORJump;

PROCEDURE pANDJump;
BEGIN
  pTag(ANDJUMP);
END pANDJump;

PROCEDURE pENDLab;
BEGIN
  pTag(eNDLAB);
END pENDLab;

PROCEDURE pJump(label:INTEGER; fixed:BOOLEAN);
BEGIN
  pTag(JUMP); pxTag(label);
  IF fixed THEN pTag(2) ELSE pTag(0) END;
END pJump;

PROCEDURE pJumpC(label:INTEGER; fixed:BOOLEAN);
BEGIN
  pTag(JUMPC); pxTag(label);
  IF fixed THEN pTag(2) ELSE pTag(0) END;
END pJumpC;

PROCEDURE pLDForm(label:INTEGER);
BEGIN
  pTag(LDFORM); pxTag(label);
END pLDForm;

PROCEDURE pFormat(label,soffs:INTEGER);
BEGIN
  pTag(FORMAT); pxTag(label); pxTag(soffs);
END pFormat;

PROCEDURE gFormat(VAR label,soffs:INTEGER);
BEGIN
  label:=gxTag(); soffs:=gxTag();
END gFormat;

END objFile.
