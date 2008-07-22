MODULE npc;

IMPORT Lib, FIO, IO;
IMPORT pcK, pcM, pcS, pcO, pcB, pcM2;
IMPORT pcVis, inGen;

CONST
  gen   ::= inGen;
  obs   ::= pcO;
  inter ::= pcM;
  pars  ::= pcM2;

VAR
  sou: FIO.File;
  err: LONGINT;
  vis: BOOLEAN;

PROCEDURE getstr(VAR s: ARRAY OF CHAR; VAR done: BOOLEAN);
BEGIN
  FIO.RdStr(sou,s);
  done:=NOT FIO.EOF;
(*IO.WrStr(s); IO.WrLn;*)
END getstr;

PROCEDURE error(l,c: INTEGER; sou,msg: ARRAY OF CHAR);
BEGIN
  INC(err);
  IO.WrInt(l,4); IO.WrChar(',');
  IO.WrInt(c,3); IO.WrStr(':  ');
  IO.WrStr(msg); IO.WrLn;
END error;

PROCEDURE message(msg: ARRAY OF CHAR);
BEGIN
  IO.WrStr(msg); IO.WrLn;
END message;

PROCEDURE parser;
  VAR cu: pcK.NODE; newSF: BOOLEAN;
BEGIN
  pars.compile(pars.opts,cu);
  gen.prepare(cu);
  IF obs.def THEN
    obs.export(cu^.obj,newSF,inter.time())
  ELSE
    IF vis THEN pcVis.vis(cu) END;
    gen.compile(cu,TRUE,TRUE,TRUE);
  END;
  inter.wi(err,0); inter.ws(' errors'); inter.wl;
END parser;

VAR c: CARDINAL;
    s: ARRAY [0..63] OF CHAR;

BEGIN
  err:=0;
  c:=Lib.ParamCount();
  IF c=0 THEN HALT END;
  IF c>1 THEN
    Lib.ParamStr(s,2);
    vis:=inter.str_equ(s,'+v');
  ELSE vis:=FALSE;
  END;
  Lib.ParamStr(s,1);
  inter.ws('Portable compiler v.0.0.1  "');
  inter.ws(s); inter.wc('"');
  inter.wl;
  inter.getstr:=getstr;
  inter.error:=error;
  inter.message:=message;
  obs.get_object:=gen.get_object;
  obs.put_object:=gen.put_object;
  obs.put_struct:=gen.put_struct;
  obs.get_struct:=gen.get_struct;
  pcB.literal:=gen.literal;
  pcB.eval:=gen.eval;
  pcB.ini_gen:=gen.ini;
  pcB.WORDs:=pcK.Forms{pcK.integer,pcK.cardinal,pcK.bitset,
                       pcK.shortIC,pcK.IC,pcK.word};
  sou:=FIO.OpenRead(s);
  IF sou=MAX(CARDINAL) THEN
    inter.ws('file not found: '); inter.ws(s); inter.wl;
  ELSE
    parser;
  END;
END npc.