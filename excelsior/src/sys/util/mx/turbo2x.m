MODULE turbo2x; (* Leo 28-Jan-89. (c) KRONOS *)
                (* Ned 05-Mar-90. (c) KRONOS *)

IMPORT  sys: SYSTEM;
IMPORT  str: Strings;
IMPORT  env: tskEnv;
IMPORT  mem: Heap;
IMPORT face: myEditor;
IMPORT  tty: Terminal;
IMPORT   tm: Time;
IMPORT args: tskArgs;

IMPORT   mx: mxPars;
IMPORT   io: coolIO;
IMPORT comp: coolDefs;

WITH STORAGE: mem;

CONST ok = 0;

VAR
     sou: ARRAY [0..79] OF CHAR;
  header: ARRAY [0..127] OF CHAR;
     cpu: INTEGER;
    opts: BITSET;
    text: comp.io_ptr;

 last_ln: INTEGER;
    line: INTEGER;
    errs: INTEGER;
  errlim: INTEGER;
    time: INTEGER;
  iotime: INTEGER;

PROCEDURE print(VAL format: ARRAY OF CHAR; SEQ arg: sys.WORD);
BEGIN
  tty.set_cursor(0);
  face.message(FALSE,format,arg)
END print;

PROCEDURE stop(text: comp.io_ptr);
BEGIN text^.done:=FALSE;
END stop;

PROCEDURE error(l,c: INTEGER; VAL s,format: ARRAY OF CHAR; SEQ arg: sys.WORD);
BEGIN
  IF errs<errlim THEN
    INC(errs);
    face.mark(l,c,format,arg);
    IF errs=errlim THEN
      face.message(FALSE,'Слишком много ошибок');
      text^.doio:=stop;
    END;
  END;
END error;

PROCEDURE result;
BEGIN
  time  :=time   DIV 1000;
  iotime:=iotime DIV 1000;
  IF time=0 THEN time:=1; DEC(iotime) END;
  IF mx.unit=comp.def THEN
  print(' lines %d   time  %$2dcp + %$2dio   "%s.sym"'
                 ,line,time,iotime,mx.name);
  ELSE
  print(' lines %d   time  %$2dcp + %$2dio   "%s.cod" %d words'
                 ,line,time,iotime,mx.name,mx.code)
  END;
END result;

PROCEDURE getstr(text: comp.io_ptr);
  VAR size: INTEGER;
BEGIN
  IF line>last_ln THEN text^.done:=FALSE; RETURN END;
  face.jump(line);
  face.get(text^.buf,size);
  IF size<=HIGH(text^.buf) THEN text^.buf[size]:=0c END;
  text^.len:=size;
  INC(line);
  IF line MOD 64 = 0 THEN
    IF errs=0 THEN
      face.message(FALSE,'%4d  no errors  %s',line,header);
    ELSIF errs=1 THEN
      face.message(FALSE,'%4d   1 error   %s',line,header);
    ELSE
      face.message(FALSE,'%4d  %2d errors  %s',line,errs,header);
    END;
  END;
  text^.done:=TRUE;
END getstr;

PROCEDURE ini(VAR x   : comp.io_ptr;
              VAL name: ARRAY OF CHAR;
                  unit: INTEGER;
                 print: comp.PRINT);
BEGIN
  IF unit IN {comp.code,comp.mcode} THEN time:=tm.sys_time(tm.milisec)-time END;
  io.ini(x,name,unit,print);
  IF unit=comp.sym_in THEN
    face.message(FALSE,'%-16.16s %s',name,header);
  END;
END ini;

PROCEDURE exi(VAR x: comp.io_ptr);
BEGIN
  IF x^.kind=comp.sym_in THEN
--  face.message(FALSE,"%16c %s",' ',header);
    time:=tm.sys_time(tm.milisec);
  END;
  io.exi(x);
END exi;

PROCEDURE parser;
BEGIN
  str.print(header,' %s  "%s"',mx.vers,sou);
  IF tty.state^.min_color<-1 THEN tty.set_color(-1)
  ELSE                            tty.set_something(1)
  END;
  IF NOT tty.done THEN tty.set_something(1) END;
  face.message(FALSE,'%16c %s',' ',header);
  errs:=0;
  line:=0;
  iotime:=tm.sys_time(tm.milisec);
  time  :=iotime;
    mx.compile(text,ini,exi,error,print,opts,cpu);
  IF (mx.unit=comp.def) OR (errs>0) THEN
    time:=tm.sys_time(tm.milisec)-time;
  END;
  iotime:=tm.sys_time(tm.milisec)-iotime-time;
  IF errs=0 THEN
    tty.set_reverse(1);
      result;
    tty.set_reverse(0);
  END;
  tty.set_something(0);
  tty.set_color(0);
END parser;

PROCEDURE set_opts;
  VAR x: STRING; i: INTEGER; c,s: CHAR;
BEGIN
  opts:=mx.opts;
  IF args.string('mxFLAGS',x) THEN
    i:=0;
    LOOP
      IF (i>=HIGH(x)) OR (x[i]=0c) THEN EXIT END;
      c:=CAP(x[i]); INC(i);
      s:=x[i];      INC(i);
      IF (c<'A') OR (c>'Z') THEN EXIT END;
      IF    s='+' THEN INCL(opts,ORD(c)-ORD('A'));
      ELSIF s='-' THEN EXCL(opts,ORD(c)-ORD('A'))
      ELSE EXIT
      END;
    END;
  END;
  FOR c:='a' TO 'z' DO
    IF    args.flag('-',c) THEN EXCL(opts,ORD(c)-ORD('a'))
    ELSIF args.flag('+',c) THEN INCL(opts,ORD(c)-ORD('a'))
    END;
  END;
END set_opts;

VAR
  sym: STRING;
  out: STRING;

BEGIN
  tty.set_cursor(0);
  IF args.string(env.sym,sym) THEN END;
  IF args.string('mxOUT',out) THEN END;
  io.set('',sym,out,print);
  IF NOT args.number('mxERRLIM',errlim) THEN errlim:=8   END;
  IF NOT args.number('mxCPU'   ,cpu   ) THEN cpu:=mx.cpu END;
  set_opts;
  face.f_name(sou);
  last_ln:=face.last();
  NEW(text);
  NEW(text^.buf,256);
  text^.doio:=getstr;
  text^.kind:=comp.text;
  text^.print:=print;
  parser;
END turbo2x.
