MODULE mc; (* Ned 04-Mar-90. (c) KRONOS *)

IMPORT  sys: SYSTEM;
IMPORT  str: Strings;
IMPORT  tty: Terminal;
IMPORT  std: StdIO;
IMPORT   tm: Time;
IMPORT  tsk: tskEnv;
IMPORT args: tskArgs;

IMPORT    mc: mcPars;
IMPORT inter: pcSystem;
IMPORT    io: coolIO;
IMPORT  comp: coolDefs;
IMPORT  mcd : defCodes;

IMPORT  nsGen;

VAR
   sou: ARRAY [0..255] OF CHAR;
   cpu: INTEGER;
  opts: BITSET;
 sopts: ARRAY [0..31] OF CHAR;
  text: comp.io_ptr;
 super: PROCEDURE (comp.io_ptr);

VAR
  errs: INTEGER;      errlim: INTEGER;
  line: INTEGER;        LINE: INTEGER;
  time: INTEGER;        TIME: INTEGER;
iotime: INTEGER;      IOTIME: INTEGER;
 files: INTEGER;        code: INTEGER;
  info: ARRAY [0..79] OF CHAR;

PROCEDURE show_err_ln(l,c: INTEGER; VAL source: ARRAY OF CHAR);
  VAR s: ARRAY [0..255] OF CHAR; i: INTEGER;
BEGIN
  str.copy(s,source); i:=0;
  WHILE (i<71) & (s[i]#0c) DO INC(i) END; s[i+1]:=0c;
  WHILE i>c DO s[i]:=s[i-1]; DEC(i) END;
  s[i]:='$';
  std.print('%4d: %-72.72s\n',l,s);
END show_err_ln;

PROCEDURE stop(text: comp.io_ptr);
BEGIN
  text^.done:=FALSE;
END stop;

PROCEDURE error(l,c: INTEGER; VAL s,format: ARRAY OF CHAR; SEQ arg: sys.WORD);
BEGIN
  IF errs<errlim THEN
    INC(errs);
    IF tsk.ipr() THEN std.print('"%s": ',sou) END;
    IF s='' THEN std.print('%4d,%d: ',l,c) END;
    std.print(format,arg); std.print('\n');
    IF s#'' THEN show_err_ln(l,c,s) END;
    IF errs=errlim THEN
      text^.doio:=stop;
      std.print('Слишком много ошибок\n');
      IF args.flag('+','d') THEN HALT(1) END;
    END;
  END;
END error;

PROCEDURE print(VAL format: ARRAY OF CHAR; SEQ arg: sys.WORD);
BEGIN
  tty.print(format,arg); tty.print('\n');
END print;

---------------------------------------------------------------

PROCEDURE result;
BEGIN
  INC(TIME,time);       INC(IOTIME,iotime);
  INC(LINE,line);       INC(files);
  INC(code,mc.code);
  time  :=  time DIV 1000;
  iotime:=iotime DIV 1000;
  IF errs>0 THEN
    tty.print("errors: %d  lines %d   time  %$2dcp + %$2dio\n"
            ,errs,line,time,iotime);
  ELSE
    IF time=0 THEN time:=1; DEC(iotime) END;
    tty.print("lines %d   time  %$2dcp + %$2dio  "
                   ,line,time,iotime);
    IF mc.unit=comp.def THEN tty.print('   "%s.%s"',mc.name,'sym');
    ELSE tty.print('   "%s.%s" %d words',mc.name,'cod',mc.code)
    END;
    tty.print('\n');
  END;
END result;

PROCEDURE total;
BEGIN
  TIME:=TIME DIV 1000; IOTIME:=IOTIME DIV 1000;

  IF TIME<=0 THEN TIME:=1 END;
  tty.print
  ("-------------------------------------------------------------------\n");
  tty.print(
    "files %d  lines %d  time cpu%$2d:%$2d  io%$2d:%$2d  speed %d l/m"
    ,files,LINE
    ,TIME   DIV 60,TIME   MOD 60
    ,IOTIME DIV 60,IOTIME MOD 60
    ,LINE*60 DIV  TIME);
  IF code>0 THEN tty.print("  code %d words",code) END;
  tty.print("\n");
END total;

PROCEDURE ini(VAR xxxx: comp.io_ptr;
              VAL name: ARRAY OF CHAR;
                  unit: INTEGER;
                 print: comp.PRINT);
BEGIN
  IF unit=comp.code THEN time:=tm.sys_time(tm.milisec)-time END;

  io.ini(xxxx,name,unit,print);

  IF (unit=comp.sym_in) & NOT tsk.ipr() THEN
    tty.print('\r'); tty.erase_line(0);
    tty.print('%s.sym\r',name);
  END;
END ini;

PROCEDURE exi(VAR x: comp.io_ptr);
BEGIN
  IF x^.kind=comp.sym_in THEN time:=tm.sys_time(tm.milisec) END;
  io.exi(x);
END exi;

PROCEDURE get_str(text: comp.io_ptr);
BEGIN
  super(text);
  INC(line);
  IF line MOD 64=0 THEN
    IF NOT tsk.ipr() THEN
      tty.print('\r'); tty.erase_line(0);
      tty.print('%d\r',line);
    END;
    IF errs>0 THEN
      str.print(info,'*%d* %s  %d',errs,sou,line);
    ELSE
      str.print(info,'%s  %d',sou,line);
    END;
    tsk.put_str(tsk.info,info,TRUE);
  END;
END get_str;

PROCEDURE parser;
BEGIN
  errs:=0;
  line:=0;
  super:=text^.doio;
  IF NOT tsk.ipr() THEN
    tty.print('%s [%d,%s] "%s"\n',mc.vers,cpu,sopts,sou);
  END;
  str.print(info,'%s  %d',sou,line);
  tsk.put_str(tsk.info,info,TRUE);
  text^.doio:=get_str;
  iotime:=tm.sys_time(tm.milisec);
  time  :=iotime;
  mc.compile(text,ini,exi,error,print,opts,cpu);
  inter.release;
--  IF (mc.unit=comp.def) OR (errs>0) THEN -- see exi()
    time:=tm.sys_time(tm.milisec)-time;
--  END;
  iotime:=tm.sys_time(tm.milisec)-iotime-time;
  IF NOT tsk.ipr() THEN result END;
END parser;

PROCEDURE extension(VAR s: ARRAY OF CHAR);
  VAR n: INTEGER;
BEGIN
  n:=str.len(s)-1;
  WHILE (n>0) & (s[n]#'.') & (s[n]#'/') DO DEC(n) END;
  IF (n>0) & (s[n]='.') THEN RETURN END;
  str.app(s,'.m');
END extension;

PROCEDURE set_opts;
  VAR x: STRING; i: INTEGER; c,s: CHAR;
BEGIN
  opts:=mc.opts;
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
  i:=0;
  FOR c:='A' TO 'Z' DO
    IF ORD(c)-ORD('A') IN opts THEN
      sopts[i]:=c; INC(i);
    END;
  END;
  sopts[i]:=0c;
END set_opts;

VAR
  sym: STRING;
  out: STRING;
  txt: STRING;
  i: INTEGER;

BEGIN
  LINE:=0; TIME:=0; IOTIME:=0; code:=0; files:=0;
  IF args.string(tsk.sym,sym)  THEN END;
  IF args.string('mxOUT',out)  THEN END;
  IF args.string('mxTEXT',txt) THEN END;
  io.set(txt,sym,out,print);
  IF NOT args.number('mxERRLIM',errlim) THEN errlim:=8   END;
  IF NOT args.number('mxCPU'   ,cpu   ) THEN cpu:=mc.cpu END;
  set_opts;
  FOR i:=0 TO HIGH(args.words) DO
    str.copy(sou,args.words[i]);
    extension(sou);
    io.ini(text,sou,comp.text,print);
    IF text^.done THEN
      parser;
      io.exi(text);
    END;
  END;
  IF NOT tsk.ipr() & (files>1) THEN total END;
END mc.
