MODULE m86; (* Ned 04-Mar-90. (c) KRONOS *)

IMPORT  sys: SYSTEM;
IMPORT  str: Strings;
IMPORT  tty: Terminal;
IMPORT  std: StdIO;
IMPORT   tm: Time;
IMPORT  tsk: tskEnv;
IMPORT args: tskArgs;

IMPORT    mc: mcPars;
IMPORT inter: pcSystem;
IMPORT    pc: pcTab;
IMPORT    io: coIO;
IMPORT  comp: coDefs;

IMPORT  inGen;

TYPE
  error_rec=RECORD l,c: INTEGER; msg: ARRAY [0..79] OF CHAR END;

VAR
  sou    : ARRAY [0..255] OF CHAR;
  opts   : BITSET;
  sopts  : ARRAY [0..31] OF CHAR;
  text   : comp.io_ptr;
  super  : PROCEDURE (comp.io_ptr);
  errs   : INTEGER;
  errlim : INTEGER;
  line   : INTEGER;
  LINE   : INTEGER;
  time   : INTEGER;
  TIME   : INTEGER;
  iotime : INTEGER;
  IOTIME : INTEGER;
  files  : INTEGER;
  code   : INTEGER;
  info   : ARRAY [0..79] OF CHAR;
  e_buf  : ARRAY [0..15] OF error_rec;

PROCEDURE print_errors;
  VAR i,l: INTEGER; x: error_rec;
BEGIN
  IF errs=0 THEN RETURN END;
  i:=0;
  LOOP
    IF i=errs-1 THEN EXIT END;
    IF e_buf[i].l>e_buf[i+1].l THEN
      x:=e_buf[i]; e_buf[i]:=e_buf[i+1]; e_buf[i+1]:=x;
      IF i>0 THEN DEC(i) END;
    ELSE INC(i);
    END;
  END;
  tty.print('\n');
  io.ini(text,sou,comp.text,inter.print);
  i:=0; l:=0;
  LOOP
    text^.doio(text);
    IF NOT text^.done OR (l=e_buf[i].l) THEN
      IF text^.done THEN tty.print('%.79s\n',text^.buf) END;
      REPEAT
        tty.print('*** %s %$4d.%$3d *** %s ***\n',
                  sou,e_buf[i].l,e_buf[i].c,e_buf[i].msg);
        INC(i);
      UNTIL (i=errs) OR text^.done & (l#e_buf[i].l);
    END;
    IF i=errs THEN EXIT END;
    INC(l);
  END;
  io.exi(text);
  tty.print('\n');
END print_errors;

PROCEDURE halt;
BEGIN
  io.exi(text);
  print_errors;
  tty.print('Compilation aborted...\n');
  HALT(1);
END halt;

PROCEDURE error(l,c: INTEGER; VAL format: ARRAY OF CHAR; SEQ arg: sys.WORD);
BEGIN
  e_buf[errs].l:=l; e_buf[errs].c:=c;
  str.print(e_buf[errs].msg,format,arg);
  INC(errs);
  IF errs>=errlim THEN inter.halt END;
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
    tty.print("lines %d   time  %$2dcp + %$2dio  "
                   ,line,time,iotime);
    IF mc.unit=comp.def THEN tty.print('   "%s.%s"',mc.name,'sym');
    ELSE tty.print('   "%s.%s" %d bytes',mc.name,'obj',mc.code)
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
  IF code>0 THEN tty.print("  code %d bytes",code) END;
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
  VAR t: INTEGER;
BEGIN
  inter.error:=error;
  inter.print:=print;
  inter.ini:=ini;
  inter.exi:=exi;
  inter.halt:=halt;
  errs:=0; line:=0;
  super:=text^.doio;
  IF NOT tsk.ipr() THEN
    IF pc.cpu_mode=0 THEN
      tty.print('%s [%d,%s] "%s"\n',mc.vers,pc.cpu_type*100+86,sopts,sou);
    ELSE
      tty.print('%s [%dp,%s] "%s"\n',mc.vers,pc.cpu_type*100+86,sopts,sou);
    END;
  END;
  str.print(info,'%s  %d',sou,line);
  tsk.put_str(tsk.info,info,TRUE);
  text^.doio:=get_str;
  iotime:=tm.sys_time(tm.milisec);
  time  :=iotime;
  mc.compile(text,opts);
  inter.release;
  t:=tm.sys_time(tm.milisec);
  time:=t-time;
  iotime:=t-iotime-time;
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
  IF args.string('mcFLAGS',x) THEN
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

PROCEDURE help;
BEGIN
  std.print('  "m86" Modula-2 compiler for Intel x86 microprocessors.\n');
  std.print('usage:\n');
  std.print('  m86 <file name> [<keys>]\n');
  std.print('keys:\n');
  std.print('  mcCPU     cpu type: 86,186,286,286p,386,386p\n');
  std.print('  mcERRLIM  errors limit\n');
  std.print('  mcOUT     output directory name\n');
  std.print('  mcTEXT    source directory name\n');
  std.print('  mcSYM     directory name for sym. files\n');
  std.print('  mcFLAGS   compiler options\n');
END help;

VAR
  sym: STRING;
  out: STRING;
  txt: STRING;
  cpu: STRING;
  i: INTEGER;

BEGIN
  IF args.flag('-','h') THEN help; HALT END;
  LINE:=0; TIME:=0; IOTIME:=0; code:=0; files:=0;
  IF args.string('mcSYM',sym)  THEN END;
  IF args.string('mcOUT',out)  THEN END;
  IF args.string('mcTEXT',txt) THEN END;
  io.set(txt,sym,out,print);
  IF NOT args.number('mcERRLIM',errlim) THEN errlim:=8 END;
  IF args.string('mcCPU',cpu) THEN
    IF    cpu='86'   THEN pc.cpu_type:=0; pc.cpu_mode:=0;
    ELSIF cpu='186'  THEN pc.cpu_type:=1; pc.cpu_mode:=0;
    ELSIF cpu='286'  THEN pc.cpu_type:=2; pc.cpu_mode:=0;
    ELSIF cpu='286p' THEN pc.cpu_type:=2; pc.cpu_mode:=1;
    ELSIF cpu='386'  THEN pc.cpu_type:=3; pc.cpu_mode:=0;
    ELSIF cpu='386p' THEN pc.cpu_type:=3; pc.cpu_mode:=1;
    ELSE tty.print('Warning: illegal value of mcCPU: %s.\n',cpu);
    END;
  END;
  set_opts;
  FOR i:=0 TO HIGH(args.words) DO
    str.copy(sou,args.words[i]);
    extension(sou);
    io.ini(text,sou,comp.text,print);
    IF text^.done THEN
      parser;
      io.exi(text);
      print_errors;
    END;
  END;
  IF NOT tsk.ipr() & (files>1) THEN total END;
END m86.
