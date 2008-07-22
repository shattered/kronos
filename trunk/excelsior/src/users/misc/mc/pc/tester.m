MODULE tester; (* Ned 04-Mar-90. (c) KRONOS *)

IMPORT  sys: SYSTEM;
IMPORT  str: Strings;
IMPORT  tty: Terminal;
IMPORT  bio: BIO;
IMPORT  std: StdIO;
IMPORT   tm: Time;
IMPORT  tsk: tskEnv;
IMPORT  mem: Heap;
IMPORT args: tskArgs;
IMPORT   sh: myShell;
IMPORT  exc: Exceptions;

IMPORT inter: pcSystem;
IMPORT    mx: mcPars;
IMPORT    pc: pcGen;
IMPORT    io: coolIO;
IMPORT  comp: coolDefs;

WITH STORAGE: mem;

TYPE
  TAG  = (c_ok,x_ok);
  TAGs = SET OF TAG;
  info_ptr = POINTER TO info_rec;
  info_rec = RECORD
               name: ARRAY [0..31] OF CHAR;
               no  : INTEGER;
               line: INTEGER;
               next: info_ptr;
             END;

VAR
   sou: ARRAY [0..255] OF CHAR;
   cpu: INTEGER;
  opts: BITSET;
 sopts: ARRAY [0..31] OF CHAR;
  text: comp.io_ptr;
 super: PROCEDURE (comp.io_ptr);
x_info: info_ptr;
c_info: info_ptr;

VAR
  exe?: BOOLEAN;
  mem?: BOOLEAN;
 text?: BOOLEAN;
 fault: BOOLEAN;

VAR
  errs: INTEGER;      errlim: INTEGER;
  line: INTEGER;        LINE: INTEGER;
                        TIME: INTEGER;
 files: INTEGER;        code: INTEGER;
  tags: TAGs;          line0: INTEGER;

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
    IF s='' THEN
      std.print('%s %4d,%d: ',mx.name,line0+l,c);
    END;
    std.print(format,arg); std.print('\n');
    IF s#'' THEN show_err_ln(line0+l,c,s) END;
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
  INC(LINE,line-line0);
  INC(files);
  INC(code,mx.code);
  std.print('"%s":  errors: %d  lines %d\n',mx.name,errs,line-line0);
END result;

PROCEDURE total;
BEGIN
  TIME:=TIME DIV 1000;
  IF TIME<=0 THEN TIME:=1 END;
  tty.print("---------------------------------------------------------------\n");
  tty.print(
    "files %d  lines %d  time %$2d:%$2d  speed %d l/m\n"
    ,files,LINE
    ,TIME   DIV 60,TIME   MOD 60
    ,LINE*60 DIV  TIME);
END total;

PROCEDURE null(io: comp.io_ptr);
BEGIN
  io^.done:=TRUE;
END null;

PROCEDURE ini(VAR xxxx: comp.io_ptr;
              VAL name: ARRAY OF CHAR;
                  unit: INTEGER;
                 print: comp.PRINT);
BEGIN
  IF (unit=comp.ref) OR (NOT exe? & (unit=comp.code)) THEN
    NEW(xxxx); NEW(xxxx^.buf);
    xxxx^.doio:=null;
    xxxx^.exts :=NIL;
    xxxx^.kind :=unit;
    xxxx^.print:=print;
    xxxx^.done:=TRUE;
    RETURN
  END;
  io.ini(xxxx,name,unit,print);
(*
  IF unit=comp.sym_in THEN
    tty.print('\r'); tty.erase_line(0);
    tty.print('%s.sym\r',name);
  END;
*)
END ini;

PROCEDURE get_str(text: comp.io_ptr);
BEGIN
  super(text);
  IF text^.buf[0]='@' THEN text^.done:=FALSE; RETURN  END;
  IF text? THEN std.print('%s\n',text^.buf) END;
  INC(line);
  IF line MOD 64=0 THEN
    tty.print('\r'); tty.erase_line(0);
    tty.print('%d\r',line);
  END;
END get_str;

PROCEDURE set_tags(VAL s: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  tags:=TAGs{};
  i:=1;
  LOOP
    CASE s[i] OF
      |'c': INCL(tags,c_ok);
      |'x': INCL(tags,x_ok);
      |0c : EXIT
    ELSE EXIT
    END;
    INC(i);
  END;
END set_tags;

PROCEDURE append(VAR x: info_ptr; n: INTEGER);
  VAR i: info_ptr;
BEGIN
  NEW(i);
  i^.no:=n;
  i^.line:=line;
  str.copy(i^.name,mx.name);
  i^.next:=x; x:=i;
END append;

CONST out_name='tmp';

PROCEDURE copy;
  VAR f: bio.FILE;
      s: ARRAY [0..255] OF CHAR;
BEGIN
  bio.open(f,out_name,'r');
  IF NOT bio.done THEN RETURN END;
  LOOP
    bio.getstr(f,s,0);
    IF bio.iolen=0 THEN EXIT END;
    std.print('%s\n',s);
  END;
  bio.close(f);
END copy;

PROCEDURE execute;
  VAR s: ARRAY [0..255] OF CHAR;
BEGIN
  IF NOT exe? THEN RETURN END;
  str.print(s,'%s.cod >%s',mx.name,out_name);
  sh.system(s,std.print);
  std.print('\n');
  IF (x_ok IN tags)#(sh.result=0) THEN
    std.print('*** execution ***\n');
    append(x_info,sh.result);
  END;
  copy;
END execute;

PROCEDURE memory(VAL s: ARRAY OF CHAR);
  VAR o,f,u: INTEGER;
BEGIN
  mem.statistics(o,f,u);
  std.print('%s os=%d free=%d user=%d\n',s,o,f,u);
END memory;

PROCEDURE compile(): BOOLEAN;
  VAR no: INTEGER;
BEGIN
  LOOP
    text^.doio(text);
    IF NOT text^.done THEN RETURN FALSE END;
    IF text^.buf[0]='$' THEN EXIT END;
  END;
  set_tags(text^.buf);
  errs:=0;
  line0:=line;
  std.print('---------------------------------------------------------------\n');
  IF mem? THEN memory(">>>") END;
  pc.Ini;
    IF NOT fault & exc.traps(no) THEN
      std.print('"%s": *** compilation aborted %h ***\n',mx.name,no);
      IF (no#4Dh) OR (c_ok IN tags) THEN append(c_info,-no) END;
      pc.Exi;
      inter.release;
      RETURN TRUE
    END;
    mx.compile(text,ini,io.exi,error,print,opts,cpu);
  pc.Exi;
  inter.release;
  IF mem? THEN memory("<<<") END;
  result;
  IF (c_ok IN tags)#(errs=0) THEN
    std.print('*** compilation ***\n');
    append(c_info,errs);
  ELSIF (mx.unit=comp.main) & (c_ok IN tags) THEN
    execute
  END;
  RETURN TRUE
END compile;

PROCEDURE results;
  VAR d: info_ptr;
BEGIN
  std.print('\n---------------------------------------------------------------\n');
  WHILE c_info#NIL DO
    IF c_info^.no=0 THEN
      std.print('%04d: %-16s ошибка не обнаружена\n'
                ,c_info^.line,c_info^.name);
    ELSIF c_info^.no<0 THEN
      std.print('%04d: %-16s развал: %h\n'
                ,c_info^.line,c_info^.name,-c_info^.no);
    ELSE
      std.print('%04d: %-16s обнаружена лишняя ошибка (errs=%d)\n'
                ,c_info^.line,c_info^.name,c_info^.no);
    END;
    d:=c_info; c_info:=c_info^.next;
    DISPOSE(d);
  END;
  WHILE x_info#NIL DO
    IF x_info^.no=0 THEN
      std.print('%04d: %-16s ошибка исполнения не обнаружена\n'
                ,x_info^.line,x_info^.name);
    ELSE
      std.perror(x_info^.no,'%04d: %-16s %s\n',x_info^.line,x_info^.name);
    END;
    d:=x_info; x_info:=x_info^.next;
    DISPOSE(d);
  END;
END results;

PROCEDURE parser;
BEGIN
  line:=0;
  super:=text^.doio;
  tty.print('TESTER: %s [%d,%s] "%s"\n',mx.vers,cpu,sopts,sou);
  text^.doio:=get_str;
  TIME:=tm.sys_time(tm.milisec);
  REPEAT UNTIL NOT compile();
  TIME:=tm.sys_time(tm.milisec)-TIME;
  sh.system('rm T*.cod|T*.ref|T*.sym -qv',std.print);
  results;
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
  i:=0;
  FOR c:='A' TO 'Z' DO
    IF ORD(c)-ORD('A') IN opts THEN
      sopts[i]:=c; INC(i);
    END;
  END;
  sopts[i]:=0c;
END set_opts;

VAR i: INTEGER;
  sym: STRING;
  txt: STRING;
  out: STRING;

BEGIN
  bio.check_io(TRUE);
  LINE:=0; TIME:=0; code:=0; files:=0;
  x_info:=NIL;
  c_info:=NIL;
  exe? :=NOT args.flag('-','X');
  mem? :=    args.flag('+','M');
  text?:=    args.flag('+','T');
  fault:=    args.flag('+','F');
  IF args.string(tsk.sym,sym)  THEN END;
  IF args.string('mxOUT',out)  THEN END;
  IF args.string('mxTEXT',txt) THEN END;
  io.set(txt,sym,out,print);
  errlim:=99;
  IF NOT args.number('mxCPU',cpu) THEN cpu:=mx.cpu END;
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
  total;
END tester.
