MODULE LEXICON; (*$U+ Leo 18-Jan-90. (c) KRONOS *)

IMPORT       SYSTEM;

IMPORT  fs : osFiles;           IMPORT  req: defRequest;
IMPORT  err: defErrors;         IMPORT  env: tskEnv;
IMPORT  mem: Heap;              IMPORT  bio: BIO;
IMPORT  tty: Terminal;          IMPORT  arg: tskArgs;
IMPORT  str: Strings;           IMPORT       ASCII;
IMPORT   os: osKernel;

WITH STORAGE: mem;

CONST ok = err.ok;
   MAGIC = 004C4558h; (* "LEX"0c *)

PROCEDURE pusage;
BEGIN
  tty.print('\n');
  tty.print('   "LEXICON"   lexicons driver program    (c) KRONOS\n');
  tty.print('usage:\n');
  tty.print('    & LEXICON  device_name FILE0 FILE1 ....\n');
  tty.print('\n');
  tty.print("                                     Leo, Jan 19 90\n");
END pusage;


TYPE REF = DYNARR OF INTEGER;

VAR text: DYNARR OF
          RECORD
            txt: STRING;
            ref: REF;
          END;

(* NOTE: additional 000c added to any text to *)
(* suppress <=HIGH checks                     *)

CONST CANT = "can't"' %s file "%s": %04hh\n';

PROCEDURE collect(no: INTEGER);
  VAR i,n: INTEGER;
      txt: STRING;
      ref: REF;
BEGIN
  txt^:=text[no].txt^;
  IF txt^.ADR=NIL THEN RETURN END;
  n:=0;
  i:=0;
  WHILE txt[i]#0c DO
    IF HIGH(ref)<n THEN RESIZE(ref,n+8) END;
    ref[n]:=i;
    WHILE (txt[i]#ASCII.NL) & (txt[i]#0c) DO INC(i) END;
    IF txt[i]#0c THEN INC(i) END;
    INC(n)
  END;
  RESIZE(ref,n);
  text[no].ref^:=ref^
END collect;

PROCEDURE read_data(no: INTEGER; VAL name: ARRAY OF CHAR);
  VAR f: bio.FILE;
BEGIN
  IF HIGH(text)<no THEN RESIZE(text,no+1) END;
  NEW(text[no].txt,0);
  NEW(text[no].ref,0);
  bio.open(f,name,'r');
  IF NOT bio.done THEN
    tty.print(CANT,'open',name,bio.error); RETURN
  END;
  NEW(text[no].txt,bio.eof(f)+1);
  bio.get(f,text[no].txt,bio.eof(f));
  IF NOT bio.done THEN
    tty.print(CANT,'read',name,bio.error);
    RESIZE(text[no].txt,0); bio.close(f); RETURN
  END;
  text[no].txt[HIGH(text[no].txt)]:=0c;
  bio.close(f);
  IF NOT bio.done THEN tty.print(CANT,'close',name,bio.error) END;
  collect(no);
END read_data;

PROCEDURE get(no,code: INTEGER; VAR buf: SYSTEM.ADDRESS;
                                VAR pos,len,res: INTEGER);
  VAR l: INTEGER;
     txt: STRING;
     ref: REF;
     i,j: INTEGER;
      ch: CHAR;
     c,n: INTEGER;
     hex: BOOLEAN;
     num: ARRAY [0..15] OF CHAR;
BEGIN
  txt^:=text[no].txt^;
  ref^:=text[no].ref^;
  buf :=txt^.ADR; pos:=0; len:=0;
  IF buf=NIL THEN res:=err.no_data; RETURN END;
  i:=0;
  l:=0;
  WHILE l<=HIGH(ref) DO
    i:=ref[l];
    WHILE (txt[i]=' ') DO INC(i) END;
    n:=0; hex:=FALSE;
    LOOP
      IF n>HIGH(num) THEN n:=0; EXIT END;
      ch:=txt[i];
      IF    ("0"<=ch) & (ch<="9") THEN
        num[n]:=CHAR(ORD(ch)-ORD("0"));
      ELSIF ("A"<=ch) & (ch<="F") THEN
        num[n]:=CHAR(ORD(ch)-ORD("A")+10); hex:=TRUE
      ELSE
        EXIT
      END;
      n:=n+1; i:=i+1
    END;
    IF n>0 THEN
      IF hex & (txt[i]#'h') THEN res:=err.inconsistency; RETURN END;
      IF txt[i]='h' THEN hex:=TRUE END;
      c:=0;
      IF hex THEN
        IF n>8 THEN res:=err.inconsistency; RETURN END;
        INC(i);
        FOR j:=0 TO n-1 DO c:=INTEGER(BITSET(c<<4)+BITSET(num[j])) END
      ELSE
        FOR j:=0 TO n-1 DO
          IF (MAX(INTEGER)-ORD(num[j])) DIV 10 > c THEN
            res:=err.inconsistency; RETURN
          END;
          c:=c*10+ORD(num[j])
        END
      END;
      IF c=code THEN
        IF txt[i]=' '  THEN pos:=i+1 ELSE pos:=i END;
        IF l<HIGH(ref) THEN len:=ref[l+1]-pos-1 ELSE len:=HIGH(txt)-pos-1 END;
        RETURN
      END
    END;
    INC(l)
  END;
  res:=err.no_entry
END get;

PROCEDURE doio(VAR r: req.REQUEST);
  VAR data: STRING;
BEGIN
  r.res:=ok;
  IF r.op MOD 100h # req.CONTROL THEN r.res:=err.inv_op; RETURN END;
  r.buf:=NIL; r.pos:=0; r.len:=0;
  CASE r.op DIV 100h OF
    |0: r.len:=MAGIC;
    |1: get(r.drn,r.ofs,r.buf,r.pos,r.len,r.res)
  ELSE
    r.res:=err.inv_op
  END
END doio;

VAR name: ARRAY [0..7] OF CHAR; nl: INTEGER;

PROCEDURE install;
  VAR i,r: INTEGER;
      inf: ARRAY [0..79] OF CHAR;
BEGIN
  mem.set_credit(2);
  NEW(text,0);
  str.copy(name,arg.words[0]);
  nl:=str.len(name);
  IF nl>6 THEN nl:=6 END;
  name[nl+1]:=0c;
  IF HIGH(arg.words)<1 THEN pusage; HALT(1) END;
  i:=1;
  WHILE (i<=HIGH(arg.words)) DO
    read_data(i-1,arg.words[i]); INC(i)
  END;
  inf:='';
  FOR i:=0 TO HIGH(arg.words)-1 DO
    name[nl]:=CHAR(ORD("0")+i);
    r:=fs.define_driver(name,"",i,fs.spec,doio);
    IF r#ok THEN tty.print(CANT,'define driver',name,r); HALT(r) END;
    str.app(inf,name);
    str.app(inf,' ')
  END;
  env.put_str(env.info,inf,TRUE)
END install;

BEGIN
  IF HIGH(arg.words)<0 THEN pusage; HALT END;
  install;
  env.become_ipr;
  os.suspend(os.active(),-1)
END LEXICON.
