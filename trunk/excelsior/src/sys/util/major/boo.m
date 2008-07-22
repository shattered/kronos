MODULE boo; (* Leo 21-Dec-1989. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  bio: BIO;
IMPORT  err: defErrors;
IMPORT  cod: defCodes;
IMPORT  mem: Heap;
IMPORT  tty: Terminal;
IMPORT  std: StdIO;
IMPORT  key: Keyboard;
IMPORT  arg: tskArgs;
IMPORT  tim: Time;
IMPORT  sta: Statistics;
IMPORT  req: defRequest;

WITH STORAGE: mem;

TYPE ADDRESS = SYSTEM.ADDRESS;

PROCEDURE di; CODE cod.li0 cod.setm END di;

TYPE process = POINTER TO
     RECORD
       G: ADDRESS;   L: ADDRESS;
      PC: INTEGER;   M: BITSET;
       S: ADDRESS;   H: ADDRESS;
       T: INTEGER;
     END;

CONST WSP = 256;

PROCEDURE transfer(VAR f,t: process); CODE cod.tra END transfer;

PROCEDURE start(system: ADDRESS; size: INTEGER);
  TYPE c = BITSET;
  VAR p: process;
    adr: ADDRESS;
    top: ADDRESS;
BEGIN
  top:=system+size;
  adr:=top-WSP;
  p:=adr;
  p^.G:=adr+08; p^.L :=adr+10;
  p^.M:={};     p^.PC:=0;
  p^.H:=top;    p^.S :=adr+20;
  p^.T:=0;      p^.G^:=adr+09;

  (* stack *)
  adr  :=p^.S;
  DEC(adr);     adr^:=5;
  DEC(adr);     adr^:=0;
  DEC(adr);     adr^:=1;
  DEC(adr);     adr^:=0;
  DEC(adr);     adr^:=system;
  DEC(adr);     adr^:=size;
  (* code *)
  adr:=p^.G^;
  adr^:=c(cod.move)+c(cod.tra<<8)+c(cod.quit<<16);
  transfer(p,p);
END start;

PROCEDURE power_off;

  VAR r: req.REQUEST;
      d: bio.FILE;
    dev: bio.FILE;
   mode: BITSET;
     no: INTEGER;
  files: ARRAY [0..63] OF bio.FILE;
   name: ARRAY [0..31] OF CHAR;
  names: ARRAY [0..63] OF ARRAY [0..31] OF CHAR;

  PROCEDURE collect;
  BEGIN
    IF no<=HIGH(files) THEN
      bio.fopen(d,files[no],name,'');
      IF bio.done THEN names[no]:=name; INC(no) END
    END
  END collect;

  PROCEDURE park_disks;
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO no-1 DO
      IF bio.kind(files[i])*bio.is_disk#{} THEN
        bio.doio(files[i],r);
        IF bio.done THEN std.print('device "%s" powered off\n',names[i])
        ELSE             std.perror(r.res,'"%s" %%s\n',names[i])
        END
      END
    END
  END park_disks;

  PROCEDURE park_others;
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO no-1 DO
      IF bio.kind(files[i])*bio.is_disk={} THEN bio.doio(files[i],r) END
    END
  END park_others;

BEGIN
  r.op:=req.POWER_OFF; r.buf:=NIL;
  r.ofs:=0;            r.len:=0;
  no:=0;
  bio.open(dev,"/dev",'rx');
  bio.dir_walk(bio.cd,0);
  bio.dir_walk(dev,0);
  LOOP
    IF     bio.get_entry(bio.cd,name,mode) THEN d:=bio.cd
    ELSIF  bio.get_entry(dev,   name,mode) THEN d:=dev
    ELSE EXIT
    END;
    IF mode*bio.e_esc#{} THEN collect END
  END;
  bio.end_walk(dev);
  bio.end_walk(bio.cd);
  park_disks;
  park_others;
  tim.delay(1,tim.sec);
END power_off;

PROCEDURE pusage;
BEGIN
  std.print('  "boo"  system bootstrap utility program    (c) KRONOS\n');
  std.print("run\n");
  std.print("   boo device_name\n");
  std.print("   boo system_name\n");
  std.print("                                         Leopold, Dec 21 89\n")
END pusage;

VAR s: STRING;
    f: bio.FILE;
    a: POINTER TO INTEGER;
    i: INTEGER;
   ch: CHAR;
  eof: INTEGER;

BEGIN
  IF (HIGH(arg.words)<0) OR arg.flag('-','h') THEN pusage; HALT END;
  bio.open(f,arg.words[0],'r');
  IF NOT bio.done THEN
    tty.perror(bio.error,'open("%s")=%%s\n',arg.words[0]); HALT(bio.error)
  END;
  IF (bio.is_dir+bio.is_tty+ bio.is_spec)*bio.kind(f)#{} THEN
    tty.print('"%s" none bootable file\n',arg.words[0]); HALT(err.unsuitable)
  END;
  IF bio.is_disk*bio.kind(f)#{} THEN eof:=4096 ELSE eof:=bio.eof(f) END;
  NEW(s,eof+3+WSP*4);
  bio.get(f,s,eof);
  IF NOT bio.done THEN
    tty.perror(bio.error,'read("%s",%d)=%%s',arg.words[0],eof); HALT(bio.error)
  END;
  REPEAT
    key.bell(1);
    tty.print('Bootstrap from "%s"; are you sure? ',arg.words[0]);
    key.read(ch);
    tty.print('%c\n',ch);
  UNTIL (CAP(ch)='Y') OR (CAP(ch)='N');
  IF CAP(ch)#'Y' THEN HALT END;
  power_off;
  di;
  a:=SYSTEM.ADR(s)+88h;
  sta.get(sta.mem_top,a^);
  start(SYSTEM.ADR(s),SIZE(s));
END boo.
