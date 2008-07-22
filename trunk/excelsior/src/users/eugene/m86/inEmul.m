IMPLEMENTATION MODULE inEmul; (* Sem 12-Mar-91. (c) KRONOS *)

FROM SYSTEM      IMPORT ADR, WORD;

IMPORT  mem : pcSystem;
IMPORT  c   : inCmd;
IMPORT  pc  : pcTab;
IMPORT  vrs : inVars;
IMPORT  flw : inFlow;
IMPORT  asm : inAsm;

IMPORT  tty : Terminal;
IMPORT  bio : BIO;

WITH STORAGE : mem;

VAR
  tbl : DYNARR OF RECORD
    proc: INTEGER;
    name: ARRAY [0..31] OF CHAR;
  END;

PROCEDURE error(VAL s: ARRAY OF CHAR; SEQ x: WORD);
BEGIN
  pc.error(0,0,s,x); HALT(50h);
END error;

PROCEDURE new(): INTEGER;
  VAR p: INTEGER;
BEGIN
  p:=vrs.new_proc();
  WITH vrs.prcs[p] DO
    mod:=0; loc_sz:=0;
    WITH vrs.cu DO
      no:=prc_sz; INC(prc_sz);
      RESIZE(flw.proc_flow,prc_sz);
      flw.proc_flow[no]:=NIL;
    END;
    lvl:=1;
    export:=FALSE;
    slink :=FALSE;
  END;
  RETURN p;
END new;

PROCEDURE load(s: ARRAY OF CHAR): INTEGER;
  CONST lib_name = 'lib.as';
  VAR
    bf : ARRAY [0..4095] OF CHAR;
    blk: INTEGER;
    pos: INTEGER;
    eof: INTEGER;
    lib: bio.FILE;
  PROCEDURE io_chk;
  BEGIN
    IF bio.done THEN RETURN END;
    HALT(bio.error);
  END io_chk;
  PROCEDURE search;
    VAR n: INTEGER;
  BEGIN
    WHILE blk+pos<eof DO
      IF pos>=4096 THEN
        INC(blk,4096); DEC(pos,4096);
        n:=eof-blk;
        IF n>4096 THEN n:=4096 END;
        bio.seek(lib,blk,0); io_chk;
        bio.read(lib,ADR(bf),n); io_chk;
      END;
      IF bf[pos]='?' THEN RETURN END;
      INC(pos);
    END;
  END search;
  PROCEDURE get(): CHAR;
    VAR n: INTEGER;
  BEGIN
    IF pos+blk>=eof THEN RETURN 36c END;
    IF pos>=4096 THEN
      INC(blk,4096); DEC(pos,4096);
      n:=eof-blk;
      IF n>4096 THEN n:=4096 END;
      bio.seek(lib,blk,0); io_chk;
      bio.read(lib,ADR(bf),n); io_chk;
    END;
    INC(pos); RETURN bf[pos-1];
  END get;
  VAR
    txt: STRING;
    i,fr,to,proc: INTEGER;
    nm : ARRAY [0..31] OF CHAR;
    etc: bio.PATHs;
BEGIN
--tty.print('load RTS "%s"\n',s);
  bio.get_paths(etc,'ETC'); io_chk;
  bio.lookup(etc,lib,lib_name,'r'); io_chk;
  bio.close_paths(etc); io_chk;
  eof:=bio.eof(lib); pos:=4096; blk:=-4096;
  REPEAT
    search;
    IF blk+pos>=eof THEN
      bio.close(lib); io_chk;
      error('RTS procedure "%s" not found',s);
      RETURN 0
    END;
    i:=0; ASSERT(get()='?');
    LOOP
      nm[i]:=get();
      IF (nm[i]=36c) OR (i=HIGH(nm)) THEN EXIT END;
      INC(i);
    END;
    nm[i]:=0c;
  UNTIL nm=s;
  fr:=blk+pos; search; to:=blk+pos;
  NEW(txt,to-fr+1);
  bio.seek(lib,fr,0); io_chk;
  bio.read(lib,ADR(txt),to-fr); io_chk;
  bio.close(lib); io_chk;
  txt[HIGH(txt)]:=0c;
  proc:=new();
  flw.start(flw.new());
  flw.proc_flow[vrs.prcs[proc].no]:=flw.block;
  asm.gen_code(txt);
  flw.finish;
  DISPOSE(txt);
  RETURN proc;
END load;

PROCEDURE find(VAL s: ARRAY OF CHAR): INTEGER;
  VAR i,j: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(tbl) DO
    IF tbl[i].name=s THEN RETURN tbl[i].proc END;
  END;
  i:=load(s);
  RESIZE(tbl,LEN(tbl)+1);
  WITH tbl[HIGH(tbl)] DO
    j:=0;
    WHILE (j<=HIGH(s)) & (j<HIGH(name)) & (s[j]#0c) DO
      name[j]:=s[j]; INC(j);
    END;
    name[j]:=0c;
    proc:=i;
  END;
  RETURN i;
END find;

PROCEDURE emul_call(VAL s: ARRAY OF CHAR);
  VAR fr,to: flw.node;
BEGIN
  to:=flw.new();
  fr:=flw.block;
  fr^.md:=flw.nm_call;
  fr^.true:=to;
  flw.finish;
  fr^.proc:=find(s);
  flw.start(to);
END emul_call;

PROCEDURE init;
BEGIN
  NEW(tbl);
END init;

BEGIN
  NEW(tbl);
END inEmul.
