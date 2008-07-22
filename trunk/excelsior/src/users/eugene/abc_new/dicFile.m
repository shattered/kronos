IMPLEMENTATION MODULE dicFile; (* Sem 21-Aug-89. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR, ADDRESS;
IMPORT  err : defErrors;
IMPORT  clc : Time;
IMPORT  str : Strings;
IMPORT  mem : Heap;
IMPORT  bio : BIO;
IMPORT  tty : Terminal;

TYPE
  dictionary=POINTER TO dict_rec;
  buffer=ARRAY [0..1023] OF INTEGER;
  buf_ptr=POINTER TO buffer;
  dict_rec=RECORD
    ident: INTEGER;
    file : bio.FILE;
    name : ARRAY [0..63] OF CHAR;
    eof  : INTEGER;
    cnt  : INTEGER;
    bufs : ARRAY [0..11] OF buffer;
    wrt  : ARRAY [0..11] OF BOOLEAN;
    blk  : ARRAY [0..11] OF INTEGER;
  END;
  rel_bits=(lss,equ,gtr);
  rel=SET OF rel_bits;
  node=RECORD
    left   : INTEGER;
    right  : INTEGER;
    inp_pos: INTEGER;
    inp_len: INTEGER;
    out_pos: INTEGER;
    out_len: INTEGER;
    time   : INTEGER;
  END;

VAR
  wsp  : ARRAY [0..3] OF dictionary;

PROCEDURE no_memory;
BEGIN
  io_error;
  tty.print('No memory for dictionary.\n');
  HALT(1);
END no_memory;

PROCEDURE check;
BEGIN
  IF bio.done THEN RETURN END;
  io_error; tty.perror(bio.error,'IO error: %%s.\n'); HALT;
END check;

PROCEDURE del_ext(VAR fn: ARRAY OF CHAR);
  CONST separator = '/'; extchar   = '.';
  VAR i: INTEGER;
BEGIN
  i:=str.len(fn)-1;
  WHILE (i>=0) & (fn[i]#separator) & (fn[i]#extchar) DO DEC(i) END;
  IF (i>=0) & (fn[i]=extchar) THEN fn[i]:=0c END;
END del_ext;

PROCEDURE app_ext(VAR fn: ARRAY OF CHAR; VAL ext: ARRAY OF CHAR);
BEGIN
  str.app(fn,'.'); str.append(fn,'%s',ext);
END app_ext;

PROCEDURE change_ext(VAR fn: ARRAY OF CHAR; VAL ext: ARRAY OF CHAR);
BEGIN
  del_ext(fn); app_ext(fn,ext);
END change_ext;

PROCEDURE open(VAR d: dictionary; name: ARRAY OF CHAR): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  i:=0;
  WHILE wsp[i]#NIL DO
    INC(i); IF i>HIGH(wsp) THEN no_memory END;
  END;
  mem.allocate(wsp[i],SIZE(wsp[i]^));
  IF wsp[i]=NIL THEN no_memory END;
  d:=wsp[i]; d^.ident:=i;
  i:=0;
  WHILE (i<HIGH(d^.name)) & (i<=HIGH(name)) & (name[i]#0c) DO
    d^.name[i]:=name[i]; INC(i);
  END;
  d^.name[i]:=0c; d^.name[HIGH(d^.name)]:=CHAR(i);
  change_ext(d^.name,'abc');
  bio.open(d^.file,d^.name,'rw');
  IF NOT bio.done THEN
    mem.deallocate(wsp[d^.ident],SIZE(d^));
    d:=NIL;
    IF bio.error=err.no_entry THEN RETURN FALSE END;
    check;
  END;
  d^.eof:=bio.eof(d^.file) DIV 4;
  FOR i:=0 TO HIGH(d^.bufs) DO d^.blk[i]:=-1; d^.wrt[i]:=FALSE END;
  d^.cnt:=0;
  RETURN TRUE;
END open;

PROCEDURE close(VAR d: dictionary);
  VAR r: BOOLEAN; i: INTEGER;
BEGIN
  IF d=NIL THEN RETURN END;
  FOR i:=0 TO HIGH(d^.bufs) DO
    IF d^.wrt[i] THEN
      bio.seek(d^.file,d^.blk[i]*4096,0); check;
      bio.write(d^.file,ADR(d^.bufs[i]),4096); check;
      d^.wrt[i]:=FALSE;
    END;
  END;
  bio.close(d^.file);
  wsp[d^.ident]:=NIL;
  mem.deallocate(d,SIZE(d^));
  check;
END close;

PROCEDURE flash_cash;
  VAR i,j: INTEGER;
BEGIN
  FOR j:=0 TO HIGH(wsp) DO
    IF wsp[j]#NIL THEN
      WITH wsp[j]^ DO
        FOR i:=0 TO HIGH(bufs) DO
          IF wrt[i] THEN
            bio.seek(file,blk[i]*4096,0); check;
            bio.write(file,ADR(bufs[i]),4096); check;
            wrt[i]:=FALSE;
          END;
        END;
      END;
    END;
  END;
END flash_cash;

PROCEDURE create(VAR d: dictionary; name: ARRAY OF CHAR);
  VAR i: INTEGER; r: BOOLEAN;
BEGIN
  i:=0;
  WHILE wsp[i]#NIL DO
    INC(i); IF i>HIGH(wsp) THEN no_memory END;
  END;
  mem.allocate(d,SIZE(d^));
  IF d=NIL THEN no_memory END;
  wsp[i]:=d; d^.ident:=i;
  i:=0;
  WHILE (i<HIGH(d^.name)) & (i<=HIGH(name)) & (name[i]#0c) DO
    d^.name[i]:=name[i]; INC(i);
  END;
  d^.name[i]:=0c; d^.name[HIGH(d^.name)]:=CHAR(i);
  change_ext(d^.name,'abc');
  bio.create(d^.file,d^.name,'rw',1);
  IF NOT bio.done THEN
    wsp[d^.ident]:=NIL; mem.deallocate(d,SIZE(d^)); check
  END;
  d^.eof:=0;
  FOR i:=0 TO HIGH(d^.bufs) DO d^.blk[i]:=-1; d^.wrt[i]:=FALSE END;
  d^.cnt:=0;
END create;

PROCEDURE read(d: dictionary; pos: INTEGER; a: ADDRESS; n: INTEGER);
  VAR b: INTEGER; buf: buf_ptr; i: INTEGER;
BEGIN
  b:=pos DIV 1024; pos:=pos MOD 1024;
  LOOP
    i:=0;
    LOOP
      IF d^.blk[i]=b THEN EXIT END;
      INC(i);
      IF i>HIGH(d^.bufs) THEN
        i:=d^.cnt MOD (HIGH(d^.bufs)+1); d^.cnt:=i+1;
        IF d^.wrt[i] THEN
          bio.seek(d^.file,d^.blk[i]*4096,0); check;
          bio.write(d^.file,ADR(d^.bufs[i]),4096); check;
          d^.wrt[i]:=FALSE;
        END;
        d^.blk[i]:=b;
        bio.seek(d^.file,d^.blk[i]*4096,0); check;
        bio.read(d^.file,ADR(d^.bufs[i]),4096);
        IF NOT bio.done & (bio.error#err.no_data) THEN check END;
      END;
    END;
    buf:=ADR(d^.bufs[i]);
    WHILE pos<1024 DO
      IF n=0 THEN RETURN END;
      a^:=buf^[pos]; INC(pos); INC(a); DEC(n);
    END;
    pos:=0; INC(b);
  END;
END read;

PROCEDURE write(d: dictionary; pos: INTEGER; a: ADDRESS; n: INTEGER);
  VAR b: INTEGER; buf: buf_ptr; i: INTEGER;
BEGIN
  b:=pos DIV 1024; pos:=pos MOD 1024;
  LOOP
    i:=0;
    LOOP
      IF d^.blk[i]=b THEN EXIT END;
      INC(i);
      IF i>HIGH(d^.bufs) THEN
        i:=d^.cnt MOD (HIGH(d^.bufs)+1); d^.cnt:=i+1;
        IF d^.wrt[i] THEN
          bio.seek(d^.file,d^.blk[i]*4096,0); check;
          bio.write(d^.file,ADR(d^.bufs[i]),4096); check;
          d^.wrt[i]:=FALSE;
        END;
        d^.blk[i]:=b;
        bio.seek(d^.file,d^.blk[i]*4096,0); check;
        bio.read(d^.file,ADR(d^.bufs[i]),4096);
        IF NOT bio.done & (bio.error#err.no_data) THEN check END;
      END;
    END;
    buf:=ADR(d^.bufs[i]);
    WHILE pos<1024 DO
      IF n=0 THEN
        d^.wrt[i]:=TRUE; RETURN
      END;
      buf^[pos]:=a^; INC(pos); INC(a); DEC(n);
    END;
    d^.wrt[i]:=TRUE;
    pos:=0; INC(b);
  END;
END write;

PROCEDURE len(VAR t: text): INTEGER;
  VAR i,j: INTEGER;
BEGIN
  i:=0;
  WHILE (i<=HIGH(t)) & (t[i]#0c) DO
    IF (t[i]<' ') OR (t[i]>=200c) & (t[i]<300c) THEN t[i]:=' ' END;
    INC(i);
  END;
  WHILE (i>0) & (t[i-1]=' ') DO DEC(i); t[i]:=0c END;
  WHILE t[0]=' ' DO
    j:=0;
    REPEAT t[j]:=t[j+1]; INC(j) UNTIL (j=HIGH(t)) OR (j=i);
    DEC(i);
  END;
  IF i>HIGH(t) THEN
    t[HIGH(t)]:=0c; RETURN SIZE(t)
  ELSE
    REPEAT t[i]:=0c; INC(i) UNTIL (i MOD 4)=0;
    RETURN i DIV 4;
  END;
END len;

PROCEDURE cmp(VAR pattern,item: text): rel;
  PROCEDURE Match(pi,si: INTEGER): BOOLEAN;
  BEGIN
    WHILE pattern[pi]#'*' DO
      IF pattern[pi]=0c THEN RETURN item[si]=0c
      ELSIF pattern[pi]=item[si] THEN INC(si); INC(pi)
      ELSE RETURN FALSE
      END;
    END;
    WHILE pattern[pi]='*' DO INC(pi) END;
    IF pattern[pi]=0c THEN RETURN TRUE END;
    WHILE item[si]#0c DO
      IF Match(pi,si) THEN RETURN TRUE END; INC(si);
    END;
    RETURN FALSE;
  END Match;
  VAR pi,si: INTEGER;
BEGIN
  pi:=0; si:=0;
  WHILE pattern[pi]#'*' DO
    IF pattern[pi]=0c THEN
      IF item[si]=0c THEN RETURN rel{equ} ELSE RETURN rel{lss} END;
    ELSIF item[si]=0c THEN RETURN rel{gtr};
    ELSIF pattern[pi]=item[si] THEN INC(si); INC(pi)
    ELSIF pattern[pi]>item[si] THEN RETURN rel{gtr};
    ELSE  RETURN rel{lss}
    END;
  END;
  WHILE pattern[pi]='*' DO INC(pi) END;
  IF pattern[pi]=0c THEN RETURN rel{lss,equ,gtr} END;
  WHILE item[si]#0c DO
    IF Match(pi,si) THEN RETURN rel{lss,equ,gtr} END; INC(si);
  END;
  RETURN rel{lss,gtr};
END cmp;

PROCEDURE find(d: dictionary; pattern: text;ip: iter_proc);
  PROCEDURE find_scan(pos: INTEGER): BOOLEAN;
    VAR n: node; inp,out: text; s: rel;
  BEGIN
    read(d,pos,ADR(n),SIZE(n));
    IF n.inp_len>SIZE(text) THEN n.inp_len:=SIZE(text) END;
    read(d,n.inp_pos,ADR(inp),n.inp_len);
    s:=cmp(pattern,inp);
    IF (lss IN s) & (n.left>0) THEN
      IF find_scan(n.left) THEN RETURN TRUE END;
    END;
    IF (equ IN s) & (n.out_len>0) THEN
      IF n.out_len>SIZE(text) THEN n.out_len:=SIZE(text) END;
      read(d,n.out_pos,ADR(out),n.out_len);
      time:=n.time;
      IF ip(inp,out) THEN RETURN TRUE END;
    END;
    IF (gtr IN s) & (n.right>0) THEN
      IF find_scan(n.right) THEN RETURN TRUE END;
    END;
    RETURN FALSE;
  END find_scan;
BEGIN
  IF (d=NIL) OR (d^.eof=0) THEN RETURN END;
  IF pattern='' THEN RETURN END;
  IF find_scan(0) THEN END;
END find;

PROCEDURE remove(d: dictionary; key: text);
  PROCEDURE rem_scan(pos: INTEGER);
    VAR n: node; inp: text;
  BEGIN
    read(d,pos,ADR(n),SIZE(n));
    IF n.inp_len>SIZE(text) THEN n.inp_len:=SIZE(text) END;
    read(d,n.inp_pos,ADR(inp),n.inp_len);
    IF key=inp THEN
      n.out_len:=0;
      write(d,pos,ADR(n),SIZE(n));
    ELSIF (n.left>0)  & (key<inp) THEN rem_scan(n.left)
    ELSIF (n.right>0) & (key>inp) THEN rem_scan(n.right)
    END;
  END rem_scan;
BEGIN
  IF (d=NIL) OR (d^.eof=0) THEN RETURN END;
  IF key='' THEN RETURN END;
  rem_scan(0);
END remove;

PROCEDURE put(d: dictionary; inp,out: text);
  PROCEDURE new_node;
    VAR n: node;
  BEGIN
    n.left:=0;
    n.right:=0;
    n.inp_len:=len(inp);
    n.inp_pos:=d^.eof+SIZE(n);
    n.out_len:=len(out);
    n.out_pos:=n.inp_pos+n.inp_len;
    n.time   :=clc.sys_time(clc.sec);
    write(d,d^.eof,ADR(n),SIZE(n));
    write(d,n.inp_pos,ADR(inp),n.inp_len);
    write(d,n.out_pos,ADR(out),n.out_len);
    d^.eof:=n.out_pos+n.out_len;
    bio.end(d^.file,d^.eof*4);
  END new_node;
  PROCEDURE find_free(pos: INTEGER);
    VAR n: node; txt: text;
  BEGIN
    read(d,pos,ADR(n),SIZE(n));
    IF n.inp_len>SIZE(text) THEN n.inp_len:=SIZE(text) END;
    read(d,n.inp_pos,ADR(txt),n.inp_len);
    IF txt=inp THEN
      n.out_len:=len(out);
      n.out_pos:=d^.eof;
      n.time   :=clc.sys_time(clc.sec);
      write(d,pos,ADR(n),SIZE(n));
      write(d,n.out_pos,ADR(out),n.out_len);
      d^.eof:=n.out_pos+n.out_len;
      bio.end(d^.file,d^.eof*4);
    ELSIF txt>inp THEN
      IF n.left=0 THEN
        n.left:=d^.eof; write(d,pos,ADR(n),SIZE(n));
        new_node;
      ELSE
        find_free(n.left)
      END;
    ELSIF txt<inp THEN
      IF n.right=0 THEN
        n.right:=d^.eof; write(d,pos,ADR(n),SIZE(n));
        new_node;
      ELSE
        find_free(n.right)
      END;
    END;
  END find_free;
BEGIN
  IF d=NIL THEN RETURN END;
  IF inp='' THEN RETURN END;
  IF d^.eof=0 THEN new_node ELSE find_free(0) END;
END put;

PROCEDURE get(d: dictionary; inp: text; VAR out: text): BOOLEAN;
  PROCEDURE get_scan(pos: INTEGER): BOOLEAN;
    VAR n: node; txt: text;
  BEGIN
    read(d,pos,ADR(n),SIZE(n));
    IF n.inp_len>SIZE(text) THEN n.inp_len:=SIZE(text) END;
    read(d,n.inp_pos,ADR(txt),n.inp_len);
    IF txt=inp THEN
      IF n.out_len>0 THEN
        IF n.out_len>SIZE(text) THEN n.out_len:=SIZE(text) END;
        read(d,n.out_pos,ADR(out),n.out_len);
        time:=n.time;
        RETURN TRUE;
      ELSE
        RETURN FALSE;
      END;
    ELSIF (n.left>0)  & (txt>inp) THEN
      IF get_scan(n.left) THEN RETURN TRUE END;
    ELSIF (n.right>0) & (txt<inp) THEN
      IF get_scan(n.right) THEN RETURN TRUE END;
    END;
    RETURN FALSE;
  END get_scan;
BEGIN
  IF (d=NIL) OR (d^.eof=0) THEN RETURN FALSE END;
  IF inp='' THEN RETURN FALSE END;
  RETURN get_scan(0);
END get;

PROCEDURE rebalance(d: dictionary; ip: iter_proc);
  VAR
    tab: POINTER TO ARRAY [0..19999] OF INTEGER;
    cnt: INTEGER;
  PROCEDURE scan(pos: INTEGER);
    VAR n: node;
  BEGIN
    read(d,pos,ADR(n),SIZE(n));
    IF n.left>0 THEN scan(n.left) END;
    IF n.out_len>0 THEN
      IF cnt>HIGH(tab^) THEN no_memory END;
      tab^[cnt]:=pos; INC(cnt)
    END;
    IF n.right>0 THEN scan(n.right) END;
  END scan;
  PROCEDURE output(pos: INTEGER);
    VAR inp,out: text; n: node;
  BEGIN
    read(d,tab^[pos],ADR(n),SIZE(n));
    IF n.inp_len>SIZE(text) THEN n.inp_len:=SIZE(text) END;
    IF n.out_len>SIZE(text) THEN n.out_len:=SIZE(text) END;
    read(d,n.inp_pos,ADR(inp),n.inp_len);
    read(d,n.out_pos,ADR(out),n.out_len);
    time:=n.time;
    IF ip(inp,out) THEN END;
  END output;
  PROCEDURE center(from,to: INTEGER);
    VAR n: INTEGER;
  BEGIN
    IF from=to THEN output(from)
    ELSIF to=from+1 THEN output(from); output(to)
    ELSE
      n:=(to+from) DIV 2;
      output(n); center(from,n-1); center(n+1,to);
    END;
  END center;
BEGIN
  IF (d=NIL) OR (d^.eof=0) THEN RETURN END;
  mem.allocate(tab,SIZE(tab^));
  IF tab=NIL THEN no_memory END;
  cnt:=0; scan(0);
  IF cnt>0 THEN center(0,cnt-1) END;
  mem.deallocate(tab,SIZE(tab^));
END rebalance;

PROCEDURE dummy_proc; BEGIN END dummy_proc;

VAR i: INTEGER;

BEGIN
  FOR i:=0 TO HIGH(wsp) DO wsp[i]:=NIL END;
  io_error:=dummy_proc;
END dicFile.
