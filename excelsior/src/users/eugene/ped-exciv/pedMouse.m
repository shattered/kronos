IMPLEMENTATION MODULE pedMouse; (* Sem 09-Oct-88. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR;

IMPORT  os  : osKernel;
IMPORT  fs  : osFiles;
IMPORT  cod : defCodes;
IMPORT  req : defRequest;
IMPORT  cpd : CPD;
IMPORT  tsk : tskEnv;

IMPORT std: Terminal;


CONST
  bufsz=32;

TYPE
  item=RECORD dx,dy: INTEGER; ch: CHAR END;

VAR
  keys : BITSET;
  buf  : ARRAY [0..bufsz-1] OF item;
  beg  : INTEGER;
  end  : INTEGER;
  hei  : os.signal_rec;
  last : INTEGER;
  next : INTEGER;
  app  : BOOLEAN;
  prs  : os.process;
  file : fs.FILE;

PROCEDURE di(): BITSET; CODE cod.getm cod.copt cod.li3 cod.bic cod.setm END di;
PROCEDURE ei(m: BITSET); CODE cod.setm END ei;

PROCEDURE put(x,y: INTEGER; c: CHAR);
  VAR m: BITSET;
BEGIN
  m:=di();
  IF hei.queue#os.QUEUE(NIL) THEN os.send(hei) END;
  IF next#beg THEN
    last:=end; end:=next; next:=(next+1) MOD bufsz;
    buf[last].dx:=x; buf[last].dy:=y; buf[last].ch:=c;
    app:=(c=0c)&(last#beg);
  END;
  ei(m);
END put;

PROCEDURE FE(i: INTEGER); CODE 0FEh END FE;

PROCEDURE mouse;
  VAR m: BITSET; dx,dy,i: INTEGER; st: BITSET; ch: CHAR;
BEGIN
  LOOP
    cpd.read(dx,dy,st);
--std.print("dx=%d dy=%d st=%{}\n", dx, dy, st);
--FE(dx);
--FE(dy);
    IF NOT cpd.done THEN HALT(cpd.error) END;
    IF (dx#0) OR (dy#0) THEN
      m:=di();
      IF app THEN INC(buf[last].dx,dx); INC(buf[last].dy,dy);
      ELSE put(dx,dy,0c);
      END;
      ei(m);
    END;
    IF st#keys THEN
      FOR i:=0 TO 2 DO
        IF i IN (st/keys) THEN
          IF i IN st THEN
            CASE i OF |0: ch:=266c |1: ch:=266c |2: ch:=266c END;
            INCL(keys,i);
          ELSE
            CASE i OF |0: ch:=267c |1: ch:=267c |2: ch:=267c END;
            EXCL(keys,i);
          END;
          put(0,0,ch);
        END;
      END;
    END;
  END;
END mouse;

PROCEDURE empty?(): BOOLEAN;
BEGIN
  RETURN beg=end;
END empty?;

PROCEDURE first(VAR x,y: INTEGER; VAR ch: CHAR);
BEGIN
  x:=buf[beg].dx;
  y:=buf[beg].dy;
  ch:=buf[beg].ch;
END first;

PROCEDURE drop;
  VAR m: BITSET;
BEGIN
  m:=di();
  ASSERT(beg#end);
  beg:=(beg+1) MOD bufsz;
  IF beg=last THEN app:=FALSE END;
  ei(m);
END drop;

PROCEDURE wait;
  VAR m: BITSET;
BEGIN
  m:=di();
  IF beg=end THEN os.wait(hei) END;
  ei(m);
END wait;

PROCEDURE Read(): CHAR;
  VAR x,y: INTEGER; c: CHAR;
BEGIN
  wait; first(x,y,c); drop; RETURN c;
END Read;

PROCEDURE keyboard;
  VAR ch: CHAR; r: req.REQUEST; res: INTEGER;
BEGIN
  LOOP
    ch:=0c;
    r.op:=req.READ;
    r.buf:=ADR(ch);
    r.pos:=0;
    r.len:=1;
    r.ofs:=0;
    res:=fs.doio(file,r);
    IF res#0 THEN HALT(res) END;
    put(0,0,ch);
  END;
END keyboard;

VAR
  r   : INTEGER;
  name: STRING;
  str : ARRAY [0..63] OF CHAR;
  i,j : INTEGER;
  dir : fs.FILE;

BEGIN
  tsk.get_str(tsk.key,name);
  IF NOT tsk.done THEN HALT(tsk.error) END;
  ASSERT(name[0]='/');
  dir:=fs.root();
  i:=1;
  LOOP
    j:=0;
    WHILE (name[i]#'/') & (name[i]#0c) DO str[j]:=name[i]; INC(i); INC(j) END;
    str[j]:=0c;
    r:=fs.open(dir,file,str,{});
    IF r#0 THEN HALT(r) END;
    IF name[i]=0c THEN EXIT END;
    dir:=file; INC(i);
  END;
  beg:=0; end:=0; last:=0; next:=1;
  app:=FALSE; rel:=TRUE; keys:={};
  os.ini_signal(hei,{},0);
  r:=os.make_process(prs,keyboard,300);
  IF r#0 THEN HALT(r) END;
  os.start(prs);
  r:=os.make_process(prs,mouse,300);
  IF r#0 THEN HALT(r) END;
  os.start(prs);
END pedMouse.
