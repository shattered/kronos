IMPLEMENTATION MODULE MouseDrv; (* Sem 09-Oct-88. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR;

IMPORT  os  : osKernel;
IMPORT  fs  : osFiles;
IMPORT  sio : SIOmBUS;
IMPORT  cod : defCodes;
IMPORT  req : defRequest;
IMPORT  tsk : tskEnv;

CONST bufsz=32;
TYPE  item=RECORD dx,dy: INTEGER; ch: CHAR END;

VAR port : INTEGER;
    keys : BITSET;
    buf  : ARRAY [0..bufsz-1] OF item;
    beg  : INTEGER;
    end  : INTEGER;
    hei  : os.signal_rec;
    x1,y1: INTEGER;
    x2,y2: INTEGER;
    cnt  : INTEGER;
    last : INTEGER;
    next : INTEGER;
    app  : BOOLEAN;
    prs  : os.process;
    file : fs.FILE;  -- клавиатура

PROCEDURE di(): BITSET; CODE cod.getm cod.copt cod.li3 cod.bic cod.setm END di;
PROCEDURE ei(m: BITSET); CODE cod.setm END ei;

PROCEDURE channel(n: INTEGER);
BEGIN
  sio.init(n);
  sio.no_xon_xoff;
END channel;

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

VAR  buff   : ARRAY [0..63] OF CHAR;

PROCEDURE read;
  VAR pos,len: INTEGER;
      dx,dy  : INTEGER;
      ch     : CHAR;
      st     : BITSET;
      i      : INTEGER;
BEGIN
  pos:=0; len:=HIGH(buff)+1; dx:=0; dy:=0; st:=keys/{0..2};
  sio.read(buff,pos,len); len:=pos; pos:=0;
  WHILE pos<len DO
    ch:=buff[pos]; INC(pos);
    CASE cnt OF
      |0  : IF (ch>=200c)&(ch<=207c) THEN
              st:=BITSET(ch)*{0..2};
            ELSE
              cnt:=-1;
            END;
      |1,3: IF ch<200c THEN INC(dx,ORD(ch)) ELSE DEC(dx,100h-ORD(ch)) END;
      |2,4: IF ch<200c THEN INC(dy,ORD(ch)) ELSE DEC(dy,100h-ORD(ch)) END;
    END;
    cnt:=(cnt+1) MOD 5;
  END;
  IF (dx#0)OR(dy#0) THEN
    IF app THEN
      INC(buf[last].dx,dx); INC(buf[last].dy,dy);
    ELSE
      put(dx,dy,0c);
    END;
  END;
  st:=st/{0..2};
  IF st#keys THEN
    FOR i:=0 TO 2 DO
      IF i IN (st/keys) THEN
        IF i IN st THEN
          CASE i OF |0: ch:=226c |1: ch:=227c |2: ch:=215c END;
          INCL(keys,i);
        ELSE
          CASE i OF |0: ch:=236c |1: ch:=237c |2: ch:=225c END;
          EXCL(keys,i);
        END;
        put(0,0,ch);
      END;
    END;
  END;
END read;

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

PROCEDURE stop;
BEGIN
  os.remove_action(read);
END stop;

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
  dir:=fs.root;
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
  beg:=0; end:=0; cnt:=0; last:=0; next:=1; app:=FALSE; rel:=TRUE;
  keys:={};
  os.ini_signal(hei,{},0);
  channel(2);
  r:=os.make_process(prs,keyboard,300);
  IF r#0 THEN HALT(r) END;
  os.start(prs);
  r:=os.final(os.self(),stop);
  IF r#0 THEN HALT(r) END;
  r:=os.insert_action(read);
  IF r#0 THEN HALT(r) END;
END MouseDrv.
