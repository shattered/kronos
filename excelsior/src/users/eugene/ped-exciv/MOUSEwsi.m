MODULE MOUSEwsi; (*$T-U+ Leg  24-Oct-90. (c) KRONOS *)

IMPORT  sys: SYSTEM;
IMPORT  os : osKernel;
IMPORT  sio: SIOws;
IMPORT   fs: osFiles;
IMPORT  def: defCPD;
IMPORT  err: defErrors;
IMPORT  req: defRequest;
IMPORT  env: tskEnv;

-- INSTALATION: "MOUSEwsi <port_no>"

CONST trans = ARRAY OF BITSET{{},{2},{1},{1..2},{0},{0,2},{0..1},{0..2}};
      MOUSE = sio.x16 + sio.stop2 + sio.bits8 ;

                                      --012345678901234567890123456789 1
VAR   buff: ARRAY [0..255] OF BITSET; --__________^^^^^^^^^^___
       beg: INTEGER;                  --    DX        DY     ST
       end: INTEGER;                  --
       rdy: os.signal_rec;
      icsr: INTEGER;
      idtr: INTEGER;
     state: def.STATE;
      name: ARRAY [0..7] OF CHAR;
keys_state: BITSET;

PROCEDURE ready(VAR r: req.REQUEST);
BEGIN
  IF beg<=end THEN r.len:=end-beg ELSE r.len:=HIGH(buff)+1-(beg-end) END
END ready;

PROCEDURE read(VAR r: req.REQUEST);
  VAR bf: POINTER TO RECORD x,y: INTEGER; st: BITSET END;
       b: BITSET;
       n: INTEGER;
BEGIN
  WITH r DO
    IF (r.len#-1) OR (r.pos#0) OR (r.ofs#0) THEN
      r.res:=err.bad_parm; RETURN
    END;
    res:=err.ok;
    n:=os.wait_del(-1,rdy);
    IF n#0 THEN res:=err.ipted_op; RETURN END;
    bf:=buf; b:=buff[beg];
    bf^.x:=INTEGER(b*{0..9})-512; b:=b>>10;
    bf^.y:=INTEGER(b*{0..9})-512; b:=b>>10;
    bf^.st:=b*{0..2};
    beg:=(beg+1) MOD (HIGH(buff)+1);
  END
END read;

PROCEDURE wait(VAR r: req.REQUEST);
  VAR i: INTEGER;
BEGIN
  i:=(r.len+19) DIV 20;
  i:=os.wait_del(i,rdy);
  IF    i=1 THEN r.res:=err.ipted_op
  ELSIF i<0 THEN r.res:=err.time_out
  ELSE  os.send(rdy)
  END
END wait;

PROCEDURE mouse(no: INTEGER; VAL x: ARRAY OF sys.WORD): INTEGER;
  VAR adr: sys.ADDRESS;
      pps: POINTER TO POINTER TO def.STATE;
BEGIN
  CASE no OF
    |def._info   : IF HIGH(x)<0 THEN RETURN err.bad_parm END;
                   adr:=x[0]; pps:=adr; pps^:=sys.ADR(state)
    |def._reset  :
    |def._restore:
  ELSE
    RETURN err.inv_op
  END;
  RETURN err.ok
END mouse;

PROCEDURE DO_IO(VAR r: req.REQUEST);
  VAR arg: DYNARR OF sys.WORD;
BEGIN
  WITH r DO
    res:=err.ok;
    CASE op MOD 256 OF
      |req.NOP    :
      |req.WAIT   : wait(r)
      |req.READ   : read (r)
      |req.READY  : ready(r)
      |req.CONTROL:
        arg^.ADR:=r.buf; arg^.HIGH:=r.len-1;
        r.res:=mouse(op DIV 256,arg)
    ELSE
      res:=err.inv_op
    END
  END
END DO_IO;

PROCEDURE ms_driver;
  VAR ch   : CHAR;
      st   : BITSET;
      i,cnt: INTEGER;
      dx,dy: INTEGER;
BEGIN
  cnt:=-1; dx:=0; dy:=0;
  LOOP
    ch:=CHAR(sio.get()*{0..7});
    cnt:=(cnt+1) MOD 5;
    CASE cnt OF
      |0  : IF (ch>=200c) & (ch<=207c) THEN
            st:=trans[INTEGER((BITSET(ch)*{0..2})/{0..2})];
            ELSE cnt:=-1 END;
      |1,3: IF ch<200c THEN INC(dx,ORD(ch)) ELSE INC(dx,ORD(ch)-100h) END;
      |2,4: IF ch<200c THEN INC(dy,ORD(ch)) ELSE INC(dy,ORD(ch)-100h) END;
    END;
    IF (cnt=4) & ((dx#0) OR (dy#0) OR (st#keys_state)) THEN
      i:=(end+1) MOD (HIGH(buff)+1);
      IF i#beg THEN
        buff[end]:=(BITSET(dx+512)*{0..9})+
                   (BITSET(dy+512)*{0..9})<<10+st<<20;
        end:=i; os.send(rdy);
        keys_state:=st;
        dx:=0; dy:=0;
      END;
    END;
  END
END ms_driver;

PROCEDURE stop;
BEGIN
  sio.stop
END stop;

PROCEDURE init(port: INTEGER);
  VAR r: INTEGER;
BEGIN
  beg:=0; end:=0; keys_state:={}; name:='mou0';
  os.ini_signal(rdy,os.break+os.sendup,0);

  r:=sio.init(port,MOUSE);
  IF r#err.ok THEN HALT(r) END;
  sio.x_inp(FALSE);

  name[3]:=CHAR(ORD(name[3])+port);
  r:=fs.define_driver(name,"",0,fs.spec,DO_IO);
  IF r#err.ok THEN HALT(r) END;
  env.put_str(env.info,name,TRUE);
  env.final(stop);
END init;

VAR s: STRING;

BEGIN

  env.get_str(env.args,s);
  IF NOT env.done THEN HALT(env.error) END;
  IF (HIGH(s)<0) OR (s[0]<"0") OR (s[0]>"9") THEN HALT(err.bad_parm) END;
  init(ORD(s[0])-ORD("0"));

  WITH state DO
    type:=1; nokeys:=3; x_max:=0; y_max:=0; rel:=TRUE;  x:=0; y:=0; keys:={}
  END;
  env.become_ipr;
  sio.monitor(ms_driver);
END MOUSEwsi.
