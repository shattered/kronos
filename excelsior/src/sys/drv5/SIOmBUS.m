IMPLEMENTATION MODULE SIOmBUS; (*$T-$N- Leo 10-Nov-90. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  err: defErrors;
IMPORT  req: defRequest;
IMPORT  cod: defCodes;
IMPORT  os : osKernel;

CONST ok = err.ok;

TYPE ADDRESS = SYSTEM.ADDRESS;

CONST BASE=80000h;

VAR put_inp: INTEGER;           MASK16_31: BITSET; (* :={16..31} *)
    put_end: INTEGER;
    put_beg: INTEGER;
    put_out: INTEGER;            put_size: INTEGER;

    get_inp: INTEGER;           MASK00_15: BITSET; (* :={00..15} *)
    get_end: INTEGER;
    get_beg: INTEGER;
    get_out: INTEGER;            get_size: INTEGER;


        BCB: POINTER TO ARRAY [0..0FFFFh] OF CHAR;
       wBCB: POINTER TO ARRAY [0..0FFFFh] OF INTEGER;
     F0000h: POINTER TO ARRAY [0..0FFFFh] OF CHAR;
    getFLAG: INTEGER; (* byte pointers *)
    putFLAG: INTEGER; (* relative 0    *)

PROCEDURE di(): BITSET; CODE cod.getm cod.copt cod.li3 cod.bic cod.setm END di;

PROCEDURE ei(m: BITSET);CODE cod.setm END ei;

PROCEDURE waitf(flag: INTEGER);
CODE cod.copt cod.trb cod.jbsc 04 cod.drop END waitf;


CONST channel = ARRAY OF INTEGER
                --  0  1  2  3  4  5  6  7  8  9
                  {09,14,12,13,02,-1,-1,-1,11,10};

PROCEDURE _init(port: INTEGER);
  VAR adr: ADDRESS;
     chan: INTEGER;
BEGIN
  chan:=channel[port];
  ASSERT(chan>=0,4Ah);
  adr:=BASE+(0F8010h+chan*4) DIV 4;
  adr:=INTEGER( BITSET(adr^)*MASK00_15 );
  BCB:=ADDRESS( F0000h ) + adr DIV 4;     wBCB:=ADDRESS(BCB);
  getFLAG:=( INTEGER(BCB)*4 )+00;
  putFLAG:=( INTEGER(BCB)*4 )+12;

  put_end:=INTEGER( BITSET(wBCB^[5]>>16) * MASK00_15); (* 22,23 bytes BCB *)
  put_beg:=INTEGER( BITSET(wBCB^[5]    ) * MASK00_15); (* 20,21 bytes BCB *)

  get_end:=INTEGER( BITSET(wBCB^[2]>>16) * MASK00_15); (* 10,11 bytes BCB *)
  get_beg:=INTEGER( BITSET(wBCB^[2]    ) * MASK00_15); (*  8, 9 bytes BCB *)

  put_inp:=INTEGER( BITSET(wBCB^[3]>>16) * MASK00_15); (* 14,15 bytes BCB *)
  get_out:=INTEGER( BITSET(wBCB^[1]    ) * MASK00_15); (*  4, 5 bytes BCB *)

  put_size:=put_end-put_beg+1;
  get_size:=get_end-get_beg+1
END _init;

PROCEDURE sio_init(port: INTEGER);
  VAR m: BITSET;
BEGIN
  _init(port);
  m:=di();
    waitf(putFLAG);     (* put_out:=put_inp:=put_beg *)
    wBCB^[4]:=INTEGER(BITSET(wBCB^[4])-MASK00_15+BITSET(put_beg));
    wBCB^[3]:=INTEGER(BITSET(wBCB^[3])*MASK00_15+BITSET(put_beg<<16)+{0});
    waitf(getFLAG);     (* get_out:=get_inp:=get_beg *)
    wBCB^[1]:=INTEGER(BITSET(wBCB^[1])-MASK00_15+BITSET(get_beg));
    wBCB^[0]:=INTEGER(BITSET(wBCB^[0])*MASK00_15+BITSET(get_beg<<16)+{0});
    put_inp:=put_beg;
    get_out:=get_beg;
  ei(m)
END sio_init;

PROCEDURE write(VAL str: ARRAY OF CHAR; VAR Pos,Len: INTEGER);
  VAR p,l,i,put: INTEGER; ch: CHAR; m: BITSET;
BEGIN
  IF out_stop OR (Len<=0) THEN RETURN END;
  m:=di();
  waitf(putFLAG);
    put_out:=INTEGER( BITSET(wBCB^[4])     * MASK00_15 );
    put_inp:=INTEGER( BITSET(wBCB^[3]>>16) * MASK00_15 );
  BCB^[12]:=1c;
  IF put_inp>=put_out THEN l:=put_size - 1 - (put_inp-put_out)
  ELSE                     l:=put_out - put_inp - 1
  END;
  p:=Pos;
  IF l>Len THEN l:=Len END;
  LOOP
    IF put_inp=put_end THEN put:=put_beg ELSE put:=put_inp+1 END;
    IF (put=put_out) OR out_stop OR (l=0) THEN EXIT END;
    ch:=str[p];
    IF (BITSET(ch=36c) * BITSET(NOT raw_out))#{} THEN
      IF put=put_end THEN i:=put_beg ELSE i:=put+1 END;
      IF i=put_out   THEN EXIT END;
      F0000h^[put_inp]:=15c;
      put_inp:=put; put:=i;  ch:=12c
    END;
    F0000h^[put_inp]:=ch; p:=p+1;
    put_inp:=put;         l:=l-1
  END;
  DEC(Len,p-Pos); Pos:=p;
  waitf(putFLAG);
  wBCB^[3]:=INTEGER( BITSET(wBCB^[3])-MASK16_31+BITSET(put_inp<<16)+{0} );
  ei(m)
END write;

PROCEDURE read(VAR s: ARRAY OF CHAR; VAR Pos,Len: INTEGER);
  VAR  ch: CHAR;
        m: BITSET;
  pos,len: INTEGER;
BEGIN
  pos:=Pos;     len:=Len;
  m:=di();
  waitf(getFLAG);
    get_inp:=INTEGER( BITSET(wBCB^[0]>>16) * MASK00_15 );
    get_out:=INTEGER( BITSET(wBCB^[1])     * MASK00_15 );
  BCB^[0]:=1c;

  IF get_inp=get_out THEN ei(m); RETURN END;
  LOOP
    IF len<=0  THEN EXIT END;
    ch:=F0000h^[get_out];
    IF xon=xoff THEN
      s[pos]:=ch; INC(pos); DEC(len)
    ELSE
      IF    (ch=xoff) & NOT out_stop THEN out_stop:=TRUE
      ELSIF (ch=xon)  &     out_stop THEN out_stop:=FALSE
      ELSE  s[pos]:=ch; INC(pos); DEC(len)
      END
    END;
    IF get_out=get_end THEN get_out:=get_beg ELSE INC(get_out) END;
    IF get_inp=get_out THEN EXIT END
  END;
  Pos:=pos;
  Len:=len;
  waitf(getFLAG);
  wBCB^[1]:=INTEGER( BITSET(wBCB^[1]) - MASK00_15 + BITSET(get_out) );
   BCB^[0]:=1c;
  ei(m);
END read;

----------------------------------------------------------------

TYPE string = POINTER TO ARRAY [0..0] OF CHAR;

VAR buf: ARRAY [0..255] OF CHAR;
    beg: INTEGER;
    end: INTEGER;
    odd: BOOLEAN;
  space: os.signal_rec;
  ready: os.signal_rec;
  puted: os.signal_rec;

PROCEDURE driver;
  VAR rdy,fre: INTEGER;
BEGIN
  odd:=NOT odd;
  IF odd THEN RETURN END;
  waitf(getFLAG);
    get_inp:=INTEGER( BITSET(wBCB^[0]>>16)*MASK00_15 );
    get_out:=INTEGER( BITSET(wBCB^[1])*MASK00_15 );
  BCB^[0]:=1c;
  IF get_inp>=get_out THEN rdy:=get_inp-get_out
  ELSE                     rdy:=get_size-(get_out-get_inp)
  END;
  IF (rdy=0) & (space.queue=os.null)   THEN RETURN END;
  IF (rdy>0) & (ready.queue#os.null)   THEN os.send(ready) END;
  IF (space.queue=os.null) OR out_stop THEN RETURN END;
  waitf(putFLAG);
    put_out:=INTEGER( BITSET(wBCB^[4])    *MASK00_15 );
    put_inp:=INTEGER( BITSET(wBCB^[3]>>16)*MASK00_15 );
  BCB^[12]:=1c;
  IF put_inp>=put_out THEN fre:=put_size - 1 - (put_inp-put_out)
  ELSE                     fre:=put_out - put_inp - 1
  END;
  IF fre>32 THEN os.send(space) END
END driver;

PROCEDURE _read(VAR r: req.REQUEST);
  VAR m: BITSET;
      i: INTEGER;
    pos: INTEGER;
    len: INTEGER;
    str: string;
BEGIN
  str:=r.buf;
  pos:=r.pos; len:=r.len;
  m:=di();
  LOOP
    IF len=0 THEN EXIT END;
    IF beg=end THEN
      puted.cou:=0;
      i:=os.wait_del(0,puted);
      IF i=1 THEN EXIT END
    END;
    IF beg#end THEN
      str^[pos]:=buf[beg];  INC(pos);  DEC(len);  beg:=(beg+1) MOD 256
    END
  END;
  ei(m);
  IF len#0 THEN r.res:=err.ipted_op END;
  r.len:=pos-r.pos;  r.pos:=pos
END _read;

PROCEDURE _ready(VAR r: req.REQUEST);
  VAR m: BITSET;
BEGIN
  m:=di();
    IF beg<=end THEN r.len:=end-beg ELSE r.len:=256-(beg-end) END;
  ei(m)
END _ready;

PROCEDURE _wait(VAR r: req.REQUEST);
  VAR m: BITSET; tick: INTEGER;
BEGIN
  tick:=(r.len+19) DIV 20;
  m:=di();
    IF beg#end THEN ei(m); RETURN END;
    puted.cou:=0;
    r.res:=os.wait_del(tick,puted);
  ei(m);
  IF    r.res=1 THEN r.res:=err.ipted_op
  ELSIF r.res<0 THEN r.res:=err.time_out
  ELSE  r.res:=ok
  END
END _wait;

PROCEDURE _write(VAR r: req.REQUEST);
  VAR i: INTEGER;
    pos: INTEGER;
    len: INTEGER;
    str: string;
BEGIN
  str:=r.buf;
  pos:=r.pos; len:=r.len;
  LOOP
    write(str^,pos,len);
    IF len=0 THEN EXIT END;
    i:=os.wait_del(0,space);
    IF i=1   THEN EXIT END
  END;
  IF len#0 THEN r.res:=err.ipted_op END;
  r.len:=pos-r.pos;  r.pos:=pos
END _write;

PROCEDURE _get_spec(VAR r: req.REQUEST);
BEGIN
  r.smode :=req.parNO+req.stops1;
  r.baud  :=1920;
  r.xon   :=xon;
  r.xoff  :=xoff;
  r.trtin :=NIL;
  r.trtout:=NIL;
END _get_spec;

PROCEDURE doio(VAR r: req.REQUEST);
BEGIN
  r.res:=ok;
  CASE r.op OF
  |req.NOP      :
  |req.READ     : _read (r)
  |req.WAIT     : _wait (r)
  |req.READY    : _ready(r)
  |req.WRITE    : _write(r)
  |req.POWER_OFF:
  |req.GET_SPEC : _get_spec(r)
  ELSE
    r.res:=err.inv_op
  END
END doio;

PROCEDURE stop;
  VAR m: BITSET;
BEGIN
  m:=di();
    REPEAT os.delay(10) UNTIL space.queue=os.null;
    os.remove_action(driver);
  ei(m)
END stop;

PROCEDURE get(VAR ch: CHAR);
  VAR m: BITSET;
    p,l: INTEGER;
    str: string;
BEGIN
  ch:=0c; str:=SYSTEM.ADR(ch);
  m:=di();
    LOOP
      p:=0; l:=1;
      read(str^,p,l);
      IF l=0 THEN EXIT END;
      os.wait(ready)
    END;
  ei(m)
END get;

PROCEDURE put(ch: CHAR);
  VAR i: INTEGER; m: BITSET;
BEGIN
  m:=di();
    i:=(end+1) MOD 256;
    IF i#beg THEN buf[end]:=ch; end:=i;
      IF puted.queue#os.null THEN os.send(puted) END
    END;
  ei(m)
END put;

PROCEDURE init(chan: INTEGER): INTEGER;
  VAR r: INTEGER;
BEGIN
  sio_init(chan);
  r:=os.insert_action(driver);
  RETURN r
END init;

VAR i: INTEGER;

BEGIN
  beg:=0;
  end:=0;
  odd:=TRUE;
  raw_out:=TRUE;
  xon :=21c;
  xoff:=23c;
  out_stop :=FALSE;
  MASK16_31:={16..31};
  MASK00_15:={00..15};
  F0000h:=ADDRESS( BASE + 0F0000h DIV 4 );
  os.ini_signal(space,os.break+os.sendup,0);
  os.ini_signal(ready,os.break+os.sendup,0);
  os.ini_signal(puted,os.break+os.sendup,0);
  _init(0); (* for legal pointers in globals *)
  i:=os.final(os.self(),stop);
  ASSERT(i=err.ok,i)
END SIOmBUS.
