MODULE TTqLBXT; (*$U+ Leo 22-Oct-89. (c) KRONOS *)

IMPORT       ASCII;             IMPORT       SYSTEM;
IMPORT  err: defErrors;         IMPORT  str: Strings;
IMPORT  dkb: defKeyboard;       IMPORT  env: tskEnv;
IMPORT  dtt: defTerminal;       IMPORT   fs: osFiles;
IMPORT  req: defRequest;        IMPORT  sio: SIOqBUS;

CONST ok = err.ok;
   LINES = 25;
   HBAR  = 275c;
   VBAR  = 252c;
   BARS0 = ARRAY OF CHAR { 271c, 273c, 253c };
   BARS1 = ARRAY OF CHAR { 274c, 276c, 251c };
   BARS2 = ARRAY OF CHAR { 270c, 272c, 254c };

VAR tt_name: ARRAY [0..7] OF CHAR;
    kb_name: ARRAY [0..7] OF CHAR;
    tstate: dtt.STATE;
    kstate: dkb.STATE;

PROCEDURE tt_reset;

  PROCEDURE set(VAR d: ARRAY OF CHAR; VAL s: ARRAY OF CHAR);
  BEGIN d[0]:=s[0]; d[1]:=s[1]; d[2]:=s[2] END set;

  VAR r: req.REQUEST;
    str: ARRAY [0..31] OF CHAR;
BEGIN
  sio.raw_out(FALSE);
  --       AWP on      CRS on    ISO ALL     SMOTH off
  str:=""33c"?17;3;1T" 33c"?2;1T" 33c"[0m" 33c"?17;2;1T";
  r.op :=req.WRITE;          r.res:=ok;
  r.drn:=0;                  r.pos:=0;
  r.buf:=SYSTEM.ADR(str);    r.len:=28;
  sio.doio(r);
  WITH tstate DO
    type     :=92;      hbar  :=HBAR;   set(bars[0],BARS0);
    lines    :=25;      vbar  :=VBAR;   set(bars[1],BARS1);
    columns  :=80;                      set(bars[2],BARS2);
    min_color:=-2;      back  :=-2;
    max_color:=+1;      color := 0;
    fonts    :=10;      font  := 0;
    screens  := 1;      scr   := 0;

    cursor   := 1;      awp   := 1;
    something:= 0;      raw   := 0;
    blinking := 0;      cinter:= 0;
    reverse  := 0;      smooth:= 0;
    underline:= 0;
  END
END tt_reset;

PROCEDURE labtam_vt100(no: INTEGER; VAL x: ARRAY OF SYSTEM.WORD): INTEGER;

  PROCEDURE ss(oper: INTEGER; SEQ rand: SYSTEM.WORD);
  BEGIN oper:=labtam_vt100(oper,rand) END ss;

  PROCEDURE tt_restore(VAL old: dtt.STATE);
    VAR cha: BITSET;
  BEGIN
    WITH old DO
      IF tstate.back     #back      THEN ss(dtt._background,back     ) END;
      IF tstate.color    #color     THEN ss(dtt._color     ,color    ) END;
      IF tstate.font     #font      THEN ss(dtt._font      ,font     ) END;
      IF tstate.cursor   #cursor    THEN ss(dtt._cursor    ,cursor   ) END;
      IF tstate.reverse  #reverse   THEN ss(dtt._reverse   ,reverse  ) END;
      IF tstate.underline#underline THEN ss(dtt._underline ,underline) END;
      IF tstate.awp      #awp       THEN ss(dtt._autowrap  ,awp      ) END;
      IF tstate.raw      #raw       THEN ss(dtt._raw       ,raw      ) END;
      IF tstate.cinter   #cinter    THEN ss(dtt._cinter    ,cinter   ) END;
      IF tstate.smooth   #smooth    THEN ss(dtt._smooth_scroll,smooth) END;
      (* note: something=reverse so it's last one allready restored *)
    END
  END tt_restore;

  VAR
    str: ARRAY [0..47] OF CHAR;
    len: INTEGER;
      i: INTEGER;
  i0,i1: INTEGER;

  PROCEDURE scp(dir: CHAR);
  BEGIN
    IF (i<1) OR (i>99) THEN i:=1 END;
    IF    i=1  THEN str:="" 233c "?";  len:=2
    ELSIF i<10 THEN str:="" 233c "0?"; len:=3;
      str[1]:=CHAR(i+ORD("0"))
    ELSE str:="" 233c "00?"; len:=4;
      str[1]:=CHAR(i DIV 10+ORD("0"));
      str[2]:=CHAR(i MOD 10+ORD("0"));
    END;
    str[len-1]:=dir;
  END scp;

  PROCEDURE app1(tail: CHAR);
  BEGIN
    IF (i<1) OR (i>99) THEN i:=1 END;
    IF i=1 THEN
      str[len]:=tail; INC(len); RETURN
    END;
    IF i<10 THEN
      str[len]:=CHAR(i MOD 10 + ORD("0")); INC(len)
    ELSE
      str[len]:=CHAR(i DIV 10 + ORD("0")); INC(len);
      str[len]:=CHAR(i MOD 10 + ORD("0")); INC(len)
    END;
    str[len]:=tail; INC(len)
  END app1;

  VAR r: req.REQUEST;
    adr: SYSTEM.ADDRESS;
   tptr: POINTER TO dtt.STATE;
tptrptr: POINTER TO POINTER TO dtt.STATE;

BEGIN
  i0:=-1; i1:=-1;
  IF HIGH(x)>=0 THEN i0:=x[0] END; i:=i0;
  IF HIGH(x)>=1 THEN i1:=x[1] END; len:=0;
  WITH tstate DO
    CASE no OF
      |dtt._info         : IF (i<=0) OR (i=NIL) THEN RETURN err.bad_parm END;
                           adr:=i; tptrptr:=adr; tptrptr^:=SYSTEM.ADR(tstate);
                           RETURN ok
      |dtt._reset        : tt_reset; RETURN ok
      |dtt._restore      : IF i<=0 THEN RETURN err.bad_parm END;
                           adr:=i; tptr:=adr; tt_restore(tptr^); RETURN ok
      |dtt._raw          : sio.raw_out(i<=0);
                           IF i<=0 THEN raw:=0 ELSE raw:=1 END; RETURN ok
      |dtt._up           : scp('A')
      |dtt._down         : scp('B')
      |dtt._right        : scp('C')
      |dtt._left         : scp('D')
      |dtt._home         : str:="" 233c "H";  len:=2
      |dtt._bottom       : str:="" 33c "=  "; len:=4;
                           str[2]:=CHAR( LINES-1+40b )
      |dtt._repeat       : IF i1<1  THEN RETURN ok END;
                           IF i0 MOD 128 < 40b THEN i0:=ORD("?") END;
                           IF i1<=4 THEN
                             FOR i:=0 TO i1-1 DO str[i]:=CHAR(i0) END; len:=i1
                           ELSE  str:=" " 233c;
                             str[0]:=CHAR(i0); i:=i1-1; len:=2; app1("b")
                           END
      |dtt._erase        : IF NOT (i IN {0..2}) THEN i:=0 END;
                           str:="" 233c "0J";  len:=3;
                           str[1]:=CHAR( i+ORD("0") )
      |dtt._erase_line   : IF NOT (i IN {0..2}) THEN i:=0 END;
                           str:="" 233c "0K";  len:=3;
                           str[1]:=CHAR( i+ORD("0") )
      |dtt._erase_chars  : str:="" 233c; len:=1; app1("X")
      |dtt._set_pos      : IF i0<0 THEN i0:=0
                           ELSIF i0>=LINES THEN i0:=LINES-1
                           END;
                           IF i1<0 THEN i1:=0 ELSIF i1>79 THEN i1:=79 END;
                           str:="" 33c "=  "; len:=4;
                           str[2]:=CHAR( i0+40b );
                           str[3]:=CHAR( i1+40b )
      |dtt._roll_up      : str:=""33c"=  "233c; str[2]:=CHAR(LINES-1+40b);
                           len:=5; app1("S")
      |dtt._roll_down    : str:="" 233c "H" 233c; len:=3;  app1("T")
      |dtt._scroll_up    :  str:=233c;  len:=1;  app1("S")
      |dtt._scroll_down  :  str:=233c;  len:=1;  app1("T")
      |dtt._ins_char     :  str:=233c;  len:=1;  app1("@")
      |dtt._del_char     :  str:=233c;  len:=1;  app1("P")
      |dtt._ins_line     :  str:=233c;  len:=1;  app1("L")
      |dtt._del_line     :  str:=233c;  len:=1;  app1("M")

      |dtt._cursor       :  str:="" 33c "?2;0T"; len:=6;
                            IF i#0 THEN cursor:=1 ELSE cursor:=0 END;
                            str[4]:=CHAR(cursor+ORD("0"))
      |dtt._cinter       :  str:="" 233c "3l"; len:=3; cinter:=1;
                            IF i=0 THEN str[2]:="h"; cinter:=0 END
      |dtt._reverse
      ,dtt._something    :  IF i<=0 THEN str:=""233c"27m"; len:=4;
                              reverse:=0; something:=0
                            ELSE str:=""233c "7m"; len:=3;
                              reverse:=1; something:=1
                            END
      |dtt._underline    :  IF i<=0 THEN str:=""233c"24m"; len:=4; underline:=0
                            ELSE         str:=""233c "4m"; len:=3; underline:=1
                            END
      |dtt._color        :  str:="" 233c "30m"; len:=4;
                            IF HIGH(x)<0 THEN i:=0 END;
                            IF    i=-2 THEN str[2]:="0"
                            ELSIF i=-1 THEN str[2]:="1"
                            ELSIF i=+1 THEN str[2]:="3"
                            ELSE  i:=0;     str[2]:="2"
                            END; color:=i
      |dtt._background   :  str:="" 233c "40m"; len:=4;
                            IF HIGH(x)<0 THEN i:=0 END;
                            IF    i= 0 THEN str[2]:="2"
                            ELSIF i=-1 THEN str[2]:="1"
                            ELSIF i=+1 THEN str[2]:="3"
                            ELSE  i:=-2;    str[2]:="0"
                            END; back:=i;
      |dtt._font         :  str:="" 233c "10m"; len:=4;
                            IF i<0 THEN i:=0 ELSE i:=i MOD 10 END;
                            str[2]:=CHAR(ORD("0")+i); font:=i

      |dtt._autowrap     :  IF i#0 THEN str:=""33c"?17;3;1T"; awp:=1
                            ELSE        str:=""33c"?17;3;0T"; awp:=0
                            END;  len:=9
      |dtt._smooth_scroll: IF i<=0 THEN str:=""33c"?17;2;1T"; smooth:=0
                           ELSE         str:=""33c"?17;2;0T"; smooth:=1
                           END; len:=9
      |dtt._blinking     : RETURN err.inv_op
      |dtt._screen       : RETURN err.inv_op
    ELSE
      RETURN err.inv_op
    END
  END;
  r.op :=req.WRITE;          r.res:=ok;
  r.drn:=0;                  r.pos:=0;
  r.buf:=SYSTEM.ADR(str);    r.len:=len;        sio.doio(r);
  RETURN r.res
END labtam_vt100;

PROCEDURE tt_doio(VAR r: req.REQUEST);
  VAR buf: DYNARR OF SYSTEM.WORD;
BEGIN
  r.res:=ok;
  IF r.op MOD 256 = req.CONTROL THEN
    buf^.ADR:=r.buf; buf^.HIGH:=r.len-1; r.res:=labtam_vt100(r.op DIV 256,buf)
  ELSE
    sio.doio(r)
  END
END tt_doio;

PROCEDURE break0(n: INTEGER); END break0;

PROCEDURE break(n: INTEGER);
BEGIN
  IF kstate.breakon#0 THEN kstate.ubrk(n) END
END break;

CONST
  NUM    = 1b;  _NUM    = 105c;
  CTRL   = 2b;  _CTRL   = 035c;
  lSHIFT = 3b;  _lSHIFT = 052c;
  rSHIFT = 4b;  _rSHIFT = 066c;
  ALT    = 5b;  _ALT    = 070c;
  CAPS   = 6b;  _CAPS   = 072c;
  PAUSE  = 7b;  _PAUSE  = 125c;

  C0 = ARRAY OF CHAR {
 000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
 000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,002c,000c,000c,
 000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,003c,000c,000c,000c,000c,000c,
 000c,000c,000c,000c,000c,000c,004c,000c,005c,000c,006c,000c,000c,000c,000c,000c,
 000c,000c,000c,000c,000c,001c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
 000c,000c,000c,000c,000c,007c };
  T0 = ARRAY OF CHAR {
 000c,033c,061c,062c,063c,064c,065c,066c,067c,070c,071c,060c,055c,075c,010c,011c,
 161c,167c,145c,162c,164c,171c,165c,151c,157c,160c,133c,135c,015c,000c,141c,163c,
 144c,146c,147c,150c,152c,153c,154c,073c,047c,140c,000c,134c,172c,170c,143c,166c,
 142c,156c,155c,054c,056c,057c,000c,213c,000c,040c,000c,220c,221c,222c,223c,224c,
 225c,226c,227c,230c,231c,000c,012c,206c,200c,204c,055c,203c,237c,202c,053c,207c,
 201c,205c,211c,210c,003c,000c,000c,052c };
  T1 = ARRAY OF CHAR {
 000c,033c,041c,100c,043c,044c,045c,136c,046c,052c,050c,051c,137c,053c,010c,011c,
 161c,167c,145c,162c,164c,171c,165c,151c,157c,160c,173c,175c,015c,000c,141c,163c,
 144c,146c,147c,150c,152c,153c,154c,072c,042c,176c,000c,174c,172c,170c,143c,166c,
 142c,156c,155c,074c,076c,077c };
  T2 = ARRAY OF CHAR {
 000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,337c,000c,000c,
 312c,303c,325c,313c,305c,316c,307c,333c,335c,332c,310c,000c,000c,000c,306c,331c,
 327c,301c,320c,322c,317c,314c,304c,326c,334c,000c,000c,000c,321c,336c,323c,315c,
 311c,324c,330c,302c,300c };

VAR STATE: BITSET;

PROCEDURE put(ch: CHAR);
BEGIN
  CASE ch OF
  |ASCII.SI: kstate.foreign:=0
  |ASCII.SO: kstate.foreign:=1
  |003c    : break(0)
  |025c    : break(1); ch:=25c (* ^U       *)
  |030c    : break(2); ch:=30c (* ^X       *)
  ELSE
  END;
  sio.put(ch)
END put;

PROCEDURE ShiftEscScrollLock;
  VAR r: req.REQUEST;
      s: ARRAY [0..63] OF CHAR;
     ch: CHAR;
BEGIN
  s:="" 33c "?46;0T" 233c "H" 233c "2JESC - exit";
  r.op :=req.WRITE;          r.res:=ok;
  r.drn:=0;                  r.pos:= 0;
  r.buf:=SYSTEM.ADR(s);      r.len:=str.len(s);
  sio.doio(r);
  REPEAT ch:=sio.get() UNTIL (ch=33c);
  s:="" 33c "?46;1T" 233c "23;0H";
  r.op :=req.WRITE;          r.res:=ok;
  r.drn:=0;                  r.pos:= 0;
  r.buf:=SYSTEM.ADR(s);      r.len:=str.len(s);
  sio.doio(r);
  STATE:={};
END ShiftEscScrollLock;

PROCEDURE translate;
  VAR ch0: CHAR;           ch: CHAR;
      ch1: CHAR;          hig: BOOLEAN;
      ch2: CHAR;         caps: BOOLEAN;
      och: INTEGER;     shift: BOOLEAN;
BEGIN
  ch:=sio.get();
  IF kstate.raw#0 THEN sio.put(ch); RETURN END;
  hig:=(ch>177c);
  och:=ORD(ch) MOD 128;
  IF (och<=HIGH(C0)) & (C0[och]#0c) THEN
    och:=ORD(C0[och]);
    IF hig THEN EXCL(STATE,och) ELSE INCL(STATE,och) END;
    CASE och OF
    |NUM  : IF hig THEN RETURN END;
            kstate.foreign:=1-kstate.foreign;
            IF kstate.foreign#0 THEN sio.put(ASCII.SO)
            ELSE                     sio.put(ASCII.SI)
            END
    |CAPS : IF hig THEN RETURN END;
            kstate.caps:=1-kstate.caps
--    |PAUSE: sio.out_stop:=NOT hig
    ELSE
    END;
    RETURN
  END;
  IF hig THEN RETURN END;

  IF (och<=HIGH(T0)) & (T0[och]#0c) THEN ch0:=T0[och] ELSE ch0:=0c END;
  IF (och<=HIGH(T1)) & (T1[och]#0c) THEN ch1:=T1[och] ELSE ch1:=0c END;
  IF (och<=HIGH(T2)) & (T2[och]#0c) THEN ch2:=T2[och] ELSE ch2:=0c END;
  shift:=STATE*{lSHIFT,rSHIFT}#{};
  caps :=kstate.caps#0;
  ch   :=0c;
  IF shift & (ch0=33c) THEN ShiftEscScrollLock; RETURN END;
  IF kstate.foreign#0 THEN
    IF (STATE*{ALT,CTRL}={}) & (ch2#0c) THEN ch:=ch2
    ELSIF shift & (ch1#0c)              THEN ch:=ch1
    ELSE                                     ch:=ch0
    END
  ELSE
    ch:=ch0;
    IF    (ch=dkb.tab) & shift            THEN put(dkb.bcktab); RETURN
    ELSIF (ch=dkb.del) & (STATE*{ALT}#{}) THEN put(30c);        RETURN
    END;
    IF shift & (ch1#0c) THEN ch:=ch1 END;
    IF STATE*{ALT}#{}   THEN ch:=CHAR(BITSET(ch)/{7}) END
  END;
  IF STATE*{CTRL}#{} THEN
    IF    (ch>=100c) & (ch<=137c) THEN DEC(ch,100b)
    ELSIF (ch>=140c) & (ch<=177c) THEN DEC(ch,140b)
    ELSIF (ch>=300c) & (ch<=337c) THEN DEC(ch,100b)
    ELSIF (ch>=340c) & (ch<=377c) THEN DEC(ch,140b)
    END
  END;
  IF shift#caps THEN ch:=ASCII.CAPITAL(ch) END;
  put(ch)
END translate;

PROCEDURE kb_reset;
  VAR r: req.REQUEST;
    str: ARRAY [0..31] OF CHAR;
BEGIN
  str:="" 33c"?41;4545T" 33c"?42;2T" 33c "?46;1T";
  r.op :=req.WRITE;          r.res:=ok;
  r.drn:=0;                  r.pos:= 0;
  r.buf:=SYSTEM.ADR(str);    r.len:=24;
  sio.doio(r);

  WITH kstate DO (* NOTE: ubrk not changed *)
    type:=2;       fkeys:=10;  breakon:=1;
    freq:=4545;    dur  :=40;  (* 40 msec = 0.04 sec (25Hz) = 2*20msec *)

    foreign:=0;    raw    :=0;
    autorep:=0;    scan   :=1;
    shift  :=0;    caps   :=0;
  END
END kb_reset;

PROCEDURE labtam_xt(no : INTEGER; VAL x: ARRAY OF SYSTEM.WORD): INTEGER;

  PROCEDURE ss(oper: INTEGER; SEQ rand: SYSTEM.WORD);
  BEGIN oper:=labtam_xt(oper,rand) END ss;

  PROCEDURE kb_restore(VAL old: dkb.STATE);
    VAR cha: BITSET;
  BEGIN
    WITH old DO
      IF (kstate.freq#freq) OR (kstate.dur#dur) THEN
        ss(dkb._bell_fd,freq,dur)
      END;
      IF kstate.ubrk   #ubrk    THEN ss(dkb._ubreak, ubrk   ) END;
      IF kstate.breakon#breakon THEN ss(dkb._break  ,breakon) END;
      IF kstate.foreign#foreign THEN ss(dkb._foreign,foreign) END;
      IF kstate.caps   #caps    THEN ss(dkb._caps   ,caps   ) END;
      IF kstate.shift  #shift   THEN ss(dkb._shift  ,shift  ) END;
      IF kstate.raw    #raw     THEN ss(dkb._raw    ,raw    ) END;
    END
  END kb_restore;

  VAR i: INTEGER;
    str: ARRAY [0..23] OF CHAR;
    adr: SYSTEM.ADDRESS;
   kptr: POINTER TO dkb.STATE;
kptrptr: POINTER TO POINTER TO dkb.STATE;
  i0,i1: INTEGER;
      r: req.REQUEST;

  PROCEDURE ws(VAL s: ARRAY OF CHAR; len: INTEGER);
  BEGIN
    r.op :=req.WRITE;          r.res:=ok;
    r.drn:=0;                  r.pos:=0;
    r.buf:=SYSTEM.ADR(str);    r.len:=len;    sio.doio(r)
  END ws;

BEGIN
  i0:=-1; i1:=-1;
  IF HIGH(x)>=0 THEN i0:=x[0] END; i:=i0;
  IF HIGH(x)>=1 THEN i1:=x[1] END;
  WITH kstate DO
    CASE no OF
    |dkb._info   : IF (i<=0) OR (i=NIL) THEN RETURN err.bad_parm END;
                   adr:=i; kptrptr:=adr; kptrptr^:=SYSTEM.ADR(kstate);
                   RETURN ok
    |dkb._reset  : kb_reset; RETURN ok
    |dkb._restore: IF i0<=0 THEN RETURN err.bad_parm END;
                   adr:=i0; kptr:=adr; kb_restore(kptr^)
    |dkb._raw    : IF i<=0 THEN raw:=0     ELSE raw:=1     END
    |dkb._break  : IF i#0  THEN breakon:=1 ELSE breakon:=0 END
    |dkb._ubreak : IF i>0  THEN ubrk:=dkb.BREAK(i) END;
    |dkb._bell_fd: IF (i0<=0) OR (i1<=0) THEN RETURN err.bad_parm END;
                   freq:=i0; dur:=i1;
                   str:="" 33c "?41;0000T" 33c "?42;00T";
                   str[8]:=CHAR(ORD("0")+i0 MOD 10);  i0:=i0 DIV 10;
                   str[7]:=CHAR(ORD("0")+i0 MOD 10);  i0:=i0 DIV 10;
                   str[6]:=CHAR(ORD("0")+i0 MOD 10);  i0:=i0 DIV 10;
                   str[5]:=CHAR(ORD("0")+i0 MOD 10);
                   i1:=(i1+19) DIV 20;
                   IF i1>99 THEN i1:=99 END;
                   str[16]:=CHAR(ORD("0")+i1 MOD 10);  i1:=i1 DIV 10;
                   str[15]:=CHAR(ORD("0")+i1 MOD 10);
                   ws(str,17);
    |dkb._bell   : IF (i0<=0) THEN i0:=1 END;
                   str:=7c;
                   FOR i:=0 TO i0-1 DO ws(str,1) END;
    |dkb._foreign: IF i0<=0 THEN foreign:=0 ELSE foreign:=1 END
    |dkb._shift  : IF i0<=0 THEN shift  :=0 ELSE shift  :=1 END
    |dkb._caps   : IF i0<=0 THEN caps   :=0 ELSE caps   :=1 END
    ELSE
      RETURN err.inv_op
    END
  END;
  RETURN ok
END labtam_xt;

PROCEDURE kb_doio(VAR r: req.REQUEST);
  VAR i: INTEGER;
    str: DYNARR OF CHAR;
    buf: DYNARR OF SYSTEM.WORD;
BEGIN
  r.res:=ok;
  IF r.op MOD 256 = req.CONTROL THEN
    buf^.ADR:=r.buf; buf^.HIGH:=r.len-1; r.res:=labtam_xt(r.op DIV 256,buf)
  ELSE
    sio.doio(r)
  END
END kb_doio;

PROCEDURE init(chan: INTEGER): INTEGER;
  VAR r: INTEGER;
      s: ARRAY [0..31] OF CHAR;
   name: ARRAY [0.. 7] OF CHAR;
BEGIN
  CASE chan OF
    |0: r:=sio.init(177560b,60b);
    |1: r:=sio.init(176500b,300b);
    |2: r:=sio.init(177540b,340b);
  ELSE r:=err.no_entry;
  END;
  IF r#ok THEN RETURN r END;
  sio.x_inp(FALSE);

  tt_reset;
  tt_name:="tty0"; tt_name[3]:=CHAR(ORD("0")+chan);
  r:=fs.define_driver(tt_name,"",0,fs.tty,tt_doio);
  IF r#ok THEN RETURN r END;
  name:="SYSLOG0"; name[6]:=CHAR(ORD("0")+chan);
  r:=fs.define_driver(name,tt_name,0,fs.tty,tt_doio);
  IF r#ok THEN RETURN r END;


  STATE:={};
  kstate.ubrk :=break0;
  kb_reset;
  kb_name:="key0"; kb_name[3]:=CHAR(ORD("0")+chan);
  r:=fs.define_driver(kb_name,"",0,fs.tty,kb_doio);
  s:='';
  str.app(s,tt_name); str.app(s,' '); str.app(s,kb_name);
  env.put_str(env.info,s,TRUE);
  RETURN r
END init;

PROCEDURE parms(VAR channel: INTEGER);
  VAR i: INTEGER;
     ps: STRING;
   done: BOOLEAN;
BEGIN
  channel:=0;
  env.get_str(env.args,ps);
  IF NOT env.done THEN RETURN END;
  i:=0;
  str.skip(ps,i,' ');
  str.iscan(channel,ps,i,done);
  IF NOT done THEN channel:=0; RETURN END;
END parms;

VAR r,chan: INTEGER;

BEGIN
  parms(chan);
  r:=init(chan);
  IF r#ok THEN HALT(r) END;
  env.become_ipr;
  sio.monitor(translate)
END TTqLBXT.
