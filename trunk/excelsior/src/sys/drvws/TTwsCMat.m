MODULE TTwsCMat; (*$U+  Leo 28-Feb-91. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  err: defErrors;         IMPORT  low: lowLevel;
IMPORT  dkb: defKeyboard;       IMPORT   os: osKernel;
IMPORT  dtt: defTerminal;       IMPORT   fs: osFiles;
IMPORT  req: defRequest;        IMPORT  env: tskEnv;
IMPORT  SCR: KTVcm;             IMPORT  SIO: SIOws;
IMPORT  str: Strings;

TYPE ADDRESS = SYSTEM.ADDRESS;

CONST ok = err.ok;
   HBAR  = 224c;
   VBAR  = 203c;
   BARS0 = ARRAY OF CHAR { 252c, 222c, 217c };
   BARS1 = ARRAY OF CHAR { 223c, 225c, 204c };
   BARS2 = ARRAY OF CHAR { 220c, 221c, 251c };

VAR tstate: dtt.STATE;
    kstate: dkb.STATE;
    kname,tname : ARRAY [0..7] OF CHAR;

    STOP  : BOOLEAN;
    RUN   : os.signal_rec;

PROCEDURE tt_reset;
  PROCEDURE set(VAR d: ARRAY OF CHAR; VAL s: ARRAY OF CHAR);
  BEGIN d[0]:=s[0]; d[1]:=s[1]; d[2]:=s[2] END set;
BEGIN
  low.zero(tstate);
  WITH tstate DO
    SCR.RESET;
    type     :=481;       hbar :=HBAR;           set(bars[0],BARS0);
    lines    :=SCR.lines; vbar :=VBAR;           set(bars[1],BARS1);
    columns  :=SCR.columns;                      set(bars[2],BARS2);
    min_color:=SCR.gmin;
    max_color:=SCR.gmax;
    SCR.set_reverse(0); SCR.set_color(0);
    color:=0; back:=SCR.gmin;
    fonts    := 1;
    screens  := 1;
    cursor:=1;
    awp:=1;
  END
END tt_reset;

PROCEDURE ttcontrol(no: INTEGER; VAL x: ARRAY OF SYSTEM.WORD): INTEGER;

  PROCEDURE ss(oper: INTEGER; SEQ rand: SYSTEM.WORD);
  BEGIN oper:=ttcontrol(oper,rand) END ss;

  PROCEDURE tt_restore(VAL o: dtt.STATE): INTEGER;
    PROCEDURE set(op,a0,a1: INTEGER);
    BEGIN IF a0#a1 THEN ss(op,a1) END END set;
  BEGIN
    WITH tstate DO
      set(dtt._reverse,      reverse,  o.reverse)  ;
      set(dtt._color,        color,    o.color)    ;
      set(dtt._font,         font,     o.font)     ;
      set(dtt._cursor,       cursor,   o.cursor)   ;
      set(dtt._underline,    underline,o.underline);
      set(dtt._autowrap,     awp,      o.awp)      ;
      set(dtt._raw,          raw,      o.raw)      ;
      set(dtt._smooth_scroll,smooth,   o.smooth)   ;
      set(dtt._blinking,     blinking, o.blinking) ;
      set(dtt._cinter,       cinter,   o.cinter)   ;
    END;
    RETURN err.ok
  END tt_restore;

  PROCEDURE Repeat(ch: CHAR; no: INTEGER): INTEGER;
    VAR len,i: INTEGER;
          str: ARRAY [0..79] OF CHAR;
  BEGIN
    len:=BYTES(str); IF len>=no THEN len:=no END;
    FOR i:=0 TO len-1 DO str[i]:=ch END;
    LOOP
      len:=BYTES(str);
      IF len>=no THEN len:=no END;
      SCR.write(str,0,len);
      DEC(no,len); IF no=0 THEN EXIT END
    END;
    RETURN err.ok
  END Repeat;

  VAR  i: INTEGER;
     adr: SYSTEM.ADDRESS;
    tptr: POINTER TO dtt.STATE;
   i0,i1: INTEGER;
 tptrptr: POINTER TO POINTER TO dtt.STATE;

BEGIN
  i0:=-1; i1:=-1;
  IF HIGH(x)>=0 THEN i0:=x[0] END; i:=i0;
  IF HIGH(x)>=1 THEN i1:=x[1] END;
  WITH tstate DO
    CASE no OF
      |dtt._info   : IF i<=0 THEN RETURN err.bad_parm END;
                     adr:=i; tptrptr:=adr; tptrptr^:=SYSTEM.ADR(tstate);
                     RETURN ok
      |dtt._reset  : tt_reset; RETURN ok
      |dtt._restore: IF i<=0 THEN RETURN err.bad_parm END;
                     adr:=i; tptr:=adr; RETURN tt_restore(tptr^)
      |dtt._raw    : IF i<=0 THEN raw:=0 ELSE raw:=1 END
      |dtt._up     : SCR.set_lc(SCR.ln-i,SCR.cl)
      |dtt._down   : SCR.set_lc(SCR.ln+i,SCR.cl)
      |dtt._right  : SCR.set_lc(SCR.ln,SCR.cl+i)
      |dtt._left   : SCR.set_lc(SCR.ln,SCR.cl-i)
      |dtt._home   : SCR.set_lc(0,0)
      |dtt._bottom : SCR.set_lc(SCR.lines-1,0)
      |dtt._repeat : RETURN Repeat(CHAR(i0),i1)
      |dtt._erase      : SCR.erase(i)
      |dtt._erase_line : SCR.eraln(i)
      |dtt._erase_chars: SCR.erach(i)
      |dtt._set_pos    : SCR.set_lc(i0,i1)
      |dtt._roll_up    : SCR.set_lc(SCR.lines-1,0); SCR.scrollup(i)
      |dtt._roll_down  : SCR.set_lc(0,0);           SCR.scrolldw(i)
      |dtt._scroll_up  : SCR.scrollup(i)
      |dtt._scroll_down: SCR.scrolldw(i)
      |dtt._ins_char   : SCR.insch(i)
      |dtt._del_char   : SCR.delch(i)
      |dtt._ins_line   : SCR.insln(i)
      |dtt._del_line   : SCR.delln(i)
      |dtt._cursor     :
                         IF i<=0 THEN i:=0 ELSE i:=1 END;
                         IF cursor=i THEN RETURN ok END;
                         SCR.toggle_carret;
                         IF SCR.car THEN cursor:=1 ELSE cursor:=0 END;
      |dtt._reverse
      ,dtt._something  : IF i<=0 THEN i:=0 ELSE i:=1 END;
                         IF i=reverse THEN RETURN ok END;
                         SCR.set_reverse(i);
                         IF SCR.reverse THEN reverse:=1 ELSE reverse:=0 END
      |dtt._underline  : IF i<=0 THEN i:=0 ELSE i:=1 END;
                         SCR.undl:=(i=1);
                         underline:=i
      |dtt._color      : IF i=color THEN RETURN ok END;
                         SCR.set_color(i); color:=i
      |dtt._background : RETURN err.inv_op
      |dtt._font       : RETURN err.inv_op
      |dtt._autowrap   : IF i<=0 THEN i:=0 ELSE i:=1 END;
                         SCR.awp:=(i=1);
                         awp:=i
      |dtt._blinking   : RETURN err.inv_op
      |dtt._screen     : RETURN err.inv_op
      |dtt._smooth_scroll: RETURN err.inv_op
    ELSE
      RETURN err.inv_op
    END
  END;
  RETURN ok
END ttcontrol;

PROCEDURE tt_doio(VAR r: req.REQUEST);
  VAR str: STRING;
      buf: DYNARR OF SYSTEM.WORD;
BEGIN
  r.res:=ok;
  CASE r.op MOD 256 OF
    |req.CONTROL  : buf^.ADR:=r.buf; buf^.HIGH:=r.len-1;
                    r.res:=ttcontrol(r.op DIV 256,buf)
    |req.POWER_OFF: SCR.final
    |req.GET_SPEC: r.baud:=0; r.xon:=0c; r.xoff:=0c; r.smode:={};
    |req.WRITE    : IF STOP THEN r.res:=os.wait_del(-1,RUN) END;
                    IF r.res=ok THEN
                      str^.ADR:=r.buf; str^.HIGH:=r.pos+r.len-1;
                      SCR.write(str,r.pos,r.len); INC(r.pos,r.len)
                    ELSE
                      r.res:=err.ipted_op
                    END
  ELSE
    r.res:=err.inv_op
  END
END tt_doio;

PROCEDURE break0(n: INTEGER); END break0;

PROCEDURE break(n: INTEGER);
BEGIN
  IF kstate.breakon#0 THEN kstate.ubrk(n) END
END break;

----------------------------------------------------------------

CONST
  RELEASE  = 360c;
  lSHIFT   = 022c;   _lshift   = 1;
  rSHIFT   = 131c;   _rshift   = 2;
  ALT      = 021c;   _alt      = 3;
  CAPS     = 130c;   _caps     = 4;
  NUMS     = 167c;   _nums     = 5;
  SCROLL   = 176c;   _scroll   = 6;
  CNTRL    = 024c;   _cntrl    = 7;

  CSI = ARRAY OF CHAR  (*  CHANGE  STATE INTRODUCERS  *)
  { 000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,000c,003c,001c,000c,007c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,000c,000c,004c,002c,000c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,000c,000c,000c,000c,000c,005c,000c,000c,000c,000c,000c,000c,
    006c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,377c
  };

  NUM = ARRAY OF CHAR  (*  NUMLOCK state  *)
  { 000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,000c,000c,000c,000c,000c,061c,000c,064c,067c,000c,000c,000c,
    060c,056c,062c,065c,066c,070c,000c,000c,000c,000c,063c,000c,000c,071c
  };

  LT0 = ARRAY OF CHAR  (*  LATIN   state NORMAL  *)
  { 000c,230c,000c,224c,222c,220c,221c,000c,000c,231c,227c,225c,223c,011c,
    140c,000c,000c,000c,000c,000c,000c,161c,061c,000c,000c,000c,172c,163c,
    141c,167c,062c,000c,000c,143c,170c,144c,145c,064c,063c,000c,000c,040c,
    166c,146c,164c,162c,065c,000c,000c,156c,142c,150c,147c,171c,066c,000c,
    000c,000c,155c,152c,165c,067c,070c,000c,000c,054c,153c,151c,157c,060c,
    071c,000c,000c,056c,057c,154c,073c,160c,055c,000c,000c,000c,047c,000c,
    133c,075c,000c,000c,000c,000c,015c,135c,000c,134c,000c,000c,000c,000c,
    000c,000c,000c,000c,010c,000c,000c,207c,000c,203c,206c,000c,000c,000c,
    211c,210c,201c,237c,202c,200c,033c,000c,000c,053c,205c,055c,052c,204c,
    000c,000c,000c,000c,000c,226c,012c
  };

  LT1 = ARRAY OF CHAR  (*  LATIN   state SHIFT  *)
  { 000c,230c,000c,236c,234c,232c,233c,000c,000c,231c,227c,225c,235c,212c,
    176c,000c,000c,000c,000c,000c,000c,121c,041c,000c,000c,000c,132c,123c,
    101c,127c,100c,000c,000c,103c,130c,104c,105c,044c,043c,000c,000c,040c,
    126c,106c,124c,122c,045c,000c,000c,116c,102c,110c,107c,131c,136c,000c,
    000c,000c,115c,112c,125c,046c,052c,000c,000c,074c,113c,111c,117c,051c,
    050c,000c,000c,076c,077c,114c,072c,120c,137c,000c,000c,000c,042c,000c,
    173c,053c,000c,000c,000c,000c,015c,175c,000c,174c,000c,000c,000c,000c,
    000c,000c,000c,000c,010c,000c,000c,061c,000c,064c,067c,000c,000c,000c,
    060c,056c,062c,065c,066c,070c,033c,000c,000c,053c,063c,055c,052c,071c,
    000c,000c,000c,000c,000c,226c,012c
  };

  NT0 = ARRAY OF CHAR  (*  NATIO   state NORMAL  *)
  { 000c,230c,000c,224c,222c,220c,221c,000c,000c,231c,227c,225c,223c,011c,
    140c,000c,000c,000c,000c,000c,000c,312c,061c,000c,000c,000c,321c,331c,
    306c,303c,062c,000c,000c,323c,336c,327c,325c,064c,063c,000c,000c,040c,
    315c,301c,305c,313c,065c,000c,000c,324c,311c,322c,320c,316c,066c,000c,
    000c,000c,330c,317c,307c,067c,070c,000c,000c,302c,314c,333c,335c,060c,
    071c,000c,000c,300c,057c,304c,326c,332c,055c,000c,000c,000c,334c,000c,
    310c,075c,000c,000c,000c,000c,015c,135c,000c,337c,000c,000c,000c,000c,
    000c,000c,000c,000c,010c,000c,000c,207c,000c,203c,206c,000c,000c,000c,
    211c,210c,201c,237c,202c,200c,033c,000c,000c,053c,205c,055c,052c,204c,
    000c,000c,000c,000c,000c,226c,012c
  };

  NT1 = ARRAY OF CHAR  (*  NATIO   state SHIFT  *)
  { 000c,230c,000c,236c,234c,232c,233c,000c,000c,231c,227c,225c,235c,212c,
    176c,000c,000c,000c,000c,000c,000c,352c,041c,000c,000c,000c,361c,371c,
    346c,343c,100c,000c,000c,363c,376c,367c,365c,044c,043c,000c,000c,040c,
    355c,341c,345c,353c,045c,000c,000c,364c,351c,362c,360c,356c,136c,000c,
    000c,000c,370c,357c,347c,046c,052c,000c,000c,342c,354c,373c,375c,051c,
    050c,000c,000c,340c,077c,344c,366c,372c,137c,000c,000c,000c,374c,000c,
    350c,053c,000c,000c,000c,000c,015c,175c,000c,377c,000c,000c,000c,000c,
    000c,000c,000c,000c,010c,000c,000c,061c,000c,064c,067c,000c,000c,000c,
    060c,056c,062c,065c,066c,070c,033c,000c,000c,053c,063c,055c,052c,071c,
    000c,000c,000c,000c,000c,226c,012c
  };

----------------------------------------------------------------

VAR   TOGS: BITSET;
       TRT: POINTER TO ARRAY [0..255] OF CHAR;
  HIGH_TRT: INTEGER;
   NUMLOCK: BOOLEAN;

PROCEDURE set_trt(VAL str: ARRAY OF CHAR);
  PROCEDURE high (VAL str: ARRAY OF CHAR): INTEGER; CODE END high;
  PROCEDURE adr  (): SYSTEM.ADDRESS;                CODE END adr;
BEGIN
  HIGH_TRT:=high(str); TRT:=adr()
END set_trt;

PROCEDURE change_trt;
BEGIN
  IF kstate.foreign#0 THEN
    IF    TOGS*{_rshift,_lshift,_alt}={_rshift,_alt} THEN set_trt(LT1)
    ELSIF TOGS*{_rshift,_lshift,_alt}={_lshift,_alt} THEN set_trt(LT1)
    ELSIF TOGS*{_alt}           #{} THEN                  set_trt(LT0)
    ELSIF TOGS*{_rshift,_lshift}#{} THEN                  set_trt(NT1)
    ELSE                                                  set_trt(NT0)
    END
  ELSE
    IF    TOGS*{_rshift,_lshift,_alt}={_rshift,_alt} THEN set_trt(NT1)
    ELSIF TOGS*{_rshift,_lshift,_alt}={_lshift,_alt} THEN set_trt(NT1)
    ELSIF TOGS*{_alt}           #{} THEN                  set_trt(NT0)
    ELSIF TOGS*{_rshift,_lshift}#{} THEN                  set_trt(LT1)
    ELSE                                                  set_trt(LT0)
    END
  END
END change_trt;

TYPE char = BITSET;

PROCEDURE unstop;
BEGIN
  IF STOP THEN os.send(RUN); STOP:=FALSE END
END unstop;

PROCEDURE press(ch: CHAR);
  VAR c: INTEGER;
BEGIN
  c:=ORD(CSI[ORD(ch)]);
  IF (c>=32) OR (c IN TOGS) THEN RETURN END;
  WITH kstate DO
    CASE c OF
      |_scroll:
        IF   STOP THEN unstop
        ELSE STOP:=TRUE
        END;
        RETURN
      |_nums  : NUMLOCK:=NOT NUMLOCK
      |_caps  : caps:=1-caps
      |_rshift: IF TOGS*{_lshift}#{} THEN foreign:=1; SIO.put(char(16c)) END
      |_lshift: IF TOGS*{_rshift}#{} THEN foreign:=0; SIO.put(char(17c)) END
    ELSE
    END
  END;
  INCL(TOGS,c); change_trt
END press;

PROCEDURE lost;
BEGIN
  TOGS:={}; kstate.caps:=0; kstate.foreign:=0; set_trt(LT0);
  unstop
END lost;

PROCEDURE release(key: char);
  VAR c: INTEGER;
     ch: CHAR;
BEGIN
  IF key-{0..7}#{} THEN lost; RETURN END;
  ch:=CHAR(key);
  IF ORD(ch)>HIGH(CSI) THEN RETURN END;
  c:=ORD(CSI[ORD(ch)]);
  IF (c=0) OR (c>=32) OR NOT (c IN TOGS) THEN RETURN END;
  EXCL(TOGS,c);
  change_trt
END release;

PROCEDURE translate_keyboard;
  VAR c: INTEGER;
     ch: CHAR;
    key: char;
BEGIN
  key:=SIO.get();
  IF kstate.raw#0  THEN SIO.put(key); RETURN END;
  IF key-{0..7}={} THEN ch:=CHAR(key*{0..7});
  ELSE   lost;          ch:=RELEASE
  END;
  c:=ORD(ch);
  IF (c<=HIGH(CSI)) & (CSI[c]#0c) THEN
    IF ch=RELEASE THEN release(SIO.get()) ELSE press(ch) END; RETURN
  END;
  IF c>HIGH_TRT THEN RETURN END;
  ch:=TRT^[c];
  IF ch=dkb.del THEN
    IF TOGS*{_cntrl}#{} THEN break(1); SIO.put(char(25c)); RETURN END;
    IF TOGS*{_alt}#{}   THEN break(2); SIO.put(char(30c)); RETURN END;
  END;
  IF NUMLOCK & (c<=HIGH(NUM)) & (NUM[c]#0c) THEN
    SIO.put(char(NUM[c])); RETURN
  END;
  IF (TOGS*{_cntrl}#{}) & (c<=HIGH(LT0)) & (100c<=LT0[c]) & (LT0[c]<=177c) THEN
    ch:=CHAR( ORD(LT0[c]) MOD 32 );
    CASE ch OF
    |03c: break(0)
    |21c: IF STOP THEN unstop; RETURN END;
    |23c: IF NOT STOP THEN STOP:=TRUE; RETURN END;
    |25c: break(1)
    ELSE
    END;
    SIO.put(char(ch)); RETURN
  END;
  IF kstate.caps#0 THEN
    IF ("a"<=ch)  & (ch<="z")  THEN DEC(ch,40b); SIO.put(char(ch)); RETURN END;
    IF (300c<=ch) & (ch<=337c) THEN INC(ch,40b); SIO.put(char(ch)); RETURN END
  END;
  SIO.put(char(ch))
END translate_keyboard;

PROCEDURE kb_init_state;
BEGIN
  STOP:=FALSE;
  os.ini_signal(RUN,os.break,0);
  TOGS:={};
  set_trt(LT0);
  NUMLOCK:=FALSE;
END kb_init_state;

PROCEDURE kb_reset;
BEGIN
  WITH kstate DO (* NOTE: ubrk not changed *)
    type:=2;     fkeys:=10;   breakon:=1;
    foreign:=0;  caps:=0;     shift:=0;    raw:=0
  END
END kb_reset;

PROCEDURE kbcontrol(no : INTEGER; VAL x: ARRAY OF SYSTEM.WORD): INTEGER;

  VAR errr: INTEGER;

  PROCEDURE ss(oper: INTEGER; SEQ rand: SYSTEM.WORD);
  BEGIN errr:=kbcontrol(oper,rand) END ss;

  PROCEDURE kb_restore(VAL o: dkb.STATE): INTEGER;

    PROCEDURE set(op,a0,a1: INTEGER): BOOLEAN;
    BEGIN
      IF a0#a1 THEN
        ss(op,a1);
        IF errr#ok THEN RETURN TRUE END;
      END;
      RETURN FALSE
    END set;

  BEGIN
    WITH kstate DO
      IF ubrk#o.ubrk        THEN ss(dkb._ubreak,o.ubrk) END;
      IF set(dkb._break,  breakon,o.breakon) THEN RETURN errr END;
      IF set(dkb._foreign,foreign,o.foreign) THEN RETURN errr END;
      IF set(dkb._caps,   caps   ,o.caps   ) THEN RETURN errr END;
      IF set(dkb._shift,  shift  ,o.shift  ) THEN RETURN errr END;
      IF set(dkb._raw,    raw    ,o.raw    ) THEN RETURN errr END
    END;
    RETURN err.ok
  END kb_restore;

  VAR i: INTEGER;
    adr: SYSTEM.ADDRESS;
   kptr: POINTER TO dkb.STATE;
kptrptr: POINTER TO POINTER TO dkb.STATE;
  i0,i1: INTEGER;

BEGIN
  i0:=-1; i1:=-1;
  IF HIGH(x)>=0 THEN i0:=x[0] END; i:=i0;
  IF HIGH(x)>=1 THEN i1:=x[1] END;
  WITH kstate DO
    CASE no OF
    |dkb._reset  : kb_reset
    |dkb._info   : IF i0<=0 THEN RETURN err.bad_parm END;
                   kptrptr:=SYSTEM.ADDRESS(i0); kptrptr^:=SYSTEM.ADR(kstate)
    |dkb._restore: IF i0<=0 THEN RETURN err.bad_parm END;
                   kptr:=SYSTEM.ADDRESS(i0); RETURN kb_restore(kptr^)
    |dkb._raw    : IF i<=0 THEN raw:=0 ELSE raw:=1 END
    |dkb._break  : IF i#0  THEN breakon:=1 ELSE breakon:=0 END
    |dkb._ubreak : IF i>0  THEN ubrk:=dkb.BREAK(i) END;
    |dkb._bell_fd:
    |dkb._bell   :
    |dkb._foreign: IF i0<=0 THEN foreign:=0 ELSE foreign:=1 END
    |dkb._shift  : IF i0<=0 THEN shift:=0   ELSE shift:=1   END
    |dkb._caps   : IF i0<=0 THEN caps:=0    ELSE caps:=1    END
    ELSE
      RETURN err.inv_op
    END
  END;
  RETURN ok
END kbcontrol;

PROCEDURE kb_doio(VAR r: req.REQUEST);
  VAR buf: DYNARR OF SYSTEM.WORD;
BEGIN
  r.res:=ok;
  CASE r.op MOD 256 OF
    |req.CONTROL : buf^.ADR:=r.buf; buf^.HIGH:=r.len-1;
                   r.res:=kbcontrol(r.op DIV 256,buf)
    |req.GET_SPEC: r.baud:=0; r.xon:=0c; r.xoff:=0c; r.smode:={}
    |req.NOP     :
  ELSE
    SIO.doio(r)
  END
END kb_doio;

PROCEDURE halt;
BEGIN IF fs.remove_driver(tname)=fs.remove_driver(kname) THEN END
END halt;

--------------------------  DRIVER  ----------------------------
                          ----------
PROCEDURE init(n: CHAR);
  VAR r: INTEGER;
      i: ARRAY [0..79] OF CHAR;
   name: ARRAY [0..7] OF CHAR;
BEGIN
  kb_init_state;
  r:=SIO.init(3,SIO.KEYBOARD); ASSERT(r=ok,r);
  kname:='key0'; tname:='tty0';
  kname[3]:=n  ; tname[3]:=n;
  env.final(halt);
  kstate.ubrk:=break0;
  kb_reset; tt_reset;
  r:=fs.define_driver(tname,'',0,fs.tty,tt_doio);
  IF r#ok THEN HALT(r) END;
  r:=fs.define_driver(kname,'',0,fs.tty,kb_doio);
  IF r#ok THEN HALT(r) END;
  name:="SYSLOG0"; name[6]:=n;
  r:=fs.define_driver(name,tname,0,fs.tty,tt_doio);
  IF r#ok THEN HALT(r) END;
  i:=tname; str.app(i,' '); str.app(i,kname);
  env.put_str(env.info,i,TRUE);
END init;

VAR r: INTEGER;
    n: INTEGER;
    s: STRING;   done: BOOLEAN;

BEGIN
  env.get_str(env.args,s);
  IF env.done THEN r:=0; str.iscan(n,s,r,done); IF NOT done THEN n:=0 END
  ELSE n:=0 END;
  init(CHAR(ORD("0")+n));
  env.become_ipr;
  SIO.monitor(translate_keyboard)
END TTwsCMat.
