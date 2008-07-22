MODULE TTwsCMlrm; (*$U+  ??? ??-???-??. (c) KRONOS *)
                  (*     Leg 09-Dec-90. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  err: defErrors;         IMPORT  low: lowLevel;
IMPORT  dkb: defKeyboard;       IMPORT   os: osKernel;
IMPORT  dtt: defTerminal;       IMPORT   fs: osFiles;
IMPORT  req: defRequest;        IMPORT  env: tskEnv;
IMPORT  SCR: KTVcm;             IMPORT  SIO: SIOcm;
IMPORT  str: Strings;

TYPE ADDRESS = SYSTEM.ADDRESS;

CONST ok = err.ok;
   LINES = 25;
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
    color:=SCR.color; back:=SCR.gmin;
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
      set(dtt._background,   back,     o.back)     ;
      set(dtt._color,        color,    o.color)    ;
      set(dtt._font,         font,     o.font)     ;
      set(dtt._cursor,       cursor,   o.cursor)   ;
      set(dtt._reverse,      reverse,  o.reverse)  ;
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
                         SCR.set_color(i); color:=SCR.color
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

-------------------------  KEYBOARD  ---------------------------
                         ------------


PROCEDURE break0(n: INTEGER); END break0;

PROCEDURE break(n: INTEGER);
BEGIN IF kstate.breakon#0 THEN kstate.ubrk(n) END END break;

----------------------------------------------------------------

CONST
  lSHIFT   = 144c;   _lshift   = 1;
  rSHIFT   = 056c;   _rshift   = 2;
  ALT      = 154c;   _alt      = 3;
  CAPS     = 146c;   _caps     = 4;
  NUMS     = 010c;   _nums     = 5;
  SCROLL   = 176c;   _scroll   = 6;
  CNTRL    = 147c;   _cntrl    = 7;
  NATIO    = 156c;   _natio    = 8;

  CSI = ARRAY OF CHAR  (*  CHANGE  STATE INTRODUCERS  *)
  { 000c,000c,000c,000c,000c,000c,000c,000c,005c,000c,000c,000c,000c,000c,
    000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,000c,000c,002c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,001c,000c,004c,007c,000c,000c,000c,000c,003c,000c,010c,000c,
    000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
    006c
  };

  NUM = ARRAY OF CHAR  (*  NUMLOCK state  *)
  { 000c,000c,000c,000c,063c,070c,066c,065c,000c,000c,000c,067c,060c,064c,
    062c,061c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,000c,
    000c,000c,000c,000c,000c,000c,000c,071c
  };

  LT0 = ARRAY OF CHAR  (*  LATIN   state NORMAL  *)
  { 000c,000c,000c,212c,205c,200c,202c,237c,000c,000c,012c,206c,211c,203c,
    201c,207c,205c,210c,000c,204c,000c,000c,201c,202c,206c,211c,220c,207c,
    000c,200c,203c,000c,012c,003c,000c,204c,213c,054c,056c,055c,010c,000c,
    235c,176c,000c,015c,000c,000c,175c,234c,233c,033c,077c,052c,137c,337c,
    060c,232c,000c,055c,076c,150c,134c,166c,070c,071c,231c,172c,074c,144c,
    100c,154c,067c,230c,227c,133c,142c,135c,170c,157c,156c,066c,226c,147c,
    040c,162c,151c,164c,145c,065c,225c,160c,000c,141c,000c,155c,173c,220c,
    221c,073c,000c,011c,000c,000c,061c,223c,222c,152c,000c,143c,000c,146c,
    062c,000c,224c,165c,000c,171c,161c,136c,064c,063c,000c,153c,000c,167c,
    000c,163c
  };

  LT1 = ARRAY OF CHAR  (*  LATIN   state SHIFT  *)
  { 000c,000c,000c,212c,205c,200c,202c,237c,000c,000c,012c,206c,211c,203c,
    201c,207c,205c,210c,000c,204c,000c,000c,201c,202c,206c,211c,220c,207c,
    000c,200c,203c,000c,012c,003c,000c,204c,213c,054c,056c,055c,010c,000c,
    235c,140c,000c,015c,000c,000c,175c,234c,233c,033c,057c,072c,137c,377c,
    060c,232c,000c,075c,056c,110c,134c,126c,050c,051c,231c,132c,054c,104c,
    100c,114c,047c,230c,227c,173c,102c,175c,130c,117c,116c,046c,226c,107c,
    040c,122c,111c,124c,105c,045c,225c,120c,000c,101c,000c,115c,174c,220c,
    221c,053c,000c,011c,000c,000c,041c,223c,222c,112c,000c,103c,000c,106c,
    042c,000c,224c,125c,000c,131c,121c,136c,044c,043c,000c,113c,000c,127c,
    000c,123c
  };

  LT2 = ARRAY OF CHAR  (*  LATIN   state ALT  *)
  { 000c,000c,000c,212c,205c,200c,202c,237c,000c,000c,012c,206c,211c,203c,
    201c,207c,205c,210c,000c,204c,000c,000c,201c,202c,206c,211c,220c,207c,
    000c,200c,203c,000c,012c,003c,000c,204c,213c,054c,056c,055c,010c,000c,
    235c,236c,000c,015c,000c,000c,235c,234c,233c,033c,077c,052c,137c,337c,
    060c,232c,000c,055c,076c,210c,134c,226c,070c,071c,231c,232c,074c,204c,
    100c,214c,067c,230c,227c,133c,202c,135c,230c,217c,216c,066c,226c,207c,
    040c,222c,211c,224c,205c,065c,225c,220c,000c,201c,000c,215c,233c,220c,
    221c,073c,000c,011c,000c,000c,061c,223c,222c,212c,000c,203c,000c,206c,
    062c,000c,224c,225c,000c,231c,221c,136c,064c,063c,000c,213c,000c,227c,
    000c,223c
  };

  LT3 = ARRAY OF CHAR  (*  LATIN   state ALT+SHIFT  *)
  { 000c,000c,000c,212c,205c,200c,202c,237c,000c,000c,012c,206c,211c,203c,
    201c,207c,205c,210c,000c,204c,000c,000c,201c,202c,206c,211c,220c,207c,
    000c,200c,203c,000c,012c,003c,000c,204c,213c,054c,056c,055c,010c,000c,
    235c,276c,000c,015c,000c,000c,275c,234c,233c,033c,057c,072c,137c,377c,
    060c,232c,000c,075c,056c,250c,134c,266c,050c,051c,231c,272c,054c,244c,
    100c,254c,047c,230c,227c,173c,242c,175c,270c,257c,256c,046c,226c,247c,
    040c,262c,251c,264c,245c,045c,225c,260c,000c,241c,000c,255c,273c,220c,
    221c,053c,000c,011c,000c,000c,041c,223c,222c,252c,000c,243c,000c,246c,
    042c,000c,224c,265c,000c,271c,261c,136c,044c,043c,000c,253c,000c,267c,
    000c,263c
  };

  NT0 = ARRAY OF CHAR  (*  NATIO   state NORMAL  *)
  { 000c,000c,000c,212c,205c,200c,202c,237c,000c,000c,012c,206c,211c,203c,
    201c,207c,205c,210c,000c,204c,000c,000c,201c,202c,206c,211c,220c,207c,
    000c,200c,203c,000c,012c,003c,000c,204c,213c,054c,056c,055c,010c,000c,
    235c,176c,000c,015c,000c,000c,175c,234c,233c,033c,077c,052c,137c,337c,
    060c,232c,000c,055c,076c,310c,334c,326c,070c,071c,231c,332c,074c,304c,
    300c,314c,067c,230c,227c,333c,302c,335c,330c,317c,316c,066c,226c,307c,
    040c,322c,311c,324c,305c,065c,225c,320c,000c,301c,000c,315c,173c,220c,
    221c,073c,000c,011c,000c,000c,061c,223c,222c,312c,000c,303c,000c,306c,
    062c,000c,224c,325c,000c,331c,321c,336c,064c,063c,000c,313c,000c,327c,
    000c,323c
  };

  NT1 = ARRAY OF CHAR  (*  NATIO   state SHIFT  *)
  { 000c,000c,000c,212c,205c,200c,202c,237c,000c,000c,012c,206c,211c,203c,
    201c,207c,205c,210c,000c,204c,000c,000c,201c,202c,206c,211c,220c,207c,
    000c,200c,203c,000c,012c,003c,000c,204c,213c,054c,056c,055c,010c,000c,
    235c,140c,000c,015c,000c,000c,175c,234c,233c,033c,057c,072c,137c,377c,
    060c,232c,000c,075c,056c,350c,374c,366c,050c,051c,231c,372c,054c,344c,
    340c,354c,047c,230c,227c,373c,342c,375c,370c,357c,356c,046c,226c,347c,
    040c,362c,351c,364c,345c,045c,225c,360c,000c,341c,000c,355c,174c,220c,
    221c,053c,000c,011c,000c,000c,041c,223c,222c,352c,000c,343c,000c,346c,
    042c,000c,224c,365c,000c,371c,361c,376c,044c,043c,000c,353c,000c,367c,
    000c,363c
  };

  NT2 = ARRAY OF CHAR  (*  NATIO   state ALT  *)
  { 000c,000c,000c,212c,205c,200c,202c,237c,000c,000c,012c,206c,211c,203c,
    201c,207c,205c,210c,000c,204c,000c,000c,201c,202c,206c,211c,220c,207c,
    000c,200c,203c,000c,012c,003c,000c,204c,213c,054c,056c,055c,010c,000c,
    235c,236c,000c,015c,000c,000c,235c,234c,233c,033c,077c,052c,137c,337c,
    060c,232c,000c,055c,076c,150c,134c,166c,070c,071c,231c,172c,074c,144c,
    100c,154c,067c,230c,227c,133c,142c,135c,170c,157c,156c,066c,226c,147c,
    040c,162c,151c,164c,145c,065c,225c,160c,000c,141c,000c,155c,233c,220c,
    221c,073c,000c,011c,000c,000c,061c,223c,222c,152c,000c,143c,000c,146c,
    062c,000c,224c,165c,000c,171c,161c,136c,064c,063c,000c,153c,000c,167c,
    000c,163c
  };

  NT3 = ARRAY OF CHAR  (*  NATIO   state ALT+SHIFT  *)
  { 000c,000c,000c,212c,205c,200c,202c,237c,000c,000c,012c,206c,211c,203c,
    201c,207c,205c,210c,000c,204c,000c,000c,201c,202c,206c,211c,220c,207c,
    000c,200c,203c,000c,012c,003c,000c,204c,213c,054c,056c,055c,010c,000c,
    235c,236c,000c,015c,000c,000c,235c,234c,233c,033c,077c,052c,137c,377c,
    060c,232c,000c,055c,076c,110c,134c,126c,070c,071c,231c,132c,074c,104c,
    100c,114c,067c,230c,227c,173c,102c,175c,130c,117c,116c,066c,226c,107c,
    040c,122c,111c,124c,105c,065c,225c,120c,000c,101c,000c,115c,233c,220c,
    221c,073c,000c,011c,000c,000c,061c,223c,222c,112c,000c,103c,000c,106c,
    062c,000c,224c,125c,000c,131c,121c,136c,064c,063c,000c,113c,000c,127c,
    000c,123c
  };

----------------------------------------------------------------

VAR   TOGS: BITSET;
       TRT: POINTER TO ARRAY [0..255] OF CHAR;
  HIGH_TRT: INTEGER;
   NUMLOCK: BOOLEAN;
      LAST: CHAR;
   LASTkey: BITSET;
      TIME: INTEGER;

TYPE char = BITSET;

PROCEDURE repeat_key;
BEGIN
  IF LAST=0c       THEN RETURN END;
  IF os.timer<TIME THEN RETURN END;
  SIO.put(char(LAST));
  TIME:=os.timer+3
END repeat_key;

PROCEDURE set_trt(VAL str: ARRAY OF CHAR);
  PROCEDURE high (VAL str: ARRAY OF CHAR): INTEGER; CODE END high;
  PROCEDURE adr  (): SYSTEM.ADDRESS;                CODE END adr;
BEGIN
  HIGH_TRT:=high(str); TRT:=adr()
END set_trt;

PROCEDURE change_trt;
BEGIN
  IF kstate.foreign#0 THEN
    IF    TOGS*{_rshift,_lshift,_alt}={_rshift,_alt} THEN set_trt(NT3)
    ELSIF TOGS*{_rshift,_lshift,_alt}={_lshift,_alt} THEN set_trt(NT3)
    ELSIF TOGS*{_alt}           #{} THEN                  set_trt(NT2)
    ELSIF TOGS*{_rshift,_lshift}#{} THEN                  set_trt(NT1)
    ELSE                                                  set_trt(NT0)
    END
  ELSE
    IF    TOGS*{_rshift,_lshift,_alt}={_rshift,_alt} THEN set_trt(LT3)
    ELSIF TOGS*{_rshift,_lshift,_alt}={_lshift,_alt} THEN set_trt(LT3)
    ELSIF TOGS*{_alt}           #{} THEN                  set_trt(LT2)
    ELSIF TOGS*{_rshift,_lshift}#{} THEN                  set_trt(LT1)
    ELSE                                                  set_trt(LT0)
    END
  END
END change_trt;

PROCEDURE press(ch: CHAR);
  VAR c: INTEGER;
BEGIN
  c:=ORD(CSI[ORD(ch)]);
  IF (c>=32) OR (c IN TOGS) THEN RETURN END;
  WITH kstate DO
    CASE c OF
      |_scroll: STOP:=TRUE
      |_nums  : NUMLOCK:=NOT NUMLOCK;
      |_caps  : caps:=1-caps;
      |_rshift: IF TOGS*{_lshift}#{} THEN
                  foreign:=1; SIO.put(char(16c))
                END
      |_lshift: IF TOGS*{_rshift}#{} THEN
                  foreign:=0; SIO.put(char(17c))
                END
      |_natio : foreign:=1-foreign;
                IF foreign#0 THEN SIO.put(char(16c))
                ELSE              SIO.put(char(17c))
                END
    ELSE
    END
  END;
  INCL(TOGS,c); change_trt
END press;

PROCEDURE unstop;
BEGIN
  IF STOP THEN os.send(RUN); STOP:=FALSE END
END unstop;

PROCEDURE lost;
BEGIN
  TOGS:={}; kstate.caps:=0; kstate.foreign:=0; set_trt(LT0); unstop;
END lost;

PROCEDURE release(key: char);
  VAR c: INTEGER;
     ch: CHAR;
BEGIN
  IF key-{0..7}#{} THEN lost; RETURN END;
  ch:=CHAR(key*{0..6});
  IF ORD(ch)>HIGH(CSI) THEN LAST:=0c; RETURN END;
  c:=ORD(CSI[ORD(ch)]);
  IF (c=0) OR (c>=32) OR NOT (c IN TOGS) THEN RETURN END;
  EXCL(TOGS,c);
  IF ch=SCROLL THEN unstop END;
  change_trt
END release;


PROCEDURE translate_keyboard;
  VAR key: char;

  PROCEDURE put_key(ch: char);
  BEGIN
    SIO.put(ch); LAST:=CHAR(ch);   LASTkey:=key;  TIME:=os.timer+30
  END put_key;

  VAR c: INTEGER;
     ch: CHAR;

BEGIN
  key:=SIO.get();
  IF key-{7}=BITSET(3Ch) THEN key:=key/{7} END;
  IF kstate.raw#0 THEN SIO.put(key); RETURN END;

  IF key-{0..7}={} THEN ch:=CHAR(key*{0..7});
  ELSE   lost;          ch:=CHAR(LASTkey/{7});
  END;
  c:=ORD(ch) MOD 128;
  IF (c<=HIGH(CSI)) & (CSI[c]#0c) THEN
    IF ch<200c THEN release(BITSET(c)) ELSE press(CHAR(c)) END;
    RETURN
  END;
  IF key*{7}={} THEN LAST:=0c; RETURN END;
  IF c>HIGH_TRT THEN RETURN END;
  ch:=TRT^[c];
  IF ch=dkb.del THEN
    IF TOGS*{_cntrl}#{} THEN break(1); SIO.put(char(25c)); RETURN END;
    IF TOGS*{_alt}#{}   THEN break(2); SIO.put(char(30c)); RETURN END;
  END;
  IF NUMLOCK & (c<=HIGH(NUM)) & (NUM[c]#0c) THEN
    put_key(char(NUM[c])); RETURN
  END;
  IF (ch<40c)
  OR (TOGS*{_cntrl}#{}) & (c<=HIGH(LT0)) & (100c<=LT0[c]) & (LT0[c]<=177c)
  THEN
    ch:=CHAR( ORD(LT0[c]) MOD 32 );
    CASE ch OF
    |03c: break(0)
    |21c: IF STOP THEN unstop; RETURN END;
    |23c: IF NOT STOP THEN STOP:=TRUE; RETURN END;
    |25c: break(1)
    |30c: break(2)
    ELSE
    END;
    SIO.put(char(ch)); RETURN
  END;
  IF kstate.caps#0 THEN
    IF ("a"<=ch)  & (ch<="z")  THEN DEC(ch,40b); put_key(char(ch)); RETURN END;
    IF (300c<=ch) & (ch<=377c) THEN INC(ch,40b); put_key(char(ch)); RETURN END
  END;
  put_key(char(ch));
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
  VAR u: dkb.BREAK;
BEGIN
  u:=kstate.ubrk; low.zero(kstate); kstate.ubrk:=u; -- NOTE: ubrk not changed
  WITH kstate DO type:=2; fkeys:=14; breakon:=1 END
END kb_reset;

PROCEDURE kbcontrol(no : INTEGER; VAL x: ARRAY OF SYSTEM.WORD): INTEGER;

  PROCEDURE ss(oper: INTEGER; SEQ rand: SYSTEM.WORD);
  BEGIN oper:=kbcontrol(oper,rand) END ss;

  PROCEDURE kb_restore(VAL old: dkb.STATE);
  BEGIN
    WITH old DO
      IF kstate.ubrk   #ubrk    THEN ss(dkb._ubreak ,ubrk   ) END;
      IF kstate.breakon#breakon THEN ss(dkb._break  ,breakon) END;
      IF kstate.foreign#foreign THEN ss(dkb._foreign,foreign) END;
      IF kstate.caps   #caps    THEN ss(dkb._caps   ,caps   ) END;
      IF kstate.shift  #shift   THEN ss(dkb._shift  ,shift  ) END;
      IF kstate.raw    #raw     THEN ss(dkb._raw    ,raw    ) END;
      IF kstate.autorep#autorep THEN ss(dkb._autorep,autorep) END;
    END
  END kb_restore;

  VAR   i: INTEGER;
      adr: SYSTEM.ADDRESS;
    i0,i1: INTEGER;
     kptr: POINTER TO dkb.STATE;
  kptrptr: POINTER TO POINTER TO dkb.STATE;

BEGIN
  i0:=-1; i1:=-1;
  IF HIGH(x)>=0 THEN i0:=x[0] END; i:=i0;
  IF HIGH(x)>=1 THEN i1:=x[1] END;
  WITH kstate DO
    CASE no OF
    |dkb._reset  :
      kb_reset
    |dkb._info   :
      IF i0<=0 THEN RETURN err.bad_parm END;
      adr:=x[0]; kptrptr:=adr; kptrptr^:=SYSTEM.ADR(kstate)
    |dkb._restore:
      IF i0<=0 THEN RETURN err.bad_parm END;
      adr:=x[0]; kptr:=adr; kb_restore(kptr^)
    |dkb._raw:
      IF i<=0 THEN raw:=0 ELSE raw:=1 END
    |dkb._break:
      IF i<=0 THEN breakon:=0 ELSE breakon:=1 END
    |dkb._ubreak: IF i>0  THEN ubrk:=dkb.BREAK(i) END;
    |dkb._bell_fd:
    |dkb._bell   :
    |dkb._foreign:
      IF i0<=0 THEN foreign:=0 ELSE foreign:=1 END
    |dkb._shift:
      IF i0<=0 THEN shift:=0 ELSE shift:=1   END
    |dkb._caps:
      IF i0<=0 THEN caps:=0    ELSE caps:=1  END
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
    |req.CONTROL  : buf^.ADR:=r.buf; buf^.HIGH:=r.len-1;
                    r.res:=kbcontrol(r.op DIV 256,buf)
    |req.GET_SPEC: r.baud:=0; r.xon:=0c; r.xoff:=0c; r.smode:={}
    |req.POWER_OFF: os.remove_action(repeat_key);
                    SIO.stop
  ELSE
    SIO.doio(r)
  END
END kb_doio;

PROCEDURE halt;
BEGIN
  os.remove_action(repeat_key)
END halt;

--------------------------  DRIVER  ----------------------------
                          ----------

PROCEDURE init(n: CHAR);
  VAR r: INTEGER;
      i: ARRAY [0..79] OF CHAR;
   name: ARRAY [0..7] OF CHAR;
BEGIN
  kb_init_state;
  kname:='key0'; tname:='tty0';
  kname[3]:=n  ; tname[3]:=n;
  SIO.init;
  LAST:=0c;  LASTkey:={};  TIME:=0;
  env.final(halt);
  r:=os.insert_action(repeat_key);
  IF r#ok THEN HALT(r) END;
  kstate.ubrk :=break0;
  kb_reset; tt_reset;
  r:=fs.define_driver(tname,'',0,fs.tty,tt_doio);
  IF r#ok THEN HALT(r) END;
  name:='SYSLOG0'; name[6]:=n;
  r:=fs.define_driver(name,tname,0,fs.tty,tt_doio);
  IF r#ok THEN HALT(r) END;
  r:=fs.define_driver(kname,'',0,fs.tty,kb_doio);
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
END TTwsCMlrm.
