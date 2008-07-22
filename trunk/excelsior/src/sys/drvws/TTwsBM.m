MODULE TTwsBM; (*$U+ Leo  24-Nov-90. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  err: defErrors;         IMPORT  low: lowLevel;
IMPORT  dkb: defKeyboard;       IMPORT  os : osKernel;
IMPORT  dtt: defTerminal;       IMPORT  fs : osFiles;
IMPORT  req: defRequest;        IMPORT  env: tskEnv;
IMPORT  SCR: KTV480;            IMPORT  SIO: SIOws;
IMPORT  scr: defScreen;         IMPORT  map: defBMG;

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
    locked: BOOLEAN;        (* bitmap locked by graphic applications *)
    STOP  : BOOLEAN;
    RUN   : os.signal_rec;

PROCEDURE tt_reset;

  PROCEDURE set(VAR d: ARRAY OF CHAR; VAL s: ARRAY OF CHAR);
  BEGIN d[0]:=s[0]; d[1]:=s[1]; d[2]:=s[2] END set;

BEGIN
  WITH tstate DO
    SCR.RESET;
    type     :=480;       hbar :=HBAR;           set(bars[0],BARS0);
    lines    :=SCR.lines; vbar :=VBAR;           set(bars[1],BARS1);
    columns  :=SCR.columns;                      set(bars[2],BARS2);
    min_color:=SCR.gmin;  back :=SCR.gmin;
    max_color:=SCR.gmax;  color:= 0;             SCR.set_ground(color,back);
    screens  := 1;        scr  := 0;
    cursor   := 1;        awp  := 1;
    underline:= 0;        raw  := 0;
    reverse  := 0;        fonts:= 1;
    something:= 0;        font := 0;
    smooth   := 0
  END
END tt_reset;

PROCEDURE ttcontrol(no: INTEGER; VAL x: ARRAY OF SYSTEM.WORD): INTEGER;

  VAR errr: INTEGER;

  PROCEDURE ss(oper: INTEGER; SEQ rand: SYSTEM.WORD);
  BEGIN errr:=ttcontrol(oper,rand) END ss;

  PROCEDURE tt_restore(VAL o: dtt.STATE): INTEGER;

    PROCEDURE set(op,a0,a1: INTEGER): BOOLEAN;
    BEGIN
      IF a0#a1 THEN ss(op,a1); IF errr#ok THEN RETURN TRUE END END;
      RETURN FALSE
    END set;

  BEGIN
    WITH tstate DO
      IF set(dtt._reverse,      reverse,  o.reverse)   THEN RETURN errr END;
      IF set(dtt._color,        color,    o.color)     THEN RETURN errr END;
      IF set(dtt._background,   back,     o.back)      THEN RETURN errr END;
      IF set(dtt._font,         font,     o.font)      THEN RETURN errr END;
      IF set(dtt._cursor,       cursor,   o.cursor)    THEN RETURN errr END;
      IF set(dtt._underline,    underline,o.underline) THEN RETURN errr END;
      IF set(dtt._autowrap,     awp,      o.awp)       THEN RETURN errr END;
      IF set(dtt._raw,          raw,      o.raw)       THEN RETURN errr END;
      IF set(dtt._smooth_scroll,smooth,   o.smooth)    THEN RETURN errr END;
      IF set(dtt._blinking,     blinking, o.blinking)  THEN RETURN errr END;
      IF set(dtt._cinter,       cinter,   o.cinter)    THEN RETURN errr END;
    END;
    RETURN err.ok
  END tt_restore;

  VAR i: INTEGER;
  i0,i1: INTEGER;

  VAR
    adr: SYSTEM.ADDRESS;
   tptr: POINTER TO dtt.STATE;
   tptrptr: POINTER TO POINTER TO dtt.STATE;

BEGIN
  i0:=-1; i1:=-1;
  IF HIGH(x)>=0 THEN i0:=x[0] END; i:=i0;
  IF HIGH(x)>=1 THEN i1:=x[1] END;
  WITH tstate DO
    CASE no OF
      |dtt._info   : IF i<=0 THEN RETURN err.bad_parm END;
                     tptrptr:=SYSTEM.ADDRESS(i);
                     tptrptr^:=SYSTEM.ADR(tstate);
                     RETURN ok

      |dtt._reset  : tt_reset; RETURN ok

      |dtt._restore: IF i<=0 THEN RETURN err.bad_parm END;
                     tptr:=SYSTEM.ADDRESS(i); RETURN tt_restore(tptr^)

      |dtt._raw    : IF i<=0 THEN raw:=0 ELSE raw:=1 END
      |dtt._up     : SCR.set_lc(SCR.ln-i,SCR.cl)
      |dtt._down   : SCR.set_lc(SCR.ln+i,SCR.cl)
      |dtt._right  : SCR.set_lc(SCR.ln,SCR.cl+i)
      |dtt._left   : SCR.set_lc(SCR.ln,SCR.cl-i)
      |dtt._home   : SCR.set_lc(0,0)
      |dtt._bottom : SCR.set_lc(SCR.lines-1,0)
      |dtt._repeat : RETURN err.inv_op
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
      |dtt._cursor     : i:=ORD(i>0);
                         IF cursor=i THEN RETURN ok END;
                         SCR.toggle_carret; cursor:=i
      |dtt._reverse
      ,dtt._something  : i:=ORD(i>0);
                         IF reverse=i THEN RETURN ok END;
                         SCR.set_ground(SCR.bg,SCR.fg);
                         reverse:=i
      |dtt._underline  : i:=ORD(i>0); SCR.undl:=(i#0); underline:=i
      |dtt._color      : IF i=color THEN RETURN ok END;
                         IF reverse=0 THEN
                           SCR.set_ground(i,SCR.bg); color:=SCR.fg
                         ELSE
                           SCR.set_ground(SCR.fg,i); color:=SCR.bg
                         END
      |dtt._background : IF i=back THEN RETURN ok END;
                         IF reverse=0 THEN
                           SCR.set_ground(SCR.fg,i); back:=SCR.bg
                         ELSE
                           SCR.set_ground(i,SCR.bg); back:=SCR.fg
                         END
      |dtt._font       : RETURN err.inv_op
      |dtt._autowrap   : i:=ORD(i>0); SCR.awp:=(i>0); awp:=i;
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
  IF locked THEN r.res:=err.busy; RETURN END;
  r.res:=ok;
  CASE r.op MOD 256 OF
    |req.NOP     :
    |req.GET_SPEC: r.baud:=0; r.xon:=0c; r.xoff:=0c; r.smode:={};
    |req.CONTROL : buf^.ADR:=r.buf; buf^.HIGH:=r.len-1;
                   r.res:=ttcontrol(r.op DIV 256,buf);
    |req.WRITE   : IF STOP THEN r.res:=os.wait_del(-1,RUN) END;
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
    211c,210c,201c,065c,202c,200c,033c,000c,000c,213c,205c,055c,052c,204c,
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
    000c,000c,000c,000c,010c,000c,000c,207c,000c,203c,206c,000c,000c,000c,
    211c,210c,201c,065c,202c,200c,033c,000c,000c,213c,205c,055c,052c,204c,
    000c,000c,000c,000c,000c,226c,012c
  };

  LT2 = ARRAY OF CHAR  (*  LATIN   state ALT  *)
  { 000c,230c,000c,224c,222c,220c,221c,000c,000c,231c,227c,225c,223c,011c,
    200c,000c,000c,000c,000c,000c,000c,221c,061c,000c,000c,000c,232c,223c,
    201c,227c,062c,000c,000c,203c,230c,204c,205c,064c,063c,000c,000c,040c,
    226c,206c,224c,222c,065c,000c,000c,216c,202c,210c,207c,231c,066c,000c,
    000c,000c,215c,212c,225c,067c,070c,000c,000c,054c,213c,211c,217c,060c,
    071c,000c,000c,056c,057c,214c,073c,220c,055c,000c,000c,000c,047c,000c,
    133c,075c,000c,000c,000c,000c,015c,135c,000c,134c,000c,000c,000c,000c,
    000c,000c,000c,000c,010c,000c,000c,207c,000c,203c,206c,000c,000c,000c,
    211c,210c,201c,065c,202c,200c,033c,000c,000c,213c,205c,055c,052c,204c,
    000c,000c,000c,000c,000c,226c,012c
  };

  LT3 = ARRAY OF CHAR  (*  LATIN   state ALT+SHIFT  *)
  { 000c,230c,000c,236c,234c,232c,233c,000c,000c,231c,227c,225c,235c,212c,
    240c,000c,000c,000c,000c,000c,000c,261c,041c,000c,000c,000c,272c,263c,
    241c,267c,100c,000c,000c,243c,270c,244c,245c,044c,043c,000c,000c,040c,
    266c,246c,264c,262c,045c,000c,000c,256c,242c,250c,247c,271c,136c,000c,
    000c,000c,255c,252c,265c,046c,052c,000c,000c,074c,253c,251c,257c,051c,
    050c,000c,000c,076c,077c,254c,072c,260c,137c,000c,000c,000c,042c,000c,
    173c,053c,000c,000c,000c,000c,015c,175c,000c,174c,000c,000c,000c,000c,
    000c,000c,000c,000c,010c,000c,000c,207c,000c,203c,206c,000c,000c,000c,
    211c,210c,201c,065c,202c,200c,033c,000c,000c,213c,205c,055c,052c,204c,
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
    211c,210c,201c,065c,202c,200c,033c,000c,000c,213c,205c,055c,052c,204c,
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
    000c,000c,000c,000c,010c,000c,000c,207c,000c,203c,206c,000c,000c,000c,
    211c,210c,201c,065c,202c,200c,033c,000c,000c,213c,205c,055c,052c,204c,
    000c,000c,000c,000c,000c,226c,012c
  };

  NT2 = ARRAY OF CHAR  (*  NATIO   state ALT  *)
  { 000c,230c,000c,224c,222c,220c,221c,000c,000c,231c,227c,225c,223c,011c,
    200c,000c,000c,000c,000c,000c,000c,161c,061c,000c,000c,000c,172c,163c,
    141c,167c,062c,000c,000c,143c,170c,144c,145c,064c,063c,000c,000c,040c,
    166c,146c,164c,162c,065c,000c,000c,156c,142c,150c,147c,171c,066c,000c,
    000c,000c,155c,152c,165c,067c,070c,000c,000c,054c,153c,151c,157c,060c,
    071c,000c,000c,056c,057c,154c,073c,160c,055c,000c,000c,000c,047c,000c,
    133c,075c,000c,000c,000c,000c,015c,135c,000c,134c,000c,000c,000c,000c,
    000c,000c,000c,000c,010c,000c,000c,207c,000c,203c,206c,000c,000c,000c,
    211c,210c,201c,065c,202c,200c,033c,000c,000c,213c,205c,055c,052c,204c,
    000c,000c,000c,000c,000c,226c,012c
  };

  NT3 = ARRAY OF CHAR  (*  NATIO   state ALT+SHIFT  *)
  { 000c,230c,000c,224c,222c,220c,221c,000c,000c,231c,227c,225c,223c,011c,
    200c,000c,000c,000c,000c,000c,000c,121c,061c,000c,000c,000c,132c,123c,
    101c,127c,062c,000c,000c,103c,130c,104c,105c,064c,063c,000c,000c,040c,
    126c,106c,124c,122c,065c,000c,000c,116c,102c,110c,107c,131c,066c,000c,
    000c,000c,115c,112c,125c,067c,070c,000c,000c,074c,113c,111c,117c,060c,
    071c,000c,000c,076c,077c,114c,072c,120c,055c,000c,000c,000c,042c,000c,
    173c,075c,000c,000c,000c,000c,015c,175c,000c,174c,000c,000c,000c,000c,
    000c,000c,000c,000c,010c,000c,000c,207c,000c,203c,206c,000c,000c,000c,
    211c,210c,201c,065c,202c,200c,033c,000c,000c,213c,205c,055c,052c,204c,
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

TYPE char = BITSET;

PROCEDURE press(ch: CHAR);
  VAR c: INTEGER;
BEGIN
  c:=ORD(CSI[ORD(ch)]);
  IF (c>=32) OR (c IN TOGS) THEN RETURN END;
  WITH kstate DO
    CASE c OF
      |_scroll: STOP:=TRUE
      |_nums  : NUMLOCK:=NOT NUMLOCK
      |_caps  : caps:=1-caps
      |_rshift: IF TOGS*{_lshift}#{} THEN foreign:=1; SIO.put(char(16c)) END
      |_lshift: IF TOGS*{_rshift}#{} THEN foreign:=0; SIO.put(char(17c)) END
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
  IF ch=SCROLL THEN unstop END;
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
    IF (300c<=ch) & (ch<=377c) THEN INC(ch,40b); SIO.put(char(ch)); RETURN END
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

---------------------------- BITMAP ----------------------------
                            --------

TYPE
  UNPACKED = POINTER TO ARRAY [0..15] OF scr.COLOR;

CONST
  palet0 = ARRAY OF INTEGER
           { 0FFFh, 0FFEh, 0FEFh, 0FEEh, 0EFFh, 0EFEh, 0EEFh, 0EEEh,
             0DDDh, 0FFCh, 0FCFh, 0FCCh, 0CFFh, 0CFCh, 0CCFh, 0CCCh
           };

  shy0=151+512;

VAR save    : SCR.PALETTE;
    black   : SCR.PALETTE;
    rainbow : SCR.PALETTE;
    save_car: BOOLEAN;      save_shf: INTEGER;
    save_x  : INTEGER;      save_l  : INTEGER;
    save_y  : INTEGER;      save_c  : INTEGER;

VAR igd480: map.BMD;

PROCEDURE bm_lock(VAR r: req.REQUEST);
BEGIN
  IF locked THEN r.res:=err.busy; RETURN END;
  locked:=TRUE;
  save_l:=SCR.ln;  save_c:=SCR.cl; save_car:=SCR.car;
  WHILE SCR.car DO SCR.toggle_carret END;
  SCR.set_lc(0,0);
  save:=SCR.palet;  save_x:=SCR.shx;  save_y:=SCR.shy;  save_shf:=SCR.scrl;
  SCR.shx:=0;  SCR.shy:=shy0;
  SCR.lock :=TRUE;
  SCR.palette(black,0,16);
  low._zero(1F8000h,128*256);
  SCR.palette(rainbow,0,16);
END bm_lock;

PROCEDURE bm_unlock(VAR r: req.REQUEST);
BEGIN
  SCR.palette(black,0,16);
  low._zero(1F8000h,128*256);
  SCR.palette(save,0,16);
  SCR.shx:=save_x;  SCR.shy:=save_y;  SCR.scrl:=save_shf;
  SCR.palette(save,0,16);
  SCR.set_lc(save_l,save_c);
  WHILE SCR.car#save_car DO SCR.toggle_carret END;
  SCR.lock:=FALSE;
  locked  :=FALSE;
END bm_unlock;

VAR screen: scr.STATUS;
    PAL   : ARRAY [0..15] OF scr.COLOR;

PROCEDURE init_scr;
  VAR i,c: INTEGER;
BEGIN
  WITH screen DO
    type:=49h;                           (* IGD480 /"I"/ *)
    ldcx:=0;  dx:=16;  xpix:=1;
    ldcy:=0;  dy:= 1;  ypix:=1;
    RGB:=4;  bpp:=4;
    W:=480;  H:=360;
    kind:=scr.bitmap;
    ext :=SYSTEM.ADR(igd480);           (*$<$X+*)
    pal^.ADR :=SYSTEM.ADR(PAL);
    pal^.HIGH:=HIGH(PAL);               (*$>*)
    FOR i:=0 TO HIGH(pal) DO
      WITH pal[i] DO
        c:=SCR.palet[i];  r:=SCR.ptrans[c MOD 16] DIV 4;
        c:=c DIV 16;      g:=SCR.ptrans[c MOD 16] DIV 4;
        c:=c DIV 16;      b:=SCR.ptrans[c MOD 16] DIV 4;
      END
    END;
    WITH igd480 DO
      W:=512;  H:=512;
      WPL:=16; BASE:=1F8000h;
      PATTERN:={0..31};  mask:={0..3};
      low.fill(layers,NIL);
      FOR i:=0 TO bpp-1 DO layers[i]:=BASE+i*(WPL*H) END;
    END
  END
END init_scr;

PROCEDURE set_rgb(VAR r: req.REQUEST);
  VAR i,j: INTEGER; c: BOOLEAN;
    R,G,B: INTEGER; p: UNPACKED;
BEGIN
  WITH r DO
    p:=buf;
    IF (pos<0) OR (pos+len>16) OR (len<=0) THEN res:=err.bad_parm; RETURN END;
    c:=FALSE;
    FOR i:=pos TO pos+len-1 DO
      WITH p^[i] DO R:=r MOD 4 * 4;  G:=g MOD 4 * 4;  B:=b MOD 4 * 4 END;
      j:=SCR.ptrans[R]+SCR.ptrans[G]<<4+SCR.ptrans[B]<<8;
      IF SCR.palet[i]#j THEN SCR.palet[i]:=j; c:=TRUE; PAL[i]:=p^[i] END;
    END;
    IF c THEN SCR.palette(SCR.palet,pos,len) END;
  END
END set_rgb;

PROCEDURE get_rgb(VAR r: req.REQUEST);
  VAR p: UNPACKED;
    i,c: INTEGER;
BEGIN
  WITH r DO
    p:=buf;
    IF (pos<0) OR (pos+len>16) OR (len<=0) THEN res:=err.bad_parm; RETURN END;
    FOR i:=pos TO pos+len-1 DO
      WITH p^[i] DO
        c:=SCR.palet[i];  r:=SCR.ptrans[c MOD 16] DIV 4;
        c:=c DIV 16;      g:=SCR.ptrans[c MOD 16] DIV 4;
        c:=c DIV 16;      b:=SCR.ptrans[c MOD 16] DIV 4;
      END
    END
  END
END get_rgb;

PROCEDURE set_ldcx(VAR r: req.REQUEST);
BEGIN
  IF (r.len<0) OR (r.len>511) THEN r.res:=err.bad_parm; RETURN END;
  screen.ldcx:=(r.len+15) DIV 16 * 16;
  SCR.shx:=screen.ldcx MOD 512;
  SCR.palette(SCR.palet,0,0)
END set_ldcx;

PROCEDURE set_ldcy(VAR r: req.REQUEST);
BEGIN
  IF (r.len<0) OR (r.len>511) THEN r.res:=err.bad_parm; RETURN END;
  screen.ldcy:=(shy0-r.len);
  SCR.shy:=screen.ldcy MOD 512;
  SCR.palette(SCR.palet,0,0)
END set_ldcy;

PROCEDURE bm_doio(VAR r: req.REQUEST);
BEGIN
  WITH r DO
    res:=err.ok;
    CASE op MOD 100h OF
    |req.LOCK    : bm_lock(r)
    |req.UNLOCK  : bm_unlock(r);
    |req.GET_SPEC:
    |req.SET_SPEC:
    |req.NOP     :
    |req.CONTROL :
      CASE op DIV 100h OF
      |scr._init    : r.buf:=SYSTEM.ADR(screen); init_scr;
      |scr._get_rgb : get_rgb(r)
      |scr._set_rgb : set_rgb(r)
      |scr._set_ldcx: set_ldcx(r)
      |scr._set_ldcy: set_ldcy(r)
      ELSE
        res:=err.inv_op
      END
    ELSE
      res:=err.inv_op
    END
  END
END bm_doio;

--------------------------  DRIVER  ----------------------------
                          ----------

PROCEDURE init;
  VAR i,r: INTEGER;
BEGIN
  kb_init_state;
  r:=SIO.init(3,SIO.KEYBOARD);
  IF r#ok THEN HALT(r) END;
  kstate.ubrk :=break0;
  kb_reset;
  tt_reset;
  locked:=FALSE;
  low.fill(black,palet0[0]);
  FOR i:=0 TO HIGH(rainbow) DO rainbow[i]:=palet0[i] END;
  r:=fs.define_driver("tty0","",0,fs.tty,tt_doio);
  IF r#ok THEN HALT(r) END;
  r:=fs.define_driver("SYSLOG0","tty0",0,fs.tty,tt_doio);
  IF r#ok THEN HALT(r) END;
  r:=fs.define_driver("key0","",0,fs.tty,kb_doio);
  IF r#ok THEN HALT(r) END;
  r:=fs.define_driver("scr0","tty0",0,fs.spec,bm_doio);
  IF r#ok THEN HALT(r) END;
  env.put_str(env.info,'tty0 key0 scr0 SYSLOG0',TRUE)
END init;

BEGIN
  init;
  env.become_ipr;
  SIO.monitor(translate_keyboard)
END TTwsBM.
