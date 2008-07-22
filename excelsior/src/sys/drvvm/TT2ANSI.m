MODULE TT2ANSI; (* Leo 22-Oct-89. (c) KRONOS *)  IMPORT  SYSTEM;

IMPORT  err: defErrors;         IMPORT   fs: osFiles;
IMPORT  dtt: defTerminal;       IMPORT  sio: SIOqqBUS;
IMPORT  req: defRequest;        IMPORT  env: tskEnv;
IMPORT  dkb: defKeyboard;       IMPORT  nms: Strings;
IMPORT  low: lowLevel;


CONST ok = err.ok;
   HBAR  = 244c;
   VBAR  = 263c;
   BARS0 = ARRAY OF CHAR { 212c, 242c, 277c };
   BARS1 = ARRAY OF CHAR { 243c, 245c, 264c };
   BARS2 = ARRAY OF CHAR { 240c, 241c, 211c };

VAR tstate: dtt.STATE;
    kstate: dkb.STATE;

PROCEDURE break0(n: INTEGER); END break0;

PROCEDURE break(n: INTEGER);
BEGIN
  IF kstate.breakon#0 THEN kstate.ubrk(n) END
END break;

PROCEDURE tt_reset;

  PROCEDURE set(VAR d: ARRAY OF CHAR; VAL s: ARRAY OF CHAR);
  BEGIN (*$<$T-*) d[0]:=s[0]; d[1]:=s[1]; d[2]:=s[2] (*$>*) END set;

  VAR r: req.REQUEST;
    str: ARRAY [0..31] OF CHAR;

BEGIN
  sio.raw_out(FALSE);
  --      ^O
  str:="" 17c;
  r.op:=req.WRITE;           r.res:=ok;
  r.drn:=0;                  r.pos:=0;
  r.buf:=SYSTEM.ADR(str);    r.len:=1;
  sio.doio(r);
  low.zero(tstate);
  WITH tstate DO
    type     :=52;       hbar:=HBAR;    set(bars[0],BARS0);
    lines    :=50;       vbar:=VBAR;    set(bars[1],BARS1);
    columns  :=80;       back:=-2;      set(bars[2],BARS2);
    max_color:=0;        min_color:=-1;
    fonts    :=01;       cursor:=1;
    screens  :=01;       cinter:=1;
(*OTHERS := 0 *)
  END;
END tt_reset;

PROCEDURE vt52(no: INTEGER; VAL x: ARRAY OF SYSTEM.WORD): INTEGER;

VAR errr: INTEGER;

  PROCEDURE ss(oper: INTEGER; SEQ rand: SYSTEM.WORD);
  BEGIN errr:=vt52(oper,rand) END ss;

  PROCEDURE tt_restore(VAL o: dtt.STATE): INTEGER;

    PROCEDURE set(op,a0,a1: INTEGER): BOOLEAN;
    BEGIN
      IF a0#a1 THEN
        ss(op,a1);
        IF errr#ok THEN RETURN TRUE END;
      END;
      RETURN FALSE
    END set;

  BEGIN
    WITH tstate DO
      IF set(dtt._background,   back,     o.back)      THEN RETURN errr END;
      IF set(dtt._color,        color,    o.color)     THEN RETURN errr END;
      IF set(dtt._font,         font,     o.font)      THEN RETURN errr END;
      IF set(dtt._cursor,       cursor,   o.cursor)    THEN RETURN errr END;
      IF set(dtt._reverse,      reverse,  o.reverse)   THEN RETURN errr END;
      IF set(dtt._underline,    underline,o.underline) THEN RETURN errr END;
      IF set(dtt._autowrap,     awp,      o.awp)       THEN RETURN errr END;
      IF set(dtt._raw,          raw,      o.raw)       THEN RETURN errr END;
      IF set(dtt._smooth_scroll,smooth,   o.smooth)    THEN RETURN errr END;
      IF set(dtt._blinking,     blinking, o.blinking)  THEN RETURN errr END;
      IF set(dtt._cinter,       cinter,   o.cinter)    THEN RETURN errr END;
    END;
    RETURN err.ok
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

  PROCEDURE app(n: INTEGER; tail: CHAR);
  BEGIN
    IF n<1 THEN n:=1 END;
    IF n>999 THEN n:=999 END;
    IF n=1 THEN
      str[len]:=tail; INC(len);
      RETURN
    END;
    IF n<10 THEN
      str[len]:=CHAR(n MOD 10 + ORD("0")); INC(len)
    ELSIF n<100 THEN
      str[len]:=CHAR(n DIV 10 + ORD("0")); INC(len);
      str[len]:=CHAR(n MOD 10 + ORD("0")); INC(len)
    ELSE
      str[len]:=CHAR(n DIV 100 + ORD("0")); INC(len); n:=n MOD 100;
      str[len]:=CHAR(n DIV 10 + ORD("0")); INC(len);
      str[len]:=CHAR(n MOD 10 + ORD("0")); INC(len)
    END;
    str[len]:=tail; INC(len)
  END app;

  PROCEDURE app1(tail: CHAR);
  BEGIN
    app(i,tail);
  END app1;

  PROCEDURE app2(y: INTEGER; x: INTEGER; tail: CHAR);
  BEGIN
    IF y>tstate.lines THEN y:=tstate.lines END;
    IF x>tstate.columns THEN x:=tstate.columns END;

    IF x=1 THEN app(y,tail)
    ELSE        app(y,";"); app(x,tail)
    END
  END app2;

  VAR r: req.REQUEST;
    adr: SYSTEM.ADDRESS;
   tptr: POINTER TO dtt.STATE;
tptrptr: POINTER TO POINTER TO dtt.STATE;

  PROCEDURE Repeat(ch: CHAR; no: INTEGER): INTEGER;
    VAR i: INTEGER;
  BEGIN
    r.op:=req.WRITE; r.drn:=0; r.buf:=SYSTEM.ADR(str);
    len:=BYTES(str); IF len>=no THEN len:=no END;
    FOR i:=0 TO len-1 DO str[i]:=CHAR(ch) END;
    LOOP
      len:=BYTES(str);
      IF len>=no THEN len:=no END;
      r.res:=ok; r.pos:=0; r.len:=len;
      sio.doio(r);
      IF r.res#err.ok THEN EXIT END;
      DEC(no,len); IF no=0 THEN EXIT END
    END;
    RETURN r.res
  END Repeat;

BEGIN
  i0:=-1; i1:=-1;
  IF HIGH(x)>=0 THEN i0:=x[0] END; i:=i0;
  IF HIGH(x)>=1 THEN i1:=x[1] END;
  WITH tstate DO
    CASE no OF
      |dtt._info         : IF i<=0 THEN RETURN err.bad_parm END;
                           adr:=i; tptrptr:=adr; tptrptr^:=SYSTEM.ADR(tstate);
                           RETURN ok
      |dtt._reset        : tt_reset; RETURN ok
      |dtt._restore      : IF i<=0 THEN RETURN err.bad_parm END;
                           adr:=i; tptr:=adr; RETURN tt_restore(tptr^)
      |dtt._up           : str:="" 33c '[A'; len:=3;
      |dtt._down         : str:="" 33c '[B'; len:=3;
      |dtt._right        : str:="" 33c '[C'; len:=3;
      |dtt._left         : str:="" 33c '[D'; len:=3;
      |dtt._home         : str:="" 33c '[H'; len:=3;
      |dtt._bottom       : str:="" 33c"["; len:=2; app(tstate.lines,"H")
      |dtt._repeat       : RETURN Repeat(CHAR(i0),i1)

      |dtt._erase        : IF NOT (i IN {0..2}) THEN i:=0 END;
                           str:="" 33c 'J'; len:=2;
      |dtt._erase_line   : IF NOT (i IN {0..2}) THEN i:=0 END;
                           str:=""33c '[K'; len:=3;
      |dtt._erase_chars  : str:=""33c"["; len:=2; app1("X")
      |dtt._set_pos      : str:=""33c"["; len:=2; app2(i0+1,i1+1,"H")

      |dtt._roll_up      : str:="" 33c '[50;1H' 12c; len:=8;
      |dtt._roll_down    : str:="" 33c '[1T'; len:=4;

--    |dtt._scroll_up    : str:=""33c"7"233c"24H"205c 33c"8"; len:=9
--    |dtt._scroll_down  : str:=""33c"7"233c  "H"215c 33c"8"; len:=7

      |dtt._ins_char     : str:=""33c"[";  len:=2;  app1("@")
      |dtt._del_char     : str:=""33c"[";  len:=2;  app1("P")
      |dtt._ins_line     : str:=""33c"[";  len:=2;  app1("L")
      |dtt._del_line     : str:=""33c"[";  len:=2;  app1("M")
      |dtt._raw          : sio.raw_out(i<=0);  raw:=i
      |dtt._cursor       : RETURN err.ok;
      |dtt._reverse
      ,dtt._something    :  IF i<=0 THEN str:=""33c"[0r"; len:=4;
                              reverse:=0; something:=0
                            ELSE str:=""33c "[1r"; len:=4;
                              reverse:=1; something:=1
                            END
      |dtt._underline    :  IF i<=0 THEN str:=""33c"[0u"; len:=4; underline:=0
                            ELSE         str:=""33c"[1u"; len:=4; underline:=1
                            END
--    |dtt._color        :
      |dtt._background   : RETURN err.inv_op
      |dtt._font         : RETURN err.inv_op
      |dtt._autowrap     : RETURN err.inv_op
      |dtt._smooth_scroll: RETURN err.inv_op
      |dtt._blinking     : RETURN err.inv_op
      |dtt._screen       : RETURN err.inv_op
    ELSE
      RETURN err.inv_op
    END
  END;
  r.op:=req.WRITE;           r.res:=ok;
  r.drn:=0;        r.pos:=0;
  r.buf:=SYSTEM.ADR(str);    r.len:=len;
  sio.doio(r);
  RETURN r.res
END vt52;

PROCEDURE tt_doio(VAR r: req.REQUEST);
  VAR buf: DYNARR OF SYSTEM.WORD;
BEGIN
  r.res:=ok;
  CASE r.op MOD 256 OF
    |req.CONTROL: (*$<U+*) buf^.ADR:=r.buf; buf^.HIGH:=r.len-1; (*$>*)
                   r.res:=vt52(r.op DIV 256,buf)
    |req.NOP    :
    |req.LOCK   :
    |req.UNLOCK :
  ELSE
    sio.doio(r)
  END
END tt_doio;

PROCEDURE kb_reset;
  VAR r: req.REQUEST;
    str: ARRAY [0..31] OF CHAR;
BEGIN
  --    application keypad
(*
  str:="" 33c"=";
  r.op:=req.WRITE;           r.res:=ok;
  r.drn:=0;                  r.pos:=0;
  r.buf:=SYSTEM.ADR(str);    r.len:=2;
  sio.doio(r);
*)
  WITH kstate DO (* NOTE: ubrk not changed *)
    type:=7;     fkeys:=14;
    freq:=-1;    dur  :=-1;
    breakon:=1;  foreign:=0;  caps   :=0;
    shift  :=0;  raw    :=0;  scan   :=0;  autorep:=1
  END
END kb_reset;

PROCEDURE facit_kb(no : INTEGER; VAL x: ARRAY OF SYSTEM.WORD): INTEGER;

  PROCEDURE ss(oper: INTEGER; SEQ rand: SYSTEM.WORD);
  BEGIN oper:=facit_kb(oper,rand) END ss;

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

  VAR i: INTEGER;
    str: ARRAY [0..23] OF CHAR;
    adr: SYSTEM.ADDRESS;
   kptr: POINTER TO dkb.STATE;
kptrptr: POINTER TO POINTER TO dkb.STATE;
  i0,i1: INTEGER;
      r: req.REQUEST;
  coder: POINTER TO ARRAY CHAR OF CHAR;

  PROCEDURE ws(VAL s: ARRAY OF CHAR; len: INTEGER);
  BEGIN
    r.op :=req.WRITE;          r.res:=ok;
    r.drn:=0;                  r.pos:=0;
    r.buf:=SYSTEM.ADR(str);    r.len:=len;
    sio.doio(r);
  END ws;

BEGIN
  i0:=-1; i1:=-1;
  IF HIGH(x)>=0 THEN i0:=x[0] END; i:=i0;
  IF HIGH(x)>=1 THEN i1:=x[1] END;
  WITH kstate DO
    CASE no OF
    |dkb._reset  : kb_reset
    |dkb._info   : IF i0<=0 THEN RETURN err.bad_parm END;
                   adr:=x[0]; kptrptr:=adr; kptrptr^:=SYSTEM.ADR(kstate)
    |dkb._restore: IF i0<=0 THEN RETURN err.bad_parm END;
                   adr:=x[0]; kptr:=adr; kb_restore(kptr^)
    |dkb._raw    : IF i<=0 THEN i:=0 END; raw:=i
    |dkb._break  : IF i#0  THEN breakon:=1 ELSE breakon:=0 END
    |dkb._ubreak : IF i>0  THEN ubrk:=dkb.BREAK(i) END
    |dkb._bell_fd: RETURN err.inv_op
    |dkb._bell   : IF (i0<=0) THEN i0:=1 END; str:=""7c;
--                   FOR i:=0 TO i0-1 DO ws(str,1) END
    |dkb._foreign: IF i0<=0 THEN foreign:=0 ELSE foreign:=1 END
    |dkb._shift  : RETURN err.inv_op
    |dkb._caps   : RETURN err.inv_op
    |dkb._set_ct : IF (i0<=0) OR (i0=NIL) THEN RETURN err.bad_parm END;
                   coder:=x[0]; sio.coder:=coder^;
    |dkb._get_ct : IF (i0<=0) OR (i0=NIL) THEN RETURN err.bad_parm END;
                   coder:=x[0]; coder^:=sio.coder;
    ELSE
      RETURN err.inv_op
    END
  END;
  RETURN ok
END facit_kb;

PROCEDURE kb_doio(VAR r: req.REQUEST);
  VAR i: INTEGER;
    str: DYNARR OF CHAR;
    buf: DYNARR OF SYSTEM.WORD;
BEGIN
  r.res:=ok;
  CASE r.op MOD 256 OF
    |req.CONTROL:
(*$<U+*)
      buf^.ADR:=r.buf; buf^.HIGH:=r.len-1;
(*$>*)
      r.res:=facit_kb(r.op DIV 256,buf)
    |req.NOP    :
    |req.LOCK   :
    |req.UNLOCK :
    |req.READ   : IF r.len=-1 THEN r.len:=1 END; sio.doio(r)
  ELSE
    sio.doio(r)
  END;
END kb_doio;

PROCEDURE init(csr, trap, tno: INTEGER);
  VAR r: INTEGER; name,sname: ARRAY [0..7] OF CHAR;
BEGIN
  r:=sio.init(csr,trap);
  IF r#ok THEN HALT(r) END;

  name:="tty0"; name[3]:=CHAR(ORD("0")+tno);
  r:=fs.define_driver(name,'',0,fs.tty,tt_doio);
  IF r#0 THEN HALT(r) END;

  sname:="SYSLOG0"; sname[6]:=CHAR(ORD("0")+tno);
  r:=fs.define_driver(sname,name,0,fs.tty,tt_doio);
  IF r#0 THEN HALT(r) END;


  name:="key0"; name[3]:=CHAR(ORD("0")+tno);
  r:=fs.define_driver(name,'',0,fs.tty,kb_doio);
  IF r#0 THEN HALT(r) END;

  kstate.ubrk:=break0;
  tt_reset;
  kb_reset;
END init;

PROCEDURE parms(VAR csr, trap, tno: INTEGER);
  PROCEDURE default; BEGIN tno:=0; csr:=177560b; trap:=60b END default;
  VAR pos: INTEGER;
       ps: STRING;
     done: BOOLEAN;
BEGIN
  env.get_str(env.args,ps);
  IF NOT env.done THEN default; RETURN END;
  pos:=0; done:=TRUE;
  nms.iscan(tno,ps,pos,done);
  IF NOT done THEN default; RETURN END;
  nms.iscan(csr,ps,pos,done);
  IF NOT done THEN default; RETURN END;
  nms.iscan(trap,ps,pos,done);
  IF NOT done THEN default; RETURN END
END parms;

----------------------------------------------------------------

PROCEDURE e233;
  VAR ch: CHAR;
BEGIN
  ch:=sio.get();
  CASE ch OF
    |'R': sio.put(dkb.ins);
    |'S': sio.put(dkb.del);
    |'G': sio.put(dkb.home);
    |'O': sio.put(dkb.end);
    |'I': sio.put(dkb.pgup);
    |'Q': sio.put(dkb.pgdw);
  ELSE
    sio.put(ch)
  END;
END e233;

PROCEDURE e000;
  VAR ch: CHAR;
BEGIN
  ch:=sio.get();
  CASE ch OF
    |224c: sio.put(dkb.bcktab);

    |'G': sio.put(dkb.f3);
    |'I': sio.put(dkb.f4);
    |'H': sio.put(dkb.f5);
    |'P': sio.put(dkb.f6);
    |'O': sio.put(dkb.f7);
    |'Q': sio.put(dkb.f8);
    |'R': sio.put(dkb.f9);
    |'S': sio.put(dkb.f10);
  ELSE
    sio.put(ch)
  END;
END e000;

PROCEDURE esc;
  VAR i: INTEGER; ch: CHAR;
BEGIN
  ch:=sio.get();
  CASE ch OF
    |'A': sio.put(dkb.up);
    |'B': sio.put(dkb.dw);
    |'C': sio.put(dkb.right);
    |'D': sio.put(dkb.left);
    |'P': sio.put(dkb.f1);
    |'Q': sio.put(dkb.f2);
    |'R': sio.put(dkb.f3);
    |'S': sio.put(dkb.f4);
    |'?': ch:=sio.get();
          i:=ORD(ch)-70h;
          IF i IN {0..9} THEN sio.put( CHAR(ORD(dkb.f5)+i) )
          ELSIF ch=CHAR(6Eh) THEN sio.put(dkb.pgup)
          ELSIF ch=CHAR(4Dh) THEN sio.put(dkb.pgdw)
          END;
  ELSE
    sio.put(ch)
  END;
END esc;

PROCEDURE normal(ch: CHAR);
  CONST
    tab0 = (* ORD("-") *)
    "-./0123456789:;<Ъ>?ЮАБЦДЕФГХИЙКЛМНОПЯРСТУЖВЬЫЗШЭЩЧ0"
    "юабцдефгхийклмнопярстужвьызшэщч";

  CONST
    tab1 = (* ORD("[") *)
    "{|}^0`abcdefghijklmnopqrstuvwxyz[\]~";

  VAR i: INTEGER;

BEGIN
  IF kstate.foreign#0 THEN
    i:=ORD(ch)-ORD("-");
    IF (i>=0) & (i<HIGH(tab0)) THEN ch:=tab0[i] END
(*  ELSE
    i:=ORD(ch)-ORD("[");
    IF (i>=0) & (i<HIGH(tab1)) THEN ch:=tab1[i] END
*)
  END;
  IF    ch=003c THEN break(0)
  ELSIF ch=025c THEN break(1)
  ELSIF ch=030c THEN break(2)
  END;
  sio.put(ch)
END normal;

PROCEDURE monitor;
  VAR ch: CHAR;
BEGIN
  LOOP
    ch:=sio.get();
    IF kstate.raw#0 THEN sio.put(ch);
    ELSIF ch=33c    THEN esc();
    ELSIF ch=233c   THEN e233();
    ELSIF ch=000c   THEN e000();
    ELSE                 normal(ch);
    END
  END
END monitor;

----------------------------------------------------------------

VAR csr,trap,tno: INTEGER;

BEGIN
  parms(csr,trap,tno);
  init(csr,trap,tno);
  env.become_ipr;
  sio.monitor(monitor);
END TT2ANSI.
