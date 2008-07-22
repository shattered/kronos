MODULE TTqFCT; (*$U+ Leo 22-Oct-89. (c) KRONOS *)  IMPORT  SYSTEM;

IMPORT  err: defErrors;
IMPORT  dtt: defTerminal;       IMPORT  fs : osFiles;
IMPORT  req: defRequest;        IMPORT  sio: SIOqBUS;
IMPORT  dkb: defKeyboard;       IMPORT  env: tskEnv;
IMPORT  low: lowLevel;          IMPORT  str: Strings;


CONST ok = err.ok;
   HBAR  = 246c;
   VBAR  = 241c;
   BARS0 = ARRAY OF CHAR { 245c, 251c, 244c };
   BARS1 = ARRAY OF CHAR { 250c, 253c, 252c };
   BARS2 = ARRAY OF CHAR { 243c, 247c, 242c };

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
  --       AWP off      CRS on    ISO ALL   SMOTH off
  str:=""233c"?7l"    233c"?25h"  233c"0m"  233c"?7l";
  r.op:=req.WRITE;           r.res:=ok;
  r.drn:=0;                  r.pos:=0;
  r.buf:=SYSTEM.ADR(str);    r.len:=16;
  sio.doio(r);
  low.zero(tstate);
  WITH tstate DO
    type     :=197;      hbar:=HBAR;    set(bars[0],BARS0);
    lines    :=24;       vbar:=VBAR;    set(bars[1],BARS1);
    columns  :=80;       back:=-2;      set(bars[2],BARS2);
    max_color:=01;       min_color:=-1;
    fonts    :=01;       cursor:=1;
    screens  :=01;       cinter:=1;
(*OTHERS := 0 *)
  END;
END tt_reset;

PROCEDURE facit_vt100(no: INTEGER; VAL x: ARRAY OF SYSTEM.WORD): INTEGER;

VAR errr: INTEGER;

  PROCEDURE ss(oper: INTEGER; SEQ rand: SYSTEM.WORD);
  BEGIN errr:=facit_vt100(oper,rand) END ss;

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

  PROCEDURE set_pos;
  BEGIN
    str[0]:=233c; len:=1;
    IF i0<=0 THEN
    ELSIF i0<9  THEN
      str[1]:=CHAR(ORD("0")+i0+1); len:=2
    ELSIF i0<24 THEN i0:=i0+1;
      str[1]:=CHAR(ORD("0")+ i0 DIV 10);
      str[2]:=CHAR(ORD("0")+ i0 MOD 10);
      len:=3
    ELSE
      str:=""233c"24"; len:=3
    END;
    IF i1<=0 THEN
    ELSIF i1<9 THEN
      str[len]:=";";                 INC(len);
      str[len]:=CHAR(ORD("0")+i1+1); INC(len)
    ELSIF i1<80 THEN i1:=i1+1;
      str[len]:=";";                       INC(len);
      str[len]:=CHAR(ORD("0")+ i1 DIV 10); INC(len);
      str[len]:=CHAR(ORD("0")+ i1 MOD 10); INC(len)
    ELSE
      str[len]:=";"; INC(len);
      str[len]:="8"; INC(len);
      str[len]:="0"; INC(len)
    END;
    str[len]:="H"; INC(len)
  END set_pos;

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
      |dtt._up           : scp('A')
      |dtt._down         : scp('B')
      |dtt._right        : scp('C')
      |dtt._left         : scp('D')
      |dtt._home         : str:="" 233c "H"  ; len:=2
      |dtt._bottom       : str:="" 233c "24H"; len:=4
      |dtt._repeat       : RETURN Repeat(CHAR(i0),i1)
      |dtt._erase        : IF NOT (i IN {0..2}) THEN i:=0 END;
                           str:="" 233c "0J";  len:=3;
                           str[1]:=CHAR( i+ORD("0") )
      |dtt._erase_line   : IF NOT (i IN {0..2}) THEN i:=0 END;
                           str:="" 233c "0K";  len:=3;
                           str[1]:=CHAR( i+ORD("0") )
      |dtt._erase_chars  : str:="" 233c; len:=1; app1("X")
      |dtt._set_pos      : set_pos
      |dtt._roll_up      : str:=""233c"24H"205c; len:=5
      |dtt._roll_down    : str:=""233c  "H"215c; len:=3
      |dtt._scroll_up    : str:=""33c"7"233c"24H"205c 33c"8"; len:=9
      |dtt._scroll_down  : str:=""33c"7"233c  "H"215c 33c"8"; len:=7
      |dtt._ins_char     : str:=233c;  len:=1;  app1("@")
      |dtt._del_char     : str:=233c;  len:=1;  app1("P")
      |dtt._ins_line     : str:=233c;  len:=1;  app1("L")
      |dtt._del_line     : str:=233c;  len:=1;  app1("M")
      |dtt._raw          : sio.raw_out(i<=0);  raw:=i
      |dtt._cursor       : IF i#0 THEN str:=""233c"?25h"; cursor:=1
                           ELSE        str:=""233c"?25l"; cursor:=0
                           END; len:=5
      |dtt._reverse
      ,dtt._something    : IF i<=0 THEN str:=""233c"27m"; len:=4; something:=0; reverse:=0
                           ELSE         str:=""233c "7m"; len:=3; something:=1; reverse:=1
                           END
      |dtt._underline    : IF i<=0 THEN str:=""233c"24m"; len:=4; underline:=0
                           ELSE         str:=""233c "4m"; len:=3; underline:=1
                           END
      |dtt._color:         IF HIGH(x)<0 THEN i:=0 END;
                           IF    i=0 THEN str:=""233c"22m"; len:=4
                           ELSIF i=1 THEN str:=""233c "1m"; len:=3
                           ELSE RETURN err.bad_parm
                           END; color:=i
      |dtt._background   : RETURN err.inv_op
      |dtt._font         : RETURN err.inv_op
      |dtt._autowrap     : IF i<=0  THEN str:=""233c"?7l"; awp:=0
                           ELSE          str:=""233c"?7h"; awp:=1
                           END; len:=4
      |dtt._smooth_scroll: IF i<=0 THEN str:=""233c"?4l"; smooth:=0
                           ELSE         str:=""233c"?4h"; smooth:=1
                           END; len:=4
      |dtt._blinking     : IF i<=0 THEN str:=""233c"25m"; len:=4; blinking:=0
                           ELSE         str:=""233c "5m"; len:=3; blinking:=1
                           END
      |dtt._screen       : RETURN err.inv_op
    ELSE
      RETURN err.inv_op
    END
  END;
  r.op:=req.WRITE;           r.res:=ok;
  r.drn:=0;                  r.pos:=0;
  r.buf:=SYSTEM.ADR(str);    r.len:=len;
  sio.doio(r);
  RETURN r.res
END facit_vt100;

PROCEDURE tt_doio(VAR r: req.REQUEST);
  VAR buf: DYNARR OF SYSTEM.WORD;
BEGIN
  r.res:=ok;
  CASE r.op MOD 256 OF
    |req.CONTROL:
      buf^.ADR:=r.buf; buf^.HIGH:=r.len-1;
      r.res:=facit_vt100(r.op DIV 256,buf)
    |req.NOP    :
    |req.LOCK   :
    |req.UNLOCK :
  ELSE
    sio.doio(r)
  END;
END tt_doio;

PROCEDURE kb_reset;
  VAR r: req.REQUEST;
    str: ARRAY [0..31] OF CHAR;
BEGIN
  --    auto repeat ON  application keypad
  str:="" 233c"8h"       33c"=";
  r.op:=req.WRITE;           r.res:=ok;
  r.drn:=0;                  r.pos:=0;
  r.buf:=SYSTEM.ADR(str);    r.len:=5;
  sio.doio(r);

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
                   FOR i:=0 TO i0-1 DO ws(str,1) END
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
      buf^.ADR:=r.buf; buf^.HIGH:=r.len-1;
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
  VAR r: INTEGER;
   name: ARRAY [0..7] OF CHAR;
  sname: ARRAY [0..7] OF CHAR;
BEGIN
  r:=sio.init(csr,trap);
  IF r#ok THEN HALT(r) END;
  env.final(sio.stop);
  name:="tty0"; name[3]:=CHAR(ORD("0")+tno);
  r:=fs.define_driver(name,"",0,fs.tty,tt_doio);
  IF r#ok THEN HALT(r) END;
  sname:="SYSLOG0"; sname[6]:=CHAR(ORD("0")+tno);
  r:=fs.define_driver(sname,name,0,fs.tty,tt_doio);
  IF r#ok THEN HALT(r) END;
  name:="key0"; name[3]:=CHAR(ORD("0")+tno);
  r:=fs.define_driver(name,"",0,fs.tty,kb_doio);
  IF r#ok THEN HALT(r) END;
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
  str.iscan(tno,ps,pos,done);
  IF NOT done THEN default; RETURN END;
  str.iscan(csr,ps,pos,done);
  IF NOT done THEN default; RETURN END;
  str.iscan(trap,ps,pos,done);
  IF NOT done THEN default; RETURN END
END parms;

----------------------------------------------------------------

PROCEDURE csi;

  CONST
    csi1 = ARRAY OF CHAR
          {dkb.up,dkb.dw,dkb.right,dkb.left};
    csi2 = ARRAY OF CHAR
          {212c,dkb.pgup,236c,dkb.ins,dkb.pgdw,dkb.del};
    csi3 = ARRAY OF CHAR
          --   17     18     19     20     21    22
          {dkb.f5,dkb.f4,dkb.f3,dkb.f2,dkb.f1,   0c,
          --   23     24     25     26     27    28    29    30
           dkb.f6,dkb.f7,dkb.f8,dkb.f9,    0c,  237c,   12c,  0c,
          --   31     32     33     34
           dkb.f10,  13c,   33c,dkb.break};

  VAR i: INTEGER;
     ch: CHAR;

BEGIN
  ch:=sio.get();
  i:=ORD(CAP(ch))-ORD("A");
  IF i IN {0..3} THEN sio.put(csi1[i]); RETURN END;
  i:=ORD(ch)-ORD("0");
  ch:=sio.get();
  IF (ch="~") & (i-1 IN {0..HIGH(csi2)}) THEN
    sio.put(csi2[i-1]); RETURN
  END;
  i:=i*10+ORD(ch)-ORD("0")-17;
  ch:=sio.get();
  IF (ch="~") & (i IN {0..HIGH(csi3)}) THEN
    sio.put(csi3[i]);
    IF csi3[i]=dkb.break THEN break(0) END;
  END;
END csi;

PROCEDURE alt;
  CONST
    altkb = ARRAY OF CHAR
             { dkb.ins ,
               dkb.end , dkb.dw   , dkb.pgdw,
               dkb.left, 36c      , dkb.right,
               dkb.home, dkb.up   , dkb.pgup };

  VAR ch: CHAR;

BEGIN
  ch:=sio.get();
  CASE ch OF
    |'M': sio.put(dkb.newln)                    -- Enter
--  |'`': kstate.foreign:=0                     -- SHIFT PF1
    |'`':                                       -- SHIFT PF1
    |'A': sio.put(30c); break(2)                -- SHIFT PF2   (^X)
    |'B': sio.put(25c); break(1)                -- SHIFT PF3   (^U)
    |'C': sio.put(03c); break(0)                -- SHIFT PF4   (^C)
    |'P': sio.put(dkb.f11)                      -- PF1
    |'Q': sio.put(dkb.f12)                      -- PF2
    |'R': sio.put(dkb.f13)                      -- PF3
    |'S': sio.put(dkb.f14)                      -- PF4
    |'m': sio.put('-')                          -- '-'
    |'l': sio.put('+')                          -- ','
    |'n': sio.put(dkb.del)                      -- '.'
    |'p'..'y': sio.put(altkb[ORD(ch)-ORD("p")]); RETURN
  ELSE
  END;
END alt;

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
  IF    ch=000c THEN ch:="_"
  ELSIF ch=33c  THEN
    kstate.foreign:=1-kstate.foreign;
    IF kstate.foreign#0 THEN ch:=16c ELSE ch:=17c END
  ELSIF kstate.foreign#0 THEN
    i:=ORD(ch)-ORD("-");
    IF (i>=0) & (i<HIGH(tab0)) THEN ch:=tab0[i] END
  ELSE
    i:=ORD(ch)-ORD("[");
    IF (i>=0) & (i<HIGH(tab1)) THEN ch:=tab1[i] END
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
    IF kstate.raw#0 THEN sio.put(ch)
    ELSIF ch=233c   THEN csi()
    ELSIF ch=217c   THEN alt()
    ELSE                 normal(ch)
    END
  END
END monitor;

VAR r,csr,trap,tno: INTEGER;

BEGIN
  parms(csr,trap,tno);
  init(csr,trap,tno);
  sio.monitor(monitor)
END TTqFCT.

CSI <dig> [<dig>] "~" (176c)            PF1    217c P
f1  CSI 2 1 ~                           PF2    217c Q
f2  CSI 2 0 ~                           PF3    217c R
f3  CSI 1 9 ~                           PF4    217c S
f4  CSI 1 8 ~                           0      217c p
f5  CSI 1 7 ~                           1      217c q
....                                    2      217c r
f6  CSI 2 3 ~                           3      217c s
f7  CSI 2 4 ~                           4      217c t
f8  CSI 2 5 ~                           5      217c u
f9  CSI 2 6 ~                           6      217c v
....                                    7      217c w
help CSI 2 8 ~                          8      217c x
Do   CSI 2 9 ~                          9      217c y
...                                     ,      217c l
f10  CSI 3 1 ~                          -      217c m
f11  CSI 3 2 ~                          .      217c n
f12  CSI 3 3 ~                          ENTER  217c M
f13  CSI 3 4 ~                          BACK   177c
                                        COMP   33c;
Insert CSI 1 ~
Find   CSI 2 ~
Remove CSI 3 ~                  UP     CSI A
Select CSI 4 ~                  DW     CSI B
Next   CSI 5 ~                  RT     CSI C
Prev   CSI 6 ~                  LF     CSI D
