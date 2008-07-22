IMPLEMENTATION MODULE KTVcm; (* Leo 12-Mar-90. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT   os: osKernel;
IMPORT  cod: defCodes;

CONST COLOR = TRUE;

TYPE
  WORD      = SYSTEM.WORD;
  ADDRESS   = SYSTEM.ADDRESS;


---------------------  HARDWARE INTERFACE  ---------------------
                     ----------------------


PROCEDURE getm(): BITSET;  CODE cod.getm END getm;
PROCEDURE setm(m: BITSET); CODE cod.setm END setm;

PROCEDURE tra(VAR f,t: ADDRESS); CODE cod.tra END tra;

PROCEDURE move(a,b,l: INTEGER);
CODE cod.move END move;

PROCEDURE croll(dest,sou: ADDRESS; size: INTEGER);
CODE 96h END croll;


PROCEDURE fill(a: ADDRESS; p: WORD; size: INTEGER);
BEGIN a^:=p; move(a+1,a,size-1) END fill;


----------------------  INIT HARDWARE  -------------------------
                      -----------------

CONST cpl=128;

VAR
  base : ADDRESS;
  mask : ADDRESS;
  left : INTEGER;
  up   : INTEGER;
  inv  : BITSET;
  und  : BITSET;
  color: INTEGER;

VAR
  wsp    : ARRAY [0..255] OF INTEGER;
  drv    : ADDRESS;
  ipt    : ADDRESS;
  bump   : ARRAY [0..4095] OF INTEGER;
  ready  : os.signal_rec;
  ins_del: INTEGER;
  ins_del_line: INTEGER;
  filler : BITSET;


PROCEDURE trap;
  VAR no: INTEGER;
     len: INTEGER;
    adr0: ADDRESS;
BEGIN
  LOOP
    mask^:=BITSET(mask^)-{6};
    IF ins_del#0 THEN
      setm(getm()+{0});
      IF ins_del<0 THEN (* delete *)
        no  :=-ins_del;
        adr0:=base+(up+ins_del_line)*cpl+left;
        len :=(lines-ins_del_line-no)*cpl;
        move(adr0,adr0+no*cpl,len);
        adr0:=adr0+len;
        fill(adr0,filler,(lines-ins_del_line+no)*cpl)
      ELSE (* insert *)
        no  :=+ins_del;
        adr0:=base+(up+ins_del_line)*cpl+left;
        len :=(lines-ins_del_line-no)*cpl;
        fill(SYSTEM.ADR(bump),filler,no*cpl);
        move(SYSTEM.ADR(bump)+no*cpl,adr0,len);
        move(adr0,SYSTEM.ADR(bump),len+no*cpl)
      END;
      ins_del:=0;
      setm(getm()-{0});
      os.send(ready)
    END;
    mask^:=BITSET(mask^)+{6};
    tra(drv,ipt)
  END
END trap;

PROCEDURE init_hw;
  VAR m: BITSET;
    adr: ADDRESS;
BEGIN
  base:=1F8000h; base^:=0;  move(base+1,base,128*256-1);  (* era bitmap *)
  base:=17F000h;
  mask:=17E003h;
  mask^:=BITSET(mask^)-{7};
  base^:=BITSET(40c)+{9,11..13};
  move(base+1,base,4096-1);
  mask^:=BITSET(mask^)+{7};
  left:=45;
  up  :=7;
  inv :={12}; und:={13};
  ins_del:=0; ins_del_line:=0;
  os.ini_signal(ready,{},0);
  m:=getm();
  setm(m-{0,1});
  os.new_process(trap,SYSTEM.ADR(wsp),SIZE(wsp),drv);
  (* trap vector *)
  adr:=31*2;   adr^:=drv;
  adr:=adr+1;  adr^:=SYSTEM.ADR(ipt);
  mask^:=BITSET(mask^)+{6};
  setm(m)
END init_hw;

-----------------------  TTY EMULATOR  -------------------------
                       ----------------

PROCEDURE set_lc(l,c: INTEGER);
  VAR CAR: BOOLEAN;
BEGIN
  CAR:=car;
  IF CAR THEN toggle_carret END;
  IF l>=lines   THEN ln:=lines-1
  ELSIF  l<0    THEN ln:=0
  ELSE               ln:=l
  END;
  IF c>columns  THEN cl:=columns-1
  ELSIF  c<0    THEN cl:=0
  ELSE               cl:=c
  END;
  IF CAR THEN toggle_carret END
END set_lc;

CONST CTRT = ARRAY OF INTEGER {0-4,4-4,5-4,3-4,2-4,6-4,1-4,7-4};

PROCEDURE set_color(c: INTEGER);
BEGIN
  IF (gmin<=c) & (c<=gmax) THEN
    IF COLOR THEN color:=CTRT[c+4] ELSE color:=c END
  END;
  IF NOT reverse THEN filler:=inv+und+BITSET(color+4)<<9+BITSET(40c)
  ELSE                filler:=    und+BITSET(color+4)<<9+BITSET(40c)
  END
END set_color;

PROCEDURE set_reverse(on: INTEGER);
BEGIN
  reverse:=(on#0);
  IF NOT reverse THEN filler:=inv+und+BITSET(color+4)<<9+BITSET(40c)
  ELSE                filler:=    und+BITSET(color+4)<<9+BITSET(40c)
  END
END set_reverse;

PROCEDURE CLEAR;
  VAR i: INTEGER;
    CAR: BOOLEAN;
    adr: ADDRESS;
BEGIN
  CAR:=car;
  IF CAR THEN toggle_carret END;
  IF reverse THEN
    adr:=base+up*cpl+left;
    FOR i:=0 TO lines-1 DO
      fill(adr,filler,columns); adr:=adr+cpl
    END
  ELSE
    fill(base,filler,4096)
  END;
  IF CAR THEN toggle_carret END
END CLEAR;

PROCEDURE RESET;
BEGIN
  awp :=TRUE;
  undl:=FALSE;
  cntl:=TRUE;
  set_color(0);
  IF car=FALSE THEN toggle_carret END
END RESET;

VAR car_save: BITSET;

PROCEDURE toggle_carret;
  VAR p: ADDRESS;
BEGIN
  car:=NOT car;
  p  :=base+(up+ln)*cpl+left+cl;
  IF car THEN
    car_save:=p^;
    --p^:=BITSET(p^)/inv-{9..11}+{9,11}
    IF BITSET(p^)*inv={} THEN
      p^:=BITSET(p^)/inv+{9,11}
    ELSE
      p^:=BITSET(p^)/inv+{9,10,11}
    END
  ELSE
    p^:=car_save
  END;
END toggle_carret;

PROCEDURE write(VAL str: ARRAY OF CHAR; pos,len: INTEGER);

  PROCEDURE lf;
  BEGIN
    IF ln<lines-1 THEN INC(ln) ELSE scrollup(1) END
  END lf;

  VAR w: BITSET;
     ch: CHAR;
    adr: ADDRESS;
    CAR: BOOLEAN;

BEGIN
  CAR:=car;
  IF CAR THEN toggle_carret END;
  w:=inv+und;
  IF reverse THEN w:=w-inv END;
  IF undl    THEN w:=w-und END;
  IF pos+len>HIGH(str) THEN len:=HIGH(str)+1-pos END;
  (*$T-$W$Z*)
  WHILE len>0 DO
    ch:=str[pos];
    CASE ch OF
    |15c: cl:=0
    |12c: IF ln<lines-1 THEN INC(ln) ELSE scrollup(1) END
    |36c: cl:=0;
          IF ln<lines-1 THEN INC(ln) ELSE scrollup(1) END
    ELSE
      IF  cntl OR (ORD(ch) MOD 128 >= 32) THEN
        adr :=base+(up+ln)*cpl+left+cl;
        adr^:=BITSET(ch)+w+BITSET(color+4)<<9;
        INC(cl);
        IF cl>=columns THEN
          IF awp THEN
            cl:=0;
            IF ln<lines-1 THEN INC(ln) ELSE scrollup(1) END
          ELSE
            cl:=columns-1
          END
        END
      END
    END;
    INC(pos); DEC(len)
  END;
  IF CAR THEN toggle_carret END
END write;

PROCEDURE _ins_del_ln(line,no: INTEGER);
  VAR CAR: BOOLEAN;
BEGIN
  CAR:=car;
  IF CAR THEN toggle_carret END;
  ins_del:=no;
  ins_del_line:=line;
  os.wait(ready);
  IF NOT CAR  THEN RETURN END;
  toggle_carret
END _ins_del_ln;

PROCEDURE scrollup(no: INTEGER);
BEGIN
  IF no<=0     THEN RETURN END;
  IF no>=lines THEN CLEAR; RETURN END;
  _ins_del_ln(0,-no)
END scrollup;

PROCEDURE scrolldw(no: INTEGER);
BEGIN
  IF no<=0     THEN RETURN END;
  IF no>=lines THEN CLEAR; RETURN END;
  _ins_del_ln(0,+no)
END scrolldw;

PROCEDURE erarng(l,c0,c1: INTEGER);
  VAR adr: ADDRESS;
BEGIN
  adr:=base+(up+l)*cpl+c0+left;
  fill(adr,filler,c1-c0+1)
END erarng;

PROCEDURE eraln(dir: INTEGER);
  VAR CAR: BOOLEAN;
BEGIN
  CAR:=car;
  IF CAR THEN toggle_carret END;
  CASE dir OF
    |0: erarng(ln,cl,columns-1)
    |1: erarng(ln,0,cl)
    |2: erarng(ln,0,columns-1)
  ELSE
  END;
  IF NOT CAR  THEN RETURN END;
  toggle_carret
END eraln;

PROCEDURE erach(no: INTEGER);
BEGIN
  IF no<=0          THEN           RETURN END;
  IF cl+no>=columns THEN eraln(0); RETURN END;
  erarng(ln,cl,cl+no-1)
END erach;

PROCEDURE erase(dir: INTEGER);
  VAR i  : INTEGER;
      CAR: BOOLEAN;
      adr: ADDRESS;
BEGIN
  IF (dir=0) & (ln=0) & (cl=0) THEN CLEAR; RETURN END;
  IF  dir=2                    THEN CLEAR; RETURN END;
  CAR:=car;
  IF CAR THEN toggle_carret END;
  CASE dir OF
    |0: eraln(0);
        adr:=base+(up+ln+1)*cpl+left;
        FOR i:=0 TO lines-ln-1 DO
          fill(adr,filler,columns); adr:=adr+cpl
        END
    |1: eraln(1);
        adr:=base+up*cpl+left;
        FOR i:=0 TO ln-1 DO
          fill(adr,filler,columns); adr:=adr+cpl
        END
  ELSE
  END;
  IF NOT CAR  THEN RETURN END;
  toggle_carret
END erase;

PROCEDURE delln(no: INTEGER);
  VAR c: INTEGER;
BEGIN
  IF no<=0        THEN RETURN END;
  IF ln+no>=lines THEN c:=cl; cl:=0; erase(0); cl:=c; RETURN END;
  _ins_del_ln(ln,-no);
END delln;

PROCEDURE insln(no: INTEGER);
  VAR c: INTEGER;
BEGIN
  IF no<=0        THEN RETURN END;
  IF ln+no>=lines THEN c:=cl; cl:=0; erase(0); cl:=c; RETURN END;
  _ins_del_ln(ln,+no);
END insln;

PROCEDURE delch(no: INTEGER);
  VAR adr: ADDRESS;
      CAR: BOOLEAN;
BEGIN
  IF no<=0         THEN RETURN END;
  IF cl+no>columns THEN eraln(0); RETURN END;
  CAR:=car;
  IF CAR THEN toggle_carret END;
  adr:=base+(up+ln)*cpl+cl+left;
  move(adr,adr+no,columns-cl-no);
  adr:=adr+columns-no-cl;
  fill(adr,filler,no);
  IF NOT CAR THEN RETURN END;
  toggle_carret
END delch;

PROCEDURE insch(no: INTEGER);
  VAR adr: ADDRESS;
      CAR: BOOLEAN;
BEGIN
  IF no<=0         THEN RETURN END;
  IF cl+no>columns THEN eraln(0); RETURN END;
  CAR:=car;
  IF CAR THEN toggle_carret END;
  adr:=base+(up+ln)*cpl+cl+left;
  croll(adr+no,adr,columns-cl-no);
  fill(adr,filler,no);
  IF NOT CAR THEN RETURN END;
  toggle_carret
END insch;

-------------------------------------------------------------

PROCEDURE final;
  VAR m: BITSET;
BEGIN
  m:=getm();
  setm(m-{0,1});
  mask^:=BITSET(mask^)-{6};
  setm(m)
END final;

-------------------------------------------------------------

BEGIN
  init_hw;
  lines:=25;            columns:=80;
  gmin:=-4;             gmax   :=3;
  set_color(0);
  set_reverse(0);
  ln  :=0;              cl     :=0;
  car :=FALSE;          CLEAR;
  set_lc(lines-1,0);    RESET
END KTVcm.
