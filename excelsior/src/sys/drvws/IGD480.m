MODULE IGD480; (*$U+ Leo 18-Jan-90. (c) KRONOS *)

IMPORT  cod: defCodes;          IMPORT       SYSTEM;
IMPORT  req: defRequest;        IMPORT  os : osKernel;
IMPORT  err: defErrors;         IMPORT  fs : osFiles;
IMPORT  scr: defScreen;         IMPORT  low: lowLevel;
IMPORT  bmg: defBMG;            IMPORT  env: tskEnv;

(* IGD480 [-cmap] *)
(* -cmap means then BITMAP and CMAP on common screen *)

PROCEDURE move(a,b,c: INTEGER); CODE cod.move END move;

PROCEDURE di(VAR m: BITSET);
CODE cod.getm cod.copt 3 cod.bic cod.setm cod.ssw0 END di;

PROCEDURE ei(m: BITSET);
CODE cod.setm END ei;

TYPE
  ADDRESS  = SYSTEM.ADDRESS;
  PALETTE  = ARRAY [0..15] OF INTEGER;
  UNPACKED = POINTER TO ARRAY [0..15] OF scr.COLOR;

VAR
  CMAP   : BOOLEAN;
  screen : scr.STATUS;
  PAL    : ARRAY [0..15] OF scr.COLOR;
  locked : BOOLEAN;
  inited : BOOLEAN;
  pauseok: BOOLEAN;
  pause  : POINTER TO BITSET;
  SHIFT  : ADDRESS;             (* pointer to shift register   *)
  PALET  : POINTER TO PALETTE;  (* pointer to begin of palette *)
  igd480 : bmg.BMD;
  palet  : PALETTE;
  black  : PALETTE;
  rainbow: PALETTE;

CONST
  ptrans = ARRAY OF INTEGER
           { 0Fh,07h,0Bh,03h,0Dh,05h,09h,01h,0Eh,06h,0Ah,02h,0Ch,04h,08h,00h };

  palet0 = ARRAY OF INTEGER
           { 0FFFh, 0FFEh, 0FEFh, 0FEEh, 0EFFh, 0EFEh, 0EEFh, 0EEEh,
             0DDDh, 0FFCh, 0FCFh, 0FCCh, 0CFFh, 0CFCh, 0CCFh, 0CCCh
           };

  shy0=151+512;

PROCEDURE palette(p: PALETTE; from,len: INTEGER);
  VAR m: BITSET; s,d,sh: INTEGER;
BEGIN
  d:=SYSTEM.ADDRESS(PALET)+from;
  s:=SYSTEM.ADR(p)+from;
  sh:=((shy0-screen.ldcy)*200h+screen.ldcx)*400h;
  IF pauseok THEN di(m);
    REPEAT ei(m); di(m) UNTIL pause^*{0}#{}; move(d,s,len); SHIFT^:=sh; ei(m)
  ELSE
    move(d,s,len); SHIFT^:=sh
  END;
  move(SYSTEM.ADR(palet)+from,s,len)
END palette;

PROCEDURE lock(VAR r: req.REQUEST);
  VAR cmap: SYSTEM.ADDRESS;
BEGIN
  IF locked THEN r.res:=err.busy; RETURN END;
  locked:=TRUE;
  palette(black,0,16);
  low._zero(1F8000h,128*256);
  IF CMAP THEN cmap:=17E003h; cmap^:=BITSET(cmap^)-{7} END;
  palette(rainbow,0,16);
END lock;

PROCEDURE unlock(VAR r: req.REQUEST);
  VAR cmap: SYSTEM.ADDRESS;
BEGIN
  screen.ldcx:=0;
  screen.ldcy:=0;
  palette(black,0,16);
  low._zero(1F8000h,128*256);
  palette(rainbow,0,16);
  IF CMAP THEN cmap:=17E003h; cmap^:=BITSET(cmap^)+{7} END;
  locked  :=FALSE;
END unlock;

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
        c:=palet[i];      r:=ptrans[c MOD 16] DIV 4;
        c:=c DIV 16;      g:=ptrans[c MOD 16] DIV 4;
        c:=c DIV 16;      b:=ptrans[c MOD 16] DIV 4;
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
      j:=ptrans[R]+ptrans[G]<<4+ptrans[B]<<8;
      IF palet[i]#j THEN palet[i]:=j; c:=TRUE; PAL[i]:=p^[i] END;
    END;
    IF c THEN palette(palet,pos,len) END;
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
        c:=palet[i];      r:=ptrans[c MOD 16] DIV 4;
        c:=c DIV 16;      g:=ptrans[c MOD 16] DIV 4;
        c:=c DIV 16;      b:=ptrans[c MOD 16] DIV 4;
      END
    END
  END
END get_rgb;

PROCEDURE set_ldcx(VAR r: req.REQUEST);
BEGIN
  IF (r.len<0) OR (r.len>511) THEN r.res:=err.bad_parm; RETURN END;
  screen.ldcx:=(r.len+15) DIV 16 * 16;
  palette(palet,0,0);
END set_ldcx;

PROCEDURE set_ldcy(VAR r: req.REQUEST);
BEGIN
  IF (r.len<0) OR (r.len>511) THEN r.res:=err.bad_parm; RETURN END;
  screen.ldcy:=(r.len+15) DIV 16 * 16;
  palette(palet,0,0);
END set_ldcy;

PROCEDURE init_hw;
  VAR i,j: INTEGER;
BEGIN
  PALET:=ADDRESS( 1F0010h );
  SHIFT:=ADDRESS( 1F0000h );
  pause:=ADDRESS( 1F0020h );
  REPEAT UNTIL BITSET(SHIFT^)*{0}={};
  REPEAT UNTIL BITSET(SHIFT^)*{0}#{};
  low.fill(black,palet0[0]);
  FOR i:=0 TO HIGH(rainbow) DO rainbow[i]:=palet0[i] END;
  SHIFT^:=(shy0*200h+0)*400h;
  i:=40000;
  REPEAT DEC(i) UNTIL (pause^*{0}={}) OR (i=0);
  j:=40000;
  REPEAT DEC(j) UNTIL (pause^*{0}#{}) OR (j=0);
  pauseok:=(i#0) & (j#0);
  screen.ldcx:=0; screen.ldcy:=0;
  palette(black,0,16);
END init_hw;


PROCEDURE doio(VAR r: req.REQUEST);
BEGIN
  IF NOT inited THEN init_hw; inited:=TRUE END;
  WITH r DO
    res:=err.ok;
    CASE op MOD 100h OF
    |req.LOCK   : lock(r)
    |req.UNLOCK : unlock(r);
    |req.NOP    :
    |req.CONTROL:
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
END doio;


VAR r: INTEGER;
    s: STRING;

BEGIN
  env.get_str(env.args,s);
  CMAP:=(env.done) & (s="-cmap");
  locked:=FALSE;
  inited:=FALSE;
  r:=fs.define_driver("scr0","",0,fs.spec,doio);
  IF r#err.ok THEN HALT(r) END;
  env.put_str(env.info,"scr0",TRUE);
  env.become_ipr;
  os.suspend(os.active(),-1)
END IGD480.
