MODULE PLvgBM; (*$U+ Leg 19-Jun-90. (c) KRONOS *)

IMPORT  sys: SYSTEM;            IMPORT  err: defErrors;
IMPORT   fs: osFiles;           IMPORT   os: osKernel;
IMPORT  dpl: defPlotter;        IMPORT  env: tskEnv;
IMPORT  req: defRequest;        IMPORT  cod: defCodes;
IMPORT  arg: tskArgs;           IMPORT  tty: Terminal;

(* instalation  &PLvgBM [dev_name] *)

VAR pls: dpl.STATE;

(************************* VG *********************************)
MODULE VG;
IMPORT  sys,cod,pls;
EXPORT  line,circ,arc,vg_ori,paint,erase,shifter,color;

TYPE BMD = RECORD
             w,h : INTEGER;
             wpl : INTEGER;
             base: sys.ADDRESS;
             patt: BITSET;
           END;

CONST layers=4;


VAR    bmd : BMD;     (* active bmd *)
      layer: ARRAY [0..layers-1] OF BMD;
    shifter: sys.ADDRESS;

TYPE ADDRESS=sys.ADDRESS;
        WORD=sys.WORD;
    CLP_PARM =
          RECORD
            xa,ya,xb,yb: INTEGER
          END;
     Palette = ARRAY [0..15] OF RECORD red,green,blue: INTEGER END;

  ARC_PARM = RECORD
               X,Y,x0,y0,x1,y1,r: INTEGER;
               x,y,co: INTEGER;
               xy0: INTEGER;  xx0: INTEGER;
               xy1: INTEGER;  xx1: INTEGER;
               yx0: INTEGER;  yy0: INTEGER;
               yx1: INTEGER;  yy1: INTEGER;
               case: BOOLEAN
             END;

  CRC_CONTEXT =
  RECORD
    x,y,co: INTEGER
  END;

  CRF_CONTEXT =
        RECORD
          x,y,co,xn,yn: INTEGER;
          do: BOOLEAN
        END;

VAR palette : Palette;
    p_trans : ARRAY [0..15] OF INTEGER;
    palet_ptr : POINTER TO ARRAY [0..15] OF INTEGER;
    back  : POINTER TO BITSET;

PROCEDURE move(dest,sou: ADDRESS; size: INTEGER);
CODE cod.move END move;

PROCEDURE in_rect(x,y,w,h: INTEGER): BOOLEAN;
CODE 0F9h 00h END in_rect;

PROCEDURE ddt(m: INTEGER; VAR bmd: BMD; x,y: INTEGER);
CODE 1 0F9h 001h END ddt;

PROCEDURE crc(mode: INTEGER; VAR bmd: BMD; VAR context: CRC_CONTEXT;
              X,Y: INTEGER);
CODE 0F9h 006h END crc;

PROCEDURE ARC(mode:INTEGER; VAR bmd: BMD; VAR arc_parm: ARC_PARM);
CODE 0F9h 007h END ARC;

PROCEDURE vg_ori(nx,ny: INTEGER);
BEGIN
  WITH pls DO
    x :=x+ox; y :=y+oy;
    ox:=nx;   oy:=ny;
    x :=x-ox; y :=y-oy
  END
END vg_ori;

PROCEDURE build_bmd(VAR bmd: BMD; w,h: INTEGER; adr: ADDRESS);
BEGIN
  ASSERT((h>0) & (w>0),4Ah);
  bmd.w:=w; bmd.h:=h; bmd.wpl:=(w+31) DIV 32; bmd.base:=adr
END build_bmd;

PROCEDURE clp(VAR clp_parm: CLP_PARM; w,h:INTEGER): BOOLEAN;
CODE 0F9h 004h END clp;

PROCEDURE dln(mode: INTEGER; VAR bmd: BMD; x,y,x1,y1: INTEGER);
CODE 0F9h 005h END dln;

PROCEDURE line (x0,y0,x1,y1: INTEGER);

  PROCEDURE draw(x,y,x1,y1: INTEGER);
  BEGIN dln(0,bmd,x,y,x1,y1) END draw;

  PROCEDURE clip(x0,y0,x1,y1,w,h: INTEGER);
    VAR clp_parm: CLP_PARM;
    VAR t: INTEGER;
  BEGIN
    WITH clp_parm DO xa:=x0; xb:=x1; ya:=y0; yb:=y1;
      WITH bmd DO
        IF NOT clp(clp_parm,w,h) THEN RETURN END;
      END;
      draw(xa,ya,xb,yb)
    END
  END clip;

  VAR i: INTEGER;
    w,h: INTEGER;
BEGIN
  WITH pls DO
    INC(x0,ox); INC(y0,oy); x:=x1; y:=y1;
    INC(x1,ox); INC(y1,oy)
  END;
  h :=bmd.h;     w :=bmd.w;
  y0:=h-1-y0 ;   y1:=h-1-y1;
  IF x1<x0 THEN i:=x1; x1:=x0; x0:=i;  i:=y1; y1:=y0; y0:=i END;
  IF in_rect(x0,y0,w,h) & in_rect(x1,y1,w,h) THEN
    draw(x0,y0,x1,y1)
  ELSE
    clip(x0,y0,x1,y1,w,h)
  END
END line;

PROCEDURE circ(X,Y: INTEGER; r: INTEGER);
  VAR context: CRC_CONTEXT;
BEGIN
  WITH pls DO
    x:=X+r; y:=Y;
    INC(X,ox); INC(Y,oy)
  END;
  Y:=bmd.h-1-Y;
  IF r<1 THEN ddt(0,bmd,X,Y); RETURN END;
  IF (X+r<0) OR (Y+r<0) OR (X-r>=bmd.w) OR (Y-r>=bmd.h) THEN RETURN END;
  WITH context DO
    x:=r; y:=0; co:=r DIV 2;
    REPEAT crc(0,bmd,context,X,Y) UNTIL y>x
  END
END circ;

PROCEDURE arc(X0,Y0,xa,ya,xb,yb,R: INTEGER);
  VAR arc_parm: ARC_PARM;
BEGIN
  WITH pls DO
    x:=xb; y:=yb;
    INC(X0,ox); INC(Y0,oy);
    INC(xa,ox); INC(ya,oy);
    INC(xb,ox); INC(yb,oy)
  END;
  IF (X0+R<0) OR (Y0+R<0) OR (X0-R>=bmd.w) OR (Y0-R>=bmd.h) THEN RETURN END;
  Y0:=bmd.h-1-Y0;
  ya:=bmd.h-1-ya;
  yb:=bmd.h-1-yb;
  IF R<1 THEN ddt(0,bmd,X0,Y0); RETURN END;
  WITH arc_parm DO
    x0:=xa-X0; x1:=xb-X0; X:=X0;
    y0:=ya-Y0; y1:=yb-Y0; Y:=Y0;
    x:=R; y:=0; co:=R DIV 2; r:=R;
    xy0:=x*y0;    xx0:=x*x0;      yx0:=0;       yy0:=0;
    xy1:=x*y1;    xx1:=x*x1;      yx1:=0;       yy1:=0;
    case:=(y1*x0>=x1*y0);
    REPEAT      ARC(0,bmd,arc_parm) UNTIL y>x
  END
END arc;

PROCEDURE paint;
  VAR m: BITSET;
  r,g,b: INTEGER;
      i: INTEGER;
      p: ARRAY [0..15] OF INTEGER;
BEGIN
  FOR i:=0 TO HIGH(palette) DO
    WITH palette[i] DO
      r:=INTEGER( BITSET(red  )*{0..3} );
      g:=INTEGER( BITSET(green)*{0..3} );
      b:=INTEGER( BITSET(blue )*{0..3} );
      r:=p_trans[r];
      g:=p_trans[g];
      b:=p_trans[b]
    END;
    p[i]:=r+g*16+b*256
  END;
  REPEAT  UNTIL back^*{0}#{};
  move(palet_ptr,sys.ADR(p),SIZE(palet_ptr^))
END paint;

PROCEDURE color(no: INTEGER; r,g,b: INTEGER);
BEGIN WITH palette[no] DO red:=r; green:=g; blue:=b END END color;

PROCEDURE screen(on: BOOLEAN);
  VAR i: INTEGER;
BEGIN
  IF on THEN paint
  ELSE FOR i:=0 TO HIGH(palet_ptr^) DO palet_ptr^[i]:=0FFFh END
  END
END screen;

PROCEDURE init_palette;
BEGIN
  p_trans[ 0]:=0Fh; p_trans[ 1]:=07h;
  p_trans[ 2]:=0Bh; p_trans[ 3]:=03h;
  p_trans[ 4]:=0Dh; p_trans[ 5]:=05h;
  p_trans[ 6]:=09h; p_trans[ 7]:=01h;
  p_trans[ 8]:=0Eh; p_trans[ 9]:=06h;
  p_trans[10]:=0Ah; p_trans[11]:=02h;
  p_trans[12]:=0Ch; p_trans[13]:=04h;
  p_trans[14]:=08h; p_trans[15]:=00h
END init_palette;

PROCEDURE init;
  VAR i: INTEGER;
BEGIN
  build_bmd(bmd,480,360,ADDRESS(1F8000h));
  bmd.wpl  :=16;
  bmd.patt :={0..31};
  shifter  :=sys.ADDRESS(1F0000h);
  back     :=ADDRESS(1F0020h);
  palet_ptr:=ADDRESS(1F0010h);
  init_palette
END init;

PROCEDURE erase; VAR p: sys.ADDRESS;
BEGIN p:=bmd.base; p^:=0; move(p+1,p,512*16*4-1) END erase;

BEGIN
  init
END VG;
(**************************************************************)

CONST ok  = err.ok;

PROCEDURE pl_reset;
BEGIN
  WITH pls DO
    pls.type  :=1;
    pls.pens  :=1;
    pls.pen   :=0;
    pen_dw    :=0;
    pls.ox    :=0;
    pls.oy    :=0;
    pls.x     :=0;
    pls.y     :=0;
    pls.speeds:=1;
    pls.speed :=1;
    pls.resols:=1;
    pls.resol :=1
  END;
  erase;
  shifter^:=0FF80000h;
  color(0,0,0,2); color(1,4,4,4);
  paint; vg_ori(0,0)
END pl_reset;

PROCEDURE control(no: INTEGER; VAL a: ARRAY OF sys.WORD): INTEGER;

VAR  er: INTEGER;
      i: INTEGER;
tptrptr: POINTER TO POINTER TO dpl.STATE;

BEGIN
  i:=HIGH(a);
  WITH pls DO
    CASE no OF
      |dpl._info     : IF (i<0) OR (a[0]<=0) THEN RETURN err.bad_parm END;
                       tptrptr :=sys.ADDRESS(a[0]);
                       tptrptr^:=sys.ADR(pls);
                       RETURN ok

      |dpl._reset    : pl_reset; RETURN ok
      |dpl._up       : pls.pen_dw:=0; RETURN err.ok;
      |dpl._down     : pls.pen_dw:=1; RETURN err.ok;
      |dpl._set_pen  : RETURN ok
      |dpl._set_pos  :
                       IF i<1 THEN RETURN err.bad_parm END;
                       IF pls.pen_dw=1 THEN line(pls.x,pls.y,a[0],a[1])
                       ELSE   pls.x:=a[0]; pls.y:=a[1];
                       END;
                       RETURN ok
      |dpl._origin   : IF i<1 THEN RETURN err.bad_parm END;
                       vg_ori(a[0],a[1]); RETURN ok
      |dpl._circ     : IF i<2 THEN RETURN err.bad_parm END;
                       circ(a[0],a[1],a[2]);
                       RETURN ok
      |dpl._arc      : IF i<6 THEN RETURN err.bad_parm END;
                       arc(a[0],a[1],a[2],a[3],a[4],a[5],a[6]);
                       RETURN ok
      |dpl._set_speed,
       dpl._set_resol: IF (i<0) OR (a[0]#0) THEN RETURN err.bad_parm END
    ELSE
      RETURN err.inv_op
    END
  END
END control;

PROCEDURE pl_doio(VAR r: req.REQUEST);
  VAR buf: DYNARR OF sys.WORD; str: DYNARR OF CHAR;
BEGIN
  r.res:=ok;
  CASE r.op MOD 256 OF
    |req.NOP    :
    |req.CONTROL:
      buf^.ADR:=r.buf; buf^.HIGH:=r.len-1;
      r.res:=control(r.op DIV 256,buf)
  ELSE
    r.res:=err.inv_op
  END
END pl_doio;

PROCEDURE halt;
BEGIN IF fs.remove_driver(arg.words[0])#ok THEN END END halt;

PROCEDURE init(VAL nm: ARRAY OF CHAR);
  VAR r: INTEGER;
BEGIN
  pl_reset;
  r:=fs.define_driver(nm,'',0,fs.spec,pl_doio);
  IF r#ok THEN ASSERT(FALSE,r) END;
  env.put_str(env.info,nm,TRUE)
END init;

VAR r: INTEGER;

BEGIN
  IF (HIGH(arg.words)<0) OR (HIGH(arg.words[0])>7) THEN HALT(1) END;
  init(arg.words[0]);
  env.become_ipr;
  os.suspend(os.active(),-1)
END PLvgBM.
