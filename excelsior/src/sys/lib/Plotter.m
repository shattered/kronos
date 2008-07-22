IMPLEMENTATION MODULE Plotter; (* Leg 17-Jun-90. (c) KRONOS *)
                               (* Leg 21-Nov-91. (c) KRONOS *)

IMPORT  sys: SYSTEM;
IMPORT  cod: defCodes;
IMPORT  def: defPlotter;
IMPORT  err: defErrors;
IMPORT  req: defRequest;
IMPORT  fmt: Formats;
IMPORT  bio: BIO;
IMPORT  env: tskEnv;
IMPORT  low: lowLevel;

WITH STORAGE (NEW    : env.allocate;
              DISPOSE: env.deallocate;
              RESIZE : env.reallocate);

CONST ok  = err.ok;
      MAG = 120b + 154b<<8 + 106b<<16 + 156b>>8; -- "PlFn"

TYPE WORD  = sys.WORD;
     LINES = DYNARR OF INTEGER;
     FONT  = POINTER TO RECORD
               mag : LONGINT;
               w,h : INTEGER;
               patt: ARRAY CHAR OF LINES
             END;

VAR  file: bio.FILE;
     doio: PROCEDURE(bio.FILE, VAR ARRAY OF WORD);
    Ss,Sc: LONGINT; -- slope  sin & cos
    Is,Ic: LONGINT; -- italic sin & cos
    dummy: STATUS;

PROCEDURE ioctl(op: INTEGER; SEQ args: WORD);
  VAR r: req.REQUEST;
BEGIN
  r.op:=req.CONTROL+op*256;
  r.buf:=sys.ADR(args); r.len:=HIGH(args)+1; r.ofs:=0; r.pos:=0;
  doio(file,r);
  done:=bio.done;
  IF NOT done THEN error:=bio.error END
END ioctl;

PROCEDURE bioctl(file: bio.FILE; op: INTEGER; SEQ args: WORD);
  VAR r: req.REQUEST;
BEGIN
  r.op:=req.CONTROL+op*256;
  r.buf:=sys.ADR(args); r.len:=HIGH(args)+1; r.ofs:=0; r.pos:=0;
  bio.doio(file,r);
  done:=bio.done;
  IF NOT done THEN error:=bio.error END
END bioctl;

PROCEDURE start(xxx: bio.FILE; VAR r: ARRAY OF WORD);
  VAR info: def.STATE;
BEGIN
  doio:=bio.doio;   ioctl(def._reset);
  doio(file,r);     done:=bio.done;
  IF NOT done THEN error:=bio.error END
END start;

PROCEDURE attach(VAL name: ARRAY OF CHAR);
  VAR new: bio.FILE;  s: STATE;
BEGIN
  bio.open(new,name,'w');
  done:=bio.done;
  IF NOT done THEN error:=bio.error; RETURN END;
  bioctl(new,def._info,sys.ADR(s)); done:=bio.done;
  IF NOT done THEN bio.close(new); RETURN END;
  IF file#bio.null THEN bio.close(file) END;
  file:=new;  doio:=start;  state:=s
END attach;

PROCEDURE START(xxx: bio.FILE; VAR r: ARRAY OF WORD);
  VAR s: STRING;
BEGIN
  env.get_str(env.plot,s); done:=env.done;
  IF done THEN attach(s) ELSE error:=env.error; RETURN END;
  IF done THEN start(file,r) END
END START;

----------------------------- MISC -----------------------------
                             ------

PROCEDURE mul_div(w: LONGINT; mul,div: LONGINT): LONGINT;
BEGIN
  IF (MAX(LONGINT) DIV w)<mul THEN
    IF w>mul THEN w:=(w DIV div)*mul ELSE w:=(mul DIV div)*w END
  ELSE w:=(w*mul) DIV div
  END;
  RETURN w
END mul_div;

PROCEDURE slope(VAR x,y: LONGINT);
  VAR a,b: LONGINT;
BEGIN
  a:=x; b:=y;                                 -- /x\   / sin  cos \ /a\
  x:=mul_div(a,Sc,1000)-mul_div(b,Ss,1000);   -- | | = |          | | |
  y:=mul_div(a,Ss,1000)+mul_div(b,Sc,1000)    -- \y/   \ cos -sin / \b/
END slope;

PROCEDURE sqrt(a: LONGINT): LONGINT;
  VAR s,b: LONGINT;
BEGIN
  ASSERT(a>=0);
  IF a IN {0,1} THEN RETURN a END;
  s:=2; b:=a DIV s;
  WHILE (ABS(s-b)>1) DO
    s:=(a DIV b + b) DIV 2;
    b:=a DIV s
  END;
  IF s>b THEN RETURN s ELSE RETURN b END
END sqrt;

------------------------------ PEN -----------------------------
                              -----

PROCEDURE up;  BEGIN ioctl(def._up)   END up;
PROCEDURE dw;  BEGIN ioctl(def._down) END dw;

PROCEDURE set_pen(no: INTEGER(*CARDINAL*)); (* change pen *)
BEGIN ioctl(def._set_pen,no) END set_pen;

-------------------------- COORDINATES -------------------------
                          -------------

PROCEDURE origin(x,y: LONGINT); BEGIN ioctl(def._origin,x,y) END origin;

PROCEDURE pos(x,y: LONGINT);    BEGIN ioctl(def._set_pos,x,y) END pos;


-------------------------- PRIMITIVES --------------------------
                          ------------

PROCEDURE line(x0,y0,x1,y1: LONGINT);
BEGIN
  IF (x0#state^.x) OR (y0#state^.y) THEN up; pos(x0,y0) END;
  dw; pos(x1,y1)
END line;

PROCEDURE frame(x0,y0,x1,y1: LONGINT);
BEGIN
  IF (x0#state^.x) OR (y0#state^.y) THEN up; pos(x0,y0) END;
  dw; pos(x1,y0); pos(x1,y1); pos(x0,y1); pos(x0,y0)
END frame;

PROCEDURE circ(x,y,r: LONGINT);
BEGIN ioctl(def._circ,x,y,r) END circ;

PROCEDURE arc (x,y,xl,yl,xr,yr,r: LONGINT);
BEGIN ioctl(def._arc,x,y,xl,yl,xr,yr,r) END arc;

PROCEDURE arc3(X0,Y0,X1,Y1,X2,Y2: LONGINT);
  VAR x,y,xl,yl,xr,yr,rds: LONGINT;

  PROCEDURE get_arc (): BOOLEAN;
    VAR xc,yc,t: LONGINT;

    PROCEDURE distance(x,y,x1,y1: LONGINT): LONGINT;
    BEGIN x:=ABS(x1-x); y:=ABS(y1-y); RETURN sqrt(x*x+y*y) END distance;

    PROCEDURE calc(VAR xc,yc,r: LONGINT; x1,y1,x2,y2,x3,y3: LONGINT): BOOLEAN;
      VAR a1,a2,b1,b2,c1,c2,det: LONGINT;
    BEGIN
      a1:=x1-x2; a2:=x1-x3; b1:=y1-y2; b2:=y1-y3;
      c1:=x1*x1-x2*x2 + y1*y1-y2*y2;
      c2:=x1*x1-x3*x3 + y1*y1-y3*y3;
      det:=2*(a1*b2-a2*b1); IF det=0 THEN RETURN TRUE END;
      xc:=(c1*b2-c2*b1) DIV det; yc:=(a1*c2-a2*c1) DIV det;
      r:=distance(x1,y1,xc,yc); RETURN FALSE
    END calc;

  BEGIN
    xl:=X0; yl:=Y0; xc:=X1; yc:=Y1; xr:=X2; yr:=Y2; X1:=X2; Y1:=Y2;
    IF calc(x,y,rds,xl,yl,xc,yc,xr,yr) THEN RETURN TRUE END;;
    IF ((xl-xr)*(yc-yr)-(xc-xr)*(yl-yr))<0 THEN -- sin between (xc,yc) & (xl,yl)
      t:=xl; xl:=xr; xr:=t; t:=yl; yl:=yr; yr:=t
    END;
    RETURN FALSE
  END get_arc;

BEGIN
  IF get_arc() THEN line(X0,Y0,X1,Y1); RETURN END;
  arc(x,y,xl,yl,xr,yr,rds)
END arc3;


----------------------------- FONTS ----------------------------
                             -------

PROCEDURE load_font(VAR f: FONT; name: ARRAY OF CHAR);
  VAR hi,i: INTEGER(*CARDINAL*);
      file: bio.FILE;
      ch  : CHAR;

  PROCEDURE e(): BOOLEAN;
  BEGIN done:=bio.done; error:=bio.error; RETURN NOT done END e;

BEGIN
  done:=TRUE;
  bio.open(file,name,'r'); IF e() THEN RETURN END;
  NEW(f);
  done:=env.done; IF NOT done THEN error:=env.error; RETURN END;
  WITH f^ DO
    bio.read(file,sys.ADR(f^.w),4); IF e() THEN RETURN END;
    bio.read(file,sys.ADR(f^.h),4); IF e() THEN RETURN END;
    FOR ch:=0c TO 377c DO NEW(patt[ch]) END;
    FOR ch:=0c TO 377c DO
      bio.read(file,sys.ADR(hi),4); IF e() THEN RETURN END;
      IF hi>=0 THEN
        NEW(patt[ch],hi+1);
        done:=env.done;
        IF NOT done THEN
          error:=env.error;
          NEW(patt[ch]);
          dispose_font(f);
          RETURN
        END;
        bio.read(file,sys.ADR(patt[ch]),BYTES(patt[ch]));
        IF e() THEN RETURN END
      END
    END;
    mag:=MAG
  END
END load_font;

PROCEDURE dispose_font(VAR fnt: FONT);
  VAR i: CHAR;
BEGIN
  done:=TRUE; IF fnt=NIL THEN RETURN END;
  done:=font^.mag=MAG;
  IF NOT done THEN error:=err.bad_desc; RETURN END;
  WITH fnt^ DO
    mag:=0;
    FOR i:=0c TO 377c DO DISPOSE(patt[i]) END
  END;
  DISPOSE(fnt);
  fnt:=NIL
END dispose_font;

PROCEDURE set_size(w,h: LONGINT);
BEGIN done:=TRUE; W:=w; H:=h END set_size;

PROCEDURE norm(VAR x,y: LONGINT);
BEGIN
  WHILE (x+y)<30000 DO x:=x  *  2; y:=y  *  2 END;
  WHILE (x+y)>60000 DO x:=x DIV 2; y:=y DIV 2 END
END norm;

PROCEDURE set_slope(x,y: LONGINT);
  VAR l: LONGINT;
BEGIN
  done:=TRUE;
  norm(x,y);
  Sx:=x; Sy:=y;
  IF (x=0) & (y=0) THEN Sc:=1; Ss:=0; RETURN END;
  l:=sqrt(x*x+y*y);
  Sc:=mul_div(x,1000,l);
  Ss:=mul_div(y,1000,l)
END set_slope;

PROCEDURE set_ital (x,y: LONGINT);
  VAR l: LONGINT;
BEGIN
  done:=TRUE;
  norm(x,y);
  Ix:=x; Iy:=y;
  IF (x=0) & (y=0) THEN Ic:=1; Is:=0; RETURN END;
  l:=sqrt(x*x+y*y);
  Ic:=mul_div(x,1000,l);
  Is:=mul_div(y,1000,l)
END set_ital;

PROCEDURE set_font(fnt: FONT);
BEGIN
  done:=(fnt#NIL) & (fnt^.mag=MAG);
  IF done THEN font:=fnt END
END set_font;

----------------------------- TEXTS ----------------------------
                             -------

PROCEDURE write0(x,y: LONGINT; ch: CHAR);

  PROCEDURE compute(VAR a,b,a1,b1: LONGINT);
  BEGIN
    WITH font^ DO
      a:=mul_div(a,W,w); a1:=mul_div(a1,W,w);
      b:=mul_div(b,H,h); b1:=mul_div(b1,H,h);
      a :=a +mul_div(b ,Ic,1000); b :=mul_div(b ,Is,1000);
      a1:=a1+mul_div(b1,Ic,1000); b1:=mul_div(b1,Is,1000);
      slope(a,b); slope(a1,b1)
    END
  END compute;

VAR i,w,a,b,A,B: LONGINT;

BEGIN
  FOR i:=0 TO HIGH(font^.patt[ch]) DO
    w:=font^.patt[ch][i];
    a:=LONGINT(BITSET(w)*{0..7}); w:=w >> 8;
    b:=LONGINT(BITSET(w)*{0..7}); w:=w >> 8;
    A:=LONGINT(BITSET(w)*{0..7}); w:=w >> 8;
    B:=LONGINT(BITSET(w)*{0..7});
    compute(a,b,A,B);
    line(a+x,b+y,A+x,B+y);
  END
END write0;

PROCEDURE write(ch: CHAR);
  VAR x,y,dx,dy: LONGINT;
BEGIN
  done:=(font#NIL) & (font^.mag=MAG);
  IF NOT done THEN error:=err.bad_desc; RETURN END;
  x:=state^.x; y:=state^.y;
  write0(x,y,ch);
  dx:=W; dy:=0;
  slope(dx,dy);
  up; pos(x+dx,y+dy)
END write;

PROCEDURE write_str(VAL s: ARRAY OF CHAR);
  VAR i,x,y,dx,dy: LONGINT;
BEGIN
  done:=(font#NIL) & (font^.mag=MAG);
  IF NOT done THEN error:=err.bad_desc; RETURN END;
  i:=0; x:=state^.x; y:=state^.y;
  WHILE (i<=HIGH(s)) & (s[i]#0c) DO
    dx:=W*i; dy:=0; slope(dx,dy);
    write0(x+dx,y+dy,s[i]); INC(i)
  END;
  dx:=W*i; dy:=0; slope(dx,dy);
  up; pos(x+dx,y+dy)
END write_str;

PROCEDURE ws(X: WORD; VAL s: ARRAY OF CHAR; ps,ln: LONGINT);
  VAR i,x,y,dx,dy: LONGINT;
BEGIN
  x:=state^.x; y:=state^.y;
  FOR i:=ps TO ps+ln-1 DO
    dx:=W*i; dy:=0; slope(dx,dy);
    write0(x+dx,y+dy,s[i])
  END;
  dx:=W*i; dy:=0; slope(dx,dy);
  up; pos(x+dx,y+dy)
END ws;

PROCEDURE print(VAL format: ARRAY OF CHAR; SEQ args: WORD);
BEGIN
  done:=(font#NIL) & (font^.mag=MAG);
  IF NOT done THEN error:=err.bad_desc; RETURN END;
  fmt.format(0,ws,format,args)
END print;

PROCEDURE cwrite(x,y: LONGINT; ch: CHAR);
BEGIN write0(x,y,ch) END cwrite;

PROCEDURE cwrite_str(x,y: LONGINT; VAL s: ARRAY OF CHAR);
BEGIN
  IF (x#state^.x) OR (0#state^.y) THEN up; pos(x,y) END;
  write_str(s)
END cwrite_str;

PROCEDURE cprint(x,y: LONGINT; format: ARRAY OF CHAR; SEQ a: WORD);
BEGIN
  IF (x#state^.x) OR (0#state^.y) THEN up; pos(x,y) END;
  print(format,a)
END cprint;

---------------------------- SPECIAL ---------------------------
                            ---------

PROCEDURE set_speed(n: INTEGER(*CARDINAL*));
BEGIN
  ioctl(def._set_speed,n)
END set_speed;

PROCEDURE set_resol(n: INTEGER(*CARDINAL*));
BEGIN
  ioctl(def._set_resol,n)
END set_resol;

PROCEDURE reset; BEGIN ioctl(def._reset) END reset;

PROCEDURE nop; VAR r: req.REQUEST;
BEGIN r.op:=req.NOP; doio(file,r) END nop;

----------------------------------------------------------------

PROCEDURE init;
BEGIN
  W:=10; H:=10; Sc:=1000; Ss:=0; Ic:=0; Is:=1000;
  low.zero(dummy); error:=ok; done:=TRUE;
  state:=sys.ADR(dummy);
  file:=bio.null; doio:=START; error:=ok
END init;

BEGIN
  init
END Plotter.
