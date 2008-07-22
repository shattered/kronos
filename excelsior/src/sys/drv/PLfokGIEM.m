MODULE PLfokGIEM; (*$U+ Leg 19-Jun-90. (c) KRONOS *)

IMPORT  sys: SYSTEM;            IMPORT  err: defErrors;
IMPORT   fs: osFiles;           IMPORT   os: osKernel;
IMPORT  dpl: defPlotter;        IMPORT  env: tskEnv;
IMPORT  req: defRequest;        IMPORT  bio: BIO;
IMPORT  arg: tskArgs;           IMPORT  tty: Terminal;

(* instalation  &PLfokGIEM dev_name file_name *)

CONST ok  = err.ok;
                        --      0.001" 0.1mm  0.125mm 0.005"
      Resols = ARRAY OF INTEGER {25400,100000,125000,127000};
                        --       EC1    ECM    ECO    EC5

                        --       25mm  50mm  75mm per second
      Speeds = ARRAY OF INTEGER {25000,50000,75000};
                        --        V1    V2    V3
VAR dev  : bio.FILE;
    pls  : dpl.STATE;

PROCEDURE out(VAL s: ARRAY OF CHAR; l: INTEGER);
BEGIN bio.write(dev,sys.ADR(s),l); END out;

PROCEDURE pl_reset(): INTEGER;

CONST reset = ';: U A 0,0 O EC0 V40 '; (* 21 bytes *)
              (* init absolute pos(0,0) origin set_size=0.125mm *)

BEGIN
  out(reset,21); IF NOT bio.done THEN RETURN bio.error END;
  WITH pls DO
    pls.type  :=1;
    pls.pens  :=1;
    pls.pen   :=0;
    pls.pen_dw:=0;
    pls.ox    :=0;
    pls.oy    :=0;
    pls.x     :=0;
    pls.y     :=0;
    pls.speeds:=3;
    pls.speed :=Speeds[1];
    pls.resols:=4;
    pls.resol :=Resols[2];
  END;
  RETURN ok
END pl_reset;

PROCEDURE pr(VAR s: ARRAY OF CHAR; VAR p: INTEGER; w: INTEGER);

  PROCEDURE place(d: INTEGER);
  BEGIN
    IF w=0 THEN RETURN END;
    w:=w DIV 10;
    place(w MOD 10);
    s[p]:=CHAR(d+ORD('0')); INC(p);
  END place;

BEGIN
  IF w=0 THEN s[p]:='0'; INC(p); RETURN END;
  place(w MOD 10);
END pr;

PROCEDURE set_pos(x,y: INTEGER);
  VAR str: ARRAY [0..31] OF CHAR;
      p  : INTEGER;

BEGIN
  pls.x:=x; pls.y:=y;
  INC(x,pls.ox); INC(y,pls.oy);
  p:=0;
  pr(str,p,x); str[p]:=','; INC(p);
  pr(str,p,y); str[p]:=' '; INC(p);
  out(str,p);
END set_pos;

PROCEDURE set_origin(a,b: INTEGER);
BEGIN
  WITH pls DO
    x:=x+ox; y:=y+oy;
    ox:=a  ; oy:=b  ;
    x:=x-ox; y:=y-oy;
  END;
END set_origin;

PROCEDURE circ(a,b,r: INTEGER);
  VAR s: ARRAY [0..47] OF CHAR;
      p: INTEGER;
BEGIN
  WITH pls DO
    x:=a; y:=b;
    INC(a,pls.ox); INC(b,pls.oy)
  END;
  s:='CC '; p:=3;
  pr(s,p,a); s[p]:=','; INC(p);
  pr(s,p,b); s[p]:=' '; INC(p);
  pr(s,p,r); s[p]:=' '; INC(p);
  out(s,p);
END circ;

PROCEDURE arc(x0,y0,xl,yl,xr,yr,r: INTEGER);
BEGIN
  WITH pls DO
    x:=xr; y:=xr;
    INC(x0,ox); INC(y0,oy);
    INC(xl,ox); INC(yl,oy);
    INC(xr,ox); INC(yr,oy)
  END
END arc;

PROCEDURE set_speed(a: INTEGER): INTEGER;
BEGIN
  CASE a OF
    |0: out('V1 ',3);
    |1: out('V2 ',3);
    |2: out('V3 ',3);
  ELSE
    RETURN err.bad_parm
  END;
  pls.speed:=Speeds[a];
  RETURN err.ok
END set_speed;

PROCEDURE set_resol(a: INTEGER): INTEGER;
BEGIN
  CASE a OF
    |0: out('EC1 ',4);
    |1: out('ECM ',4);
    |2: out('ECO ',4);
    |3: out('EC5 ',4);
  ELSE
    RETURN err.bad_parm
  END;
  pls.resol:=Resols[a];
  RETURN err.ok
END set_resol;

PROCEDURE control(no: INTEGER; VAL a: ARRAY OF INTEGER): INTEGER;

  VAR i: INTEGER;
     er: INTEGER;
tptrptr: POINTER TO POINTER TO dpl.STATE;

BEGIN
  i:=HIGH(a);
  WITH pls DO
    CASE no OF
      |dpl._info     : IF i<0 THEN RETURN err.bad_parm END;
                       tptrptr :=sys.ADDRESS(a[0]);
                       tptrptr^:=sys.ADR(pls);
                       RETURN ok
      |dpl._reset    : RETURN pl_reset()
      |dpl._up       : out('U ',2); pls.pen_dw:=0
      |dpl._down     : out('D ',2); pls.pen_dw:=1
      |dpl._set_pen  : IF i<0 THEN RETURN err.bad_parm END;
                       IF a[0]#0 THEN RETURN err.bad_parm
                       ELSE RETURN err.ok END
      |dpl._set_pos  : IF i<1 THEN RETURN err.no_data END;
                       set_pos(a[0],a[1])
      |dpl._origin   : IF i<1 THEN RETURN err.no_data END;
                       set_origin(a[0],a[1])
      |dpl._circ     : IF i<2 THEN RETURN err.no_data END;
                       circ(a[0],a[1],a[2])
      |dpl._arc      : IF i<6 THEN RETURN err.no_data END;
                       arc(a[0],a[1],a[2],a[3],a[4],a[5],a[6]);
      |dpl._set_speed: RETURN set_speed(a[0]);
      |dpl._set_resol: RETURN set_resol(a[0]);
    ELSE
      RETURN err.inv_op
    END
  END;
  IF bio.done THEN RETURN ok END;
  RETURN bio.error
END control;

PROCEDURE pl_doio(VAR r: req.REQUEST);
  VAR buf: DYNARR OF INTEGER; str: DYNARR OF CHAR;
BEGIN
  r.res:=ok;
  CASE r.op MOD 256 OF
    |req.NOP    :
    |req.CONTROL:
      buf^.ADR:=r.buf; buf^.HIGH:=r.len-1;
      r.res:=control(r.op DIV 256,buf)
  ELSE
    r.res:=err.bad_parm
  END;
END pl_doio;

PROCEDURE halt;
BEGIN IF fs.remove_driver(arg.words[0])#ok THEN END; bio.close(dev) END halt;

PROCEDURE init(VAL nm,fl: ARRAY OF CHAR);
  VAR r: INTEGER;
BEGIN
  bio.open(dev,fl,'a');
  IF NOT bio.done THEN ASSERT(FALSE,bio.error) END;
  r:=pl_reset();
  IF r#ok THEN ASSERT(FALSE,r) END;
  r:=fs.define_driver(nm,'',0,fs.spec,pl_doio);
  IF r#ok THEN ASSERT(FALSE,r) END;
  env.put_str(env.info,nm,TRUE);
END init;

VAR r: INTEGER;

BEGIN
  IF HIGH(arg.words)<1    THEN tty.print('Not all parameters\n'); HALT(1) END;
  IF HIGH(arg.words[0])>7 THEN tty.print('Too long driver name\n'); HALT(1) END;
  env.final(halt);
  init(arg.words[0],arg.words[1]);
  env.become_ipr;
  os.suspend(os.active(),-1);
END PLfokGIEM.
