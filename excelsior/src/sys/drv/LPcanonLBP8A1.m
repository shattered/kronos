MODULE LPcanonLBP8A1; (*$U+ Leg 19-Sep-90. (c) KRONOS *)

IMPORT  sys: SYSTEM;            IMPORT  err: defErrors;
IMPORT   fs: osFiles;           IMPORT   os: osKernel;
IMPORT  dlp: defPrinter;        IMPORT  env: tskEnv;
IMPORT  req: defRequest;        IMPORT  bio: BIO;
IMPORT  arg: tskArgs;           IMPORT  tty: Terminal;

(* instalation  &LPcanonLBP8A1 dev_name file_name *)

CONST ok  = err.ok;

VAR dev  : bio.FILE;
    lps  : dlp.STATE;

PROCEDURE out(VAL s: ARRAY OF CHAR; len: INTEGER);
BEGIN bio.write(dev,sys.ADR(s),len); END out;

PROCEDURE lp_reset(): INTEGER;

BEGIN
  out(''33c'<',2); IF NOT bio.done THEN RETURN bio.error END;
  WITH lps DO
    type     := 1;
    fonts    := 4; font  :=0;
    hchar    :=44; wchar :=dlp.WIDTH(-40);
    something:=0; underline:=0; Wx2      :=0; Hx2      :=0;
    reverse  :=0; raw      :=0; awp      :=0;
    grounds  :=2 ;   ground   :=0;
    hbar     :='-'; vbar     :='|';
    bars[0,0]:='+'; bars[0,1]:='+'; bars[0,2]:='+';
    bars[1,0]:='+'; bars[1,1]:='+'; bars[1,2]:='+';
    bars[2,0]:='+'; bars[2,1]:='+'; bars[2,2]:='+';
    densities:=1;  density:=300;
  END;
  RETURN ok
END lp_reset;

PROCEDURE canon(no: INTEGER; VAL x: ARRAY OF sys.WORD): INTEGER;

VAR er: INTEGER;

  PROCEDURE ss(op: INTEGER; SEQ a: sys.WORD);
  BEGIN
    er:=ok; op:=canon(op,a); IF op#ok THEN er:=op END;
  END ss;

  PROCEDURE lp_restore(VAR old: dlp.STATE);
  BEGIN
   er:=ok;
   WITH old DO
     IF density      #300       THEN density:=300 END;
     IF lps.font     #font      THEN ss(dlp._font,      font )     END;
     IF lps.underline#underline THEN ss(dlp._underline, underline) END;
     IF lps.Wx2      #Wx2       THEN ss(dlp._Wx2,       Wx2)       END;
     IF lps.Hx2      #Hx2       THEN ss(dlp._Hx2,       Hx2)       END;
     IF lps.reverse  #reverse   THEN ss(dlp._reverse,   reverse)   END;
     IF lps.raw      #raw       THEN ss(dlp._raw,       raw)       END;
     IF lps.awp      #awp       THEN ss(dlp._autowrap,  awp)       END;
     IF lps.ground   #ground    THEN ss(dlp._ground,    ground)    END;
   END;
  END lp_restore;

  PROCEDURE set_font(f: INTEGER): INTEGER;
  BEGIN RETURN 0 END set_font;

  PROCEDURE repeat(ch: CHAR; no: INTEGER): INTEGER;
    VAR len,i: INTEGER;
        str  : ARRAY [0..63] OF CHAR;
  BEGIN
    IF no<=0 THEN RETURN ok END;
    len:=BYTES(str);
    IF len>no THEN len:=no END;
    FOR i:=0 TO len-1 DO str[i]:=ch END;
    WHILE no>0 DO
      i:=len; IF i>no THEN i:=no END;
      bio.write(dev,sys.ADR(str),i);
      IF NOT bio.done THEN RETURN bio.error END;
      DEC(no,i);
    END;
    RETURN ok
  END repeat;

  VAR adr : sys.ADDRESS;
      tptr: POINTER TO dlp.STATE;
      tptt: POINTER TO POINTER TO dlp.STATE;
      i,i0,i1: INTEGER;

BEGIN
  i0:=-1; i1:=-1;
  IF HIGH(x)>=0 THEN i0:=x[0] END; i:=i0;
  IF HIGH(x)>=1 THEN i1:=x[1] END;
  WITH lps DO
    CASE no OF
      |dlp._info     : IF i<=0 THEN RETURN err.bad_parm END;
                       adr:=i; tptt:=adr; tptt^:=sys.ADR(lps);
                       RETURN ok
      |dlp._reset    : RETURN lp_reset()
      |dlp._restore  : IF i<=0 THEN RETURN err.bad_parm END;
                       adr:=i; tptr:=adr; lp_restore(tptr^);
                       RETURN er
      |dlp._repeat   : RETURN repeat(CHAR(i0),i1)
      |dlp._underline:
        IF i0#0 THEN out(''33c'[4m',4)
        ELSE         out(''33c'[24m',5)
        END;
        underline:=i0
      |dlp._Wx2,dlp._Hx2:
        IF no=dlp._Wx2 THEN Wx2:=i0 ELSE Hx2:=i0 END;
        i0:=0;
        IF Wx2#0 THEN INC(i0  ) END;
        IF Hx2#0 THEN INC(i0,2) END;
        CASE i0 OF
          |0: out(''33c'[100;100 B',11);
          |1: out(''33c'[100;200 B',11);
          |2: out(''33c'[200;100 B',11);
          |3: out(''33c'[200;200 B',11);
        END;
      |dlp._autowrap :
        IF i0#0 THEN out(''33c'[?31',5);
        ELSE         out(''33c'[?3h',5);
        END; awp:=i0
      |dlp._raw      : RETURN err.inv_op
      |dlp._reverse  :
        IF i0#0 THEN out(''33c'[7m',4);
        ELSE         out(''33c'[27m',5);
        END;
        reverse:=i0
      |dlp._ground
      ,dlp._something:
        IF i0#ground THEN
          IF i IN {0..1} THEN
            ground:=i0;
            IF i0=0 THEN out(''33c'[25m',5)
            ELSE         out(''33c'[5m' ,4)
            END;
          ELSE RETURN err.bad_parm
          END;
        END;
      |dlp._density  : RETURN err.inv_op;
      |dlp._font     : RETURN err.inv_op;
      |dlp._load_font: RETURN err.inv_op
      |dlp._back     : RETURN err.inv_op
      |dlp._fwd      : RETURN err.inv_op
      |dlp._left     : RETURN err.inv_op
      |dlp._right    : RETURN err.inv_op
      |dlp._bflf     : out(''214c 214c,2)
      |dlp._bhlf     : out(''214c,1)
      |dlp._fhlf     : out(''213c,1)
      |dlp._fflf     ,
       dlp._write_ln : out(''12c 15c,2)
      |dlp._eject    : out(''14c,1)
      |dlp._paint    : RETURN err.inv_op
      |dlp._set_attr : RETURN err.inv_op
      |dlp._get_attr : RETURN err.inv_op
    ELSE
      RETURN err.inv_op
    END
  END;
  IF bio.done THEN RETURN ok END;
  RETURN bio.error
END canon;

PROCEDURE lp_doio(VAR r: req.REQUEST);
  VAR arg: DYNARR OF sys.WORD;
BEGIN
  WITH r DO
    res:=ok;
    CASE op MOD 256 OF
      |req.NOP    :
      |req.CONTROL:
        arg^.ADR:=buf; arg^.HIGH:=len-1;
        res:=canon(op DIV 256,arg)
      |req.WRITE : bio.fwrite(dev,buf,pos,len);
                   IF NOT bio.done THEN res:=bio.error END
    ELSE
    END;
  END;
END lp_doio;

PROCEDURE halt;
BEGIN IF fs.remove_driver(arg.words[0])#ok THEN END; bio.close(dev) END halt;

PROCEDURE init(VAL nm,fl: ARRAY OF CHAR);
  VAR r: INTEGER;
BEGIN
  bio.open(dev,fl,'a');
  IF NOT bio.done THEN ASSERT(FALSE,bio.error) END;
  IF lp_reset()#0 THEN END;
  r:=fs.define_driver(nm,'',0,fs.spec,lp_doio);
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
  os.suspend(os.active(),-1)
END LPcanonLBP8A1.

elit bold compr bot top

      simp
      bold
      compr
      bold compr
      elit
      elit bold
      elit compr
      elit bold compr
      bot
      bot compr
      top
      top compr
