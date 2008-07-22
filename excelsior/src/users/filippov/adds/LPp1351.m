MODULE LPp1351; (*$U+ Leg 20-Feb-91. (c) KRONOS *)

IMPORT  sys: SYSTEM;            IMPORT  err: defErrors;
IMPORT   fs: osFiles;           IMPORT   os: osKernel;
IMPORT  dlp: defPrinter;        IMPORT  env: tskEnv;
IMPORT  req: defRequest;        IMPORT  bio: BIO;
IMPORT  cod: defCodes;          IMPORT  low: lowLevel;
IMPORT  arg: tskArgs;

(* instalation  LPlx800 dev_name file_name *)

CONST ok  = err.ok;
            --юабцдефгхийклмнопярстужвьызшэщчъ
       tran= '~`avdetcuhijklmno pqrsfb|{gx}ywz'
             '^@AVDETCUHIJKLMNO_PQRSFB\[GX]YWZ';
            --ЮАБЦДЕФГХИЙКЛМНОПЯРСТУЖВЬЫЗШЭЩЧЪ

VAR dev: bio.FILE;
    lps: dlp.STATE;
    cyr: BOOLEAN;

PROCEDURE out(VAL s: ARRAY OF CHAR; l: INTEGER);
BEGIN bio.write(dev,sys.ADR(s),l) END out;

PROCEDURE lp_reset(send: BOOLEAN): INTEGER;

CONST reset = ''33c 'J' 33c '"' 33c '*1';

BEGIN
  cyr:=FALSE;
  IF send THEN
    out(reset,HIGH(reset));
    IF NOT bio.done THEN RETURN bio.error END
  END;
  WITH lps DO
    something:=0;    underline:=0;    Wx2:=0; Hx2 :=0;
    reverse  :=0;    raw      :=0;    awp:=0; type:=3;
    fonts    :=1;    font     :=0;
    hchar    :=9;    wchar    :=dlp.WIDTH(-9);
    grounds  :=1;    ground   :=0;
    hbar     :=244c; vbar     :=245c;
    bars[0,0]:=240c; bars[0,1]:=246c;  bars[0,2]:=241c;
    bars[1,0]:=251c; bars[1,1]:=252c;  bars[1,2]:=247c;
    bars[2,0]:=243c; bars[2,1]:=250c;  bars[2,2]:=242c;
    densities:=1;    density:=1;
  END;
  RETURN ok
END lp_reset;

PROCEDURE Write(c: CHAR);
  VAR cy: BOOLEAN;
BEGIN
  cy:=c>=300c;
  IF (cy#cyr)&(c#' ')&(c#'.')&(c#',')&(c#'(')&(c#')') THEN
    IF cyr THEN
      cyr:=FALSE; out(''33c'*1',3);
    ELSE
      cyr:=TRUE;  out(''33c'*5',3);
    END;
  END;
  IF cy THEN
    IF c='я' THEN c:=CHAR(0A0h) ELSE c:=tran[ORD(c)-ORD('ю')] END;
  END;
  bio.fwrite(dev,sys.ADR(c),0,1)
END Write;

PROCEDURE ws(VAL s: ARRAY OF CHAR; pos,len: INTEGER);
BEGIN
  WHILE len>0 DO Write(s[pos]); DEC(len); INC(pos) END;
END ws;

PROCEDURE repeat(ch: CHAR; no: INTEGER): INTEGER;
  VAR i: INTEGER;
BEGIN
  FOR i:=1 TO no DO Write(ch) END;
  RETURN ok
END repeat;

CONST name='/usr/etc/t1351.prn';
      buf_sz=256;

VAR  Cbuf   : ARRAY [0..buf_sz-1] OF CHAR;

PROCEDURE LoadFont;
  VAR file: bio.FILE;
       eof: INTEGER;
       cnt: INTEGER;
BEGIN
  bio.open(file,name,'r');           IF NOT bio.done THEN RETURN END;
  eof:=bio.eof(file);                IF NOT bio.done THEN RETURN END;
  WHILE eof>0 DO
    cnt:=eof;
    IF cnt>buf_sz THEN cnt:=buf_sz END;
    bio.get(file,Cbuf,cnt);
    bio.put(dev,Cbuf,cnt);
    DEC(eof,cnt)
  END;
END LoadFont;

PROCEDURE p1351(no: INTEGER; VAL x: ARRAY OF sys.WORD): INTEGER;

VAR er: INTEGER;

  PROCEDURE ss(op: INTEGER; SEQ a: sys.WORD);
  BEGIN
    op:=p1351(op,a);
    IF op#ok THEN er:=op END;
  END ss;

  PROCEDURE lp_restore(VAR old: dlp.STATE);
  BEGIN
   er:=ok;
   WITH old DO
     IF lps.something#something THEN
       ss(dlp._something,something);
     END;
     IF lps.underline#underline THEN
       ss(dlp._underline,underline);
     END;
     IF lps.Wx2#Wx2 THEN
       ss(dlp._Wx2,Wx2);
     END;
   END;
  END lp_restore;

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
      |dlp._reset    : RETURN lp_reset(TRUE);
      |dlp._restore  : IF i<=0 THEN RETURN err.bad_parm END;
                       adr:=i; tptr:=adr; lp_restore(tptr^);
                       RETURN ok
      |dlp._something,
       dlp._underline:
        underline:=i0; something:=underline;
        IF i0#0 THEN out(''33c'I',2) ELSE out(''33c'J',2) END
      |dlp._Wx2      :
        Wx2:=i0; IF i0#0 THEN out(''33c'!',2) ELSE out(''33c'"',2) END
      |dlp._Hx2      : IF i0#0 THEN RETURN err.bad_parm END

      |dlp._autowrap : IF i0#0 THEN RETURN err.bad_parm END
      |dlp._raw      : IF i0=0 THEN RETURN err.bad_parm END
      |dlp._reverse  : IF i0#0 THEN RETURN err.bad_parm END
      |dlp._ground   : IF i0#0 THEN RETURN err.bad_parm END
      |dlp._density  : IF i0#0 THEN RETURN err.bad_parm END
      |dlp._font     : IF (i0#0) & (i0#48) THEN RETURN err.bad_parm END
      |dlp._load_font: LoadFont;
      |dlp._back     : RETURN err.inv_op
      |dlp._fwd      : RETURN err.inv_op
      |dlp._left     : RETURN err.inv_op
      |dlp._right    : RETURN err.inv_op
      |dlp._bflf     : out(''33c'VP@H',5)
      |dlp._bhlf     : out(''33c'VP@D',5)
      |dlp._fhlf     : out(''33c'V@@D',5)
      |dlp._fflf     : out(''33c'V@@H',5)
      |dlp._write_ln : out(''12c''15c,2)
      |dlp._eject    : out(''14c,1)
      |dlp._repeat   : RETURN repeat(CHAR(i0),i1)
      |dlp._paint    : RETURN err.inv_op
      |dlp._set_attr : RETURN err.inv_op
      |dlp._get_attr : RETURN err.inv_op
    ELSE
      RETURN err.inv_op
    END
  END;
  IF bio.done THEN RETURN ok END;
  RETURN bio.error
END p1351;

PROCEDURE lp_doio(VAR r: req.REQUEST);
  VAR arg: DYNARR OF sys.WORD; str: DYNARR OF CHAR;
BEGIN
  WITH r DO
    res:=ok;
    CASE op MOD 256 OF
      |req.NOP    :
      |req.CONTROL:
        arg^.ADR:=buf; arg^.HIGH:=len-1;
        res:=p1351(op DIV 256,arg)
      |req.LOCK  :
      |req.UNLOCK:
      |req.WRITE :
        str^.ADR:=buf; str^.HIGH:=pos+len-1;
        ws(str,pos,len);
        IF NOT bio.done THEN res:=bio.error END;
    ELSE
    END;
  END;
END lp_doio;

PROCEDURE halt;
BEGIN IF fs.remove_driver(arg.words[0])#ok THEN END; bio.close(dev) END halt;

PROCEDURE init(VAL nm,p: ARRAY OF CHAR);
  VAR r: INTEGER; c: CHAR;
BEGIN
  LoadFont;
  bio.open(dev,p,'w');
  IF NOT bio.done THEN HALT(bio.error) END;
  r:=fs.define_driver(nm,'',0,fs.spec,lp_doio);
  IF r#ok THEN HALT(r) END;
  env.put_str(env.info,nm,TRUE);
  env.become_ipr;
  IF lp_reset(FALSE)=ok THEN END;
END init;

BEGIN
  cyr:=FALSE;
  IF (HIGH(arg.words)<1) OR (HIGH(arg.words[0])>7) THEN
    HALT(err.bad_parm)
  END;
  env.final(halt);
  init(arg.words[0],arg.words[1]);
  os.suspend(os.active(),-1)
END LPp1351.
