MODULE LPcm6329; (*$U+ Leg 22-Jan-90. (c) KRONOS *)

IMPORT  sys: SYSTEM;            IMPORT  err: defErrors;
IMPORT   fs: osFiles;           IMPORT   os: osKernel;
IMPORT  dlp: defPrinter;        IMPORT  env: tskEnv;
IMPORT  req: defRequest;        IMPORT  bio: BIO;
IMPORT  arg: tskArgs;           IMPORT  tty: Terminal;

(* instalation  &LP5CM6329 dev_name file_name *)

CONST ok  = err.ok;
      dns = ARRAY OF INTEGER {60,120,120,240,80,72,90};

VAR dev  : bio.FILE;
    lps  : dlp.STATE;
    c_dns: INTEGER;
    ccg  : INTEGER; --current character generator

PROCEDURE out(VAL s: ARRAY OF CHAR; l: INTEGER);
BEGIN bio.write(dev,sys.ADR(s),l); END out;

PROCEDURE lp_reset(): INTEGER;

CONST reset = ''33c 'R' 0c 33c 'R' 0c 30c 33c 'C' 310c 33c 'O'
                33c '8' 33c 'H' 33c 'F' 22c 24c 33c 'T' 33c '-' 0c
                33c 'W0' 33c 'A' 14c 33c 'M'; -- 33 bytes

BEGIN
  out(reset,33); IF NOT bio.done THEN RETURN bio.error END;
  ccg:=0;
  WITH lps DO
    something:=0;   underline:=0;   Wx2:=0; Hx2 :=0;
    reverse  :=0;   raw      :=0;   awp:=0; type:=0;
    fonts    :=12;  font     :=0;
    hchar    :=9;   wchar    :=dlp.WIDTH(-9);
    grounds  :=1;   ground   :=0;
    hbar     :='-'; vbar     :='|';
    bars[0,0]:='+'; bars[0,1]:='+'; bars[0,2]:='+';
    bars[1,0]:='+'; bars[1,1]:='+'; bars[1,2]:='+';
    bars[2,0]:='+'; bars[2,1]:='+'; bars[2,2]:='+';
    densities:=6;   c_dns:=5; density:=dns[c_dns];
  END;
  RETURN ok
END lp_reset;

VAR ccg_com : ARRAY [0..3] OF CHAR;

PROCEDURE ws(VAL s: ARRAY OF CHAR; pos,len: INTEGER);
  VAR f,t,h: INTEGER; ch: CHAR;
BEGIN
  f:=pos; t:=f; h:=pos+len-1;
  LOOP
    LOOP
      IF t>h THEN EXIT END;
      ch:=s[t];
      IF (ch=0c) OR ((ccg=0) & (ch>177c)) OR
         ((ccg=1) & (ch<200c)) THEN EXIT
      END;
      INC(t);
    END;
    IF t>f THEN bio.fwrite(dev,sys.ADR(s),f,t-f) END;
    IF t>h THEN RETURN END;
    f:=t;
    ccg:=(ccg+1) MOD 2; ccg_com[2]:=CHAR(ccg);
    out(ccg_com,3)
  END;
END ws;

PROCEDURE epson_rob(no: INTEGER; VAL x: ARRAY OF sys.WORD): INTEGER;

VAR er: INTEGER;

  PROCEDURE ss(oper: INTEGER; SEQ rand: sys.WORD);
  BEGIN
    oper:=epson_rob(oper,rand);
    IF oper#ok THEN er:=oper END;
  END ss;

  PROCEDURE lp_restore(VAR old: dlp.STATE);
  BEGIN
   er:=ok;
   WITH old DO
     IF density#dns[c_dns] THEN
       c_dns:=0;
       WHILE (c_dns<=HIGH(dns)) & (dns[c_dns]#density) DO INC(c_dns) END;
       IF c_dns>HIGH(dns) THEN c_dns:=5 END;
       lps.density:=dns[c_dns]
     END;
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

  PROCEDURE bck_fwd(i: INTEGER; d: CHAR): INTEGER;
    VAR s: ARRAY [0..3] OF CHAR;
  BEGIN
    WHILE i>0 DO
      IF i>255 THEN s[2]:=377c ELSE s[2]:=CHAR(i) END;
      out(s,3);
      DEC(i,255);
      IF NOT bio.done THEN RETURN bio.error END;
    END;
    RETURN ok
  END bck_fwd;

  PROCEDURE set_font(f: INTEGER): INTEGER;
    VAR i: INTEGER;
  BEGIN
    out('' 33c 'H' 33c 'F' 22c 33c 'T' 33c 'M',9); i:=f;
    IF NOT bio.done THEN RETURN bio.error END;
    IF (lps.something#0) & (f<7) & (NOT ODD(f)) THEN INC(i) END;
    (* IF something & font does not contain bold THEN include bold *)
    CASE i OF
      |00:                                      -- simp
      |01: out(''33c'G',2)                      -- bold
      |02: out(''33c 17c,2)                     -- compr
      |03: out(''33c'G' 33c 17c,4)              -- bold compr
      |04: out(''33c'P',2)                      -- elit
      |05: out(''33c'P'33c'E',4)                -- elit bold
      |06: out(''33c'P'33c 17c,4)               -- elit compr
      |07: out(''33c'P'33c'E'33c 17c,6)         -- elit bold compr
      |08: out(''33c'S1',3)                     -- bot
      |09: out(''33c'S1'33c 17c,5)              -- bot compr
      |10: out(''33c'S0',3)                     -- top
      |11: out(''33c'S0'33c 17c,5)              -- top compr
    ELSE
      RETURN err.bad_parm
    END;
    lps.font:=f;
    RETURN ok;
  END set_font;

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
      ws(str,0,i);
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
      |dlp._reset    : RETURN lp_reset();
      |dlp._restore  : IF i<=0 THEN RETURN err.bad_parm END;
                       adr:=i; tptr:=adr; lp_restore(tptr^);
                       RETURN ok
      |dlp._something:
        IF i0#0 THEN
          IF (font>7) OR (ODD(font)) THEN RETURN ok
          ELSIF font<4 THEN out(''33c'G',2)
          ELSE              out(''33c'E',2)
          END;
        ELSE
          IF (font<8) & (ODD(font)) THEN RETURN ok
          ELSIF font<4 THEN out(''33c'H',2)
          ELSE              out(''33c'F',2)
          END;
        END;
        something:=i0;
      |dlp._underline:
        underline:=i0;
        IF i0#0 THEN out(''33c'-'1c,3) ELSE out(''33c'-'0c,3) END
      |dlp._Wx2      :
        Wx2:=i0; IF i0#0 THEN out(''33c'W'1c,3) ELSE out(''33c'W'0c,3) END
      |dlp._Hx2      : RETURN err.inv_op
      |dlp._autowrap : RETURN err.inv_op
      |dlp._raw      : RETURN err.inv_op
      |dlp._reverse  : RETURN err.inv_op
      |dlp._ground   : RETURN err.inv_op
      |dlp._density  : IF (i0<0) OR (i0>6) THEN RETURN err.bad_parm END;
                       c_dns:=i0; density:=dns[i0]; RETURN ok
      |dlp._font     : RETURN set_font(i0)
      |dlp._load_font: RETURN err.inv_op
      |dlp._back     : RETURN bck_fwd(i,'j')
      |dlp._fwd      : RETURN bck_fwd(i,'J')
      |dlp._left     : RETURN err.inv_op
      |dlp._right    : RETURN err.inv_op
      |dlp._bflf     : out(''33c 'j' 22c,3)
      |dlp._bhlf     : out(''33c 'j' 11c,3)
      |dlp._fhlf     : out(''33c 'J' 11c,3);
      |dlp._fflf     ,
       dlp._write_ln : out(''12c''15c,2);
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
END epson_rob;

PROCEDURE lp_doio(VAR r: req.REQUEST);
  VAR arg: DYNARR OF sys.WORD; str: DYNARR OF CHAR;
BEGIN
  WITH r DO
    res:=ok;
    CASE op MOD 256 OF
      |req.NOP    :
      |req.CONTROL:
        arg^.ADR:=buf; arg^.HIGH:=len-1;
        res:=epson_rob(op DIV 256,arg)
      |req.LOCK  :
      |req.UNLOCK:
      |req.WRITE :
        str^.ADR:=buf; str^.HIGH:=pos+len-1;
        ws(str,pos,len); -- english-russian send(r)
        IF NOT bio.done THEN res:=bio.error END;
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
  r:=fs.define_driver(nm,'',0,fs.spec,lp_doio);
  IF r#ok THEN ASSERT(FALSE,r) END;
  env.put_str(env.info,nm,TRUE);
  c_dns:=5; ccg_com:=''33c'R'
END init;

BEGIN
  IF HIGH(arg.words)<1    THEN tty.print('Not all parameters\n'); HALT(1) END;
  IF HIGH(arg.words[0])>7 THEN tty.print('Too long driver name\n'); HALT(1) END;
  env.final(halt);
  init(arg.words[0],arg.words[1]);
  env.become_ipr;
  os.suspend(os.active(),-1)
END LPcm6329.

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


PROCEDURE paint(VAL map: ARRAY OF sys.WORD;
                    w,h: INTEGER;
                  dx,dy: INTEGER);

PROCEDURE bblt(ta,to,fa,fo,sz: INTEGER); CODE cod.bblt END bblt;

  VAR i: INTEGER;
    p,b: INTEGER; -- byte & bit pos in buf
  c,l,m: INTEGER; -- column, line & bit  pos in map
    f,t: sys.ADDRESS;
    buf: ARRAY [0..79] OF CHAR;

BEGIN
  IF ((w*h+31) DIV 32 - 1) > HIGH(map) THEN
    --  e r r o r
  END;
  t:=sys.ADR(buf); f:=sys.ADR(map); l:=0;
  (*
    This place must be occupied by "skip dy"
  *)
  WHILE l<h DO
    c:=0; p:=0; b:=0;
    low.zero(buf);

    i:=dx;
    WHILE i>0 DO -- make left margin
      IF i>BYTES(buf) THEN out(buf,BYTES(buf))
      ELSE                 out(buf,i)
      END;
      DEC(i,BYTES(buf));
    END;

    WHILE c<w DO -- print one raw
      m:=(l+pins-1)*w+c;

      FOR i:=0 TO pins-1 DO -- fill one byte in buf
        IF (l+pins-1-i)<=h THEN bblt(t,b,f,m,1) END;
        INC(b);
        DEC(m,w)
      END;

      IF p<HIGH(buf) THEN INC(p)
      ELSE
        out(buf,p+1);             -- flash when buf full
        p:=0; b:=0; low.zero(buf)
      END;
      INC(c)
    END;

    IF p>0 THEN out(buf,p) END;   -- flash when buf not empty

    INC(l,pins); newln;
  END
END paint;
