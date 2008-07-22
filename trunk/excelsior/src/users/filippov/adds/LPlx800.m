MODULE LPlx800; (*$U+ Leg 20-Feb-91. (c) KRONOS *)

IMPORT  sys: SYSTEM;            IMPORT  err: defErrors;
IMPORT   fs: osFiles;           IMPORT   os: osKernel;
IMPORT  dlp: defPrinter;        IMPORT  env: tskEnv;
IMPORT  req: defRequest;        IMPORT  bio: BIO;
IMPORT  cod: defCodes;          IMPORT  low: lowLevel;
IMPORT  arg: tskArgs;

(* instalation  LPlx800 dev_name file_name *)

CONST ok  = err.ok;
      dns = ARRAY OF INTEGER {60,120,120,240,80,72,90};
      lpt = 'АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ'
            'абвгдежзийклмнопрстуфхцчшщъыьэюя';

VAR  tr: ARRAY CHAR OF CHAR;
    gcs: ARRAY [0..6] OF CHAR; -- graphic control string;
    dev: bio.FILE;
    lps: dlp.STATE;
  c_dns: INTEGER;
    fnt: INTEGER; -- font draft roman or sanserif
    hei: INTEGER; -- normal, low index or high index height
    wid: INTEGER; -- normal or compressed width
    pit: INTEGER; -- pitch "elit"(15 cpi) or "pica" (12cpi)
    ita: INTEGER; -- italic on or off
    bol: INTEGER; -- bold on or off

PROCEDURE out(VAL s: ARRAY OF CHAR; l: INTEGER);
BEGIN bio.write(dev,sys.ADR(s),l) END out;

PROCEDURE lp_reset(send: BOOLEAN): INTEGER;

(*
CONST reset = ''33c '@'      -- init
                30c          -- flash internal buffer
                33c 'C' 310c -- set page(format) length
                33c 'O'      -- not skip perforation line
                33c 's' 0c   -- full speed
                33c '9'      -- not check paper out
                33c 'A' 14c  -- set 12/72 " line feed
                33c 'R'  0c  -- set US set
                33c '6'      -- expand printing area
                33c 'H'      -- duble  print off
                33c 'F'      -- bold   print off
                33c '5'      -- italic print off
                33c 'x' 0c   -- set draft mode
                22c          -- compressed print off
                24c          -- duble width off
                33c 'T'      -- upper/low index off
                33c '-' 0c   -- underline off
                33c 'W' '0'  -- duble width off
                33c 'M'      -- set "elit" step
*)

CONST reset = ''33c '@' 30c 33c 'C' 310c 33c 'O' 33c 's' 0c 33c '9' 33c 'A' 14c
                33c 'R'  0c 33c '6' 33c 'H' 33c 'F' 33c '5' 33c 'x' 0c 22c
                24c 33c 'T' 33c '-' 0c 33c 'W' '0' 33c 'M';
BEGIN
  IF send THEN
    out(reset,HIGH(reset));
    IF NOT bio.done THEN RETURN bio.error END
  END;
  fnt:=0; hei:=0; wid:=0; pit:=0; ita:=0; bol:=0;
  WITH lps DO
    something:=0;    underline:=0;    Wx2:=0; Hx2 :=0;
    reverse  :=0;    raw      :=0;    awp:=0; type:=2;
    fonts    :=144;  font     :=0;
    hchar    :=9;    wchar    :=dlp.WIDTH(-9);
    grounds  :=1;    ground   :=0;
    hbar     :=244c; vbar     :=245c;
    bars[0,0]:=240c; bars[0,1]:=246c;  bars[0,2]:=241c;
    bars[1,0]:=251c; bars[1,1]:=252c;  bars[1,2]:=247c;
    bars[2,0]:=243c; bars[2,1]:=250c;  bars[2,2]:=242c;
    densities:=6;    c_dns:=5; density:=dns[c_dns];
  END;
  RETURN ok
END lp_reset;

PROCEDURE ws(s: ARRAY OF CHAR; pos,len: INTEGER);
  VAR i: INTEGER;
BEGIN
  FOR i:=pos TO pos+len-1 DO s[i]:=tr[s[i]] END;
  bio.fwrite(dev,sys.ADR(s),pos,len)
END ws;

PROCEDURE fwd(i: INTEGER): INTEGER;
  VAR s: ARRAY [0..3] OF CHAR;
BEGIN
  s:='' 33c 'J';
  WHILE i>0 DO
    IF i>255 THEN s[2]:=377c ELSE s[2]:=CHAR(i) END;
    out(s,3);
    DEC(i,255);
    IF NOT bio.done THEN RETURN bio.error END;
  END;
  RETURN ok
END fwd;

PROCEDURE adjust_font(f,h,w,p,i,b: INTEGER);
BEGIN
  IF f#fnt THEN
    IF    f=0 THEN out(''33c 'x' 0c,3)
    ELSIF f=1 THEN out(''33c 'x' 1c 33c 'k' 0c,6)
    ELSE           out(''33c 'x' 1c 33c 'k' 1c,6)
    END; fnt:=f
  END;
  IF h#hei THEN
    IF    h=0 THEN out(''33c 'T',2);
    ELSIF h=1 THEN out(''33c 'T' 33c 'S' 1c,5);
    ELSE           out(''33c 'T' 33c 'S' 0c,5);
    END; hei:=h;
  END;
  IF w#wid THEN
    IF h=0 THEN out(''22c,1) ELSE out(''17c,1) END; w:=wid
  END;
  IF p#pit THEN
    IF p=0 THEN out(''33c 'M',2) ELSE out(''33c 'P',2) END;
    pit:=p
  END;
  IF i#ita THEN
    IF i=0 THEN out(''33c '5',2) ELSE out(''33c '4',2) END;
    ita:=i
  END;
  IF b#bol THEN
    IF b=0 THEN out(''33c 'F',2) ELSE out(''33c 'E',2) END;
    bol:=b
  END;
END adjust_font;

PROCEDURE set_font(fnt: INTEGER): INTEGER;
  VAR f,h,w,p,i,b: INTEGER;
BEGIN
  IF (fnt<0) OR (fnt>143) THEN RETURN err.bad_parm END;
  lps.font:=fnt;
  b:=fnt MOD 2; fnt:=fnt DIV 2;
  i:=fnt MOD 2; fnt:=fnt DIV 2;
  p:=fnt MOD 2; fnt:=fnt DIV 2;
  w:=fnt MOD 2; fnt:=fnt DIV 2;
  h:=fnt MOD 3; fnt:=fnt DIV 3;
  f:=fnt MOD 3;
  IF lps.something#0 THEN b:=1 END;
  adjust_font(f,h,w,p,i,b);
  RETURN ok
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

VAR buf: ARRAY [0..1023] OF CHAR;
CONST pins = 8;

PROCEDURE paint(VAL map: sys.ADDRESS;
                    w,h: INTEGER;
                  dx,dy: INTEGER): INTEGER;

  PROCEDURE bblt(ta,to,fa,fo,sz: INTEGER); CODE cod.bblt END bblt;

  PROCEDURE ou(len: INTEGER);
  BEGIN
    gcs[2]:=CHAR(c_dns);
    gcs[3]:=CHAR(len MOD 256);
    gcs[4]:=CHAR(len DIV 256);
    out(gcs,5); out(buf,len);
  END ou;

  VAR i: INTEGER;
    p,b: INTEGER; -- byte & bit pos in buf
  c,l,m: INTEGER; -- column, line & bit  pos in map
    f,t: sys.ADDRESS;

BEGIN
  IF (dy<0) OR (dx<0) OR (w<=0) OR (h<=0)  THEN RETURN err.bad_parm END;
  f:=map; l:=0; t:=sys.ADR(buf);
  IF fwd(dy*3)=0 THEN END;
  WHILE l<h DO
    IF (l#0) THEN -- newln only if more then 1 raw
      out(''15c 33c 'J' 30c,4)
    END;
    c:=0;
    low.zero(buf);
    i:=dx;
    WHILE i>BYTES(buf) DO -- make left margin
      ou(BYTES(buf));
      DEC(i,BYTES(buf));
    END;
    p:=i; b:=p*pins;
    WHILE c<w DO -- print one raw
      m:=(l+pins-1)*w+c;

      FOR i:=0 TO pins-1 DO -- fill one byte in buf
        IF (l+pins-1-i)<=h THEN bblt(t,b,f,m,1) END;
        INC(b);
        DEC(m,w)
      END;

      IF p<HIGH(buf) THEN INC(p)
      ELSE
        ou(p+1);             -- flash when buf full
        p:=0; b:=0; low.zero(buf)
      END;
      INC(c)
    END;

    IF p>0 THEN ou(p) END;   -- flash when buf not empty

    INC(l,pins);
  END;
  RETURN ok
END paint;

PROCEDURE epson_lx(no: INTEGER; VAL x: ARRAY OF sys.WORD): INTEGER;

VAR er: INTEGER;

  PROCEDURE ss(op: INTEGER; SEQ a: sys.WORD);
  BEGIN
    op:=epson_lx(op,a);
    IF op#ok THEN er:=op END;
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
      |dlp._something: something:=i0;
        IF i0#0 THEN              adjust_font(fnt,hei,wid,pit,ita,1)
        ELSIF (font MOD 2)=0 THEN adjust_font(fnt,hei,wid,pit,ita,0)
        END
      |dlp._underline:
        underline:=i0;
        IF i0#0 THEN out(''33c'-'1c,3) ELSE out(''33c'-'0c,3) END
      |dlp._Wx2      :
        Wx2:=i0; IF i0#0 THEN out(''33c'W'1c,3) ELSE out(''33c'W'0c,3) END
      |dlp._Hx2      : IF i0#0 THEN RETURN err.bad_parm END
      |dlp._autowrap : IF i0#0 THEN RETURN err.bad_parm END
      |dlp._raw      : IF i0=0 THEN RETURN err.bad_parm END
      |dlp._reverse  : IF i0#0 THEN RETURN err.bad_parm END
      |dlp._ground   : IF i0#0 THEN RETURN err.bad_parm END
      |dlp._density  : IF (i0<0) OR (i0>6) THEN RETURN err.bad_parm END;
                       c_dns:=i0; density:=dns[i0]; RETURN ok
      |dlp._font     : RETURN set_font(i0)
      |dlp._load_font: RETURN err.inv_op
      |dlp._back     : RETURN err.inv_op
      |dlp._fwd      : RETURN fwd(i)
      |dlp._left     : RETURN err.inv_op
      |dlp._right    : RETURN err.inv_op
      |dlp._bflf     : RETURN err.inv_op
      |dlp._bhlf     : RETURN err.inv_op
      |dlp._fhlf     : out(''33c 'J' 11c,3);
      |dlp._fflf     ,
       dlp._write_ln : out(''12c''15c,2);
      |dlp._eject    : out(''14c,1)
      |dlp._repeat   : RETURN repeat(CHAR(i0),i1)
      |dlp._paint    :
        IF HIGH(x)<4 THEN RETURN err.bad_parm END;
        RETURN paint(x[0],x[1],x[2],x[3],x[4])
      |dlp._set_attr : RETURN err.inv_op
      |dlp._get_attr : RETURN err.inv_op
    ELSE
      RETURN err.inv_op
    END
  END;
  IF bio.done THEN RETURN ok END;
  RETURN bio.error
END epson_lx;

PROCEDURE lp_doio(VAR r: req.REQUEST);
  VAR arg: DYNARR OF sys.WORD; str: DYNARR OF CHAR;
BEGIN
  WITH r DO
    res:=ok;
    CASE op MOD 256 OF
      |req.NOP    :
      |req.CONTROL:
        arg^.ADR:=buf; arg^.HIGH:=len-1;
        res:=epson_lx(op DIV 256,arg)
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
  bio.open(dev,p,'w');
  IF NOT bio.done THEN HALT(bio.error) END;
  r:=fs.define_driver(nm,'',0,fs.spec,lp_doio);
  IF r#ok THEN HALT(r) END;
  env.put_str(env.info,nm,TRUE);
  FOR c:=0c TO 257c DO tr[c]:=c END;
  FOR c:=260c TO 277c DO tr[c]:=CHAR(ORD(c)+100b) END;
  FOR c:=260c TO 357c DO tr[lpt[ORD(c)-260b]]:=c END;
  gcs:='' 33c '*' 0c 0c 0c;
  env.become_ipr;
  IF lp_reset(FALSE)=ok THEN END;
END init;

BEGIN
  IF (HIGH(arg.words)<1) OR (HIGH(arg.words[0])>7) THEN
    HALT(err.bad_parm)
  END;
  env.final(halt);
  init(arg.words[0],arg.words[1]);
  os.suspend(os.active(),-1)
END LPlx800.

elit bold compr bot top
---------------------------------------------------------------------
|   |  Font    | Height |  Width     | Pitch       | Italic | Bold  |
|   |   f      |   h    |    w       |   p         |    i   |   b   |
|---|----------|--------|------------|-------------|--------|-------|
| 0 | draft    | normal |   normal   | elit(15cpi) |   on   |   on  |
| 1 | roman    |  low   | compressed | pica(12cpi) |   off  |   off |
| 2 | sanserif |   up   |            |             |        |       |
---------------------------------------------------------------------

Font no. = b + i*2 + p*4 + w*8 + h*16 + f*16*3
b + i*2 + p*4 + w*8 + h*16 + f*16*3
