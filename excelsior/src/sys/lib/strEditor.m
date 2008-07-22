IMPLEMENTATION MODULE strEditor;        (* Andy 09-Jul-89. (c) KRONOS *)
                                        (* Ned  05-Oct-89. (c) KRONOS *)
                                        (* Leo  08-Nov-89. (c) KRONOS *)
                                        (* Andy 15-Dec-89. (c) KRONOS *)
                                        (* Andy 10-Jan-90. (c) KRONOS *)
                                        (* Andy 16-Oct-90. (c) KRONOS *)

(*$U+*)

                IMPORT   sys: SYSTEM;
                IMPORT  keyb: Keyboard;
                IMPORT   tty: Terminal;
                IMPORT   mem: Heap;
                IMPORT   cmd: defCodes;
                IMPORT        ASCII;

WITH STORAGE (NEW    : mem.allocate;
              DISPOSE: mem.deallocate;
              RESIZE : mem.reallocate);

PROCEDURE control(ch: CHAR): BOOLEAN;
(*
BEGIN RETURN ORD(ch) MOD 128 < 40b
*)
CODE cmd.lib 7Fh cmd.and cmd.lib 20h cmd.lss
END control;

CONST bits= BITS(BITSET);

TYPE SCALE= ARRAY [0..((256+bits-1) DIV bits)-1] OF BITSET;

PROCEDURE mk_scale(VAR sc: SCALE; SEQ keys: CHAR);
  VAR i,k: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(sc) DO sc[i]:={} END;
  FOR i:=0 TO HIGH(keys) DO k:=ORD(keys[i]);
     INCL(sc[k DIV bits],k MOD bits);
  END;
END mk_scale;

VAR vkeys    : SCALE;

PROCEDURE _std_valid(VAR ch: CHAR): BOOLEAN;
BEGIN
  RETURN (ORD(ch) MOD bits) IN vkeys[ORD(ch) DIV bits]
END _std_valid;

VAR std_valid: VALID;

------------------- Strings Buffer Handling ----------------------------
                    -----------------------
CONST MAGIC= 454C5324h; (* $SLE *)

TYPE
  buf_rec  = RECORD
               magic: INTEGER;
               buf  : DYNARR OF STRING;
               bp   : INTEGER;
              prefix: STRING;
             END;
  LINE_BUFFER = POINTER TO buf_rec;

VAR     dflt: descriptor;
    dflt_rec: desc_rec;
    dflt_buf:  buf_rec;

PROCEDURE new(VAR d: descriptor; n: INTEGER);
  VAR i: INTEGER;
BEGIN
  NEW(d); IF d=NIL THEN RETURN END;
  d^.ins:=TRUE;  d^.bel:=TRUE;
  d^.how:=empty; d^.last:=0c;
  d^.valid:=std_valid;
  d^.read:=keyb.read;
  d^.bell:=keyb.bell;
  NEW(d^.buf);
  IF d^.buf=NIL THEN DISPOSE(d); d:=NIL; RETURN END;
  WITH d^.buf^ DO
      magic:=MAGIC; bp:=0;
      NEW(buf); NEW(prefix);
      IF n<=0 THEN RETURN END;
      IF n=1 THEN n:=2 END;
      NEW(buf,n);
      IF buf^.ADR=NIL THEN NEW(buf) END;
      FOR i:=0 TO HIGH(buf) DO NEW(buf[i]) END;
  END;
END new;

PROCEDURE dispose(VAR d: descriptor);
  VAR i: INTEGER;
BEGIN
  IF d=NIL THEN RETURN END;
  WITH d^.buf^ DO ASSERT(magic=MAGIC,4Fh);
     FOR i:=0 TO HIGH(buf) DO DISPOSE(buf[i]) END;
     DISPOSE(buf);
     DISPOSE(prefix);
     magic:=0;
  END;
  DISPOSE(d^.buf);
  DISPOSE(d);
  d:=NIL;
END dispose;

PROCEDURE str_equ(VAL s0,s1: ARRAY OF CHAR; len: INTEGER): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  IF len>0 THEN i:=0;
    REPEAT
      IF s0[i]#s1[i] THEN RETURN FALSE END;
      INC(i);
    UNTIL i=len;
  END;
  RETURN TRUE
END str_equ;

PROCEDURE put_buf(VAR buf: LINE_BUFFER; push?: BOOLEAN;
                  VAL s: ARRAY OF CHAR; l: INTEGER;
                  VAL d: descriptor);
---- Буфер buf нормальный
  VAR x: STRING;

  PROCEDURE make(pos: INTEGER);
    VAR i: INTEGER;
  BEGIN
    NEW(x,l+1);
    IF x^.ADR=NIL THEN NEW(x); IF d^.bel THEN d^.bell(1) END;
    ELSE
      FOR i:=0 TO l-1 DO x[i]:=s[i]
    END; x[l]:=0c END;
    buf^.buf[pos]^:=x^;
  END make;

  VAR i: INTEGER;
BEGIN
  ASSERT(buf^.magic=MAGIC,4Fh);
  IF NOT push? THEN
      DISPOSE(buf^.buf[0]);
      IF l=0 THEN NEW(buf^.buf[0]); RETURN END;
      make(0); RETURN
  END;
  x^:=buf^.buf[1]^;
  IF (l=0) OR ((l=HIGH(x)) & str_equ(s,x,l)) THEN RETURN END;
  DISPOSE(buf^.buf[HIGH(buf^.buf)]);
  FOR i:=HIGH(buf^.buf) TO 2 BY -1 DO buf^.buf[i]^:=buf^.buf[i-1]^ END;
  make(1);
END put_buf;

PROCEDURE get_buf(VAR buf: LINE_BUFFER; bpos: INTEGER;
                    start: INTEGER; maxl: INTEGER;
                    VAR s: ARRAY OF CHAR; VAR l: INTEGER; VAR c: INTEGER);
  VAR i,len: INTEGER; x: STRING; ch: CHAR;
BEGIN ASSERT(buf^.magic=MAGIC,4Fh);
  x^:=buf^.buf[bpos]^;
  len:=HIGH(x)-start;
  IF len>maxl-start THEN len:=maxl-start END;
  IF len<0 THEN len:=0 END;
  i:=start; l:=start+len; c:=0;
  WHILE i<l DO
    ch:=x[i]; s[i]:=ch;
    IF control(ch) THEN INC(c) END;
    INC(i)
  END;
  s[l]:=0c;
  FOR i:=0 TO start-1 DO IF control(s[i]) THEN INC(c) END END;
END get_buf;

------------------------------------------------------------------------

PROCEDURE form_byte(VAR b: CHAR; VAL d: descriptor): BOOLEAN;
  VAR c,s: INTEGER; res: BOOLEAN;

  PROCEDURE get(bnd: INTEGER): BOOLEAN;
    VAR ch: CHAR;
  BEGIN
    d^.read(ch);
    s:=ORD(ch)-ORD('0');
    IF (s<0) OR (s>bnd) THEN RETURN FALSE END;
    c:=c*8+s; RETURN TRUE
  END get;

BEGIN c:=0;
  res:=get(3) & get(7) & get(7);
  IF res THEN b:=CHAR(c) END; RETURN res
END form_byte;

PROCEDURE stop_bytes(VAR s: SCALE; SEQ term: CHAR);
  VAR k: INTEGER;
BEGIN mk_scale(s,term);
  k:=ORD(keyb.cr);  INCL(s[k DIV bits],k MOD bits);
  k:=ORD(ASCII.NL); INCL(s[k DIV bits],k MOD bits);
END stop_bytes;

PROCEDURE del_byte(VAR s: ARRAY OF CHAR; sp: INTEGER; VAR l: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF sp=l THEN RETURN END;
  FOR i:=sp TO l-2 DO s[i]:=s[i+1] END;
  DEC(l);
END del_byte;

PROCEDURE ins_byte(VAR s: ARRAY OF CHAR; ch: CHAR;
                      sp: INTEGER; VAR l: INTEGER; maxl: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF sp=maxl THEN RETURN END;
  IF l<maxl THEN INC(l) END;
  FOR i:=l-2 TO sp BY -1 DO s[i+1]:=s[i] END;
  s[sp]:=ch;
END ins_byte;

----------------------------------------------------------------
PROCEDURE alpha_num(ch: CHAR): BOOLEAN;
BEGIN
  RETURN (ch>=300c)
      OR (ORD(CAP(ch))-ORD("A") IN {0..25})
      OR (ORD(ch)-ORD("0") IN {0..9})
      OR (ch='_');
END alpha_num;

PROCEDURE ctrl(ch: CHAR): BOOLEAN;
BEGIN
  RETURN control(ch) OR ((ch>=240c) & (ch<=277c)) (* pseudo-graphics *)
END ctrl;

PROCEDURE other(ch: CHAR): BOOLEAN;
BEGIN
  RETURN (ch#' ') &
         (NOT (alpha_num(ch) OR ctrl(ch))
          OR (ch='_')
         )
END other;

PROCEDURE skip_GOLD_word(VAL  s: ARRAY OF CHAR; l: INTEGER;
                             sp: INTEGER;
                         VAR new_sp: INTEGER; VAR cs: INTEGER);
  VAR np: INTEGER;
BEGIN
  cs:=0;
  np:=sp;
  WHILE (np<l) & (s[np]=' ') DO INC(np) END;
  IF np<l THEN
      IF alpha_num(s[np]) THEN INC(np);
           WHILE (np<l) & alpha_num(s[np]) DO INC(np) END;
      ELSIF other(s[np]) THEN INC(np);
           WHILE (np<l) & other(s[np]) DO INC(np) END;
      ELSE INC(cs,ORD(control(s[np])));
           INC(np);
      END;
  END;
  new_sp:=np;
END skip_GOLD_word;

PROCEDURE skip_GOLD_word_left(VAL  s: ARRAY OF CHAR; l: INTEGER;
                                  sp: INTEGER;
                              VAR new_sp: INTEGER; VAR cs: INTEGER);
  VAR np: INTEGER;
BEGIN
  cs:=0;
  IF sp=0 THEN new_sp:=sp; RETURN END;
  np:=sp-1;
  WHILE (np>0) & (s[np]=' ') DO DEC(np) END;
  IF np>0 THEN
      IF alpha_num(s[np]) THEN
           WHILE (np>0) & alpha_num(s[np-1]) DO DEC(np) END;
      ELSIF other(s[np]) THEN
           WHILE (np>0) & other(s[np-1]) DO DEC(np) END;
      END;
  END;
  INC(cs,ORD(control(s[np])));
  new_sp:=np;
END skip_GOLD_word_left;

PROCEDURE skip_word(VAL  s: ARRAY OF CHAR; l: INTEGER;
                        sp: INTEGER;
                    VAR new_sp: INTEGER; VAR cs: INTEGER);
  VAR np: INTEGER;
BEGIN
  cs:=0;
  np:=sp;
  WHILE (np<l) & (s[np]#' ') DO
     INC(cs,ORD(control(s[np]))); INC(np);
  END;
  WHILE (np<l) & (s[np]=' ') DO INC(np) END;
  new_sp:=np;
END skip_word;

PROCEDURE skip_word_left(VAL  s: ARRAY OF CHAR; l: INTEGER;
                             sp: INTEGER;
                         VAR new_sp: INTEGER; VAR cs: INTEGER);
  VAR np: INTEGER;
BEGIN
  cs:=0;
  np:=sp;
  WHILE (np>0) & (s[np-1]=' ') DO DEC(np) END;
  WHILE (np>0) & (s[np-1]#' ') DO DEC(np);
     INC(cs,ORD(control(s[np])));
  END;
  new_sp:=np;
END skip_word_left;

CONST
  lpull   = CHAR( ORD('W') MOD 32 );
  rpull   = CHAR( ORD('D') MOD 32 );

PROCEDURE read_str(VAL prompt: ARRAY OF CHAR;
                   VAR string: ARRAY OF CHAR;
                          dsc: descriptor;
              SEQ terminators: CHAR);

  VAR vt52: BOOLEAN;
  VAR key: CHAR;
        l: INTEGER; -- string length
     maxl: INTEGER; -- max string length
    sp,cp: INTEGER;  (* sp -- string pos   cp -- cursor pos *)
       cs: INTEGER;
      buf: LINE_BUFFER;
     desc: descriptor;

  VAR term_scale: SCALE;

  PROCEDURE db;           BEGIN del_byte(string,sp,l) END db;

  PROCEDURE ib(ch: CHAR); BEGIN ins_byte(string,ch,sp,l,maxl) END ib;

  PROCEDURE bell;
  BEGIN IF desc^.bel THEN desc^.bell(1) END END bell;

  PROCEDURE to(d: INTEGER);
    VAR i: INTEGER;
  BEGIN
    IF cp=d THEN RETURN END;
    i:=ABS(cp-d);
    IF cp>d THEN tty.left(i) ELSE tty.right(i) END;
    cp:=d
  END to;

  PROCEDURE w(ch: CHAR);
  BEGIN
    IF control(ch) THEN
         tty.Write("^"); tty.Write(CHAR(ORD(ch)+100b)); INC(cp,2)
    ELSE tty.Write(ch); INC(cp) END;
  END w;

  PROCEDURE p(ch: CHAR);
  BEGIN
    IF control(ch) THEN
         tty.Write("^"); tty.Write(CHAR(ORD(ch)+100b)); tty.left(2);
    ELSE tty.Write(ch); tty.left(1) END;
  END p;

  PROCEDURE clear(ctrl: BOOLEAN);
  BEGIN
    IF ctrl THEN
         tty.Write(' '); tty.Write(' '); tty.left(2);
    ELSE tty.Write(' ');                 tty.left(1) END;
  END clear;

  PROCEDURE refresh;
    VAR i,p: INTEGER;
  BEGIN
    p:=cp; to(0);
    FOR i:=0 TO l-1 DO w(string[i]) END; tty.erase_line(0);
    to(p);
  END refresh;

  PROCEDURE refresh_tail;
    VAR i,p: INTEGER;
  BEGIN p:=cp;
    FOR i:=sp TO l-1 DO w(string[i]) END; tty.erase_line(0);
    to(p);
  END refresh_tail;

  PROCEDURE moveleft; FORWARD;

  PROCEDURE end;
    VAR i: INTEGER;
  BEGIN
    FOR i:=sp TO l-1 DO w(string[i]) END; tty.erase_line(0);
    sp:=l; IF l=maxl THEN moveleft END;
  END end;

  PROCEDURE insert(ch: CHAR);
  BEGIN
    IF l=maxl THEN bell; RETURN END;
    ib(ch);
    IF l=maxl THEN refresh_tail; RETURN END;
    IF vt52 THEN refresh_tail
    ELSE tty.ins_char(1+ORD(control(ch))); p(ch);
    END;
  END insert;

  PROCEDURE delete;
    VAR ctrl: BOOLEAN;
  BEGIN
    IF sp=l THEN bell; RETURN END;
    ctrl:=control(string[sp]);
    db;
    IF sp=l THEN clear(ctrl); RETURN END;
    IF vt52 THEN refresh_tail
    ELSE tty.del_char(1+ORD(ctrl)) END;
  END delete;

  PROCEDURE moveleft;
    VAR ctrl: BOOLEAN;
  BEGIN
    IF sp=0 THEN bell; RETURN END;
    DEC(sp);
    ctrl:=control(string[sp]);
    DEC(cp,1+ORD(ctrl)); tty.left(1+ORD(ctrl))
  END moveleft;

  PROCEDURE moveright;
  BEGIN
    IF (l=maxl) & (sp>=l-1) THEN bell; RETURN END;
    IF sp<l THEN
      w(string[sp]); INC(sp);
    ELSE
      string[l]:= ' '; INC(l);
      IF l<maxl THEN w(' '); INC(sp) END;
    END;
  END moveright;

  PROCEDURE putch(ch: CHAR);
  BEGIN
    IF (l=maxl) & (desc^.ins OR (sp>=l)) THEN bell; RETURN END;
    IF desc^.ins OR (sp=l) THEN insert(ch); moveright; RETURN END;
    IF control(string[sp])#control(ch) THEN
      IF vt52 THEN
        string[sp]:=ch; INC(sp); w(ch); refresh_tail; RETURN
      END;
      IF control(ch) THEN tty.ins_char(1) ELSE tty.del_char(1) END;
    END;
    string[sp]:=ch; w(ch); INC(sp);
    IF sp=maxl THEN bell; moveleft END;
  END putch;

  PROCEDURE delch;
  BEGIN
    IF sp=0 THEN bell; RETURN END;
    moveleft;
    IF desc^.ins THEN delete ELSE putch(' '); moveleft END;
  END delch;

  PROCEDURE tab(left: BOOLEAN);
  BEGIN
    IF left THEN moveleft;
      WHILE (sp MOD 8)#0 DO moveleft END;
    ELSE moveright;
      WHILE (sp MOD 8 # 0) & (sp<maxl-1) DO moveright END;
    END;
  END tab;

  PROCEDURE Lpull;
    VAR i,j: INTEGER;
  BEGIN
    IF (sp=l) OR (string[sp]#' ') THEN RETURN END;
    i:=sp;
    WHILE (i<l) & (string[i]=' ') DO INC(i) END;
    FOR j:=i TO l-1 DO string[sp+j-i]:=string[j] END;
    DEC(l,i-sp);
    refresh_tail
  END Lpull;

  PROCEDURE Rpull;
    VAR i,j,p,c: INTEGER;
  BEGIN i:=0;
    WHILE (i<l) & (string[i]=' ') DO INC(i) END;
    IF (i=l) OR (i=sp) THEN RETURN END;
    IF i>sp THEN Lpull; RETURN END;
    IF (sp-i)>(maxl-l) THEN bell; RETURN END;
    p:=sp; sp:=i;
    c:=0;
    FOR j:=sp TO p-1 DO
      IF control(string[j]) THEN INC(c) END
    END;
    FOR j:=1 TO p-sp DO ib(' ') END;
    sp:=p; refresh; to(cp-c)
  END Rpull;

----------------------- Words Processing -----------------------------
                        ----------------
  PROCEDURE word_right;
    VAR ctrls,np: INTEGER;
  BEGIN
    skip_word(string,l,sp,np,ctrls);
    IF (np=maxl) & (maxl>0) THEN
        DEC(np); DEC(ctrls,ORD(control(string[np])));
    END;
    to(cp+np-sp+ctrls);
    sp:=np;
  END word_right;

  PROCEDURE word_left;
    VAR ctrls,np: INTEGER;
  BEGIN
    skip_word_left(string,l,sp,np,ctrls);
    to(cp-(sp-np+ctrls));
    sp:=np;
  END word_left;

  PROCEDURE del_substr(from,len,ctrls: INTEGER);
    VAR i: INTEGER;
  BEGIN
    IF len<=0 THEN RETURN END;
    FOR i:=0 TO l-from-len-1 DO string[from+i]:=string[from+len+i] END;
    DEC(l,len); DEC(cs,ctrls);
    IF vt52 THEN refresh_tail ELSE tty.del_char(len+ctrls) END;
  END del_substr;

  PROCEDURE delete_word(GOLD: BOOLEAN);
    VAR np,ctrls: INTEGER;
  BEGIN
    IF GOLD THEN skip_GOLD_word(string,l,sp,np,ctrls)
            ELSE      skip_word(string,l,sp,np,ctrls)
    END;
    del_substr(sp,np-sp,ctrls);
  END delete_word;

  PROCEDURE word_back; BEGIN word_left; delete_word(FALSE) END word_back;

  PROCEDURE word_GOLD_back;
    VAR n,np,ctrls: INTEGER;
  BEGIN
    skip_GOLD_word_left(string,l,sp,np,ctrls);
    n:=sp-np; sp:=np;
    to(cp-(n+ctrls));
    del_substr(sp,n,ctrls);
  END word_GOLD_back;
----------------------------------------------------------------------

  PROCEDURE use_buf(dir: INTEGER);
  BEGIN
    IF buf=NIL THEN bell; RETURN END;
    IF dir>0 THEN
         IF buf^.bp=0 THEN put_buf(buf,FALSE,string,l,desc) END;
         IF buf^.bp>=HIGH(buf^.buf) THEN bell; RETURN END;
         INC(buf^.bp);
    ELSE IF buf^.bp<=0 THEN bell; RETURN END;
         DEC(buf^.bp);
    END;
    get_buf(buf,buf^.bp,0,maxl,string,l,cs);
    sp:=0; to(0);
    IF _bol IN desc^.how THEN refresh ELSE end END;
  END use_buf;

  PROCEDURE duptail;
  BEGIN
    IF buf=NIL THEN bell; RETURN END;
    IF buf^.bp>=HIGH(buf^.buf) THEN bell; RETURN END;
    get_buf(buf,buf^.bp+1,sp,maxl,string,l,cs);
    refresh_tail;
  END duptail;

  PROCEDURE exit;
  BEGIN desc^.last:=key; string[l]:=0c;
    IF buf#NIL THEN put_buf(buf,TRUE,string,l,desc) END;
    DISPOSE(desc^.buf^.prefix);
        NEW(desc^.buf^.prefix);
  END exit;

  VAR     pfx: STRING;
      pfx_len: INTEGER;
           pi: INTEGER;

  PROCEDURE read(VAR key: CHAR);
  BEGIN
    IF pfx_len>0 THEN
         key:=pfx[pi]; INC(pi); DEC(pfx_len);
    ELSE desc^.read(key) END;
  END read;

  VAR i: INTEGER;
BEGIN
  desc:=dsc; IF desc=NIL THEN desc:=dflt END;
  ASSERT(desc^.buf^.magic=MAGIC,4Fh);
  IF (desc^.buf^.buf^.ADR#NIL) & (desc^.buf^.buf^.HIGH>0) THEN
       buf:=desc^.buf;
  ELSE buf:=NIL
  END;

  IF buf#NIL THEN buf^.bp:=0 END;

  stop_bytes(term_scale,terminators);
  tty.print('%s',prompt);
  IF HIGH(string)<0 THEN desc^.last:=0c;
      DISPOSE(desc^.buf^.prefix); NEW(desc^.buf^.prefix);
      RETURN
  END;

  pfx^:=desc^.buf^.prefix^;
  pfx_len:=0; pi:=0;
  WHILE (pfx_len<=HIGH(pfx)) & (pfx[pfx_len]#0c) DO INC(pfx_len) END;

  l:=0; cs:=0;
  WHILE (l<HIGH(string)) & (string[l]#0c) DO
     IF control(string[l]) THEN INC(cs) END;
     INC(l);
  END;
  maxl:= HIGH(string);
  vt52:=FALSE;
  tty.ins_char(1); IF NOT tty.done THEN vt52:=TRUE END;
  tty.del_char(1); IF NOT tty.done THEN vt52:=TRUE END;

  sp:=0; cp:=0;
  IF _empty IN desc^.how THEN l:=0; cs:=0; string:='' END;
  IF _refresh IN desc^.how THEN
       IF _bol IN desc^.how THEN refresh ELSE end END;
  ELSIF NOT (_bol IN desc^.how) THEN
       sp:=l; to(l+cs); IF l=maxl THEN moveleft END;
  END;

  LOOP
    read(key);
    IF desc^.valid=std_valid THEN
      IF (ORD(key) MOD bits) IN term_scale[ORD(key) DIV bits] THEN
          exit; RETURN
      END;
    END;
    IF desc^.valid(key) THEN
        IF (ORD(key) MOD bits) IN term_scale[ORD(key) DIV bits] THEN
          exit; RETURN
        END;
        CASE key OF
          |keyb.f1     : read(key);
                         IF desc^.valid(key) THEN
                             IF    key=keyb.del   THEN delete_word(TRUE)
                             ELSIF key=keyb.back  THEN word_GOLD_back
                             ELSE bell END;
                         END;
          |keyb.f2     : read(key);
                         IF desc^.valid(key) THEN
                             IF    key=keyb.del   THEN delete_word(FALSE)
                             ELSIF key=keyb.back  THEN word_back
                             ELSIF key=keyb.left  THEN word_left
                             ELSIF key=keyb.right THEN word_right
                             ELSE bell END;
                         END;
          |keyb.f4     : to(0); tty.erase_line(0); l:=0; sp:=0; cs:=0;
          |keyb.f7     : duptail
          |keyb.f8     : l:=sp;
                         IF cs>0 THEN cs:=0;
                             FOR i:=0 TO l-1 DO
                                INC(cs,ORD(control(string[i])))
                             END;
                         END;
                         tty.erase_line(0);
          |keyb.tab    : tab(FALSE)
          |keyb.bcktab : tab(TRUE)
          |lpull       : Lpull
          |rpull       : Rpull
          |keyb.left   : moveleft
          |keyb.right  : moveright
          |keyb.back   : delch;
          |keyb.ins    : insert(' ');
          |keyb.del    : delete;
          |keyb.home   : to(0); sp:=0;
          |keyb.end    : end
          |keyb.up     : use_buf(+1)
          |keyb.dw     : use_buf(-1)
          |keyb.rep    : desc^.ins:=NOT desc^.ins;
          |keyb.lf     : l:=sp; tty.erase_line(0); exit; RETURN
          |ASCII.BEL   : desc^.bel:=NOT desc^.bel;
          |ASCII.DC2   : IF form_byte(key,desc) THEN putch(key) ELSE bell END;
          |ASCII.DC4   : desc^.read(key); putch(key);
        ELSE             putch(key)
        END;
    END;
  END;
END read_str;

PROCEDURE edit_str(VAL prompt: ARRAY OF CHAR;
                   VAR string: ARRAY OF CHAR;
               line,col1,col2: INTEGER;
                          dsc: descriptor;
              SEQ terminators: CHAR);

  VAR sp, cp, fc: INTEGER;
      rb, lb, wd: INTEGER;
         l, maxl: INTEGER;
              cs: INTEGER; -- number of controls in -string-
  VAR term_scale: SCALE;
             key: CHAR;
             buf: LINE_BUFFER;
            desc: descriptor;

  VAR vt52: BOOLEAN;

  PROCEDURE db;           BEGIN del_byte(string,sp,l) END db;

  PROCEDURE ib(ch: CHAR); BEGIN ins_byte(string,ch,sp,l,maxl) END ib;

  PROCEDURE bell;         BEGIN IF desc^.bel THEN desc^.bell(1) END END bell;

  PROCEDURE pos(p: INTEGER); BEGIN tty.set_pos(line,col1+p) END pos;

  PROCEDURE sync; BEGIN tty.set_pos(line,col1+cp) END sync;

  PROCEDURE w(ch: CHAR);
  BEGIN
    IF control(ch) THEN
         tty.Write("^"); INC(cp);
         IF cp<wd THEN tty.Write(CHAR(ORD(ch)+100b)); INC(cp) END;
    ELSE tty.Write(ch); INC(cp) END;
  END w;

  PROCEDURE p(ch: CHAR);
  BEGIN
    IF control(ch) THEN
      tty.Write("^");
      IF (cp+1)<wd THEN tty.Write(CHAR(ORD(ch)+100b)); tty.left(1) END;
      tty.left(1);
    ELSE
      tty.Write(ch); tty.left(1)
    END;
  END p;

  PROCEDURE clear(ctrl: BOOLEAN);
  BEGIN
    IF ctrl THEN
      tty.Write(' ');
      IF (cp+1)<wd THEN tty.Write(' '); tty.left(1) END;
      tty.left(1);
    ELSE
      tty.Write(' '); tty.left(1)
    END;
  END clear;

  PROCEDURE r(from: INTEGER; pos: INTEGER);
    VAR p,i: INTEGER;
        ch: CHAR; ctrl: BOOLEAN;
  BEGIN
    tty.set_cursor(0);
    p:=cp; cp:=pos; sync;
    i:=from;
    WHILE (i<l) & (cp<wd) DO
      ch:=string[i];
      IF ORD(ch) MOD 128 < 40b THEN
        tty.Write("^"); INC(cp);
        IF cp<wd THEN tty.Write(CHAR(ORD(ch)+100b)); INC(cp) END;
      ELSE tty.Write(ch); INC(cp)
      END;
      INC(i);
    END;
    WHILE cp<wd DO tty.Write(' '); INC(cp) END;
    cp:=p;
    sync;
    tty.set_cursor(1);
  END r;

  PROCEDURE refresh;      BEGIN r(fc,0)  END refresh;
  PROCEDURE refresh_tail; BEGIN r(sp,cp) END refresh_tail;

  PROCEDURE bol;
  BEGIN
    sp:=0; cp:=0;
    IF fc#0 THEN fc:=0; refresh END;
    sync;
  END bol;

  PROCEDURE eol(first?,show?: BOOLEAN);
    VAR width,rwd,i,sw: INTEGER;
        ctrl: BOOLEAN;
    PROCEDURE set;
    BEGIN
      IF first? & show? THEN cp:=0; refresh END;
      width:=width-rwd;
      IF l<maxl THEN
        sp:=l; cp:=width;
      ELSE
        sp:=l-1; cp:=width-1;
        IF control(string[sp]) THEN DEC(cp) END;
      END;
      sync;
    END set;
  BEGIN
    IF l=0 THEN
        sp:=0; cp:=0; fc:=0;
        IF first? & show? THEN refresh END; sync; RETURN
    END;
    IF l=maxl THEN width:=wd ELSE width:=wd-1 END;
    i:=l-1; rwd:=width;
    LOOP
      ctrl:=control(string[i]);
      sw:=1+ORD(ctrl);
      IF rwd<sw THEN
        IF fc=(i+1) THEN set; RETURN END;
        IF (i+1)=l THEN -- строка вообще не помещается в окно
          fc:=l-1; sp:=l-1; cp:=0;
          IF show? THEN refresh ELSE sync END; RETURN
        END;
        fc:=i+1; first?:=TRUE; set; RETURN
      END;
      DEC(rwd,sw);
      IF i=0 THEN set; RETURN END; -- в окно поместилась вся строка
      DEC(i);
    END;
  END eol;

  PROCEDURE insert(ch: CHAR);
    VAR ctrl,cc: BOOLEAN;
  BEGIN
    IF l=maxl THEN bell; RETURN END;
    cc:=(sp=l) OR (sp=l-1) & (string[sp]=' '); -- +++
    ib(ch);
    ctrl:= control(ch);
    IF ctrl THEN INC(cs) END;
    IF cc THEN p(ch); RETURN END; -- ++
    IF vt52 THEN refresh_tail; RETURN END;
    IF cs=0 THEN
      IF cp<(wd-1) THEN
        pos(wd-1); tty.del_char(1); sync; tty.ins_char(1)
      END;
      tty.Write(ch); tty.left(1);
      RETURN
    END;
    IF (cp=wd-1) OR (ctrl & (cp=wd-2)) THEN
      p(ch); RETURN
    END;
    pos(wd-1-ORD(ctrl)); tty.del_char(1+ORD(ctrl));
    sync; tty.ins_char(1+ORD(ctrl)); p(ch);
  END insert;

  PROCEDURE delete;
    VAR ctrl: BOOLEAN; ch: CHAR;
  BEGIN
    IF sp=l THEN bell; RETURN END;
    IF (cs=0) & NOT vt52 THEN db;
      IF sp=l THEN tty.Write(' '); tty.left(1); RETURN END;
      tty.del_char(1); pos(wd-1); tty.ins_char(1);
      IF (fc+wd-1)<l THEN ch:=string[fc+wd-1] ELSE ch:=' ' END;
      tty.Write(ch); sync; RETURN
    END;
    ctrl:=control(string[sp]);
    IF ctrl THEN DEC(cs) END;
    db;
    IF sp=l THEN clear(ctrl); RETURN END;
    refresh_tail;
  END delete;

  PROCEDURE shiftright;
    VAR ctrl: BOOLEAN;
  BEGIN
    IF (fc=0) OR ((sp-fc)>=lb) THEN sync; RETURN END;
    ASSERT((sp-fc)=(lb-1));
    DEC(fc);
    IF vt52 THEN
      INC(cp,1+ORD(control(string[fc])));
      refresh; RETURN
    END;
    IF cs=0 THEN
      INC(cp); pos(wd-1); tty.del_char(1);
      pos(0); tty.ins_char(1); tty.Write(string[fc]); sync;
      RETURN
    END;
    ctrl:=control(string[fc]);
    INC(cp,1+ORD(ctrl));
    IF ctrl & (wd>1) THEN
      pos(wd-2); tty.del_char(2); pos(0); tty.ins_char(2);
    ELSE
      pos(wd-1); tty.del_char(1); pos(0); tty.ins_char(1)
    END;
    p(string[fc]); sync;
  END shiftright;

  PROCEDURE moveleft;
    VAR ctrl: BOOLEAN;
  BEGIN
    IF sp=0 THEN bell; RETURN END;
    DEC(sp);
    ctrl:= control(string[sp]);
    DEC(cp); IF ctrl THEN DEC(cp) END;
    IF (fc=0) OR (sp-fc>=lb) THEN
      tty.left(1+ORD(ctrl))
    ELSE
      shiftright
    END
  END moveleft;

  PROCEDURE full?(): BOOLEAN;
    VAR i,c: INTEGER;
  BEGIN c:=0;
    FOR i:=sp TO l-1 DO INC(c,ORD(control(string[i]))) END;
    RETURN (cp+maxl-sp+c)<=wd
  END full?;

  PROCEDURE shiftleft;
    VAR s: INTEGER;
  BEGIN
    ASSERT(l>0);
    s:=1+ORD(control(string[fc]));
    IF s>=(cp-rb) THEN INC(fc);
    ELSE
      INC(s,1+ORD(control(string[fc+1])));
      INC(fc,2)
    END;
    DEC(cp,s);
    refresh
  END shiftleft;

  PROCEDURE shl;
  BEGIN
    IF (cp<=rb) OR ((maxl-fc)<=wd) THEN sync; RETURN END;
    INC(fc); DEC(cp); pos(0);
    tty.del_char(1); pos(wd-1); tty.ins_char(1);
    IF (fc+wd-1)<l THEN tty.Write(string[fc+wd-1]) ELSE tty.Write(' ') END;
    sync
  END shl;

  PROCEDURE mvr;
  BEGIN
    IF (l=maxl) & (sp>=l-1) THEN bell; RETURN END;
    IF sp<l THEN INC(sp); INC(cp);
      IF (cp<=rb) OR ((maxl-fc)<=wd) THEN tty.right(1) ELSE shl END;
    ELSE string[l]:=' '; INC(l);
      IF l<maxl THEN mvr END;
    END;
  END mvr;

  PROCEDURE moveright;
    VAR ctrl: BOOLEAN;
  BEGIN
    IF (l=maxl) & (sp>=l-1) THEN bell; RETURN END;
    IF (cs=0) & NOT vt52 THEN mvr; RETURN END;
    IF sp<l THEN
      ctrl:=control(string[sp]);
      INC(sp); INC(cp,1+ORD(ctrl));
      IF (cp<=rb) OR full?() THEN tty.right(1+ORD(ctrl));
      ELSE shiftleft
      END;
    ELSE
      string[l]:=' '; INC(l);
      IF l<maxl THEN moveright END;
    END;
  END moveright;

  PROCEDURE tab(left: BOOLEAN);
  BEGIN
    IF left THEN
      moveleft;
      WHILE (sp MOD 8)#0 DO moveleft END;
    ELSE
      moveright;
      WHILE (sp MOD 8 # 0) & (sp<maxl-1) DO moveright END;
    END;
  END tab;

  PROCEDURE Lpull;
    VAR i,j: INTEGER;
  BEGIN
    IF (sp=l) OR (string[sp]#' ') THEN RETURN END;
    i:=sp;
    WHILE (i<l) & (string[i]=' ') DO INC(i) END;
    FOR j:=i TO l-1 DO string[sp+j-i]:=string[j] END;
    DEC(l,i-sp);
    refresh_tail;
  END Lpull;

  PROCEDURE Rpull;
    VAR i,j,p: INTEGER;
  BEGIN i:=0;
    WHILE (i<l) & (string[i]=' ') DO INC(i) END;
    IF (i=l) OR (i=sp) THEN RETURN END;
    IF i>sp THEN Lpull; RETURN END;
    IF (sp-i)>(maxl-l) THEN bell; RETURN END;
    p:=sp; sp:=i; FOR j:=1 TO p-sp DO ib(' ') END; sp:=p;
    cp:=sp-fc; refresh;
  END Rpull;

  PROCEDURE fputch(ch: CHAR);
  -- Put non-control symbol -ch- into string without controls --
  -- Terminal is NOT vt52                                     --
  BEGIN
    IF desc^.ins OR (sp=l) THEN
         ib(ch);
         IF (cp<(wd-1)) & (sp<(l-1)) THEN
             pos(wd-1); tty.del_char(1);
             sync;      tty.ins_char(1);
         END;
    ELSE string[sp]:=ch END;
    tty.Write(ch); INC(sp); INC(cp);
    IF (cp<=rb) OR ((maxl-fc)<=wd) THEN RETURN END;
    INC(fc); DEC(cp); pos(0);
    tty.del_char(1); pos(wd-1); tty.ins_char(1);
    IF (fc+wd-1)<l THEN tty.Write(string[fc+wd-1]) ELSE tty.Write(' ') END;
    sync;
  END fputch;

  PROCEDURE putch(ch: CHAR);
    VAR ctrl, rctrl: BOOLEAN;
  BEGIN
    IF (l=maxl) & (desc^.ins OR (sp>=l)) THEN bell; RETURN END;
    ctrl:=control(ch);
    IF (NOT ctrl) AND (cs=0) AND (NOT vt52) THEN fputch(ch); RETURN END;
    IF desc^.ins OR (sp=l) THEN insert(ch); moveright; RETURN END;
    rctrl:=control(string[sp]);
    string[sp]:=ch;
    IF    ctrl=rctrl THEN p(ch)
    ELSIF rctrl      THEN DEC(cs); refresh_tail
    ELSE
      INC(cs);
      IF vt52 THEN refresh_tail
      ELSE
        IF cp<(wd-1) THEN
          pos(wd-1); tty.del_char(1); sync; tty.ins_char(1)
        END; p(ch)
      END;
    END;
    moveright;
  END putch;

  PROCEDURE delch;
  BEGIN
    IF sp=0 THEN bell; RETURN END;
    moveleft;
    IF desc^.ins THEN delete ELSE putch(' '); moveleft END;
  END delch;

----------------------- Words Processing -----------------------------
                        ----------------
  PROCEDURE place(pos: INTEGER; cpos: INTEGER;
                  VAR fpos: INTEGER; VAR acpos: INTEGER);

    VAR rwd,iwd,ind: INTEGER;
  BEGIN
    IF wd=1 THEN fpos:=pos; acpos:=0; RETURN END;
    rwd:=cpos; ind:=pos;
    LOOP IF (rwd=0) OR (ind=0) THEN
             fpos:=ind; acpos:=cpos-rwd; RETURN
         END;
         DEC(ind);
         iwd:=1+ORD(control(string[ind]));
         IF rwd>=iwd THEN DEC(rwd,iwd)
         ELSE IF cpos<rb THEN acpos:=cpos+1; fpos:=ind;
                         ELSE acpos:=cpos-1; fpos:=ind+1;
              END;
              RETURN
         END;
    END;
  END place;

  PROCEDURE word_right;
    VAR ctrls,np: INTEGER;
  BEGIN
    skip_word(string,l,sp,np,ctrls); IF sp=np THEN RETURN END;
    IF (np=maxl) & (maxl>0) THEN
        DEC(np); DEC(ctrls,ORD(control(string[np])));
    END;
    INC(cp,np-sp+ctrls);
    sp:=np;
    IF cp<=rb THEN sync ELSE place(sp,rb,fc,cp); refresh END;
  END word_right;

  PROCEDURE word_left;
    VAR ctrls,np: INTEGER;
  BEGIN
    skip_word_left(string,l,sp,np,ctrls); IF sp=np THEN RETURN END;
    DEC(cp,sp-np+ctrls);
    sp:=np;
    IF (cp>=lb) OR (fc=0) THEN sync ELSE place(sp,lb,fc,cp); refresh END;
  END word_left;

  PROCEDURE del_substr(from,len,ctrls: INTEGER);
    VAR i: INTEGER;
  BEGIN
    IF len<=0 THEN RETURN END;
    FOR i:=0 TO l-from-len-1 DO string[from+i]:=string[from+len+i] END;
    DEC(l,len); DEC(cs,ctrls);
  END del_substr;

  PROCEDURE delete_word(GOLD: BOOLEAN);
    VAR np,ctrls: INTEGER;
  BEGIN
    IF GOLD THEN skip_GOLD_word(string,l,sp,np,ctrls)
            ELSE      skip_word(string,l,sp,np,ctrls)
    END;
    del_substr(sp,np-sp,ctrls);
    refresh_tail;
  END delete_word;

  PROCEDURE word_back(GOLD: BOOLEAN);
    VAR ctrls,np,n: INTEGER;
  BEGIN
    IF GOLD THEN skip_GOLD_word_left(string,l,sp,np,ctrls);
    ELSE skip_word_left(string,l,sp,np,ctrls) END;
    n:=sp-np; sp:=np;
    DEC(cp,n+ctrls);
    IF NOT GOLD THEN skip_word(string,l,sp,np,ctrls); n:=np-sp END;
    del_substr(sp,n,ctrls);
    IF (cp>=lb) OR (fc=0) THEN
         sync; refresh_tail;
    ELSE place(sp,lb,fc,cp); refresh END;
  END word_back;
----------------------------------------------------------------------

  PROCEDURE use_buf(dir: INTEGER);
  BEGIN
    IF buf=NIL THEN bell; RETURN END;
    IF dir>0 THEN
         IF buf^.bp=0 THEN put_buf(buf,FALSE,string,l,desc) END;
         IF buf^.bp>=HIGH(buf^.buf) THEN bell; RETURN END;
         INC(buf^.bp);
    ELSE IF buf^.bp<=0 THEN bell; RETURN END;
         DEC(buf^.bp);
    END;
    get_buf(buf,buf^.bp,0,maxl,string,l,cs);
    sp:=0; cp:=0; fc:=0; sync;
    IF _bol IN desc^.how THEN refresh ELSE eol(TRUE,TRUE) END;
  END use_buf;

  PROCEDURE duptail;
  BEGIN
    IF buf=NIL THEN bell; RETURN END;
    IF buf^.bp>=HIGH(buf^.buf) THEN bell; RETURN END;
    get_buf(buf,buf^.bp+1,sp,maxl,string,l,cs);
    refresh_tail;
  END duptail;

  PROCEDURE put_prompt;
    VAR i: INTEGER;
  BEGIN
    i:=0; tty.set_pos(line,col1);
    WHILE (i<=HIGH(prompt)) & (prompt[i]#0c) & (col1<=col2) DO
      tty.Write(prompt[i]); INC(i); INC(col1);
    END;
  END put_prompt;

  VAR     pfx: STRING;
      pfx_len: INTEGER;
           pi: INTEGER;

  PROCEDURE read(VAR key: CHAR);
  BEGIN
    IF pfx_len>0 THEN
         key:=pfx[pi]; INC(pi); DEC(pfx_len);
    ELSE desc^.read(key) END;
  END read;

  PROCEDURE dummy_loop;
    PROCEDURE term():BOOLEAN;
    BEGIN RETURN (ORD(key) MOD bits) IN term_scale[ORD(key) DIV bits]
    END term;
  BEGIN
    LOOP
      read(key);
      IF (desc^.valid=std_valid) & term() THEN RETURN END;
      IF desc^.valid(key) THEN
          IF term() OR (key=keyb.lf) THEN RETURN ELSE bell END;
      END;
    END;
  END dummy_loop;

  PROCEDURE exit;
  BEGIN desc^.last:=key; string[l]:=0c;
    IF buf#NIL THEN put_buf(buf,TRUE,string,l,desc) END;
    DISPOSE(desc^.buf^.prefix); NEW(desc^.buf^.prefix);
  END exit;

  VAR i: INTEGER;
BEGIN
  tty.nop; desc:=dsc; IF desc=NIL THEN desc:=dflt END;
  ASSERT(desc^.buf^.magic=MAGIC,4Fh);
  IF (desc^.buf^.buf^.ADR#NIL) & (desc^.buf^.buf^.HIGH>0) THEN
       buf:=desc^.buf;
  ELSE buf:=NIL END;

  IF buf#NIL THEN buf^.bp:=0 END;

  pfx^:=desc^.buf^.prefix^;
  pfx_len:=0; pi:=0;
  WHILE (pfx_len<=HIGH(pfx)) & (pfx[pfx_len]#0c) DO INC(pfx_len) END;

  stop_bytes(term_scale,terminators);
  IF (HIGH(string)<0) OR (col1>col2) OR
     (col1<0) OR (col2>=tty.state^.columns) THEN
      desc^.last:=0c;
      DISPOSE(desc^.buf^.prefix); NEW(desc^.buf^.prefix);
      RETURN
  END;
  put_prompt;
  l:=0; cs:=0;
  WHILE (l<HIGH(string)) & (string[l]#0c) DO
    IF control(string[l]) THEN INC(cs) END;
    INC(l)
  END;
  maxl:=HIGH(string);
  wd:=col2-col1+1;
  IF wd=0 THEN dummy_loop; exit; RETURN END;
  vt52:=(wd<=16);
  IF NOT vt52 THEN
      tty.ins_char(1); IF NOT tty.done THEN vt52:=TRUE END;
      tty.del_char(1); IF NOT tty.done THEN vt52:=TRUE END;
  END;
  sp:=0; cp:=0; fc:=0; sync;
  IF    wd>=14 THEN lb:=3; rb:=wd-4;
  ELSIF wd>=11 THEN lb:=2; rb:=wd-3;
  ELSIF wd>= 8 THEN lb:=1; rb:=wd-2;
  ELSE              lb:=0; rb:=wd-1;
  END;

  IF _empty IN desc^.how THEN l:=0; cs:=0; string:='' END;
  IF _refresh IN desc^.how THEN
       IF _bol IN desc^.how THEN refresh ELSE eol(TRUE,TRUE) END;
  ELSIF NOT (_bol IN desc^.how) THEN
       eol(TRUE,FALSE);
  END;

  LOOP
    read(key);
    IF desc^.valid=std_valid THEN
      IF (ORD(key) MOD bits) IN term_scale[ORD(key) DIV bits] THEN
          exit; RETURN
      END;
    END;
    IF desc^.valid(key) THEN
        IF (ORD(key) MOD bits) IN term_scale[ORD(key) DIV bits] THEN
          exit; RETURN
        END;

        CASE key OF
          |keyb.f1     : read(key);
                         IF desc^.valid(key) THEN
                             IF    key=keyb.del  THEN delete_word(TRUE)
                             ELSIF key=keyb.back THEN word_back(TRUE)
                             ELSE bell END;
                         END;
          |keyb.f2     : read(key);
                         IF desc^.valid(key) THEN
                             IF    key=keyb.del   THEN delete_word(FALSE)
                             ELSIF key=keyb.back  THEN word_back(FALSE)
                             ELSIF key=keyb.left  THEN word_left
                             ELSIF key=keyb.right THEN word_right
                             ELSE bell END;
                         END;
          |keyb.f4     : l:=0; sp:=0; fc:=0; cp:=0; cs:=0; refresh_tail;
          |keyb.f7     : duptail;
          |keyb.f8     : l:=sp;
                         IF cs>0 THEN cs:=0;
                             FOR i:=0 TO l-1 DO
                                INC(cs,ORD(control(string[i])));
                             END;
                         END;
                         refresh_tail;
          |keyb.tab    : tab(FALSE)
          |keyb.bcktab : tab(TRUE)
          |lpull       : Lpull
          |rpull       : Rpull
          |keyb.left   : moveleft
          |keyb.right  : moveright
          |keyb.back   : delch;
          |keyb.ins    : insert(' ');
          |keyb.del    : delete;
          |keyb.home   : bol;
          |keyb.end    : eol(FALSE,TRUE);
          |keyb.up     : use_buf(+1);
          |keyb.dw     : use_buf(-1);
          |keyb.rep    : desc^.ins:=NOT desc^.ins;
          |keyb.lf     : l:=sp; refresh_tail; exit; RETURN
          |ASCII.BEL   : desc^.bel:=NOT desc^.bel;
          |ASCII.DC2   : IF form_byte(key,desc) THEN putch(key) ELSE bell END;
          |ASCII.DC4   : desc^.read(key); putch(key);
        ELSE             putch(key)
        END;
    END;
  END;
END edit_str;

PROCEDURE set_prefix(desc: descriptor; VAL pfx: ARRAY OF CHAR;
                                       VAR done: BOOLEAN);
  VAR i,l: INTEGER;
BEGIN
  IF desc=NIL THEN desc:=dflt END;
  IF (desc^.buf=NIL) OR (desc^.buf^.magic#MAGIC) THEN
     done:=FALSE; RETURN
  END;
  l:=0; WHILE (l<=HIGH(pfx)) & (pfx[l]#0c) DO INC(l) END;
  DISPOSE(desc^.buf^.prefix);
  NEW(desc^.buf^.prefix,l+1);
  IF desc^.buf^.prefix^.ADR=NIL THEN done:=FALSE; RETURN END;
  FOR i:=0 TO l-1 DO desc^.buf^.prefix[i]:=pfx[i] END;
  desc^.buf^.prefix[l]:=0c;
  done:=TRUE;
END set_prefix;

PROCEDURE mk_vkeys;
  VAR i: INTEGER;
BEGIN
  mk_scale(vkeys,keyb.cr,keyb.lf,ASCII.NL,
                 keyb.f1,keyb.f2,keyb.f4,keyb.f7,keyb.f8,
                 keyb.tab,keyb.bcktab,
                 lpull,rpull,keyb.left,keyb.right,
                 keyb.back,keyb.ins,keyb.del,keyb.home,keyb.end,
                 keyb.up,keyb.dw,keyb.rep,
                 ASCII.BEL,ASCII.DC2,ASCII.DC4
          );
  FOR i:=0 TO 255 DO
     IF NOT control(CHAR(i)) THEN
         INCL(vkeys[i DIV bits],i MOD bits)
     END;
  END;
END mk_vkeys;

PROCEDURE chk_assume; -- A bit of crazy testing
  VAR i,j: INTEGER;
BEGIN i:=1; j:=2;
  ASSERT(ORD(i<j)=1);
  ASSERT(ORD(i>j)=0);
END chk_assume;

BEGIN
  chk_assume;
  mk_vkeys;
  std_valid:=_std_valid;
  dflt_buf.magic:=MAGIC; dflt_buf.bp:=0;
  NEW(dflt_buf.buf);
  NEW(dflt_buf.prefix);
  dflt_rec.ins:=TRUE;  dflt_rec.bel:=TRUE;
  dflt_rec.how:=empty; dflt_rec.last:=0c;
  dflt_rec.valid:=std_valid;
  dflt_rec.read:=keyb.read;
  dflt_rec.bell:=keyb.bell;
  dflt_rec.buf:=LINE_BUFFER(sys.ADR(dflt_buf));
  dflt:=descriptor(sys.ADR(dflt_rec));
END strEditor.
