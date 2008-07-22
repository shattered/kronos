MODULE ptp; (* Leg 14-Nov-88. (c) KRONOS *) (*$T+*)

IMPORT  sys: SYSTEM,    io : StdIO,     arg: tskArgs,
        tty: Terminal,  key: Keyboard,  tim: Time,
        bio: BIO,       str: Strings,   lp : Printer,
        asc: ASCII,     wlk: fsWalk,    tsk: tskEnv,
        low: lowLevel;

CONST Hh = 2;  -- lines for header
      Th = 3;  -- lines for tailer

TYPE Str256 = ARRAY [0..256] OF CHAR;
     Decor  = ARRAY [0..001] OF Str256;
     Item   = RECORD
                fnt : INTEGER;
                some: INTEGER;
                rvrs: INTEGER;
                uder: INTEGER;
                wx2 : INTEGER;
                hx2 : INTEGER;
                back: INTEGER;
              END;
     Stack  = ARRAY [0..031] OF Item;

VAR
  header : Decor;
  tailer : Decor;
  bars   : ARRAY [0..2] OF ARRAY [0..2] OF CHAR;
  vbar   : CHAR;
  hbar   : CHAR;
  f_name : Str256;
  save   : Stack;   -- save font stack
  pointer: INTEGER; -- index in save font stack
  lines  : INTEGER; -- lines per page
  lfs    : INTEGER; -- no of half LF per one line
  PH     : INTEGER; -- paper height
  ph     : INTEGER; -- current paper height
  space  : INTEGER; -- skip between pages in roll mode
  width  : INTEGER; -- characters per line
  Margin : INTEGER; -- left margin for odd pages
  margin : INTEGER; -- left margin for all pages
  pgno   : INTEGER; -- page number
  pages  : INTEGER; -- skip page number
  delay  : INTEGER;
  upma   : INTEGER; -- up margin
  line   : INTEGER; -- current line in file
  spec   : CHAR;    -- special simbol (default is '@')
  print? : BOOLEAN;
  ts     : Item;    -- state for tto

  start  : RECORD
             header,
             tailer : Decor;   -- header, tailer,
             pos    : INTEGER; -- position in file and
             state  : Item;    -- save state for again sheet printing
           END;

  flags  : BITSET;

CONST
  hdr = {0}; tlr = {1}; -- print(/no print) header(tailer)
  roll= {2};
  inc = {3};  -- reset(/not reset) page counter after each printed file
  one = {4};  -- use only one(/both) side of paper
  tto = {5};  -- print on terminal/printer
  ctl = {6};  -- interpret '@'_control_sequences/ignore one
  que = {7};  -- require(/no require) prompt for each page

PROCEDURE WriteLn;
BEGIN
  IF tto*flags#{} THEN tty.WriteLn ELSE lp.WriteLn END;
END WriteLn;

PROCEDURE eject;
BEGIN
  IF tto*flags={} THEN lp.eject
  ELSE WriteLn; tty.WriteLn; tty.WriteLn;
  END
END eject;

PROCEDURE fhlf(i: INTEGER);
BEGIN IF tto*flags={} THEN lp.fhlf(i) END END fhlf;

PROCEDURE bhlf(i: INTEGER);
BEGIN IF tto*flags={} THEN lp.bhlf(i) END END bhlf;

PROCEDURE right(i: INTEGER);
BEGIN IF tto*flags={} THEN lp.right(i) END END right;

PROCEDURE set_font(i: INTEGER);
BEGIN IF tto*flags#{} THEN RETURN END;
  IF i#lp.state^.font THEN lp.set_font(i) END;
END set_font;

PROCEDURE set_underline(i: INTEGER);
BEGIN
  IF tto*flags#{} THEN tty.set_underline(i); ts.uder:=i
  ELSIF i#lp.state^.underline THEN lp.set_underline(i) END
END set_underline;

PROCEDURE set_something(i: INTEGER);
BEGIN
  IF tto*flags#{} THEN tty.set_something(i); ts.some:=i
  ELSIF i#lp.state^.something THEN lp.set_something(i) END
END set_something;

PROCEDURE set_reverse(i: INTEGER);
BEGIN
  IF tto*flags#{} THEN tty.set_reverse(i); ts.rvrs:=i;
  ELSIF i#lp.state^.reverse THEN lp.set_reverse(i) END
END set_reverse;

PROCEDURE set_Wx2(i: INTEGER);
BEGIN
  IF tto*flags#{} THEN RETURN END;
  IF i#lp.state^.underline THEN lp.set_Wx2(i) END
END set_Wx2;

PROCEDURE set_Hx2(i: INTEGER);
BEGIN
  IF tto*flags#{} THEN RETURN END;
  IF i#lp.state^.Wx2 THEN lp.set_Hx2(i) END
END set_Hx2;

PROCEDURE set_ground(i: INTEGER);
BEGIN
  IF tto*flags#{} THEN tty.set_back(i-tty.state^.min_color); ts.back:=i
  ELSIF i#lp.state^.ground THEN lp.set_ground(i) END
END set_ground;

PROCEDURE hchar(): INTEGER;
BEGIN
  IF tto*flags#{} THEN RETURN 10 ELSE RETURN lp.state^.hchar END;
END hchar;

PROCEDURE font(): INTEGER;
BEGIN
  IF tto*flags#{} THEN RETURN 0
  ELSE RETURN lp.state^.font
  END
END font;

PROCEDURE something(): INTEGER;
BEGIN
  IF tto*flags#{} THEN RETURN ts.some
  ELSE RETURN lp.state^.something
  END
END something;

PROCEDURE reverse(): INTEGER;
BEGIN
  IF tto*flags#{} THEN RETURN ts.rvrs
  ELSE RETURN lp.state^.reverse
  END
END reverse;

PROCEDURE underline(): INTEGER;
BEGIN
  IF tto*flags#{} THEN RETURN ts.uder
  ELSE RETURN lp.state^.underline
  END
END underline;

PROCEDURE Wx2(): INTEGER;
BEGIN
  IF tto*flags#{} THEN RETURN 0
  ELSE RETURN lp.state^.Wx2
  END
END Wx2;

PROCEDURE Hx2(): INTEGER;
BEGIN
  IF tto*flags#{} THEN RETURN 0
  ELSE RETURN lp.state^.Hx2
  END
END Hx2;

PROCEDURE ground(): INTEGER;
BEGIN
  IF tto*flags#{} THEN RETURN ts.back
  ELSE RETURN lp.state^.ground
  END
END ground;

PROCEDURE letter(ch: CHAR): BOOLEAN;
BEGIN RETURN (ch>=300c) OR ("A"<=CAP(ch)) & (CAP(ch)<="Z") END letter;

PROCEDURE discuss;
  VAR ch: CHAR;
BEGIN
  tty.print('  Abort?');
  LOOP
    key.read(ch);
    IF (ch='y') OR (ch='Y') OR (ch='N') OR (ch='n') THEN EXIT
    ELSE key.bell(1)
    END;
  END;
  tty.print('\r'); tty.erase_line(0);
  IF CAP(ch)='Y' THEN HALT(1) END
END discuss;

PROCEDURE sys_err(n: INTEGER; VAL a: ARRAY OF CHAR);
BEGIN tty.perror(n,"Line %d -- %s: %%s\n",line,a); discuss
END sys_err;

PROCEDURE my_err(n: INTEGER);
BEGIN
  key.bell(1);
  tty.print('Line # %d ',line);
  CASE n OF
     0 : tty.print('Operation is completed normally.');
    |1 : tty.print('Save font stack is empty.')
    |2 : tty.print('Save font stack is full.')
    |3 : tty.print('Invalid header number.')
    |4 : tty.print('Invalid tailer number.')
    |5 : tty.print('Invalid header or tailer.')
    |6 : tty.print('Too long or unlimited header or tailer.')
    |7 : tty.print('Command "@nnn." is not implemented.')
    |8 : tty.print('Command "@^" is not implemented.')
    |9 : tty.print('Command "@_" is not implemented.')
    |13: tty.print('Invalid dot graphic density.')
    |14: tty.print('Invalid left margin for graphic.')
    |15: tty.print('Invalid bitmap file name.')
    |16: tty.print("Can't draw picture.")
    |17: tty.print('Insufficient memory for picture buffer.');
    |18: tty.print('Illegal bar no.');
  ELSE tty.print('\nUnknown error %d.',n)
  END;
  discuss;
 END my_err;

PROCEDURE e(n: INTEGER);
  VAR s: ARRAY [0..23] OF CHAR;
BEGIN
  IF tto*flags#{} THEN RETURN END;
  IF NOT lp.done THEN
    CASE n OF
      |0: s:='set font'
      |1: s:='set somethting'
      |2: s:='set reverse'
      |3: s:='set underline'
      |4: s:='set double width'
      |5: s:='set double height'
      |6: s:='set ground'
    END;
    tty.perror(lp.error,'Line # %d -- %s: %%s',line,s); discuss
  END
END e;

 PROCEDURE line_feed;
   VAR i,n: INTEGER;
 BEGIN
   IF tto*flags#{} THEN
     n:=lfs DIV 2; IF n=0 THEN n:=1 END;
     IF print? THEN
       FOR i:=1 TO n DO tty.WriteLn END;
     END;
   ELSE
     n:=lfs;
     IF ODD(n) &  print? THEN
       fhlf(1);
       IF lp.done THEN DEC(n)
       ELSIF n<2 THEN n:=2 END
     END;
     IF print? THEN FOR i:=1 TO n DIV 2 DO lp.WriteLn END END;
   END;
   INC(ph,hchar()*lfs DIV 2);
 END line_feed;

PROCEDURE sv(VAR it: Item);
BEGIN
  WITH it DO
    fnt :=font();
    hx2 :=Hx2();
    wx2 :=Wx2();
    some:=something();
    rvrs:=reverse();
    uder:=underline();
    back:=ground();
  END;
END sv;

PROCEDURE rs(VAL it: Item);
BEGIN
  WITH it DO
    IF font()#fnt       THEN set_font(fnt)       END;
    IF something()#some THEN set_something(some) END;
    IF reverse()#rvrs   THEN set_reverse(rvrs)   END;
    IF underline()#uder THEN set_underline(uder) END;
    IF Wx2()#wx2        THEN set_Wx2(wx2)        END;
    IF Hx2()#hx2        THEN set_Hx2(hx2)        END;
    IF ground()#back    THEN set_ground(back)    END;
  END;
END rs;

PROCEDURE pop;
BEGIN
  IF pointer=0 THEN my_err(1); RETURN END;
  DEC(pointer); rs(save[pointer])
END pop;

PROCEDURE push;
BEGIN
  IF pointer>HIGH(save) THEN
    my_err(2); RETURN
  END;
  sv(save[pointer]);
  INC(pointer);
END push;

PROCEDURE out(c: CHAR);
BEGIN
  IF print? THEN
    IF tto*flags#{} THEN tty.Write(c)
    ELSE                 lp. Write(c) END;
  END
END out;

PROCEDURE repeat(ch: CHAR; no: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF print? THEN
    IF tto*flags#{} THEN tty.repeat(ch,no)
    ELSE                 lp. repeat(ch,no) END;
  END
END repeat;

PROCEDURE make_margin;
  VAR i: INTEGER;
BEGIN
  IF print? THEN
    i:=margin; IF (one*flags#{}) OR ODD(pgno) THEN INC(i,Margin) END;
    IF i=0 THEN RETURN END;
    push;
    IF font()#0      THEN set_font(0)      END;
    IF something()#0 THEN set_something(0) END;
    IF reverse()#0   THEN set_reverse(0)   END;
    IF underline()#0 THEN set_underline(0) END;
    IF Wx2()#0       THEN set_Wx2(0)       END;
    IF Hx2()#0       THEN set_Hx2(0)       END;
    IF ground()#0    THEN set_ground(0)    END;
    repeat(' ',i);
    pop;
  END;
END make_margin;

PROCEDURE make_control(VAL s: ARRAY OF CHAR; VAR i: INTEGER);
  VAR p,j,k: INTEGER;
BEGIN
  INC(i);
  IF s[i]=spec THEN out(spec)
  ELSE
    p:=0; j:=i-1;
    WHILE (i<HIGH(s)) & ("0"<=s[i]) & (s[i]<="9") DO
      p:=p*10+ORD(s[i])-ORD('0'); INC(i)
    END;
    CASE s[i] OF
      |'<': out(15c)
      |'U': set_underline(1); e(3);
      |'R': set_reverse(1); e(2);
      |'W': set_Wx2(1); e(4);
      |'H': set_Hx2(1); e(5);
      |'A': set_something(1); e(1);
      |'u': set_underline(0); e(3);
      |'r': set_reverse(0); e(2);
      |'w': set_Wx2(0); e(4);
      |'h': set_Hx2(0); e(5);
      |'a': set_something(0); e(1);
      |'g': set_ground(p); e(6);
      |'L': PH:=(PH DIV lines)*p; lines:=p
      |'^': IF print? THEN
              bhlf(1);
              IF NOT lp.done THEN my_err(8) END;
            END;
            INC(ph,hchar())
      |'_': IF print? THEN
              fhlf(2);
              IF NOT lp.done THEN my_err(9) END;
            END;
            DEC(ph,hchar())
      |'f': set_font(p); e(0);
      |'P': pgno:=p
      |'M': Margin:=p
      |'m': margin:=p
      |'l': lfs:=p
      |'.': IF print? THEN right(p);
              IF NOT lp.done THEN my_err(7) END;
            END;
      |'E': pop; INC(pointer)
      |'e': pop;
      |'s': push;
      |'=': spec:=s[i+1]; INC(i);
      |'#': IF p>8 THEN my_err(18); RETURN END;
            out(bars[p DIV 3,p MOD 3])
      |'-': repeat(hbar,p)
      |'|': out(vbar)
    ELSE
      out(spec); i:=j;
    END
  END;
END make_control;

PROCEDURE print(VAL s: ARRAY OF CHAR; len: INTEGER);
  VAR i,p,k: INTEGER;

  PROCEDURE out_s(ps,ln: INTEGER);
  BEGIN
    IF print? THEN
      IF tto*flags#{} THEN tty.write(s,ps,ln)
      ELSE                  lp.write(s,ps,ln)
      END
    END;
  END out_s;

BEGIN
  IF len=0 THEN line_feed; RETURN END;
  make_margin; i:=0; p:=0;
  IF ctl*flags#{} THEN
    WHILE i<len DO
      IF (s[i]=spec) THEN
        IF p#i THEN out_s(p,i-p) END;
        make_control(s,i);
        p:=i+1
      END;
      INC(i)
    END;
    IF p#i THEN out_s(p,i-p) END;
  ELSE out_s(0,len)
  END;
  line_feed
END print;

PROCEDURE get_word(VAR t,s: ARRAY OF CHAR; len: INTEGER);
  VAR i,p,l,ht,hs: INTEGER;
BEGIN
  ht:=HIGH(t); hs:=len-1; l:=0;
  i:=0; WHILE (i<=hs) & (s[i]=' ') DO INC(i) END; p:=i;
  WHILE (i<=hs) & (l<=ht) & (s[i]#' ') DO
    t[l]:=s[i]; INC(l); INC(i);
  END;
  IF l<=ht THEN t[l]:=0c END;
  str.delete(s,p,l)
END get_word;

PROCEDURE take_decor(VAR s: ARRAY OF CHAR; what: BITSET; len: INTEGER);
  VAR t,w   : Str256;
      hs,ht : INTEGER;
      no,i,j: INTEGER;
      p     : CHAR;
BEGIN
  get_word(t,s,len);
  IF    t[0]='0' THEN no:=0
  ELSIF t[0]='1' THEN no:=1
  ELSIF what=hdr THEN my_err(3); RETURN
  ELSE                my_err(4); RETURN
  END;
  hs:=len-1; ht:=HIGH(t); i:=0; j:=0; t:='';
  WHILE (i<=hs) & (s[i]#"'") & (s[i]#'"') DO INC(i) END;
  IF i>hs THEN my_err(5); RETURN END;
  p:=s[i]; INC(i);
  WHILE (i<=hs) & (j<=ht) & (s[i]#p) DO t[j]:=s[i]; INC(i); INC(j) END;
  IF j<=ht THEN t[j]:=0c END;
  IF i>hs THEN my_err(6); RETURN END;
  IF what=hdr THEN header[no]:=t ELSE tailer[no]:=t END;
END take_decor;

PROCEDURE control(s: ARRAY OF CHAR; len: INTEGER): BOOLEAN;
  VAR t: Str256;
BEGIN
  IF s[0]#'.' THEN RETURN FALSE END;
  get_word(t,s,len);
  IF t='.PAGE' THEN
    IF ph#0 THEN
      WHILE ph<PH DO line_feed END;
    END;
    RETURN TRUE
  ELSIF t='.HEAD' THEN take_decor(s,hdr,len); RETURN TRUE
  ELSIF t='.TAIL' THEN take_decor(s,tlr,len); RETURN TRUE
  ELSE RETURN FALSE END;
END control;

PROCEDURE wr_head(VAL s: ARRAY OF CHAR); VAR i: INTEGER;
BEGIN print(s,str.len(s)); FOR i:=1 TO Hh-1 DO line_feed END END wr_head;

PROCEDURE wr_tail(VAL s: ARRAY OF CHAR); VAR i: INTEGER;
BEGIN FOR i:=1 TO Th-1 DO line_feed END; print(s,str.len(s)) END wr_tail;

PROCEDURE app_image(VAR s: ARRAY OF CHAR; i: ARRAY OF CHAR);
  VAR t      : Str256;
      y,m,d,h: INTEGER;
      mn,sc,l: INTEGER;
BEGIN
  t:='';
  l:=str.len(i)-1;
  CASE i[l] OF
     'd','h','i'
    ,'b','o'    : str.append(s,i,pgno);
    |'s'        : str.append(s,i,f_name);
    |'x'        : i[l]:='s'; str.append(s,i,'');
    |'K'        : str.app(s,'(c) KRONOS')
    |'D'        : tim.unpack(tim.time(),y,m,d,h,mn,sc);
                  str.append(s,"%$2d-%$2d-%d",d,m,y);
    |'T'        : tim.unpack(tim.time(),y,m,d,h,mn,sc);
                  str.append(s,"%$2d:%$2d.%$2d",h,mn,sc);
  ELSE str.app(s,i) END;
END app_image;

PROCEDURE decorate(what: BITSET);
  VAR  s,t1,t   : Str256;
       i,l,no,p : INTEGER;
       sv       : BOOLEAN;
BEGIN
  no:=0;
  IF ODD(pgno) OR (one*flags#{}) THEN no:=1 END;
  IF what=hdr THEN t1:=header[no] ELSE t1:=tailer[no] END;
  i:=0; l:=str.len(t1); s:='';
  LOOP
    t:=''; p:=0;
    WHILE (i<l) & (t1[i]#'%') DO t[p]:=t1[i]; INC(i); INC(p) END;
    t[p]:=0c;
    str.app(s,t); IF i=l THEN EXIT END;
    t:=''; p:=0; DEC(i);
    REPEAT  INC(i); t[p]:=t1[i]; INC(p);
    UNTIL (i>=(l-1)) OR (letter(t1[i]));
    t[p]:=0c;
    INC(i);
    app_image(s,t);
    IF i>=l THEN EXIT END;
  END;
  sv:=(ctl*flags#{}); flags:=flags+ctl;
  push;
  IF what=hdr THEN wr_head(s) ELSE wr_tail(s) END;
  pop;
  IF sv THEN flags:=flags+ctl ELSE flags:=flags-ctl END;
END decorate;

VAR s_fil: bio.FILE;
    s_buf: ARRAY [0..4095] OF CHAR;
    s_pos: INTEGER;
    s_bp : INTEGER;
    s_eof: INTEGER;

CONST hb=HIGH(s_buf)+1;

PROCEDURE read_buf;
  VAR len: INTEGER;
BEGIN
  len:=s_eof-s_pos; IF len>hb THEN len:=hb END;
  bio.get(s_fil,s_buf,len);
  IF NOT bio.done THEN sys_err(bio.error,f_name) END;
END read_buf;

PROCEDURE init_buf;
BEGIN s_bp:=0; s_pos:=0; s_eof:=bio.eof(s_fil); read_buf END init_buf;

PROCEDURE set_pos(p: INTEGER);
  VAR bn,bo: INTEGER;
BEGIN
  bn:=p   DIV hb;
  bo:=s_pos DIV hb;
  IF bo#bn THEN
    s_pos:=bn*hb;
    bio.seek(s_fil,s_pos,0);
    IF NOT bio.done THEN sys_err(bio.error,f_name) END;
    read_buf;
  END;
  s_pos:=p;
  s_bp:=s_pos MOD hb;
END set_pos;

PROCEDURE get_s(VAR s: ARRAY OF CHAR; VAR len: INTEGER): BOOLEAN;
  VAR ch: CHAR;
BEGIN
  len:=0; IF s_pos=s_eof THEN s[0]:=0c; RETURN FALSE END;
  LOOP
    IF len>=HIGH(s) THEN EXIT END;
    ch:=s_buf[s_bp]; INC(s_bp); INC(s_pos);
    IF s_bp>HIGH(s_buf) THEN read_buf; s_bp:=0 END;
    IF (ch=36c) OR (ch=12c) OR (ch=0c) THEN EXIT END;
    IF ch=15c THEN
      IF s_pos=s_eof THEN EXIT END;
      IF s_buf[s_bp]=12c THEN
        INC(s_bp); INC(s_pos);
        IF s_bp>HIGH(s_buf) THEN read_buf; s_bp:=0 END;
      END;
      EXIT
    END;
    s[len]:=ch; INC(len)
  END;
  s[len]:=0c; INC(line);
  RETURN TRUE
END get_s;

PROCEDURE again(): BOOLEAN;
  VAR ch: CHAR;
BEGIN
  tty.print('\rPrint previous sheet?');
  tty.erase_line(0);
  LOOP
    key.read(ch); ch:=CAP(ch);
    IF (ch='Y') THEN
      DEC(pgno); INC(pages);
      set_pos(start.pos);
      rs(start.state);
      header:=start.header;
      tailer:=start.tailer;
      tty.print('\r'); tty.erase_line(0); RETURN TRUE
    ELSIF ch='N' THEN
      tty.print('\r'); tty.erase_line(0); RETURN FALSE
    ELSE key.bell(1)
    END;
  END;
END again;

PROCEDURE query(aga?: BOOLEAN);
  VAR i: INTEGER;
     ch: CHAR;

  CONST pmt='\rMay I print page %d ?';

BEGIN
  IF (que*flags={}) THEN
    tim.delay(delay,tim.sec); RETURN
  END;
  tty.print(pmt,pgno);
  LOOP
    key.read(ch);
    CASE CAP(ch) OF
      |0c        :
      |'A'       : IF aga? THEN
                     IF again() THEN RETURN END;
                     tty.print(pmt,pgno);
                   END;
      |'Y'       : print?:=TRUE; EXIT
      |'N'       : print?:=FALSE; EXIT
      |'Q'       : HALT
      |'2',key.dw: IF tto*flags={} THEN lp.bflf(1) END
      |'8',key.up: IF tto*flags={} THEN lp.WriteLn END
      | asc.CR   : IF tto*flags={} THEN
                     FOR i:=1 TO 18 DO lp.WriteLn END
                   END
    ELSE key.bell(1);
    END;
  END;
  tty.print('\r'); tty.erase_line(0)
END query;

PROCEDURE get_n(VAL s: ARRAY OF CHAR; VAR w: INTEGER; def,min: INTEGER);
BEGIN
  IF NOT arg.number(s,w) THEN w:=def; RETURN END;
  IF w<min THEN w:=min END
END get_n;

PROCEDURE init;
  VAR i: INTEGER;
BEGIN
  spec:='@'; pointer:=0;
  get_n('width',width ,64,1);
  get_n('lines',lines ,59,1);
  get_n('int'  ,lfs   ,02,1);
  get_n('gap'  ,space ,01,1);
  get_n('page' ,pgno  ,01,1);
  get_n('from' ,pages ,01,1); pages:=pages-pgno;
  get_n('ma'   ,Margin,05,1);
  get_n('mo'   ,margin,08,1);
  get_n('font' ,i     ,00,0);
  get_n('del'  ,delay ,00,0);
  get_n('upma' ,upma  ,00,0);
  flags:=hdr+tlr+que;
  IF arg.flag('-','h') THEN flags:=flags-hdr END;
  IF arg.flag('-','t') THEN flags:=flags-tlr END;
  IF arg.flag('+','i') THEN flags:=flags+ctl END;
  IF arg.flag('+','r') THEN flags:=flags+roll+one END;
  IF arg.flag('-','q') THEN flags:=flags-que END;
  IF arg.flag('+','o') THEN flags:=flags+one END;
  IF arg.flag('+','z') THEN flags:=flags+inc END;
  IF arg.flag('+','t') THEN flags:=flags+tto END;
  set_font(i);
  str.print(header[1],'@s@u@r@w@h@0g@A%%D%%%ds@e' ,width-11);
  str.print(tailer[1],'@s@u@r@w@h@0g@A%%K%%%dd@e' ,width-10);
  str.print(header[0],'@s@u@r@w@h@0g@A%%-%ds%%D@e',width-11);
  str.print(tailer[0],'@s@u@r@w@h@0g@A%%-%dd%%K@e',width-10);
  IF tlr*flags#{} THEN DEC(lines,Th) END; -- for header printing
  PH:=lines * hchar() * lfs DIV 2;
END init;

PROCEDURE type;
  VAR i,len: INTEGER;
      chars: INTEGER;
      s    : Str256;
      done : BOOLEAN;
BEGIN
  tty.print('Printing "%s"\n',f_name);
  done:=FALSE; line:=0;
  LOOP
    print?:=(pages<1);
    IF print? THEN query(line#0) END;
    sv(start.state);
    start.header:=header;
    start.tailer:=tailer;
    start.pos:=s_pos;
    ph:=0;
    REPEAT IF NOT get_s(s,len) THEN EXIT END
    UNTIL NOT control(s,len);
    FOR i:=1 TO upma DO line_feed END;
    IF hdr*flags#{} THEN decorate(hdr) END;
    LOOP
      IF NOT control(s,len) THEN print(s,len) END;
      IF ph>=PH THEN EXIT END;
      done:=NOT get_s(s,len)
    END;
    IF tlr*flags#{} THEN decorate(tlr) END;
    IF print? THEN
      IF roll*flags#{} THEN FOR i:=1 TO space DO line_feed END
      ELSE  eject END;
    ELSE  tty.print('%d\r',pgno)
    END;
    INC(pgno); DEC(pages);
    IF done THEN EXIT END;
  END;
  IF inc*flags={} THEN init END;
END type;

PROCEDURE walk(VAL patt: ARRAY OF CHAR);
  VAR tr  : wlk.TREE;
      dir : bio.FILE;
      ext : ARRAY [0..31] OF CHAR;
      mode: BITSET;
      i,l : INTEGER;
BEGIN
  wlk.walk(tr,patt,TRUE);
  IF NOT wlk.done THEN
    tty.print('Error in pattern:\n');
    tty.print('%s\n',patt);
    tty.print('%*c\n\n',wlk.pos,'^');
    HALT
  END;
  WHILE wlk.next_dir(tr) DO
    dir:=wlk.dir(tr);
    IF dir#bio.null THEN
      WHILE wlk.next_entry(tr,f_name,mode) DO
        l:=str.len(f_name); i:=l-1;
        WHILE (i>=0) & (f_name[i]#'.') DO DEC(i) END;
        str.sub_str(ext,f_name,i+1,l-i-1);
        IF (ext#"cod") & (ext#"ref") & (ext#"sym") THEN
          bio.fopen(dir,s_fil,f_name,"r");
          IF NOT bio.done THEN sys_err(bio.error,f_name) END;
          init_buf; type;
          bio.close(s_fil);
          IF NOT bio.done THEN sys_err(bio.error,f_name) END;
        END;
      END;
      IF NOT wlk.done THEN sys_err(wlk.error,'File search') END;
    END
  END;
  IF NOT wlk.done THEN sys_err(wlk.error,'File search') END;
END walk;

PROCEDURE help;
BEGIN
  tty.home; tty.erase(0);
  tty.print('ptp v0.2.1  /06-Mar-90/ (c) KRONOS \n');
  tty.print('   Produce Toilet Paper:\n');
  tty.print('ptp {<путь>/<образец>} [-htq] [+iroty] [lines=n] [width=n] [int=n]\n');
  tty.print('    [page=n] [from=n] [gap=n] [ma=n] [mo=n] [font=n] [del=n]\n');
  tty.print('Распечатывает файлы, сопоставившиеся с образцом\n');
  tty.print('Ключи:\n');
  tty.print('  +z    Сквозная нумерация страниц (для множества файлов)\n');
  tty.print('  +o    Печать с одной стороны листа\n');
  tty.print('  +r    (Roll) Печать на рулонной бумаге\n');
  tty.print('  -h(t) Не печатать верхнюю(нижнюю) декоративные строки\n');
  tty.print('  -q    Не запрашивать подтверждения на печать\n');
  tty.print('  +t    Вывод вместо принтера на терминал\n');
  tty.print('  +i    Рассматривать @-последовательности в тексте, как команды\n');
  tty.print('Числовые ключи (в скобках "[]" -- текущие значения параметров)\n');
  tty.print(' gap    [%3d] Промежуток между страницами в '
          'строках (с ключом "+r")\n',space);
  tty.print(' lines  [%3d] Размер страницы в строках\n',lines+
                                        INTEGER(tlr*flags#{})*Th+
                                        INTEGER(hdr*flags#{})*Hh);
  tty.print(' ma     [%3d] Левый отступ для нечетных страниц\n',Margin);
  tty.print(' mo     [%3d] Левый отступ для всех страниц\n',margin);
  tty.print(' page   [%3d] Начать нумерацию страниц с номера -n-\n',pgno);
  tty.print(' from   [%3d] Начать печать со страницы номер -n-\n',pages);
  tty.print(' font   [%3d] Установить текущий шрифт с номером -n-\n',font());
  tty.print(' int    [%3d] Интервал между строк -n-\n',lfs);
  tty.print(' upma   [%3d] Верхний отступ -n- строк\n',upma);

  HALT
END help;

VAR lp_env: STRING;
    i,j   : INTEGER;
    emp   : BOOLEAN;

BEGIN
  emp:=HIGH(arg.words)<0;
  flags:=hdr+tlr+que;
  IF arg.flag('+','t') THEN flags:=flags+tto END;
  IF (tto*flags={}) & NOT emp THEN
    IF NOT arg.string(tsk.lp,lp_env) THEN
      tty.print('No line printer driver name in environment\n\n'); HALT
    END;
    lp.nop;
    IF NOT lp.done THEN
      tty.perror(lp.error,'LP "%s" error: %%s\n',lp_env); HALT
    END;
    FOR i:=0 TO 2 DO FOR j:=0 TO 2 DO bars[i,j]:=lp.state^.bars[i,j] END END;
    hbar:=lp.state^.hbar; vbar:=lp.state^.vbar
  ELSE
    low.zero(ts);
    FOR i:=0 TO 2 DO FOR j:=0 TO 2 DO bars[i,j]:=tty.state^.bars[i,j] END END;
    hbar:=tty.state^.hbar; vbar:=tty.state^.vbar
  END;
  init;
  IF emp THEN help END;
  FOR i:=0 TO HIGH(arg.words) DO walk(arg.words[i]) END;
END ptp.
