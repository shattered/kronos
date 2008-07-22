IMPLEMENTATION MODULE cdsMenu; (* Sem 07-Feb-91. (c) KRONOS *)

IMPORT  str: Strings;
IMPORT  wnd: libWindows;
IMPORT  mem: libHeap;
IMPORT  vg : cdsGrafic;

FROM SYSTEM      IMPORT WORD, ADDRESS;

WITH STORAGE : mem;

CONST
  stp = 16;

TYPE
  menu_info=POINTER TO RECORD
    sel : BITSET;
    job : menu_proc;
  END;

PROCEDURE center(w: wnd.window);
BEGIN
  IF w^.x<-1000 THEN w^.x:=(wnd.screen^.sx-w^.sx) DIV 2;
  ELSIF w^.x<0 THEN w^.x:=0;
  ELSIF w^.x+w^.sx>wnd.screen^.sx THEN w^.x:=wnd.screen^.sx-w^.sx
  END;
  IF w^.y<-1000 THEN w^.y:=(wnd.screen^.sy-w^.sy) DIV 2;
  ELSIF w^.y<0 THEN w^.y:=0;
  ELSIF w^.y+w^.sy>wnd.screen^.sy THEN w^.y:=wnd.screen^.sy-w^.sy
  END;
END center;

PROCEDURE text_frame(w: wnd.window; x,y,sx,sy,c: INTEGER; VAL s: ARRAY OF CHAR);
  VAR l: INTEGER;
BEGIN
  w^.patt:={0..31}; vg.prop_font(w,TRUE);
  vg.color(w,15); vg.mode(w,vg.bic); vg.box(w,x,y,x+sx-1,y+sy-1);
  vg.color(w,1); vg.mode(w,vg.rep); vg.frame(w,x,y,x+sx-1,y+sy-1);
  vg.color(w,2); vg.box(w,x,y+1,x+sx-2,y+sy-1);
  vg.color(w,INTEGER(BITSET(c)/{1})); vg.mode(w,vg.xor);
  l:=vg.string_len(TRUE,s,0);
  vg.write_string(w,x+(sx-l) DIV 2,y-1+(sy-vg.char_h) DIV 2,s);
END text_frame;

PROCEDURE white_box(w: wnd.window; x,y,sx,sy: INTEGER);
BEGIN
  w^.patt:={0..31};
  vg.color(w,15); vg.mode(w,vg.bic); vg.box(w,x,y,x+sx-1,y+sy-1);
  vg.color(w,1); vg.mode(w,vg.rep); vg.frame(w,x,y,x+sx-1,y+sy-1);
  vg.color(w,2); vg.box(w,x,y+1,x+sx-2,y+sy-1);
END white_box;

PROCEDURE message_job(w: wnd.window; x,y: INTEGER; c: CHAR);
BEGIN
  x:=x-w^.x; y:=y-w^.y;
  IF (x>=2) & (x<=17) & (y>=2) & (y<=17) & (
     (c=wnd.l_on) OR (c=wnd.m_on) OR (c=wnd.r_on) ) THEN
    x:=w^.x; y:=w^.y;
    REPEAT wnd.read_point(wnd.rp_move,x,y,w^.sx,w^.sy,c);
    UNTIL (c=wnd.l_off) OR (c=wnd.m_off) OR (c=wnd.r_off);
    wnd.close(w); w^.x:=x; w^.y:=y; wnd.open(w);
  ELSIF (x>=2) & (x<=17) & (y>=w^.sy-18) & (y<=w^.sy-3) & (
     (c=wnd.l_on) OR (c=wnd.m_on) OR (c=wnd.r_on) ) THEN wnd.remove(w);
  ELSIF c=wnd.l_on THEN wnd.ontop(w);
  ELSIF c=wnd.r_on THEN wnd.onbottom(w);
  END;
END message_job;

PROCEDURE message(x,y,cb,cf: INTEGER; VAL fmt: ARRAY OF CHAR; SEQ a: WORD);
  VAR
    buf  : ARRAY [0..4095] OF CHAR;
    lns  : ARRAY [0..31] OF INTEGER;
    lno  : INTEGER;
    cno  : INTEGER;
    w    : wnd.window;
    i,j,l: INTEGER;
BEGIN
  str.print(buf,fmt,a);
  i:=0; lno:=0; cno:=10; lns[lno]:=0;
  LOOP
    IF buf[i]=0c THEN
      l:=vg.string_len(TRUE,buf,lns[lno]);
      IF l>cno THEN cno:=l END;
      INC(lno); EXIT;
    ELSIF buf[i]=36c THEN
      buf[i]:=0c;
      l:=vg.string_len(TRUE,buf,lns[lno]);
      IF l>cno THEN cno:=l END;
      INC(lno);
      IF lno>HIGH(lns) THEN EXIT END;
      lns[lno]:=i+1;
    END;
    INC(i);
  END;
  i:=vg.sign_w+8+cno; j:=vg.sign_h*2+8;
  IF j<8+vg.char_h*lno THEN j:=8+vg.char_h*lno END;
  w:=wnd.create(i,j);
  vg.color(w,1);
  vg.frame(w,0,0,i-1,j-1);
  vg.frame(w,vg.sign_w+3,3,i-4,j-4);
  vg.color(w,2);
  vg.box(w,0,1,i-2,j-1);
  vg.mode(w,vg.bic);
  vg.box(w,vg.sign_w+3,4,i-5,j-4);
  vg.color(w,cb); vg.mode(w,vg.rep);
  vg.box(w,vg.sign_w+4,4,i-5,j-5);
  vg.color(w,15);
  vg.sign(w,2,2,vg.sg_move);
  vg.sign(w,2,j-18,vg.sg_close);
  vg.prop_font(w,TRUE);
  vg.color(w,INTEGER(BITSET(cb)/BITSET(cf))); vg.mode(w,vg.xor);
  FOR l:=0 TO lno-1 DO
    vg.write_pos(w,4+vg.sign_w,j-4-vg.char_h*(l+1),buf,lns[l]);
  END;
  w^.x:=x; w^.y:=y; w^.job:=message_job;
  center(w);
  wnd.open(w);
END message;

PROCEDURE menu_job(w: wnd.window; x,y: INTEGER; ch: CHAR);
  VAR n: BITSET; inf: menu_info; i,j,nx,ny,sl: INTEGER;
BEGIN
  x:=x-w^.x; y:=y-w^.y; inf:=w^.info;
  IF inf=NIL THEN RETURN END;
  IF (x<stp) OR (x>=w^.sx) OR (y<0) OR (y>=w^.sy-stp) THEN
    n:={}
  ELSE
    sl:=(w^.sy-y-1) DIV stp; n:={sl};
  END;
  IF n#inf^.sel THEN
    vg.color(w,12);
    FOR i:=1 TO 31 DO
      IF i IN (n/inf^.sel) THEN
        IF i IN n THEN vg.mode(w,vg.or) ELSE vg.mode(w,vg.bic) END;
        vg.box(w,stp+1,w^.sy-(i+1)*stp+1,w^.sx-2,w^.sy-i*stp-2);
        wnd.ref_box(w,w^.sy-(i+1)*stp,stp);
      END;
    END;
    inf^.sel:=n;
  END;
  IF (ch=wnd.r_on) OR (ch=wnd.m_on) OR (ch=wnd.l_on) THEN
    IF ch=wnd.r_on THEN wnd.onbottom(w);
    ELSIF n#{} THEN wnd.del_crs; inf^.job(w,sl,0);
    ELSIF (x>=0) & (x<stp) & (y>=0) & (y<stp) THEN
      x:=w^.x; y:=w^.y;
      REPEAT wnd.read_point(wnd.rp_move,x,y,w^.sx,w^.sy,ch);
      UNTIL (ch=wnd.l_off) OR (ch=wnd.m_off) OR (ch=wnd.r_off);
      wnd.close(w); w^.x:=x; w^.y:=y; wnd.open(w);
    ELSIF (x>=0) & (x<stp) & (y>=w^.sy-stp) & (y<w^.sy) THEN
      DISPOSE(inf); wnd.remove(w);
    ELSIF ch=wnd.l_on THEN wnd.ontop(w);
    END;
  END;
END menu_job;

PROCEDURE menu(x,y: INTEGER; sel: menu_proc; fmt: ARRAY OF CHAR; SEQ a: WORD);
  VAR
    buf  : ARRAY [0..4095] OF CHAR;
    lns  : ARRAY [0..31] OF INTEGER;
    lsz  : ARRAY [0..31] OF INTEGER;
    lno  : INTEGER;
    cno  : INTEGER;
    i,j,l: INTEGER;
    sx,sy: INTEGER;
    w    : wnd.window;
    inf  : menu_info;
BEGIN
  str.print(buf,fmt,a);
  i:=0; lno:=0; cno:=10; lns[lno]:=0;
  LOOP
    IF buf[i]=0c THEN
      l:=vg.string_len(TRUE,buf,lns[lno]);
      lsz[lno]:=l;
      IF l>cno THEN cno:=l END;
      INC(lno); EXIT;
    ELSIF buf[i]=36c THEN
      buf[i]:=0c;
      l:=vg.string_len(TRUE,buf,lns[lno]);
      lsz[lno]:=l;
      IF l>cno THEN cno:=l END;
      INC(lno);
      IF lno>HIGH(lns) THEN EXIT END;
      lns[lno]:=i+1;
    END;
    INC(i);
  END;
  sx:=cno+stp; sy:=lno*stp; NEW(inf);
  w:=wnd.create(sx,sy);
  w^.info:=inf; inf^.sel:={}; inf^.job:=sel;
  vg.color(w,15); vg.sign(w,0,0,vg.sg_move); vg.sign(w,0,sy-stp,vg.sg_close);
  white_box(w,0,stp,stp,sy-stp*2);
  white_box(w,stp,sy-stp,sx-stp,stp);
  vg.color(w,3); vg.mode(w,vg.or); vg.prop_font(w,TRUE);
  vg.write_pos(w,stp+(cno-lsz[0]) DIV 2,sy-stp+1,buf,lns[0]);
  FOR i:=1 TO lno-1 DO
--    white_box(w,stp,sy-(i+1)*stp,sx-stp,stp);
vg.mode(w,vg.rep); vg.color(w,1);
vg.box(w,stp,sy-(i+1)*stp,sx-1,sy-i*stp-1);
    vg.mode(w,vg.or); vg.color(w,2);
    vg.write_pos(w,stp+(cno-lsz[i]) DIV 2,sy-(i+1)*stp+1,buf,lns[i]);
  END;
  w^.job:=menu_job; w^.x:=x; w^.y:=y; wnd.open(w);
END menu;

PROCEDURE tmp_menu(x,y: INTEGER; fmt: ARRAY OF CHAR; SEQ a: WORD): INTEGER;
  VAR
    buf  : ARRAY [0..4095] OF CHAR;
    lns  : ARRAY [0..31] OF INTEGER;
    lno  : INTEGER;
    cno  : INTEGER;
    i,j,l: INTEGER;
    sx,sy: INTEGER;
    x0,y0: INTEGER;
    w    : wnd.window;
    sl   : INTEGER;
    n,sel: BITSET;
    c    : CHAR;
BEGIN
  str.print(buf,fmt,a);
  i:=0; lno:=0; cno:=10; lns[lno]:=0;
  LOOP
    IF buf[i]=0c THEN
      l:=vg.string_len(TRUE,buf,lns[lno]);
      IF l>cno THEN cno:=l END;
      INC(lno); EXIT;
    ELSIF buf[i]=36c THEN
      buf[i]:=0c;
      l:=vg.string_len(TRUE,buf,lns[lno]);
      IF l>cno THEN cno:=l END;
      INC(lno);
      IF lno>HIGH(lns) THEN EXIT END;
      lns[lno]:=i+1;
    END;
    INC(i);
  END;
  sx:=cno+8;
  sy:=lno*vg.char_h+8;
  w:=wnd.create(sx,sy);
  vg.color(w,1); vg.frame(w,0,0,sx-1,sy-1);
  vg.color(w,2); vg.box(w,0,1,sx-2,sy-1);
  vg.color(w,1); vg.box(w,3,3,sx-4,sy-4-vg.char_h);
  vg.color(w,2); vg.mode(w,vg.bic);
  vg.box(w,3,4,sx-5,sy-4-vg.char_h);
  vg.color(w,3); vg.mode(w,vg.or); vg.prop_font(w,TRUE);
  vg.write_pos(w,4,sy-vg.char_h-2,buf,lns[0]);
  FOR i:=1 TO lno-1 DO
    vg.write_pos(w,4,sy-(i+1)*vg.char_h-4,buf,lns[i]);
  END;
  w^.x:=x; w^.y:=y; center(w); wnd.open(w); sel:={}; x0:=-1; y0:=-1;
  LOOP
    wnd.read_point(wnd.rp_flow,x0,y0,x0,y0,c);
    x:=x0-w^.x; y:=y0-w^.y; n:={}; sl:=0;
    IF (x>=4) & (x<=sx-5) & (y>=4) & (y<=sy-vg.char_h-5) THEN
      sl:=(sy-y-5) DIV vg.char_h; n:={sl};
    END;
    IF n#sel THEN
      vg.color(w,12);
      FOR i:=1 TO 31 DO
        IF i IN (n/sel) THEN
          IF i IN n THEN vg.mode(w,vg.or) ELSE vg.mode(w,vg.bic) END;
          vg.box(w,4,sy-4-(i+1)*vg.char_h,sx-5,sy-5-i*vg.char_h);
        END;
      END;
      sel:=n; wnd.refresh(w);
    END;
    IF c#0c THEN wnd.remove(w); wnd.del_crs; RETURN sl END;
  END;
END tmp_menu;

PROCEDURE readln(rx,ry: INTEGER; pmt: ARRAY OF CHAR; VAR s: ARRAY OF CHAR);
  VAR tn,tsx,tx,sx,sy,i,j,d,x,y: INTEGER; w: wnd.window; ch: CHAR;
BEGIN
  tn:=(wnd.screen^.sx-8) DIV (vg.char_w+2);
  IF tn>BYTES(s) THEN tn:=BYTES(s) END;
  tsx:=tn*(vg.char_w+2);
  sx:=vg.string_len(TRUE,pmt,0)+8;
  IF sx<tsx+8 THEN sx:=tsx+8 END;
  tx:=(sx-tsx) DIV 2;
  sy:=vg.char_h*2+14;
  w:=wnd.create(sx,sy);
  w^.x:=rx; w^.y:=ry;
  vg.color(w,1);
  vg.frame(w,0,0,sx-1,sy-1);
  vg.frame(w,tx-1,3,tx+tsx,vg.char_h+6);
  vg.color(w,2);
  vg.box(w,0,1,sx-2,sy-1);
  vg.mode(w,vg.bic);
  vg.box(w,tx-1,4,tx+tsx-1,vg.char_h+6);
  vg.color(w,1); vg.mode(w,vg.or); vg.prop_font(w,TRUE);
  vg.write_string(w,4,10+vg.char_h,pmt);
  vg.color(w,8); vg.mode(w,vg.rep); vg.prop_font(w,FALSE);
  i:=0;
  WHILE (i<tn) & (s[i]#0c) DO
    vg.write_char(w,tx+1+i*(vg.char_w+2),5,s[i]); INC(i);
  END;
  center(w);
  wnd.open(w);
  IF i=tn THEN DEC(i) END;
  d:=0;
  LOOP
    vg.mode(w,vg.xor); vg.color(w,1);
    vg.box(w,(vg.char_w+2)*i+tx,4,(vg.char_w+2)*i+1+tx+vg.char_w,5+vg.char_h);
    wnd.ref_box(w,4,vg.char_h+2); wnd.wait; wnd.first(x,y,ch); wnd.drop;
    vg.box(w,(vg.char_w+2)*i+tx,4,(vg.char_w+2)*i+1+tx+vg.char_w,5+vg.char_h);
    vg.mode(w,vg.rep); vg.color(w,8);
    IF ch=15c THEN EXIT END;
    j:=tn;
    IF (ch>=' ')&(ch<200c) OR (ch>300c)&(ch<=377c) THEN
      x:=HIGH(s);
      WHILE x>d+i DO s[x]:=s[x-1]; DEC(x) END;
      s[HIGH(s)]:=0c;
      IF i+d<HIGH(s) THEN
        s[d+i]:=ch; j:=i; INC(i);
        IF i>=tn THEN DEC(i); INC(d); j:=0 END
      END;
    ELSIF (ch=wnd.k_left) & (i+d>0) THEN
      IF (i<=2) & (d>0) THEN DEC(d); j:=0
      ELSE DEC(i);
      END;
    ELSIF (ch=wnd.k_right) & (s[i+d]#0c) THEN
      IF i>=tn-1 THEN INC(d); j:=0
      ELSE INC(i);
      END;
    END;
    WHILE (j<tn) & (s[j+d]#0c) DO
      vg.write_char(w,tx+1+j*(vg.char_w+2),5,s[d+j]); INC(j);
    END;
  END;
  s[d+i]:=0c;
  wnd.remove(w);
END readln;

PROCEDURE qwest(x,y: INTEGER; fmt: ARRAY OF CHAR; SEQ a: WORD): BOOLEAN;
  VAR
    buf  : ARRAY [0..4095] OF CHAR;
    lns  : ARRAY [0..31] OF INTEGER;
    lno  : INTEGER;
    cno  : INTEGER;
    w    : wnd.window;
    i,j,l: INTEGER;
    sx,sy: INTEGER;
    ch   : CHAR;
BEGIN
  str.print(buf,fmt,a);
  i:=0; lno:=0; cno:=10; lns[lno]:=0;
  LOOP
    IF buf[i]=0c THEN
      l:=vg.string_len(TRUE,buf,lns[lno]);
      IF l>cno THEN cno:=l END;
      INC(lno); EXIT;
    ELSIF buf[i]=36c THEN
      buf[i]:=0c;
      l:=vg.string_len(TRUE,buf,lns[lno]);
      IF l>cno THEN cno:=l END;
      INC(lno);
      IF lno>HIGH(lns) THEN EXIT END;
      lns[lno]:=i+1;
    END;
    INC(i);
  END;
  sx:=4+cno; sy:=(vg.char_h+2)*lno+4+vg.sign_h;
  w:=wnd.create(sx,sy); w^.x:=x; w^.y:=y; center(w);
  vg.color(w,1); vg.frame(w,0,0,sx-1,sy-1);
  vg.prop_font(w,TRUE);
  FOR i:=0 TO lno-1 DO
    vg.write_pos(w,2,sy-vg.char_h*(i+1)-2,buf,lns[i]);
  END;
  vg.color(w,2); vg.box(w,0,1,sx-2,sy-1);
  i:=(sx-vg.sign_w*2) DIV 3; j:=vg.sign_w+i+i;
  vg.color(w,15); vg.sign(w,i,2,vg.sg_no); vg.sign(w,j,2,vg.sg_yes);
  wnd.open(w);
  LOOP
    wnd.read_point(wnd.rp_free,x,y,x,y,ch);
    IF (ch=wnd.r_on) OR (ch=wnd.m_on) OR (ch=wnd.l_on) THEN
      x:=x-w^.x; y:=y-w^.y;
      IF (y>=2) & (y<=vg.sign_h+1) THEN
        IF (x>=i) & (x<=i+vg.sign_w-1) THEN wnd.remove(w); RETURN FALSE
        ELSIF (x>=j) & (x<=j+vg.sign_w-1) THEN wnd.remove(w); RETURN TRUE
        END;
      END;
    END;
  END;
END qwest;

PROCEDURE zoom;
  PROCEDURE bbu(a: ADDRESS; x,s: INTEGER): INTEGER; CODE 0ECh END bbu;
  VAR
    tb: ARRAY [0..255] OF INTEGER; i,j,n: INTEGER; a,b,bb: ADDRESS;
    w: wnd.window; x1,y1,x0,y0,x2,y2,l,m,r: INTEGER; ch: CHAR;
BEGIN
  FOR i:=0 TO HIGH(tb) DO
    n:=0;
    FOR j:=0 TO 7 DO
      IF j IN BITSET(i) THEN n:=n+15 END;
      n:=n>>4;
    END;
    tb[i]:=n;
  END;
  w:=wnd.create(96,96); w^.y:=250; wnd.open(w); vg.color(w,15);
  x1:=-1; y1:=-1; x2:=-1; y2:=-1;
  LOOP
    REPEAT
      wnd.read_point(wnd.rp_flow,x0,y0,x2,y2,ch);
      IF (ch=33c) OR (ch=wnd.r_on) THEN wnd.remove(w); RETURN END;
    UNTIL (ABS(x1-x2)>1) OR (ABS(y1-y2)>1);
    x1:=x2; y1:=y2;
    IF x1<12 THEN x0:=0;
    ELSIF x1>wnd.screen^.sx-12 THEN x0:=wnd.screen^.sx-24;
    ELSE x0:=x1-12;
    END;
    IF y1<12 THEN y0:=24;
    ELSIF y1>wnd.screen^.sy-12 THEN y0:=wnd.screen^.sy;
    ELSE y0:=y1+12;
    END;
    a:=w^.mem;
    bb:=wnd.screen^.mem+(wnd.screen^.sy-1-y0)*wnd.screen^.wpl;
    wnd.del_crs;
    FOR j:=0 TO 3 DO
      b:=bb;
      FOR i:=0 TO 23 DO
        INC(b,wnd.screen^.wpl);
        l:=tb[bbu(b,x0,8)]; m:=tb[bbu(b,x0+8,8)]; r:=tb[bbu(b,x0+16,8)];
        a^:=l; INC(a); a^:=m; INC(a); a^:=r; INC(a);
        a^:=l; INC(a); a^:=m; INC(a); a^:=r; INC(a);
        a^:=l; INC(a); a^:=m; INC(a); a^:=r; INC(a);
        a^:=l; INC(a); a^:=m; INC(a); a^:=r; INC(a);
      END;
      INC(bb,wnd.screen^.wpp);
    END;
    vg.frame(w,0,0,w^.sx-1,w^.sy-1);
    wnd.refresh(w);
  END;
END zoom;

PROCEDURE rainbow_job(w: wnd.window; x,y: INTEGER; c: CHAR);
BEGIN
  IF (x<w^.x) OR (x>w^.x+w^.sx) THEN RETURN END;
  IF (y<w^.y) OR (y>w^.y+w^.sy) THEN RETURN END;
  IF c=wnd.r_on THEN wnd.remove(w);
  ELSIF c=wnd.l_on THEN wnd.ontop(w);
  END;
END rainbow_job;

PROCEDURE rainbow;
  VAR w: wnd.window; i: INTEGER;
BEGIN
  w:=wnd.create(316,40);
  FOR i:=1 TO 15 DO vg.color(w,i); vg.box(w,i*21-20,1,i*21-1,38) END;
  vg.color(w,1); vg.frame(w,0,0,315,39);
  FOR i:=1 TO 15 DO vg.vect(w,i*21-21,1,i*21-21,38) END;
  w^.job:=rainbow_job;
  wnd.open(w);
END rainbow;

PROCEDURE colors_job(w: wnd.window; x,y: INTEGER; c: CHAR);
  VAR r,g,b: INTEGER;
BEGIN
  IF (c#wnd.r_on) & (c#wnd.m_on) & (c#wnd.l_on) THEN RETURN END;
  x:=(x-w^.x) DIV 16; y:=(y-w^.y) DIV 16;
  IF (x>=0) & (x<=15) THEN
    wnd.get_pal(x,r,g,b);
    r:=r DIV 4; g:=g DIV 4; b:=b DIV 4;
    CASE y OF
      |0: IF r>0 THEN DEC(r) END;
      |1: IF r<3 THEN INC(r) END;
      |2: IF g>0 THEN DEC(g) END;
      |3: IF g<3 THEN INC(g) END;
      |4: IF b>0 THEN DEC(b) END;
      |5: IF b<3 THEN INC(b) END;
    ELSE
    END;
    vg.print(w,x*16,6*16+ 2,'%d',r);
    vg.print(w,x*16,6*16+18,'%d',g);
    vg.print(w,x*16,6*16+34,'%d',b);
    wnd.ref_box(w,16*6,16*3);
    wnd.put_pal(x,r*4,g*4,b*4);
  END;
END colors_job;

PROCEDURE colors;
  VAR w: wnd.window; i,j,r,g,b: INTEGER;
BEGIN
  w:=wnd.create(16*16,11*16);
  FOR i:=0 TO 15 DO
    wnd.get_pal(i,r,g,b);
    vg.color(w,15);
    FOR j:=0 TO 5 BY 2 DO
      vg.sign(w,i*16,j*16,vg.sg_down);
      vg.sign(w,i*16,j*16+16,vg.sg_up);
    END;
    vg.print(w,i*16,6*16+ 2,'%d',r DIV 4);
    vg.print(w,i*16,6*16+18,'%d',g DIV 4);
    vg.print(w,i*16,6*16+34,'%d',b DIV 4);
    vg.color(w,i);
    vg.box(w,i*16,9*16,i*16+15,11*16-1);
  END;
  w^.job:=colors_job;
  wnd.open(w);
END colors;

PROCEDURE edit_number(w: wnd.window; x,y,sx,sy: INTEGER; ch: CHAR;
                      fr,to: INTEGER; VAR num: INTEGER);
  PROCEDURE print_num(VAR n: INTEGER; color: INTEGER);
    VAR l: INTEGER; ln: ARRAY [0..79] OF CHAR;
  BEGIN
    vg.prop_font(w,FALSE); w^.patt:={0..31};
    vg.mode(w,vg.bic); vg.color(w,15); vg.box(w,x,y,x+sx-1,y+sy-1);
    vg.mode(w,vg.rep); vg.color(w,2); vg.box(w,x+1,y,x+sx-1,y+sy-2);
    vg.color(w,1); vg.frame(w,x,y,x+sx-1,y+sy-1);
    LOOP
      str.print(ln,'%d',n); l:=vg.string_len(FALSE,ln,0);
      IF l<=sx-4 THEN EXIT END;
      n:=n/10;
    END;
    vg.mode(w,vg.xor); vg.color(w,INTEGER({1}/BITSET(color)));
    vg.write_string(w,x+sx-l-1,y+(sy-vg.char_h) DIV 2,ln);
  END print_num;
  VAR n,i,j: INTEGER;
BEGIN
  IF ch=0c THEN print_num(num,0); RETURN END;
  wnd.del_crs;
  IF ch=' ' THEN n:=0 ELSE n:=num END;
  LOOP
    print_num(n,8);
    wnd.ref_box(w,y,sy);
    REPEAT
      wnd.wait; wnd.first(i,j,ch); wnd.drop;
    UNTIL ch#0c;
    IF (ch=15c) OR (ch=wnd.l_on) THEN num:=n; EXIT END;
    IF (ch=33c) OR (ch=wnd.r_on) THEN EXIT END;
    IF ch=10c THEN n:=n/10
    ELSIF ch='-' THEN IF n#MIN(INTEGER) THEN n:=-n END;
    ELSIF ch=' ' THEN n:=0;
    ELSIF (ch>='0') & (ch<='9') THEN
      IF n>=0 THEN
        IF n < (MAX(INTEGER)-10)/10 THEN n:=n*10+(ORD(ch)-ORD('0')) END;
      ELSE
        IF n > (MIN(INTEGER)+10)/10 THEN n:=n*10-(ORD(ch)-ORD('0')) END;
      END;
    END;
  END;
  IF num>to THEN num:=to ELSIF num<fr THEN num:=fr END;
  print_num(num,0);
  wnd.ref_box(w,y,sy);
END edit_number;

PROCEDURE pcb_number(w: wnd.window; x,y,cx,cy: INTEGER; ch: CHAR;
                     VAR num: INTEGER): BOOLEAN;
  PROCEDURE print_num(n,x,color: INTEGER);
    VAR l: INTEGER; ln: ARRAY [0..79] OF CHAR;
  BEGIN
    vg.prop_font(w,FALSE); w^.patt:={0..31};
    vg.mode(w,vg.bic); vg.color(w,15); vg.box(w,x,y,x+74,y+14);
    vg.mode(w,vg.rep); vg.color(w,1); vg.frame(w,x,y,x+74,y+14);
    vg.mode(w,vg.xor); vg.color(w,2); vg.box(w,x+1,y,x+74,y+13);
    vg.color(w,INTEGER({1}/BITSET(color)));
    str.print(ln,'%d.%$2d',n/100,ABS(n REM 100));
    l:=vg.string_len(FALSE,ln,0);
    IF l>72 THEN vg.write_string(w,x+2,y+1,'*******');
    ELSE vg.write_string(w,x+73-l,y+1,ln)
    END;
  END print_num;
  PROCEDURE edit_num(VAR n: INTEGER; x: INTEGER): BOOLEAN;
     VAR i,j: INTEGER; ch: CHAR; pnt: BOOLEAN;
  BEGIN
    pnt:=FALSE; wnd.del_crs;
    LOOP
      print_num(n,x,8); wnd.ref_box(w,y,15);
      REPEAT wnd.wait; wnd.first(i,j,ch); wnd.drop UNTIL ch#0c;
      IF (ch=15c) OR (ch=wnd.l_on) THEN RETURN TRUE END;
      IF (ch=33c) OR (ch=wnd.r_on) THEN RETURN FALSE END;
      IF ch=10c THEN n:=n/10
      ELSIF ch='-' THEN IF n#MIN(INTEGER) THEN n:=-n END;
      ELSIF ch=' ' THEN n:=0;
      ELSIF ch='.' THEN
        pnt:=NOT pnt;
        IF pnt THEN n:=n/100*100 END;
      ELSIF (ch>='0') & (ch<='9') THEN
        IF pnt THEN
          i:=ABS(n REM 100);
          i:=(i*10+ORD(ch)-ORD('0')) REM 100;
          IF n<0 THEN n:=n/100*100+i ELSE n:=n/100*100+i END;
        ELSIF n>=0 THEN
          IF  n<1000000 THEN
            n:=n REM 100 + (n/100*10+(ORD(ch)-ORD('0')))*100;
          END;
        ELSE
          IF -n<1000000 THEN
            n:=n REM 100 + (n/100*10-(ORD(ch)-ORD('0')))*100;
          END;
        END;
      END;
    END;
  END edit_num;
  VAR n: INTEGER;
BEGIN
  IF ch=0c THEN RETURN FALSE END;
  IF ch=1c THEN
    vg.mode(w,vg.bic); vg.color(w,15); vg.box(w,x,y,x+219,y+14);
    vg.mode(w,vg.rep); vg.color(w,2); vg.box(w,x,y,x+219,y+14);
    vg.mode(w,vg.or); vg.color(w,1); vg.prop_font(w,TRUE);
    vg.write_string(w,x+76,y+1,'x1.25');
    vg.write_string(w,x+201,y+1,'мм');
    print_num((num*50+12) DIV 24,x,0);
    print_num((num*125+24) DIV 48,x+125,0);
    RETURN TRUE;
  END;
  IF (cy<y) OR (cy>=y+15) THEN RETURN FALSE END;
  IF (cx>=x) & (cx<x+75) THEN
    IF ch=' ' THEN n:=0;
    ELSIF ch=wnd.l_on THEN n:=(num*50+12) DIV 24;
    ELSE RETURN FALSE;
    END;
    IF edit_num(n,x) THEN num:=(n*24+25) DIV 50 END;
    print_num((num*50+12) DIV 24,x,0);
    print_num((num*125+24) DIV 48,x+125,0);
    wnd.ref_box(w,y,15); RETURN TRUE;
  ELSIF (cx>=x+125) & (cx<x+200) THEN
    IF ch=' ' THEN n:=0;
    ELSIF ch=wnd.l_on THEN n:=(num*125+24) DIV 48;
    ELSE RETURN FALSE;
    END;
    IF edit_num(n,x+125) THEN num:=(n*48+62) DIV 125 END;
    print_num((num*50+12) DIV 24,x,0);
    print_num((num*125+24) DIV 48,x+125,0);
    wnd.ref_box(w,y,15); RETURN TRUE;
  ELSE RETURN FALSE;
  END;
END pcb_number;

BEGIN
  ASSERT(stp=vg.sign_w);
  ASSERT(stp=vg.sign_h);
END cdsMenu.
