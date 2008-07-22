MODULE vx; (* John 27-Sep-88. (c) KRONOS *)
           (* Leo  07-Jan-89. (c) KRONOS *)
           (* vik  14-Feb-89. (c) KRONOS *)
           (* Ned  17-Dec-89. (c) KRONOS *)
           (* vik  12-Apr-90. (c) KRONOS *)
           (* vik  16-May-90. (c) KRONOS color windows *)

(*$U+*)  --++ Ned 01-Dec-90

IMPORT  sys: SYSTEM;            IMPORT  mcd  : defCodes;
IMPORT  sym: coolSym;           IMPORT  env  : tskEnv;
IMPORT  err: defErrors;         IMPORT  ascii: ASCII;

IMPORT  bio: BIO;               IMPORT args  : tskArgs;
IMPORT  key: Keyboard;          IMPORT time  : Time;
IMPORT  str: Strings;           IMPORT  mem  : Heap;
IMPORT  tty: Terminal;          IMPORT  lex  : Lexicon;
IMPORT  sed: strEditor;

IMPORT  ref: xRef;              IMPORT  vis  : visCode;
IMPORT  tsk: tskEnv;

WITH STORAGE: mem;

-------------------------- WINDOWS ----------------------------
                          ---------
MODULE wnd; (* Ned 03-Jun-89. (c) KRONOS *)
            (* vik 13-Jun-90. (c) KRONOS *)

IMPORT  sys,str,tty,key,mem;

EXPORT  QUALIFIED blck,dgr,gr,brck,lgr,red,blue,wht,color?,
                  line_ptr, line_rec,_under,_high,_low,_inv,_user,
                  menu_ptr, READ, ACTION, menu_rec,new_window,
                  rem_window,place,undef,erase,w_name,frame?,
                  show_frame,show_frm,erase_frm,get_col,set_col,
                  fr_color,w_color,new_menu,rem_menu,place_menu,
                  append,refresh,select,jump,screen_pos,first,last,
                  rem_col;



 -------------------------  WINDOWs  --------------------------
                          -----------

CONST -- modes
  _under= 0;
  _high = 1;
  _low  = 2;
  _inv  = 3;
  _user =16;  -- [_user..31] for user defined options

CONST MAGIC = -12345678h;

TYPE
  WINDOW   = POINTER TO
               RECORD
                 magic: INTEGER;
                 name : STRING;
                 undef: BOOLEAN;
                 frame: BOOLEAN;
                 l0,c0: INTEGER;
                 high : INTEGER;
                 width: INTEGER;
                 cols : INTEGER;        -- number of colons
                 scr  : INTEGER;        -- current line in window [1..high-1]
                 col  : INTEGER;        -- current colon
                 frfor: INTEGER;
                 frcol: INTEGER;
                 fore : INTEGER;
                 color: INTEGER;
               END;

CONST  max_line = 24; max_col = 80;

TYPE
  line_ptr = POINTER TO line_rec;
  line_rec = RECORD
               line: STRING;
               mode: BITSET;    -- modes for refresh
               cur : BITSET;    -- additional modes when line is current
               id  : sys.WORD;
             END;
  menu_ptr = POINTER TO menu_rec;
  READ     = PROCEDURE (menu_ptr): CHAR;
  ACTION   = PROCEDURE (menu_ptr);
  menu_rec = RECORD
               lines: DYNARR OF line_ptr;
               free : INTEGER;        -- first free element in lines
               ln   : INTEGER;        -- current position in lines
               read : READ;           -- default Keyboard.ReadKey
               act  : ACTION;         -- default dummy action
               add  : INTEGER;        -- unit for extension
               page : INTEGER;        -- page len (default 8)
               w    : WINDOW;
             END;

---------------------------- COLORs ----------------------------
                            --------
VAR  blck, dgr, gr, brck, lgr, red, blue, wht: INTEGER;
    color?       : BOOLEAN;
VAR max, min, mod: INTEGER;

--------------------------  WINDOWs  --------------------------
                          -----------

PROCEDURE new_str(VAL line: ARRAY OF CHAR; VAR s: STRING);
  VAR i,l: INTEGER;
BEGIN l:=0;
  WHILE (l<=HIGH(line)) & (line[l]#0c) DO INC(l) END;
  NEW(s,l+1); i:=0;
  WHILE (i<l) DO s[i]:=line[i]; INC(i) END;
  s[i]:=0c;
END new_str;

PROCEDURE new_window(VAL name: ARRAY OF CHAR; frame: BOOLEAN; VAR w: WINDOW);
BEGIN
  NEW(w); w^.magic:=MAGIC; w^.undef:=TRUE; w^.frame:=frame;
  new_str(name,w^.name);
  w^.scr:=0;   w^.col:=0;
  w^.frfor:=0; w^.frcol:=0;
  w^.fore:=0;  w^.color:=0;
END new_window;

PROCEDURE rem_window(VAR w: WINDOW);
BEGIN ASSERT(w#NIL);
  ASSERT(w^.magic=MAGIC);
  DISPOSE(w^.name);
  DISPOSE(w);
END rem_window;

PROCEDURE place(w: WINDOW; l0,c0,high,width,col: INTEGER);
  VAR ww,wh: INTEGER;
BEGIN ASSERT((w#NIL) & (w^.magic=MAGIC));
  ASSERT((high>0) & (width>0) & (col>0),4Fh);
  ASSERT((l0>=0) & (c0>=0),4Fh);
  wh:=high; ww:=width*col+col-1;
  IF w^.frame THEN INC(wh,2); INC(ww,2) END;
  IF l0+wh>max_line THEN RETURN END;
  IF c0+ww>max_col  THEN RETURN END;
  w^.l0:=l0; w^.c0:=c0;
  w^.high:=high; w^.width:=width; w^.cols:=col;
  w^.undef:=FALSE;
END place;

PROCEDURE undef(w: WINDOW): BOOLEAN;
BEGIN ASSERT((w#NIL) & (w^.magic=MAGIC));
  RETURN w^.undef
END undef;

PROCEDURE erase(w: WINDOW);
  VAR s: ARRAY [0..max_col] OF CHAR;
   i,ww,wh: INTEGER;
    f,c   : INTEGER;
BEGIN ASSERT(w^.magic=MAGIC);
  IF w^.undef THEN RETURN END;
  WITH tty.state^ DO
   c:=color; f:=back;
   tty.set_color(0); tty.set_back(min_color);
  END;
  ww:=w^.width*w^.cols+w^.cols-1;
  wh:=w^.high;
  IF w^.frame THEN INC(ww,2); INC(wh,2) END;
  s[ww]:=0c;
  FOR i:=0 TO ww-1 DO s[i]:=' ' END;
  FOR i:=w^.l0 TO w^.l0+wh-1 DO
    tty.set_pos(i,w^.c0); tty.WriteString(s);
  END;
  tty.set_color(c); tty.set_back(f);
END erase;

PROCEDURE w_name(w: WINDOW; VAR s: ARRAY OF CHAR);
BEGIN str.copy(s, w^.name);
END w_name;

PROCEDURE frame?(w: WINDOW): BOOLEAN;
BEGIN ASSERT(w^.magic=MAGIC); RETURN w^.frame END frame?;

PROCEDURE get_col(w: WINDOW): INTEGER;
BEGIN ASSERT(w^.magic=MAGIC); RETURN w^.col END get_col;

PROCEDURE set_col(w: WINDOW; col: INTEGER);
BEGIN ASSERT(w^.magic=MAGIC); w^.col:=col END set_col;

PROCEDURE fr_color(w: WINDOW; f, c: INTEGER);
BEGIN w^.frfor:=f MOD mod + min;
      w^.frcol:=c MOD mod + min
END fr_color;

PROCEDURE w_color(w: WINDOW; f, c: INTEGER);
BEGIN w^.fore:=f  MOD mod + min;
      w^.color:=c MOD mod + min;
END w_color;

---------------------------  MENUes ---------------------------
                           ---------

PROCEDURE dummy(m: menu_ptr); END dummy;

PROCEDURE read(m: menu_ptr): CHAR;
 VAR ch: CHAR;
BEGIN key.read(ch); RETURN ch END read;

PROCEDURE new_menu(VAR m: menu_ptr);
BEGIN NEW(m);
  m^.read:=read;
  m^.act :=dummy;
  NEW(m^.lines); m^.free:=0; m^.ln:=0;
  m^.add:=8; m^.page:=8;
  m^.w:=NIL;
END new_menu;

PROCEDURE rem_menu(VAR m: menu_ptr);
  VAR i: INTEGER; l: line_ptr;
BEGIN ASSERT(m#NIL);
  FOR i:=0 TO m^.free-1 DO
    l:=m^.lines[i];
    DISPOSE(l^.line); DISPOSE(l);
  END;
  DISPOSE(m);
END rem_menu;

PROCEDURE append(m: menu_ptr; VAL s: ARRAY OF CHAR; md,c: BITSET; x: sys.WORD);
  VAR l: line_ptr;
BEGIN ASSERT(m#NIL);
  IF m^.free>HIGH(m^.lines) THEN RESIZE(m^.lines,m^.free+m^.add) END;
  NEW(l); new_str(s,l^.line); l^.mode:=md; l^.cur:=c; l^.id:=x;
  m^.lines[m^.free]:=l;
  INC(m^.free);
END append;

PROCEDURE place_menu(menu: menu_ptr; l0,c0,hi,width,col: INTEGER);
  VAR i,max,len: INTEGER;
BEGIN ASSERT(menu#NIL);
  IF menu^.free<=0 THEN RETURN END;
  IF menu^.free<=col*hi THEN
    IF menu^.free<=hi THEN col:=1; hi:=menu^.free
    ELSE
      col:=(menu^.free+hi -1) DIV hi;
      hi :=(menu^.free+col-1) DIV col;
    END;
  END;
  max:=HIGH(menu^.lines[0]^.line);
  FOR i:=1 TO menu^.free-1 DO
    len:=HIGH(menu^.lines[i]^.line);
    IF max<len THEN max:=len END;
  END;
  IF max<HIGH(menu^.w^.name) THEN max:=HIGH(menu^.w^.name) END;
  IF max<width THEN width:=max END;
  place(menu^.w,l0,c0,hi,width,col);
END place_menu;

PROCEDURE screen_pos(m: menu_ptr; VAR line,col,width,high,cols: INTEGER);
  VAR w: WINDOW;
BEGIN ASSERT(m#NIL);
  w:=m^.w;
  ASSERT((w#NIL) & (w^.magic=MAGIC));
  width:=w^.width;
  line :=w^.l0;  col  :=w^.c0;
  high:=w^.high; cols:=w^.cols;
END screen_pos;

--------------------------  REFRESH  --------------------------
                          -----------

PROCEDURE modes(w: WINDOW; mode: BITSET; on: BOOLEAN);
  VAR i: INTEGER;
BEGIN i:=0;
  mode:=mode*{_low,_high,_inv,_under};
  WHILE mode#{} DO
    IF i IN mode THEN
      CASE i OF
        |_low  : tty.set_color(-max*ORD(on))
        |_high : tty.set_color(+max*ORD(on))
        |_inv  : IF NOT color? THEN  tty.set_reverse(ORD(on))
                 ELSIF on THEN tty.set_color(w^.fore ); tty.set_back(w^.color);
                 ELSE          tty.set_color(w^.color); tty.set_back(w^.fore);
                 END;
        |_under: tty.set_underline(ORD(on))
      ELSE
      END;
      EXCL(mode,i);
    END;
    INC(i);
  END;
END modes;

PROCEDURE show_frame(w: WINDOW);

  VAR s: ARRAY [0..81] OF CHAR;
     ww: INTEGER;

  PROCEDURE bound(scr: INTEGER; VAL f: ARRAY OF CHAR);
    VAR i: INTEGER;
  BEGIN
    FOR i:=1 TO ww-2 DO s[i]:=tty.state^.hbar END;
    FOR i:=1 TO w^.cols-1 DO s[i*w^.width+i]:=f[1] END;
    s[0]:=f[0]; s[ww-1]:=f[2];
    IF (scr=w^.l0) & (w^.name[0]#0c) THEN i:=0;
      WHILE (i<HIGH(w^.name)) & (i<ww-4) DO
        s[i+2]:=w^.name[i]; INC(i)
      END;
    END;
    tty.set_pos(scr,w^.c0);
    s[ww]:=0c;
    tty.WriteString(s);
  END bound;

  VAR i: INTEGER;
BEGIN ASSERT(w^.frame);
  ww:=w^.width*w^.cols+w^.cols+1;
  IF color? THEN tty.set_back( w^.frfor); tty.set_color(w^.frcol) END;
  bound(w^.l0          ,tty.state^.bars[0]);
  bound(w^.l0+w^.high+1,tty.state^.bars[2]);
  FOR i:=1 TO w^.high DO
    tty.set_pos(w^.l0+i,w^.c0     ); tty.Write(tty.state^.vbar);
    tty.set_pos(w^.l0+i,w^.c0+ww-1); tty.Write(tty.state^.vbar);
  END;
END show_frame;

PROCEDURE show_frm(w: WINDOW);
 VAR ww: INTEGER;
BEGIN ASSERT(w^.magic=MAGIC);
  ww:=w^.width*w^.cols+w^.cols+1;
  IF color? THEN tty.set_back( w^.frfor); tty.set_color(w^.frcol) END;
  WITH tty.state^ DO
    tty.set_pos(w^.l0          ,w^.c0     ); tty.Write(bars[0][0]);
    tty.set_pos(w^.l0          ,w^.c0+ww-1); tty.Write(bars[0][2]);
    tty.set_pos(w^.l0+w^.high+1,w^.c0     ); tty.Write(bars[2][0]);
    tty.set_pos(w^.l0+w^.high+1,w^.c0+ww-1); tty.Write(bars[2][2]);
  END;
END show_frm;

PROCEDURE erase_frm(w: WINDOW);
 VAR ww: INTEGER;
    f,c: INTEGER;
BEGIN ASSERT(w^.magic=MAGIC);
  c:=tty.state^.color; f:=tty.state^.back;
  ww:=w^.width*w^.cols+w^.cols+1;
  tty.set_color(0); tty.set_back(tty.state^.min_color);
  tty.set_pos(w^.l0          ,w^.c0     ); tty.Write(' ');
  tty.set_pos(w^.l0          ,w^.c0+ww-1); tty.Write(' ');
  tty.set_pos(w^.l0+w^.high+1,w^.c0     ); tty.Write(' ');
  tty.set_pos(w^.l0+w^.high+1,w^.c0+ww-1); tty.Write(' ');
  tty.set_color(c); tty.set_back(f);
END erase_frm;

PROCEDURE _refresh(m: menu_ptr; first,frame: BOOLEAN);
   VAR s: ARRAY [0..81] OF CHAR; sp: INTEGER;
       w: WINDOW;
     c,f: INTEGER;
   VAR i,n,F,scr,ln: INTEGER; l: line_ptr;

BEGIN ASSERT(m#NIL);
  w:=m^.w;
  ASSERT(w#NIL);
  ASSERT(w^.magic=MAGIC);
  IF w^.undef THEN RETURN END;
  c:=tty.state^.color; f:=tty.state^.back;
  IF frame & w^.frame THEN show_frame(w) END;
  IF first THEN m^.ln:=0; w^.scr:=0; w^.col:=0
  ELSIF w^.scr>=w^.high THEN w^.scr:=w^.high-1
  END;
  ln:=m^.ln-w^.col*w^.high-w^.scr;
  IF w^.frame THEN F:=1 ELSE F:=0 END;
  IF color? THEN tty.set_back( w^.fore ); tty.set_color(w^.color) END;
  FOR scr:=0 TO w^.high-1 DO
    tty.set_pos(w^.l0+scr+F,w^.c0+F);
    sp:=0;
    FOR i:=0 TO w^.cols-1 DO n:=ln+i*w^.high;
      IF n<m^.free THEN l:=m^.lines[n];
        IF l^.mode={} THEN
          str.image(s,sp,'%-*.*s',w^.width,w^.width,l^.line);
        ELSE
          IF sp#0 THEN tty.WriteString(s); sp:=0 END;
          modes(w,l^.mode,TRUE);
          tty.print('%-*.*s',w^.width,w^.width,l^.line);
          modes(w,l^.mode,FALSE);
        END;
        IF (i#w^.cols-1) & w^.frame THEN
          str.image(s,sp,'%c',tty.state^.vbar)
        END;
      ELSE
        IF (i#w^.cols-1) & w^.frame THEN
          str.image(s,sp,'%*c',w^.width+1,tty.state^.vbar);
        END;
      END;
    END;
    IF sp>0 THEN tty.WriteString(s); sp:=0 END;
    INC(ln);
  END;
  tty.set_back( f); tty.set_color(c);
END _refresh;

PROCEDURE refresh(m: menu_ptr; first: BOOLEAN);
BEGIN _refresh(m,first,TRUE);
END refresh;

---------------------------  SELECT  --------------------------
                           ----------

PROCEDURE up(m: menu_ptr; n: INTEGER);
  VAR r: BOOLEAN; w: WINDOW;
BEGIN r:=FALSE;   w:=m^.w;
  WHILE n>0 DO
    IF m^.ln>0 THEN DEC(m^.ln);
      IF    w^.scr>0 THEN DEC(w^.scr)
      ELSIF w^.col>0 THEN DEC(w^.col); w^.scr:=w^.high-1;
      ELSE r:=TRUE;
      END;
    END;
    DEC(n);
  END;
  IF r THEN _refresh(m,FALSE,FALSE) END;
END up;

PROCEDURE dw(m: menu_ptr; n: INTEGER);
  VAR r: BOOLEAN; w: WINDOW;
BEGIN r:=FALSE;   w:=m^.w;
  WHILE n>0 DO
    IF m^.ln<m^.free-1 THEN INC(m^.ln);
      IF    w^.scr<w^.high-1 THEN INC(w^.scr)
      ELSIF w^.col<w^.cols-1 THEN INC(w^.col); w^.scr:=0;
      ELSE r:=TRUE;
      END;
    END;
    DEC(n);
  END;
  IF r THEN _refresh(m,FALSE,FALSE) END;
END dw;

PROCEDURE r_frame(m: menu_ptr; x,F: INTEGER);
 VAR i: INTEGER;
BEGIN
  IF m^.w^.frame THEN
    WITH m^ DO
      IF color? THEN tty.set_back( w^.frfor); tty.set_color(w^.frcol) END;
      FOR i:=1 TO x DO
        tty.set_pos(w^.l0+i,w^.c0           ); tty.Write(tty.state^.vbar);
        tty.set_pos(w^.l0+i,w^.c0+w^.width+F); tty.Write(tty.state^.vbar);
      END;
    END;
  END;
END r_frame;

PROCEDURE show(m: menu_ptr; on: BOOLEAN);
  VAR x: BITSET; l: line_ptr; w: WINDOW; F: INTEGER;
BEGIN w:=m^.w;
  IF m^.ln<0 THEN m^.ln:=0 END;
  IF m^.ln>=m^.free THEN m^.ln:=m^.free-1 END;
  l:=m^.lines[m^.ln];
  IF on THEN x:=l^.cur ELSE x:=l^.mode END;
  F:=ORD(w^.frame);
  tty.set_pos(w^.l0+w^.scr+F,w^.c0+w^.width*w^.col+w^.col+F+(F-1)*w^.col);
  modes(m^.w,x,TRUE);
  tty.print('%-*.*s',w^.width,w^.width,l^.line);
  modes(m^.w,x,FALSE);
END show;

PROCEDURE select(m: menu_ptr; VAR exit: BOOLEAN);

  VAR w: WINDOW; c,f: INTEGER;

  PROCEDURE skip;
  BEGIN
    WHILE m^.ln>=m^.free DO
      DEC(m^.ln); DEC(w^.scr);
      IF w^.scr<0 THEN w^.scr:=w^.high-1; DEC(w^.col) END;
    END;
  END skip;

  VAR ch: CHAR; ln: INTEGER;
BEGIN ASSERT(m#NIL);
  w:=m^.w;
  ASSERT(w#NIL);
  ASSERT (w^.magic=MAGIC);
  IF w^.undef OR (m^.free<=0) THEN exit:=TRUE; RETURN END;
  tty.set_cursor(0); c:=tty.state^.color; f:=tty.state^.back;
  exit:=FALSE; ln:=-1;
  LOOP
    IF ln#m^.ln THEN m^.act(m); ln:=m^.ln END;
    show(m,TRUE);
    ch:=m^.read(m);
    CASE ch OF
      |0c      : (* nothing *)
      |33c     : show(m,FALSE); exit:=TRUE; EXIT
      |key.up  : show(m,FALSE); up(m,1);
      |key.dw  : show(m,FALSE); dw(m,1);
      |key.pgdw: show(m,FALSE); dw(m,m^.page);
      |key.pgup: show(m,FALSE); up(m,m^.page);
      |key.left: show(m,FALSE);
         IF w^.col>0 THEN DEC(w^.col); DEC(m^.ln,w^.high);
         ELSE w^.col:=w^.cols-1; INC(m^.ln,w^.col*w^.high); skip;
         END;
      |key.right: show(m,FALSE);
         IF w^.col=w^.cols-1 THEN DEC(m^.ln,w^.high*w^.col); w^.col:=0;
         ELSE INC(w^.col); INC(m^.ln,w^.high); skip;
         END;
      |key.lf   : show(m,FALSE); EXIT
      |key.cr   : EXIT
    ELSE tty.Write(7c);
    END;
  END;
  tty.set_back(f); tty.set_color(c);
END select;

PROCEDURE jump(m: menu_ptr; ln: INTEGER);
BEGIN
  IF ln>=m^.free THEN ln:=m^.free-1
  ELSIF ln<0    THEN ln:=0
  END;
  show(m,FALSE);
  IF    ln>m^.ln THEN dw(m,ln-m^.ln)
  ELSIF ln<m^.ln THEN up(m,m^.ln-ln)
  END;
  show(m,TRUE);
END jump;

PROCEDURE first(m: menu_ptr): INTEGER;
BEGIN RETURN m^.ln-m^.w^.scr END first;

PROCEDURE last(m: menu_ptr): INTEGER;
BEGIN RETURN m^.ln-m^.w^.scr+m^.w^.high END last;

PROCEDURE rem_col;
BEGIN
   IF max-min=7 THEN
     blck:= 0;  dgr := 1;  lgr := 1;  brck:= 1;
     gr  := 4;  blue:= 1;  red := 1;  wht := 4;
   END;
END rem_col;

BEGIN
   max:=tty.state^.max_color;
   min:=tty.state^.min_color;
   mod:=-min*2;
   IF mod=0 THEN mod:=1 END; -- -tty.state^.min_color*2;
   color?:=FALSE;
   CASE max-min OF
     1: blck:= 0;  dgr := 1;  lgr := 1;  brck:= 1;
        gr  := 1;  blue:= 1;  red := 1;  wht := 1;
    |3: blck:= 0;  dgr := 1;  lgr := 2;  brck:= 1;
        gr  := 2;  blue:= 1;  red := 1;  wht := 2;
    |7: blck:= 0;  dgr := 1;  lgr := 2;  brck:= 3;
        gr  := 4;  blue:= 5;  red := 6;  wht := 7;
       color?:=TRUE;
   ELSE blck:= 0;  dgr := 1;  lgr := 2;  brck:= 1;
        gr  := 2;  blue:= 1;  red := 1;  wht := 2;
   END;

END wnd;

----------------------------  vx  ----------------------------
                            ------
CONST  ON = TRUE;  OFF  =  FALSE;
CONST  setup_file  = "VX.SETUP";
CONST dig16 = "0123456789ABCDEF";

TYPE
  str32    = ARRAY [0.. 31] OF CHAR;
  str64    = ARRAY [0.. 63] OF CHAR;
  str80    = ARRAY [0.. 79] OF CHAR;
  str256   = ARRAY [0..255] OF CHAR;
  node_ptr = POINTER TO node_rec;
  node_rec = RECORD
               name: str32;
               buf : STRING;            -- for code file
               code: vis.code_ptr;
               cu  : ref.cu_ptr;
               pno : INTEGER;           -- no of proc
               link: INTEGER;
               next: node_ptr;
             END;

VAR  cur : node_ptr;
  no_ref : BOOLEAN;
  module : str80;
  SED    : sed.descriptor;
  start  : INTEGER;
  finish : INTEGER;

TYPE
  m_s_ptr  = POINTER TO m_s_rec;
  m_s_rec  = RECORD
                 l0,c0,high,width,cols : INTEGER;
             END;
  ring_ptr = POINTER TO ring_rec;
  ring_rec = RECORD
               m   : wnd.menu_ptr;
               h   : wnd.menu_ptr;
               m_s : m_s_ptr;
               node: node_ptr;
               proc: PROC;
               prev: ring_ptr;
               next: ring_ptr;
             END;


 VAR
  ring_no: INTEGER;             ring   : ring_ptr;
  r_board: ring_ptr;            r_load : ring_ptr;

  h_in_proc : wnd.menu_ptr;     h_proc_tab: wnd.menu_ptr;
  h_move    : wnd.menu_ptr;     h_board   : wnd.menu_ptr;
  h_load    : wnd.menu_ptr;     h_1str    : wnd.menu_ptr;
  h_select  : wnd.menu_ptr;     m_load    : wnd.menu_ptr;
  p_load    : wnd.menu_ptr;     i_load    : wnd.menu_ptr;

  read_done : BOOLEAN;          load?     : BOOLEAN;
  fr_board  : BOOLEAN;          refr?     : BOOLEAN;
  board     : wnd.menu_ptr;

VAR setup_rec : RECORD
                     board,proc,in_proc,
                     in_proch,head_proc,
                     ext,str,load,const,
                     types,glob,multi,file: m_s_rec;
                END;

VAR original_cd, cd_name: str256;

---------------------------  SCREEN  --------------------------
                           ----------
PROCEDURE norm_color;
BEGIN tty.set_color(0); tty.set_back(tty.state^.min_color) END norm_color;

PROCEDURE clear_scr;
BEGIN tty.home;  norm_color; tty.erase(0) END clear_scr;

PROCEDURE clear_line(l: INTEGER);
 VAR c,b: INTEGER;
BEGIN c:=tty.state^.color; b:=tty.state^.back; tty.set_pos(l,0);
  norm_color; tty.erase_line(0); tty.set_color(c); tty.set_back(b)
END clear_line;

PROCEDURE message(wait: BOOLEAN; VAL f: ARRAY OF CHAR; SEQ x: sys.WORD);
  VAR s: ARRAY [0..78] OF CHAR; ch: CHAR;
      col, for: INTEGER;
BEGIN col:=tty.state^.color; for:=tty.state^.back;
  IF NOT wnd.color? THEN tty.set_reverse(1)
  ELSE tty.set_color(tty.state^.min_color);
       tty.set_back(tty.state^.min_color+1)
  END;
  s[0]:=0c; str.append(s,f,x);
  tty.set_pos(24,0);
  IF wait THEN tty.print('%-59sPRESS CR TO CONTINUE\r',s);
  ELSE tty.print('%-79s\r',s);
  END;
  IF wait THEN
    LOOP
      key.read(ch);
      IF ch=key.cr THEN norm_color; tty.erase_line(0); EXIT END
    END;
  END;
  IF wnd.color? THEN tty.set_color(col); tty.set_back( for);
  ELSE tty.set_reverse(0)
  END;
END message;

PROCEDURE refresh_scr;
 VAR i: INTEGER; r: ring_ptr;
BEGIN IF NOT refr? THEN RETURN END;
  i:=0; r:=ring;
  WHILE i<=ring_no DO r:=r^.prev; wnd.refresh(r^.m, FALSE); INC(i) END;
END refresh_scr;

---------------------------- cd_name ---------------------------
                            ---------
PROCEDURE sub_str(VAL sou,pat: ARRAY OF CHAR; s: INTEGER): BOOLEAN;
  VAR i: INTEGER;
BEGIN i:=0;
  WHILE (i<=HIGH(pat)) & (s<=HIGH(sou)) & (pat[i]#0c) & (sou[s]=pat[i]) DO
    INC(i); INC(s)
  END;
  RETURN pat[i]=0c;
END sub_str;

PROCEDURE change_cd_name(VAL name: ARRAY OF CHAR);
  VAR c,i,j: INTEGER;
BEGIN c:=0;
  IF name="/"0c  THEN cd_name:="/"; RETURN END;
  IF name[0]#'/' THEN
    WHILE (c<HIGH(cd_name)) & (cd_name[c]#0c) DO INC(c) END;
    IF c>1 THEN cd_name[c]:='/'; INC(c) END;
  END;
  i:=0;
  WHILE (i<=HIGH(name)) & (c<HIGH(cd_name)) & (name[i]#0c) DO
    IF    sub_str(name, "./",i) THEN INC(i,2);
    ELSIF sub_str(name,"../",i) THEN INC(i,3);
      IF c>1 THEN DEC(c);
        REPEAT DEC(c) UNTIL (c<=0) OR (cd_name[c]='/');
        INC(c);
      END
    ELSE cd_name[c]:=name[i]; INC(i); INC(c);
    END
  END;
  IF c>1 THEN DEC(c) END;
  cd_name[c]:=0c;
END change_cd_name;
-------------------------- ini_help ---------------------------
                          ----------
PROCEDURE ini_inproc_help;
BEGIN
  wnd.new_menu(h_in_proc); wnd.new_window(' HELP ',ON,h_in_proc^.w);
  wnd.append(h_in_proc, "'->|' - change menu",   {},{},0);
  wnd.append(h_in_proc, "'|<-' - change menu",   {},{},0);
  wnd.append(h_in_proc, "'f  ' - find global pc",{},{},0);
  wnd.append(h_in_proc, "'l  ' - find  local pc",{},{},0);
  wnd.append(h_in_proc, "'c  ' - find  command" ,{},{},0);
  wnd.append(h_in_proc, "'_  ' - bottom of proc",{},{},0);
  wnd.append(h_in_proc, "'w  ' - write to file", {},{},0);
  wnd.append(h_in_proc, "'[  ' - begin marker",  {},{},0);
  wnd.append(h_in_proc, "']  ' - end marker",    {},{},0);
  wnd.append(h_in_proc, "'Esc' - delite menu",   {},{},0);
  wnd.place_menu(h_in_proc,1,45,9,23,1);
  wnd.fr_color(h_in_proc^.w, wnd.blck,wnd.brck);
  wnd.w_color( h_in_proc^.w, wnd.blck,wnd.brck);
END ini_inproc_help;

PROCEDURE ini_proctable_help;
BEGIN
  wnd.new_menu(h_proc_tab); wnd.new_window(' HELP ',ON,h_proc_tab^.w);
  wnd.append(h_proc_tab, "'f'    - find proc by pc " ,{},{},0);
  wnd.append(h_proc_tab, "'n'    - find proc by name",{},{},0);
  wnd.append(h_proc_tab, "'.'    - current directory",{},{},0);
  wnd.append(h_proc_tab, "'f0 '  - to board menu",    {},{},0);
  wnd.append(h_proc_tab, "'Esc'  - delite menu",      {},{},0);
  wnd.append(h_proc_tab, "'Home' - top of menu",      {},{},0);
  wnd.append(h_proc_tab, "'End'  - bottom of menu",   {},{},0);
  wnd.place_menu(h_proc_tab,10,15,7,26,1);
  wnd.fr_color(h_proc_tab^.w, wnd.blck,wnd.brck);
  wnd.w_color( h_proc_tab^.w, wnd.blck,wnd.brck);
END ini_proctable_help;

PROCEDURE ini_move_help;
BEGIN
  wnd.new_menu(h_move); wnd.new_window(' HELP ',ON,h_move^.w);
  wnd.append(h_move, "      SELECT PLACE",                {},{},0);
  wnd.append(h_move, "' -> ' - shift menu right",         {},{},0);
  wnd.append(h_move, "' <- ' - shift menu left",          {},{},0);
  wnd.append(h_move, "' Up ' - shift menu up",            {},{},0);
  wnd.append(h_move, "'Down' - shift menu down",          {},{},0);
  wnd.append(h_move, "'->| ' - shift menu left  to 8",    {},{},0);
  wnd.append(h_move, "'|<- ' - shift menu right to 8",    {},{},0);
  wnd.append(h_move, "'PgUp' - shift menu up to 4",       {},{},0);
  wnd.append(h_move, "'PgDn' - shift menu down to 4",     {},{},0);
  wnd.append(h_move, "'Home' - shift menu to left bound", {},{},0);
  wnd.append(h_move, "'End ' - shift menu to right bound",{},{},0);
  wnd.append(h_move, "      SELECT SIZE",                 {},{},0);
  wnd.append(h_move, "'Ins ' - INC width of menu",        {},{},0);
  wnd.append(h_move, "'Del ' - DEC width of menu",        {},{},0);
  wnd.append(h_move, "' +  ' - INC column ",              {},{},0);
  wnd.append(h_move, "' -  ' - DEC column",               {},{},0);
  wnd.append(h_move, "' f3 ' - INC high of menu",         {},{},0);
  wnd.append(h_move, "' f4 ' - DEC high of menu",         {},{},0);
  wnd.append(h_move, "      RETURN",                      {},{},0);
  wnd.append(h_move, "' CR ' - fix place/size",           {},{},0);
  wnd.place_menu(h_move,1,10,20,35,1);
  wnd.fr_color(h_move^.w, wnd.blck,wnd.brck);
  wnd.w_color( h_move^.w, wnd.blck,wnd.brck);
END ini_move_help;

PROCEDURE ini_load_help;
BEGIN
  wnd.new_menu(h_load); wnd.new_window(' HELP ',ON,h_load^.w);
  wnd.append(h_load, "'CR '  - load select module",{},{},0);
  wnd.append(h_load, "'f2 '  - move menu",         {},{},0);
  wnd.append(h_load, "'f10'  - to board menu",     {},{},0);
  wnd.append(h_load, "'->|'  - change menu",       {},{},0);
  wnd.append(h_load, "'|<-'  - change menu",       {},{},0);
  wnd.append(h_load, "'s'    - set new directory", {},{},0);
  wnd.append(h_load, "'.'    - current directory", {},{},0);
  wnd.append(h_load, "'Esc'  - delite menu",       {},{},0);
  wnd.append(h_load, "'Home' - top of menu",       {},{},0);
  wnd.append(h_load, "'End'  - bottom of menu",    {},{},0);
  wnd.place_menu(h_load,1,10,10,27,1);
  wnd.fr_color(h_load^.w, wnd.blck,wnd.brck);
  wnd.w_color( h_load^.w, wnd.blck,wnd.brck);
END ini_load_help;

PROCEDURE ini_1str_help;
BEGIN
  wnd.new_menu(h_1str); wnd.new_window(' HELP ',ON,h_1str^.w);
  wnd.append(h_1str, "Show information in first", {},{},0);
  wnd.append(h_1str, "   line of the screen",     {},{},0);
  wnd.append(h_1str, "'f2 '  - move menu",        {},{},0);
  wnd.append(h_1str, "'f10'  - to board menu",    {},{},0);
  wnd.append(h_1str, "'->|'  - change menu",      {},{},0);
  wnd.append(h_1str, "'|<-'  - change menu",      {},{},0);
  wnd.append(h_1str, "'Esc'  - delite menu",      {},{},0);
  wnd.append(h_1str, "'Home' - top of menu",      {},{},0);
  wnd.append(h_1str, "'End'  - bottom of menu",   {},{},0);
  wnd.append(h_1str, "'.'    - current directory",{},{},0);
  wnd.place_menu(h_1str,1,10,10,27,1);
  wnd.fr_color(h_1str^.w, wnd.blck,wnd.brck);
  wnd.w_color( h_1str^.w, wnd.blck,wnd.brck);
END ini_1str_help;

PROCEDURE ini_select_help;
BEGIN
  wnd.new_menu(h_select); wnd.new_window(' HELP ',ON,h_select^.w);
  wnd.append(h_select, "'f2 '  - move menu",       {},{},0);
  wnd.append(h_select, "'f10'  - to board menu",   {},{},0);
  wnd.append(h_select, "'->|'  - change menu",     {},{},0);
  wnd.append(h_select, "'|<-'  - change menu",     {},{},0);
  wnd.append(h_select, "'Esc'  - delite menu",     {},{},0);
  wnd.append(h_select, "'Home' - top of menu",     {},{},0);
  wnd.append(h_select, "'End'  - bottom of menu",  {},{},0);
  wnd.append(h_select, "'.'    - current directory",{},{},0);
  wnd.place_menu(h_select,1,10,8,27,1);
  wnd.fr_color(h_select^.w, wnd.blck,wnd.brck);
  wnd.w_color( h_select^.w, wnd.blck,wnd.brck);
END ini_select_help;

PROCEDURE ini_board_help;
BEGIN
  wnd.new_menu(h_board); wnd.new_window(' HELP ',ON,h_board^.w);
  wnd.append(h_board, "'f1 '  - help in all menu",     {},{},0);
  wnd.append(h_board, "'f2 '  - move menu"        ,    {},{},0);
  wnd.append(h_board, "'f10'  - to board menu",        {},{},0);
  wnd.append(h_board, "'->|'  - change menu",          {},{},0);
  wnd.append(h_board, "'|<-'  - change menu",          {},{},0);
  wnd.append(h_board, "'Esc'  - RETURN to prev module",{},{},0);
  wnd.append(h_board, "'Home' - top of menu",          {},{},0);
  wnd.append(h_board, "'End'  - bottom of menu",       {},{},0);
  wnd.append(h_board, "'.'    - current directory",    {},{},0);
  wnd.place_menu(h_board,1,10,9,30,1);
  wnd.fr_color(h_board^.w, wnd.blck,wnd.brck);
  wnd.w_color( h_board^.w, wnd.blck,wnd.brck);
END ini_board_help;

PROCEDURE help(m: wnd.menu_ptr);
 VAR ch: CHAR;
BEGIN IF m=NIL THEN message(ON, 'help is NIL!'); RETURN END;
  wnd.refresh(m,OFF); key.read(ch);
  norm_color; wnd.erase(m^.w); refresh_scr;
END help;

---------------------------  NODEs  ---------------------------
                           ---------
VAR BIN, ETC: bio.PATHs;

PROCEDURE bio_err(VAL f: ARRAY OF CHAR; SEQ x: sys.WORD);
 VAR s: ARRAY [0..78] OF CHAR;
BEGIN
    lex.perror(s,bio.error,'%%s'); str.append(s,f,x);
    message(ON, s);
END bio_err;

PROCEDURE read_code(VAL s: ARRAY OF CHAR; VAR buf: STRING): BOOLEAN;

  PROCEDURE app_ext(VAR s: ARRAY OF CHAR);
    VAR i,l: INTEGER;
  BEGIN
    l:=str.len(s); i:=l-1;
    WHILE (i>=0) & (s[i]#'.') DO DEC(i) END;
    IF i<0 THEN i:=l END;
    str.image(s,i,'.cod')
  END app_ext;

  VAR name: ARRAY [0..255] OF CHAR;
      f   : bio.FILE;
      eof : INTEGER;

BEGIN
  str.print(name,s); app_ext(name);
  bio.lookup(BIN,f,name,'r');
  IF NOT bio.done THEN bio_err(' file "%s"',name); RETURN TRUE END;
  eof:=bio.eof(f);
  NEW(buf,bio.eof(f));
  IF buf^.ADR=NIL THEN
    message(ON,"can't"' allocate buffer for file "%s"',name);
    bio.close(f);
    RETURN TRUE
  END;
  bio.read(f,buf^.ADR,eof);
  IF NOT bio.done THEN
    bio.close(f); bio_err('read file "%s"',name);
    RETURN TRUE
  END;
  bio.close(f);
  RETURN FALSE
END read_code;

PROCEDURE rem(n_p: node_ptr; next: BOOLEAN);
BEGIN
  IF n_p^.link=0   THEN
    IF n_p^.code#NIL THEN vis.disconnect(n_p^.code) END;
    DISPOSE(n_p^.buf);
    IF n_p^.cu#NIL   THEN ref.exit_cu(n_p^.cu) END;
  END;
  IF next          THEN cur:=n_p^.next;      END;
END rem;

PROCEDURE set_alias(cu: ref.cu_ptr; code: vis.code_ptr);
   VAR i,j: INTEGER;
BEGIN
  IF (cu=NIL) OR (code=NIL) THEN RETURN END;
  FOR i:=1 TO HIGH(code^.proc_tab) DO
    FOR j:=i+1 TO HIGH(code^.proc_tab) DO
      IF code^.proc_tab[i]=code^.proc_tab[j] THEN
        cu^.proc_tab[i]:=cu^.proc_tab[j]
      END;
    END;
  END;
END set_alias;

PROCEDURE new(VAL s: ARRAY OF CHAR): BOOLEAN;
  VAR n: node_ptr; cu: ref.cu_ptr;
    msg: ARRAY [0..79] OF CHAR;
BEGIN
  NEW(n);      IF n=NIL          THEN RETURN TRUE END;
  str.copy(n^.name,s);
  NEW(n^.buf); --IF n^.buf^.ADR=NIL THEN RETURN TRUE END;
  IF read_code(s,n^.buf) THEN DISPOSE(n^.buf); n^.code:=NIL;
  ELSE vis.connect(n^.code,n^.buf);
    IF NOT vis.done THEN lex.perror(msg,vis.error,'%%s error %hh', vis.error);
      message(ON, msg);
    END;
  END;
  IF NOT no_ref THEN
    ref.read_cu(ref.main,cu,s,ON);
    IF ref.done THEN n^.cu:=cu
    ELSE message(ON,'ref not found %s',ref.note); n^.cu:=NIL;
    END;
  ELSE n^.cu:=NIL;
  END;
  IF (n^.code=NIL) & (n^.cu=NIL) THEN rem(n, OFF);
     message(OFF, "it's unposible...");  RETURN TRUE
  END;
  n^.next:=cur; cur:=n;
  IF (cur^.code#NIL) & (cur^.cu#NIL) THEN
    IF cur^.code^.def_time#cur^.cu^.def_time THEN
      message(ON,'time conflict');
      ref.exit_cu(cur^.cu);
      cur^.cu:=NIL;
    ELSE set_alias(cur^.cu,cur^.code);
    END;
  END;
  RETURN FALSE
END new;

---------------------------  RING -----------------------------
                            ------
PROCEDURE app_ring(m,h: wnd.menu_ptr; p: PROC; m_p: m_s_ptr): BOOLEAN;
 VAR r_p: ring_ptr;
BEGIN NEW(r_p);
  IF r_p=NIL THEN message(ON,' NEW return NIL!'); RETURN TRUE END;
  r_p^.m:=m; r_p^.h:=h; r_p^.proc:=p; r_p^.node:=cur; r_p^.m_s:=m_p;
  ASSERT(r_p^.m#NIL);
  IF r_p^.node#NIL THEN INC(r_p^.node^.link) END;
  IF ring=NIL THEN ring:=r_p; ring^.next:=ring; ring^.prev:=ring;
  ELSE r_p^.next:=ring^.next;  ring^.next:=r_p;
       r_p^.prev:=ring;        r_p^.next^.prev:=r_p;
       ring:=r_p;
  END;
  INC(ring_no);
  RETURN FALSE;
END app_ring;

PROCEDURE rem_from_ring();
 VAR r: ring_ptr; i: INTEGER;
BEGIN r:=ring;
  ASSERT(r^.m#NIL);
  norm_color; wnd.erase(r^.m^.w);
  IF r^.m#NIL THEN wnd.rem_window(r^.m^.w); wnd.rem_menu(r^.m) END;
  r^.prev^.next:=r^.next;
  r^.next^.prev:=r^.prev;
  ring:=r^.prev;
  IF (r^.node#NIL)&(r^.node^.link=1) THEN rem(r^.node, OFF);
  ELSE DEC(r^.node^.link);
  END;
  DISPOSE(r);  DEC(ring_no);
  refresh_scr;
END rem_from_ring;

---------------------------  REF  ------------------------------
                           -------

PROCEDURE app_name(VAR s: ARRAY OF CHAR; id: INTEGER);
  VAR name: ARRAY [0..63] OF CHAR;
BEGIN ASSERT(id>=0);
  ref.id_str(ring^.node^.cu^.names,id,name);
  str.append(s,'%s',name);
END app_name;

PROCEDURE enum_const(t: ref.type_ptr; val: INTEGER; VAR e: ref.enum_ptr);
BEGIN
  ASSERT(t^.mode=sym.enumtype);
  e:=t^.consts;
  WHILE e#NIL DO
    IF e^.val=val THEN RETURN END;
    e:=e^.next;
  END;
  ASSERT(FALSE);
END enum_const;

PROCEDURE app_tag(tag: BITSET; VAR ou: ARRAY OF CHAR);
BEGIN
  IF sym.seqparam IN tag THEN str.append(ou,'SEQ ') END;
  IF sym.varparam IN tag THEN str.append(ou,'VAR ') END;
  IF sym.readonly IN tag THEN str.append(ou,'VAL ') END;
END app_tag;

PROCEDURE app_type(t: ref.type_ptr; VAR ou: ARRAY OF CHAR;
                                           this?: BOOLEAN);

  PROCEDURE app_parm(t: ref.type_ptr; VAR ou: ARRAY OF CHAR);
    VAR p: ref.parm_ptr;
  BEGIN p:=t^.parms;
    WHILE p#NIL DO
      app_tag(p^.tags,ou);
      app_type(p^.type, ou, ON);
      IF p^.next#NIL THEN str.append(ou,',') END;
      p:=p^.next;
    END;
  END app_parm;

  PROCEDURE app_range(VAR ou: ARRAY OF CHAR; t: ref.type_ptr);
    VAR e: ref.enum_ptr;
  BEGIN
    CASE t^.base^.mode OF
    |sym.int     : str.append(ou,' [%d..%d]'  ,t^.min,t^.max);
    |sym.char    : str.append(ou,' [%bc..%bc]',t^.min,t^.max);
    |sym.enumtype: str.append(ou,' [');
                   enum_const(t^.base,t^.min,e); app_name(ou,e^.id);
                   str.append(ou,'..');
                   enum_const(t^.base,t^.max,e); app_name(ou,e^.id);
                   str.append(ou,']');
    ELSE ASSERT(FALSE,5Eh);
    END;
  END app_range;

  PROCEDURE app_enum(VAR ou: ARRAY OF CHAR; t: ref.type_ptr);
    VAR e: ref.enum_ptr;
  BEGIN e:=t^.consts;
    str.append(ou,' (');
    IF e#NIL THEN app_name(ou,e^.id); e:=e^.next END;
    WHILE e#NIL DO str.append(ou,','); app_name(ou,e^.id); e:=e^.next END;
    str.append(ou,')');
  END app_enum;

BEGIN ASSERT(t#NIL);
  IF (t^.id>=0) & this? THEN str.append(ou,' ');
      app_name(ou,t^.id); RETURN
  END;
  CASE t^.mode OF
  |sym.int     : str.append(ou,' INTEGER');
  |sym.bool    : str.append(ou,' BOOLEAN');
  |sym.char    : str.append(ou,' CHAR'   );
  |sym.bitset  : str.append(ou,' BITSET' );
  |sym.real    : str.append(ou,' REAL'   );
  |sym.addr    : str.append(ou,' ADDRESS');
  |sym.word    : str.append(ou,' WORD'   );
  |sym.enumtype: app_enum(ou,t);
  |sym.range   : app_range(ou,t);
  |sym.pointer : str.append(ou,' POINTER TO '); app_type(t^.base,ou,ON);
  |sym.hidden  : str.append(ou,' HIDDEN');
  |sym.proctype: str.append(ou,' PROCEDURE ('); app_parm(t,ou);
                 str.append(ou,')');
                 IF t^.base#NIL THEN
                   str.append(ou,':'); app_type(t^.base,ou, ON);
                 END;
  |sym.set     : str.append(ou,' SET OF '); app_type(t^.base,ou, ON);
  |sym.array   : str.append(ou,' ARRAY');
                 app_type(t^.inx,ou, ON);
                 str.append(ou,' OF');
                 app_type(t^.base,ou, ON);

  |sym.openarr : str.append(ou,' ARRAY OF'); app_type(t^.base,ou,ON);
  |sym.dynarr  : str.append(ou,' DYNARR OF'); app_type(t^.base,ou,ON);

  |sym.record  : str.append(ou,' RECORD');
  ELSE str.append(ou,'unknown type%d',t^.mode);
  END;
END app_type;

---------------------------------------------------------------
PROCEDURE Return;
 VAR ch: CHAR;
BEGIN
  IF (cur=NIL) OR (cur^.next=NIL) THEN
    message(OFF," Exit vx?"); key.read(ch);
    IF (ch='y') OR (ch='Y') THEN HALT
    ELSE clear_line(24)
    END;
  ELSE rem(cur, ON);
    IF cur#NIL THEN message(OFF,' RETURN to %s',cur^.name)
    ELSE            message(OFF," isn't any module")
    END;
    IF NOT cur^.cu^.complete THEN
      ref.read_cu(ref.main,cur^.cu,cur^.name,ON)
    END;
  END;
END Return;

PROCEDURE to_external(no: INTEGER): BOOLEAN;
  VAR name: str32;
BEGIN
  IF ring^.node^.cu#NIL THEN
    ref.id_str(ring^.node^.cu^.exts[no]^.names,0,name);
  ELSE ASSERT(ring^.node^.code#NIL);
    str.copy(name,ring^.node^.code^.exts[no]^.name);
  END;
  message(OFF,' to %s',name);
  RETURN new(name)
END to_external;

--------------------------- select -------------------------
                           --------
PROCEDURE select_board;
  VAR exit: BOOLEAN; p: PROC;
BEGIN clear_line(1); fr_board:=ON;
  wnd.refresh(board,OFF);
  LOOP wnd.select(board,exit);
    IF exit THEN EXIT
    ELSE wnd.refresh(board, OFF);
      p:=board^.lines[board^.ln]^.id; p(); fr_board:=OFF; RETURN
    END;
  END;
  IF read_done THEN Return END;
END select_board;

PROCEDURE select_menu;
 VAR exit: BOOLEAN;
BEGIN wnd.refresh(ring^.m,OFF);
  LOOP wnd.select(ring^.m,exit); IF exit THEN EXIT END END;
  IF read_done THEN rem_from_ring END;
END select_menu;

PROCEDURE select_Externals;
 VAR exit: BOOLEAN;
BEGIN clear_line(1); wnd.refresh(ring^.m,OFF);
  LOOP wnd.select(ring^.m,exit);
    IF exit THEN EXIT END;
    IF NOT to_external(ring^.m^.ln+1) THEN
      ring:=r_board;  wnd.refresh(ring^.m,OFF); RETURN
    END;
  END;
  IF read_done THEN rem_from_ring END;
END select_Externals;

PROCEDURE ini_load; FORWARD;

PROCEDURE new_dir(VAR name: ARRAY OF CHAR);
 VAR i: INTEGER; f: bio.FILE; s: str256;
BEGIN i:=str.len(name)-1; s:=name;
  WHILE (i>0)&(name[i]='/') DO name[i]:=0c; DEC(i); END;
  bio.open(f,name,'r');
  IF NOT bio.done THEN bio_err(name); RETURN END;
  bio.close(f); bio.chdir(name);
  IF bio.done THEN ini_load; change_cd_name(s);
  ELSE bio_err('in chdir to "%s"', name)
  END;
END new_dir;

PROCEDURE select_load;
 VAR exit: BOOLEAN; mode: BITSET;
     i   : INTEGER; name: str80;
BEGIN ASSERT(m_load#NIL);
  clear_line(1); wnd.refresh(m_load, OFF);
  LOOP wnd.select(m_load, exit);
    IF exit THEN EXIT END;
    mode:=m_load^.lines[m_load^.ln]^.id;
    IF mode*bio.e_dir#{} THEN name:=m_load^.lines[m_load^.ln]^.line;
      new_dir(name);
    ELSE
      IF new(m_load^.lines[m_load^.ln]^.line) THEN
         message(OFF,'S o r r y . . .')
      ELSE message(OFF,' to %s',m_load^.lines[m_load^.ln]^.line);
           wnd.refresh(m_load, OFF); ring:=r_board;
      END;
      RETURN
    END;
  END;
  IF read_done THEN rem_from_ring; m_load:=NIL;
    load?:=OFF; r_load:=NIL;
  END;
END select_load;

PROCEDURE select_common;
  VAR p: PROC;
BEGIN p:=ring^.proc; p(); END select_common;

-------------------------- move -------------------------
                          ---------
PROCEDURE w_move(m: wnd.menu_ptr);
 VAR high, width, cols, l, c : INTEGER;
     high0,width0,cols0,l0,c0: INTEGER;
     ch       : CHAR;
     name     : str32;
     fr, m_col: INTEGER;
     intns    : BITSET;

  PROCEDURE frame;
  BEGIN norm_color; wnd.erase(m^.w);
    IF wnd.frame?(m^.w) THEN wnd.show_frm(m^.w)
    ELSE wnd.refresh(m,OFF)
    END;
  END frame;

BEGIN fr:=ORD(wnd.frame?(m^.w)); frame; wnd.w_name(m^.w,name);
  message(OFF," move window '%s'", name);
  wnd.screen_pos(m,l0,c0,width0,high0,cols0);
  high:=high0; width:=width0; cols:=cols0; l:=l0; c:=c0;
  m_col:=wnd.get_col(m^.w);
  LOOP key.read(ch);
     CASE ch OF
      |key.cr,33c: EXIT
      |key.up    : DEC(l)
      |key.dw    : INC(l)
      |key.right : INC(c)
      |key.left  : DEC(c)
      |key.tab   : INC(c,8)
      |key.bcktab: DEC(c,8)
      |key.pgup  : DEC(l,4)
      |key.pgdw  : INC(l,4)
      |key.ins   : INC(width)
      |key.del   : DEC(width)
      |key.f1    : help(h_move);
      |key.f3    : INC(high)
      |key.f4    : DEC(high)
      |key.home  : c:=0
      |key.end   : c:=79-(width+fr)*cols-fr
      |'+'       : INC(cols)
      |'-'       : DEC(cols)
     ELSE key.bell(3)
     END;
     IF (high>0)&(width>0)&(cols>0)&(l>=0)&(c>=0)&
        (l+high+2*fr<25)&(c+(width+fr)*cols<79)  THEN
       norm_color; wnd.erase_frm(m^.w);
       wnd.place(m^.w,l,c,high,width,cols);
       IF wnd.undef(m^.w) THEN key.bell(3);
          wnd.place_menu(m,l0,c0,high0,width0,cols0);
       ELSE high0:=high; width0:=width; cols0:=cols; l0:=l; c0:=c;
       END;
       frame;
     ELSE key.bell(3);
          high:=high0; width:=width0; cols:=cols0; l:=l0; c:=c0;
     END;
  END;
  IF cols0<=m_col THEN wnd.set_col(m^.w,cols0-1) END;
  wnd.refresh(m, OFF);
  message(OFF,'fix to setup?'); key.read(ch);
  IF (ch='y') OR (ch='Y') THEN
       ring^.m_s^.l0   :=l0;
       ring^.m_s^.c0   :=c0;
       ring^.m_s^.high :=high0;
       ring^.m_s^.width:=width0;
       ring^.m_s^.cols :=cols0;
  END;
  clear_line(24); refresh_scr;
END w_move;
------------------------------------------------------------

PROCEDURE read_proc(m: wnd.menu_ptr): CHAR;
 VAR ch: CHAR; s : str80;
BEGIN key.read(ch); read_done:=ON; clear_line(24);
  CASE ch OF
  | key.tab   : ring:=ring^.next;
  | key.bcktab: ring:=ring^.prev;
  | key.f10   : ring:=r_board;
  | key.f1    : help(ring^.h); refresh_scr;                        RETURN 0c
  | key.f2    : w_move(m);                                         RETURN 0c
  | key.home  : wnd.jump(m, 0);                                    RETURN 0c
  | key.end   : wnd.jump(m, m^.free);                              RETURN 0c
  | '.'       : message(OFF, ' curent directory is "%s"',cd_name); RETURN 0c
  ELSE RETURN ch
  END;
  read_done:=OFF;
  RETURN 33c;
END read_proc;

PROCEDURE ini_externals;
  VAR m: wnd.menu_ptr; exit: BOOLEAN; i,no: INTEGER;
      s: str256; name: str32;
BEGIN
  IF cur=NIL THEN
    key.bell(3); message(OFF, 'Select module, please...'); RETURN
  END;
  wnd.new_menu(m);
  str.print(s,' %s EXTERNAL ',cur^.name);
  wnd.new_window(s,ON,m^.w);
  IF app_ring(m,h_load,select_Externals,sys.ADR(setup_rec.ext)) THEN RETURN END;
  IF cur^.cu#NIL THEN
    no:=HIGH(cur^.cu^.exts);
    FOR i:=1 TO no DO
      str.print(s,'[%$2h] %s',i,ring^.node^.cu^.exts[i]^.names);
      wnd.append(m,s,{},{wnd._inv},i);
    END;
  ELSE ASSERT(cur^.code#NIL);
    no:=HIGH(cur^.code^.exts);
    FOR i:=1 TO no DO
      str.print(s,'[%$2h] %s',i,ring^.node^.code^.exts[i]^.name);
      wnd.append(m,s,{},{wnd._inv},i);
    END;
  END;
  IF no<1 THEN message(OFF,"no external modules");   RETURN END;
  WITH setup_rec.ext DO wnd.place_menu(m,l0,c0,high,width,cols) END;
  m^.read:=read_proc;
  wnd.fr_color(m^.w, wnd.blck,wnd.lgr);
  wnd.w_color( m^.w, wnd.blck,wnd.gr);
END ini_externals;

------------------------- PROCEDURE -------------------------
                         -----------

PROCEDURE apph(VAR str: ARRAY OF CHAR; VAR s:INTEGER; val,n: INTEGER);
  VAR i: INTEGER;
BEGIN i:=8-n;
  WHILE i>0 DO val:=val<<4; DEC(i) END;
  WHILE n>0 DO val:=val<<4; str[s]:=dig16[val MOD 16]; INC(s); DEC(n) END;
  str[s]:=' '; INC(s);
END apph;

PROCEDURE vis_cmd(VAL code: ARRAY OF CHAR;
                      proc: INTEGER;
                  VAR pc  : INTEGER;
                  VAR mnem: ARRAY OF CHAR);

  PROCEDURE next(): INTEGER;
  BEGIN RETURN ORD(code[proc+pc]) END next;

  PROCEDURE next2(): INTEGER;
  BEGIN RETURN ORD(code[proc+pc]) + ORD(code[proc+pc+1])*256
  END next2;

  PROCEDURE next4(): INTEGER;
  BEGIN RETURN INTEGER(
        BITSET(code[proc+pc+0]    ) + BITSET(code[proc+pc+1]<<8)
      + BITSET(code[proc+pc+2]<<16) + BITSET(code[proc+pc+3]>>8) )
  END next4;

  VAR cmd: INTEGER; len,m,n: INTEGER;
BEGIN
  cmd:=ORD(code[proc+pc]); INC(pc);
  len:=ORD(vis.cmd_len[cmd]);
  vis.vis_command(cmd,mnem);
  m:=str.len(mnem);
  CASE cmd OF
  |mcd.jflc,mcd.jfl  : n:=next2(); apph(mnem,m,n,4); INC(pc,2);
                       str.image(mnem,m,'->.%h',pc+n);
  |mcd.jblc,mcd.jbl  : n:=next2(); apph(mnem,m,n,4); INC(pc,2);
                       str.image(mnem,m,'->.%h',pc-n);
  |mcd.jfsc,mcd.jfs
  ,mcd.andjp,mcd.orjp: n:=next(); apph(mnem,m,n,2);  INC(pc);
                       str.image(mnem,m,'->.%h',pc+n);
  |mcd.jbsc,mcd.jbs  : n:=next(); apph(mnem,m,n,2);  INC(pc);
                       str.image(mnem,m,'->.%h',pc-n);
  |mcd.liw           : apph(mnem,m,next4(),8); INC(pc,4);
  |mcd.lid,mcd.lsta  : apph(mnem,m,next2(),4); INC(pc,2);
  ELSE
    WHILE len>0 DO
      apph(mnem,m,next(),2); INC(pc); DEC(len);
    END;
  END;
  mnem[m]:=0c;
END vis_cmd;

PROCEDURE proc_range(n: INTEGER; VAR sta,fin: INTEGER);
  VAR proc_tab: vis.INTs; i: INTEGER;
BEGIN
  proc_tab^:=ring^.node^.code^.proc_tab^;
  fin:=HIGH(ring^.node^.code^.code);
  sta:=proc_tab[n];
  FOR i:=0 TO HIGH(proc_tab) DO
    IF (proc_tab[i]>sta) & (proc_tab[i]-1<fin) THEN
      fin:=proc_tab[i]-1
    END;
  END;
END proc_range;

PROCEDURE deass(n: INTEGER; m: wnd.menu_ptr);
  VAR sta,fin,pc,_pc,l,c,_l,_c,sp: INTEGER;
      mnem: str64; code: STRING; s: str256;
BEGIN
  ASSERT(ring^.node^.code#NIL);
  code^:=ring^.node^.code^.code^;
  proc_range(n,sta,fin);
  _l:=0; _c:=0; pc:=0; DEC(fin,sta);
  REPEAT sp:=0; _pc:=pc;
    vis_cmd(code,sta,pc,mnem);
    str.image(s,sp,'%$4h: %$3h   %s',sta+_pc,_pc,mnem);
    IF ring^.node^.cu#NIL THEN
      ref.text_pos(ring^.node^.cu^.proc_tab[n]^.locs^.xpos,_pc,l,c);
      IF ((l>=0) & (c>=0)) & ((_l#l) OR (_c#c)) THEN
        WHILE sp<26 DO s[sp]:=' '; INC(sp) END;
        str.image(s,sp,'  %d.%d', l, c); _l:=l; _c:=c;
      END;
    END;
    wnd.append(m,s,{},{wnd._inv},_pc);
  UNTIL pc>fin;
END deass;

PROCEDURE in_proc(n,pc: INTEGER): BOOLEAN;
  VAR sta,fin: INTEGER;
BEGIN proc_range(n,sta,fin); RETURN (pc>=sta) & (pc<=fin) END in_proc;

PROCEDURE read_str(VAL pro: ARRAY OF CHAR; VAR s: ARRAY OF CHAR);
BEGIN sed.edit_str(pro,s,24,0,78,SED); tty.set_cursor(0) END read_str;

PROCEDURE read_num(ch: CHAR): INTEGER;
 VAR n,i: INTEGER;   s: str32;
     ok : BOOLEAN;
BEGIN tty.set_cursor(1);
  IF    ch='f' THEN read_str('proc with pc:',s);
  ELSIF ch='l' THEN read_str('local pc:',    s);
  ELSE              read_str('global pc:',   s);
  END;
  clear_line(24); tty.set_cursor(0); i:=0;
  str.iscan(n,s,i,ok);
  IF NOT ok THEN n:=-1 END;
  RETURN n
END read_num;

PROCEDURE write_to_file(m: wnd.menu_ptr);
 VAR line  : str80;
     s     : str256;
     f     : bio.FILE;
     n, pos: INTEGER;
BEGIN
  IF finish-start<=0 THEN message(OFF, ' What?'); RETURN END;
  read_str('file for write:', s);
  IF s[0]=0c THEN RETURN END;
  IF original_cd#cd_name THEN bio.chdir(original_cd);
     IF NOT bio.done THEN bio_err(" can't write"); RETURN END;
  END;
  bio.lookup(bio.here, f, s, 'a');
  IF NOT bio.done THEN
     IF bio.error=err.sec_vio THEN bio_err(''); RETURN END;
     bio.create(f, s, 'w', 0);
     IF NOT bio.done THEN bio_err('create "%s"'); RETURN END;
  END;
  n:=start; message(OFF,' --- W R I T I N G ---');
  wnd.w_name(m^.w, line);
  str.print(s,'----- MODULE %s: %s -----'036c, ring^.node^.name, line);
  bio.putstr(f, s, 0);
  WHILE (n#finish)&(n<m^.free) DO pos:=0;
    str.image(line, pos, '%s'036c, m^.lines[n]^.line);
    bio.putstr(f, line, 0); INC(n);
    IF NOT bio.done THEN bio_err(' in write'); RETURN END;
  END;
  bio.putstr(f, '----------------------------------'036c, 0);
  message(OFF,' DONE'); bio.close(f);
  IF original_cd#cd_name THEN bio.chdir(cd_name);
     IF NOT bio.done THEN bio_err(" can't return") END;
  END;

END write_to_file;

PROCEDURE marking;
 VAR up, bt, i: INTEGER;
BEGIN
  ASSERT(finish<p_load^.free);
  IF start>finish   THEN i:=start; start:=finish; finish:=i END;
  up:=wnd.first(p_load); bt:=wnd.last(p_load);
  IF start>up  THEN up:=start  END;
  IF finish<bt THEN bt:=finish END;
  FOR i:=up TO bt DO p_load^.lines[i]^.mode:={wnd._inv} END;
  wnd.refresh(p_load, OFF);
  FOR i:=up TO bt DO p_load^.lines[i]^.mode:={}         END;
  REPEAT key.wait(100) UNTIL key.done;
  wnd.refresh(p_load, OFF);
END marking;

PROCEDURE find_command(m: wnd.menu_ptr);
 VAR ln,i: INTEGER;     com, buf : str80;
     ch  : CHAR;        done     : BOOLEAN;
BEGIN ln:=m^.ln+1;
  read_str('search command: ', com); i:=0;
  WHILE com[i]#0c DO com[i]:=ascii.CAPITAL(com[i]); INC(i); END;
  message(OFF,'search');
  WHILE ln<m^.free DO i:=12;
    message(OFF,'search %d', ln);
    str.scan(m^.lines[ln]^.line, i, com, done);
    IF done THEN wnd.jump(m,ln); message(OFF,'press "+" to search next');
       key.read(ch);
       IF ch#'+' THEN clear_line(24); RETURN ELSE INC(ln) END;
    ELSE INC(ln);
    END;
  END;
  message(OFF, 'not find');
END find_command;

PROCEDURE read_in_proc(m: wnd.menu_ptr): CHAR;

   PROCEDURE refr;
   BEGIN
     IF m=p_load THEN
        IF i_load#NIL THEN wnd.refresh(i_load, OFF) END;
        wnd.refresh(p_load, OFF)
     ELSE wnd.refresh(p_load, OFF);
        IF i_load#NIL THEN wnd.refresh(i_load, OFF) END;
     END;
   END refr;

 VAR ch: CHAR; sta,fin,i,n,x: INTEGER;
BEGIN
  key.read(ch); read_done:=TRUE;
  CASE ch OF
   |'w'       : write_to_file(m);           RETURN 0c;
   |'c'       : find_command(m);            RETURN 0c;
   |'['       : IF m=p_load THEN
                  start :=m^.ln; marking;
                END;                        RETURN 0c;
   |']'       : IF m=p_load THEN
                  finish:=m^.ln; marking;
                END;                        RETURN 0c;
   |key.home  : wnd.jump(m, 0);             RETURN 0c;
   |key.end   : wnd.jump(m, m^.free);       RETURN 0c;
   |key.bcktab,
    key.tab   : read_done:=FALSE;           RETURN 33c;
   |key.f1    : help(h_in_proc); refr;      RETURN 0c;
   |key.f2    : w_move(m);       refr;      RETURN 0c;
   RETURN 0c
   |'g','l'   :
  ELSE RETURN ch
  END;
  proc_range(ring^.node^.pno,sta,fin); n:=read_num(ch);
  IF ch='g' THEN DEC(n,sta) END;
  IF (n>=0) & (n<=fin-sta) THEN
    i:=p_load^.ln; x:=p_load^.lines[i]^.id;
    IF    n<x THEN
      WHILE (i#0)&(n<INTEGER(p_load^.lines[i]^.id)) DO DEC(i) END;
    ELSIF n>x THEN
      REPEAT INC(i)
      UNTIL (i>=p_load^.free-1) OR (n<INTEGER(p_load^.lines[i+1]^.id))
    END;
    wnd.jump(p_load, i);
  ELSE
    IF ch='l' THEN
         message(ON,' select local  pc in range [%h..%h]',0,fin-sta);
    ELSE message(ON,' select global pc in range [%h..%h]',sta,fin);
    END;
  END;
  RETURN 0c
END read_in_proc;

PROCEDURE showProc(n: INTEGER);
 VAR m, h: wnd.menu_ptr;  i: INTEGER;
     exit: BOOLEAN;       s: str64;

  PROCEDURE jump?(m: wnd.menu_ptr): INTEGER;
    VAR code: STRING; line,pc,x,proc: INTEGER;
  BEGIN line:=m^.ln;
    proc:=ring^.node^.code^.proc_tab[n];
    pc:=INTEGER(m^.lines[line]^.id)+proc;
    code^:=ring^.node^.code^.code^;
    x:=ORD(code[pc]); INC(pc);
    CASE x OF
     |mcd.jfsc,mcd.jfs,
      mcd.orjp,mcd.andjp: x:=pc+1+ORD(code[pc]);
     |mcd.jbsc,mcd.jbs  : x:=pc+1-ORD(code[pc]);
     |mcd.jflc,mcd.jfl  : x:=pc+2+ORD(code[pc])+ORD(code[pc+1])*100h;
     |mcd.jblc,mcd.jbl  : x:=pc+2-ORD(code[pc])-ORD(code[pc+1])*100h;
    ELSE RETURN line
    END;
    DEC(x ,proc); DEC(pc,proc);
    IF x<pc THEN REPEAT DEC(line) UNTIL x>=INTEGER(m^.lines[line]^.id)
    ELSE         REPEAT INC(line) UNTIL x<=INTEGER(m^.lines[line]^.id)
    END; RETURN line
  END jump?;

  PROCEDURE select_inproc;
  BEGIN
  wnd.refresh(m,OFF);
    LOOP
      wnd.select(m,exit);
      IF exit THEN EXIT END;
       i:=jump?(m);
      IF i#m^.ln THEN wnd.jump(m,i) END;
    END;
  END select_inproc;

  PROCEDURE vis_proc(p: ref.obj_ptr);
    VAR ou: str256;        type : ref.type_ptr;
      i   : INTEGER;       first: BOOLEAN;
      vars: ref.obj_ptr;

    PROCEDURE app_parms(parm: ref.parm_ptr);
      VAR tags: BITSET;
          type: ref.type_ptr;
    BEGIN str.append(ou,'(');
      first:=ON;
      tags:=parm^.tags;
      type:=parm^.type;
      WHILE parm#NIL DO
        IF (NOT first) & ((parm^.type#type)OR(parm^.tags#tags)) THEN
          str.append(ou,':');  app_type(type,ou,ON);
          str.append(ou,'; '); app_tag(parm^.tags,ou);
        ELSIF first THEN app_tag(tags,ou); first:=OFF
        ELSE  str.append(ou, ', ');
        END;
        IF parm^.var#NIL  THEN app_name(ou, parm^.var^.id) END;
        IF parm^.next=NIL THEN
          str.append(ou,':'); app_type(parm^.type,ou,ON);
        END;
        type:=parm^.type; tags:=parm^.tags;
        parm:=parm^.next;
      END;
      str.append(ou,')');
    END app_parms;

  BEGIN
    wnd.new_menu(h); wnd.new_window('', ON, h^.w);
    str.print(ou,'PROCEDURE '); app_name(ou, p^.id);
    IF p^.type^.parms#NIL THEN app_parms(p^.type^.parms) END;
    type:=p^.type;
    IF type^.base#NIL THEN
      IF p^.type^.parms=NIL THEN str.append(ou,'()') END;
      str.append(ou,':');
      app_type(type^.base,ou,ON);
    END;
    str.append(ou,';');
    wnd.append(h,ou,{},{wnd._inv},0);
    first:=ON;
    vars:=p^.locs^.vars;
    WHILE vars#NIL DO ou[0]:=0c;
      IF vars^.parm=NIL THEN
        IF first THEN str.append(ou,'  VAR ')
        ELSE          str.append(ou,'      ')
        END;
        app_name(ou, vars^.id);  str.append(ou,': ');
        app_type(vars^.type,ou,ON);
        wnd.append(h,ou,{},{wnd._inv},0);
        first:=OFF;
      END;
      vars:=vars^.next;
    END;
    WITH setup_rec.head_proc DO wnd.place_menu(h,l0,c0,high,width,cols) END;
    wnd.fr_color(h^.w, wnd.blck,wnd.brck);
    wnd.w_color( h^.w, wnd.blck,wnd.brck);
  END vis_proc;

BEGIN
  IF n<0 THEN RETURN  END;
  IF n>0 THEN
    IF ring^.node^.cu#NIL THEN vis_proc(ring^.node^.cu^.proc_tab[n]) END;
    h^.read:=read_in_proc;
  ELSE h:=NIL
  END;
  clear_scr; str.print(s,' PROCEDURE ');
  IF (ring^.node^.cu#NIL)&(n#0) THEN
    app_name(s, ring^.node^.cu^.proc_tab[n]^.id);
  ELSE i:=10; str.image(s, i, '[%hh] ', n);
  END;
  wnd.new_menu(m); wnd.new_window(s, ON, m^.w);
  deass(n,m); i:=0;
  IF h#NIL THEN i_load:=h;
    IF h^.free<setup_rec.in_proch.high THEN i:=h^.free+2;
    ELSE i:=setup_rec.in_proch.high;
    END;
  ELSE i_load:=NIL;
  END;
  WITH setup_rec.in_proc DO wnd.place_menu(m,i,c0,22-i,width,cols) END;
  ring^.node^.pno:=n;
  m^.read:=read_in_proc;
  p_load:=m;
  start:=0; finish:=m^.free-1;
  wnd.fr_color(m^.w, wnd.blck,wnd.gr);
  wnd.w_color( m^.w, wnd.blck,wnd.gr);
  clear_scr; refr?:=OFF;
  IF h#NIL THEN wnd.refresh(h,OFF) END;
  LOOP
    select_inproc;
    IF read_done THEN EXIT END;
    IF h#NIL THEN wnd.refresh(h, OFF); wnd.select(h,exit);
      IF exit & read_done THEN norm_color; wnd.erase(h^.w);
         wnd.rem_window(h^.w); wnd.rem_menu(h); h:=NIL
      ELSE wnd.refresh(h, OFF);
      END;
    END
  END;
  IF h#NIL THEN wnd.rem_window(h^.w); wnd.rem_menu(h) END;
  wnd.rem_window(m^.w); wnd.rem_menu(m);
  clear_scr; refr?:=ON; p_load:=NIL; i_load:=NIL;
END showProc;

PROCEDURE proc_pc(m: wnd.menu_ptr);
 VAR l,c,pc: INTEGER;
BEGIN
  ASSERT((m^.ln>=0)&(m^.ln<m^.free));
  clear_line(1);
  pc:=ring^.node^.code^.proc_tab[m^.ln];
  tty.print('pc: %-4h', pc);
  IF ring^.node^.cu#NIL THEN
(*
    IF m^.ln>=HIGH(ring^.node^.cu^.proc_tab) THEN
      message(ON, 'ln %d  in p_tab[%d]', m^.ln,
                  HIGH(ring^.node^.cu^.proc_tab))
    END;
*)
    ref.text_pos(ring^.node^.cu^.proc_tab[m^.ln]^.locs^.xpos, 0, l,c);
    tty.print('    text line: %-4d', l);
  END;
END proc_pc;

PROCEDURE read_in_ptab(m: wnd.menu_ptr): CHAR;
 VAR ch   : CHAR;
   i ,n   : INTEGER;
   name, s: str256;
BEGIN ch:=read_proc(m);
  CASE ch OF
  |'f':n:=read_num(ch); i:=0;
       WHILE (i<HIGH(ring^.node^.code^.proc_tab))&(NOT in_proc(i,n)) DO INC(i) END;
       IF i=HIGH(ring^.node^.code^.proc_tab) THEN
         message(TRUE,'pc must be in range [%h..%h]'
                      ,BYTES(cur^.code^.proc_tab),BYTES(cur^.code^.code));
       ELSE wnd.jump(m,i);
       END;
  |'n':IF ring^.node^.cu=NIL THEN
         message(OFF,'only with ref_file '); RETURN 0c
       END;
       read_str('procedure with name ', name);
       tty.set_cursor(0); i:=0;
       WHILE i<m^.free DO
         IF i=0 THEN  s:='BEGIN';
         ELSIF ring^.node^.cu^.proc_tab[i]^.id>0 THEN
           s[0]:=0c; app_name(s,ring^.node^.cu^.proc_tab[i]^.id);
         END;
         IF name=s THEN wnd.jump(m,i); clear_line(24); RETURN 0c END;
         INC(i);
       END;
       message(OFF, 'PROCEDURE "%s" not found', name); clear_line(24);
  |'_': wnd.jump(m, m^.free)
  ELSE RETURN ch
  END;
       RETURN 0c;
END read_in_ptab;

PROCEDURE select_procTable;
 VAR m   : wnd.menu_ptr;
     exit: BOOLEAN;
BEGIN ASSERT(ring#NIL); m:=ring^.m;
  wnd.refresh(m,OFF);
  LOOP
    wnd.select(m,exit);
    IF exit THEN EXIT END;
    IF ring^.node^.code#NIL THEN showProc(m^.ln); refresh_scr;
    ELSE message(OFF, 'Widht cod only...')
    END;
  END;
  IF read_done THEN rem_from_ring END;
END select_procTable;

PROCEDURE ini_proc_table;
  VAR m: wnd.menu_ptr; exit: BOOLEAN; i,no: INTEGER;
      s: str64;
BEGIN
  IF cur=NIL THEN
    key.bell(3); message(OFF, 'Select module, please...'); RETURN
  END;
  IF cur^.code#NIL THEN
    no:=HIGH(cur^.code^.proc_tab)+1;
  ELSE ASSERT(cur^.cu#NIL);
    no:=HIGH(cur^.cu^.proc_tab)+1;
  END;
  IF no<=0 THEN RETURN END;
  wnd.new_menu(m);
  str.print(s,' %s PROCEDUREs ',cur^.name);
  wnd.new_window(s,ON,m^.w);
  IF app_ring(m, h_proc_tab, select_procTable,sys.ADR(setup_rec.proc)) THEN
     RETURN
  END;
  wnd.append(m,'[ 0] BEGIN',{},{wnd._inv},0);
  FOR i:=1 TO no-1 DO
    str.print(s, '[%$2h] ', i);
    IF (cur^.cu#NIL) & (cur^.cu^.proc_tab[i]^.id>0) THEN
      app_name(s,cur^.cu^.proc_tab[i]^.id);
    END;
    wnd.append(m,s,{},{wnd._inv},i);
  END;
  WITH setup_rec.proc DO wnd.place_menu(m,l0,c0,high,width,cols) END;
  IF cur^.code#NIL THEN
    m^.act:=proc_pc;
    m^.read:=read_in_ptab;
  END;
  wnd.fr_color(m^.w, wnd.blck,wnd.gr);
  wnd.w_color( m^.w, wnd.blck,wnd.gr);
END ini_proc_table;

----------------  GLOBALs ------------------------------
                ----------
PROCEDURE vis_glob(m: wnd.menu_ptr);
  VAR ou: str256; var: ref.obj_ptr;
BEGIN
  clear_line(1);
  var:=m^.lines[m^.ln]^.id;
  ou[0]:=0c;
  app_type(var^.type,ou,ON);
  tty.print('%s;',ou);
END vis_glob;

PROCEDURE showGlob;
 VAR glob: ref.obj_ptr; s: str64;
     m: wnd.menu_ptr; exit: BOOLEAN;
BEGIN
  IF (cur=NIL) OR (cur^.cu=NIL) THEN
    message(OFF,' only with ref_file'); RETURN
  END;
  glob:=cur^.cu^.locs^.vars;
  IF glob=NIL THEN message(OFF,'no globals'); RETURN END;
  wnd.new_menu(m);
  str.print(s,' %s globals ',cur^.name);
  wnd.new_window(s,ON,m^.w);
  IF app_ring(m,h_1str,select_menu,sys.ADR(setup_rec.glob)) THEN  RETURN END;
  WHILE glob#NIL DO
    str.print(s,'[%02h] ',glob^.ofs-2);
    IF glob^.id>0 THEN app_name(s,glob^.id);
       wnd.append(m,s,{},{wnd._inv},glob);
    END;
    glob:=glob^.next;
  END;
  WITH setup_rec.glob DO wnd.place_menu(m,l0,c0,high,width,cols) END;
  m^.act:=vis_glob;
  m^.read:=read_proc;
  wnd.fr_color(m^.w, wnd.blck,wnd.blue);
  wnd.w_color( m^.w, wnd.blck,wnd.gr);
  select_menu;
END showGlob;

--------------------------- TYPEs ---------------------------
                           -------
PROCEDURE vis_type(m: wnd.menu_ptr);
  VAR t: ref.type_ptr; ou: str256;
BEGIN
  clear_line(1); t:=m^.lines[m^.ln]^.id;
  ou[0]:=0c;     app_type(t,ou,OFF);
  tty.print('%s', ou);
END vis_type;

PROCEDURE typeTable;
 VAR cu: ref.cu_ptr; type: ref.type_ptr;
   s,mod: str64; no,i: INTEGER;
     m: wnd.menu_ptr; exit: BOOLEAN;
BEGIN
  IF (cur=NIL) OR (cur^.cu=NIL) THEN
    message(OFF,' only with ref_file'); RETURN
  END;
  cu:=cur^.cu;
  no:=HIGH(cu^.types)+1;
  IF no<=0 THEN message(OFF,'no types'); RETURN END;
  wnd.new_menu(m);
  str.print(s,' %s TYPEs ',cur^.name);
  wnd.new_window(s,ON,m^.w);
  IF app_ring(m,h_1str,select_menu,sys.ADR(setup_rec.types)) THEN RETURN END;
  FOR i:=0 TO no-1 DO
    s[0]:=0c;
    IF cu^.types[i]^.id>0 THEN
      type:=cur^.cu^.types[i];
      IF type^.modno>0 THEN
        ref.id_str(cu^.exts[type^.modno]^.names,0,mod);
        str.append(s,'%s.',mod);
      END;
      app_name(s,type^.id);
      wnd.append(m,s,{},{wnd._inv},type);
    END;
  END;
  WITH setup_rec.types DO wnd.place_menu(m,l0,c0,high,width,cols) END;
  m^.act:=vis_type; m^.read:=read_proc;
  wnd.fr_color(m^.w,wnd.blck,wnd.red);
  wnd.w_color( m^.w,wnd.blck,wnd.gr);
  select_menu;
END typeTable;

PROCEDURE Constants;
  VAR cu: ref.cu_ptr; cons: ref.obj_ptr;
     st : STRING; body,s,l: str80; i,j: INTEGER;
     m: wnd.menu_ptr; exit: BOOLEAN;
BEGIN
  IF (cur=NIL) OR (cur^.cu=NIL) THEN
    message(OFF,' only with ref_file'); RETURN
  END;
  cu:=cur^.cu;
  IF (cu^.locs=NIL) OR (cu^.locs^.cons=NIL) THEN
    message(OFF,'no consts'); RETURN
  END;
  wnd.new_menu(m);
  str.print(s,' %s constants ',cur^.name);
  wnd.new_window(s,ON,m^.w);
  IF app_ring(m,h_select,select_menu,sys.ADR(setup_rec.const)) THEN RETURN END;
  cons:=cu^.locs^.cons;
  st^:=cur^.code^.strings^;
  WHILE cons#NIL DO
    s[0]:=0c;
    app_name(s,cons^.id);
    i:=0; j:=cons^.ofs*4;
    WHILE st[j]#0c DO
      IF ORD(st[j]) MOD 128 >= 40b THEN body[i]:=st[j]
      ELSE body[i]:='.'
      END;
      INC(i); INC(j);
    END;
    body[i]:=0c;
    str.print(l,' <%4hh> %s: %s',cons^.ofs,s,body);
    wnd.append(m,l,{},{wnd._inv},0);
    cons:=cons^.next;
  END;
  WITH setup_rec.const DO wnd.place_menu(m,l0,c0,high,width,cols) END;
  m^.read:=read_proc;
  wnd.fr_color(m^.w, wnd.blck,wnd.brck);
  wnd.w_color( m^.w, wnd.blck,wnd.gr);
  select_menu;
END Constants;

PROCEDURE select_str;
  CONST head =
  "    "
  " :  0           1           2           3           : 0   1   2   3    :"
  "\n    "
  " : 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F  : 0123456789ABCDEF :"
  ;
 VAR m: wnd.menu_ptr; exit: BOOLEAN;
BEGIN ASSERT(ring#NIL); m:=ring^.m;
  clear_line(2); clear_line(1);
  norm_color; tty.print(head);
  wnd.refresh(m,OFF);
  LOOP wnd.select(m,exit); IF exit THEN EXIT END END;
  IF read_done THEN rem_from_ring END;
  clear_line(2); clear_line(1);
END select_str;

PROCEDURE ini_strings;
  VAR st: STRING;

     sym,hex: str64;        i,j,k: INTEGER;
    s,h,len: INTEGER;       m    : wnd.menu_ptr;
     exit  : BOOLEAN;       l    : str80;
BEGIN
  IF cur=NIL THEN
    key.bell(3); message(OFF, 'Select module, please...'); RETURN
  END;
  wnd.new_menu(m); wnd.new_window('Strings & CONSTANTs',ON,m^.w);
  message(OFF, 'wait, please...');
  st^:=cur^.code^.strings^;
  len:=BYTES(st); i:=0;
  WHILE i<len DO
    IF i+15 < len THEN k:=i+15 ELSE k:=len-1 END;
    h:=0; s:=0;
    FOR j:=i TO k DO
      IF ORD(st[j]) MOD 128 >= 40b THEN sym[s]:=st[j]
      ELSE sym[s]:='.'
      END;
      apph(hex,h,ORD(st[j]),2); INC(s);
    END;
    hex[h]:=0c; sym[s]:=0c;
    str.print(l,"%04h: %-48s : %s :",i DIV 4,hex,sym);
    wnd.append(m,l,{},{wnd._inv},0);
    i:=k+1;
  END;
  m^.read:=read_proc;
  IF app_ring(m,h_select,select_str,sys.ADR(setup_rec.str)) THEN RETURN END;
  WITH setup_rec.str DO wnd.place_menu(m,l0,c0,high,width,cols) END;
  m^.page:=16;
  wnd.fr_color(m^.w,wnd.blck,wnd.brck);
  wnd.w_color( m^.w,wnd.blck,wnd.gr);
  clear_line(24);
END ini_strings;

PROCEDURE Info;

  PROCEDURE dt(VAR date: ARRAY OF CHAR; t: INTEGER);
    VAR y,m,d,ho,mi,se: INTEGER;
  BEGIN
    time.unpack(t,y,m,d,ho,mi,se);
    str.print(date,'%02d:%02d.%02d  %02d/%02d/%02d',ho,mi,se,d,m,y);
  END dt;

  VAR date: str32; code: vis.code_ptr; i: INTEGER;
BEGIN
  IF cur=NIL THEN
    key.bell(3); message(OFF, 'Select module, please...'); RETURN
  END;
  norm_color;
  code:=cur^.code;
  IF code=NIL THEN RETURN END;
  dt(date,code^.imp_time);
  clear_scr;
  tty.print("MODULE %s (* %s *)\n\n",code^.name,date);
  tty.print('  from file "%s"\n\n',cur^.name);
  IF code^.def_time#code^.imp_time THEN
    dt(date,code^.def_time);
    tty.print("  definition compiled at: %s\n\n",date);
  END;
  WITH code^ DO
    tty.print("  compiler   : %-4.4s\n",compiler);
    tty.print("  # proc's   : %d\n",HIGH(proc_tab)+1);
    tty.print("  # glob's   : %d\n",no_glo-2);
    tty.print("  # ext's    : %d\n",HIGH(exts));
    tty.print("  # multi_glo: %d\n",HIGH(multi_glo)+1);
    tty.print("  strings    : %d words\n",SIZE(structs));
    tty.print("  code       : %d words\n",SIZE(code));
    tty.print("  version    : %h \n", vers);
    tty.print("  stack0     : %d words\n",min_stack);
    tty.print("  addstk     : %d words\n",add_stack);
    tty.print("  tag        : %08h\n",tag);
    tty.print("  total      : %d words\n",SIZE(cur^.buf));
  END;
  tty.print("\nEND %s.\n",code^.name);
  message(ON,''); clear_scr; refresh_scr;
END Info;

---------------------------- ini -------------------------------
                            -----
PROCEDURE ini_multi;
 VAR m   : wnd.menu_ptr;
     exit: BOOLEAN;
     name: str80;
     i   : INTEGER;
BEGIN
  IF cur=NIL THEN
    key.bell(3); message(OFF, 'Select module, please...'); RETURN
  END;
  IF cur^.code=NIL THEN RETURN END;
  IF HIGH(cur^.code^.multi_glo)<0 THEN
    message(OFF, 'no multi_globals'); RETURN
  END;
  wnd.new_menu(m);
  str.print(name,' %s MULTI_GLOBALs ',cur^.name);
  wnd.new_window(name,ON,m^.w);
  FOR i:=0 TO HIGH(cur^.code^.multi_glo) DO
    str.print(name, '[%$2h] ofs: %$4hh size: %$4hh',
                     i,    cur^.code^.multi_glo[i].ofs,
                           cur^.code^.multi_glo[i].size);
    wnd.append(m, name, {},{wnd._inv}, 0);
  END;
  m^.read:=read_proc;
  IF app_ring(m,h_select,select_menu,sys.ADR(setup_rec.multi)) THEN RETURN END;
  wnd.fr_color(m^.w, wnd.blck,wnd.wht);
  wnd.w_color( m^.w, wnd.blck,wnd.gr);
  WITH setup_rec.multi DO wnd.place_menu(m,l0,c0,high,width,cols) END;
END ini_multi;

PROCEDURE read_load(m: wnd.menu_ptr): CHAR;
 VAR ch: CHAR; s: str256;
BEGIN  ch:=read_proc(m);
  IF ch#'s' THEN RETURN ch END;
  read_str(' new directory: ', s);
  new_dir(s); read_done:=OFF; clear_line(24);
  RETURN 33c;
END read_load;

PROCEDURE ini_load;
 VAR f   : bio.FILE;     name, s  : str80;
     exit: BOOLEAN;      mode, low: BITSET;
     m   : wnd.menu_ptr;

  PROCEDURE get?(m: wnd.menu_ptr; VAL s: ARRAY OF CHAR): BOOLEAN;
   VAR i: INTEGER;
  BEGIN i:=0;
    WHILE i<m^.free DO
      IF m^.lines[i]^.line=s THEN RETURN FALSE END;
      INC(i);
    END;
    RETURN TRUE
  END get?;

  PROCEDURE file_ext(m: wnd.menu_ptr; VAR s: ARRAY OF CHAR): BOOLEAN;
    VAR i,l: INTEGER;
  BEGIN
    l:=str.len(s); i:=l-1;
    WHILE (i>=0) & (s[i]#'.') DO DEC(i) END;
    IF i>0 THEN
      IF ((s[i]='.')&(s[i+1]='c')&(s[i+2]='o')&(s[i+3]='d')) OR
         ((s[i]='.')&(s[i+1]='r')&(s[i+2]='e')&(s[i+3]='f')) THEN
         s[i]:=0c;
         RETURN get?(m, s);
      END;
    END;
    RETURN FALSE
  END file_ext;

BEGIN
  IF load? & fr_board THEN ring:=r_load; RETURN END;
  wnd.new_menu(m);  bio.fname(bio.cd, name);
  str.print(s, ' %s FILEs ', name); wnd.new_window(s,ON,m^.w);
  bio.dir_walk(bio.cd,bio.s_name+bio.s_dirfwd);
  IF NOT bio.done THEN bio_err('in "%s"', name); RETURN END;
  LOOP
    IF bio.get_entry(bio.cd, name, mode) THEN low:={};
      bio.open(f,name,'r');
      IF NOT bio.done THEN  low:={wnd._low} END;
      bio.close(f);
      IF mode*bio.e_dir#{} THEN str.app(name,'/');
        wnd.append(m, name, low,{wnd._inv}, mode);
      ELSIF file_ext(m,name) THEN wnd.append(m, name,low,{wnd._inv}, mode);
      END;
    ELSE
      IF NOT bio.done THEN bio_err('get_entry %s',name) END;
      bio.end_walk(bio.cd); EXIT
    END;
  END;
  m^.read:=read_load;
  IF load? & (m_load#NIL) THEN
    wnd.erase(m_load^.w); wnd.rem_window(m_load^.w); wnd.rem_menu(m_load)
  END;
  m_load:=m; m_load^.w:=m^.w; r_load^.m:=m_load;
  IF NOT load? THEN
    IF  app_ring(m,h_load,select_load,sys.ADR(setup_rec.load)) THEN RETURN END;
  END;
  load?:=ON;  r_load:=ring;
  IF m_load#NIL THEN
    wnd.fr_color(m_load^.w, wnd.blck,wnd.dgr);
    wnd.w_color( m_load^.w, wnd.blck,wnd.gr);
    WITH setup_rec.load DO wnd.place_menu(m_load,l0,c0,high,width,cols) END;
    wnd.refresh(m_load, OFF); load?:=ON;
  END;
END ini_load;

PROCEDURE original;
BEGIN
  WITH setup_rec DO
     WITH board     DO l0:=0; c0:= 0; high:= 1; width:=20; cols:=10 END;
     WITH ext       DO l0:=2; c0:=10; high:=12; width:=25; cols:=1  END;
     WITH proc      DO l0:=2; c0:= 0; high:=19; width:=30; cols:=1  END;
     WITH in_proc   DO l0:=0; c0:= 0; high:=22; width:=78; cols:=1  END;
     WITH in_proch  DO l0:=0; c0:= 0; high:= 7; width:=78; cols:=1  END;
     WITH head_proc DO l0:=0; c0:= 0; high:= 5; width:=78; cols:=1  END;
     WITH glob      DO l0:=2; c0:=40; high:=12; width:=30; cols:=1  END;
     WITH types     DO l0:=2; c0:=20; high:=12; width:=40; cols:=1  END;
     WITH const     DO l0:=2; c0:= 0; high:=20; width:=78; cols:=1  END;
     WITH str       DO l0:=2; c0:= 0; high:=20; width:=78; cols:=1  END;
     WITH multi     DO l0:=2; c0:=40; high:=20; width:=30; cols:=1  END;
     WITH load      DO l0:=2; c0:=45; high:=20; width:=32; cols:=1  END;
  END;
END original;

PROCEDURE ini_setup;
  VAR f: bio.FILE;
BEGIN
  bio.lookup(ETC,f,setup_file,'r');
  IF NOT bio.done THEN
--++    message(ON,"can't"' lookup file "%s"',setup_file);
    original; RETURN
  END;
  bio.read(f,sys.ADR(setup_rec),BYTES(setup_rec));
  IF NOT bio.done THEN
    message(ON,"can't"' read file "%s"',setup_file);
    original; RETURN
  END;
  bio.close(f);
END ini_setup;

PROCEDURE save;
  VAR f: bio.FILE; s: str64;
BEGIN
  IF original_cd#cd_name THEN bio.chdir(original_cd);
     IF NOT bio.done THEN bio_err(" can't save"); RETURN END;
  END;
  bio.create(f,setup_file,'wh',0);
  IF NOT bio.done THEN bio_err('in write file "%s" ',setup_file); RETURN
  END;
  bio.write(f,sys.ADR(setup_rec),BYTES(setup_rec));
  IF NOT bio.done THEN bio_err("can't"' write  file "%s"',setup_file);
    bio.close(f); RETURN
  END;
  bio.end(f,bio.pos(f));
  IF NOT bio.done THEN bio_err("can't"' set eof in "%s"',setup_file) END;
  bio.close(f);
  message(OFF, 'save DONE');
  IF original_cd#cd_name THEN bio.chdir(cd_name);
     IF NOT bio.done THEN bio_err(" can't return") END;
  END;
END save;

PROCEDURE SetUp;
 VAR ch: CHAR;
BEGIN
    message(OFF,'SAVE setup to file "VX.SETUP"?'); key.read(ch);
    IF (ch='y') OR (ch='Y') THEN save
    ELSE clear_line(24)
    END;
END SetUp;

---------------------------------------------------------------

PROCEDURE ini_board;
BEGIN
  wnd.new_menu(board); wnd.new_window('board',OFF,board^.w);
  wnd.append(board,' PROC ',  {},{wnd._inv, wnd._low},ini_proc_table);
  wnd.append(board,' EXT ',   {},{wnd._inv, wnd._low},ini_externals);
  wnd.append(board,' INFO ',  {},{wnd._inv, wnd._low},Info);
  wnd.append(board,' STR ',   {},{wnd._inv, wnd._low},ini_strings);
  wnd.append(board,' CONST ', {},{wnd._inv, wnd._low},Constants);
  wnd.append(board,' TYPEs ', {},{wnd._inv, wnd._low},typeTable);
  wnd.append(board,' GLOB ',  {},{wnd._inv, wnd._low},showGlob);
  wnd.append(board,' MULTI ', {},{wnd._inv, wnd._low},ini_multi);
  wnd.append(board,' FILEs ', {},{wnd._inv, wnd._low},ini_load);
  wnd.append(board,' SetUp ' ,{},{wnd._inv, wnd._low},SetUp);
  WITH setup_rec.board DO wnd.place_menu(board,l0,c0,high,width,cols) END;
  board^.ln:=0; board^.read:=read_proc;
  IF app_ring(board,h_board,select_board,sys.ADR(setup_rec.board)) THEN
    HALT(1)
  END;
  wnd.w_color(board^.w, wnd.blue, wnd.wht);
  r_board:=ring;
END ini_board;

PROCEDURE ini_all;
 VAR s: STRING;
BEGIN cur:=NIL; ring:=NIL; board:=NIL; m_load:=NIL;
  r_load:=NIL;
  sed.new(SED,8);
  ini_inproc_help;    ini_board_help;
  ini_proctable_help; ini_load_help;
  ini_move_help;      ini_1str_help;
  ini_select_help;    ini_setup;
  ini_board;
  load?:=OFF;         fr_board:=OFF;    refr?:=ON;
  bio.get_paths(BIN, env.bin);
  tsk.get_str(env.cd,s);
  original_cd:=s; cd_name:=s;
END ini_all;

---------------------------------------------------------------
PROCEDURE load_new(VAR s: ARRAY OF CHAR): BOOLEAN;
 VAR i, l: INTEGER;
BEGIN
    l:=str.len(s); i:=l-1;
    WHILE (i>=0) & (s[i]#'.') DO DEC(i) END;
    IF i>0 THEN s[i]:=0c END;
    RETURN new(s);
END load_new;
(*$U-*)
---------------------------------------------------------------

BEGIN
  IF args.flag('-','#') THEN
    tty.print('code visualizer vx 0.11 17-May-90.  Excelsior iV\n'); HALT
  END;
  IF args.flag('-','h') THEN
    tty.print('usage: "vx [module_name] [-rch#]"\n');
    tty.print('    CR for select in menu\n');
    tty.print('        F1 for  HELP\n');
    HALT
  END;
  bio.get_paths(BIN, env.bin);
  bio.get_paths(ETC, env.etc);
  IF NOT bio.done THEN BIN:=bio.here END;
  no_ref:=args.flag('-','r');
  IF args.flag('-','c') THEN wnd.rem_col END;
  ini_all; clear_scr; ring_no:=0;
  wnd.refresh(board, OFF);
  IF HIGH(args.words)<0 THEN ini_load
  ELSE module:=args.words[0];
       IF load_new(module) THEN HALT(1)
       ELSE ini_proc_table
       END;
  END;
  LOOP select_common; END;
END vx.
