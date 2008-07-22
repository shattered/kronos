IMPLEMENTATION MODULE bcSet; (*$X+ brd 26-Jan-91. (c) KRONOS *)

IMPORT  wnd: pmWnd;
IMPORT  bpm: bcPM;
IMPORT  obj: bcObj;
IMPORT  bas: bcBase;
IMPORT  dft: bcDraft;
IMPORT  str: Strings;
IMPORT  mth: realMath;
IMPORT  mem: Heap;
IMPORT  err: defErrors;
IMPORT  fnt: Fonts;
IMPORT  SYSTEM;

IMPORT  tty: Terminal;

VAR error: INTEGER;
     done: BOOLEAN;

---------------------------- Memory ----------------------------

PROCEDURE mem_error; BEGIN done:=FALSE; error:=mem.error    END mem_error;

PROCEDURE allocate(VAR a: SYSTEM.ADDRESS; size: INTEGER);
BEGIN
  mem.allocate(a,size); done:=mem.done;
  IF NOT done THEN mem_error END
END allocate;

PROCEDURE reallocate(VAR    a: SYSTEM.ADDRESS;
                     VAR high: INTEGER; len,bytesperelem: INTEGER);
BEGIN
  mem.reallocate(a,high,len,bytesperelem); done:=mem.done;
  IF NOT done THEN mem_error END
END reallocate;

WITH STORAGE (NEW    : allocate;
              DISPOSE: mem.deallocate;
              RESIZE : reallocate);

----------------------------------------------------------------

PROCEDURE no_mem;
BEGIN
  done:= TRUE; error:= err.ok;
  IF  bas.ENG THEN bpm.message(bas.SW DIV 2,bas.SH DIV 2,'No memory')
  ELSE             bpm.message(bas.SW DIV 2,bas.SH DIV 2,'нет памяти') END
END no_mem;

VAR maxNAME,maxSTATE: INTEGER;

PROCEDURE app_end(VAR s: ARRAY OF CHAR; l: INTEGER);
  VAR i,d: INTEGER;
        S: ARRAY [0..127] OF CHAR;
BEGIN
  d:=(l-wnd.lenght(bpm.font,s)) DIV wnd.lenght(bpm.font,' ');
  IF d > 0 THEN
    FOR i:=0 TO d DO S[i]:=' ' END;
    S[d+1]:=0c;
    str.append(s,S)
  ELSIF d < 0 THEN s[str.len(s)+d]:=0c END;
  d:=l-wnd.lenght(bpm.font,s);
  IF d > 0 THEN
    FOR i:=0 TO d DO S[i]:=37c END;
    S[d+1]:=0c;
    str.append(s,S)
  END
END app_end;

--------------------------- SET LAYER --------------------------

PROCEDURE STATE(st: INTEGER; VAR s: ARRAY OF CHAR);
BEGIN
  IF bas.ENG THEN
    CASE st OF
       0: str.print(s,'invisible')
      |1: str.print(s,'visible  ')
      |2: str.print(s,'  WORK   ')
    ELSE  s[0]:=0c END
  ELSE
    CASE st OF
       0: str.print(s,'невидимый')
      |1: str.print(s,'видимый  ')
      |2: str.print(s,'текущий  ')
    ELSE  s[0]:=0c END
  END
END STATE;

PROCEDURE state?(X,Y: INTEGER): INTEGER;
 VAR m_s: bpm.TABLET;
  state: INTEGER;
BEGIN
  IF bas.ENG THEN
    bpm.tnew(m_s,1,3,X,Y-60,75,bpm.font^.H+5,bpm.xsel,'State');
    bpm.tprint(m_s,0,'Invistible');  bpm.thotkey(m_s,0,"i",TRUE);
    bpm.tprint(m_s,1,'  Visible ');  bpm.thotkey(m_s,1,"v",TRUE);
    bpm.tprint(m_s,2,'  WORK   ');   bpm.thotkey(m_s,2,"w",TRUE)
  ELSE
    bpm.tnew(m_s,1,3,X,Y-60,85,bpm.font^.H+5,bpm.xsel,'Статус');
    bpm.tprint(m_s,0,'Невидимый');  bpm.thotkey(m_s,0,"н",TRUE);
    bpm.tprint(m_s,1,' Видимый ');  bpm.thotkey(m_s,1,"в",TRUE);
    bpm.tprint(m_s,2,' Текущий ');  bpm.thotkey(m_s,2,"т",TRUE)
  END;
  bpm.topen(m_s);
  bpm.tselect(m_s);
  IF NOT bpm.tselected(m_s) THEN bpm.tdispose(m_s); RETURN 1 END;
  state:=bpm.talt(m_s);
  bpm.tdispose(m_s);
  RETURN state
END state?;

PROCEDURE name?(VAR name: ARRAY OF CHAR; X,Y: INTEGER);
BEGIN
  IF bas.ENG THEN bpm.diabox(X,Y,120,name,'Name layer:')
  ELSE            bpm.diabox(X,Y,120,name,'Имя слоя:') END
END name?;

VAR sl: bpm.TEXT;

PROCEDURE mod_sl;
  VAR s: ARRAY [0..127] OF CHAR;
      i: INTEGER;
BEGIN
  s[0]:=0c;
  WITH bas.cview^ DO
    NEW(sl,LEN(model^.mbody^)+1);
    FOR i:=0 TO HIGH(sl)-1 DO
      NEW(sl[i],128);
      sl[i][0]:=0c;
      str.append(s,'%s%c',model^.mbody^[i]^.name,0c);
      app_end(s,maxNAME);
      str.append(sl[i],'%s',s); s[0]:=0c;

      STATE(cntx^.mask[i],s);
      app_end(s,maxSTATE);
      str.append(sl[i],'%s',s); s[0]:=0c
    END
  END;
  NEW(sl[HIGH(sl)],32);
  IF bas.ENG THEN str.print(sl[HIGH(sl)],'       Create new layer')
  ELSE            str.print(sl[HIGH(sl)],'       Завести новый слой') END
END mod_sl;

PROCEDURE chk_name(s: ARRAY OF CHAR): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(bas.cview^.model^.mbody^) DO
    IF bas.cview^.model^.mbody^[i]^.name=s THEN
      IF bas.ENG THEN bpm.message(240,180,'This name allready exist !');
      ELSE            bpm.message(240,180,'Такое имя уже есть!') END;
      RETURN FALSE
    END
  END;
  RETURN TRUE
END chk_name;

PROCEDURE mod_lay(VAR lay: INTEGER; X,Y: INTEGER);
  VAR  s: ARRAY [0..11] OF CHAR;
      ml: bpm.TABLET;

PROCEDURE _print;
BEGIN
  WITH bas.cview^ DO
    bpm.tundisable(ml,1);
    IF bas.ENG THEN
      bpm.tprint(ml,0,'Name  layer     %s ',model^.mbody^[lay]^.name);
      bpm.thotkey(ml,0,"n",TRUE);
      STATE(cntx^.mask[lay],s);
      bpm.tprint(ml,1,'State layer  %s  ',s);      bpm.thotkey(ml,1,"s",TRUE)
    ELSE
      bpm.tprint(ml,0,'Имя слоя      %s ',model^.mbody^[lay]^.name);
      bpm.thotkey(ml,0,"и",TRUE);
      STATE(cntx^.mask[lay],s);
      bpm.tprint(ml,1,'Статус слоя   %s ',s);      bpm.thotkey(ml,1,"с",TRUE)
    END;
    IF lay= cntx^.work THEN bpm.tdisable(ml,1) END;
  END
END _print;

  VAR  new_name: ARRAY [0..15] OF CHAR;
BEGIN
  IF bas.cview^.model^.mbody^[lay] = NIL THEN RETURN END;
  WITH bas.cview^ DO
    IF bas.ENG THEN
      bpm.tnew(ml,1,2,X,Y-45,200,bpm.font^.H+6,bpm.xsel,'Edit layer :')
    ELSE
      bpm.tnew(ml,1,2,X,Y-45,200,bpm.font^.H+6,bpm.xsel,'Параметры слоя:')
    END;
    _print;
    bpm.topen(ml);
    LOOP
      bpm.tselect(ml);
      IF NOT bpm.tselected(ml) THEN bpm.tdispose(ml); EXIT END;
      CASE bpm.talt(ml) OF
         0:  name?(new_name,X+60,Y-24);
             IF chk_name(new_name) THEN
               model^.mbody^[lay]^.name:=new_name; _print
             END
        |1:  WITH cntx^ DO
               mask[lay]:= state?(X+12,Y-12);
               IF mask[lay]=2 THEN mask[work]:=1; work:=lay END;
             END;
             _print
      ELSE END;
      bpm.tunselect(ml)
    END
  END
END mod_lay;

PROCEDURE new_lay(X,Y: INTEGER);
  VAR s: ARRAY [0..15] OF CHAR;
      n: INTEGER;
BEGIN
  name?(s,X,Y);
  IF (s[0]=0c) OR NOT chk_name(s) THEN RETURN END;
  WITH bas.cview^.cntx^ DO
    RESIZE(mask,HIGH(mask)+2);
    IF NOT done THEN no_mem; RETURN END;
    mask[HIGH(mask)]:=1
  END;
  obj.new_layer(bas.cview^.model^.mbody);
  IF NOT obj.done THEN no_mem;
    RESIZE(bas.cview^.cntx^.mask,HIGH(bas.cview^.cntx^.mask));
    no_mem; RETURN
  END;
  WITH bas.cview^.model^ DO mbody^[HIGH(mbody^)]^.name:=s END;
END new_lay;

PROCEDURE layers(X,Y: INTEGER);
  VAR r_l: bpm.ROLLER;
  --     wn: wnd.WINDOW;
        n: INTEGER;
BEGIN
  IF bas.ENG THEN bpm.rnew(r_l,X,Y-180,225,140,{2},'     Layer')
  ELSE            bpm.rnew(r_l,X,Y-180,225,140,{2},'     Слои') END;
--  bpm.rmove(r_l,X - wn^.w DIV 2,Y- wn^.h DIV 2);
  mod_sl; bpm.rsettext(r_l, sl, 0,0);
  bpm.rchoose(r_l,bas.cview^.cntx^.work);
  bpm.ropen(r_l);
  LOOP
    bpm.rselect(r_l);
    IF NOT bpm.rselected(r_l) THEN bpm.rdispose(r_l); RETURN  END;
    n:= bpm.ralt(r_l);
    IF n= HIGH(sl) THEN
      new_lay(X+15,Y-10); mod_sl; bpm.rsettext(r_l,sl,0,0)
    ELSE
      mod_lay(n,X+30,Y-45); mod_sl; bpm.rsettext(r_l,sl,0,0)
    END
  END
END layers;

---------------------------- SCREEN ----------------------------

PROCEDURE st?(VAR S: REAL; prmt: ARRAY OF CHAR; x,y: INTEGER): BOOLEAN;
  VAR s: ARRAY [0..7] OF CHAR;
     dn: BOOLEAN; p: INTEGER;
BEGIN
  s:='x x'; p:=0; bpm.diabox(x,y,150,s,prmt);
  IF s='x x' THEN  RETURN FALSE END;
  str.rscan(S,s,p,dn);
  RETURN dn
END st?;

VAR by_grid: BOOLEAN;

PROCEDURE grid(X,Y: INTEGER);
 VAR stx,sty: REAL;
     gof,gon: ARRAY [0..15] OF CHAR;
          wn: wnd.WINDOW;
           s: ARRAY [0..7] OF CHAR;
          cg: CHAR;
           m: bpm.TABLET;
BEGIN
  WITH bas.cview^.cntx^ DO
    stx:= gstep_x;
    sty:= gstep_y;

    IF bas.ENG THEN
      s:='step';  cg:='g';
      gon:= 'Grid on     ';  gof:= 'Grid off    ';
      bpm.tnew(m,1,3,X,Y-60,100,bpm.font^.H+5,bpm.xsel,'   Grid');
    ELSE
      s:='шаг по'; cg:='с';
      gon:= 'Сетка вкл.     ';  gof:= 'Сетка выкл.';
      bpm.tnew(m,1,3,X,Y-60,100,bpm.font^.H+5,bpm.xsel,' Сетка');
    END;
    bpm.twindow(m,wn);
    bpm.tmove(m,X - wn^.w DIV 2,Y- wn^.h DIV 2);
    IF GRID THEN bpm.tprint(m,0,gon)
    ELSE         bpm.tprint(m,0,gof) END;  bpm.thotkey(m,0, cg,TRUE);
    bpm.tprint(m,1,'%s Y %f',s,gstep_y);   bpm.thotkey(m,0,"y",TRUE);
    bpm.tprint(m,2,'%s X %f',s,gstep_x);   bpm.thotkey(m,1,"x",TRUE);
    bpm.topen(m);
    LOOP
      bpm.tselect(m);
      IF NOT bpm.tselected(m) THEN EXIT END;
      CASE bpm.talt(m) OF
         0: IF GRID THEN bas.erase_grid(bas.cview); bpm.tprint(m,0,gof)
            ELSE         bas.show_grid(bas.cview);  bpm.tprint(m,0,gon) END;
            GRID:= NOT GRID
        |1: IF st?(sty,s,X+20,Y-30) THEN
              bpm.tprint(m,1,'%s Y %f',s,sty);
            END
        |2: IF st?(stx,s,X+20,Y-40) THEN
              bpm.tprint(m,2,'%s X %f',s,stx)
            END
      ELSE END;
      bpm.tunselect(m)
    END;
    IF GRID & ((stx#gstep_x) OR (sty#gstep_y)) THEN
      bpm.tclose(m);  bas.erase_grid(bas.cview);
      gstep_x:=stx; gstep_y:=sty;
      bas.show_grid(bas.cview); bpm.topen(m)
    END;
    gstep_x:=stx; gstep_y:=sty;
    IF by_grid THEN cstep_x:=stx; cstep_y:=sty; bas.step END;
    bpm.tdispose(m)
  END
END grid;

PROCEDURE step(X,Y: INTEGER);
 VAR sof,son,sg: ARRAY [0..15] OF CHAR;
          cs,cg: CHAR;
             wn: wnd.WINDOW;
              s: ARRAY [0..7] OF CHAR;
              m: bpm.TABLET;
BEGIN
  IF bas.ENG THEN
    s:='step';  cg:='g'; cs:='s';
    son:= 'Step on     ';  sof:= 'Step off    ';
    sg:='step by Grid';
    bpm.tnew(m,1,4,X,Y-70,100,bpm.font^.H+5,bpm.xsel,'   Step');
  ELSE
    s:='шаг по'; cg:='г'; cs:='ш';
    sg:='шаг по Сетке';
    son:= 'Шаг вкл.     ';  sof:= 'Шаг выкл.';
    bpm.tnew(m,1,3,X,Y-70,100,bpm.font^.H+5,bpm.xsel,' Шаг ');
  END;
  bpm.twindow(m,wn);
  bpm.tmove(m,X - wn^.w DIV 2,Y- wn^.h DIV 2);
  WITH bas.cview^.cntx^ DO
    IF STEP THEN bpm.tprint(m,0,son)
    ELSE         bpm.tprint(m,0,sof) END;  bpm.thotkey(m,0,cs ,TRUE);
    bpm.tprint(m,1,'%s Y %f',s,cstep_y);   bpm.thotkey(m,1,"y",TRUE);
    bpm.tprint(m,2,'%s X %f',s,cstep_x);   bpm.thotkey(m,2,"x",TRUE);
--    bpm.tprint(m,3,'%s    ',sg);               bpm.thotkey(m,3,cg ,TRUE);
    bpm.topen(m);
    LOOP
      bpm.tselect(m);
      IF NOT bpm.tselected(m) THEN EXIT END;
      CASE bpm.talt(m) OF
         0: STEP:= NOT STEP; bas.step;
            IF STEP THEN bpm.tprint(m,0,son)
            ELSE         bpm.tprint(m,0,sof) END
        |1: IF st?(cstep_y,s,X+20,Y-30) THEN
              bpm.tprint(m,1,'%s Y %f',s,cstep_y);
              bpm.tprint(m,3,'%s   ',sg);
              by_grid:=FALSE;
              bas.step
            END
        |2: IF st?(cstep_x,s,X+20,Y-40) THEN
              bpm.tprint(m,2,'%s X %f',s,cstep_x);
              bpm.tprint(m,3,'%s   ',sg);
              by_grid:=FALSE;
              bas.step
            END
     --   |3: bas.cstep_x:= bas.gstep_x; bpm.tprint(m,1,'%s X %f',s,bas.cstep_x);
--            bas.cstep_y:= bas.gstep_y; bpm.tprint(m,2,'%s Y %f',s,bas.cstep_y);
--            bpm.tprint(m,3,'%s *',sg);
--            by_grid:=TRUE; bas.step
      ELSE END;
      bpm.tunselect(m)
    END;
    bpm.tdispose(m)
  END
END step;

VAR LTYP: bpm.TABLET;

PROCEDURE type_line(X,Y: INTEGER);
 VAR wn: wnd.WINDOW;

BEGIN
  bpm.tchoose(LTYP,dft.c_ltype);
  bpm.twindow(LTYP,wn);
  bpm.tmove(LTYP,X - wn^.w DIV 2,Y- wn^.h DIV 2);
  bpm.topen(LTYP);
  bpm.tselect(LTYP);
  bpm.tclose(LTYP);
  dft.c_ltype:= bpm.talt(LTYP)
END type_line;

PROCEDURE dimension(X,Y: INTEGER);
BEGIN
END dimension;

VAR TCOL: bpm.TABLET;

PROCEDURE color?(VAR c: INTEGER; X,Y: INTEGER);
 VAR wn: wnd.WINDOW;
BEGIN
  bpm.tchoose(TCOL,c-1);
  bpm.twindow(TCOL,wn);
  bpm.tmove(TCOL,X - wn^.w DIV 2,Y- wn^.h DIV 2);
  bpm.topen(TCOL);
  bpm.tselect(TCOL);
  IF bpm.tselected(TCOL) THEN  c:=bpm.talt(TCOL)+1 END;
  bpm.tclose(TCOL)
END color?;

PROCEDURE color(x,y: INTEGER); BEGIN color?(dft.c_color,x,y); END color;

----------------------------- model ----------------------------

VAR rmod: bpm.ROLLER;
    tmod: bpm.TEXT;

PROCEDURE select_model(x,y: INTEGER; VAR mod: bas.VMODEL);
  VAR wind :wnd.WINDOW;
          i: INTEGER;
BEGIN
  IF HIGH(tmod)#HIGH(bas.models) THEN
    RESIZE(tmod,HIGH(bas.models)+1);
    FOR i:=0 TO HIGH(tmod) DO
      NEW(tmod[i],32)
    END
  END;
  FOR i:=0 TO HIGH(tmod) DO
    str.print(tmod[i],bas.models[i]^.name)
  END;
  bpm.rwindow(rmod,wind);
  wnd.move(wind,x-wind^.w DIV 2,y-wind^.h DIV 2);
  bpm.rsettext(rmod,tmod,0,0);
  bpm.rchoose(rmod,-1);
  bpm.rselect(rmod);
  IF bpm.rselected(rmod) THEN
    mod:=bas.models[bpm.ralt(rmod)];
  END
END select_model;

----------------------------- INIT -----------------------------

PROCEDURE ini_men;
VAR  b: wnd.BLOCK;
     W: wnd.WINDOW;
     T: wnd.TOOL;
     i: INTEGER;

PROCEDURE rect(b: wnd.BLOCK);
BEGIN WITH b DO wnd.rect(W,T,x,y,x+w,y+h) END END rect;

BEGIN

----------------------------- COLOR ----------------------------

  bpm.tnew(TCOL,7,1,50,50,28,28,bpm.xsel,'   Color');
  bpm.twindow(TCOL,W);
  T:= W^.inner;
  T.mode:=wnd.rep;
  T.mask:={0..3};
  FOR i:=0 TO 6 DO
    T.color:= BITSET(i+1);
    bpm.tblocks(TCOL,i,b);
    WITH b DO INC(x); INC(y); DEC(w,2); DEC(h,2) END;
    rect(b)
  END;
   T.color:=BITSET(bas.white_l);
   rect(b);

--------------------------- LINE TYPE --------------------------

  bpm.tnew(LTYP,5,1,50,50,28,28,bpm.xsel,' Type line');
  bpm.twindow(LTYP,W);
  T:= W^.inner;
  T.color:=bpm.black;
  T.back:= bpm.normal;
  T.mode:=wnd.rep;
  T.mask:={0..3};
  FOR i:=0 TO 4 DO
    bpm.tblocks(LTYP,i,b);
    T.clip:=b;
    wnd.print(W,T,b.x+1,b.y+1,bpm.ufont,'%c',CHAR(16+i))
  END;

---------------------------- MODELS ----------------------------

   bpm.rnew(rmod,0,0,100,90,{},'Models');
   NEW(tmod,0);

END ini_men;


PROCEDURE init;
BEGIN
  fnt.read(bpm.ufont,'bcSpec.fnt'); ASSERT(fnt.done);
  fnt.unpack(bpm.ufont);            ASSERT(fnt.done);

  maxNAME := wnd.lenght(bpm.font,'ЩЩЩЩЩЩЩЩЩЩЩЩЩЩЩЩ_');
  maxSTATE:= wnd.lenght(bpm.font,'невидимый_');
  ini_men;
END init;

PROCEDURE view(x,y: INTEGER);
BEGIN
END view;

BEGIN
 init
END bcSet.
