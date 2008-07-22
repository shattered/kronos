MODULE lr; (* 06-Feb-90. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT   tm: Time;
IMPORT  key: Keyboard;
IMPORT  rnd: Random;
IMPORT  bio: BIO;
IMPORT args: tskArgs;
IMPORT   vg: videoGames;

IMPORT  Heap;

WITH STORAGE: Heap;

CONST _fier=1; _new=2; _box=3; _climb=4;

TYPE DIRS = (empty,up,dw,lf,rg,fall,fier_lf,fier_rg);
     ds   = SET OF DIRS;

     PLAYER =
     RECORD
       mn  : INTEGER;
       l,c : INTEGER;
       dir : DIRS;
       new : DIRS;
       time: INTEGER;
       mode: BITSET;
       take: BOOLEAN;
     END;

     FIER_REC =
     RECORD
       l,c : INTEGER;
       time: INTEGER;
     END;

CONST free=0; wall=1; block=2; steps=3; pole=4; hole=5; exit=6; case=7;
      _man=9; _gad=8; fier=17;

CONST X0 = 70; Y0 = 220;

VAR field: ARRAY [0..25] OF ARRAY [0..15] OF INTEGER;

VAR size_X,size_Y: INTEGER;

VAR man: PLAYER;
    gad: ARRAY [0..5] OF PLAYER;
    gad_no : INTEGER;
    fiered : ARRAY [0..15] OF FIER_REC;
    fier_no: INTEGER;
    news   : ARRAY [0..5] OF PLAYER;
    new_no : INTEGER;

    mans   : INTEGER;
    boxes  : INTEGER;
    first  : BOOLEAN;

    FONT   : ARRAY CHAR OF vg.sprite;

VAR slow: BOOLEAN;


PROCEDURE sp_write(x,y: INTEGER; ch: CHAR);
BEGIN
  vg.show_sprite(x,y,FONT[ch]);
END sp_write;

--------------------------  SCREEN  ----------------------------
                          ----------

PROCEDURE sh_f(l,c: INTEGER; on: BOOLEAN);
  VAR x,y: INTEGER; f: INTEGER;
BEGIN
  x:=X0 + l*size_X;
  y:=Y0 - c*size_Y;
  f:=field[l,c];
  IF on THEN vg.mode:=vg.or ELSE vg.mode:=vg.bic END;
  CASE f OF
  | free :
  | wall : vg.layer(1); sp_write(x,y,14c);
  | block: vg.layer(1); sp_write(x,y,15c);
  | steps: vg.layer(0); sp_write(x,y,16c);
  | pole : vg.layer(0); sp_write(x,y,17c);
  | hole : vg.layer(1); sp_write(x,y,14c);
  | case : vg.layer(0); sp_write(x,y,34c);
           vg.layer(1); sp_write(x,y,35c);
  ELSE
  END;
END sh_f;

PROCEDURE show_man(VAL m: PLAYER; on: BOOLEAN);
  VAR f,ch,x,y,ln,cl,ln1,cl1: INTEGER; L: INTEGER;
BEGIN
  IF slow & NOT on THEN tm.delay(2,tm.tick) END;
  IF on THEN vg.mode:=vg.or ELSE vg.mode:=vg.bic END;
  WITH m DO
    cl:=c DIV 5;
    ln:=l DIV 6;
    y:=Y0 - cl*size_Y - (c MOD 5)*2;
    x:=X0 + ln*size_X + (l MOD 6)*4;
    f:=field[(l+3) DIV 6,(c+3) DIV 5];
    L:=l MOD 6;
    --vg.bmd:=vg.layer[2];
    vg.layer(1);
    CASE dir OF
    |lf    : DEC(x,(l MOD 2)*2);
             ch:=6;
             IF f=pole THEN INC(ch,20b) END;
             sp_write(x,y,CHAR(ch + L))
    |rg    : INC(x,(l MOD 2)*2);
             ch:=0;
             IF f=pole THEN INC(ch,20b) END;
             sp_write(x,y,CHAR(ch + L))
    |up,dw : sp_write(x,y,CHAR(40b + (c MOD 5)))
    |fall  : sp_write(x,y,45c);
    ELSE
    END;
    IF mn=_gad THEN
    --vg.bmd:=vg.layer[3];
      vg.layer(0);
      CASE dir OF
      |lf    : ch:=66b;
               IF f=pole THEN INC(ch,20b) END;
               sp_write(x,y,CHAR(ch + L))
      |rg    : ch:=60b;
               IF f=pole THEN INC(ch,20b) END;
               sp_write(x,y,CHAR(ch + L))
      |up,dw : sp_write(x,y,CHAR(120b + (c MOD 5)))
      |fall  : sp_write(x,y,125c)
      ELSE
      END;
      IF field[ln,cl] IN {pole,steps,case} THEN sh_f(ln,cl,TRUE) END;
      cl1:=(c+4) DIV 5;
      ln1:=(l+5) DIV 6;
      IF (ln1#ln) OR (cl1#cl) THEN
        IF field[ln1,cl1] IN {pole,steps,case} THEN sh_f(ln1,cl1,TRUE) END
      END;
      RETURN
    END;
    IF field[ln,cl]=case THEN sh_f(ln,cl,TRUE) END;
    cl1:=(c+4) DIV 5;
    ln1:=(l+5) DIV 6;
    IF (ln1#ln) OR (cl1#cl) THEN
      IF field[ln1,cl1]=case THEN sh_f(ln1,cl1,TRUE) END
    END
  END
END show_man;

PROCEDURE sh_fire(VAL m: PLAYER; on: BOOLEAN);
  CONST rg_2 = ARRAY OF CHAR{47c,50c,51c,52c};
        lf_2 = ARRAY OF CHAR{53c,54c,55c,56c};
        _4   = ARRAY OF CHAR{116c,117c};
        _all = ARRAY OF CHAR{140c,160c, 141c,161c, 142c,162c,
                             143c,163c, 144c,164c, 145c,165c};

  VAR ch: INTEGER; x,y: INTEGER;
BEGIN
  IF on THEN vg.mode:=vg.or ELSE vg.mode:=vg.bic END;
  WITH m DO
    ASSERT(dir IN ds{fier_lf,fier_rg});
    y:=Y0 - (c DIV 5)*size_Y - (c MOD 5)*2;
    x:=X0 + (l DIV 6)*size_X + (l MOD 6)*4;
    IF dir=fier_rg THEN
    --vg.bmd:=vg.layer[2]; sp_write(x,y,0c); -- man
      vg.layer(1); sp_write(x,y,0c); -- man
      IF     time<2 THEN
        vg.layer(0); sp_write(x+size_X,y,rg_2[time*2  ]);
        vg.layer(1); sp_write(x+size_X,y,rg_2[time*2+1]);
      ELSIF  time<4 THEN
        vg.layer(1); sp_write(x+size_X,y,_4[time-2])
      END;
      ASSERT(time<6);
      vg.layer(0); sp_write(x+size_X,y-size_Y,_all[time*2  ]);
      vg.layer(1); sp_write(x+size_X,y-size_Y,_all[time*2+1])
    END;
    IF dir=fier_lf THEN
    --vg.bmd:=vg.layer[2]; sp_write(x,y,20c); -- man
      vg.layer(1); sp_write(x,y,20c); -- man
      IF     time<2 THEN
        vg.layer(0); sp_write(x-size_X,y,lf_2[time*2  ]);
        vg.layer(1); sp_write(x-size_X,y,lf_2[time*2+1])
      ELSIF  time<4 THEN
        vg.layer(1); sp_write(x-size_X,y,_4[time-2])
      END;
      ASSERT(time<6);
      vg.layer(0); sp_write(x-size_X,y-size_Y,_all[time*2  ]);
      vg.layer(1); sp_write(x-size_X,y-size_Y,_all[time*2+1])
    END
  END
END sh_fire;

PROCEDURE sh_fier(l,c: INTEGER; time: INTEGER);
  CONST fi = ARRAY OF CHAR{14c,74c,75c};
  VAR x,y: INTEGER;
BEGIN
  x:=X0 + l*size_X;
  y:=Y0 - c*size_Y;
  vg.mode:=vg.or;
  vg.layer(1);
  sp_write(x,y,fi[time]);
END sh_fier;

PROCEDURE sh_new(l,c: INTEGER; time: INTEGER);
  CONST ksi = ARRAY OF CHAR{77c,77c,76c};
  VAR x,y: INTEGER;
BEGIN
  x:=X0 + l*size_X;
  y:=Y0 - c*size_Y;
  time:=time MOD 3;
  IF time#0 THEN vg.mode:=vg.or ELSE vg.mode:=vg.bic END;
--vg.layer(2); sp_write(x,y,ksi[time]);
  vg.layer(1); sp_write(x,y,ksi[time]);
--vg.layer(3); sp_write(x,y,ksi[time])
  vg.layer(1); sp_write(x,y,ksi[time])
END sh_new;

PROCEDURE showfield(level: INTEGER);

  VAR l,c,i: INTEGER;

  PROCEDURE line(c: INTEGER);
    VAR l,j: INTEGER;
  BEGIN
    FOR l:=0 TO HIGH(field) DO sh_f(l,c,TRUE) END;
    FOR j:=0 TO 300 DO END;
  END line;

  CONST MIDDLE = HIGH(field[0]) DIV 2;

BEGIN
  FOR l:=0 TO 1 DO vg.layer(l); vg.erase END;
  vg.layer(1);
  vg.print(X0 + 13*size_X - 60,Y0+10+size_Y,"У Р О В Е Н Ь   %d",level+1);
  vg.layer(0);
  vg.print(X0 + 13*size_X - 60,Y0+10+size_Y,"                %d",level+1);
  vg.layer(1);
  vg.frame(X0-1, Y0+1+size_Y, X0 + 26*size_X     , Y0-15*size_Y-1);
  vg.frame(X0-4, Y0+4+size_Y, X0 + 26*size_X + 3 , Y0-15*size_Y-4);
  FOR i:=0 TO MIDDLE DO
    line(MIDDLE - i);  line(MIDDLE + i + 1)
  END;
  show_man(man,TRUE);
  FOR l:=0 TO gad_no-1 DO show_man(gad[l],TRUE) END
END showfield;

PROCEDURE make_font;

  CONST TITLE = "СОБЕРИ ЧЕМОДАНЧИКИ";

  VAR i: INTEGER;
    buf: STRING;
    ptr: SYSTEM.ADDRESS;
    W,H: INTEGER;
    j,k: INTEGER;  s,x,xx: BITSET;
      f: bio.FILE;

BEGIN
  bio.open(f,'lr.bmf','r');
  IF NOT bio.done THEN HALT(1) END;
  NEW(buf,bio.eof(f));
  bio.get(f,buf,BYTES(buf));
  IF NOT bio.done THEN HALT(1) END;
  bio.close(f);
  FOR i:=0 TO 255 DO FONT[CHAR(i)].w:=0; FONT[CHAR(i)].h:=0 END;
  ptr:=buf.ADR;
  W:=ptr^;  INC(ptr);
  H:=ptr^;  INC(ptr);
            INC(ptr);
  vg.mode:=vg.xor;
  FOR i:=0 TO 200b DO
    j:=(i+tm.sys_time(tm.tick)+rnd.next() MOD 100) MOD HIGH(TITLE);
    vg.layer(tm.sys_time(tm.tick) MOD 2);
    vg.print(400+(j-HIGH(TITLE) DIV 2)*20,135,"%c",TITLE[j]);
    WITH FONT[CHAR(i)] DO
      w:=W*2; h:=H;
      FOR j:=0 TO H-1 DO
        s:={}; xx:={0..1}; x:=BITSET(ptr^);
        FOR k:=0 TO W-1 DO
          IF x*{0}#{} THEN s:=s+xx END; x:=x>>1; xx:=xx<<2
        END;
        body[j]:=s; INC(ptr)
      END
    END
  END;
  DISPOSE(buf);
  size_X:=W*2;
  size_Y:=H
END make_font;

PROCEDURE Init;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO 1 DO vg.layer(i); vg.erase END;
  make_font;
  FOR i:=0 TO 1 DO vg.layer(i); vg.erase END;
END Init;

PROCEDURE InitField(level: INTEGER);
  CONST size=200h;
  VAR buf: ARRAY [0..size-1] OF CHAR;
      b,l,c,d: INTEGER;
        f: bio.FILE;
BEGIN
  bio.open(f,'lr.dat','r');  IF NOT bio.done THEN HALT(1) END;
  bio.seek(f,level*size,0);  IF NOT bio.done THEN HALT(1) END;
  bio.get(f,buf,size);       IF NOT bio.done THEN HALT(1) END;
  bio.close(f);
  d:=0;
  FOR l:=0 TO HIGH(field) DO
    FOR c:=0 TO HIGH(field[l]) DO
      field[l,c]:=INTEGER(buf[d]); INC(d);
      IF field[l,c]>7 THEN field[l,c]:=free END;
    END;
  END;
  INC(d);   boxes:=INTEGER(buf[d]);
  INC(d,2); gad_no:=INTEGER(buf[d]);
  man.mn:=_man;
  INC(d);   man.l :=6*INTEGER(buf[d]);
  INC(d);   man.c :=5*INTEGER(buf[d]);
  man.dir:=rg;
  man.new:=empty;
  man.mode:={};
  FOR l:=0 TO gad_no-1 DO
    WITH gad[l] DO
      mn:=_gad;
      INC(d); l:=6*INTEGER(buf[d]);
      INC(d); c:=5*INTEGER(buf[d]);
      dir:=lf;   new:=empty;
      mode:={};  take:=FALSE;
    END;
  END;
  showfield(level);
  first:=TRUE;
  fier_no:=0;
END InitField;

--------------------------  TIMER  -----------------------------
                          ---------

PROCEDURE wait(ms: INTEGER);
BEGIN
  tm.delay(ms,tm.milisec);
END wait;

--------------------------  FIERS  -----------------------------
                          ---------

PROCEDURE add_fier(L,C: INTEGER);
  CONST fier_time=100;
BEGIN
  ASSERT(fier_no<=HIGH(fiered));
  WITH fiered[fier_no] DO
    l:=L; c:=C; time:=fier_time;
    sh_f(L,C,FALSE);
    field[l,c]:=fier;
  END;
  INC(fier_no);
END add_fier;

PROCEDURE remove_fier(no: INTEGER): BOOLEAN;

  PROCEDURE new(VAR g: PLAYER);
    CONST new_time=20;

    VAR xx: ARRAY [0..25] OF INTEGER;
        i,j,x: INTEGER;
  BEGIN
    WITH g DO
      INCL(mode,_new);
      x:=0; j:=0;
      REPEAT
        FOR i:=0 TO HIGH(field) DO
          IF field[i,j]=free THEN xx[x]:=i; INC(x) END;
        END;
        INC(j);
      UNTIL x>0;
      c:=(j-1)*5; l:=xx[rnd.next() MOD x]*6;
      time:=new_time;
    END;
  END new;

  VAR j,L,C: INTEGER;
BEGIN
  ASSERT(no<fier_no);
  WITH fiered[no] DO
    ASSERT(field[l,c]=fier);
    field[l,c]:=wall;
    sh_f(l,c,TRUE);
    L:=(man.l+3) DIV 6;
    C:=(man.c+3) DIV 5;
    IF (L=l) & (c=C) THEN RETURN TRUE END;
    FOR j:=0 TO gad_no-1 DO
      L:=(gad[j].l+3) DIV 6;
      C:=(gad[j].c+3) DIV 5;
      IF (l=L) & (c=C) THEN
        show_man(gad[j],FALSE);
        sh_f(l,c,TRUE);
        new(gad[j])
      END
    END
  END;
  FOR j:=no TO fier_no-2 DO fiered[j]:=fiered[j+1] END;
  DEC(fier_no);
  RETURN FALSE
END remove_fier;

--------------------------  MOVING  ----------------------------
                          ----------
PROCEDURE set(VAR m: PLAYER; d: DIRS; l,c: INTEGER);
BEGIN
  show_man(m,FALSE);
  IF (c>=0) & (c<16*5) THEN m.c:=c END;
  IF (l>=0) & (l<26*6) THEN m.l:=l END;
  m.dir:=d;
  show_man(m,TRUE);
END set;

PROCEDURE gads?(L,C: INTEGER): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO gad_no-1 DO
    WITH gad[i] DO
      IF (dir=fall) & ((l+3) DIV 6=L) & ((c+3) DIV 5=C) THEN RETURN TRUE END
    END
  END;
  RETURN FALSE
END gads?;

PROCEDURE ?(new: DIRS; L,C: INTEGER): DIRS;
  VAR f,d,n: INTEGER; i: INTEGER;
BEGIN
  f:=field[L,C];
  IF C<HIGH(field[0]) THEN d:=field[L,C+1] ELSE d:=block END;
  IF gads?(L,C+1) THEN d:=block END;
  IF NOT (d IN {block,wall,steps}) & (
     (f=pole) & (new=dw)             OR
     (f IN {free,case,fier,exit})     )
  THEN
    RETURN fall
  END;
  CASE new OF
  |lf  : IF L>0 THEN n:=field[L-1,C] ELSE n:=block END;
         IF NOT (n IN {block,wall,hole}) THEN RETURN lf END
  |rg  : IF L<HIGH(field) THEN n:=field[L+1,C] ELSE n:=block END;
         IF NOT (n IN {block,wall,hole}) THEN RETURN rg END
  |dw  : IF NOT (d IN {wall,block})      THEN RETURN dw END
  |up  : IF (C>0) THEN n:=field[L,C-1] ELSE n:=block END;
         IF NOT (n IN {block,wall,hole}) & (f=steps) THEN RETURN up END
  |fier_lf: IF L>0 THEN n:=field[L-1,C];
              IF C<HIGH(field[0]) THEN d:=field[L-1,C+1] ELSE d:=block END;
            ELSE n:=block
            END;
            IF (n IN {free,exit,fier}) & (d=wall) THEN RETURN fier_lf END;
  |fier_rg: IF L<HIGH(field) THEN n:=field[L+1,C];
              IF C<HIGH(field[0]) THEN d:=field[L+1,C+1] ELSE d:=block END;
            ELSE n:=block
            END;
            IF (n IN {free,exit,fier}) & (d=wall) THEN RETURN fier_rg END;
  ELSE RETURN new
  END;
  RETURN empty;
END ?;

PROCEDURE fall?(man: BOOLEAN; new: DIRS; L,C: INTEGER): DIRS;
  VAR f,d,n: INTEGER; legal: BITSET;
BEGIN
  IF man THEN
    legal:={free,hole,fier,case,exit}
  ELSE
    legal:={free,hole,case,exit}
  END;
  f:=field[L,C];
  IF C<HIGH(field[0]) THEN d:=field[L,C+1] ELSE d:=block END;
  IF gads?(L,C+1) THEN d:=block END;
  IF NOT (d IN {block,wall,steps}) &                (
     (new=dw) & (f#steps)                           OR
     (new IN ds{lf,rg}) & (NOT (f IN {pole,steps})) OR
     (f IN legal)                                     )
  THEN
    RETURN fall
  END;
  IF (new=up) & NOT (f IN {steps}) OR
     (new=dw) & (d IN {block,wall})
  THEN RETURN empty END;
  IF (new=fier_lf) THEN
    IF L>0 THEN n:=field[L-1,C];
      IF C<HIGH(field[0]) THEN d:=field[L-1,C+1] ELSE d:=block END;
    ELSE n:=block
    END;
    IF (n IN {free,exit,fier}) & (d=wall) THEN RETURN fier_lf END;
    RETURN empty
  END;
  IF (new=fier_rg) THEN
    IF L<HIGH(field) THEN n:=field[L+1,C];
      IF C<HIGH(field[0]) THEN d:=field[L+1,C+1] ELSE d:=block END;
    ELSE n:=block
    END;
    IF (n IN {free,exit,fier}) & (d=wall) THEN RETURN fier_rg END;
    RETURN empty
  END;
  RETURN new
END fall?;

PROCEDURE Fier!(VAR m: PLAYER; d: DIRS);
BEGIN
  WITH m DO
    time:=0;
    set(m,d,l,c);
    INCL(mode,_fier);
    new:=empty;
  END;
END Fier!;

PROCEDURE move0(VAR m: PLAYER);
BEGIN
  WITH m DO
    IF _fier IN mode THEN
      IF time<5 THEN
        sh_fire(m,FALSE);
        INC(time);
        sh_fire(m,TRUE)
      ELSE
        IF dir=fier_lf THEN
          add_fier(l DIV 6-1,c DIV 5+1);
          sh_fire(m,FALSE);
          set(m,lf,l,c)
        ELSE
          add_fier(l DIV 6+1,c DIV 5+1);
          sh_fire(m,FALSE);
          set(m,rg,l,c)
        END;
        EXCL(mode,_fier);
      END
    ELSIF _new IN mode THEN
      IF (time<12) & (time MOD 4=0) THEN
        sh_new(l DIV 6,c DIV 5,time DIV 4);
        IF time=0 THEN
          m.dir:=fall;
          m.new:=empty;
          show_man(m,TRUE);
          EXCL(mode,_new)
        END
      END;
      DEC(time)
    END
  END
END move0;

PROCEDURE move(VAR m: PLAYER);
  VAR d: DIRS;
BEGIN
  WITH m DO
    IF mode#{} THEN move0(m); RETURN END;
    IF dir#fall THEN
      IF (new=empty) THEN RETURN END;
      IF (dir#new) THEN
        IF (ds{dir,new}*ds{lf,rg,up,dw}=ds{dir,new}) THEN
          IF (dir IN ds{lf,rg})=(new IN ds{lf,rg}) THEN
            set(m,new,l,c); RETURN
          END
        END;
        IF (l MOD 6 IN {0,1,5}) & (c MOD 5 IN {0,1,4}) THEN
          d:=?(new,(l+3) DIV 6,(c+3) DIV 5);
          IF d=fall THEN
            set(m,fall,((l+3) DIV 6)*6,((c+3) DIV 5)*5);
            RETURN
          ELSIF (d IN ds{fier_lf,fier_rg}) THEN
            IF (l MOD 6=0)&(c MOD 5=0) THEN Fier!(m,d); RETURN END
          ELSE
            IF   d=new THEN set(m,new,((l+3) DIV 6)*6,((c+3) DIV 5)*5);
            ELSE new:=empty; RETURN
            END
          END
        END
      END
    ELSE
      IF c MOD 5 = 0 THEN
        d:=fall?(mn=1,new,l DIV 6,c DIV 5);
        IF d=empty THEN new:=empty; RETURN END;
        IF d#fall THEN
          IF (d IN ds{fier_lf,fier_rg}) THEN Fier!(m,d)
          ELSE set(m,d,l,c)
          END;
          RETURN
        END
      END
    END;
    IF (l MOD 6 IN {0,1,5}) & (c MOD 5 IN {0,1,4}) THEN
      IF (dir=fall) & (c MOD 5=0) THEN
        d:=fall?(mn=1,new,(l+3) DIV 6,(c+3) DIV 5);
        IF d=empty THEN RETURN END;
        IF d#fall THEN
          set(m,d,((l+3) DIV 6)*6,((c+3) DIV 5)*5);
          RETURN
        END;
      ELSE
        d:=?(dir,(l+3) DIV 6,(c+3) DIV 5);
        IF d#dir THEN
          IF d=fall THEN set(m,fall,((l+3) DIV 6)*6,((c+3) DIV 5)*5);
          ELSIF (d=empty) & (l MOD 6=0) & (c MOD 5=0) THEN
            new:=empty; RETURN
          ELSE ASSERT(d=empty)
          END;
        END;
      END;
    END;
    CASE dir OF
    |lf  : set(m,dir,l-1,c  );
    |rg  : set(m,dir,l+1,c  );
    |up  : set(m,dir,l  ,c-1);
    |dw  : set(m,dir,l  ,c+1);
    |fall: set(m,dir,l  ,c+1);
    ELSE
    END;
  END;
END move;

PROCEDURE break_fire(VAR m: PLAYER);
  VAR x,y: INTEGER;
BEGIN
  WITH man DO
    EXCL(mode,_fier);
    x:=l DIV 6;
    y:=c DIV 5;
    sh_fire(man,FALSE);
    IF dir=fier_lf THEN
      field[x-1,y+1]:=wall;
      sh_f(x-1,y+1,TRUE);
      IF (new#dir) & (?(new,l DIV 6,c DIV 5)#empty) THEN dir:=new
      ELSE dir:=lf; new:=empty;
      END;
    ELSE
      field[x+1,y+1]:=wall;
      sh_f(x+1,y+1,TRUE);
      IF (new#dir) & (?(new,x,y)#empty) THEN dir:=new
      ELSE dir:=lf; new:=empty
      END
    END
  END;
  show_man(m,TRUE)
END break_fire;

VAR wrk: ARRAY [0..25] OF ARRAY [0..15] OF DIRS;

PROCEDURE moveGads(): BOOLEAN;

  PROCEDURE range(VAL man,gad: PLAYER): INTEGER;
  BEGIN RETURN ABS(man.l-gad.l) + ABS(man.c-gad.c);
  END range;

  VAR i,r: INTEGER; x,y: INTEGER;

BEGIN
  FOR i:=0 TO gad_no-1 DO
    WITH gad[i] DO
      move(gad[i]);
      IF range(man,gad[i])<5 THEN RETURN TRUE END;
      IF (man.dir IN ds{fier_lf,fier_rg}) THEN
        r:=man.l-l;
        IF (man.c=c) & (ABS(r)<9) & ((r>0)=(man.dir=fier_lf)) THEN
        break_fire(man)
        END
      END
    END
  END;
  RETURN FALSE
END moveGads;

-------------------------  STRATEGY  ---------------------------
                         ------------

PROCEDURE cal(VAL m: PLAYER): BOOLEAN;
  VAR L,C: INTEGER;
BEGIN
  WITH m DO
    IF (l MOD 6=0) & (c MOD 5=0) THEN
      L:=l DIV 6; C:=c DIV 5;
      IF (field[L,C]=case) THEN
        sh_f(L,C,FALSE);
        field[L,C]:=free;
        DEC(boxes);
      END;
    END;
    IF (boxes=0) & first THEN
      first:=FALSE;
      FOR L:=0 TO HIGH(field) DO
        FOR C:=0 TO HIGH(field[L]) DO
          IF field[L,C]=exit THEN field[L,C]:=steps; sh_f(L,C,TRUE) END;
        END;
      END;
    END;
    RETURN (c+boxes=0)
  END;
END cal;

PROCEDURE Strategy;

  VAR L,C,dx,dy,i,x,y,cou: INTEGER; _d,_u,_l,_r: BOOLEAN;


  PROCEDURE speed(dir: DIRS): BOOLEAN;
  BEGIN
    CASE dir OF
      |up: RETURN dy<0  |dw: RETURN dy>0
      |lf: RETURN dx<0  |rg: RETURN dx>0
    ELSE
      RETURN FALSE
    END
  END speed;

  PROCEDURE try_change(dir: DIRS): DIRS;

    PROCEDURE try(d0,d1: DIRS; p0,p1: BOOLEAN): DIRS;
    BEGIN
      IF p0 & (speed(d0)>speed(dir)) THEN RETURN d0 END;
      IF p1 & (speed(d1)>speed(dir)) THEN RETURN d1 END;
      RETURN dir
    END try;

  BEGIN
    CASE dir OF
      |up,dw: RETURN try(lf,rg,_l,_r)
      |lf,rg: RETURN try(up,dw,_u,_d)
    ELSE RETURN dir
    END
  END try_change;

  PROCEDURE change_dir(dir: DIRS): DIRS;
    VAR new: DIRS;
  BEGIN
    new:=try_change(dir);
    IF new#dir THEN RETURN new END;
    IF (dy>0) & _d THEN RETURN dw END;
    IF (dy<0) & _u THEN RETURN up END;
    IF (dx>0) & _r THEN RETURN rg END;
    IF (dx<0) & _l THEN RETURN lf END;
    cou:=16;
    REPEAT
      CASE (rnd.next()+i) MOD 4 OF
        |0: IF _d THEN RETURN dw END
        |1: IF _u THEN RETURN up END
        |2: IF _r THEN RETURN rg END
        |3: IF _l THEN RETURN lf END
      END;
      DEC(cou)
    UNTIL (cou=0);
    RETURN empty
  END change_dir;

BEGIN
  C:=(man.c+3) DIV 5;
  L:=(man.l+3) DIV 6;
  FOR i:=0 TO gad_no-1 DO
    WITH gad[i] DO
      x:=(l+3) DIV 6;       dx:=L-x;
      y:=(c+3) DIV 5;       dy:=C-y;
      _u:=?(up,x,y)#empty;  _l:=?(lf,x,y)#empty;
      _d:=?(dw,x,y)#empty;  _r:=?(rg,x,y)#empty;
      cou:=ORD(_d)+ORD(_u)+ORD(_l)+ORD(_r);

      IF (field[x,y]=case) & NOT take THEN
        sh_f(x,y,FALSE); field[x,y]:=free;
        take:=TRUE
      END;
      IF (field[x,y]=fier) & NOT (dir IN ds{lf,rg}) THEN
        new:=empty;
        IF    time=0 THEN time:=50;
          IF take THEN
            take:=FALSE; field[x,y-1]:=case; sh_f(x,y-1,TRUE)
          END
        ELSIF time=1 THEN
          IF field[x,y-1] IN {free,exit,case} THEN
            new:=change_dir(up); time:=0;
            IF new=lf THEN
              set(gad[i],new,(x-1)*6,(y-1)*5)
            ELSE
              set(gad[i],new,(x+1)*6,(y-1)*5)
            END
          ELSE
            time:=5
          END;
        ELSE
          DEC(time)
        END
      ELSIF  cou=0         THEN new:=empty;
      ELSIF (new=empty) OR (?(dir,x,y)=empty) THEN new:=change_dir(dir)
      ELSIF (cou=2) & (dy=0) & NOT speed(dir) &
         (ODD(rnd.next())=ODD(i))         THEN new:=change_dir(dir)
      ELSIF cou>2                         THEN new:=try_change(dir)
      ELSE new:=dir
      END;
      IF new=fall THEN time:=0; new:=empty END
    END
  END
END Strategy;

PROCEDURE cal_fiers(): BOOLEAN;
  CONST start = 16*3;

  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO fier_no-1 DO
    WITH fiered[i] DO
      IF (time<start) & (time MOD 16=0) THEN sh_fier(l,c,time DIV 16); END;
      IF (time=0) & remove_fier(i) THEN RETURN TRUE END;
      DEC(time);
    END;
  END;
  RETURN FALSE
END cal_fiers;

---------------------------  GAME  -----------------------------
                           --------

PROCEDURE Game(VAR level: INTEGER);
  VAR i: INTEGER;
    odd: INTEGER;
    ch : CHAR;
BEGIN
  WHILE key.pressed()>0 DO
    key.read(ch);
  END;
  WHILE key.pressed()=0 DO
    wait(140); show_man(man,FALSE);
    wait(140); show_man(man,TRUE);
  END;
  odd:=0;
  LOOP
    IF key.pressed()>0 THEN
      key.read(ch);
      CASE ch OF
      |'8',key.up   : man.new:=up;
      |'2',key.dw   : man.new:=dw;
      |'4',key.left : man.new:=lf;
      |'6',key.right: man.new:=rg;
      |'7',02c,
           key.ins,
           key.end,
           key.home : man.new:=fier_lf;
      |'9',key.del,
           key.pgup,
           key.pgdw : man.new:=fier_rg;
      |'+'          : level:=(level+1+150) MOD 150; RETURN
      |'-'          : level:=(level-1+150) MOD 150; RETURN
      |33c          : RETURN
      |'r','R'      : showfield(level);
      ELSE man.new:=empty
      END;
    END;
    move(man);
    IF cal(man) THEN EXIT END;
    IF (odd MOD 2=0) THEN Strategy
    ELSIF   moveGads() OR cal_fiers() THEN RETURN
    END;
    INC(odd);
    wait(30);
  END;
  INC(level)
END Game;

VAR ch: CHAR;
   lev: INTEGER;

BEGIN
  Init;
  slow:=args.flag('+','s');
  IF args.number('level',lev) THEN DEC(lev) ELSE lev:=0 END;
  LOOP
    InitField(lev);
    Game(lev);
    LOOP
      IF key.pressed()=0 THEN wait(250) END;
      IF key.pressed()=0 THEN EXIT END;
      key.read(ch);
      IF    ch='+' THEN lev:=(lev+1+150) MOD 150
      ELSIF ch='-' THEN lev:=(lev-1+150) MOD 150
      ELSE wait(500); EXIT
      END
    END
  END
END lr.
