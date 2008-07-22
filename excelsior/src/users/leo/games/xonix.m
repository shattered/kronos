MODULE xonix; (* Dima 10-Apr-88. (c) KRONOS *)

IMPORT  vdu: videoGames;
IMPORT  sys: SYSTEM;
IMPORT  key: Keyboard;
IMPORT  rnd: Random;
IMPORT  tim: Time;

CONST
  lns=70; cls=100;
  up = -cls-2;  le = -1;
  dw =  cls+2;  ri = +1;

TYPE
  fish = (inv, emp, ful, tra, wav, man, xon, dog,  xxx, ooo, nnn, iii);
  set  = SET OF fish;
  info = RECORD pos, movh, movv: INTEGER END;

VAR
  field: ARRAY [0..(lns+2)*(cls+2)-1] OF fish;
  me   : info;
  pict : ARRAY fish OF vdu.sprite;

PROCEDURE showfish(f: fish; pos: INTEGER);
  VAR l,c: INTEGER;
BEGIN
  l:=(pos DIV dw - 1) * 4; c:=(pos MOD dw - 1) * 8;
  vdu.show_sprite(c,l,pict[f]);
END showfish;

PROCEDURE showfield;
BEGIN
  vdu.erase();
  vdu.rect(0,0,cls*8-1,7);
  vdu.rect(0,lns*4-8,cls*8-1,lns*4-1);
  vdu.rect(0,8,15,lns*4-9);
  vdu.rect(cls*8-16,8,cls*8-1,lns*4-9);
END showfield;

PROCEDURE paint(): INTEGER;
  VAR painted: INTEGER;
      l, c, i: INTEGER;
      p      : sys.ADDRESS;
BEGIN
  painted:=0;
  FOR l:=0 TO lns-1 DO
    p:=sys.ADR(field[(l+1)*dw+1]);
    FOR c:=0 TO cls-1 DO
      IF fish(p^) =wav THEN
        p^:=emp;
      ELSIF (fish(p^)=emp) OR (fish(p^)=tra) THEN
        p^:=ful;
        showfish(ful,(l+1)*dw+1+c);
        INC(painted);
      END;
      INC(p);
    END;
  END;
  RETURN painted
END paint;

PROCEDURE showlife(n: INTEGER);
BEGIN
  vdu.print(0,285,'life: %2d',n);
END showlife;

PROCEDURE showfull(n: INTEGER);
BEGIN
  vdu.print(200,285,'full: %4d',n);
END showfull;

PROCEDURE showtotal(n: INTEGER);
BEGIN
  vdu.print(400,285,'total: %6d',n);
END showtotal;

PROCEDURE showlevel(n: INTEGER);
BEGIN
  vdu.print(600,285,'level: %d', n);
END showlevel;

PROCEDURE iniman;
BEGIN WITH me DO
  pos:=0+dw+ri; movv:=0; movh:=0; showfish(man, pos);
END END iniman;

PROCEDURE dir(VAR I: info);
BEGIN
  I.movh:=le+2*ri*INTEGER(ODD(rnd.next()));
  I.movv:=up+2*dw*INTEGER(ODD(rnd.next()));
END dir;

PROCEDURE inidog(VAR d: info);
BEGIN
  d.pos:=0+dw+ri*(cls DIV 2);
  dir(d);
  showfish(dog, d.pos);
END inidog;

PROCEDURE inixon(VAR x: info);
BEGIN
  REPEAT x.pos:=rnd.next() MOD (HIGH(field)+1) UNTIL field[x.pos]=emp;
  dir(x);
  showfish(xon, x.pos);
END inixon;

PROCEDURE inifield;
  TYPE row=ARRAY [0..cls+1] OF fish;
       fld=ARRAY [0..lns+1] OF row;
  VAR i: INTEGER; r: row; p: POINTER TO fld;
BEGIN
  FOR i:=0 TO HIGH(r) DO r[i]:=inv END;
  p:=sys.ADR(field);
  p^[0]:=r; p^[lns+1]:=r;

  FOR i:=1 TO HIGH(r)-1 DO r[i]:=ful END; r[0]:=inv; r[cls+1]:=inv;
  p^[1]:=r; p^[lns  ]:=r;
  p^[2]:=r; p^[lns-1]:=r;

  FOR i:=0 TO HIGH(r) DO r[i]:=emp END;
  r[0    ]:=inv; r[1  ]:=ful; r[2    ]:=ful;
  r[cls+1]:=inv; r[cls]:=ful; r[cls-1]:=ful;
  FOR i:=3 TO lns-2 DO p^[i]:=r END;
  showfield;
END inifield;

TYPE result = (ok, killed, returned);

PROCEDURE next(VAL I: info): fish;
BEGIN RETURN field[I.pos+I.movh+I.movv]
END next;

PROCEDURE step(VAR I: info; f,trace: fish); -- without checks
BEGIN
  showfish(trace, I.pos);
  INC(I.pos, I.movh+I.movv);
  showfish(f    , I.pos);
END step;

PROCEDURE movexon(VAR I: info): result;
  VAR f: fish; f1,f2: BOOLEAN;
BEGIN
  f:=next(I);
  IF f=emp THEN step(I, xon, emp); RETURN ok END;
  IF f=tra THEN RETURN killed END;
  ASSERT(f=ful);
  f1:=(field[I.pos+I.movh]=ful);
  f2:=(field[I.pos+I.movv]=ful);
  IF f1=f2 THEN I.movh:=-I.movh; I.movv:=-I.movv;
  ELSIF f1 THEN I.movh:=-I.movh;
  ELSIF f2 THEN                  I.movv:=-I.movv;
  END;
  RETURN ok
END movexon;

PROCEDURE movedog(VAR I: info): result;
  VAR f1,f2: BOOLEAN;
BEGIN
  IF next(I)=ful THEN
    step(I, dog, ful);
    IF I.pos=me.pos THEN RETURN killed ELSE RETURN ok END;
  END;
  f1:=(field[I.pos+I.movh-I.movv]=ful);
  f2:=(field[I.pos+I.movv-I.movh]=ful);
  IF    f1=f2  THEN I.movh:=-I.movh; I.movv:=-I.movv;
  ELSIF f1     THEN                  I.movv:=-I.movv;
  ELSIF f2     THEN I.movh:=-I.movh;
  END;
  RETURN movedog(I)
END movedog;

PROCEDURE moveme(): result;
  VAR f1,f2,head: fish;
BEGIN
  IF me.movv=me.movh (* =0 *) THEN RETURN ok END;
  f1:=field[me.pos]; f2:=next(me);
  IF f2=inv THEN me.movv:=0; me.movh:=0; RETURN ok END;
  IF f2=tra THEN RETURN killed END;
  IF f2=emp THEN step(me, tra, f1); field[me.pos]:=tra;
  ELSE           step(me, man, f1); field[me.pos]:=ful;
  END;
  IF (f1=tra) & (f2=ful) THEN
    me.movv:=0; me.movh:=0;
    RETURN returned
  END;
  RETURN ok
END moveme;

CONST maxonx=10; maxdog=3;

VAR  xonx : ARRAY [0..maxonx-1] OF info;
     dogs : ARRAY [0..maxdog-1] OF info;

PROCEDURE territory(xonum: INTEGER; VAR full: INTEGER);
  VAR w     : ARRAY [0..lns*cls-1] OF sys.ADDRESS;
      p, p0 : sys.ADDRESS;
      f, f0 : sys.ADDRESS;
      x     : INTEGER;
BEGIN
  p0:=sys.ADR(w[0]); f0:=sys.ADR(field[0]);
  FOR x:=0 TO xonum-1 DO
    IF field[xonx[x].pos]=emp THEN
      w[0]:=f0 + xonx[x].pos;
      w[0]^:=wav;
      p:=p0;
      WHILE p>=p0 DO
        f:=sys.ADDRESS(p^);
        DEC(p);
        INC(f,    up); IF fish(f^)=emp THEN f^:=wav; INC(p); p^:=f END;
        INC(f, dw-up); IF fish(f^)=emp THEN f^:=wav; INC(p); p^:=f END;
        INC(f, le-dw); IF fish(f^)=emp THEN f^:=wav; INC(p); p^:=f END;
        INC(f, ri-le); IF fish(f^)=emp THEN f^:=wav; INC(p); p^:=f END;
      END;
    END;
  END;
  INC(full, paint());
  showfull(full);
END territory;

PROCEDURE traceoff;
  VAR c: INTEGER;
BEGIN
  FOR c:=0 TO HIGH(field) DO
    IF field[c]=tra THEN field[c]:=emp; showfish(inv, c); END;
  END;
  iniman;
END traceoff;

PROCEDURE play(): INTEGER;

  CONST enough  = lns*cls*3 DIV 4;
        dogtime = 1000*60;

  VAR xonum, life,
      level, total,
      donum, full : INTEGER;

  PROCEDURE readmove;
    CONST UP=key.up;   DW=key.dw;
          LE=key.left; RI=key.right;
          ST='5';
    VAR ch: CHAR;
  BEGIN
    IF key.pressed()=0 THEN RETURN END;
    key.read(ch);
    CASE ch OF
     |UP : me.movv:=dw; me.movh:=00
     |DW : me.movv:=up; me.movh:=00
     |LE : me.movv:=00; me.movh:=le
     |RI : me.movv:=00; me.movh:=ri
     |ST : me.movv:=00; me.movh:=00
     |'+': IF level<9 THEN INC(level); showlevel(level) END
     |'-': IF level>0 THEN DEC(level); showlevel(level) END
    ELSE END;
  END readmove;

  VAR tick: INTEGER;

  PROCEDURE zad;
  BEGIN
    INC(tick,2);
    REPEAT UNTIL tim.sys_time(tim.tick)>=tick;
  END zad;

  VAR i, time: INTEGER; r: result; lifeout: BOOLEAN; ch: CHAR;
BEGIN
  total:=0; life:=4; xonum:=3; level:=5;
  LOOP
    inifield; iniman; FOR i:=0 TO xonum-1 DO inixon(xonx[i]) END;
    full:=0; donum:=0; time:=tim.sys_time(tim.milisec)+dogtime;
    showlife(life);   showfull(full);
    showlevel(level); showtotal(total);
    REPEAT
      lifeout:=FALSE; tick:=tim.sys_time(tim.tick);
      FOR i:=0 TO xonum-1 DO
        IF movexon(xonx[i])=killed THEN lifeout:=TRUE; traceoff END;
      END;
      FOR i:=0 TO donum-1 DO
        IF movedog(dogs[i])=killed THEN lifeout:=TRUE END;
      END;
      IF NOT lifeout & (tim.sys_time(tim.milisec)>=time) THEN
        IF donum=maxdog THEN lifeout:=TRUE
        ELSE
          inidog(dogs[donum]); INC(donum);
          time:=tim.sys_time(tim.milisec)+dogtime;
        END;
      END;
      IF NOT lifeout THEN
        readmove;
        r:=moveme();
        IF    r=returned THEN territory(xonum, full)
        ELSIF r=killed   THEN lifeout:=TRUE; traceoff;
        END;
      END;
      IF lifeout THEN
        DEC(life);
        FOR i:=0 TO donum-1 DO showfish(ful, dogs[i].pos) END;
        donum:=0;
        showlife(life);
        IF life=0 THEN EXIT END;
      END;
      zad;
    UNTIL (full>=enough);
    key.read(ch);
    INC(total, full); INC(life);
    IF xonum<maxonx THEN INC(xonum) END;
  END;
  INC(total, full);
  RETURN total
END play;

PROCEDURE header;
  VAR letters: ARRAY [0..4] OF info;
      fishes : ARRAY [0..4] OF fish;

  PROCEDURE movexon(n: INTEGER);
    VAR f: fish; f1,f2: BOOLEAN;
  BEGIN
    WITH letters[n] DO
      f:=field[pos+movv+movh];
      IF f=emp THEN
        INC(pos, movv+movh);
        showfish(fishes[n], pos);
        RETURN
      END;
      f1:=(field[pos+movh]=ful);
      f2:=(field[pos+movv]=ful);
      IF f1=f2 THEN movh:=-movh; movv:=-movv;
      ELSIF f1 THEN movh:=-movh;
      ELSIF f2 THEN              movv:=-movv;
      END;
    END;
    movexon(n);
  END movexon;

  VAR i,t: INTEGER;
BEGIN
  FOR i:=0 TO 4 DO WITH letters[i] DO
    pos:=dw*(7-i)+ri*(3+i);
    movv:=dw; movh:=ri;
  END END;
  fishes[0]:=xxx; fishes[1]:=ooo; fishes[2]:=nnn;
  fishes[3]:=iii; fishes[4]:=xxx;
  inifield;
  t:=tim.sys_time(tim.tick);
  REPEAT
    FOR i:=0 TO 4 DO movexon(i) END;
    INC(t,15);
    REPEAT UNTIL tim.sys_time(tim.tick)>=t;
  UNTIL key.pressed()>0;
END header;

VAR resultat: INTEGER;

BEGIN
  vdu.build_sprite(pict[inv],'        |        |        |        |');
  vdu.build_sprite(pict[emp],'        |        |        |        |');
  vdu.build_sprite(pict[ful],'********|********|********|********|');
  vdu.build_sprite(pict[tra],'  **  **|  **  **|  **  **|  **  **|');
  vdu.build_sprite(pict[wav],'        |        |        |        |');
  vdu.build_sprite(pict[man],'        |  ****  |  ****  |        |');
  vdu.build_sprite(pict[xon],'  ****  |**    **|**    **|  ****  |');
  vdu.build_sprite(pict[dog],'  ******|   *****|        |      **|');
  vdu.build_sprite(pict[xxx],' **  ** |   **   |  *  *  |**    **|');
  vdu.build_sprite(pict[ooo],'  ****  |**    **|**    **|  ****  |');
  vdu.build_sprite(pict[nnn],'**   ***|**  * **|** *  **|***   **|');
  vdu.build_sprite(pict[iii],'   **   |   **   |   **   |   **   |');
  resultat:=0;
  header;
  resultat:=play();
END xonix.
