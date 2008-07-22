MODULE tetris; (* Sem 10-Dec-88. (c) KRONOS *)

IMPORT os : osKernel;
IMPORT mcd: defCodes;
IMPORT bio: BIO;
IMPORT env: tskEnv;
IMPORT tim: Time;

FROM SYSTEM     IMPORT  ADR, ADDRESS, WORD;
FROM LabtamVDU  IMPORT  layer, auto, block;
FROM Keyboard   IMPORT  read, busy_read;
FROM Random     IMPORT  next;
FROM Strings    IMPORT  image;

TYPE fig =ARRAY [0..3] OF RECORD x,y: INTEGER END;
     blk =ARRAY [0..11] OF BITSET;
     pblk=POINTER TO blk;

CONST xsz=20; ysz=10; dx=300;

VAR font : ARRAY CHAR OF pblk;
    st   : ARRAY [0..23] OF ARRAY [0..9] OF BOOLEAN;
    fgs  : ARRAY [0..6] OF fig;
    ctr  : ARRAY [0..6] OF RECORD x,y: INTEGER END;
    X,Y  : INTEGER;
    N,R  : INTEGER;
    time : INTEGER;
    score: INTEGER;
    record: ARRAY [0..9] OF RECORD
      name: ARRAY [0..15] OF CHAR;
      cnt : INTEGER;
    END;

PROCEDURE BBP(a: ADDRESS; x,sz: INTEGER; v: WORD); CODE 0EDh END BBP;

PROCEDURE xy?(x0,y0,r,n: INTEGER; VAR x,y: INTEGER);
BEGIN
  CASE r OF
    |0: x:=(x0+x0+(fgs[N][n].x-ctr[N].x)+ctr[N].x) DIV 2;
        y:=(y0+y0+(fgs[N][n].y-ctr[N].y)+ctr[N].y) DIV 2;
    |1: x:=(x0+x0-(fgs[N][n].y-ctr[N].y)+ctr[N].x) DIV 2;
        y:=(y0+y0+(fgs[N][n].x-ctr[N].x)+ctr[N].y) DIV 2;
    |2: x:=(x0+x0-(fgs[N][n].x-ctr[N].x)+ctr[N].x) DIV 2;
        y:=(y0+y0-(fgs[N][n].y-ctr[N].y)+ctr[N].y) DIV 2;
    |3: x:=(x0+x0+(fgs[N][n].y-ctr[N].y)+ctr[N].x) DIV 2;
        y:=(y0+y0-(fgs[N][n].x-ctr[N].x)+ctr[N].y) DIV 2;
  END;
END xy?;

PROCEDURE check(nx,ny,nr: INTEGER): BOOLEAN;
  VAR x,y,i: INTEGER;
BEGIN
  FOR i:=0 TO 3 DO
    xy?(nx,ny,nr,i,x,y);
    IF (x<0)OR(y<0)OR(y>HIGH(st))OR(x>HIGH(st[y]))OR st[y][x] THEN
      RETURN FALSE;
    END;
  END;
  RETURN TRUE;
END check;

PROCEDURE drow(): BOOLEAN;
  VAR i,j,x,y,mix,miy,max,may: INTEGER; a: ADDRESS;
BEGIN
  mix:=800; miy:=300; max:=0; may:=0;
  IF NOT check(X,Y,R) THEN RETURN FALSE END;
  FOR i:=0 TO 3 DO
    xy?(X,Y,R,i,x,y); x:=x*xsz; y:=y*ysz;
    IF x<mix THEN mix:=x END; IF x+xsz>max THEN max:=x+xsz END;
    IF y<miy THEN miy:=y END; IF y+ysz>may THEN may:=y+ysz END;
    ASSERT((y>=0)&(y<300)); ASSERT((x>=0)&(y<800));
    a:=ADR(layer[0]^[y]);
    FOR j:=0 TO ysz-1 DO BBP(a,dx+x,xsz,-1); INC(a,25) END;
  END;
  block(0,dx+mix,miy,max-mix,may-miy);
  RETURN TRUE;
END drow;

PROCEDURE wait;
BEGIN
  INC(time,1); REPEAT UNTIL tim.sys_time(tim.tick)>=time;
END wait;

PROCEDURE rol;
  VAR i,j,x,y,mix,miy,max,may: INTEGER; a: ADDRESS;
BEGIN
  mix:=800; miy:=300; max:=0; may:=0;
  IF NOT check(X,Y,(R+3) MOD 4) THEN RETURN END;
  FOR i:=0 TO 3 DO
    xy?(X,Y,R,i,x,y);
    x:=x*xsz; y:=y*ysz;
    IF x<mix THEN mix:=x END; IF x+xsz>max THEN max:=x+xsz END;
    IF y<miy THEN miy:=y END; IF y+ysz>may THEN may:=y+ysz END;
    ASSERT((y>=0)&(y<300)); ASSERT((x>=0)&(y<800));
    a:=ADR(layer[0]^[y]);
    FOR j:=0 TO ysz-1 DO BBP(a,dx+x,xsz,0); INC(a,25) END;
  END;
  R:=(R+3) MOD 4;
  FOR i:=0 TO 3 DO
    xy?(X,Y,R,i,x,y);
    ASSERT(NOT st[y][x]);
    x:=x*xsz; y:=y*ysz;
    IF x<mix THEN mix:=x END; IF x+xsz>max THEN max:=x+xsz END;
    IF y<miy THEN miy:=y END; IF y+ysz>may THEN may:=y+ysz END;
    ASSERT((y>=0)&(y<300)); ASSERT((x>=0)&(y<800));
    a:=ADR(layer[0]^[y]);
    FOR j:=0 TO ysz-1 DO BBP(a,dx+x,xsz,-1); INC(a,25) END;
  END;
  block(0,dx+mix,miy,max-mix,may-miy);
END rol;

PROCEDURE left;
  VAR xl,xr,yu: ARRAY [0..3] OF INTEGER;
      i,j,k,mix,max,miy,may: INTEGER;
      a: ADDRESS;
BEGIN
  IF NOT check(X-1,Y,R) THEN RETURN END;
  mix:=800; miy:=300; max:=0; may:=0;
  FOR i:=0 TO 3 DO
    xy?(X,Y,R,i,xl[i],k);
    xl[i]:=xl[i]*xsz; xr[i]:=xl[i]+xsz; yu[i]:=k*ysz;
    IF xl[i]<mix THEN mix:=xl[i] END;
    IF xr[i]>max THEN max:=xr[i] END;
    IF yu[i]<miy THEN miy:=yu[i] END;
    IF yu[i]+ysz>may THEN may:=yu[i]+ysz END;
  END;
  DEC(mix,xsz);
  FOR i:=0 TO 3 DO
    FOR j:=0 TO 3 DO
      IF (xl[i]=xr[j])&(yu[i]=yu[j]) THEN xl[i]:=-1; xr[j]:=-1 END;
    END;
  END;
  FOR i:=2 TO xsz BY 2 DO
    FOR j:=0 TO 3 DO
      IF xl[j]>=0 THEN
        a:=ADR(layer[0]^[yu[j]]);
        FOR k:=0 TO ysz-1 DO BBP(a,dx+xl[j]-i,2,-1); INC(a,25) END;
      END;
      IF xr[j]>=0 THEN
        a:=ADR(layer[0]^[yu[j]]);
        FOR k:=0 TO ysz-1 DO BBP(a,dx+xr[j]-i,2,0); INC(a,25) END;
      END;
    END;
    block(0,dx+mix,miy,max-mix,may-miy);
    wait;
  END;
  DEC(X);
END left;

PROCEDURE right;
  VAR xl,xr,yu: ARRAY [0..3] OF INTEGER;
      i,j,k,mix,max,miy,may: INTEGER;
      a: ADDRESS;
BEGIN
  IF NOT check(X+1,Y,R) THEN RETURN END;
  mix:=800; miy:=300; max:=0; may:=0;
  FOR i:=0 TO 3 DO
    xy?(X,Y,R,i,xl[i],k);
    xl[i]:=xl[i]*xsz; xr[i]:=xl[i]+xsz; yu[i]:=k*ysz;
    IF xl[i]<mix THEN mix:=xl[i] END;
    IF xr[i]>max THEN max:=xr[i] END;
    IF yu[i]<miy THEN miy:=yu[i] END;
    IF yu[i]+ysz>may THEN may:=yu[i]+ysz END;
  END;
  INC(max,xsz);
  FOR i:=0 TO 3 DO
    FOR j:=0 TO 3 DO
      IF (xl[i]=xr[j])&(yu[i]=yu[j]) THEN xl[i]:=-1; xr[j]:=-1 END;
    END;
  END;
  FOR i:=0 TO xsz-1 BY 2 DO
    FOR j:=0 TO 3 DO
      IF xl[j]>=0 THEN
        a:=ADR(layer[0]^[yu[j]]);
        FOR k:=0 TO ysz-1 DO BBP(a,dx+xl[j]+i,2,0); INC(a,25) END;
      END;
      IF xr[j]>=0 THEN
        a:=ADR(layer[0]^[yu[j]]);
        FOR k:=0 TO ysz-1 DO BBP(a,dx+xr[j]+i,2,-1); INC(a,25) END;
      END;
    END;
    block(0,dx+mix,miy,max-mix,may-miy);
    wait;
  END;
  INC(X);
END right;

PROCEDURE down(): BOOLEAN;
  VAR xl,yd,yu: ARRAY [0..3] OF INTEGER;
      i,j,k,mix,max,miy,may: INTEGER;
      a: ADDRESS;
BEGIN
  IF NOT check(X,Y+1,R) THEN RETURN FALSE END;
  mix:=800; miy:=300; max:=0; may:=0;
  FOR i:=0 TO 3 DO
    xy?(X,Y,R,i,xl[i],k);
    xl[i]:=xl[i]*xsz; yu[i]:=k*ysz; yd[i]:=yu[i]+ysz;
    IF xl[i]<mix THEN mix:=xl[i] END;
    IF xl[i]+xsz>max THEN max:=xl[i]+xsz END;
    IF yu[i]<miy THEN miy:=yu[i] END;
    IF yd[i]>may THEN may:=yd[i] END;
  END;
  INC(may,ysz);
  FOR i:=0 TO 3 DO
    FOR j:=0 TO 3 DO
      IF (xl[i]=xl[j])&(yd[i]=yu[j]) THEN yd[i]:=-1; yu[j]:=-1 END;
    END;
  END;
  FOR i:=0 TO ysz-1 DO
    FOR j:=0 TO 3 DO
      IF yu[j]>=0 THEN a:=ADR(layer[0]^[yu[j]+i]); BBP(a,dx+xl[j],xsz,0) END;
      IF yd[j]>=0 THEN a:=ADR(layer[0]^[yd[j]+i]); BBP(a,dx+xl[j],xsz,-1) END;
    END;
    block(0,dx+mix,miy,max-mix,may-miy);
    wait;
  END;
  INC(Y);
  RETURN TRUE;
END down;

PROCEDURE drop;
  VAR i,j,x,y,mix,miy,max,may,dy: INTEGER; a: ADDRESS;
BEGIN
  mix:=800; miy:=300; max:=0; may:=0;
  dy:=0;
  WHILE check(X,Y+dy+1,R) DO INC(dy) END;
  IF dy=0 THEN RETURN END;
  FOR i:=0 TO 3 DO
    xy?(X,Y,R,i,x,y);
    x:=x*xsz; y:=y*ysz;
    IF x<mix THEN mix:=x END; IF x+xsz>max THEN max:=x+xsz END;
    IF y<miy THEN miy:=y END; IF y+ysz>may THEN may:=y+ysz END;
    ASSERT((y>=0)&(y<300)); ASSERT((x>=0)&(y<800));
    a:=ADR(layer[0]^[y]);
    FOR j:=0 TO ysz-1 DO BBP(a,dx+x,xsz,0); INC(a,25) END;
  END;
  Y:=Y+dy;
  FOR i:=0 TO 3 DO
    xy?(X,Y,R,i,x,y);
    ASSERT(NOT st[y][x]);
    x:=x*xsz; y:=y*ysz;
    IF x<mix THEN mix:=x END; IF x+xsz>max THEN max:=x+xsz END;
    IF y<miy THEN miy:=y END; IF y+ysz>may THEN may:=y+ysz END;
    ASSERT((y>=0)&(y<300)); ASSERT((x>=0)&(y<800));
    a:=ADR(layer[0]^[y]);
    FOR j:=0 TO ysz-1 DO BBP(a,dx+x,xsz,-1); INC(a,25) END;
  END;
  block(0,dx+mix,miy,max-mix,may-miy);
  INC(score,dy);
END drop;

PROCEDURE move;
  VAR x,y,i: INTEGER; ch,ch1: CHAR;
BEGIN
  time:=tim.sys_time(tim.tick);
  LOOP
    busy_read(ch);
    IF    ch='7' THEN left
    ELSIF ch='8' THEN rol
    ELSIF ch='9' THEN right
    ELSIF ch=' ' THEN drop
    ELSIF ch='p' THEN REPEAT read(ch1) UNTIL ch1='p';
      time:=tim.sys_time(tim.tick);
    ELSIF NOT down() THEN EXIT;
    END;
  END;
  FOR i:=0 TO 3 DO xy?(X,Y,R,i,x,y); st[y][x]:=TRUE END;
END move;

PROCEDURE MOVE(a,b: ADDRESS; s: INTEGER); CODE mcd.move END MOVE;

PROCEDURE del_line;
  PROCEDURE full?(y: INTEGER): BOOLEAN;
    VAR r: BOOLEAN; i: INTEGER;
  BEGIN
    r:=TRUE;
    FOR i:=0 TO HIGH(st[y]) DO r:=r & st[y][i] END;
    RETURN r;
  END full?;
  VAR i,j,y,n,cnt: INTEGER;
      zero: ARRAY [0..24] OF INTEGER;
      a,b: ADDRESS;
BEGIN
  FOR i:=0 TO HIGH(zero) DO zero[i]:=0 END;
  y:=0; cnt:=0;
  REPEAT
    IF full?(y) THEN
      n:=1; WHILE (n+y<=HIGH(st))& full?(y+n) DO INC(n) END;
      a:=ADR(layer[0]^[y*ysz]);
      b:=ADR(layer[0]^[(y+n)*ysz]);
      FOR i:=0 TO y*ysz-1 DO DEC(a,25); DEC(b,25); MOVE(b,a,25) END;
      a:=ADR(zero);
      FOR i:=0 TO n*ysz-1 DO DEC(b,25); MOVE(b,a,25) END;
      FOR i:=y+n-1 TO n BY -1 DO st[i]:=st[i-n] END;
      FOR i:=n-1 TO 0 BY -1 DO
        FOR j:=0 TO HIGH(st[i]) DO st[i][j]:=FALSE END;
      END;
      INC(y,n); INC(cnt); INC(score,n*10);
    ELSE
      INC(y);
    END;
  UNTIL y>HIGH(st);
  IF cnt>0 THEN block(0,dx,0,xsz*(HIGH(st[0])+1),ysz*(HIGH(st)+1)) END;
END del_line;

PROCEDURE prt(x,y: INTEGER; s: ARRAY OF CHAR);
  VAR i,l: INTEGER; a,b: ADDRESS;
BEGIN
  i:=0; a:=ADR(layer[1]^[y]);
  WHILE s[i]#0c DO
    b:=a;
    FOR l:=0 TO 11 DO BBP(b,x,10,font[s[i]]^[l]); INC(b,25) END;
    INC(x,10); INC(i);
  END;
  block(1,x-i*10,y,i*10,12);
END prt;

PROCEDURE chemp;
  VAR i,j: INTEGER; ln: ARRAY [0..15] OF CHAR;
      f: bio.FILE; nm: ARRAY [0..255] OF CHAR; ch: CHAR;
BEGIN
  prt(200,276,'Congratulations! You have best result.');
  prt(200,288,'Please, enter your name:');
  i:=0;
  LOOP
    IF i>=15 THEN EXIT END;
    read(ch);
    IF (ch>='A')&(ch<='Z') OR (ch>='a')&(ch<='z') THEN
      ln[i]:=ch; INC(i); ln[i]:=0c; prt(450,288,ln);
    END;
    IF ch=15c THEN EXIT END;
  END;
  prt(200,276,'                                      ');
  prt(200,288,'                                        ');
  IF i=0 THEN RETURN END;
  i:=0;
  WHILE record[i].cnt>=score DO INC(i) END;
  FOR j:=HIGH(record) TO i+1 BY -1 DO record[j]:=record[j-1] END;
  record[i].name:=ln; record[i].cnt:=score;
  nm:='tetris.best';
  bio.open(f,nm,'rw');
  ASSERT(bio.done);
  bio.write(f,ADR(record),BYTES(record));
  ASSERT(bio.done);
  bio.close(f);
  ASSERT(bio.done);
END chemp;

PROCEDURE game;
  VAR a: ADDRESS; i,j,k,l: INTEGER; ln: ARRAY [0..31] OF CHAR;
BEGIN
  a:=layer[0];
  FOR i:=0 TO 299 DO a^:=0; MOVE(ADDRESS(a+1),a,24); INC(a,25) END;
  a:=layer[1];
  FOR i:=0 TO 299 DO a^:=0; MOVE(ADDRESS(a+1),a,24); INC(a,25) END;
  a:=layer[1];
  FOR i:=0 TO HIGH(st)+1 DO
    IF i<=HIGH(st) THEN
      FOR j:=0 TO HIGH(st[i]) DO st[i][j]:=FALSE END;
    END;
    FOR j:=0 TO ysz-1 DO
      BBP(a,dx-10,10,-1);
      BBP(a,dx+xsz*(HIGH(st[i])+1),10,-1);
      IF i>HIGH(st) THEN
        FOR k:=0 TO HIGH(st[i]) DO BBP(a,dx+k*xsz,xsz,-1) END;
      END;
      INC(a,25);
    END;
  END;
  block(0,0,0,800,300);
  block(1,0,0,800,300);
  prt(600,20,'score'); score:=0;
  LOOP
    l:=0;
    image(ln,l,'%5d',score); prt(660,20,ln);
    R:=0; X:=3; Y:=2; N:=next() MOD 7;
    IF NOT drow() THEN EXIT END;
    move; del_line;
  END;
  IF score>record[HIGH(record)].cnt THEN chemp END;
  prt(20,0,'The best is:');
  FOR i:=0 TO HIGH(record) DO
    l:=0;
    image(ln,l,'%15s %4d',record[i].name,record[i].cnt);
    prt(0,i*12+14,ln);
  END;
  block(1,0,0,200,300);
  time:=tim.sys_time(tim.tick);
  FOR i:=0 TO 7 DO
    prt(300,280,'  Game OVER   ');
    INC(time,15); REPEAT UNTIL tim.sys_time(tim.tick)>=time;
    prt(300,280,'              ');
    INC(time,10); REPEAT UNTIL tim.sys_time(tim.tick)>=time;
  END;
END game;

PROCEDURE Readfont;
  VAR fnt: RECORD
             x,y,z: INTEGER;
             b  : ARRAY CHAR OF ARRAY [0..11] OF BITSET;
           END;
      f: bio.FILE; nm: ARRAY [0..255] OF CHAR; i: INTEGER;
BEGIN
  nm:='FONT10.fnt';
  bio.open(f,nm,'rw');
  ASSERT(bio.done);
  bio.read(f,ADR(fnt),BYTES(fnt));
  ASSERT(bio.done);
  bio.close(f);
  ASSERT(bio.done);
  ASSERT((fnt.x=10) & (fnt.y=12));
  FOR i:=0 TO 255 DO MOVE(font[CHAR(i)],ADR(fnt.b[CHAR(i)]),12) END;
  nm:='tetris.best';
  bio.open(f,nm,'rw');
  ASSERT(bio.done);
  bio.read(f,ADR(record),BYTES(record));
  ASSERT(bio.done);
  bio.close(f);
  ASSERT(bio.done);
END Readfont;

VAR i,j: INTEGER;
    fnt: ARRAY [0..256*SIZE(blk)-1] OF INTEGER;
    a  : ADDRESS;
    ch : CHAR;

BEGIN
  auto(FALSE);
  FOR i:=0 TO 255 DO font[CHAR(i)]:=ADR(fnt[i*SIZE(blk)]) END;
  Readfont;
  fgs[0][0].x:=0; fgs[0][0].y:=0;
  fgs[0][1].x:=2; fgs[0][1].y:=0;
  fgs[0][2].x:=4; fgs[0][2].y:=0;
  fgs[0][3].x:=6; fgs[0][3].y:=0;
  ctr[0].x:=3;    ctr[0].y:=1;

  fgs[1][0].x:=0; fgs[1][0].y:=0;
  fgs[1][1].x:=2; fgs[1][1].y:=0;
  fgs[1][2].x:=4; fgs[1][2].y:=0;
  fgs[1][3].x:=4; fgs[1][3].y:=2;
  ctr[1].x:=2;    ctr[1].y:=0;

  fgs[2][0].x:=0; fgs[2][0].y:=0;
  fgs[2][1].x:=2; fgs[2][1].y:=0;
  fgs[2][2].x:=4; fgs[2][2].y:=0;
  fgs[2][3].x:=0; fgs[2][3].y:=2;
  ctr[2].x:=2;    ctr[2].y:=0;

  fgs[3][0].x:=0; fgs[3][0].y:=0;
  fgs[3][1].x:=2; fgs[3][1].y:=0;
  fgs[3][2].x:=4; fgs[3][2].y:=0;
  fgs[3][3].x:=2; fgs[3][3].y:=2;
  ctr[3].x:=2;    ctr[3].y:=0;

  fgs[4][0].x:=0; fgs[4][0].y:=0;
  fgs[4][1].x:=2; fgs[4][1].y:=0;
  fgs[4][2].x:=0; fgs[4][2].y:=2;
  fgs[4][3].x:=2; fgs[4][3].y:=2;
  ctr[4].x:=1;    ctr[4].y:=1;

  fgs[5][0].x:=0; fgs[5][0].y:=0;
  fgs[5][1].x:=2; fgs[5][1].y:=0;
  fgs[5][2].x:=2; fgs[5][2].y:=2;
  fgs[5][3].x:=4; fgs[5][3].y:=2;
  ctr[5].x:=1;    ctr[5].y:=1;

  fgs[6][0].x:=0; fgs[6][0].y:=2;
  fgs[6][1].x:=2; fgs[6][1].y:=2;
  fgs[6][2].x:=2; fgs[6][2].y:=0;
  fgs[6][3].x:=4; fgs[6][3].y:=0;
  ctr[6].x:=1;    ctr[6].y:=1;

  LOOP
    prt(300,280,'Are you ready?');
    REPEAT read(ch) UNTIL ch='y';
    prt(300,280,'              ');
    game;
  END;
END tetris.
