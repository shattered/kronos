MODULE udav; (* Dima 10-Apr-88. (c) KRONOS *)
             (* Сперто с Ямахи             *)

                IMPORT  SYSTEM;
                IMPORT  LabtamVDU;
                IMPORT  Keyboard, Terminal, Clock, Image, StdIO, Edit, Args;
                IMPORT  Resource;

CONST lns=35; cls=50;
      up = -cls;  le = -1;
      dw =  cls;  ri = +1;

TYPE fish = (emp, ful, gat
           , hup, hdw, hle, hri            --  Head
           , tup, tdw, tle, tri            --  Tail
           , bho, bve, bul, bdl, bur, bdr  --  Body
           , fro, sto, poi);
     set  = SET OF fish;

TYPE FIELD = ARRAY [0..lns*cls-1] OF fish;

VAR  field: FIELD;

MODULE out;
                IMPORT  fish, set, lns, cls, dw, field;
                IMPORT  SYSTEM, StdIO;
FROM SYSTEM     IMPORT  WORD, ADDRESS, ADR;
FROM LabtamVDU  IMPORT  Line, Lay, layer, auto, block;

EXPORT QUALIFIED showfish, showfield;

TYPE square = RECORD L,H: ARRAY [0..7] OF BITSET END; -- doubled picture

VAR layH, layL: Lay;
    pict      : ARRAY fish OF square;

PROCEDURE showfish0(f: fish; pos: INTEGER);
  VAR l,c,c2,i: INTEGER; mask: BITSET; pH,pL: POINTER TO BITSET;
BEGIN
  l:=pos DIV dw * 8 + 5; c:=pos MOD dw ; c2:=c DIV 2;
  IF ODD(c) THEN mask:={16..31} ELSE mask:={0..15} END;
  FOR i:=0 TO 7 DO
    pH:=ADR(layH^[l+i,c2]);            pL:=ADR(layL^[l+i,c2]);
    pH^:=(pH^-mask)+pict[f].H[i]*mask; pL^:=(pL^-mask)+pict[f].L[i]*mask;
  END;
END showfish0;

PROCEDURE showfish(f: fish; pos: INTEGER);
  VAR l,c2: INTEGER;
BEGIN
  l:=pos DIV dw * 8 + 5; c2:=pos MOD dw DIV 2;
  showfish0(f, pos);
  block(0, c2*32, l, 32, 8); block(1, c2*32, l, 32, 8);
END showfish;

PROCEDURE showfield;
  VAR i,j: INTEGER; row: Line;
BEGIN
  FOR i:=0 TO HIGH(row) DO row[i]:={} END;
  FOR i:=0 TO HIGH(layH^) DO layH^[i]:=row; layL^[i]:=row END;
  FOR i:=0 TO HIGH(field) DO IF field[i]#emp THEN
    showfish0(field[i], i)
  END END;
  block(0, 0, 0, 800, 300); block(1, 0, 0, 800, 300);
END showfield;

MODULE inipict;

                IMPORT  square, fish, pict;
FROM StdIO      IMPORT  Stream, LookUp, GetS, Close;

PROCEDURE mirror(VAR m: square; VAL p: square);
  VAR i,j: INTEGER;
BEGIN
  FOR i:=0 TO 7 DO
    m.L[i]:={}; m.H[i]:={};
    FOR j:=0 TO 31 DO
      IF j IN p.L[i] THEN INCL(m.L[i], 31-j) END;
      IF j IN p.H[i] THEN INCL(m.H[i], 31-j) END;
    END;
  END;
END mirror;

PROCEDURE bottup(VAR m: square; VAL p: square);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO 7 DO
    m.L[i]:=p.L[7-i]; m.H[i]:=p.H[7-i];
  END;
END bottup;

PROCEDURE string(VAR L,H: BITSET; VAL s: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  L:={}; H:={};
  i:=0;
  WHILE (s[i]#0c) & (i<16) DO
    IF    s[i]='.' THEN INCL(L,i);
    ELSIF s[i]=':' THEN            INCL(H,i);
    ELSIF s[i]='*' THEN INCL(L,i); INCL(H,i);
    END;
    INC(i);
  END;
  L:=L+BITSET(L<<16); H:=H+BITSET(H<<16);
END string;

VAR in: Stream;

PROCEDURE one(VAR p: square);
  VAR i,r: INTEGER; s: ARRAY [0..31] OF CHAR;
BEGIN
  REPEAT r:=GetS(in, s) UNTIL s[0]#'-';
  FOR i:=0 TO 7 DO
    string(p.L[i], p.H[i], s);
    r:=GetS(in, s);
  END;
END one;

VAR i: INTEGER;

BEGIN
  in:=LookUp('.udav.pict'); IF in<0 THEN HALT END;
  WITH pict[emp] DO FOR i:=0 TO 7 DO
    L[i]:={}; H[i]:={};
  END END;
  one(pict[ful]);
  one(pict[gat]);
  one(pict[hri]); mirror(pict[hle], pict[hri]);
  one(pict[hup]); bottup(pict[hdw], pict[hup]);
  one(pict[tri]); mirror(pict[tle], pict[tri]);
  one(pict[tup]); bottup(pict[tdw], pict[tup]);
  one(pict[bho]); one(pict[bve]);
  one(pict[bul]); bottup(pict[bdl], pict[bul]);
                  mirror(pict[bur], pict[bul]);
                  mirror(pict[bdr], pict[bdl]);
  one(pict[fro]);
  one(pict[sto]);
  one(pict[poi]);
  in:=Close(in);
END inipict;

BEGIN layL:=layer[0]; layH:=layer[1];
END out;

MODULE ini;
                IMPORT  out;
                IMPORT  field, fish, set, up, dw, le, ri, lns, cls;
FROM Image      IMPORT  image0;
FROM StdIO      IMPORT  LookUp, GetS, Close, Stream, EOF;

EXPORT QUALIFIED inifield;

VAR fi    : ARRAY CHAR OF fish;

PROCEDURE inifield(level: INTEGER; VAR head,tail,frogs,gate: INTEGER);
  TYPE row=ARRAY [0..cls-1] OF fish;
       fld=ARRAY [0..lns-1] OF row;
  VAR l,c,i: INTEGER;
      f    : fish;
      name,
      str  : ARRAY [0..79] OF CHAR;
      s    : Stream;
BEGIN
  image0(name, '.udav.%d', level);
  s:=LookUp(name); IF s<0 THEN HALT END;
  FOR i:=0 TO HIGH(field) DO field[i]:=emp END;
  frogs:=0;
  l:=0;
  LOOP
    c:=0; i:=GetS(s,str); IF i<=0 THEN EXIT END;
    WHILE (c<cls)&(str[c]#0c) DO
      f:=fi[str[c]]; fld(field)[l,c]:=f;
      IF f=fro THEN INC(frogs) END;
      IF f IN set{hup..hri} THEN head:=l*dw+c END;
      IF f IN set{tup..tri} THEN tail:=l*dw+c END;
      IF f = gat THEN gate:=l*dw+c; field[gate]:=ful END;
      INC(c);
    END;
    INC(l); IF l>=lns THEN EXIT END;
  END;
  s:=Close(s);
  out.showfield;
END inifield;

VAR c: CHAR;

BEGIN
  FOR c:=0c TO 377c DO fi[c]:=emp END;
  fi[' ']:=emp; fi['Ш']:=ful; fi['=']:=gat;
  fi['q']:=bul; fi['d']:=bdl; fi['p']:=bur; fi['b']:=bdr;
  fi['-']:=bho; fi['|']:=bve;
  fi['U']:=tup; fi['П']:=tdw; fi['[']:=tri; fi[']']:=tle;
  fi['A']:=hup; fi['V']:=hdw; fi['>']:=hri; fi['<']:=hle;
  fi['@']:=fro; fi['O']:=sto; fi['x']:=poi;
END ini;

MODULE moving;
                IMPORT  out;
                IMPORT  fish, set, field, up, dw, le, ri;

EXPORT QUALIFIED movhead, movtail, result;

TYPE result = (ok, fail, eaten, killed);

VAR  info: ARRAY fish OF INTEGER;
   -- body: dir change; head & tail: dir

PROCEDURE movtail(VAR p: INTEGER);
  VAR dir: INTEGER; f: fish;
BEGIN
  dir:=info[field[p]];
  out.showfish(emp, p); field[p]:=emp;
  INC(p,dir);
  INC(dir, info[field[p]]);
  FOR f:=tup TO tri DO IF info[f]=dir THEN
    out.showfish(f, p); field[p]:=f; RETURN
  END END;
END movtail;

PROCEDURE calc(dold,dnew: INTEGER): fish;
  VAR f: fish; ddir: INTEGER;
BEGIN
  ddir:=dnew-dold;
  IF ddir=0 THEN
    IF ABS(dold)=ABS(ri) THEN RETURN bho ELSE RETURN bve END;
  END;
  FOR f:=bul TO bdr DO
    IF info[f]=ddir THEN RETURN f END;
  END;
END calc;

PROCEDURE movhead(VAR p: INTEGER; dnew: INTEGER): result;
  CONST go = set{emp,gat,fro,sto,poi};
  VAR next,trace,f: fish;
BEGIN
  next:=field[p+dnew];
  IF NOT(next IN go) OR (next=sto)&(field[p+2*dnew]#emp)
  THEN RETURN fail END;
  trace:=calc(info[field[p]], dnew);
  out.showfish(trace, p); field[p]:=trace; INC(p, dnew);
  FOR f:=hup TO hri DO IF info[f]=dnew THEN
    out.showfish(f, p); field[p]:=f;
    IF next=fro THEN RETURN eaten  END;
    IF next=poi THEN RETURN killed END;
    IF next=sto THEN out.showfish(sto, p+dnew); field[p+dnew]:=sto END;
    RETURN ok
  END END;
END movhead;

BEGIN
  info[hup]:=up; info[hdw]:=dw; info[hle]:=le; info[hri]:=ri;
  info[tup]:=up; info[tdw]:=dw; info[tle]:=le; info[tri]:=ri;
  info[bho]:=0;  info[bve]:=0;
  info[bul]:=le-up; info[bur]:=ri-up;
  info[bdl]:=le-dw; info[bdr]:=ri-dw;
END moving;

MODULE player;
                IMPORT  out, Keyboard;
                IMPORT  field, fish, set, up, dw, le, ri, lns, cls;
FROM ini        IMPORT  inifield;
FROM moving     IMPORT  movhead, movtail, result;
FROM SYSTEM     IMPORT  ADDRESS, ADR;
FROM Keyboard   IMPORT  Pressed?, ReadKey;
FROM Clock      IMPORT  miliClock;

EXPORT play;

PROCEDURE play(VAR level: INTEGER): INTEGER;

  PROCEDURE readmove(VAR dir: INTEGER): BOOLEAN;
    CONST UP=Keyboard.up;   DW=Keyboard.dw;
          LE=Keyboard.left; RI=Keyboard.right;
  BEGIN
--  IF NOT Pressed?() THEN RETURN FALSE END;
    CASE ReadKey() OF
     |UP : dir:=up
     |DW : dir:=dw
     |LE : dir:=le
     |RI : dir:=ri
     |'5': dir:=0
     |' ': RETURN TRUE
     |'+': INC(level); RETURN TRUE
     |'-': DEC(level); RETURN TRUE
    ELSE dir:=0 END;
    RETURN FALSE
  END readmove;

  VAR head, tail, hdir, frogs, gate: INTEGER;
      steps: INTEGER;
      res  : result;
      i    : INTEGER;
BEGIN
  inifield(level, head,tail,frogs,gate); hdir:=0;
  steps:=0;
  LOOP
    i:=miliClock()+120;
    REPEAT UNTIL miliClock()>i;
    IF readmove(hdir) THEN RETURN -1 END;
    IF hdir#0 THEN
      res:=movhead(head, hdir);
      IF res=ok     THEN movtail(tail); INC(steps); END;
      IF res=eaten  THEN DEC(frogs);    INC(steps);
        IF frogs=0 THEN field[gate]:=gat; out.showfish(gat, gate) END;
      END;
      IF res=killed THEN RETURN -1    END;
      IF head=gate  THEN RETURN steps END;
    END;
  END;
END play;

END player;

MODULE recorder;

FROM SYSTEM     IMPORT  ADR;
FROM StdIO      IMPORT  Stream, LookUp, Create, Seek, sRead, sWrite, Close;
FROM Terminal   IMPORT  print, SetCrs;
FROM Keyboard   IMPORT  ReadKey;

EXPORT Record?;

PROCEDURE Record?(nm: ARRAY OF CHAR; level, steps: INTEGER);

  TYPE record = RECORD name: ARRAY [0..15] OF CHAR; res: INTEGER END;

  VAR recs: ARRAY [0..59] OF record;
      s   : Stream;
      n   : INTEGER;
     write: BOOLEAN;
BEGIN
  SetCrs(23,0);
  print('Level %2d: Your result %4d steps\n', level, steps);
  IF (nm='') OR (level<0) OR (level>HIGH(recs)) OR (steps<0) THEN RETURN END;
  write:=FALSE;
  s:=LookUp('.udav.best');
  IF s<0 THEN
    s:=Create('.udav.best');
    FOR n:=0 TO HIGH(recs) DO recs[n].name:='' END;
    write:=TRUE;
  ELSE
    n:=sRead(s, ADR(recs), BYTES(recs)); n:=Seek(s,0,0);
  END;
  WITH recs[level] DO
    IF (name='') OR (steps<res) THEN name:=nm; res:=steps; write:=TRUE END;
    print('Level %2d: Champion %8s, %4d steps', level, name, res);
  END;
  IF ReadKey()=0c THEN END;
  IF write THEN n:=sWrite(s, ADR(recs), BYTES(recs)); END;
  n:=Close(s);
END Record?;

END recorder;

VAR level, steps: INTEGER;
    name        : ARRAY [0..15] OF CHAR;

BEGIN
  LabtamVDU.auto(FALSE);
  IF Args.Num?(level) THEN level:=0; END;
  Terminal.Home; Edit.ReadString('Your name? ', name);
  LOOP
    steps:=play(level);
    IF steps>=0 THEN Record?(name, level, steps) END;
  END;
END udav.
