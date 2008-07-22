MODULE cave; (* Dima 10-Apr-88. (c) KRONOS *)

                IMPORT  SYSTEM;
                IMPORT  LabtamVDU;
                IMPORT  Keyboard, Terminal, Clock, Time
                      , Image, StdIO, Edit, Args;

CONST lns=35; cls=50;
      up = -cls;  le = -1;
      dw =  cls;  ri = +1;

TYPE fish = (emp, ful, poi
           , hup, hdw, hle, hri   --  Head
           , sto, stp);
     set  = SET OF fish;

TYPE FIELD = ARRAY [0..lns*cls-1] OF fish;

VAR  field: FIELD;
     head : INTEGER;    -- Head position
     hfish: fish;       -- Head fish

MODULE out;
                IMPORT  fish, set, lns, cls, dw, field, head, hfish;
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
  showfish0(hfish, head);
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
  VAR i,r: INTEGER; s: ARRAY [0..255] OF CHAR;
BEGIN
  REPEAT r:=GetS(in, s) UNTIL s[0]#'-';
  FOR i:=0 TO 7 DO
    string(p.L[i], p.H[i], s);
    r:=GetS(in, s);
  END;
END one;

VAR i: INTEGER;

BEGIN
  in:=LookUp('.cave.pict'); IF in<0 THEN HALT END;
  WITH pict[emp] DO FOR i:=0 TO 7 DO
    L[i]:={}; H[i]:={};
  END END;
  one(pict[ful]);
  one(pict[hri]); mirror(pict[hle], pict[hri]);
  one(pict[hup]); bottup(pict[hdw], pict[hup]);
  one(pict[sto]);
  one(pict[poi]);
  one(pict[stp]);
  in:=Close(in);
END inipict;

BEGIN layH:=layer[1]; layL:=layer[0];
END out;

MODULE ini;
                IMPORT  out;
                IMPORT  field, head, hfish
                      , fish, set, up, dw, le, ri, lns, cls;
FROM Image      IMPORT  image0;
FROM StdIO      IMPORT  LookUp, GetS, Close, Stream, EOF;

EXPORT QUALIFIED inifield;

VAR fi    : ARRAY CHAR OF fish;

PROCEDURE inifield(level: INTEGER; VAR stones: INTEGER);
  TYPE row=ARRAY [0..cls-1] OF fish;
       fld=ARRAY [0..lns-1] OF row;
  VAR l,c,i: INTEGER;
      f    : fish;
      name,
      str  : ARRAY [0..79] OF CHAR;
      s    : Stream;
BEGIN
  image0(name, '.cave.%d', level);
  s:=LookUp(name); IF s<0 THEN HALT END;
  FOR i:=0 TO HIGH(field) DO field[i]:=emp END;
  stones:=0;
  l:=0;
  LOOP
    c:=0; i:=GetS(s,str); IF i<=0 THEN EXIT END;
    WHILE (c<cls)&(str[c]#0c) DO
      f:=fi[str[c]];
      IF f=sto THEN INC(stones) END;
      IF f IN set{hup..hri} THEN head:=l*dw+c; hfish:=f
      ELSE                       fld(field)[l,c]:=f;
      END;
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
  fi[' ']:=emp; fi['Ш']:=ful;
  fi['A']:=hup; fi['V']:=hdw; fi['>']:=hri; fi['<']:=hle;
  fi['O']:=sto; fi['.']:=poi; fi['@']:=stp;
END ini;

MODULE moving;
                IMPORT  out;
                IMPORT  fish, set, field, head, hfish, up, dw, le, ri;

EXPORT QUALIFIED movhead, result;

TYPE result = (ok, fail, inc, dec);

PROCEDURE movhead(dnew: INTEGER): result;
  CONST ston=set{sto, stp}; empt=set{emp, poi};
  VAR next,next2,f: fish;
      change      : INTEGER;
BEGIN
  next:=field[head+dnew];
  IF (next=ful) OR (next IN ston) & NOT(field[head+2*dnew] IN empt)
  THEN RETURN fail END;
  out.showfish(field[head], head); INC(head, dnew);
  IF    dnew=up THEN hfish:=hup
  ELSIF dnew=dw THEN hfish:=hdw
  ELSIF dnew=le THEN hfish:=hle
  ELSIF dnew=ri THEN hfish:=hri
  END;
  out.showfish(hfish, head);
  IF next IN ston THEN
    change:=0;
    IF next=stp THEN field[head]:=poi; DEC(change);
    ELSE (*next=sto*)field[head]:=emp;
    END;
    next2:=field[head+dnew];
    IF next2=poi THEN next2:=stp; INC(change);
    ELSE              next2:=sto;
    END;
    out.showfish(next2, head+dnew); field[head+dnew]:=next2;
    IF    change>0 THEN RETURN dec
    ELSIF change<0 THEN RETURN inc
    END;
  END;
  RETURN ok
END movhead;

END moving;

MODULE player;
                IMPORT  out, Keyboard, Terminal;
                IMPORT  field, head, hfish
                      , fish, set, up, dw, le, ri, lns, cls;
FROM ini        IMPORT  inifield;
FROM moving     IMPORT  movhead, result;
FROM SYSTEM     IMPORT  ADDRESS, ADR;
FROM Keyboard   IMPORT  Pressed?, ReadKey;
FROM Clock      IMPORT  miliClock;

EXPORT play, inimacros;

TYPE macro = RECORD
               len: INTEGER;
               txt: ARRAY [0..255] OF INTEGER;
             END;

CONST max=25;

VAR mcr     : ARRAY [0..max] OF macro;
    from, to: INTEGER;
    nfr     : INTEGER;

PROCEDURE inimacros;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(mcr) DO mcr[i].len:=0; END;
  from:=-1; nfr:=0; to:=-1;
END inimacros;

PROCEDURE from?(VAR dir: INTEGER): BOOLEAN;
BEGIN
  IF (from<0) OR (from>HIGH(mcr)) OR (from=to) THEN
    nfr:=0; from:=-1; RETURN FALSE
  END;
  WITH mcr[from] DO
    IF nfr>=len THEN nfr:=0; from:=-1; RETURN FALSE END;
    dir:=txt[nfr]; INC(nfr); RETURN TRUE
  END;
END from?;

PROCEDURE to?(dir: INTEGER);
BEGIN
  IF (to<0) OR (to>HIGH(mcr)) THEN RETURN END;
  WITH mcr[to] DO
    IF len>HIGH(txt) THEN to:=-1; RETURN END;
    txt[len]:=dir; INC(len);
  END;
END to?;

PROCEDURE play(VAR level: INTEGER): INTEGER;

  PROCEDURE readmove(VAR dir: INTEGER): BOOLEAN;
    CONST UP=Keyboard.up;   DW=Keyboard.dw;
          LE=Keyboard.left; RI=Keyboard.right;
    VAR ch: CHAR; n: INTEGER;
  BEGIN
    IF from?(dir) THEN RETURN FALSE END;
    ch:=ReadKey();
    CASE ch OF
     |UP : dir:=up
     |DW : dir:=dw
     |LE : dir:=le
     |RI : dir:=ri
     |'[': ch:=ReadKey();
           n:=ORD(ch)-ORD('a');
           IF (n>=0) & (n<=HIGH(mcr)) THEN
             to:=n; mcr[to].len:=0;
             Terminal.SetCrs(24,40); Terminal.print('MACRO: %c\r', ch);
           END;
           dir:=0; RETURN FALSE
     |']': to:=-1; dir:=0;
           Terminal.SetCrs(24,40); Terminal.ClearLine; Terminal.SetCrs(24,0);
     |'+': INC(level); inimacros; RETURN TRUE
     |'-': DEC(level); inimacros; RETURN TRUE
     |' ': RETURN TRUE
    ELSE
      n:=ORD(ch)-ORD('a');
      IF (n>=0) & (n<=HIGH(mcr)) THEN from:=n; nfr:=0; END;
      dir:=0;
    END;
    RETURN FALSE
  END readmove;

  VAR stones, dir, steps: INTEGER;
      res   : result;
      i     : INTEGER;
BEGIN
  inifield(level, stones); dir:=0;
  steps:=0;
  LOOP
    IF readmove(dir) THEN RETURN -1 END;
    IF dir#0 THEN
      res:=movhead(dir);
      IF res=inc  THEN INC(stones); END;
      IF res=dec  THEN DEC(stones); END;
      IF res=fail THEN dir:=0
      ELSE INC(steps); Terminal.print('%5d\r', steps)
      END;
    END;
    IF dir#0 THEN to?(dir); END;
    IF stones=0 THEN RETURN steps END;
  END;
END play;

END player;

MODULE recorder;

FROM SYSTEM     IMPORT  ADR;
FROM StdIO      IMPORT  Stream, LookUp, Create, Seek, sRead, sWrite, Close
                      , Query;
FROM Keyboard   IMPORT  ReadKey;
FROM Clock      IMPORT  SystemClock;
FROM Time       IMPORT  AppTime;
FROM Image      IMPORT  image0;
                IMPORT  Terminal;

EXPORT Record?, ShowTable;

TYPE record = RECORD name: ARRAY [0..15] OF CHAR;
                     res : INTEGER;
                     date: INTEGER;
              END;

VAR recs: ARRAY [0..59] OF record;
    s   : Stream;

PROCEDURE read;
  VAR n: INTEGER;
BEGIN          s:=LookUp('.cave.best');
  IF s<0 THEN  s:=Create('.cave.best');
    FOR n:=0 TO HIGH(recs) DO recs[n].name:='' END;
  ELSE
    n:=sRead(s, ADR(recs), BYTES(recs)); n:=Seek(s,0,0);
  END;
END read;

PROCEDURE showrec(l: INTEGER; VAR s: ARRAY OF CHAR);
BEGIN
  WITH recs[l] DO
    IF name='' THEN s:=''; RETURN END;
    image0(s, 'Level %2d: Champion %8s, %4d steps', l, name, res);
    AppTime(s, ' (%d %m3 %y)', date);
  END;
END showrec;

PROCEDURE Record?(nm: ARRAY OF CHAR; level, steps: INTEGER);
  VAR n    : INTEGER;
      write: BOOLEAN;
      str  : ARRAY [0..79] OF CHAR;
BEGIN
  Terminal.SetCrs(23,0);
  Terminal.print('Level %2d: Your result %4d steps\n', level, steps);
  IF (nm='') OR (level<0) OR (level>HIGH(recs)) OR (steps<0) THEN RETURN END;
  read;
  write:=FALSE;
  WITH recs[level] DO
    IF (name='') OR (steps<res) THEN
      name:=nm; res:=steps;
      date:=SystemClock();
      write:=TRUE;
    END;
  END;
  showrec(level,str); IF str#'' THEN Terminal.print(str) END;
  IF write THEN n:=sWrite(s, ADR(recs), BYTES(recs)); END;
  n:=Close(s);
  IF Terminal.Read()=0c THEN END;
  Terminal.SetCrs(24,0);
END Record?;

PROCEDURE ShowTable;
  VAR l,n: INTEGER; str: ARRAY [0..79] OF CHAR;
BEGIN
  read; l:=Close(s);
  Terminal.Home; n:=0;
  FOR l:=0 TO HIGH(recs) DO
    showrec(l,str);
    IF str#'' THEN
      Terminal.print('%s\n', str); INC(n);
      IF n=24 THEN n:=0;
        IF Query('Continue?') THEN Terminal.Home; Terminal.Clear;
        ELSE RETURN END;
      END;
    END;
  END;
  IF ReadKey()#0c THEN END;
END ShowTable;

END recorder;

VAR level, steps: INTEGER;
    name        : ARRAY [0..15] OF CHAR;

BEGIN
  LabtamVDU.auto(FALSE);
  IF Args.Num?(level) THEN level:=0; END;
  Terminal.Home; Edit.ReadString('Your name? ', name);
  IF level=0 THEN ShowTable END;
  Terminal.SetCrs(24,0);
  inimacros;
  LOOP
    steps:=play(level);
    IF steps>=0 THEN Record?(name, level, steps); END;
  END;
END cave.
