MODULE robot; (* Dima 20-Jul-88. (c) KRONOS *)
              (* Идею подкинул Ned *)

                IMPORT  LabtamVDU;
                IMPORT  Keyboard, Terminal, Clock, Image, StdIO, Edit, SYSTEM;
                IMPORT  Args;

CONST lns=16; cls=lns; rest=lns;
      up = 0;  le = 1;
      dw = 3;  ri = 2;

TYPE fish = ( emp, ful, rob, fro, cur   -- fishka
            , aup, ale, ari, adw        -- arrow
            , wup, wle, wri, wdw        -- wall
            , sta);
     set  = SET OF fish;

TYPE Program = ARRAY [0..63] OF CHAR; -- [up..dw];
                --  5  4  3  2 | 1  0
                --  dw ri le up| from
     FIELD   = ARRAY [0..lns-1],[0..cls-1] OF fish;

VAR  field  : FIELD;

MODULE out;
                IMPORT  fish, set, lns, cls, rest, field;
                IMPORT  SYSTEM, StdIO;
FROM LabtamVDU  IMPORT  Lay, Line, layer;
                IMPORT  LabtamVDU;

EXPORT QUALIFIED showfish, showfield, showset, clear;

TYPE square = RECORD L,H: ARRAY [0..15] OF BITSET END;

VAR layH, layL: Lay;
    pict      : ARRAY fish OF square;

PROCEDURE showfish(f: fish; ln, cl: INTEGER);
  VAR sq: square; l: INTEGER;
BEGIN ln:=ln*16; sq:=pict[f];
  FOR l:=0 TO 15 DO
    layH^[l+ln][cl]:=sq.H[l];
    layL^[l+ln][cl]:=sq.L[l];
  END;
  LabtamVDU.block(0, cl*32, ln, 32, 16);
  LabtamVDU.block(1, cl*32, ln, 32, 16);
END showfish;

PROCEDURE showset(s: set; ln, cl: INTEGER);
  VAR sq: square; l: INTEGER; f: fish;
BEGIN
  sq:=pict[emp];
  WITH sq DO
    FOR f:=MIN(fish) TO MAX(fish) DO IF f IN s THEN
      FOR l:=0 TO 15 DO
        H[l]:=H[l]/pict[f].H[l];
        L[l]:=L[l]/pict[f].L[l];
      END;
    END END;
  END;
  ln:=ln*16;
  FOR l:=0 TO 15 DO
    layH^[l+ln][cl]:=layH^[l+ln][cl]/sq.H[l];
    layL^[l+ln][cl]:=layL^[l+ln][cl]/sq.L[l];
  END;
  LabtamVDU.block(0, cl*32, ln, 32, 16);
  LabtamVDU.block(1, cl*32, ln, 32, 16);
END showset;

PROCEDURE clear;
  VAR i: INTEGER; row: Line;
BEGIN
  LabtamVDU.auto(TRUE);
  FOR i:=0 TO HIGH(row)   DO   row[i]:={}  END;
  FOR i:=0 TO HIGH(layL^) DO layL^[i]:=row END;
  FOR i:=0 TO HIGH(layH^) DO layH^[i]:=row END;
  LabtamVDU.update;
  LabtamVDU.auto(FALSE);
END clear;

PROCEDURE showfield;
  VAR i,j: INTEGER;
BEGIN
  FOR i:=0 TO lns-1 DO FOR j:=0 TO cls-1 DO
    showfish(field[i,j], i,j)
  END END;
END showfield;

MODULE inipict;

                IMPORT  square, fish, pict;
FROM StdIO      IMPORT  Stream, LookUp, GetS, Close;

PROCEDURE mirror(VAR m: square; VAL p: square);
  VAR i,j: INTEGER;
BEGIN
  FOR i:=0 TO 15 DO
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
  FOR i:=0 TO 15 DO
    m.L[i]:=p.L[15-i]; m.H[i]:=p.H[15-i];
  END;
END bottup;

PROCEDURE string(VAR L,H: BITSET; VAL s: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  L:={}; H:={};
  i:=0;
  WHILE (i<32) & (s[i]#0c) DO
    IF    s[i]='.' THEN INCL(L,i);
    ELSIF s[i]=':' THEN            INCL(H,i);
    ELSIF s[i]='*' THEN INCL(L,i); INCL(H,i);
    END;
    INC(i);
  END;
END string;

VAR in: Stream;

PROCEDURE one(VAR p: square);
  VAR i,r: INTEGER; s: ARRAY [0..255] OF CHAR;
BEGIN
  REPEAT r:=GetS(in, s) UNTIL s[0]#'-';
  FOR i:=0 TO 15 DO
    string(p.L[i], p.H[i], s);
    r:=GetS(in, s);
  END;
END one;

VAR i: INTEGER;

BEGIN
  in:=LookUp('.robot.pict'); IF in<0 THEN HALT END;
  WITH pict[emp] DO FOR i:=0 TO 15 DO
    L[i]:={}; H[i]:={};
  END END;
  one(pict[ful]);
  one(pict[rob]);
  one(pict[fro]);
  one(pict[cur]);
  one(pict[ari]); mirror(pict[ale], pict[ari]);
  one(pict[aup]); bottup(pict[adw], pict[aup]);
  one(pict[wri]); mirror(pict[wle], pict[wri]);
  one(pict[wup]); bottup(pict[wdw], pict[wup]);
  one(pict[sta]);
  in:=Close(in);
END inipict;

BEGIN
  layH:=layer[1]; layL:=layer[0];
END out;

MODULE ini;
                IMPORT  out;
                IMPORT  field, fish, set, up, dw, le, ri, lns, cls;
FROM Image      IMPORT  image0;
FROM StdIO      IMPORT  LookUp, GetS, Close, Stream, EOF;

EXPORT QUALIFIED inifield;

VAR fi    : ARRAY CHAR OF fish;

PROCEDURE inifield(level: INTEGER; VAR startl,startc,startdir,frogs: INTEGER);
  VAR l,c,i: INTEGER;
      f    : fish;
      name,
      str  : ARRAY [0..79] OF CHAR;
      s    : Stream;
BEGIN
  image0(name, '.robot.%d', level);
  s:=LookUp(name); IF s<0 THEN HALT END;
  FOR l:=0 TO HIGH(field) DO FOR c:=0 TO HIGH(field[0]) DO
    field[l,c]:=emp
  END END;
  l:=0; frogs:=0;
  LOOP i:=GetS(s,str); IF i<=0 THEN EXIT END;
    c:=0;
    WHILE (c<cls)&(str[c]#0c) DO
      f:=fi[str[c]]; field[l,c]:=f;
      INC(frogs, INTEGER(f=fro));
      IF f IN set{aup..adw} THEN
        startl:=l; startc:=c; startdir:=INTEGER(f)-INTEGER(aup);
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
  fi['@']:=fro;
  fi['A']:=aup; fi['V']:=adw; fi['>']:=ari; fi['<']:=ale;
END ini;

MODULE loader;

                IMPORT  Program, fish, set, up, dw, le, ri, rest;
                IMPORT  Keyboard, out;

EXPORT QUALIFIED iniprogram, editprogram, showprogram;

VAR progno: ARRAY [0..8],[0..8] OF INTEGER;

PROCEDURE calcset(n: INTEGER): set;
  VAR ar: fish;
BEGIN
  ar:=fish(n MOD 4 + INTEGER(aup));
  RETURN set((n DIV 4)<<INTEGER(wup))+set{ar}
END calcset;

PROCEDURE iniprogno;

  CONST str   = '    7    '
                '   315   '
                '   264   '
                ' 31 0 15 '
                'd2b0 0b4f'
                ' ca 0 ae '
                '   264   '
                '   cae   '
                '    g    ';

  VAR i,j,n,ch: INTEGER;
BEGIN
  FOR i:=0 TO 8 DO FOR j:=0 TO 8 DO
    progno[i,j]:=-1;
    ch:=ORD(str[i*9+j]);
    IF ch#ORD(' ') THEN
      n:=ch-ORD('0'); IF NOT(n IN {0..7}) THEN n:=ch-ORD('a')+8 END;
      progno[i,j]:=n*4+INTEGER(i>j)+2*INTEGER(i+j>8);
    END;
  END END;
END iniprogno;

PROCEDURE showprogram(VAL p: Program);
  VAR i,j: INTEGER; n: INTEGER;
BEGIN
  FOR i:=0 TO 8 DO FOR j:=0 TO 8 DO n:=progno[i,j];
    IF n>=0 THEN
      out.showfish(emp, i, j+rest);
      out.showset(calcset(n-n MOD 4 + INTEGER(p[n])), i,j+rest);
    END
  END END;
  out.showfish(sta, 4, 4+rest);
END showprogram;

PROCEDURE iniprogram(VAR p: Program);
  VAR n: INTEGER;
BEGIN
  FOR n:=0 TO HIGH(p) DO p[n]:=CHAR(3-n MOD 4); END;
  showprogram(p);
END iniprogram;

PROCEDURE editprogram(VAR p: Program): INTEGER; -- +-1 to change level
  VAR l,c: INTEGER; ch: CHAR;

  PROCEDURE move(dl, dc: INTEGER);
  BEGIN
    out.showset(set{cur}, l,c+rest);
    INC(l,dl); INC(c,dc);
    IF (l<0) OR (c<0) OR (l>8) OR (c>8) THEN DEC(l,dl); DEC(c,dc); END;
    out.showset(set{cur}, l,c+rest);
  END move;

  PROCEDURE rotate;
    VAR n, old, new: INTEGER;
  BEGIN
    n:=progno[l,c];
    IF n<0 THEN RETURN END;
    old:=INTEGER(p[n]) MOD 4; new:=old;
    REPEAT new:=(new+1) MOD 4 UNTIL NOT(new+2 IN BITSET(n));
    IF new=old THEN RETURN END;
    p[n]:=CHAR(new);
    out.showset(set({old+INTEGER(aup), new+INTEGER(aup)}), l, c+rest);
  END rotate;

  VAR res: INTEGER;
BEGIN
  out.showset(set{cur}, 4, 4+rest); l:=4; c:=4;
  res:=0;
  LOOP
    ch:=Keyboard.ReadKey();
    CASE ch OF
     |Keyboard.up   : move(-1, 0)
     |Keyboard.dw   : move( 1, 0)
     |Keyboard.left : move( 0,-1)
     |Keyboard.right: move( 0, 1)
     |'5' : rotate;
     |'+' : res:=+1; EXIT
     |'-' : res:=-1; EXIT
     |' ' : EXIT
    ELSE END;
  END;
  out.showset(set{cur}, l, c+rest);
  RETURN res
END editprogram;

BEGIN iniprogno;
END loader;

MODULE runner;
                IMPORT  FIELD, field, fish, Program, up,dw,le,ri;
                IMPORT  out;
FROM Terminal   IMPORT  BusyRead, Read;
FROM Clock      IMPORT  miliClock;

EXPORT QUALIFIED runprogram;

PROCEDURE delay;
  VAR i: INTEGER;
BEGIN
  i:=miliClock()+200;
  REPEAT UNTIL miliClock()>=i;
END delay;

PROCEDURE runprogram(VAL program: Program; l,c,dir,frogs: INTEGER): INTEGER;

  PROCEDURE put(f: fish; l,c: INTEGER);
  BEGIN field[l,c]:=f; out.showfish(f, l,c);  END put;

  PROCEDURE calcmove(dir: INTEGER; VAR l,c: INTEGER);
  BEGIN
    CASE dir OF
     |up: DEC(l) |dw: INC(l)
     |le: DEC(c) |ri: INC(c)
    ELSE ASSERT(FALSE) END;
  END calcmove;

  PROCEDURE move(VAR l,c,dir: INTEGER);
    VAR no: BITSET;
  BEGIN
    no:=BITSET(dir);
    IF field[l-1, c]=ful THEN INCL(no, 2) END;
    IF field[l+1, c]=ful THEN INCL(no, 5) END;   --  5  4  3  2 | 1  0
    IF field[l, c-1]=ful THEN INCL(no, 3) END;   --  dw ri le up| from
    IF field[l, c+1]=ful THEN INCL(no, 4) END;
    dir:=INTEGER(program[INTEGER(no)]);
    calcmove(dir, l, c);
  END move;

  VAR res: INTEGER;
      startl,startc: INTEGER;
      startdir     : fish;
BEGIN
  startl:=l; startc:=c; startdir:=fish(INTEGER(aup)+dir);
  calcmove(dir, l, c); out.showfish(rob,l,c);
  put(startdir, startl, startc);
  res:=1;
  LOOP
--  IF Read()#' ' THEN res:=-1; EXIT END;
    IF BusyRead()#0c THEN res:=-1; EXIT END;
    IF field[l,c]=fro THEN field[l,c]:=emp; DEC(frogs) END;
    IF field[l,c]=startdir THEN
      IF frogs=0 THEN EXIT END;
    END;
    delay;
    out.showfish(emp, l,c); move(l,c, dir); out.showfish(rob, l,c);
    INC(res);
  END;
  RETURN res
END runprogram;

END runner;

MODULE recorder;
                IMPORT  Program;
FROM loader     IMPORT  showprogram;
FROM SYSTEM     IMPORT  ADR;
FROM StdIO      IMPORT  Stream, LookUp, Create, Seek, sRead, sWrite, Close
                      , Query;
FROM Terminal   IMPORT  print, SetCrs, ClearLine;

EXPORT Record?, ShowTable;

TYPE record = RECORD name: ARRAY [0..15] OF CHAR;
                     res : INTEGER;
                     prog: Program;
              END;

VAR recs: ARRAY [0..59] OF record;
    s   : Stream;

PROCEDURE read;
  VAR n: INTEGER;
BEGIN          s:=LookUp('.robot.best');
  IF s<0 THEN  s:=Create('.robot.best');
    FOR n:=0 TO HIGH(recs) DO recs[n].name:='' END;
  ELSE
    n:=sRead(s, ADR(recs), BYTES(recs)); n:=Seek(s,0,0);
  END;
END read;

PROCEDURE Record?(nm: ARRAY OF CHAR; level, steps: INTEGER; VAR p: Program);
  VAR n    : INTEGER;
      write: BOOLEAN;
      str  : ARRAY [0..79] OF CHAR;
BEGIN
  SetCrs(24,0);
  print('Level %2d: Your result %4d steps. ', level, steps);
  IF (nm='') OR (level<0) OR (level>HIGH(recs)) OR (steps<0) THEN RETURN END;
  read;
  write:=FALSE;
  WITH recs[level] DO
    IF (name='') OR (steps>res) THEN
      name:=nm; res:=steps; prog:=p; write:=TRUE;
      print(' - Champion!');
    ELSIF (steps<=res) OR (name#nm) THEN
      print(' Champion %8s, %4d steps. ', name, res);
      IF Query('Want to see?') THEN
        showprogram(prog); p:=prog;
      END;
      print('\r'); ClearLine;
    END;
  END;
  IF write THEN n:=sWrite(s, ADR(recs), BYTES(recs)); END;
  n:=Close(s);
END Record?;

PROCEDURE ShowTable;
  VAR l: INTEGER;
BEGIN
  read; l:=Close(s);
  FOR l:=0 TO HIGH(recs) DO IF recs[l].name#'' THEN
    print('Level %2d: Champion %8s, %4d steps\n', l, recs[l].name, recs[l].res);
  END END;
END ShowTable;

END recorder;

VAR name : ARRAY [0..15] OF CHAR;
    level: INTEGER;

PROCEDURE play;
  VAR res, startl, startc, startdir, frogs: INTEGER; program: Program;
BEGIN
  out.clear;
  loader.iniprogram(program);
  LOOP
    ini.inifield(level, startl,startc,startdir, frogs);
    res:=loader.editprogram(program);
    IF res#0 THEN INC(level, res); RETURN END;
    res:=runner.runprogram(program, startl,startc,startdir, frogs);
    IF res>0 THEN Record?(name, level, res, program) END;
  END;
END play;

BEGIN
  LabtamVDU.auto(FALSE);
  IF Args.Num?(level) THEN level:=0; END;
  IF level=0 THEN Terminal.Home; ShowTable END;
  Terminal.SetCrs(24,0);
  Edit.ReadString('Your name? ', name);
  LOOP play END;
END robot.
