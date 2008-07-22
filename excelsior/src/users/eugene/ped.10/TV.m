IMPLEMENTATION MODULE TV; (* Leo 06-Nov-87. (c) KRONOS *)

IMPORT Terminal;
FROM Terminal   IMPORT  SetCrs, Write, Cursor;
FROM Resource   IMPORT  Final;
FROM Image      IMPORT  image0, image;
FROM VIDEO      IMPORT  add, rep, com
                      , Lines, Dots, Colors, Color, Mode, Cursors
                      , dotszX, dotszY, charszX, charszY, rollX, rollY;

VAR X,Y: INTEGER;
    Activ: BOOLEAN;

CONST writestr=Terminal.WriteString;

            Y0=00;

PROCEDURE wc(i: CARDINAL; last: CHAR);
  VAR s: ARRAY [0..15] OF CHAR;
     co: INTEGER;

  PROCEDURE wdig(i: CARDINAL);
    VAR j: INTEGER;
  BEGIN
    IF i=0 THEN RETURN END;
    IF i<10 THEN
      s[co]:=CHAR(ORD('0') + i); INC(co); RETURN
    END;
    wdig(i DIV 10);
    s[co]:=CHAR(ORD('0') + i MOD 10); INC(co);
  END wdig;

BEGIN co:=0;  ASSERT(i>=0);
  IF i=0 THEN s[0]:='0'; co:=1
  ELSE        wdig(i);
  END; s[co]:=last; INC(co); s[co]:=0c;  writestr(s);
END wc;

PROCEDURE wi(i: INTEGER; last: CHAR);
BEGIN
  IF i>=0 THEN Write('+') ELSE Write('-') END; wc(ABS(i),last);
END wi;

PROCEDURE color(i: INTEGER);
BEGIN
  IF (NOT Activ)OR(i=Color) THEN RETURN END;
  Color:=i;
  CASE i OF
   |0: writestr("W(I0)");
   |1: writestr("W(I1)");
   |2: writestr("W(I2)");
   |3: writestr("W(I3)");
  END;
END color;

PROCEDURE patt(n: INTEGER);
BEGIN
  CASE n OF
   |0: writestr("W(P0)");
   |1: writestr("W(P1)");
   |2: writestr("W(P2)");
   |3: writestr("W(P3)");
   |4: writestr("W(P4)");
   |5: writestr("W(P5)");
   |6: writestr("W(P6)");
   |7: writestr("W(P7)");
  END;
END patt;

PROCEDURE mode(m: INTEGER);
BEGIN
  IF (NOT Activ)OR(m=Mode) THEN RETURN END;
  Mode:=m;
  CASE Mode OF
   |com: writestr("W(C)")
   |rep: writestr("W(R)")
   |add: writestr("W(V)")
  END;
END mode;

PROCEDURE pos(x,y: INTEGER);
BEGIN
  ASSERT((0<=x) & (x<Dots));    ASSERT((0<=y) & (y<Lines));
  IF NOT Activ OR((x=X)&(y=Y)) THEN RETURN END;
  IF    x=X THEN writestr("P[,");
    IF ABS(Y-y)<5  THEN wi((Y-y)*2,"]") ELSE wc((Lines-y)*2+Y0,"]") END;
  ELSIF y=Y THEN writestr("P[");
    IF ABS(X-x)<10 THEN wi(x-X,"]")     ELSE wc(x,"]")           END;
  ELSE           writestr("P[");
    IF ABS(X-x)<10 THEN wi(x-X,",")     ELSE wc(x,",")           END;
    IF ABS(Y-y)<5  THEN wi((Y-y)*2,"]") ELSE wc((Lines-y)*2+Y0,"]") END;
  END;
  X:=x; Y:=y;
END pos;

VAR stk: ARRAY [0..7] OF INTEGER;
     sp: INTEGER;

PROCEDURE push;
BEGIN
  IF NOT Activ THEN RETURN END;
  IF sp<HIGH(stk) THEN writestr("P(B)");
    stk[sp]:=X+Y*10000h; INC(sp);
  END;
END push;

PROCEDURE pop;
BEGIN
  IF NOT Activ THEN RETURN END;
  IF sp>0 THEN writestr("P(E)");
    DEC(sp); X:=stk[sp]; Y:=X DIV 10000h; X:=X MOD 10000h;
  END;
END pop;

PROCEDURE vect(x,y: INTEGER);
BEGIN
  IF NOT Activ THEN RETURN END;
  ASSERT((0<=x) & (x<Dots)); ASSERT((0<=y) & (y<Lines));
  IF (x=X) & (y=Y) THEN writestr("V[]"); RETURN END;
  IF    x=X THEN writestr("V[,");
    IF ABS(Y-y)<5  THEN wi((Y-y)*2,"]") ELSE wc((Lines-y)*2+Y0,"]") END;
  ELSIF y=Y THEN writestr("V[");
    IF ABS(X-x)<10 THEN wi(x-X,"]")     ELSE wc(x,"]")           END;
  ELSE           writestr("V[");
    IF ABS(X-x)<10 THEN wi(x-X,",")     ELSE wc(x,",")           END;
    IF ABS(Y-y)<5  THEN wi((Y-y)*2,"]") ELSE wc((Lines-y)*2+Y0,"]") END;
  END;
  X:=x; Y:=y;
END vect;

PROCEDURE circ(r: INTEGER);
BEGIN
  IF NOT Activ THEN RETURN END;
  writestr("C["); wi(r,"]");
END circ;

PROCEDURE rect(x,y: INTEGER);
BEGIN
  IF NOT Activ THEN RETURN END;
  writestr("W(S[");
  IF ABS(X-x)<10 THEN wi(x-X,"]")     ELSE wc(x,"]")           END;
  writestr(")V[,");
  IF ABS(Y-y)<5  THEN wi((Y-y)*2,"]") ELSE wc((Lines-y)*2+Y0,"]") END;
  writestr("W(S0)");
  Y:=y;
END rect;

PROCEDURE gotoRegis;
  VAR m,c,x,y: INTEGER;
BEGIN
  SetCrs(0,0);
  writestr(''33c'P');
  x:=X;    y:=Y;     m:=Mode;  c:=Color;
  X:=-999; Y:=-999;  Mode:=-1; Color:=-1;
  pos(x,y); mode(m); color(c);
END gotoRegis;

PROCEDURE InitMacros;
BEGIN
  writestr("@.");
  writestr("@:AP(B)W(I3)W(C)W(M8)P2V22P666666V22P20V00P4444V44P(E)V[]@;");
  writestr("@:BP(B)W(I3)W(C)W(M12)P2V2774P0V552P6V330P4V11P(E)V[]@;");
  writestr("@:CW(I3)W(C)C[+16]V[]@;");
  writestr("@:DP(B)W(I3)W(C)W(M8)P3V30000V5P66V7;V2222;P6666V44441P5V2222P(E)V[]@;");
  writestr("@:EP(B)W(I3)W(C)W(M6)P3V33P000000V55P55V55P000000V33P(E)V[]@;");
  writestr("@:FP(B)W(I3)W(C)W(M8)P2V22P666666V22P20V00P4444V44P(E)C[+8]V[]@;");
END InitMacros;

PROCEDURE WriteString(ln,cl: INTEGER; VAL s: ARRAY OF CHAR);
  VAR msg: ARRAY [0..83] OF CHAR;
BEGIN
  IF NOT Activ THEN RETURN END;
  writestr(";" 33c "\");
  Terminal.SetCrs(ln,cl);
  writestr(s); Terminal.ClearLine;
  gotoRegis;
END WriteString;

PROCEDURE fill(i: INTEGER);
BEGIN
  IF NOT Activ THEN RETURN END;
  ASSERT((i>=0) & (i<Colors));
  writestr("P(B)S(I"); wc(i,")"); writestr("S(E)S(I0)P(E)");
  Color:=-1; Mode:=-1;
END fill;

PROCEDURE roll(dir,n: INTEGER);
  TYPE c=CHAR;
  VAR i: INTEGER; rol: ARRAY [0..255] OF CHAR;
BEGIN
  IF NOT Activ THEN RETURN END;
  writestr(";" 33c "\");
  writestr(''33c'='44c' ');
  CASE dir OF
    |0: image0(rol,''33c'[%dM',n); writestr(rol);
    |2: image0(rol,''33c'[%dL',n); writestr(rol);
    |1: image0(rol,'');
        FOR i:=0 TO 19 DO image(rol,''33c'[%d@'33c'[%dX'12c,n,n+1) END;
        image(rol,''33c'[%d@'33c'[%dX',n,n+1);
        writestr(rol);
    |3: image0(rol,'');
        FOR i:=0 TO 19 DO image(rol,''33c'[%dP'12c,n) END;
        image(rol,''33c'[%dP',n);
        writestr(rol);
        image0(rol,'');
        FOR i:=0 TO 19 DO image(rol,''33c'[%dX'12c,n+1) END;
        image(rol,''33c'[%dX',n+1);
        Terminal.SetCrs(4,79-n); writestr(rol);
  END;
  gotoRegis;
END roll;

PROCEDURE cursor(no: INTEGER);
  VAR s: ARRAY [0..3] OF CHAR; m: INTEGER;
BEGIN
  IF NOT Activ THEN RETURN END;
  s:="@@A"; s[2]:=CHAR(ORD('A')+no); writestr(s);
  IF Mode#com THEN m:=Mode; Mode:=-1; mode(m) END;
END cursor;

------------------------------------------------------------------------

PROCEDURE FINISH;
BEGIN
  IF NOT Activ THEN RETURN END;
  writestr(";" 33c "\" 33c "[32m");
  SetCrs(24,0);
  IF Cursor(1)#0 THEN END;
  Activ:=FALSE;
END FINISH;

PROCEDURE INIT;
BEGIN
  IF Activ THEN RETURN END;
  Colors:=4;   sp   :=0;
  Lines :=300; Dots :=800;
  dotszY:=2;   Color:=2;
  dotszX:=1;       X:=0;
  charszY:=12;     Y:=0;
  charszX:=10;  Mode:=rep;
  rollX  :=10; rollY:=12;
  Cursors:=6;
  IF Cursor(0)#0 THEN END;
  Activ:=TRUE;
  gotoRegis;
  InitMacros;
END INIT;

BEGIN
  Activ:=FALSE;
  Final(FINISH);
END TV.
