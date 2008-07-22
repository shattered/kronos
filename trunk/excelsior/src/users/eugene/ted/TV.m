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

CONST
  Y0=00;

PROCEDURE wc(i: INTEGER; last: CHAR);
  VAR s: ARRAY [0..15] OF CHAR;
     co: INTEGER;

  PROCEDURE wdig(i: INTEGER);
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
  END; s[co]:=last; INC(co); s[co]:=0c;  Terminal.WriteString(s);
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
   |0: Terminal.WriteString("W(I0)");
   |1: Terminal.WriteString("W(I1)");
   |2: Terminal.WriteString("W(I2)");
   |3: Terminal.WriteString("W(I3)");
  END;
END color;

PROCEDURE patt(n: INTEGER);
BEGIN
  CASE n OF
   |0: Terminal.WriteString("W(P0)");
   |1: Terminal.WriteString("W(P1)");
   |2: Terminal.WriteString("W(P2)");
   |3: Terminal.WriteString("W(P3)");
   |4: Terminal.WriteString("W(P4)");
   |5: Terminal.WriteString("W(P5)");
   |6: Terminal.WriteString("W(P6)");
   |7: Terminal.WriteString("W(P7)");
  END;
END patt;

PROCEDURE mode(m: INTEGER);
BEGIN
  IF (NOT Activ)OR(m=Mode) THEN RETURN END;
  Mode:=m;
  CASE Mode OF
   |com: Terminal.WriteString("W(C)")
   |rep: Terminal.WriteString("W(R)")
   |add: Terminal.WriteString("W(V)")
  END;
END mode;

PROCEDURE pos(x,y: INTEGER);
BEGIN
  ASSERT((0<=x) & (x<Dots));    ASSERT((0<=y) & (y<Lines));
  IF NOT Activ OR((x=X)&(y=Y)) THEN RETURN END;
  IF    x=X THEN Terminal.WriteString("P[,");
    IF ABS(Y-y)<5  THEN wi((Y-y)*2,"]") ELSE wc((Lines-y)*2+Y0,"]") END;
  ELSIF y=Y THEN Terminal.WriteString("P[");
    IF ABS(X-x)<10 THEN wi(x-X,"]")     ELSE wc(x,"]")           END;
  ELSE           Terminal.WriteString("P[");
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
  IF sp<HIGH(stk) THEN Terminal.WriteString("P(B)");
    stk[sp]:=X+Y*10000h; INC(sp);
  END;
END push;

PROCEDURE pop;
BEGIN
  IF NOT Activ THEN RETURN END;
  IF sp>0 THEN Terminal.WriteString("P(E)");
    DEC(sp); X:=stk[sp]; Y:=X DIV 10000h; X:=X MOD 10000h;
  END;
END pop;

PROCEDURE vect(x,y: INTEGER);
BEGIN
  IF NOT Activ THEN RETURN END;
  ASSERT((0<=x) & (x<Dots)); ASSERT((0<=y) & (y<Lines));
  IF (x=X) & (y=Y) THEN Terminal.WriteString("V[]"); RETURN END;
  IF    x=X THEN Terminal.WriteString("V[,");
    IF ABS(Y-y)<5  THEN wi((Y-y)*2,"]") ELSE wc((Lines-y)*2+Y0,"]") END;
  ELSIF y=Y THEN Terminal.WriteString("V[");
    IF ABS(X-x)<10 THEN wi(x-X,"]")     ELSE wc(x,"]")           END;
  ELSE           Terminal.WriteString("V[");
    IF ABS(X-x)<10 THEN wi(x-X,",")     ELSE wc(x,",")           END;
    IF ABS(Y-y)<5  THEN wi((Y-y)*2,"]") ELSE wc((Lines-y)*2+Y0,"]") END;
  END;
  X:=x; Y:=y;
END vect;

PROCEDURE circ(r: INTEGER);
BEGIN
  IF NOT Activ THEN RETURN END;
  Terminal.WriteString("C["); wi(r,"]");
END circ;

PROCEDURE rect(x,y: INTEGER);
BEGIN
  IF NOT Activ THEN RETURN END;
  Terminal.WriteString("W(S[");
  IF ABS(X-x)<10 THEN wi(x-X,"]")     ELSE wc(x,"]")           END;
  Terminal.WriteString(")V[,");
  IF ABS(Y-y)<5  THEN wi((Y-y)*2,"]") ELSE wc((Lines-y)*2+Y0,"]") END;
  Terminal.WriteString("W(S0)");
  Y:=y;
END rect;

PROCEDURE rect_s(x1,y1,x2,y2,x3,y3: INTEGER);
BEGIN
  IF NOT Activ THEN RETURN END;
  Terminal.WriteString("W(S[+0,+0])V["); wc(x1,','); wc((Lines-y1)*2,']');
  Terminal.WriteString("V["); wc(x2,','); wc((Lines-y2)*2,']');
  Terminal.WriteString("V["); wc(x3,','); wc((Lines-y3)*2,']');
  Terminal.WriteString("W(S0)");
  Y:=y3; X:=x3;
END rect_s;

PROCEDURE gotoRegis;
  VAR m,c,x,y: INTEGER;
BEGIN
  SetCrs(0,0);
  Terminal.WriteString(''33c'P');
  x:=X;    y:=Y;     m:=Mode;  c:=Color;
  X:=-999; Y:=-999;  Mode:=-1; Color:=-1;
  pos(x,y); mode(m); color(c);
END gotoRegis;

PROCEDURE InitMacros;
BEGIN
  Terminal.WriteString("@.");
  Terminal.WriteString("@:AP(B)W(I3)W(C)W(M8)P2V22P666666V22P20V00P4444V44P(E)V[]@;");
  Terminal.WriteString("@:BP(B)W(I3)W(C)W(M12)P2V2774P0V552P6V330P4V11P(E)V[]@;");
  Terminal.WriteString("@:CW(I3)W(C)C[+16]V[]@;");
  Terminal.WriteString("@:DP(B)W(I3)W(C)W(M8)P3V30000V5P66V7;V2222;P6666V44441P5V2222P(E)V[]@;");
  Terminal.WriteString("@:EP(B)W(I3)W(C)W(M6)P3V33P000000V55P55V55P000000V33P(E)V[]@;");
  Terminal.WriteString("@:FP(B)W(I3)W(C)W(M8)P2V22P666666V22P20V00P4444V44P(E)C[+8]V[]@;");
END InitMacros;

PROCEDURE WriteString(ln,cl: INTEGER; VAL s: ARRAY OF CHAR);
  VAR msg: ARRAY [0..83] OF CHAR;
BEGIN
  IF NOT Activ THEN RETURN END;
  Terminal.WriteString(";" 33c "\");
  Terminal.SetCrs(ln,cl);
  Terminal.WriteString(s); Terminal.ClearLine;
  gotoRegis;
END WriteString;

PROCEDURE fill(i: INTEGER);
BEGIN
  IF NOT Activ THEN RETURN END;
  ASSERT((i>=0) & (i<Colors));
  Terminal.WriteString("P(B)S(I"); wc(i,")"); Terminal.WriteString("S(E)S(I0)P(E)");
  Color:=-1; Mode:=-1;
END fill;

PROCEDURE roll(dir,n: INTEGER);
  TYPE c=CHAR;
  VAR i: INTEGER; rol: ARRAY [0..255] OF CHAR;
BEGIN
  IF NOT Activ THEN RETURN END;
  Terminal.WriteString(";" 33c "\");
  Terminal.WriteString(''33c'='44c' ');
  CASE dir OF
    |0: image0(rol,''33c'[%dM',n); Terminal.WriteString(rol);
    |2: image0(rol,''33c'[%dL',n); Terminal.WriteString(rol);
    |1: image0(rol,'');
        FOR i:=0 TO 19 DO image(rol,''33c'[%d@'33c'[%dX'12c,n,n+1) END;
        image(rol,''33c'[%d@'33c'[%dX',n,n+1);
        Terminal.WriteString(rol);
    |3: image0(rol,'');
        FOR i:=0 TO 19 DO image(rol,''33c'[%dP'12c,n) END;
        image(rol,''33c'[%dP',n);
        Terminal.WriteString(rol);
        image0(rol,'');
        FOR i:=0 TO 19 DO image(rol,''33c'[%dX'12c,n+1) END;
        image(rol,''33c'[%dX',n+1);
        Terminal.SetCrs(4,79-n); Terminal.WriteString(rol);
  END;
  gotoRegis;
END roll;

PROCEDURE cursor(no: INTEGER);
  VAR s: ARRAY [0..3] OF CHAR; m: INTEGER;
BEGIN
  IF NOT Activ THEN RETURN END;
  s:="@@A"; s[2]:=CHAR(ORD('A')+no); Terminal.WriteString(s);
  IF Mode#com THEN m:=Mode; Mode:=-1; mode(m) END;
END cursor;

------------------------------------------------------------------------

PROCEDURE FINISH;
BEGIN
  IF NOT Activ THEN RETURN END;
  Terminal.WriteString(";" 33c "\" 33c "[32m");
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
