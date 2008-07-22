IMPLEMENTATION MODULE Random[1]; (* Hady. 30-Aug-90. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  cod: defCodes;

TYPE INT64 = ARRAY [0..1] OF INTEGER;
     BYTE8 = POINTER TO ARRAY [0..7] OF CHAR;

CONST null = INT64(ARRAY OF INTEGER {0,0});

PROCEDURE add(VAR res: INT64; x,y: INT64);
  PROCEDURE di; CODE cod.getm cod.li1 cod.li1 cod.ror cod.bic cod.setm END di;
  PROCEDURE ei; CODE cod.getm cod.li1 cod.li1 cod.ror cod.or  cod.setm END ei;
  PROCEDURE Treg(): SYSTEM.ADDRESS; CODE cod.activ cod.li6 cod.add END Treg;
  VAR t: SYSTEM.ADDRESS;
BEGIN
  di; t:=Treg(); t^:=0;
    res[0]:=x[0]+y[0];
    res[1]:=x[1]+y[1]+ORD(t^#0);
  ei; t^:=0
END add;

PROCEDURE ror(VAR res: INT64; x,n: INTEGER);
  VAR i,j: INTEGER;
      a,b: BYTE8;
BEGIN
  res:=null;
  a:=SYSTEM.ADR(res);
  b:=SYSTEM.ADR(x);
  i:=0; j:=n;
  WHILE (i<3) & (j<7) DO
    a^[j]:=b^[i]; INC(i); INC(j)
  END
END ror;

PROCEDURE mul(VAR res: INT64; x,y: INT64);
  VAR a,b: BYTE8;
    i,j,t: INTEGER;
       tt: INT64;
BEGIN
  a:=SYSTEM.ADR(x);
  b:=SYSTEM.ADR(y);
  res:=null;
  FOR j:=7 TO 0 BY -1 DO
    t:=0;
    FOR i:=0 TO j DO
      t:=ORD(a^[i])*ORD(b^[j-i])+t
    END;
    ror(tt,t,j);
    add(res,res,tt);
  END
END mul;

(*
Xn+1 = (a*Xn + c) MOD M;

X0= 330Eh
a = 5 DE EC E6 6Dh
c = 0Bh
M = 2**48;

returns {16..47} - 32 high order bits;

*)

VAR XN, MUL, APP: INT64;

PROCEDURE random(VAR x: SYSTEM.WORD);
  VAR t: INT64;
  tf,tt: BYTE8;
BEGIN
  t:=null;
  mul(t,XN,MUL);
  add(t,t,APP);
  t[1]:=INTEGER(BITSET(t[1])*{0..15});
  XN:=t;
  tt:=SYSTEM.ADR(x);
  tf:=SYSTEM.ADR(XN);
  tt^[0]:=tf^[2];
  tt^[1]:=tf^[3];
  tt^[2]:=tf^[4];
  tt^[3]:=tf^[5];
END random;

VAR XN16: INT64;

PROCEDURE rand16(VAR x: SYSTEM.WORD);
  VAR t: INT64;
  tf,tt: BYTE8;
BEGIN
  t:=null;
  mul(t,XN16,MUL);
  add(t,t,APP);
  t[1]:=INTEGER(BITSET(t[1])*{0..15});
  XN16:=t;
  tt:=SYSTEM.ADR(x);
  tf:=SYSTEM.ADR(XN16);
  tt^[0]:=tf^[2];
  tt^[1]:=tf^[3];
END rand16;

PROCEDURE init(x: SYSTEM.WORD);
  VAR r,shift: INTEGER;
BEGIN
  XN  [0]:=330Eh;      XN  [1]:=0;
  MUL [0]:=0DEECE66Dh; MUL [1]:=5;
  APP [0]:=0Bh;        APP [1]:=0;
  shift:=INTEGER(x);
  shift:=shift MOD 153;
  WHILE shift>=0 DO random(r); DEC(shift) END;
  shift:=INTEGER(BITSET(r)-{31}) DIV 8000000h;
  WHILE shift>=0 DO random(r); DEC(shift) END;
END init;

PROCEDURE init16(x: SYSTEM.WORD);
  VAR r,shift: INTEGER;
BEGIN
  XN16[0]:=330Eh;      XN16[1]:=0;
  MUL [0]:=0DEECE66Dh; MUL [1]:=5;
  APP [0]:=0Bh;        APP [1]:=0;
  shift:=INTEGER(x);
  shift:=shift MOD 153;
  WHILE shift>=0 DO random(r); DEC(shift) END;
  shift:=INTEGER(BITSET(r)-{31}) DIV 8000000h;
  WHILE shift>=0 DO random(r); DEC(shift) END;
END init16;

BEGIN
  XN  [0]:=330Eh;      XN  [1]:=0;
  XN16[0]:=330Eh;      XN16[1]:=0;
  MUL [0]:=0DEECE66Dh; MUL [1]:=5;
  APP [0]:=0Bh;        APP [1]:=0
END Random.
