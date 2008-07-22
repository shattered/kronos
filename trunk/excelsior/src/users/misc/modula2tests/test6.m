$cx

MODULE T; (*  24-Nov-90. (c) KRONOS *)

VAR i,j,k,l,m,n: INTEGER;

BEGIN
  k:=0;
  FOR i:=0 TO 9 DO ASSERT(i=k); INC(k); IF l#m THEN m:=n ELSE END; END;
  ASSERT(k=10); k:=0;
  FOR i:=k TO 9 DO ASSERT(i=k); INC(k); IF l#m THEN m:=n ELSE END; END;
  ASSERT(k=10); m:=20; k:=0;
  FOR i:=0 TO (m+7+k) DIV 3 DO ASSERT(i=k); INC(k); IF l#m THEN m:=n ELSE END; END;
  ASSERT(k=10); k:=0;
  FOR i:=k TO k+9 DO ASSERT(i=k); INC(k); IF l#m THEN m:=n ELSE END; END;
  ASSERT(k=10); k:=0;
  FOR i:=0 TO i+9 DO ASSERT(i=k); INC(k); IF l#m THEN m:=n ELSE END; END;
  ASSERT(k=10); k:=0;
  FOR i:=0 TO 9 BY 2 DO ASSERT(i=k); INC(k,2); IF l#m THEN m:=n ELSE END; END;
  ASSERT(k=10); k:=0;
  FOR i:=k TO 9 BY 2 DO ASSERT(i=k); INC(k,2); IF l#m THEN m:=n ELSE END; END;
  ASSERT(k=10); m:=20; k:=0;
  FOR i:=0 TO (m+7+k) DIV 3 BY 4 DO
    ASSERT(i=k); INC(k,4); IF l#m THEN m:=n ELSE END;
  END;
  ASSERT(k=12); k:=0;
  FOR i:=k TO k+9 BY -1 DO ASSERT(i=k); INC(k); IF l#m THEN m:=n ELSE END; END;
  ASSERT(k=0); k:=0;
  FOR i:=0 TO i-9 BY -1 DO ASSERT(i=k); DEC(k); IF l#m THEN m:=n ELSE END; END;
  ASSERT(k=-10);
END T.

$cx

MODULE T; (*  24-Nov-90. (c) KRONOS *)

VAR i: INTEGER;

PROCEDURE p1(pp1: INTEGER); VAR l1: INTEGER;
PROCEDURE p2(pp2: INTEGER); VAR l2: INTEGER;
PROCEDURE p3(pp3: INTEGER); VAR l3: INTEGER;
PROCEDURE p4(pp4: INTEGER); VAR l4: INTEGER;
PROCEDURE p5(pp5: INTEGER); VAR l5: INTEGER;
PROCEDURE p6(pp6: INTEGER); VAR l6: INTEGER;
PROCEDURE p7(pp7: INTEGER); VAR l7: INTEGER;
PROCEDURE p8(pp8: INTEGER); VAR l8: INTEGER;
PROCEDURE p9(pp9: INTEGER); VAR l9: INTEGER;

PROCEDURE p0(n: INTEGER);
BEGIN
  ASSERT(n=333+9);
  ASSERT(l1=333+10);
  ASSERT(l2=333+11);
  ASSERT(l3=333+12);
  ASSERT(l4=333+13);
  ASSERT(l5=333+14);
  ASSERT(l6=333+15);
  ASSERT(l7=333+16);
  ASSERT(l8=333+17);
  ASSERT(l9=333+18);
  ASSERT(pp1=333+0);
  ASSERT(pp2=333+1);
  ASSERT(pp3=333+2);
  ASSERT(pp4=333+3);
  ASSERT(pp5=333+4);
  ASSERT(pp6=333+5);
  ASSERT(pp7=333+6);
  ASSERT(pp8=333+7);
  ASSERT(pp9=333+8);
END p0;

BEGIN l9:=pp9+10; p0(pp9+1); INC(i) END p9;
BEGIN l8:=pp8+10; p9(pp8+1); INC(i) END p8;
BEGIN l7:=pp7+10; p8(pp7+1); INC(i) END p7;
BEGIN l6:=pp6+10; p7(pp6+1); INC(i) END p6;
BEGIN l5:=pp5+10; p6(pp5+1); INC(i) END p5;
BEGIN l4:=pp4+10; p5(pp4+1); INC(i) END p4;
BEGIN l3:=pp3+10; p4(pp3+1); INC(i) END p3;
BEGIN l2:=pp2+10; p3(pp2+1); INC(i) END p2;
BEGIN l1:=pp1+10; p2(pp1+1); INC(i) END p1;

BEGIN
  i:=0; p1(333); ASSERT(i=9);
END T.
