IMPLEMENTATION MODULE RoutDeb; (* Sem 24-Dec-87. (c) KRONOS *)

FROM Model      IMPORT  Object;
FROM Terminal   IMPORT  SetTermNo, TransparentOut, WriteString;
FROM Image      IMPORT  image0;
FROM DWS        IMPORT  Xsize, Ysize, Busy, Buf;
FROM Args       IMPORT  Flag?;
FROM cdsHeap    IMPORT  Allocate, Deallocate;

TYPE Line=ARRAY [0..123] OF CHAR;
VAR  Page: POINTER TO ARRAY [0..71] OF Line;

PROCEDURE Text(l,c: INTEGER; ln: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  i:=0;
  WHILE (i<=HIGH(ln))&(ln[i]#0c) DO
    Page^[l][c]:=ln[i]; INC(c); INC(i);
  END;
END Text;

PROCEDURE Num(l,c,s,n: INTEGER);
  VAR i: INTEGER;
BEGIN
  i:=c+s-1;
  REPEAT
    IF i>=c THEN Page^[l][i]:=CHAR(ORD('0')+n MOD 10) END;
    n:=n DIV 10; DEC(i);
  UNTIL n=0;
END Num;

PROCEDURE StartDeb(sig: Object);
  VAR ln: Line; i,j,n,l: INTEGER;
BEGIN
  IF NOT Flag?('d') THEN RETURN END;
  Allocate(Page,SIZE(Page^));
  FOR i:=0 TO 71 DO
    FOR j:=0 TO 122 DO Page^[i][j]:=' ' END; Page^[i][123]:=0c;
  END;
  n:=19;
  FOR i:=0 TO 60 BY 3 DO
    FOR j:=0 TO 122 DO Page^[i][j]:='-' END;
    IF i#60 THEN
      FOR j:=2 TO 122 BY 6 DO Page^[i+1][j]:='|' END;
      FOR j:=2 TO 122 BY 6 DO Page^[i+2][j]:='|' END;
      Num(i+2,0,2,n); DEC(n);
    END;
  END;
  FOR i:=0 TO 19 DO Num(61,i*6+6,2,i) END;
  image0(ln,'  layout proces for signal %s',sig^.Name);
  Text(63,0,ln);
  FOR i:=0 TO Ysize-1 DO
    FOR j:=0 TO Xsize-1 DO
      IF (i<20)&(j<20) THEN
        FOR l:=0 TO 1 DO
          IF Busy(j,i,l,Buf) THEN
            Text(59-i*3-l,3+j*6,'*****');
            Num(59-i*3-l,4+j*6,2,INTEGER(Busy(j,i,l,Buf)));
          END;
        END;
      END;
    END;
  END;
END StartDeb;

PROCEDURE DebPnt(x,y,l,v,d: INTEGER);
  VAR Ch: CHAR;
BEGIN
  IF Page=NIL THEN RETURN END;
  DEC(x,20);
  IF (x>=0)&(y>=0)&(x<20)&(y<20) THEN
    Num(59-y*3-l,3+x*6,4,v);
    CASE d OF
      5 : Ch:='>'
     |6 : Ch:=CHAR(0AFh);
     |7 : Ch:='^';
     |8 : Ch:=CHAR(0A1h);
     |9 : Ch:='<';
     |10: Ch:='L';
     |11: Ch:='V';
     |12: Ch:='J';
     |13: Ch:='o';
     |14: Ch:='*';
    ELSE  Ch:=' ';
    END;
    Page^[59-y*3-l][7+x*6]:=Ch;
  END;
END DebPnt;

PROCEDURE FinishDeb;
  VAR i: INTEGER;
BEGIN
  IF NOT Flag?('d') THEN RETURN END;
  Deallocate(Page,SIZE(Page^));
  SetTermNo(3);
  TransparentOut(TRUE);
  FOR i:=0 TO 71 DO
    WriteString(Page^[i]);
    WriteString(''15c''12c);
  END;
  SetTermNo(0);
END FinishDeb;

BEGIN
  Page:=NIL;
END RoutDeb.
