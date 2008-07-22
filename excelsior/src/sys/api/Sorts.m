IMPLEMENTATION MODULE Sorts; (* Ned 03-Mar-90. (c) KRONOS *)

IMPORT  sys: SYSTEM;
IMPORT  cod: defCodes;

PROCEDURE quick(x: sys.WORD; n: INTEGER; comp: COMP; swap: SWAP);

  PROCEDURE do(l,r: INTEGER);
    VAR i,j,ix: INTEGER;
  BEGIN
    i:=l; j:=r;
    ix:=(l+r) DIV 2;
    REPEAT
      WHILE comp(x,i,ix)<0 DO INC(i) END;
      WHILE comp(x,ix,j)<0 DO DEC(j) END;
      IF i<=j THEN
        IF i#j THEN
          IF    ix=i THEN ix:=j
          ELSIF ix=j THEN ix:=i
          END;
          swap(x,i,j);
        END;
        INC(i); DEC(j);
      END;
    UNTIL i>j;
    IF l<j THEN do(l,j) END;
    IF i<r THEN do(i,r) END;
  END do;

BEGIN
  do(0,n-1);
END quick;

PROCEDURE quickw(VAR a: ARRAY OF sys.WORD; len: INTEGER; comp: COMPW);

  PROCEDURE do(l,r: INTEGER);
    VAR i,j: INTEGER; x,w: INTEGER;
  BEGIN
    i:=l; j:=r;
    x:=a[(l+r) DIV 2];
    REPEAT
      WHILE comp(a[i],x)<0 DO INC(i) END;
      WHILE comp(x,a[j])<0 DO DEC(j) END;
      IF i<=j THEN
        w:=a[i]; a[i]:=a[j]; a[j]:=w;
        INC(i); DEC(j);
      END;
    UNTIL i>j;
    IF l<j THEN do(l,j) END;
    IF i<r THEN do(i,r) END;
  END do;

BEGIN
  do(0,len-1);
END quickw;

PROCEDURE heap(x: sys.WORD; n: INTEGER; comp: COMP; swap: SWAP);

  PROCEDURE sift(l,r: INTEGER);
    VAR i,j,ix: INTEGER;
  BEGIN
    i:=l; j:=2*i; ix:=i;
    LOOP
      IF j>r THEN EXIT END;
      IF j<r THEN
        IF comp(x,j,j+1)<0 THEN INC(j) END;
      END;
      IF comp(x,ix,j)>=0  THEN EXIT END;
      swap(x,i,j);
      i:=j; j:=2*i; ix:=i;
    END;
    swap(x,i,ix);
  END sift;

  VAR l,r: INTEGER;
BEGIN
  DEC(n);
  l:=(n DIV 2) + 1; r:=n;
  WHILE l>0 DO DEC(l); sift(l,r) END;
  WHILE r>0 DO
    swap(x,l,r);
    DEC(r);
    sift(l,r);
  END;
END heap;

PROCEDURE heapw(VAR a: ARRAY OF sys.WORD; len: INTEGER; comp: COMPW);

  PROCEDURE sift(l,r: INTEGER);
    VAR i,j,x: INTEGER;
  BEGIN
    i:=l; j:=2*i; x:=a[i];
    LOOP
      IF j>r THEN EXIT END;
      IF j<r THEN
        IF comp(a[j],a[j+1])<0 THEN INC(j) END;
      END;
      IF comp(x,a[j])>=0 THEN EXIT END;
      a[i]:=a[j]; i:=j; j:=2*i;
    END;
    a[i]:=x;
  END sift;

  VAR l,r,x: INTEGER;
BEGIN
  DEC(len);
  l:=(len DIV 2) + 1; r:=len;
  WHILE l>0 DO DEC(l); sift(l,r) END;
  WHILE r>0 DO
    x:=a[l]; a[l]:=a[r]; a[r]:=x;
    DEC(r);
    sift(l,r);
  END;
END heapw;

---------------------------------------------------------------

TYPE TABLE = ARRAY CHAR OF CHAR;

VAR
  abc: TABLE;
  ABC: TABLE;

PROCEDURE str_comp(VAL a,b: ARRAY OF CHAR): INTEGER;
  PROCEDURE cmp(a,b: sys.ADDRESS): INTEGER;
  CODE cod.comp cod.sub END cmp;
BEGIN
  RETURN cmp(sys.ADR(a),sys.ADR(b))
END str_comp;

PROCEDURE abc_comp(VAL a,b: ARRAY OF CHAR): INTEGER;

  PROCEDURE cmp (a,b: sys.ADDRESS); CODE cod.comp END cmp;
  PROCEDURE save(VAR x: sys.WORD);  CODE cod.swap cod.ssw0 END save;

  VAR ca,cb: CHAR;
BEGIN
  cmp(sys.ADR(a),sys.ADR(b)); save(cb); save(ca);
  RETURN ORD(abc[ca])-ORD(abc[cb])
END abc_comp;

PROCEDURE ABC_comp(VAL a,b: ARRAY OF CHAR): INTEGER;
  VAR i: INTEGER; ca,cb: CHAR;
BEGIN
  i:=0;
  LOOP
    ca:=ABC[a[i]];
    cb:=ABC[b[i]];
    IF    ca=0c THEN RETURN        -ORD(cb)
    ELSIF ca#cb THEN RETURN ORD(ca)-ORD(cb)
    END;
    INC(i);
  END;
END ABC_comp;

PROCEDURE ini;
  CONST
    rus = 'абвгдежзийклмнопрстуфхцчшщъыьэюя';
    RUS = 'АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ';
  VAR c,x,X: CHAR; i: INTEGER;
BEGIN
  FOR c:=0c TO 377c DO abc[c]:=c END;
  ABC:=abc;
  x:=340c; X:=300c;
  FOR i:=0 TO 31 DO
    abc[ rus[i] ]:=x;        abc[ RUS[i] ]:=X;
    ABC[ rus[i] ]:=X;        ABC[ RUS[i] ]:=X;
    INC(x); INC(X);
  END;
  FOR c:='a' TO 'z' DO ABC[c]:=CAP(c) END;
END ini;

BEGIN
  ini;
END Sorts.
