IMPLEMENTATION MODULE abcBuf; (* Hady & Leg. 22-Apr-88. (c) KRONOS *)

FROM Strings    IMPORT  Str0, Str1;
FROM Heap       IMPORT  ALLOCATE, DEALLOCATE
                      , OnOverflow, returnNIL;
FROM abcDefs    IMPORT  NoMemory, EmptyBuf;

TYPE tree = POINTER TO node;
     node = RECORD
              string: ARRAY [0..79] OF CHAR;
              left, right: tree;
            END;

VAR buffer: tree;

PROCEDURE FindNode(start: tree; VAL s: ARRAY OF CHAR; VAR t: tree): INTEGER;
  VAR j,i: INTEGER;
BEGIN
  IF start=NIL THEN RETURN 0 END;
  IF start^.string=s THEN RETURN -1
  ELSIF start^.string>s THEN j:=1; i:=FindNode(start^.left,s,t);
  ELSE j:=2; i:=FindNode(start^.right,s,t);
  END;
  IF i=0 THEN t:=start; RETURN j END;
  RETURN i;
END FindNode;

PROCEDURE PutString(VAL s: ARRAY OF CHAR): INTEGER;
  VAR t,n: tree;
        d: INTEGER;
BEGIN
  IF s[0]=0c THEN RETURN 0 END;
  d:=FindNode(buffer,s,n);
  ASSERT(d<=2,4Bh);
  IF d<0 THEN RETURN 1 END;
  ALLOCATE(t,SIZE(node));
  IF t=NIL THEN RETURN NoMemory END;
  Str1(t^.string,s);
  t^.left:=NIL; t^.right:=NIL;
  CASE d OF
    0: buffer:=t;
   |1: n^.left:=t;
   |2: n^.right:=t;
  END;
  RETURN 1;
END PutString;

PROCEDURE GetString(VAR s: ARRAY OF CHAR): INTEGER;
  VAR pred: tree;
         t: tree;
      r: INTEGER;
BEGIN
  IF buffer=NIL THEN RETURN EmptyBuf END;
  IF buffer^.left=NIL THEN
    Str1(s,buffer^.string);
    t:=buffer;
    buffer:=buffer^.right;
    DEALLOCATE(t,SIZE(node));
    RETURN 1
  END;
  t:=buffer;
  WHILE t^.left#NIL DO pred:=t; t:=t^.left END;
  Str1(s,t^.string);
  pred^.left:=t^.right;
  DEALLOCATE(t,SIZE(node));
  RETURN 1
END GetString;

PROCEDURE Iterator(st: tree; sh: showProc): BOOLEAN;
BEGIN
  IF st=NIL THEN RETURN FALSE END;
  IF Iterator(st^.left,sh) THEN RETURN TRUE END;
  IF sh(st^.string)  THEN RETURN TRUE END;
  RETURN Iterator(st^.right,sh);
END Iterator;

PROCEDURE ShowBuffer(show: showProc);
BEGIN
  IF Iterator(buffer,show) THEN END;
END ShowBuffer;

BEGIN
  buffer:=NIL;
  OnOverflow(returnNIL);
END abcBuf.
