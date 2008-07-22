IMPLEMENTATION MODULE Points; (* Sem 27-Jun-87. (c) KRONOS *)

FROM DWS       IMPORT   PT, setPT, adrPT, Ptr, Xsize, Ysize, setWS, finish, Buf;
FROM SYSTEM    IMPORT   ADR;
FROM cdsHeap   IMPORT   Allocate, Deallocate;
FROM pedScreen IMPORT   Text;
FROM Image     IMPORT   image0;

(*$T-*)

TYPE block=POINTER TO ARRAY [0..499] OF Point;

VAR Idents  : ARRAY [0..99] OF pPoint;
    Cnt     : INTEGER;
    Cnt1    : INTEGER;
    MaxIdent: INTEGER;
    blocks  : ARRAY [0..99] OF block;
    blk_no  : INTEGER;
    free    : pPoint;

PROCEDURE NewPoint(x,y,l,v,d,id: INTEGER; hd: BOOLEAN);
  PROCEDURE Info;
    VAR str: ARRAY [0..79] OF CHAR;
  BEGIN
    image0(str,'%4d',Cnt); Text(2,str)
  END Info;
  VAR p,t: pPoint; b: block; i: INTEGER;
BEGIN
  ASSERT((x>=0)&(x<Xsize)&(y>=0)&(y<Ysize));
  ASSERT(l IN {0..1});
  ASSERT((id>=0)&(id<=HIGH(Idents)));
  INC(Cnt1); INC(Cnt);
  IF Cnt1 MOD 512 = 0 THEN Info END;
  IF free=NIL THEN
    ASSERT(blk_no<=HIGH(blocks));
    Allocate(b,SIZE(b^));
    FOR i:=0 TO HIGH(b^) DO p:=ADR(b^[i]); p^.Xfwd:=free; free:=p END;
    blocks[blk_no]:=b; INC(blk_no);
  END;
  p:=free; free:=free^.Xfwd;
  p^.x:=x; p^.y:=y; p^.l:=l; p^.v:=v; p^.d:=d; p^.id:=id;
  p^.stopped:=FALSE; p^.hard:=hd;
  t:=PT(x,y,Ptr);
  IF t#NIL THEN t^.Xback:=ADR(p^.Xfwd) END;
  p^.Xfwd:=t; setPT(x,y,Ptr,p);
  p^.Xback:=adrPT(x,y,Ptr);
  IF Idents[id]=NIL THEN
    p^.Dback:=p; p^.Dfwd:=p; Idents[id]:=p;
  ELSE
    t:=Idents[id]^.Dfwd;
    t^.Dback:=p;
    Idents[id]^.Dfwd:=p;
    p^.Dfwd:=t; p^.Dback:=Idents[id];
  END;
END NewPoint;

PROCEDURE NewIdent(): INTEGER;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(Idents) DO
    IF Idents[i]=NIL THEN
      IF MaxIdent<i THEN MaxIdent:=i END;
      RETURN i;
    END;
  END;
END NewIdent;

PROCEDURE FindPoint(x,y,l,id: INTEGER): pPoint;
  VAR p: pPoint;
BEGIN
  ASSERT((x>=0)&(x<Xsize)&(y>=0)&(y<Ysize)&(id>=0)&(id<=HIGH(Idents)));
  p:=PT(x,y,Ptr);
  WHILE (p#NIL)&((p^.l#l)OR(p^.id#id)OR(p^.x#x)OR(p^.y#y)) DO
    ASSERT(p^.l IN {0..1});
    p:=p^.Xfwd
  END;
  RETURN p;
END FindPoint;

PROCEDURE FindOther(x,y,l,id: INTEGER): pPoint;
  VAR p: pPoint;
BEGIN
  ASSERT((x>=0)&(x<Xsize)&(y>=0)&(y<Ysize)&(id>=0)&(id<=HIGH(Idents)));
  p:=PT(x,y,Ptr);
  WHILE (p#NIL)&((p^.l#l)OR(p^.id=id)OR(p^.x#x)OR(p^.y#y)) DO p:=p^.Xfwd END;
  RETURN p;
END FindOther;

VAR Last: pPoint;
    Mins: ARRAY [0..999] OF pPoint;
    MinNo : INTEGER;

PROCEDURE FindMin(): pPoint;
  VAR i,x,mv: INTEGER; p,q: pPoint;
BEGIN
  IF MinNo>0 THEN
    DEC(MinNo); Last:=Mins[MinNo]; RETURN Last
  END;
  mv:=1000000000; Last:=NIL;
  FOR i:=0 TO HIGH(Idents) DO
    IF Idents[i]#NIL THEN
      p:=Idents[i]; q:=p;
      REPEAT
        IF (p^.v<=mv)&(NOT p^.stopped) THEN
          IF p^.v<mv THEN
            mv:=p^.v; Last:=p; MinNo:=0;
          ELSIF MinNo<=HIGH(Mins) THEN
            Mins[MinNo]:=p; INC(MinNo);
          END;
        END;
        p:=p^.Dfwd;
      UNTIL q=p;
    END;
  END;
  RETURN Last;
END FindMin;

PROCEDURE KilLastPoint;
  VAR p,b,f: pPoint;
BEGIN
  IF Last=NIL THEN RETURN END;
  DEC(Cnt);
  p:=Last;
  IF p^.Xfwd#NIL THEN p^.Xfwd^.Xback:=p^.Xback END; p^.Xback^:=p^.Xfwd;
  b:=p^.Dback; f:=p^.Dfwd;
  b^.Dfwd:=f; f^.Dback:=b;
  IF Idents[p^.id]=p THEN
    IF p#f THEN Idents[p^.id]:=f ELSE Idents[p^.id]:=NIL END;
  END;
  Last^.Xfwd:=free; free:=Last; Last:=NIL;
END KilLastPoint;

PROCEDURE Cat(from,to: INTEGER);
  VAR a,b,c,d: pPoint;
BEGIN
  a:=Idents[from];
  ASSERT(a#NIL);
  b:=a;
  REPEAT b^.id:=to; b:=b^.Dfwd UNTIL a=b;
  b:=a^.Dfwd;
  c:=Idents[to];
  ASSERT(c#NIL);
  d:=c^.Dfwd;
  Idents[from]:=NIL;
  a^.Dfwd:=d;
  d^.Dback:=a;
  c^.Dfwd:=b;
  b^.Dback:=c;
  IF from=MaxIdent THEN
    REPEAT DEC(MaxIdent) UNTIL Idents[MaxIdent]#NIL;
  END;
END Cat;

PROCEDURE ShotWave;
  VAR p,p1,q: pPoint; x,min,mincnt,n: INTEGER;
BEGIN
  ASSERT(FALSE);
END ShotWave;

PROCEDURE RemovePoints;
  VAR p,p1,q: pPoint; x: INTEGER;
BEGIN
  FOR x:=0 TO HIGH(Idents) DO
    p:=Idents[x]; q:=p;
    IF p#NIL THEN
      REPEAT
        p1:=p^.Dfwd; p^.Xfwd:=free; free:=p; p:=p1
      UNTIL p=q;
      Idents[x]:=NIL;
    END;
  END;
  Cnt:=0; Cnt1:=0; MaxIdent:=-1;
  MinNo:=0; Last:=NIL;
END RemovePoints;

PROCEDURE DeallocPoints;
  VAR i: INTEGER; b: block;
BEGIN
  RemovePoints;
  FOR i:=0 TO blk_no-1 DO
    b:=blocks[i];
    Deallocate(b,SIZE(b^));
  END;
  blk_no:=0; free:=NIL;
END DeallocPoints;

VAR i: INTEGER;

BEGIN
  free:=NIL;
  Cnt:=0; Cnt1:=0; MaxIdent:=-1; MinNo:=0; Last:=NIL; blk_no:=0;
  FOR i:=0 TO HIGH(Idents) DO Idents[i]:=NIL END;
END Points.
