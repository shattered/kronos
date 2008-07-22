IMPLEMENTATION MODULE Points; (* Sem 27-Jun-87. (c) KRONOS *)
                              (* Ned 06-Dec-88. (c) KRONOS *)

IMPORT T: Terminal, vg: pedVG, Windows;

                IMPORT  SYSTEM, Image;
                IMPORT  pedScreen, Exc: ModelPbl, Heap: cdsHeap;
                IMPORT  DWS;
FROM pedEditor  IMPORT  Sheet;

-----------------------------------------------------------------------

MODULE heap;

IMPORT Heap, Point, pPoint, Exc, SYSTEM;

EXPORT Allocate, Deallocate, Deallocate_all;

TYPE block=POINTER TO ARRAY [0..499] OF Point;

VAR blks: ARRAY [0..99] OF block;
    b_no: INTEGER;
    free: pPoint;

PROCEDURE Allocate(VAR p: pPoint);
  VAR i: INTEGER; b: block;
BEGIN
  IF free=NIL THEN
    IF b_no>HIGH(blks) THEN
      Exc.Message:='Points table overflow';
      Exc.RaiseInMe(Exc.MemoryOverflow);
    END;
    Heap.Allocate(b,SIZE(b^));
    blks[b_no]:=b; INC(b_no);
    FOR i:=0 TO HIGH(b^) DO p:=SYSTEM.ADR(b^[i]); p^.Xfwd:=free; free:=p END;
  END;
  p:=free; free:=free^.Xfwd;
END Allocate;

PROCEDURE Deallocate(VAR p: pPoint);
BEGIN
  p^.Xfwd:=free; free:=p; p:=NIL;
END Deallocate;

PROCEDURE Deallocate_all;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO b_no-1 DO
    Heap.Deallocate(blks[i],SIZE(blks[i]^));
  END;
  free:=NIL; b_no:=0;
END Deallocate_all;

BEGIN
  free:=NIL; b_no:=0;
END heap;

-----------------------------------------------------------------------

(*$T-*)

VAR Idents  : ARRAY [0..399] OF pPoint;
    Cnt     : INTEGER;
    Cnt1    : INTEGER;
    str     : ARRAY [0..50] OF CHAR;
    MaxIdent: INTEGER;
    buf     : ARRAY [0..127] OF pPoint;
    inp     : INTEGER;
    V       : INTEGER;
    Sh      : Sheet;

PROCEDURE NewPoint(x,y,l,v,d,id: INTEGER; hd: BOOLEAN; sht: Sheet);
  VAR p,t: pPoint; i: INTEGER;
BEGIN
--ASSERT((x>=0)&(x<DWS.Xsize)&(y>=0)&(y<DWS.Ysize));
--ASSERT(l IN {0..1});
--ASSERT((id>=0)&(id<=HIGH(Idents)));
  INC(Cnt1); INC(Cnt);
  IF Cnt1 MOD 512=0 THEN pedScreen.Text(sht,2,'%4d',Cnt) END;
  Allocate(p);

Sh:=sht;
(*
Windows.lock_window(sht^.wnd);
vg.color(sht^.wnd,l+1); vg.mode(sht^.wnd,vg.rep);
vg.dot(sht^.wnd,x*2+240-sht^.mdl^.ctX DIV 24,y*2+288 DIV 2-sht^.mdl^.ctY DIV 24);
Windows.refresh(sht^.wnd);
Windows.release_window(sht^.wnd);
*)
  p^.x:=x; p^.y:=y; p^.l:=l; p^.v:=v; p^.d:=d; p^.id:=id;
  p^.stopped:=FALSE; p^.hard:=hd;
  t:=DWS.PT(x,y);
  IF t#NIL THEN t^.Xback:=SYSTEM.ADR(p^.Xfwd) END;
  p^.Xfwd:=t; DWS.setPT(x,y,p);
  p^.Xback:=DWS.adrPT(x,y);
  IF Idents[id]=NIL THEN
    p^.Dback:=p; p^.Dfwd:=p; Idents[id]:=p;
  ELSE
    p^.Dfwd:=Idents[id]^.Dfwd; p^.Dback:=Idents[id];
    p^.Dfwd^.Dback:=p; p^.Dback^.Dfwd:=p;
  END;
  i:=v MOD SIZE(buf);
  IF buf[i]=NIL THEN
    buf[i]:=p; p^.Mfwd:=p; p^.Mback:=p;
  ELSE
    p^.Mfwd:=buf[i]^.Mfwd; p^.Mback:=buf[i];
    p^.Mfwd^.Mback:=p; p^.Mback^.Mfwd:=p;
  END;
  ASSERT(v-V<HIGH(buf));
--ASSERT(v-V>=0);
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
--ASSERT((x>=0)&(x<DWS.Xsize)&(y>=0)&(y<DWS.Ysize)&(id>=0)&(id<=HIGH(Idents)));
  p:=DWS.PT(x,y);
  WHILE (p#NIL)&((p^.l#l)OR(p^.id#id)OR(p^.x#x)OR(p^.y#y)) DO
    p:=p^.Xfwd
  END;
  RETURN p;
END FindPoint;

PROCEDURE FindOther(x,y,l,id: INTEGER): pPoint;
  VAR p: pPoint;
BEGIN
--ASSERT((x>=0)&(x<DWS.Xsize)&(y>=0)&(y<DWS.Ysize)&(id>=0)&(id<=HIGH(Idents)));
  p:=DWS.PT(x,y);
  WHILE (p#NIL)&((p^.l#l)OR(p^.id=id)OR(p^.x#x)OR(p^.y#y)) DO p:=p^.Xfwd END;
  RETURN p;
END FindOther;

VAR Last: pPoint;

PROCEDURE FindMin(): pPoint;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(buf) DO
    IF buf[inp]#NIL THEN
      Last:=buf[inp];
      IF Last^.Mfwd=Last THEN
        buf[inp]:=NIL;
      ELSE
        buf[inp]:=Last^.Mback;
        Last^.Mfwd^.Mback:=Last^.Mback;
        Last^.Mback^.Mfwd:=Last^.Mfwd;
        Last^.Mfwd:=Last;
        Last^.Mback:=Last;
      END;
      V:=Last^.v;
--    ASSERT(inp= V MOD SIZE(buf));
      RETURN Last;
    END;
    inp:=(inp+1) MOD SIZE(buf);
  END;
  Last:=NIL; RETURN NIL;
END FindMin;

PROCEDURE NewVeight(p: pPoint; v: INTEGER);
  VAR i: INTEGER;
BEGIN
--ASSERT(v>=V);
--ASSERT(v<V+HIGH(buf));
  i:=p^.v MOD SIZE(buf);
  IF p^.Mfwd=p THEN
--  ASSERT(buf[i]=p);
    buf[i]:=NIL;
  ELSE
--  ASSERT(buf[i]#NIL);
--  ASSERT(buf[i]^.v MOD SIZE(buf) = i);
    buf[i]:=p^.Mfwd;
    p^.Mfwd^.Mback:=p^.Mback;
    p^.Mback^.Mfwd:=p^.Mfwd;
  END;
  i:=v MOD SIZE(buf); p^.v:=v;
  IF buf[i]=NIL THEN
    buf[i]:=p; p^.Mfwd:=p; p^.Mback:=p;
  ELSE
--  ASSERT(buf[i]^.v MOD SIZE(buf) = i);
    p^.Mfwd:=buf[i]^.Mfwd; p^.Mback:=buf[i];
    p^.Mfwd^.Mback:=p; p^.Mback^.Mfwd:=p;
  END;
END NewVeight;

PROCEDURE KilLastPoint;
  VAR p: pPoint;
BEGIN
  IF Last=NIL THEN RETURN END;
--ASSERT(Last^.Mfwd=Last);
--ASSERT(Last^.Mback=Last);
  DEC(Cnt);
  p:=Last;
  IF p^.Xfwd#NIL THEN p^.Xfwd^.Xback:=p^.Xback END;
  p^.Xback^:=p^.Xfwd;
  IF p^.Dback=p THEN
--  ASSERT(Idents[p^.id]=p);
    Idents[p^.id]:=NIL;
  ELSE
    Idents[p^.id]:=p^.Dfwd;
    p^.Dfwd^.Dback:=p^.Dback;
    p^.Dback^.Dfwd:=p^.Dfwd;
    p^.Dback:=p; p^.Dfwd:=p;
  END;
(*
Windows.lock_window(Sh^.wnd);
vg.color(Sh^.wnd,Last^.l+1); vg.mode(Sh^.wnd,vg.bic);
vg.dot(Sh^.wnd,Last^.x*2+240-Sh^.mdl^.ctX DIV 24,Last^.y*2+288 DIV 2-Sh^.mdl^.ctY DIV 24);
Windows.refresh(Sh^.wnd);
Windows.release_window(Sh^.wnd);
*)
  Deallocate(Last);
END KilLastPoint;

PROCEDURE Cat(from,to: INTEGER);
  VAR a,b,c,d: pPoint;
BEGIN
  a:=Idents[from];
--ASSERT(a#NIL);
  b:=a;
  REPEAT b^.id:=to; b:=b^.Dfwd UNTIL a=b;
  Idents[from]:=NIL;
  b:=a^.Dfwd;
  c:=Idents[to];
--ASSERT(c#NIL);
  d:=c^.Dfwd;
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
  FOR x:=0 TO HIGH(buf   ) DO buf[x]:=NIL END;
  FOR x:=0 TO HIGH(Idents) DO
    p:=Idents[x]; q:=p;
    IF p#NIL THEN
      REPEAT p1:=p^.Dfwd;
(*
Windows.lock_window(Sh^.wnd);
vg.color(Sh^.wnd,p^.l+1); vg.mode(Sh^.wnd,vg.bic);
vg.dot(Sh^.wnd,p^.x*2+240-Sh^.mdl^.ctX DIV 24,p^.y*2+288 DIV 2-Sh^.mdl^.ctY DIV 24);
Windows.refresh(Sh^.wnd);
Windows.release_window(Sh^.wnd);
*)
      Deallocate(p); p:=p1 UNTIL p=q;
      Idents[x]:=NIL;
    END;
  END;
  Cnt:=0; Cnt1:=0; MaxIdent:=-1; inp:=0; V:=0;
  Last:=NIL;
END RemovePoints;

PROCEDURE DeallocPoints;
BEGIN
  RemovePoints;
  Deallocate_all;
END DeallocPoints;

VAR i: INTEGER;

BEGIN
  Cnt:=0; Cnt1:=0; MaxIdent:=-1; Last:=NIL; inp:=0; V:=0;
  FOR i:=0 TO HIGH(Idents) DO Idents[i]:=NIL END;
  FOR i:=0 TO HIGH(buf)    DO buf   [i]:=NIL END;
END Points.
