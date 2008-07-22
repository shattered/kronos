IMPLEMENTATION MODULE pedTopology;(* Sem 06-Mar-87. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR, ADDRESS;
FROM KRONOS     IMPORT  MOVE;
FROM mCodeMnem  IMPORT  copt,mul,fmul;
FROM cdsHeap    IMPORT  Allocate, Deallocate;

FROM pedScreen  IMPORT  Drow, Delete;
FROM pedModel   IMPORT  seg_arr, board, signal, seg_rec, signal_mds;

CONST Clearens=8; Overlay=2;

VAR Chain: seg_arr;
    mask : BITSET;

(* bits
  0 - fixed
  1 - main direction
  2 - circle mask
  3 - without metal in vias
*)

PROCEDURE PackSeg(VAR s: seg_rec);
  VAR x1,x2,y1,y2: INTEGER;
BEGIN
  WITH s DO
    c:=0;
    IF X1>X2 THEN
      x1:=X2-Size; x2:=X1+Size;
      IF Y1>Y2 THEN
        y1:=Y2-Size; y2:=Y1+Size; INCL(BITSET(c),1);
      ELSE
        y1:=Y1-Size; y2:=Y2+Size;
      END;
    ELSE
      x1:=X1-Size; x2:=X2+Size;
      IF Y1>Y2 THEN
        y1:=Y2-Size; y2:=Y1+Size;
      ELSE
        y1:=Y1-Size; y2:=Y2+Size; INCL(BITSET(c),1);
      END;
    END;
    IF x1<0 THEN x1:=0 END;
    IF x2<0 THEN x2:=0 END;
    IF y1<0 THEN y1:=0 END;
    IF y2<0 THEN y2:=0 END;
    IF x1>0FFFFh THEN x1:=0FFFFh END;
    IF x2>0FFFFh THEN x2:=0FFFFh END;
    IF y1>0FFFFh THEN y1:=0FFFFh END;
    IF y2>0FFFFh THEN y2:=0FFFFh END;
    IF Fixed      THEN INCL(BITSET(c),0) END;
    INC(c,INTEGER(Size>>12)+INTEGER(Layer<<12)+INTEGER(ViasSize<<4));
    a:=x1+INTEGER(y1<<16);
    b:=x2+INTEGER(y2<<16);
  END;
END PackSeg;

PROCEDURE UnPackSeg(VAL s: seg_rec);
BEGIN
  WITH s DO
    IF c=0 THEN Undef:=TRUE; RETURN END;
    Size:=INTEGER(BITSET(c<<12)*{0..11});
    IF 1 IN BITSET(c) THEN
      X1:=INTEGER(BITSET(a)*mask)+Size;
      Y1:=INTEGER(BITSET(a>>16)*mask)+Size;
      X2:=INTEGER(BITSET(b)*mask)-Size;
      Y2:=INTEGER(BITSET(b>>16)*mask)-Size;
    ELSE
      X1:=INTEGER(BITSET(a)*mask)+Size;
      Y2:=INTEGER(BITSET(a>>16)*mask)+Size;
      X2:=INTEGER(BITSET(b)*mask)-Size;
      Y1:=INTEGER(BITSET(b>>16)*mask)-Size;
    END;
    Layer:=BITSET(c>>12)*{0..7};
    ViasSize:=INTEGER(BITSET(c>>4)*{0..7});
    Fixed:=0 IN BITSET(c);
  END;
  Undef:=FALSE;
END UnPackSeg;

PROCEDURE StartConductor(s: signal);
BEGIN
  Signal:=s;
  Chain:=s^.cu;
  Ident:=-1; Empty:=FALSE;
  NextConductor;
END StartConductor;

PROCEDURE NextConductor;
BEGIN
  LOOP
    IF Empty THEN RETURN END;
    INC(Ident);
    IF Ident>=Signal^.cno THEN
      Empty:=TRUE; Undef:=TRUE;
    ELSE
      Empty:=FALSE;
      UnPackSeg(Chain^[Ident]);
    END;
    IF NOT Undef THEN RETURN END;
  END;
END NextConductor;

PROCEDURE AppConductor;
  VAR i: INTEGER; sa: seg_arr;
  CONST delta=4;
BEGIN
  IF (Ident<0)OR(Ident>=Signal^.cno) THEN Ident:=0 END;
  IF Signal^.cno=0 THEN
    Allocate(Chain,SIZE(seg_rec)*delta);
    Signal^.cu:=Chain; Signal^.cno:=delta;
    FOR i:=0 TO delta-1 DO Chain^[i].c:=0 END;
  ELSE
    i:=Ident;
    LOOP
      IF Chain^[i].c=0 THEN Ident:=i; EXIT END;
      i:=(i+1) MOD Signal^.cno;
      IF i=Ident THEN
        Ident:=Signal^.cno;
        Allocate(sa,SIZE(seg_rec)*(Signal^.cno+delta));
        MOVE(sa,Chain,SIZE(seg_rec)*Signal^.cno);
        FOR i:=Signal^.cno TO Signal^.cno+delta-1 DO sa^[i].c:=0 END;
        Deallocate(Chain,SIZE(seg_rec)*Signal^.cno);
        Signal^.cu:=sa; INC(Signal^.cno,delta);
        Chain:=sa; EXIT;
      END;
    END;
  END;
  PackSeg(Chain^[Ident]);
  Empty:=FALSE; Undef:=FALSE;
END AppConductor;

PROCEDURE CreateConductor(n: INTEGER);
  VAR i,x1,x2,y1,y2: INTEGER; sa: seg_arr;
BEGIN
  ASSERT(Signal^.cu=Chain);
  IF Signal^.cno>=n THEN RETURN END;
  Allocate(sa,SIZE(seg_rec)*n);
  MOVE(sa,Chain,Signal^.cno*SIZE(seg_rec));
  FOR i:=Signal^.cno TO n-1 DO sa^[i].c:=0 END;
  Deallocate(Chain,Signal^.cno*SIZE(seg_rec));
  Signal^.cno:=n; Signal^.cu:=sa;
  StartConductor(Signal);
END CreateConductor;

PROCEDURE DelConductor;
BEGIN
  IF Empty OR Undef THEN RETURN END;
  ASSERT(Ident<Signal^.cno);
  ASSERT(Ident>=0);
  WITH Chain^[Ident] DO a:=0; b:=0; c:=0 END;
  Undef:=TRUE;
END DelConductor;

PROCEDURE TruncConductor;
  VAR i,j,k: INTEGER; sa: seg_arr;
BEGIN
  ASSERT(Signal^.cu=Chain);
  FOR i:=0 TO Signal^.cno-1 DO IF Chain^[i].c#0 THEN INC(j) END END;
  Allocate(sa,j*SIZE(seg_rec)); k:=0;
  FOR i:=0 TO Signal^.cno-1 DO
    IF Chain^[i].c#0 THEN sa^[k]:=Chain^[i]; INC(k) END;
  END;
  ASSERT(k=j);
  Deallocate(Chain,Signal^.cno*SIZE(seg_rec));
  Signal^.cno:=j; Signal^.cu:=sa;
  StartConductor(Signal);
END TruncConductor;

PROCEDURE FindConductor(id: INTEGER);
  VAR i: INTEGER;
BEGIN
  Ident:=id;
  IF (Ident<0)OR(Ident>=Signal^.cno) THEN
    Empty:=TRUE; Undef:=TRUE;
  ELSE
    Empty:=FALSE;
    UnPackSeg(Chain^[Ident]);
  END;
END FindConductor;

VAR Shoted   : BOOLEAN;
    CurSignal: signal;
    CurLayer : BITSET;
    CurSize  : INTEGER;

PROCEDURE App;
BEGIN
  Drow(X1,Y1,X2,Y2,Size,Layer);
  AppConductor;
END App;

PROCEDURE Del;
BEGIN
  Delete(X1,Y1,X2,Y2,Size,Layer);
  DelConductor;
END Del;

PROCEDURE OnLine(x1,y1,x2,y2: INTEGER): BOOLEAN;
  VAR dx,dy: INTEGER;
BEGIN
  dx:=LineXe-LineXs; dy:=LineYe-LineYs;
  RETURN ((x1-LineXs)*dy=(y1-LineYs)*dx)&((x2-LineXs)*dy=(y2-LineYs)*dx);
END OnLine;

PROCEDURE Side(x1,y1,x2,y2,x,y: INTEGER): INTEGER;
  VAR dx,dy,z1,z2,r: INTEGER;
BEGIN
  dx:=x2-x1; dy:=y2-y1;
  IF (dx=0)&(dy=0) THEN
    IF (x=x1)&(y=y1) THEN RETURN 0 ELSE RETURN 1 END;
  END;
  r:=x*dx+y*dy;
  z1:=r-x1*dx-y1*dy;
  z2:=x2*dx+y2*dy-r;
  IF (z1>=0)&(z2>=0) THEN RETURN 0
  ELSIF z1<0 THEN RETURN -1
  ELSE RETURN 1 END;
END Side;

PROCEDURE StrongSide(x1,y1,x2,y2,x,y: INTEGER): INTEGER;
  VAR dx,dy,z1,z2,r: INTEGER;
BEGIN
  dx:=x2-x1; dy:=y2-y1;
  IF (dx=0)&(dy=0) THEN RETURN 1 END;
  r:=x*dx+y*dy;
  z1:=r-x1*dx-y1*dy;
  z2:=x2*dx+y2*dy-r;
  IF (z1>0)&(z2>0) THEN RETURN 0
  ELSIF z1<=0 THEN RETURN -1
  ELSE RETURN 1 END;
END StrongSide;

PROCEDURE SQ(r: REAL): REAL;
CODE copt fmul END SQ;

PROCEDURE ISQ(i: INTEGER): INTEGER;
CODE copt mul END ISQ;

PROCEDURE Len(x1,y1,x2,y2,x,y: INTEGER): INTEGER;
  VAR sd,A,B,C: INTEGER;
BEGIN
  B:=x2-x1; A:=y1-y2;
  IF NOT BOOLEAN(BITSET(A)+BITSET(B)) THEN RETURN ISQ(x1-x)+ISQ(y1-y) END;
  sd:=Side(x1,y1,x2,y2,x,y);
  IF sd<0 THEN RETURN ISQ(x1-x)+ISQ(y1-y) END;
  IF sd>0 THEN RETURN ISQ(x2-x)+ISQ(y2-y) END;
  C:=-x1*A-y1*B;
  RETURN TRUNC(SQ(FLOAT(A*x+B*y+C))/FLOAT(ISQ(A)+ISQ(B)));
END Len;

VAR ChkDX,ChkDY,ChkXs,ChkXe,ChkYs,ChkYe: INTEGER;

PROCEDURE Check(): BOOLEAN;
  VAR r,dx,dy: INTEGER;
BEGIN
  IF (Signal#CurSignal)&(Layer*CurLayer#{}) THEN
    IF (fantom IN Signal^.type)OR(fantom IN CurSignal^.type) THEN
      RETURN FALSE
    END;
    r:=ISQ(Clearens+Size+CurSize);
    dx:=X2-X1; dy:=Y2-Y1;
    IF
     ((ChkDX*(Y1-ChkYs)-ChkDY*(X1-ChkXs)>=0)#
       (ChkDX*(Y2-ChkYs)-ChkDY*(X2-ChkXs)>=0))
     &((dx*(ChkYs-Y1)-dy*(ChkXs-X1)>=0)#(dx*(ChkYe-Y1)-dy*(ChkXe-X1)>=0))
     OR (Len(ChkXs,ChkYs,ChkXe,ChkYe,X1,Y1)<r)
     OR (Len(ChkXs,ChkYs,ChkXe,ChkYe,X2,Y2)<r)
     OR (Len(X1,Y1,X2,Y2,ChkXs,ChkYs)<r)
     OR (Len(X1,Y1,X2,Y2,ChkXe,ChkYe)<r)
    THEN
      Shoted:=TRUE; ShotedSignal:=Signal; ShotedIdent:=Ident;
      RETURN TRUE
    END;
  END;
  RETURN FALSE;
END Check;

VAR ip: IterProc;
    FinSeek: BOOLEAN;

PROCEDURE SeekInBox0(s: signal);
  VAR i,j: INTEGER; c: seg_arr; p,e,b: POINTER TO seg_rec;
BEGIN
  IF s^.cno=0 THEN RETURN END;
  c:=s^.cu;
  b:=ADR(c^[0]); p:=b;
  e:=ADR(c^[s^.cno-1]);
  LOOP
    IF BOOLEAN(p^.c) THEN
      IF BOOLEAN(BITSET(INTEGER(BITSET(p^.a)*mask)<=LineXe)*
                 BITSET(INTEGER(BITSET(p^.a>>16)*mask)<=LineYe)*
                 BITSET(INTEGER(BITSET(p^.b)*mask)>=LineXs)*
                 BITSET(INTEGER(BITSET(p^.b>>16)*mask)>=LineYs)) THEN
        Signal:=s; Chain:=s^.cu; Empty:=FALSE;
        Ident:=(INTEGER(p)-INTEGER(b)) DIV SIZE(seg_rec);
        UnPackSeg(Chain^[Ident]);
        FinSeek:=ip();
        IF FinSeek THEN RETURN END;
      END;
    END;
    IF p=e THEN RETURN END;
    INC(INTEGER(p),SIZE(seg_rec));
  END;
END SeekInBox0;

PROCEDURE SeekInBox(mdl: board; p: IterProc);
  VAR i: INTEGER;
BEGIN
  IF LineXs>LineXe THEN i:=LineXs; LineXs:=LineXe; LineXe:=i END;
  IF LineYs>LineYe THEN i:=LineYs; LineYs:=LineYe; LineYe:=i END;
  FinSeek:=FALSE; ip:=p;
  FOR i:=0 TO mdl^.sno-1 DO SeekInBox0(mdl^.sigs^[i]) END;
END SeekInBox;

PROCEDURE Shoted?(size,layer: INTEGER; mdl: board; sig: signal): BOOLEAN;
BEGIN
  Shoted:=FALSE; CurSignal:=sig;
  CurLayer:={}; INCL(CurLayer,layer);
  CurSize:=size;
  INC(size,Clearens);
  ChkXs:=LineXs; ChkXe:=LineXe;
  ChkYs:=LineYs; ChkYe:=LineYe;
  ChkDX:=ChkXe-ChkXs; ChkDY:=ChkYe-ChkYs;
  IF ChkXs>ChkXe THEN LineXs:=ChkXe; LineXe:=ChkXs END;
  IF ChkYs>ChkYe THEN LineYs:=ChkYe; LineYe:=ChkYs END;
  DEC(LineXs,size); DEC(LineYs,size);
  INC(LineXe,size); INC(LineYe,size);
  SeekInBox(mdl,Check);
  LineXs:=ChkXs; LineXe:=ChkXe;
  LineYs:=ChkYs; LineYe:=ChkYe;
  RETURN Shoted;
END Shoted?;

PROCEDURE Change(x1,y1,x2,y2: INTEGER);
BEGIN
  Del; X1:=x1; Y1:=y1; X2:=x2; Y2:=y2; App;
END Change;

PROCEDURE InsertRange
  (size,layer: INTEGER; fix: BOOLEAN; mdl: board; sig: signal): BOOLEAN;
  VAR l,l1,k: INTEGER; DelMarks: ARRAY [0..499] OF BITSET; Del?: BOOLEAN;
      ls: BITSET;
  PROCEDURE Mark;
  BEGIN INCL(DelMarks[Ident DIV 32],Ident MOD 32) END Mark;
  PROCEDURE Mark?(): BOOLEAN;
  BEGIN RETURN (Ident MOD 32) IN DelMarks[Ident DIV 32] END Mark?;
BEGIN
  IF (LineXs=LineXe)&(LineYs=LineYe) THEN RETURN FALSE END;
  FOR k:=0 TO HIGH(DelMarks) DO DelMarks[k]:={} END;
  ls:={}; INCL(ls,layer);
  StartConductor(sig);
  WHILE NOT Empty DO
    Del?:=FALSE;
    IF (Layer=ls)&(ViasSize=0)&(fix=Fixed)&
        NOT Mark?()&OnLine(X1,Y1,X2,Y2) THEN
      l:=Side(X1,Y1,X2,Y2,LineXs,LineYs);
      l1:=Side(X1,Y1,X2,Y2,LineXe,LineYe);
      IF (l1=0)&(l=0) THEN
        IF Size>=size THEN RETURN FALSE END;
      ELSIF l=0 THEN
        IF Size=size THEN
          IF l1>0 THEN
            LineXs:=X1; LineYs:=Y1; Del?:=TRUE;
          ELSE
            LineXs:=X2; LineYs:=Y2; Del?:=TRUE;
          END;
        END;
      ELSIF l1=0 THEN
        IF Size=size THEN
          IF l>0 THEN
            LineXe:=X1; LineYe:=Y1; Del?:=TRUE;
          ELSE
            LineXe:=X2; LineYe:=Y2; Del?:=TRUE;
          END;
        END;
      ELSIF (l<0)#(l1<0) THEN
        IF Size<=size THEN Del?:=TRUE END;
      END;
    END;
    IF Del? THEN Mark; StartConductor(sig);
    ELSE NextConductor END;
  END;
  IF Shoted?(size,layer,mdl,sig) THEN RETURN TRUE END;
  StartConductor(sig);
  WHILE NOT Empty DO
    IF Mark?() THEN Del END;
    NextConductor;
  END;
  StartConductor(sig);
  X1:=LineXs; Y1:=LineYs;
  X2:=LineXe; Y2:=LineYe;
  Layer:=ls;
  Size:=size;
  ViasSize:=0;
  Fixed:=fix;
  App;
  RETURN FALSE;
END InsertRange;

PROCEDURE DeleteRange(layer: INTEGER; sig: signal);
  VAR r1,r2,x,y,l,l1: INTEGER;
BEGIN
  IF (LineXs=LineXe)&(LineYs=LineYe) THEN RETURN END;
  StartConductor(sig);
  WHILE NOT Empty DO
    IF (layer IN Layer)&OnLine(X1,Y1,X2,Y2) THEN
      l:=StrongSide(X1,Y1,X2,Y2,LineXs,LineYs);
      l1:=StrongSide(X1,Y1,X2,Y2,LineXe,LineYe);
      IF (l=0)&(l1=0) THEN
        r1:=(X1-LineXs)*(X1-LineXs)+(Y1-LineYs)*(Y1-LineYs);
        r2:=(X1-LineXe)*(X1-LineXe)+(Y1-LineYe)*(Y1-LineYe);
        IF r1<r2 THEN
          x:=X2; y:=Y2; Change(X1,Y1,LineXs,LineYs);
          X1:=LineXe; Y1:=LineYe; X2:=x; Y2:=y;
          App;
        ELSE
          x:=X2; y:=Y2; Change(X1,Y1,LineXe,LineYe);
          X1:=LineXs; Y1:=LineYs; X2:=x; Y2:=y;
          App;
        END;
      ELSIF l=0 THEN
        IF l1>0 THEN Change(X1,Y1,LineXs,LineYs)
        ELSE Change(LineXs,LineYs,X2,Y2) END;
      ELSIF l1=0 THEN
        IF l>0 THEN Change(X1,Y1,LineXe,LineYe)
        ELSE Change(LineXe,LineYe,X2,Y2) END
      ELSIF (l<0)#(l1<0) THEN
        IF NOT((X1=X2)&(Y1=Y2)) THEN Del END;
      END;
    END;
    NextConductor;
  END;
END DeleteRange;

PROCEDURE InsertVias
  (size,vsize: INTEGER; fix: BOOLEAN; mdl: board; sig: signal): BOOLEAN;
  VAR l: INTEGER; DelMarks: ARRAY [0..499] OF BITSET;
  PROCEDURE Mark;
  BEGIN INCL(DelMarks[Ident DIV 32],Ident MOD 32) END Mark;
  PROCEDURE Mark?(): BOOLEAN;
  BEGIN RETURN (Ident MOD 32) IN DelMarks[Ident DIV 32] END Mark?;
BEGIN
  ASSERT((LineXs=LineXe)&(LineYs=LineYe));
  FOR l:=0 TO HIGH(DelMarks) DO DelMarks[l]:={} END;
  StartConductor(sig);
  WHILE NOT Empty DO
    IF (Fixed=fix)&NOT Mark?()&(X1=X2)&(Y1=Y2)&(X1=LineXs)&(Y1=LineYs) THEN
      IF Size>size THEN size:=Size END;
      IF (Layer={0,1})&(Size=size)&(ViasSize=vsize) THEN RETURN FALSE END;
      Mark; StartConductor(sig);
    ELSE
      NextConductor;
    END;
  END;
  IF Shoted?(size,0,mdl,sig) THEN RETURN TRUE END;
  IF Shoted?(size,1,mdl,sig) THEN RETURN TRUE END;
  StartConductor(sig);
  WHILE NOT Empty DO
    IF Mark?() THEN Del END;
    NextConductor;
  END;
  StartConductor(sig);
  X1:=LineXs; Y1:=LineYs;
  X2:=LineXe; Y2:=LineYe;
  Layer:={0,1};
  Size:=size;
  ViasSize:=vsize;
  Fixed:=fix;
  App;
  RETURN FALSE;
END InsertVias;

PROCEDURE DeleteVias(sig: signal);
BEGIN
  ASSERT((LineXs=LineXe)&(LineYs=LineYe));
  StartConductor(sig);
  WHILE NOT Empty DO
    IF (X1=X2)&(Y1=Y2)&(LineXs=X1)&(LineYs=Y1) THEN Del END;
    NextConductor;
  END;
END DeleteVias;

VAR FindSz : INTEGER;
    FindX  : INTEGER;
    FindY  : INTEGER;
    FindSig: signal;

PROCEDURE FindSg(): BOOLEAN;
  VAR sz: INTEGER;
BEGIN
  sz:=Size+FindSz;
  IF (Layer*CurLayer#{})&(Len(X1,Y1,X2,Y2,FindX,FindY)<=sz*sz)&
     (Signal#FindSig) THEN
    ShotedSignal:=Signal; RETURN TRUE;
  END;
  RETURN FALSE;
END FindSg;

PROCEDURE FindSignal(x,y,l,size: INTEGER; mdl: board; sig: signal);
BEGIN
  LineXs:=x-size; LineXe:=x+size;
  LineYs:=y-size; LineYe:=y+size;
  FindSz:=size; FindX:=x; FindY:=y; FindSig:=sig;
  CurLayer:={}; INCL(CurLayer,l);
  ShotedSignal:=NIL; Signal:=NIL;
  SeekInBox(mdl,FindSg);
  IF ShotedSignal=NIL THEN Signal:=NIL END;
END FindSignal;

PROCEDURE Min(x,y: INTEGER): INTEGER;
BEGIN IF x<y THEN RETURN x ELSE RETURN y END END Min;

PROCEDURE Max(x,y: INTEGER): INTEGER;
BEGIN IF x>y THEN RETURN x ELSE RETURN y END END Max;

TYPE Range=RECORD
             minX,minY,maxX,maxY: INTEGER;
             Id,x1,x2,y1,y2,size: INTEGER;
             layer: BITSET
           END;
     pRange=POINTER TO Range;
(*$T-*)

VAR Idents: POINTER TO ARRAY [0..0] OF Range;
    IdCnt : INTEGER;
    IdPtr : INTEGER;
    MinX,MaxX,MinY,MaxY: INTEGER;

PROCEDURE Connect(x1,y1,x2,y2,sz: INTEGER; ls: BITSET; p: pRange): BOOLEAN;
  VAR r,r1,dx,dy,cdx,cdy: INTEGER;
BEGIN
  r1:=p^.size+sz-Overlay;
  r:=r1*r1;
  dx:=p^.x2-p^.x1; dy:=p^.y2-p^.y1;
  cdx:=x2-x1; cdy:=y2-y1;
  RETURN
    ((cdx*(p^.y1-y1)-cdy*(p^.x1-x1)>=0)#(cdx*(p^.y2-y1)-cdy*(p^.x2-x1)>=0))
    & ((dx*(y1-p^.y1)-dy*(x1-p^.x1)>=0)#(dx*(y2-p^.y1)-dy*(x2-p^.x1)>=0))
    OR (Len(x1,y1,x2,y2,p^.x1,p^.y1)<r)
    OR (Len(x1,y1,x2,y2,p^.x2,p^.y2)<r)
    OR (Len(p^.x1,p^.y1,p^.x2,p^.y2,x1,y1)<r)
    OR (Len(p^.x1,p^.y1,p^.x2,p^.y2,x2,y2)<r);
END Connect;

PROCEDURE Areas(sig: signal): INTEGER;
  VAR size,from,to,i: INTEGER; p,q,r: pRange;
BEGIN
  IdCnt:=0; IdPtr:=1;
  StartConductor(sig); size:=0;
  WHILE NOT Empty DO INC(size); NextConductor END;
  IF size<=1 THEN RETURN size END;
  Allocate(Idents,size*SIZE(Range));
  StartConductor(sig); i:=0;
  WHILE NOT Empty DO
    ASSERT(i<size);
    WITH Idents^[i] DO
      x1:=X1; x2:=X2; y1:=Y1; y2:=Y2;
      size:=Size; layer:=Layer;
      minX:=Min(X1,X2)-Size; maxX:=Max(X1,X2)+Size;
      minY:=Min(Y1,Y2)-Size; maxY:=Max(Y1,Y2)+Size;
      Id:=IdPtr; INC(IdPtr); INC(IdCnt);
    END;
    INC(i);
    NextConductor;
  END;
  r:=ADR(Idents^[size]);
  FOR i:=0 TO size-1 DO
    WITH Idents^[i] DO
      p:=ADR(Idents^[i+1]);
      WHILE p<r DO
        IF (Id#p^.Id)&(layer*p^.layer#{}) THEN
          IF NOT ( (minX>=p^.maxX)OR(p^.minX>=maxX)OR
                   (minY>=p^.maxY)OR(p^.minY>=maxY) ) &
                   Connect(x1,y1,x2,y2,size,layer,p) THEN
            from:=Id; to:=p^.Id;
            q:=ADR(Idents^[0]);
            REPEAT
              IF q^.Id=from THEN q^.Id:=to END;
              INC(INTEGER(q),TSIZE(Range));
            UNTIL q>=r;
            DEC(IdCnt);
          END;
        END;
        INC(INTEGER(p),TSIZE(Range));
      END;
    END;
  END;
  Deallocate(Idents,size*TSIZE(Range));
  RETURN IdCnt;
END Areas;

BEGIN
  Signal:=NIL;
  Chain:=NIL;
  Empty:=TRUE;
  mask:={0..15};
END pedTopology.
