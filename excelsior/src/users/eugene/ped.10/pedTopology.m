IMPLEMENTATION MODULE pedTopology;(* Sem 06-Mar-87. (c) KRONOS *)

IMPORT ModelMisc;
FROM SYSTEM    IMPORT   ADR, ADDRESS;
FROM pedScreen IMPORT   Drow, Delete;
FROM pedPbl    IMPORT   Clearens, Overlay;
FROM Model     IMPORT   Tag, Objects, Iterate, Segment, Object, SigTypes;
FROM ModelMisc IMPORT   StartConductor, NextConductor, AppConductor,
                        DelConductor, FindConductor,
                        X1, Y1, X2, Y2, Layer, Signal,
                        Size, ViasSize, Empty, Fixed, Ident;
FROM mCodeMnem  IMPORT  copt,mul,fmul;
FROM cdsHeap    IMPORT  Allocate, Deallocate;

VAR Shoted: BOOLEAN;
    CurSignal: Object;
    CurLayer: BITSET;
    CurSize : CARDINAL;

PROCEDURE App;
BEGIN
  Drow;
  AppConductor;
END App;

PROCEDURE Del;
BEGIN
  Delete;
  DelConductor;
END Del;

PROCEDURE OnLine(x1,y1,x2,y2: CARDINAL): BOOLEAN;
  VAR dx,dy: CARDINAL;
BEGIN
  dx:=LineXe-LineXs; dy:=LineYe-LineYs;
  RETURN ((x1-LineXs)*dy=(y1-LineYs)*dx)&((x2-LineXs)*dy=(y2-LineYs)*dx);
END OnLine;

PROCEDURE Side(x1,y1,x2,y2,x,y: CARDINAL): INTEGER;
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

PROCEDURE StrongSide(x1,y1,x2,y2,x,y: CARDINAL): INTEGER;
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

PROCEDURE Len(x1,y1,x2,y2,x,y: CARDINAL): CARDINAL;
  VAR sd,A,B,C: CARDINAL;
BEGIN
  B:=x2-x1; A:=y1-y2;
  IF NOT BOOLEAN(BITSET(A)+BITSET(B)) THEN RETURN ISQ(x1-x)+ISQ(y1-y) END;
  sd:=Side(x1,y1,x2,y2,x,y);
  IF sd<0 THEN RETURN ISQ(x1-x)+ISQ(y1-y) END;
  IF sd>0 THEN RETURN ISQ(x2-x)+ISQ(y2-y) END;
  C:=-x1*A-y1*B;
  RETURN TRUNC(SQ(FLOAT(A*x+B*y+C))/FLOAT(ISQ(A)+ISQ(B)));
END Len;

VAR ChkDX,ChkDY,ChkXs,ChkXe,ChkYs,ChkYe: CARDINAL;

PROCEDURE Check(): BOOLEAN;
  VAR r,dx,dy: CARDINAL;
BEGIN
  IF (Signal#CurSignal)&(Layer*CurLayer#{}) THEN
    IF (fantom IN Signal^.sType)OR(fantom IN CurSignal^.sType) THEN
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

VAR ip: IterProc; mask: BITSET;
    FinSeek: BOOLEAN;

PROCEDURE SeekInBox0(s: Object);
  VAR
    i,j: CARDINAL; c: Object; p,e,b: POINTER TO Segment;
    lxs,lxe,lys,lye: INTEGER;
BEGIN
  IF (Tag(s)#signal)OR(s^.ChainB=NIL)OR FinSeek THEN RETURN END;
  c:=s^.ChainB;
  IF c^.cLen=0 THEN RETURN END;
  lxs:=LineXs+8000h; lxe:=LineXe+8000h;
  lys:=LineYs+8000h; lye:=LineYe+8000h;
(*$T-*)
  b:=ADR(c^.cType[0]);
  p:=ADR(c^.cType[0]);
  e:=ADR(c^.cType[c^.cLen-1]);
  LOOP
    IF BOOLEAN(p^.size) THEN
      IF BOOLEAN(BITSET(INTEGER(BITSET(p^.start)*mask)<=lxe)*
                 BITSET(INTEGER(BITSET(p^.start>>16)*mask)<=lye)*
                 BITSET(INTEGER(BITSET(p^.end)*mask)>=lxs)*
                 BITSET(INTEGER(BITSET(p^.end>>16)*mask)>=lys)) THEN
        IF s#Signal THEN StartConductor(s) END;
        FindConductor((INTEGER(p)-INTEGER(b)) DIV SIZE(Segment));
        FinSeek:=ip();
        IF FinSeek THEN RETURN END;
      END;
    END;
    IF p=e THEN RETURN END;
    INC(INTEGER(p),SIZE(Segment));
  END;
(*$T+*)
END SeekInBox0;

PROCEDURE SeekInBox(mdl: Object; p: IterProc);
  VAR i: INTEGER;
BEGIN
  IF LineXs>LineXe THEN i:=LineXs; LineXs:=LineXe; LineXe:=i END;
  IF LineYs>LineYe THEN i:=LineYs; LineYs:=LineYe; LineYe:=i END;
  FinSeek:=FALSE; ip:=p; mask:={0..15};
  Iterate(mdl^.All,SeekInBox0);
END SeekInBox;

PROCEDURE Shoted?(size,layer: CARDINAL; mdl,sig: Object): BOOLEAN;
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

PROCEDURE Change(x1,y1,x2,y2: CARDINAL);
BEGIN
  Del; X1:=x1; Y1:=y1; X2:=x2; Y2:=y2; App;
END Change;

PROCEDURE InsertRange
  (size,layer: CARDINAL; fix: BOOLEAN; mdl,sig: Object): BOOLEAN;
  VAR l,l1,k: CARDINAL; DelMarks: ARRAY [0..499] OF BITSET; Del?: BOOLEAN;
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
  IF check_on & Shoted?(size,layer,mdl,sig) THEN RETURN TRUE END;
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

PROCEDURE DeleteRange(layer: CARDINAL; sig: Object);
  VAR r1,r2,x,y,l,l1: CARDINAL;
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
  (size,vsize: CARDINAL; fix: BOOLEAN; mdl,sig: Object): BOOLEAN;
  VAR l: CARDINAL; DelMarks: ARRAY [0..499] OF BITSET;
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
  IF check_on THEN
    IF Shoted?(size,0,mdl,sig) THEN RETURN TRUE END;
    IF Shoted?(size,1,mdl,sig) THEN RETURN TRUE END;
  END;
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

PROCEDURE DeleteVias(sig: Object);
BEGIN
  ASSERT((LineXs=LineXe)&(LineYs=LineYe));
  StartConductor(sig);
  WHILE NOT Empty DO
    IF (X1=X2)&(Y1=Y2)&(LineXs=X1)&(LineYs=Y1) THEN Del END;
    NextConductor;
  END;
END DeleteVias;

VAR FindSz: INTEGER;
    FindX : INTEGER;
    FindY : INTEGER;
    FindSig: Object;

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

PROCEDURE FindSignal(x,y,l,size: CARDINAL; mdl,sig: Object);
BEGIN
  LineXs:=x-size; LineXe:=x+size;
  LineYs:=y-size; LineYe:=y+size;
  FindSz:=size; FindX:=x; FindY:=y; FindSig:=sig;
  CurLayer:={}; INCL(CurLayer,l);
  ShotedSignal:=NIL; Signal:=NIL;
  SeekInBox(mdl,FindSg);
  IF ShotedSignal=NIL THEN Signal:=NIL END;
END FindSignal;

PROCEDURE Min(x,y: CARDINAL): CARDINAL;
BEGIN IF x<y THEN RETURN x ELSE RETURN y END END Min;

PROCEDURE Max(x,y: CARDINAL): CARDINAL;
BEGIN IF x>y THEN RETURN x ELSE RETURN y END END Max;

TYPE Range=RECORD
             minX,minY,maxX,maxY: CARDINAL;
             Id,X1,X2,Y1,Y2,Size: CARDINAL;
             Layer: BITSET
           END;
     pRange=POINTER TO Range;
(*$T-*)

VAR Idents: POINTER TO ARRAY [0..0] OF Range;
    IdCnt : CARDINAL;
    IdPtr : CARDINAL;
    MinX,MaxX,MinY,MaxY: CARDINAL;

PROCEDURE Connect(x1,y1,x2,y2,sz: CARDINAL; ls: BITSET; p: pRange): BOOLEAN;
  VAR r,r1,dx,dy,cdx,cdy: CARDINAL;
BEGIN
  WITH p^ DO
    r1:=Size+sz-Overlay;
    r:=r1*r1;
    dx:=X2-X1; dy:=Y2-Y1;
    cdx:=x2-x1; cdy:=y2-y1;
    RETURN
      ((cdx*(Y1-y1)-cdy*(X1-x1)>=0)#(cdx*(Y2-y1)-cdy*(X2-x1)>=0))
      & ((dx*(y1-Y1)-dy*(x1-X1)>=0)#(dx*(y2-Y1)-dy*(x2-X1)>=0))
      OR (Len(x1,y1,x2,y2,X1,Y1)<r)
      OR (Len(x1,y1,x2,y2,X2,Y2)<r)
      OR (Len(X1,Y1,X2,Y2,x1,y1)<r)
      OR (Len(X1,Y1,X2,Y2,x2,y2)<r);
  END;
END Connect;

PROCEDURE Areas(sig: Object): CARDINAL;
  VAR size,from,to,i: CARDINAL; p,q,r: pRange;
BEGIN
  ASSERT(Tag(sig)=signal);
  IdCnt:=0; IdPtr:=1;
  StartConductor(sig); size:=0;
  WHILE NOT Empty DO INC(size); NextConductor END;
  IF size<=1 THEN RETURN size END;
  Allocate(Idents,size*TSIZE(Range));
  StartConductor(sig); i:=0;
  WHILE NOT Empty DO
    ASSERT(i<size);
    WITH Idents^[i] DO
      X1:=ModelMisc.X1; X2:=ModelMisc.X2;
      Y1:=ModelMisc.Y1; Y2:=ModelMisc.Y2;
      Size:=ModelMisc.Size;
      minX:=Min(X1,X2)-Size; maxX:=Max(X1,X2)+Size;
      minY:=Min(Y1,Y2)-Size; maxY:=Max(Y1,Y2)+Size;
      Layer:=ModelMisc.Layer;
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
        IF (Id#p^.Id)&(Layer*p^.Layer#{}) THEN
          IF NOT ( (minX>=p^.maxX)OR(p^.minX>=maxX)OR
                   (minY>=p^.maxY)OR(p^.minY>=maxY) ) &
                   Connect(X1,Y1,X2,Y2,Size,Layer,p) THEN
            from:=Id; to:=p^.Id;
            q:=ADR(Idents^[0]);
            REPEAT
              IF q^.Id=from THEN q^.Id:=to END;
              INC(CARDINAL(q),TSIZE(Range));
            UNTIL q>=r;
            DEC(IdCnt);
          END;
        END;
        INC(CARDINAL(p),TSIZE(Range));
      END;
    END;
  END;
  Deallocate(Idents,size*TSIZE(Range));
  RETURN IdCnt;
END Areas;

BEGIN
  check_on:=TRUE;
END pedTopology.
