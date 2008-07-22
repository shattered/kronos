IMPLEMENTATION MODULE pedTopology;(* Sem 06-Mar-87. (c) KRONOS *)

                IMPORT ModelMisc;
FROM SYSTEM     IMPORT  ADR, ADDRESS, WORD;
FROM pedEditor  IMPORT  Sheet;
FROM pedScreen  IMPORT  Drow, Delete;
FROM Model      IMPORT  Objects, Iterate, Segment, Object, SigTypes;
FROM ModelMisc  IMPORT  StartConductor, NextConductor, AppConductor,
                        DelConductor, FindConductor, UnPackSeg,
                        X1, Y1, X2, Y2, Layer, Signal,
                        Size, ViasSize, Empty, Fixed, Ident;
FROM defCodes   IMPORT  copt,mul,fmul,lxb,move;
FROM cdsHeap    IMPORT  Allocate, Deallocate;

VAR Shoted: BOOLEAN;
    CurSignal: Object;
    CurLayer: BITSET;
    CurSize : INTEGER;

PROCEDURE Tag(o: Object): Objects;
CODE 0 lxb END Tag;

PROCEDURE MOVE(t,f: ADDRESS; s: INTEGER); CODE move END MOVE;

PROCEDURE App(sht: Sheet);
BEGIN
  Drow(sht);
  AppConductor(sht^.PublicContext^.ExtPin);
END App;

PROCEDURE Del(sht: Sheet);
BEGIN
  Delete(sht);
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

TYPE pSegment = POINTER TO Segment;

PROCEDURE chk_box(p1,p2: pSegment): BOOLEAN; CODE 0F8h END chk_box;

PROCEDURE SeekInBox0(s: Object; ExtPin: BOOLEAN);
  VAR i,j: INTEGER; c: Object; p,e,b: pSegment; l: Segment;
BEGIN
  IF ExtPin THEN
    IF (Tag(s)#externalpin)OR(s^.PinType=NIL)OR FinSeek THEN RETURN END;
    c:=s^.PinType;
  ELSE
    IF (Tag(s)#signal)     OR(s^.ChainB =NIL)OR FinSeek THEN RETURN END;
    c:=s^.ChainB;
  END;
  IF c^.cFree=0 THEN RETURN END;
  l.start:=LineXs+8000h+INTEGER((LineYs+8000h)<<16);
  l.end  :=LineXe+8000h+INTEGER((LineYe+8000h)<<16);
(*$T-*)
  b:=ADR(c^.cType[0]);
  p:=ADR(c^.cType[0]);
  e:=ADR(c^.cType[c^.cFree]);
  Signal:=s;
  REPEAT
    IF chk_box(p,ADR(l)) THEN
      UnPackSeg(p^); FinSeek:=ip();
      IF FinSeek THEN RETURN END;
    END;
    p:=ADDRESS(INTEGER(p)+SIZE(Segment));
  UNTIL p=e;
(*$T+*)
END SeekInBox0;

PROCEDURE SeekInBox(sht: Sheet; p: IterProc);
  VAR i: INTEGER;
BEGIN
  IF LineXs>LineXe THEN i:=LineXs; LineXs:=LineXe; LineXe:=i END;
  IF LineYs>LineYe THEN i:=LineYs; LineYs:=LineYe; LineYe:=i END;
  FinSeek:=FALSE; ip:=p; mask:={0..15};
  IF sht^.PublicContext^.ExtPin THEN
    Iterate(sht^.mdl^.ExternalPins,SeekInBox0,sht^.PublicContext^.ExtPin);
  ELSE
    Iterate(sht^.mdl^.All,SeekInBox0,sht^.PublicContext^.ExtPin);
  END;
END SeekInBox;

PROCEDURE Shoted?(size,layer: INTEGER; sht: Sheet; sig: Object): BOOLEAN;
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
  SeekInBox(sht,Check);
  LineXs:=ChkXs; LineXe:=ChkXe;
  LineYs:=ChkYs; LineYe:=ChkYe;
  RETURN Shoted;
END Shoted?;

PROCEDURE Change(x1,y1,x2,y2: INTEGER; sht: Sheet);
BEGIN
  Del(sht); X1:=x1; Y1:=y1; X2:=x2; Y2:=y2; App(sht);
END Change;

PROCEDURE InsertRange
  (size,layer: INTEGER; fix: BOOLEAN; sht: Sheet; sig: Object;
   chk: BOOLEAN): BOOLEAN;
  VAR l,l1,k,Xs,Ys,Xe,Ye: INTEGER;
      ls: BITSET;
      c: Object;
      ln: Segment;
      p,e,b: pSegment;
      Del?: BOOLEAN;

BEGIN
  IF (LineXs=LineXe)&(LineYs=LineYe) THEN RETURN FALSE END;
  IF chk&Shoted?(size,layer,sht,sig) THEN RETURN TRUE END;
  ls:={}; INCL(ls,layer);
  IF sht^.PublicContext^.ExtPin THEN c:=sig^.PinType ELSE c:=sig^.ChainB END;
  IF c^.cFree#0 THEN
    IF LineXs>LineXe THEN Xe:=LineXs; Xs:=LineXe
    ELSE                  Xs:=LineXs; Xe:=LineXe END;
    IF LineYs>LineYe THEN Ye:=LineYs; Ys:=LineYe
    ELSE                  Ys:=LineYs; Ye:=LineYe END;
    ln.start:=Xs-size+8000h+INTEGER((Ys-size+8000h)<<16);
    ln.end  :=Xe+size+8000h+INTEGER((Ye+size+8000h)<<16);
(*$T-*)
    b:=ADR(c^.cType[0]);
    p:=ADR(c^.cType[0]);
    e:=ADR(c^.cType[c^.cFree]);
    StartConductor(sig,sht^.PublicContext^.ExtPin);
    REPEAT
      IF chk_box(p,ADR(ln)) THEN
        UnPackSeg(p^);
        Del?:=FALSE;
        IF (Layer=ls)&(ViasSize=0)&(fix=Fixed)&OnLine(X1,Y1,X2,Y2) THEN
          l:=Side(X1,Y1,X2,Y2,LineXs,LineYs);
          l1:=Side(X1,Y1,X2,Y2,LineXe,LineYe);
          IF (l1=0)&(l=0) THEN
            IF Size>=size THEN RETURN FALSE END;
          ELSIF l=0 THEN
            IF Size=size THEN
              IF l1>0 THEN LineXs:=X1; LineYs:=Y1; Del?:=TRUE;
              ELSE         LineXs:=X2; LineYs:=Y2; Del?:=TRUE END;
            END;
          ELSIF l1=0 THEN
            IF Size=size THEN
              IF l>0 THEN LineXe:=X1; LineYe:=Y1; Del?:=TRUE;
              ELSE        LineXe:=X2; LineYe:=Y2; Del?:=TRUE END;
            END;
          ELSIF ((l<0)#(l1<0))&(Size<=size) THEN Del?:=TRUE END;
        END;
        IF Del? THEN
          Del(sht);
          e:=ADDRESS(e)-SIZE(Segment); p:=ADDRESS(p)-SIZE(Segment);
        END;
      END;
      p:=ADDRESS(p)+SIZE(Segment); INC(Ident);
    UNTIL p=e;
(*$T+*)
  END;
  StartConductor(sig,sht^.PublicContext^.ExtPin);
  X1:=LineXs; Y1:=LineYs; X2:=LineXe; Y2:=LineYe;
  Layer:=ls; Size:=size; ViasSize:=0; Fixed:=fix;
  App(sht);
  RETURN FALSE;
END InsertRange;

PROCEDURE DeleteRange(layer: INTEGER; sig: Object; sht: Sheet);
  VAR Id,l,l1,Xs,Ys,Xe,Ye,r1,r2,x,y: INTEGER;
      c: Object;
      ln: Segment;
      p,e,b: pSegment;
BEGIN
  IF (LineXs=LineXe)&(LineYs=LineYe) THEN RETURN END;
  IF sht^.PublicContext^.ExtPin THEN c:=sig^.PinType ELSE c:=sig^.ChainB END;
  IF c^.cFree#0 THEN
    IF LineXs>LineXe THEN Xe:=LineXs; Xs:=LineXe
    ELSE                  Xs:=LineXs; Xe:=LineXe END;
    IF LineYs>LineYe THEN Ye:=LineYs; Ys:=LineYe
    ELSE                  Ys:=LineYs; Ye:=LineYe END;
    ln.start:=Xs+8000h+INTEGER((Ys+8000h)<<16);
    ln.end  :=Xe+8000h+INTEGER((Ye+8000h)<<16);
(*$T-*)
    b:=ADR(c^.cType[0]);
    p:=ADR(c^.cType[0]);
    e:=ADR(c^.cType[c^.cFree]);
    StartConductor(sig,sht^.PublicContext^.ExtPin);
    Id:=0;
    REPEAT
      IF chk_box(p,ADR(ln)) THEN
        UnPackSeg(p^);
        IF (layer IN Layer)&OnLine(X1,Y1,X2,Y2) THEN
          l:=StrongSide(X1,Y1,X2,Y2,LineXs,LineYs);
          l1:=StrongSide(X1,Y1,X2,Y2,LineXe,LineYe);
          Ident:=Id;
          IF (l=0)&(l1=0) THEN
            r1:=(X1-LineXs)*(X1-LineXs)+(Y1-LineYs)*(Y1-LineYs);
            r2:=(X1-LineXe)*(X1-LineXe)+(Y1-LineYe)*(Y1-LineYe);
            IF r1<r2 THEN
              x:=X2; y:=Y2; Change(X1,Y1,LineXs,LineYs,sht);
              X1:=LineXe; Y1:=LineYe; X2:=x; Y2:=y;
              App(sht);
            ELSE
              x:=X2; y:=Y2; Change(X1,Y1,LineXe,LineYe,sht);
              X1:=LineXs; Y1:=LineYs; X2:=x; Y2:=y;
              App(sht);
            END;
            p:=ADDRESS(p)-SIZE(Segment); DEC(Id);
          ELSIF l=0 THEN
            IF l1>0 THEN Change(X1,Y1,LineXs,LineYs,sht)
            ELSE Change(LineXs,LineYs,X2,Y2,sht) END;
            p:=ADDRESS(p)-SIZE(Segment); DEC(Id);
          ELSIF l1=0 THEN
            IF l>0 THEN Change(X1,Y1,LineXe,LineYe,sht)
            ELSE Change(LineXe,LineYe,X2,Y2,sht) END;
            p:=ADDRESS(p)-SIZE(Segment); DEC(Id);
          ELSIF ((l<0)#(l1<0))& NOT((X1=X2)&(Y1=Y2)) THEN
            Del(sht);
            e:=ADDRESS(e)-SIZE(Segment); p:=ADDRESS(p)-SIZE(Segment);
            DEC(Id);
          END;
        END;
      END;
      p:=ADDRESS(p)+SIZE(Segment); INC(Id);
    UNTIL p=e;
  END;
(*$T+*)
END DeleteRange;

PROCEDURE InsertVias
  (size,vsize: INTEGER; fix: BOOLEAN; sht: Sheet; sig: Object;
   chk: BOOLEAN): BOOLEAN;
  VAR l: INTEGER;
      c: Object;
      ln: Segment;
      p,e,b: pSegment;
BEGIN
  ASSERT((LineXs=LineXe)&(LineYs=LineYe));
  IF chk&(Shoted?(size,0,sht,sig) OR Shoted?(size,1,sht,sig)) THEN
    RETURN TRUE
  END;
  IF sht^.PublicContext^.ExtPin THEN c:=sig^.PinType ELSE c:=sig^.ChainB END;
  IF c^.cFree#0 THEN
    ln.start:=LineXs-size+8000h+INTEGER((LineYs-size+8000h)<<16);
    ln.end  :=LineXe+size+8000h+INTEGER((LineYe+size+8000h)<<16);
(*$T-*)
    b:=ADR(c^.cType[0]);
    p:=ADR(c^.cType[0]);
    e:=ADR(c^.cType[c^.cFree]);
    StartConductor(sig,sht^.PublicContext^.ExtPin);
    REPEAT
      IF chk_box(p,ADR(ln)) THEN
        UnPackSeg(p^);
        IF (Fixed=fix)&(X1=X2)&(Y1=Y2)&(X1=LineXs)&(Y1=LineYs) THEN
          IF Size>size THEN size:=Size END;
          IF (Layer={0,1})&(Size=size)&(ViasSize=vsize) THEN RETURN FALSE END;
          Del(sht);
          e:=ADDRESS(e)-SIZE(Segment); p:=ADDRESS(p)-SIZE(Segment);
        END;
      END;
      p:=ADDRESS(p)+SIZE(Segment); INC(Ident);
    UNTIL p=e;
(*$T+*)
  END;
  StartConductor(sig,sht^.PublicContext^.ExtPin);
  X1:=LineXs; Y1:=LineYs; X2:=LineXe; Y2:=LineYe;
  Layer:={0,1}; Size:=size; ViasSize:=vsize; Fixed:=fix;
  App(sht);
  RETURN FALSE;
END InsertVias;

PROCEDURE DeleteVias(sig: Object; sht: Sheet);
VAR   c: Object;
      ln: Segment;
      p,e,b: pSegment;
BEGIN
  ASSERT((LineXs=LineXe)&(LineYs=LineYe));
  StartConductor(sig,sht^.PublicContext^.ExtPin);
  IF sht^.PublicContext^.ExtPin THEN c:=sig^.PinType ELSE c:=sig^.ChainB END;
  IF c^.cFree#0 THEN
    ln.start:=LineXs+8000h+INTEGER((LineYs+8000h)<<16);
    ln.end  :=LineXe+8000h+INTEGER((LineYe+8000h)<<16);
(*$T-*)
    b:=ADR(c^.cType[0]);
    p:=ADR(c^.cType[0]);
    e:=ADR(c^.cType[c^.cFree]);
    StartConductor(sig,sht^.PublicContext^.ExtPin);
    REPEAT
      IF chk_box(p,ADR(ln)) THEN
        UnPackSeg(p^);
        IF (X1=X2)&(Y1=Y2)&(LineXs=X1)&(LineYs=Y1) THEN
          Del(sht);
          e:=ADDRESS(e)-SIZE(Segment); p:=ADDRESS(p)-SIZE(Segment);
        END;
      END;
      p:=ADDRESS(p)+SIZE(Segment); INC(Ident);
    UNTIL p=e;
(*$T+*)
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

PROCEDURE FindSignal(x,y,l,size: INTEGER; sht: Sheet; sig: Object);
BEGIN
  LineXs:=x-size; LineXe:=x+size;
  LineYs:=y-size; LineYe:=y+size;
  FindSz:=size; FindX:=x; FindY:=y; FindSig:=sig;
  CurLayer:={}; INCL(CurLayer,l);
  ShotedSignal:=NIL; Signal:=NIL;
  SeekInBox(sht,FindSg);
  IF ShotedSignal=NIL THEN Signal:=NIL END;
END FindSignal;

PROCEDURE Min(x,y: INTEGER): INTEGER;
BEGIN IF x<y THEN RETURN x ELSE RETURN y END END Min;

PROCEDURE Max(x,y: INTEGER): INTEGER;
BEGIN IF x>y THEN RETURN x ELSE RETURN y END END Max;

TYPE Range=RECORD
             minX,minY,maxX,maxY: INTEGER;
             Id,X1,X2,Y1,Y2,Size: INTEGER;
             Layer: BITSET
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

PROCEDURE Areas(sig: Object; sht: Sheet): INTEGER;
  VAR size,from,to,i: INTEGER; p,q,r: pRange;
BEGIN
  ASSERT(Tag(sig)=signal);
  IdCnt:=0; IdPtr:=1;
  StartConductor(sig,sht^.PublicContext^.ExtPin); size:=0;
  WHILE NOT Empty DO INC(size); NextConductor END;
  IF size<=1 THEN RETURN size END;
  Allocate(Idents,size*SIZE(Range));
  StartConductor(sig,sht^.PublicContext^.ExtPin); i:=0;
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
              q:=ADDRESS(INTEGER(q)+SIZE(Range));
            UNTIL q>=r;
            DEC(IdCnt);
          END;
        END;
        p:=ADDRESS(INTEGER(p)+SIZE(Range));
      END;
    END;
  END;
  Deallocate(Idents,size*SIZE(Range));
  RETURN IdCnt;
END Areas;

END pedTopology.
