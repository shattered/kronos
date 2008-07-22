IMPLEMENTATION MODULE cdsSeq; (* Sem 05-Jan-88. (c) KRONOS *)

IMPORT  t: Terminal;

IMPORT  sys: SYSTEM;
IMPORT  mcd: defCodes;
IMPORT  Model;            FROM Model      IMPORT  Objects;
IMPORT  mm: ModelMisc;
IMPORT  heap: cdsHeap;
IMPORT  tpl: pedTopology;
IMPORT  img: Strings;

TYPE ADDRESS=sys.ADDRESS;
     WORD   =sys.WORD;
     Object =Model.Object;

PROCEDURE Tag(o: Object): Objects;
CODE 0 mcd.lxb END Tag;

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

VAR Idents   : POINTER TO ARRAY [0..0] OF Range;
    RangeCnt : INTEGER;
    IdCnt    : INTEGER;
    IdPtr    : INTEGER;
    CurSignal: Object;
    CurIdent : INTEGER;

PROCEDURE Connect(x1,y1,x2,y2,sz: INTEGER; ls: BITSET; p: pRange): BOOLEAN;
  VAR r,r1,dx,dy,cdx,cdy: INTEGER;
BEGIN
  WITH p^ DO
    r1:=Size+sz-tpl.Overlay;
    r:=r1*r1;
    dx:=X2-X1; dy:=Y2-Y1;
    cdx:=x2-x1; cdy:=y2-y1;
    RETURN
      ((cdx*(Y1-y1)-cdy*(X1-x1)>=0)#(cdx*(Y2-y1)-cdy*(X2-x1)>=0))
      & ((dx*(y1-Y1)-dy*(x1-X1)>=0)#(dx*(y2-Y1)-dy*(x2-X1)>=0))
      OR (tpl.Len(x1,y1,x2,y2,X1,Y1)<r)
      OR (tpl.Len(x1,y1,x2,y2,X2,Y2)<r)
      OR (tpl.Len(X1,Y1,X2,Y2,x1,y1)<r)
      OR (tpl.Len(X1,Y1,X2,Y2,x2,y2)<r);
  END;
END Connect;

PROCEDURE Areas(sig: Object; Epin: BOOLEAN);
  VAR from,to,i: INTEGER; p,q,r: pRange;
BEGIN
t.print('Areas\n');
  ASSERT(Tag(sig)=signal);
  IdCnt:=0; IdPtr:=1;
  mm.StartConductor(sig,Epin); RangeCnt:=0;
  WHILE NOT mm.Empty DO INC(RangeCnt); mm.NextConductor END;
  heap.Allocate(Idents,RangeCnt*SIZE(Range));
  mm.StartConductor(sig,Epin); i:=0;
  WHILE NOT mm.Empty DO
    ASSERT(i<RangeCnt);
    WITH Idents^[i] DO
      X1:=mm.X1; X2:=mm.X2;
      Y1:=mm.Y1; Y2:=mm.Y2;
      Size:=mm.Size;
      minX:=Min(X1,X2)-Size; maxX:=Max(X1,X2)+Size;
      minY:=Min(Y1,Y2)-Size; maxY:=Max(Y1,Y2)+Size;
      Layer:=mm.Layer;
      Id:=IdPtr; INC(IdPtr); INC(IdCnt);
    END;
    INC(i);
    mm.NextConductor;
  END;
  r:=sys.ADR(Idents^[RangeCnt]);
t.print('MaxRange=%d\n',RangeCnt);
  FOR i:=0 TO RangeCnt-1 DO
t.print('RangeCnt=%d\r',i);
    p:=sys.ADR(Idents^[i+1]);
    WITH Idents^[i] DO
      WHILE p<r DO
        IF (Id#p^.Id)&(Layer*p^.Layer#{}) THEN
          IF NOT ( (minX>=p^.maxX)OR(p^.minX>=maxX)OR
                   (minY>=p^.maxY)OR(p^.minY>=maxY) ) &
                   Connect(X1,Y1,X2,Y2,Size,Layer,p) THEN
            from:=Id; to:=p^.Id;
            q:=sys.ADR(Idents^[0]);
            REPEAT
              IF q^.Id=from THEN q^.Id:=to END;
              q:=ADDRESS(INTEGER(q)+SIZE(Range));
            UNTIL q=r;
            DEC(IdCnt);
          END;
        END;
        p:=ADDRESS(INTEGER(p)+SIZE(Range));
      END (* WHILE *);
    END (* WITH *);
  END (* FOR *);
t.print('\n');
END Areas;

TYPE PinRec=RECORD pin: Object; ident: INTEGER END;

VAR Pins    : ARRAY [0..399] OF PinRec;
    PinsCnt : INTEGER;
    MaxIdent: INTEGER;

PROCEDURE PinLocation(o: Object; VAR X,Y: INTEGER);
  VAR p: Object; X1,Y1,R: INTEGER;
BEGIN
  ASSERT ((o#NIL)&(Tag(o)=pin)&(o^.Chip#NIL));
  X:=0; Y:=0;
  p:=o^.Chip^.ChipType;
  ASSERT((p#NIL)&(Tag(p)=chiptype));
  X1:=o^.Chip^.XB;
  Y1:=o^.Chip^.YB;
  R :=o^.Chip^.RB;
  p:=Model.Lget(p^.ExternalPins,o^.No);
  ASSERT((p#NIL)&(Tag(p)=externalpin));
  CASE R MOD 4 OF
    0: X:=X1+p^.PinX; Y:=Y1+p^.PinY;
   |1: X:=X1+p^.PinY; Y:=Y1-p^.PinX;
   |2: X:=X1-p^.PinX; Y:=Y1-p^.PinY;
   |3: X:=X1-p^.PinY; Y:=Y1+p^.PinX;
  END;
END PinLocation;

PROCEDURE MarkPin(p: Object; info: WORD);
  VAR i,x,y: INTEGER;
BEGIN
  PinLocation(p,x,y);
  FOR i:=0 TO RangeCnt-1 DO
    WITH Idents^[i] DO
      IF tpl.Len(X1,Y1,X2,Y2,x,y)<Size*Size THEN
        IF PinsCnt<=HIGH(Pins) THEN
          Pins[PinsCnt].pin:=p;
          Pins[PinsCnt].ident:=Id;
          IF Id>MaxIdent THEN MaxIdent:=Id END;
          INC(PinsCnt);
        END;
        RETURN;
      END;
    END;
  END;
END MarkPin;

PROCEDURE Next(sig: Object; VAR x,y: INTEGER; Epin: BOOLEAN);
  VAR i,id: INTEGER;
BEGIN
  IF CurSignal#sig THEN
    Areas(sig,Epin);
    PinsCnt:=0; MaxIdent:=-1;
    Model.Iterate(sig^.TiedPins,MarkPin,0);
    CurSignal:=sig; CurIdent:=MaxIdent;
    heap.Deallocate(Idents,RangeCnt*SIZE(Range));
  END;
  IF PinsCnt=0 THEN RETURN END;
  id:=CurIdent; i:=PinsCnt;
  REPEAT
    IF i>=PinsCnt-1 THEN
      i:=0; INC(id); IF id>MaxIdent THEN id:=0 END;
    ELSE
      INC(i)
    END;
  UNTIL (Pins[i].ident=id)OR(id=CurIdent);
  CurIdent:=id;
  PinLocation(Pins[i].pin,x,y);
END Next;

PROCEDURE PinNet(sig: Object; ip: IterProc; Epin: BOOLEAN);
  VAR x,y,x1,y1,id,id1,i,j: INTEGER;
BEGIN
  IF CurSignal#sig THEN
    Areas(sig,Epin);
    PinsCnt:=0; MaxIdent:=-1;
    Model.Iterate(sig^.TiedPins,MarkPin,0);
    CurSignal:=sig; CurIdent:=MaxIdent;
    heap.Deallocate(Idents,RangeCnt*SIZE(Range));
  END;
  IF PinsCnt=0 THEN RETURN END;
  PinLocation(Pins[0].pin,x,y);
  id:=Pins[0].ident;
  FOR i:=1 TO PinsCnt-1 DO
    IF Pins[i].ident#id THEN
      id1:=Pins[i].ident;
      PinLocation(Pins[i].pin,x1,y1);
      ip(x,y,x1,y1);
      FOR j:=i TO PinsCnt-1 DO
        IF Pins[j].ident=id1 THEN Pins[j].ident:=id END;
      END;
    END;
  END;
END PinNet;

(*
PROCEDURE DelPin(p: Object; d: INTEGER);
BEGIN
  IF Tag(p)#pin THEN RETURN END;
  IF p^.Signal=FindSig THEN
    p^.Signal:=Free;
    IF Free#NIL THEN Tie(Free^.TiedPins,p) END;
  END;
END DelPin;
*)

PROCEDURE DisConnect(mdl,sig: Object);
  VAR i,j,cnt,CurId: INTEGER;
      sg: Object;
      name: ARRAY [0..15] OF CHAR;
BEGIN
  Areas(sig,FALSE);
t.print('DisConnect\n');
  cnt:=0; CurId:=0;
  FOR i:=1 TO RangeCnt DO
t.print('RangeCnt=%d\r',i);
    FOR j:=0 TO RangeCnt-1 DO
      WITH Idents^[j] DO
        IF Id=i THEN
          IF CurId#Id THEN
            sg:=Model.NewObject(signal);
            Model.Tie(mdl^.All,sg);
            img.print(name,'signal_%d',cnt); INC(cnt);
            sg^.Name:=name;
            CurId:=Id;
            mm.StartConductor(sg,FALSE);
          END;
          mm.X1:=X1; mm.X2:=X2; mm.Y1:=Y1; mm.Y2:=Y2;
          mm.Size:=Size; mm.Layer:=Layer; mm.Fixed:=TRUE;
          mm.AppConductor(FALSE)
        END;
      END;
    END;
  END;
t.print('\nDelete free\n');
  mm.StartConductor(sig,FALSE);
  WHILE NOT mm.Empty DO mm.DelConductor; mm.NextConductor END;
  Model.UnTie(mdl^.All,sig);
  --Iterate(mdl^.All,DelPin,0);
  --Iterate(sig^.TiedPins,DelPin,0);
  Model.KillObject(sig^.ChainB);
  Model.KillList  (sig^.TiedPins);
  Model.KillObject(sig);
  heap.Deallocate(Idents,RangeCnt*SIZE(Range));
END DisConnect;

BEGIN
  CurSignal:=NIL; Idents:=NIL; IdCnt:=0;
END cdsSeq.
