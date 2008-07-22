IMPLEMENTATION MODULE fcObj; (* 03-Nov-88.  *)
IMPORT StdIO;

FROM SYSTEM  IMPORT ADDRESS, WORD, ADR;
FROM fcDefs  IMPORT NoIds, dplen,cxlen;
FROM fcScan  IMPORT Fault, Error, Symbol;
FROM fcHeap  IMPORT Give, Free;

CONST eol=0c;

(* ------------- I D E N T T A B L E ------------------ *)
VAR Ip: POINTER TO Idname;
    Ibusy: [0..NoIds];
VAR Col,Sum,Len,looks:INTEGER; C:CHAR;

CONST Ilim=NoIds-30;

PROCEDURE MOVE(t,f: ADDRESS; s: INTEGER); CODE 0C0h END MOVE;

PROCEDURE Hash(VAL IdName:Idname);
BEGIN
  Sum:=0; Len:=0;
  REPEAT  C:=IdName[Len];
    Sum := INTEGER(BITSET(Sum)/BITSET(C<<Len));
    INC(Len);
  UNTIL C = eol;
  Sum := Sum MOD NoIds;
END Hash;

PROCEDURE StrId(VAL IdName:Idname):INTEGER;
VAR R:INTEGER;
BEGIN (*$T-*) INC(looks);
  Hash(IdName); Len:=(Len+3) DIV 4; R:=13;
  LOOP
    IF Id[Sum]=NIL THEN
      IF Ibusy > Ilim THEN Fault(13) END;
      Give(Id[Sum],Len);
      MOVE(Id[Sum],ADR(IdName),Len); INC(Ibusy);
      RETURN Sum;
    END;
    Ip:=Id[Sum];
    IF IdName=Ip^ THEN RETURN Sum; END;
    Sum := (Sum+R) MOD NoIds;  R := (R+Sum) MOD NoIds;
    INC(Col);
  END; (*$T+*)
END StrId;

PROCEDURE Find(VAR IdName:Idname):INTEGER;
  VAR R:INTEGER;
BEGIN (*$T-*)
  Hash(IdName); R:=13;
  LOOP
    IF Id[Sum]=NIL THEN RETURN NoIds;
    ELSE Ip:=Id[Sum];
      IF IdName=Ip^ THEN RETURN Sum; END;
      Sum:=(Sum+R) MOD NoIds;  R := (R+Sum) MOD NoIds;
    END;
  END; (*$T+*)
END Find;

PROCEDURE IdStr(d:INTEGER;VAR IdName:Idname);
BEGIN
  IF Id[d]=NIL THEN IdName[0]:=177c; IdName[1]:=eol; RETURN; END;
  Ip:=Id[d]; Len:=0;
  WHILE Ip^[Len]#eol DO INC(Len); END;
  INC(Len); Len:=(Len+3) DIV 4;
  MOVE(ADR(IdName),Id[d],Len);
END IdStr;

PROCEDURE DelId(d:INTEGER);
BEGIN
    Ip:=Id[d]; Len:=0; IF Ip=NIL THEN RETURN END;
    WHILE Ip^[Len]#eol DO INC(Len); END;
    INC(Len); Len:=(Len+3) DIV 4;
    Free(Id[d],Len); DEC(Ibusy);
END DelId;

PROCEDURE InitId;
VAR h: INTEGER;
BEGIN Col:=0; looks:=0; Ibusy:=0;
  FOR h:=0 TO HIGH(Id) DO
    Id[h]:=NIL;
  END;
END InitId;

PROCEDURE VisIdTable;
  VAR IdName:Idname; h:INTEGER;I:Info;
      tp:Types;
BEGIN
  StdIO.WriteString(
"               TABLE OF SYMBOLS ");
  StdIO.WriteLn;
  StdIO.WriteLn;
  StdIO.WriteString(
" idno      name       md typ cls len area  bits   off com  equ dim");
  StdIO.WriteLn;
  FOR h:=0 TO HIGH(Id) DO
  IF Id[h]#NIL THEN
    IdStr(h,IdName); StdIO.print("%5d %15s",h,IdName);
    IF GetMode(h)#Empty THEN
      I.name:=h; Unpack(I);
      CASE GetMode(h) OF
      Proc    : StdIO.WriteString(" Pro");
      |Var    : StdIO.WriteString(" Var");
      |xVar   : StdIO.WriteString(" xVr");
      |Const  : StdIO.WriteString(" Con");
      |Array  : StdIO.WriteString(" Arr");
      |LocVar : StdIO.WriteString(" LcV");
      END;
      tp:=GetType(h);
      CASE tp OF
      Int    : StdIO.WriteString(" I ");
      |Real  : StdIO.WriteString(" R ");
      |Double: StdIO.WriteString(" D ");
      |Complex:StdIO.WriteString(" Cx");
      |Logic:  StdIO.WriteString(" L ");
      |Char :  StdIO.WriteString(" Ch");
      |Holl :  StdIO.WriteString(" H ");
      |Undef:  StdIO.WriteString(" U ");
      END;
      CASE I.cl OF
      Func  :  StdIO.WriteString(" Fun");
      |Subr :  StdIO.WriteString(" Sub");
      |Intr :  StdIO.WriteString(" Inr");
      |Ext  :  StdIO.WriteString(" Ext");
      |Param:  StdIO.WriteString(" Par");
      |Local:  StdIO.WriteString(" Loc");
      |Global: StdIO.WriteString(" Glo");
      END;
      StdIO.print("%4d%4d%8{}",I.lenel,I.darea,I.bits);
      CASE GetMode(h) OF
      Proc    : StdIO.print("%5d",I.offset);
      |Var    : StdIO.print("%5d%4d%4d",I.offset,I.commP,I.equiP);
      |xVar   : StdIO.print("%5d",I.offset);
      |Const  : IF (tp=Int) OR (tp=Logic) THEN
                  StdIO.print("%5d",I.offset);
                ELSIF (tp=Real) OR (tp=Double) THEN
                  StdIO.print("%e",REAL(I.offset));
                ELSIF (tp=Char) OR (tp=Holl)   THEN
                  StdIO.print("%5d",I.offset);
                ELSIF tp=Complex THEN
                  StdIO.print("%e %e",REAL(I.offset),REAL(I.commP) );
                END;
      |Array  : StdIO.print("%5d%4d%4d%3d",I.offset,I.commP,I.equiP,I.dim);
      |LocVar : StdIO.print("%5d",I.offset);
      END;
    ELSE
    --Empty
    END;
    StdIO.WriteLn;
  END;
  END; (* for-cycle *)
  StdIO.print('  busy=%d  looks=%d collisions= %d\n',Ibusy,looks,Col);
END VisIdTable;

(* ---------------E N D   I D E N T T A B L E --------------- *)
(* --------------- O B J E C T S ---------------------------- *)

TYPE P =POINTER TO so;
     so=RECORD
          idn:INTEGER;
          tp:Types;
          md:Mode;
          pnt:ADDRESS;
          next:P
        END;

VAR Itype: ARRAY [0..NoIds] OF CHAR;
    Imode: ARRAY [0..NoIds] OF CHAR;
    Object:ARRAY [0..NoIds] OF ADDRESS; (*указатели на упакованные об'екты*)
    SO:P;

PROCEDURE SetMode(d:INTEGER;M:Mode);
BEGIN Imode[d]:=CHAR(ORD(M)); END SetMode;

PROCEDURE GetMode(d:INTEGER):Mode;
BEGIN RETURN Mode(ORD(Imode[d])); END GetMode;

PROCEDURE SetType(d:INTEGER;T:Types);
BEGIN Itype[d]:=CHAR(ORD(T)); END SetType;

PROCEDURE GetType(d:INTEGER):Types;
BEGIN RETURN Types(ORD(Itype[d])); END GetType;

CONST xx=777;

PROCEDURE GetDefType(c:CHAR; VAR T:Types; VAR len: INTEGER);
VAR p: PchImpl;
BEGIN
  T:=Types(INTEGER(BITSET(Default[c])*{0..6})); len:=1;
  IF T=Double THEN len:=dplen
  ELSIF T=Complex THEN  len:=cxlen
  ELSIF T=Char THEN
    len:=0; p:=Pimpl;
    WHILE p#NIL DO
      IF ( c >= p^.c1 ) AND ( c <= p^.c2 ) THEN
          len:=p^.len; RETURN
      END;
      p:=p^.next
    END;
    Error(xx)
  END;
END GetDefType;

PROCEDURE SetImplType;
  VAR IdName:Idname; h:INTEGER;I:Info;
      tp:Types;
BEGIN
  FOR h:=0 TO HIGH(Id) DO
  IF Id[h]#NIL THEN
    IF GetMode(h)#Empty THEN
      IF GetType(h)#Undef THEN
        I.name:=h; Unpack(I);
        IF NOT(tpbit IN I.bits) THEN
           IdStr(h,IdName);
           GetDefType(IdName[0],tp,I.lenel);
           SetType(h,tp); Pack(I);
        END;
      END;
    END;
  END;
  END;
END SetImplType;

PROCEDURE InitObjects;
VAR h: INTEGER; C:CHAR;
BEGIN
  SO:=NIL; Pimpl:=NIL;
  InitId;
  FOR h:=0 TO HIGH(Itype) DO
    Object[h]:=NIL; Itype[h]:=CHAR(ORD(Undef));
    Imode[h]:=CHAR(ORD(Empty));
  END;
  FOR C:=0c TO 377c DO Default[C]:=CHAR(ORD(Real)); END;
  FOR C:="I" TO "N" DO Default[C]:=CHAR(ORD(Int));   END;
END InitObjects;

PROCEDURE pack4(I:Info):WORD;
VAR w: WORD;
    a: POINTER TO ARRAY [0..3] OF CHAR;
BEGIN
   a:=ADR(w);
   a^[0]:=CHAR(I.darea);
   a^[1]:=CHAR(I.lenel);
   a^[2]:=CHAR(I.cl);
   a^[3]:=CHAR(I.bits);
--PACK(w,0,I.darea); PACK(w,1,I.lenel);
--PACK(w,2,I.cl); PACK(w,3,I.bits);
   RETURN w;
END pack4;

PROCEDURE pack2(i,j:WORD):WORD;
VAR w: WORD;
BEGIN
   w:=INTEGER(i << 16) + INTEGER(j);
   RETURN w;
END pack2;

PROCEDURE unpack4(w:WORD; VAR I:Info);
  VAR a: POINTER TO ARRAY [0..3] OF CHAR;
BEGIN
   a:=ADR(w);
   I.darea:=WORD(a^[0]);
   I.lenel:=WORD(a^[1]);
   I.cl:=WORD(a^[2]);
   I.bits:=WORD(a^[3]);
--I.darea:= UNPACK(w,0); I.lenel:=UNPACK(w,1);
--I.cl:= UNPACK(w,2); I.bits:=UNPACK(w,3);
END unpack4;

PROCEDURE unpack2(w:WORD; VAR i,j:WORD);
BEGIN
 --i:=SHR(w , 16) ;
   i:=BITSET(w>>16)*{0..15};
   j:=BITSET(w)*{0..15}
END unpack2;

PROCEDURE GenObj(I:Info);
  VAR p,p1:ADDRESS; w:WORD;
BEGIN
  CASE GetMode(I.name) OF
    Proc:      Give(p,3); p^:=pack4(I); p1:=p+1;
               p1^:=I.offset; p1:=p1+1; p1^:=I.commP;
   |xVar,LocVar: Give(p,3); p^:=pack4(I); p1:=p+1;
               p1^:=I.offset; p1:=p1+1; p1^:=0;
   |Var      : Give(p,3); p^:=pack4(I);
               p1:=p+1; p1^:=I.offset;
               p1:=p1+1; p1^:=pack2(I.commP,I.equiP);
   |Const    : Give(p,3); p^:=pack4(I);
               p1:=p+1; p1^:=I.offset;
               p1:=p1+1; p1^:=WORD(I.commP);
   |Array    : Give(p,5); p^:=pack4(I);
               p1:=p+1; p1^:=I.offset;
               p1:=p1+1; p1^:=pack2(I.commP,I.equiP);
               p1:=p1+1; p1^:=I.desc;
               p1:=p1+1; w:=I.dim;
               IF I.farray THEN w:=BITSET(w)+{31} END;
               p1^:=w;
   |Empty    : Fault(14);
  END;
  Object[I.name]:=p;
END GenObj;

PROCEDURE GenLocal(VAR I:Info);
VAR tp:Types;
BEGIN
  GetDefType(Ident[0],tp,I.lenel);  SetType(I.name,tp);
  I.darea:=0; I.cl:=Local; I.bits:={}; I.offset:=0;
  I.commP:=0; I.equiP:=0;
  SetMode(I.name,xVar);
END GenLocal;

PROCEDURE Unpack(VAR I:Info);
  VAR p:ADDRESS; w:WORD; d:INTEGER;
BEGIN
  d:=I.name; p:=Object[d];
  IF p=NIL THEN  Fault(15) END;
  w:=p^; unpack4(w,I);
  p:=p+1; I.offset:=INTEGER(p^); p:=p+1;
  CASE GetMode(d) OF
    Proc     : I.commP:=INTEGER(p^); I.equiP:=0;
   |xVar,LocVar: I.commP:=0; I.equiP:=0;
   |Var      : w:=p^; unpack2(w,I.commP,I.equiP);
   |Const    : I.commP:=INTEGER(p^);
   |Array    : w:=p^; unpack2(w,I.commP,I.equiP);
               p:=p+1; I.desc:=ADDRESS(p^); p:=p+1; w:=p^;
               I.farray:=31 IN BITSET(w);
               I.dim:=INTEGER(BITSET(w)*{0..30});
  END;
END Unpack;

PROCEDURE Pack(I:Info);
  VAR p,p1:ADDRESS; w:WORD;
BEGIN
  p:=Object[I.name];
  CASE GetMode(I.name) OF
    Proc     : p^:=pack4(I); p1:=p+1;
               p1^:=I.offset; p1:=p1+1; p1^:=I.commP;
   |xVar,LocVar: p^:=pack4(I); p1:=p+1;
               p1^:=I.offset; p1:=p1+1; p1^:=0;
   |Var      : p^:=pack4(I);
               p1:=p+1; p1^:=I.offset;
               p1:=p1+1; p1^:=pack2(I.commP,I.equiP);
   |Const    : p^:=pack4(I);
               p1:=p+1; p1^:=I.offset;
               p1:=p1+1; p1^:=WORD(I.commP);
   |Array    : p^:=pack4(I);
               p1:=p+1; p1^:=I.offset;
               p1:=p1+1; p1^:=pack2(I.commP,I.equiP);
               p1:=p1+1; p1^:=I.desc;
               p1:=p1+1; w:=I.dim;
               IF I.farray THEN w:=BITSET(w)+{31} END;
               p1^:=w;
   |Empty    : Fault(14);
  END;
END Pack;

PROCEDURE DelObj(d:INTEGER);
  VAR m:Mode;
BEGIN
  IF Object[d]=NIL THEN Fault(16); END;
  m:=Mode(ORD(Imode[d])); SetMode(d,Empty);
  IF m=Array THEN Free(Object[d],5);
  ELSE Free(Object[d],3);
  END
END DelObj;

PROCEDURE RestoreO;
  VAR p,p1:P; idn:INTEGER;
BEGIN
  p:=SO; SO:=NIL;
  WHILE p#NIL DO
    idn:=p^.idn;
    DelObj(idn);
    SetType(idn,p^.tp);
    SetMode(idn,p^.md);
    Object[idn]:=p^.pnt;
    p1:=p; p:=p^.next;
    Free(p1,SIZE(so));
  END;
END RestoreO;

PROCEDURE SaveO(i:INTEGER);
  VAR p:P;
BEGIN
  Give(p,SIZE(so));
  p^.idn :=i;
  p^.tp  :=GetType(i);
  p^.md  :=GetMode(i);
  p^.pnt :=Object[i];
  p^.next:=SO; SO:=p;
END SaveO;

END fcObj.
