DEFINITION MODULE fcGen;

FROM SYSTEM    IMPORT WORD;
FROM fcDefs    IMPORT CodeSize,StpSize;

VAR cp   :  INTEGER;   (* указатель в COD *)
    COD  :  ARRAY[0..CodeSize] OF CHAR;
    MinPS:  INTEGER;   (* размер мультизначений *)
    STP  :  ARRAY [0.. StpSize] OF CHAR;
    stpPos :INTEGER; (* указатель в STP *)
    ModName:ARRAY [0..15] OF CHAR; (* имя текущего модуля *)
    ProcNo: INTEGER;
    procno: INTEGER;
    global: INTEGER;
    proTab: ARRAY [0..255] OF INTEGER;

PROCEDURE MarkC; PROCEDURE MarkC1;
PROCEDURE BackC; PROCEDURE BackC1;
PROCEDURE epush;
PROCEDURE epop;
PROCEDURE b (byte: INTEGER);
PROCEDURE b2(offs: INTEGER);
PROCEDURE c (Command: INTEGER);
PROCEDURE c1(Command: INTEGER; byte: INTEGER);
PROCEDURE c2(Command: INTEGER; byte1,byte2: INTEGER);
PROCEDURE cJump(label: INTEGER; cond,fixed:BOOLEAN);
PROCEDURE Getdepth():INTEGER;
PROCEDURE Setdepth(d:INTEGER);
PROCEDURE Store(VAR ds,mxs:INTEGER);
PROCEDURE Lodfv(    ds,mxs,n:INTEGER);
PROCEDURE InPoolStr(VAR s:ARRAY OF CHAR): INTEGER;
PROCEDURE InPoolConst(wd:WORD): INTEGER;
PROCEDURE InitPool(VAR fnm:ARRAY OF CHAR);
PROCEDURE pSpool;
PROCEDURE StartProc(parno,procno,maplen,Locals,templen:INTEGER);
PROCEDURE FinishProc;
PROCEDURE genCbase(procno,import,comno,moffs:INTEGER);
PROCEDURE genAbase(procno,bmoffs,moffs,offset:INTEGER);
PROCEDURE InitGen(VAR name:ARRAY OF CHAR);
PROCEDURE FinishGen;
PROCEDURE BpCode;
PROCEDURE ProcCode;
PROCEDURE Alloc(sz:INTEGER);
PROCEDURE sgw(gw:INTEGER);
PROCEDURE lgw(gw:INTEGER);
PROCEDURE slw(lw:INTEGER);
PROCEDURE llw(lw:INTEGER);
PROCEDURE lla(lw:INTEGER);
PROCEDURE ssw(lw:INTEGER);
PROCEDURE lsw(lw:INTEGER);
PROCEDURE lsa(lw:INTEGER);
PROCEDURE lew(mod,wd:INTEGER);
PROCEDURE sew(mod,wd:INTEGER);
PROCEDURE li(val:INTEGER);
PROCEDURE CallExt(mod,proc:INTEGER);
PROCEDURE cCall(VAR name:ARRAY OF CHAR; nopar,noaltr:INTEGER);
PROCEDURE moveCode;
PROCEDURE DataCode;
PROCEDURE InsertMode(cp0:INTEGER);
PROCEDURE InsertEnd;
PROCEDURE cOrJump;
PROCEDURE cAndJump;
PROCEDURE cEndLab;
PROCEDURE cLabel(label:INTEGER);
PROCEDURE genSjump(code:INTEGER);
PROCEDURE genSJlab;
PROCEDURE LdFormat(soffset:INTEGER);
PROCEDURE lsta(offs:INTEGER);
PROCEDURE cLdFormat(label:INTEGER);
PROCEDURE genLPC(VAR name:ARRAY OF CHAR);


END fcGen.
