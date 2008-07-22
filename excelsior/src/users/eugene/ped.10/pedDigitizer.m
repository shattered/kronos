IMPLEMENTATION MODULE pedDigitizer; (* Sem 12-May-87. (c) KRONOS *)

FROM SYSTEM    IMPORT   WORD, ADR, PROCESS, ADDRESS;
FROM KRONOS    IMPORT   SETM, GETM;

CONST
  BufSize=16;

TYPE BufRec=RECORD bx,by,bz: CARDINAL END;

VAR
  State         : CARDINAL;
  X,Y,Z         : CARDINAL;
  Xneg,Yneg     : BOOLEAN;
  Buf           : ARRAY [0..BufSize-1] OF BufRec;
  iBegin, iEnd  : CARDINAL;
  Scale,Scale2,Delta   : CARDINAL;
  Zsv,Ysv,Xsv   : CARDINAL;
  Saved         : BOOLEAN;
  Xshift,Yshift : CARDINAL;

PROCEDURE Finish;
  VAR NewEnd,OldEnd: CARDINAL;
BEGIN
  NewEnd:=(iEnd+1) MOD BufSize;
  IF NewEnd=iBegin THEN RETURN END;
  IF Saved&(Z=Zsv) THEN
    IF (ABS(X-Xsv)<Delta)&(ABS(Y-Ysv)<Delta) THEN RETURN END;
    IF iBegin#iEnd THEN
      IF iEnd=0 THEN OldEnd:=BufSize-1 ELSE OldEnd:=iEnd-1 END;
      WITH Buf[OldEnd] DO
        bx:=(X+Scale2) DIV Scale;
        by:=(Y+Scale2) DIV Scale;
        Xsv:=bx*Scale;
        Ysv:=by*Scale;
      END;
      RETURN;
    END;
  END;
  WITH Buf[iEnd] DO
    IF Z#4 THEN
      bx:=(X+Scale2) DIV Scale;
      by:=(Y+Scale2) DIV Scale;
    ELSE
      bx:=Xsv DIV Scale;
      by:=Ysv DIV Scale;
    END;
    bz:=Z;
    Zsv:=Z;
    Xsv:=bx*Scale;
    Ysv:=by*Scale;
  END;
  iEnd:=NewEnd;
  Saved:=TRUE;
END Finish;

PROCEDURE Get;
  VAR inp,out,ptr,end: CARDINAL;
      Ch: CHAR; i: CARDINAL;
BEGIN
(*
  WHILE inp#out DO
    Ch:=CHAR(BITSET(Byte(out+s0F))*{0..6});
    IF (Ch<' ') THEN
      IF State=13 THEN Finish END;
      State:=0;
    ELSE
      CASE State OF
        0:i:=ORD(Ch)-ORD('0');
          IF i IN {0..4} THEN Z:=i; INC(State); END;
       |1:IF Ch='+' THEN Xneg:=FALSE; X:=0; INC(State) END;
          IF Ch='-' THEN Xneg:=TRUE ; X:=0; INC(State) END;
       |2..6:
          i:=ORD(Ch)-ORD('0');
          IF i IN {0..9} THEN
            IF Xneg THEN X:=X*10-i ELSE X:=X*10+i END;
            INC(State);
          END;
       |7:IF Ch='+' THEN Yneg:=FALSE; Y:=0; INC(State) END;
          IF Ch='-' THEN Yneg:=TRUE ; Y:=0; INC(State) END;
       |8..12:
          i:=ORD(Ch)-ORD('0');
          IF i IN {0..9} THEN
            IF Yneg THEN Y:=Y*10-i ELSE Y:=Y*10+i END;
            INC(State);
          END;
      ELSE
      END;
    END;
    IF out=end THEN out:=ptr ELSE INC(out) END;
    SetWord16(BCB+4,out);
  END;
  SetByte(BCB,1c);
*)
END Get;

PROCEDURE BusyRead(VAR x,y,z: CARDINAL): BOOLEAN;
BEGIN
  RETURN FALSE;
  Get;
  IF iBegin=iEnd THEN RETURN FALSE END;
  WITH Buf[iBegin] DO
    x:=bx*48; y:=by*48; z:=bz;
  END;
  iBegin:=(iBegin+1) MOD BufSize;
  INC(x,Xshift); INC(y,Yshift);
  RETURN TRUE;
END BusyRead;

PROCEDURE SetWindow(Size,X,Y: CARDINAL);
  VAR n: CARDINAL;
BEGIN
  n:=11000 DIV Size;
  IF n=0 THEN n:=1 END;
  Scale:=50*n; Delta:=40*n; Scale2:=25*n;
  Xshift:=X-5500 DIV n;
  DEC(Xshift,Xshift MOD 48);
  Yshift:=Y-5500 DIV n;
  DEC(Yshift,Yshift MOD 48);
END SetWindow;

PROCEDURE SetBoard(Xsize,Ysize: CARDINAL);
BEGIN
END SetBoard;

BEGIN
  State:=0;
  iBegin:=0; iEnd:=0;
  Scale:=50; Delta:=40; Scale2:=25;
  Saved:=FALSE;
END pedDigitizer.
