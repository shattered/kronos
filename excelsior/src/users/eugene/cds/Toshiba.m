IMPLEMENTATION MODULE Toshiba; (* 15-Nov-87. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR;
FROM cdsHeap    IMPORT  Allocate, Deallocate;
FROM FsPublic   IMPORT  FileName, File;
FROM BIO        IMPORT  bRead, OpenOnDir, checkHALT, CD, Close;
FROM Media      IMPORT  DevDrv, WriteDisk;
FROM Image      IMPORT  image0;
FROM mCodeMnem  IMPORT  bit;
FROM Terminal   IMPORT  print, SetCrs, Write;

CONST LineSize=2400;

VAR  Window     : POINTER TO ARRAY [0..0] OF BITSET;
     Wsz        : INTEGER;
     WindowW    : CARDINAL;
     WindowE    : CARDINAL;
     WindowS    : CARDINAL;
     WindowN    : CARDINAL;
     WindowX    : CARDINAL;
     WindowY    : CARDINAL;
     lp         : INTEGER;
     Buf        : ARRAY [0..4095] OF CHAR;
     BufCnt     : INTEGER;

PROCEDURE WrBuf;
BEGIN
  IF BufCnt>0 THEN
    IF WriteDisk(lp,0,ADR(Buf),BufCnt) THEN print('Error...\n') END;
    BufCnt:=0;
  END;
END WrBuf;

PROCEDURE PutChar(Ch: CHAR);
BEGIN
  IF BufCnt>=4096 THEN WrBuf END;
  Buf[BufCnt]:=Ch; INC(BufCnt);
END PutChar;

PROCEDURE ReadWindow(Name: ARRAY OF CHAR);
  VAR fName: FileName; fl: File;
      wnd: RECORD sz,w,e,s,n: INTEGER END;
BEGIN
  image0(fName,'%s.bmp',Name);
  checkHALT(OpenOnDir(CD(),fl,fName),fName);
  checkHALT(bRead(fl,0,ADR(wnd),SIZE(wnd)*4),fName);
  WITH wnd DO
    Wsz:=sz;
    WindowE:=e; WindowW:=w;
    WindowS:=s; WindowN:=n;
  END;
  Allocate(Window,Wsz);
  checkHALT(bRead(fl,1,Window,Wsz*4),fName);
  checkHALT(Close(fl),fName);
  WindowX:=WindowE-WindowW+1;
  WindowY:=WindowN-WindowS+1;
  IF WindowY>LineSize THEN print('Too long Y size...\n'); HALT END;
END ReadWindow;

PROCEDURE BIT(n: CARDINAL): CARDINAL;
CODE bit END BIT;

PROCEDURE PrintWindow(nm: ARRAY OF CHAR);
  VAR p: ARRAY [0..3] OF ARRAY [0..LineSize-1] OF CHAR;
      x,y,k,n,n1,cnt: CARDINAL;
      power,ye: INTEGER;
BEGIN
  checkHALT(DevDrv('lp0',lp),'lp0');
  ReadWindow(nm);
  (*$T-*)
  BufCnt:=0;
  x:=WindowW;
  WHILE x<=WindowE DO
    FOR y:=0 TO WindowY-1 DO p[0][y]:=0c; p[1][y]:=0c; p[2][y]:=0c END;
    FOR y:=0 TO WindowY-1 DO
      cnt:=0;
      FOR k:=x TO x+23 DO
        IF k<=WindowE THEN
          n:=k-WindowW+WindowX*y;
          IF (n MOD 32) IN (Window^[n DIV 32]) THEN
            n:=k-x; n1:=7-(n MOD 8); n:=n DIV 8;
            p[n][y]:=CHAR(BITSET(p[n][y])+{n1});
            INC(cnt);
          END;
        END;
      END;
      p[3][y]:=CHAR(cnt);
    END;
    REPEAT
      power:=200; cnt:=0;
      PutChar(33c); PutChar('I');
      PutChar(CHAR(WindowY DIV 256));
      PutChar(CHAR(WindowY MOD 256));
      FOR y:=0 TO WindowY-1 DO
        IF power>0 THEN
          FOR k:=0 TO 2 DO PutChar(p[k][y]) END;
          DEC(power,ORD(p[3][y])-10);
          FOR k:=0 TO 3 DO p[k][y]:=0c END;
        ELSE
          FOR k:=0 TO 2 DO PutChar(0c) END; INC(power,10);
        END;
        INC(cnt,ORD(p[3][y]));
        IF power>200 THEN power:=200 END;
      END;
      PutChar(15c);
    UNTIL cnt=0;
    PutChar(33c);
    PutChar('W'); PutChar('0'); PutChar('0'); PutChar('1'); PutChar('6');
    INC(x,24);
  END;
  WrBuf;
  (*T+*)
  Deallocate(Window,Wsz);
END PrintWindow;

END Toshiba.
