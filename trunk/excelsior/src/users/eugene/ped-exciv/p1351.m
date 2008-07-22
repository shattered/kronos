IMPLEMENTATION MODULE Toshiba; (* 15-Nov-87. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR;
FROM cdsHeap    IMPORT  Allocate, Deallocate;
FROM FsPublic   IMPORT  FileName, File;
FROM BIO        IMPORT  bRead, OpenOnDir, checkHALT, CD, Close;
FROM Media      IMPORT  DevDrv, WriteDisk;
FROM Image      IMPORT  image0;
FROM mCodeMnem  IMPORT  bit;
FROM Terminal   IMPORT  print, SetCrs, Write;
IMPORT s: SYSTEM;
IMPORT T: Terminal;
IMPORT mcd: mCodeMnem;

CONST LineSize=2400;

VAR oCSR, oDTR: INTEGER;

VAR  Window     : POINTER TO ARRAY [0..0] OF BITSET;
     Wsz        : INTEGER;
     WindowW    : INTEGER;
     WindowE    : INTEGER;
     WindowS    : INTEGER;
     WindowN    : INTEGER;
     WindowX    : INTEGER;
     WindowY    : INTEGER;
     lp         : INTEGER;
     Buf        : ARRAY [0..4095] OF CHAR;
     BufCnt     : INTEGER;
     pass       : INTEGER;

PROCEDURE inp(a: s.ADDRESS): s.WORD;    CODE mcd.inp END inp;
PROCEDURE out(a: s.ADDRESS; w: s.WORD); CODE mcd.out END out;

PROCEDURE PutCh(ch: s.WORD);
BEGIN
  REPEAT UNTIL 7 IN BITSET(inp(oCSR)) ;
  out(oDTR,ch);
END PutCh;

PROCEDURE WrBuf;
  VAR i: INTEGER;
BEGIN
  IF BufCnt>0 THEN
--  IF WriteDisk(lp,0,ADR(Buf),BufCnt) THEN print('Error...\n') END;
    FOR i:=0 TO BufCnt-1 DO PutCh(Buf[i]) END;
    BufCnt:=0;
  END;
END WrBuf;

PROCEDURE PutChar(Ch: s.WORD);
BEGIN
  IF BufCnt>=4096 THEN WrBuf END;
  Buf[BufCnt]:=Ch; INC(BufCnt);
END PutChar;

  VAR fName: FileName; fl: File;
PROCEDURE ReadWindow(Name: ARRAY OF CHAR);
  VAR  wnd: RECORD sz,w,e,s,n: INTEGER END;
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
  WindowX:=WindowE-WindowW+1;
  WindowY:=WindowN-WindowS+1;
  checkHALT(Close(fl),fName);
  IF WindowY>LineSize THEN print('Too long Y size...\n'); HALT END;
END ReadWindow;

PROCEDURE bbb;
  VAR bas: POINTER TO ARRAY [0..359] OF ARRAY [0..15] OF s.WORD;
  VAR i,j: INTEGER;

  PROCEDURE move(to,from: s.ADDRESS; sz: INTEGER);
  CODE mcd.move END move;

BEGIN
  ReadWindow('tt');
  bas:=s.ADDRESS(1F8000h);
  bas^[0][0]:=0; move(s.ADDRESS(INTEGER(bas)+1),bas,512*16*4-1);

  T.print('WindowY %d\n',WindowY);
  T.print('WindowX %d\n',WindowX);
  T.print('Wsz     %d\n',Wsz);
  T.print('Wsz  s  %d\n',(WindowX+31) DIV 32 * WindowY);

  FOR j:=0 TO WindowY-1 DO
    FOR i:=0 TO (WindowX+31) DIV 32 - 1 DO
      (*$T-*)
      IF ((WindowX+31) DIV 32 * j + i)<Wsz THEN
        bas^[j][i]:=Window^[(WindowX+31) DIV 32 * j + i];
      END;
      (*$T+*)
    END;
  END;
HALT;
END bbb;

PROCEDURE BIT(n: INTEGER): INTEGER;
CODE bit END BIT;

PROCEDURE PrintWindow(nm: ARRAY OF CHAR);
  VAR p: ARRAY [0..4] OF ARRAY [0..LineSize-1] OF CHAR;
      x,y,k,n,n1,cnt: INTEGER;
      power,ye: INTEGER;
      s: ARRAY [0..79] OF CHAR;
BEGIN
  ReadWindow(nm);
  (*$T-*)
  BufCnt:=0;
  x:=0;
  image0(s,'%$4d',WindowY);
  WHILE x<WindowX DO
    FOR y:=0 TO WindowY-1 DO
      p[0][y]:=0c; p[1][y]:=0c; p[2][y]:=0c; p[3][y]:=0c
    END;
    FOR y:=0 TO WindowY-1 DO
      cnt:=0;
      FOR k:=x TO x+23 DO
        IF k<=WindowX THEN
          n:=k DIV 32 + (WindowX+31) DIV 32 * y;
          IF (k MOD 32) IN (Window^[n]) THEN
            n:=k-x; n1:=5-(n MOD 6); n:=n DIV 6;
            p[n][y]:=CHAR(BITSET(p[n][y])+{n1});
            INC(cnt);
          END;
        END;
      END;
      p[4][y]:=CHAR(cnt);
    END;
    REPEAT
      power:=200; cnt:=0;
      PutChar(33c); PutChar(';');
      PutChar(s[0]); PutChar(s[1]); PutChar(s[2]); PutChar(s[3]);
      FOR y:=0 TO WindowY-1 DO
        IF power>0 THEN
          FOR k:=0 TO 3 DO PutChar(p[k][y]) END;
          DEC(power,ORD(p[4][y])-10);
          FOR k:=0 TO 4 DO p[k][y]:=0c END;
        ELSE
          FOR k:=0 TO 3 DO PutChar(0c) END; INC(power,10);
        END;
        INC(cnt,ORD(p[4][y]));
        IF power>200 THEN power:=200 END;
      END;
      PutChar(15c);
    UNTIL cnt=0;
    PutChar(33c);
    PutChar('L'); PutChar('0'); PutChar('7'); PutChar(12b);
    INC(x,24);
  END;
  (*T+*)
  WrBuf;
  Deallocate(Window,Wsz);
END PrintWindow;

BEGIN
  oCSR:=177514b DIV 2;
  oDTR:=oCSR+1;
END Toshiba.
