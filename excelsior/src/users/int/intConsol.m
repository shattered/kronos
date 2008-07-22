IMPLEMENTATION MODULE intConsol; (* 01-Jun-88. (c) KRONOS *)

IMPORT Nil, noIpt, trace, show, InitCash;
IMPORT PCore, Core, WE, RE, Wd, Bt, Max;
FROM Misc       IMPORT  Control?;
FROM Image      IMPORT  GetNum;
FROM Terminal   IMPORT  print, Read, Write, WriteLn,
                        Clear, ClearLine, Show;
FROM FsPublic   IMPORT  File;
FROM BIO        IMPORT  Open, Close, checkHALT, GetEof;
IMPORT BIO;
IMPORT ADR;

VAR ch: CHAR;
   Ins: ARRAY [0..79] OF CHAR;
   Len: INTEGER;
   Adr: INTEGER;

PROCEDURE WriteAdr();
VAR i: INTEGER;
BEGIN
  WriteLn;
  IF (Adr<=Max)&(Adr>=0) THEN
    Core(Adr,i,RE,Wd);
    print('%$5h=%$8h ',Adr,i)
  ELSE print('%$5h=%$8h ',Adr,0)
  END
END WriteAdr;

PROCEDURE BootDX();
  VAR   f: File;
BEGIN
  checkHALT(Open(f,'/ii/users/john/int/INT.DISK ',Show),'BootVD0: ');
  BIO.Read(f,0,PCore,4096);
  checkHALT(Close(f),'BootTST: ');
END BootDX;

PROCEDURE next;
BEGIN IF Adr<Nil THEN INC(Adr) ELSE Adr:=0 END; WriteAdr; Len:=0 END next;

PROCEDURE Pred;
BEGIN IF Adr>0 THEN DEC(Adr) ELSE Adr:=Nil END; WriteAdr; Len:=0 END Pred;

PROCEDURE NI();
BEGIN Write('?'); WriteAdr; Len:=0 END NI;

PROCEDURE App();
BEGIN IF Len<79 THEN Ins[Len]:=ch; INC(Len) ELSE NI END END App;

PROCEDURE Caps(c: CHAR): CHAR;
BEGIN
  IF  (ORD(c)>140b) & (ORD(c)<200b) OR (ORD(c)>340b) & (ORD(c)<=377b)
  THEN RETURN CHAR(ORD(c)-40b) ELSE RETURN c END
END  Caps;

PROCEDURE GetValue(): INTEGER;
VAR i: INTEGER;
    s: ARRAY [0..10] OF CHAR;
BEGIN i:=0;
  WHILE (i<8)&(Len-i>0) DO s[7-i]:=Ins[Len-i-1]; INC(i) END;
  WHILE i<8             DO s[7-i]:=60c;          INC(i) END;
  s[8]:='h';            s[9]:=0c;                Len:=0;
  ASSERT(GetNum(s,i)>=0); RETURN i
END GetValue;

PROCEDURE SetAdr();
VAR i: INTEGER;
BEGIN
  IF Len=0 THEN
    IF Adr<=Max THEN
      Core(Adr,i,RE,Wd);
      Adr:=i MOD Nil
    ELSE Adr:=0
    END; RETURN;
  END;
  Adr:=(GetValue() MOD Nil);
END SetAdr;

PROCEDURE SetValue();
VAR i: INTEGER;
BEGIN  IF Len=0 THEN  RETURN END;
  IF Adr<=Max THEN i:=GetValue(); Core(Adr,i,WE,Wd)  END
END SetValue;

PROCEDURE Header();
BEGIN
  print('\n\n\n\n\n\n');
  print('                          (c) Kronos  Group 1987\n\n');
  print('                     Kronos 32-bit family member (P4.1)\n\n');
  print('                               Novosibirsk\n\n');
  print('                              Made in  USSR');
  print('\n\n\n\n\n\n');
END Header;

PROCEDURE Consol(noipt: INTEGER);
  VAR offon: ARRAY BOOLEAN OF ARRAY [0..3] OF CHAR;
        j,i: INTEGER;
BEGIN
  IF noipt=8 THEN Header END;
  offon[FALSE]:="OFF";
  offon[TRUE ]:="ON";
  print('\nHalt %$2h\n',noipt);
  Adr:=1; WriteAdr;
  LOOP
    ch:=Read();
    CASE ch OF
      '0'..'9', 'A'..'F', 'a'..'f': App(); Write(Caps(ch));
     |'/'    :  Write('/'); SetAdr(); WriteAdr;
     |'T','t':  trace:=NOT trace;
                print("trace %s",offon[trace]); WriteAdr;
     |'S','s':  show:=NOT show;
                print("show %s",offon[show]); WriteAdr;
     |'L','l':  Write('L'); BootDX; InitCash;  Adr:=1;  WriteAdr();
     |15c,'m':  SetValue; Pred;
     |12c,'n':  SetValue; next;
     |33c    :
     |'G','g':  Show('G'); EXIT
    ELSE        Write(' '); NI;
    END
  END
END Consol;

END intConsol.
