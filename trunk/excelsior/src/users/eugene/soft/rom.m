MODULE rom; (* Sem 13-Jan-88. (c) KRONOS *)

FROM Qbus       IMPORT  QIN, QOUT;
FROM KRONOS     IMPORT  PACK, UNPACK, DROP;
FROM StdIO      IMPORT  Stream, Open, Close, Create, sRead, sWrite, Why?,
                        BusyRead, Write, WriteString, Show, WriteLn,
                        Read, print, Seek;
FROM Args       IMPORT  ArgC, ScanFlags, TakeWord, Flag?;
FROM Edit       IMPORT  ReadString;
FROM SYSTEM     IMPORT  ADR;
FROM Misc       IMPORT  Capital;

VAR pos    : ARRAY [0..7] OF INTEGER;
    Rom    : ARRAY [0..1023] OF ARRAY [0..7] OF CHAR;
    Name   : ARRAY [0..80] OF CHAR;
    inp,out: Stream;

PROCEDURE AskInt(s: ARRAY OF CHAR; VAR i: INTEGER): BOOLEAN;
  VAR ln: ARRAY [0..79] OF CHAR; n: INTEGER;
BEGIN
  ReadString(s,ln); WriteLn;
  IF ln[0]=0c THEN RETURN TRUE END;
  i:=0; n:=0;
  WHILE (ln[n]>='0')&(ln[n]<='9') DO
    i:=i*10+ORD(ln[n])-ORD('0'); INC(n);
  END;
  RETURN ln[n]#0c;
END AskInt;

PROCEDURE Check(i: INTEGER): INTEGER;
  VAR s: ARRAY [0..80] OF CHAR;
BEGIN
  IF i>=0 THEN RETURN i END;
  Why?(i,s); print('rom: io error "%s", file name "%s".\n',s,Name); HALT;
END Check;

PROCEDURE Bc(VAR s:ARRAY OF CHAR);
  VAR i: CARDINAL;
BEGIN
  FOR i:=0 TO HIGH(s) DO s[i]:=Capital(s[i]) END;
END Bc;

PROCEDURE wrRom(a,n: INTEGER; v: CHAR);
  VAR i: INTEGER; s: BITSET;
BEGIN
  IF n=6 THEN
    s:={};
    IF 0 IN BITSET(v) THEN INCL(s,0) END;
    IF 1 IN BITSET(v) THEN INCL(s,1) END;
    IF 2 IN BITSET(v) THEN INCL(s,2) END;
    IF 3 IN BITSET(v) THEN INCL(s,3) END;
    IF 4 IN BITSET(v) THEN INCL(s,7) END;
    IF 5 IN BITSET(v) THEN INCL(s,6) END;
    IF 6 IN BITSET(v) THEN INCL(s,5) END;
    IF 7 IN BITSET(v) THEN INCL(s,4) END;
    v:=CHAR(s);
  END;
  n:=pos[n];
  a:=INTEGER(BITSET(a)/{0..9});
  QOUT(167730b DIV 2,a);
  i:=QIN(167720b DIV 2 + n DIV 2);
  PACK(i,n MOD 2,BITSET(v)/{0..7});
  QOUT(167720b DIV 2 + n DIV 2,i);
END wrRom;

PROCEDURE rdRom(a,n: INTEGER): CHAR;
  VAR i,n1: INTEGER; s: BITSET; v: CHAR;
BEGIN
  n1:=n;
  n:=pos[n];
  a:=INTEGER(BITSET(a)/{0..9});
  QOUT(167730b DIV 2,a);
  i:=QIN(167720b DIV 2 + n DIV 2);
  v:=CHAR(BITSET(UNPACK(i,n MOD 2))/{0..7});
  IF n1=6 THEN
    s:={};
    IF 0 IN BITSET(v) THEN INCL(s,0) END;
    IF 1 IN BITSET(v) THEN INCL(s,1) END;
    IF 2 IN BITSET(v) THEN INCL(s,2) END;
    IF 3 IN BITSET(v) THEN INCL(s,3) END;
    IF 4 IN BITSET(v) THEN INCL(s,7) END;
    IF 5 IN BITSET(v) THEN INCL(s,6) END;
    IF 6 IN BITSET(v) THEN INCL(s,5) END;
    IF 7 IN BITSET(v) THEN INCL(s,4) END;
    v:=CHAR(s);
  END;
  RETURN v;
END rdRom;

VAR str: ARRAY [0..80] OF CHAR;
    j,i,k,cnt: CARDINAL;
    ch: CHAR;

BEGIN
  ScanFlags;
  IF Flag?('0') THEN
    QOUT(167732b DIV 2,{2});
    FOR i:=0 TO 1000 DO END;
    QOUT(167732b DIV 2,{});
    HALT;
  END;
  IF ArgC()>0 THEN
    TakeWord(Name);
    inp:=Check(Open(Name));
    FOR i:=0 TO HIGH(Rom) DO
      FOR j:=0 TO 7 DO Rom[i][j]:=377c END;
    END;
    i:=Check(Seek(inp,0,2));
    IF i>SIZE(Rom)*4 THEN i:=SIZE(Rom)*4 END;
    DROP(Check(Seek(inp,0,0)));
    DROP(Check(sRead(inp,ADR(Rom),i)));
    DROP(Check(Close(inp)));
    FOR i:=0 TO HIGH(Rom) DO
      cnt:=0;
      FOR j:=0 TO 7 DO
        FOR k:=0 TO 7 DO
          IF k IN BITSET(Rom[i][j]) THEN INC(cnt) END;
        END;
      END;
      IF (cnt MOD 2)#1 THEN
        IF 7 IN BITSET(Rom[i][7]) THEN
          Rom[i][7]:=CHAR(CARDINAL(Rom[i][7])-80h);
        ELSE
          Rom[i][7]:=CHAR(CARDINAL(Rom[i][7])+80h);
        END;
      END;
    END;
    print('File %s loaded.\n',Name);
  END;
  pos[0]:=6; pos[1]:=4; pos[2]:=2; pos[3]:=0;
  pos[4]:=7; pos[5]:=5; pos[6]:=3; pos[7]:=1;
  QOUT(167732b DIV 2,{0,5,2});
  IF Flag?('f') THEN
    FOR i:=0 TO 63 DO
      FOR j:=0 TO 7 DO wrRom(0,j,0c) END;
      wrRom(0,i DIV 8,CHAR({i MOD 8}));
      print('%2d, continue?',i);
      REPEAT UNTIL Read()='y';
      print('y\n');
    END;
  END;
  IF NOT Flag?('c') THEN
    FOR j:=0 TO 7 DO
      FOR i:=0 TO HIGH(Rom) DO wrRom(i,j,Rom[i][j]) END;
    END;
    print('Memory writed.\n');
  END;
  FOR j:=0 TO 7 DO
    FOR i:=0 TO HIGH(Rom) DO
      ch:=rdRom(i,j);
      IF ch#Rom[i][j] THEN
        print('Error in memory: %4d %2d: %$2h#%$2h - %$2h\n',i,j,ch,Rom[i][j],
                 BITSET(ch)/BITSET(Rom[i][j]));
      END;
    END;
  END;
  QOUT(167732b DIV 2,{5,2});
  QOUT(167732b DIV 2,{2});
  FOR i:=0 TO 1000 DO END;
  QOUT(167732b DIV 2,{});
END rom.
