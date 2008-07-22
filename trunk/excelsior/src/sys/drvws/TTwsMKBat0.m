MODULE TTwsMKBat0; (* 10-Mar-90. (c) KRONOS *)

IMPORT  key: Keyboard;
IMPORT  std: StdIO;
IMPORT  tty: Terminal;

TYPE
  ITC = ARRAY [0..255] OF CHAR;

VAR
  t0: ITC;  (* normal  state *)
  t1: ITC;  (* shift   state *)
  t2: ITC;  (* alt     state *)
  t3: ITC;  (* alt+shf state *)
  nm: ITC;  (* numlock state *)
  n0: ITC;  (* normal  state  national *)
  n1: ITC;  (* shift   state  national *)
  n2: ITC;  (* alt     state  national *)
  n3: ITC;  (* alt+shf state  national *)
  cs: ITC;  (* change  state  codes    *)

PROCEDURE csi(VAL name: ARRAY OF CHAR; key,cod: INTEGER);
  VAR i: INTEGER;
  lname: ARRAY [0..15] OF CHAR;
BEGIN
  IF cod>0 THEN
    i:=0;
    REPEAT
      IF ("A"<=name[i]) & (name[i]<="Z") THEN
        lname[i]:=CHAR(ORD(name[i])+40b)
      ELSE
        lname[i]:=name[i]
      END;
      INC(i)
    UNTIL name[i-1]=0c;
    std.print("  %-8s = %03bc;   _%-8s = %d;\n",name,key,lname,cod);
  ELSE
    std.print("  %-8s = %03bc;\n",name,key)
  END;
  cs[key]:=CHAR(cod)
END csi;

PROCEDURE make;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO 255 DO cs[i]:=0c END;  t0:=cs; nm:=cs;
  csi("RELEASE",0F0h,-1);
  csi("lSHIFT" ,012h,1);
  csi("rSHIFT" ,059h,2);
  csi("ALT"    ,011h,3);
  csi("CAPS"   ,058h,4);
  csi("NUMS"   ,077h,5);
  csi("SCROLL" ,07Eh,6);
  csi("CNTRL"  ,014h,7);

  t0[005h]:=key.f1;     t0[006h]:=key.f2;
  t0[004h]:=key.f3;     t0[00Ch]:=key.f4;
  t0[003h]:=key.f5;     t0[00Bh]:=key.f6;
  t0[083h]:=key.f7;     t0[00Ah]:=key.f8;
  t0[001h]:=key.f9;     t0[009h]:=key.f10;

  t0[00Dh]:=key.tab;    t0[066h]:=key.back;
  t0[05Ah]:=15c;        t0[076h]:=33c;
  t0[084h]:=12c;        t0[07Ch]:='*';
  t0[07Bh]:='-';        t0[079h]:='+';

  t0[06Ch]:=key.home;   t0[075h]:=key.up;       t0[07Dh]:=key.pgup;
  t0[06Bh]:=key.left;   t0[073h]:=key.center;   t0[074h]:=key.right;
  t0[069h]:=key.end;    t0[072h]:=key.dw;       t0[07Ah]:=key.pgdw;
  t0[070h]:=key.ins;    t0[071h]:=key.del;

  nm[06Ch]:='7';        nm[075h]:='8';          nm[07Dh]:='9';
  nm[06Bh]:='4';        nm[073h]:='5';          nm[074h]:='6';
  nm[069h]:='1';        nm[072h]:='2';          nm[07Ah]:="3";
  nm[070h]:='0';        nm[071h]:='.';

  t0[00Eh]:='`';        t0[016h]:='1';
  t0[01Eh]:='2';        t0[026h]:='3';
  t0[025h]:='4';        t0[02Eh]:='5';
  t0[036h]:='6';        t0[03Dh]:='7';
  t0[03Eh]:='8';        t0[046h]:='9';
  t0[045h]:='0';        t0[04Eh]:='-';
  t0[055h]:='=';        t0[05Dh]:='\';

  t0[015h]:='q';        t0[01Dh]:='w';
  t0[024h]:='e';        t0[02Dh]:='r';
  t0[02Ch]:='t';        t0[035h]:='y';
  t0[03Ch]:='u';        t0[043h]:='i';
  t0[044h]:='o';        t0[04Dh]:='p';
  t0[054h]:='[';        t0[05Bh]:=']';

  t0[01Ch]:='a';        t0[01Bh]:='s';
  t0[023h]:='d';        t0[02Bh]:='f';
  t0[034h]:='g';        t0[033h]:='h';
  t0[03Bh]:='j';        t0[042h]:='k';
  t0[04Bh]:='l';        t0[04Ch]:=';';          t0[052h]:="'";

  t0[01Ah]:='z';        t0[022h]:='x';
  t0[021h]:='c';        t0[02Ah]:='v';
  t0[032h]:='b';        t0[031h]:='n';
  t0[03Ah]:='m';        t0[041h]:=',';
  t0[049h]:='.';        t0[04Ah]:='/';          t0[029h]:=" ";

  n0:=t0;

  n0[015h]:='й';        n0[01Dh]:='ц';
  n0[024h]:='у';        n0[02Dh]:='к';
  n0[02Ch]:='е';        n0[035h]:='н';
  n0[03Ch]:='г';        n0[043h]:='ш';
  n0[044h]:='щ';        n0[04Dh]:='з';
  n0[054h]:='х';

  n0[01Ch]:='ф';        n0[01Bh]:='ы';
  n0[023h]:='в';        n0[02Bh]:='а';
  n0[034h]:='п';        n0[033h]:='р';
  n0[03Bh]:='о';        n0[042h]:='л';
  n0[04Bh]:='д';        n0[04Ch]:='ж';          n0[052h]:='э';

  n0[01Ah]:='я';        n0[022h]:='ч';
  n0[021h]:='с';        n0[02Ah]:='м';
  n0[032h]:='и';        n0[031h]:='т';
  n0[03Ah]:='ь';        n0[041h]:='б';
  n0[049h]:='ю';        n0[05Dh]:='ъ';

  t1:=t0;

  t1[005h]:=key.f11;    t1[006h]:=key.f12;
  t1[004h]:=key.f13;    t1[00Ch]:=key.f14;
  t1[003h]:=key.f15;    t1[00Dh]:=key.bcktab;

  t1[06Ch]:='7';   t1[075h]:='8';   t1[07Dh]:='9';
  t1[06Bh]:='4';   t1[073h]:='5';   t1[074h]:='6';
  t1[069h]:='1';   t1[072h]:='2';   t1[07Ah]:='3';
  t1[070h]:='0';   t1[071h]:='.';

  t1[00Eh]:='~';        t1[016h]:='!';
  t1[01Eh]:='@';        t1[026h]:='#';
  t1[025h]:='$';        t1[02Eh]:='%';
  t1[036h]:='^';        t1[03Dh]:='&';
  t1[03Eh]:='*';        t1[046h]:='(';
  t1[045h]:=')';        t1[04Eh]:='_';
  t1[055h]:='+';        t1[05Dh]:='|';

  t1[054h]:='{';        t1[05Bh]:='}';

  t1[04Ch]:=':';        t1[052h]:='"';

  t1[041h]:='<';        t1[049h]:='>';
  t1[04Ah]:='?';

  FOR i:=0 TO 255 DO
    IF ("a"<=t0[i]) & (t0[i]<="z") THEN t1[i]:=CHAR(ORD(t0[i])-40b) END
  END;

  n1:=t1;
  FOR i:=0 TO 255 DO
    IF (300c<=n0[i]) & (n0[i]<=337c) THEN n1[i]:=CHAR(ORD(n0[i])+40b) END
  END;
END make;

VAR b,z: INTEGER;

PROCEDURE output(VAL name: ARRAY OF CHAR; t: ITC;
                 VAL cmnt: ARRAY OF CHAR);
  VAR i,h: INTEGER;
BEGIN
  std.print("\n  %s = ARRAY OF CHAR  (*  %s  *)\n  { ",name,cmnt);
  h:=HIGH(t);
  WHILE t[h]=0c DO DEC(h) END;
  FOR i:=0 TO h DO
    std.print("%03bc",t[i]);
    IF i=h THEN std.print("\n")
    ELSE
      std.print(",");
      IF i MOD 14 = 13 THEN std.print("\n    ") END;
    END;
    INC(z,ORD(t[i]=0c));
  END;
  INC(b,h);
  std.print("  };\n");
END output;

BEGIN
  std.print("CONST\n");
  make;
  b:=0;
  z:=0;
  output("CSI",cs,"CHANGE  STATE INTRODUCERS");
  output("NUM",nm,"NUMLOCK state");
  output("LT0",t0,"LATIN   state NORMAL");
  output("LT1",t1,"LATIN   state SHIFT");
  output("NT0",n0,"NATIO   state NORMAL");
  output("NT1",n1,"NATIO   state SHIFT");
  tty.print("total bytes: %d (%d zeros)\n",b,z);
END TTwsMKBat0.


---------       -----------------
|f1 | f2|       |ESC|NML|SCL|SYS|
| 5 | 6 |       |76 |77 |7E |84 |
|---|---|       |---|---|---|---|
|f3 | f4|       |Hom|Up |Pup|Prt|
| 4 | C |       |6C |75 |7D |7C |
|---|---|       |---|---|---|---|
|f5 | f6|       |Lft| C |Rgt| - |
| 3 | B |       |6B |73 |74 |7B |
|---|---|       |---|---|---|---|
|f7 | f8|       |End|Dw |Pdw|   |
|83 | A |       |69 |72 |7A |   |
|---|---|       |-------|---| - |
|f9 | f0|       |  Ins  |Del|   |
| 1 | 9 |       |70     |71 |79 |
---------       -----------------

-------------------------------------------------------------
| ` | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 0 | _ | = | \ |BT |
| E |16 |1E |26 |25 |2E |36 |3D |3E |46 |45 |4E |55 |5D |66 |
|-----------------------------------------------------------|
| Tab | Q | W | E | R | T | Y | U | I | O | P | { | } |     |
|  D  |15 |1D |24 |2D |2C |35 |3C |43 |44 |4D |54 |5B | 5A  |
|------------------------------------------------------     |
| Ctrl | A | S | D | F | G | H | J | K | L | : | " |  Enter |
|  14  |1C |1B |23 |3B |34 |33 |3B |42 |4B |4C |52 |        |
|-----------------------------------------------------------|
| Shift  | Z | X | C | V | B | N | M | < | > | ? |  Shift   |
|   12   |1A |22 |32 |3A |32 |31 |3A |41 |49 |4A |    59    |
|-----------------------------------------------------------|
| Alt  |  |                                       |  | Caps |
|  11  |  |                 29                    |  |  58  |
--------  -----------------------------------------  --------
