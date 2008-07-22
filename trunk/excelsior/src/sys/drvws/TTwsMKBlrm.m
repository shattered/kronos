MODULE TTwsMKBlrm; (* 19-Jun-90. (c) KRONOS *)

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
  csi("lSHIFT" ,064h,1);
  csi("rSHIFT" ,02Eh,2);
  csi("ALT"    ,06Ch,3);
  csi("CAPS"   ,066h,4);
  csi("NUMS"   ,008h,5);
  csi("SCROLL" ,07Eh,6);
  csi("CNTRL"  ,067h,7);
  csi("NATIO"  ,06Eh,8);

  t0[021h]:=key.break;
  t0[054h]:=' ';

  t0[061h]:=key.f1;     t0[062h]:=key.f2;
  t0[06Ah]:=key.f3;     t0[069h]:=key.f4;
  t0[072h]:=key.f5;     t0[05Ah]:=key.f6;
  t0[052h]:=key.f7;     t0[04Ah]:=key.f8;
  t0[049h]:=key.f9;     t0[042h]:=key.f10;
  t0[039h]:=key.f11;    t0[032h]:=key.f12;
  t0[031h]:=key.f13;    t0[02Ah]:=key.f14;

  t0[065h]:=key.tab;    t0[028h]:=key.back;
  t0[02Dh]:=15c;        t0[033h]:=33c;
  t0[020h]:=12c;        t0[025h]:=',';
  t0[027h]:='-';        t0[026h]:='.';
  t0[024h]:=key.newln;

  t0[00Bh]:=key.home;   t0[005h]:=key.up;       t0[023h]:=key.pgup;
  t0[00Dh]:=key.left;   t0[007h]:=key.center;   t0[006h]:=key.right;
  t0[00Fh]:=key.end;    t0[00Eh]:=key.dw;       t0[004h]:=key.pgdw;
  t0[00Ch]:=key.ins;


  t0[01Ah]:=key.f1;                             t0[00Ah]:=12c;

  t0[018h]:=key.home;   t0[019h]:=key.ins;      t0[011h]:=key.del;
  t0[01Bh]:=key.end;    t0[013h]:=key.pgup;     t0[010h]:=key.pgdw;

                        t0[01Dh]:=key.up;
  t0[01Eh]:=key.left;   t0[016h]:=key.dw;       t0[017h]:=key.right;



  nm[00Bh]:='7';        nm[005h]:='8';          nm[023h]:='9';
  nm[00Dh]:='4';        nm[007h]:='5';          nm[006h]:='6';
  nm[00Fh]:='1';        nm[00Eh]:='2';          nm[004h]:="3";
  nm[00Ch]:='0';

  t0[060h]:='{';
  t0[063h]:=';';        t0[068h]:='1';
  t0[070h]:='2';        t0[079h]:='3';
  t0[078h]:='4';        t0[059h]:='5';
  t0[051h]:='6';        t0[048h]:='7';
  t0[040h]:='8';        t0[041h]:='9';
  t0[038h]:='0';        t0[03Bh]:='-';
  t0[030h]:='}';

  t0[06Bh]:='j';        t0[06Dh]:='c';
  t0[073h]:='u';        t0[07Bh]:='k';
  t0[058h]:='e';        t0[050h]:='n';
  t0[053h]:='g';        t0[04Bh]:='[';
  t0[04Dh]:=']';        t0[043h]:='z';
  t0[03Dh]:='h';        t0[035h]:='*';
  t0[02Bh]:='~';

  t0[06Fh]:='f';        t0[075h]:='y';
  t0[07Dh]:='w';        t0[05Dh]:='a';
  t0[05Bh]:='p';        t0[055h]:='r';
  t0[04Fh]:='o';        t0[047h]:='l';
  t0[045h]:='d';        t0[03Fh]:='v';
  t0[03Eh]:='\';        t0[036h]:='_';
  t0[037h]:='ъ';

  t0[076h]:='q';        t0[077h]:='^';
  t0[07Fh]:='s';        t0[05Fh]:='m';
  t0[056h]:='i';        t0[057h]:='t';
  t0[04Eh]:='x';        t0[04Ch]:='b';
  t0[046h]:='@';        t0[044h]:='<';
  t0[03Ch]:='>';        t0[034h]:='?';

  t0[003h]:=key.bcktab;

  t1:=t0;

  t1[060h]:='|';
  t1[063h]:='+';        t1[068h]:='!';
  t1[070h]:='"';        t1[079h]:='#';
  t1[078h]:='$';        t1[059h]:='%';
  t1[051h]:='&';        t1[048h]:="'";
  t1[040h]:='(';        t1[041h]:=')';
  t1[038h]:='0';        t1[03Bh]:='=';
  t1[030h]:='}';


  t1[04Bh]:='{';        t1[04Dh]:='}';

  t1[035h]:=':';        t1[02Bh]:='`';

  t1[044h]:=',';        t1[03Ch]:='.';
  t1[034h]:='/';
  t1[037h]:=377c;


  FOR i:=0 TO 255 DO
    IF ("a"<=t0[i]) & (t0[i]<="z") THEN t1[i]:=CHAR(ORD(t1[i])-40b) END
  END;

  t2:=t0;
  FOR i:=0 TO 255 DO
    IF (140c<=t2[i]) & (t2[i]<=177c) THEN t2[i]:=CHAR(200b+ORD(t2[i])-140b) END
  END;

  t3:=t1;
  FOR i:=0 TO 255 DO
    IF (140c<=t0[i]) & (t3[i]<=177c) THEN t3[i]:=CHAR(240b+ORD(t0[i])-140b) END
  END;

  n0:=t0;

  n0[06Bh]:='й';        n0[06Dh]:='ц';
  n0[073h]:='у';        n0[07Bh]:='к';
  n0[058h]:='е';        n0[050h]:='н';
  n0[053h]:='г';        n0[04Bh]:='ш';
  n0[04Dh]:='щ';        n0[043h]:='з';
  n0[03Dh]:='х';

  n0[06Fh]:='ф';        n0[075h]:='ы';
  n0[07Dh]:='в';        n0[05Dh]:='а';
  n0[05Bh]:='п';        n0[055h]:='р';
  n0[04Fh]:='о';        n0[047h]:='л';
  n0[045h]:='д';        n0[03Fh]:='ж';
  n0[03Eh]:='э';        n0[037h]:='ъ';


  n0[076h]:='я';        n0[077h]:='ч';
  n0[07Fh]:='с';        n0[05Fh]:='м';
  n0[056h]:='и';        n0[057h]:='т';
  n0[04Eh]:='ь';        n0[04Ch]:='б';
  n0[046h]:='ю';

  n1:=t1;
  FOR i:=0 TO 255 DO
    IF (300c<=n0[i]) & (n0[i]<=337c) THEN n1[i]:=CHAR(ORD(n0[i])+40b) END
  END;

  n2:=t2;
  FOR i:=0 TO 255 DO
    IF (300c<=n0[i]) & (n0[i]<=377c) THEN n2[i]:=t0[i] END
  END;

  n3:=n2;
  FOR i:=0 TO 255 DO
    IF (300c<=n0[i]) & (n0[i]<=377c) THEN n3[i]:=t1[i] END
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
  output("LT2",t2,"LATIN   state ALT");
  output("LT3",t3,"LATIN   state ALT+SHIFT");
  output("NT0",n0,"NATIO   state NORMAL");
  output("NT1",n1,"NATIO   state SHIFT");
  output("NT2",n2,"NATIO   state ALT");
  output("NT3",n3,"NATIO   state ALT+SHIFT");
  tty.print("total bytes: %d (%d zeros)\n",b,z);
END TTwsMKBlrm.
