IMPLEMENTATION MODULE SPOws; (*$T-$N-$I- Leo 16-Aug-91. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  cod: defCodes;

TYPE ADDRESS = SYSTEM.ADDRESS;

CONST FONT = ARRAY OF INTEGER {
 084004104h, 0A00000A2h, 038A7CA7Ch, 011394385h, 050910842h, 010858958h,
 004200000h, 041022041h, 010A00210h, 01F10400Ah, 000000041h, 001F00020h,
 000000000h, 010842102h, 039155138h, 0CE104184h, 0F7C13903h, 028F40E41h,
 05F7087C9h, 0138F40F0h, 041F3913Ch, 091384108h, 0E44E3913h, 000400E41h,
 000200004h, 040842042h, 07C07C020h, 084204080h, 0400844E0h, 078175538h,
 0D145F44Eh, 0E3D13D13h, 03DE04105h, 05F3D1451h, 017DF04F0h, 004E0413Ch,
 0D144E45Dh, 0410E4517h, 020870E10h, 04725110Ah, 010410512h, 04556D17Ch,
 059553451h, 0E45144E4h, 00413D13Ch, 0D625144Eh, 0E4493D13h, 07CE40E04h,
 051104104h, 0144E4514h, 045110A45h, 00A44A555h, 0429144A1h, 01087C410h,
 08208E7C2h, 0810204E0h, 020820E40h, 00044A10Eh, 0F0000000h, 000000001h};

VAR font: ADDRESS;
     bmd: ADDRESS;

   shift: ADDRESS;
  pallet: ADDRESS;
    back: ADDRESS;
  line00: ARRAY [0..15] OF INTEGER;

---------------------  HARDWARE INTERFACE  ---------------------
                     ----------------------

PROCEDURE bblt(to: ADDRESS;   to_ofs: INTEGER;
             from: ADDRESS; from_ofs: INTEGER; nobits: INTEGER);
CODE cod.bblt END bblt;

PROCEDURE move(dest,sou: ADDRESS; size: INTEGER);
CODE cod.move END move;

-------------------------------------------------------------

PROCEDURE erase;
  VAR adr: ADDRESS;
BEGIN
  adr:=1F8000h; adr^:=0;  move(adr+1,adr,128*256-1)
END erase;

PROCEDURE putc(l,c: INTEGER; ch: CHAR);
  VAR i,j,adr: INTEGER;
  PROCEDURE z; BEGIN bblt(adr,c,SYSTEM.ADR(line00),0,6); INC(adr,16) END z;
BEGIN
   ch:=CHAR(BITSET(ch)*{0..6});
   IF ("a"<=ch) & (ch<="z") THEN ch:=CAP(ch) END;
   l:=l*8;
   c:=c*6;
   j:=ORD(ch)-41b;
   adr:=bmd+l*16;
   IF (j>=0) & (j<=137b-41b) THEN
     j:=j*(5*6);
     FOR i:=0 TO 4 DO bblt(adr,c,font,j,6); INC(adr,16); INC(j,6) END;
     FOR i:=5 TO 7 DO z END
   ELSE
     FOR i:=0 TO 7 DO z END
   END
END putc;


PROCEDURE scrollup;
  VAR adr: ADDRESS;
BEGIN
  adr:=bmd+360*16;  adr^:=0;  move(adr+1,adr,16*8-1);
  move(bmd,bmd+8*16,360*16)
END scrollup;

----------------------------------------------------------------

VAR wl,wc: INTEGER;


PROCEDURE eraseline;
  VAR l,c,i,j,adr,w: INTEGER;
BEGIN
   l:=wl*8;
   c:=wc*6;
   adr:=bmd+l*16;
   w:=480-c;
   FOR i:=0 TO 7 DO bblt(adr,c,SYSTEM.ADR(line00),0,w); INC(adr,16) END
END eraseline;


PROCEDURE puts(VAL s: ARRAY OF CHAR; pos,len: INTEGER);

  PROCEDURE nl;
  BEGIN
    IF wl<44 THEN INC(wl) ELSE scrollup END
  END nl;

  VAR ch: CHAR;

BEGIN
  WHILE (len>0) & (pos<=HIGH(s)) & (s[pos]#0c) DO
    ch:=s[pos];
    IF    ch=15c THEN wc:=0
    ELSIF ch=12c THEN nl
    ELSIF ch=36c THEN wc:=0; nl
    ELSE
      putc(wl,wc,ch);
      IF autowrap THEN
         IF wc=79 THEN wc:=0; nl ELSE INC(wc) END
      ELSIF wc<79 THEN INC(wc)
      END
    END;
    DEC(len); INC(pos)
  END
END puts;

PROCEDURE setpos(l,c: INTEGER);
BEGIN
  IF l<0 THEN wl:=0 ELSIF l>44 THEN wl:=44 ELSE wl:=l END;
  IF c<0 THEN wc:=0 ELSIF c>79 THEN wc:=79 ELSE wc:=c END
END setpos;

PROCEDURE writech(ch: SYSTEM.WORD);
  VAR s: POINTER TO ARRAY [0..0] OF CHAR;
BEGIN
  s:=SYSTEM.ADR(ch);
  puts(s^,0,1);
END writech;

PROCEDURE writestr(adr: ADDRESS);
  VAR ptr: POINTER TO ARRAY [0..0FFh] OF CHAR;
      pos: INTEGER;
      len: INTEGER;
BEGIN
  ptr:=adr;
  len:=0;
  WHILE (len<255) & (ptr^[len]#0c) DO INC(len) END;
  puts(ptr^,0,len)
END writestr;

PROCEDURE writeint(i: INTEGER);
  PROCEDURE writedig(i: INTEGER);
  BEGIN
    IF i>=10 THEN writedig(ABS(i DIV 10)) END;
    writech(ORD('0')+ABS(i MOD 10));
  END writedig;
BEGIN
  IF i<0 THEN writech('-') END;
  writedig(i);
END writeint;

CONST hexdig = "0123456789ABCDEF";

PROCEDURE writehex(v: INTEGER);
  VAR i: INTEGER;
BEGIN i:=8;
  REPEAT
    v:=v<<4; writech(hexdig[v MOD 16]); i:=i-1;
  UNTIL i=0;
END writehex;

PROCEDURE print(VAL fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
  VAR i: INTEGER;
      a: INTEGER;
     ch: CHAR;
     op: CHAR;
    pos: INTEGER;
    len: INTEGER;
    val: INTEGER;
BEGIN
  IF HIGH(fmt)<=0 THEN RETURN END;
  i:=0; a:=0;
  LOOP
    ch :=fmt[i];
    pos:=i;
    WHILE (i<=HIGH(fmt)) & (ch#0c) & (ch#'%') & (ch#'\') DO
      INC(i); ch:=fmt[i]
    END;
    len:=i-pos;
    puts(fmt,pos,len);
    op:=ch;
    IF (i>HIGH(fmt)) OR (ch=0c) THEN EXIT END;
    INC(i);
    IF i>HIGH(fmt) THEN EXIT ELSE ch:=fmt[i] END;
    IF op='\' THEN
      IF    ch='\' THEN writech(op)
      ELSIF ch='n' THEN writech(36c)
      ELSIF ch='r' THEN writech(15c)
      ELSIF ch='l' THEN writech(12c)
      ELSE writech(op); writech(ch)
      END
    ELSE (* op='%' *)
      IF a>HIGH(args) THEN
        writech(op); writech(ch); EXIT
      ELSE val:=args[a]; INC(a)
      END;
      IF    ch='d' THEN writeint(val);
      ELSIF ch='s' THEN writestr(val);
      ELSIF ch='h' THEN writehex(val);
      ELSIF ch='c' THEN writech (val);
      ELSE writech(op); writech (ch)
      END;
    END;
    INC(i);
    IF i>HIGH(fmt) THEN EXIT ELSE ch:=fmt[i] END;
  END;
  pos:=i;
  WHILE (i<=HIGH(fmt)) & (ch#0c) DO INC(i); ch:=fmt[i] END;
  len:=i-pos;
  puts(fmt,pos,len)
END print;

PROCEDURE _adr(VAL s: ARRAY OF SYSTEM.WORD): ADDRESS;
BEGIN RETURN SYSTEM.ADR(s) END _adr;

PROCEDURE plane(n: INTEGER);
  CONST
    palet0 = ARRAY OF INTEGER
             { 0FFFh, 0FFCh, 0FCFh, 0FCCh, 0CFFh, 0CFCh, 0CCFh, 0DDDh,
               0CCCh, 0FFEh, 0FEFh, 0FEEh, 0EFFh, 0EFEh, 0EEFh, 0EEEh
             };
  VAR i: INTEGER;
      p: ARRAY [0..15] OF INTEGER;
   adr1: ADDRESS;
   adr0: ADDRESS;
BEGIN
  n:=n MOD 4;
  IF bmd=1F8000h+n*(512*16) THEN RETURN END;
  FOR i:=0 TO 15 DO
    IF {n}*BITSET(i)={} THEN p[i]:=palet0[0] ELSE p[i]:=palet0[2] END
  END;
  adr0:=bmd;
  bmd:=1F8000h+n*(512*16);
  move(bmd,adr0,32*256);
  adr1:=pallet;
  REPEAT UNTIL BITSET(back^)*{0}={};
  REPEAT UNTIL BITSET(back^)*{0}#{};
  move(adr1,SYSTEM.ADR(p),16);
  adr0^:=0;
  move(adr0+1,adr0,32*256-1)
END plane;

PROCEDURE init;

  VAR i: INTEGER;
    adr: ADDRESS;

BEGIN
  shift:=ADDRESS(1F0000h);      pallet:=ADDRESS(1F0010h);
  shift^:=0;                      back:=ADDRESS(1F0020h);

  erase;
  bmd:=1F8000h;
  plane(1);

  adr:=SYSTEM.ADR(line00);
  adr^:=0;
  move(adr+1,adr,SIZE(line00)-1);

  font:=_adr(FONT);

  setpos(0,0);
  autowrap:=TRUE

END init;


VAR i,j: INTEGER;

BEGIN
  init
END SPOws.
