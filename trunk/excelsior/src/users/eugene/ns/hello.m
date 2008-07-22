MODULE cold_lab; (* Sem 06-May-88. (c) KRONOS *)

IMPORT SYSTEM, mCodeMnem;

PROCEDURE loader;

MODULE con; (* 08-May-88. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADDRESS, WORD;
FROM mCodeMnem  IMPORT  copt, jbsc, drop, li0, swap;

EXPORT print, Read, Write, Peek;

CONST
  BASE=0F00000h;
  HALF=10000h;


VAR
  put_in : INTEGER;           MASK16_31: BITSET; (* :={16..31} *)
  put_end: INTEGER;
  put_beg: INTEGER;
  put_out: INTEGER;

  get_in : INTEGER;           MASK00_15: BITSET; (* :={00..15} *)
  get_end: INTEGER;
  get_beg: INTEGER;
  get_out: INTEGER;

  BCB    : POINTER TO ARRAY [0..0FFFFh] OF CHAR;
  wBCB   : POINTER TO ARRAY [0..0FFFFh] OF INTEGER;
  F0000h : POINTER TO ARRAY [0..0FFFFh] OF CHAR;
  inFLAG : INTEGER; (* byte pointers *)
  outFLAG: INTEGER; (* relative 0    *)

PROCEDURE InitBCB(channel: INTEGER);
  VAR adr: ADDRESS;
BEGIN
  adr:=BASE+(0F8010h+channel*4) DIV 4;
  adr:=INTEGER(adr^) MOD HALF;
  ASSERT(adr MOD 4 = 0);
  BCB:=ADDRESS( F0000h ) + adr DIV 4; wBCB:=ADDRESS(BCB);
  outFLAG:=( INTEGER(BCB)*4 )+12;
   inFLAG:=( INTEGER(BCB)*4 )+00;

  put_in :=INTEGER(wBCB^[3]>>16) MOD HALF; (* 14,15 bytes BCB *)
  put_end:=INTEGER(wBCB^[5]>>16) MOD HALF; (* 22,23 bytes BCB *)
  put_beg:=        wBCB^[5] MOD HALF;      (* 20,21 bytes BCB *)

  get_out:=        wBCB^[1]      MOD HALF;  (*  4, 5 bytes BCB *)
  get_end:=INTEGER(wBCB^[2]>>16) MOD HALF;  (* 10,11 bytes BCB *)
  get_beg:=        wBCB^[2]      MOD HALF;  (*  8, 9 bytes BCB *)

END InitBCB;

PROCEDURE wait(flag: INTEGER);
CODE copt li0 swap 094h 02h jbsc 07h drop END wait;

PROCEDURE Write(c: CHAR);
  VAR new: INTEGER;
BEGIN
  IF c=36c THEN Write(15c); Write(12c); RETURN END;
  LOOP
    wait(outFLAG);
    put_out:=INTEGER( BITSET(wBCB^[4])*MASK00_15 );
    BCB^[12]:=1c;
    IF put_in=put_end THEN new:=put_beg ELSE new:=put_in+1 END;
    IF new#put_out THEN
      F0000h^[put_in]:=c; put_in:=new; EXIT
    END;
  END;
  wait(outFLAG);
  wBCB^[3]:=INTEGER( BITSET(wBCB^[3])-MASK16_31+BITSET(put_in<<16)+{0} );
END Write;

PROCEDURE Peek(n: INTEGER): CHAR;
  VAR ch: CHAR;
BEGIN
  IF n#0 THEN RETURN 0c END;
  wait(inFLAG);
  get_in:=INTEGER( BITSET(wBCB^[0]>>16)*MASK00_15 );
  BCB^[0]:=1c;
  IF get_in=get_out THEN RETURN 0c END;
  ch:=F0000h^[get_out];
  IF get_out=get_end THEN get_out:=get_beg ELSE INC(get_out) END;
  wait(inFLAG);
  wBCB^[1]:=INTEGER( BITSET(wBCB^[1]) - MASK00_15 + BITSET(get_out) );
   BCB^[0]:=1c;
  RETURN ch
END Peek;

PROCEDURE Read(): CHAR;
  VAR c: CHAR;
BEGIN
  REPEAT c:=Peek(0) UNTIL c#0c; RETURN c
END Read;

PROCEDURE print(VAL fmt: ARRAY OF CHAR; SEQ args: WORD);

TYPE String = ARRAY [0..255] OF CHAR;
     flags  = SET OF (lj,sg,zr,bs,sp,cap,w1,w2,pnt,bck);

VAR buf       : String;
    bcnt,bptr : INTEGER;
         fcnt : INTEGER;
    acnt      : INTEGER;
    ch        : CHAR;
    base      : CHAR;
    val       : WORD;
    wd1,wd2   : INTEGER;
    flg       : flags;

PROCEDURE AppInt(n: INTEGER);
  VAR j: INTEGER; sig: BOOLEAN;
BEGIN
  sig:=INTEGER(val)<0; bcnt:=HIGH(buf)+1; bptr:=bcnt;
  IF bs IN flg THEN DEC(bptr); buf[bptr]:=base END;
  REPEAT
    j:=ABS(INTEGER(val) MOD n);
    IF j>9 THEN INC(j,7) END;
    DEC(bptr); buf[bptr]:=CHAR(j+ORD('0'));
    val:=INTEGER(val) DIV n;
  UNTIL val=0;
  j:=0;
  IF zr IN flg THEN
    IF w1 IN flg THEN
      j:=wd1-bcnt+bptr;
      IF (sp IN flg)OR(sg IN flg)OR sig THEN DEC(j) END;
    END;
    WHILE j>0 DO DEC(bptr); buf[bptr]:='0'; DEC(j) END;
  END;
  IF sp IN flg THEN
    DEC(bptr); IF sig THEN buf[bptr]:='-' ELSE buf[bptr]:=' ' END;
  ELSIF sg IN flg THEN
    DEC(bptr); IF sig THEN buf[bptr]:='-' ELSE buf[bptr]:='+' END;
  ELSIF sig THEN
    DEC(bptr); buf[bptr]:='-';
  END;
END AppInt;

PROCEDURE AppCar(n: INTEGER);
  VAR j: INTEGER; m: BITSET;
BEGIN
  m:={};
  FOR j:=0 TO n-1 DO INCL(m,j) END;
  bcnt:=HIGH(buf)+1; bptr:=bcnt;
  IF bs IN flg THEN DEC(bptr); buf[bptr]:=base END;
  REPEAT
    j:=INTEGER(BITSET(val)*m);
    IF j>9 THEN INC(j,7) END;
    DEC(bptr); buf[bptr]:=CHAR(j+ORD('0'));
    val:=(BITSET(val)-m)>>n;
  UNTIL val=0;
  j:=0;
  IF zr IN flg THEN
    IF w1 IN flg THEN j:=wd1-bcnt+bptr END;
    WHILE j>0 DO DEC(bptr); buf[bptr]:='0'; DEC(j) END;
  END;
END AppCar;

PROCEDURE AppStr;
  VAR p: POINTER TO String;
BEGIN
  p:=val; bptr:=0; bcnt:=0;
  WHILE (bcnt<=HIGH(buf))&(p^[bcnt]#0c) DO buf[bcnt]:=p^[bcnt]; INC(bcnt) END;
END AppStr;

PROCEDURE AppCh;
BEGIN
  bcnt:=1; bptr:=0; buf[0]:=CHAR(val);
END AppCh;

PROCEDURE AppNum(i: INTEGER);
BEGIN
  IF i DIV 10 > 0 THEN AppNum(i DIV 10) END;
  buf[bcnt]:=CHAR(i MOD 10 + ORD('0')); INC(bcnt);
END AppNum;

PROCEDURE AppSet;
  VAR  i,Cou: INTEGER;
       Emp  : BOOLEAN;
BEGIN
  Cou:=0; Emp:=TRUE;
  bcnt:=1; bptr:=0; buf[0]:='{';
  FOR i:=0 TO 32 DO
    IF i IN BITSET(val) THEN
      INC(Cou)
    ELSIF Cou>0 THEN
      IF NOT Emp THEN buf[bcnt]:=','; INC(bcnt) END;
      IF Cou=1 THEN
        AppNum(i-1);
      ELSE
        AppNum(i-Cou);
        buf[bcnt]:='.'; INC(bcnt); buf[bcnt]:='.'; INC(bcnt);
        AppNum(i-1);
      END;
      Cou:=0; Emp:=FALSE;
    END;
  END;
  buf[bcnt]:='}'; INC(bcnt);
END AppSet;

PROCEDURE Next;
BEGIN
  IF fcnt<=HIGH(fmt) THEN ch:=fmt[fcnt]; INC(fcnt) ELSE ch:=0c END;
END Next;

PROCEDURE GetArg(): WORD;
  VAR w: WORD;
BEGIN
  IF acnt<=HIGH(args) THEN w:=args[acnt]; INC(acnt) ELSE w:=0 END;
  RETURN w;
END GetArg;

PROCEDURE GetNo(): INTEGER;
  VAR i: INTEGER;
BEGIN
  i:=ORD(ch)-ORD('0'); Next;
  WHILE (ch>='0')&(ch<='9') DO i:=i*10+(ORD(ch)-ORD('0')); Next END;
  RETURN i;
END GetNo;

PROCEDURE width(n: INTEGER);
BEGIN
  IF pnt IN flg THEN INCL(flg,w2); wd2:=n ELSE INCL(flg,w1); wd1:=n END;
END width;

PROCEDURE SubFormat(): BOOLEAN;
  VAR j: INTEGER;
BEGIN
  val:=GetArg(); wd1:=0; wd2:=0; flg:=flags{};
  LOOP
    CASE ch OF
     |'#': INCL(flg,bs); Next;
     |'$': INCL(flg,zr); Next;
     |'-': INCL(flg,lj); Next;
     |'+': INCL(flg,sg); Next;
     |' ': INCL(flg,sp); Next;
     |'*': width(GetArg()); Next;
     |'0'..'9': width(GetNo());
     |'.': INCL(flg,pnt); Next;
    ELSE base:=ch; Next; EXIT
    END;
  END;
  IF base='{' THEN IF ch='}' THEN Next ELSE RETURN TRUE END END;
  IF base<'a' THEN INCL(flg,cap); INC(base,40b) END;
  CASE base OF
   |'d': AppInt(10);
-- |'e': Float (s,w[0],AppRfE,flg,n,wd);
-- |'f': Float (s,w[0],AppRfF,flg,n,wd);
-- |'g': Float (s,w[0],AppRfG,flg,n,wd);
   |'h': AppCar(4);
   |'x': AppInt(16);
   |'b': AppCar(3);
   |'o': AppInt(8);
   |'i': AppInt(2);
   |'{': AppSet;
   |'s': AppStr; flg:=flg/flags{lj};
   |'c': AppCh;
  ELSE RETURN TRUE;
  END;
  IF w1 IN flg THEN j:=wd1-bcnt+bptr ELSE j:=0 END;
  IF lj IN flg THEN
    WHILE bptr<bcnt DO Write(buf[bptr]); INC(bptr) END;
    WHILE j>0 DO Write(' '); DEC(j) END;
  ELSE
    WHILE j>0 DO Write(' '); DEC(j) END;
    WHILE bptr<bcnt DO Write(buf[bptr]); INC(bptr) END;
  END;
  RETURN FALSE;
END SubFormat;

PROCEDURE Format(): BOOLEAN;
  VAR i,j: INTEGER; c: CHAR;
BEGIN
  i:=fcnt; j:=acnt; c:=ch;
  IF SubFormat() THEN
    fcnt:=i; acnt:=j; ch:=c; RETURN TRUE
  END;
  RETURN FALSE;
END Format;

BEGIN
  fcnt:=1; acnt:=0; ch:=fmt[0];
  LOOP
    IF     ch=0c THEN
      RETURN;
    ELSIF ch='\' THEN
      Next;
      IF    ch='n' THEN Write(15c); Write(12c)
      ELSIF ch='e' THEN Write(33c);
      ELSIF ch='r' THEN Write(15c);
      ELSIF ch='l' THEN Write(12c);
      ELSIF ch='\' THEN Write('\');
      ELSIF ch=0c  THEN Write('\');
      ELSE Write('\'); Write(ch)
      END;
      Next;
    ELSIF ch='%' THEN
      Next;
      IF (ch=0c)OR(ch='%') THEN
        Write('%'); Next;
      ELSIF Format() THEN
        Write('%')
      END;
    ELSE
      Write(ch);
      IF fcnt<=HIGH(fmt) THEN ch:=fmt[fcnt]; INC(fcnt) ELSE ch:=0c END;
    END;
  END;
END print;

BEGIN
  MASK16_31:={16..31};
  MASK00_15:={00..15};
  F0000h:=ADDRESS( BASE + 0F0000h DIV 4 );
  InitBCB(9);
END con;

----------------------------------------------------------------------------

MODULE disk; (* 08-May-88. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADDRESS;
FROM mCodeMnem  IMPORT  quit, move;
IMPORT print;

EXPORT spec, ReadBlock, check, SetSpecification, GetSpecification, NoVolume;

CONST
  mBusBuff=ADDRESS((0D0000h+32*1024) DIV 4 + 0F00000h);
  READ =5;
  WRITE=6;
  SEEK =13;
  FLASH=4;
  READ_TRACK =7;
  WRITE_TRACK=8;
  ATN   = 00F4h<<30;

  ok            = BOOLEAN(0);
  WriteProtect  = BOOLEAN(33);
  ReadError     = BOOLEAN(30);
  CheckSum      = BOOLEAN(46);
  NoVolume      = BOOLEAN(37);
  IllegalAddress= BOOLEAN(41);
  TimeOut       = BOOLEAN(42);
  HardError     = BOOLEAN(40);
  WriteError    = BOOLEAN(31);
  NoSuchBlock   = BOOLEAN(34);
  NoSuchDrive   = BOOLEAN(34);
  NotImplemented= BOOLEAN(43);

VAR
  spec: ARRAY [0..6] OF RECORD
    tracks : INTEGER; (* for one head *)
    restrk : INTEGER;
    heads  : INTEGER; (* sides for FD *)
    secno  : INTEGER;
    secsize: INTEGER;
    minsec : INTEGER;
  END;
  ST    : BITSET;
  BCB   : POINTER TO ARRAY [0..31] OF CHAR;

PROCEDURE QUIT; CODE quit END QUIT;
PROCEDURE MOVE(f,t: ADDRESS; s: INTEGER); CODE move END MOVE;
PROCEDURE OUT(n,v: INTEGER); CODE 93h END OUT;

PROCEDURE wait():BOOLEAN;
BEGIN
  LOOP IF BCB^[0]=0c THEN RETURN ok END END;
END wait;

PROCEDURE exec(): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  BCB^[0]:=377c; OUT(ATN,5); RETURN wait();
END exec;

PROCEDURE err?(): BOOLEAN;
  VAR r: BOOLEAN; s: BITSET;  i,Sec,Sur,Trk,Drv: INTEGER;
BEGIN
  s:=BITSET (BCB^[1]);
  IF NOT (7 IN s) THEN RETURN FALSE END;
  Drv:=ORD(BCB^[2]) MOD 8;
  IF Drv<4 THEN
    IF    6 IN s THEN r:=WriteProtect
    ELSIF 4 IN s THEN r:=ReadError
    ELSIF 3 IN s THEN r:=CheckSum
    ELSIF 2 IN s THEN r:=TimeOut
    ELSIF 1 IN s THEN r:=ReadError
    ELSE  r:=NoVolume
    END;
  ELSE
    IF NOT (7 IN s) THEN RETURN ok END;
    IF    5 IN s THEN r:=WriteError
    ELSE  r:=NoVolume
    END;
  END;
  Trk:=ORD(BCB^[3]);
  Sec:=ORD(BCB^[5]);
  IF Drv<4 THEN
    Sur:=ORD(BCB^[4]);
  ELSE
    Sur:=ORD(BCB^[4]) DIV 10h;
    Trk:=ORD(BCB^[4]) MOD 10h * 100h + Trk;
    s:=s+BITSET(100h*ORD(BCB^[6]));
  END;
  BCB^[1]:=2c; -- abort operation
  IF exec() THEN END;
  RETURN r
END err?;

PROCEDURE trackio(drv,op,track,side,sec,no,adr: INTEGER): BOOLEAN;
  VAR r: BOOLEAN;
BEGIN
  WITH spec[drv] DO
    IF (side<0)OR(side>=heads)    THEN RETURN NoSuchBlock END;
    r:=wait(); IF r THEN RETURN r END;
    BCB^[1]:=CHAR(op);
    BCB^[2]:=CHAR(drv);
    BCB^[3]:=CHAR(track);
    IF drv<4 THEN
      BCB^[4]:=CHAR(side);
    ELSE
      BCB^[4]:=CHAR(side*10h+(track DIV 100h));
    END;
    BCB^[5]:=CHAR(sec+minsec);
    BCB^[6]:=CHAR(secsize*no); BCB^[7]:=CHAR((secsize*no)>>8);
    BCB^[8]:=CHAR(adr); BCB^[9]:=CHAR(adr>>8);
    BCB^[10]:=CHAR(adr>>16); BCB^[11]:=CHAR(adr>>24);
  END;
  r:=exec();
  IF r THEN RETURN r END;
  r:=err?();
  RETURN r;
END trackio;

PROCEDURE SetSpecification(drv: INTEGER): BOOLEAN;
  VAR r: BOOLEAN; s: BITSET;
BEGIN
  WITH spec[drv] DO
    r:=trackio(drv,10,0,0,0,1,mBusBuff);
    IF r THEN RETURN r END;
    IF    secsize=128  THEN BCB^[7]:=0c;
    ELSIF secsize=256  THEN BCB^[7]:=1c
    ELSIF secsize=512  THEN BCB^[7]:=2c
    ELSIF secsize=1024 THEN BCB^[7]:=3c
    END;
    BCB^[9]:=CHAR(minsec);
    BCB^[8]:=CHAR(minsec+secno-1);
    s:=BITSET(BCB^[5]);
    IF drv>=4 THEN
      s:=s-{6,5}+{4}
    ELSE
      IF (tracks=40) & NOT (drv IN ST) THEN INCL(s,5) ELSE EXCL(s,5) END;
      IF heads>1   THEN INCL(s,6) ELSE EXCL(s,6) END;
      IF tracks=77 THEN
        INCL(s,7);
        IF secno*secsize>4096 THEN BCB^[4]:=0c ELSE BCB^[4]:=377c END;
      ELSE
        EXCL(s,7);
        IF secno*secsize>3000 THEN BCB^[4]:=0c ELSE BCB^[4]:=377c END;
      END;
    END;
    BCB^[5]:=CHAR(s);
    BCB^[1]:=CHAR(11);
    RETURN exec();
  END;
END SetSpecification;

PROCEDURE GetSpecification(d: INTEGER): BOOLEAN;
  VAR r: BOOLEAN;
BEGIN
  IF d>=4 THEN RETURN FALSE END;
  WITH spec[d] DO
    r:=trackio(d,12,0,0,0,1,mBusBuff);
    IF r THEN RETURN r END;
--    IF NOT (0 IN BITSET(BCB^[5])) THEN RETURN NoVolume END;
    secsize:=128<<ORD(BCB^[7]);
    minsec:=1;
    IF 7 IN BITSET(BCB^[5]) THEN
      IF 6 IN BITSET(BCB^[5]) THEN heads:=2 ELSE heads:=1 END;
      tracks:=77;
      IF secsize=128 THEN secno:=52 END;
      IF secsize=256 THEN secno:=26 END;
      IF secsize=512 THEN secno:=16 END;
      IF secsize=1024 THEN secno:=8 END;
    ELSE
      IF secsize=128 THEN secno:=30 END;
      IF secsize=256 THEN secno:=18 END;
      IF secsize=512 THEN secno:=8  END;
      IF secsize=1024 THEN secno:=5 END;
    END;
  END;
  RETURN SetSpecification(d);
END GetSpecification;

PROCEDURE sectorIO(drv,op,sector,sno: INTEGER; adr: ADDRESS): BOOLEAN;
  VAR sd,trk,a,i: INTEGER; r: BOOLEAN;
BEGIN
  IF (adr<0F00000h)OR(adr>=0F3FFFFh) THEN RETURN IllegalAddress END;
  IF NOT (op IN {READ,WRITE}) THEN RETURN NotImplemented END;
  WITH spec[drv] DO
    INC(sector,restrk*secno*heads);
    IF (sector<0) OR (sector>=secno*tracks*heads) THEN
      RETURN NoSuchBlock;
    END;
    trk:=sector DIV secno;
    sector:=sector MOD secno;
    sd:=trk MOD heads;
    trk:=trk DIV heads;
    a:=(adr-0F00000h)*4;
    r:=FALSE;
    WHILE sno>0 DO
      IF sno>secno-sector THEN i:=secno-sector ELSE i:=sno END;
      r:=r OR trackio(drv,op,trk,sd,sector,i,a);
      DEC(sno,i); INC(sector,i); INC(a,secsize*i);
      IF sector>=secno THEN
        sector:=0; INC(sd); IF sd>=heads THEN sd:=0; INC(trk) END;
      END;
    END;
  END;
  RETURN r;
END sectorIO;

PROCEDURE ReadBlock(u,blk: INTEGER; a: ADDRESS; sz: INTEGER): BOOLEAN;
  VAR res: BOOLEAN;
      from,to: POINTER TO ARRAY [0..4095] OF CHAR;
      l,i,Try,sec,cnt: INTEGER;
BEGIN
  l:=spec[u].secsize;
  sec:=blk*(4096 DIV l);
  cnt:=(sz+l-1) DIV l;
  res:=sectorIO(u,READ,sec,cnt,mBusBuff);
  from:=mBusBuff; to:=a;
  MOVE(to,from,sz DIV 4);
  FOR i:=INTEGER(BITSET(sz)-{0,1}) TO sz-1 DO to^[i]:=from^[i] END;
  RETURN res
END ReadBlock;

PROCEDURE check(r: BOOLEAN): BOOLEAN;
BEGIN
  IF NOT r THEN RETURN FALSE END;
  print('Can"t read disk: ');
  CASE r OF
   |WriteProtect  : print('write protected\n');
   |ReadError     : print('read error\n');
   |CheckSum      : print('check summ error\n');
   |NoVolume      : print('drive not ready\n');
   |IllegalAddress: print('illegal memory address\n');
   |TimeOut       : print('time out\n');
   |HardError     : print('drive fault\n');
   |NoSuchBlock   : print('no such block\n');
   |NotImplemented: print('not implemented function\n');
  ELSE
    print('unknown error.\n');
  END;
  RETURN r;
END check;

BEGIN
  BCB:=ADDRESS(0F8010h DIV 4 + 0F00000h);
  BCB:=ADDRESS((ORD(BCB^[4])+256*ORD(BCB^[5])+0F0000h) DIV 4 + 0F00000h);
  ST:={};
  WITH spec[0] DO
    tracks :=77;
    restrk :=0;
    heads  :=2;
    secno  :=8;
    secsize:=1024;
    minsec :=1;
  END;
  WITH spec[1] DO
    tracks :=80;
    restrk :=0;
    heads  :=2;
    secno  :=5;
    secsize:=1024;
    minsec :=1;
  END;
  WITH spec[2] DO
    tracks :=80;
    restrk :=0;
    heads  :=2;
    secno  :=5;
    secsize:=1024;
    minsec :=1;
  END;
  WITH spec[3] DO
    tracks :=80;
    restrk :=0;
    heads  :=2;
    secno  :=5;
    secsize:=1024;
    minsec :=1;
  END;
  WITH spec[4] DO
    tracks :=612;
    restrk :=0;
    heads  :=4;
    secno  :=16;
    secsize:=512;
    minsec :=0;
  END;
  WITH spec[5] DO
    tracks :=612;
    restrk :=0;
    heads  :=4;
    secno  :=16;
    secsize:=512;
    minsec :=0;
  END;
  WITH spec[6] DO
    tracks :=612;
    restrk :=0;
    heads  :=4;
    secno  :=16;
    secsize:=512;
    minsec :=0;
  END;
END disk;

----------------------------------------------------------------------------

PROCEDURE start;
CODE 1 1 85h END start;

PROCEDURE pult;
  PROCEDURE read(): CHAR;
    VAR ch: CHAR;
  BEGIN
    ch:=Read();
    IF (ch>='a') & (ch<='z') THEN ch:=CHAR(ORD(ch)-ORD('a')+ORD('A')) END;
    RETURN ch;
  END read;
  PROCEDURE get_num(VAR n: INTEGER);
    VAR i: INTEGER; ch: CHAR;
  BEGIN
    i:=0;
    LOOP
      ch:=read();
      IF (ch=15c) OR (ch=12c) THEN
        n:=i; RETURN
      ELSIF (ch>='0') & (ch<='9') & (i<=99999) THEN
        Write(ch); i:=i*10+ORD(ch)-ORD('0');
      ELSE
        Write('?'); RETURN
      END;
    END;
  END get_num;
  VAR v,i: INTEGER; a: SYSTEM.ADDRESS; ch: CHAR;
BEGIN
  WHILE Peek(0)#0c DO ch:=Read() END;
  a:=0;
  LOOP
    print('%$8h=%$8h ',a,a^); i:=0; v:=0;
    LOOP
      ch:=read();
      IF ch='Q' THEN print('\n'); RETURN END;
      IF ch='S' THEN
        print('\ndrive ? ');
        ch:=read();
        IF (ch<'0') OR (ch>'6') THEN Write('?'); EXIT END;
        i:=ORD(ch)-ORD('0');
        print('%d',i);
        LOOP
          print('\n>'); ch:=read(); Write(ch);
          IF ch='T' THEN    get_num(spec[i].tracks);
          ELSIF ch='R' THEN get_num(spec[i].restrk);
          ELSIF ch='H' THEN get_num(spec[i].heads);
          ELSIF ch='S' THEN get_num(spec[i].secno);
          ELSIF ch='Z' THEN get_num(spec[i].secsize);
          ELSIF ch='M' THEN get_num(spec[i].minsec);
          ELSE
            EXIT;
          END;
        END;
        WITH spec[i] DO
          print('\ndrive   %4d\n',i);
          print('Tracks  %4d\n',tracks);
          print('Restrk  %4d\n',restrk);
          print('Heads   %4d\n',heads);
          print('Secno   %4d\n',secno);
          print('secsiZe %4d\n',secsize);
          print('Minsec  %4d\n',minsec);
        END;
        EXIT;
      END;
      IF ch='L' THEN
        print('\ndrive ? ');
        ch:=read();
        IF (ch<'0') OR (ch>'6') THEN Write('?'); EXIT END;
        i:=ORD(ch)-ORD('0');
        print('%d\n',i);
        IF check(SetSpecification(i) OR ReadBlock(i,0,0,4096)) THEN END;
        EXIT;
      END;
      IF ch='G' THEN start; EXIT END;
      IF ch=12c THEN IF i>0 THEN a^:=v END; INC(a); EXIT END;
      IF ch=15c THEN IF i>0 THEN a^:=v END; DEC(a); EXIT END;
      IF ch='/' THEN
        IF i>0 THEN a:=v ELSE a:=a^ END; Write(ch);
        EXIT
      END;
      IF (ch>='0')&(ch<='9') THEN
        v:=INTEGER(BITSET(ch)*{0..3}+(BITSET(v<<4)-{0..3}));
        INC(i); Write(ch);
      ELSIF (ch>='A')&(ch<='F') THEN
        v:=INTEGER(BITSET(ORD(ch)+9)*{0..3}+(BITSET(v<<4)-{0..3}));
        INC(i); Write(ch);
      ELSE
        print('?'); EXIT
      END;
    END;
    print('\n');
  END;
END pult;

PROCEDURE load;
  VAR r: BOOLEAN;
BEGIN
  print('Kronos 2.6 bootstrap, 10-Jul-90, (c) Sem.\n');
  print('Press any key for abort bootstrap.\n');
  LOOP
    IF Peek(0)#0c THEN
      pult;
      print('Press any key for abort bootstrap.\n');
    END;
    LOOP
      r:=GetSpecification(0) OR ReadBlock(0,0,0,4096);
      IF r=NoVolume THEN EXIT END;
      IF r THEN print('Boot from floppy disk. '); r:=check(r) END;
      IF Peek(0)#0c THEN EXIT END;
      IF NOT r THEN start END;
    END;
    IF Peek(0)=0c THEN
      LOOP
        r:=ReadBlock(4,0,0,4096);
        IF r=NoVolume THEN EXIT END;
        IF r THEN print('Boot from hard disk. '); r:=check(r) END;
        IF Peek(0)#0c THEN EXIT END;
        IF NOT r THEN start END;
      END;
    END;
  END;
END load;

BEGIN
  load;
END loader;

PROCEDURE di; CODE mCodeMnem.li0 mCodeMnem.setm END di;

VAR i: INTEGER;

BEGIN
  di;
  FOR i:=1 TO 400000 DO END;
  loader;
END cold_lab.
