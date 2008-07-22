MODULE spo25boo; (*$T-$I-$N- Leo 04-Mar-88. (c) KRONOS *)

IMPORT        SYSTEM;
IMPORT  cod: defCodes;

TYPE WORD = SYSTEM.WORD;
  ADDRESS = SYSTEM.ADDRESS;
  PROCESS = ADDRESS;
       bs = BITSET;
      int = INTEGER;

PROCEDURE move(to,from: ADDRESS; size: INTEGER); CODE cod.move END move;

PROCEDURE TRANSFER(VAR from,to: PROCESS); CODE cod.tra END TRANSFER;

CONST BASE=80000h;
      HALF=10000h;

VAR MASK16_31: BITSET; (* :={16..31} *)
    MASK00_15: BITSET; (* :={00..15} *)
       F0000h: POINTER TO ARRAY [0..0FFFFh] OF CHAR;

(************ Interface with Serial Ports & Consol ************)

TYPE
  serialBCB  =
  RECORD
    put_in  : INTEGER;  get_end : INTEGER;
    put_end : INTEGER;  get_beg : INTEGER;
    put_beg : INTEGER;  get_out : INTEGER;
    out_size: INTEGER;  inp_size: INTEGER;
    outFLAG : INTEGER; (* relative 0    *)
    inpFLAG : INTEGER; (* byte pointers *)
       wBCB : POINTER TO ARRAY [0..0FFh] OF INTEGER;
        BCB : POINTER TO ARRAY [0..0FFh] OF CHAR;
  END;

  serial_ptr = POINTER TO serialBCB;

PROCEDURE initBCB(VAR bcb: serialBCB; channel_no: INTEGER);
  VAR type: INTEGER;
       adr: ADDRESS;
ph_channel: INTEGER;
       ptr: serial_ptr;
BEGIN
  ptr:=SYSTEM.ADR(bcb);
  ph_channel:=9;

  adr:=BASE+(0F8010h+ph_channel*4) DIV 4;
  type:=int(adr^) DIV HALF MOD 256;
  IF NOT (type IN {1,2} ) THEN ASSERT(FALSE,4Fh) END;
  adr:=int(adr^) MOD HALF;
  ASSERT(adr MOD 4 = 0,4Bh);
  WITH ptr^ DO
     BCB:=ADDRESS( F0000h ) + adr DIV 4;
    wBCB:=ADDRESS(BCB);
    outFLAG :=( int(BCB)*4 )+12;
    inpFLAG :=( int(BCB)*4 )+00;

    put_in  :=int(wBCB^[3]>>16) MOD HALF; (* 14,15 bytes BCB *)
    put_end :=int(wBCB^[5]>>16) MOD HALF; (* 22,23 bytes BCB *)
    put_beg :=        wBCB^[5] MOD HALF;      (* 20,21 bytes BCB *)
    out_size:=put_end-put_beg+1;

    get_out :=        wBCB^[1]      MOD HALF;  (*  4, 5 bytes BCB *)
    get_end :=int(wBCB^[2]>>16) MOD HALF;  (* 10,11 bytes BCB *)
    get_beg :=        wBCB^[2]      MOD HALF;  (*  8, 9 bytes BCB *)
    inp_size:=get_end-get_beg+1
  END
END initBCB;

PROCEDURE waitf(flag: INTEGER);
CODE cod.copt cod.trb cod.jbsc 04 cod.drop END waitf;

PROCEDURE puts(VAR bcb: serialBCB; VAL s: ARRAY OF CHAR; VAR Pos,Len: INTEGER);
  VAR pos: INTEGER;
      len: INTEGER;
      new: INTEGER;    put_out: INTEGER;
      ptr: serial_ptr;
BEGIN ptr:=SYSTEM.ADR(bcb);
  WITH ptr^ DO
    pos:=Pos;     len:=Len;
    waitf(outFLAG);
      put_out:=int( bs(wBCB^[4])*MASK00_15 );
    BCB^[12]:=1c;
    LOOP
      IF    len=0       THEN EXIT END;
      IF put_in=put_end THEN new:=put_beg ELSE new:=put_in+1 END;
      IF new=put_out    THEN EXIT END;
      F0000h^[put_in]:=s[pos];   put_in:=new;
      INC(pos);  DEC(len);
    END;
    Pos:=pos;     Len:=len;
    waitf(outFLAG);
    wBCB^[3]:=int( bs(wBCB^[3])-MASK16_31+bs(put_in<<16)+{0} );
    (* +{0} is equal BCB^[12]:=1c *)
  END;
END puts;

PROCEDURE gets(VAR bcb: serialBCB; VAR s: ARRAY OF CHAR; VAR Pos,Len: INTEGER);
  VAR pos: INTEGER;
      len: INTEGER;     get_in: INTEGER;
      ptr: serial_ptr;
BEGIN ptr:=SYSTEM.ADR(bcb);
  pos:=Pos;     len:=Len;
  WITH ptr^ DO
    waitf(inpFLAG);
      get_in:=int( bs(wBCB^[0]>>16)*MASK00_15 );
    BCB^[0]:=1c;
    IF get_in=get_out THEN RETURN END;
    LOOP
      IF len=0  THEN EXIT END;
      s[pos]:=F0000h^[get_out]; INC(pos); DEC(len);
      IF get_out=get_end THEN get_out:=get_beg ELSE INC(get_out) END;
      IF get_in =get_out THEN EXIT END;
    END;
    Pos:=pos;
    Len:=len;
    waitf(inpFLAG);
    wBCB^[1]:=int( bs(wBCB^[1]) - MASK00_15 + bs(get_out) );
     BCB^[0]:=1c;
  END;
END gets;

VAR spo: serialBCB;
 spo_co: INTEGER;
spo_buf: ARRAY [0..127] OF CHAR;

PROCEDURE read_key(): CHAR;
  VAR s: ARRAY [0..0] OF CHAR;
    l,p: INTEGER;
BEGIN
  l:=1; p:=0;
  gets(spo,s,p,l);
  IF l=0 THEN RETURN s[0] ELSE RETURN 0c END;
END read_key;

PROCEDURE flush_spo;
  VAR i: INTEGER;
BEGIN
  i:=0;
  REPEAT puts(spo,spo_buf,i,spo_co) UNTIL spo_co=0;
END flush_spo;

PROCEDURE write_ch(ch: WORD);
BEGIN
  IF spo_co=HIGH(spo_buf) THEN flush_spo END;
  spo_buf[spo_co]:=ch; INC(spo_co);
END write_ch;

PROCEDURE write_str(adr: ADDRESS);
  VAR ptr: POINTER TO ARRAY [0..0FFh] OF CHAR;
      pos: INTEGER;
      len: INTEGER;
BEGIN
  ptr:=adr;
  IF spo_co>0 THEN flush_spo END;
  len:=0;
  WHILE (len<255) & (ptr^[len]#0c) DO INC(len) END;
  IF len>0 THEN pos:=0;
    REPEAT puts(spo,ptr^,pos,len) UNTIL len=0;
  END;
END write_str;

PROCEDURE write_int(i: INTEGER);
  PROCEDURE write_dig(i: INTEGER);
  BEGIN
    IF i>=10 THEN write_dig(ABS(i DIV 10)) END;
    write_ch(ORD('0')+ABS(i MOD 10));
  END write_dig;
BEGIN
  IF i<0 THEN write_ch('-') END;
  write_dig(i);
END write_int;

CONST hex_dig = "0123456789ABCDEF";

PROCEDURE write_hex(v: INTEGER);
  VAR i: INTEGER;
BEGIN i:=8;
  REPEAT
    v:=v<<4; write_ch(hex_dig[v MOD 16]); i:=i-1;
  UNTIL i=0;
END write_hex;

PROCEDURE print(VAL fmt: ARRAY OF CHAR; SEQ args: WORD);
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
    IF len>0 THEN
      IF spo_co>0 THEN flush_spo END;
      REPEAT puts(spo,fmt,pos,len) UNTIL len=0
    END;
    op:=ch;
    IF (i>HIGH(fmt)) OR (ch=0c) THEN EXIT END;
    INC(i);
    IF i>HIGH(fmt) THEN EXIT ELSE ch:=fmt[i] END;
    IF op='\' THEN
      IF    ch='\' THEN write_ch(op)
      ELSIF ch='n' THEN write_ch(15c); write_ch(12c)
      ELSIF ch='g' THEN write_ch(07c)
      ELSIF ch='r' THEN write_ch(15c)
      ELSIF ch='l' THEN write_ch(12c)
      ELSE write_ch(op); write_ch(ch)
      END;
    ELSE (* op='%' *)
      IF a>HIGH(args) THEN
        write_ch(op); write_ch(ch); EXIT
      ELSE val:=args[a]; INC(a)
      END;
      IF    ch='d' THEN write_int(val);
      ELSIF ch='s' THEN write_str(val);
      ELSIF ch='h' THEN write_hex(val);
      ELSIF ch='c' THEN write_ch(CHAR(val));
      ELSE write_ch(op); write_ch(ch)
      END;
    END;
    INC(i);
    IF i>HIGH(fmt) THEN EXIT ELSE ch:=fmt[i] END;
  END;
  IF spo_co>0 THEN flush_spo END;
  pos:=i;
  WHILE (i<=HIGH(fmt)) & (ch#0c) DO INC(i); ch:=fmt[i] END;
  len:=i-pos;
  IF len>0 THEN
    REPEAT puts(spo,fmt,pos,len) UNTIL len=0
  END;
END print;

PROCEDURE init_tty;
BEGIN
  initBCB(spo,0); spo_co:=0
END init_tty;

(************  Interface with Winchester & Floppy  ************)



TYPE
 REQUEST = RECORD
             drn: INTEGER;
             res: INTEGER;
             ofs: INTEGER;
             len: INTEGER;
             buf: ADDRESS;
           END;


    PORT = RECORD
             cyls   : INTEGER; (* # cylinders        *)
             heads  : INTEGER; (* # heads            *)
             secs   : INTEGER; (* # sectors          *)
             ssc    : INTEGER; (* sector size code   *)
             secsize: INTEGER; (* sector len 2**ssc  *)
             res_sec: INTEGER; (* reserved sectors   *)
           END;

VAR unit: INTEGER;
   ports: ARRAY [0..4] OF PORT;

CONST ok = 0;      time_out = -1;
    cash = 4;      hw_fail  = -2;


VAR fdc0: ADDRESS;
    fdc1: ADDRESS;
    fdc2: ADDRESS;
     HEI: ADDRESS;
     DMA: ADDRESS;
  BUFFER: ADDRESS;

CONST
  BUFSIZE = 12*1024;

  IGNORE=bs(00FFh);        RDTRK =bs(07FFh);
  RETRY =bs(01FFh);        WRTRK =bs(08FFh);
  ABORT =bs(02FFh);        FORMAT=bs(09FFh);
  RESET =bs(03FFh);        GDS   =bs(0AFFh);
  FLUSH =bs(04FFh);        PDS   =bs(0BFFh);
  READ  =bs(05FFh);        RDS   =bs(0CFFh);
  WRITE =bs(06FFh);        SEEK  =bs(0DFFh);
  RAWRD =bs(0EFFh);

PROCEDURE wait;
BEGIN
  REPEAT UNTIL (bs(fdc0^)*{0..7}+bs(HEI^))={}
END wait;

PROCEDURE abort;
BEGIN
  REPEAT fdc0^:=ABORT;  HEI^:=5;  wait UNTIL bs(fdc0^)*{8..15}={}
END abort;

PROCEDURE errorw(): BITSET;  (* decode error for Winchester drive *)
BEGIN
  RETURN bs(fdc0^>>8)*{0..7}+bs(fdc1^>>8)*{8..15}
END errorw;

PROCEDURE errorf(): BITSET;  (* decode error for Floppy drive *)
BEGIN
  RETURN bs(fdc0^>>8)*{0..7}
END errorf;

PROCEDURE error(): INTEGER;
BEGIN
  IF int( bs(fdc0^>>16)*{0..7} )<4 THEN RETURN int(errorf())
  ELSE                                  RETURN int(errorw())
  END
END error;

PROCEDURE exec(drn: INTEGER; VAR res: INTEGER);
  VAR sta: BITSET;
BEGIN
  wait;
  sta:=fdc0^;
  IF (sta>>16)*{0..7}#bs(drn) THEN res:=int(bs(res)+bs(hw_fail)) END;
  IF (sta>> 8)*{0..7}={}      THEN RETURN END;
  IF drn<4 THEN res:=int(bs(res)+errorf())
  ELSE          res:=int(bs(res)+errorw())
  END;
  abort
END exec;

PROCEDURE get_spec(drn: INTEGER; VAR res: INTEGER);
  VAR tag: BITSET;
BEGIN
  res:=ok;
  WITH ports[drn] DO
    DMA^ :=(BUFFER-BASE)*4;
    fdc1^:=0;
    fdc0^:=GDS + bs(drn<<16);
    HEI^ :=5; exec(drn,res);
    IF res#ok THEN RETURN END;
    tag:=bs(fdc1^>>8)*{0..7};
    IF drn>=4 THEN
    ELSE
      IF  tag*{7}#{}   THEN cyls:=77;
        IF tag*{6}#{}  THEN heads:=2 ELSE heads:=1 END
      ELSIF tag*{5}#{} THEN cyls:=40  (* it isn't possible to determine *)
      ELSE                  cyls:=80  (* number of sides for 5.25"      *)
      END
    END;
    ssc :=int(bs(fdc1^<<8)*{0..7})+7;
    secs:=int(bs(fdc2^)*{0..7}) - int(bs(fdc2^>>8)*{0..7}) + 1;
    secsize:=1<<ssc
  END;
  res:=ok
END get_spec;

PROCEDURE set_spec(drn: INTEGER; VAR res: INTEGER);
  VAR i: INTEGER;
    den: INTEGER;
    pre: INTEGER;
    tag: BITSET;
BEGIN
  res:=ok;
  WITH ports[drn] DO
    DMA^ :=(BUFFER-BASE)*4;
    fdc1^:=0;
    fdc0^:=GDS + bs(drn<<16);
    HEI^ :=5; exec(drn,res);
    IF res#ok THEN RETURN END;
    tag:=bs(fdc1^>>8)*{0..7};
    IF drn>=4 THEN     den:=000;     tag:={};
      secs:=16;        ssc:=9;   secsize:=1<<ssc;  (* fixed parameters *)
      IF cyls=697 THEN pre:=0 ELSIF cyls<=612 THEN pre:=32 ELSE pre:=255 END
    ELSE  den:=0FFh;   pre:=0;
      IF cyls=40 THEN tag:=tag+{5} ELSE tag:=tag-{5} END;
      i:=secs*secsize;
      IF (cyls=77) & (i>4096) OR (cyls#77) & (i>3000) THEN den:=0 END
    END;
    IF res_sec>=secs*heads*cyls THEN res_sec:=0 END;
    IF drn>=4 THEN fdc2^:=bs(secs-1) + bs(0<<8) + bs(pre<<16)
    ELSE           fdc2^:=bs(secs  ) + bs(1<<8) + bs(pre<<16)
    END;
    fdc1^:= bs(den)  + bs(tag<<8)  + bs(0<<16)   + bs(ssc-7)>>8;
    fdc0^:= PDS      + bs(drn<<16) + bs(0FFh>>8);
    HEI^ :=5;  exec(drn,res)
  END
END set_spec;

PROCEDURE retry(VAR res: INTEGER);
  VAR try: INTEGER;
  erp,lep: BITSET;
  sta,cmd: BITSET;
BEGIN
  try:=4;
  lep:={0..31};   (* Last Error Position *)
  sta:=fdc0^;
  WHILE sta*{8..15}#{} DO
    (* error position in bytes 3,4,5 *)
    erp:=sta-{0..15};       erp:=erp+bs(fdc1^)*{0..7};
    IF (sta>>8)*{6}#{} THEN cmd:=ABORT;  res:=int(bs(res)+errorw())
    ELSIF  erp#lep     THEN cmd:=RETRY;  try:=4;  lep:=erp
    ELSIF   try>0      THEN cmd:=RETRY;  DEC(try)
    ELSE                    cmd:=IGNORE; res:=int(bs(res)+errorw())
    END;
    fdc0^:= cmd + (sta-{0..15});
    HEI^ :=5;  wait;  sta:=fdc0^
  END
END retry;

PROCEDURE readw(VAR r: REQUEST);
  VAR sec: INTEGER;   dma: ADDRESS;
      trk: INTEGER;   buf: ADDRESS;
      hed: INTEGER;   len: INTEGER;
     size: INTEGER;   tai: INTEGER;
BEGIN
  WITH ports[r.drn] DO
    len:=r.len;
    IF len<=0 THEN RETURN END;
    buf:=r.buf;
    sec:=r.ofs+res_sec;
    trk:=sec DIV 16;    hed:=trk MOD heads;
    sec:=sec MOD 16;    trk:=trk DIV heads;
    LOOP
      IF buf>=BASE THEN dma:=buf ELSE dma:=BUFFER END;
      tai:=len;
      IF sec+tai>16 THEN tai:=16-sec END;
      size :=tai*128;
      DMA^ :=(dma-BASE)*4;
      fdc1^:=bs(trk DIV 256 + hed*16) + bs(sec<<8)    + bs(size<<18);
      fdc0^:= READ  +   bs(r.drn<<16) + bs(trk MOD 256)>>8;
      HEI^ :=5;         wait;
      IF bs(fdc0^)*{8..15}#{} THEN retry(r.res) END;
      IF buf<BASE THEN move(buf,BUFFER,size) END;
      DEC(len,tai);
      IF len=0       THEN RETURN END;
      INC(sec,tai);  INC(buf,size);
      IF sec=16      THEN sec:=0; INC(hed);
        IF hed=heads THEN hed:=0; INC(trk) END
      END
    END
  END
END readw;


PROCEDURE ignore(VAR res: INTEGER);
  VAR sta,cmd: BITSET;
BEGIN
  sta:=fdc0^;
  WHILE sta*{8..15}#{} DO
    res:=int(bs(res)+errorf());
    IF (sta>>8)*{5,6}#{} THEN cmd:=ABORT ELSE cmd:=IGNORE END;
    fdc0^:=cmd + (sta-{0..15});
    HEI^ :=5;  wait;  sta:=fdc0^
  END
END ignore;

PROCEDURE readf(VAR r: REQUEST);
  VAR sec: INTEGER;   dma: ADDRESS;
      trk: INTEGER;   buf: ADDRESS;
      hed: INTEGER;   len: INTEGER;
     size: INTEGER;   tai: INTEGER;
BEGIN
  WITH ports[r.drn] DO
    len:=r.len;
    IF len<=0 THEN RETURN END;
    buf:=r.buf;
    sec:=r.ofs+res_sec;
    trk:=sec DIV secs;  hed:=trk MOD heads;
    sec:=sec MOD secs;  trk:=trk DIV heads;
    LOOP
      IF buf>=BASE THEN dma:=buf ELSE dma:=BUFFER END;
      tai:=len;
      IF sec+tai>secs THEN tai:=secs-sec END;
      size :=tai<<(ssc-2);
      DMA^ :=(dma-BASE)*4;
      fdc1^:=bs(hed*16) + bs(sec+1)<<8 + bs(size<<18);
      fdc0^:= READ      + bs(r.drn<<16) + bs(trk MOD 256)>>8;
      HEI^ :=5;       wait;
      IF bs(fdc0^)*{8..15}#{} THEN ignore(r.res) END;
      IF buf<BASE    THEN move(buf,BUFFER,size) END;
      DEC(len,tai);
      IF len=0       THEN RETURN END;
      INC(sec,tai);  INC(buf,size);
      IF sec=secs    THEN sec:=0; INC(hed);
        IF hed=heads THEN hed:=0; INC(trk) END
      END
    END
  END
END readf;

PROCEDURE mount(drn: INTEGER; VAR res: INTEGER);

  VAR ign: INTEGER;
      ptr: POINTER TO ARRAY [0..3Fh] OF CHAR;
      sav: PORT;
      bad: BOOLEAN;

  PROCEDURE unpack(VAL s: ARRAY OF CHAR; p,l: INTEGER): INTEGER;
    VAR i,c: INTEGER;
  BEGIN
    i:=0;
    FOR p:=p TO p+l-1 DO
      c:=ORD(s[p]);
      IF    (ORD("0")<=c) & (c<=ORD("9")) THEN i:=i<<4+(c-ORD("0"))
      ELSIF (ORD("A")<=c) & (c<=ORD("F")) THEN i:=i<<4+(c-ORD("A")+10)
      ELSE bad:=TRUE
      END
    END;
    RETURN i
  END unpack;

BEGIN
  res:=ok;
  WITH ports[drn] DO
    DMA^ :=(BUFFER-BASE)*4;
    fdc1^:=bs(ORD(drn<4)<<8) + bs(40h<<16);
    fdc0^:= READ + bs(drn<<16);
    HEI^ :=5;   exec(drn,res);
    IF res#ok THEN RETURN END;
    ptr:=BUFFER;
    IF (ptr^[8]="x") & (ptr^[9]="2") & (ptr^[10]="d") THEN
      secs   :=ORD(ptr^[11]);
      cyls   :=ORD(ptr^[12])+ORD(ptr^[13])*256;
      heads  :=ORD(ptr^[14]) MOD 16;
      ssc    :=ORD(ptr^[14]) DIV 16;
      res_sec:=ORD(ptr^[15])
      (* ptr^[16..19] = time *)
    ELSIF (ptr^[35h]='x') & (ptr^[36h]='d') THEN
      sav:=ports[drn];
      bad:=FALSE;
      cyls :=unpack(ptr^,37h,3);
      secs :=unpack(ptr^,3Ah,2);
      heads:=unpack(ptr^,3Ch,1);
      ssc  :=unpack(ptr^,3Dh,1);      res_sec:=unpack(ptr^,3Eh,2);
      IF bad THEN ports[drn]:=sav; set_spec(drn,ign); RETURN END
    ELSE
      RETURN
    END;
    secsize:=1<<ssc;
    set_spec(drn,res)
  END
END mount;

PROCEDURE init_disks;
  VAR r: INTEGER;
    adr: ADDRESS;
BEGIN
  BUFFER:=ADDRESS(0D4000h DIV 4 + BASE);
  HEI   :=ADDRESS(0D0000h DIV 4 + BASE);

  adr :=(0F8010h+1*4) DIV 4 + BASE; (* int(adr^) DIV HALF MOD 256 M.B.=4 *)
  adr :=int(adr^) MOD HALF;
  fdc0:=ADDRESS( F0000h ) + adr DIV 4;
  fdc1:=fdc0+1;
  fdc2:=fdc0+2;
  DMA :=fdc2;
  unit:=int(bs(fdc0^>>16)*{0..7});

  REPEAT UNTIL HEI^=0;
  abort;
  WITH ports[0] DO
    heads:= 2;      res_sec:=5*2;
    get_spec(0,r);
    ssc  :=10;      secsize:=1<<ssc;
    IF cyls=77 THEN secs:=8 ELSE cyls:=80; secs:=5 END;
    set_spec(0,r)
  END;
  WITH ports[4] DO
    secs := 16;     heads  := 4;
    cyls :=306;     res_sec:= secs*heads;
    ssc  :=  9;     secsize:= 1<<ssc;
    set_spec(4,r)
  END;
  mount(unit,r);
  IF r=0 THEN RETURN END;
  unit:=4;
  mount(unit,r)
END init_disks;

PROCEDURE read(drn: INTEGER; buf: ADDRESS; sec,len: INTEGER; VAR res: INTEGER);
  VAR r: REQUEST;
BEGIN
  r.drn:=drn;     r.len:=len;
  r.ofs:=sec;     r.res:=ok;
  r.buf:=buf;
  IF drn<4  THEN readf(r) ELSE readw(r) END;
  res:=r.res;
  IF res=ok THEN RETURN END;
  IF drn<4  THEN readf(r) ELSE readw(r) END;
  res:=r.res
END read;

(**********************  Consol Monitor  **********************)

PROCEDURE boot;
  VAR r: INTEGER;
BEGIN
  LOOP
    print("%c[31m\n...read booter from DK%c[33m",33c,33c);
    print("%d%c[31m...%c[32m\n",unit,33c,33c);
    read(unit,0,0,4096>>ports[unit].ssc,r);
    IF r=ok THEN RETURN END;
    unit:=4-unit;
  END;
END boot;

PROCEDURE start;
  VAR adr: ADDRESS; ipted,dummy: PROCESS;
BEGIN
  adr:=1; ipted:=adr^; TRANSFER(dummy,ipted)
END start;

PROCEDURE monitor;
  VAR adr,new: ADDRESS;
       dir,ch: CHAR;
          pri: BOOLEAN;
    w,i,x,pos: INTEGER;
BEGIN
  i:=2000;
  REPEAT i:=i-1; ch:=read_key() UNTIL (ch=3c) OR (i=0);
  IF ch#3c THEN boot; start END;
  adr:=1; dir:=" ";
  LOOP
    new:=0;
    x:=adr^;
    print("\n%c%h=%h ",dir,adr,x);
    pos:=0;
    LOOP
      ch:=read_key();
      IF (ch>='a') & (ch<='z') THEN ch:=CAP(ch) END;
      IF (ch=201c) THEN ch:=15c END;
      IF (ch=202c) THEN ch:=12c END;
      pri:=TRUE;
      IF    ("0"<=ch) & (ch<="9") THEN
        new:=int(new<<4)+ORD(ch)-ORD("0");    INC(pos);
      ELSIF ("A"<=ch) & (ch<="F") THEN
        new:=int(new<<4)+ORD(ch)-ORD("A")+10; INC(pos);
      ELSIF ch = 10c THEN
        IF pos>0 THEN DEC(pos);
          new:=ADDRESS((bs(new)-{0..3})>>4);
        END;
      ELSIF ch = 12c THEN
        IF pos>0 THEN adr^:=new END;
        INC(adr); dir:=" "; EXIT
      ELSIF ORD(ch) IN {15b,13b} THEN
        IF pos>0 THEN adr^:=new END;
        DEC(adr); dir:="-"; EXIT
      ELSIF ch = "/" THEN
        IF pos=0 THEN new:=x END; print("/");
        adr :=int(bs(new)-{31}); dir:=" "; EXIT
      ELSIF ch = "G" THEN
        print("\n"); start; RETURN
      ELSIF ch="L" THEN
        boot; EXIT
      ELSIF ch="Q" THEN
        boot; start
      ELSE
        pri:=FALSE;
      END;
      IF pri THEN
        IF pos>8 THEN pos:=8 END;
        IF pos#0 THEN print("\r%c%h=%h ",dir,adr,x);
          i:=pos; w:=new<<((8-pos)*4);
          WHILE i>0 DO
            w:=w<<4; print("%c",hex_dig[w MOD 16]); DEC(i)
          END;
        ELSE
          print("\r%c%h=%h ",dir,adr,x);
        END; print("" 33c "[J");
      END;
    END;
  END;
END monitor;

(**************************************************************)

PROCEDURE di; CODE cod.getm cod.li3 cod.bic cod.setm END di;

BEGIN
  di;
  MASK16_31:={16..31};
  MASK00_15:={00..15};
  F0000h:=ADDRESS( BASE + 0F0000h DIV 4 );
  init_tty;
  init_disks;
  monitor
END spo25boo.
