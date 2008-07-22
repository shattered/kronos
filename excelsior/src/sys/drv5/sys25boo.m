MODULE sys25boo; (*$T-$N-$I- Leo 20-Dec-89. (c) KRONOS *)

IMPORT  cod: defCodes;
IMPORT       SYSTEM;

TYPE ADDRESS = SYSTEM.ADDRESS;
     WORD    = SYSTEM.WORD;

CONST system_base = 64*256;

PROCEDURE move(to,from: ADDRESS; words: INTEGER); CODE cod.move END move;
PROCEDURE quit; CODE cod.quit END quit;
PROCEDURE transfer(VAR from,to: ADDRESS); CODE cod.tra END transfer;

----------------------------  TTY   ----------------------------

CONST BASE=80000h;
      HALF=10000h;

VAR MASK16_31: BITSET; (* :={16..31} *)
    MASK00_15: BITSET; (* :={00..15} *)
       F0000h: POINTER TO ARRAY [0..0FFFFh] OF CHAR;

VAR
    put_in  : INTEGER;
    put_end : INTEGER;
    put_beg : INTEGER;
    out_size: INTEGER;
    outFLAG : INTEGER; (* relative 0    *)
       wBCB : POINTER TO ARRAY [0..0FFh] OF INTEGER;
        BCB : POINTER TO ARRAY [0..0FFh] OF CHAR;

PROCEDURE tty_wait(flag: INTEGER);
CODE cod.copt cod.trb cod.jbsc 04 cod.drop END tty_wait;

PROCEDURE puts(VAL s: ARRAY OF CHAR; VAR Pos,Len: INTEGER);
  VAR pos,len,new,put_out: INTEGER; ch,nextch: CHAR;
BEGIN
  pos:=Pos;     len:=Len;
  tty_wait(outFLAG);
    put_out:=INTEGER( BITSET(wBCB^[4])*MASK00_15 );
  BCB^[12]:=1c;
  nextch:=s[pos];
  LOOP
    ch:=nextch;
    IF (ch=0c) OR (len=0) THEN len:=0; EXIT END;
    IF put_in=put_end     THEN new:=put_beg ELSE new:=put_in+1 END;
    IF new=put_out        THEN EXIT END;
    INC(pos);  DEC(len);       nextch:=s[pos];
    F0000h^[put_in]:=ch;       put_in:=new;
  END;
  Pos:=pos;     Len:=len;
  tty_wait(outFLAG);
  wBCB^[3]:=INTEGER( BITSET(wBCB^[3])-MASK16_31+BITSET(put_in<<16)+{0} );
END puts;

PROCEDURE write_str(VAL s: ARRAY OF CHAR);
  VAR i,l: INTEGER;
BEGIN
  i:=0; l:=HIGH(s);
  REPEAT puts(s,i,l) UNTIL l=0;
END write_str;

PROCEDURE write_int(i: INTEGER; n: INTEGER);

  VAR str: ARRAY [0..15] OF CHAR; s: INTEGER;

  PROCEDURE putDig(i: INTEGER; n: INTEGER);
    VAR d: INTEGER;
  BEGIN
    d:=i MOD 10; i:=i DIV 10; DEC(n);
    IF i=0 THEN
      WHILE n>0 DO str[s]:=' '; INC(s); DEC(n) END
    ELSE
      putDig(i,n)
    END;
    str[s]:=CHAR(ORD('0')+d); INC(s);
  END putDig;

BEGIN s:=0;
  IF n<=0 THEN n:=1 END;
  IF i<0 THEN str[s]:='-'; INC(s); DEC(n); i:=ABS(i) END;
  putDig(i,n);
  str[s]:=0c; write_str(str);
END write_int;

PROCEDURE init_tty;
  VAR adr: ADDRESS;
     port: INTEGER;
BEGIN
  MASK16_31:={16..31};
  MASK00_15:={00..15};

  port:=9;  (* VDU Consol       *)
  adr:=BASE+(0F8010h+port*4) DIV 4;
  adr:=INTEGER(adr^) MOD HALF;

   BCB:=ADDRESS( F0000h ) + adr DIV 4;
  wBCB:=ADDRESS(BCB);
  outFLAG :=( INTEGER(BCB)*4 )+12;

  put_in  :=INTEGER(wBCB^[3]>>16) MOD HALF; (* 14,15 bytes BCB *)
  put_end :=INTEGER(wBCB^[5]>>16) MOD HALF; (* 22,23 bytes BCB *)
  put_beg :=        wBCB^[5] MOD HALF;      (* 20,21 bytes BCB *)
  out_size:=put_end-put_beg+1;
END init_tty;


----------------------------  DISK  ----------------------------

TYPE
 bs      = BITSET;
 int     = INTEGER;
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
  REPEAT fdc0^:=ABORT;  HEI^:=5;  wait UNTIL bs(fdc0^)*{8..15}={};
  fdc0^:=FLUSH; HEI^:=5; wait;
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
      fdc1^:=bs(trk DIV 256 + hed*16) + bs(sec<<8) + bs(size<<18);
      fdc0^:= READ  +   bs(r.drn<<16) + bs(trk MOD 256)>>8;
      HEI^ :=5;         wait;
      IF bs(fdc0^)*{8..15}#{} THEN r.res:=int(errorw()); abort; RETURN END;
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
      IF bs(fdc0^)*{8..15}#{} THEN r.res:=int(errorf()); abort; RETURN END;
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
    IF res#ok THEN
      DMA^ :=(BUFFER-BASE)*4;
      fdc1^:=bs(ORD(drn<4)<<8) + bs(40h<<16);
      fdc0^:= READ + bs(drn<<16);
      HEI^ :=5;   exec(drn,res);
    END;
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
      ssc  :=unpack(ptr^,3Dh,1);   res_sec:=unpack(ptr^,3Eh,2);
      IF bad THEN ports[drn]:=sav; set_spec(drn,ign); RETURN END
    ELSE
      RETURN
    END;
    secsize:=1<<ssc;
    set_spec(drn,res)
  END
END mount;

--VAR msg: ARRAY [0..3] OF CHAR;

PROCEDURE seek00;
  VAR res: INTEGER;
BEGIN
  DMA^ := (BUFFER-BASE)*4;
  fdc1^:= bs(ORD(unit<4)<<8);
  fdc0^:= SEEK + bs(unit<<16);
  HEI^ := 5;   exec(unit,res)
END seek00;

PROCEDURE read(bno: INTEGER; buf: ADDRESS; len: INTEGER; VAR res: INTEGER);
  VAR r: REQUEST;
BEGIN
--  write_str(msg);
--  IF msg[0]#'o' THEN msg:="o" 15c 0c ELSE msg:="i" 15c 0c END;
  WITH ports[unit] DO
    r.drn:=unit;            r.len:=len<<(12-ssc);
    r.ofs:=bno<<(12-ssc);   r.res:=ok;
    r.buf:=buf
  END;
  IF unit<4 THEN readf(r) ELSE readw(r) END;
  res:=r.res;
  IF res=ok THEN RETURN END;
--  write_str("R" 15c);
  IF unit>=4 THEN readw(r); res:=r.res END
END read;

PROCEDURE init_disks;
  VAR r: INTEGER;
    adr: ADDRESS;
BEGIN
  BUFFER:=ADDRESS(0D4000h DIV 4 + BASE);
  HEI   :=ADDRESS(0D0000h DIV 4 + BASE);

  adr :=(0F8010h+1*4) DIV 4 + BASE;
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
    get_spec(4,r);
    secs := 16;     heads  := 4;
    cyls :=306;     res_sec:= secs*heads;
    ssc  :=  9;     secsize:= 1<<ssc;
    set_spec(4,r)
  END;
  LOOP
    unit:=0;
    mount(unit,r);
    IF r=0 THEN RETURN END;
    unit:=4;
    mount(unit,r);
    IF r=0 THEN RETURN END
  END
END init_disks;

---------------------------- BOOTER ----------------------------
                            --------

VAR top : ADDRESS;  (* :=MemoryTop() *)
    size: INTEGER;

PROCEDURE memory_top(): ADDRESS;
  VAR S: WORD;
    A,B: ADDRESS;
BEGIN
  A:=0; S:=A^;
  B:=20000h;
  LOOP
    IF B>=80000h THEN EXIT END;
    B^:=0;
    IF A^#S THEN EXIT END;
    INC(B,20000h)
  END;
  A^:=S;
  DEC(B);
  RETURN B
END memory_top;

PROCEDURE read_system;
  CONST long={2};
  VAR adr: ADDRESS;     buf: ADDRESS;   err: INTEGER;
      lim: ADDRESS;     bno: INTEGER;   tai: INTEGER;
      ino: ADDRESS;     ref: ADDRESS;   len: INTEGER;
     mode: POINTER TO BITSET;
     link: POINTER TO INTEGER;
      eof: INTEGER;

  PROCEDURE bad;
  BEGIN write_str("illegal SYSTEM.BOOT file" 12c 15c); quit END bad;

BEGIN
--msg:="i" 15c 0c;
  ino:=system_base-1024*2;
  ref:=system_base-1024*1;
  REPEAT read(2,ino,1,err) UNTIL err=0;
  ino :=ino+16;
  adr :=ino+10;
  eof :=adr^;   eof:=(eof+4095) DIV 4096;
  link:=ino+9;
  mode:=ino+8;
  IF link^<=0 THEN bad END;
  IF long*mode^={} THEN
    IF eof>8 THEN bad END;
    move(ref,ino,eof)
  ELSE
    IF INTEGER(ino^)<=0 THEN bad END;
    REPEAT read(ino^,ref,1,err) UNTIL err=0
  END;
  buf:=system_base;
  lim:=top-256;
  bno:=ref^;     size:=0;
  WHILE eof>0 DO
    len:=1; adr:=ref+1; tai:=eof-1;
    WHILE (tai>0) & (adr^=bno+len) & (len<15) DO (* 16*4K = 64K *)
      INC(len); INC(adr); DEC(tai)
    END;
    IF buf+len*1024>=lim THEN
      write_str("too small memory" 12c 15c); quit
    END;
    IF bno<=0 THEN bad END;
    read(bno,buf,len,err);
    IF err=0 THEN
      INC(buf,len*1024);  INC(ref,len); bno:=ref^;
      INC(size,len*1024); DEC(eof,len)
    END
  END;
--  write_str(" " 15c)
END read_system;

TYPE process = POINTER TO
     RECORD
       G: ADDRESS;   L: ADDRESS;
      PC: INTEGER;   M: BITSET;
       S: ADDRESS;   H: ADDRESS;
       T: INTEGER;
     END;

PROCEDURE start_system;
  TYPE c = BITSET;
  VAR p: process;
    adr: ADDRESS;
BEGIN
  adr:=system_base+88h; adr^:=top;
  adr:=top-256;
  p:=adr;
  p^.G:=adr+08; p^.L :=adr+10;
  p^.M:={};     p^.PC:=0;
  p^.H:=top;    p^.S :=adr+20;
  p^.T:=0;      p^.G^:=adr+09;

  (* stack *)
  adr  :=p^.S;
  DEC(adr);     adr^:=5;
  DEC(adr);     adr^:=0;
  DEC(adr);     adr^:=1;
  DEC(adr);     adr^:=0;
  DEC(adr);     adr^:=system_base;
  DEC(adr);     adr^:=size;
  (* code *)
  adr:=p^.G^;
  adr^:=c(cod.move)+c(cod.tra<<8)+c(cod.quit<<16);
  transfer(p,p)
END start_system;

BEGIN
  F0000h:=ADDRESS( BASE + 0F0000h DIV 4 );
  init_tty;
  write_str("" 233c "H" 33c "?2;0T");
  init_disks;
  write_str("" 233c "J");
  IF unit<4 THEN
    write_str(""15c 12c"fd"); write_int(unit,0);   write_str("  ")
  ELSE
    write_str(""15c 12c"wd"); write_int(unit-4,0); write_str("  ")
  END;
  top:=memory_top();
  write_int((top+255) DIV 256,0); write_str("KB" 15c 12c);
  read_system;
  start_system
END sys25boo.
