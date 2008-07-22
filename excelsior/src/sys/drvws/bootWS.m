MODULE bootWS; (*$T-$I-$N- Leo 22-Mar-90. (c) KRONOS *)

(* NOTE! WD micro booter reads only  7*512 = 3584 bytes *)
(* assign SCSI:=-1 & Scsi:=-1 to make booter for WD     *)

IMPORT       SYSTEM;
IMPORT  cod: defCodes;

TYPE WORD = SYSTEM.WORD;
  ADDRESS = SYSTEM.ADDRESS;


CONST
  (* to unselect drive from load sequence assign -1 to it;  *)
  (* the number determines the order of testing for boot;   *)
  (* (and don't forget to rename resulting booter!)         *)

  bootFD   = 0;
  bootWD   = 1;
  bootSCSI =-1;
  bootScsi =-1;

  done     =   0;
  error    =  -1;
  KB       = 256; (* words in 1KB *)
  RETRIES  =  32;

VAR
  TIMEOUT: INTEGER;

PROCEDURE move(t,f: ADDRESS; s: INTEGER); CODE cod.move END move;
PROCEDURE quit; CODE cod.quit END quit;

---------------------------- SCREEN ----------------------------
                            --------

CONST
  WHITE = 222h;
  RED   = 312h;
  _SP =  ARRAY OF INTEGER { 0 };
  _FD =  ARRAY OF INTEGER {
         0FC7Fh,1987Eh,19846h,19806h,19826h,1983Eh,1983Eh
        ,19826h,19806h,19806h,19806h,19806h,0FC0Fh};
  _WD =  ARRAY OF INTEGER {
         1FB33h,33333h,33333h,33333h,33333h,33333h,33333h
        ,33333h,33333h,33333h,33133h,331FEh,1F8CCh};
  _Scsi= ARRAY OF INTEGER {
         01C1C1Ch,23E623Eh,2232323h,0730373h,2030303h,2030303h,23E033Eh
        ,2600360h,2600360h,2670367h,2622362h,23E623Eh,71C1C1Ch};
  _SCSI= ARRAY OF INTEGER {
         71C1C1Ch,23E623Eh,2232323h,2730373h,2030303h,2030303h,23E033Eh
        ,2600360h,2600360h,2670367h,2622362h,23E623Eh,71C1C1Ch};
  _IO =  ARRAY OF INTEGER {
         0F80Fh,18D06h,18D06h,18C86h,18C86h,18C46h,18C46h
        ,18C26h,18C26h,18C16h,18C16h,18C06h,0F80Fh};
  _NODEV = ARRAY OF INTEGER {
         0E38Eh,11451h,10410h,08208h,04104h,04104h,00000h
        ,04104h,00000h,117CFh,11051h,113D1h,0A051h,047CFh};
  _NOMEM = ARRAY OF INTEGER {
         0E38Eh,11451h,10410h,08208h,04104h,04104h,00000h
        ,04104h,00000h,117E1h,1B06Bh,153E5h,11061h,117E1h};
  _NOSYS = ARRAY OF INTEGER {
         00391h,00453h,00455h,00455h,00459h,00391h,00000h
        ,1E45Eh,01441h,01281h,0E10Eh,10110h,10110h,0F10Fh};
  _BADSYS = ARRAY OF INTEGER {
         0F38Fh,11451h,1144Fh,117D1h,11451h,0F44Fh,00000h
        ,1E45Eh,01441h,01281h,0E10Eh,10110h,10110h,0F10Fh};
  _T80    = ARRAY OF INTEGER {
         0F03Ch,19866h,30CC3h,30CC3h,30CC3h,30C66h,30C3Ch
        ,30C66h,30CC3h,30CC3h,30CC3h,19866h,0F03Ch};


PROCEDURE erase;
  VAR a: ADDRESS;
BEGIN
  a:=1F8000h; a^:=0; move(a+1,a,128*KB-1);
END erase;

PROCEDURE color(c: INTEGER);
  CONST p_trans = ARRAY OF INTEGER
        {0Fh,07,0Bh,03,0Dh,05,09h,01,0Eh,06,0Ah,02,0Ch,04,08h,00};
  CONST BLACK = p_trans[0*4]+p_trans[0*4]*16+p_trans[0*4]*256;
  VAR i,r,g,b: INTEGER; a,l: ADDRESS;
BEGIN
  b:=c MOD 16; c:=c DIV 16;
  g:=c MOD 16; c:=c DIV 16;
  r:=c MOD 16; a:=1F0010h;
  c:=p_trans[r*4]+p_trans[g*4]*16+p_trans[b*4]*256;
  i:=TIMEOUT;     l:=1F0020h;
  REPEAT DEC(i) UNTIL (i=0) OR (BITSET(l^)*{0}#{});
  l:=1F0000h;  l^:=0;
  FOR i:=0 TO 15 DO
    IF ODD(i) THEN a^:=c ELSE a^:=BLACK END; INC(a);
  END
END color;

PROCEDURE print(c: INTEGER; VAL w: ARRAY OF WORD);
  VAR a: ADDRESS; i: INTEGER;
BEGIN
  a:=1F8000h+(350-17)*16;  color(c);
  FOR i:=0 TO HIGH(w)    DO a^:=w[i]; INC(a,16) END;
  FOR i:=HIGH(w)+1 TO 15 DO a^:=0;    INC(a,16) END
END print;

PROCEDURE dot(i: INTEGER);
  VAR a: POINTER TO BITSET;
BEGIN
  i:=i+i;
  a:=ADDRESS(1F8000h+350*16+i DIV 32); INCL(a^,i MOD 32)
END dot;

--------------------------- RESET WS ---------------------------
                           ----------

PROCEDURE reset_ws;
  VAR i: INTEGER;
    csr: ADDRESS;
BEGIN
  FOR i:=0 TO 3 DO  (* SIO *)
    csr:=ADDRESS(8200F1h+i*2);
    csr^:={};  csr^:={};  csr^:={};  csr^:={};  csr^:={6};
  END;
  csr :=ADDRESS(17E003h);  (* CM *)
  csr^:=0;
  csr :=ADDRESS(840014h);  (* FD *)
  csr^:={0};    csr^:={};
  csr^:={1};    csr^:={};
END reset_ws;

--------------------------- DISK SPEC --------------------------
                           -----------

TYPE READPROC = PROCEDURE (ADDRESS,INTEGER,INTEGER,VAR INTEGER);

VAR mountBUF: ADDRESS;

PROCEDURE mount(VAR res: INTEGER;
                   read: READPROC;
                  reset: PROC;      VAR trk,scs,hds,ssc,rsc: INTEGER);
  VAR i: INTEGER;
    ptr: POINTER TO ARRAY [0..31] OF CHAR;
BEGIN
  ptr:=mountBUF;
  i:=4;
  REPEAT
    read(ptr,0,32,res); DEC(i);
    IF res#done THEN reset END
  UNTIL (res=done) OR (i=0);
  IF res#done THEN RETURN END;
  IF (ptr^[8]="X") & (ptr^[9]="D") & (ptr^[10]="0") THEN
    trk:=ORD(ptr^[12])+ORD(ptr^[13])*256;
    scs:=ORD(ptr^[15])-ORD(ptr^[14])+1;
    hds:=ORD(ptr^[16]);
    ssc:=ORD(ptr^[17]);
    rsc:=ORD(ptr^[18])
  END
END mount;

------------------------------ FD ------------------------------
                              ----

MODULE FD;

IMPORT  cod, SYSTEM, ADDRESS, WORD, done, error, TIMEOUT
      , bootFD, mount, print, RED, _T80;

EXPORT QUALIFIED init, read, reset;

CONST UNIT = 0;

VAR
  SSC    : INTEGER;
  HEADS  : INTEGER;
  SECS   : INTEGER;
  SECSIZE: INTEGER;
  RESSEC : INTEGER;

CONST
  (* step rate *)
  _ms3  = {   };     _ms10 = {1  };
  _ms6  = {  0};     _ms15 = {1,0};

  (* seek comands *)
  _restore    = {      3    };
  _seek       = {    4,3,2  };
     _down    = {      3    };       (* head down        *)
     _chktrk  = {        2  };       (* check after seek *)

  (* i/o  comands *)
  _read       = { 7         };
     _side0   = {           };
     _side1   = {     3     };
     _delay   = {      2    };       (* 15ms delay before  i/o   *)
     _chkside = {       1   };       (* check side durring i/o   *)

  _reset      = { 7,6, 4    };       (* may be executed any time *)

  (* mode *)
  _off = { };
  _fd0 = {0};
  _fd1 = {1};
  _sd0 = { };
  _sd1 = {2};
  _mtr = {3};  (* motor on      *)
  _ssr = {4};  (* set step rate *)
  _FM  = {5};
  _MFM = { };
  _in5 = { };
  _in8 = {6};
  _ei  = {7};

TYPE DMA =
  RECORD
    adr0: INTEGER;  cnt0: INTEGER;
    adr1: INTEGER;  cnt1: INTEGER;
    adr2: INTEGER;  cnt2: INTEGER;
    adr3: INTEGER;  cnt3: INTEGER;
    csr : INTEGER;
  END;

VAR
  HOL    : ADDRESS;
  csr,slu: POINTER TO BITSET;
  cyl,rsc: POINTER TO INTEGER;
  dtr    : POINTER TO INTEGER;
  dma    : POINTER TO DMA;

  ctrk   : INTEGER;
  double : BOOLEAN;

PROCEDURE delay; END delay; (* it MUST be called after each csr^:= *)

PROCEDURE seek00(VAR r: INTEGER);
BEGIN
  IF bootFD>=0 THEN
    csr^:=_restore; delay;
    r:=TIMEOUT;
    REPEAT DEC(r) UNTIL (r=0) OR (csr^*{0}={});
    IF (r=0) OR (csr^*{0,2,7}#{2}) THEN r:=error; RETURN END;
    cyl^:=0; ctrk:=0; r:=done
  END
END seek00;

PROCEDURE move_head(trk: INTEGER; VAR r: INTEGER);
BEGIN
  IF bootFD>=0 THEN
    IF (trk=0) OR (ctrk<0) THEN seek00(r) END;
    IF trk=0 THEN RETURN END;
    IF double THEN
      cyl^:=ctrk*2;  dtr^:=trk*2; csr^:=_seek-_chktrk;  delay;
      REPEAT DEC(r) UNTIL (r=0) OR (csr^*{0}={});
      IF (r=0) OR (csr^*{0,7}#{}) THEN r:=error; RETURN END;
      ctrk:=trk
    END;
    cyl^:=ctrk;  dtr^:=trk;  csr^:=_seek;  delay;  r:=TIMEOUT;
    REPEAT DEC(r) UNTIL (r=0) OR (csr^*{0}={});
    IF (r=0) OR (csr^*{0,7}#{}) THEN r:=error; RETURN END;
    ctrk:=trk;  cyl^:=trk;  r:=done
  END
END move_head;

PROCEDURE read(buf: ADDRESS; blk,bytes: INTEGER; VAR res: INTEGER);

  PROCEDURE word(a: ADDRESS): WORD;
  CODE
    cod.copt cod.lsw0 cod.swap
    cod.copt cod.lsw0 cod.swap
    cod.copt cod.lsw0 cod.swap
             cod.lsw0
    cod.li8  cod.rol  cod.or
    cod.li8  cod.rol  cod.or
    cod.li8  cod.rol  cod.or
  END word;

  VAR i: INTEGER;       w: INTEGER;
    len: INTEGER;     end: ADDRESS;
    trk: INTEGER;     ptr: ADDRESS;
    sec: INTEGER;    side: INTEGER;
    spt: INTEGER;    size: INTEGER;     cmd: BITSET;

BEGIN
  IF bootFD>=0 THEN
    sec :=blk*(4096>>SSC)+RESSEC;
    trk :=sec DIV SECS;
    sec :=sec MOD SECS;
    side:=trk MOD HEADS;
    trk :=trk DIV HEADS;
    ptr :=buf;
    len :=(bytes+SECSIZE-1) DIV SECSIZE;
    move_head(trk,res);
    IF res#done THEN RETURN END;
    LOOP
      IF side=0 THEN slu^:=_mtr+_ssr+_sd0+{UNIT}; cmd:=_read+_side0
      ELSE           slu^:=_mtr+_ssr+_sd1+{UNIT}; cmd:=_read+_side1
      END;
      spt:=len;
      IF spt>SECS-sec THEN spt:=SECS-sec END;
      WITH dma^ DO (* fd_buf *)
        csr:=86h; adr0:=0;  adr0:=0;  cnt0:=0;  cnt0:=54h;  csr:=87h
      END;
      size:=spt<<SSC;
      WITH dma^ DO (* buf_cpu(0,spt<<SSC); *)
        csr:=85h;
        adr1:=0000;   adr1:=0000   DIV 100h;
        cnt1:=size;   cnt1:=size DIV 100h + 80h;  csr:=87h
      END;
  
      rsc^:=sec+1;
      csr^:=cmd;
      DEC(len,spt);
      REPEAT
        i:=TIMEOUT;
        REPEAT DEC(i) UNTIL (i=0) OR (csr^*{0}={});
        res:=INTEGER(csr^*{0..5,7});
        IF spt>1 THEN INC(rsc^); csr^:=cmd END;
        IF (i=0) OR (res#0) THEN res:=error; RETURN END;
        end:=ptr+SECSIZE DIV 4;
        REPEAT ptr^:=word(HOL); INC(ptr) UNTIL ptr=end;
        spt:=spt-1
      UNTIL spt=0;
      IF len=0 THEN RETURN END;
      sec:=0;
      INC(side);
      IF side=HEADS THEN side:=0; INC(trk); move_head(trk,res) END
    END
  END
END read;

PROCEDURE reset;
  VAR r: INTEGER;
BEGIN
  IF bootFD>=0 THEN csr^:=_reset; delay; seek00(r) END
END reset;

PROCEDURE test(): INTEGER;
  VAR t0,t1: INTEGER;
BEGIN
  csr^:=_restore;  delay;
  REPEAT UNTIL csr^*{2,0}={2};  cyl^:=0; ctrk:=0;
  cyl^:=ctrk;  dtr^:=79;  csr^:=_seek-_chktrk;  delay; t0:=0;
  REPEAT INC(t0) UNTIL csr^*{0}={};
  ctrk:=79;  cyl^:=79;  csr^:=_restore; delay; t1:=0;
  REPEAT INC(t1) UNTIL csr^*{2,0}={2}; cyl^:=0; ctrk:=0;
  IF t1<(t0+t0+t0) DIV 4 THEN RETURN 40 ELSE RETURN 80 END
END test;

PROCEDURE init(VAR res: INTEGER);
  VAR i,trkS,trkH: INTEGER;
BEGIN
  IF bootFD>=0 THEN
    SSC   :=10;
    HEADS := 2;
    SECS  := 5;
    RESSEC:= 0;
    SECSIZE:=INTEGER(1<<SSC);   double:=FALSE;
    dma:=ADDRESS(840000h);
    csr:=ADDRESS(840010h);
    cyl:=ADDRESS(840011h);
    rsc:=ADDRESS(840012h);
    dtr:=ADDRESS(840013h);
    slu:=ADDRESS(840014h);
    HOL:=ADDRESS(850000h);
    (* start_refresh; *)
    WITH dma^ DO
      csr :=000h;
      adr3:=000h;  adr3:=000h;
      cnt3:=0FFh;  cnt3:=0BFh;
      adr2:=000h;  adr2:=000h;
      cnt2:=0FFh;  cnt2:=0BFh;
      csr :=084h
    END;
    slu^:=_fd0; slu^:={};  csr^:={7,6,4};
    ctrk:=0;
    (* select & run motor *)
    slu^:=_ssr;     slu^:=_ssr+{UNIT};
    slu^:=_ssr+_mtr+{UNIT};
    csr^:=_reset+{0};
    i:=TIMEOUT;
    REPEAT DEC(i) UNTIL (i=0) OR (csr^*{0,7}={});
    IF i=0      THEN res:=error ELSE seek00(res) END;
    IF res#done THEN slu^:={UNIT}; slu^:={}; RETURN END;
    trkH:=test();
    mount(res,read,reset,trkS,SECS,HEADS,SSC,RESSEC);
    IF res#done THEN RETURN END;
    SECSIZE:=INTEGER(1<<SSC);
    IF trkS>trkH THEN print(RED,_T80);
      FOR i:=0 TO 50000 DO END; res:=error; RETURN
    END;
    double:=(trkS=40) & (trkH=80)
  END
END init;

END FD;

------------------------------ WD ------------------------------
                              ----
MODULE WD;

IMPORT  move, SYSTEM, ADDRESS, WORD, done, error, TIMEOUT, bootWD, mount;

EXPORT QUALIFIED init, read, reset;

VAR
  SECS   : INTEGER;
  HEADS  : INTEGER;
  RESSEC : INTEGER;

CONST
   UNIT    =  0 ;
  _clear   = 80h;
  _select  = 81h;
  _restore = 82h;
  _read    = 84h;

VAR
  kbf     : POINTER TO INTEGER; -- счетчик буферов драйвера
  cbf     : POINTER TO INTEGER; -- счетчик буферов контроллера
  csr     : ADDRESS;            -- регистр команд
  dad     : POINTER TO INTEGER; -- регистр дискового адреса
  err     : POINTER TO BITSET;  -- регистр ошибок

  dma     : ADDRESS;
  dma_secs: INTEGER;
  dma_cou : INTEGER;

PROCEDURE reset;
  VAR i: INTEGER;
BEGIN
  IF bootWD>=0 THEN
    csr^:=BITSET(_clear<<24)+BITSET(UNIT<<16);   i:=TIMEOUT;
    REPEAT DEC(i) UNTIL (INTEGER(csr^)>=0) OR (i=0);
    csr^:=BITSET(_restore<<24)+BITSET(UNIT<<16); i:=TIMEOUT;
    REPEAT DEC(i) UNTIL (INTEGER(csr^)>=0) OR (i=0)
  END
END reset;

PROCEDURE read(buf: ADDRESS; blk,bytes: INTEGER; VAR res: INTEGER);
  CONST READ_8_SECS = BITSET(_read>>8)+BITSET(UNIT<<16)+BITSET(8);
  VAR s,t,h,l,len: INTEGER;
BEGIN
  IF bootWD>=0 THEN
    l:=(bytes+511) DIV 512;
    s:=blk*8+RESSEC;
    t:=s DIV SECS;
    h:=t MOD HEADS;
    t:=t DIV HEADS;
    s:=s MOD SECS;
    res:=done;
    WHILE l>0 DO
      REPEAT UNTIL INTEGER(csr^)>=0;
      dma:=buf;   dma_secs:=8;  dma_cou:=0;
      cbf^:=0;    dad^:=t+h*10000h;
      kbf^:=0;    csr^:=READ_8_SECS+BITSET(s<<8);

      REPEAT
        IF (cbf^#dma_cou) OR (INTEGER(csr^)>=0) THEN
          move(dma,ADDRESS(900000h+dma_cou*128),128);
          INC(dma,128);
          DEC(dma_secs);
          dma_cou:=(dma_cou+1) MOD 7;   kbf^:=dma_cou
        END
      UNTIL dma_secs=0;

      IF INTEGER(csr^)<0  THEN res:=error  END;
      IF err^*{7}#{}      THEN res:=INTEGER(err^) END;
      IF res#0 THEN RETURN END;
      DEC(l,8);   INC(s,8);  INC(buf,1024);
      IF s>=SECS  THEN s:=s-SECS;  INC(h) END;
      IF h>=HEADS THEN h:=h-HEADS; INC(t) END
    END
  END
END read;

PROCEDURE init(VAR res: INTEGER);
  VAR i: INTEGER;
    stp: POINTER TO INTEGER;
BEGIN
  IF bootWD>=0 THEN
    SECS  :=16;
    HEADS :=6;
    RESSEC:=0;
    kbf:=ADDRESS(9003F6h);
    cbf:=ADDRESS(9003F7h);
    csr:=ADDRESS(9003FFh);
    dad:=ADDRESS(9003FEh);
    err:=ADDRESS(9003FDh);
    res:=done;
    i:=TIMEOUT;  csr^:=_clear>>8;
    REPEAT DEC(i) UNTIL (INTEGER(csr^)>=0) OR (i=0);
    IF i=0 THEN res:=error; RETURN END;
    stp:=ADDRESS(9003FBh); stp^:=02010FFFh;
    IF stp^=0 THEN res:=error; RETURN END;
    stp:=ADDRESS(9003FAh); stp^:=02010FFFh;
    i:=TIMEOUT;  csr^:=_select>>8 + UNIT<<16;
    REPEAT DEC(i) UNTIL (INTEGER(csr^)>=0) OR (i=0);
    IF i=0 THEN res:=error; RETURN END;
    mount(res,read,reset,i,SECS,HEADS,i,RESSEC)
    (* number of tracks and sector size ignored *)
  END
END init;

END WD;

----------------------------- SCSI -----------------------------
                             ------

MODULE SCSI;

IMPORT  move, SYSTEM, ADDRESS, WORD, done, error, TIMEOUT, bootSCSI, mount;

EXPORT QUALIFIED init, read, reset;

CONST
  scsiCSR = 800C00h;
  scsiDAT = 800800h;

  WrD   = 0;         -- RAM A6SCSI -->> DK data
  WrC   = 1;         -- Command    -->> DK command
  RdD   = 2;         -- DK data    -->> RAM A6SCSI
  RdC   = 3;         -- DK status  -->> status A6SCSI
  WrNop = 4;         -- Nop
  WrMSG = 5;         -- MSG A6SCSI -->> DK MSG
  RdNop = 6;         -- Nop
  RdMSG = 7;         -- DK MSG     -->> MSG A6SCSI

  nop   = RdNop;     -- Nop
  c     = 10h;
  _read = 08h;

CONST
  UNIT = 0;

VAR csr: POINTER TO ARRAY [0..1023] OF INTEGER;
 RESSEC: INTEGER;

PROCEDURE reset;
  VAR i: INTEGER;
BEGIN
  IF bootSCSI>=0 THEN
    csr^[nop*c]:=INTEGER({26});
    i:=255;
    REPEAT DEC(i) UNTIL i=128;
    csr^[nop*c]:=0h;
    REPEAT DEC(i) UNTIL i=0
  END
END reset;

PROCEDURE read(buf: ADDRESS; blk,bytes: INTEGER; VAR res: INTEGER);
  VAR i,s,len: INTEGER;
BEGIN
  IF bootSCSI>=0 THEN
    res:=done;
    s:=blk*8+RESSEC;
    len:=(bytes+511) DIV 512;
    WHILE len>0 DO
      i:=TIMEOUT;
      REPEAT UNTIL (i=0) OR (BITSET(csr^[0])*{29,30}={30});
      IF i=0 THEN res:=error; csr^[nop*c]:=0; RETURN END;
  
      csr^[WrD]  :=0FFFh;        -- Wr data
      csr^[WrC]  :=WrC*c*4-1;    -- Wr status
      csr^[RdD]  :=0FFFh;        -- Rd data
      csr^[RdC]  :=RdC*c*4-1;    -- Rd command
      csr^[WrMSG]:=WrMSG*c*4-1;  -- Wr MSG
      csr^[WrNop]:=nop*c*4-1;    -- NOP
      csr^[RdMSG]:=RdMSG*c*4-1;  -- Rd MSG
      csr^[RdNop]:=nop*c*4-1;    -- NOP
      csr^[WrC*c+0]:=_read;
      csr^[WrC*c+1]:=INTEGER({0..4}*BITSET(s DIV 10000h));
      csr^[WrC*c+2]:=INTEGER({0..7}*BITSET(s DIV 100h));
      csr^[WrC*c+3]:=INTEGER({0..7}*BITSET(s));
      csr^[WrC*c+4]:=8;
      csr^[WrC*c+5]:=0;
      csr^[RdC*c]  :=0FFh;
      csr^[WrMSG*c]:=000h;
      csr^[RdMSG*c]:=000h;
      csr^[nop*c]  :=000h;
  
      csr^[nop*c]:=INTEGER({24}+(({UNIT}/{0..7})<<16));
  
      i:=TIMEOUT;
      REPEAT UNTIL (i=0) OR (BITSET(csr^[0])*{29,30}={30});
      csr^[nop*c]:=0;

      IF (i=0) OR (BITSET(csr^[RdC*c])*{1..4}#{}) THEN res:=error; RETURN END;
  
      move(buf,scsiDAT,1024);
      INC(buf,1024); INC(s,8); DEC(len,8)
    END
  END
END read;

PROCEDURE init(VAR res: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF bootSCSI>=0 THEN
    RESSEC:=0;
    csr:=ADDRESS(scsiCSR);
    reset;
    IF csr^[0]=0 THEN res:=error; RETURN END;
    mount(res,read,reset,i,i,i,i,RESSEC)
  END
END init;

END SCSI;

----------------------------- Scsi -----------------------------
                             ------
MODULE Scsi;

IMPORT  cod, SYSTEM, ADDRESS, WORD, done, error, TIMEOUT, bootScsi, mount;

EXPORT QUALIFIED init, read, reset;

CONST
  CSR   = 801C00h;
  DAT   = 800C00h;

VAR
  csr  : POINTER TO BITSET;
  dat  : POINTER TO ARRAY [0..1023] OF INTEGER;
 RESSEC: INTEGER;

PROCEDURE _read(fr,to: ADDRESS);
  VAR i: INTEGER;
  PROCEDURE w;
  CODE
    cod.copt
    cod.llw5
    cod.copt cod.lsw0 cod.swap
    cod.copt cod.lsw1 cod.swap
    cod.copt cod.lsw2 cod.swap
    cod.copt cod.lsw3 cod.swap 4 cod.add
    cod.slw5
    8h cod.rol cod.or
    8h cod.rol cod.or
    8h cod.rol cod.or
    cod.ssw0 1 cod.add
  END w;
  PROCEDURE push(n: INTEGER); CODE END push;
  PROCEDURE pop(): INTEGER;   CODE END pop;
BEGIN
  IF bootScsi>=0 THEN
    push(to); i:=128;
    REPEAT w; DEC(i) UNTIL i=0; i:=pop()
  END
END _read;

PROCEDURE read(buf: ADDRESS; blk,bytes: INTEGER; VAR res: INTEGER);
  VAR i,l,s: INTEGER;
BEGIN
  IF bootScsi>=0 THEN
    res:=0; l:=(bytes+511) DIV 512;
    s:=blk*8+RESSEC;
    WHILE l>0 DO
      i:=TIMEOUT;
      REPEAT DEC(i) UNTIL (i=0) OR (csr^*{0..1}={0..1});
      IF i=0 THEN res:=error; RETURN END;
      dat^[0]:=1;         dat^[2]:=s DIV 10000h;
      dat^[1]:=08h;       dat^[3]:=s DIV 100h;
      dat^[4]:=s;         dat^[6]:=0;
      dat^[5]:=1;         dat^[7+512]:=0FFh;
      csr^:={1};
      i:=TIMEOUT;
      REPEAT DEC(i) UNTIL csr^*{0..1}={0..1};
      IF i=0 THEN res:=error; RETURN END;
      _read(SYSTEM.ADR(dat^[7]),buf); INC(buf,128);
      IF csr^*{3}#{}                   THEN res:=error; RETURN END;
      IF BITSET(dat^[7+512])*{1..4}#{} THEN res:=error; RETURN END;
      INC(s);  DEC(l)
    END
  END
END read;

PROCEDURE reset; END reset;

PROCEDURE init(VAR res: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF bootScsi>=0 THEN
    RESSEC:=0;
    res:=done;
    dat:=ADDRESS(DAT); csr:=ADDRESS(CSR); i:=TIMEOUT;
    REPEAT DEC(i) UNTIL (i=0) OR (csr^*{0..1}={0..1});
    IF i=0 THEN res:=error; RETURN END;
    mount(res,read,reset,i,i,i,i,RESSEC)
  END
END init;

END Scsi;

---------------------------- BOOTER ----------------------------
                            --------

VAR read: READPROC;
   reset: PROC;

PROCEDURE READ(buf: ADDRESS; blk,bytes: INTEGER; VAR res: INTEGER);
  VAR t: INTEGER;
BEGIN
  t:=RETRIES;
  LOOP
    res:=done;
    read(buf,blk,bytes,res);
    IF (res=done) OR (t<=0) THEN RETURN END;
    print(RED,_IO); reset; DEC(t); print(WHITE,_SP);
  END
END READ;

CONST system_base = 64*KB;

VAR top : ADDRESS;  (* :=MemoryTop() *)
    size: INTEGER;

PROCEDURE memory_top(): ADDRESS;
  CONST bank = 256*KB;  pattern=12345678h;
  PROCEDURE S_reg(): ADDRESS; CODE 0 cod.alloc END S_reg;
  VAR a: ADDRESS;
BEGIN
  a:=S_reg(); move(a,0,bank-a); -- filling first bank
  a:=bank;
  LOOP
    a^:=pattern;
    IF INTEGER(a^)#pattern THEN EXIT END;
    move(a,0,bank); a:=a+bank;
    IF a>=100000h          THEN EXIT END;
  END;
  RETURN a-1
END memory_top;

PROCEDURE read_system;

  CONST long={2};

  VAR   i: INTEGER;
      adr: ADDRESS;     buf: ADDRESS;
      lim: ADDRESS;     bno: INTEGER;
      ino: ADDRESS;     ref: ADDRESS;
      eof: INTEGER;     res: INTEGER;
     mode: POINTER TO BITSET;
     link: POINTER TO INTEGER;
BEGIN
  ref:=system_base-4*KB;
  ino:=system_base-4*KB*2;
  READ(ino,2,128,res);
  IF res#0 THEN print(RED,_IO); quit END;
  ino :=ino+16;
  adr :=ino+10;
  eof :=adr^;   eof:=(eof+4095) DIV 4096;
  link:=ino+9;
  mode:=ino+8;
  IF link^<=0 THEN print(RED,_NOSYS); quit END;
  IF long*mode^={} THEN
    IF eof>8 THEN print(RED,_BADSYS); quit END;
    move(ref,ino,eof)
  ELSE
    IF INTEGER(ino^)<=0 THEN print(RED,_BADSYS); quit END;
    READ(ref,ino^,4096,res);
    IF res#0 THEN print(RED,_IO); quit END
  END;
  buf:=system_base;
  lim:=top-4*KB;
  bno:=ref^;
  size:=0;
  i:=0;
  WHILE eof>0 DO
    IF buf>=lim THEN print(RED,_NOMEM);  quit END;
    IF bno<=0   THEN print(RED,_BADSYS); quit END;
    READ(buf,bno,4096,res);
    IF res#0 THEN
      print(RED,_IO);  quit
    ELSE
      INC(buf ,1024);  INC(ref); bno:=ref^;
      INC(size,1024);  DEC(eof); dot(i); INC(i)
    END
  END
END read_system;

TYPE process =
     POINTER TO
     RECORD
       G: ADDRESS;   L: ADDRESS;
      PC: INTEGER;   M: BITSET;
       S: ADDRESS;   H: ADDRESS;
       T: INTEGER;
     END;

PROCEDURE transfer(VAR f,t: process); CODE cod.tra END transfer;

PROCEDURE start_system;
  VAR p: process;
    adr: ADDRESS;
BEGIN
  top:=memory_top();
  read_system;

  adr:=system_base+88h; adr^:=top;

  adr:=top-KB;
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
  adr :=p^.G^;
  adr^:=cod.move+cod.tra*100h+cod.quit*10000h;
  transfer(p,p);
END start_system;

VAR i,res: INTEGER;

BEGIN
  TIMEOUT:=20000;
  mountBUF:=system_base-8*KB*2;
  color(WHITE);
  erase;
  i:=0;
  LOOP
    reset_ws;
    IF bootFD=i THEN
      FD.init(res);
      IF res=done THEN
        print(WHITE,_FD); read:=FD.read; reset:=FD.reset; start_system
      END
    END;
    IF bootWD=i THEN
      WD.init(res);
      IF res=done THEN
        print(WHITE,_WD); read:=WD.read; reset:=WD.reset; start_system
      END
    END;
    IF bootSCSI=i THEN
      SCSI.init(res);
      IF res=done THEN
        print(WHITE,_SCSI); read:=SCSI.read; reset:=SCSI.reset; start_system
      END
    END;
    IF bootScsi=i THEN
      Scsi.init(res);
      IF res=done THEN
        print(WHITE,_Scsi); read:=Scsi.read; reset:=Scsi.reset; start_system
      END
    END;
    i:=(i+1) MOD 8;
    print(RED,_NODEV)
  END
END bootWS.



%h {      02,03,04,05,                  12,13,14,15      } = 0F03Ch   1
%h {   01,02,      05,06,            11,12,      15,16   } = 19866h   2
%h {00,01,            06,07,      10,11,            16,17} = 30CC3h   3
%h {00,01,            06,07,      10,11,            16,17} = 30CC3h   5
%h {00,01,            06,07,      10,11,            16,17} = 30CC3h   6
%h {   01,02,      05,06,         10,11,            16,17} = 30C66h   7
%h {      02,03,04,05,            10,11,            16,17} = 30C3Ch   8
%h {   01,02,      05,06,         10,11,            16,17} = 30C66h   9
%h {00,01,            06,07,      10,11,            16,17} = 30CC3h   0
%h {00,01,            06,07,      10,11,            16,17} = 30CC3h   2
%h {00,01,            06,07,      10,11,            16,17} = 30CC3h   3
%h {   01,02,      05,06,            11,12,      15,16   } = 19866h   4
%h {      02,03,04,05,                  12,13,14,15      } = 0F03Ch   5
