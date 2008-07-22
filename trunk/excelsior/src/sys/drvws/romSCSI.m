MODULE romSCSI; (*$T-$N-$I-  Igo 06-Nov-90. (c) KRONOS *)

(* booter from Fil SCSI bus adapter for workstation *)

IMPORT  cod: defCodes;
IMPORT  sys: SYSTEM;

TYPE ADDRESS = sys.ADDRESS;
        WORD = sys.WORD;

PROCEDURE move(to,from: ADDRESS; words: INTEGER); CODE cod.move END move;
PROCEDURE transfer(VAR from,to: ADDRESS); CODE cod.tra   END transfer;

CONST   err  = -1;   ok = 0;    FD=TRUE;

-- set FD=TRUE to try FD before SCSI

--------------------------SCSI DISK-------------------------------------------

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

  _geterror  = 03h;
  _read      = 08h;       _write = 0Ah;
  _rezero    = 01h;       _seek  = 0Bh;

CONST
   unit = (* use bootWD *) 0;

VAR
  scsr        : POINTER TO ARRAY [0..1023] OF INTEGER;

PROCEDURE InitBCB();
BEGIN
  scsr^[WrD]  :=0FFFh;        -- Wr data
  scsr^[WrC]  :=WrC*c*4-1;    -- Wr status
  scsr^[RdD]  :=0FFFh;        -- Rd data
  scsr^[RdC]  :=RdC*c*4-1;    -- Rd command
  scsr^[WrMSG]:=WrMSG*c*4-1;  -- Wr MSG
  scsr^[WrNop]:=nop*c*4-1;    -- NOP
  scsr^[RdMSG]:=RdMSG*c*4-1;  -- Rd MSG
  scsr^[RdNop]:=nop*c*4-1;    -- NOP
  scsr^[WrC*c+0]:=_read;
  scsr^[WrC*c+1]:=0;
  scsr^[WrC*c+2]:=0;
  scsr^[WrC*c+3]:=0;
  scsr^[WrC*c+4]:=8;
  scsr^[WrC*c+5]:=0;

  scsr^[RdC*c]  :=0FFh;
  scsr^[WrMSG*c]:=0h;
  scsr^[RdMSG*c]:=0h;
  scsr^[nop*c]:=0h;
END InitBCB;

PROCEDURE not_ready(): BOOLEAN;
  VAR t :INTEGER;
BEGIN
  t:=100000;
  REPEAT
    IF BITSET(scsr^[0])*{29,30}={30} THEN RETURN FALSE END;
    t:=t-1
  UNTIL t<=0;
  scsr^[nop*c]:=0;
  RETURN TRUE
END not_ready;

PROCEDURE reset_scsi;
  VAR i:INTEGER;
BEGIN
  scsr^[nop*c]:=INTEGER({26});
  FOR i:=0 TO 10 DO END;
  scsr^[nop*c]:=0h;
  FOR i:=0 TO 1000 DO END;
END reset_scsi;

PROCEDURE select(): BOOLEAN;
  VAR res: INTEGER;
BEGIN
  IF not_ready() THEN RETURN FALSE END;
  InitBCB;
  scsr^[nop*c]:=INTEGER({24}+(({unit}/{0..7})<<16));
  IF not_ready() THEN RETURN FALSE END;
  IF BITSET(scsr^[0])*{29,30}={30} THEN RETURN TRUE END;
  scsr^[nop*c]:=0;
  RETURN FALSE
END select;

CONST system_base = 64*256;

PROCEDURE read_scsi(VAR ERR: INTEGER);
  VAR try: INTEGER;
BEGIN
  try:=7;
  LOOP
    IF select() THEN ERR:=ok; EXIT END;
    IF try MOD 4 = 0 THEN reset_scsi; IF select() THEN END; END;
    IF try=0 THEN ERR:=err; RETURN END; DEC(try);
  END;
  move(system_base,scsiDAT,1024);
  RETURN
END read_scsi;

--------------------------FLOPPY DISK--------------------------------------

CONST  RETRIES = 8;

  (* DEFAULT DRIVE SPEC *)
  SSC     = 10;
  SECS    =  5;
  SECSIZE = INTEGER(1<<SSC);

CONST
  (* step rate *)
  _ms3  = {   };     _ms10 = {1  };
  _ms6  = {  0};     _ms15 = {1,0};

  (* seek comands *)
  _restore    = {      3    };
--_seek       = {    4,3,2  };
  _step_fwd   = {6,  4,3,1,0};
     _down    = {      3    };       (* head down        *)
     _chktrk  = {        2  };       (* check after seek *)

  (* i/o  comands *)
  _flread     = { 7         };
     _long    = {    4      };
     _side0   = {           };
     _side1   = {     3     };
     _delay   = {      2    };       (* 15ms delay before  i/o  *)
     _chkside = {       1   };       (* check side durring i/o  *)

  _reset      = { 7,6, 4    };      (* may be executed any time *)

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
  BUF    : ADDRESS;
  csr,slu: POINTER TO BITSET;
  cyl,rsc: POINTER TO INTEGER;
  dma    : POINTER TO DMA;

PROCEDURE seek00(): INTEGER;
  VAR r: INTEGER;
BEGIN
  slu^:=_ssr;
  slu^:=_ssr+_fd0;
  slu^:=_ssr+_fd0+_mtr;
  csr^:=_restore;
  FOR r:=0 TO 99 DO END; (* delay *)
  r:=100000;
  REPEAT DEC(r) UNTIL (r=0) OR (csr^*{0}={});
  IF r=0         THEN RETURN err          END;
  IF csr^*{2}#{} THEN cyl^:=0; RETURN ok  END;
  RETURN err
END seek00;

PROCEDURE buf_cpu(no,size: INTEGER);
BEGIN
  WITH dma^ DO
    csr:=85h;
    adr1:=no;     adr1:=no   DIV 100h;
    cnt1:=size;   cnt1:=size DIV 100h + 80h;
    csr:=87h
  END
END buf_cpu;

PROCEDURE fd_buf;
BEGIN
  WITH dma^ DO
    csr :=86h;
    adr0:=00;  adr0:=00h;
    cnt0:=00;  cnt0:=54h;
    csr :=87h
  END
END fd_buf;

PROCEDURE start_refresh;
BEGIN
  WITH dma^ DO
    csr :=000h;
    adr3:=000h;  adr3:=000h;
    cnt3:=0FFh;  cnt3:=0BFh;
    adr2:=000h;  adr2:=000h;
    cnt2:=0FFh;  cnt2:=0BFh;
    csr :=084h
  END
END start_refresh;

PROCEDURE _fread(VAR ERR: INTEGER);

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
    ptr: ADDRESS;     cmd: BITSET;

BEGIN
  ptr :=system_base;
  len :=SECS;
  ERR :=seek00();
  IF ERR#ok THEN RETURN END;
  slu^:=_mtr+_ssr+_sd0+_fd0; cmd:=_flread+_side0;
  fd_buf;
  buf_cpu(0,len<<SSC);
  rsc^:=1;
  csr^:=cmd;
  FOR i:=0 TO 99 DO END;
  REPEAT
    i:=50000;
    REPEAT DEC(i) UNTIL (i=0) OR (csr^*{0}={});
    ERR:=INTEGER(csr^*{0..5,7});
    IF len>1 THEN INC(rsc^); csr^:=cmd END;
    IF (i=0) OR (ERR#0) THEN ERR:=err; RETURN END;
    end:=ptr+SECSIZE DIV 4;
    REPEAT ptr^:=word(BUF); INC(ptr) UNTIL ptr=end;
    len:=len-1
  UNTIL len=0
END _fread;

PROCEDURE read_floppy(VAR ERR: INTEGER);
  VAR t,i: INTEGER;
BEGIN
  t:=RETRIES;
  LOOP
    ERR:=ok;
    _fread(ERR);
    IF (ERR=ok) OR (t<=0) THEN RETURN END;
    csr^:=_reset; DEC(t);
    FOR i:=0 TO 25000 DO END;
  END
END read_floppy;

PROCEDURE init_fd(VAR error: INTEGER);
  VAR i: INTEGER;
BEGIN
  dma:=ADDRESS(840000h);
  csr:=ADDRESS(840010h);
  cyl:=ADDRESS(840011h);
  rsc:=ADDRESS(840012h);
  slu:=ADDRESS(840014h);
  BUF:=ADDRESS(850000h);
  start_refresh;
  slu^:=_fd0; slu^:={};
  slu^:=_fd1; slu^:={};
  csr^:={7,6,4};
  FOR i:=0 TO 99 DO END; (* delay *)
  (* select & run motor *)
  slu^:=_ssr;
  slu^:=_ssr+_fd0;
  slu^:=_ssr+_fd0+_mtr;
  csr^:=_reset+{0};
  FOR i:=0 TO 99 DO END; (* delay *)
  i:=100000;
  REPEAT DEC(i) UNTIL (i=0) OR (csr^*{0,7}={});
  IF i=0 THEN error:=err ELSE error:=ok END
END init_fd;

PROCEDURE fd_off;
  VAR i: INTEGER;
BEGIN
  slu^:=_fd0; slu^:={};
  slu^:=_fd1; slu^:={};
  csr^:={7,6,4};
  FOR i:=0 TO 99 DO END (* delay *)
END fd_off;

---------------------------- BOOTER ----------------------------
                            --------

PROCEDURE read_booter;
  VAR ERR: INTEGER;
BEGIN
  LOOP
    IF FD THEN
      init_fd(ERR);
      IF ERR=ok THEN
        read_floppy(ERR);
        IF ERR=ok THEN fd_off; RETURN END
      END
    END;
    fd_off;
    read_scsi(ERR);
    read_scsi(ERR);
    IF ERR=ok THEN RETURN END;
  END
END read_booter;

TYPE process = POINTER TO
     RECORD
       G: ADDRESS;   L: ADDRESS;
      PC: INTEGER;   M: BITSET;
       S: ADDRESS;   H: ADDRESS;
       T: INTEGER;
     END;

PROCEDURE start_booter;
  TYPE c = BITSET;
  VAR adr: ADDRESS;
      dev: POINTER TO ARRAY [0..3] OF CHAR;
        p: process;
BEGIN
  adr:=system_base-256;
  p:=adr;
  p^.G:=adr+08; p^.L :=adr+10;
  p^.M:={};     p^.PC:=0;
  p^.H:=system_base-1;    p^.S :=adr+20;
  p^.T:=0;      p^.G^:=adr+09;

  (* stack *)
  adr  :=p^.S;
  DEC(adr);     adr^:=5;
  DEC(adr);     adr^:=0;
  DEC(adr);     adr^:=1;
  DEC(adr);     adr^:=0;
  DEC(adr);     adr^:=system_base;
  DEC(adr);     adr^:=1024;
  (* code *)
  adr:=p^.G^;
  adr^:=c(cod.move)+c(cod.tra<<8)+c(cod.quit<<16);
  transfer(p,p)
END start_booter;

VAR i: INTEGER;

BEGIN
  FOR i:=0 TO 20000 DO END;
  scsr:=ADDRESS(scsiCSR);
  read_booter;
  start_booter;
END romSCSI.
