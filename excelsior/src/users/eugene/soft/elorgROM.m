MODULE elorgROM; (*$T-$N-$I- Leo 20-Dec-89. (c) KRONOS *)
                       (* Igo 06-Nov-90. (c) KRONOS *)

(* booter from Fil SCSI bus adapter for workstation *)

IMPORT  cod: mCodeMnem;
IMPORT  sys: SYSTEM;

TYPE ADDRESS = sys.ADDRESS;
        WORD = sys.WORD;

PROCEDURE move(to,from: ADDRESS; words: INTEGER); CODE cod.move END move;
PROCEDURE quit;                           CODE cod.quit  END quit;
PROCEDURE transfer(VAR from,to: ADDRESS); CODE cod.tra   END transfer;
PROCEDURE S_reg(): ADDRESS;               CODE cod.li0   cod.alloc END S_reg;

 --------------------------DISK-------------------------------------------

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
  csr        : POINTER TO ARRAY [0..1023] OF INTEGER;

PROCEDURE InitBCB();
BEGIN
  csr^[WrD]  :=0FFFh;        -- Wr data
  csr^[WrC]  :=WrC*c*4-1;    -- Wr status
  csr^[RdD]  :=0FFFh;        -- Rd data
  csr^[RdC]  :=RdC*c*4-1;    -- Rd command
  csr^[WrMSG]:=WrMSG*c*4-1;  -- Wr MSG
  csr^[WrNop]:=nop*c*4-1;    -- NOP
  csr^[RdMSG]:=RdMSG*c*4-1;  -- Rd MSG
  csr^[RdNop]:=nop*c*4-1;    -- NOP
  csr^[WrC*c+0]:=_read;
  csr^[WrC*c+1]:=0;
  csr^[WrC*c+2]:=0;
  csr^[WrC*c+3]:=0;
  csr^[WrC*c+4]:=8;
  csr^[WrC*c+5]:=0;

  csr^[RdC*c]  :=0FFh;
  csr^[WrMSG*c]:=0h;
  csr^[RdMSG*c]:=0h;
  csr^[nop*c]:=0h;
END InitBCB;

PROCEDURE not_ready(): BOOLEAN;
  VAR t :INTEGER;
BEGIN
  t:=100000;
  REPEAT
    IF BITSET(csr^[0])*{29,30}={30} THEN RETURN FALSE END;
    t:=t-1
  UNTIL t<=0;
  csr^[nop*c]:=0;
  RETURN TRUE
END not_ready;

PROCEDURE reset_scsi;
  VAR i:INTEGER;
BEGIN
  csr^[nop*c]:=INTEGER({26});
  FOR i:=0 TO 10 DO END;
  csr^[nop*c]:=0h;
  FOR i:=0 TO 1000 DO END;
END reset_scsi;

PROCEDURE select(): BOOLEAN;
  VAR res: INTEGER;
BEGIN
  IF not_ready() THEN RETURN FALSE END;
  InitBCB;
  csr^[nop*c]:=INTEGER({24}+(({unit}/{0..7})<<16));
  IF not_ready() THEN RETURN FALSE END;
  IF BITSET(csr^[0])*{29,30}={30} THEN RETURN TRUE END;
  csr^[nop*c]:=0;
  RETURN FALSE
END select;

CONST system_base = 64*256;
               KB = 256; (* words in 1KB *)

PROCEDURE read_block(VAR err: INTEGER);
  VAR try: INTEGER;
BEGIN err:=0;
  try:=8;
  LOOP
    IF select() THEN err:=0; EXIT END;
    IF try MOD 4 = 0 THEN reset_scsi END;
    IF try=0 THEN err:=1; RETURN END; DEC(try);
  END;
  move(system_base,scsiDAT,1024);
  RETURN
END read_block;

---------------------------- BOOTER ----------------------------
                            --------

PROCEDURE read_booter;
  VAR err: INTEGER;
BEGIN
  REPEAT read_block(err) UNTIL err=0
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
  transfer(p,p);
END start_booter;

VAR i: INTEGER;

BEGIN
  FOR i:=0 TO 20000 DO END;
  csr:=ADDRESS(scsiCSR);
  reset_scsi;
  read_booter; read_booter;
  start_booter;
END elorgROM.
