MODULE DK6qSCSI; (* Igo 06-Aug-87. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  req: defRequest;
IMPORT  err: defErrors;
IMPORT  cod: defCodes;
IMPORT   os: osKernel;
IMPORT  fs : osFiles;
IMPORT  env: tskEnv;

TYPE WORD = SYSTEM.WORD;
  ADDRESS = SYSTEM.ADDRESS;

PROCEDURE inp(reg: WORD): BITSET; CODE cod.inp  END inp;
PROCEDURE inc(reg: WORD): CHAR;   CODE cod.inp  END inc;
PROCEDURE out (reg,data: WORD);   CODE cod.out  END out;
PROCEDURE outm(reg,data: WORD);   CODE cod.ssw0 END outm;

PROCEDURE push(w: WORD); CODE END push;
PROCEDURE drop; CODE cod.drop END drop;

CONST

  CSR        =  172160b DIV 2; DTR=CSR+1; VEC = 150b DIV 4;
  sec_size   = 512;
  sec_no     = 84254;
  last_block = sec_no DIV 8 * 8 - 8;  -- sector offset to last block

  geterror   = 03b;
  readcom    = 10b;       writecom = 12b;
  rezero     = 1h;        seek     = 0Bh;

VAR dtr : INTEGER;                --
    mask: BITSET;                 --  do not touch or replace !!!!!
    buf0,buf1,buf2,buf3: INTEGER; --
  -- csr: INTEGER;                --

PROCEDURE Ry(MASK: BITSET);
BEGIN REPEAT UNTIL MASK*inp(CSR)#{} END Ry;

PROCEDURE Get_IO_Adr(q_adr: INTEGER): INTEGER;
  VAR i,j: INTEGER;
BEGIN
  i:=INTEGER(BITSET(q_adr)*{0..11});
  IF 0 IN BITSET(i) THEN
    j:=783800h;
  ELSE
    j:=683800h;
  END;
  i:=i DIV 2;
  RETURN INTEGER(BITSET(i)+BITSET(j));
END Get_IO_Adr;

(*$<*) (*$T-*)
PROCEDURE WriteDCB(drv,op,dad,bloCount: INTEGER);
  VAR DCB: ARRAY [0..5] OF CHAR;
        i: INTEGER;
BEGIN
  DCB[0]:=CHAR(INTEGER(BITSET(op)*{0..7}));
  drv:=INTEGER(BITSET(drv)*{0})<<5;
  IF (130        *8<=dad) & (dad<=130     *8+7) THEN
     dad:=last_block + dad MOD 8
  ELSIF (last_block<=dad) & (dad<=last_block+7) THEN
     dad:=130*8 +      dad MOD 8
  END;
  dad:=INTEGER(BITSET(dad)*{0..20});

  DCB[1]:=CHAR(drv+INTEGER((BITSET(dad)*{16..20})>>16));
  DCB[2]:=CHAR(BITSET(dad)*{8..15}>>8);
  DCB[3]:=CHAR(BITSET(dad)*{0..7});
  DCB[4]:=CHAR(BITSET(bloCount)*{0..7});
  DCB[5]:=0c;
  out(DTR,1); out(CSR,1); Ry({15});
  FOR i:=0 TO 5 DO
    out(DTR,DCB[i]); Ry({15})
  END;
END WriteDCB;
(*$>*)

PROCEDURE r;
CODE
  cod.copt
  cod.lgw2 cod.lsw0 cod.lgw3 cod.and
  cod.lgw2 cod.lsw0 cod.lgw3 cod.and cod.li8    cod.rol cod.or
  cod.lgw2 cod.lsw0 cod.lgw3 cod.and cod.lib 16 cod.rol cod.or
  cod.lgw2 cod.lsw0 cod.lgw3 cod.and cod.li8    cod.ror cod.or
  cod.ssw0 cod.li1  cod.add
END r;

PROCEDURE w;
CODE
  cod.copt cod.lsw0
  cod.copt                 cod.sgw7
  cod.li8 cod.rol cod.copt cod.sgw4
  cod.li8 cod.rol cod.copt cod.sgw5
  cod.li8 cod.rol          cod.sgw6
  cod.lgw2
  cod.copt cod.lgw7 cod.ssw0
  cod.copt cod.lgw6 cod.ssw0
  cod.copt cod.lgw5 cod.ssw0
           cod.lgw4 cod.ssw0
  cod.li1  cod.add
END w;

PROCEDURE DoIn(buf: ADDRESS): BOOLEAN;
  VAR i: INTEGER;
      p: POINTER TO ARRAY [0..3] OF CHAR;
      b: BITSET;
BEGIN p:=buf;
  Ry({15});
  p^[0]:=inc(DTR);
  REPEAT b:=inp(CSR) UNTIL 15 IN b;
  p^[1]:=inc(DTR);
  IF 10 IN b THEN RETURN TRUE END;
  p^[2]:=inc(DTR);
  p^[3]:=inc(DTR);
  buf:=buf+1;
  push(buf);
      r;r;r; r;r;r;r;
  FOR i:=0 TO 14 DO
    r;r;r;r; r;r;r;r;
  END;
  drop;
  RETURN FALSE;
END DoIn;

PROCEDURE read(drn,ofs: INTEGER; buf: ADDRESS; len: INTEGER): INTEGER;
  VAR r: BOOLEAN;
BEGIN
  IF drn#0                       THEN RETURN err.bad_parm END;
  IF (ofs<0) OR (ofs+len>sec_no) THEN RETURN err.bad_parm END;
  WriteDCB(drn,readcom,ofs,len);
  Ry({15});
  out(CSR,0);
  REPEAT
    IF DoIn(buf) THEN RETURN err.io_error END;
    len:=len-1;
    buf:=buf + sec_size DIV 4;
  UNTIL len=0;
  Ry({15});
  r:={1}*inp(DTR)#{};
  Ry({15});
  IF inp(DTR)#{} THEN END;
  IF r THEN RETURN err.io_error ELSE RETURN err.ok END;
END read;

PROCEDURE DoOut(buf: ADDRESS);
  VAR i   : INTEGER;
BEGIN
  push(buf);
  Ry({15});
  FOR i:=0 TO 15 DO
    w;w;w;w;  w;w;w;w
  END;
  drop;
  RETURN
END DoOut;

PROCEDURE write(drn,ofs: INTEGER; buf: ADDRESS; len: INTEGER): INTEGER;
  VAR r: BOOLEAN;
BEGIN
  IF drn#0                       THEN RETURN err.bad_parm END;
  IF (ofs<0) OR (ofs+len>sec_no) THEN RETURN err.bad_parm END;
  WriteDCB(drn,writecom,ofs,len);
  Ry({15});
  out(CSR,0);
  REPEAT
    DoOut(buf);
    len:=len-1;
    buf:=buf + sec_size DIV 4;
  UNTIL len=0;
  Ry({15});
  r:=({1}*inp(DTR)#{});
  Ry({15});
  IF inp(DTR)#{} THEN END;
  IF r THEN RETURN err.io_error ELSE RETURN err.ok END;
END write;

PROCEDURE reset;
  VAR i: INTEGER;
BEGIN
  out(DTR,1b); out(CSR,2b);
  FOR i:=0 TO 3 DO END;
  WriteDCB(0,rezero,0,0);
  Ry({15});
  IF inp(DTR)#{} THEN END;
  Ry({15});
  IF inp(DTR)#{} THEN END;
END reset;

PROCEDURE Read(VAR r: req.REQUEST);
  VAR i: INTEGER;
BEGIN
  i:=8;
  REPEAT
    r.res:=read(r.drn,r.ofs,r.buf,r.len); DEC(i);
--    IF r.res#err.ok THEN reset END
  UNTIL (r.res=err.ok) OR (i=0);
END Read;

PROCEDURE Write(VAR r: req.REQUEST);
  VAR i: INTEGER;
BEGIN
  i:=8;
  REPEAT
    r.res:=write(r.drn,r.ofs,r.buf,r.len); DEC(i);
--    IF r.res#err.ok THEN reset END
  UNTIL (r.res=err.ok) OR (i=0);
END Write;

VAR PWOFF: BOOLEAN;

PROCEDURE park_drv;
BEGIN
  WriteDCB(0,readcom,130*8,1); Ry({15});
  PWOFF:=TRUE
END park_drv;

PROCEDURE get_spec(VAR r: req.REQUEST);
BEGIN
  WITH r DO
    dsecs  :=sec_no;
    minsec :=-1;
    minsec :=-1;
    cyls   :=-1;
    ssc    := 9;
    secsize:=sec_size;
    ressec:=  0;
  END
END get_spec;

PROCEDURE doio(VAR r: req.REQUEST);
  VAR try: INTEGER;
BEGIN
  IF PWOFF & (r.op#req.POWER_OFF) THEN r.res:=err.ill_access; RETURN END;
  r.res:=err.ok; try:=8;
  LOOP
    CASE r.op OF
      |req.NOP      :
      |req.READ     : Read(r)
      |req.WRITE    : Write(r)
      |req.MOUNT    : reset; r.res:=err.ok -- TEMPORARY
      |req.UNMOUNT  : reset; r.res:=err.ok -- TEMPORARY
      |req.POWER_OFF: park_drv; r.res:=err.ok
      |req.GET_SPEC : get_spec(r)
    ELSE r.res:=err.inv_op; RETURN
    END;
    IF (try=0) OR (r.res=err.ok) THEN RETURN END;
    DEC(try)
  END
END doio;


VAR res: INTEGER;

BEGIN
  mask:={0..7}; dtr:=Get_IO_Adr(DTR);
              --csr:=Get_IO_Adr(CSR);
  buf0:=4; buf1:=5; buf2:=6; buf3:=7;
  PWOFF:=FALSE;
  res:=fs.define_driver("dk0","",0,fs.disk,doio);
  IF res#err.ok THEN HALT(res) END;
  reset;
  os.suspend(os.active(),-1);
END DK6qSCSI.
