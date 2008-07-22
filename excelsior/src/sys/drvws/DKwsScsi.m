MODULE DKwsScsi; (* 23-Mar-89. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  cod: defCodes;
IMPORT  req: defRequest;
IMPORT  err: defErrors;
IMPORT  os : osKernel;
IMPORT  fs : osFiles;
IMPORT  str: Strings;
IMPORT  env: tskEnv;

TYPE
  ADDRESS = SYSTEM.ADDRESS;

CONST
  ok    = err.ok;
  CSR   = 801C00h;
  DAT   = 800C00h;
  TRIES = 8;
  sec_size   = 512;
  sec_no     = 84254;

VAR
  dsecs : INTEGER;
  ressec: INTEGER;
  csr   : POINTER TO BITSET;
  dat   : POINTER TO ARRAY [0..1023] OF INTEGER;

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
  push(to);
  FOR i:=0 TO 7 DO
    w; w; w; w; w; w; w; w;
    w; w; w; w; w; w; w; w;
  END;
  i:=pop();
END _read;

PROCEDURE read(VAR r: req.REQUEST);
  VAR adr: ADDRESS;
      l,s: INTEGER;
BEGIN
  l:=r.len;  r.len:=0;
  s:=r.ofs;  adr:=r.buf;  r.res:=ok;
  WHILE l>0 DO
    REPEAT UNTIL csr^*{0..1}={0..1};
    dat^[0]:=1;
    dat^[1]:=08h;
    dat^[2]:=s DIV 10000h;
    dat^[3]:=s DIV 100h;
    dat^[4]:=s;
    dat^[5]:=1;
    dat^[6]:=0;
    dat^[7+512]:=0FFh;
    csr^:={1};
    REPEAT UNTIL csr^*{0..1}={0..1};
    IF adr#NIL THEN _read(SYSTEM.ADR(dat^[7]),adr); INC(adr,128) END;
    IF csr^*{3}#{} THEN r.res:=err.time_out; RETURN END;
    IF BITSET(dat^[7+512])*{1..4}#{} THEN r.res:=err.io_error; RETURN END;
    INC(s);  DEC(l);  INC(r.len)
  END
END read;

PROCEDURE write(VAR r: req.REQUEST);
  VAR i: INTEGER;
    l,s: INTEGER;
    ptr: ADDRESS;
    end: ADDRESS;
    buf: POINTER TO ARRAY [0..511] OF CHAR;
BEGIN
  l:=r.len;  r.len:=0;
  s:=r.ofs;  buf:=r.buf;   r.res:=ok;
  WHILE l>0 DO
    REPEAT UNTIL csr^*{0..1}={0..1};
    dat^[0]:=1;
    dat^[1]:=0Ah;
    dat^[2]:=s DIV 10000h;
    dat^[3]:=s DIV 100h;
    dat^[4]:=s;
    dat^[5]:=1;
    dat^[6]:=0;
    ptr:=SYSTEM.ADR(dat^[7]);
    end:=ptr+512;
    i:=0;               (*$<$T-*)
    REPEAT ptr^:=INTEGER(buf^[i]); i:=i+1; ptr:=ptr+1 UNTIL ptr=end;
    dat^[7+512]:=0FFh;  (*$>*)
    csr^:={1};
    REPEAT UNTIL csr^*{0..1}={0..1};
    IF csr^*{3}#{}  THEN r.res:=err.time_out; RETURN END;
    IF BITSET(dat^[7+512])*{1..4}#{} THEN r.res:=err.io_error; RETURN END;
    INC(s);  DEC(l);  INC(r.len);  buf:=ADDRESS(buf)+128
  END
END write;

PROCEDURE READ(VAR r: req.REQUEST);
  VAR t,len: INTEGER;
BEGIN
  t:=TRIES; len:=r.len;
  IF len<=0 THEN r.res:=err.bad_parm; RETURN END;
  INC(r.ofs,ressec);
  LOOP
    read(r); DEC(t);
    IF (r.res=err.ok) OR (t=0) THEN RETURN END;
    r.len:=len
  END
END READ;

PROCEDURE WRITE(VAR r: req.REQUEST);
  VAR t,len: INTEGER;
BEGIN
  t:=TRIES; len:=r.len;
  IF len<=0 THEN r.res:=err.bad_parm; RETURN END;
  INC(r.ofs,ressec);
  LOOP
    write(r); DEC(t);
    IF (r.res=ok) OR (t=0) THEN RETURN END;
    r.len:=len
  END
END WRITE;

PROCEDURE SEEK(VAR r: req.REQUEST);
BEGIN
  INC(r.ofs,ressec);
  r.buf:=NIL; r.len:=1; read(r)
END SEEK;

PROCEDURE get_spec(VAR r: req.REQUEST);
BEGIN
  r.dmode  :=req.ready+req.wint+req.fmtunit;
  r.ressec :=ressec;       r.dsecs :=dsecs;
  r.secsize:=512;          r.ssc   := 9;
  r.cyls   :=dsecs DIV 2;  r.minsec:= 0;
  r.heads  :=2;            r.maxsec:= 0
END get_spec;

PROCEDURE mount(VAR r: req.REQUEST);
  VAR LAB: POINTER TO ARRAY [0..511] OF CHAR;
BEGIN
  r.len:=1; r.ofs:=0;  LAB:=r.buf;
  read(r);
  IF r.res#ok THEN read(r) END;
  IF r.res#ok THEN RETURN  END;
  IF (LAB^[8+0]#"X") OR (LAB^[8+1]#"D") OR (LAB^[8+2]#"0") THEN RETURN END;
  dsecs :=(ORD(LAB^[8+4])+ORD(LAB^[8+5])*256)*ORD(LAB^[8+8]);
  ressec:= ORD(LAB^[8+10]);
END mount;


PROCEDURE doio(VAR r: req.REQUEST);
BEGIN
  r.res:=ok;
  CASE r.op OF
    |req.READ     : READ(r)
    |req.WRITE    : WRITE(r)
    |req.SEEK     : SEEK(r)
    |req.MOUNT    : mount(r)
    |req.UNMOUNT  :
    |req.POWER_OFF: r.ofs:=sec_no-1; SEEK(r)
    |req.SET_SPEC : ressec:=r.ressec;  dsecs:=r.dsecs
    |req.GET_SPEC : get_spec(r);
  ELSE
    r.res:=err.inv_op
  END
END doio;

VAR name: ARRAY [0..7] OF CHAR;

VAR i: INTEGER;
    s: STRING;

BEGIN
  env.get_str(env.args,s);
  IF NOT env.done OR (s="") THEN str.copy(name,"dk0")
  ELSE                           str.copy(name,s)
  END;
  dat:=ADDRESS(DAT); csr:=ADDRESS(CSR);
  IF csr^={} THEN HALT(err.not_ready) END;
  ressec:=0;  dsecs:=sec_no;
  i:=fs.define_driver(name,"",0,fs.disk,doio);
  env.put_str(env.info,name,TRUE);
  IF i#ok THEN HALT(i) END;
  env.become_ipr;
  os.suspend(os.active(),-1)
END DKwsScsi.
