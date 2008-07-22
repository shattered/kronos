IMPLEMENTATION MODULE inFPP; (* Sem 25-Mar-91. (c) KRONOS *)

IMPORT  pc  : pcTab;
IMPORT  sym : inSym;
IMPORT  put : inCmd;
IMPORT  cmd : inCmds;
IMPORT  des : inDesig;
IMPORT  vrs : inVars;

IMPORT  tty : Terminal;

TYPE
  cmd_desc=RECORD
    size : INTEGER;
    cop  : INTEGER;
  END;

PROCEDURE put_cmd(VAL d: cmd_desc; am,sr,ofs: INTEGER);
  VAR n,rr: INTEGER; sgo: BOOLEAN;
BEGIN
  ASSERT((ofs>=0) & (ofs<=10000h-d.size));
  put.b(put.wait);
  sgo:=(am IN {2,3,6}) & (sr#2) OR (am IN {0,1,4,5,7,8}) & (sr#3);
  IF sgo THEN put.b(46b+sr*8) END;
  put.b(d.cop);
  n:=ofs MOD 10000h; rr:=d.cop DIV 100h;
  IF am=8 THEN put.b(006b+rr); put.w(n)
  ELSIF (am#6) & (n=0) THEN put.b(am+rr);
  ELSIF (n<80h) OR (n>=0FF80h) THEN put.b(am+rr+100b); put.b(n);
  ELSE put.b(am+rr+200b); put.w(n);
  END;
END put_cmd;

PROCEDURE gen_cmd(VAL d: cmd_desc; a: sym.access);
  VAR am,sr,ofs,i: INTEGER;
BEGIN
  IF a.am=sym.am_imm THEN
    a.disp:=vrs.new_str(d.size);
    FOR i:=0 TO d.size-1 DO vrs.scode[a.disp+i]:=CHAR(a.n); a.n:=a.n>>8 END;
    a.am:=sym.am_G; a.level:=0;
  END;
  IF a.am=sym.am_Gimm THEN a.am:=sym.am_G END;
  IF a.am=sym.am_Limm THEN a.am:=sym.am_L END;
  cmd.gen_access(a,d.size,am,sr,ofs);
  put_cmd(d,am,sr,ofs);
END gen_cmd;

PROCEDURE calc_flags;
  VAR a: sym.access; i,am,sr,ofs: INTEGER;
BEGIN
  DEC(stk);
  cmd.alloc_reg(1);
  IF cmd.pos[cmd.stk-1]=cmd.da THEN i:=put.AX ELSE i:=put.CX END;
  des.alloc_var(a,2);
  cmd.gen_access(a,2,am,sr,ofs);
  cmd.put_access(am,sr,ofs,70b,09Bh,0DDh);
  cmd.put_access(am,sr,ofs+1,i*8,09Bh,put.mov_brm);
END calc_flags;

PROCEDURE load(cc: cmd_cop; a: sym.access; sz: INTEGER);
  VAR i: INTEGER; d: cmd_desc;
BEGIN
  IF a.am=sym.am_FPP THEN
    CASE cc OF
      |c_mov: RETURN
      |c_add: put.b(9Bh); put.b(0DEh); put.b(0C1h);
      |c_mul: put.b(9Bh); put.b(0DEh); put.b(0C9h);
      |c_sub: put.b(9Bh); put.b(0DEh); put.b(0E9h);
      |c_div: put.b(9Bh); put.b(0DEh); put.b(0F9h);
      |c_cmp: put.b(9Bh); put.b(0DEh); put.b(0D9h); calc_flags;
    END;
    DEC(stk);
  ELSIF a.am=sym.am_STK THEN
    ASSERT(sz=4);
    des.alloc_var(a,sz); cmd.store(a); load(cc,a,sz);
  ELSE
    ASSERT(sz IN {4,8});
    d.size:=sz;
    CASE cc OF
      |c_mov: d.cop:=0D9h+ORD(sz=8)*4; INC(stk);
      |c_add: d.cop:=0D8h+ORD(sz=8)*4;
      |c_mul: d.cop:=08D8h+ORD(sz=8)*4;
      |c_sub: d.cop:=20D8h+ORD(sz=8)*4;
      |c_div: d.cop:=30D8h+ORD(sz=8)*4;
      |c_cmp: d.cop:=18D8h+ORD(sz=8)*4;
    END;
    gen_cmd(d,a);
    IF cc=c_cmp THEN calc_flags END;
  END;
END load;

PROCEDURE store(a: sym.access; sz: INTEGER);
  VAR d: cmd_desc;
BEGIN
  IF a.am=sym.am_FPP THEN
  ELSIF a.am=sym.am_STK THEN
    ASSERT(sz=4);
    des.alloc_var(a,sz); store(a,sz); cmd.load(a,sz);
  ELSE
    d.size:=sz; d.cop:=18D9h+ORD(sz=8)*4;
    gen_cmd(d,a); DEC(stk);
    put.b(put.wait);
  END;
END store;

PROCEDURE load_integer(cc: cmd_cop; a: sym.access; sz: INTEGER);
  VAR i: INTEGER; d: cmd_desc;
BEGIN
  IF a.am=sym.am_FPP THEN
    CASE cc OF
      |c_mov: RETURN
      |c_add: put.b(9Bh); put.b(0DEh); put.b(0C1h);
      |c_mul: put.b(9Bh); put.b(0DEh); put.b(0C9h);
      |c_sub: put.b(9Bh); put.b(0DEh); put.b(0E9h);
      |c_div: put.b(9Bh); put.b(0DEh); put.b(0F9h);
      |c_cmp: put.b(9Bh); put.b(0DEh); put.b(0D9h); calc_flags;
    END;
    DEC(stk);
  ELSIF a.am=sym.am_STK THEN
    ASSERT(sz=4);
    des.alloc_var(a,sz); cmd.store(a); load_integer(cc,a,sz);
  ELSE
    ASSERT(sz IN {2,4});
    d.size:=sz;
    CASE cc OF
      |c_mov: d.cop:= 0DBh+ORD(sz=2)*4; INC(stk);
      |c_add: d.cop:= 0DAh+ORD(sz=2)*4;
      |c_mul: d.cop:=08DAh+ORD(sz=2)*4;
      |c_sub: d.cop:=20DAh+ORD(sz=2)*4;
      |c_div: d.cop:=30DAh+ORD(sz=2)*4;
      |c_cmp: d.cop:=18DAh+ORD(sz=2)*4;
    END;
    gen_cmd(d,a);
    IF cc=c_cmp THEN calc_flags END;
  END;
END load_integer;

PROCEDURE store_integer(a: sym.access; sz: INTEGER);
  VAR d: cmd_desc;
BEGIN
  IF a.am=sym.am_FPP THEN
  ELSIF a.am=sym.am_STK THEN
    ASSERT(sz IN {2,4});
    des.alloc_var(a,sz); store_integer(a,sz); cmd.load(a,sz);
  ELSE
    d.size:=sz;
    CASE sz OF
      |2: d.cop:=18DFh;
      |4: d.cop:=18DBh;
      |8: d.cop:=38DFh;
    END;
    gen_cmd(d,a); DEC(stk);
    put.b(put.wait);
  END;
END store_integer;

PROCEDURE ucm(cc: ucm_cop);
BEGIN
  CASE cc OF
    |u_abs: put.b(09Bh); put.b(0D9h); put.b(0E1h);
    |u_neg: put.b(09Bh); put.b(0D9h); put.b(0E0h);
  END;
END ucm;

BEGIN
  stk:=0; top.am:=sym.am_FPP;
END inFPP.
