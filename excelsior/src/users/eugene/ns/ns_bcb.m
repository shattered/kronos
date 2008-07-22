IMPLEMENTATION MODULE ns_bcb; (* Ned 13-Jan-89. (c) KRONOS *)

IMPORT SYSTEM, T: Terminal, clock: Time, str: Strings, os : osKernel;

CONST m_base = 0F00000h;

VAR
  m_ptr: POINTER TO ARRAY [0..0] OF CHAR;  -- Multibus memory
  k_ptr: POINTER TO ARRAY [0..0] OF CHAR;  -- Kronos   memory
  flag : INTEGER;

--------------------------------------------------------------

PROCEDURE vis_bcb;
BEGIN
  T.print('magic = %8$h\n',bcb^.magic);
  IF bcb^.magic#70077007h THEN T.print('*** illegal magic ***\n') END;
  T.print('free  = %8$h\n',bcb^.free);
  T.print('cycle = %8$h\n',bcb^.cycle);
  T.print('cmd   = %8$h\n',bcb^.cmd);
  T.print('to    = %8$h\n',bcb^.to);
  T.print('from  = %8$h\n',bcb^.from);
  T.print('len   = %8$h\n',bcb^.len);
  T.print('res   = %8$h\n',bcb^.res);
END vis_bcb;

PROCEDURE trb(a: SYSTEM.ADDRESS; ma: INTEGER): INTEGER; CODE 94h 02h END trb;

PROCEDURE wait;
  VAR i: INTEGER;
BEGIN
  WHILE trb(0,flag)=0 DO
    FOR i:=0 TO 100 DO END;
  END;
END wait;

PROCEDURE exec;
BEGIN
(*$T-*)
  k_ptr^[flag]:=1c;
(*$T+*)
END exec;

PROCEDURE move(to,from,len: INTEGER);
  VAR i: INTEGER;
BEGIN
  wait;
  bcb^.to  :=to;
  bcb^.from:=from;
  bcb^.len :=len;
  bcb^.cmd :=2;
  bcb^.free:=TRUE;
  REPEAT
    FOR i:=0 TO 1000 DO END;
    wait;
    i:=bcb^.cmd;
    bcb^.free:=TRUE;
  UNTIL i=0;
END move;

PROCEDURE call(pc: INTEGER; VAR res,time: INTEGER);
  VAR i: INTEGER;
BEGIN
  wait;
  bcb^.to  :=pc;
  bcb^.cmd :=1;
  bcb^.free:=TRUE;
  time:=clock.sys_time(clock.milisec);
  REPEAT
    FOR i:=0 TO 1000 DO END;
    wait;
    i:=bcb^.cmd;
    res:=bcb^.res;
    bcb^.free:=TRUE;
  UNTIL i=0;
  time:=clock.sys_time(clock.milisec)-time;
END call;

--------------------------------------------------------------

PROCEDURE buf_def(VAL s: ARRAY OF CHAR;
                  VAR a: SYSTEM.ADDRESS;  VAR size: INTEGER): BOOLEAN;
  VAR
    i: INTEGER;
   done: BOOLEAN;
BEGIN
  i:=1;
  str.skip(s,i,' ');
  IF i+4>HIGH(s) THEN RETURN FALSE END;
  IF (s[i]='N')&(s[i+1]='S')&(s[i+2]='3')&(s[i+3]='2')&(s[i+4]=' ') THEN
    i:=i+5
  ELSE
    RETURN FALSE
  END;
  str.skip(s,i,' ');
  str.iscan(a,s,i,done);
  IF NOT done THEN RETURN FALSE END;
  str.skip(s,i,' ');
  str.iscan(size,s,i,done);
  IF NOT done THEN RETURN FALSE END;
  IF (i<=HIGH(s)) & (s[i]='K') THEN size:=size*1024 END;
  IF size<=0 THEN RETURN FALSE END;
  RETURN TRUE
END buf_def;

PROCEDURE init_buf;
  VAR
    i   : INTEGER;
    adr : SYSTEM.ADDRESS;
    str : STRING;
    size: INTEGER;
BEGIN
  i:=0;
  WHILE os.get_sys_parm(i,str) DO
    IF buf_def(str,adr,size) THEN
      buffer.HIGH:=size-1;
      buffer.ADR :=adr;
      buf_ma:=(adr-m_base)*4+0C00000h;
      T.print('buffer adr=%$8hh, bytes=%$8hh.\n',adr,size);
      RETURN;
    END
  END;
  T.print('buffer not found.\n'); HALT;
END init_buf;

PROCEDURE get_bcb(channel: INTEGER): SYSTEM.ADDRESS;
  VAR ma: INTEGER;
BEGIN
  ma:=0F8010h+channel*4;
(*$T-*)
  ma:=ORD(m_ptr^[ma])+ORD(m_ptr^[ma+1])*100h+0F0000h;
(*$T+*)
  ASSERT(ma MOD 4=0);
  RETURN m_base+(ma DIV 4);
END get_bcb;

BEGIN
  k_ptr:=SYSTEM.ADDRESS(0);
  m_ptr:=SYSTEM.ADDRESS(m_base);
  bcb  :=get_bcb(15);
  flag :=INTEGER(bcb)*4+4;
  init_buf;
END ns_bcb.
