MODULE t; (*  01-Dec-90. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  tty: Terminal;

CONST
  m_base = 0F00000h;

VAR
  bcb   : POINTER TO RECORD
    magic: INTEGER;  -- 00h
    free : BOOLEAN;  -- 04h
    cycle: INTEGER;  -- 08h
    cmd  : INTEGER;  -- 0Ch
    to   : INTEGER;  -- 10h  move or call
    from : INTEGER;  -- 14h  move
    len  : INTEGER;  -- 18h  of move
    res  : INTEGER;  -- 1Ch  res of call
  END;

--------------------------------------------------------------

PROCEDURE vis_bcb;
BEGIN
  tty.print('magic = %8$h\n',bcb^.magic);
  IF bcb^.magic#70077007h THEN tty.print('*** illegal magic ***\n') END;
  tty.print('free  = %8$h\n',bcb^.free);
  tty.print('cycle = %8$h\n',bcb^.cycle);
  tty.print('cmd   = %8$h\n',bcb^.cmd);
  tty.print('to    = %8$h\n',bcb^.to);
  tty.print('from  = %8$h\n',bcb^.from);
  tty.print('len   = %8$h\n',bcb^.len);
  tty.print('res   = %8$h\n',bcb^.res);
END vis_bcb;

PROCEDURE get_bcb(channel: INTEGER): SYSTEM.ADDRESS;
  VAR
    ma   : INTEGER;
    m_ptr: POINTER TO ARRAY [0..0FFFFFh] OF CHAR;  -- Multibus memory
BEGIN
  m_ptr:=SYSTEM.ADDRESS(m_base);
  ma:=0F8010h+channel*4;
  ma:=ORD(m_ptr^[ma])+ORD(m_ptr^[ma+1])*100h+0F0000h;
  ASSERT(ma MOD 4=0);
  RETURN m_base+(ma DIV 4);
END get_bcb;

VAR p: POINTER TO INTEGER;

BEGIN
  bcb:=get_bcb(15);
  LOOP
    tty.print('%$8h\r',bcb^.cycle);
  END;
(*
  p:=SYSTEM.ADDRESS(m_base+20000h);
  LOOP
    tty.print('%$8h\r',p^);
  END;
*)
END t.
