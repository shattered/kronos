MODULE sr; (* 03-Dec-89. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADDRESS, ADR, WORD;
FROM Resource   IMPORT  Final;
FROM Scheduler  IMPORT  Signal, Send, Wait, InitSignal, PROCESS,
                        new_process, Gate, InitGate, EnterGate, ExitGate;
FROM FsDrv      IMPORT  IamDriver, RemDriver, serial;
IMPORT mcd : mCodeMnem;

FROM Terminal   IMPORT  Read, print;

CONST
  VEC=18h;
  csr=0F1h;
  dtr=0F0h;
  empty=Signal(NIL);

TYPE
  channel_rec=RECORD
    rate    : INTEGER;
    i_sig   : Signal;
    i_inp   : INTEGER;
    i_out   : INTEGER;
    i_cnt   : INTEGER;
    rx_off  : BOOLEAN;
    o_sig   : Signal;
    o_inp   : INTEGER;
    o_out   : INTEGER;
    o_sinp  : INTEGER;
    o_sout  : INTEGER;
    tx_empty: BOOLEAN;
    tx_off  : BOOLEAN;
    i_buf   : ARRAY [0..255] OF CHAR;
    o_buf   : ARRAY [0..255] OF CHAR;
    o_seq   : ARRAY [0..15] OF CHAR;
  END;

VAR
  ipter  : PROCESS;
  ipted  : PROCESS;
  wsp    : ARRAY [0..499] OF INTEGER;
  chnl   : ARRAY [0..3] OF channel_rec;

PROCEDURE out(n: INTEGER; v: WORD); CODE 93h END out;
PROCEDURE inp(n: INTEGER): WORD; CODE 92h END inp;
PROCEDURE TRANSFER(VAR fr,to: PROCESS); CODE mcd.tra END TRANSFER;
PROCEDURE SETM(m: BITSET); CODE mcd.setm END SETM;
PROCEDURE GETM(): BITSET; CODE mcd.getm END GETM;
PROCEDURE TR(VAR f: BOOLEAN): BOOLEAN; CODE mcd.tr END TR;

PROCEDURE set_rate(n,r: INTEGER);
  VAR cr,i: INTEGER;
BEGIN
  IF    r=19200 THEN i:=64
  ELSIF r= 9600 THEN i:=8
  ELSIF r= 4800 THEN i:=16
  ELSIF r= 2400 THEN i:=32
  ELSIF r= 1200 THEN i:=64
  ELSIF r=  600 THEN i:=128
  ELSIF r=  300 THEN i:=64
  ELSIF r=  150 THEN i:=128
  ELSE RETURN
  END;
  CASE n OF
    |0: out(0FBh<<30,{1,2,4,5});    out(0F8h<<30,i); out(0F8h<<30,0);
    |1: out(0FBh<<30,{1,2,4,5,6});  out(0F9h<<30,i); out(0F9h<<30,0);
    |2: out(0FBh<<30,{1,2,4,5,7});  out(0FAh<<30,i); out(0FAh<<30,0);
    |3: out(0FFh<<30,{1,2,4,5,7});  out(0FEh<<30,i); out(0FEh<<30,0);
  ELSE RETURN
  END;
  cr:=csr+n*2;
  out(cr<<30,{}); out(cr<<30,{});
  out(cr<<30,{}); out(cr<<30,{});
  out(cr<<30,{6});
  IF r>9600 THEN
    out(cr<<30,{0,2,3,4,6,7});
  ELSIF r>=600 THEN
    out(cr<<30,{1,2,3,4,6,7});
  ELSE
    out(cr<<30,{0,1,2,3,4,6,7});
  END;
  out(cr<<30,{0,2});
END set_rate;

PROCEDURE stop;
BEGIN
  out(0FDh<<30,{0..7});
  out((csr  )<<30,{6});
  out((csr+2)<<30,{6});
  out((csr+4)<<30,{6});
  out((csr+6)<<30,{6});
  IF RemDriver('lp') THEN END;
END stop;

PROCEDURE ipt_proc;
  VAR
    s      : BITSET;
    n,i    : INTEGER;
    ch     : CHAR;
BEGIN
  LOOP
    out(0FCh<<30,{2,3});
    s:=BITSET(inp(0FCh<<30));
    IF 7 IN s THEN
      n:=INTEGER(s*{1,2})>>1;
      WITH chnl[n] DO
        IF 0 IN s THEN
          -- transiver
          IF o_sinp#o_sout THEN
            out((dtr+n*2)<<30,o_seq[o_sout]);
            o_sout:=(o_sout+1) MOD (HIGH(o_seq)+1);
          ELSIF NOT tx_off & (o_out#o_inp) THEN
            ch:=o_buf[o_out];
            out((dtr+n*2)<<30,ch);
            o_out:=(o_out+1) MOD (HIGH(o_buf)+1);
            IF o_sig#empty THEN Send(o_sig) END;
            IF (ch=21c) & rx_off THEN
              o_seq[o_sinp]:=21c;
              o_sinp:=(o_sinp+1) MOD (HIGH(o_seq)+1);
              o_seq[o_sinp]:=23c;
              o_sinp:=(o_sinp+1) MOD (HIGH(o_seq)+1);
            ELSIF (ch=23c) & NOT rx_off THEN
              o_seq[o_sinp]:=23c;
              o_sinp:=(o_sinp+1) MOD (HIGH(o_seq)+1);
              o_seq[o_sinp]:=21c;
              o_sinp:=(o_sinp+1) MOD (HIGH(o_seq)+1);
            END;
          ELSE
            tx_empty:=TRUE; out((csr+n*2)<<30,{2});
          END;
        ELSE
          -- reciver
          ch:=inp((dtr+n*2)<<30);
          IF    (ch=23c) & NOT tx_off THEN
            tx_off:=TRUE;
          ELSIF (ch=21c) & tx_off THEN
            tx_off:=FALSE;
            IF TR(tx_empty) THEN out((csr+n*2)<<30,{0,2}) END;
          ELSE
            i:=(i_inp+1) MOD (HIGH(i_buf)+1);
            IF i#i_out THEN i_buf[i_inp]:=ch; i_inp:=i; INC(i_cnt) END;
            IF i_sig#empty THEN Send(i_sig) END;
            IF (i_cnt>HIGH(i_buf)-32) & NOT rx_off THEN
              rx_off:=TRUE;
              o_seq[o_sinp]:=23c;
              o_sinp:=(o_sinp+1) MOD (HIGH(o_seq)+1);
              IF TR(tx_empty) THEN out((csr+n*2)<<30,{0,2}) END;
            END;
          END;
        END;
      END;
      out(0FCh<<30,{5});
    ELSE
      -- is not my
    END;
    TRANSFER(ipter,ipted);
  END;
END ipt_proc;

PROCEDURE init;
  VAR ei: BITSET; i: INTEGER; adr: ADDRESS;
BEGIN
  ei:=GETM(); SETM(ei-{0..1});
  Final(stop);
  out(0FCh<<30,{4});
  out(0FDh<<30,{});
  out(0FDh<<30,{});
  out(0FDh<<30,{}); -- mask
  new_process(ipt_proc,ADR(wsp),SIZE(wsp),ipter);
  adr:=VEC*2;  adr^:=ipter; adr:=adr+1;  adr^:=ADR(ipted);
  FOR i:=0 TO HIGH(chnl) DO
    WITH chnl[i] DO
      InitSignal(i_sig);
      InitSignal(o_sig);
      i_inp:=0; i_out:=0;
      o_inp:=0; o_out:=0;
      o_sinp:=0; o_sout:=0;
      tx_empty:=FALSE;
      tx_off:=FALSE;
      rx_off:=FALSE;
      i_cnt:=0;
      set_rate(i,9600);
    END;
  END;
  SETM(ei);
END init;

PROCEDURE put(n: INTEGER; ch: CHAR);
  VAR ei: BITSET; i: INTEGER;
BEGIN
  WITH chnl[n] DO
    ei:=GETM(); SETM(ei-{0..1});
    i:=(o_inp+1) MOD (HIGH(o_buf)+1);
    WHILE i=o_out DO Wait(o_sig) END;
    o_buf[o_inp]:=ch; o_inp:=i;
    IF TR(tx_empty) THEN out((csr+n*2)<<30,{0,2}) END;
    SETM(ei);
  END;
END put;

PROCEDURE get(n: INTEGER): CHAR;
  VAR ei: BITSET; ch: CHAR;
BEGIN
  WITH chnl[n] DO
    ei:=GETM(); SETM(ei-{0..1});
    WHILE i_inp=i_out DO Wait(i_sig) END;
    ch:=i_buf[i_out]; DEC(i_cnt);
    i_out:=(i_out+1) MOD (HIGH(i_buf)+1);
    IF (i_cnt<32) & TR(rx_off) THEN
      o_seq[o_sinp]:=21c;
      o_sinp:=(o_sinp+1) MOD (HIGH(o_seq)+1);
      IF TR(tx_empty) THEN out((csr+n*2)<<30,{0,2}) END;
    END;
    SETM(ei);
  END;
  RETURN ch;
END get;

----------------------------------------------------------------------------

TYPE str=POINTER TO ARRAY [0..4095] OF CHAR;

VAR
  door: Gate;

PROCEDURE wr(d,b: INTEGER; p: str; s: INTEGER): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  EnterGate(door);
  FOR i:=0 TO s-1 DO put(1,p^[i]) END;
  ExitGate(door);
  RETURN FALSE;
END wr;

PROCEDURE rd(d,b: INTEGER; p: str; s: INTEGER): BOOLEAN;
BEGIN
  RETURN TRUE;
END rd;

PROCEDURE CFE(dev,op: INTEGER; info: ADDRESS): BOOLEAN;
BEGIN
  CASE op OF
      0: info^:=0  --  0 - Обьем в блоках
    | 4:
    |10:
    |21:
    |22:
   ELSE
     RETURN TRUE
   END;
   RETURN FALSE
END CFE;

VAR never: Signal;

BEGIN
  init;
  InitGate(door);
  IF IamDriver('lp',{0},serial,rd,wr,CFE) THEN HALT(1) END;
  InitSignal(never);
  Wait(never);
END sr.
