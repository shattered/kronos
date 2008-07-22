IMPLEMENTATION MODULE intMemory; (* 01-Jun-88. (c) KRONOS *)

FROM Terminal   IMPORT  print;
FROM intRAM     IMPORT  FailPH;
FROM SYSTEM     IMPORT  WORD, ADR, ADDRESS;
FROM Resource   IMPORT  Final;

CONST Max      = 0FFFFh;
      CashSize = 128;
      TlbSize  = 64;

TYPE TlbBlock=ARRAY [0..1] OF RECORD
       pg : INTEGER;
       ph : INTEGER;
       wp : BOOLEAN;
     END;
     CashBlock=RECORD
       ph : INTEGER;
       dat: INTEGER;
     END;

VAR mem : ARRAY [0..Max] OF WORD;
    Cash: ARRAY [0..CashSize-1] OF CashBlock;
    Tlb : ARRAY [0..TlbSize-1] OF TlbBlock;
    rep : INTEGER;
    rcnt: INTEGER;
    wcnt: INTEGER;
    rmem: INTEGER;
    fcnt: INTEGER;
    acnt: INTEGER;

PROCEDURE Core(op: INTEGER; adr: INTEGER; VAR n: WORD);
  VAR page,off,ph: INTEGER; p: POINTER TO TlbBlock; wp: BOOLEAN;
BEGIN
  page:=INTEGER(BITSET(adr)-{0..9})>>10;
  off :=INTEGER(BITSET(adr)*{0..9});
  IF op IN {RD,WR,RDck,WRck} THEN
    p:=ADR(Tlb[page*2 MOD TlbSize+ORD(BITSET(page)*{20..21}={})]);
    IF    p^[0].pg=page THEN
      ph:=p^[0].ph; wp:=p^[0].wp;
    ELSIF p^[1].pg=page THEN
      ph:=p^[1].ph; wp:=p^[1].wp;
    ELSE
      INC(fcnt); FailPH:=TRUE; RETURN;
    END;
    ph:=ph+off;
  ELSE
    ph:=adr; wp:=FALSE;
  END;
  IF wp & (op IN {WR,WRck}) THEN FailPH:=TRUE; RETURN END;
  IF (ph<0)OR(ph>HIGH(mem)) THEN print('Illegal ph. address\n'); HALT END;
  IF op IN {WR,RD,WRph,RDph} THEN
    off:=off MOD CashSize;
    CASE op OF
      |WR  : Cash[off].ph:=ph; Cash[off].dat:=n; mem[ph]:=n; INC(wcnt);
      |RD  : INC(rcnt);
             IF Cash[off].ph=ph THEN
               n:=Cash[off].dat
             ELSE
               n:=mem[ph]; Cash[off].dat:=n; Cash[off].ph:=ph; INC(rmem);
             END;
      |WRph: IF Cash[off].ph=ph THEN Cash[off].dat:=n END; mem[ph]:=n; INC(wcnt);
      |RDph: INC(rcnt);
             IF Cash[off].ph=ph THEN
               n:=Cash[off].dat
             ELSE
               n:=mem[ph]; INC(rmem);
             END;
    END;
  END;
END Core;

PROCEDURE SetAdr(adr: INTEGER; ph: INTEGER; wp: BOOLEAN);
  VAR page: INTEGER; p: POINTER TO TlbBlock;
BEGIN
  INC(acnt);
  ph:=INTEGER(BITSET(ph)-{0..9});
  page:=INTEGER(BITSET(adr)-{0..9})>>10;
  p:=ADR(Tlb[page*2 MOD TlbSize+ORD(BITSET(page)*{20..21}={})]);
  IF (p^[0].pg=page)OR(p^[1].pg=page) THEN
    IF p^[0].pg=page THEN p^[0].ph:=ph; p^[0].wp:=wp END;
    IF p^[1].pg=page THEN p^[1].ph:=ph; p^[1].wp:=wp END;
    RETURN;
  END;
  IF rep=0 THEN
    rep:=1; p^[1].pg:=page; p^[1].ph:=ph; p^[1].wp:=wp
  ELSE
    rep:=0; p^[0].pg:=page; p^[0].ph:=ph; p^[0].wp:=wp
  END;
END SetAdr;

PROCEDURE ClearTlb(a: INTEGER);
BEGIN
  a:=INTEGER(BITSET(a)*{0..4})*2+ORD(BITSET(a)*{30..31}={});
  Tlb[a][0].pg:=INTEGER({30..31});
  Tlb[a][1].pg:=INTEGER({30..31});
END ClearTlb;

PROCEDURE GetCode(f: INTEGER): ADDRESS;
BEGIN
  IF (f<0)OR(f>HIGH(mem)) THEN print('Illegal F\n'); HALT END;
  RETURN ADR(mem[f]);
END GetCode;

PROCEDURE Stat;
BEGIN
  print('Статистика памяти:\n');
  print('  всего обращений к памяти  : %6d\n',rcnt+wcnt+acnt);
  print('  всего операций чтения     : %6d\n',rcnt);
  print('  всего операций записи     : %6d\n',wcnt);
  print('  медленных чтений          : %6d\n',rmem);
  print('  быстрых операций          : %6d\n',rcnt+acnt-rmem);
  print('  медленных операций        : %6d\n',rmem+wcnt);
  print('  отказов памяти            : %6d\n',fcnt);
END Stat;

VAR i: INTEGER;

BEGIN
  rep:=0; rcnt:=0; wcnt:=0; rmem:=0; fcnt:=0; acnt:=0;
  Final(Stat);
END intMemory.
