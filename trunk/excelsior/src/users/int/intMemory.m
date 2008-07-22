IMPLEMENTATION MODULE intMemory; (* 01-Jun-88. (c) KRONOS *)

FROM Terminal   IMPORT  print;
FROM SYSTEM     IMPORT  WORD, ADR, ADDRESS;
FROM mCodeMnem  IMPORT  li0A, lib, shr, and;
FROM intExcp    IMPORT  RaiseInMe, NoPage, WriteProtect;

CONST Max      = 0FFFFh;
      CashDep  = 64;
      CashWid  = 16;

TYPE CashItem =RECORD
       Vadr: INTEGER; -- Виртуальный адрес
       Padr: INTEGER; -- Физический  адрес
       Val : WORD;    -- Данные
       Wren: BOOLEAN; -- Запись разрешена
     END;
     CashBlock=ARRAY [0..CashWid-1] OF CashItem;

VAR Mem : ARRAY [0..Max] OF WORD;
    Cash: ARRAY [0..CashDep-1] OF CashBlock;
    rep : INTEGER;
    rcnt: INTEGER;
    wcnt: INTEGER;
    rmem: INTEGER;
    fcnt: INTEGER;
    wrpr: INTEGER;

PROCEDURE Index(i: INTEGER): INTEGER; CODE li0A shr lib 03Fh and END Index;

PROCEDURE Fail;
BEGIN
  INC(fcnt); RaiseInMe(NoPage);
END Fail;

PROCEDURE WrFail;
BEGIN
  INC(wrpr); RaiseInMe(WriteProtect);
END WrFail;

PROCEDURE CoreWR(Adr: INTEGER; Val: WORD);
  VAR p : POINTER TO CashBlock;
      i : INTEGER;
      ph: INTEGER;
BEGIN
  INC(wcnt);
  p:=ADR(Cash[Index(Adr)]);
  FOR i:=0 TO HIGH(p^) DO
    IF p^[i].Vadr=Adr THEN
      IF NOT p^[i].Wren THEN WrFail END;
      p^[i].Val:=Val; Mem[p^[i].Padr*1024+Adr MOD 1024]:=Val; RETURN;
    END;
  END;
  FOR i:=0 TO HIGH(p^) DO
    IF (BITSET(p^[i].Vadr)/BITSET(Adr))*{10..31}={} THEN
      IF NOT p^[i].Wren THEN WrFail END;
      ph:=p^[i].Padr;
      i:=rep; rep:=(rep+1) MOD CashWid;
      p^[i].Vadr:=Adr; p^[i].Padr:=ph; p^[i].Wren:=TRUE;
      p^[i].Val:=Val; Mem[p^[i].Padr*1024+Adr MOD 1024]:=Val; RETURN;
    END;
  END;
  Fail;
END CoreWR;

PROCEDURE CoreRD(Adr: INTEGER): WORD;
  VAR p : POINTER TO CashBlock;
      i : INTEGER;
      ph: INTEGER;
      wp: BOOLEAN;
BEGIN
  INC(rcnt);
  p:=ADR(Cash[Index(Adr)]);
  FOR i:=0 TO HIGH(p^) DO
    IF p^[i].Vadr=Adr THEN RETURN p^[i].Val END;
  END;
  FOR i:=0 TO HIGH(p^) DO
    IF (BITSET(p^[i].Vadr)/BITSET(Adr))*{10..31}={} THEN
      ph:=p^[i].Padr; wp:=p^[i].Wren;
      i:=rep; rep:=(rep+1) MOD CashWid;
      p^[i].Vadr:=Adr; p^[i].Padr:=ph; p^[i].Wren:=wp; INC(rmem);
      p^[i].Val:=Mem[p^[i].Padr*1024+Adr MOD 1024]; RETURN p^[i].Val;
    END;
  END;
  Fail;
END CoreRD;

PROCEDURE RDdirect(Adr: INTEGER): WORD;
  VAR p : POINTER TO CashBlock;
      i : INTEGER;
BEGIN
  INC(rcnt);
  p:=ADR(Cash[Index(Adr)]);
  FOR i:=0 TO HIGH(p^) DO
    IF (BITSET(p^[i].Vadr)/BITSET(Adr))*{10..31}={} THEN
      INC(rmem); p^[i].Val:=Mem[p^[i].Padr*1024+Adr MOD 1024]; RETURN p^[i].Val;
    END;
  END;
  Fail;
END RDdirect;

PROCEDURE Check(Adr: INTEGER): BOOLEAN;
  VAR p : POINTER TO CashBlock;
      i: INTEGER;
BEGIN
  p:=ADR(Cash[Index(Adr)]);
  FOR i:=0 TO HIGH(p^) DO
    IF (BITSET(p^[i].Vadr)/BITSET(Adr))*{10..31}={} THEN RETURN TRUE END;
  END;
  RETURN FALSE;
END Check;

PROCEDURE SetAdr(Adr: INTEGER; ph: INTEGER; we: BOOLEAN);
  VAR p : POINTER TO CashBlock;
      i,k: INTEGER;
BEGIN
  p:=ADR(Cash[Index(Adr)]); k:=0;
  FOR i:=0 TO HIGH(p^) DO
    IF (BITSET(p^[i].Vadr)/BITSET(Adr))*{10..31}={} THEN p^[i].Wren:=we; INC(k) END;
  END;
  IF k=0 THEN
    i:=rep; rep:=(rep+1) MOD CashWid;
    p^[i].Vadr:=Adr; p^[i].Padr:=ph; p^[i].Wren:=we;
  END;
END SetAdr;

PROCEDURE ShowStat;
BEGIN
  print('Статистика кеша:\n');
  print('  всего операций записи  : %5d\n',wcnt);
  print('  всего операций чтения  : %5d\n',rcnt);
  print('  чтений из ОЗУ          : %5d\n',rmem);
  print('  вычислений физ. адреса : %5d\n',fcnt);
  print('  нарушений защиты записи: %5d\n',wrpr);
END ShowStat;

PROCEDURE GetCode(f: INTEGER): ADDRESS;
BEGIN
  RETURN ADR(Mem[f]);
END GetCode;

VAR i,j: INTEGER;

BEGIN
  FOR i:=0 TO HIGH(Cash) DO
    FOR j:=0 TO HIGH(Cash[i]) DO Cash[i][j].Vadr:=INTEGER({31,30}) END;
  END;
  rep:=0;
  wcnt:=0; rcnt:=0; rmem:=0; fcnt:=0; wrpr:=0;
END intMemory.
