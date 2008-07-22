IMPLEMENTATION MODULE UTK6; (*$T- 27-Dec-89. (c) KRONOS *)

IMPORT mcd : mCodeMnem;
FROM SYSTEM     IMPORT  WORD, ADR;
FROM Image      IMPORT  image0;
FROM Resource   IMPORT  Final;
FROM Terminal   IMPORT  print;

CONST
  rg_cmd  = 164002b DIV 2;
  rg_dwr  = 164004b DIV 2;
  rg_stat = 164006b DIV 2;
  rg_adr1 = 164010b DIV 2;
  rg_drd  = 164012b DIV 2;
  rg_adr2 = 164014b DIV 2;

  bf_size = 2000;

TYPE
  set     = ARRAY [0..5] OF BITSET;
  command = (c_out,c_inp,c_0,c_1,f_0,f_1);
  c_set   = SET OF command;

VAR
  bf_cmd : ARRAY [0..bf_size-1] OF command;
  bf_dat : ARRAY [0..bf_size-1] OF INTEGER;
  bf_cnt : INTEGER;
  outs   : ARRAY [0..31] OF BITSET; -- каналы, включенные на вывод
  test   : ARRAY [0..31] OF BITSET; -- значения каналов
  errs   : ARRAY [0..31] OF BITSET; -- результат контроля

PROCEDURE inp(n: INTEGER): BITSET; CODE mcd.inp END inp;
PROCEDURE out(n: INTEGER; v: WORD); CODE mcd.out END out;

PROCEDURE output(SEQ n: INTEGER);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(n) DO
    bf_cmd[bf_cnt]:=c_out; bf_dat[bf_cnt]:=n[i]-1; INC(bf_cnt);
  END;
END output;

PROCEDURE input(SEQ n: INTEGER);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(n) DO
    bf_cmd[bf_cnt]:=c_inp; bf_dat[bf_cnt]:=n[i]-1; INC(bf_cnt);
  END;
END input;

PROCEDURE a(n,val: INTEGER);
BEGIN
  IF val=0 THEN
    bf_cmd[bf_cnt]:=c_0; bf_dat[bf_cnt]:=n-1; INC(bf_cnt);
  ELSE
    bf_cmd[bf_cnt]:=c_1; bf_dat[bf_cnt]:=n-1; INC(bf_cnt);
  END;
END a;

PROCEDURE b(n,val: INTEGER);
BEGIN
  IF val=0 THEN
    bf_cmd[bf_cnt]:=f_0; bf_dat[bf_cnt]:=n-1; INC(bf_cnt);
  ELSE
    bf_cmd[bf_cnt]:=f_1; bf_dat[bf_cnt]:=n-1; INC(bf_cnt);
  END;
END b;

PROCEDURE flash(): INTEGER;
  VAR
    adr : INTEGER;
    pos : INTEGER;
    crc : BITSET;
    i   : INTEGER;
  PROCEDURE set_outs;
    VAR i: INTEGER;
  BEGIN
    -- заносим команду КОМУТАЦИЯ
    out(rg_cmd,10h); out(rg_adr2,adr); out(rg_cmd,18h);
    out(rg_dwr,{1..4}); out(rg_dwr,{5});
    out(rg_dwr,{0}); out(rg_dwr,{6});
    INC(adr);
    out(rg_cmd,10h); out(rg_adr2,adr); out(rg_cmd,18h);
    crc:={};
    FOR i:=31 TO 0 BY -1 DO
      crc:=crc/outs[i]; out(rg_dwr,outs[i]);
    END;
    out(rg_dwr,{}); out(rg_dwr,crc/{0,1,2,4,5});
    out(rg_dwr,{}); out(rg_dwr,{3});
    out(rg_dwr,{6});
    INC(adr);
    out(rg_cmd,10h); out(rg_adr2,adr); out(rg_cmd,18h);
    out(rg_dwr,{0,1,3,4,5}); out(rg_dwr,{});
    out(rg_dwr,{2}); out(rg_dwr,{6});
    INC(adr);
  END set_outs;
BEGIN
  adr:=0; pos:=0;
  LOOP
    IF pos=bf_cnt THEN EXIT END;
    CASE bf_cmd[pos] OF
      |c_out,c_inp:
        REPEAT
          IF bf_cmd[pos]=c_out THEN
            INCL(outs[bf_dat[pos] DIV 6],bf_dat[pos] MOD 6);
          ELSE
            EXCL(outs[bf_dat[pos] DIV 6],bf_dat[pos] MOD 6);
          END;
          INC(pos);
        UNTIL (pos>=bf_cnt) OR NOT (bf_cmd[pos] IN c_set{c_out,c_inp});
        set_outs;
      |c_0,c_1:
        IF pos=0 THEN set_outs END;
        REPEAT
          IF bf_cmd[pos]=c_1 THEN
            INCL(test[bf_dat[pos] DIV 6],bf_dat[pos] MOD 6);
          ELSE
            EXCL(test[bf_dat[pos] DIV 6],bf_dat[pos] MOD 6);
          END;
          INC(pos);
        UNTIL (pos>=bf_cnt) OR NOT (bf_cmd[pos] IN c_set{c_0,c_1});
        out(rg_cmd,10h); out(rg_adr2,adr); out(rg_cmd,18h);
        crc:={};
        FOR i:=31 TO 0 BY -1 DO
          crc:=crc/test[i]; out(rg_dwr,test[i]);
        END;
        out(rg_dwr,{}); out(rg_dwr,crc/{1,2,3,4});
        out(rg_dwr,{0}); out(rg_dwr,{5});
        out(rg_dwr,{6});
        INC(adr);
      |f_0,f_1:
        IF pos=0 THEN set_outs END;
        IF bf_cmd[pos]=f_1 THEN
          INCL(test[bf_dat[pos] DIV 6],bf_dat[pos] MOD 6);
        ELSE
          EXCL(test[bf_dat[pos] DIV 6],bf_dat[pos] MOD 6);
        END;
        INC(pos);
        out(rg_cmd,10h); out(rg_adr2,adr); out(rg_cmd,18h);
        crc:={};
        FOR i:=31 TO 0 BY -1 DO
          crc:=crc/test[i]; out(rg_dwr,test[i]);
        END;
        out(rg_dwr,{}); out(rg_dwr,crc/{1,2,3,4});
        out(rg_dwr,{0}); out(rg_dwr,{5});
        out(rg_dwr,{6});
        INC(adr);
    END;
  END;
  bf_cnt:=0;
  RETURN adr;
END flash;

PROCEDURE b?(n,val: INTEGER): BOOLEAN;
  VAR
    adr: INTEGER;
    i  : INTEGER;
    crc: BITSET;
    s  : BITSET;
BEGIN
  adr:=flash();
  IF adr#0 THEN
    -- заносим команду MASK
    out(rg_cmd,10h); out(rg_adr2,adr); out(rg_cmd,18h);
    FOR i:=31 TO 0 BY -1 DO out(rg_dwr,{0..5}) END;
    out(rg_dwr,{}); out(rg_dwr,{0,1,3,4,5});
    out(rg_dwr,{}); out(rg_dwr,{2});
    out(rg_dwr,{6});
    INC(adr);
    -- заносим команду ТЕСТ НАБОР
    out(rg_cmd,10h); out(rg_adr2,adr); out(rg_cmd,18h);
    crc:={};
    FOR i:=31 TO 0 BY -1 DO
      crc:=crc/test[i]; out(rg_dwr,test[i]);
    END;
    out(rg_dwr,{}); out(rg_dwr,crc/{1,2,3});
    out(rg_dwr,{0,4}); out(rg_dwr,{5});
    out(rg_dwr,{6});
    INC(adr);
    out(rg_cmd,00h); -- выключение контроля уровней
    out(rg_cmd,08h); -- пуск сегмента
    REPEAT s:=inp(rg_stat) UNTIL 0 IN s;
    IF 1 IN s THEN
      out(rg_cmd,1Bh); -- вывод результата сравнения
      FOR i:=31 TO 0 BY -1 DO errs[i]:=inp(rg_drd) END;
    ELSE
      print('******** stat %{}\n',s);
      IF 3 IN s THEN
        print('Брак уровней сигналов.\n');
      ELSIF 2 IN s THEN
        print('Брак выходных сигналов.\n');
        out(rg_cmd,1Bh); -- вывод результата сравнения
        FOR i:=31 TO 0 BY -1 DO errs[i]:=inp(rg_drd) END;
        FOR i:=0 TO 191 DO
          IF (i MOD 6) IN (errs[i DIV 6]*outs[i DIV 6]) THEN
            print('%3d ',i+1)
          END;
        END;
        print('\n');
      END;
      HALT(1);
    END;
  END;
  DEC(n);
  IF (n MOD 6) IN outs[n DIV 6] THEN
    RETURN (((n MOD 6) IN test[n DIV 6]) )-- #((n MOD 6) IN errs[n DIV 6]))=(val#0);
  ELSE
    RETURN ((n MOD 6) IN (errs[n DIV 6]/test[n DIV 6]))=(val#0);
  END;
END b?;

PROCEDURE init;
BEGIN
  out(rg_cmd,16b); -- reset
  out(rg_cmd,07b); -- power off
END init;

PROCEDURE clear;
  VAR i: INTEGER;
BEGIN
  out(rg_cmd,16b); -- reset
  out(rg_cmd,07b); -- power off
  FOR i:=0 TO HIGH(outs) DO outs[i]:={} END;
  FOR i:=0 TO HIGH(test) DO test[i]:={} END;
  FOR i:=0 TO HIGH(errs) DO errs[i]:={} END;
  bf_cnt:=0;
END clear;

PROCEDURE power(on: BOOLEAN; val: INTEGER);
-- val = -2    -10%
-- val = -1    -5%
-- val =  0    normal
-- val =  1    +5%
-- val =  2    +10%
BEGIN
  CASE val OF
    |-2: out(rg_cmd,14b); out(rg_cmd,03b);
    |-1: out(rg_cmd,02b); out(rg_cmd,14b);
    | 0: out(rg_cmd,12b);
    | 1: out(rg_cmd,02b); out(rg_cmd,13b);
    | 2: out(rg_cmd,13b); out(rg_cmd,03b);
  END;
  IF on THEN out(rg_cmd,04b) ELSE out(rg_cmd,07b) END;
END power;

BEGIN
  clear;
  Final(clear);
END UTK6.
