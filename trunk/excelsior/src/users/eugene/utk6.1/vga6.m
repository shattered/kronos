MODULE vga6; (* 02-Nov-88. (c) KRONOS *)

FROM Terminal   IMPORT  Read, print, BusyRead;
FROM SYSTEM     IMPORT  WORD;
FROM UTK6       IMPORT  input, output, b?, b, a, power;
IMPORT mcd : mCodeMnem;

-- программа управления стендом для тестирования модулей ЖШСИ 010.03

CONST
  bfa   = 19;   -- A21
  bfb   = 71;   -- C21
  bin   = 22;   -- A24
  bout  = 74;   -- C24
  madr0 = 20;   -- A22
  madr1 = 72;   -- C22
  madr2 = 21;   -- A23
  madr3 = 73;   -- C23
  binhibit = 75;-- C25
  bmcl  = 78;   -- C28
  bd00  =  3;   -- A05
  bd01  = 55;   -- C05
  bd02  =  4;   -- A06
  bd03  = 56;   -- C06
  bd04  =  5;   -- A07
  bd05  = 57;   -- C07
  bd06  =  6;   -- A08
  bd07  = 58;   -- C08
  bd08  =  7;   -- A09
  bd09  = 59;   -- C09
  bd10  =  8;   -- A10
  bd11  = 60;   -- C10
  bd12  =  9;   -- A11
  bd13  = 61;   -- C11
  bd14  = 10;   -- A12
  bd15  = 62;   -- C12
  bd16  = 11;   -- A13
  bd17  = 63;   -- C13
  bd18  = 12;   -- A14
  bd19  = 64;   -- C14
  bd20  = 13;   -- A15
  bd21  = 65;   -- C15
  bd22  = 14;   -- A16
  bd23  = 66;   -- C16
  bd24  = 15;   -- A17
  bd25  = 67;   -- C17
  bd26  = 16;   -- A18
  bd27  = 68;   -- C18
  bd28  = 17;   -- A19
  bd29  = 69;   -- C19
  bd30  = 18;   -- A20
  bd31  = 70;   -- C20

VAR
  base: INTEGER;
  stp : BOOLEAN;

PROCEDURE wait;
  VAR ch: CHAR;
BEGIN
  IF b?(bfb,1) THEN END;
  print(' Нажмите любую клавишу для продолжения...\r');
  ch:=Read();
  print('                                         \n');
END wait;

PROCEDURE wait_s;
  VAR ch: CHAR;
BEGIN
  IF b?(bfa,0) THEN END;
  print(
  'Нажмите клавишу "S" для пошаговой проверки или пробел для продолжения...\r');
  LOOP
    ch:=Read();
    IF ch='s' THEN stp:=TRUE; EXIT END;
    IF ch='S' THEN stp:=TRUE; EXIT END;
    IF ch=' ' THEN stp:=FALSE; EXIT END;
  END;
  print(
  '                                                                        \n');
END wait_s;

PROCEDURE bd?(): WORD;
  VAR n: BITSET;
BEGIN
  n:={};
  IF b?(bd00,1) THEN INCL(n, 0) END; IF b?(bd01,1) THEN INCL(n, 1) END;
  IF b?(bd02,1) THEN INCL(n, 2) END; IF b?(bd03,1) THEN INCL(n, 3) END;
  IF b?(bd04,1) THEN INCL(n, 4) END; IF b?(bd05,1) THEN INCL(n, 5) END;
  IF b?(bd06,1) THEN INCL(n, 6) END; IF b?(bd07,1) THEN INCL(n, 7) END;
  IF b?(bd08,1) THEN INCL(n, 8) END; IF b?(bd09,1) THEN INCL(n, 9) END;
  IF b?(bd10,1) THEN INCL(n,10) END; IF b?(bd11,1) THEN INCL(n,11) END;
  IF b?(bd12,1) THEN INCL(n,12) END; IF b?(bd13,1) THEN INCL(n,13) END;
  IF b?(bd14,1) THEN INCL(n,14) END; IF b?(bd15,1) THEN INCL(n,15) END;
  IF b?(bd16,1) THEN INCL(n,16) END; IF b?(bd17,1) THEN INCL(n,17) END;
  IF b?(bd18,1) THEN INCL(n,18) END; IF b?(bd19,1) THEN INCL(n,19) END;
  IF b?(bd20,1) THEN INCL(n,20) END; IF b?(bd21,1) THEN INCL(n,21) END;
  IF b?(bd22,1) THEN INCL(n,22) END; IF b?(bd23,1) THEN INCL(n,23) END;
  IF b?(bd24,1) THEN INCL(n,24) END; IF b?(bd25,1) THEN INCL(n,25) END;
  IF b?(bd26,1) THEN INCL(n,26) END; IF b?(bd27,1) THEN INCL(n,27) END;
  IF b?(bd28,1) THEN INCL(n,28) END; IF b?(bd29,1) THEN INCL(n,29) END;
  IF b?(bd30,1) THEN INCL(n,30) END; IF b?(bd31,1) THEN INCL(n,31) END;
  RETURN n;
END bd?;

PROCEDURE bd!(n: WORD);
  VAR s: BITSET;
BEGIN
  s:=n;
  a(bd00,INTEGER( 0 IN s)); a(bd01,INTEGER( 1 IN s));
  a(bd02,INTEGER( 2 IN s)); a(bd03,INTEGER( 3 IN s));
  a(bd04,INTEGER( 4 IN s)); a(bd05,INTEGER( 5 IN s));
  a(bd06,INTEGER( 6 IN s)); a(bd07,INTEGER( 7 IN s));
  a(bd08,INTEGER( 8 IN s)); a(bd09,INTEGER( 9 IN s));
  a(bd10,INTEGER(10 IN s)); a(bd11,INTEGER(11 IN s));
  a(bd12,INTEGER(12 IN s)); a(bd13,INTEGER(13 IN s));
  a(bd14,INTEGER(14 IN s)); a(bd15,INTEGER(15 IN s));
  a(bd16,INTEGER(16 IN s)); a(bd17,INTEGER(17 IN s));
  a(bd18,INTEGER(18 IN s)); a(bd19,INTEGER(19 IN s));
  a(bd20,INTEGER(20 IN s)); a(bd21,INTEGER(21 IN s));
  a(bd22,INTEGER(22 IN s)); a(bd23,INTEGER(23 IN s));
  a(bd24,INTEGER(24 IN s)); a(bd25,INTEGER(25 IN s));
  a(bd26,INTEGER(26 IN s)); a(bd27,INTEGER(27 IN s));
  a(bd28,INTEGER(28 IN s)); a(bd29,INTEGER(29 IN s));
  a(bd30,INTEGER(30 IN s)); a(bd31,INTEGER(31 IN s));
END bd!;

PROCEDURE wr_adr(n: INTEGER);
BEGIN
  a(bin,1); a(bout,0);
  a(madr0,1); a(madr1,1); a(madr2,1); a(madr3,1);
  bd!(BITSET(n)/{0..31});
  output(
    bd00,bd01,bd02,bd03,bd04,bd05,bd06,bd07,
    bd08,bd09,bd10,bd11,bd12,bd13,bd14,bd15,
    bd16,bd17,bd18,bd19,bd20,bd21,bd22,bd23,
    bd24,bd25,bd26,bd27,bd28,bd29,bd30,bd31);
  b(bfa,0); b(bfb,0);
  IF stp THEN
    print('Производится запись в адресный регистр адреса %$8h.\n',n);
    wait_s;
  END;
  b(bfa,1); b(bfb,1);
END wr_adr;

PROCEDURE wr_dat(n,v: WORD);
  VAR v1,v2: WORD; ei: BITSET;
BEGIN
  wr_adr(n);
  a(madr0,0);
  bd!(v);
  b(bfa,0); b(bfb,0);
  IF stp THEN
    print('Производится запись слова %$8h по адресу %$8h.\n',v,n);
    wait_s;
  END;
  b(bfa,1); b(bfb,1);
  input(
    bd00,bd01,bd02,bd03,bd04,bd05,bd06,bd07,
    bd08,bd09,bd10,bd11,bd12,bd13,bd14,bd15,
    bd16,bd17,bd18,bd19,bd20,bd21,bd22,bd23,
    bd24,bd25,bd26,bd27,bd28,bd29,bd30,bd31);
  a(bout,1);
END wr_dat;

PROCEDURE rd_dat(n: WORD): WORD;
  VAR v: WORD;
BEGIN
  wr_adr(n);
  input(
    bd00,bd01,bd02,bd03,bd04,bd05,bd06,bd07,
    bd08,bd09,bd10,bd11,bd12,bd13,bd14,bd15,
    bd16,bd17,bd18,bd19,bd20,bd21,bd22,bd23,
    bd24,bd25,bd26,bd27,bd28,bd29,bd30,bd31);
  a(madr0,0);
  a(bin,0); a(bout,1);
  b(bfa,0); b(bfb,0);
  v:=bd?();
  IF stp THEN
    print('Производится чтение слова по адресу %$8h.\n',n);
    print('Прочитано %$8h.\n',v);
    wait_s;
  END;
  b(bfa,1); b(bfb,1);
  RETURN v;
END rd_dat;

PROCEDURE test_dat;
  VAR i: INTEGER; rd,wr: BITSET;
BEGIN
  FOR i:=0 TO 31 DO
    wr:={i}/{0..31};
    wr_dat(base,wr);
    rd:=rd_dat(base);
    IF rd#wr THEN
      print('Ошибка прохождения данных:\n'
            '   адрес %$5h, записано %$8h, прочитано %$8h, разница %$8h.\n',
            base,wr,rd,wr/rd);
      wait;
    END;
  END;
END test_dat;

PROCEDURE wr2_rd(n1,v1,n2,v2: WORD): WORD;
BEGIN
  wr_dat(n1,v1);
  wr_dat(n2,v2);
  RETURN rd_dat(n1);
END wr2_rd;

PROCEDURE test_adr;
  VAR i,rd: INTEGER;
BEGIN
  FOR i:=0 TO 30 DO
    rd:=wr2_rd(base,INTEGER(BITSET(base)/{i}),0EEEEEEEEh,0FFFFFFFFh);
    IF rd#0EEEEEEEEh THEN
      print('Ошибка прохождения адреса, бит %d.\n',i); wait;
    END;
    rd:=wr2_rd(base+1FFFFh,INTEGER((BITSET(base)+{0..16})/{i}),
               0EEEEEEEEh,0FFFFFFFFh);
    IF rd#0EEEEEEEEh THEN
      print('Ошибка прохождения адреса, бит %d.\n',i); wait;
    END;
  END;
END test_adr;

VAR i: INTEGER;

BEGIN
  base:=1F8000h;
  FOR i:=1 TO 192 DO input(i) END;
  FOR i:=1 TO 192 DO a(i,0) END;
  a(binhibit,1); a(bmcl,1); a(bfa,1); a(bfb,1);
  IF b?(bfa,0) THEN END;
  power(TRUE,0);
  output(bfa,bfb,madr0,madr1,madr2,madr3,bin,bout,binhibit,bmcl);
  test_dat;
  test_adr;
END vga6.
