MODULE alu6; (* 02-Nov-88. (c) KRONOS *)

FROM Terminal   IMPORT  Read, print, BusyRead;
FROM SYSTEM     IMPORT  WORD, ADR, ADDRESS;
FROM Random     IMPORT  Rand;
FROM UTK6       IMPORT  input, output, b?, b, a, power;

-- программа управления стендом для тестирования модулей ЖШСИ 010.02

CONST
-- out
  bmF0  = 97;
  bmF1  = 98;
  bmF2  = 99;
  bmF3  =100;
  bmF4  =101;
  bmF5  =102;
  bmF6  =103;
  bmF7  =104;
  bmAA0 =105;
  bmAA1 =106;
  bmAA2 =107;
  bmAA3 =108;
  bmAB0 =109;
  bmAB1 =110;
  bmAB2 =111;
  bmAB3 =112;
  bmRDA =113;
  bmRDB =114;
  bmWRA =115;
  bmWRB =116;
  bmCI0 =117;
  bmCI1 =118;
  bmALUEN   =119;
  bmSHIFTEN =120;
  bmRDSTK   =121;
  bmWRSTK   =122;
  bmRDCONST =123;
  bmSREG    =124;
  bmNEXT    =125;
  bmBUSD0   =126;
  bmBUSD1   =127;
  brestart  =128;
  const0    =161;
  const1    =162;
  const2    =163;
  const3    =164;
  const4    =165;
  const5    =166;
  const6    =167;
  const7    =168;
  bmCOND0   =169;
  bmCOND1   =170;
  bmCOND2   =171;
  bmCOND3   =172;
  bmDECCNT  =173;
  bfa       = 19;
  bfb       = 71;

  bd00  =  3;     bd16 = 11;
  bd01  = 55;     bd17 = 63;
  bd02  =  4;     bd18 = 12;
  bd03  = 56;     bd19 = 64;
  bd04  =  5;     bd20 = 13;
  bd05  = 57;     bd21 = 65;
  bd06  =  6;     bd22 = 14;
  bd07  = 58;     bd23 = 66;
  bd08  =  7;     bd24 = 15;
  bd09  = 59;     bd25 = 67;
  bd10  =  8;     bd26 = 16;
  bd11  = 60;     bd27 = 68;
  bd12  =  9;     bd28 = 17;
  bd13  = 61;     bd29 = 69;
  bd14  = 10;     bd30 = 18;
  bd15  = 62;     bd31 = 70;

  cash0 =177;
  cash1 =178;
  cash2 =179;
  cash3 =180;
  cash4 =181;
  cash5 =182;
  cash6 =183;
  cash7 =184;
  cash8 =192;

  flag  =175;
  intreq=176;

VAR stp: BOOLEAN;

PROCEDURE ?1(n: INTEGER): BOOLEAN;
BEGIN
  RETURN b?(n,1);
END ?1;

PROCEDURE ?0(n: INTEGER): BOOLEAN;
BEGIN
  RETURN b?(n,0);
END ?0;

PROCEDURE wait;
  VAR ch: CHAR;
BEGIN
  IF b?(bfa,0) THEN END;
  print('Нажмите любую клавишу для продолжения...\r');
  ch:=Read();
  print('                                        \n');
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

PROCEDURE const(n: WORD);
  VAR s: BITSET;
BEGIN
  s:=n;
  a(const0,INTEGER(0 IN s)); a(const1,INTEGER(1 IN s));
  a(const2,INTEGER(2 IN s)); a(const3,INTEGER(3 IN s));
  a(const4,INTEGER(4 IN s)); a(const5,INTEGER(5 IN s));
  a(const6,INTEGER(6 IN s)); a(const7,INTEGER(7 IN s));
END const;

PROCEDURE testbd;
  VAR n,v: BITSET; i: INTEGER;
BEGIN
  n:=bd?();
  IF n#{0..31} THEN
    print('На всех разрядах шины данных должен быть высокий уровень.\n');
    print('Посмотри разряды %{}.\n',n/{0..31});
    wait;
  END;
  output(bd00,bd01,bd02,bd03,bd04,bd05,bd06,bd07,
         bd08,bd09,bd10,bd11,bd12,bd13,bd14,bd15);
  v:={1..31};
  FOR i:=0 TO 15 DO
    bd!(v); n:=bd?();
    IF n#v THEN
      print(
 'На шине данных BD0..BD31 низкий уровень должен быть только в разряде %d.\n',
             i);
      print('Посмотри разряды %{}.\n',n/v);
      wait;
    END;
    v:=v<<1;
  END;
  input(bd00,bd01,bd02,bd03,bd04,bd05,bd06,bd07,
        bd08,bd09,bd10,bd11,bd12,bd13,bd14,bd15);
  b(bmBUSD1,0); b(brestart,1);
  n:=bd?();
  IF n#{0..31} THEN
    print('На всех разрядах шины B должен быть высокий уровень.\n');
    print('Посмотри разряды %{}.\n',n/{0..31});
    wait;
  END;
  b(bmBUSD1,1);
END testbd;

PROCEDURE testconst;
  VAR n: BITSET; i: INTEGER;
BEGIN
  b(bmBUSD1,0); b(brestart,1);
  b(bmRDCONST,1);
  FOR i:=0 TO 7 DO
    const({0..7}-{i}); n:=bd?();
    IF n#({0..31}-{i}) THEN
      print('Включен генератор констант D50.\n');
      print('На шине B низкий уровень должен быть только в разряде %d.\n',i);
      print('Посмотри разряды %{}.\n',n/({0..31}-{i}));
      wait;
    END;
  END;
  b(bmRDCONST,0);
  b(bmBUSD1,1);
END testconst;

PROCEDURE regrd(n: INTEGER): WORD;
  VAR v: WORD;
BEGIN
  IF 0 IN BITSET(n) THEN a(bmAB0,1) ELSE a(bmAB0,0) END;
  IF 1 IN BITSET(n) THEN a(bmAB1,1) ELSE a(bmAB1,0) END;
  IF 2 IN BITSET(n) THEN a(bmAB2,1) ELSE a(bmAB2,0) END;
  IF 3 IN BITSET(n) THEN a(bmAB3,1) ELSE a(bmAB3,0) END;
  a(bmRDB,1); a(bmBUSD1,0);
  v:=bd?();
  IF stp THEN
    print('Производится чтение из регистрового файла D14..D21.\n');
    print('Регистр %d, прочитано (шина B) %$8h.\n',n,v);
    wait_s;
  END;
  a(bmBUSD1,1); a(bmRDB,0);
  RETURN v;
END regrd;

PROCEDURE regrdA(n: INTEGER): WORD;
  VAR v: WORD;
BEGIN
  a(bmF0,0); a(bmF1,1); a(bmF2,0); a(bmF3,1);
  a(bmF4,1); a(bmF5,1); a(bmF6,0); a(bmF7,1);
  a(bmCI0,1); a(bmCI1,1); -- ALU=A
  a(bmAA0,INTEGER(0 IN BITSET(n)));
  a(bmAA1,INTEGER(1 IN BITSET(n)));
  a(bmAA2,INTEGER(2 IN BITSET(n)));
  a(bmAA3,INTEGER(3 IN BITSET(n)));
  a(bmRDA,1);
  a(bmALUEN,1);
  IF stp THEN
    print('Производится чтение из регистрового файла D14..D21.\n');
    print('Регистр %d, чтение по шине A.\n',n);
    print('Прочитанные данные будут переданы на шину B через АЛУ.\n');
    wait_s;
  END;
  b(bfa,0); b(bfb,0);
  b(bmBUSD1,0); v:=bd?();
  IF stp THEN
    print('Из АЛУ получено %$8h.\n',v); wait_s;
  END;
  b(bmBUSD1,1);
  b(bfa,1); b(bfb,1);
  a(bmALUEN,0);
  a(bmRDA,0);
  RETURN v;
END regrdA;

PROCEDURE regwr(n: INTEGER; v: WORD);
BEGIN
  a(bmAB0,INTEGER(0 IN BITSET(n)));
  a(bmAB1,INTEGER(1 IN BITSET(n)));
  a(bmAB2,INTEGER(2 IN BITSET(n)));
  a(bmAB3,INTEGER(3 IN BITSET(n)));
  bd!(v);
  output(bd00,bd01,bd02,bd03,bd04,bd05,bd06,bd07,
         bd08,bd09,bd10,bd11,bd12,bd13,bd14,bd15,
         bd16,bd17,bd18,bd19,bd20,bd21,bd22,bd23,
         bd24,bd25,bd26,bd27,bd28,bd29,bd30,bd31);
  a(bmBUSD0,1); a(bmWRB,1);
  b(bfa,0); b(bfb,0);
  IF stp THEN
    print('Производится запись в регистровый файл D14..D21.\n');
    print('Регист %d, данные (шина B) %$8h.\n',n,v);
    wait_s;
  END;
  b(bfa,1); b(bfb,1);
  a(bmWRB,0); a(bmBUSD0,0);
  input (bd00,bd01,bd02,bd03,bd04,bd05,bd06,bd07,
         bd08,bd09,bd10,bd11,bd12,bd13,bd14,bd15,
         bd16,bd17,bd18,bd19,bd20,bd21,bd22,bd23,
         bd24,bd25,bd26,bd27,bd28,bd29,bd30,bd31);
END regwr;

PROCEDURE regcp(n: INTEGER);
BEGIN
  a(bmAA0,INTEGER(0 IN BITSET(n)));
  a(bmAA1,INTEGER(1 IN BITSET(n)));
  a(bmAA2,INTEGER(2 IN BITSET(n)));
  a(bmAA3,INTEGER(3 IN BITSET(n)));
  a(bmAB0,INTEGER(0 IN BITSET(n)));
  a(bmAB1,INTEGER(1 IN BITSET(n)));
  a(bmAB2,INTEGER(2 IN BITSET(n)));
  a(bmAB3,INTEGER(3 IN BITSET(n)));
  a(bmWRA,1); a(bmRDB,1); a(bmALUEN,1);
  a(bmF0,1); a(bmF1,1); a(bmF2,1); a(bmF3,0);
  a(bmF4,1); a(bmF5,1); a(bmF6,0); a(bmF7,1);
  a(bmCI0,1); a(bmCI1,1); -- ALU=B
  IF stp THEN
    print('Производится чтение из регистрового файла D14..D21.\n');
    print('Регистр %d, чтение по шине B.\n',n);
    print('Прочитанные данные будут переданы на шину A через АЛУ.\n');
    wait_s;
  END;
  b(bfa,0); b(bfb,0);
  IF stp THEN
    print('Производится запись регистрового файла D14..D21.\n');
    print('Регистр %d, запись по шине A.\n',n);
    print('Данные выдаются на шину из АЛУ.\n');
    wait_s;
  END;
  b(bfa,1); b(bfb,1);
  a(bmWRA,0); a(bmRDB,0); a(bmALUEN,0);
END regcp;

PROCEDURE testrg;
  VAR v,n,n1,l,m: BITSET; p,i,i1: INTEGER;
BEGIN
  FOR p:=0 TO 2 DO
    v:=BITSET({0,3,6,9,12,15,18,21,24,27,30}<<p);
    n:=v;
    FOR i:=0 TO 15 DO regwr(i,n); n:=n<<1 END;
    n:=v;
    FOR i:=0 TO 15 DO
      l:=regrd(i);
      IF l#n THEN
        print('Ошибка записи регистрового файла D14..D21.\n');
        print('Регистр %2d, записано %$8h, прочитано (bus B) %$8h,'
              ' разница %$8h\n',i,n,l,l/n);
        wait_s;
      END;
      n:=n<<1;
    END;
    n:=v;
    FOR i:=0 TO 15 DO
      regcp(i);
      l:=regrdA(i);
      IF l#n THEN
        print('Ошибка записи регистрового файла D14..D21.\n');
        print('Регистр %2d, записано %$8h, прочитано (bus A) %$8h,'
              ' разница %$8h\n',i,n,l,l/n);
        wait_s;
      END;
      n:=n<<1;
    END;
    v:=v<<1;
  END;
  stp:=FALSE;
END testrg;

PROCEDURE cash?(): WORD;
  VAR s: BITSET;
BEGIN
  s:={};
  IF b?(cash0,1) THEN INCL(s,0) END;
  IF b?(cash1,1) THEN INCL(s,1) END;
  IF b?(cash2,1) THEN INCL(s,2) END;
  IF b?(cash3,1) THEN INCL(s,3) END;
  IF b?(cash4,1) THEN INCL(s,4) END;
  IF b?(cash5,1) THEN INCL(s,5) END;
  IF b?(cash6,1) THEN INCL(s,6) END;
  IF b?(cash7,1) THEN INCL(s,7) END;
  RETURN s;
END cash?;

PROCEDURE testcash;
  VAR i,j,m: INTEGER; k,n: BITSET;
BEGIN
  a(bmBUSD1,0); a(brestart,1);
  LOOP
    a(bmAB3,0); a(bmAB2,1); a(bmSREG,1);
    b(bfb,0); b(bfb,1); i:=ORD(?0(bd00))+ORD(?0(bd01))*2;
    b(bfb,0);
    a(bmAB0,1); a(bmAB1,1); a(bmAB2,0); a(bmAB3,1);
    WHILE i>0 DO b(bfb,1);
      IF stp THEN
        print('На вход +1 счетчика D55 подано LOW.\n');
        print('После изменения входа в HIGH долно изменится состояние счетчика.\n');
        wait;
      END;
      b(bfb,0); DEC(i);
      IF stp THEN
        print('Сработало?\n'); wait;
      END;
    END;
    -- pc must be 0
    a(bmAB3,0); a(bmAB2,1);
    b(bfb,1); i:=ORD(?0(bd00))+ORD(?0(bd01))*2;
    IF i=0 THEN
      stp:=FALSE; EXIT;
    ELSE
      print('Не могу установить PC в 0 !\r');
      print('D55(3), D55(2), D58(6), D58(10) должны быть HIGH.\n');
      wait_s;
    END;
  END;
  a(bmBUSD1,1);
  a(bmRDA,1);
  FOR i:=0 TO 31 DO
    regwr(0,{0..31}-{i});
    a(bmF0,0); a(bmF1,1); a(bmF2,0); a(bmF3,1);
    a(bmF4,1); a(bmF5,1); a(bmF6,0); a(bmF7,1);
    a(bmCI0,1); a(bmCI1,1); -- ALU=A
    a(bmAA0,0); a(bmAA1,0); a(bmAA2,0); a(bmAA3,0);
    a(bmALUEN,1);
    a(bmAB3,1); a(bmAB2,0); a(bmAB1,0); a(bmAB0,0);
    a(bmSREG,1);
    IF stp THEN
      print('Число %8$h передается из регистрового файла в АЛУ по шине B,\n',
             {0..31}-{i});
      print('Затем оно будет передано из АЛУ в регистры кэша.\n');
      wait_s;
    END;
    b(bfa,0); b(bfb,0);
    IF stp THEN
      print('Производится запись в регистры кэша (D46,D47,D48,D49).\n');
      print('Записываеися число %$8h.\n',{0..31}-{i});
      wait_s;
    END;
    b(bfa,1); b(bfb,1);
    a(bmSREG,0);
    a(bmALUEN,0);
    a(bmBUSD1,0);
    FOR j:=0 TO 3 DO
      a(bmSREG,1); a(bmAB3,0); a(bmAB2,1);
      m:=ORD(?0(bd00))+ORD(?0(bd01))*2+ORD(?0(bd02))*4;
      IF j#m THEN
        print('Не сработал счетчик кэша (D55, D58).\n');
        IF 0 IN BITSET(j) THEN
          print('D55(3), D58(6) должны быть LOW.\n');
        ELSE
          print('D55(3), D58(6) должны быть HIGH.\n');
        END;
        IF 1 IN BITSET(j) THEN
          print('D55(2), D58(10) должны быть LOW.\n');
        ELSE
          print('D55(2), D58(10) должны быть HIGH.\n');
        END;
        wait_s;
      END;
      a(bmSREG,0);
      k:=cash?(); n:={0..7}-BITSET({i}>>(j*8));
      IF n#k THEN
        print('Ошибка записи в регистр D%d.\n',46+j);
        print('Записано %$2h, прочитано %$2h, разница %$2h.\n',n,k,n/k);
        wait_s;
      END;
      b(bfa,0); b(bfb,0);
      b(bmNEXT,1);
      b(bfa,1); b(bfb,1);
      IF stp THEN
        print('На вход -1 счетчика D55 подано LOW.\n');
        print('После изменения входа в HIGH долно изменится состояние счетчика.\n');
        wait_s;
      END;
      b(bmNEXT,0); b(bfa,0); b(bfb,0);
      IF stp THEN
        print('Сработало?\n'); wait_s;
      END;
      b(bfa,1); b(bfb,1);
    END;
    b(bmSREG,1); b(bmAB3,0); b(bmAB2,1);
    m:=ORD(?0(bd00))+ORD(?0(bd01))*2+ORD(?0(bd02))*4;
    IF 4#m THEN
      print('D58(13), D25(9) должно быть LOW.\n');
      print('D58(6), D58(10) должно быть HIGH.\n');
      wait_s;
    END;
    b(bmSREG,0);
    b(bmBUSD1,1);
  END;
  b(bmRDA,0);
  stp:=FALSE;
END testcash;

PROCEDURE testsh;
  PROCEDURE loadcnt(n: WORD);
  BEGIN
    bd!(n);
    output(bd00,bd01,bd02,bd03,bd04,bd05,bd06,bd07,
           bd08,bd09,bd10,bd11,bd12,bd13,bd14,bd15);
    b(bmBUSD0,1);
    b(bmAB0,1); b(bmAB1,0); b(bmAB2,0); b(bmAB3,1);
    b(bmSREG,1);
    b(bfb,1); b(bfa,0); b(bfb,0);
    IF stp THEN
      print('Производится запись в счетчик сдвигов D56, D57.\n');
      print('Записываися число %$8h.\n',n);
      wait_s;
    END;
    b(bfa,1); b(bmSREG,0); b(bmBUSD0,0);
  END loadcnt;
  VAR i,j: INTEGER; n: BITSET;
BEGIN
  b(bfa,1); b(bfb,0);
  LOOP
    loadcnt(-1);
    b(bmCOND0,0); b(bmCOND1,1); b(bmCOND2,1); b(bmCOND3,1);
    b(bmDECCNT,1);
    IF ?0(intreq) THEN
      print('Сигнал INTREQ (D22(6)) должен быть HIGH.\n'); wait_s;
    END;
    b(bmDECCNT,0);
    IF ?1(intreq) THEN
      print('Сигнал INTREQ (D22(6)) должен быть LOW.\n'); wait_s;
    ELSE
      stp:=FALSE; EXIT;
    END;
  END;
  LOOP
    loadcnt({3..31});
    b(bmDECCNT,1);
    IF ?1(intreq) THEN
      print('Сигнал INTREQ (D22(6)) должен быть LOW.\n'); wait_s;
    ELSE
      FOR i:=0 TO 6 DO
        b(bfb,1); b(bfa,0); b(bfb,0);
        IF stp THEN
          print('На вход +1 счетчика D56 подано LOW.\n');
          print('После изменения сигнала на входе в HIGH,\n');
          print('состояние счетчика должно изменится.\n');
          wait_s;
        END;
        b(bfa,1);
        IF stp THEN print('Сработало?\n'); wait_s END;
      END;
      IF ?0(intreq) THEN
        print('Чтото не то со счетчиком D56.\n');
        print('Он должен был досчитать до состояния HIGH на всех выходах.\n');
        wait_s;
      ELSE
        b(bmDECCNT,0); stp:=FALSE; EXIT;
      END;
    END;
    b(bmDECCNT,0);
  END;
  LOOP
    loadcnt({0..2,5..31});
    b(bmDECCNT,1);
    IF ?1(intreq) THEN
      print('Сигнал INTREQ (D22(6)) должен быть LOW.\n'); wait_s;
    ELSE
      FOR i:=0 TO 2 DO
        b(bfb,1); b(bfa,0); b(bfb,0);
        IF stp THEN
          print('На вход +1 счетчика D57 подано LOW.\n');
          print('После изменения сигнала на входе в HIGH,\n');
          print('состояние счетчика должно изменится.\n');
          wait_s;
        END;
        b(bfa,1);
        IF stp THEN print('Сработало?\n'); wait_s END;
      END;
      IF ?0(intreq) THEN
        print('Чтото не то со счетчиком D57.\n');
        print('Он должен был досчитать до состояния HIGH на всех выходах.\n');
        wait_s;
      ELSE
        b(bmDECCNT,0); stp:=FALSE; EXIT;
      END;
    END;
    b(bmDECCNT,0);
  END;
  b(bmF0,1); b(bmF1,1); b(bmF2,1); b(bmF3,0);
  b(bmF4,0); b(bmF5,1); b(bmF6,0); b(bmF7,0);
  b(bmCI0,1); b(bmCI1,1); -- ALU=ROL(B);
  FOR i:=0 TO 31 DO
    loadcnt({0..31}/BITSET(i));
    regwr(0,{1..31});
    b(bmSHIFTEN,1); b(bmALUEN,1); b(bmRDB,1); b(bmWRB,1); b(bmDECCNT,1);
    WHILE ?0(intreq) DO
      b(bfb,1); b(bfa,0); b(bfb,0);
      IF stp THEN
        print('Производиться операция сдвига.\n');
        print('Проверьте что все нормально.\n');
        wait_s;
      END;
      b(bfa,1);
    END;
    b(bfb,1); b(bfa,0); b(bfb,0); b(bfa,1);
    b(bmBUSD1,0);
    b(bfb,1); n:=bd?(); b(bfa,0); b(bfb,0); b(bfa,1);
    b(bmBUSD1,1);
    IF n#({1..31}<<i) THEN
      print('Ошибка в работе сдвигателя (D42..D45).\n');
      print('Записано %$8h, число сдвигов %2d, прочитано %$8h,'
            ' должно быть %$8h.\n',{1..31},i,n,{1..31}<<i);
      wait_s;
    END;
    b(bmSHIFTEN,0); b(bmALUEN,0); b(bmRDB,0); b(bmWRB,0); b(bmDECCNT,0);
  END;
END testsh;

PROCEDURE teststk;
  PROCEDURE wrstk(v: WORD);
  BEGIN
    a(bmAB0,1); a(bmAB1,0); a(bmAB2,0); a(bmAB3,0);
    a(bmAA0,1); a(bmAA1,0); a(bmAA2,0); a(bmAA3,0);
    bd!(v);
    output(bd00,bd01,bd02,bd03,bd04,bd05,bd06,bd07,
           bd08,bd09,bd10,bd11,bd12,bd13,bd14,bd15,
           bd16,bd17,bd18,bd19,bd20,bd21,bd22,bd23,
           bd24,bd25,bd26,bd27,bd28,bd29,bd30,bd31);
    a(bmBUSD0,1);
    a(bmWRB,1); b(bfb,1); b(bfa,0); b(bfb,0); b(bfa,1); a(bmWRB,0);
    a(bmBUSD0,0);
    input (bd00,bd01,bd02,bd03,bd04,bd05,bd06,bd07,
           bd08,bd09,bd10,bd11,bd12,bd13,bd14,bd15,
           bd16,bd17,bd18,bd19,bd20,bd21,bd22,bd23,
           bd24,bd25,bd26,bd27,bd28,bd29,bd30,bd31);
    a(bmRDA,1); a(bmWRSTK,0);
    b(bfb,1);
    IF stp THEN
      print('Производится запись числа %$8h в стек (D26..D33).\n',v);
      wait_s;
    END;
    b(bfa,0); b(bfb,0); b(bfa,1);
    a(bmRDA,0); a(bmWRSTK,1);
  END wrstk;
  PROCEDURE rdstk(): WORD;
    VAR n: WORD;
  BEGIN
    a(bmAB0,1); a(bmAB1,0); a(bmAB2,0); a(bmAB3,0);
    a(bmAA0,1); a(bmAA1,0); a(bmAA2,0); a(bmAA3,0);
    a(bmWRA,1); a(bmRDSTK,0);
    b(bfb,1); b(bfa,0);
    IF stp THEN
      print('Производится чтение из стека (D26..D33).\n');
      print('Прочитанные данные записываются в регист 1'
            ' регистрового файла.\n');
      wait_s;
    END;
    b(bfb,0); b(bfa,1);
    a(bmWRA,0); a(bmRDSTK,1);
    a(bmBUSD1,0); a(bmRDB,1);
    b(bfb,1); n:=bd?(); b(bfa,0); b(bfb,0); b(bfa,1);
    a(bmBUSD1,1); a(bmRDB,0);
    RETURN n;
  END rdstk;
  VAR i,j: INTEGER; m,n: BITSET;
BEGIN
  b(bfb,0); b(bfa,1);
  b(bmWRSTK,0);
  FOR i:=0 TO 31 DO
    b(bfb,1); b(bfa,0); b(bfb,0); b(bfa,1);
  END;
  a(bmWRSTK,1);
  FOR j:=0 TO 9 DO
    FOR i:=0 TO 15 DO
      m:={0..31}-{(i+j*3) MOD 32}; wrstk(m);
    END;
    FOR i:=15 TO 0 BY -1 DO
      m:={0..31}-{(i+j*3) MOD 32}; n:=rdstk();
      IF n#m THEN
        print('Ошибка записи/чтения стека (D26..D33).\n');
        print('Записано %$8h, прочитано %$8h, разница %$8h.\n',m,n,m/n);
        wait_s;
      END;
    END;
    n:=rdstk();
  END;
END teststk;

PROCEDURE testflag;
  VAR mx_adr: INTEGER;
  PROCEDURE cond(n: INTEGER);
  BEGIN
    mx_adr:=n;
    a(bmCOND0,INTEGER(0 IN BITSET(n)));
    a(bmCOND1,INTEGER(1 IN BITSET(n)));
    a(bmCOND2,INTEGER(2 IN BITSET(n)));
    a(bmCOND3,INTEGER(3 IN BITSET(n)));
  END cond;
  PROCEDURE intreq1;
  BEGIN
    IF ?0(intreq) THEN
      print('На адресные входы мультиплексора флагов'
            ' D22, D23 подано %d.\n',mx_adr);
      print('Сигнал INTREQ (D22(6)) должен быть HIGH.\n'); wait_s;
    END;
  END intreq1;
  PROCEDURE intreq0;
  BEGIN
    IF ?1(intreq) THEN
      print('На адресные входы мультиплексора флагов'
            ' D22, D23 подано %d.\n',mx_adr);
      print('Сигнал INTREQ (D22(6)) должен быть LOW.\n'); wait_s;
    END;
  END intreq0;
  VAR i,n: INTEGER;
BEGIN
  cond(8); intreq0; cond(9); intreq1; cond(11); intreq1;
  cond(12); intreq1; cond(13); intreq0;
  b(bfb,0);
  LOOP
    b(bfa,1);
    cond(9); b(bfa,0); b(bfa,1);
    b(bfb,1);
    IF ?1(flag) THEN
      print('Сигнал FLAG (D24(8)) должен быть LOW.\n'); wait;
    ELSE
      EXIT;
    END;
  END;
  b(bfb,0);
  cond(8); b(bfa,0); b(bfa,1);
  IF ?1(flag) THEN
    print('Не работает триггер D24.1 .. D24.4\n'); wait;
  END;
  cond(0);
  intreq1;
  b(bfb,1);
  IF ?0(flag) THEN
    print('Не работает триггер D24.1 .. D24.4\n'); wait;
  END;
  intreq0;
  b(bfb,0);
  cond(9); b(bfa,0); b(bfa,1);
  IF ?0(flag) THEN
    print('Не работает триггер D24.1 .. D24.4\n'); wait;
  END;
  cond(0);
  intreq0;
  b(bfb,1);
  IF ?1(flag) THEN
    print('Не работает триггер D24.1 .. D24.4\n'); wait;
  END;
  intreq1;
  cond(9); b(bfa,0); b(bfb,0); b(bfa,1); b(bfb,1);
  cond(6);
  intreq0;
  FOR i:=0 TO 7 DO
    b(bfa,0); intreq0;
    b(bfb,0); intreq0;
    b(bfa,1); intreq0;
    b(bfb,1); intreq1;
    b(bfa,0); intreq1;
    b(bfb,0); intreq1;
    b(bfa,1); intreq1;
    b(bfb,1); intreq0;
  END;
  regwr(0,0FFFFFFFFh);
  a(bmF0,0); a(bmF1,1); a(bmF2,0); a(bmF3,1);
  a(bmF4,1); a(bmF5,1); a(bmF6,0); a(bmF7,1);
  a(bmCI0,1); a(bmCI1,1); -- ALU=A
  a(bmAA0,0); a(bmAA1,0); a(bmAA2,0); a(bmAA3,0); a(bmRDA,1); -- bus_A=R0
  cond(9); intreq1;
  cond(2); intreq0;
  b(bfa,0); intreq0;
  b(bfb,0); intreq0;
  b(bfa,1); intreq0;
  b(bfb,1); intreq0;
  regwr(0,0FFFFFFFEh);
  a(bmF0,0); a(bmF1,1); a(bmF2,0); a(bmF3,1);
  a(bmF4,0); a(bmF5,1); a(bmF6,0); a(bmF7,0);
  a(bmCI0,1); a(bmCI1,1); a(bmALUEN,1); -- ALU=ROL(A)
  a(bmAA0,0); a(bmAA1,0); a(bmAA2,0); a(bmAA3,0);
  a(bmRDA,1); a(bmWRA,1); -- bus_A=RW_R0
  FOR i:=0 TO 32 DO
    b(bfa,0); intreq1;
    b(bfb,0); intreq1;
    b(bmBUSD1,0); b(brestart,1);
    n:=bd?();
    IF BITSET(n)#(BITSET(1<<(i+1))/{0..31}) THEN
      print('На шине B низкий уровень должен быть только в разряде %d.\n',
             (i+1) MOD 32);
      wait;
    END;
    b(bmBUSD1,1);
    b(bfa,1); intreq1;
    b(bfb,1); intreq1;
  END;
  b(bmRDA,0); b(bmWRA,0); b(bmALUEN,0);
  -- flags 1, 4, 5, 7
  a(bmF0,1); a(bmF1,1); a(bmF2,1); a(bmF3,0);
  a(bmF4,1); a(bmF5,1); a(bmF6,0); a(bmF7,1);
  a(bmCI0,0); a(bmCI1,1);  -- ALU=B+1
  regwr(0,80000000h);
  a(bmRDB,1); a(bmALUEN,1);
  cond(4); intreq0; cond(1); intreq0; cond(5); intreq1; cond(7); intreq0;
  b(bfa,0); b(bfb,0);
  cond(4); intreq1; cond(1); intreq0; cond(5); intreq0; cond(7); intreq1;
  b(bfa,1); b(bfb,1);
  a(bmRDB,0); a(bmALUEN,0);
END testflag;

PROCEDURE testalu;
  VAR i,j: INTEGER; sm,x,y,x1,x2: BITSET;
BEGIN
  -- ADD test
  a(bmF0,0); a(bmF1,1); a(bmF2,1); a(bmF3,0);
  a(bmF4,1); a(bmF5,1); a(bmF6,0); a(bmF7,1);
  -- ALU=A+B
  regwr(1,0FFFFFFFEh);
  FOR i:=0 TO 6 DO
    LOOP
      IF ODD(i) THEN
        a(bmCOND0,0); a(bmCOND1,0); a(bmCOND2,0); a(bmCOND3,1);
      ELSE
        a(bmCOND0,1); a(bmCOND1,0); a(bmCOND2,0); a(bmCOND3,1);
      END;
      regwr(2,0FFFFFFF0h<<(i*4));
      IF ODD(i)#b?(flag,1) THEN
        IF ODD(i) THEN
          print('Сигнал FLAG, D11(8) должен быть HIGH.\n');
        ELSE
          print('Сигнал FLAG, D11(8) должен быть LOW.\n');
        END;
        wait_s;
      ELSE
        EXIT
      END;
    END;
    a(bmAA0,1); a(bmAA1,0); a(bmAA2,0); a(bmAA3,0);
    a(bmAB0,0); a(bmAB1,1); a(bmAB2,0); a(bmAB3,0);
    a(bmRDA,1); a(bmWRA,1); a(bmRDB,1); a(bmALUEN,1); a(bmBUSD1,0);
    a(bmCI0,1); a(bmCI1,1);
    b(bfa,0); b(bfb,0);
    sm:=BITSET(bd?())/{0..31};
    IF sm#{i*4+4} THEN
      print('Ошибка в АЛУ при выполнении команды сложения.\n');
      print('%$8h + %$8h -> %$8h, должно было получиться %$8h.\n',
            {i*4},0Fh<<(i*4),sm,{i*4+4});
      wait_s;
    END;
    a(bmCI0,0); a(bmCI1,1); sm:=BITSET(bd?())/{0..31};
    IF sm#{i*4+4,0} THEN
      print('Сигнал входного переноса D11(5) должен быть HIGH.\n');
      wait_s;
    END;
    IF ODD(i) THEN a(bmCI0,1); a(bmCI1,0) ELSE a(bmCI0,0); a(bmCI1,0) END;
    sm:=BITSET(bd?())/{0..31};
    IF sm#{i*4+4,0} THEN
      print('Сигнал входного переноса D11(5) должен быть HIGH.\n');
      wait_s;
    END;
    IF ODD(i) THEN a(bmCI0,0); a(bmCI1,0) ELSE a(bmCI0,1); a(bmCI1,0) END;
    sm:=BITSET(bd?())/{0..31};
    IF sm#{i*4+4} THEN
      print('Сигнал входного переноса D11(5) должен быть LOW.\n');
      wait_s;
    END;
    b(bfa,1); b(bfb,1);
    a(bmRDA,0); a(bmWRA,0); a(bmRDB,0); a(bmALUEN,0); a(bmBUSD1,1);
    IF sm#{i*4+4} THEN
      regwr(1,{0..31}/{i*4+4});
    END;
  END;
  -- SUB test
  a(bmF0,1); a(bmF1,1); a(bmF2,0); a(bmF3,0);
  a(bmF4,1); a(bmF5,1); a(bmF6,0); a(bmF7,1);
  a(bmCI0,0); a(bmCI1,1); -- ALU=A-B
  regwr(1,0FFFFFFFEh);
  FOR i:=0 TO 6 DO
    x:={};
    FOR j:=31 TO i*4+4 BY -1 DO INCL(x,j) END;
    INCL(x,i*4);
    regwr(2,x/{0..31});
    a(bmAA0,1); a(bmAA1,0); a(bmAA2,0); a(bmAA3,0);
    a(bmAB0,0); a(bmAB1,1); a(bmAB2,0); a(bmAB3,0);
    a(bmRDA,1); a(bmWRA,1); a(bmRDB,1); a(bmALUEN,1);
    b(bfa,0); b(bfb,0);
    IF stp THEN
      print('АЛУ выполняет вычитание %$8h - %$8h.\n',{i*4},x);
      wait_s;
    END;
    b(bfa,1); b(bfb,1);
    a(bmRDA,0); a(bmWRA,0); a(bmRDB,0); a(bmALUEN,0);
    sm:=BITSET(regrd(1))/{0..31};
    IF sm#{i*4+4} THEN
      print('Ошибка в АЛУ при выполнении команды вычитания.\n');
      print('%$8h - %$8h -> %$8h, должно было получиться %$8h.\n',
            {i*4},x,sm,{i*4+4});
      wait_s;
      regwr(1,{0..31}/{i*4+4});
    END;
  END;
  -- ROL test
  a(bmF0,0); a(bmF1,1); a(bmF2,0); a(bmF3,1);
  a(bmF4,0); a(bmF5,1); a(bmF6,0); a(bmF7,0);
  a(bmCI0,1); a(bmCI1,1); -- ALU=ROL(A)
  FOR i:=0 TO 3 DO
    regwr(1,{i*8+7}/{0..31});
    a(bmAA0,1); a(bmAA1,0); a(bmAA2,0); a(bmAA3,0);
    a(bmRDA,1); a(bmWRA,1); a(bmALUEN,1);
    b(bfa,0); b(bfb,0);
    IF stp THEN
      print('АЛУ выполняет циклический сдвиг влево %$8h.\n',{i*8+7});
      wait_s;
    END;
    b(bfa,1); b(bfb,1);
    a(bmRDA,0); a(bmWRA,0); a(bmALUEN,0);
    sm:=BITSET(regrd(1))/{0..31};
    IF sm#{(i*8+8) MOD 32} THEN
      print('Ошибка в АЛУ при выполнении циклического сдвига влево.\n');
      print('%$8h -> %$8h, должно было получиться %$8h.\n',
            {i*8+7},sm,{(i*8+8) MOD 32});
      wait_s;
      regwr(1,{0..31}/{(i*8+8) MOD 32});
    END;
  END;
  -- ROR test
  a(bmF0,0); a(bmF1,1); a(bmF2,0); a(bmF3,1);
  a(bmF4,1); a(bmF5,1); a(bmF6,0); a(bmF7,0);
  a(bmCI0,1); a(bmCI1,1); -- ALU=ROL(A)
  FOR i:=0 TO 3 DO
    regwr(1,{(i*8+8) MOD 32}/{0..31});
    a(bmAA0,1); a(bmAA1,0); a(bmAA2,0); a(bmAA3,0);
    a(bmRDA,1); a(bmWRA,1); a(bmALUEN,1);
    b(bfa,0); b(bfb,0);
    IF stp THEN
      print('АЛУ выполняет циклический сдвиг вправо %$8h.\n',{(i*8+8)});
      wait_s;
    END;
    b(bfa,1); b(bfb,1);
    a(bmRDA,0); a(bmWRA,0); a(bmALUEN,0);
    sm:=BITSET(regrd(1))/{0..31};
    IF sm#{i*8+7} THEN
      print('Ошибка в АЛУ при выполнении циклического сдвига вправо.\n');
      print('%$8h -> %$8h, должно было получиться %$8h.\n',
            {(i*8+8) MOD 32},sm,{i*8+7});
      wait_s;
      regwr(1,{0..31}/{i*8+7});
    END;
  END;
  -- AND test
  a(bmF0,0); a(bmF1,0); a(bmF2,0); a(bmF3,0);
  a(bmF4,1); a(bmF5,1); a(bmF6,0); a(bmF7,1);
  a(bmCI0,1); a(bmCI1,1); -- ALU=A_AND_B
  LOOP
    x1:={0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30};
    x2:={0,1,4,5,8,9,12,13,16,17,20,21,24,25,28,29};
    y:=x1+x2;
    LOOP
      regwr(1,x1); regwr(2,x2);
      a(bmAA0,1); a(bmAA1,0); a(bmAA2,0); a(bmAA3,0);
      a(bmAB0,0); a(bmAB1,1); a(bmAB2,0); a(bmAB3,0);
      a(bmRDA,1); a(bmRDB,1); a(bmALUEN,1); b(bmBUSD1,0);
      sm:=bd?();
      IF sm#x2 THEN
        print('Ошибка записи регистрового файла D14..D21.\n');
        print('Регистр %2d, записано %$8h, прочитано (bus B) %$8h,'
              ' разница %$8h\n',2,x2,sm,x2/sm);
        wait_s;
      ELSE
        EXIT;
      END;
    END;
    b(bfa,0); b(bfb,0);
    sm:=bd?();
    IF sm#y THEN
      print('Ошибка в АЛУ при выполнении операции AND.\n');
      print('%$8h AND %$8h -> %$8h, должно было получиться %$8h.\n',
            x1,x2,sm,y);
      wait_s;
    END;
    b(bfa,1); b(bfb,1);
    a(bmRDA,0); a(bmRDB,0); a(bmALUEN,0); a(bmBUSD1,1);
    IF sm=y THEN EXIT END;
  END;
  -- P test
  FOR i:=0 TO 1 DO
    LOOP
      a(bmF0,1); a(bmF1,1); a(bmF2,1); a(bmF3,0);
      a(bmF4,0); a(bmF5,0); a(bmF6,1); a(bmF7,0);
      a(bmCI0,1); a(bmCI1,1); -- ALU=B->P
      x1:={0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30}<<i;
      LOOP
        LOOP
          regwr(2,x1);
          a(bmAB0,0); a(bmAB1,1); a(bmAB2,0); a(bmAB3,0);
          a(bmRDB,1); a(bmALUEN,1); b(bmBUSD1,0);
          sm:=bd?();
          IF sm#x1 THEN
            print('Ошибка записи регистрового файла D14..D21.\n');
            print('Регистр %2d, записано %$8h, прочитано (bus B) %$8h,'
                  ' разница %$8h\n',2,x1,sm,x1/sm);
            wait_s;
          ELSE
            EXIT;
          END;
        END;
        -- записал в регистровый файл
        b(bfa,0); b(bfb,0);
        sm:=bd?();
        IF sm#x1 THEN
          print('Ошибка в АЛУ при выполнении операции B->P.\n');
          print('%$8h -> %$8h, должно было получиться %$8h.\n',
                x1,sm,x1);
          wait_s;
        END;
        b(bfa,1); b(bfb,1);
        b(bmRDB,0); a(bmALUEN,0); a(bmBUSD1,1);
        IF sm=x1 THEN EXIT END;
      END;
      -- переслал в P
      a(bmF0,1); a(bmF1,1); a(bmF2,1); a(bmF3,0);
      a(bmF4,0); a(bmF5,0); a(bmF6,0); a(bmF7,1);
      a(bmCI0,1); a(bmCI1,1); -- ALU=P
      a(bmALUEN,1); b(bmBUSD1,0);
      sm:=bd?();
      IF sm#{0..31} THEN
        print('Должно быть HIGH на всех разрядах шины B.\n');
        print('Посмотри разряды %{}.\n',sm/{0..31});
        wait_s;
      END;
      b(bfa,0); b(bfb,0);
      sm:=bd?();
      IF sm#x1 THEN
        print('Ошибка в АЛУ при выполнении операции P.\n');
        print('%$8h -> %$8h, должно было получиться %$8h.\n',
              x1,sm,x1);
        wait_s;
      END;
      b(bfa,0); b(bfb,0);
      a(bmALUEN,0); b(bmBUSD1,1);
      IF sm=x1 THEN EXIT END;
    END;
  END;
  stp:=FALSE;
END testalu;

PROCEDURE testrestart;
  VAR rd: BITSET; i: INTEGER;
BEGIN
  b(brestart,1);
  regwr(0,0);
  b(brestart,0);
  regwr(0,{0..31});
  b(brestart,1);
  rd:=regrd(0);
  IF rd#{} THEN
    b(brestart,0);
    print('Сигнал BRESTART не доходит до регистрового файла.\n');
    print('D14(10), D14(22), D15(10), D15(22), ... , D21(10), D21(22)\n');
    print('  должны быть HIGH.\n');
    wait;
    b(brestart,1);
    regwr(0,0);
  END;
  b(brestart,0);
  regcp(0);
  rd:=regrd(0);
  IF rd#{} THEN
    b(brestart,0);
    print('Сигнал BRESTART не доходит до регистрового файла.\n');
    print('D14(10), D14(22), D15(10), D15(22), ... , D21(10), D21(22)\n');
    print('  должны быть HIGH.\n');
    wait;
    b(brestart,1);
    regwr(0,0);
  END;
  b(brestart,0);
  a(bmCOND0,0); a(bmCOND1,1); a(bmCOND2,0); a(bmCOND3,1);
  a(bmWRSTK,0);
  LOOP
    LOOP
      FOR i:=0 TO 32 DO b(brestart,1); b(brestart,0) END;
      a(bmWRSTK,1); a(bmRDSTK,0);
      i:=0;
      LOOP
        IF b?(intreq,0) THEN EXIT END;
        b(brestart,1); b(brestart,0);
        INC(i);
        IF i=16 THEN EXIT END;
      END;
      a(bmWRSTK,0); a(bmRDSTK,1);
      b(brestart,1);
      IF stp THEN
        print('На вход -1 счетчика D34 подано LOW.\n');
        print('После изменения входа в HIGH долно изменится состояние счетчика.\n');
        wait_s;
      END;
      b(brestart,0);
      IF stp THEN
        print('Сработало?\n');
        wait_s;
      END;
      IF b?(intreq,0) THEN
        print('D41(3) должно быть LOW, D22(5) должно быть HIGH.\n');
        wait_s;
      ELSE
        EXIT
      END;
    END;
    a(bmWRSTK,1); a(bmRDSTK,0); b(brestart,1); b(brestart,0); a(bmRDSTK,1);
    IF b?(intreq,1) THEN
      print('D41(3) должно быть HIGH, D22(5) должно быть LOW.\n');
      wait_s;
    ELSE
      EXIT
    END;
  END;
  b(brestart,1);
  stp:=FALSE;
END testrestart;

VAR i: INTEGER;

BEGIN
  stp:=FALSE;
  power(TRUE,0);
  FOR i:=1 TO 192 DO input(i) END;
  FOR i:=1 TO 192 DO a(i,0) END;
  output(bmF0,bmF1,bmF2,bmF3,bmF4,bmF5,bmF6,bmF7,
         bmAA0,bmAA1,bmAA2,bmAA3,bmAB0,bmAB1,bmAB2,bmAB3,bmRDA,bmRDB,
         bmWRA,bmWRB,bmCI0,bmCI1,bmALUEN,bmSHIFTEN,bmRDSTK,bmWRSTK,
         bmRDCONST,bmSREG,bmNEXT,bmBUSD0,bmBUSD1,brestart,const0,
         const1,const2,const3,const4,const5,const6,const7,bmCOND0,
         bmCOND1,bmCOND2,bmCOND3,bmDECCNT,bfa,bfb);
  b(bmBUSD1,1);
  b(brestart,1);
  b(bmRDSTK,1);
  b(bmWRSTK,1);
  b(bfa,1);
  b(bfb,1);
  FOR i:=-2 TO 2 BY 4 DO
    power(TRUE,i);
    print('test data bus\r'); testbd;
    print('test const   \r'); testconst;
    print('test regs    \r'); testrg;
    print('test restart \r'); testrestart;
    print('test ALU     \r'); testalu;
    print('test cash    \r'); testcash;
    print('test shifter \r'); testsh;
    print('test stack   \r'); teststk;
    print('test flag    \r'); testflag;
  END;
END alu6.
