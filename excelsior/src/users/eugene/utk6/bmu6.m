MODULE bmu6; (* 02-Nov-88. (c) KRONOS *)

FROM Terminal   IMPORT  Read, print, BusyRead;
FROM SYSTEM     IMPORT  WORD, ADR, ADDRESS;
FROM FsPublic   IMPORT  File, FileName;
FROM Args       IMPORT  ScanFlags,Flag?;
FROM BIO        IMPORT  OpenOnDir, checkHALT, bRead, Close,
                        CD, GetEof, bWrite;
FROM UTK6       IMPORT  b, a, b?, power, input, output;

-- программа управления стендом для тестирования модулей ЖШСИ 010.02

PROCEDURE MOVE(a,b: ADDRESS; s: INTEGER); CODE 0C0h END MOVE;

CONST
-- out
  deben =187; -- X2C27, разрешения сдвига микрокоманды (low)
  debma =141; -- X2B13, запрет выдачи микроадреса на ПЗУ (low)
  debin =186; -- X2C26, последовательный ввод микрокоманды (high)
  debstr=185; -- X2C25, такт микрокоманды (low)
  debbd =142; -- X2B14, запрет BUSD1          (low)
  debfab=143; -- X2B15, запрет генератора фаз (low)
  bmcl  = 78; -- X1C28, сброс БМУ (low)
  bfb   = 71; -- X1C21, фаза Б (low)
  bfa   = 19; -- X1A21, фаза А (low)
  cash0 =177; -- X2C17
  cash1 =178; -- X2C18
  cash2 =179; -- X2C19
  cash3 =180; -- X2C20
  cash4 =181; -- X2C21
  cash5 =182; -- X2C22
  cash6 =183; -- X2C23
  cash7 =184; -- X2C24
  cash8 =192; -- X2C32
  brest =128; -- X2A32
  bflag =175; -- X2C15
  bintreq=176;-- X2C16
  bint  = 76; -- X1C26
  bcint = 25; -- X1A26
  bacl  = 27; -- X1A28
  bconsole= 77; -- X1C27
  deba00=129; -- X2B01
  deba01=130; -- X2B02
  deba02=131; -- X2B03
  deba03=132; -- X2B04
  deba04=133; -- X2B05
  deba05=134; -- X2B06
  deba06=135; -- X2B07
  deba07=136; -- X2B08
  deba08=137; -- X2B09
  deba09=138; -- X2B10
  deba10=139; -- X2B11
  deba11=140; -- X2B12
  mf0   = 97; -- X2A01
  mf1   = 98; -- X2A02
  mf2   = 99; -- X2A03
  mf3   =100; -- X2A04
  mf4   =101; -- X2A05
  mf5   =102; -- X2A06
  mf6   =103; -- X2A07
  mf7   =104; -- X2A08
  mci0  =117; -- X2A21
  mci1  =118; -- X2A22
  mcond0=169; -- X2C09
  mcond1=170; -- X2C10
  mcond2=171; -- X2C11
  mcond3=172; -- X2C12
  maa0  =105; -- X2A09
  maa1  =106; -- X2A10
  maa2  =107; -- X2A11
  maa3  =108; -- X2A12
  mab0  =109; -- X2A13
  mab1  =110; -- X2A14
  mab2  =111; -- X2A15
  mab3  =112; -- X2A16
  mrda  =113; -- X2A17
  mrdb  =114; -- X2A18
  mwra  =115; -- X2A19
  mwrb  =116; -- X2A20
  maluen=119; -- X2A23
  mshen =120; -- X2A24
  mrdstk=121; -- X2A25
  mwrstk=122; -- X2A26
  mconst=123; -- X2A27
  msreg =124; -- X2A28
  mbusd0=126; -- X2A30
  mdcnt =173; -- X2C13
  bmadr0= 20; -- X1A22
  bmadr1= 72; -- X1C22
  bmadr2= 21; -- X1A23
  bmadr3= 73; -- X1C23
  ma00  =129; -- X2B01
  ma01  =130; -- X2B02
  ma02  =131; -- X2B03
  ma03  =132; -- X2B04
  ma04  =133; -- X2B05
  ma05  =134; -- X2B06
  ma06  =135; -- X2B07
  ma07  =136; -- X2B08
  ma08  =137; -- X2B09
  ma09  =138; -- X2B10
  ma10  =139; -- X2B11
  ma11  =140; -- X2B12
  bcrash= 24; -- X1A26
  const0=161; -- X2C01
  const1=162; -- X2C02
  const2=163; -- X2C03
  const3=164; -- X2C04
  const4=165; -- X2C05
  const5=166; -- X2C06
  const6=167; -- X2C07
  const7=168; -- X2C08
  busd1 =127; -- X2A31
  binhib= 75; -- X1C25
  mcl   = 78; -- X1C28

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

TYPE
  mask_bits=(int_mask,cint_mask,cmd_mask,user_lamp,quit_lamp,idle_lamp);
  mask_set=SET OF mask_bits;

PROCEDURE b1(n: INTEGER);
BEGIN
  b(n,1);
END b1;

PROCEDURE b0(n: INTEGER);
BEGIN
  b(n,0);
END b0;

PROCEDURE a1(n: INTEGER);
BEGIN
  a(n,1);
END a1;

PROCEDURE a0(n: INTEGER);
BEGIN
  a(n,0);
END a0;

PROCEDURE ?0(n: INTEGER): BOOLEAN;
BEGIN
  RETURN b?(n,0);
END ?0;

PROCEDURE ?1(n: INTEGER): BOOLEAN;
BEGIN
  RETURN b?(n,1);
END ?1;

PROCEDURE takt;
BEGIN
  b(bfb,1);
  b(bfa,0);
  b(bfb,0);
  b(bfa,1);
END takt;

PROCEDURE reset;
BEGIN
  b(bmcl,0); takt; takt; takt; b(bmcl,1); takt;
END reset;

TYPE
  set64=ARRAY [0..1] OF BITSET;

VAR
  mask: set64;
  cmd : set64;

PROCEDURE getcmd;
  PROCEDURE get(bit,channel: INTEGER);
  BEGIN
    ASSERT(NOT ((bit MOD 32) IN mask[bit DIV 32]));
    INCL(mask[bit DIV 32],bit MOD 32);
    IF ?1(channel) THEN
      INCL(cmd[bit DIV 32],bit MOD 32);
    END;
  END get;
BEGIN
  b(debbd,1);
  mask[0]:={}; mask[1]:={};
  cmd [0]:={}; cmd [1]:={};
  get(49,mf0); get(57,mf1); get(48,mf2); get(42,mf3);
  get(51,mf4); get(40,mf5); get(41,mf6); get(54,mf7);
  get(10,mci0); get(09,mci1); get(53,mcond0); get(63,mcond1);
  get(44,mcond2); get(19,mcond3); get(52,maa0); get(45,maa1);
  get(46,maa2); get(22,maa3); get(17,mab0); get(04,mab1);
  get(05,mab2); get(06,mab3); get(13,mwra); get(03,mwrb);
  get(07,mrda); get(14,mrdb); get(18,maluen); get(02,mshen);
  get(24,mconst); get(08,msreg); get(01,mrdstk); get(00,mwrstk);
  get(21,mdcnt); get(12,mbusd0); get(28,busd1);
  get(61,bmadr0); get(62,bmadr1); get(60,bmadr2); get(59,bmadr3);
END getcmd;

PROCEDURE uadr?(): INTEGER;
  VAR s: BITSET;
BEGIN
  s:={};
  IF ?1(ma00) THEN INCL(s,0) END;
  IF ?1(ma01) THEN INCL(s,1) END;
  IF ?1(ma02) THEN INCL(s,2) END;
  IF ?1(ma03) THEN INCL(s,3) END;
  IF ?1(ma04) THEN INCL(s,4) END;
  IF ?1(ma05) THEN INCL(s,5) END;
  IF ?1(ma06) THEN INCL(s,6) END;
  IF ?1(ma07) THEN INCL(s,7) END;
  IF ?1(ma08) THEN INCL(s,8) END;
  IF ?1(ma09) THEN INCL(s,9) END;
  IF ?1(ma10) THEN INCL(s,10) END;
  IF ?1(ma11) THEN INCL(s,11) END;
  RETURN INTEGER(s);
END uadr?;

PROCEDURE wait;
  VAR ch: CHAR;
BEGIN
  IF b?(bfb,1) THEN END;
  print(' Нажмите любую клавишу для продолжения...\r');
  ch:=Read();
  print('                                         \n');
END wait;

PROCEDURE testrg;
-- тест функционирования регистра микрокоманды
  PROCEDURE reg(): BOOLEAN;
    VAR p: set64; i,j: INTEGER; ch: CHAR;
  BEGIN
    reset;
    b(debin,0);
    FOR i:=0 TO 63 DO b(debstr,1); b(debstr,0) END;
    p[0]:={0}; p[1]:={};
    b(debin,1);
    b(debstr,1); b(debstr,0);
    b(debin,0);
    FOR i:=0 TO 63 DO
      getcmd;
      IF (p[0]*mask[0]#cmd[0])OR(p[1]*mask[1]#cmd[1]) THEN
        print('Ошибка в регистре микрокоманды:\n');
        FOR j:=0 TO 31 DO
          IF j IN (p[0]*mask[0]/cmd[0]) THEN
            print('  бит %d должен быть %d.\n',j,j=i)
          END;
        END;
        FOR j:=0 TO 31 DO
          IF j IN (p[1]*mask[1]/cmd[1]) THEN
            print('  бит %d должен быть %d.\n',j+32,j+32=i)
          END;
        END;
        wait;
      ELSE
        takt;
        IF ?0(bcrash) THEN
          print('Какого черта возник CRASH? Микрокоманда {%d}.\n',i);
          wait;
        END;
      END;
      b(debstr,1); b(debstr,0);
      IF 31 IN p[0] THEN
        p[0]:={}; p[1]:={0}
      ELSE
        p[0]:=p[0]<<1; p[1]:=p[1]<<1;
      END;
    END;
    RETURN TRUE;
  END reg;
  VAR i: INTEGER;
BEGIN
  b(bmcl,1);
  b(bfa,1);
  b(bfb,0);
  b(debstr,0);
  b(debfab,0);
  b(deben,0);
  b(debma,0);
  FOR i:=63 TO 0 BY -1 DO b(debin,INTEGER(i=11)); b(debstr,1); b(debstr,0) END;
  b(bfa,0);
  b(bfa,1);
  b(bfa,0);
  b(bfa,1);
  LOOP
    IF reg() THEN RETURN END;
    LOOP
      b(debin,0);
      b(debstr,1); b(debstr,0);
      b(debin,1);
      b(debstr,1); b(debstr,0);
      IF BusyRead()#0c THEN EXIT END;
    END;
  END;
  FOR i:=63 TO 0 BY -1 DO b(debin,INTEGER(i=11)); b(debstr,1); b(debstr,0) END;
  reset;
  b(debin,1);
  b(debstr,1); b(debstr,0);
  b(debstr,1); b(debstr,0);
  b(debin,0);
  FOR i:=0 TO 62 DO
    takt;
    IF ?1(bcrash) THEN
      print('Почему-то нет сигнала BCRASH. Микрокоманда {%d,%d}.\n',i,i+1);
      wait;
    ELSE
      b(bmcl,0); takt; b(bmcl,1); takt;
      IF ?0(bcrash) THEN
        print('Не могу сбросить CRASH.\n');
        wait;
      END;
    END;
    b(debstr,1); b(debstr,0);
  END;
  FOR i:=63 TO 0 BY -1 DO b(debin,INTEGER(i=11)); b(debstr,1); b(debstr,0) END;
  reset;
END testrg;

PROCEDURE testconst;
  PROCEDURE const?(): INTEGER;
    VAR s: BITSET;
  BEGIN
    s:={};
    IF ?1(const0) THEN INCL(s,0) END;
    IF ?1(const1) THEN INCL(s,1) END;
    IF ?1(const2) THEN INCL(s,2) END;
    IF ?1(const3) THEN INCL(s,3) END;
    IF ?1(const4) THEN INCL(s,4) END;
    IF ?1(const5) THEN INCL(s,5) END;
    IF ?1(const6) THEN INCL(s,6) END;
    IF ?1(const7) THEN INCL(s,7) END;
    RETURN INTEGER(s);
  END const?;
  VAR i,j,k,cns,bt: INTEGER;
BEGIN
  b(bmcl,1);
  b(bfa,1);
  b(bfb,0);
  b(debstr,0);
  b(debfab,0);
  b(deben,0);
  input(
    deba00,deba01,deba02,deba03,deba04,deba05,
    deba06,deba07,deba08,deba09,deba10,deba11);
  b(debma,1);
  FOR bt:=0 TO 7 DO
    cns:=INTEGER({bt});
    FOR i:=63 TO 0 BY -1 DO
      CASE i OF
        |47: IF 0 IN BITSET(cns) THEN b(debin,1) ELSE b(debin,0) END;
        |50: IF 1 IN BITSET(cns) THEN b(debin,1) ELSE b(debin,0) END;
        |43: IF 2 IN BITSET(cns) THEN b(debin,1) ELSE b(debin,0) END;
        |31: IF 3 IN BITSET(cns) THEN b(debin,1) ELSE b(debin,0) END;
        |30: IF 4 IN BITSET(cns) THEN b(debin,1) ELSE b(debin,0) END;
        |26: IF 5 IN BITSET(cns) THEN b(debin,1) ELSE b(debin,0) END;
        |29: IF 6 IN BITSET(cns) THEN b(debin,1) ELSE b(debin,0) END;
        |20: IF 7 IN BITSET(cns) THEN b(debin,1) ELSE b(debin,0) END;
        |11: k:=0;
             FOR j:=0 TO 7 DO IF j IN BITSET(cns) THEN INC(k) END END;
             IF ODD(k) THEN b(debin,0) ELSE b(debin,1) END;
      ELSE
        b(debin,0);
      END;
      b(debstr,1); b(debstr,0);
    END;
    reset;
    takt;
    IF ?0(bcrash) THEN
      print('Какого черта возник CRASH?.\n');
      wait;
    END;
    i:=const?();
    IF i#cns THEN
      print('Ошибка генератора констант:');
      print('  должно быть %3$h, прочитано %3$h\n',cns,i);
      wait;
    END;
  END;
END testconst;

PROCEDURE testcsh;
-- тест прохождения данных: кэш->микроадрес, кэш->константа
  PROCEDURE csh(n: INTEGER);
  BEGIN
    b(cash0,INTEGER(0 IN BITSET(n)));
    b(cash1,INTEGER(1 IN BITSET(n)));
    b(cash2,INTEGER(2 IN BITSET(n)));
    b(cash3,INTEGER(3 IN BITSET(n)));
    b(cash4,INTEGER(4 IN BITSET(n)));
    b(cash5,INTEGER(5 IN BITSET(n)));
    b(cash6,INTEGER(6 IN BITSET(n)));
    b(cash7,INTEGER(7 IN BITSET(n)));
  END csh;
  PROCEDURE const?(): INTEGER;
    VAR s: BITSET;
  BEGIN
    s:={};
    IF ?1(const0) THEN INCL(s,0) END;
    IF ?1(const1) THEN INCL(s,1) END;
    IF ?1(const2) THEN INCL(s,2) END;
    IF ?1(const3) THEN INCL(s,3) END;
    IF ?1(const4) THEN INCL(s,4) END;
    IF ?1(const5) THEN INCL(s,5) END;
    IF ?1(const6) THEN INCL(s,6) END;
    IF ?1(const7) THEN INCL(s,7) END;
    RETURN INTEGER(s);
  END const?;
  VAR i,j: INTEGER; ch: CHAR;
BEGIN
  b(bmcl,1);
  b(bfa,1);
  b(bfb,0);
  b(debstr,0);
  b(debfab,0);
  b(deben,0);
  input(
    deba00,deba01,deba02,deba03,deba04,deba05,
    deba06,deba07,deba08,deba09,deba10,deba11);
  b(debma,1);
  FOR i:=63 TO 0 BY -1 DO
    IF (i=34)OR(i=33)OR(i=56) THEN b(debin,1) ELSE b(debin,0) END;
    b(debstr,1); b(debstr,0);
  END;
  reset;
  takt;
  IF ?0(bcrash) THEN
    print('Какого черта возник CRASH? Микрокоманда {33,34,56}.\n');
    wait;
  END;
  j:=const?();
  IF j#0 THEN
    print('Ошибка генератора констант:');
    print('  должно быть 000, прочитано %3$h\n',j);
    wait;
  END;
  b(cash8,0);
  FOR i:=0 TO 7 DO
    csh(1<<i);
    takt;
    j:=uadr?();
    IF j#(0FFh-INTEGER(1<<i))*2+200h THEN
      print('Ошибка микроадреса (cash8=0):');
      print('  должно быть %3$h, прочитано %3$h\n',
            (0FFh-INTEGER(1<<i))*2+200h,j);
      wait;
    END;
  END;
  b(cash8,1);
  FOR i:=0 TO 7 DO
    csh(1<<i);
    takt;
    j:=uadr?();
    IF j#(0FFh-INTEGER(1<<i))*2+400h THEN
      print('Ошибка микроадреса (cash8=1):');
      print('  должно быть %3$h, прочитано %3$h\n',
            (0FFh-INTEGER(1<<i))*2+400h,j);
      wait;
    END;
  END;
  FOR i:=63 TO 0 BY -1 DO
    CASE i OF |23,11,34,33,56: b(debin,1) ELSE b(debin,0) END;
    b(debstr,1); b(debstr,0);
  END;
  takt;
  j:=uadr?();
  IF j#0FFFh THEN
    print('Ошибка микроадреса:');
    print('  должно быть 0FFF, прочитано %3$h\n',j);
    wait;
  END;
  FOR i:=0 TO 7 DO
    csh(1<<i); j:=const?();
    IF j#INTEGER(1<<i) THEN
      print('Ошибка генератора констант:');
      print('  должно     RETURN;
    END;
  END;
END testinhib;

PROCEDURE testROM;
  PROCEDURE ma(n: INTEGER);
  BEGIN
    a(deba00,INTEGER(0 IN BITSET(n)));
    a(deba01,INTEGER(1 IN BITSET(n)));
    a(deba02,INTEGER(2 IN BITSET(n)));
    a(deba03,INTEGER(3 IN BITSET(n)));
    a(deba04,INTEGER(4 IN BITSET(n)));
    a(deba05,INTEGER(5 IN BITSET(n)));
    a(deba06,INTEGER(6 IN BITSET(n)));
    a(deba07,INTEGER(7 IN BITSET(n)));
    a(deba08,INTEGER(8 IN BITSET(n)));
    a(deba09,INTEGER(9 IN BITSET(n)));
    a(deba10,INTEGER(10 IN BITSET(n)));
    a(deba11,INTEGER(11 IN BITSET(n)));
  END ma;
  VAR
    rom         : ARRAY [0..4095] OF ARRAY [0..7] OF CHAR;
    Name        : FileName;
    Inp         : File;
    Eof         : INTEGER;
    i,j,j1,par  : INTEGER;
    r           : BOOLEAN;
    cmd,dif,cmo : ARRAY [0..1] OF BITSET;

BEGIN
--ScanFlags;
  IF     Flag?('1') THEN  Name:='kr61.rom'; --самая первая
  ELSIF  Flag?('2') THEN  Name:='kr62.rom'; --поставки по кооп
  ELSIF  Flag?('3') THEN  Name:='kr63.rom'; --ошибoчная WS
  ELSIF  Flag?('4') THEN  Name:='kr64.rom'; --первые 10 шт. WS
  ELSIF  Flag?('5') THEN  Name:='kr65.rom'; --WS
  ELSIF  Flag?('6') THEN  Name:='kr66.rom'; --не реализованная
  ELSE                    Name:='kr6.rom';
  END;
  rom[0][0]:=0c; rom[0][1]:=0c; rom[0][2]:=0c; rom[0][3]:=0c;
  MOVE(ADR(rom)+1,ADR(rom),SIZE(rom)-1);
  r:=OpenOnDir(CD(),Inp,Name);
  IF r THEN
    print('Can not read file %s\n',Name); Eof:=0;
  --RETURN
  ELSE
    Eof:=SIZE(rom)*4;
    IF Eof>GetEof(Inp) THEN Eof:=GetEof(Inp) END;
    checkHALT(bRead(Inp,0,ADR(rom),Eof),Name);
    checkHALT(Close(Inp),Name);
  END;
  b(bmcl,1);
  b(bfa,1);
  b(bfb,0);
  b(debstr,0);
  b(debfab,0);
  b(deben,0);
  b(debma,0);
  output(
    deba00,deba01,deba02,deba03,deba04,deba05,
    deba06,deba07,deba08,deba09,deba10,deba11);
  FOR i:=0 TO 11 DO
    ma(1<<i); j:=uadr?();
    IF j#INTEGER(1<<i) THEN
      print('Ошибка микроадреса:');
      print('  должно быть %3$h, прочитано %3$h\n',1<<i,j);
      wait;
    END;
  END;
  dif[0]:={0..31}; dif[1]:={0..31};
  FOR i:=0 TO 4095 DO
    IF (i MOD 40)=0 THEN print('%4d\r',i) END;
    ma(i);
    b(deben,1);
    b(debstr,1); b(debstr,0);
    b(deben,0);
    b(brest,0);
    par:=0;
    FOR j:=63 TO 42 BY -1 DO
      IF Eof>0 THEN
        IF b?(mcond1,1)#((j MOD 8) IN BITSET(rom[i][j DIV 8])) THEN
          print('Адрес %4$h, ',i);
          print('ошибка в ПЗУ #%d.%d, бит #%d.\n',i DIV 2048,j DIV 8,j MOD 8);
          wait;
        END;
        j1:=j-22;
        IF b?(mf6,1)#((j1 MOD 8) IN BITSET(rom[i][j1 DIV 8])) THEN
          print('Адрес %4$h, ',i);
          print('ошибка в ПЗУ #%d.%d, бит #%d.\n',i DIV 2048,j1 DIV 8,j1 MOD 8);
          wait;
        END;
        j1:=j-44;
        IF (j1>=0) &
          (b?(mcond3,1)#((j1 MOD 8) IN BITSET(rom[i][j1 DIV 8]))) THEN
          print('Адрес %4$h, ',i);
          print('ошибка в ПЗУ #%d.%d, бит #%d.\n',i DIV 2048,j1 DIV 8,j1 MOD 8);
          wait;
        END;
      ELSE
        IF b?(mcond1,1) THEN
          INCL(cmd[j DIV 32],j MOD 32); INC(par);
        ELSE
  0, прочитано %3$h\n',j);
    wait;
  END;
  b(cash8,0);
  FOR i:=0 TO 7 DO
    csh(1<<i);
    takt;
    j:=uadr?();
    IF j#(0FFh-INTEGER(1<<i))*2+200h THEN
      print('Ошибка микроадреса (cash8=0):');
      print('  должно быть %3$h, прочитано %3$h\n',
            (0FFh-INTEGER(1<<i))*2+200h,j);
      wait;
    END;
  END;
  b(cash8,1);
  FOR i:=0 TO 7 DO
    csh(1<<i);
    takt;
    j:=uadr?();
    IF j#(0FFh-INTEGER(1<<i))*2+400h THEN
      print('Ошибка микроадреса (cash8=1):');
      print('  должно быть %3$h, прочитано %3$h\n',
            (0FFh-INTEGER(1<<i))*2+400h,j);
      wait;
    END;
  END;
  FOR i:=63 TO 0 BY -1 DO
    CASE i OF |23,11,34,33,56: b(debin,1) ELSE b(debin,0) END;
    b(debstr,1); b(debstr,0);
  END;
  takt;
  j:=uadr?();
  IF j#0FFFh THEN
    print('Ошибка микроадреса:');
    print('  должно быть 0FFF, прочитано %3$h\n',j);
    wait;
  END;
  FOR i:=0 TO 7 DO
    csh(1<<i); j:=const?();
    IF j#INTEGER(1<<i) THEN
      print('Ошибка генератора констант:');
      print('  должно) ELSE print('  parity error\n') END;
      cmo:=cmd;
    END;
  END;
  input(
    deba00,deba01,deba02,deba03,deba04,deba05,
    deba06,deba07,deba08,deba09,deba10,deba11);
  b(debma,1);
END testROM;

PROCEDURE resipt(n: INTEGER; mask: mask_set);
  VAR i: INTEGER; prt: BOOLEAN;
BEGIN
  prt:=FALSE;
  FOR i:=63 TO 0 BY -1 DO
    CASE i OF
      |55,56,58: a(debin,1); prt:=NOT prt;
      |47: IF n IN {1,3} THEN a(debin,1); prt:=NOT prt ELSE a(debin,0) END;
      |50: IF n IN {2,3} THEN a(debin,1); prt:=NOT prt ELSE a(debin,0) END;
      |63: IF NOT prt THEN a(debin,1) ELSE a(debin,0) END;
      |31: IF quit_lamp IN mask THEN
             a(debin,1); prt:=NOT prt
           ELSE
             a(debin,0)
           END;
      |43: IF NOT (idle_lamp IN mask) THEN
             a(debin,1); prt:=NOT prt
           ELSE
             a(debin,0)
           END;
    ELSE a(debin,0)
    END;
    a(debstr,1); b(debstr,0);
  END;
  a(bd00,INTEGER(NOT (int_mask IN mask)));
  a(bd10,INTEGER(NOT (cint_mask IN mask)));
  a(bd11,INTEGER(NOT (cmd_mask IN mask)));
  a(bd12,INTEGER(NOT (user_lamp IN mask)));
  takt;
  IF ?0(bcrash) THEN print('Какого черта возник CRASH?\n'); wait END;
END resipt;

PROCEDURE testipt;
  CONST di=mask_set{};
  VAR i,j,k,c0,c1: INTEGER;
BEGIN
  b(bmcl,1);
  b(bfa,1);
  b(bfb,0);
  b(debstr,0);
  b(debfab,0);
  b(deben,0);
  input(
    deba00,deba01,deba02,deba03,deba04,deba05,
    deba06,deba07,deba08,deba09,deba10,deba11);
  b(debma,1);
  resipt(0,di);
  resipt(1,di);
  resipt(2,di);
  resipt(3,di);
  c0:=0; c1:=0;
  FOR k:=0 TO 19 DO
    FOR i:=63 TO 0 BY -1 DO
      CASE i OF |25,23,33,34,56: b(debin,1) ELSE b(debin,0) END;
      b(debstr,1); b(debstr,0);
    END;
    b(bfb,1);
    j:=uadr?();
    IF (j#0FFCh) & (j#0FFFh) THEN
      print('Тест прохождения прерываний от таймера.\n');
      print('Ожидалось появление вектора на шине микроадреса.\n');
      print('Ожидалось 0FFC или 0FFF, прочитано %4$h.\n',j);
      wait;
    END;
    b(bfa,0);
    b(bfb,0);
    b(bfa,1);
    IF ?0(bcrash) THEN
      print('Какого черта возник CRASH? Микрокоманда {25,23,33,34,56}.\n');
      wait;
    END;
    IF j=0FFCh THEN
      resipt(1,di); INC(c1)
    ELSE
      IF j#0FFFh THEN
        resipt(0,di); resipt(1,di); resipt(2,di); resipt(3,di);
      ELSE
        INC(c0)
      END;
    END;
  END;
  IF c1=0 THEN print('Нет прерываний от таймера.\n'); wait END;
  IF c0=0 THEN
    resipt(1,di); b(bfb,1); b(bfa,0);
    print('Не проходит сброс запроса от таймера.\n'); wait;
    b(bfb,0); b(bfa,1);
  END;
  reset;
END testipt;

PROCEDURE testWU2;
  TYPE
    mode=(next,reg,stk,jmp);
    condition=(ne_flag,flag,intreq,ne_intreq,true,cint,int);
  PROCEDURE cmd(uadr: INTEGER; md: mode; cn: condition; fe,re: BOOLEAN);
    VAR i,cnt: INTEGER; k: BOOLEAN;
  BEGIN
    b(bfb,0);
    cnt:=0;
    FOR i:=63 TO 0 BY -1 DO
      CASE i OF
        |58: k:=0 IN BITSET(cn);
        |55: k:=1 IN BITSET(cn);
        |56: k:=2 IN BITSET(cn);
        |23: k:=TRUE;
        |47: k:=NOT (00 IN BITSET(uadr));
        |50: k:=NOT (01 IN BITSET(uadr));
        |43: k:=NOT (02 IN BITSET(uadr));
        |31: k:=NOT (03 IN BITSET(uadr));
        |30: k:=NOT (04 IN BITSET(uadr));
        |26: k:=NOT (05 IN BITSET(uadr));
        |29: k:=NOT (06 IN BITSET(uadr));
        |20: k:=NOT (07 IN BITSET(uadr));
        |37: k:=NOT (08 IN BITSET(uadr));
        |38: k:=NOT (09 IN BITSET(uadr));
        |32: k:=NOT (10 IN BITSET(uadr));
        |39: k:=NOT (11 IN BITSET(uadr));
        |34: k:=0 IN BITSET(md);
        |33: k:=1 IN BITSET(md);
        |35: k:=fe;
        |36: k:=re;
        |00: k:=NOT ODD(cnt);
      ELSE k:=FALSE;
      END;
      IF k THEN INC(cnt); a(debin,1) ELSE a(debin,0) END;
      a(debstr,1); b(debstr,0);
    END;
  END cmd;

  PROCEDURE check(a: INTEGER);
    VAR aa: INTEGER;
  BEGIN
    aa:=uadr?();
    IF aa#a THEN
      print('Ошибка прохождения микроадреса, '
            'ожидолось %$3h, получено %$3h\n',a,aa);
      wait;
    END;
  END check;
  VAR a,aa,i: INTEGER;
BEGIN
  input(
    deba00,deba01,deba02,deba03,deba04,deba05,
    deba06,deba07,deba08,deba09,deba10,deba11);
  b(debma,1);
  b(bfa,1); b(bfb,0);
  FOR i:=0 TO 11 DO
    a:=INTEGER({i});
    cmd(a,jmp,true,FALSE,FALSE);
    b(bfb,1); check(a);
    b(bfa,0); check(a);
    b(bfb,0); check(a);
    b(bfa,1); check(a);
    cmd(a,jmp,flag,FALSE,FALSE);
    b(bfb,1);
    b(bflag,1); check(a);
    b(bflag,0); check((a+1) MOD 1000h);
    b(bfa,0); b(bfb,0); b(bfa,1);
    cmd(a,jmp,ne_flag,FALSE,FALSE);
    b(bfb,1);
    b(bflag,0); check(a);
    b(bflag,1); check((a+2) MOD 1000h);
    b(bfa,0); b(bfb,0); b(bfa,1);
    cmd(a,jmp,intreq,FALSE,FALSE);
    b(bfb,1);
    b(bflag,0); b(bintreq,0); check(a);
    b(bflag,1); b(bintreq,0); check(a);
    b(bflag,0); b(bintreq,1); check((a+3) MOD 1000h);
    b(bflag,1); b(bintreq,1); check((a+3) MOD 1000h);
    b(bfa,0); b(bfb,0); b(bfa,1);
    cmd(a,jmp,ne_intreq,FALSE,FALSE);
    b(bfb,1);
    b(bintreq,1); check(a);
    b(bintreq,0); check((a+4) MOD 1000h);
    b(bfa,0); b(bfb,0); b(bfa,1);
    cmd(a,next,true,FALSE,TRUE);
    b(bfb,1); check((a+5) MOD 1000h);
    b(bfa,0); check((a+5) MOD 1000h);
    b(bfb,0); check((a+5) MOD 1000h);
    b(bfa,1); check((a+5) MOD 1000h);
    cmd(0,reg,true,FALSE,FALSE);
    b(bfb,1); check(a);
    b(bfa,0); check(a);
    b(bfb,0); check(a);
    b(bfa,1); check(a);
    cmd(INTEGER(BITSET(a)/{0..11}),jmp,true,TRUE,FALSE); -- a+1
    b(bfb,1); b(bfa,0); b(bfb,0); b(bfa,1);
    cmd(a+3,jmp,true,TRUE,FALSE);                -- a/{0..11}+1
    b(bfb,1); b(bfa,0); b(bfb,0); b(bfa,1);
    cmd(INTEGER(BITSET(a+5)/{0..11}),jmp,true,TRUE,FALSE); -- a+4
    b(bfb,1); b(bfa,0); b(bfb,0); b(bfa,1);
    cmd(0,jmp,true,TRUE,FALSE);     -- (a+5)/{0..11}+1
    b(bfb,1); b(bfa,0); b(bfb,0); b(bfa,1);
    check(0);
    cmd(0FFFh,stk,true,TRUE,FALSE);
    b(bfb,1); b(bfa,0); b(bfb,0); b(bfa,1);
    check((INTEGER(BITSET(a+5)/{0..11})+1) MOD 1000h);
    cmd(0FFFh,stk,true,TRUE,FALSE);
    b(bfb,1); b(bfa,0); b(bfb,0); b(bfa,1);
    check((a+4) MOD 1000h);
    cmd(0FFFh,stk,true,TRUE,FALSE);
    b(bfb,1); b(bfa,0); b(bfb,0); b(bfa,1);
    check((INTEGER(BITSET(a)/{0..11})+1) MOD 1000h);
    cmd(0FFFh,stk,true,TRUE,FALSE);
    b(bfb,1); b(bfa,0); b(bfb,0); b(bfa,1);
    check((a+1) MOD 1000h);
  END;
  b(bintreq,1); b(bflag,1);
  resipt(0,mask_set{}); resipt(2,mask_set{}); resipt(3,mask_set{});
  ------- проверка прохождения BINT
  cmd(0,jmp,true,FALSE,FALSE);
  takt; check(0);
  resipt(1,mask_set{int_mask}); check(1);
  cmd(333,jmp,int,FALSE,FALSE);
  b(bint,1); b(bfb,1); b(bfb,0);
  IF uadr?()=111 THEN
    print('Проверка прохождения сигнала BINT.\n');
    print('К3(07) должен быть 0.\n'); wait;
  ELSE
    check(2);
  END;
  b(bint,0); b(bfb,1); b(bfb,0);
  IF uadr?()=2 THEN
    print('Проверка прохождения сигнала BINT.\n');
    print('К3(07) должен быть 1.\n'); wait;
  ELSE
    check(333);
  END;
  b(bint,1);
  ------- проверка прохождения BCINT
  cmd(0,jmp,true,FALSE,FALSE);
  takt; check(0);
  resipt(1,mask_set{cint_mask}); check(1);
  cmd(333,jmp,int,FALSE,FALSE);
  b(bcint,1); b(bfb,1); b(bfb,0);
  IF uadr?()=111 THEN
    print('Проверка прохождения сигнала BCINT.\n');
    print('К3(07) должен быть 0.\n'); wait;
  ELSE
    check(2);
  END;
  b(bcint,0); b(bfb,1); b(bfb,0);
  IF uadr?()=2 THEN
    print('Проверка прохождения сигнала BCINT.\n');
    print('К3(07) должен быть 1.\n'); wait;
  ELSE
    check(333);
  END;
  b(bcint,1);
  ------- проверка прохождения BCONSOLE
  cmd(0,jmp,true,FALSE,FALSE);
  takt; check(0);
  resipt(1,mask_set{}); check(1);
  cmd(333,jmp,int,FALSE,FALSE);
  b(bconsole,1); b(bfb,1); b(bfb,0);
  IF uadr?()=111 THEN
    print('Проверка прохождения сигнала BCONSOLE.\n');
    print('К3(07) должен быть 0.\n'); wait;
  ELSE
    check(2);
  END;
  b(bconsole,0); b(bfb,1); b(bfb,0);
  IF uadr?()=2 THEN
    print('Проверка прохождения сигнала BCONSOLE.\n');
    print('К3(07) должен быть 1.\n'); wait;
  ELSE
    check(333);
  END;
  b(bconsole,1);
  ------- проверка прохождения BACL
  cmd(0,jmp,true,FALSE,FALSE);
  takt; check(0);
  resipt(1,mask_set{}); check(1);
  cmd(333,jmp,int,FALSE,FALSE);
  b(bfb,1); b(bfb,0);
  IF uadr?()=111 THEN
    print('Проверка прохождения сигнала BACL.\n');
    print('К3(07) должен быть 0.\n'); wait;
  ELSE
    check(2);
  END;
  b(bacl,0); b(bacl,1); b(bfb,1); b(bfb,0);
  IF uadr?()=2 THEN
    print('Проверка прохождения сигнала BACL.\n');
    print('К3(07) должен быть 1.\n'); wait;
  ELSE
    check(333);
  END;
  b(bconsole,1);
END testWU2;

VAR i: INTEGER;

BEGIN
  power(TRUE,-2);
  FOR i:=1 TO 192 DO input(i) END;
  FOR i:=1 TO 192 DO a(i,0) END;
  output(
    deben,debma,debin,debstr,debbd,debfab,bmcl,bfb,bfa,
    cash0,cash1,cash2,cash3,cash4,cash5,cash6,cash7,cash8,
    bflag,bintreq,bint,bcint,bconsole,bacl,
    bd00,bd01,bd02,bd03,bd04,bd05,bd06,bd07,
    bd08,bd09,bd10,bd11,bd12,bd13,bd14,bd15,
    bd16,bd17,bd18,bd19,bd20,bd21,bd22,bd23,
    bd24,bd25,bd26,bd27,bd28,bd29,bd30,bd31);
  a(bint,1); a(bcint,1); a(bconsole,1); a(bacl,1);
  b(bmcl,0);
  IF ?1(mcl) THEN
    print('Ты уверен что вставил нужный кабель?\n');
    wait;
    WHILE ?1(mcl) DO
      print('Разберись почему нет сигнала BMCL.\n');
      wait;
    END;
  END;
  b(bmcl,1);
  IF ?0(mcl) THEN
    print('Выключи тумблер MCL!\r');
    REPEAT UNTIL ?1(mcl);
    print('                    \r');
  END;
  ScanFlags;
  IF     Flag?('f') THEN
    power(TRUE,2);
    print('test ROM       \r'); testROM;
  ELSE
    FOR i:=-2 TO 2 BY 4 DO
      power(TRUE,i);
      print('test inhibit   \r'); testinhib;
      print('test rg        \r'); testrg;
      print('test 1804ВУ2   \r'); testWU2;
      print('test const     \r'); testconst;
      print('test csh       \r'); testcsh;
      print('test interrupt \r'); testipt;
    END;
    print('test ROM       \r'); testROM;
  END;
END bmu6.
