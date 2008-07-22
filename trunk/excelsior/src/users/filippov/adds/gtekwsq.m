IMPLEMENTATION MODULE Gtek; (* Sem 26-Oct-86. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  cod: defCodes;
IMPORT  os : osKernel;
IMPORT  tty: Terminal;

CONST CSR=177510b DIV 2;
      Trap=310b DIV 4;
      BufSize=1024;

VAR Buf: ARRAY [0..BufSize-1] OF CHAR;
    Inp,Out: INTEGER;
    Exist: os.signal_rec;
    Wsp: ARRAY [0..199] OF INTEGER;
    Drv,Ipted: os.PROCESS;
    Time: INTEGER;
    Activ: BOOLEAN;

PROCEDURE QIN(a: SYSTEM.WORD): SYSTEM.WORD; CODE cod.inp  END QIN;
PROCEDURE QOUT(a,w: SYSTEM.WORD);    CODE cod.out  END QOUT;
PROCEDURE SETM(m: BITSET);    CODE cod.setm END SETM;
PROCEDURE GETM(): BITSET;     CODE cod.getm END GETM;
PROCEDURE TRANSFER(VAR f,t: os.PROCESS); CODE cod.tra END TRANSFER;

PROCEDURE Driver;
  VAR Ch: CHAR; nInp: INTEGER;
BEGIN
  SETM(GETM()-{0..1});
  LOOP
    Ch:=CHAR(BITSET(QIN(CSR+1))*{0..6});
    nInp:=(Inp+1) MOD BufSize;
    IF nInp#Out THEN Buf[Inp]:=Ch; Inp:=nInp; os.send(Exist) END;
    Time:=0;
    TRANSFER(Drv,Ipted);
  END;
END Driver;

PROCEDURE Argus;
BEGIN
  IF Time>0 THEN
    DEC(Time);
    IF Time=0 THEN
      Buf[Inp]:=1c; Inp:=(Inp+1) MOD BufSize; os.send(Exist);
      ASSERT(Inp#Out);
    END;
  END;
END Argus;

PROCEDURE ReadChar1(t: INTEGER): CHAR;
  VAR Ch: CHAR; ei: BITSET;
BEGIN
  ei:=GETM(); SETM(ei-{0..1});
  Time:=t; os.wait(Exist); Time:=0;
  Ch:=Buf[Out]; Out:=(Out+1) MOD BufSize;
  SETM(ei); RETURN Ch;
END ReadChar1;

PROCEDURE sh(c: CHAR);
BEGIN
  IF c<40c THEN
    tty.print('%bc\n',c);
  ELSE
    tty.print('%c\n',c);
  END;
END sh;

PROCEDURE ReadChar(t: INTEGER): CHAR;
  VAR Ch: CHAR;
BEGIN
  Ch:=ReadChar1(t);
--  sh(Ch);
  RETURN Ch;
END ReadChar;

PROCEDURE Put(Ch: CHAR);
BEGIN
--  Terminal.Write('!'); sh(Ch);
  REPEAT UNTIL 7 IN BITSET(QIN(CSR+2));
  QOUT(CSR+3,Ch);
END Put;

PROCEDURE PutGet(ch: CHAR; p: ErrProc): BOOLEAN;
  VAR Ch: CHAR;
BEGIN
  Put(ch); Ch:=ReadChar(50);
  IF Ch=1c  THEN p('Нет ответа от программатора.'); RETURN TRUE END;
  IF Ch#ch  THEN p('Неверный ответ от программатора.'); RETURN TRUE END;
  RETURN FALSE;
END PutGet;

PROCEDURE Clear?(p: ErrProc);
  VAR Ch: CHAR; i: INTEGER; line: ARRAY [0..79] OF CHAR;
BEGIN
  REPEAT Ch:=ReadChar(10) UNTIL Ch=1c;
  IF PutGet('V',p) THEN RETURN END;
  Put(15c);
  i:=0;
  LOOP
    Ch:=ReadChar(3000);
    IF Ch='>' THEN RETURN END;
    IF (Ch=15c)OR(Ch=12c) THEN
      line[i]:=0c;
      IF i>0 THEN p(line); i:=0 END;
    ELSIF Ch=1c THEN
      p('Нет ответа от программатора.'); RETURN
    ELSIF i<HIGH(line) THEN
      line[i]:=Ch; INC(i);
    END;
  END;
END Clear?;

VAR err: ErrProc;

PROCEDURE WriteDig(i: INTEGER): BOOLEAN;
  VAR ch: CHAR;
BEGIN
  i:=INTEGER(BITSET(i)*{0..3});
  IF i>9 THEN INC(i,7) END;
  ch:=CHAR(i+ORD('0'));
  RETURN PutGet(ch,err);
END WriteDig;

PROCEDURE WriteHex(i: INTEGER): BOOLEAN;
  VAR k: INTEGER;
BEGIN
  IF WriteDig(0) THEN RETURN TRUE END;
  k:=0;
  REPEAT INC(k); i:=i << 4 UNTIL (k=9) OR (BITSET(i)*{0..3}#{});
  WHILE (k<9) DO
    IF WriteDig(i) THEN RETURN TRUE END;
    INC(k); i:=i << 4;
  END;
  RETURN FALSE;
END WriteHex;

PROCEDURE PutByte(i: INTEGER);
  VAR j: INTEGER;
BEGIN
  j:=INTEGER(BITSET(i>>4)*{0..3});
  IF j>9 THEN INC(j,7) END; Put(CHAR(j+ORD('0')));
  j:=INTEGER(BITSET(i)*{0..3});
  IF j>9 THEN INC(j,7) END; Put(CHAR(j+ORD('0')));
END PutByte;

PROCEDURE Line(e: ErrProc);
  VAR i: INTEGER; ln: ARRAY [0..79] OF CHAR; Ch: CHAR;
BEGIN
  i:=0;
  LOOP
    Ch:=ReadChar(25);
    IF (Ch=1c)OR(Ch=15c)OR(Ch=12c) THEN EXIT END;
    IF i<HIGH(ln) THEN ln[i]:=Ch; INC(i) END;
  END;
  ln[i]:=0c; e(ln);
  IF Ch=1c THEN RETURN END;
  REPEAT Ch:=ReadChar(25) UNTIL (Ch=1c)OR(Ch='>');
END Line;

PROCEDURE Skip(ch: CHAR; e: ErrProc): BOOLEAN;
  VAR Ch: CHAR;
BEGIN
  Ch:=ReadChar(50);
  IF Ch='*' THEN Line(e); RETURN TRUE END;
  IF Ch=1c  THEN e('Нет ответа от программатора.'); RETURN TRUE END;
  IF Ch#ch  THEN e('Неверный ответ от программатора.'); RETURN TRUE END;
  RETURN FALSE;
END Skip;

PROCEDURE Check(i: INTEGER; e: ErrProc): BOOLEAN;
  VAR j: INTEGER; ch: CHAR;
BEGIN
  j:=INTEGER(BITSET(i>>4)*{0..3});
  IF j>9 THEN INC(j,7) END; ch:=CHAR(j+ORD('0'));
  IF Skip(ch,e) THEN RETURN TRUE END;
  j:=INTEGER(BITSET(i)*{0..3});
  IF j>9 THEN INC(j,7) END; ch:=CHAR(j+ORD('0'));
  RETURN Skip(ch,e);
END Check;

PROCEDURE Write(First,Last: INTEGER; get: GetProc; e: ErrProc);
  VAR i: INTEGER; Ch: CHAR;
BEGIN
  err:=e;
  REPEAT Ch:=ReadChar(10) UNTIL Ch=1c;
  IF PutGet('P',e) OR WriteHex(First) OR PutGet('-',e) THEN RETURN END;
  FOR i:=First TO Last DO
    IF i=First THEN PutByte(get(i))   END;
    IF i#Last  THEN PutByte(get(i+1)) END;
    IF Check(get(i),e) THEN RETURN END;
  END;
  REPEAT Ch:=ReadChar(50) UNTIL (Ch#12c)&(Ch#15c);
  IF Ch='*' THEN
    Line(e)
  ELSE
    IF PutGet('$',e) THEN RETURN END;
  END;
  REPEAT Ch:=ReadChar(50) UNTIL (Ch=1c)OR(Ch='>');
END Write;

PROCEDURE GetDig(VAR i: INTEGER; e: ErrProc): BOOLEAN;
  VAR Ch: CHAR;
BEGIN
  Ch:=ReadChar(25);
  IF (Ch>='0')&(Ch<='9') THEN i:=ORD(Ch)-ORD('0'); RETURN FALSE END;
  IF (Ch>='A')&(Ch<='F') THEN i:=ORD(Ch)-ORD('A')+10; RETURN FALSE END;
  IF Ch=1c  THEN e('Нет ответа от программатора.'); RETURN TRUE END;
  IF Ch='*' THEN Line(e); RETURN TRUE END;
  e('Неверный ответ от программатора.'); RETURN TRUE;
END GetDig;

PROCEDURE Read(First,Last: INTEGER; set: SetProc; e: ErrProc);
  VAR Ch: CHAR; i,j,k: INTEGER;
BEGIN
  REPEAT Ch:=ReadChar(10) UNTIL Ch=1c;
  IF PutGet('R',e) OR WriteHex(First) THEN RETURN END;
  IF PutGet(',',e) OR WriteHex(Last ) THEN RETURN END;
  IF PutGet(15c,e) THEN RETURN END;
  FOR k:=First TO Last DO
    IF GetDig(i,e) OR GetDig(j,e) THEN RETURN END;
    set(k,i*16+j);
  END;
  REPEAT Ch:=ReadChar(50) UNTIL (Ch=1c)OR(Ch='>');
END Read;

PROCEDURE SetType(n: INTEGER);
BEGIN
END SetType;

PROCEDURE IllegalType(t: CHAR): BOOLEAN;
BEGIN
  CASE t OF
  |"A","G","L","P","R"
  ,"B","H","M","Q","S"
  ,"C","I","N","X","T"
  ,"D","J","O","Y","U"
  ,"E","K","0","3","V"
  ,"1","8","9","W"
  ,"F","2","Z","7": RETURN FALSE
  ELSE RETURN TRUE
  END;
END IllegalType;

PROCEDURE Init(type: CHAR): BOOLEAN;
  VAR Ch: CHAR; a: SYSTEM.ADDRESS;
BEGIN
  IF IllegalType(type) THEN RETURN FALSE END;
  IF NOT Activ THEN
    Activ:=TRUE;
    os.new_process(Driver,SYSTEM.ADR(Wsp),SIZE(Wsp),Drv);
    a:=SYSTEM.ADDRESS(Trap*2); a^:=Drv; INC(a); a^:=SYSTEM.ADR(Ipted);
    os.ini_signal(Exist,{},0); Inp:=0; Out:=0; Time:=0;
    IF os.insert_action(Argus)#0 THEN Stop; HALT(1) END;
    QOUT(CSR,100b);
  END;
  REPEAT
    REPEAT
      REPEAT
        REPEAT
          Ch:=ReadChar(25);
          IF Ch=1c THEN
            tty.print('Init Gtek...\n');
            Put(' ');
          END;
        UNTIL Ch='>';
        Put('M'); Ch:=ReadChar(25);
      UNTIL Ch='M';
      Put(15c);
      REPEAT Ch:=ReadChar(25) UNTIL (Ch=1c)OR(Ch='>');
    UNTIL Ch='>';
    Put(type); Ch:=ReadChar(25);
  UNTIL Ch=type;
  REPEAT Ch:=ReadChar(50) UNTIL (Ch=1c) OR (Ch='>');
  RETURN TRUE

END Init;

PROCEDURE Stop;
BEGIN
  IF Activ THEN
    Activ:=FALSE;
    QOUT(CSR,0);
    os.remove_action(Argus);
  END;
END Stop;

BEGIN
  Activ:=FALSE;
  IF os.final(os.self(),Stop)=0 THEN END
END Gtek.
