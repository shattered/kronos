IMPLEMENTATION MODULE Gtek; (* Sem 26-Oct-86. (c) KRONOS *)

IMPORT sio : SIOmBUS;
IMPORT clc : Time;
IMPORT Terminal;

-- 1c - time out

VAR
  Activ : BOOLEAN;

PROCEDURE ReadChar(t: INTEGER): CHAR;
  VAR str: ARRAY [0..3] OF CHAR; i,l,j: INTEGER;
BEGIN
  i:=0; l:=1; t:=clc.sys_time(clc.tick)+t;
  LOOP
    sio.read(str,i,l);
    IF l=0 THEN RETURN str[0] END;
    IF clc.sys_time(clc.tick)>=t THEN RETURN 1c END;
  END;
END ReadChar;

PROCEDURE Put(Ch: CHAR);
  VAR str: ARRAY [0..3] OF CHAR; i,l,t: INTEGER;
BEGIN
  i:=0; l:=1; str[0]:=Ch;
  LOOP
    sio.write(str,i,l);
    IF l=0 THEN RETURN END;
    FOR t:=0 TO 4000 DO END;
  END;
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
  i:=i MOD 16;
  IF i>9 THEN INC(i,7) END;
  ch:=CHAR(i+ORD('0'));
  RETURN PutGet(ch,err);
END WriteDig;

PROCEDURE WriteHex(i: INTEGER): BOOLEAN;
  VAR k: INTEGER;
BEGIN
  IF WriteDig(0) THEN RETURN TRUE END;
  k:=0;
  REPEAT INC(k); i:=i << 4 UNTIL (k=9)OR(i MOD 16 # 0);
  WHILE (k<9) DO
    IF WriteDig(i MOD 16) THEN RETURN TRUE END;
    INC(k); i:=i << 4;
  END;
  RETURN FALSE;
END WriteHex;

PROCEDURE PutByte(i: INTEGER);
  VAR j: INTEGER;
BEGIN
  j:=(i DIV 16) MOD 16; IF j>9 THEN INC(j,7) END; Put(CHAR(j+ORD('0')));
  j:= i         MOD 16; IF j>9 THEN INC(j,7) END; Put(CHAR(j+ORD('0')));
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
  j:=(i DIV 16) MOD 16; IF j>9 THEN INC(j,7) END; ch:=CHAR(j+ORD('0'));
  IF Skip(ch,e) THEN RETURN TRUE END;
  j:= i         MOD 16; IF j>9 THEN INC(j,7) END; ch:=CHAR(j+ORD('0'));
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
    IF (i>First)& Check(get(i-1),e) THEN RETURN END;
    IF (i=Last) & Check(get(i),e)   THEN RETURN END;
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
  VAR Ch: CHAR;
BEGIN
  IF IllegalType(type) THEN RETURN FALSE END;
  IF NOT Activ THEN Activ:=TRUE; sio.init(2) END;
  REPEAT
    REPEAT
      REPEAT
        REPEAT
          Ch:=ReadChar(25); IF Ch=1c THEN Put(' ') END;
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
  Activ:=FALSE;
END Stop;

BEGIN
  Activ:=FALSE;
END Gtek.
