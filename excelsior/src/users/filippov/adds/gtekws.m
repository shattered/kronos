IMPLEMENTATION MODULE Gtek; (* Sem 26-Oct-86. (c) KRONOS *)

IMPORT  sys: SYSTEM;
IMPORT  tty: Terminal;
IMPORT  bio: BIO;
IMPORT  req: defRequest;
IMPORT  err: defErrors;

VAR porti: bio.FILE;
    porto: bio.FILE;

PROCEDURE ReadChar1(time: INTEGER): CHAR;
  VAR Ch: CHAR; r: req.REQUEST;
BEGIN
  time:=time*20;
  IF time<0 THEN time:=0 END;
  r.op:=req.WAIT; r.buf:=NIL;  r.len:=time;  r.pos:=0;  r.ofs:=0;
  r.res:=err.ok;
  bio.doio(porti,r);
  IF r.res=err.time_out THEN
    RETURN 1c
  ELSIF r.res=err.ok THEN
    r.op:=req.READ; r.buf:=sys.ADR(Ch);  r.len:=1;  r.pos:=0;  r.ofs:=0;
    r.res:=err.ok;
    bio.doio(porti,r);
    RETURN CHAR(BITSET(Ch)*{0..7})
  ELSE
    HALT
  END
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
  VAR r: req.REQUEST;
BEGIN
  r.op:=req.WRITE; r.buf:=sys.ADR(Ch);  r.len:=1;  r.pos:=0;  r.ofs:=0;
  r.res:=err.ok;
  bio.doio(porto,r);
END Put;

PROCEDURE PutGet(ch: CHAR; p: ErrProc): BOOLEAN;
  VAR Ch: CHAR;
BEGIN
  Put(ch); Ch:=ReadChar(50);
  IF Ch=1c  THEN p('Нет ответа от программатора.'); RETURN TRUE END;
  IF Ch#ch  THEN p('Неверный ответ от программатора. PutGet');  RETURN TRUE END;
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

VAR errp: ErrProc;

PROCEDURE WriteDig(i: INTEGER): BOOLEAN;
  VAR ch: CHAR;
BEGIN
  i:=INTEGER(BITSET(i)*{0..3});
  IF i>9 THEN INC(i,7) END;
  ch:=CHAR(i+ORD('0'));
  RETURN PutGet(ch,errp);
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
  IF Ch#ch  THEN e('Неверный ответ от программатора. Skip'); RETURN TRUE END;
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
  errp:=e;
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
  tty.print("GetDig ch=%c,%bc\n",Ch,ORD(Ch) MOD 256);
  e('Неверный ответ от программатора. GetDig'); RETURN TRUE;
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
  VAR Ch: CHAR; a: sys.ADDRESS;
BEGIN
  IF IllegalType(type) THEN RETURN FALSE END;
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

PROCEDURE Stop();
BEGIN
END Stop;

BEGIN
  bio.open(porti,"/dev/sci1","r");
  IF NOT bio.done THEN tty.print("Input driver not found\n"); HALT END;
  bio.open(porto,"/dev/sco1","w");
  IF NOT bio.done THEN tty.print("Output driver not found\n"); HALT END
END Gtek.
