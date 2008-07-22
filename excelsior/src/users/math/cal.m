IMPLEMENTATION MODULE cal; (* Dima 22-Mar-88. (c) KRONOS *)

IMPORT Image, Terminal, Edit, Keyboard;
FROM SYSTEM     IMPORT  WORD;
FROM Image      IMPORT  image0;

VAR mode: Mode;

PROCEDURE Mode?():Mode ; BEGIN RETURN mode END Mode?;
PROCEDURE Mode!(m:Mode); BEGIN mode:=m     END Mode!;

MODULE Scan;
IMPORT Mode, mode, WORD;
FROM Image      IMPORT  PeekNum, PeekReal;
EXPORT Symbol, mulop, addop, sy, val, Input, GetSy;

TYPE Symbol = (inv, eol
             , plus, minus
             , lbr, rbr, lset, rset, coma, range
             , mul, div, mod
             , num);
     SymSet = SET OF Symbol;

CONST mulop=SymSet{mul,div,mod};
      addop=SymSet{plus,minus};

VAR sy : Symbol;
    val: WORD;
    inp: ARRAY [0..255] OF CHAR;
    pos: INTEGER;
    ch : CHAR;

PROCEDURE GetCh; BEGIN
  INC(pos); ch:=inp[pos];
END GetCh;

PROCEDURE GetNum;
  VAR wi,wr: WORD; pi,pr: INTEGER;
BEGIN
  sy:=num;
  IF (ORD(ch)-ORD('0')) IN {0..9} THEN
    pi:=PeekNum (inp,pos,wi);
    pr:=PeekReal(inp,pos,wr);
    IF (pi<0) & (pr<0) THEN sy:=inv
    ELSIF (pr>pi) & NOT (inp[pr-1]=inp[pr]) THEN          -- ОЙ-ЕЙ-ЕЙ! --
      pos:=pr-1;                                          ---------------
      IF (mode#real)&(mode#bitset) THEN val:=TRUNC(REAL(wr)) ELSE val:=wr END;
    ELSE
      pos:=pi-1;
      IF mode=real THEN val:=FLOAT(INTEGER(wi)) ELSE val:=wi END;
    END;
  ELSE sy:=inv
  END;
END GetNum;

PROCEDURE GetSy;
BEGIN
  WHILE ch=' ' DO GetCh END;
  CASE ch OF
   |0c : sy:=eol
   |'(': sy:=lbr    |')': sy:=rbr
   |'{': sy:=lset   |'}': sy:=rset  |',': sy:=coma
   |'.': IF inp[pos+1]='.' THEN sy:=range; INC(pos) ELSE sy:=inv END;
   |'+': sy:=plus   |'-': sy:=minus
   |'*': sy:=mul    |'/': sy:=div   |'%': sy:=mod
  ELSE GetNum
  END;
  GetCh;
END GetSy;

PROCEDURE Input(VAL s:ARRAY OF CHAR); BEGIN
  inp:=s;
  pos:=-1;
  GetCh;
END Input;

END Scan;

VAR error: BOOLEAN;

PROCEDURE Binar(op:Symbol; w1,w2:WORD): WORD;
  TYPE BS=BITSET; INT=INTEGER;
BEGIN
  IF error THEN RETURN 0 END;
  IF mode=bitset THEN
    CASE op OF
     |plus : RETURN BS(w1)+BS(w2)     |minus: RETURN BS(w1)-BS(w2)
     |mul  : RETURN BS(w1)*BS(w2)     |div  : RETURN BS(w1)/BS(w2)
    ELSE error:=TRUE; RETURN 0
    END;
  ELSIF mode=real THEN
    CASE op OF
     |plus : RETURN REAL(w1)+REAL(w2) |minus: RETURN REAL(w1)-REAL(w2)
     |mul  : RETURN REAL(w1)*REAL(w2) |div  : RETURN REAL(w1)/REAL(w2)
    ELSE error:=TRUE; RETURN 0
    END;
  ELSE  -- mode=intN
    CASE op OF
     |plus : RETURN INT(w1)+INT(w2)   |minus: RETURN INT(w1)-INT(w2)
     |mul  : RETURN INT(w1)*INT(w2)   |div  : RETURN INT(w1) DIV INT(w2)
     |mod  : RETURN INT(w1) MOD INT(w2)
    ELSE error:=TRUE; RETURN 0
    END;
  END;
END Binar;

PROCEDURE Sum(): WORD; FORWARD;

PROCEDURE Range():BITSET;
  VAR i,n1,n2: INTEGER; b: BITSET;
BEGIN
  n1:=INTEGER(Sum());
  IF n1<0 THEN n1:=0 END;
  IF sy=range THEN GetSy; n2:=INTEGER(Sum()) ELSE n2:=n1 END;
  IF n2>31 THEN n2:=31 END;
  b:={};
  FOR i:=n1 TO n2 DO INCL(b,i) END;
  RETURN b
END Range;

PROCEDURE Set(): WORD;
  VAR b: BITSET;
BEGIN
  IF error THEN RETURN 0 END;
  IF sy=rset THEN RETURN {} END;
  b:=Range();
  WHILE NOT error & (sy=coma) DO
    GetSy;
    b:=b+Range();
  END;
  RETURN b
END Set;

PROCEDURE Factor(): WORD;
  VAR w:WORD; old:Mode;
BEGIN
  IF error THEN RETURN 0 END;
  IF    sy=num  THEN w:=val;
  ELSIF sy=lbr  THEN
    GetSy; w:=Sum();
    IF error OR (sy#rbr) THEN error:=TRUE; RETURN 0 END;
  ELSIF sy=lset THEN
    old:=mode; mode:=int10;    -- ой-ей-ей! --
    GetSy; w:=Set();
    mode:=old;
    IF error OR (sy#rset) THEN error:=TRUE; RETURN 0 END;
  ELSE error:=TRUE; RETURN 0
  END;
  GetSy; RETURN w
END Factor;

PROCEDURE Term(): WORD;
  VAR w: WORD; op: Symbol;
BEGIN
  IF error THEN RETURN 0 END;
  w:=Factor();
  WHILE NOT error & (sy IN mulop) DO
    op:=sy; GetSy;
    w:=Binar(op,w,Factor());
  END;
  RETURN w
END Term;

PROCEDURE Sum(): WORD;
  VAR w: WORD; op: Symbol;
BEGIN
  IF error THEN RETURN 0 END;
  IF sy IN addop THEN
    IF (sy=minus)&(mode=bitset) THEN w:={0..31} ELSE w:=0 END;
  ELSE w:=Term() END;
  WHILE NOT error & (sy IN addop) DO
    op:=sy; GetSy;
    w:=Binar(op,w,Term());
  END;
  RETURN w
END Sum;

VAR format: ARRAY Mode,[0..7] OF CHAR;

PROCEDURE Kalk(VAL s: ARRAY OF CHAR; VAR out:ARRAY OF CHAR);
  VAR w: WORD;
BEGIN
  Input(s); GetSy; error:=FALSE;
  w:=Sum();
  IF error OR (sy#eol) THEN image0(out,' - ERRORS!')
  ELSE image0(out, format[mode], w)
  END;
END Kalk;

MODULE Monitor;
IMPORT Mode, mode, Kalk;
FROM Edit       IMPORT  ReadString;
FROM Keyboard   IMPORT  ReadKey, PeekKey, up,dw,left,right,empty;
FROM Terminal   IMPORT  WaitUntilPressed, print,
                        Home,Clear,SetCrs, Reverse, ClearLine;
EXPORT fuck;

VAR Name: ARRAY Mode,[0..7] OF CHAR;

CONST mode_ln=2; mode_cl=0;
      kalk_ln=4; kalk_cl=2;

CONST min=int2; max=bitset;

PROCEDURE Jump; BEGIN
  SetCrs(mode_ln, mode_cl+8*INTEGER(mode));
END Jump;

PROCEDURE ToMode(m:Mode); VAR i: INTEGER; BEGIN
  i:=Reverse(0); print(Name[mode]);
  mode:=m;
  Jump;
  i:=Reverse(1); print(Name[mode]);
  Jump;
  i:=Reverse(0);
END ToMode;

PROCEDURE SetMode;
  VAR c:CHAR;
BEGIN
  Jump;
  REPEAT
    c:=ReadKey();
    IF (c=left) &(mode#min) THEN ToMode(Mode(INTEGER(mode)-1)) END;
    IF (c=right)&(mode#max) THEN ToMode(Mode(INTEGER(mode)+1)) END;
  UNTIL c=dw;
  SetCrs(kalk_ln,kalk_cl);
END SetMode;

PROCEDURE fuck;
  VAR m:Mode; str,out:ARRAY [0..255] OF CHAR;
BEGIN
  Home; Clear;
  print('              K A L K U L A T O R');
  SetCrs(mode_ln, mode_cl);
  FOR m:=min TO max DO print('%-8s',Name[m]) END;
  mode:=int10; Jump; ToMode(mode);
  SetCrs(kalk_ln, kalk_cl-2); print('>>');
  LOOP
    --WaitUntilPressed;
    REPEAT UNTIL PeekKey()#empty;
    ClearLine;
    IF PeekKey()=up THEN SetMode END;
    ReadString('',str);
    Kalk(str,out);
    print(out);
    SetCrs(kalk_ln,kalk_cl);
  END;
END fuck;

BEGIN
  Name[int2 ]:='int2';  Name[int8]:='int8'; Name[int10 ]:='int10';
  Name[int16]:='int16'; Name[real]:='real'; Name[bitset]:='bitset';
END Monitor;

BEGIN
  format[int2]:=' = %i';  format[int10]:=' = %d';
  format[int8]:=' = %b';  format[int16]:=' = %h';
  format[real]:=' = %f';  format[bitset]:=' = %{}';

  fuck;

END cal.
