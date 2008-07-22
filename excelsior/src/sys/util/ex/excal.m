MODULE excal; (* Dima 22-Mar-88. (c) KRONOS *)
              (* Ned  11-May-89. (c) KRONOS *)
              (* Ned  25-Dec-89. (c) KRONOS *)

IMPORT  sys: SYSTEM;
IMPORT   ex: myEditor;
IMPORT  str: Strings;

TYPE Symbol = (inv, eol
             , plus, minus
             , lbr, rbr, lset, rset, coma, range
             , mul, div, mod
             , num);
     SymSet = SET OF Symbol;

CONST mulop=SymSet{mul,div,mod};
      addop=SymSet{plus,minus};

VAR sy : Symbol;
    val: sys.WORD;
    inp: ARRAY [0..255] OF CHAR;
    len: INTEGER;
    pos: INTEGER;
    ch : CHAR;
   real: BOOLEAN;
   form: ARRAY [0..15] OF CHAR;
  error: BOOLEAN;

PROCEDURE GetCh;
BEGIN
  INC(pos);
  IF pos<len THEN ch:=inp[pos] ELSE ch:=0c; DEC(pos) END;
END GetCh;

PROCEDURE GetNum;
  VAR wi,wr: sys.WORD; pi,pr: INTEGER; di,dr: BOOLEAN;
BEGIN
  sy:=num;
  IF (ORD(ch)-ORD('0')) IN {0..9} THEN
    pi:=pos;
    str.iscan(wi,inp,pi,di);    -- pi:=nums.get_num (inp,pos,wi);
    pr:=pos;
    str.rscan(wr,inp,pr,dr);    -- pr:=nums.get_real(inp,pos,wr);
    IF NOT di & NOT dr THEN sy:=inv
    ELSIF (pr>pi) & NOT (inp[pr-1]=inp[pr]) THEN          -- real
      pos:=pr-1;
      IF NOT real THEN val:=TRUNC(REAL(wr)) ELSE val:=wr END;
    ELSE
      pos:=pi-1;
      IF real THEN val:=FLOAT(INTEGER(wi)) ELSE val:=wi END;
    END;
  ELSE sy:=inv
  END;
END GetNum;

PROCEDURE GetSy;
  VAR done: BOOLEAN;
BEGIN
  REPEAT
    done:=TRUE;
    CASE ch OF
     |' ': done:=FALSE
     |0c : sy:=eol
     |'=': sy:=eol;
           DEC(pos);
           WHILE (pos>=0) & (inp[pos]=' ') DO DEC(pos) END;
           INC(pos); inp[pos]:=0c;
     |'(': sy:=lbr    |')': sy:=rbr
     |'{': sy:=lset   |'}': sy:=rset  |',': sy:=coma
     |'.': IF inp[pos+1]='.' THEN sy:=range; INC(pos) ELSE sy:=inv END;
     |'+': sy:=plus   |'-': sy:=minus
     |'*': sy:=mul    |'/': sy:=div   |'\': sy:=mod
     |'%': GetCh;
           CASE ch OF
             |'i'    : form:='%i';   real:=FALSE;
             |'b'    : form:='%bb';  real:=FALSE;
             |'d'    : form:='%d';   real:=FALSE;
             |'h'    : form:='%hh';  real:=FALSE;
             |'{','}': form:='%{}';  real:=FALSE;
             |'c'    : form:='%c';   real:=FALSE;
             |'g'    : form:='%g';   real:=TRUE;
             |'e'    : form:='%e';   real:=TRUE;
             |'f'    : form:='%f';   real:=TRUE;
           ELSE
           END;
           done:=FALSE;
    ELSE GetNum
    END;
    GetCh;
  UNTIL done;
END GetSy;

----------------------------------------------------------------

PROCEDURE Binar(op: Symbol; w1,w2: sys.WORD): sys.WORD;
  TYPE BS=BITSET; INT=INTEGER;
BEGIN
  IF error THEN RETURN 0 END;
  IF real THEN
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

PROCEDURE Sum(): sys.WORD; FORWARD;

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

PROCEDURE Set(): sys.WORD;
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

PROCEDURE Factor(): sys.WORD;
  VAR w: sys.WORD; old: BOOLEAN;
BEGIN
  IF error THEN RETURN 0 END;
  IF    sy=num  THEN w:=val;
  ELSIF sy=lbr  THEN
    GetSy; w:=Sum();
    IF error OR (sy#rbr) THEN error:=TRUE; RETURN 0 END;
  ELSIF sy=lset THEN
    old:=real; real:=FALSE;
    GetSy; w:=Set();
    real:=old;
    IF error OR (sy#rset) THEN error:=TRUE; RETURN 0 END;
  ELSE error:=TRUE; RETURN 0
  END;
  GetSy; RETURN w
END Factor;

PROCEDURE Term(): sys.WORD;
  VAR w: sys.WORD; op: Symbol;
BEGIN
  IF error THEN RETURN 0 END;
  w:=Factor();
  WHILE NOT error & (sy IN mulop) DO
    op:=sy; GetSy;
    w:=Binar(op,w,Factor());
  END;
  RETURN w
END Term;

PROCEDURE Sum(): sys.WORD;
  CONST zero=0;
  VAR w: sys.WORD; op: Symbol;
BEGIN
  IF error THEN RETURN 0 END;
  IF sy IN addop THEN w:=zero ELSE w:=Term() END;
  WHILE NOT error & (sy IN addop) DO
    op:=sy; GetSy;
    w:=Binar(op,w,Term());
  END;
  RETURN w
END Sum;

PROCEDURE skip;
BEGIN
  WHILE pos<len DO
    CASE inp[pos] OF
      '0'..'9','(','+','-','%': DEC(pos); RETURN
    ELSE
    END;
    INC(pos);
  END;
END skip;

VAR    w: sys.WORD;
     new: ARRAY [0..255] OF CHAR;
  format: ARRAY [0..15] OF CHAR;

BEGIN
  form:='%d';
  real:=FALSE;
  ex.get(inp,len);
  pos:=0;
  skip;
  GetCh; GetSy; error:=FALSE;
  w:=Sum();
  IF error OR (sy#eol) THEN ex.message(TRUE,'error');
  ELSE
    str.print(format,'%%s = %s',form);
    str.print(new,format,inp,w);
    ex.put(new,str.len(new));
  END;
END excal.
