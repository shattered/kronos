MODULE logic; (* Dima 07-Mar-88. (c) KRONOS *)

IMPORT Strings;
FROM Strings    IMPORT  Len;
FROM StdIO      IMPORT  print;
FROM Keyboard   IMPORT  ReadKey;
FROM Clock      IMPORT  miliClock;
FROM Random     IMPORT  RanGe;

MODULE Expr;
FROM Strings    IMPORT  Str0,Str1, Len, App, InsCh, AppStr;
IMPORT  RanGe;
EXPORT Form,Constr;

CONST N=70;

TYPE Oper = (not, and, or);
     Form = RECORD
              op  : Oper;
              txt : ARRAY [0..N] OF CHAR;
              val : BITSET;
            END;

CONST true={0..31};

VAR Sym : ARRAY Oper OF CHAR;
    Vals: ARRAY [0..4] OF BITSET;

PROCEDURE Paren(VAR f:Form):BOOLEAN;
BEGIN
  IF Len(f.txt)+2>N THEN RETURN TRUE END;
  InsCh(f.txt,0); f.txt[0]:='(';
  App(f.txt,')');
  f.op:=not;
  RETURN FALSE
END Paren;

PROCEDURE Apply(VAR f,g:Form; o:Oper):BOOLEAN;
BEGIN
  IF (f.op>o) & Paren(f) THEN RETURN TRUE END;
  IF (o#not) & (g.op>o) & Paren(g) THEN RETURN TRUE END;
  IF o=not THEN
    IF Len(f.txt)+1>N THEN RETURN TRUE END;
    InsCh(f.txt,0); f.txt[0]:=Sym[not];
    f.val:=true-f.val;
    f.op:=not;
    RETURN FALSE
  ELSE
    IF Len(f.txt)+Len(g.txt)+1>N THEN RETURN TRUE END;
    App(f.txt, Sym[o]); AppStr(f.txt, g.txt);
    IF o=and THEN
      f.val:=f.val*g.val
    ELSE
      f.val:=f.val+g.val
    END;
    f.op:=o;
  END;
  RETURN FALSE
END Apply;

PROCEDURE Var(VAR f:Form; i:INTEGER);
BEGIN
  Str0(f.txt); App(f.txt, CHAR(i+ORD('A')));
  f.op:=not;
  f.val:=Vals[i];
END Var;

PROCEDURE Constr(VAR f:Form; l:INTEGER):BOOLEAN;
  VAR g:Form; o:Oper; l1,l2: INTEGER;
BEGIN
  IF l<=0 THEN Var(f,RanGe(0,4)); RETURN FALSE END;
  l1:=l-1; IF RanGe(20,23)=20 THEN DEC(l1) END;
  l2:=l-1; IF RanGe(20,23)=20 THEN DEC(l2) END;
  o:=Oper(RanGe(0,2));
  RETURN Constr(f,l1) OR Constr(g,l2) OR Apply(f,g,o)
END Constr;

VAR i,j: INTEGER; b:BITSET;

BEGIN
  Sym[not]:='^';
  Sym[and]:='&';
  Sym[or ]:='+';
  FOR i:=0 TO 4 DO
    Vals[i]:={};
    FOR j:=0 TO 31 DO IF i IN BITSET(j) THEN
      INCL(Vals[i],j)
    END END;
  END;
END Expr;

PROCEDURE Task(l:INTEGER; VAR cou,time:INTEGER):BOOLEAN;
  VAR r:BOOLEAN; i:INTEGER; f:Form; b:BITSET;
BEGIN
  b:={};
  print('\n\n');
  FOR i:=0 TO 4 DO
    r:=(RanGe(0,999)<500);
    print('%c=%d ', ORD('A')+i, r);
    IF r THEN INCL(b,i) END;
  END;
  WHILE Constr(f,l) DO END;
  print('\n%s = ',f.txt);
  DEC(time,miliClock());
  r:=( (ReadKey()#'0')=(INTEGER(b) IN f.val) );
  INC(time,miliClock());
  IF r THEN INC(cou,Len(f.txt)); print('OK')
  ELSE      DEC(cou,Len(f.txt)); print('bad...')
  END;
  RETURN r
END Task;

CONST Time=2*60*1000; Level=6;

VAR ok,bad,cou,time,level,best: INTEGER;
    r: BOOLEAN;

BEGIN
  best:=0;
  LOOP
    ok:=0; bad:=0; cou:=0; time:=0; level:=1;
    REPEAT
      IF Task(level,cou,time) THEN
        INC(ok);
        IF level<Level THEN INC(level) END;
      ELSE
        INC(bad);
        IF level>1 THEN DEC(level) END;
      END;
      print('\n%d:%d, result=%d, time=%d', ok,bad,cou,time DIV 1000);
    UNTIL time>=Time;
    IF cou>best THEN best:=cou END;
    print('\n Your time is up. Once again?');
    IF ReadKey()='n' THEN EXIT END;
  END;
  IF best>0 THEN print('Your best result is %d',best)
  ELSE print('BANAN!!!')
  END;
END logic.
