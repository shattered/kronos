MODULE T;

IMPORT StdIO;
IMPORT Lib;

CONST
  std ::= StdIO;

TYPE S0 = SET OF [0..14];
     S1 = SET OF [0..30];


VAR a: ARRAY [0..256] OF CHAR;
VAR i,j: INTEGER;  ch: CHAR;
 str1: ARRAY [0..31] OF CHAR;
 str2: ARRAY [0..31] OF CHAR;
    c: CARDINAL;
    r: LONGCARD;
   s0: S0;
   s1: S1;
   f0: REAL;
   f1: LONGREAL;
  err: BOOLEAN;
 bool: BOOLEAN;


BEGIN
  Lib.EnableBreakCheck;
  bool:=TRUE;
  std.print("Hello World .%l40{}. %08@i %08@i\n",s1,bool,NOT bool);


  str1:="Hello";
  str2:="World";

  i:=123;
  s0:=S0{0,2..4,8..11,14};
  i:=20; j:=6;

  std.print(".%|*.*c.! .%30|{}.\n",i,j,'*',s0);

  r:=123A567FH;
  s1:=S1{0,2..4,8..11,14,23..27,30};


  f0:=3.14159265;
  f1:=3.1415926535;
  FOR i:=0 TO 10 DO
    std.print("f=% 16.5G  fl=%+20.10lG %04X\n",f0,f1,i);
    f0:=f0*(-10.0);
    f1:=f1*(-10.0);
  END
END T.



