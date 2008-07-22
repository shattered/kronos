MODULE dry; (*$T-$R-$I- 04-Mar-91. (c) KRONOS *)

IMPORT  tty : Terminal;
IMPORT  tim : Time;
IMPORT  low : lowLevel;
IMPORT  heap: Heap;

TYPE Enumeration   = ( Ident1, Ident2, Ident3, Ident4, Ident5 );
     OneToThirty   = INTEGER;
     OneToFifty    = INTEGER;
     CapitalLetter = CHAR;
     String30      = ARRAY [0..30]                  OF CHAR;
     Array1Dim     = ARRAY [0..50]                  OF INTEGER;
     Array2Dim     = ARRAY [0..50] OF ARRAY [0..50] OF INTEGER;
     RecordPtr     = POINTER TO RecordType;
     RecordType    = RECORD
                       PtrComp   : RecordPtr;
                       Discr     : Enumeration;
                       EnumComp  : Enumeration;
                       IntComp   : OneToFifty;
                       StringComp: String30
                     END;


CONST NULL = 0;


VAR IntGlob    : INTEGER;
    BoolGlob   : BOOLEAN;
    Char1Glob  : CHAR;
    Char2Glob  : CHAR;
    Array1Glob : Array1Dim;
    Array2Glob : Array2Dim;
    PtrGlob    : RecordPtr;
    PtrGlobNext: RecordPtr;

PROCEDURE Proc2 (VAR IntParIO: OneToFifty);
  VAR  IntLoc : OneToFifty;
       EnumLoc: Enumeration;
BEGIN
  IntLoc:=IntParIO + 10;
  LOOP
    IF Char1Glob='A' THEN
      DEC(IntLoc);
      IntParIO:= IntLoc - IntGlob;
      EnumLoc := Ident1;
    END;
    IF EnumLoc=Ident1 THEN EXIT END
  END
END Proc2;

PROCEDURE Proc7 (IntParI1,IntParI2: OneToFifty; VAR IntParOut: OneToFifty); FORWARD;

PROCEDURE Proc3 (PtrParOut: RecordPtr);
BEGIN
  IF PtrGlob#NIL THEN
    PtrParOut^.PtrComp:=PtrGlob^.PtrComp
  ELSE
    IntGlob:=100
  END;
  Proc7 (10, IntGlob, PtrGlob^.IntComp)
END Proc3;

PROCEDURE Proc6 (EnumParIn: Enumeration; VAR EnumParOut: Enumeration); FORWARD;

PROCEDURE Proc1 (PtrParIn: RecordPtr);
BEGIN
  PtrParIn^.PtrComp^:=PtrGlob^;
  PtrParIn^.IntComp :=       5;
  PtrParIn^.PtrComp^.IntComp:=PtrParIn^.IntComp;
  PtrParIn^.PtrComp^.PtrComp:=PtrParIn^.PtrComp;
  Proc3(PtrParIn^.PtrComp^.PtrComp);
  IF PtrParIn^.PtrComp^.Discr=Ident1 THEN
    PtrParIn^.PtrComp^.IntComp:=6;
    Proc6 (PtrParIn^.EnumComp,PtrParIn^.PtrComp^.EnumComp);
    PtrParIn^.PtrComp^.PtrComp:=PtrGlob^.PtrComp;
    Proc7 (PtrParIn^.PtrComp^.IntComp, 10, PtrParIn^.PtrComp^.IntComp);
  ELSE
    PtrParIn^:=PtrParIn^.PtrComp^
  END
END Proc1;

PROCEDURE Proc4 ();
  VAR BoolLoc: BOOLEAN;
BEGIN
  BoolLoc:=Char1Glob='A';
  BoolLoc:=NOT BoolGlob;
  Char2Glob:= 'B'
END Proc4;

PROCEDURE Proc5 ();
BEGIN
  Char1Glob:= 'A';
  BoolGlob := FALSE
END Proc5;

PROCEDURE Func3 (EnumParIn: Enumeration): BOOLEAN; FORWARD;

PROCEDURE Proc6 (EnumParIn: Enumeration; VAR EnumParOut: Enumeration);
BEGIN
  EnumParOut:=EnumParIn;
  IF NOT Func3(EnumParIn) THEN EnumParOut:=Ident4 END;
  CASE EnumParIn OF
    |Ident1: EnumParOut:=Ident1
    |Ident2: IF IntGlob>100 THEN EnumParOut:=Ident1 ELSE EnumParOut:=Ident4 END
    |Ident3: EnumParOut:= Ident2
    |Ident4:
    |Ident5: EnumParOut:= Ident3
  END
END Proc6;

PROCEDURE Proc7 (IntParI1,IntParI2: OneToFifty; VAR IntParOut: OneToFifty);
  VAR IntLoc: OneToFifty;
BEGIN
  IntLoc   :=IntParI1 + 2;
  IntParOut:=IntParI2 + IntLoc
END Proc7;

PROCEDURE Proc8 (VAR Array1Par: Array1Dim; VAR Array2Par: Array2Dim;
                                       IntParI1,IntParI2: OneToFifty);
  VAR IntLoc,IntIndex: OneToFifty;
BEGIN
  IntLoc:=IntParI1 + 5;
  Array1Par[IntLoc   ]:=IntParI2;
  Array1Par[IntLoc+1 ]:=Array1Par[IntLoc];
  Array1Par[IntLoc+30]:=IntLoc;
  FOR IntIndex:=IntLoc TO IntLoc+1 DO INC(Array2Par[IntLoc][IntLoc-1]) END;
  Array2Par[IntLoc+20][IntLoc]:=Array1Par[IntLoc];
  IntGlob:=5
END Proc8;

PROCEDURE Func1 (CharPar1,CharPar2: CapitalLetter): Enumeration;
  VAR CharLoc1,CharLoc2: CapitalLetter;
BEGIN
  CharLoc1:=CharPar1;
  CharLoc2:=CharLoc1;
  IF CharLoc2 # CharPar2 THEN RETURN Ident1 ELSE RETURN Ident2 END
END Func1;

PROCEDURE Func2 (VAR StrParI1,StrParI2: String30): BOOLEAN;
  VAR IntLoc : OneToThirty;
      CharLoc: CapitalLetter;
BEGIN
  IntLoc:=1;
  WHILE IntLoc<=1 DO
    IF Func1 (StrParI1[IntLoc], StrParI2[IntLoc + 1]) = Ident1 THEN
      CharLoc:= 'A'; INC(IntLoc)
    END
  END;
  IF BITSET(CharLoc>='W') * BITSET(CharLoc<='Z') # {} THEN IntLoc:=7 END;
  IF CharLoc='X' THEN RETURN TRUE
  ELSE
    IF StrParI1#StrParI2 THEN
      INC(IntLoc,7); RETURN TRUE
    ELSE
      RETURN FALSE
    END
  END
END Func2;

PROCEDURE Func3 (EnumParIn: Enumeration): BOOLEAN;
  VAR EnumLoc: Enumeration;
BEGIN
  EnumLoc:=EnumParIn;
  IF EnumLoc=Ident3 THEN RETURN TRUE END;
  RETURN FALSE
END Func3;

PROCEDURE Proc0 ();
CONST LOOPS = 50000;
VAR   IntLoc1  : OneToFifty;
      IntLoc2  : OneToFifty;
      IntLoc3  : OneToFifty;
      CharLoc  : CHAR;
      CharIndex: CHAR;
      EnumLoc  : Enumeration;
      String1Loc: String30;
      String2Loc: String30;
      starttime : INTEGER;
      benchtime : INTEGER;
      nulltime  : INTEGER;
      i         : INTEGER;
BEGIN
  low.zero(Array1Glob);
  low.zero(Array2Glob);
  low.zero(String1Loc);
  tty.print ("Please, wait about 60 seconds\n");
  heap.ALLOCATE(PtrGlobNext,SIZE(RecordType));
  heap.ALLOCATE(PtrGlob    ,SIZE(RecordType));
  low._zero(PtrGlob    ,SIZE(RecordType));
  low._zero(PtrGlobNext,SIZE(RecordType));
  PtrGlob^.PtrComp   :=PtrGlobNext;
  PtrGlob^.Discr     :=Ident1;
  PtrGlob^.EnumComp  :=Ident3;
  PtrGlob^.IntComp   :=    40;
  PtrGlob^.StringComp:="DHRYSTONE PROGRAM, SOME STRING";
  starttime:=tim.sys_time (tim.milisec);
  FOR i:=0 TO LOOPS-1 DO END;
  nulltime :=tim.sys_time (tim.milisec) - starttime;
  starttime:=tim.sys_time(tim.milisec);
  FOR i:=0 TO LOOPS-1 DO
    Proc5 ();
    Proc4 ();
    IntLoc1   :=2;
    IntLoc2   :=3;
    String2Loc:="DHRYSTONE PROGRAM, 2'ND STRING";
    EnumLoc   :=Ident2;
    BoolGlob  :=NOT Func2 (String1Loc, String2Loc);
    WHILE IntLoc1 < IntLoc2 DO
      IntLoc3:= 5 * IntLoc1 - IntLoc2;
      Proc7 (IntLoc1, IntLoc2, IntLoc3);
      INC(IntLoc1)
    END;
    Proc8 (Array1Glob, Array2Glob, IntLoc1, IntLoc3);
    Proc1 (PtrGlob);
    FOR CharIndex:='A' TO Char2Glob DO
      IF Func1(CharIndex,'C')=EnumLoc THEN Proc6(Ident1,EnumLoc) END
    END;
    IntLoc3:= IntLoc2 * IntLoc1;
    IntLoc2:= IntLoc3 / IntLoc1;
    IntLoc2:= 7* (IntLoc3 - IntLoc2) - IntLoc1;
    Proc2  (IntLoc1)
  END;
  benchtime:=tim.sys_time(tim.milisec) - starttime - nulltime;
  tty.print ("Dhrystone time for %d passes = %d secs\n"
                            , LOOPS, benchtime DIV 1000);
  tty.print ("This machine benchmarks at %d dhrystones/second\n",
                                            LOOPS*1000 DIV benchtime);
END Proc0;

BEGIN
  Proc0
END dry.


1258 KRONOS P2.6 300/300ns with    checks
1349 KRONOS P2.6 300/300ns without checks
1556 KRONOS P2.6 250/300ns without checks
