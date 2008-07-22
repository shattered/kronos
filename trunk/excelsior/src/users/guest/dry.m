MODULE dry; (*$T-R- Rewriten to Modula-2 by Andy 21-Apr-88. (c) KRONOS *)

FROM Heap       IMPORT  ALLOCATE;
FROM Time       IMPORT  sys_time, tick;
FROM StdIO      IMPORT  print;

TYPE Enumeration = (Ident1, Ident2, Ident3, Ident4, Ident5);

TYPE OneToThirty   = INTEGER;
     OneToFifty    = INTEGER;
     CapitalLetter = CHAR;
     String30      = ARRAY [0..31] OF CHAR;
     Array1Dim     = ARRAY [0..51] OF INTEGER;
     Array2Dim     = ARRAY [0..51],[0..51] OF INTEGER;

TYPE
  RecordPtr=POINTER TO Record;
  Record=RECORD
    PtrComp   : RecordPtr;
    Discr     : Enumeration;
    EnumComp  : Enumeration;
    IntComp   : OneToFifty;
    StringComp: String30;
  END;

TYPE
  RecordType=Record;

CONST
  NULL=0;

PROCEDURE Func1(CharPar1,CharPar2: CapitalLetter):Enumeration; FORWARD;
PROCEDURE Func2(VAR StrParI1,StrParI2: String30):BOOLEAN; FORWARD;
PROCEDURE Proc1(PtrParIn: RecordPtr); FORWARD;
PROCEDURE Proc2(VAR IntParIO: OneToFifty); FORWARD;
PROCEDURE Proc3(VAR PtrParOut: RecordPtr); FORWARD;
PROCEDURE Proc4(); FORWARD;
PROCEDURE Proc5(); FORWARD;
PROCEDURE Proc6(EnumParIn: Enumeration;
                VAR EnumParOut: Enumeration); FORWARD;
PROCEDURE Proc7(IntParI1,IntParI2 : OneToFifty;
                VAR IntParOut: OneToFifty); FORWARD;
PROCEDURE Proc8(VAR Array1Par: Array1Dim; VAR Array2Par: Array2Dim;
                IntParI1, IntParI2: OneToFifty); FORWARD;

VAR
  IntGlob : INTEGER;
  BoolGlob: BOOLEAN;
  Char1Glob,Char2Glob:CHAR;
  Array1Glob: Array1Dim;
  Array2Glob: Array2Dim;
  PtrGlob, PtrGlobNext: RecordPtr;

PROCEDURE Proc0();
 CONST LOOPS=100000;
 VAR IntLoc1, IntLoc2, IntLoc3: OneToFifty;
     CharLoc, CharIndex: CHAR;
     EnumLoc: Enumeration;
     String1Loc, String2Loc : String30;
     starttime, benchtime, nulltime:INTEGER;
     i:INTEGER;
BEGIN
  starttime := sys_time(tick);
  FOR i:=0 TO LOOPS-1 DO END;
  nulltime:=sys_time(tick)-starttime;

  ALLOCATE(PtrGlobNext,SIZE(RecordType));
  ALLOCATE(PtrGlob,SIZE(RecordType));
  PtrGlob^.PtrComp := PtrGlobNext;
  PtrGlob^.Discr   := Ident1;
  PtrGlob^.EnumComp:= Ident3;
  PtrGlob^.IntComp := 40;
  String1Loc:= "DHRYSTONE PROGRAM, SOME STRING";
  PtrGlob^.StringComp :=String1Loc;

  starttime :=sys_time(tick);
  FOR i:=0 TO LOOPS-1 DO
    Proc5();
    Proc4();
    IntLoc1:=2;
    IntLoc2:=3;
    String2Loc := "DHRYSTONE PROGRAM; 2'ND STRING";
    EnumLoc := Ident2;
    BoolGlob := NOT Func2(String1Loc, String2Loc);
    WHILE IntLoc1 < IntLoc2 DO
                        IntLoc3 := 5*IntLoc1 - IntLoc2;
                        Proc7(IntLoc1, IntLoc2, IntLoc3);
                        INC(IntLoc1)
    END;
    Proc8 (Array1Glob, Array2Glob, IntLoc1, IntLoc3);
    Proc1 (PtrGlob);
    FOR CharIndex := 'A' TO Char2Glob DO
                    IF EnumLoc = Func1(CharIndex,'C') THEN
                         Proc6 (Ident1, EnumLoc)
                    END
    END;
    IntLoc3 := IntLoc2 * IntLoc1;
    IntLoc2 := IntLoc3 DIV IntLoc1;
    IntLoc2 := 7 * ( IntLoc3 - IntLoc2 ) - IntLoc1;
    Proc2( IntLoc1 )
  END;
  benchtime := sys_time(tick) - starttime - nulltime;
  print("Drystone time for %d passes = %.2f\n",
        LOOPS,FLOAT(benchtime)/50.);
  print("This machine benchmarks at %d drystones/second \n",
                        LOOPS*50 DIV benchtime );
END Proc0;

PROCEDURE Proc1(PtrParIn:RecordPtr);
BEGIN
        PtrParIn^.PtrComp^:= PtrGlob^;
        PtrParIn^.IntComp := 5;
        PtrParIn^.PtrComp^.IntComp := PtrParIn^.IntComp;
        PtrParIn^.PtrComp^.PtrComp := PtrParIn^.PtrComp;
        Proc3 (PtrParIn^.PtrComp^.PtrComp);
        IF    (PtrParIn^.PtrComp^.Discr = Ident1 ) THEN
                PtrParIn^.PtrComp^.IntComp := 6;
                Proc6( PtrParIn^.EnumComp, PtrParIn^.PtrComp^.EnumComp);
                PtrParIn^.PtrComp^.PtrComp := PtrGlob^.PtrComp;
                Proc7(PtrParIn^.PtrComp^.IntComp, 10,
                      PtrParIn^.PtrComp^.IntComp);
        ELSE    PtrParIn^ := PtrParIn^.PtrComp^
        END
END Proc1;

PROCEDURE Proc2(VAR IntParIO: OneToFifty);
  VAR IntLoc: OneToFifty;
     EnumLoc: Enumeration;
BEGIN
        LOOP
           IF Char1Glob = 'A' THEN
                DEC(IntLoc);
                IntParIO := IntLoc - IntGlob;
                EnumLoc  := Ident1
           END;
           IF EnumLoc = Ident1 THEN EXIT END
        END
END Proc2;

PROCEDURE Proc3(VAR PtrParOut:RecordPtr);
BEGIN
        IF INTEGER(PtrGlob) # NULL THEN
             PtrParOut := PtrGlob^.PtrComp
        ELSE IntGlob := 100 END;
        Proc7(10, IntGlob, PtrGlob^.IntComp)
END Proc3;

PROCEDURE Proc4();
  VAR BoolLoc:BOOLEAN;
BEGIN BoolLoc := (Char1Glob = 'A');
      BoolLoc := BoolLoc OR BoolGlob;
      Char2Glob := 'B';
END Proc4;

PROCEDURE Proc5();
BEGIN Char1Glob := 'A';
      BoolGlob  := FALSE;
END Proc5;

PROCEDURE Func3(EnumParIn:Enumeration):BOOLEAN; FORWARD;

PROCEDURE Proc6(EnumParIn:Enumeration;
                VAR EnumParOut:Enumeration);
BEGIN   EnumParOut := EnumParIn;
        IF NOT Func3(EnumParIn) THEN EnumParOut := Ident4 END;
        CASE EnumParIn       OF
          |Ident1: EnumParOut := Ident1
          |Ident2: IF IntGlob > 100 THEN EnumParOut := Ident1
                   ELSE EnumParOut := Ident4 END
          |Ident3: EnumParOut := Ident2
          |Ident4:
          |Ident5: EnumParOut := Ident3
        END
END Proc6;

PROCEDURE Proc7 (IntParI1, IntParI2 :OneToFifty;
                 VAR IntParOut:OneToFifty);
 VAR IntLoc:OneToFifty;
BEGIN IntLoc := IntParI1 + 2;
      IntParOut := IntParI2 + IntLoc;
END Proc7;

PROCEDURE Proc8 (VAR Array1Par:Array1Dim; VAR Array2Par:Array2Dim;
                 IntParI1, IntParI2 : OneToFifty);
 VAR IntLoc, IntIndex: OneToFifty;
BEGIN
        IntLoc := IntParI1 + 5;
        Array1Par[IntLoc] := IntParI2;
        Array1Par[IntLoc+1] := Array1Par[IntLoc];
        Array1Par[IntLoc+30] := IntLoc;
        FOR IntIndex := IntLoc TO (IntLoc+1) DO
          INC(Array2Par[IntLoc][IntLoc-1])
        END;
        Array2Par[IntLoc+20][IntLoc] := Array1Par[IntLoc];
        IntGlob := 5;
END Proc8;

PROCEDURE Func1(CharPar1, CharPar2 : CapitalLetter):Enumeration;
 VAR CharLoc1, CharLoc2 :CapitalLetter;
BEGIN
        CharLoc1 := CharPar1;
        CharLoc2 := CharLoc1;
        IF (CharLoc2 # CharPar2) THEN RETURN Ident1
        ELSE RETURN Ident2 END
END Func1;

PROCEDURE Func2(VAR StrParI1,StrParI2: String30):BOOLEAN;
 VAR IntLoc:OneToThirty;
     CharLoc:CapitalLetter;
BEGIN IntLoc := 1;
      WHILE IntLoc <= 1 DO
        IF Func1(StrParI1[IntLoc],StrParI2[IntLoc+1]) = Ident1
        THEN CharLoc := 'A'; INC(IntLoc) END
      END;
      IF (CharLoc >= 'W' ) AND (CharLoc <= 'Z' ) THEN IntLoc := 7 END;
      IF (CharLoc = 'X' ) THEN RETURN TRUE
      ELSE
        IF StrParI1 # StrParI2 THEN
             INC(IntLoc,7); RETURN TRUE
        ELSE RETURN FALSE END
      END
END Func2;

PROCEDURE Func3(EnumParIn:Enumeration):BOOLEAN;
 VAR  EnumLoc:Enumeration;
BEGIN EnumLoc:= EnumParIn;
      IF (EnumLoc = Ident3) THEN RETURN TRUE END;
      RETURN FALSE
END Func3;

VAR i,j:INTEGER;

BEGIN
  FOR i:= 0 TO HIGH(Array1Glob) DO Array1Glob[i]:=0 END;
  FOR i:= 0 TO HIGH(Array2Glob) DO
    FOR j:=0 TO HIGH(Array2Glob[0]) DO
         Array2Glob[i,j] := 0;
    END;
  END;
  Proc0
END dry.

Kronos 2.2:  240 drystones/second
Kronos 2.5:  526 drystones/second  (531 -tr)
Kronos 2.6:  930 drystones/second
IBM PC AT : 2000 drystones/second
NS32032   :  600 drystones/second  Unix
NS32032   :  800 drystones/second  Clean mashine (single user mode)
this one  :  757 drystones/second (802 -tr)
