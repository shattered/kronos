IMPLEMENTATION MODULE dryStone; (* Leo 17-Aug-91. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  cod: defCodes;

PROCEDURE getm(): BITSET;  CODE cod.getm  END getm;
PROCEDURE setm(m: BITSET); CODE cod.setm  END setm;

PROCEDURE move(a,b,c: SYSTEM.ADDRESS); CODE cod.move END move;

PROCEDURE _zero(a: SYSTEM.ADDRESS; size: INTEGER);
BEGIN
  a^:=0; move(a+1,a,size-1)
END _zero;

PROCEDURE zero(VAR a: ARRAY OF SYSTEM.WORD);
BEGIN
  _zero(SYSTEM.ADR(a),SIZE(a))
END zero;

------------------------------ DRY -----------------------------
                              -----


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

VAR heap0: ARRAY [0..SIZE(RecordType)-1] OF INTEGER;
    heap1: ARRAY [0..SIZE(RecordType)-1] OF INTEGER;

PROCEDURE Proc0 (): INTEGER;

  CONST LOOPS = 5000;

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
  zero(Array1Glob);
  zero(Array2Glob);
  zero(String1Loc);
(*
  spo.print ("DRYSTONE TEST\n\nPlease, wait ...\n");
*)
  PtrGlobNext:=SYSTEM.ADR(heap0);
  PtrGlob    :=SYSTEM.ADR(heap1);
(*
 *heap.ALLOCATE(PtrGlobNext,SIZE(RecordType));
 *heap.ALLOCATE(PtrGlob    ,SIZE(RecordType));
 *)
  _zero(PtrGlob    ,SIZE(RecordType));
  _zero(PtrGlobNext,SIZE(RecordType));
  PtrGlob^.PtrComp   :=PtrGlobNext;
  PtrGlob^.Discr     :=Ident1;
  PtrGlob^.EnumComp  :=Ident3;
  PtrGlob^.IntComp   :=    40;
  PtrGlob^.StringComp:="DHRYSTONE PROGRAM, SOME STRING";
(*starttime:=tim.sys_time (tim.milisec); *)
  starttime:=tick*20;
  FOR i:=0 TO LOOPS-1 DO END;
(*nulltime :=tim.sys_time (tim.milisec) - starttime; *)
  nulltime :=tick*20 - starttime;
(*starttime:=tim.sys_time(tim.milisec); *)
  starttime:=tick*20;
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
(*benchtime:=tim.sys_time(tim.milisec) - starttime - nulltime;*)
  benchtime:=tick*20 - starttime - nulltime;
(*
  spo.print ("Dhrystone time for %d passes = %d secs\n"
                            , LOOPS, benchtime DIV 1000);
  spo.print ("This machine benchmarks at %d dhrystones/second\n",
                                            LOOPS*1000 DIV benchtime);
*)
  RETURN LOOPS*1000 DIV benchtime;
END Proc0;


PROCEDURE clock(VAR sn,ln: INTEGER);

  PROCEDURE cycles16; (* 16*1.75  (7/4) *)
  CODE
    cod.li0 cod.drop cod.li0 cod.drop
    cod.li0 cod.drop cod.li0 cod.drop
    cod.li0 cod.drop cod.li0 cod.drop
    cod.li0 cod.drop cod.li0 cod.drop
  END cycles16;

  PROCEDURE cycles16m;
  CODE
    cod.llw4 cod.drop cod.llw4 cod.drop
    cod.llw4 cod.drop cod.llw4 cod.drop
    cod.llw4 cod.drop cod.llw4 cod.drop
    cod.llw4 cod.drop cod.llw4 cod.drop
  END cycles16m;

  CONST LOOPS = 24*1024;

  VAR i,st,lt: INTEGER;
BEGIN
  st:=tick;
  i:=LOOPS;
  REPEAT
    cycles16; cycles16; cycles16; cycles16;
    cycles16; cycles16; cycles16; cycles16;
    cycles16; cycles16; cycles16; cycles16;
    cycles16; cycles16; cycles16; cycles16; DEC(i)
  UNTIL i=0;
  st:=(tick-st)*20;

  lt:=tick;
  i:=LOOPS;
  REPEAT
    cycles16m; cycles16m; cycles16m; cycles16m;
    cycles16m; cycles16m; cycles16m; cycles16m;
    cycles16m; cycles16m; cycles16m; cycles16m;
    cycles16m; cycles16m; cycles16m; cycles16m; DEC(i)
  UNTIL i=0;
  lt:=(tick-lt)*20;

  sn:=1000000 DIV ((LOOPS*(256+35)*7 DIV 4) DIV st);

  ln:=1000000 DIV ((LOOPS*8*16+(lt-st)*128*2) DIV (lt-st));

  st:=st*3 DIV 2 - lt DIV 2;
  sn:=1000000 DIV ((LOOPS*(6*4*16+35)+st*128*2) DIV st)

END clock;

PROCEDURE dry(VAR n: INTEGER; VAR sn,ln: INTEGER);
  VAR m: BITSET;
BEGIN
  m:=getm();
  setm(m+{1});  n:=Proc0(); clock(sn,ln);  setm(m)
END dry;

VAR  wsp: ARRAY [0..1023] OF INTEGER;
   ipted: SYSTEM.ADDRESS;
  driver: SYSTEM.ADDRESS;

PROCEDURE transfer(VAR from,to: SYSTEM.ADDRESS);
CODE
  cod.tra
END transfer;

PROCEDURE nop; END nop;

PROCEDURE timer;
BEGIN
  LOOP
    INC(tick);
    action();
    transfer(driver,ipted)
  END
END timer;

VAR   s: SYSTEM.ADDRESS;
    adr: SYSTEM.ADDRESS;

PROCEDURE lga0(): INTEGER; CODE cod.lga 0 END lga0;

PROCEDURE pc(p: PROC): INTEGER;
  VAR adr: SYSTEM.ADDRESS;
BEGIN
  adr:=INTEGER(p) MOD 1000000h;
  adr:=adr^;
  adr:=INTEGER(adr^)+INTEGER(p) DIV 1000000h;
  RETURN adr^
END pc;

BEGIN
  action:=nop;
  tick:=0;
  IF getm()*{0..1}={} THEN
    driver:=SYSTEM.ADR(wsp);
    adr:=2;   adr^:=driver;
    INC(adr); adr^:=SYSTEM.ADR(ipted);
    adr:=driver;
    adr^:=lga0();                            INC(adr);
    adr^:=SYSTEM.ADR(wsp)+16;                INC(adr);
    adr^:=pc(timer);                         INC(adr);
    adr^:={};                                INC(adr);
    adr^:=SYSTEM.ADR(wsp)+16+6;  s:=adr^;    INC(adr);   DEC(s); s^:=0;
    adr^:=SYSTEM.ADR(wsp)+SIZE(wsp)-16
  END
END dryStone.
