MODULE memGD; (* Sem & Ned  16-Mar-86 *)

FROM SYSTEM    IMPORT   ADDRESS, WORD, ADR;
FROM Terminal  IMPORT   print;
FROM Scheduler  IMPORT  PROCESS;
IMPORT  mcd: mCodeMnem;

CONST
  csr=0C1C0h;

VAR
  banks  : BITSET;
  Pattern: BITSET;
  Count  : INTEGER;
  hem_on : BOOLEAN;

PROCEDURE MOVE(t,f: ADDRESS; s: INTEGER); CODE mcd.move END MOVE;
PROCEDURE MySelf(): PROCESS; CODE mcd.li0 mcd.lsw0 END MySelf;
PROCEDURE GETM(): BITSET; CODE mcd.getm END GETM;
PROCEDURE TRANSFER(VAR f,t: PROCESS); CODE mcd.tra END TRANSFER;
PROCEDURE out(n: INTEGER; v: BITSET);
CODE mcd.swap mcd.li2 mcd.ror mcd.swap 93h END out;
PROCEDURE inp(n: INTEGER): BITSET;
CODE mcd.li2 mcd.ror 92h END inp;

PROCEDURE Err(a: ADDRESS; w,pat: WORD);
  VAR d: WORD;
BEGIN
  d:=BITSET(w)/BITSET(pat);
  print('\radr %$5h, write %$8h, read %$8h\n'
        '           diff %$8h, next read %$8h',
        a,pat,w,d,a^);
  IF NOT hem_on THEN print(', hemming off\n') ELSE print('\n') END;
END Err;

PROCEDURE CheckPeace(lo,hi: ADDRESS; pat: WORD);
  VAR a,x: ADDRESS; pat1,w: WORD; min,i: INTEGER;
BEGIN
  IF lo>hi THEN RETURN END;
  print('  %$5h..%$5h',lo,hi);
  FOR x:=lo TO hi BY 33 DO
    pat1:=pat;
    IF hi>(x+32) THEN
      a:=x;
      FOR i:=0 TO 3 DO
        a^:=pat1; pat1:=pat1<<1; INC(a); a^:=pat1; pat1:=pat1<<1; INC(a);
        a^:=pat1; pat1:=pat1<<1; INC(a); a^:=pat1; pat1:=pat1<<1; INC(a);
        a^:=pat1; pat1:=pat1<<1; INC(a); a^:=pat1; pat1:=pat1<<1; INC(a);
        a^:=pat1; pat1:=pat1<<1; INC(a); a^:=pat1; pat1:=pat1<<1; INC(a);
      END;
      a^:=pat1;
    ELSE
      min:=hi; FOR a:=x TO min DO a^:=pat1; pat1:=pat1<<1 END
    END;
  END;
  FOR x:=lo TO hi BY 33 DO
    pat1:=pat;
    IF hi>(x+32) THEN
      a:=x;
      FOR i:=0 TO 7 DO
        w:=a^; IF w#pat1 THEN Err(a,w,pat1) END; pat1:=pat1<<1; INC(a);
        w:=a^; IF w#pat1 THEN Err(a,w,pat1) END; pat1:=pat1<<1; INC(a);
        w:=a^; IF w#pat1 THEN Err(a,w,pat1) END; pat1:=pat1<<1; INC(a);
        w:=a^; IF w#pat1 THEN Err(a,w,pat1) END; pat1:=pat1<<1; INC(a);
      END;
      w:=a^; IF w#pat1 THEN Err(a,w,pat1) END;
    ELSE
      min:=hi;
      FOR a:=x TO min DO
        w:=a^; IF w#pat1 THEN Err(a,w,pat1) END; pat1:=pat1<<1;
      END;
    END;
  END
END CheckPeace;

PROCEDURE Test;
  VAR i,j: INTEGER;
BEGIN
  Count:=0;
  LOOP
    print('\npass %4d,',Count);
    Pattern:={0..31}/BITSET(Pattern<<1);
    i:=0;
      CheckPeace(ADDRESS(0F00000h)+0F0000h DIV 4,
                 ADDRESS(0F00000h)+0F8000h DIV 4 -1,Pattern);
    INC(Count);
  END;
END Test;

BEGIN
  Pattern:={1..31};
  print(' GD memory test (c) KRONOS\n');
  Test;
END memGD.
