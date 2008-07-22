MODULE mem; (* Sem & Ned  16-Mar-86 *)

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

PROCEDURE set_hem(on: BOOLEAN);
  VAR i: INTEGER; s: BITSET;
BEGIN
  hem_on:=on;
  IF on THEN s:={0..3} ELSE s:={0,1,3} END;
  FOR i:=0 TO 31 DO
    IF i IN banks THEN out(csr+i,s) END;
  END;
END set_hem;

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

PROCEDURE CheckPeaceA(lo,hi: ADDRESS; pat: WORD);
  VAR a,x: ADDRESS; pat1,w: WORD; min,i: INTEGER;
BEGIN
  IF lo>hi THEN RETURN END;
  print('  %$5h..%$5h',lo,hi);
  FOR x:=lo TO hi DO
    x^:=-x;
  END;
  FOR x:=lo TO hi DO
    w:=x^; IF w#-x THEN Err(x,w,-x) END;
  END
END CheckPeaceA;

PROCEDURE check_hem(b: INTEGER);
  PROCEDURE c(n: INTEGER);
    VAR k: INTEGER;
  BEGIN
    k:=INTEGER(inp(csr+b));
    IF k#n THEN print('error in correction code: %$2h %$2h\n',n,k) END;
  END c;
  VAR i: INTEGER; a: ADDRESS;
BEGIN
  IF b#0 THEN RETURN END;
  out(csr+b,{0..2});
  a:=0F00000h+b*256*256;
  FOR i:=0 TO 31 DO
    a^:={i};
    CASE i OF
      |16: c(48h); |17: c(4Eh); |18: c(4Dh); |19: c(50h);
      |20: c(56h); |21: c(55h); |22: c(59h); |23: c(5Fh);
      |24: c(60h); |25: c(66h); |26: c(6Ah); |27: c(69h);
      |28: c(6Fh); |29: c(72h); |30: c(71h); |31: c(77h);
    ELSE c(43h);
    END;
  END;
  out(csr+b,{0..3});
END check_hem;

PROCEDURE Test;
  VAR i,j: INTEGER;
BEGIN
  Count:=0;
  LOOP
    print('\npass %4d,',Count);
--    FOR i:=0 TO 31 DO check_hem(i) END;
    Pattern:={0..31}/BITSET(Pattern<<1);
    set_hem(TRUE);
    i:=0;
    LOOP
      WHILE NOT (i IN banks) DO
        IF i=31 THEN EXIT END;
        INC(i);
      END;
      j:=i;
      WHILE j IN banks DO INC(j) END;
      CheckPeace(ADDRESS(0F00000h)+i*256*256,
--    CheckPeace(ADDRESS(0F00000h)+256*128,
                 ADDRESS(0F00000h)+j*256*256-1,Pattern);
--               ADDRESS(0F00000h)+2*256*128-1,Pattern);
      i:=j;
    END;
    set_hem(FALSE);
    i:=0;
    LOOP
      WHILE NOT (i IN banks) DO
        IF i=31 THEN EXIT END;
        INC(i);
      END;
      j:=i;
      WHILE j IN banks DO INC(j) END;
      CheckPeace(ADDRESS(0F00000h)+i*256*256,
                 ADDRESS(0F00000h)+j*256*256-1,Pattern);
      i:=j;
    END;
    INC(Count);
  END;
END Test;

BEGIN
  Pattern:={1..31};
  print(' I41 Memory test (c) KRONOS\n');
  banks:={0};
  Test;
END mem.
