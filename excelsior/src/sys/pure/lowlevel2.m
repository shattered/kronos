IMPLEMENTATION MODULE lowLevel; (* Leo 29-Nov-89. (c) KRONOS *) IMPORT  SYSTEM;

IMPORT  cod: defCodes;

VAR BAD_BBLT: BITSET;

TYPE
  WORD    = SYSTEM.WORD;
  ADDRESS = SYSTEM.ADDRESS;
  STRPTR  = POINTER TO ARRAY [0..0] OF CHAR;

PROCEDURE _move(des,sou: ADDRESS; size: INTEGER); CODE cod.move END _move;

PROCEDURE move(des,sou: ADDRESS; size: INTEGER);
BEGIN _move(des,sou,size) END move;

PROCEDURE cmove(to  : ADDRESS; ofs_to  : INTEGER;
                from: ADDRESS; ofs_from: INTEGER; clen: INTEGER);
  VAR l: INTEGER;
  pt,pf: STRPTR;        (*$<*) (*$T-*)
    lim: INTEGER;
BEGIN
  IF clen<=0 THEN
    RETURN
  ELSIF (BITSET(ofs_to) + BITSET(ofs_from)) * {0,1} = {} THEN
    to  :=to   + ofs_to   DIV 4;
    from:=from + ofs_from DIV 4;
    l:=clen DIV 4; _move(to,from,l);
    clen:=clen MOD 4;
    IF clen=0 THEN RETURN END;
    pt:=to+l; pf:=from+l;
    FOR l:=0 TO clen-1 DO pt^[l]:=pf^[l] END
  ELSE
    pt:=ADDRESS(to); pf:=ADDRESS(from);
    REPEAT
      pt^[ofs_to]:=pf^[ofs_from]; INC(ofs_to); INC(ofs_from); clen:=clen-1
    UNTIL clen=0
  END                   (*$>*)
END cmove;

PROCEDURE _zero(adr: ADDRESS; size: INTEGER);
BEGIN
  IF size>0 THEN adr^:=0; _move(adr+1,adr,size-1) END
END _zero;

PROCEDURE _fill(adr: ADDRESS; size: INTEGER; val: WORD);
BEGIN
  IF size>0 THEN adr^:=val; _move(adr+1,adr,size-1) END
END _fill;

(*$<*) (*$T-*)
PROCEDURE zero(VAR area: ARRAY OF WORD);
BEGIN
  IF HIGH(area)>=0 THEN
    area[0]:=0; _move(SYSTEM.ADR(area[1]),SYSTEM.ADR(area[0]),HIGH(area))
  END
END zero;

PROCEDURE fill(VAR area: ARRAY OF WORD; val: WORD);
BEGIN
  IF HIGH(area)>=0 THEN
    area[0]:=val; _move(SYSTEM.ADR(area[1]),SYSTEM.ADR(area[0]),HIGH(area))
  END
END fill;
(*$>*)

PROCEDURE quit; CODE cod.quit END quit;

PROCEDURE QUIT;
BEGIN quit END QUIT;

PROCEDURE _cpu (): INTEGER; CODE cod.sys 00 END _cpu;
PROCEDURE _cpum(): INTEGER; CODE cod.sys 02 END _cpum;
PROCEDURE checkm;  CODE cod.getm cod.setm END checkm;
PROCEDURE tags(): BITSET;  CODE cod.llw 2 END tags;

BEGIN
  cpu:=2;
  cpu_model:=2;
  BAD_BBLT:={0};
  cpu_vers:=0;
END lowLevel.
