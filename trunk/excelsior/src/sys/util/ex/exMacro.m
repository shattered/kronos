IMPLEMENTATION MODULE exMacro; (* Leo 29-Jun-87. (c) KRONOS *)

FROM SYSTEM    IMPORT   ADR, ADDRESS;
FROM exHead    IMPORT   message;
FROM Heap      IMPORT   ALLOCATE, DEALLOCATE;

CONST empty = 0c;

TYPE
  Buf=ARRAY [0..255] OF CHAR;
  Ptr=POINTER TO Buf;
  macro=
  RECORD
    sz : INTEGER;
    ptr: Ptr;
  END;

VAR Gmacro
   ,Smacro
   ,Bmacro: ARRAY CHAR OF macro;
    seq   : Buf;
    sz    : INTEGER;
    co,lim: INTEGER;
    in,out: BOOLEAN;
    outptr: Ptr;

PROCEDURE in? (): BOOLEAN; BEGIN RETURN in  END in?;
PROCEDURE out?(): BOOLEAN; BEGIN RETURN out END out?;

PROCEDURE StartMacro;
BEGIN
  ASSERT(NOT in); in:=TRUE;  sz:=0;
END StartMacro;

PROCEDURE finish(VAR m: macro);
  VAR szW: INTEGER; d,s: ADDRESS;
BEGIN
  ASSERT(in); in:=FALSE;
  IF m.sz>0 THEN
    DEALLOCATE(m.ptr,(m.sz+3) DIV 4);
    m.sz:=0; m.ptr:=NIL;
  END;
  IF sz<=0 THEN RETURN END;
  szW:=(sz+3) DIV 4;
  ALLOCATE(m.ptr,szW);
  IF m.ptr=NIL THEN
    m.sz:=0; message(TRUE,"NO MEMORY FOR MACRO"); RETURN
  END;
  d:=m.ptr; s:=ADR(seq);
  WHILE szW>0 DO d^:=s^; INC(d); INC(s); DEC(szW) END;
  m.sz:=sz;
END finish;

PROCEDURE FinishGold(char: CHAR; del: INTEGER);
BEGIN DEC(sz,del); finish(Gmacro[char]) END FinishGold;

PROCEDURE FinishBronze(char: CHAR; del: INTEGER);
BEGIN DEC(sz,del); finish(Bmacro[char]) END FinishBronze;

VAR stk: ARRAY [0..3*32-1] OF INTEGER;
     sp: INTEGER;

VAR s: ARRAY [0..63] OF CHAR;

PROCEDURE push;
BEGIN
  ASSERT(out);
  IF sp<HIGH(stk) THEN
    stk[sp]:=INTEGER(outptr); stk[sp+1]:=co; stk[sp+2]:=lim; INC(sp,3)
  END;
END push;

PROCEDURE pop;
BEGIN
  IF sp>2 THEN DEC(sp,3);
    outptr:=Ptr(stk[sp]); co:=stk[sp+1]; lim:=stk[sp+2]; out:=TRUE
  ELSE out:=FALSE
  END
END pop;

PROCEDURE getmacro(): CHAR;
  VAR k: CHAR;
BEGIN ASSERT(out);
  WHILE out & (co>=lim) DO pop END;
  IF out THEN k:=outptr^[co]; INC(co) ELSE k:=empty END;
  RETURN k;
END getmacro;

PROCEDURE peekmacro(): CHAR;
BEGIN ASSERT(out);
  IF co>=lim THEN RETURN empty END;
  RETURN outptr^[co];
END peekmacro;

PROCEDURE intomacro(k: CHAR);
BEGIN
  IF out OR (sz>=HIGH(seq)) THEN RETURN END;
  seq[sz]:=k; INC(sz);
END intomacro;

PROCEDURE takemacro(VAR m: macro);
BEGIN
  IF out THEN push END;
  out:=TRUE; co:=0;
  WITH m DO outptr:=ptr; lim:=sz END;
END takemacro;

PROCEDURE GetBronzeMacro(char: CHAR);
BEGIN takemacro(Bmacro[char]) END GetBronzeMacro;

PROCEDURE GetGoldMacro(char: CHAR);
BEGIN takemacro(Gmacro[char]) END GetGoldMacro;

(*----------------------------------------------------------------------*)

PROCEDURE new_macro(VAR m: macro; VAR adr: ADDRESS; size: INTEGER);
BEGIN
  IF m.sz>0 THEN
    DEALLOCATE(m.ptr,(m.sz+3) DIV 4);
    m.sz:=0; m.ptr:=NIL;
  END;
  IF size<=0 THEN RETURN END;
  ALLOCATE(m.ptr,(size+3) DIV 4);
  adr:=m.ptr;
  IF m.ptr=NIL THEN m.sz:=0 ELSE m.sz:=size END;
END new_macro;

PROCEDURE newGmacro(ch: CHAR; VAR adr: ADDRESS; size: INTEGER);
BEGIN new_macro(Gmacro[ch],adr,size) END newGmacro;

PROCEDURE newBmacro(ch: CHAR; VAR adr: ADDRESS; size: INTEGER);
BEGIN new_macro(Bmacro[ch],adr,size) END newBmacro;

PROCEDURE newSmacro(ch: CHAR; VAR adr: ADDRESS; size: INTEGER);
BEGIN new_macro(Smacro[ch],adr,size) END newSmacro;

PROCEDURE getGmacro(ch: CHAR; VAR adr: ADDRESS; VAR size: INTEGER);
BEGIN adr:=Gmacro[ch].ptr; size:=Gmacro[ch].sz END getGmacro;

PROCEDURE getBmacro(ch: CHAR; VAR adr: ADDRESS; VAR size: INTEGER);
BEGIN adr:=Bmacro[ch].ptr; size:=Bmacro[ch].sz END getBmacro;

PROCEDURE getSmacro(ch: CHAR; VAR adr: ADDRESS; VAR size: INTEGER);
BEGIN adr:=Smacro[ch].ptr; size:=Smacro[ch].sz END getSmacro;


VAR i: CHAR;

BEGIN in:=FALSE; out:=FALSE; sp:=0;
  FOR i:=0c TO HIGH(Gmacro) DO Gmacro[i].ptr:=NIL; Gmacro[i].sz:=0 END;
  FOR i:=0c TO HIGH(Bmacro) DO Bmacro[i].ptr:=NIL; Bmacro[i].sz:=0 END;
  FOR i:=0c TO HIGH(Smacro) DO Smacro[i].ptr:=NIL; Smacro[i].sz:=0 END;
END exMacro.
