IMPLEMENTATION MODULE exMem; (* Leo  23-Jun-87. (c) KRONOS *)
                             (* Ned  16-Nov-87. (c) KRONOS *)

IMPORT  cod: defCodes;
IMPORT  defErrors;
IMPORT  SYSTEM, exHead;
IMPORT  hp : Heap;
FROM exHead     IMPORT  message, start, finish, alarm;
FROM SYSTEM     IMPORT  ADDRESS, ADR;

PROCEDURE move(to,from: ADDRESS; size: INTEGER); CODE cod.move END move;

MODULE Ref; (* Leo  23-Jun-87. (c) KRONOS *)

IMPORT hp, cur, last;
FROM SYSTEM    IMPORT   ADDRESS, ADR;
FROM exHead    IMPORT   message, alarm;

EXPORT maxln, ref, appref, ref?, jump, deleteref, insert
     , recalclast, sat, gat;

CONST noSlots = 64;
      SlotSize=256;

CONST maxln=noSlots*SlotSize;

TYPE aSlot=POINTER TO ARRAY [0..SlotSize-1] OF ADDRESS;
     iSlot=POINTER TO ARRAY [0..SlotSize-1] OF INTEGER;

VAR
   rf: ARRAY [0..noSlots-1] OF aSlot;
   sz: ARRAY [0..noSlots-1] OF iSlot;
 curS: INTEGER;
 curL: INTEGER;

CONST  N  = SlotSize;
      KB  = 256;

PROCEDURE over;
BEGIN
  message(TRUE,"TOO MANY LINES!  text may be lost!");
  alarm:=TRUE;
END over;

PROCEDURE allocslot(i: INTEGER): BOOLEAN;

  VAR  j: INTEGER;  s: iSlot;  r: aSlot;

  PROCEDURE err;
  BEGIN over;
    IF rf[i]#NIL THEN hp.deallocate(rf[i],SlotSize) END; rf[i]:=NIL;
    IF sz[i]#NIL THEN hp.deallocate(sz[i],SlotSize) END; sz[i]:=NIL;
    IF last>=maxln THEN last:=maxln-1; END;
  END err;

BEGIN ASSERT((rf[i]=NIL) & (sz[i]=NIL));
  hp.allocate(rf[i],SlotSize);
  IF  rf[i]=NIL  THEN err; RETURN TRUE END;
  hp.allocate(sz[i],SlotSize);
  IF  sz[i]=NIL  THEN err; RETURN TRUE
  END;
  s:=sz[i]; r:=rf[i];
  FOR j:=0 TO SlotSize-1 DO r^[j]:=NIL; s^[j]:=0 END;
  RETURN FALSE
END allocslot;

PROCEDURE clean;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(rf) DO rf[i]:=NIL; sz[i]:=NIL  END;
END clean;

PROCEDURE jump(n: INTEGER);
BEGIN
  IF n>=maxln THEN n:=maxln-1 END;
  IF n<0  THEN n:=0 END;
  cur:=n; curS:=cur DIV N; curL:=cur MOD N;
END jump;

PROCEDURE recalclast;
BEGIN
 WHILE (last>=0) & ((sz[last DIV N]=NIL) OR (sz[last DIV N]^[last MOD N]=0)) DO
   DEC(last)
 END;
END recalclast;

PROCEDURE ref (adr: ADDRESS;     size: INTEGER);
  VAR i: INTEGER;
BEGIN
--ASSERT( (adr=NIL) = (size=0) );
  IF (rf[curS]=NIL) & (adr#NIL) THEN i:=0;
    WHILE i<=curS DO
      IF rf[i]=NIL THEN
        IF allocslot(i) THEN RETURN END;
      END; INC(i);
    END;
  END;
  IF (adr=NIL) & (rf[curS]=NIL) THEN RETURN END;
  ASSERT(rf[curS]#NIL); ASSERT(sz[curS]#NIL);
  ASSERT(size<=0FFh);
  rf[curS]^[curL]:=adr;
  sz[curS]^[curL]:=INTEGER( BITSET(sz[curS]^[curL])-{0..7}+BITSET(size) );
  IF cur>last THEN last:=cur END;
  IF (cur=last) & (size=0)  THEN recalclast END;
END ref;

PROCEDURE appref(adr: ADDRESS; size: INTEGER): BOOLEAN;
  VAR sav: INTEGER;
BEGIN sav:=cur;
  cur:=last+1; curS:=cur DIV N; curL:=cur MOD N;
  IF cur>=maxln  THEN over; jump(sav); RETURN TRUE END;
  IF rf[curS]=NIL THEN
    IF allocslot(curS) THEN jump(sav); RETURN TRUE END;
  END;
  ASSERT(size<=0FFh);
  rf[curS]^[curL]:=adr;
  sz[curS]^[curL]:=INTEGER( BITSET(sz[curS]^[curL])-{0..7}+BITSET(size) );
  last:=cur; RETURN FALSE
END appref;

PROCEDURE ref?(VAR adr: ADDRESS; VAR size: INTEGER);
BEGIN
  IF (cur<=last) & (rf[curS]#NIL) THEN
    adr :=rf[curS]^[curL];
    size:=sz[curS]^[curL] MOD 100h; RETURN
  ELSE
    adr:=NIL; size:=0
  END;
END ref?;

PROCEDURE sat(val: INTEGER);
BEGIN
  IF (cur>last) OR (rf[curS]=NIL) THEN RETURN END;
  val:=INTEGER( (BITSET(val)*{0..23}) << 8 );
  sz[curS]^[curL]:=INTEGER( BITSET(sz[curS]^[curL]) + BITSET(val) );
END sat;

PROCEDURE gat(): INTEGER;
BEGIN
  IF (cur>last) OR (rf[curS]=NIL) THEN RETURN 0 END;
  RETURN INTEGER( BITSET(sz[curS]^[curL] >> 8)*{0..23} );
END gat;

PROCEDURE deleteref(no,after: INTEGER; resh: PROC);
  VAR fRf: aSlot; fSz: iSlot;
      tRf: aSlot; tSz: iSlot;
      fL,tL,fS,tS: INTEGER;
      co,i,botm,from,to: INTEGER;
BEGIN botm:=last;
  IF cur>botm    THEN
    IF after>0 THEN resh END; RETURN
  END;
  from:=cur+no;   fL:=from MOD N;  fS:=from DIV N;  fRf:=rf[fS];  fSz:=sz[fS];
  to  :=cur;      tL:= to  MOD N;  tS:= to  DIV N;  tRf:=rf[tS];  tSz:=sz[tS];
  FOR co:=0 TO botm-from DO
    IF tL=N THEN INC(tS); tL:=0;  tRf:=rf[tS];  tSz:=sz[tS] END;
    IF fL=N THEN INC(fS); fL:=0;  fRf:=rf[fS];  fSz:=sz[fS] END;
    tRf^[tL]:=fRf^[fL];  tSz^[tL]:=fSz^[fL];
    INC(tL);             INC(fL);
    IF co=after THEN resh() END;
  END;
  FOR i:=botm-no+1 TO last DO
    rf[i DIV N]^[i MOD N]:=NIL; sz[i DIV N]^[i MOD N]:=0;
  END;
  DEC(last,no);
  IF botm-from<after THEN resh() END;
END deleteref;

PROCEDURE insert(no: INTEGER);
  VAR fRf: aSlot; fSz: iSlot;
      tRf: aSlot; tSz: iSlot;
      fL,tL,fS,tS: INTEGER;
      co,i,botm,from,to,ocur: INTEGER;
BEGIN botm:=last; ocur:=cur;
  IF ocur>botm      THEN       RETURN END;
  IF botm+no>=maxln THEN over; RETURN END;
  i:=botm+1+no;
  WHILE i>=botm DO
    IF rf[i DIV N]=NIL THEN
      IF allocslot(i DIV N) THEN RETURN END;
    END; DEC(i,SlotSize);
  END;
  from:=botm;     fL:=from MOD N;  fS:=from DIV N;  fRf:=rf[fS];  fSz:=sz[fS];
  to  :=botm+no;  tL:= to  MOD N;  tS:= to  DIV N;  tRf:=rf[tS];  tSz:=sz[tS];
  INC(last,no);
  FOR co:=0 TO from-ocur DO
    IF tL<0 THEN DEC(tS); tL:=N-1;  tRf:=rf[tS];  tSz:=sz[tS] END;
    IF fL<0 THEN DEC(fS); fL:=N-1;  fRf:=rf[fS];  fSz:=sz[fS] END;
    tRf^[tL]:=fRf^[fL];  tSz^[tL]:=fSz^[fL];
    DEC(tL);             DEC(fL);
  END;
  FOR i:=ocur TO ocur+no-1 DO
    rf[i DIV N]^[i MOD N]:=NIL; sz[i DIV N]^[i MOD N]:=0;
  END;
END insert;

BEGIN
  last:=-1; cur:=0; curS:=0; curL:=0; clean;
END Ref;

PROCEDURE maxline(): INTEGER; BEGIN RETURN maxln-1 END maxline;

CONST KB=256;  dummy="";

TYPE StrPtr=POINTER TO ARRAY [0..4095] OF CHAR;

PROCEDURE over;
BEGIN message(TRUE,"NO MEMORY FOR TEXT! it may be lost!");
  alarm:=TRUE;
END over;

VAR co: INTEGER;

PROCEDURE size?(): INTEGER;
  VAR a: ADDRESS; sz: INTEGER;
BEGIN
  ref?(a,sz);
(*IF sz=0 THEN RETURN 0 ELSE RETURN sz-1 END;*)
  RETURN sz;
END size?;

PROCEDURE adr(): ADDRESS;
  VAR pStr: ADDRESS; sz: INTEGER;
BEGIN
  ref?(pStr,sz);
  IF sz=0 THEN ASSERT(pStr=NIL); RETURN ADDRESS(dummy) END;
  ASSERT((sz>0) & (pStr#NIL));
  RETURN pStr;
END adr;

PROCEDURE dispose;
  VAR pStr: ADDRESS; sz,words: INTEGER;
BEGIN
  ref?(pStr,sz);
  IF sz=0 THEN ASSERT(pStr=NIL); RETURN END;
  ASSERT((sz>0) & (pStr#NIL));   words:=(sz+4) DIV 4;
  hp.deallocate(pStr,words);
  ref(NIL,0);
END dispose;

PROCEDURE put(VAL s: ARRAY OF CHAR; size: INTEGER);
  VAR pStr: StrPtr; sz,words: INTEGER;
BEGIN ASSERT(size>=0);
  IF (size=0) & (cur>last) THEN RETURN END;
  IF size>255 THEN size:=255 END;
  ref?(pStr,sz);
  IF (sz>0) & (sz=size) THEN
    words:=(size+4) DIV 4; ASSERT(pStr#NIL);
    move(pStr,ADR(s),words);
    pStr^[size]:=0c;
    RETURN
  END;
  IF sz=0 THEN ASSERT(pStr=NIL);
  ELSE ASSERT(sz>0); ASSERT(pStr#NIL);
    words:=(sz+4) DIV 4;
    hp.deallocate(pStr,words);
  END;
  IF size=0 THEN ref(NIL,0); RETURN END;
  words:=(size+4) DIV 4;
  hp.allocate(pStr,words);
  IF pStr=NIL THEN over; ref(NIL,0); RETURN END;
  move(pStr,ADR(s),words);
  pStr^[size]:=0c;
  ref(pStr,size);
END put;

PROCEDURE app(VAL s: ARRAY OF CHAR; size: INTEGER): BOOLEAN;
  VAR pStr: StrPtr; words: INTEGER;
BEGIN
  IF size=0 THEN RETURN appref(NIL,0) END;
  IF size>255 THEN size:=255 END;
  words:=(size+4) DIV 4;
  hp.allocate(pStr,words);
  IF pStr=NIL THEN over; RETURN TRUE END;
  move(pStr,ADR(s),words);
  pStr^[size]:=0c;
  IF appref(pStr,size) THEN
    hp.deallocate(pStr,words);
    recalclast; RETURN TRUE
  END;
  RETURN FALSE;
END app;

PROCEDURE get(VAR s: ARRAY OF CHAR; VAR size: INTEGER);
  VAR pStr: ADDRESS; sz: INTEGER;
BEGIN
  ref?(pStr,sz);
  IF (sz=0) THEN ASSERT(pStr=NIL); s[0]:=0c; size:=0; RETURN END;
  ASSERT((sz>0) & (pStr#NIL));
  move(ADR(s),pStr,(sz+4) DIV 4); size:=sz; s[sz]:=0c;
END get;

PROCEDURE delete(no,after: INTEGER; resh: PROC);
  VAR sav,i: INTEGER;
BEGIN
  sav:=cur;
  FOR i:=sav TO sav+no-1 DO jump(i); dispose END;
  jump(sav);
  deleteref(no,after,resh);
END delete;

VAR fL,fC,tL,tC: INTEGER;

PROCEDURE findframe(fl,fc,tl,tc: INTEGER);
BEGIN fL:=fl; fC:=fc; tL:=tl; tC:=tc END findframe;

PROCEDURE find(VAR line_from, col_from: INTEGER;
                   rect: BOOLEAN;
               VAL pattern: ARRAY OF CHAR): BOOLEAN;
  VAR s: StrPtr;
   size: INTEGER; (* of -s- *)
    pts: INTEGER; (* of -pattern- *)
    sav: INTEGER;
    l,c: INTEGER;
    ofs: ARRAY CHAR OF INTEGER;
      S: STRING;

  PROCEDURE maketable(): BOOLEAN;
    VAR i: INTEGER;  c: CHAR;
  BEGIN
    FOR c:=MIN(CHAR) TO MAX(CHAR) DO ofs[c]:=-1 END;
    i:=0;
    WHILE (i<=HIGH(pattern)) & (pattern[i]#0c) DO
      ofs[pattern[i]]:=i; INC(i)
    END; pts:=i;
    RETURN i=0
  END maketable;

  PROCEDURE match(): INTEGER;
    VAR i: INTEGER;
     step: INTEGER;
     next: INTEGER;
      pos: INTEGER;
  BEGIN
    next:=c;
    WHILE (next<size) & (s^[next]#pattern[0]) DO INC(next) END;
    LOOP
      i:=pts-1; pos:=next+i;
      IF pos>=size THEN RETURN -1 END;
      WHILE (i>=0) & (s^[pos]=pattern[i]) DO DEC(i); DEC(pos) END;
      IF (i<0) THEN RETURN pos+1
      ELSE step:=ofs[s^[pos]];
        IF step>i THEN INC(next,pts-step)
        ELSE           INC(next,  i-step)
        END;
      END;
    END;
  END match;

BEGIN
  ASSERT(fL>=0);
  IF maketable() THEN RETURN FALSE END;
  sav:=cur; c:=col_from;
  FOR l:=line_from TO tL-1 DO
    jump(l); s:=adr(); size:=size?();
    c:=match();
    IF (c>=0) & ((NOT rect) OR (c<=tC)) THEN
      jump(sav); line_from:=l; col_from:=c; RETURN TRUE
    END;
    IF rect THEN c:=fC ELSE c:=0 END;
  END;
  l:=tL; jump(l); s:=adr(); size:=size?();
  c:=match();
  IF (c>=0) & (c<=tC) THEN line_from:=l; col_from:=c END;
  jump(sav); RETURN (c>=0) & (c<=tC)
END find;

PROCEDURE alloc(VAR adr: ADDRESS; size: INTEGER);
BEGIN
  IF    size>0 THEN   hp.allocate(adr,+size)
  ELSIF size<0 THEN hp.deallocate(adr,-size)
  ELSE
    ASSERT(FALSE)
  END;
END alloc;

BEGIN
  findframe(-1,-1,-1,-1);
END exMem.
