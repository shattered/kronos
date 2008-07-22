IMPLEMENTATION MODULE abcSLE; (* Leo   10-Dec-88. (c) KRONOS *)
                              (* Hady. 19-Feb-89. (c) KRONOS *)
IMPORT term: Terminal;
IMPORT kbrd: Keyboard;
IMPORT  mem: Heap;

TYPE str1024 = ARRAY [0..1023] OF CHAR;
     ptr1024 = POINTER TO str1024;

CONST MAGIC = 42554646h; (* 'BUFF' *)

CONST ring_high = 15;

TYPE BUFFbody = RECORD
                    tag : INTEGER;
                    ring: ARRAY [0..ring_high] OF ptr1024;
                    rlen: ARRAY [0..ring_high] OF INTEGER;
                      rp: INTEGER;
                END;

      BUFF = POINTER TO BUFFbody;

CONST ring_size=ring_high+1;

PROCEDURE get_ring(x: INTEGER; VAR str: ARRAY OF CHAR;
                 pos: INTEGER; VAR len: INTEGER);
  (* x IN {1..ring_size} *)
  VAR i: INTEGER; ptr: ptr1024;
BEGIN i:=pos;
  ASSERT(buff#NIL);
  WITH buff^ DO
    x:=(rp+ring_size-x) MOD ring_size;
    ptr:=ring[x];
    len:=rlen[x];
    IF (ptr=NIL) OR (len=0) THEN len:=0; RETURN END;
    IF len>HIGH(str)+1 THEN len:=HIGH(str)+1 END;
    WHILE i<len DO str[i]:=ptr^[i]; i:=i+1 END;
  END; (* WITH *);
END get_ring;

PROCEDURE put_ring(VAL str: ARRAY OF CHAR; len: INTEGER; move: INTEGER);

  VAR ptr: ptr1024;

  PROCEDURE realloc;
  BEGIN ASSERT(buff#NIL);
    WITH buff^ DO
      IF (ptr#NIL) & (rlen[rp]=len) THEN RETURN END;
      IF  ptr#NIL  THEN mem.DEALLOCATE(ptr,(rlen[rp]+3) DIV 4) END;
      IF len=0 THEN ptr:=NIL
      ELSE mem.ALLOCATE(ptr,(len+3) DIV 4);
      END;
      IF  ptr=NIL  THEN len:=0 END;
      ring[rp]:=ptr;   rlen[rp]:=len;
    END;
  END realloc;

  PROCEDURE str_equ(VAL s0,s1: ARRAY OF CHAR; len: INTEGER): BOOLEAN;
    VAR i: INTEGER;
  BEGIN i:=0;
    IF len>0 THEN
      REPEAT IF s0[i]#s1[i] THEN RETURN FALSE END; i:=i+1 UNTIL i=len
    END; RETURN TRUE
  END str_equ;

  VAR i,p: INTEGER;

BEGIN i:=0; ASSERT(buff#NIL);
  WITH buff^ DO
    IF (len=0) & (move#0) THEN RETURN END;
    p:=(rp+ring_size-1) MOD ring_size;
    IF (ring[p]#NIL) & (rlen[p]=len) & str_equ(ring[p]^,str,len) THEN
      RETURN
    END;
    ptr:=ring[rp];
    realloc;
    IF ptr=NIL THEN RETURN END;
    REPEAT ptr^[i]:=str[i]; i:=i+1 UNTIL i=len;
    rp :=(rp+move) MOD ring_size;
  END;
END put_ring;

PROCEDURE ref_pos; BEGIN term.SetCrs(line,cp) END ref_pos;

VAR s_l, s_c: INTEGER;
         s_b: BUFF;
         s_r: PROCEDURE (): CHAR;

PROCEDURE save;
BEGIN s_l:=line; s_c:=cp; s_b:=buff; s_r:=read;
END save;

PROCEDURE restore;
BEGIN line:=s_l; cp:=s_c; Set(s_b); read:=s_r;
END restore;

PROCEDURE SLE(VAL prompt: ARRAY OF CHAR;
              VAR    str: ARRAY OF CHAR;
           lin,col0,col1: INTEGER;
                     old: BOOLEAN;
              SEQ  terms: CHAR);

VAR
    done: BOOLEAN; (* finish editor            *)
    sp  : INTEGER; (* position in the -str-ing *)
    len : INTEGER; (* lenght of -str-ing       *)
    high: INTEGER;

    vt52: BOOLEAN;
     sp0: INTEGER;
    bump: ARRAY [0..127] OF CHAR;
   width: INTEGER;

    stop: ARRAY [0..7] OF BITSET;

     rng: INTEGER; (* rng -- ring pointer      *)

  PROCEDURE bell; BEGIN IF bel THEN term.Write(7c) END END bell;

  PROCEDURE pos(c: INTEGER); BEGIN term.SetCrs(line,c) END pos;

  PROCEDURE sync; BEGIN pos(cp) END sync;

  PROCEDURE dc; BEGIN IF term.DC() THEN END END dc;
  PROCEDURE ic; BEGIN IF term.IC() THEN END END ic;

  CONST space=
   (*0123456789012345678901234567890123456789012345678901234567890123*)
    "                                                                "
    "                                                               ";

  PROCEDURE spaces(n: INTEGER);
  BEGIN bump:=space; bump[n]:=0c; term.WriteString(bump) END spaces;

  PROCEDURE refresh;
    VAR i,j,l: INTEGER;
  BEGIN
    pos(col0);
    l:=sp0+width-1;
    IF l>=len THEN l:=len-1 END;
    j:=0; i:=sp0;
    IF i<=l THEN
      REPEAT bump[j]:=str[i]; j:=j+1; i:=i+1 UNTIL i>l;
    END;
    bump[j]:=0c;
    term.WriteString(bump);
    IF j<width THEN spaces(width-j) END;
    sync;
  END refresh;

  PROCEDURE shiftleft;
  BEGIN
    IF (sp<len) & (cp<col1-4) THEN RETURN END;
    IF (sp=len) & (cp<=col1)  THEN RETURN END;
    IF high-sp0 < width  THEN RETURN END;
    pos(col0);
    sp0:=sp0+1;
    IF vt52 THEN refresh; cp:=cp-1; sync; RETURN END;
    dc; pos(col1); ic;
    IF len-sp0>=width THEN
      pos(col1); term.Write(str[sp0+width-1])
    END;
    cp:=cp-1; sync;
  END shiftleft;

  PROCEDURE shiftright;
  BEGIN
    sp0:=sp0-1;
    pos(col1);
    IF vt52 THEN refresh; cp:=cp+1; sync; RETURN END;
    dc; pos(col0); ic;
    term.Write(str[sp0]);
    cp:=cp+1; sync
  END shiftright;

  PROCEDURE moveleft;
    VAR p: INTEGER;
  BEGIN
    IF sp=0 THEN bell; RETURN END;
    DEC(sp); DEC(cp);
    IF (cp<col0) OR ((sp0#0) & (cp<col0+4)) THEN shiftright ELSE sync END;
  END moveleft;

  PROCEDURE moveright;
    VAR p: INTEGER;
  BEGIN
    IF sp>=high THEN bell; RETURN END;
    IF sp=len THEN
      str[len]:=' '; INC(len); term.Write(' ');
    ELSE
      term.Right
    END;
    INC(cp); INC(sp); shiftleft
  END moveright;

  PROCEDURE tab(left: BOOLEAN);
  BEGIN
    IF left THEN moveleft ELSE moveright END;
    WHILE (sp MOD 8 # 0) & (sp<high) DO
      IF left THEN moveleft ELSE moveright END;
    END;
  END tab;

  PROCEDURE insert(ch: CHAR): BOOLEAN;
    VAR p,i: INTEGER;
  BEGIN
    IF (len=high) & (str[len-1]=' ') THEN DEC(len) END;
    IF len>=high THEN bell; RETURN FALSE END;
    IF sp=len THEN
      str[len]:=ch; INC(len);
    ELSE
      i:=len;
      WHILE i>=sp DO str[i+1]:=str[i]; DEC(i) END; INC(len);
      str[sp]:=ch;
      IF vt52 THEN sync; refresh; RETURN TRUE END;
      pos(col1); dc; sync; ic
    END;
    term.Write(ch); sync;
    RETURN TRUE;
  END insert;

  PROCEDURE dellete;
    VAR p,i: INTEGER;
  BEGIN
    IF sp=len   THEN RETURN END;
    IF sp=len-1 THEN term.Write(' '); sync; DEC(len); RETURN END;
    i:=sp+1;
    WHILE i<len DO str[i-1]:=str[i]; INC(i) END; DEC(len);
    IF vt52 THEN refresh; sync END;
    dc; pos(col1); ic;
    IF len-sp0>=width THEN term.Write(str[sp0+width-1]) END;
    sync;
  END dellete;

  PROCEDURE putch(ch: CHAR);
  BEGIN
    IF sp>=high THEN RETURN END;
    IF ins & (sp<len) THEN
      IF insert(ch) THEN moveright END; RETURN
    END;
    term.Write(ch); INC(cp);
    IF sp=len THEN str[len]:=ch; INC(len); sp:=len;
    ELSE str[sp]:=ch; INC(sp)
    END;
    shiftleft
  END putch;

  PROCEDURE delch;
  BEGIN
    IF sp=0 THEN bell; RETURN END;
    moveleft;
    IF (NOT ins) & (sp<len-1) THEN
      putch(' '); moveleft
    ELSE  dellete
    END;
  END delch;

  PROCEDURE jumpBOL;
  BEGIN
    IF sp0#0 THEN sp0:=0; cp:=col0; sp:=0; refresh
    ELSE cp:=col0; sp:=0
    END; sync;
  END jumpBOL;

  PROCEDURE jumpEOL;
  BEGIN
    IF len-sp0>width THEN sp0:=len-width+1;
         sp:=len; cp:=col0+(sp-sp0); refresh
    ELSE sp:=len; cp:=col0+(sp-sp0)
    END; sync;
  END jumpEOL;

  PROCEDURE get_old;
  BEGIN
    get_ring(rng,str,0,len);
    IF len>width THEN sp0:=len-width+1 ELSE sp0:=0 END;
    sp:=len;  cp:=col0+(sp-sp0);
    refresh
  END get_old;

  PROCEDURE up;
  BEGIN
    IF rng=0 THEN put_ring(str,len,0) END;
    IF rng=ring_size-1 THEN bell ELSE INC(rng); get_old END
  END up;

  PROCEDURE dup_tail;
    VAR i: INTEGER;
  BEGIN
    IF rng>=ring_size-1 THEN bell; RETURN END;
    get_ring(rng+1,str,sp,i);
    IF i<=sp THEN RETURN END; len:=i; refresh
  END dup_tail;

  PROCEDURE dw;
  BEGIN
    IF rng=0 THEN bell ELSE DEC(rng); get_old END
  END dw;

  PROCEDURE Action(key: CHAR);
  BEGIN
    IF  ORD(key) MOD 128 >= 32 THEN putch(key); RETURN  END;
    IF (ORD(key) MOD 32) IN stop[ORD(key) DIV 32] THEN
      done:=TRUE; RETURN
    END;
    IF     key=kbrd.eraln   THEN len:=sp; refresh;
    ELSIF  key=kbrd.ltab    THEN tab(TRUE)
    ELSIF  key=kbrd.rtab    THEN tab(FALSE)
    ELSIF  key=kbrd.del     THEN delch
    ELSIF  key=kbrd.up      THEN up
    ELSIF  key=kbrd.dw      THEN dw
    ELSIF  key=kbrd.dupln   THEN dup_tail
    ELSIF  key=kbrd.left    THEN moveleft
    ELSIF  key=kbrd.right   THEN moveright
    ELSIF  key=kbrd.uppg    THEN jumpBOL;
    ELSIF  key=kbrd.dwpg    THEN jumpEOL;
    ELSIF  key=kbrd.insc    THEN IF insert(' ') THEN END;
    ELSIF  key=kbrd.delc    THEN dellete;
    ELSIF  key=kbrd.ins_rep THEN ins:=NOT ins;
    ELSIF  key=7c           THEN bel:=NOT bel;
    ELSIF  key=kbrd.break   THEN sp:=0; cp:=col0; len:=0; refresh;
    ELSIF  key=12c          THEN len:=sp; refresh; done:=TRUE;
    ELSE
    END;
  END Action;

  PROCEDURE show_prompt;
    VAR i,w: INTEGER;
  BEGIN i:=0; w:=(col1-col0+1) DIV 2;
    WHILE (i<w) & (i<HIGH(bump)) & (i<=HIGH(prompt)) & (prompt[i]#0c) DO
      bump[i]:=prompt[i]; INC(i)
    END; bump[i]:=0c;
    pos(col0); term.WriteString(bump); col0:=col0+i;
  END show_prompt;

  PROCEDURE stop_char(ch: CHAR);
  BEGIN INCL(stop[ORD(ch) DIV 32],ORD(ch) MOD 32) END stop_char;

  PROCEDURE init;
    VAR i: INTEGER;
  BEGIN line:=lin;
    high:=HIGH(str);
    IF high>=1024 THEN high:=1023 END;
    done:=FALSE;
    len:=0; sp:=0; sp0:=0;
    IF old THEN
      WHILE (len<high) & (str[len]#0c) DO INC(len) END;
    END;
    show_prompt;
    width:=col1-col0+1; cp:=col0;
    sync;
    vt52:=(term.DC() OR term.IC()); spaces(col1-col0+1);
    sync;
    rng:=0;
    FOR i:=0 TO HIGH(stop) DO stop[i]:={} END;
    stop_char(15c); stop_char(kbrd.newln);
    FOR i:=0 TO HIGH(terms) DO stop_char(terms[i]) END;
  END init;

BEGIN
  init;
  IF old THEN jumpEOL; refresh END;
  REPEAT
    ASSERT(sp<=len);
    last_char:=read();
    Action(last_char);
  UNTIL done;
  str[len]:=0c;
  put_ring(str,len,1);
END SLE;

PROCEDURE New(): BUFF;
  VAR b: BUFF; i: INTEGER;
BEGIN
  mem.ALLOCATE(b,SIZE(BUFFbody)); ASSERT(b#NIL);
  WITH b^ DO
    FOR i:=0 TO ring_high DO ring[i]:=NIL; rlen[i]:=0 END;
    rp:=0; tag:=MAGIC;
  END;
  RETURN b;
END New;

PROCEDURE Set(b: BUFF);
BEGIN ASSERT(b^.tag=MAGIC); buff:=b;
END Set;

PROCEDURE init;
  VAR i: INTEGER;
BEGIN
  ins:=TRUE; bel:=TRUE; read:=kbrd.ReadKey;
END init;

BEGIN init;
END abcSLE.
