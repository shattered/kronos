IMPLEMENTATION MODULE E7004; (* 28-Sep-88. (c) KRONOS *)

FROM Scheduler  IMPORT  Ticker;
FROM SYSTEM     IMPORT  ADDRESS;
FROM mCodeMnem IMPORT   liw, add, ror, rol, lgw2, lgw3, lxb, sxb, lib, li1,
                        li2, li3, li0E, li0F, copt, stot, jbsc, lodt,
                        li0, li8, li5, li4, li0C, lgw4;

CONST
  i_free     =   0; (* byte   1 => free ; 0 => busy *)
  i_stopped  =   1;
  i_in       =   2; (* word   next char goes here   *)
  i_out      =   4; (* word   next char from here   *)
  i_size     =   6; (* word   buffer size           *)
  i_buff_ptr =   8; (* word   16 bit offset adr of sys buffer *)
  i_buff_end =  10; (* word   offset of last byte in buffer   *)
  o_free     =  12; (* byte   1 => free ; 0 => busy *)
  o_stopped  =  13;
  o_in       =  14; (* word   next char goes here   *)
  o_out      =  16; (* word   next char from here   *)
  o_size     =  18; (* word   buffer size           *)
  o_buff_ptr =  20; (* word   16 bit offset adr of sys buffer *)
  o_buff_end =  22; (* word   offset of last byte in buffer   *)

VAR
  segF000         : POINTER TO ARRAY [0..0FFFFh] OF CHAR;
  optr,oend,oinp  : INTEGER;
  iptr,iend,iout  : INTEGER;

PROCEDURE inp?(): INTEGER;
CODE
  lgw4 li2 lgw2 93h jbsc 04h lxb lgw4 li3 lxb
  lgw4 li0 li1 sxb li8 rol add
END inp?;

PROCEDURE out!(h: INTEGER);
CODE
  copt stot li8 ror stot lgw4 li5 lodt lgw2 93h jbsc 04h sxb
  lgw4 li4 lodt sxb lgw4 li0 li1 sxb
END out!;

PROCEDURE out?(): INTEGER;
CODE
  lgw4 lib 10h lgw3 93h jbsc 04h lxb
  lgw4 lib 11h lxb lgw4 li0C li1 sxb li8 rol add
END out?;

PROCEDURE inp!(h: INTEGER);
CODE
  copt stot li8 ror stot lgw4 li0F lodt lgw3 93h jbsc 04h sxb
  lgw4 li0E lodt sxb lgw4 li0C li1 sxb
END inp!;

PROCEDURE put(c: CHAR);
  VAR ninp,out: INTEGER;
BEGIN
  out:=out?();
  IF oinp=oend   THEN ninp:=optr ELSE ninp:=oinp+1 END;
  IF ninp=out    THEN RETURN  END;
  segF000^[oinp]:=c;
  oinp:=ninp;
  inp!(oinp);
END put;

PROCEDURE out(c: CHAR);
BEGIN
  Ch:=c;
END out;

VAR ctrl    : BOOLEAN;
    shft    : BOOLEAN;
    shftlock: BOOLEAN;
    capslock: BOOLEAN;
    lat     : BOOLEAN;
    latlock : BOOLEAN;
    numlock : BOOLEAN;
    SH      : BOOLEAN;
    CP      : BOOLEAN;
    LT      : BOOLEAN;

PROCEDURE lamp;
  VAR c: CHAR;
BEGIN
  c:=340c;
  LT:=(lat#latlock) OR ctrl;
  SH:=shft#shftlock;
  CP:=(shft#shftlock#capslock) OR ctrl;
  IF LT THEN INCL(BITSET(c),0) END;
  IF SH THEN INCL(BITSET(c),1) END;
  IF CP THEN INCL(BITSET(c),2) END;
  IF numlock THEN INCL(BITSET(c),3) END;
  put(c);
END lamp;

PROCEDURE cap(c: CHAR);
BEGIN
  IF CP THEN c:=CHAR(BITSET(c)/{5}) END;
  IF ctrl THEN c:=CHAR(BITSET(c)*{0..4}) END;
  out(c);
END cap;

PROCEDURE ctr(c: CHAR);
BEGIN
  IF ctrl THEN c:=CHAR(BITSET(c)*{0..4}) END; out(c);
END ctr;

VAR key: CHAR;
    tim: INTEGER;

PROCEDURE get;
  VAR c: CHAR; inp: INTEGER;
  PROCEDURE rp;
  BEGIN
    IF key=0c THEN tim:=Ticker()+30 ELSE tim:=Ticker()+5 END;
    key:=CHAR(BITSET(c)*{0..6});
  END rp;
BEGIN
  inp:=inp?();
  IF inp=iout THEN
    IF key=0c THEN RETURN END;
    IF Ticker()<tim THEN RETURN END;
    INC(tim,5);
    c:=CHAR(ORD(key)+200b);
  ELSE
    c:=segF000^[iout];
    IF c>=200c THEN key:=0c END;
    IF iout=iend THEN iout:=iptr ELSE INC(iout) END;
    out!(iout);
  END;
  IF c>=200c THEN
    CASE ORD(c)-200b OF
     |67h     : ctrl:=TRUE; lamp;
     |64h, 2Eh: shft:=TRUE; lamp;
     |6Ch     : lat :=TRUE; lamp;
     |66h     : capslock:=NOT capslock; lamp;
     |65h     : shftlock:=NOT shftlock; lamp;
     |34h     : latlock :=NOT latlock ; lamp;
     |20h     : numlock :=NOT numlock ; lamp;
     |60h     : out(33c);
     |63h     : IF SH THEN out('!') ELSE out('1') END; rp;
     |68h     : IF SH THEN ctr('@') ELSE out('2') END; rp;
     |70h     : IF SH THEN out('#') ELSE out('3') END; rp;
     |79h     : IF SH THEN out('$') ELSE out('4') END; rp;
     |78h     : IF SH THEN out('%') ELSE out('5') END; rp;
     |59h     : IF SH THEN ctr('^') ELSE out('6') END; rp;
     |51h     : IF SH THEN out('&') ELSE out('7') END; rp;
     |48h     : IF SH THEN out('*') ELSE out('8') END; rp;
     |40h     : IF SH THEN out('(') ELSE out('9') END; rp;
     |41h     : IF SH THEN out(')') ELSE out('0') END; rp;
     |38h     : IF SH THEN ctr('_') ELSE out('-') END; rp;
     |3Bh     : IF SH THEN out('+') ELSE out('=') END; rp;
     |30h     : IF SH THEN out('~') ELSE out('`') END; rp;
     |33h     : out(3c);
     |28h     : out(10c); rp;
     |6Bh     : IF LT THEN cap('q') ELSE cap('й') END; rp;
     |6Dh     : IF LT THEN cap('w') ELSE cap('ц') END; rp;
     |73h     : IF LT THEN cap('e') ELSE cap('у') END; rp;
     |7Bh     : IF LT THEN cap('r') ELSE cap('к') END; rp;
     |58h     : IF LT THEN cap('t') ELSE cap('е') END; rp;
     |50h     : IF LT THEN cap('y') ELSE cap('н') END; rp;
     |53h     : IF LT THEN cap('u') ELSE cap('г') END; rp;
     |4Bh     : IF LT THEN cap('i') ELSE cap('ш') END; rp;
     |4Dh     : IF LT THEN cap('o') ELSE cap('щ') END; rp;
     |43h     : IF LT THEN cap('p') ELSE cap('з') END; rp;
     |3Dh     : IF LT THEN IF SH THEN out('{') ELSE ctr('[') END
                ELSE cap('х') END; rp;
     |35h     : IF SH THEN out('}') ELSE ctr(']') END; rp;
     |2Bh     : out(12c); rp;
     |2Dh     : out(15c); rp;
     |6Fh     : IF LT THEN cap('a') ELSE cap('ф') END; rp;
     |75h     : IF LT THEN cap('s') ELSE cap('ы') END; rp;
     |7Dh     : IF LT THEN cap('d') ELSE cap('в') END; rp;
     |5Dh     : IF LT THEN cap('f') ELSE cap('а') END; rp;
     |5Bh     : IF LT THEN cap('g') ELSE cap('п') END; rp;
     |55h     : IF LT THEN cap('h') ELSE cap('р') END; rp;
     |4Fh     : IF LT THEN cap('j') ELSE cap('о') END; rp;
     |47h     : IF LT THEN cap('k') ELSE cap('л') END; rp;
     |45h     : IF LT THEN cap('l') ELSE cap('д') END; rp;
     |3Fh     : IF LT THEN IF SH THEN out(':') ELSE out(';') END
                ELSE cap('ж') END; rp;
     |3Eh     : IF LT THEN IF SH THEN out('"') ELSE out("'") END
                ELSE cap('э') END; rp;
     |36h     : out(210c); rp;
     |37h     : IF CP THEN out(377c) ELSE out(337c) END; rp;
     |6Eh     : IF SH THEN out('|') ELSE ctr('\') END; rp;
     |76h     : IF LT THEN cap('z') ELSE cap('я') END; rp;
     |77h     : IF LT THEN cap('x') ELSE cap('ч') END; rp;
     |7Fh     : IF LT THEN cap('c') ELSE cap('с') END; rp;
     |5Fh     : IF LT THEN cap('v') ELSE cap('м') END; rp;
     |56h     : IF LT THEN cap('b') ELSE cap('и') END; rp;
     |57h     : IF LT THEN cap('n') ELSE cap('т') END; rp;
     |4Eh     : IF LT THEN cap('m') ELSE cap('ь') END; rp;
     |4Ch     : IF LT THEN IF SH THEN out('<') ELSE out(',') END
                ELSE cap('б') END; rp;
     |46h     : IF LT THEN IF SH THEN out('>') ELSE out(".") END
                ELSE cap('ю') END; rp;
     |44h     : IF SH THEN out('?') ELSE out('/') END; rp;
     |3Ch     : out(32c);
     |54h     : out(' '); rp;
     |1Dh     : out(201c); rp; -- up
     |16h     : out(202c); rp; -- down
     |17h     : out(203c); rp; -- right
     |1Eh     : out(204c); rp; -- left
     |10h     : out( 11c); rp; -- right tab
     |1Bh     : out(211c); rp; -- left tab
     |19h     : out(220c); rp; -- up tab
     |13h     : out(222c); rp; -- down tab
     |18h     : out(224c); rp; -- ins char
     |11h     : out(177c); rp; -- del char
     |1Ah     : out(217c); rp; -- ins line
     |0Ah     : out(231c); rp; -- del line
     |08h     : out(206c); rp; -- gold
     |7Eh     : out(205c); rp; -- silver
     |03h     : out(216c); rp; -- bronz
     |39h     : out(232c); rp; -- down swap
     |32h     : out(233c); rp; -- up swap
     |31h     : out(234c); rp; -- dup line
     |2Ah     : out(235c); rp; -- erase eol
     |0Bh     : IF numlock THEN out('7') END;
     |05h     : IF numlock THEN out('8') END;
     |23h     : IF numlock THEN out('9') END;
     |25h     : IF numlock THEN out('*') END;
     |0Dh     : IF numlock THEN out('4') END;
     |07h     : IF numlock THEN out('5') END;
     |06h     : IF numlock THEN out('6') END;
     |27h     : IF numlock THEN out('-') END;
     |0Fh     : IF numlock THEN out('1') END;
     |0Eh     : IF numlock THEN out('2') END;
     |04h     : IF numlock THEN out('3') END;
     |26h     : IF numlock THEN out('+') END;
     |0Ch     : IF numlock THEN out('0') END;
     |24h     : IF numlock THEN out(15c) END;
    ELSE
    END;
  ELSE
    CASE ORD(c) OF
     |67h     : ctrl:=FALSE; lamp;
     |64h, 2Eh: shft:=FALSE; lamp;
     |6Ch     : lat :=FALSE; lamp;
    ELSE
      IF c=key THEN key:=0c END;
    END;
  END;
END get;

PROCEDURE Word16(a: INTEGER): INTEGER;
BEGIN
  RETURN INTEGER(segF000^[a])+256*INTEGER(segF000^[a+1]);
END Word16;

PROCEDURE WordBCB(a: INTEGER): INTEGER;
BEGIN
  RETURN INTEGER(BCB_word_adr^[a])+256*INTEGER(BCB_word_adr^[a+1]);
END WordBCB;

VAR base,channel: INTEGER;

BEGIN
  channel:=13;
  segF000:=ADDRESS(0BC000h);
  base:=08010h+4*channel;
  IF (segF000^[base+2]#1c)&(segF000^[base+2]#2c) THEN HALT(1) END;
  BCB_i_byte_adr:=Word16(base)+2F0000h;
  BCB_o_byte_adr:=BCB_i_byte_adr+12;
  BCB_word_adr  :=ADDRESS(BCB_i_byte_adr DIV 4);
  optr:=WordBCB(o_buff_ptr);
  iptr:=WordBCB(i_buff_ptr);
  oend:=WordBCB(o_buff_end);
  iend:=WordBCB(i_buff_end);
  oinp:=WordBCB(o_in);
  iout:=WordBCB(i_out);
  Ch:=0c; key:=0c; tim:=0;
  ctrl    :=FALSE;
  shft    :=FALSE;
  shftlock:=FALSE;
  capslock:=FALSE;
  lat     :=FALSE;
  latlock :=TRUE;
  numlock :=FALSE;
  lamp;
END E7004.
