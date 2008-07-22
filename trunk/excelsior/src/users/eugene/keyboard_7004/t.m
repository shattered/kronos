MODULE t; (* 26-Sep-88. (c) KRONOS *)

FROM Terminal   IMPORT  print, Read, SetTermNo, TransparentIn,
                        TransparentOut, Write, BusyRead;



PROCEDURE out(c: CHAR);
BEGIN
  SetTermNo(0);
  IF c<40c THEN Write('^'); c:=CHAR(BITSET(c)+{6}) END;
  IF (c>=200c)&(c<240c) THEN Write('~'); c:=CHAR(BITSET(c)-{7}+{6}) END;
  Write(c);
  SetTermNo(3);
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
  Write(c);
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

PROCEDURE in(c: CHAR);
BEGIN
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
     |63h     : IF SH THEN out('!') ELSE out('1') END;
     |68h     : IF SH THEN ctr('@') ELSE out('2') END;
     |70h     : IF SH THEN out('#') ELSE out('3') END;
     |79h     : IF SH THEN out('$') ELSE out('4') END;
     |78h     : IF SH THEN out('%') ELSE out('5') END;
     |59h     : IF SH THEN ctr('^') ELSE out('6') END;
     |51h     : IF SH THEN out('&') ELSE out('7') END;
     |48h     : IF SH THEN out('*') ELSE out('8') END;
     |40h     : IF SH THEN out('(') ELSE out('9') END;
     |41h     : IF SH THEN out(')') ELSE out('0') END;
     |38h     : IF SH THEN ctr('_') ELSE out('-') END;
     |3Bh     : IF SH THEN out('+') ELSE out('=') END;
     |30h     : IF SH THEN out('~') ELSE out('`') END;
     |33h     : out(3c);
     |28h     : out(10c);
     |6Bh     : IF LT THEN cap('q') ELSE cap('й') END;
     |6Dh     : IF LT THEN cap('w') ELSE cap('ц') END;
     |73h     : IF LT THEN cap('e') ELSE cap('у') END;
     |7Bh     : IF LT THEN cap('r') ELSE cap('к') END;
     |58h     : IF LT THEN cap('t') ELSE cap('е') END;
     |50h     : IF LT THEN cap('y') ELSE cap('н') END;
     |53h     : IF LT THEN cap('u') ELSE cap('г') END;
     |4Bh     : IF LT THEN cap('i') ELSE cap('ш') END;
     |4Dh     : IF LT THEN cap('o') ELSE cap('щ') END;
     |43h     : IF LT THEN cap('p') ELSE cap('з') END;
     |3Dh     : IF LT THEN IF SH THEN out('{') ELSE ctr('[') END
                ELSE cap('х') END;
     |35h     : IF SH THEN out('{') ELSE ctr(']') END;
     |2Bh     : out(12c);
     |2Dh     : out(15c);
     |6Fh     : IF LT THEN cap('a') ELSE cap('ф') END;
     |75h     : IF LT THEN cap('s') ELSE cap('ы') END;
     |7Dh     : IF LT THEN cap('d') ELSE cap('в') END;
     |5Dh     : IF LT THEN cap('f') ELSE cap('а') END;
     |5Bh     : IF LT THEN cap('g') ELSE cap('п') END;
     |55h     : IF LT THEN cap('h') ELSE cap('р') END;
     |4Fh     : IF LT THEN cap('j') ELSE cap('о') END;
     |47h     : IF LT THEN cap('k') ELSE cap('л') END;
     |45h     : IF LT THEN cap('l') ELSE cap('д') END;
     |3Fh     : IF LT THEN IF SH THEN out(':') ELSE out(';') END
                ELSE cap('ж') END;
     |3Eh     : IF LT THEN IF SH THEN out('"') ELSE out("'") END
                ELSE cap('э') END;
     |36h     : out(210c);
     |37h     : IF CP THEN out(377c) ELSE out(337c) END;
     |6Eh     : IF SH THEN out('|') ELSE ctr('\') END;
     |76h     : IF LT THEN cap('z') ELSE cap('я') END;
     |77h     : IF LT THEN cap('x') ELSE cap('ч') END;
     |7Fh     : IF LT THEN cap('c') ELSE cap('с') END;
     |5Fh     : IF LT THEN cap('v') ELSE cap('м') END;
     |56h     : IF LT THEN cap('b') ELSE cap('и') END;
     |57h     : IF LT THEN cap('n') ELSE cap('т') END;
     |4Eh     : IF LT THEN cap('m') ELSE cap('ь') END;
     |4Ch     : IF LT THEN IF SH THEN out('<') ELSE out(',') END
                ELSE cap('б') END;
     |46h     : IF LT THEN IF SH THEN out('>') ELSE out(".") END
                ELSE cap('ю') END;
     |44h     : IF SH THEN out('?') ELSE out('/') END;
     |3Ch     : out(32c);
     |54h     : out(' ');
     |1Dh     : out(201c); -- up
     |16h     : out(202c); -- down
     |17h     : out(203c); -- right
     |1Eh     : out(204c); -- left
     |10h     : out( 11c); -- right tab
     |1Bh     : out(211c); -- left tab
     |19h     : out(220c); -- up tab
     |13h     : out(222c); -- down tab
     |18h     : out(224c); -- ins char
     |11h     : out(177c); -- del char
     |1Ah     : out(217c); -- ins line
     |0Ah     : out(231c); -- del line
     |08h     : out(206c); -- gold
     |7Eh     : out(205c); -- silver
     |03h     : out(216c); -- bronz
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
    END;
  END;
END in;

BEGIN
  ctrl    :=FALSE;
  shft    :=FALSE;
  shftlock:=FALSE;
  capslock:=FALSE;
  lat     :=FALSE;
  latlock :=TRUE;
  numlock :=FALSE;
  SetTermNo(3); TransparentIn(TRUE); TransparentOut(TRUE);
  lamp;
  LOOP in(Read()) END;
END t.
