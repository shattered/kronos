IMPLEMENTATION MODULE deBug; (* Leo 19-Dec-88. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  cod: defCodes;
IMPORT  fmt: Formats;

TYPE ADDRESS = SYSTEM.ADDRESS;

CONST BASE=80000h;

VAR put_inp: INTEGER;           MASK16_31: BITSET; (* :={16..31} *)
    put_end: INTEGER;
    put_beg: INTEGER;
    put_out: INTEGER;            put_size: INTEGER;

    get_inp: INTEGER;           MASK00_15: BITSET; (* :={00..15} *)
    get_end: INTEGER;
    get_beg: INTEGER;
    get_out: INTEGER;            get_size: INTEGER;


        BCB: POINTER TO ARRAY [0..0FFFFh] OF CHAR;
       wBCB: POINTER TO ARRAY [0..0FFFFh] OF INTEGER;
     F0000h: POINTER TO ARRAY [0..0FFFFh] OF CHAR;
    getFLAG: INTEGER; (* byte pointers *)
    putFLAG: INTEGER; (* relative 0    *)

PROCEDURE di(): BITSET; CODE cod.getm cod.copt cod.li3 cod.bic cod.setm END di;

PROCEDURE ei(m: BITSET);CODE cod.setm END ei;

PROCEDURE waitf(flag: INTEGER);
CODE cod.copt cod.trb cod.jbsc 04 cod.drop END waitf;


CONST channel = ARRAY OF INTEGER
                --  0  1  2  3  4  5  6  7  8  9
                  {09,14,12,13,02,-1,-1,-1,11,10};

PROCEDURE _init(port: INTEGER);
  VAR adr: ADDRESS;
     chan: INTEGER;
BEGIN
  chan:=channel[port];
  ASSERT(chan>=0,4Ah);
  adr:=BASE+(0F8010h+chan*4) DIV 4;
  adr:=INTEGER( BITSET(adr^)*MASK00_15 );
  BCB:=ADDRESS( F0000h ) + adr DIV 4;     wBCB:=ADDRESS(BCB);
  getFLAG:=( INTEGER(BCB)*4 )+00;
  putFLAG:=( INTEGER(BCB)*4 )+12;

  put_end:=INTEGER( BITSET(wBCB^[5]>>16) * MASK00_15); (* 22,23 bytes BCB *)
  put_beg:=INTEGER( BITSET(wBCB^[5]    ) * MASK00_15); (* 20,21 bytes BCB *)

  get_end:=INTEGER( BITSET(wBCB^[2]>>16) * MASK00_15); (* 10,11 bytes BCB *)
  get_beg:=INTEGER( BITSET(wBCB^[2]    ) * MASK00_15); (*  8, 9 bytes BCB *)

  put_inp:=INTEGER( BITSET(wBCB^[3]>>16) * MASK00_15); (* 14,15 bytes BCB *)
  get_out:=INTEGER( BITSET(wBCB^[1]    ) * MASK00_15); (*  4, 5 bytes BCB *)

  put_size:=put_end-put_beg+1;
  get_size:=get_end-get_beg+1
END _init;

PROCEDURE sio_init(port: INTEGER);
  VAR m: BITSET;
BEGIN
  _init(port);
  m:=di();
    waitf(putFLAG);     (* put_out:=put_inp:=put_beg *)
    wBCB^[4]:=INTEGER(BITSET(wBCB^[4])-MASK00_15+BITSET(put_beg));
    wBCB^[3]:=INTEGER(BITSET(wBCB^[3])*MASK00_15+BITSET(put_beg<<16)+{0});
    waitf(getFLAG);     (* get_out:=get_inp:=get_beg *)
    wBCB^[1]:=INTEGER(BITSET(wBCB^[1])-MASK00_15+BITSET(get_beg));
    wBCB^[0]:=INTEGER(BITSET(wBCB^[0])*MASK00_15+BITSET(get_beg<<16)+{0});
    put_inp:=put_beg;
    get_out:=get_beg;
  ei(m)
END sio_init;

PROCEDURE write(VAL str: ARRAY OF CHAR; VAR Pos,Len: INTEGER);
  VAR p,l,i,put: INTEGER; ch: CHAR; m: BITSET;
BEGIN
  IF (Len<=0) THEN RETURN END;
  m:=di();
  waitf(putFLAG);
    put_out:=INTEGER( BITSET(wBCB^[4])     * MASK00_15 );
    put_inp:=INTEGER( BITSET(wBCB^[3]>>16) * MASK00_15 );
  BCB^[12]:=1c;
  IF put_inp>=put_out THEN l:=put_size - 1 - (put_inp-put_out)
  ELSE                     l:=put_out - put_inp - 1
  END;
  p:=Pos;
  IF l>Len THEN l:=Len END;
  LOOP
    IF put_inp=put_end THEN put:=put_beg ELSE put:=put_inp+1 END;
    IF (put=put_out) OR (l=0) THEN EXIT END;
    ch:=str[p];
    IF BITSET(ch=36c)#{} THEN
      IF put=put_end THEN i:=put_beg ELSE i:=put+1 END;
      IF i=put_out   THEN EXIT END;
      F0000h^[put_inp]:=15c;
      put_inp:=put; put:=i;  ch:=12c
    END;
    F0000h^[put_inp]:=ch; p:=p+1;
    put_inp:=put;         l:=l-1
  END;
  DEC(Len,p-Pos); Pos:=p;
  waitf(putFLAG);
  wBCB^[3]:=INTEGER( BITSET(wBCB^[3])-MASK16_31+BITSET(put_inp<<16)+{0} );
  ei(m)
END write;

PROCEDURE write_str(xxx: SYSTEM.WORD; VAL s: ARRAY OF CHAR; pos,len: INTEGER);
BEGIN
  REPEAT write(s,pos,len) UNTIL len=0
END write_str;

PROCEDURE print(VAL f: ARRAY OF CHAR; SEQ a: SYSTEM.WORD);
BEGIN
  fmt.format(0,write_str,f,a)
END print;

BEGIN
  MASK16_31:={16..31};
  MASK00_15:={00..15};
  F0000h:=ADDRESS( BASE + 0F0000h DIV 4 );
  sio_init(0)
;print("deBug2_5\n");
END deBug.
