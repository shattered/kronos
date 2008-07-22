IMPLEMENTATION MODULE bootDSK;

IMPORT  mcd: mCodeMnem;
IMPORT  sys: SYSTEM;

TYPE ADDRESS = sys.ADDRESS;

CONST

  drive = 0;

  CSR = 177170b DIV 2; DTR = CSR+1;

  readcom = 7; bufread = 3;

  ROUNDS = 2;

VAR map: ARRAY [0..3] OF BITSET;


PROCEDURE wait(bit: BITSET); (*
BEGIN REPEAT UNTIL i*BITSET(QIN(CSR))#{} END wait;*)
CODE mcd.copt mcd.lid CSR MOD 100h CSR DIV 100h
     mcd.inp  mcd.and mcd.jbsc 8 mcd.drop
END wait;

PROCEDURE r;
CODE
  mcd.copt

  mcd.lib 80h mcd.copt mcd.lid CSR MOD 100h CSR DIV 100h
  mcd.inp     mcd.and  mcd.jbsc 8  mcd.drop

  mcd.lid DTR MOD 100h DTR DIV 100h
  mcd.inp

  mcd.lib 80h mcd.copt mcd.lid CSR MOD 100h CSR DIV 100h
  mcd.inp     mcd.and  mcd.jbsc 8  mcd.drop

  mcd.lid DTR MOD 100h DTR DIV 100h mcd.inp      8 mcd.rol mcd.or

  mcd.lib 80h mcd.copt mcd.lid CSR MOD 100h CSR DIV 100h
  mcd.inp     mcd.and  mcd.jbsc 8 mcd.drop

  mcd.lid DTR MOD 100h DTR DIV 100h mcd.inp mcd.lib 16 mcd.rol mcd.or

  mcd.lib 80h mcd.copt mcd.lid  CSR MOD 100h CSR DIV 100h
  mcd.inp     mcd.and  mcd.jbsc 8  mcd.drop

  mcd.lid DTR MOD 100h DTR DIV 100h mcd.inp 8 mcd.ror mcd.or

  mcd.ssw0 1 mcd.add
END r;

PROCEDURE push(w: sys.WORD); CODE END push;

PROCEDURE drop; CODE mcd.drop END drop;

PROCEDURE inp(reg: INTEGER): BITSET; CODE mcd.inp END inp;

PROCEDURE out(reg: INTEGER; data: sys.WORD); CODE mcd.out END out;

PROCEDURE read_sector(trk,sec: INTEGER; buf: ADDRESS; VAR err: INTEGER);
  VAR i: INTEGER;
    sta: BITSET;
BEGIN
  err:=0;
  IF inp(CSR)#{5} THEN err:=-1; RETURN END;
  wait({5}); out(CSR,readcom+drive*16);
  wait({7}); out(DTR,sec);
  wait({7}); out(DTR,trk);
  wait({5});
  sta:=inp(CSR); out(CSR,0);
  IF sta*{15}#{} THEN err:=INTEGER(sta); RETURN END;
  out(CSR,bufread);

  push(buf);
  i:=32;
  REPEAT r; i:=i-1 UNTIL i=0;
  drop;
END read_sector;

PROCEDURE read(bno: INTEGER; buf: ADDRESS; VAR ERR: INTEGER; VAR OK: BITSET);
  VAR sex: INTEGER;     first: INTEGER;
      sno: INTEGER;     track: INTEGER;         size: INTEGER;
      sec: INTEGER;     start: INTEGER;         lsec: INTEGER;
      err: INTEGER;     round: INTEGER;
BEGIN
  ERR:=0;
  IF inp(CSR)#{5} THEN err:=-1; RETURN END;
  size:=4096;
  sex :=size DIV 128;

  first:=bno*32 MOD 26; (* first sector on track *)
  track:=bno*32 DIV 26;
  sno:=0;
  WHILE (size>0) DO
    start:=sno;  round:=0;
    WHILE (round<ROUNDS) & (size>0) DO
      sec:=first;       lsec:=start;
      WHILE  (sec<26) & (size>0) & (lsec<sex) DO
        IF sec IN map[round] THEN
          IF NOT (lsec IN OK) THEN
            read_sector(track,sec+1,buf+lsec*32,err);
            IF err=0 THEN INCL(OK,lsec)
            ELSE          ERR:=err
            END;
          END;
          size:=size-128;   sno:=sno+1;
        END;
        sec:=sec+1;        lsec:=lsec+1;
      END;
      round:=round+1;
    END;
    first:=0; track:=track+1;
  END;
END read;

PROCEDURE reset;
BEGIN
  REPEAT
    REPEAT out(CSR,40000b) UNTIL inp(CSR)={};
    REPEAT UNTIL inp(CSR)#{};
  UNTIL inp(CSR)-{15}={5};
END reset;

VAR reseted: BOOLEAN;

PROCEDURE read_block(bno: INTEGER; buf: ADDRESS; VAR ERR: INTEGER);
  VAR err: INTEGER;
      try: INTEGER;   OK: BITSET;
BEGIN
  OK:={}; ERR:=0; try:=8;
  IF NOT reseted THEN reset; reseted:=TRUE END;
  LOOP
    read(bno,buf,err,OK);
    IF err=0 THEN RETURN END;
    reset
  END;
END read_block;

BEGIN
  reseted:=FALSE;
  boot_device:="rx0";
  boot_device[2]:=CHAR(ORD("0")+drive);
  IF    ROUNDS=2 THEN
    map[0]:={00,02,04,06,08,10,12,14,16,18,20,22,24};
    map[1]:={01,03,05,07,09,11,13,15,17,19,21,23,25};
  ELSIF ROUNDS=3 THEN
    map[0]:={00,03,06,09,12,15,18,21,24};
    map[1]:={01,04,07,10,13,16,19,22,25};
    map[2]:={05,08,11,14,17,20,23,02   };
  END;
END bootDSK.
