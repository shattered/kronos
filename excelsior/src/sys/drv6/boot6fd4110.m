IMPLEMENTATION MODULE bootDSK; (*$T- KRG 08-Feb-89. (c) KRONOS *)


IMPORT  sys: SYSTEM;
IMPORT  tty: bootTTY;
IMPORT  mcd: mCodeMnem;

PROCEDURE inp(reg: INTEGER): BITSET;      CODE mcd.inp END inp;
PROCEDURE out(reg: INTEGER; w: sys.WORD); CODE mcd.out END out;

PROCEDURE move(to,from: sys.ADDRESS; size: INTEGER); CODE mcd.move END move;


CONST (*****************  REGISTERS  & CONSTANTS  ************************)

   CSR=177170b DIV 2; DTR=CSR+1;

             double=400b; (* 256 *)
             single=000b;

   density = double;

   bps     = density DIV 2 + 128;  -- bytes per sector (128 or 256)

   sectors=26;
   tracks=77;

   roundsR=2;

   VAR LayOutR: ARRAY [0..roundsR-1] OF BITSET;

CONST (*************************  COMMANDS  ******************************)

   readbuf  = 003b;
   read     = 007b;
   readstat = 013b;
   readerr  = 017b;

CONST (***********************  BOOT DRIVE  ******************************)

   drive    = 0;


PROCEDURE wait(bit: BITSET); BEGIN REPEAT UNTIL bit*inp(CSR)#{} END wait;

PROCEDURE read_sector(sector: INTEGER; VAR err: INTEGER);
  VAR trk: INTEGER;
      sec: INTEGER;
BEGIN err:=0;
  sec:=sector MOD sectors+1;
  trk:=sector DIV sectors;
  wait({5}); out(CSR,read+drive*20b+density);
  wait({7}); out(DTR,sec);
  wait({7}); out(DTR,trk);
  wait({5});
  IF 15 IN inp(CSR) THEN err:=INTEGER(inp(CSR)); RETURN END;
  out(CSR,readbuf+density+drive*20b);
  wait({7}); out(DTR,bps DIV 2);
  wait({7}); out(DTR,0);
  wait({5});
  IF 15 IN inp(CSR) THEN err:=INTEGER(inp(CSR)); RETURN END;
END read_sector;

PROCEDURE sector_io(sector: INTEGER; buf: sys.ADDRESS; VAR err: INTEGER);
  VAR try: INTEGER;
BEGIN try:=4;
  REPEAT
    read_sector(sector,err);
    IF err#0 THEN out(CSR,40000b+drive*20b); DEC(try) END;
  UNTIL (err=0) OR (try=0);
  move(buf,400000h,bps DIV 4);
END sector_io;

PROCEDURE read_block(bno: INTEGER; buf: sys.ADDRESS; VAR err: INTEGER);

VAR  sec: INTEGER;      no_sex  : INTEGER;
     trk: INTEGER;      hard_sec: INTEGER;
    size: INTEGER;      soft_sec: INTEGER;

   first: INTEGER;
   start: INTEGER;
   round: INTEGER;

BEGIN err:=0;
  tty.write_str(''15c);
  tty.write_int(bno,0);
  size:=4096;
  no_sex:=4096 DIV bps; -- 16 | 32
  first:=bno*(4096 DIV bps) MOD sectors;
  trk :=(bno*(4096 DIV bps) DIV sectors);
  sec:=0;
  WHILE (size>0) DO
    start:=sec;  round:=0;
    WHILE (round<roundsR) & (size>0) DO
      hard_sec:=first;       soft_sec:=start;
      WHILE  (hard_sec<sectors) & (size>0) & (soft_sec<no_sex) DO
        IF hard_sec IN LayOutR[round] THEN
          sector_io(trk*sectors+hard_sec,buf+soft_sec*(bps DIV 4),err);
          IF err#0 THEN
            RETURN
          END;
          DEC(size,bps);        INC(sec);
        END;
        INC(hard_sec);          INC(soft_sec);
      END;
      INC(round);
    END;
    first:=0; INC(trk);
  END;
END read_block;

BEGIN
  out(CSR,40000b+drive*20b);
  LayOutR[0]:={00,02,04,06,08,10,12,14,16,18,20,22,24};
  LayOutR[1]:={01,03,05,07,09,11,13,15,17,19,21,23,25};
  boot_device:="fd0";
END bootDSK.
