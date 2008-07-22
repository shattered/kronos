DEFINITION MODULE pedModel; (* Sem 13-Sep-86. (c) KRONOS *)

TYPE
  string= ARRAY [0..15] OF CHAR;

  chip  = POINTER TO chip_rec;
  signal= POINTER TO signal_rec;
  pin_rec=RECORD
    no  : INTEGER;
    sig : signal;
    chp : chip;
  END;
  pin   = POINTER TO pin_rec;
  pin_blk=ARRAY [0..0FFFFh] OF pin;
  pin_arr=POINTER TO pin_blk;
  seg_rec=RECORD a,b,c: INTEGER END;
  seg_arr=POINTER TO seg_blk;
  seg_blk=ARRAY [0..0FFFFh] OF seg_rec;
  signal_mds = (fixed, fantom, power);
  signal_type= SET OF signal_mds;
  signal_rec=RECORD
    pins: pin_arr;
    pno : INTEGER;
    cu  : seg_arr;
    cno : INTEGER;
    type: signal_type;
    gang: INTEGER;
    hard: INTEGER;
    name: string;
  END;
  signal_blk = ARRAY [0..0FFFFh] OF signal;
  signal_arr = POINTER TO signal_blk;

  tpin_rec=RECORD
    x,y : INTEGER;
    cu  : seg_arr;
    cno : INTEGER;
    tool: INTEGER;
  END;
  ctype_rec=RECORD
    id  : INTEGER;
    x,y : INTEGER;
    name: string;
    xnm : INTEGER;
    ynm : INTEGER;
    pno : INTEGER;
    pins: ARRAY [0..255] OF tpin_rec;
  END;
  ctype = POINTER TO ctype_rec;
  ctype_blk  = ARRAY [0..0FFFFh] OF ctype;
  ctype_arr  = POINTER TO ctype_blk;
  chip_rec=RECORD
    type: ctype;
    x,y,r: INTEGER;
    name: string;
    pno : INTEGER;
    pins: ARRAY [0..255] OF pin_rec;
  END;
  chip_blk   = ARRAY [0..0FFFFh] OF chip;
  chip_arr   = POINTER TO chip_blk;
  board_rec=RECORD
    sigs: signal_arr;
    sno : INTEGER;
    chps: chip_arr;
    cno : INTEGER;
    typs: ctype_arr;
    tno : INTEGER;
    name: string;
    x,y : INTEGER;
    lays: BITSET;
  END;
  board = POINTER TO board_rec;

PROCEDURE cre_board (VAR b: board);
PROCEDURE cre_signal(VAR s: signal; host: board);
PROCEDURE del_signal(VAR s: signal; host: board);
PROCEDURE cre_chip  (VAR c: chip; host: board; type: ctype);
PROCEDURE cre_ctype (VAR t: ctype; no: INTEGER; host: board);
PROCEDURE tie       (s: signal; c: chip; n: INTEGER; host: board);

PROCEDURE ReadModel (fnm: ARRAY OF CHAR): board;
PROCEDURE ReadType  (fnm: ARRAY OF CHAR; tnm: string; host: board): ctype;
PROCEDURE WriteModel(fnm: ARRAY OF CHAR; bd: board);

END pedModel.
