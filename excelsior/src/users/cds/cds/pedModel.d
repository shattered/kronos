DEFINITION MODULE pedModel; (* Sem 13-Sep-86. (c) KRONOS *)

IMPORT  mem : libHeap;

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
  seg_rec=RECORD a,b,c: INTEGER END;
  signal_mds = (fixed, fantom, power);
  signal_type= SET OF signal_mds;
  signal_rec=RECORD
    pins: DYNARR OF pin;
    cu  : DYNARR OF seg_rec;
    type: signal_type;
    gang: INTEGER;
    hard: INTEGER;
    name: string;
  END;

  tpin_rec=RECORD
    x,y : INTEGER;
    cu  : DYNARR OF seg_rec;
    tool: INTEGER;
  END;
  ctype_rec=RECORD
    id  : INTEGER;
    x,y : INTEGER;
    name: string;
    xnm : INTEGER;
    ynm : INTEGER;
    pins: DYNARR OF tpin_rec;
  END;
  ctype = POINTER TO ctype_rec;
  chip_rec=RECORD
    type: ctype;
    x,y,r: INTEGER;
    name: string;
    pins: DYNARR OF pin_rec;
  END;
  board_rec=RECORD
    sigs: DYNARR OF signal;
    chps: DYNARR OF chip;
    typs: DYNARR OF ctype;
    name: string;
    x,y : INTEGER;
    lays: BITSET;
    mem : mem.AREA;
  END;
  board = POINTER TO board_rec;

PROCEDURE cre_board (VAR b: board);
PROCEDURE del_board (VAR b: board);

PROCEDURE cre_signal(VAR s: signal; host: board);
PROCEDURE del_signal(VAR s: signal; host: board);

PROCEDURE cre_chip  (VAR c: chip; host: board; type: ctype);
PROCEDURE del_chip  (VAR c: chip; host: board);

PROCEDURE cre_ctype (VAR t: ctype; no: INTEGER; host: board);
PROCEDURE tie       (s: signal; c: chip; n: INTEGER; host: board);

PROCEDURE read_model (fnm: ARRAY OF CHAR): board;
PROCEDURE read_type  (fnm: ARRAY OF CHAR; tnm: string; host: board): ctype;
PROCEDURE write_model(fnm: ARRAY OF CHAR; bd: board);

END pedModel.
