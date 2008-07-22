DEFINITION MODULE pcGenExpr; (* Sem 06-Oct-90. (c) KRONOS *)

IMPORT tbl : pcTab;
IMPORT gen : krSym;

FROM SYSTEM      IMPORT WORD;
FROM pcTab       IMPORT ref;

TYPE
  node_mode = (nm_goto,nm_case,nm_cond);
  node=POINTER TO node_rec;
  node_rec=RECORD
    true : node;
    false: node;
    md   : node_mode;
    alts : DYNARR OF node; -- таблица алтернатив перехода по таблице
    spos : INTEGER; -- позиция таблицы переходов в scode
    slen : INTEGER;
    pos  : INTEGER; -- начало сегмента в code
    len  : INTEGER; -- длина сегмента в code
    cmp  : INTEGER; -- pos of cmp command
    res  : INTEGER; -- длина хвоста сегмента в .buf
    adr  : INTEGER; -- PC начала сегмента
    cnt  : INTEGER; -- счетчик копирований сегмента
    next : node;    -- следующий сегмент в области кода модуля
    mark : BOOLEAN; -- определен PC начала сегмента
    buf  : ARRAY [0..15] OF CHAR;
  END;
  box_mode = (bm_imm,bm_stk,bm_size,bm_adr);
  box = RECORD
    bm   : box_mode;
    imm  : INTEGER;
  END;
  val_mode    = (vm_integer,vm_real,vm_string,vm_boolean,vm_set,vm_undef);
  trap = RECORD
    cnt  : INTEGER;  -- code
    scnt : INTEGER;  -- str
    ecnt : INTEGER;  -- enter
    vcnt : INTEGER;  -- vars
    pcnt : INTEGER;  -- procs
    mcnt : INTEGER;  -- modules
    rcnt : INTEGER;  -- ranges
    mgcnt: INTEGER;  -- multiglob
    blk  : node;
    stk  : INTEGER;
    mstk : INTEGER;
  END;
  try_load_proc=PROCEDURE (ref,VAR box);

VAR
  LAST     : BOOLEAN;
  procs    : ref;
  proc     : ref;
  last_proc: ref;
  stat_pos : INTEGER;
  level    : INTEGER;

PROCEDURE error(l: ref; s: ARRAY OF CHAR; SEQ x: WORD);

PROCEDURE new(): node;

PROCEDURE save(VAR r: trap);
PROCEDURE restore(r: trap);

PROCEDURE type(l: ref): ref;

PROCEDURE calc_load(l: ref; VAR b: box; p: try_load_proc);
PROCEDURE gen_load (l: ref; VAR b: box; p: try_load_proc);
PROCEDURE try_load (l: ref; VAR b: box; p: try_load_proc);
PROCEDURE load_val (l: ref; VAR b: box);
PROCEDURE load_adr (l: ref; VAR b: box);
PROCEDURE load_min (l: ref; VAR b: box);
PROCEDURE load_max (l: ref; VAR b: box);
PROCEDURE load_size(l: ref; VAR b: box);
PROCEDURE load_bits(l: ref; VAR b: box);

PROCEDURE gen_var(l: ref);

PROCEDURE do_flow(): node;

END pcGenExpr.
