DEFINITION MODULE nsExpr; (* Sem 01-Dec-90. (c) KRONOS *)

IMPORT des : nsDesig;

FROM pcTab      IMPORT  ref;
FROM nsSym      IMPORT  access;
FROM nsCmd      IMPORT  condition;

TYPE
  val_mode = (
    vm_integer,
    vm_cardinal,
    vm_boolean,
    vm_real,
    vm_set,
    vm_string,
    vm_undef
  );
  node_mode = (nm_call,nm_case,nm_cond,nm_goto);
  node      = POINTER TO node_rec;
  node_rec  = RECORD
    true : node;
    false: node;
    md   : node_mode;
    flag : condition;
    proc : INTEGER; -- номер процедуры или регистра для case
    alts : DYNARR OF node;    -- список алтернатив перехода по таблице
    ctbl : DYNARR OF INTEGER; -- таблица переходов
    pos  : INTEGER; -- начало сегмента в code
    len  : INTEGER; -- длина сегмента в code
    tpos : INTEGER; -- начало хвоста сегмента в code
    tlen : INTEGER; -- длина хвоста сегмента в code
    adr  : INTEGER; -- PC начала сегмента
    cnt  : INTEGER; -- счетчик копирований сегмента
    next : node;    -- следующий сегмент в области кода модуля
    mark : BOOLEAN; -- определен PC начала сегмента
  END;

CONST
  fpp_regs_no = 8;

VAR
  fpp_regs      : des.regs;
  block         : node;
  gen_call      : PROCEDURE (ref,BOOLEAN,VAR access);

PROCEDURE alloc_fpp_reg(VAR a: access; sz: INTEGER);
PROCEDURE dealloc_fpp_reg(a: access; min: INTEGER);

PROCEDURE new(): node;
PROCEDURE finish;
PROCEDURE start(n: node);

PROCEDURE vm(l: ref): val_mode;

PROCEDURE bytes(l: ref): INTEGER;

PROCEDURE gen_plus      (l: ref; VAR a: access);
PROCEDURE gen_minus2    (l: ref; VAR a: access);
PROCEDURE expression    (l: ref; VAR a: access);
PROCEDURE gen_condition (cond: ref; then,else: node);

END nsExpr.
