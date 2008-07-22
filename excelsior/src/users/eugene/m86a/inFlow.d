DEFINITION MODULE inFlow; (* Sem 28-Feb-91. (c) KRONOS *)

FROM inCmd       IMPORT condition;

TYPE
  node_mode = (nm_call,nm_case,nm_cond,nm_goto);
  node      = POINTER TO node_rec;
  node_rec  = RECORD
    md   : node_mode;
    true : node;
    false: node;
    proc : INTEGER;           -- номер процедуры или регистра для case
    alts : DYNARR OF node;    -- список алтернатив перехода по таблице
    ctbl : DYNARR OF INTEGER; -- таблица переходов
    flag : condition;
    --
    line : DYNARR OF INTEGER; -- line_no + offset*10000h
    pos  : INTEGER;           -- начало сегмента в code
    len  : INTEGER;           -- длина сегмента в code
    tpos : INTEGER;           -- начало хвоста сегмента в code
    tlen : INTEGER;           -- длина хвоста сегмента в code
    adr  : INTEGER;           -- PC начала сегмента
    cnt  : INTEGER;           -- счетчик копирований сегмента
    next : node;              -- следующий сегмент в области кода модуля
    mark : BOOLEAN;           -- определен PC начала сегмента
  END;

VAR
  block    : node;
  proc_flow: DYNARR OF node;

PROCEDURE new(): node;
PROCEDURE finish;
PROCEDURE start(n: node);
PROCEDURE gen_jp(f: node): INTEGER;
PROCEDURE mark(pos: INTEGER);

END inFlow.
