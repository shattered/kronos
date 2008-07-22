DEFINITION MODULE inFlow; (* Sem 28-Feb-91. (c) KRONOS *)

FROM inCmd       IMPORT condition;

TYPE
  node_mode = (nm_call,nm_ext_call,nm_case,nm_cond,nm_goto);
  ext_name  = POINTER TO ename_rec;
  ename_rec = RECORD
    dw,up: ext_name;
    nm   : DYNARR OF CHAR;
    index: INTEGER;
  END;
  node      = POINTER TO node_rec;
  node_rec  = RECORD
    goto : node;
    CASE md: node_mode OF
      |nm_call    : proc : INTEGER;        -- номер процедуры в inVars.prcs
      |nm_case    : reg  : INTEGER;        -- номер регистра для case
                    ctbl : DYNARR OF node; -- таблица переходов
      |nm_ext_call: name : ext_name;       -- имя внешней процедуры
      |nm_cond    : flag : condition;
                    else : node;
    END;
    -----------------------------------------------------------------------
    line : DYNARR OF INTEGER; -- line_no + offset*10000h
    pos  : INTEGER;           -- начало сегмента в inCmd.code
    len  : INTEGER;           -- длина сегмента в inCmd.code
    tpos : INTEGER;           -- начало хвоста сегмента в inCmd.code
    tlen : INTEGER;           -- длина хвоста сегмента в inCmd.code
    adr  : INTEGER;           -- PC начала сегмента
    cnt  : INTEGER;           -- счетчик копирований сегмента
    next : node;              -- следующий сегмент в области кода модуля
  END;

VAR
  block    : node;
  proc_flow: DYNARR OF node;
  enm_tree : ext_name;

PROCEDURE new(): node;
PROCEDURE finish;
PROCEDURE start(n: node);
PROCEDURE gen_jp(f: node): INTEGER;

-- пометить текущую позицию в коде ссылкой в текст
PROCEDURE mark(pos: INTEGER);

-- вызов внешней процедуры
-- pos - ссылка в текст, nm - полное имя процедуры
PROCEDURE external_call(pos: INTEGER; nm: ARRAY OF CHAR);

END inFlow.
