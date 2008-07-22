DEFINITION MODULE inSym; (* 13-Nov-90. (c) KRONOS *)

TYPE
  adr_mode = (
    am_STK,
    am_FPP,
    am_aSTK,      -- disp
    am_imm,       -- n
    am_abs,       -- 0 [disp]
    am_G,         -- DS[disp]
    am_Gstr,      -- DS[disp], scode[disp]
    am_L,         -- SS[BP+disp]
    am_Gimm,      -- n, DS[disp]
    am_Limm,      -- n, SS[BP+disp]
    am_aG,
    am_aL,
    am_PB2,
    am_PB4,
    am_PB8,
    am_aPB
  );

  access = RECORD
    am   : adr_mode;
    n    : INTEGER; -- константа или промежуточное смещение
    disp : INTEGER; -- смещение
    level: INTEGER; -- уровень описания (L, Limm, aL)
                    -- или номер модуля (G, Gimm, aG)
  END;

  access_proc = RECORD
    mod   : INTEGER; -- module no (host of procedure code)
    ofs   : INTEGER; -- pointer to procedure position in DS of curr. module
    lvl   : INTEGER; -- procedure level
    no    : INTEGER; -- position in procedure table
    loc_sz: INTEGER; -- size of locals area (bytes)
    export: BOOLEAN; -- TRUE если поцедура экспортируется
    slink : BOOLEAN; -- TRUE если процедура использует статическую цепочку
  END;
  -- if ofs<0 then pointer jast is not allocated

  access_cu = RECORD
    prc_sz: INTEGER; -- number of procedures
    glo_sz: INTEGER; -- size of globals area     (bytes)
    ini_sz: INTEGER; -- size of code             (bytes)
    mg_sz : INTEGER; -- number of multiglobals
  END;
  -- additional:  <globals> <code> <multiglobals>

  access_import = RECORD
    def_time: INTEGER;
    ofs     : INTEGER; -- module base position DS
    no      : INTEGER;
  END;
  -- if ofs<0 then jast is not allocated
  -- импорт представлен в DS импортирующего модуля словом,
  -- содержащим индикант DS импортируемого модуля

  access_range = RECORD
    l   : access;
    r   : access;
    sign: BOOLEAN;
    size: INTEGER;
  END;

END inSym.
