DEFINITION MODULE krSym; (* 13-Nov-90. (c) KRONOS *)

TYPE
  access_mode = (
    am_L,       -- lvl    disp
    am_aL,      -- lvl no disp
    am_G,       -- lvl    disp - if lvl#0 then external access
    am_aG,      -- lvl no disp - if lvl#0 then external access
    am_STR,     -- lvl    disp - if lvl#0 then external access
    am_imm,     --     no
    am_adr,     --        disp
    am_abs,     --     no disp
    am_xw,      --        disp
    am_xb,      --        disp
    am_xt,      --        disp
    am_stk      --
  );

  access = RECORD
    am  : access_mode;
    lvl : INTEGER; -- номер уровня или номер модуля в локальной dft
    no  : INTEGER;
    disp: INTEGER; -- смещение в битах
  END;
  access_proc = RECORD
    mod   : INTEGER;
    lvl   : INTEGER;
    disp  : INTEGER; -- position in procedure table
    loc_sz: INTEGER; -- size of locals area (words);
  END;
  access_cu = RECORD
    prc_sz: INTEGER; -- size of procedure table  (words)
    glo_sz: INTEGER; -- size of globals area     (words)
    str_sz: INTEGER; -- size of strings area     (words)
    ini_sz: INTEGER; -- size of code             (bytes)
    mg_sz : INTEGER; -- number of multiglobals
  END;
  -- additional:  <strings> <ini> <multiglobals>

  access_import = RECORD
    def_time: INTEGER;
    offset  : INTEGER;
  END;
  access_range = RECORD
    l : access;
    r : access;
  END;

END krSym.
