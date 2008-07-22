DEFINITION MODULE nsSym; (* 13-Nov-90. (c) KRONOS *)

TYPE
  adr_mode = (
    am_RG,
    am_FPP4,
    am_FPP8,
    am_aRG,
    am_aaFP,
    am_aaSP,
    am_aaSB,
    am_imm,
    am_abs,
    am_EXT,
    am_TOS,
    am_aFP,
    am_aFPimm,
    am_aSP,
    am_aSB,
    am_aSBimm,
    am_aPC
  );

  index_mode = (xm_off,xm_b,xm_w,xm_d,xm_q);

  access = RECORD
    am   : adr_mode;
    xm   : index_mode;
    rg   : INTEGER;
    rg_x : INTEGER;
    n    : INTEGER; -- константа или промежуточное смещение
    disp : INTEGER; -- НЕ используется для am_RG,am_imm,am_TOS
    level: INTEGER; -- для aFP и aaFP уровень описания процедуры
  END;

  access_proc = RECORD
    mod   : INTEGER;
    lvl   : INTEGER;
    disp  : INTEGER; -- position in procedure table
    loc_sz: INTEGER; -- size of locals area (bytes);
  END;

  access_cu = RECORD
    prc_sz: INTEGER; -- size of procedure table  (words)
    glo_sz: INTEGER; -- size of globals area     (bytes)
    str_sz: INTEGER; -- size of strings area     (bytes)
    ini_sz: INTEGER; -- size of code             (bytes)
    mg_sz : INTEGER; -- number of multiglobals
  END;
  -- additional:  <strings> <ini> <multiglobals>

  access_import = RECORD
    def_time: INTEGER;
    offset  : INTEGER;
  END;

  access_range = RECORD
    l   : access;
    r   : access;
    sign: BOOLEAN;
    size: INTEGER;
  END;

END nsSym.
