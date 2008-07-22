DEFINITION MODULE pcGenDesig; (*$N+ Sem 06-Oct-90. (c) KRONOS *)

FROM pcTab      IMPORT  ref, mode;
FROM SYSTEM     IMPORT  WORD;

IMPORT gen : krSym;

VAR
  cu    : gen.access_cu;

  mg    : DYNARR OF RECORD
    offset: INTEGER;
    size  : INTEGER;
  END;
  mg_no : INTEGER;

  scode : DYNARR OF INTEGER;
  scnt  : INTEGER;

  vars   : DYNARR OF gen.access;         -- var, const
  prcs   : DYNARR OF gen.access_proc;    -- procedure
  mdls   : DYNARR OF gen.access_import;  -- module
  mdls_nm: DYNARR OF INTEGER;            -- module name
  rngs   : DYNARR OF gen.access_range;   -- range
  inln   : DYNARR OF DYNARR OF CHAR;     -- inline
  vars_no: INTEGER;
  prcs_no: INTEGER;
  mdls_no: INTEGER;
  rngs_no: INTEGER;
  inln_no: INTEGER;

PROCEDURE new_ref (m: mode): ref;
PROCEDURE new_var (): INTEGER;
PROCEDURE new_proc(): INTEGER;
PROCEDURE new_mod (): INTEGER;
PROCEDURE new_str (sz: INTEGER): INTEGER;
PROCEDURE new_mg  (n: INTEGER): INTEGER;
PROCEDURE new_rng (): INTEGER;
PROCEDURE new_inln(): INTEGER;

PROCEDURE convert_toAGL(VAR a: gen.access);
PROCEDURE convert_to_address(a: gen.access);

PROCEDURE load_access (l: ref; VAR a: gen.access);

END pcGenDesig.
