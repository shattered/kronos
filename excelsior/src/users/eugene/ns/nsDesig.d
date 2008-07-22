DEFINITION MODULE nsDesig; (* Sem 06-Oct-90. (c) KRONOS *)

IMPORT sym : nsSym;

FROM pcTab      IMPORT  ref;
FROM nsSym      IMPORT  access;

CONST
  regs_no = 8;

TYPE
  regs = RECORD
    stk : INTEGER; -- свободны регистры от stk до lim-1
    lim : INTEGER; -- можно использовать регистры от stk до res-1
    res : INTEGER; -- реально использованы регистры от stk до max-1
    max : INTEGER;
  END;
  trap = RECORD
    cnt  : INTEGER;
    scnt : INTEGER;
    vcnt : INTEGER;
    pcnt : INTEGER;
    mcnt : INTEGER;
    rcnt : INTEGER;
    mgcnt: INTEGER;
    freg : regs;
  END;

VAR
  cu        : sym.access_cu;

  mg        : DYNARR OF RECORD
    offset  : INTEGER;
    size    : INTEGER;
  END;
  mg_no     : INTEGER;

  scode     : DYNARR OF INTEGER;
  scnt      : INTEGER;

  vars      : DYNARR OF access;             -- var, const
  prcs      : DYNARR OF sym.access_proc;    -- procedure
  mdls      : DYNARR OF sym.access_import;  -- module
  mdls_nm   : DYNARR OF INTEGER;            -- module name
  rngs      : DYNARR OF sym.access_range;   -- range
  inln      : DYNARR OF DYNARR OF CHAR;     -- inline
  vars_no   : INTEGER;
  prcs_no   : INTEGER;
  mdls_no   : INTEGER;
  rngs_no   : INTEGER;
  inln_no   : INTEGER;

  expression: PROCEDURE (ref,VAR access);
  assign    : PROCEDURE (ref,access,access);

  level     : INTEGER;
  proc      : ref;
  free_regs : regs;

PROCEDURE new_var (): INTEGER;
PROCEDURE new_proc(): INTEGER;
PROCEDURE new_mod (): INTEGER;
PROCEDURE new_str (sz: INTEGER): INTEGER;
PROCEDURE new_mg  (n: INTEGER): INTEGER;
PROCEDURE new_rng (): INTEGER;
PROCEDURE new_inln(): INTEGER;

PROCEDURE dynarr?  (l: ref): BOOLEAN;
PROCEDURE string?  (l: ref): BOOLEAN;
PROCEDURE pointer? (l: ref): BOOLEAN;
PROCEDURE array_of?(l: ref): BOOLEAN;
PROCEDURE boolean? (l: ref): BOOLEAN;
PROCEDURE set?     (l: ref): BOOLEAN;
PROCEDURE sign?    (l: ref): BOOLEAN;

PROCEDURE const?  (a: access): BOOLEAN;
PROCEDURE imm_int?(VAR a: access; sig: BOOLEAN; sz: INTEGER): BOOLEAN;

PROCEDURE integer(VAR a: access; sig: BOOLEAN; sz: INTEGER);
PROCEDURE sub_base(VAR a: access; sig: BOOLEAN; sz: INTEGER);

PROCEDURE gb(VAR a: access);

PROCEDURE add_offset(VAR a: access; offset: INTEGER);

PROCEDURE alloc_reg  (VAR a: access);
PROCEDURE dealloc_reg(a: access; limit: INTEGER);
PROCEDURE alloc_var  (VAR a: access; size: INTEGER);

PROCEDURE save   (VAR t: trap);
PROCEDURE restore(t: trap);

PROCEDURE min    (l: ref; VAR a: access);
PROCEDURE max    (l: ref; VAR a: access);
PROCEDURE low    (l: ref; VAR a: access);
PROCEDURE high   (l: ref; VAR a: access);
PROCEDURE len    (l: ref; VAR a: access);
PROCEDURE bytes  (l: ref; VAR a: access);
PROCEDURE bytes_w(l: ref; VAR a: access);
PROCEDURE bytes_d(l: ref; VAR a: access);
PROCEDURE bytes_u(l: ref; VAR a: access);
PROCEDURE adr    (l: ref; VAR a: access);
PROCEDURE adr_u  (l: ref; VAR a: access);
PROCEDURE index  (l: ref; VAR a: access);
PROCEDURE field  (l: ref; VAR a: access);

PROCEDURE designator  (l: ref; VAR a: access);
PROCEDURE designator_u(l: ref; VAR a: access);

END nsDesig.
