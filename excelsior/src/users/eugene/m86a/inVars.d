DEFINITION MODULE inVars; (* Sem 06-Oct-90. (c) KRONOS *)

IMPORT sym : inSym;
IMPORT pc  : pcTab;

TYPE
  val_mode = (
    vm_integer,vm_cardinal,vm_boolean,vm_set,
    vm_real,vm_string,vm_undef,vm_address
  );
  val_mode_set = SET OF val_mode;

CONST
  scalars = val_mode_set {
    vm_integer,vm_cardinal,vm_boolean,vm_set,vm_real,vm_address
  }; -- имеют размер 1,2,4 байтов

VAR
  cu        : sym.access_cu;
  mg        : DYNARR OF RECORD
      offset   : INTEGER;
      size     : INTEGER;
      name     : INTEGER;
  END;
  mg_no     : INTEGER;
  scode     : DYNARR OF CHAR;
  scnt      : INTEGER;
  vars      : DYNARR OF sym.access;         -- var, const
  prcs      : DYNARR OF sym.access_proc;    -- procedure
  prcs_nm   : DYNARR OF INTEGER;            -- procedure name
  mdls      : DYNARR OF sym.access_import;  -- module
  mdls_nm   : DYNARR OF INTEGER;            -- module name
  rngs      : DYNARR OF sym.access_range;   -- range
  vars_no   : INTEGER;
  prcs_no   : INTEGER;
  mdls_no   : INTEGER;
  rngs_no   : INTEGER;

PROCEDURE new_var (): INTEGER;
PROCEDURE new_proc(): INTEGER;
PROCEDURE new_mod (): INTEGER;
PROCEDURE new_str (sz: INTEGER): INTEGER;
PROCEDURE new_mg  (n: INTEGER): INTEGER;
PROCEDURE new_rng (): INTEGER;

PROCEDURE module_ref_offset(mod: INTEGER): INTEGER;
-- выдает позицию ссылки на модуль mod в DS компилируемого модуля

PROCEDURE dynarr?  (l: pc.ref): BOOLEAN;
PROCEDURE string?  (l: pc.ref): BOOLEAN;
PROCEDURE pointer? (l: pc.ref): BOOLEAN;
PROCEDURE array_of?(l: pc.ref): BOOLEAN;
PROCEDURE boolean? (l: pc.ref): BOOLEAN;
PROCEDURE set?     (l: pc.ref): BOOLEAN;
PROCEDURE sign?    (l: pc.ref): BOOLEAN;

PROCEDURE vm(l: pc.ref): val_mode;

END inVars.
