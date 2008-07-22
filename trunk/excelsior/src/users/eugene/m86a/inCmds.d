DEFINITION MODULE inCmds; (* Sem 26-Feb-91. (c) KRONOS *)

IMPORT  sym : inSym;
IMPORT  pc  : pcTab;

VAR
  top : sym.access;
  proc: pc.ref;  -- md  = procedure
                 -- adr = index in vrs.prcs
                 -- r   = up ref

TYPE
  reg = (stack,da,bc);
  cmd_cop  = (c_mov,c_add,c_sub,c_or,c_xor,c_and,c_cmp);
  cmd_mode = (m_rm,m_mr,m_ri,m_mi,m_sm);
  cmd_desc = RECORD
    cop  : cmd_cop;
    mode : cmd_mode;
    r0,r1: INTEGER;
    size : INTEGER;
    val  : INTEGER;
  END;

VAR
  stk: INTEGER;
  pos: ARRAY [0..99] OF reg;
  siz: ARRAY [0..99] OF INTEGER;

PROCEDURE pop_reg(n: INTEGER);
PROCEDURE alloc_reg(sz: INTEGER);

PROCEDURE gen_access(a: sym.access; sz: INTEGER; VAR am,sr,ofs: INTEGER);
PROCEDURE put_access(am,sr,ofs,cop_1: INTEGER; SEQ cop_0: INTEGER);
PROCEDURE gen_cmd   (d: cmd_desc; a: sym.access);

PROCEDURE cmd_stk_m(c: cmd_cop; a: sym.access);
PROCEDURE cmd_m_stk(c: cmd_cop; a: sym.access);
PROCEDURE cmd_m_imm(c: cmd_cop; a: sym.access; val,sz: INTEGER);

PROCEDURE parm_ofs(VAR ofs: INTEGER; lvl: INTEGER);

PROCEDURE swap;
PROCEDURE drop;
PROCEDURE copt;
PROCEDURE save;
PROCEDURE hold_out;
PROCEDURE set_flag_z;  -- and drop
PROCEDURE func_ret(sz: INTEGER);
PROCEDURE set_size(sz: INTEGER; sign: BOOLEAN);

PROCEDURE load      (a: sym.access; sz: INTEGER);
PROCEDURE store     (to: sym.access);
PROCEDURE load_store(to,fr: sym.access; sz: INTEGER);
PROCEDURE load_adr  (a: sym.access);
PROCEDURE add_adr   (a: sym.access);
PROCEDURE sub_adr   (a: sym.access);
PROCEDURE stot      (a: sym.access; sz: INTEGER);

PROCEDURE imul   (pos: INTEGER; a: sym.access);
PROCEDURE idiv   (pos: INTEGER; a: sym.access);

PROCEDURE mov_c  (sz: INTEGER);
PROCEDURE move;

PROCEDURE cmps;

END inCmds.
