DEFINITION MODULE mxCmd; (* Ned 20-Jun-88. (c) KRONOS *)

IMPORT SYSTEM;

--------------------------  CONTEXT  --------------------------
                          -----------

VAR
  CPU     : INTEGER;  -- версия системы команд
                      -- 0 for 2.2
                      -- 1 for 2.5 (rch,rckz,jump,chknil,pdx)
                      -- 2 for 2.6WS (QOUT, long INCL,EXCL)

VAL
  depth   : INTEGER;  -- глубина стека выражений
  curlevel: INTEGER;  -- текущий уровень вложенности
  cur_segm: INTEGER;  -- начало текущего сегмента
  cur_pos : INTEGER;  -- текущая позиция в коде
  pool_ofs: INTEGER;  -- текущее смещение в строковом пуле

------------------------  CODE BUFFER  ------------------------
                        ---------------

PROCEDURE out_code_pos(): INTEGER;
(* Позиция в выходном буфере. *)

PROCEDURE b(n: INTEGER);
PROCEDURE c(n: INTEGER);

PROCEDURE undo;  -- откат последней команды

--------------------------  E-STACK  --------------------------
                          -----------

CONST eStackDepth = 7;  -- максимальная глубина стека выражений

PROCEDURE ePush;
PROCEDURE ePop;

PROCEDURE set_depth(n: INTEGER);

PROCEDURE pop_const;

--------------------------  COMMANDS  -------------------------
                          ------------

PROCEDURE li(n: INTEGER);

PROCEDURE lsw(n: INTEGER);
PROCEDURE ssw(n: INTEGER);

PROCEDURE cADD(n: INTEGER);
PROCEDURE cMUL(n: INTEGER);

PROCEDURE cDIV(n: INTEGER; quo: BOOLEAN);
-- quo - округление к нулю
PROCEDURE cMOD(n: INTEGER; rem: BOOLEAN);
-- rem - округление к нулю

PROCEDURE logical_not(not_not: BOOLEAN);
(* Если not_not то убирает последовательность NOT NOT *)

PROCEDURE move;
PROCEDURE lsta(n: INTEGER);
PROCEDURE getbase(level: INTEGER);
PROCEDURE lxa(size: INTEGER);

PROCEDURE check_nil;

PROCEDURE raise(n: INTEGER);
(* генерация TRAP(n) *)

PROCEDURE copt;
PROCEDURE stot;         PROCEDURE lodt;
PROCEDURE store;        PROCEDURE stofv;        PROCEDURE lodfv;

PROCEDURE alloc(n: INTEGER);

-----------------------  EXPRESSIONs  ------------------------
                       ---------------

PROCEDURE bin_op(sy,mode: INTEGER);
(* mode:
         0 - integer operation
         1 - set     operation
         2 - real    operation
*)

PROCEDURE comp0(sy: INTEGER);  -- =0, #0

PROCEDURE string_cmp(sy: INTEGER);

------------------------  VARs & PROCs  -----------------------
                        ----------------

PROCEDURE loadword (scope,addr: INTEGER);
PROCEDURE storeword(scope,addr: INTEGER);
PROCEDURE loadadr  (scope,addr: INTEGER);
PROCEDURE loadconst(scope,addr: INTEGER);       -- for struct const

PROCEDURE call(scope,addr: INTEGER);

PROCEDURE copy_farr(scope,addr,hi_addr: INTEGER; byte: BOOLEAN; size: INTEGER);
PROCEDURE copy_struct(scope,addr: INTEGER; size: INTEGER);

----------------------  CODE PROCEDUREs  ----------------------
                      -------------------

PROCEDURE enter_code_proc(VAR pp: INTEGER);
PROCEDURE app_code_expr(VAR pp: INTEGER; byte: INTEGER);

PROCEDURE insert_code_proc(pp: INTEGER);

----------------------------  POOL  ---------------------------
                            --------

PROCEDURE app_word(w: INTEGER);
PROCEDURE app_str (s: ARRAY OF CHAR; len: INTEGER): INTEGER;

PROCEDURE get_word(ofs,inx: INTEGER): INTEGER;
PROCEDURE get_byte(ofs,inx: INTEGER): INTEGER;

TYPE
  PUT_BYTE = PROCEDURE (INTEGER);
  GET_BYTE = PROCEDURE (): INTEGER;

PROCEDURE iter_bytes(ofs,len: INTEGER; one: PUT_BYTE);
PROCEDURE  put_bytes(ofs,len: INTEGER; one: GET_BYTE);

--------------------------  CONTROLs  -------------------------
                          ------------

TYPE
  POINT;
  POSITION;

PROCEDURE mcode;
PROCEDURE save_mcode;
PROCEDURE mark_pos;

PROCEDURE set_point(VAR p: POINT);
PROCEDURE insert(p: POINT);

PROCEDURE save_pos(VAR pos: POSITION);
PROCEDURE resume  (    pos: POSITION);
PROCEDURE release (    pos: POSITION);

PROCEDURE enterloop;
PROCEDURE exitloop;
PROCEDURE endloop;

PROCEDURE enterwhile;
PROCEDURE do(b: INTEGER);
PROCEDURE endwhile;

PROCEDURE repeat;
PROCEDURE until(b: INTEGER);

PROCEDURE enterfor(vaddr,baddr: INTEGER);

PROCEDURE for_from(cons: BOOLEAN; val: INTEGER);
PROCEDURE for_to  (cons: BOOLEAN; val: INTEGER);
-- bound offset

PROCEDURE for_step(val: INTEGER);
PROCEDURE for_do;
PROCEDURE endfor(VAR baddr: INTEGER);

PROCEDURE if(b: INTEGER);
PROCEDURE else;
PROCEDURE endif;

PROCEDURE entercase(cons: BOOLEAN; val: INTEGER; taddr: INTEGER);
PROCEDURE caselabel(val: INTEGER);
PROCEDURE caserange(v1,v2: INTEGER);
PROCEDURE exitvariant;
PROCEDURE elsecase;
PROCEDURE endcase;

PROCEDURE return;

PROCEDURE mark(n: INTEGER);
PROCEDURE goto(n: INTEGER);

PROCEDURE entercond(b: INTEGER; and: BOOLEAN);
PROCEDURE exitcond (b: INTEGER; VAR val: INTEGER);

PROCEDURE assert(b: INTEGER; one: BOOLEAN);

TYPE PUT_XREF   = PROCEDURE (INTEGER,INTEGER,INTEGER,INTEGER);

PROCEDURE enter_proc;
PROCEDURE exit_proc(p_no,start_proc: INTEGER; put_xref: PUT_XREF);

PROCEDURE correct_case_tables(no_proc: INTEGER);

-------------------  OVERFLOWs & UNDERFLOWs  ------------------
                   --------------------------

PROCEDURE mask_overflow;

PROCEDURE check_overflow(): BOOLEAN;

-------------------------  INI & EXI  -------------------------
                         -------------

TYPE WRITE = PROCEDURE (ARRAY OF SYSTEM.WORD, INTEGER);

PROCEDURE write_code(write: WRITE);
PROCEDURE write_pool(write: WRITE);

PROCEDURE Ini;
PROCEDURE Exi;

END mxCmd.
