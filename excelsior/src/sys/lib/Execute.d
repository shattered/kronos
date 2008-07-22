DEFINITION MODULE Execute; (* Hady. 13-May-91. (c) KRONOS *)

IMPORT  BIO;

VAL (* global state *)
   done: BOOLEAN;  error: INTEGER;

   note: ARRAY  [0..127]  OF CHAR;
  chain: STRING;

   task: INTEGER;         -- last runned task number
  cause: BITSET ;  CONST  -- cause of last task abort
      awake = {0};        -- became independent
      break = {1};        -- was broken by keyboard or HALT(number)
      abort = {2};        -- was aborted by any trap
        cfi = {3};        -- was broken by keyboard with command file
   continue ={31};        -- "chain" is not empty !

PROCEDURE change_env(task: INTEGER; name,val: ARRAY OF CHAR; priv: BOOLEAN);

PROCEDURE task_controll(op: INTEGER; tasks: ARRAY OF INTEGER);
               CONST _stop=0; _kill=1; _wait=2;

TYPE ENV;  VAL null: ENV;

PROCEDURE run_task(name: ARRAY OF CHAR; ch_env: ENV;
                  alias: ARRAY OF CHAR;
                   args: ARRAY OF CHAR; wait: BOOLEAN);

PROCEDURE new    (VAR env: ENV);
PROCEDURE dispose(VAR env: ENV);
PROCEDURE append (VAR env: ENV; name,val: ARRAY OF CHAR; priv: BOOLEAN);
PROCEDURE fappend(VAR env: ENV;     str: ARRAY OF CHAR;
                npos,nlen: INTEGER; vpos,vlen: INTEGER;
                  private: BOOLEAN);

VAL etc: BIO.PATHs; mask: BITSET;

PROCEDURE hold_break(on: BOOLEAN); PROCEDURE break_mask(mask: BITSET);

END Execute.
