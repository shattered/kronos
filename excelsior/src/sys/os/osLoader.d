DEFINITION MODULE osLoader; (* Ned 28-Apr-90. (c) KRONOS *)

IMPORT  SYSTEM, defCode, osKernel;

CONST
  unic = {0};
  new  = {1};

TYPE LOOKUP = PROCEDURE(osKernel.task_ptr,
                        ARRAY OF CHAR,
                   VAR  defCode.code_ptr,
                   VAR  SYSTEM.ADDRESS,
                   VAR  BITSET         -- unic OR new
                       ): INTEGER;

PROCEDURE lookupModule(task: osKernel.task_ptr;
                       name: ARRAY OF CHAR;
                      VAR c: defCode.code_ptr;
                      VAR G: SYSTEM.ADDRESS);

PROCEDURE load(task: osKernel.task_ptr;
               name: ARRAY OF CHAR;
             lookup: LOOKUP;
           VAR  msg: ARRAY OF CHAR;
                   ): INTEGER;

PROCEDURE run(task: osKernel.task_ptr; stack: INTEGER): INTEGER;

PROCEDURE load_codes(task: osKernel.task_ptr;
                     name: ARRAY OF CHAR;
                   lookup: LOOKUP;
                 VAR  msg: ARRAY OF CHAR;
                     ): INTEGER;

----------------------------------------------------------------

TYPE
  ENTRY    = POINTER TO SYSTEM.ADDRESS;
  UNPACKED = RECORD
               cmds: STRING;
               pool: STRING;
               proc: DYNARR OF INTEGER;
               code: defCode.code_ptr;
               exts: DYNARR OF ENTRY;
             END;

PROCEDURE unpack(glo: SYSTEM.ADDRESS; VAR u: UNPACKED);

END osLoader.
