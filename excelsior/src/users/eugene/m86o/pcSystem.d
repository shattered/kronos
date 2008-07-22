DEFINITION MODULE pcSystem; (* Leo 05-Jun-88. (c) KRONOS *)
                            (* Ned 12-Oct-88. (c) KRONOS *)
                            (* Ned 28-Sep-89. (c) KRONOS *)

IMPORT  sys: SYSTEM;
IMPORT comp: coDefs;
IMPORT  ers: coErrors;

VAR
  ini  : comp.INI;
  exi  : comp.EXI;
  error: comp.ERROR;
  print: comp.PRINT;
  halt : PROC;

--------------------------  SERVICE  --------------------------
                          -----------

PROCEDURE xprint(format: ARRAY OF CHAR; SEQ args: sys.WORD);

PROCEDURE append(VAR s: ARRAY OF CHAR; f: ARRAY OF CHAR; SEQ args: sys.WORD);
PROCEDURE sprint(VAR s: ARRAY OF CHAR; f: ARRAY OF CHAR; SEQ args: sys.WORD);
PROCEDURE app_time(VAR s: ARRAY OF CHAR; time: INTEGER);

PROCEDURE err_msg(err: ers.T; VAR msg: ARRAY OF CHAR);

PROCEDURE time(): INTEGER;

PROCEDURE final(closure_proc: PROC);

------------------------  M E M O R Y  ------------------------
                        ---------------

PROCEDURE   ALLOCATE(VAR a: sys.ADDRESS; size: INTEGER);
PROCEDURE DEALLOCATE(VAR a: sys.ADDRESS; size: INTEGER);
PROCEDURE REALLOCATE(VAR a: sys.ADDRESS; VAR high: INTEGER;
                                    len,byte_size: INTEGER);

PROCEDURE release;

----------------------  STACKS & QUEUES  ----------------------
                      -------------------

TYPE QUEUE;

PROCEDURE lifo (VAR q: QUEUE); -- инициализация стека
PROCEDURE fifo (VAR q: QUEUE); -- инициализация очереди

PROCEDURE clear(VAR q: QUEUE); -- очистка

PROCEDURE push(q: QUEUE; info: sys.WORD);
PROCEDURE pop (q: QUEUE; VAR info: sys.WORD): BOOLEAN;

END pcSystem.
