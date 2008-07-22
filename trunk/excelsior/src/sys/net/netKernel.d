DEFINITION MODULE netKernel[1]; (* Igo&John 01-Oct-91. (c) KRONOS *)
                                (* Igo      10-Dec-91. (c) KRONOS *)

IMPORT  SYSTEM;

TYPE ADDRESS = SYSTEM.ADDRESS;
     WORD    = SYSTEM.WORD;

--------------------------- BYTE MOVE --------------------------
                           -----------

PROCEDURE move(t: ADDRESS; to: INTEGER; f: ADDRESS; fo,len: INTEGER);

---------------------------- BUFFERS ---------------------------
                            ---------

TYPE BUFFER = POINTER TO buffer;
     buffer = RECORD
                buf : ADDRESS;
                pos : INTEGER;
                len : INTEGER;
                next: BUFFER;
              END;

PROCEDURE bmove(t: ADDRESS; to: INTEGER; VAR f: buffer; len: INTEGER);
PROCEDURE bfind(VAR b: buffer; pos: INTEGER);

---------------------------- MEMORY ----------------------------
                            --------

PROCEDURE   allocate(VAR a: ADDRESS; sz: INTEGER);
PROCEDURE deallocate(VAR a: ADDRESS; sz: INTEGER);
PROCEDURE reallocate(VAR a: ADDRESS; VAR high: INTEGER;
                                 len,byte_size: INTEGER);

---------------------------- TIMEOUT ---------------------------
                            ---------
CONST toutprio = 10;

TYPE TIME     = POINTER TO time;
     timeproc = PROCEDURE (TIME);
     time     = RECORD
                  f,b : TIME;
                  tout: INTEGER;
                  done: timeproc;
                  obj : SYSTEM.WORD
                END;

PROCEDURE settout(VAR t: time; tout: INTEGER);
PROCEDURE deltout(VAR t: time);


---------------------------- PROCESS ---------------------------
                            ---------
TYPE PROCESS = POINTER TO process;
     process = RECORD
                 self: SYSTEM.ADDRESS;
                 prio: INTEGER;
                 pri0: INTEGER;
                 work: BOOLEAN;
                 next: PROCESS
               END;

PROCEDURE new(VAR prs: process; p: PROC; size,pri0: INTEGER): BOOLEAN;

PROCEDURE active(): PROCESS;
(* return active process *)

PROCEDURE self  (): SYSTEM.ADDRESS;

PROCEDURE send(VAR p: process);
(* activate realtime process from realtime process *)

PROCEDURE wait;
(* suspend me *)

PROCEDURE switch(VAR p: process; ipted: SYSTEM.ADDRESS);
(* switch to realtime process from interrupt handler *)

PROCEDURE call  (VAR p: process);
(* call realtime system from scheduler process       *)

PROCEDURE   lock(prio: INTEGER);
PROCEDURE unlock;

---------------------------- QUEUES ----------------------------
                            --------
TYPE QUEUE = POINTER TO queue;
     queue = RECORD
               f,b: QUEUE
             END;

PROCEDURE tie   (VAR head: SYSTEM.ADDRESS; what: SYSTEM.ADDRESS);
PROCEDURE untie (VAR head: SYSTEM.ADDRESS);
PROCEDURE untien(VAR head: SYSTEM.ADDRESS; what: SYSTEM.ADDRESS);

END netKernel.
