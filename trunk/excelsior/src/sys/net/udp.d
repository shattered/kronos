DEFINITION MODULE udp; (* Igo 22-Dec-91. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  nos: netKernel;
IMPORT  ip;

TYPE ADDRESS = SYSTEM.ADDRESS;
     WORD    = SYSTEM.WORD;

TYPE inputproc  = PROCEDURE (ip.DATA,WORD,INTEGER,INTEGER,INTEGER);

     doneproc   = PROCEDURE ( WORD );

     REQUEST = POINTER TO request;
     request = RECORD
                 dst : INTEGER;    (* destination address *)
                 dprt: INTEGER;    (* destination port    *)
                 sprt: INTEGER;    (* source      port    *)

                 buf : nos.buffer;
                 len : INTEGER;

                 res : INTEGER;    (* result of operation *)

                 tout: INTEGER;    (* time-out            *)
                 done: doneproc;
                 obj : WORD;

                 hdr : ARRAY [0..1] OF BITSET;
                 ip  : ip.request
               END;

PROCEDURE transmit(VAR r: request): INTEGER;
PROCEDURE install (port: INTEGER; input: inputproc; obj: WORD): INTEGER;
PROCEDURE remove  (port: INTEGER): INTEGER;

END udp.
