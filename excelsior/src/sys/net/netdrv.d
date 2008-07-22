DEFINITION MODULE netdrv; (* Igo 10-Dec-91. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  nos: netKernel;

TYPE ADDRESS = SYSTEM.ADDRESS;
     WORD    = SYSTEM.WORD;

---------------------------- ERRORS ----------------------------
                            --------

CONST closing = 100h;
      reset   = 101h;
      dead    = 102h;

--------------------------- PROTOCOLS --------------------------
                           -----------

CONST IP      = 0F0h;
      ARP     = 0F1h;

---------------------------- DRIVER ----------------------------
                            --------

TYPE BUFFER  = POINTER TO buffer;
     buffer  = RECORD
                 f,b: BUFFER;
               cf,cb: BUFFER; (* pointer to next cluster *)
                 pro: INTEGER;

                 ofs: INTEGER;
                 pos: INTEGER;
                 len: INTEGER;
                 buf: ADDRESS;
                 sz : INTEGER
               END;

     NODE    = POINTER TO node;
     node    = RECORD
                 f,b  : NODE;
                 magic: INTEGER;
                 dest : INTEGER;
                 prot : INTEGER;
                 stat : INTEGER;
                 try  : INTEGER;
                 tout : INTEGER;
                 time : nos.time;
                 obj  : WORD;
               END;

  startproc  = PROC;

                  ---                                adr       pos       len
  allocproc  = PROCEDURE ( WORD , VAR BUFFER , startproc , ADDRESS , INTEGER , INTEGER );
  inputproc  = PROCEDURE ( WORD , VAR BUFFER);

                  ---     channel    da        do
  moveproc   = PROCEDURE ( WORD , ADDRESS , INTEGER );

                  ---     channel  node         last          len
  nextproc   = PROCEDURE ( WORD  , NODE , VAR BOOLEAN , VAR INTEGER );

                  ---     channel  node    result
  readyproc  = PROCEDURE ( WORD  , NODE , INTEGER );

CONST
      _transmit = 0;
      _install  = 1;

TYPE

  REQUEST   = POINTER TO request;
  request   = RECORD
                op     : INTEGER;
                res    : INTEGER;
                prot   : INTEGER;
                CASE : INTEGER OF
                |_transmit:
                  node: NODE
                |_install :
                  alloc  : allocproc;
                  input  : inputproc;
                  move   : moveproc;
                  next   : nextproc;
                  ready  : readyproc;
                  channel: WORD
                END
              END;

  DOIO    = PROCEDURE ( VAR request ): INTEGER;
  REFDOIO = POINTER TO DOIO;

PROCEDURE definedriver(name: ARRAY OF CHAR; VAR r: REFDOIO; doio: DOIO): INTEGER;
PROCEDURE removedriver(r: REFDOIO): INTEGER;

PROCEDURE open(name: ARRAY OF CHAR; VAR d: REFDOIO): INTEGER;

END netdrv.
