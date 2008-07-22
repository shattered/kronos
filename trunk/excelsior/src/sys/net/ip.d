DEFINITION MODULE ip; (* Igo 13-Dec-91. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  nos: netKernel;
IMPORT  drv: netdrv;

TYPE WORD    = SYSTEM.WORD;
     ADDRESS = SYSTEM.ADDRESS;

CONST TCP = 6;
      UDP = 17;

PROCEDURE unpackadr(ia: INTEGER; VAR net,host: INTEGER): BOOLEAN;

TYPE DATA     = POINTER TO datagramm;
     datagramm= RECORD
                  f,b : DATA;       (* ip's internal                         *)
                  src : INTEGER;    (* source     network address            *)
                  dst : INTEGER;    (* distenator network address            *)
                  did : INTEGER;    (* external ip unic datagramm identifier *)
                  prot: INTEGER;    (* destination protocol                  *)
                  cque: drv.BUFFER; (* list of data blocks                   *)
                  last: BOOLEAN;    (* last fargment recieved                *)
                  tlen: INTEGER;    (* total length of datagramm             *)
                  stop: BOOLEAN;    (* stopped, all memory must be free      *)

                  time: nos.time    (* timeout for reassemble datagramm      *)
                END;

     inputproc = PROCEDURE ( DATA );

     readyproc = PROCEDURE ( WORD );

     REQUEST = POINTER TO request;
     request = RECORD
                f,b  : REQUEST;
                prot : INTEGER;    (* host protocol id *)
                res  : INTEGER;
                magic: INTEGER;
                obj  : WORD;
                node : drv.node;   (* driver node         *)
                dst  : INTEGER;    (* destination address *)
                buf  : nos.buffer;
                len  : INTEGER;
                dntf : BOOLEAN;    (* don't fragment   *)
                ttl  : INTEGER;    (* time to live     *)
                tout : INTEGER;    (* time-out         *)
                done : readyproc;

                did  : INTEGER;    (* for internal use only *)
                hadr : INTEGER;    (* for internal use only *)

              END;

      range   = RECORD
                  from: INTEGER;
                  to  : INTEGER
                END;

VAL hostadr: INTEGER;

PROCEDURE transmit(VAR r: request): INTEGER;

PROCEDURE install (p: INTEGER; input: inputproc): INTEGER;
PROCEDURE remove  (p: INTEGER): INTEGER;

PROCEDURE sethost (host: INTEGER): INTEGER;

PROCEDURE definedriver(nm : ARRAY OF CHAR;
                       d  : ARRAY OF range; g: BOOLEAN): INTEGER;
PROCEDURE definepath  (n,h: INTEGER; d: ARRAY OF range): INTEGER;

END ip.
