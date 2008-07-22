DEFINITION MODULE tcp; (* Igo 18-Dec-91. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  nos: netKernel;

TYPE ADDRESS = SYSTEM.ADDRESS;
     WORD    = SYSTEM.WORD;

     SESSION;


CONST
      null = SESSION(NIL);

      (* events *)
      _close    = 0;
      _reset    = 1;
      _removed  = 2;
      _timeout  = 3;
      _urgent   = 4;

      (* operations *)
      _callses  = 01;
      _waitses  = 02;
      _closeses = 03;
      _send     = 04;
      _receive  = 05;

      (* request mode *)
      _pushm    = {00};
      _urgentm  = {01};

                        -- session id,obj, event --
TYPE eventproc = PROCEDURE (ADDRESS,WORD,INTEGER);

     doneproc  = PROCEDURE ( WORD );

     REQUEST = POINTER TO request;
     request = RECORD
                 f,b  : REQUEST;
                 res  : INTEGER;
                 sid  : SESSION;        (* session id  *)
                 len  : INTEGER;        (* len of data *)

                 mode : BITSET;         (* mode of request *)

                 tout : INTEGER;        (* timeout in miliseconds *)
                 time : nos.time;

                 done : doneproc;       (* done procedure               *)
                 obj  : SYSTEM.WORD;    (* parameter for done procedure *)

                 rcnt : INTEGER;
                 rm   : BOOLEAN;        (* request marked to done *)

                 CASE op: INTEGER OF
                 |_callses,
                  _waitses :
                   lnid : INTEGER;      (* local  node id *)
                   rnid : INTEGER;      (* remote node id *)
                   radr : INTEGER;      (* remote host internet adress *)

                   ibsz : INTEGER;      (* input fifo size *)
                   ipbsz: INTEGER;      (* input push fifo size *)

                   event: eventproc;    (* event handler for session   *)
                   eobj : WORD;         (* parameter for event handler *)

                 |_closeses:

                 |_send    :
                   obuf  : nos.buffer;   (* output buffer *)

                 |_receive :
                   ilen : INTEGER;      (* len of received data     *)
                   ibuf : ADDRESS;      (* address of input buffer  *)
                   ipos : INTEGER;      (* position in input buffer *)

                 END
               END;

PROCEDURE doio(VAR r: request);

END tcp.
