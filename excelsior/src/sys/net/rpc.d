DEFINITION MODULE rpc; (* Igo 26-Nov-91. (c) KRONOS *)

(* Remote Procedure Call *)

IMPORT  SYSTEM;
IMPORT  os : osKernel;
IMPORT  nos: netKernel;

-- reply stat

CONST MSG_ACCEPTED = 0;
      MSG_DENIED   = 1;

-- accept_stat

CONST SUCCESS       = 0; -- RPC executed successfully
      PROG_UNAVAIL  = 1; -- remote hasn't exported program
      PROG_MISMATCH = 2; -- remote can't support version
      PROC_UNAVAIL  = 3; -- program can't support procedure
      GARBAGE_ARGS  = 4; -- procedure can't decode params

-- reject_stat

CONST RPC_MISMATCH = 0; -- rpc version mismatch
      AUTH_ERROR   = 1; -- remote can't authenticate caller

TYPE WORD    = SYSTEM.WORD;
     ADDRESS = SYSTEM.ADDRESS;

     PORT;

     BUFFER  = POINTER TO buffer;
     buffer  = RECORD
                 adr: ADDRESS;
                 sz : INTEGER
               END;

     rpcADR  = POINTER TO rpcadr;
     rpcadr  = RECORD
                 iadr: INTEGER; (* internet address *)
                 port: INTEGER; (* udp port         *)
                 prog: INTEGER; (* rpc programm     *)
                 vers: INTEGER;
                 proc: INTEGER; (* rpc procedure    *)

                 stat: INTEGER;
                 res : INTEGER;

                 tout: INTEGER;
                 try : INTEGER
               END;

     DOIT    = PROCEDURE ( WORD, INTEGER , INTEGER , VAR ARRAY OF WORD );

     INSTALL = POINTER TO install;
     install = RECORD
                 port : INTEGER;         (* udp port                  *)
                 prog : INTEGER;         (* rpc programm              *)
                 nproc: INTEGER;         (* number of procedures      *)
                 vlow : INTEGER;         (* lowest  supported version *)
                 vhigh: INTEGER;         (* highest supported version *)
                 doit : DOIT;
                 obj  : WORD
               END;

CONST null = PORT(NIL);

PROCEDURE readnum   (VAR a: ARRAY OF WORD; VAR p: INTEGER; VAR n: INTEGER): BOOLEAN;
PROCEDURE writenum  (VAR a: ARRAY OF WORD; VAR p: INTEGER;     n: INTEGER);
PROCEDURE writeshort(VAR a: ARRAY OF WORD; VAR p: INTEGER;     n: INTEGER);

PROCEDURE call   (VAR a: rpcadr; p: PORT; VAR pb: nos.buffer; len: INTEGER;
                  VAR rb: buffer): INTEGER;
(* call remote procedure specified by adr *)

PROCEDURE garbageargs(p: PORT);
(* badreply                                                   *)
(* this procedure must be called from doit procedure only !!! *)

PROCEDURE reply  (p: PORT; VAR pb: buffer);
(* reply remote call                                          *)
(* this procedure must be called from doit procedure only !!! *)
(* memory for parameters must be allocated by nos.allocate    *)

PROCEDURE create (VAR p: PORT; VAR i: install): INTEGER;
(* install remote program on udp port                         *)

PROCEDURE remove (VAR p: PORT): INTEGER;
(* remove remote program                                      *)

END rpc.
