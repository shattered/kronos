DEFINITION MODULE SIOmBUS; (* Leo 10-Nov-90. (c) KRONOS *)

IMPORT  req: defRequest;

VAR
  xon     : CHAR; (* init 21c *)
  xoff    : CHAR; (* init 23c *)
  (* to suppress xon/xoff set it BOTH to 000c *)
  raw_out : BOOLEAN; (* init TRUE;  FALSE =>  36c to CR+LF *)
  out_stop: BOOLEAN; (* output stopped                     *)

PROCEDURE doio(VAR r: req.REQUEST);

PROCEDURE init(channel: INTEGER): INTEGER;
PROCEDURE stop;

(* Data wich will be readen by doio(READ) MUST be       *)
(* previosly translated by put(get()) procedure call.   *)
(* Caller of get() is suspended until input buffer will *)
(* not be empty.                                        *)

PROCEDURE get(VAR ch: CHAR);
PROCEDURE put(ch: CHAR);

END SIOmBUS.
