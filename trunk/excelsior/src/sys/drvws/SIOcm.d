DEFINITION MODULE SIOcm; (* Leo 11-Nov-89. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  req: defRequest;

VAL
  natiout: BOOLEAN; (* only for Friaschick *)

PROCEDURE doio(VAR r: req.REQUEST);

PROCEDURE init;
(* note: channel may by inited ONLY once after power on! *)

PROCEDURE stop;

PROCEDURE raw_out(on: BOOLEAN);  (* 36c # 15c 12c     *)
PROCEDURE x_inp  (on: BOOLEAN);  (* xon/xoff at input *)

PROCEDURE monitor(m: PROC);
(* never ending process to translate keyboard buffer to   *)
(* input buffer (may and must call get() and put() procs) *)

PROCEDURE raw_inp; (* may be used in monitor(raw_inp) *)

PROCEDURE get(): BITSET;
(* low 8 bits accepted byte,   8..10 bits error code *)

PROCEDURE put(ch: BITSET);
(* low 8 bits translated byte, 8..10 bits error code *)

END SIOcm.
