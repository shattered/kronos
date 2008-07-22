DEFINITION MODULE SIOws; (* Leo 11-Nov-89. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  req: defRequest;

VAL
  natiout: BOOLEAN; (* only for Friaschick *)

PROCEDURE doio(VAR r: req.REQUEST);

CONST (* MODES *)
  x1  = {0};            bits5 = {};             odd     = {4};
  x16 = {1};            bits6 = {2};            even    = {4,5};
  x64 = {0,1};          bits7 = {3};            stop1   = {6};
                        bits8 = {2,3};          stop1_5 = {7};
                                                stop2   = {6,7};

  DEFAULT  = x1 + stop2 + bits8;        (* 9600 bauds, no  parity *)
  KEYBOARD = x1 + stop1 + bits8 + odd;  (* 9600 bauds, odd parity *)

PROCEDURE init(channel: INTEGER; mode: BITSET): INTEGER;
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

END SIOws.
