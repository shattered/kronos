DEFINITION MODULE SIOqqBUS; (* Igo & Leo 27-Nov-89. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  req: defRequest;

VAR coder: ARRAY CHAR OF CHAR;
----------------------------- Hady. 06-Mar-90.

PROCEDURE raw_out(on: BOOLEAN);

PROCEDURE doio(VAR r: req.REQUEST);

PROCEDURE get(): CHAR;

PROCEDURE put(ch: CHAR);

PROCEDURE init(csr: INTEGER; trap: SYSTEM.ADDRESS): INTEGER;

PROCEDURE stop;

PROCEDURE x_inp(on: BOOLEAN);

PROCEDURE monitor(m: PROC);

END SIOqqBUS.
