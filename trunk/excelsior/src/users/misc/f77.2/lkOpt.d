DEFINITION MODULE lkOpt; (* 27-Mar-89. *)

PROCEDURE InitLabs;
PROCEDURE setFormat(label,offs:INTEGER);
PROCEDURE setldFormat(label:INTEGER);
PROCEDURE ldFormat;
PROCEDURE setLabel(label:INTEGER);
PROCEDURE setJump(label:INTEGER; fixed,cond:BOOLEAN);
PROCEDURE optJump;
PROCEDURE procJump;

END lkOpt.
