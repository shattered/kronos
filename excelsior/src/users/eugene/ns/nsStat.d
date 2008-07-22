DEFINITION MODULE nsStat; (* Sem 06-Oct-90. (c) KRONOS *)

FROM pcTab       IMPORT ref;
FROM nsExpr      IMPORT node;

VAR
  LAST     : BOOLEAN;
  procs    : ref;
  last_proc: ref;

PROCEDURE do_flow(): node;

END nsStat.
