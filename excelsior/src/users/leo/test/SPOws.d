DEFINITION MODULE SPOws; (*  16-Aug-91. (c) KRONOS *)

IMPORT  SYSTEM;

VAR autowrap: BOOLEAN; (* initialy TRUE *)

PROCEDURE plane(n: INTEGER);

PROCEDURE erase;
PROCEDURE eraseline;

PROCEDURE setpos(l,c: INTEGER);

PROCEDURE print(format: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
(* only \n \r \l %s %c %d %h available in format *)

END SPOws.
