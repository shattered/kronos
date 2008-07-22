DEFINITION MODULE exHead; (* Leo  28-Jun-87. (c) KRONOS *)

FROM SYSTEM     IMPORT  WORD;

TYPE String = ARRAY [0..127] OF CHAR;

VAR Home: INTEGER;  (* default:=22 *)
  prompt: ARRAY [0..31] OF CHAR;
 command: String;
   width: INTEGER;  (* default:=64 *)
   alarm: BOOLEAN;  (* normally FALSE *)
    UPPG: BOOLEAN;


PROCEDURE ReadCommand;

PROCEDURE message(wait: BOOLEAN; fmt: ARRAY OF CHAR; SEQ args: WORD);
(* cleans the info line on screen & not refresh it! *)

PROCEDURE ask(s: ARRAY OF CHAR): CHAR;
PROCEDURE readstr(prompt: ARRAY OF CHAR; VAR str: ARRAY OF CHAR);

PROCEDURE start(s: ARRAY OF CHAR);
PROCEDURE finish;

END exHead.
