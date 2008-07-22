DEFINITION MODULE regExpr; (* Leo 07-Oct-89. (c) KRONOS *)

(* This module manage strings comparisions with regular expressions *)

TYPE
  EXPR;

VAL null: EXPR;
    done: BOOLEAN;    epos: INTEGER; (* position where error occured *)
   error: INTEGER;

PROCEDURE compile(expr: ARRAY OF CHAR; VAR reg: EXPR);
(* compile regular expression             *)
(* error will be not changed if done      *)
(* errors: no_memory, bad_parm            *)

PROCEDURE const(reg: EXPR): BOOLEAN;
(* returns TRUE iff 'reg' is a constant expression *)

PROCEDURE match(re: EXPR; string: ARRAY OF CHAR; pos: INTEGER): BOOLEAN;
(* matching 'string' with regular expression 're' *)

PROCEDURE len(re: EXPR; n: INTEGER): INTEGER;
PROCEDURE pos(re: EXPR; n: INTEGER): INTEGER;
(* returns length and position start of substring *)
(* matched to $n at last call of 'match' proc     *)

PROCEDURE substitute(re      : EXPR;
                     string  : ARRAY OF CHAR;
                     model   : ARRAY OF CHAR;
                     VAR dstr: ARRAY OF CHAR);

PROCEDURE dispose(VAR reg: EXPR); (* not changed "done", "error" *)
(* makes memory occupied by regular expression free *)


END regExpr.


(* This module manage strings comparisions with regular  *)
(* expressions with following syntax:                    *)
(*                                                       *)
(*   RE     = term { "|" term } .                        *)
(*   term   = factor { "&" factor } .                    *)
(*   factor = ["^"] factor | "(" RE ")" pfix | simple .  *)
(*   simple = re { re } .                                *)
(*   pfix   = [ "$" dig1_9 ] .                           *)
(*   re     = str | "[" ["^"] set "]" pfix               *)
(*                | "{" ["^"] set "}" pfix               *)
(*                | "*" pfix                             *)
(*                | "?" pfix .                           *)
(*   str    = char1 { char1 } .                          *)
(*   char1  = ' any character except                     *)
(*             "[","*","?","\","(","{",")", 0c '         *)
(*          | "\" od od od | "\*" | "\?" | "\[" | "\&"   *)
(*          | "\|" |  "\^" | "\(" | "\{" | "\)"          *)
(*          | "\n" |  "\r" | "\l" | "\$" | "\\" .        *)
(*   set    = { char2 | char2 "-" char2 } .              *)
(*   char2  = ' any character except "]","-",0c '        *)
(*          | "\" od od od | "\-" | "\]" | "\^"          *)
(*          | "\n" |  "\r" | "\l" | "\\" .               *)
(*   od     = "0" | "1" | "2" | "3" |                    *)
(*   dig1_9 = "1" | "2" | "3" | "4" | "5" |              *)
(*            "6" | "7" | "8" | "9" .                    *)

(* NOTE: implementation uses Heap!                       *)
