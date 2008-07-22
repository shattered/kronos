DEFINITION MODULE chessBoard; (* Leo 13-Oct-89. (c) KRONOS *)

IMPORT  SYSTEM;

PROCEDURE show_board;

PROCEDURE put(men,line,col: INTEGER);
(*
   men: (if negative then BLACK)
   0 - empty
   1 - QUEEN
   2 - ROOK (CASTLE)    (ЛАДЬЯ)
   3 - BISHOP           (СЛОН)
   4 - KNIGHT           (КОНЬ)
   5 - KING
   6 - PAWN             (ПЕШКА)
*)

PROCEDURE read(men,l,c: INTEGER; VAR ch: CHAR);

PROCEDURE print(str: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);

PROCEDURE fixmove(men,l0,c0,l1,c1: INTEGER);

PROCEDURE wait;

PROCEDURE bell;

END chessBoard.
