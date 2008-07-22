DEFINITION MODULE exMem; (* Leo  23-Jun-87. (c) KRONOS *)

FROM SYSTEM    IMPORT   ADDRESS;

PROCEDURE sat(val: INTEGER);   PROCEDURE gat(): INTEGER;
(* set & get atr of current line *)

PROCEDURE maxline(): INTEGER;
(* possible lines no. lays in range [0..maxline] *)

PROCEDURE jump(n: INTEGER);

VAL cur, last: INTEGER;  (* READ ONLY. *)

PROCEDURE size?(): INTEGER;

PROCEDURE put(s: ARRAY OF CHAR; sz: INTEGER);

PROCEDURE app(s: ARRAY OF CHAR; sz: INTEGER): BOOLEAN;
(* append line to the end of file & jump to it *)

PROCEDURE get(VAR s: ARRAY OF CHAR; VAR sz: INTEGER);

PROCEDURE adr(): ADDRESS;

PROCEDURE delete(no,after: INTEGER; refresh: PROC);
(* -refresh- will be called after delleting min -after- lines or at the end *)

PROCEDURE insert(no: INTEGER);

PROCEDURE findframe(fromline,fromcol,toline,tocol: INTEGER);
(* set the find frame *)

PROCEDURE find(VAR line_from, col_from: INTEGER;
                   rect: BOOLEAN;
                   pattern: ARRAY OF CHAR): BOOLEAN;
(* returns TRUE if found *)

PROCEDURE alloc(VAR adr: ADDRESS; size: INTEGER);

END exMem.
