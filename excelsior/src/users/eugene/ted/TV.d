DEFINITION MODULE TV; (* Leo 06-Nov-87. (c) KRONOS *)

PROCEDURE INIT;
(* Init TV. Must be called before any other TVD proc *)

PROCEDURE FINISH;
(* переход в символьный режим, курсор слева внизу *)

PROCEDURE mode(m: INTEGER);
(* see TV.mode:
      com= ; -- complement (new xor old)
      rep= ; -- replace    (new independent of old)
      add= ; -- overlay    (new  or old)
*)

PROCEDURE color(i: INTEGER);

PROCEDURE patt(i: INTEGER);

PROCEDURE fill(i: INTEGER);

PROCEDURE pos(x,y: INTEGER);

PROCEDURE vect(x1,y1: INTEGER);

PROCEDURE circ(r: INTEGER);

PROCEDURE rect(x,y: INTEGER);

PROCEDURE rect_s(x1,y1,x2,y2,x3,y3: INTEGER);

PROCEDURE roll(dir,n: INTEGER);
(* 0 -- N, 1 -- E, 2 -- W, 3 -- S *)

PROCEDURE cursor(no: INTEGER);

PROCEDURE push;

PROCEDURE pop;

PROCEDURE WriteString(ln,col: INTEGER; VAL s: ARRAY OF CHAR);
(* current position not changed *)

END TV.
