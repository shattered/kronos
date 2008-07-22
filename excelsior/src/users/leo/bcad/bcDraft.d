DEFINITION MODULE bcDraft; (* brd 10-Jan-91. (c) KRONOS *)

IMPORT  def: bcDef;
IMPORT  bas: bcBase;

TYPE PATTERN = DYNARR OF REAL;

(* PATTERN[0]:= PATTERN[1]+ PATTERN[2]+..+PATTERN[HIGH(PATTERN)] *)

----------------------------------------------------------------------------

VAR    step_p: REAL;      (* step for aproximation  curves        *)
      c_ltype: INTEGER;   (* current type line                    *)
      c_color: bas.COLOR; (* current color                        *)
        h_tgA: REAL;      (* tangens angle hatch                  *)
       h_step: REAL;      (* step hatch                           *)
       h_type: INTEGER;   (* current type hatch                   *)
     cir_type: INTEGER;   (* способ задания окружности            *)
     arc_type: INTEGER;   (* способ задания дуги                  *)
     ell_type: INTEGER;   (* способ задания эллипса               *)
     pln_type: INTEGER;   (* способ задания ломаной               *)

VAL
    patts: ARRAY [0..4] OF PATTERN;
    error: INTEGER;
     done: BOOLEAN;

PROCEDURE line;
PROCEDURE circle;
PROCEDURE arca;
PROCEDURE ellips;
PROCEDURE pline;
PROCEDURE box;

PROCEDURE make_vpatt(v: def.POLYLINE; patt: PATTERN; VAR pict: def.VPICTURE);
PROCEDURE make_patt (v: def.POLYLINE; patt: PATTERN; VAR pict: def.PICTURE);
(*  make pict from v at pattern *)

PROCEDURE text;
PROCEDURE hatch;

END bcDraft.
