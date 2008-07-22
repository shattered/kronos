DEFINITION MODULE bcTool; (* brd 19-Jan-91 *)

IMPORT  def: bcDef;

CONST COPY= TRUE; MOVE= FALSE;
      SHIFT= 0;  ROTATE= 1; MIRROR= 2;

VAR OPERATION: INTEGER;  (* shift/rotate/mirror/..   *)
         MODE: BOOLEAN;  (* copy/move *)

PROCEDURE delete;
PROCEDURE shift;
PROCEDURE copy;
PROCEDURE mirror;
PROCEDURE turn;
PROCEDURE scale;
PROCEDURE zoom_w;
PROCEDURE zoom_a;
PROCEDURE zoom_d;
PROCEDURE ed_pline;
PROCEDURE center;

PROCEDURE search_lin(n: INTEGER; top: def.VERTEX);
PROCEDURE search_cir(n: INTEGER; top: def.VERTEX);
PROCEDURE search_pln(n: INTEGER; top: def.VERTEX);
PROCEDURE search_arc(n: INTEGER; top: def.VERTEX);
PROCEDURE search_ell(n: INTEGER; top: def.VERTEX);
PROCEDURE search_txt(n: INTEGER; top: def.VERTEX);
PROCEDURE search_grp(n: INTEGER; top: def.VERTEX);
(* поиск в n-ом слое, найденные помещаются в  obj.mark_xxx *)

PROCEDURE select(): BOOLEAN;
PROCEDURE unmark;

PROCEDURE make_group;
PROCEDURE dest_group;

END bcTool.
