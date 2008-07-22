DEFINITION MODULE mcSym; (* Ned 21-May-88. (c) KRONOS *)

IMPORT  obs: mcObj;

PROCEDURE get_sym(cu: obs.obj_ptr; id: INTEGER);

PROCEDURE put_sym(cu: obs.obj_ptr; unit,def_time,imp_time: INTEGER);

PROCEDURE Ini;
PROCEDURE Exi;

END mcSym.
