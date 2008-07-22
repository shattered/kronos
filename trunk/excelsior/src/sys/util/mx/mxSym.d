DEFINITION MODULE mxSym; (* Ned 21-May-88. (c) KRONOS *)

PROCEDURE getsym(id: INTEGER);

PROCEDURE createsym(name: ARRAY OF CHAR; unit,def_time,imp_time: INTEGER);
PROCEDURE outscope;
PROCEDURE put_xpos(proc_no,pc,line,col: INTEGER);
PROCEDURE closesym;

PROCEDURE Ini;
PROCEDURE Exi;

END mxSym.
