DEFINITION MODULE mcGen; (* Ned 29-Oct-90. (c) KRONOS *)

IMPORT obs: mcObj;
IMPORT  pc: pcTab;

PROCEDURE new(VAR r: pc.ref; m: pc.mode);

PROCEDURE tie(VAR r: pc.ref; m: pc.mode);

PROCEDURE const (VAR r: pc.ref; o: obs.obj_ptr; val: pc.ref);
PROCEDURE var   (VAR r: pc.ref; o: obs.obj_ptr);
PROCEDURE proc  (VAR r: pc.ref; o: obs.obj_ptr);
PROCEDURE param (VAR r: pc.ref; p: obs.obj_ptr);
PROCEDURE module(VAR r: pc.ref; p: obs.obj_ptr);

PROCEDURE array   (VAR r: pc.ref; t: obs.type_ptr);
PROCEDURE dynarr  (VAR r: pc.ref; t: obs.type_ptr);
PROCEDURE array_of(VAR r: pc.ref; t: obs.type_ptr);

PROCEDURE set     (VAR r: pc.ref; t: obs.type_ptr);
PROCEDURE range   (VAR r: pc.ref; t: obs.type_ptr; l,r: pc.ref);
PROCEDURE enum    (VAR r: pc.ref; t: obs.type_ptr);
PROCEDURE pointer (VAR r: pc.ref; t: obs.type_ptr);
PROCEDURE proctype(VAR r: pc.ref; t: obs.type_ptr);

PROCEDURE hidden(VAR r: pc.ref; t: obs.type_ptr);
PROCEDURE bitset(VAR r: pc.ref; t: obs.type_ptr);

PROCEDURE inverse(VAR p: pc.ref);

---------------------------------------------------------------

PROCEDURE number(VAR n: pc.ref; val: INTEGER; type: obs.type_ptr);
PROCEDURE usage(VAR r: pc.ref; v: obs.obj_ptr);

PROCEDURE app_valparm(VAR type: pc.ref);

END mcGen.
