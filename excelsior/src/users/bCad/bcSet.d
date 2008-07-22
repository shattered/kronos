DEFINITION MODULE bcSet; (* brd 26-Jan-91 *)

IMPORT  bas: bcBase;

--------------------- control context editor ---------------------

PROCEDURE layers   (x,y: INTEGER);
PROCEDURE type_line(x,y: INTEGER);
PROCEDURE color    (x,y: INTEGER);
PROCEDURE grid     (x,y: INTEGER);
PROCEDURE step     (x,y: INTEGER);
  (* x,y -center menus *)

PROCEDURE view(x,y: INTEGER);

PROCEDURE select_model(x,y: INTEGER; VAR m: bas.VMODEL);
PROCEDURE color?(VAR c: INTEGER; X,Y: INTEGER);


END bcSet.
