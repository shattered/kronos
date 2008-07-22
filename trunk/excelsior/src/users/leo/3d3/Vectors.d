DEFINITION MODULE Vectors; (* nick 02-Aug-90. (c) KRONOS *)

TYPE VECTOR = ARRAY [0..2] OF REAL;
     MATRIX = ARRAY [0..2] OF VECTOR;

     (*           [0,j][1,j][2,j]        *)
     (*           | v    v    v | [i,0]  *)
     (* matrix =  | e    e    e | [i,1]  *)
     (*           | c    c    c | [i,2]  *)


VAL null: VECTOR;

PROCEDURE sub(VAR vr: VECTOR; v1,v2: VECTOR);  --- vr=v1-v2
PROCEDURE add(VAR vr: VECTOR; v1,v2: VECTOR);  --- vr=v1+v2
PROCEDURE vml(VAR vr: VECTOR; v1,v2: VECTOR);  --- vr=[v1*v2]

PROCEDURE sml(v1,v2: VECTOR): REAL;            --- RETURN [v1 v2]
PROCEDURE len(vect : VECTOR): REAL;            --- lenght of vect

PROCEDURE normal(VAR v: VECTOR);

PROCEDURE equal (v1,v2: VECTOR): BOOLEAN;

PROCEDURE VxM(VAR vr: VECTOR; v: VECTOR; m: MATRIX);
PROCEDURE MxM(VAR mr: MATRIX; l: MATRIX; r: MATRIX);

END Vectors.
