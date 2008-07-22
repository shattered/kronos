DEFINITION MODULE Poly;

-- All variables of type _poly_ should be initialized by _ini_
--  (otherwise - ASSERT!)

TYPE poly;

PROCEDURE ini(VAR z: poly; SEQ coef: REAL);     -- ini(p, 1.,0.,0.) p=x^2
                                                -- ini(p) p=0
--  for memory release --
PROCEDURE drop(VAR p: poly);    -- for local variables of procedures
PROCEDURE tmp (p: poly);        -- such as  tmp(p); RETURN p

PROCEDURE asg(VAR z: poly; w: poly);

PROCEDURE !(SEQ coef: REAL): poly;              -- !()=0
PROCEDURE mono(deg: INTEGER; coef: REAL): poly;

PROCEDURE add(z1,z2: poly): poly;
PROCEDURE sub(z1,z2: poly): poly;
PROCEDURE mul(z1,z2: poly): poly;

PROCEDURE eq(z1,z2: poly): BOOLEAN;

PROCEDURE eval(z: poly; x: REAL): REAL;

PROCEDURE deg?(z: poly): INTEGER;
PROCEDURE coef?(z: poly; deg: INTEGER): REAL;

PROCEDURE appoly(VAR s: ARRAY OF CHAR; fmt: ARRAY OF CHAR; SEQ arg: poly);
(*
   формат ::=% [ ('^'|'~'!space) буква]p.
   по умолчанию переменная = x
   %^yp   --> 2.y^3
   % yp   --> 2.y3
*)

END Poly.
