DEFINITION MODULE bcDef; (* brd 05-Dec-90. (c) KRONOS *)

CONST  X=0; Y=1; Z=2;

TYPE VERTEX   = ARRAY [0..2] OF REAL;
     VECTOR   = ARRAY [0..1] OF VERTEX;
     POLYLINE = DYNARR OF VERTEX;

     PICTURE  = DYNARR OF POLYLINE;
     VPICTURE = DYNARR OF VECTOR;

     Line   = RECORD
                xyz,XYZ: VERTEX;
                pict : VPICTURE;
                ltyp : INTEGER;
                color: INTEGER
              END;

     Circ   = RECORD
                centr: VERTEX;
                r    : REAL;
                pict : PICTURE;
                ltyp : INTEGER;
                color: INTEGER
              END;

        Arc = RECORD
                xyz1, xyz2, xyz3: VERTEX;
                pict : PICTURE;
                ltyp : INTEGER;
                color: INTEGER
              END;

     Ellips = RECORD
                centr: VERTEX;
                a,b  : REAL;
                angle: REAL; --angle between horizontal and a
                pict : PICTURE;
                ltyp : INTEGER;
                color: INTEGER;
              END;

     Pline  = RECORD
                body : POLYLINE;
                pict : PICTURE;
                type : INTEGER; --line/spline/other
                ltyp : INTEGER;
                color: INTEGER
              END;

     Text = RECORD
              stxt : STRING;
              pict : VPICTURE;
              xyz  : VERTEX;
              w,h,a,i: REAL;
              color: INTEGER;
            END;


   tria_ptr;

   TRIANGLE = RECORD
                  xyz: ARRAY [0..2] OF VERTEX;
                 ngbr: ARRAY [0..2] OF tria_ptr;
                 next: tria_ptr;
                color: INTEGER;
              END;

   tria_ptr = POINTER TO TRIANGLE;

    Surf = tria_ptr;

      Group = RECORD
                line  : DYNARR OF Line;
                circ  : DYNARR OF Circ;
                arc   : DYNARR OF Arc;
                ellips: DYNARR OF Ellips;
                pline : DYNARR OF Pline;
                txt   : DYNARR OF Text;
                surf  : DYNARR OF Surf
              END;

END bcDef.
