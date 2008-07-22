DEFINITION MODULE GPHtechnology; (* 19-Nov-87. (c) KRONOS *)

TYPE
    Track=RECORD
      diameter : INTEGER;
      displaceX: INTEGER;
      displaceY: INTEGER;
    END;
    Pin=RECORD
      diameter0: INTEGER;
      diameter1: INTEGER;
    END;
    Via=RECORD
      type     : INTEGER;
      dril     : INTEGER;
      displaceX: INTEGER;
      displaceY: INTEGER;
    END;

VAR RouterMode: BITSET;
    RoutGrid  : ARRAY [0..1] OF INTEGER;
    ViasGrid  : INTEGER;
    Clearance : INTEGER;
    Tracks    : ARRAY [0..15] OF Track;
    Pins      : ARRAY [0..15] OF Pin;
    Vias      : ARRAY [0..15] OF Via;
    Resist    : INTEGER;

PROCEDURE Save(nm: ARRAY OF CHAR);

PROCEDURE Restore(nm: ARRAY OF CHAR);

END GPHtechnology.
