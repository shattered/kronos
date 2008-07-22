DEFINITION MODULE defCPD; (* Leg 19-Oct-90. (c) KRONOS *)

IMPORT  SYSTEM;

TYPE STATE  = RECORD
               type  : INTEGER;
               nokeys: INTEGER;
               rel   : BOOLEAN;
               x_max : INTEGER;
               y_max : INTEGER;
               x,y   : INTEGER;
               keys  : BITSET;
             END;

CONST

   _info    = 00h;  -- (POINTER TO STATE);
   _reset   = 01h;
   _restore = 02h;  -- (POINTER TO STATE);

END defCPD.
