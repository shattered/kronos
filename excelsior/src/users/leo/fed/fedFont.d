DEFINITION MODULE fedFont; (* nick 29-May-91 *)

IMPORT  defFont, defBMG, defScreen;

CONST  prop = defFont.prop;
     italic = defFont.italic;
     packed = defFont.packed;

TYPE Font;

VAL null: Font;
    done: BOOLEAN;
   error: INTEGER;

------------------ F o n t  O p e r a t i o n ------------------
                  ----------------------------

PROCEDURE new    (VAR f: Font; w,h: INTEGER; f,l: CHAR; s: BITSET);
PROCEDURE dispose(VAR f: Font);

PROCEDURE read(VAR f: Font; name: ARRAY OF CHAR);
PROCEDURE save(    f: Font; name: ARRAY OF CHAR);

CONST up={0};  dw={1};  lf={2};  rg={3};    (* <- mode *)

PROCEDURE fontW!(f: Font; w: INTEGER; mode: BITSET);
PROCEDURE fontH!(f: Font; h: INTEGER; mode: BITSET);
PROCEDURE bline!(f: Font; y: INTEGER);
PROCEDURE uline!(f: Font; y: INTEGER);
PROCEDURE state!(f: Font; s: BITSET);
PROCEDURE fchar!(f: Font; c: CHAR);
PROCEDURE lchar!(f: Font; c: CHAR);

PROCEDURE fontW?(f: Font): INTEGER;
PROCEDURE fontH?(f: Font): INTEGER;
PROCEDURE bline?(f: Font): INTEGER;
PROCEDURE uline?(f: Font): INTEGER;
PROCEDURE state?(f: Font): BITSET;
PROCEDURE fchar?(f: Font): CHAR;
PROCEDURE lchar?(f: Font): CHAR;

PROCEDURE proportional(f: Font): BOOLEAN;

------------------ C h a r  O p e r a t i o n ------------------
                  ----------------------------

PROCEDURE readchar(f: Font; bmd: defBMG.BITMAP; ch: CHAR);
PROCEDURE savechar(f: Font; bmd: defBMG.BITMAP; ch: CHAR);

PROCEDURE cellx!(f: Font; ch: CHAR; x : INTEGER);
PROCEDURE propx!(f: Font; ch: CHAR; px: INTEGER);
PROCEDURE propw!(f: Font; ch: CHAR; pw: INTEGER);

PROCEDURE cellx?(f: Font; ch: CHAR): INTEGER;
PROCEDURE propx?(f: Font; ch: CHAR): INTEGER;
PROCEDURE propw?(f: Font; ch: CHAR): INTEGER;

PROCEDURE xwrite( bmd: defBMG.BITMAP; tool: defScreen.TOOL;
                  x,y: INTEGER;       font: Font;
                  str: ARRAY OF CHAR;  p,l: INTEGER): INTEGER;

END fedFont.
