DEFINITION MODULE GameBoard; (* Shu 09-Jul-87. (c) KRONOS *)


FROM SYSTEM    IMPORT   WORD;

CONST MAXKEY = 255;

(* The -keys- must be nor of INTEGER in range [0..255]  *)
(* or of any enumeration type. Value -1                 *)
(* reserved to message of 'nothing pressed'.            *)


PROCEDURE Raw(on: BOOLEAN);
(* on-off raw keyboard *)

PROCEDURE ReadPair(VAR on,off: CHAR);
(* read code-pair *)

PROCEDURE LetBe(on,off: CHAR; key: WORD);
(* Let's code-pair (on-off) corresponds to key *)
(* Setting  key to -1 is uncorrectly action    *)
(* Default:                                    *)
(*   LetBe(124c,324c,255);                     *)
(* When Break pushbutton pressed first user    *)
(* program got  key 255 and after it invoke    *)
(* program terminate                           *)

PROCEDURE SetBreak(on,off: CHAR);
(* no comment. Default 124c 324c == "Break" pushbutton *)

PROCEDURE WaitAny(VAR p: WORD): BOOLEAN;
(* on - off result *)

PROCEDURE Flush;
(* flush all pending signals about pressed pushbuttons *)

VAR press?: ARRAY [0..MAXKEY] OF BOOLEAN;

PROCEDURE Pressed(): INTEGER;
(* number of pressed pushbuttons *)

END GameBoard.

(* LABTAM 3000.V86.1 IBM PC style keyboard:


   1  =   2c;    2  =   3c;      3    =   4c;     4  =   5c;
   5  =   6c;    6  =   7c;      7    =  10c;     8  =  11c;
   9  =  12c;    0  =  13c;      -    =  14c;     =  =  15c;
   `  =  51c;  BACK =  16c;     Scroll= 106c;    TAB =  17c;
  Num = 105c;

     Q  =  20c;     W  =  21c;    E  =  22c;    R  =  23c;
     T  =  24c;     Y  =  25c;    U  =  26c;    I  =  27c;
     O  =  30c;     P  =  31c;    [  =  32c;    ]  =  33c;
    Ctrl=  35c;     A  =  36c;    S  =  37c;    D  =  40c;
     F  =  41c;     G  =  42c;    H  =  43c;    J  =  44c;
     K  =  45c;     L  =  46c;    ;  =  47c;    '  =  50c;
  lSHIFT=  52c;     \  =  53c;    Z  =  54c;
     X  =  55c;     C  =  56c;    V  =  57c;    B  =  60c;
     N  =  61c;     M  =  62c;    <  =  63c;    >  =  64c;
     /  =  65c; rSHIFT =  66c;   Prt =  67c;
    Alt =  70c;  SPACE =  71c;  Break= 124c;

    Pause= 125c; CAPS  =  72c;

    End = 117c;  down = 120c;  PgDw = 121c;
    left= 113c;    5  = 114c;  right= 115c;
    Home= 107c;   up  = 110c;  PgUp = 111c;
     *  = 127c;   -   = 112c;  +    = 116c;    CR = 34c;
    Ins = 122c;   Del = 123c;  Enter= cr;

    f1  =  74c;   f2  =  75c;   f3  =  76c;    f4 =  77c;
    f5  = 100c;   f6  = 101c;   f7  = 102c;    f8 = 103c;
    f9  = 104c;   f0  =  73c;   Esc = 1c;

*)
