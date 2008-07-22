DEFINITION MODULE abcSLE; (* Leo   10-Dec-88. (c) KRONOS *)
                          (* Hady. 20-Feb-89. (c) KRONOS *)

VAR  ins: BOOLEAN; (* insert mode; initialy TRUE *)
     bel: BOOLEAN; (* bell   mode; initialy TRUE *)

TYPE BUFF; -- кольцевой буфер для редактируемых строк

VAL buff: BUFF;

VAR read: PROCEDURE (): CHAR; (* keyboard read proc *)

PROCEDURE save;         PROCEDURE restore;

---------------------  BUFFERS SUPPORT  -----------------------
                       ---------------
PROCEDURE New():  BUFF; -- creates new buffer

PROCEDURE Set(b: BUFF); -- set buffer current

---------------------  Single Line Editor  --------------------
                     ----------------------

VAL line, cp: INTEGER; -- line & cursor position in frame

PROCEDURE ref_pos;     -- sets cursor to actual place in screen;

PROCEDURE SLE(    prompt: ARRAY OF CHAR;
              VAR string: ARRAY OF CHAR;
              line_on_screen,left_column,right_column: INTEGER;
                     old: BOOLEAN;
              SEQ terminators: CHAR);

(* Max  1024  characters lenght string  may be  entered by -SLE-.       *)
(* You must specify the frame on the screen where reading perfomed.     *)
(* You may add terminators chars to default terminators (cr,lf,         *)
(* newln). Last pressed key SLE puts in -last_char- after termination.  *)

VAL  last_char: CHAR;  (* last pressed char *)

END abcSLE.
