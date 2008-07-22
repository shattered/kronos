DEFINITION MODULE myEditor; (* Leo 26-Jan-89. (c) KRONOS *) IMPORT SYSTEM;


VAR

  f_name : PROCEDURE (VAR ARRAY OF CHAR);

  last   : PROCEDURE (): INTEGER;   (* last line in text *)
  jump   : PROCEDURE (INTEGER);     (* jumps to line     *)

  crs_pos: PROCEDURE (VAR INTEGER, VAR INTEGER);

  get    : PROCEDURE (VAR ARRAY OF CHAR, VAR INTEGER);
  put    : PROCEDURE (    ARRAY OF CHAR,     INTEGER);
  app    : PROCEDURE (    ARRAY OF CHAR,     INTEGER): BOOLEAN;
  del    : PROCEDURE (INTEGER); (* delete N lines *)
  ins    : PROCEDURE (INTEGER); (* insert N lines *)
  adr    : PROCEDURE (): SYSTEM.ADDRESS;
  size   : PROCEDURE (): INTEGER;

  refresh: PROCEDURE;
  goto   : PROCEDURE (INTEGER,INTEGER); (* moves center of screen to pos *)

           --             line0        column0      line1        column1
  frame  : PROCEDURE (VAR INTEGER, VAR INTEGER, VAR INTEGER, VAR INTEGER);

           --           wait     format            args
  message: PROCEDURE (BOOLEAN, ARRAY OF CHAR, SEQ SYSTEM.WORD);

  mark   : PROCEDURE (INTEGER, INTEGER, ARRAY OF CHAR, SEQ SYSTEM.WORD);
           --           fname
  onread : PROCEDURE (ARRAY OF CHAR): BOOLEAN;          -- TRUE if possible
           --                        exit
  onwrite: PROCEDURE (ARRAY OF CHAR, BOOLEAN): BOOLEAN; -- TRUE if possible
  onbreak: PROCEDURE (): BOOLEAN;                       -- TRUE if possible

  start  : PROC;  (* this procedure called at the beginning of editor work *)
                  (* but after reading source file                         *)


END myEditor.
