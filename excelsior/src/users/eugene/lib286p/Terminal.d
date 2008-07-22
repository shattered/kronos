DEFINITION MODULE Terminal; (* Leo 18-Oct-85. (c) KRONOS *) IMPORT  SYSTEM;
                            (* Leo 24-Apr-86. (c) KRONOS *)
                            (* Ned 21-Aug-89. (c) KRONOS *)
                            (* Ned 19-Sep-89. (c) KRONOS *)

(* Определяет операции над текущим терминалом задачи. *)

--------------------------  STANDARD  --------------------------
                          ------------
PROCEDURE Write(ch: CHAR);

PROCEDURE WriteString(s: ARRAY OF CHAR);

PROCEDURE WriteLn;

PROCEDURE Show(str: ARRAY OF CHAR);

--------------------------  EXTENDED  --------------------------
                          ------------

PROCEDURE print(format: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);

PROCEDURE write(str: ARRAY OF CHAR; pos,len: INTEGER);

--------------------------  SCREEN    --------------------------
                          ------------

PROCEDURE set_pos(line,col: INTEGER);
PROCEDURE home;
PROCEDURE bottom;

PROCEDURE repeat(ch: CHAR; times: INTEGER);

PROCEDURE erase     (how: INTEGER);
PROCEDURE erase_line(how: INTEGER);

PROCEDURE set_cursor   (ON_OFF: INTEGER);
PROCEDURE set_blinking (ON_OFF: INTEGER);
PROCEDURE set_something(ON_OFF: INTEGER);

PROCEDURE set_color    (color: INTEGER);
PROCEDURE set_back     (color: INTEGER);

END Terminal.
