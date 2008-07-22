DEFINITION MODULE Terminal; (* Leo 18-Oct-85. (c) KRONOS *) IMPORT  SYSTEM;
                            (* Leo 24-Apr-86. (c) KRONOS *)
                            (* Ned 21-Aug-89. (c) KRONOS *)
                            (* Ned 19-Sep-89. (c) KRONOS *)

(* Определяет операции над текущим терминалом задачи. *)

-----------------------  TERMINAL STATE  ----------------------
                       ------------------
TYPE
  BARS   = ARRAY [0..2],[0..2] OF CHAR;
  STATE  = POINTER TO STATUS;
  STATUS = RECORD
             type     : INTEGER;        hbar : CHAR;
             lines    : INTEGER;        vbar : CHAR;
             columns  : INTEGER;        bars : BARS;
             min_color: INTEGER;        back : INTEGER;
             max_color: INTEGER;        color: INTEGER;
             fonts    : INTEGER;        font : INTEGER;
             screens  : INTEGER;        scr  : INTEGER;

             cursor   : INTEGER;        awp   : INTEGER;
             something: INTEGER;        raw   : INTEGER;
             blinking : INTEGER;        smooth: INTEGER;
             reverse  : INTEGER;        cinter: INTEGER;
             underline: INTEGER;
           END;

VAL
   done: BOOLEAN;
  error: INTEGER;
  iolen: INTEGER;
  state: STATE;

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

PROCEDURE perror(errcode: INTEGER;
                  format: ARRAY OF CHAR;
                SEQ args: SYSTEM.WORD);

---------------------------  SCREEN  --------------------------
                           ----------

PROCEDURE set_pos(line,col: INTEGER);
PROCEDURE home;
PROCEDURE bottom;

PROCEDURE repeat(ch: CHAR; times: INTEGER);

PROCEDURE erase     (how: INTEGER);
PROCEDURE erase_line(how: INTEGER);
PROCEDURE erase_chars(no: INTEGER);

PROCEDURE roll_down(n: INTEGER);
PROCEDURE roll_up  (n: INTEGER);

PROCEDURE scroll_down(n: INTEGER);
PROCEDURE scroll_up  (n: INTEGER);

PROCEDURE up   (n: INTEGER);            PROCEDURE left (n: INTEGER);
PROCEDURE down (n: INTEGER);            PROCEDURE right(n: INTEGER);

PROCEDURE ins_char(n: INTEGER);         PROCEDURE ins_line(n: INTEGER);
PROCEDURE del_char(n: INTEGER);         PROCEDURE del_line(n: INTEGER);

PROCEDURE set_cursor   (ON_OFF: INTEGER);
PROCEDURE set_reverse  (ON_OFF: INTEGER);
PROCEDURE set_underline(ON_OFF: INTEGER);
PROCEDURE set_blinking (ON_OFF: INTEGER);
PROCEDURE set_something(ON_OFF: INTEGER);
PROCEDURE set_cinter   (ON_OFF: INTEGER);
PROCEDURE set_font     (no: INTEGER);
PROCEDURE load_font    (no: INTEGER; from,to: CHAR;
                        font: ARRAY OF SYSTEM.WORD);


PROCEDURE set_attr     (no: INTEGER; val: INTEGER);
PROCEDURE get_attr     (no: INTEGER): INTEGER;

PROCEDURE set_color(color: INTEGER);
PROCEDURE set_back (color: INTEGER);

PROCEDURE reset; (* to initial state *)

PROCEDURE restore(status: STATUS);

PROCEDURE nop;

----------------------------------------------------------------

PROCEDURE set_raw   (ON_OFF: INTEGER);
PROCEDURE set_awp   (ON_OFF: INTEGER);
PROCEDURE set_smooth(ON_OFF: INTEGER);
PROCEDURE set_scr   (no: INTEGER);

PROCEDURE ioctl(no: INTEGER; SEQ args: SYSTEM.WORD);

PROCEDURE attach(dev_name: ARRAY OF CHAR);

END Terminal.

---------------------------  NOTES  ----------------------------
                           ---------


(* NOW WE KNOW NEXT TYPES OF TERMINALS:

    1    Friashik "Electronika-EA-15-000-013"
   92    Labtam-3000 (something less then VT100)
  100    VT-100      (I never see it! Leo 10-Nov-89)
  197    Facit-2000A (something less then VT220)
  200    VT-200
  220    VT-220


PROCEDURE erase     (how: INTEGER);  (* Очищает экран *)
PROCEDURE erase_line(how: INTEGER);  (* Очищает строку *)
(* how:
        0 - очистить от текущей позиции до конца экрана/строки
        1 - очистить от начала экрана/строки до текущей позиции
        2 - очистить целиком экран/строку
*)

PROCEDURE home;   (* Перемещает курсор в верхний левый угол экрана *)
PROCEDURE bottom; (* Перемещает курсор в верхний нижний угол экрана *)

PROCEDURE roll_down(n: INTEGER);     (* Сдвиг экрана вниз *)
PROCEDURE roll_up  (n: INTEGER);     (* Сдвиг экрана вверх *)

PROCEDURE scroll_down(n: INTEGER);     (* Сдвиг экрана вниз *)
PROCEDURE scroll_up  (n: INTEGER);     (* Сдвиг экрана вверх *)

PROCEDURE set_pos(line: INTEGER; col: INTEGER); (*Установить позицию курсора*)

PROCEDURE set_background(color: INTEGER);
(*
   FOR type=92:
     The BACKGROUND color is the color the screen is set to
     when any ERASURE operation is performed. But character
     background always BLACK!!!
*)


PROCEDURE attach(dev_name: ARRAY OF CHAR);
(* Смена терминала на терминал определенный драйвером
   с именем dev_name.
*)

                 В ДРАЙВЕРАХ ЖЕЛАТЕЛЬНО ВЫБИРАТЬ

              something выделитель в таком порядке
              (по отсутсвию возможностей)

                        REVERSE
                        COLOR
                        UNDERLINE
                        HIGH INTENSITY
                        LOW  INTENSITY
                        FONT (ITALIC, GOTIC, FRANFURT)
                        BLINKING

N O T E

     This  module  does  not  attach  driver  at  initialization.
Attaching  is  peformed  when  first  calling  of  driver occure.
Variable  -state- points to blank record before attaching. Hence,
if you want to use variable -state- before any calling to driver,
you have to call procedure -nop- to fulfil attaching.

                                             Leg, 15-Oct-90
