DEFINITION MODULE abcTT; (* Hady. 18-Feb-89. (c) KRONOS *)

(* Экран для словаря *)

IMPORT  voc: abcVoc;

CONST vt220 = TRUE; -- TRUE, если словарь компилируется для работы с vt220;

CONST    screen  = 24;                     -- strings in whole screen;
      work_line  = 0;
      info_line  = screen - 1;
       cmd_line  = work_line+1;
       scr_start = work_line+3;
       scr_final = screen - scr_start - 1;

----------------------INFO LINE SUPPORT--------------------
                      -----------------
(*
0         1         2         3         4         5         6         7
01234567890123456789012345678901234567890123456789012345678901234567890123456789
<<<                 BUFF 000  INS off   BELL off  FILE      abracarba       >>>
*)

VAL bell?,
    info?: BOOLEAN;

PROCEDURE  ins(on_off: BOOLEAN);
PROCEDURE bell(on_off: BOOLEAN);        PROCEDURE bell!; -- bells if possible;
PROCEDURE info(on_off: BOOLEAN);

PROCEDURE file(name: ARRAY OF CHAR);

PROCEDURE state(s: ARRAY OF CHAR);

VAL buff_depth: INTEGER;
PROCEDURE buff(i: INTEGER);

PROCEDURE ref_info; -- refresh info in screen if info?=TRUE;

PROCEDURE message(s: ARRAY OF CHAR);

----------------------------SCREEN-------------------------
                            ------

PROCEDURE refresh;

PROCEDURE show (w: voc.Word); -- with    rolling
PROCEDURE write(w: voc.Word); -- without rolling

PROCEDURE get_word(VAR w: voc.Word);

VAL cur_str: INTEGER;

PROCEDURE mark(c: CHAR);

PROCEDURE mark_up;              PROCEDURE mark_dw;

PROCEDURE move_left;            PROCEDURE move_right;

TYPE put_proc = PROCEDURE (ARRAY OF CHAR);
PROCEDURE write_out(put: put_proc); -- writes marked line w.h. "put"

PROCEDURE save(put: put_proc); -- saves whole screen w.h. "put"

TYPE get_proc = PROCEDURE (VAR ARRAY OF CHAR);
PROCEDURE restore(get: get_proc);

PROCEDURE init_scr;

END abcTT.
