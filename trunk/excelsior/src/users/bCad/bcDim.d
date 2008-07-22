DEFINITION MODULE bcDim; (* brd 31-Jan-91 *)

IMPORT   bcText;
IMPORT   bcBase;
(*
  ctrl-d [004c]  знак диаметра
  ctrl-z [032c]  +/-
  ctrl-g [007c]  градус
  ctrl-r [022c]  значок шероховатости
 *)

VAL  h_tex: REAL;         (* высота размерного текста                       *)
     w_tex: REAL;         (* ширина букв текста                             *)
     i_tex: REAL;         (* наклон букв                                    *)
     H_tex: REAL;         (* положение текста над размерной линией          *)
     arr_s: REAL;         (* размер стрелочек                               *)
     col_pc: bcBase.COLOR;
     col_ch: bcBase.COLOR;
     dfont: bcText.PFONT; (* current font for dimesion text                 *)

PROCEDURE horizontal;
PROCEDURE vertical;
PROCEDURE radius;
PROCEDURE diametr;
PROCEDURE angle;
PROCEDURE other;

END bcDim.
