DEFINITION MODULE defBMG; (* nick 01-Jun-90. (c) KRONOS *)

IMPORT  SYSTEM;

(********************************************************)
(* This modle define two base types:                    *)
(*      BITMAP - pointer to bitmap descriptor and       *)
(*      BMD    - bitmap descriptor                      *)
(*                                                      *)
(* In this version of module the maximum number of      *)
(* layers is 8                                          *)
(********************************************************)

TYPE

  BITMAP = POINTER TO BMD;

  BMD    = RECORD
             W,H    : INTEGER;
             WPL    : INTEGER;
             BASE   : SYSTEM.ADDRESS;
             PATTERN: BITSET;
             mask   : BITSET;
             layers : ARRAY [0..7] OF SYSTEM.ADDRESS; -- base addrs of layers
           END;

(* W,H - bitmap width and height, WPL - words per line    *)
(* layers - array of pointer to base adresses of layers   *)
(*                                                        *)
(* NOTE:                                                  *)
(*                                                        *)
(* BASE and PATTERN - reserved for graphic microprogramms *)
(* rfe0,rfe1,rfe2   - reserved for future extetions       *)
(* if layers[i]=NIL then llayer number i is not exist     *)

END defBMG.

================================================================

     В  модуле  defBMD определяются два базовых типа - стадартное
для всех библиотек нижнего уровня описание битовых карт.

     В  записи BMD - дескриптора битовой карты обзначены поля W и
H   определяют   ее   горизонтальный   и   вертикальный  размеры,
соответственно, WPL - количество количество слов памяти для одной
битовой  строки,  а  массив layers содержит в себе базовые адреса
битовых  слоев,  PATTERN - шаблон для рисования линий. Поля W, H,
WPL,   BASE   и   PATTERN   используются  графическими  командами
процессора. Поля rfe0,rfe1 и rfe0 зарезервированы для последующих
расширений.

     Тип  BITMAP  удобен  для низкоуровневых прикладных библиотек
графики.
