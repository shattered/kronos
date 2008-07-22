DEFINITION MODULE defFont;  (* nick 07-May-90. (c) KRONOS *)
                            (* Leo  28-Jan-91. (c) KRONOS *)
IMPORT  SYSTEM;

TYPE
  FONT  = POINTER TO FNTD;
  BTAB  = ARRAY CHAR OF CHAR;
  ITAB  = ARRAY CHAR OF INTEGER;
  BTPTR = POINTER TO BTAB;
  ITPTR = POINTER TO ITAB;

  FNTD = RECORD
           W,H  : INTEGER;
           BASE : SYSTEM.ADDRESS;
           rfe  : SYSTEM.WORD;
           magic: INTEGER;
           state: BITSET;
           size : INTEGER;
           bline: INTEGER;
           uline: INTEGER;
           space: INTEGER;
           fchar: CHAR;
           lchar: CHAR;
           propW: BTPTR;  (* #NIL only of state*prop#{} *)
           propX: BTPTR;  (* #NIL only of state*prop#{} *)
           cellW: BTPTR;
           cellH: BTPTR;
           cellY: BTPTR;
           cellX: BTPTR;  (* allways zero now *)
           bases: ITPTR;
         END;

CONST
   prop   = {0};
   italic = {1};
   packed = {2};

END defFont.

(****************************************************************

  FNTD =
    RECORD
      W,H  : INTEGER;          максимальный размер знакоместа
      BASE : SYSTEM.ADDRESS;   указатель на массив шаблонов шрифта
      rfe  : SYSTEM.WORD;      reserved for future extention
      magic: INTEGER;          "волшебное слово"
      state: BITSET;           статус фонта (биты 00..15 reserved by System
                                                  16..31 for user applications
      size : INTEGER;          размер массива шаблонов шрифта (в словах)
      fchar: CHAR;             первый    символ в массиве шаблонов
      lchar: CHAR;             последний символ в массиве шаблонов
      bline: INTEGER;          положение базовой линии относительно
                               нижнего края максимального знакоместа
      uline: INTEGER;          положение линии подчеркивания относительно
                               нижнего края максимального знакоместа
      space: INTEGER;          ширина пробела для prop шрифтов
      propW: BTPTR;            указатель на массив смещений для
                               пропорционального шрифта
  *** ТОЛЬКО ДЛЯ PACKED шрифтов ***
      cellW: BTPTR;            ширины знакомест
      cellH: BTPTR;            высоты знакомест
      charY: BTPTR;            смещения знакомест по вертикали относительно
                               нижнего края максимального знакоместа
      bases: ITPTR;            битовые смещения до шаблона символа,
                               (-1  если символа нет)
    END;

    +--------------------------------------------+
    |              |propW cellW cellH charY bases|
    +--------------------------------------------+
    |       prop   |       NIL   NIL   NIL   NIL |
    |              | NIL   NIL   NIL   NIL   NIL |
    |packed prop   |                             |
    |packed        | NIL                         |
    +--------------------------------------------+

****************************************************************)
