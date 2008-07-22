DEFINITION MODULE defKeyboard; (* Ned 23-Aug-89. (c) KRONOS *)

(* Модуль набор кодировок для функциональных клавиш клавиатуры *)

CONST
  lf    = 012c; (* ^J *)
  cr    = 015c; (* ^M *)
  break = 003c; (* ^C break proc(0) *)
  exit  = 005c; (* ^E *)
  back  = 010c; (* ^H *)
  tab   = 011c; (* ^I *)
  vt    = 013c; (* ^K *)
  rep   = 032c; (* ^Z *)
  nak   = 025c; (* ^U break proc(1) *)
  can   = 030c; (* ^X break proc(2) *)
  nl    = 036c; (* ^^ *)

  up    = 200c;  dw    = 201c;  right = 202c;  left  = 203c;
  pgup  = 204c;  pgdw  = 205c;  home  = 206c;  end   = 207c;
  del   = 210c;  ins   = 211c;  bcktab= 212c;  newln = 213c;

  f1    = 220c;  f2    = 221c;  f3    = 222c;  f4    = 223c;
  f5    = 224c;  f6    = 225c;  f7    = 226c;  f8    = 227c;
  f9    = 230c;  f10   = 231c;  f11   = 232c;  f12   = 233c;
  f13   = 234c;  f14   = 235c;  f15   = 236c;  center= 237c;

TYPE
  BREAK = PROCEDURE (INTEGER);

  STATE = RECORD
            type : INTEGER;
            fkeys: INTEGER;
            buf  : CHAR;
            togs : BITSET;
            ubrk : BREAK;
            freq : INTEGER;
            dur  : INTEGER;

            breakon: INTEGER;
            foreign: INTEGER;
            caps   : INTEGER;
            shift  : INTEGER;
            raw    : INTEGER;
            nums   : INTEGER;
            scan   : INTEGER;
            autorep: INTEGER;
          END;

CONST (* togs *)
  altL   = {0};
  altR   = {1};
  alt    = altL+altR;
  ctrlL  = {2};
  ctrlR  = {3};
  ctrl   = ctrlL+ctrlR;
  shiftL = {4};
  shiftR = {5};
  shift  = shiftL+shiftR;
  press  = {6};
  add    = {7}; (* arrow,pgup,pgdw,home,end,ins,del,center,+,-,*,enter
                   pressed on additional keyboard
                 *)

CONST        --- изменение состояния драйвера ---

  _info     = 00h;      -- (POINTER TO POINTER TO STATE);
  _reset    = 01h;      --
  _restore  = 02h;      -- (POINTER TO STATE);

  _ubreak   = 03h;      -- (proc: BREAK)
  _break    = 04h;      -- (ON_OFF: INTEGER)
                        -- TRUE : разрешает стандартную BREAK реакцию
                        -- FALSE: запрещает реакцию на BREAK

  _foreign  = 05h;      -- (ON_OFF: BOOLEAN)

  _bell_fd  = 06h;      -- (frequncy,duration: INTEGER)
  _bell     = 07h;      -- generate r.len bells

  _raw      = 08h;      -- (ON_OFF: BOOLEAN) - raw input mode
  _nums     = 09h;      -- (ON_OFF: BOOLEAN)
  _shift    = 0Ah;      -- (ON_OFF: BOOLEAN)
  _caps     = 0Bh;      -- (ON_OFF: BOOLEAN)
  _autorep  = 0Ch;      -- (rate:   INTEGER)
  _set_ct   = 0Dh;      -- (POINTER TO ARRAY CHAR OF CHAR) set coder table
  _get_ct   = 0Eh;      -- (POINTER TO ARRAY CHAR OF CHAR) get coder table
  _readtogs = 0Fh;      -- (POINTER TO ARRAY OF CHAR)  [0] key [1] togs

END defKeyboard.
