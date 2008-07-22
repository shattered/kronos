DEFINITION MODULE defRequest; (* Ned 19-Sep-89. (c) KRONOS *)

(* Определяет стандарт запроса к драйверу. *)

IMPORT  sys: SYSTEM;

CONST (* dmode *)
  ready   = {0};
  floppy  = {1};
  wint    = {2};
  fmtsec  = {3};
  fmttrk  = {4};
  fmtunit = {5};
  wpro    = {6};

(*sync    = { 8};*)

CONST (* smode *)
  raw        = { 0};
  parNO      = { 1};
  parODD     = { 2};
  parEVEN    = { 3};
  stops0     = { 4};
  stops1     = { 5};
  stops1_5   = { 6};
  stops2     = { 7};
  sync       = { 8};

TYPE
  TRANSLATE = POINTER TO ARRAY CHAR OF CHAR;

  REQUEST = RECORD (* 28 bytes *)
              op : INTEGER;     -- код операции
              drn: INTEGER;     -- номер под-устройства
              res: INTEGER;     -- результат
              CASE :INTEGER OF
              |0: ofs    : INTEGER;     -- адрес на устройстве
                  buf    : sys.ADDRESS; -- адрес буфера в памяти
                  pos    : INTEGER;     -- смещение в буфере
                  len    : INTEGER;     -- длина

              |1: (* for GET_SPEC, SET_SPEC only *)
                  dmode  : BITSET;
                  dsecs  : INTEGER; (* device size in secs *)
                  ssc    : INTEGER; (* sector size code    *)
                  secsize: INTEGER; (* sector len 2**ssc   *)
                  cyls   : INTEGER; (* # cylinders         *)
                  heads  : INTEGER; (* # heads             *)
                  minsec : INTEGER; (* min sector # on trk *)
                  maxsec : INTEGER; (* max sector # on trk *)
                  ressec : INTEGER; (* reserved sectors    *)
                  precomp: INTEGER; (* precompensation     *)
                  rate   : INTEGER; (* heads stepping      *)

              |2: (* for GET_SPEC, SET_SPEC only *)
                  smode  : BITSET;
                  baud   : INTEGER; (* baud rate code      *)
                  xon    : CHAR;    (* XON/XOFF            *)
                  xoff   : CHAR;    (* protocol            *)
                  limxon : INTEGER; (* send xon  limit     *)
                  limxoff: INTEGER; (* send xoff limit     *)
                  trtin  : TRANSLATE;
                  trtout : TRANSLATE;
              END
            END;

CONST  -- operations (* L.S.B. (Low Significant Byte *)

  NOP        =  0;

  LOCK       =  1; (* LOCK   not used yet *)
  UNLOCK     =  2; (* UNLOCK not used yet *)

  READ       =  3; (* чтение данных с устройства            *)
  WRITE      =  4; (* запись данных на устройство           *)

  WAIT       =  5; (* ожидание данных                       *)
  READY      =  6; (* запрос о числе байтов в буфере ввода  *)
  CONTROL    =  7; (* управляющая операция для клавиатур,   *)
                   (* экранов и т.д. Старшие байты содержат *)
                   (* код операции                          *)

  GET_SPEC   =  8; (* запрос спецификации устройства        *)
  SET_SPEC   =  9; (* определение спецификации устройства   *)
  POWER_OFF  = 10; (* завершение работы                     *)
  FORMAT     = 11; (* форматирование устройства             *)
  SEEK       = 12; (* позиционирование                      *)
  MOUNT      = 13; (* монтирование носителя                 *)
  UNMOUNT    = 14; (* размонтирование носителя              *)


(***************************************************************

-------------------------  ПРИМЕЧАНИЕ  -------------------------
                         --------------

     Следущее  описание  определяет  параметры каждой операции и
поля  через  которые  эти  параметры  передаются.  Все  операции
возвращают результат операции в поле res. Для всех операций поле
drn содержит номер (под-)устройства.

READ, WRITE (disk):
-----------------
        IN
            drn - номер (под-)устройства
            buf - буфер в/в
            ofs - номер сектора на диске
            len - число секторов
        OUT
            res - результат операции

READ, WRITE (serial):
------------------------------
        IN
            drn - номер (под-)устройства
            buf - буфер в/в
            pos - смещение в буфере
            len - число байтов
        OUT
            len - число непрочитанных байтов
            res - результат операции

WAIT (serial):
------------
        IN
            drn - номер (под-)устройства
        OUT
            res - результат операции


READY (input serial):
--------------------
        IN
            drn - номер (под-)устройства
        OUT
            len - число байтов в буфере ввода
            res - результат операции

CONTROL:
-------
        IN
            op  - старшие байты содержат код
                  управляющей операциии
            drn - номер (под-)устройства
        OUT
            res - результат операции

FORMAT:
------
   driver must say to the system (in GET_SPEC) operation
   what kind of FORMAT capability it supports:
      fmtsec
      fmttrk
      fmtunit
   exclusively.

        OUT res - результат операции
            len - size of buffer (for fmttrk see later)

   system calls driver with "ofs"=-1 "buf"=NIL for prepearing format.
   if driver needs buffer it must return "not_enough" result
   and set "len" to the neccessary size,
   after that system will allocate buffer and
   retry operation with "ofs"=-1, "buf"=allocated_buffer "len"=it lenght
   -----
   If driver returns "ok" when ofs=-1 then buffer allocation
   NOT performed.

   for "fmtunit" drives:
        IN  drn - номер (под-)устройства
            ofs = 0

   for "fmttrk" drives:
        IN  drn - номер (под-)устройства
            buf - буфер в/в
            ofs - смещение на устройстве (sectors)
            len - size of buffer (bytes)

   for "fmtsec" drives: NOT SUPPORTED BY SYSTEM YET

GET_SPEC
SET_SPEC
POWER_OFF

SEEK
MOUNT
UNMOUNT

***************************************************************)

END defRequest.
