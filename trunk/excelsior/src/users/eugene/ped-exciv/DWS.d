DEFINITION MODULE DWS; (* Sem 15-Feb-87. (c) KRONOS *)

(*********************************************************************

   Для корректной работы модуля необходима истинность неравенства:
              (Clearens+Size)*SQRT(2) > GridSize

 *********************************************************************)

FROM SYSTEM    IMPORT   ADDRESS;
FROM Model     IMPORT   Object;

TYPE IterProc=PROCEDURE (INTEGER,INTEGER,INTEGER);

VAR Xsize,Ysize: INTEGER;

PROCEDURE WS(x,y,l: INTEGER): INTEGER;
PROCEDURE setWS(x,y,l: INTEGER; v: INTEGER);

PROCEDURE PT(x,y: INTEGER): ADDRESS;
PROCEDURE setPT(x,y: INTEGER; v: ADDRESS);
PROCEDURE adrPT(x,y: INTEGER): ADDRESS;

PROCEDURE Busy(x,y,l: INTEGER): BOOLEAN;

PROCEDURE Vias(x,y: INTEGER): BOOLEAN;

PROCEDURE Pipe(x,y: INTEGER): BITSET;

-----------------------------------------------------------------------

PROCEDURE InitDWS(o: Object; Tool: INTEGER);
-- Создает и инициализирует DWS в динамической памяти.
-- Во время существования DWS в модели не должно изменятся
-- множество неподвижных сегментов!

PROCEDURE RemoveDWS;
-- Удаляет DWS из динамической памяти.

CONST

-- Семантика хранимой в DWS информации:
-- Busy
--   значение TRUE означает что через ячеку невозможно провести
--   метализацию заданным инструментом.
-- Vias
--   значение TRUE означает что в ячеке невозможно разместить
--   площадку под отверстие
-- WS и PT
--   предназначены для хранения волны,
--   используются в трассировщике.
--   WS может принимать следующие значения:
       free   =  0; start  =  1; front  =  2; finish =  3;
       mE     =  5; mNE    =  6; mN     =  7; mNW    =  8;
       mW     =  9; mSW    = 10; mS     = 11; mSE    = 12;
       mLdown = 13; mLup   = 14;
-- Pipe
--   содержит следующие булевские атрибуты ячеек монтажного пространства:
  vias     =       0;  -- наличие метализированного отверстия
  fixedvias=       1;  -- TRUE в Vias вызвано неподвижной площадкой
  fixed    =       2;  -- TRUE в Busy (слой 0) вызвано неподвижной метализации
  fixed1   = fixed+1;  -- TRUE в Busy (слой 1) вызвано неподвижной метализации

PROCEDURE InsertSignal(signal: Object; info: INTEGER);
-- Помещает в Busy, Vias и Pipe информацию о сигнале signal.

PROCEDURE UnPackSignal(signal: Object; p: IterProc);
-- Удаляет из Busy и Vias информацию о сигнале signal,
-- затем для каждой ячейки, содержащей метализацию этого сигнала,
-- вызывается процедура p.
-- Для каждой ячейки процедура вызываеся ровно один раз.

PROCEDURE PackSignal;
-- Помещает в Busy, Vias и Pipe информацию о сигнале,
-- указанном в последнем вызове UnPackSignal.
-- Информация в Pipe корректируеся в соответствии с новым состоянием сигнала.
-- Во все ячейки WS заносятся нули.
-- Во все ячейки PT заносится NIL.

END DWS.
