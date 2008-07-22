DEFINITION MODULE DWS; (* Sem 15-Feb-87. (c) KRONOS *)

(*********************************************************************

   Для корректной работы модуля необходима истинность неравенства:
              (Clearens+Size)*SQRT(2) > GridSize

 *********************************************************************)

FROM SYSTEM    IMPORT   ADDRESS;
FROM Model     IMPORT   Object;
FROM pedPbl    IMPORT   LayersNo;
FROM mCodeMnem IMPORT   lxw, lxb, add, ssw0, lodt, stot, sxb, shr;

TYPE IterProc=PROCEDURE (CARDINAL,CARDINAL,CARDINAL);
     Matr; Pws;
     Dws = ARRAY [0..LayersNo*2+1] OF Matr;

VAR Buf: Dws;
    Ptr: Pws;
    Xsize,Ysize: INTEGER;

CONST
  swap=0F0h;

PROCEDURE WS(x,y,l: INTEGER; VAR Buf: Dws): INTEGER;
CODE lxw lxw swap lxb END WS;

PROCEDURE setWS(x,y,l: INTEGER; VAR Buf: Dws; v: INTEGER);
CODE stot lxw lxw swap lodt sxb END setWS;

PROCEDURE PT(x,y: INTEGER; Ptr: Pws): ADDRESS;
CODE swap 1 shr lxw swap 1 shr lxw END PT;

PROCEDURE setPT(x,y: INTEGER; Ptr: Pws; v: ADDRESS);
CODE stot swap 1 shr lxw swap 1 shr add lodt ssw0 END setPT;

PROCEDURE adrPT(x,y: INTEGER; Ptr: Pws): ADDRESS;
CODE swap 1 shr lxw swap 1 shr add END adrPT;

PROCEDURE Busy(x,y,l: INTEGER; VAR Buf: Dws): BOOLEAN;
CODE LayersNo add lxw lxw swap lxb END Busy;

PROCEDURE Vias(x,y: INTEGER; VAR Buf: Dws): BOOLEAN;
CODE LayersNo*2 lxw lxw swap lxb END Vias;

PROCEDURE Pipe(x,y: INTEGER; Buf: Dws): BITSET;
CODE LayersNo*2+1 lxw lxw swap lxb END Pipe;


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

PROCEDURE InsertSignal(signal: Object);
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
