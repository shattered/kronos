DEFINITION MODULE intRAM; (* 02-Jun-88. (c) KRONOS *)

FROM SYSTEM     IMPORT  WORD, ADDRESS;

VAR FailPH : BOOLEAN; -- отсутствует физический адрес страницы
    FailWR : BOOLEAN; -- последняя операция была запись
    FailAdr: INTEGER; -- логический адрес для последней операции
    FailDat: INTEGER; -- данные для последней операции

PROCEDURE CoreRD(Adr: INTEGER): WORD;

PROCEDURE CoreRDph(Adr: INTEGER): WORD;

PROCEDURE CoreWR(Adr: INTEGER; Val: WORD);

PROCEDURE CoreWRph(Adr: INTEGER; Val: WORD);

PROCEDURE CheckPage(Adr: INTEGER);

PROCEDURE CheckWrPage(Adr: INTEGER);

PROCEDURE SetAdr(Adr: INTEGER; ph: INTEGER; wp: BOOLEAN);

PROCEDURE ClearAdr(Adr: INTEGER; t: INTEGER);

PROCEDURE SetTag(t: INTEGER);

PROCEDURE Tag?(): INTEGER;

PROCEDURE GetCode(f: INTEGER): ADDRESS;

-- инициализация памяти:
-- адрес   содержимое
--  0h       400h  текущая таблица сегментов
--  1h       400h  таблица сегментов, используемая при прерываниях
--  2h         4h  адрес списка недостающих страниц
--  3h         0h  длина списка недостающих страниц
--400h       81Fh  инициальная таблица сегментов
--800h..81Fh       инициальная таблица страниц

END intRAM.
