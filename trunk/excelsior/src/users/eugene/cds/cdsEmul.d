DEFINITION MODULE cdsEmul; (* 21-Jul-86 (c)KRONOS *)

-- Для корректной работы модуля необходимо:
--   сигналы питания должны называться GND и VCC
--   свободные ножки микросхем должны быть подключены к сигналу ..free..
--

FROM Model IMPORT Object;

VAR TMps : INTEGER;
    TMms : INTEGER;
-- Моделируемое время = TMms*1000_000_000+TMps пикосекунд.

    Stop : BOOLEAN;
    Value: INTEGER;

CONST Undef=BOOLEAN(2);

TYPE JobPROC=PROCEDURE (INTEGER,BOOLEAN);

PROCEDURE CalculateState(model: Object);

PROCEDURE Emulate(model: Object);

PROCEDURE NewIvent(pinno,time: INTEGER; state: BOOLEAN);

PROCEDURE State?(pinno: INTEGER): BOOLEAN;

PROCEDURE SetPin(no: INTEGER; state: BOOLEAN);

END cdsEmul.
