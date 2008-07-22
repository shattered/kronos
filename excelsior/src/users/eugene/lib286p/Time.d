DEFINITION MODULE Time; (* Leo & Ned 27-Feb-90. (c) KRONOS *)

(* Модуль  осуществляет  службу времени в системе. *)

PROCEDURE time(): INTEGER;
PROCEDURE set_time(time: INTEGER);

PROCEDURE zone(): INTEGER;
PROCEDURE set_zone(zone: INTEGER);

TYPE UNIT = (tick,microsec,milisec,sec,minute,hour);

PROCEDURE sys_time(unit: UNIT): INTEGER;

PROCEDURE eval(from: UNIT; value: INTEGER; to: UNIT): INTEGER;

PROCEDURE delay(time_value: INTEGER; time_unit: UNIT);

PROCEDURE pack  (y,m,d,ho,mn,sc: INTEGER): INTEGER;
PROCEDURE unpack(t: INTEGER; VAR y,m,d,ho,mn,sc: INTEGER);

PROCEDURE day(time: INTEGER): INTEGER;

PROCEDURE scan_date(VAR time: INTEGER; str: ARRAY OF CHAR;
                       night: BOOLEAN);

END Time.

(************************************************************

    Модуль  осуществляет  службу времени в системе.
 Здесь всюду INTEGER  выражает  время  в  секундах, начиная
 с 00:00.00 01/01/1986 года.
    Отрицательное время свидетельствует об ошибке. Последнее
 допустимое время - 23:59.59 31/12/2017 года (1986+31).

PROCEDURE time(): INTEGER;
--------------------------
Выдает (зональное) время в секундах от начального.
Время по Гринвичу вычисляется как: time()-zone()*3600

PROCEDURE set_time(time: INTEGER);
----------------------------------
Устанавливает время, for SUPRUSER only

PROCEDURE zone(): INTEGER;
--------------------------
Выдает номер временно'й зоны (-11..+12)

PROCEDURE set_zone(zone: INTEGER);
----------------------------------
Устанавливает номер временно'й зоны (-11..+12).
Изначально до первого "set_zone" zone()=0 (Grinvich).

TYPE UNIT = (tick,microsec,milisec,sec,minute,hour);

PROCEDURE sys_time(unit: UNIT): INTEGER;
----------------------------------------
Выдает время, прошедшее с момента загрузки системы.

PROCEDURE eval(from: UNIT; value: INTEGER; to: UNIT): INTEGER;
--------------------------------------------------------------
Переводит время из одних единиц в другие

PROCEDURE delay(time_value: INTEGER; time_unit: UNIT);
------------------------------------------------------
Задерживает обратившийся процесс

PROCEDURE pack(y,m,d,ho,mn,sc: INTEGER): INTEGER;
-------------------------------------------------
По  году "y", месяцу "m", дате "d", часу "ho", минутам "mn"
и секундам "sc" выдает время.

PROCEDURE unpack(t: INTEGER; VAR y,m,d,ho,mn,sc: INTEGER);
----------------------------------------------------------
По  времени  "t"  присваивает значения переменным году "y",
месяцу  "m",  дню  "d", часу "ho", минутам "mn" и секундам
"sc".

PROCEDURE day(time: INTEGER): INTEGER;
--------------------------------------
По времени выдает номер дня недели (1 Monday ... 7 Sunday)

PROCEDURE scan_date(VAR time: INTEGER; str: ARRAY OF CHAR;
-------------------    night: BOOLEAN);
str ::=   [DD/MN/[19]YY,][HH:MM[.SS]]
        | [[19]YY#DD#MM,][HH:MM[.SS]]
if date ommited if suggested TODAY
   night = FALSE: if HH:MM.SS ommited if suggested 00:00.00
   night = TRUE:  if HH:MM.SS ommited if suggested 23:59.59

   scan_date(t,"01/01/86,00:00.00",FALSE) => t=00000000h

*************************************************************)
