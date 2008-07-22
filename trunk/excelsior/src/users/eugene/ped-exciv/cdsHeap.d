DEFINITION MODULE cdsHeap;                 (* Andy 08-Aug-89. (c) KRONOS *)

                IMPORT  sys: SYSTEM;

-- Процедуры, возбуждающие TRAP 4Eh при нехватке памяти:

PROCEDURE   ALLOCATE(VAR a: sys.ADDRESS; size: INTEGER);
PROCEDURE DEALLOCATE(VAR a: sys.ADDRESS; size: INTEGER);
PROCEDURE REALLOCATE(VAR a: sys.ADDRESS; size,new_size: INTEGER);

-- Процедуры, при нехватке памяти возвращающие NIL:

PROCEDURE   Allocate(VAR a: sys.ADDRESS; size: INTEGER);
PROCEDURE Deallocate(VAR a: sys.ADDRESS; size: INTEGER);

PROCEDURE Reallocate(VAR a: sys.ADDRESS; size,new_size: INTEGER): BOOLEAN;
-- Возвращается TRUE, если операция удалась;
-- иначе возвращается FALSE и адрес -a- не изменяется.

-- Запрос на 0 слов считается корректным - возвращается NIL;
-- соответственно можно вернуть куче кусок памяти
-- с адресом начала NIL и длиной 0 слов.

PROCEDURE set_limit(total: INTEGER);
-- Установление верхнего предела на общий объем кучи (в словах).
-- При total<0 верхний предел отменяется.

PROCEDURE mem_free(with_os: BOOLEAN):INTEGER;

PROCEDURE set_os_request_size(words: INTEGER);

END cdsHeap.
