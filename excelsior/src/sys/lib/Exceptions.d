DEFINITION MODULE Exceptions; (* Ned 20-Apr-90. (c) KRONOS *)

PROCEDURE exceptions(VAR n: INTEGER): BOOLEAN;
PROCEDURE exception(n: INTEGER): BOOLEAN;

PROCEDURE raise(n: INTEGER);

PROCEDURE traps(VAR n: INTEGER): BOOLEAN;
PROCEDURE trap(n: INTEGER): BOOLEAN;

(**************************************************************

     Модуль работы с исключительными ситуациями.

PROCEDURE exceptions(VAR n: INTEGER): BOOLEAN;
PROCEDURE traps     (VAR n: INTEGER): BOOLEAN;
---------------------------------------------

     Процедуры   определяют   начало   ловушки  исключительных
ситуаций.  Ловушка будет снята в момент выхода из процедуры, в
которой она была поставлена или после перехвата исключительной
ситуации.  Возвращает  FALSE при постановке ловушки, TRUE, при
возникновении исключительной ситуации (при этом "n"= индиканту
ситуации).
     Процедура  exceptions  ставит ловушку на пользовательские
ситуации,   возбуждаемые  процедурой  raise.  Процедура  traps
ставит   ловушку  на  ситуации  возбуждаемые  аппаратурой  или
командой trap.

PROCEDURE exception(n: INTEGER): BOOLEAN;
PROCEDURE trap     (n: INTEGER): BOOLEAN;
---------------------------------------------

     Процедуры  ставят  ловушку  на  конкретную исключительныю
ситуацию   и   не   реагируют  на  дугие  ситуации.  Остальное
аналогично процедурам exceptions и traps.

PROCEDURE raise(n: INTEGER);
----------------------------

     Возбуждает  исключительную  ситуацию. Заметим, что именно
это  значение  вернет  процедура  exception.

ОШИБКИ:

        TRAP(54h) -- нет ловушки.

Способ использования:

PROCEDURE p;
  VAR e: INTEGER;
BEGIN
  IF exceptions(e) THEN
(*1*)
    print('exception(%d)\n',e); RETURN
  END;
-- procedure body
END p;

Если   при   выполнении   тела   процедуры   будет  возбуждена
исключительная ситуация, то выполнение тела будет прекращено и
управление попадет в точку (*1*).

**************************************************************)

END Exceptions.
