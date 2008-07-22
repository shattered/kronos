DEFINITION MODULE Threads; (* Ned 20-Oct-89. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  Signals;

TYPE THREAD;

VAL null: THREAD;        -- NIL for THREADs

--------------------------  THREADs  ---------------------------
                          -----------

TYPE Forkee = PROCEDURE (SEQ SYSTEM.WORD);

PROCEDURE fork(VAR thread: THREAD;
                     proc: Forkee;
                work_size: INTEGER;
                 VAR done: BOOLEAN;
                 SEQ args: SYSTEM.WORD
                     );

PROCEDURE xfork(VAR thread: THREAD;
                      proc: Forkee;
                 work_size: INTEGER;
                      halt: Signals.SIGNAL;
                  VAR done: BOOLEAN;
                  SEQ args: SYSTEM.WORD
                      );

PROCEDURE rem_thread(VAR thread: THREAD; VAR done: BOOLEAN);


---------------------------------------------------------------

PROCEDURE abort(thread: THREAD; how: INTEGER);

PROCEDURE status(thread: THREAD; VAR n: INTEGER);
CONST
  invalid   = 0;
  ready     = 1;
  blocked   = 2;
  suspended = 3;
  aborted   = 4;

PROCEDURE history(thread: THREAD; VAR packed: ARRAY OF CHAR);

PROCEDURE cause(thread: THREAD; VAR n: INTEGER);

----------------------------------------------------------------

PROCEDURE self(): THREAD;

PROCEDURE get_prio(thread: THREAD; VAR prio: INTEGER);
PROCEDURE set_prio(thread: THREAD;     prio: INTEGER);

---------------------------  DELAYs  ---------------------------
                           ----------

PROCEDURE delay(milisec: INTEGER);

PROCEDURE suspend(thread: THREAD; milisec: INTEGER);
PROCEDURE resume (thread: THREAD);

(**************************************************************


PROCEDURE fork(
---------------
               VAR thread: THREAD;
                     proc: Forkee;
                work_size: INTEGER;
                 VAR done: BOOLEAN;
                 SEQ args: SYSTEM.WORD
                     );

     Создает  и  запускает  новый  процесс с параметрами args.
done=FALSE, если не хватило памяти под процесс. При завершении
процесса  будет  выполнена  стандартная  процедура  завершения
задачи.


PROCEDURE xfork(VAR thread: THREAD;
                      proc: Forkee;
                 work_size: INTEGER;
                      halt: Signals.SIGNAL;
                  VAR done: BOOLEAN;
                  SEQ args: SYSTEM.WORD
                      );

     Создает  и  запускает  новый  процесс с параметрами args.
done=FALSE, если не хватило памяти под процесс. При завершении
процесса  будет послан сигнал halt.

PROCEDURE rem_thread(VAR thread: THREAD; VAR done: BOOLEAN);
------------------------------------------------------------

     Освобождения памяти, занимаемой процессом.


PROCEDURE abort(thread: THREAD; how: INTEGER);
----------------------------------------------

     Останавливает процесс(ы).
how:
     0 - будет завершен только данный процесс;
     1 - будет завершен данный процесс и его потомки;

PROCEDURE status(thread: THREAD; VAR n: INTEGER);
-------------------------------------------------

Выдает состояние процесса:

  invalid   -- что-то не в порядке;
  ready     -- активный или готовый к исполнению процесс;
  blocked   -- процессс ждет сигнала или блокирован на воротах;
  suspended -- задержанный процесс;
  aborted   -- завершенный процесс.

PROCEDURE history(thread: THREAD; VAR packed: ARRAY OF CHAR);
-------------------------------------------------------------

     Выдает историю завершенного процесса.

PROCEDURE cause(thread: THREAD; VAR n: INTEGER);
------------------------------------------------

     Выдает причину завершения процесса (код ошибки).

PROCEDURE self(): THREAD;
-------------------------

     Текущий процесс.

PROCEDURE get_prio(thread: THREAD; VAR prio: INTEGER);
PROCEDURE set_prio(thread: THREAD;     prio: INTEGER);
------------------------------------------------------

     Установка и определение приоритета процесса. Игнорируются
в текущей реализации.

PROCEDURE delay(milisec: INTEGER);
----------------------------------

     Приостановка текущего процесса на время. Пустое действие,
если время задержки <=0.

PROCEDURE suspend(thread: THREAD; milisec: INTEGER);
----------------------------------------------------

     Задерживает  процесс  на  указанное  время,  а если время
задержки  <0,  то  процесс  может  быть продолжен только явным
вызовом процедуры resume.

PROCEDURE resume(thread: THREAD);
----------------------------------

     Восстановление  задержанного  процесса.  Пустое действие,
если процесс не задержан.

**************************************************************)

END Threads.
