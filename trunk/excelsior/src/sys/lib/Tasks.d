DEFINITION MODULE Tasks; (* Ned 19-Nov-89. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  def: defTasks;
IMPORT  syn: Signals;

TYPE TASK;

VAL done: BOOLEAN;
   error: INTEGER;
    note: ARRAY [0..79] OF CHAR;

(* "note" setted when error in "create","chpaths" only *)

VAL
  null: TASK;
  self: TASK;
 task0: TASK;

PROCEDURE chpaths;

PROCEDURE create(VAR task: TASK;
                     papa: TASK;
                     name: ARRAY OF CHAR;
                    alias: ARRAY OF CHAR;
                    stack: INTEGER;
                     parm: ARRAY OF CHAR;
                    );

PROCEDURE run(task: TASK);

PROCEDURE load_codes(task: TASK;
                     name: ARRAY OF CHAR;
                    alias: ARRAY OF CHAR);

PROCEDURE open (VAR task: TASK; papa: TASK; id: INTEGER);
PROCEDURE close(VAR task: TASK);

PROCEDURE caller(VAR id: INTEGER);

----------------------------------------------------------------

CONST
  stop = def.stop;
  kill = def.kill;
  ipr  = def.ipr;

PROCEDURE signal(T: TASK; no: INTEGER);

PROCEDURE  get_signal(VAR s: syn.SIGNAL; T: TASK; no: INTEGER);
PROCEDURE free_signal(VAR s: syn.SIGNAL; T: TASK; no: INTEGER);

----------------------------------------------------------------

PROCEDURE son    (task: TASK; VAR id: INTEGER);
PROCEDURE brother(task: TASK; VAR id: INTEGER);
PROCEDURE papa   (task: TASK; VAR id: INTEGER);

PROCEDURE get_attr(T: TASK; no: INTEGER; VAR val: SYSTEM.WORD);
PROCEDURE set_attr(T: TASK; no: INTEGER;     val: SYSTEM.WORD);

CONST a_status = 0;     -- task status  (read only)
      a_mem    = 1;     -- task memory  (read only)
      a_user   = 2;     -- user         (read/write)
      a_id     = 3;     -- task ident   (read only)
      a_ipr    = 4;     -- independent? (read/write)
      a_res    = 5;     -- task result  (read only)

PROCEDURE history(task: TASK; VAR cause: INTEGER;
                              VAR his  : ARRAY OF CHAR);

PROCEDURE lookup_module(task: TASK; name: ARRAY OF CHAR);

PROCEDURE find(task: TASK; name: ARRAY OF CHAR);
(* try lookup_module otherwise try find through BIN paths *)


------------------------  ENVIRONMENT  -------------------------
                        ---------------

PROCEDURE put_env(task: TASK;
                  name: ARRAY OF CHAR;
                  data: ARRAY OF SYSTEM.WORD;
                  priv: BOOLEAN;
                  );

PROCEDURE put_str(task: TASK;
                  name: ARRAY OF CHAR;
                  data: ARRAY OF CHAR;
                  priv: BOOLEAN
                  );

PROCEDURE get_str(task: TASK;
                  name: ARRAY OF CHAR;
              VAR data: STRING);

PROCEDURE get_env(task: TASK;
                  name: ARRAY OF CHAR;
               milisec: INTEGER;
              VAR data: STRING;
              VAR priv: BOOLEAN);

PROCEDURE env_entry(task: TASK;
                   entry: INTEGER;
                 milisec: INTEGER;
                VAR name: STRING;
                VAR data: STRING;
                VAR priv: BOOLEAN);

PROCEDURE del_env(task: TASK; name: ARRAY OF CHAR);

----------------------------------------------------------------

PROCEDURE xole(task: TASK; VAR x: SYSTEM.WORD);

(***************************************************************

--------------------------  COMMENTs  -------------------------
                          ------------

     Модуль  реализует  операции  создания,  запуска и общения с
задачами, работу с окружением задачи.

     Индикант  задачи  можно  получить  двумя способами: создать
задачу  и  открыть  созданную  задачу.  Все  остальные  операции
работают  с индикатном открытой задачи. Каждая задача имеет свой
номер, который и является уникальным именем задачи.

     Общение  с  задачей  происходит  с помощью сигналов. Сигнал
идентифицируется  номером сигнала. Задаче можно послать сигнал с
данным  номером и у задачи можно получить сигнал, которого можно
потом  ждать.  На  уровне  ядра  системы  фиксируются два номера
сигналов. Это сигналы stop и kill.

     Дескриптор  задачи существует пока задача не уничтожена или
задача   открыта.  При  создании  задачи  она  открывается.  Все
открытые  задачи  будут  автоматически  закрыты  при  завершении
открывшей задачи.

---------------------------------------------------------------

PROCEDURE create(
-----------------
                 VAR task: TASK;
                     papa: TASK;
                     name: ARRAY OF CHAR;
                    alias: ARRAY OF CHAR);

     Создает  новую  задачу на базе задачи papa. Если papa=null,
то создается независимая задача. Создание на базе означает что:

        - при загрузке задачи кодофайлы будут искаться
          по списку (ветви дерева) прямых предков;
        - если кодофайл, найденный у какого-либо предка
          уникален, то задача будет разделять (т.е. совместно
          использовать) глобалы этого модуля;
        - при уничтожении задачи, все задачи созданные на
          ее базе (ее потомки) будут уничтожены.

     Все   задачи   используют  коды  ядра  системы  (и  глобалы
уникальных модулей), а независимые задачи (papa=null) используют
только их.

     Параметр name определяет имя головного модуля задачи, alias
- определяет задание для загрузчика (разделитель - пробел).

alias:: { выключение | уникализация | переопределение }

выключение     :: "-"имя_модуля
уникализация   :: "+"имя_модуля
переопределение:: "="имя_модуля имя_файла

     Выключение:  даже  если  код  модуля  с  таким именем будет
найден  среди  предков  задачи  или  в  ядре  системы,  он будет
прочитан из файла. ОШИБКА: busy, если модуль уникален.

     Уникализация:  модуль  с этим именем становиться уникальным
для  данной  задачи  (не оказывает влияния на другие параллельно
создаваемые  задачи).  ОШИБКА: no_entry, если модуль не найден у
предков.

     Переопределение:  код  модуля с таким именем будет прочитан
из  файла  с  именем  имя_файла.  Подразумевает выключение этого
модуля. ОШИБКА: busy, если модуль уникален.

     Действия "+","-" и "+","=" для одного модуля несовместимы.
Запрещается также повторное переопределение. ОШИБКА: duplicate.

     При  загрузке  файла  с  кодом  в  нем может быть несколько
кодофайлов.  При  этом  у всех кодофайлов должно быть выставлено
поле  size  (defCode.code_ptr ^.size) равное размеру кодофайла в
словах.   Все   эти  кодофайлы,  в  том  числе  те,  которые  не
используются  в задаче, будут находиться в памяти до уничтожения
задачи.

     В   случае   ошибки   переменная  note  содержит  некоторую
дополнительную информацию об ошибке.

ОШИБКИ:
        файловые ошибки при чтении кодофайла;
        no_memory
        inv_vers, inconsistency - некоректность кодофайлов;



PROCEDURE run(
--------------
              task: TASK;
        stack_size: INTEGER;
              parm: ARRAY OF CHAR;
               ipr: BOOLEAN);

     Запусткает задачу на стеке размером stack_size, причем этот
размер задает прибавку к вычисленному компилятором и загрузчиком
минимальному   размеру   стека.  Копирует  окружение  задачи  из
окружения запускающей задачи (может не совпадать с папой).


PROCEDURE open(VAR task: TASK; papa: TASK; id: INTEGER);
-------------------------------------------------------

     Открывает  задачу  по  ее  номеру. Если papa=null, то поиск
задачи  будет выполняться по всему дереву задач, иначе только по
потомкам задачи papa.


PROCEDURE close(VAR task: TASK);
--------------------------------

     Закрывает задачу. ОШИБКА: busy, если не все сигналы
задачи освобождены (см. free_signal).

----------------------------------------------------------------

PROCEDURE signal(T: TASK; no: INTEGER);
---------------------------------------

     Посылает задаче сигнал с номером no.

PROCEDURE get_signal(VAR s: syn.SIGNAL; T: TASK; no: INTEGER);
--------------------------------------------------------------

     Выдает сигнал задачи с номером no. Этот сигнал можно только
ждать.  Сигнал  может  быть  послан  задачей  при  изменении  ее
состояния. После использования сигнал должен быть освобожден.

PROCEDURE free_signal(VAR s: syn.SIGNAL; T: TASK; no: INTEGER);

     Освобождает взятый сигнал.

PROCEDURE install(T: TASK; no: INTEGER; proc: PROC);

     Определяет  реакию  на сигнал. Процедура proc будет вызвана
при получении задачей сигнала с номером no.

----------------------------------------------------------------

PROCEDURE papa   (task: TASK; VAR id: INTEGER);
PROCEDURE son    (task: TASK; VAR id: INTEGER);
PROCEDURE brother(task: TASK; VAR id: INTEGER);
-----------------------------------------------

     Возвращает   номер   задачи-папы/сына/брата,   если  таковой
известен, иначе id=-1.

PROCEDURE get_attr(T: TASK; no: INTEGER; VAR val: SYSTEM.WORD);
---------------------------------------------------------------

     Возвращает атрибут задачи.

PROCEDURE set_user(T: TASK; user: INTEGER);
-------------------------------------------

     Устанавливает  пользователя  -  хозяина  задачи. Только для
суперюзера!

PROCEDURE history(task: TASK; VAR his: ARRAY OF CHAR);
------------------------------------------------------

     История головного процесса задачи.

------------------------  ENVIRONMENT  -------------------------
                        ---------------

PROCEDURE put_env(task: TASK;
                  name: ARRAY OF CHAR;
                  data: ARRAY OF SYSTEM.WORD;
                  priv: BOOLEAN;
                  );

PROCEDURE put_str(task: TASK;
                  name: ARRAY OF CHAR;
                  data: ARRAY OF CHAR;
                  priv: BOOLEAN
                  );

PROCEDURE get_str(task: TASK; name: ARRAY OF CHAR; VAR str : STRING);
PROCEDURE get_env(task: TASK; name: ARRAY OF CHAR; VAR data: WORDs);

PROCEDURE del_env(task: TASK; name: ARRAY OF CHAR);

----------------------------------------------------------------

PROCEDURE xole(task: TASK; VAR x: SYSTEM.WORD);

***************************************************************)

END Tasks.

4D in main == exit
