DEFINITION MODULE osKernel; (* Ned 27-Apr-90. (c) KRONOS *)
                            (* Leo 14-May-90. (c) KRONOS *)

IMPORT  SYSTEM;

VAL MASK: BITSET; (* MAIN INTERRUPT MASK *)

CONST MAGIC = 4154h; (* TA -- task magic *)

TYPE
  PROCESS  = POINTER TO HW_descriptor;
  process  = POINTER TO OS_descriptor;
  task_ptr = POINTER TO task_rec;
  WORDs    = DYNARR OF SYSTEM.WORD;

TYPE
  node_ptr = POINTER TO node_rec;
  node_rec = RECORD
               papa : node_ptr;
               sons : node_ptr;
               bro  : node_ptr;
               next : node_ptr;
               CASE :INTEGER OF
                 |0: exts: SYSTEM.ADDRESS;
                 |1: task: task_ptr;
                 |2: prs : process;
               END
             END;

TYPE         -- описание низкоуровневых процессов --

  HW_descriptor = RECORD
                    G   : SYSTEM.ADDRESS;
                    L   : SYSTEM.ADDRESS;
                    PC  : INTEGER;
                    M   : BITSET;
                    S   : SYSTEM.ADDRESS;
                    H   : SYSTEM.ADDRESS;
                    T   : INTEGER;
                    XX  : SYSTEM.WORD;
                    prs : process;
                  END;

TYPE QUEUE;

CONST null = QUEUE(NIL);

CONST -- signal tags
  guard  = {0};
  break  = {1};
  sendup = {2};

TYPE
  signal_ptr = POINTER TO signal_rec;
  signal_rec = RECORD
                 magic: INTEGER;
                 queue: QUEUE;
                 cou  : INTEGER;
                 tags : BITSET;
               END;
  mutex_ptr  = POINTER TO mutex_rec;
  mutex_rec  = RECORD
                 magic: INTEGER;
                 prs  : process;
                 cou  : INTEGER;
                 queue: QUEUE;
                 next : mutex_ptr;
               END;

TYPE
  AREA;
  env_ptr   = POINTER TO env_rec;
  env_rec   = RECORD
                name: STRING;
                data: WORDs;
                priv: BOOLEAN;
                next: env_ptr;
              END;
  reso_ptr  = POINTER TO reso_rec;
  reso_rec  = RECORD
                proc: PROC;
                next: reso_ptr;
              END;
  signals   = ARRAY [0..7] OF signal_ptr;
  task_rec  = RECORD
                magic : INTEGER;
                node  : node_ptr;
                id    : INTEGER;          -- индикант задачи
                user  : INTEGER;          --
                opens : INTEGER;          --
                status: INTEGER;          --
                lock  : mutex_rec;        -- захват дескриптора
                area  : AREA;             --
                env   : env_ptr;
                prs   : process;          -- tree of process
                main  : process;          -- main process or closure
                reso  : reso_ptr;
                res   : INTEGER;
                ipr   : BOOLEAN;
                inp   : signals;          --
                out   : signals;          --
                mucode: INTEGER;          -- inherited from main code
                wspX  : WORDs;
                loader: SYSTEM.ADDRESS;
              END;

CONST   -- состояния процесса

  _new       = 0;  -- новенький процесс
  _ready     = 1;  -- готовый к исполнению или активный
  _suspended = 2;  -- задержанный (suspend)
  _wait      = 3;  -- ожидание сигнала
  _brk_wait  = 4;  -- ожидание сигнала с признаком break
  _aborted   = 5;  -- убитый

TYPE
  OS_descriptor = RECORD
                    magic : INTEGER;
                    pp    : PROCESS;      --
                    status: INTEGER;      -- состояние
                    tags  : BITSET;       -- признаки
                    guard : INTEGER;      -- если >0, то неабортируем
                    prio  : INTEGER;      -- приоритет процесса
                    ticks : INTEGER;      -- время работы
                    r_time: INTEGER;      -- время возобновления
                    halt  : signal_ptr;   -- сигнал завершения
                    mutex : mutex_ptr;    -- список пройденных ворот
                    parm  : WORDs;        --
                    wsp   : WORDs;        --
                    task  : task_ptr;     --
                    node  : node_ptr;     --
                    server: PROCESS;      -- личный обработчик прерываний
                    snares: SYSTEM.ADDRESS;  -- список ловушек исключений
                  END;

---------------------------------------------------------------

VAL version: INTEGER;
      timer: INTEGER;  -- число тиков с момента запуска системы
      tick : INTEGER;  -- длина тика в милисекундах

VAR
      time : INTEGER;  -- время по Гринвичу      в секундах
      zone : INTEGER;  -- расстояние до Гринвича в секундах

VAL
  task0 : task_ptr;
  system: AREA;
  core  : INTEGER;  -- размер области занятой ядром системы
  total : INTEGER;
  free  : INTEGER;
  memtop: INTEGER;

----------------------------------------------------------------

PROCEDURE active(): process;

PROCEDURE start(p: process);

PROCEDURE stop (p: process; wait: BOOLEAN);
PROCEDURE abort(p: process; wait: BOOLEAN);   -- tree of process

PROCEDURE set_prio(p: process; prio: INTEGER);

PROCEDURE lock;
PROCEDURE unlock;

PROCEDURE delay(ticks: INTEGER);

PROCEDURE suspend(p: process; ticks: INTEGER);
PROCEDURE resume(p: process);

--------------------------  SIGNALs  --------------------------
                          -----------

PROCEDURE ini_signal(VAR s: signal_rec; tags: BITSET; no: INTEGER);

PROCEDURE send     (VAR s: signal_rec);
PROCEDURE signal   (VAR s: signal_rec; no: INTEGER);
PROCEDURE broadcast(VAR s: signal_rec);

PROCEDURE wait    (                VAR s: signal_rec);
PROCEDURE wait_del(delay: INTEGER; VAR s: signal_rec): INTEGER;
PROCEDURE alt_wait(delay: INTEGER;     s: ARRAY OF signal_ptr): INTEGER;

---------------------------  MUTEX  ---------------------------
                           ---------

PROCEDURE ini_mutex(VAR m: mutex_rec);

PROCEDURE acquire(VAR m: mutex_rec);
PROCEDURE release(VAR m: mutex_rec);

PROCEDURE acquire_del(delay: INTEGER; VAR m: mutex_rec): BOOLEAN;

--------------------------  ACTIONs  --------------------------
                          -----------

PROCEDURE insert_action(p: PROC): INTEGER;
PROCEDURE remove_action(p: PROC);

---------------------  LOW-LEVEL PROCESS  ---------------------
                     ---------------------

PROCEDURE new_process(p: PROC; wsp: SYSTEM.ADDRESS; size: INTEGER;
            VAR process: PROCESS);
(* Создание нового процесса == NEWPROCESS *)

PROCEDURE extract(VAR his: ARRAY OF CHAR; p: PROCESS);

---------------------------------------------------------------

PROCEDURE new(VAR area: AREA): BOOLEAN;
(* TRUE, if ok *)

PROCEDURE delete(VAR area: AREA);

PROCEDURE   ALLOCATE(area: AREA; VAR a: SYSTEM.ADDRESS; words: INTEGER);
PROCEDURE DEALLOCATE(area: AREA; VAR a: SYSTEM.ADDRESS; words: INTEGER);

PROCEDURE info(area: AREA; VAR total,free: INTEGER);

----------------------------------------------------------------

PROCEDURE create(VAR task: task_ptr; papa: task_ptr): INTEGER;

PROCEDURE run(task: task_ptr; proc: PROC; stack: INTEGER): INTEGER;

PROCEDURE self(): task_ptr;

PROCEDURE open(VAR task: task_ptr; papa: task_ptr; id: INTEGER): INTEGER;

PROCEDURE reopen(task: task_ptr);

PROCEDURE close(VAR task: task_ptr);

PROCEDURE papa   (task: task_ptr; VAR id: INTEGER);
PROCEDURE son    (task: task_ptr; VAR id: INTEGER);
PROCEDURE brother(task: task_ptr; VAR id: INTEGER);

PROCEDURE final(task: task_ptr; p: PROC): INTEGER;

---------------------------------------------------------------

PROCEDURE make_process(VAR p: process;
                        base: PROC;
                        size: INTEGER): INTEGER;

PROCEDURE rem_process(VAR p: process): INTEGER;

-----------------------  ENVIRONMENT  --------------------------
                       ---------------

PROCEDURE put_env(task: task_ptr;
                  name: ARRAY OF CHAR;
                  data: ARRAY OF SYSTEM.WORD;
                  priv: BOOLEAN;
                     ): INTEGER;

PROCEDURE put_str(task: task_ptr;
                  name: ARRAY OF CHAR;
                  data: ARRAY OF CHAR;
                  priv: BOOLEAN;
                     ): INTEGER;

PROCEDURE get_env(task: task_ptr;
                 delay: INTEGER;
                  name: ARRAY OF CHAR;
              VAR data: STRING;
              VAR priv: BOOLEAN): INTEGER;

PROCEDURE del_env(task: task_ptr; name: ARRAY OF CHAR): INTEGER;

PROCEDURE copy_env(from,to: task_ptr): INTEGER;

PROCEDURE show_env(task: task_ptr;
                  entry: INTEGER;
                  delay: INTEGER;
               VAR name: STRING;
               VAR data: STRING;
               VAR priv: BOOLEAN): INTEGER;

---------------------------------------------------------------

PROCEDURE task_manager;

---------------------------------------------------------------

TYPE print_proc = PROCEDURE (ARRAY OF CHAR, SEQ SYSTEM.WORD);

VAL print: print_proc;

PROCEDURE set_debug(p: print_proc);
PROCEDURE debug_off;

PROCEDURE get_sys_parm(VAR n: INTEGER; VAR s: STRING): BOOLEAN;

---------------------------------------------------------------

PROCEDURE vis_common;
PROCEDURE vis_area(a: AREA);

END osKernel.
