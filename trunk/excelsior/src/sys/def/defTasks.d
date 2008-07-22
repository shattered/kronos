DEFINITION MODULE defTasks; (* Ned 20-Mar-90. (c) KRONOS *)

CONST  -- состояния задачи

  new     = 1;
  loaded  = 2;
  loaderr = 3;
  ready   = 4;
  running = 5;
  stopped = 6;
  killed  = 7;

CONST  -- номера сигналов задачи

  start   = 0;
  stop    = 1;
  kill    = 2;
  ipr     = 3;
  suspend = 4;
  resume  = 5;

END defTasks.
