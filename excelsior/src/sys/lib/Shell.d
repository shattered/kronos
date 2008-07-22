DEFINITION MODULE Shell; IMPORT  SYSTEM; (* Hady. 07-Sep-90. (c) KRONOS *)

TYPE PRINT = PROCEDURE (ARRAY OF CHAR, SEQ SYSTEM.WORD);
      READ = PROCEDURE (VAR CHAR);

VAR print: PRINT;    (* default is Terminal.print *)
     read: READ;     (* default is Keyboard.read  *)

VAL result: INTEGER; (* result of last operation  *)

PROCEDURE get_prompt(VAR prompt: ARRAY OF CHAR;
                     VAR ins, bell: BOOLEAN);

PROCEDURE get_echo(VAR echo: ARRAY OF CHAR);
(* chars mean
     "@" - echo in command files
     "B" - reports of USER BREAK and HALTs
     "T" - reports of ASSERTs and TRAPs
     "I" - reports of awake
     "H" - print history at every abort
     "0" - no echo at all
     "1" - all echoes on
*)

PROCEDURE system(command: ARRAY OF CHAR; echo: ARRAY OF CHAR);
PROCEDURE submit(command: ARRAY OF CHAR; silent: BOOLEAN);

PROCEDURE hold_break(on: BOOLEAN);

(*
                        БИБЛИОТЕКА Shell.
                        -----------------

VAR print: PRINT;
  через это процедурное значение выдаются все
  сообщения библиотеки, возникшие в ходе выполнения
  процедуры system.

PROCEDURE get_prompt(VAR prompt: ARRAY OF CHAR; VAR ins,bell: BOOLEAN);
  распаковывает в переменную prompt приглашение, согласно
  формату, хранящемуся в окружении задачи под именем PROMPT.
  В формате:
     %/  заменяется на имя текущей директории
     %t  заменяется на время в формате ЧЧ:ММ
  Значения переменных ins, bell инициализируются согласно
  уравнениям окружения с именами INSERT, BELL.

PROCEDURE system(cmd: ARRAY OF CHAR; echo: ARRAY OF CHAR);
  СИНТАКСИС строки cmd:
     string ::= command { ( separator ) command } [ separator ] .
     separator ::= "&" | ";" .
     command ::= cange_environment | [ prelude ] cmd_with_args .
     prelude ::= "{" { change_environment } { alias } "}" .
     change_environment ::= var_name "=" string.
     alias     ::= interface | unload | rename .
     interface ::= "+" module_name .
     unload    ::= "-" module_name .
     rename    ::= "=" module_name file_name .
     cmd_with_args ::=
        | "set" [ pattern ] | "mem" | "ps" { modifier } { no }
        | "his" [ task_no ] | "su"  | "us"
        | "cd"  [ dir_name] | "delay" seconds
        | "stop"  task_no { task_no }
        | "kill"  task_no { task_no }
        | "wait"  task_no { task_no }
        | "mount"   ["-ro"] directory device
        | "unmount" ["-ro"] directory
        | task_name { task_args } .

  "set" -- выдает значения некоторых уравнений ENVIRONMENT
  "mem" -- выдает распределение системной памяти
  "his" [ no ]  -- low-level history задачи с номером no,
                   или последней аварийно закончившейся задачи
  "cd" [ name ] -- меняет текущую директорию на name,
                   или выдает имя текущей директории
  "ps" { modificator } { no }
     выдает информацию о задачах с номерами no,
     либо о задачах, имеющих тот-же юзеркод.
     МОДИФИКАТОРЫ:
      "-mem" - информацию о занятой задачей памяти
      "-tty" - информацию о занятом задачей терминале
      "usr"  - имя и группу пользователя, запустившего задачу
      "-l"   - эквивалентно всем предыдущим ключам вместе
      "-all" - информацию о всех запущеных задачах
  "su"  -- переводит задачу в режим супер'юзера
  "us"  -- выводит задачу из режима супер'юзера
  "delay" no -- задерживает задачу на no секунд
*)

END Shell.
