MODULE load; (*  26-Nov-90. (c) KRONOS *)

IMPORT  tsk: Tasks;
IMPORT  tty: Terminal;
IMPORT  arg: tskArgs;
IMPORT  std: StdIO;

PROCEDURE help;
BEGIN
  std.print(
    '   "load"  load codes utility program    (c) KRONOS\n'
    'usage:\n'
    '    load [-h] {module_name} [ALIAS=alias] [FATHER=father]\n');
END help;

VAR
  T: tsk.TASK;
i,n: INTEGER;
  s: STRING;

BEGIN
  IF (LEN(arg.words)=0) OR arg.flag('-','h') THEN help; HALT END;
  IF arg.number('FATHER',n) & (n>0) THEN
    tsk.open(T,tsk.task0,n);
    IF NOT tsk.done THEN
      tty.perror(tsk.error,'TASK %d %%s\n',n);   HALT(tsk.error)
    END
  ELSE
    n:=0; T:=tsk.task0
  END;
  IF arg.string('ALIAS',s) THEN NEW(s) END;
  FOR i:=0 TO HIGH(arg.words) DO
    tsk.load_codes(T,arg.words[i],s);
    IF NOT tsk.done THEN
      tty.perror(tsk.error,'%s %%s\n',tsk.note); HALT(tsk.error)
    END
  END
END load.
