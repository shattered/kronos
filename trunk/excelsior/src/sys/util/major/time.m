MODULE time; (*  12-Jun-90. (c) KRONOS *)

IMPORT  exe: Shell;
IMPORT  err: defErrors;
IMPORT  env: tskEnv;
IMPORT  std: StdIO;
IMPORT  tim: Time;

VAR t: INTEGER;
    s: STRING;
    echo: BITSET;

BEGIN
  NEW(s);
  env.get_str(env.args,s);
  IF NOT env.done & (env.error=err.no_entry) OR (HIGH(s)<0) THEN HALT END;
  IF (NOT env.done) THEN
    std.perror(env.error,"time - can't get $%s: %%s\n",env.args); HALT
  END;
  t:=tim.sys_time(tim.milisec);
  exe.print:=std.print;
  exe.get_echo(echo);
  exe.system(s,echo);
  t:=tim.sys_time(tim.milisec)-t;
  std.print("time = %d.%03d secs\n",t DIV 1000,t MOD 1000);
END time.
