MODULE tmspo; (* Igo 15-Feb-89. (c) KRONOS *)

IMPORT  Time;
IMPORT  err: defErrors;
IMPORT  tty: Terminal;
IMPORT  std: StdIO;
IMPORT  env: tskEnv;
IMPORT  arg: tskArgs;
IMPORT  sys: SYSTEM;
IMPORT  bio: BIO;

VAR f: bio.FILE;
    t: INTEGER;

PROCEDURE check(VAL s: ARRAY OF CHAR);
BEGIN
  IF bio.done THEN RETURN END;
  tty.perror(bio.error,"tmspo.%s(): %%s\n",s); HALT(bio.error)
END check;

PROCEDURE create;
BEGIN
  bio.create(f,arg.words[0],"rwh",4); check("create");
  t:=Time.time();
  bio.write(f,sys.ADR(t),4);          check("write");
  bio.seek(f,0,0);                    check("seek");
END create;

PROCEDURE try_save_time;
  VAR t: INTEGER;
BEGIN
  bio.open(f,arg.words[0],'w');
  IF NOT bio.done THEN RETURN END;
  t:=Time.time();
  bio.write(f,sys.ADR(t),4);    check("write");
  bio.close(f);                 check("close");
END try_save_time;

PROCEDURE TimeSaver;
  VAR t: INTEGER;
BEGIN
  env.become_ipr;
  LOOP
    t:=Time.time();
    REPEAT Time.delay(10,Time.sec) UNTIL (Time.time()-3*60)>=t;
    try_save_time
  END;
END TimeSaver;

PROCEDURE start_spooler;
BEGIN
  bio.open(f,arg.words[0],"rw");
  IF bio.done THEN
  ELSIF bio.error=err.no_entry THEN create
  ELSE check("open")
  END;
  bio.read(f,sys.ADR(t),4); check("read");
  bio.close(f);             check("close");
  Time.set_time(t);
  TimeSaver
END start_spooler;

BEGIN
  IF (HIGH(arg.words)<0) OR arg.flag('-','h') THEN
    std.print('  "tmspo" time spooler utility program    (c) KRONOS\n'
              "usage:\n"
              "   tmspo filename \n");
    std.print(" read time from specified file being started\n"
              " and save current time to it every minute");
    HALT
  ELSE
    start_spooler
  END
END tmspo.
