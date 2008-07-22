MODULE system[1]; (* Leo  19-Dec-88. (c) KRONOS *)
                  (* Ned  22-Aug-89. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  mcd: defCodes;
IMPORT  def: defCode;
IMPORT  tsk: defTasks;
IMPORT  err: defErrors;
IMPORT  str: Strings;
IMPORT  low: lowLevel;

IMPORT   os: osKernel;
IMPORT  ldr: osLoader;

IMPORT  Users;      (* to link with system and init *)
IMPORT  osFiles;    (* to link with system and init *)

CONST KB = 256; (* 256 words per 1KB *)

TYPE ADDRESS = SYSTEM.ADDRESS;

PROCEDURE quit; CODE mcd.quit END quit;

PROCEDURE setm(m: BITSET); CODE mcd.setm END setm;

----------------------------------------------------------------

CONST ok = err.ok;

PROCEDURE lookup(task: os.task_ptr;
             VAL name: ARRAY OF CHAR;
             VAR code: def.code_ptr;
             VAR glo : ADDRESS;
             VAR tags: BITSET): INTEGER;
BEGIN
  ldr.lookupModule(task,name,code,glo); tags:={};
  IF code=NIL THEN
    os.print('code file absent: "%s"\n',name); quit; RETURN -1
  END;
  RETURN ok
END lookup;


PROCEDURE run(VAL name: ARRAY OF CHAR;
              VAL parm: ARRAY OF CHAR;
              stk_size: INTEGER;
             VAR error: ARRAY OF CHAR;
              ): INTEGER;
  VAR ss: ARRAY [0..1] OF os.signal_ptr;
     res: INTEGER;
     msg: ARRAY [0..79] OF CHAR;
    task: os.task_ptr;
BEGIN
  res:=os.create(task,os.task0);
  IF res#ok THEN str.print(error,'create("%s")',name); RETURN res END;
  res:=ldr.load(task,name,lookup,msg);
  IF res#ok THEN str.print(error,'load("%s") => %s',name,msg); RETURN res END;
  res:=os.put_str(task,'ARGS',parm,TRUE);
  IF res#ok THEN str.print(error,'put_env("%s")',name); RETURN res END;
  res:=os.put_str(task,'NAME',name,TRUE);
  IF res#ok THEN str.print(error,'put_env("%s")',name); RETURN res END;
  res:=ldr.run(task,stk_size);
  IF res#ok THEN str.print(error,'run("%s")',name); RETURN res END;
  os.send(task^.inp[tsk.start]^);
  ss[0]:=task^.out[tsk.ipr];
  ss[1]:=task^.out[tsk.stop];
  res:=os.alt_wait(-1,ss);
--os.print("stop or awake('%s')\n",name);
  IF res=1 THEN os.send(task^.inp[tsk.kill]^) END;
  os.close(task);
  RETURN ok
END run;

PROCEDURE _run(a: ADDRESS);
  VAR i: INTEGER;
    stk: INTEGER;
    msg: ARRAY [0..79] OF CHAR;
   name: STRING;
   args: STRING;
BEGIN
  stk:=a^; INC(a);                       (*$<$U+*)
  name^.HIGH:=a^; INC(a);  name^.ADR:=a;  INC(a,SIZE(name));
  args^.HIGH:=a^; INC(a);  args^.ADR:=a;  (*$>*)
--os.print("run('%s','%s',%dKB+%04d)\n",name,args,stk DIV 256,stk*4 MOD 1024);
  IF name[0]='$' THEN RETURN END;
--os.print("run('%s','%s',%dKB+%04d)\n",name,args,stk DIV 256,stk*4 MOD 1024);
  i:=run(name,args,stk,msg);
  IF i#ok THEN os.print('%s: %h\n',msg,i); quit END;
END _run;

PROCEDURE run_new;
  VAR a: ADDRESS;
BEGIN
  a:=83h; a:=a^;
  WHILE a#0 DO _run(a+1); a:=a^ END;
END run_new;

PROCEDURE reset_serials;

  PROCEDURE reset; CODE mcd.reset END reset;
  PROCEDURE delay; BEGIN END delay;

  VAR i: INTEGER;
    csr: ADDRESS;
BEGIN
  reset;
  FOR i:=0 TO 3 DO
    csr:=ADDRESS(8200F1h+i*2);
    IF csr^#0 THEN
      csr^:={};  delay; csr^:={};  delay;
      csr^:={};  delay; csr^:={};  delay; csr^:={6}; delay
    END;
    csr:=ADDRESS(17E003h); csr^:=0;
  END
END reset_serials;

BEGIN
  IF low.cpu=6 THEN reset_serials  END;
  setm(os.MASK);
  run_new;
  os.task_manager
END system.
