MODULE login; (*$U+$X+ Leo 15-Jan-90 (c) KRONOS *)

IMPORT  low: lowLevel;
IMPORT  str: Strings;
IMPORT  usr: Users;
IMPORT  tim: Time;
IMPORT  tsk: Tasks;
IMPORT  syn: Signals;
IMPORT  tty: Terminal;
IMPORT  key: Keyboard;
IMPORT  arg: tskArgs;
IMPORT       ASCII;

PROCEDURE readstr(VAR s: ARRAY OF CHAR; nm: BOOLEAN; VAR len: INTEGER): BOOLEAN;
  VAR i: INTEGER;
     ch: CHAR;
BEGIN
  i:=0;
  REPEAT
    key.reset;
    key.wait(4000);
    IF NOT key.done THEN
      IF (i#0) OR NOT nm THEN RETURN TRUE END;
    ELSE
      key.read(ch);
      IF ch=33c THEN
        IF (i#0) OR NOT nm THEN RETURN TRUE END
      ELSIF (ch>40c) & (ch<177c) THEN
        s[i]:=ch; INC(i);
        IF nm THEN tty.print("%c",ch) END;
      ELSIF nm & (ch=key.back) & (i>0) THEN
        tty.left(1); tty.del_char(1);
        DEC(i);
      END;
    END
  UNTIL (ch=ASCII.CR) OR (i=HIGH(s));
  s[i]:=0c; len:=i; RETURN FALSE
END readstr;

PROCEDURE delay(n: INTEGER);
BEGIN tty.print("\n"); tim.delay(n,tim.milisec) END delay;

PROCEDURE roll;
  VAR i: INTEGER;
BEGIN
  tty.set_cursor(0);
  FOR i:=0 TO tty.state^.lines+1 DO tty.print("\n") END;
  tty.set_cursor(1);
END roll;

PROCEDURE log_in(VAR uc: INTEGER; VAR shell: ARRAY OF CHAR);
  CONST default = "shell";
  VAR i: INTEGER;
      l: INTEGER;
      u: usr.USER;
    psw: ARRAY [0..7] OF CHAR;
BEGIN
  uc:=-1; shell:=default;
  tty.reset;
  key.reset;
  tty.print("\nusername: ");
  IF readstr(u.name,TRUE,l) THEN roll; uc:=-1; RETURN END;
  IF l>7       THEN delay(4000); RETURN END;
  IF u.name="" THEN delay(500);  RETURN END;
  IF usr.no_users=0 THEN
    uc:=0; shell:=default; RETURN
  END;
  usr.find(u);
  IF u.done THEN
    usr.get_user(u);
    IF u.done THEN usr.get_shell(u,shell); u.done:=TRUE END;
  END;
  IF u.done & (u.pass="") THEN
    uc:=usr.pack(u.usr,u.gro,FALSE); RETURN
  END;
  i:=3;
  REPEAT
    tty.print("\npassword: ");
    IF readstr(psw,FALSE,l) THEN roll; uc:=-1; RETURN END;
    IF u.done & (u.pass[0]#377c) & (psw=u.pass) THEN
      uc:=usr.pack(u.usr,u.gro,FALSE); RETURN
    END;
    i:=i-1
  UNTIL (i=0);
  delay(4000);
END log_in;

PROCEDURE run(uc: INTEGER; VAR shell: ARRAY OF CHAR);

  VAR t: tsk.TASK;
     pa: tsk.TASK;

  PROCEDURE tsk_error(VAL op: ARRAY OF CHAR);
  BEGIN
    tty.perror(tsk.error,"%s(%s): %%s %s\n",op,shell,tsk.note)
  END tsk_error;

  PROCEDURE close;
  BEGIN
    tsk.signal(t,tsk.kill);
    tsk.close(t);
    IF NOT tsk.done THEN tsk_error("close") END
  END close;

  VAR i,j: INTEGER;
      u,g: INTEGER;
      stk: INTEGER;
     trap: INTEGER;
     bump: ARRAY [0..127] OF CHAR;
     done: BOOLEAN;
     priv: BOOLEAN;
     kill: syn.SIGNAL;
   x,stop: syn.SIGNAL;
BEGIN
  roll;
  i:=0;
  str.search(shell,i,' ');
  IF (i<HIGH(shell)) & (shell[i]=' ') THEN
    j:=i;
    str.skip(shell,i,' ');
    str.sub_str(bump,shell,i,HIGH(bump));
    shell[j]:=0c
  END;
  IF shell[0]='+' THEN
    str.delete(shell,0,1);
    usr.unpack((uc),u,g,priv); uc:=usr.pack(u,g,TRUE)
  END;
  IF shell[0]='[' THEN
    str.delete(shell,0,1);
    i:=0;
    str.iscan(stk,shell,i,done);
    IF NOT done OR (shell[i]#']') THEN
      tty.print("illegal shell syntax: %s\n",shell); RETURN
    END;
    stk:=stk*256;
    str.delete(shell,0,i+1); i:=0; str.skip(shell,i,' '); str.delete(shell,0,i)
  ELSE
    stk:=8*256
  END;
  tsk.create(t,tsk.task0,shell,"",stk,bump);
  IF NOT tsk.done THEN tsk_error("create"); RETURN END;
  tsk.set_attr(t,tsk.a_user,uc);
  IF NOT tsk.done THEN tsk_error("set user"); close; RETURN END;
  tsk.set_attr(t,tsk.a_ipr,FALSE);
  IF NOT tsk.done THEN tsk_error("set ipr"); close; RETURN END;
  tsk.run(t);
  IF NOT tsk.done THEN tsk_error("run"); close; RETURN END;
  tsk.get_signal(stop,t,tsk.stop);
  IF NOT tsk.done THEN tsk_error("get signal"); close; RETURN END;
  tsk.get_signal(kill,t,tsk.kill);
  IF NOT tsk.done THEN tsk_error("get signal"); close; RETURN END;
  syn.alt(x,-1,stop,kill);
  IF x=stop THEN
    tsk.history(t,trap,bump);
    tty.print('%s\n',bump);
    tsk.signal(t,tsk.kill);
    syn.wait(kill)
  ELSE
    roll
  END;
  tsk.free_signal(stop,t,tsk.stop);
  tsk.free_signal(kill,t,tsk.kill);
  tsk.close(t);
  IF NOT tsk.done THEN tsk_error("close") END
END run;

VAR
  shell: ARRAY [0..127] OF CHAR;
  ucode: INTEGER;

BEGIN
  roll;
  LOOP
    log_in(ucode,shell);
    roll;
    IF ucode>=0 THEN run(ucode,shell) END
  END
END login.
