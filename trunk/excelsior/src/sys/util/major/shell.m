MODULE shell; (* Hady. 20-Aug-90. (c) KRONOS *)

IMPORT  shell: Shell;
IMPORT  err: defErrors;
IMPORT  sys: SYSTEM;
IMPORT  tsk: Tasks;
IMPORT  usr: Users;
IMPORT  env: tskEnv;
IMPORT  arg: tskArgs;
IMPORT  tty: Terminal;
IMPORT  key: Keyboard;
IMPORT  sle: strEditor;
IMPORT  bio: BIO;
IMPORT  str: Strings;
IMPORT  sta: Statistics;
IMPORT  mem: Heap;
IMPORT  clk: Time;
IMPORT  sci: ASCII;

WITH STORAGE  (NEW: mem.allocate;
           DISPOSE: mem.deallocate;
            RESIZE: mem.reallocate);

CONST version   = 'shell  v1.020  (c) 1990 KRONOS';
      PRO_NAME  = 'profile.@';

TYPE str80 = ARRAY [0..79] OF CHAR;
      str8 = ARRAY  [0..7] OF CHAR;

---------------------------- LIBRARY ---------------------------
                            ---------

PROCEDURE comp(VAL patt: ARRAY OF CHAR;
               VAL sour: ARRAY OF CHAR;
                  sf,st: INTEGER): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  IF sf>st THEN RETURN FALSE END;
  i:=0;
  WHILE (i<=HIGH(patt)) & (patt[i]#0c) & (sf<=st) DO
    IF sour[sf]#patt[i] THEN RETURN FALSE END;
    sf:=sf+1; i:=i+1
  END;
  RETURN (sf>st) & ((i>HIGH(patt)) OR (patt[i]=0c))
END comp;

PROCEDURE skip(VAL s: ARRAY OF CHAR; VAR p: INTEGER);
BEGIN
  WHILE (p<=HIGH(s)) & (s[p]=" ") DO p:=p+1 END
END skip;

PROCEDURE skipn(VAL s: ARRAY OF CHAR; VAR p: INTEGER);
BEGIN
  WHILE (p<=HIGH(s)) & (s[p]#" ") & (s[p]#0c) DO
    p:=p+1
  END
END skipn;

----------------------------- SHELL ----------------------------
                             -------

PROCEDURE check_bio(VAL fmt: ARRAY OF CHAR;
                    SEQ arg: sys.WORD): BOOLEAN;
BEGIN
  IF bio.done THEN RETURN FALSE END;
  tty.print(fmt,arg); tty.perror(bio.error," %%s\n");
  RETURN TRUE
END check_bio;

PROCEDURE check_mem(): BOOLEAN;
BEGIN
  IF mem.done THEN RETURN FALSE END;
  tty.perror(mem.error," %%s\n");
  RETURN TRUE
END check_mem;

TYPE str256 = ARRAY [0..255] OF CHAR;

VAR desc: sle.descriptor;
    bump: str256;

VAR echo: ARRAY [0..31] OF CHAR;

PROCEDURE profile(VAL s: ARRAY OF CHAR); FORWARD;

PROCEDURE home(VAL dir: ARRAY OF CHAR);
  VAR s: STRING;                             --++
BEGIN
  NEW(s,str.len(dir)+4);                       --++
  IF NOT mem.done THEN HALT(mem.error) END;    --++
  str.print(s,"CD=%s",dir);
  shell.system(s,"");  DISPOSE(s);             --++
  IF shell.result#0 THEN RETURN END;           --++
  env.put_str(env.home,dir,FALSE);
  IF arg.string('profile',s) THEN profile(s)          --++
  ELSE                            profile(PRO_NAME)   --++
  END                                                 --++
END home;

PROCEDURE execute(VAR cmd: ARRAY OF CHAR; echo: ARRAY OF CHAR);
  VAR i,j: INTEGER;
      par: ARRAY [0..79] OF CHAR;
        s: STRING;
BEGIN
  i:=0; skip(cmd,i);
  IF (i>HIGH(cmd)) OR (cmd[i]=0c) THEN RETURN END;
  j:=i; skipn(cmd,j);
  IF    comp("bye"    ,cmd,i,j-1) THEN HALT
  ELSIF comp("home"   ,cmd,i,j-1) THEN
    par:=""; i:=j; skip(cmd,i);
    IF (i<=HIGH(cmd)) & (cmd[i]#0c) THEN
      j:=i; skipn(cmd,j);
      str.sub_str(par,cmd,i,j-i)
    END;
    IF par="" THEN
      env.get_str(env.home,s);
      IF env.done THEN
        tty.print('#home is "%s"\n',s)
      ELSE
        tty.print('#home is not defined\n')
      END
    ELSE
      home(par)
    END
  ELSE
    shell.system(cmd, echo)
  END
END execute;

CONST NULL = CHAR(-1);

PROCEDURE monitor(sep: CHAR);
  VAR prompt: ARRAY [0.. 79] OF CHAR;
         cmd: ARRAY [0..255] OF CHAR;
         res: INTEGER;

  PROCEDURE empty(VAL s: ARRAY OF CHAR): BOOLEAN;
    VAR i: INTEGER;
  BEGIN
    i:=0; str.skip(s,i,' ');
    RETURN (i>HIGH(s)) OR (s[i]=0c)
  END empty;

BEGIN
  cmd:=""; --++
  tty.reset;
  LOOP
    shell.get_prompt(prompt, desc^.ins, desc^.bel);
    sle.edit_str(prompt,cmd,tty.state^.lines-1,0,
                 tty.state^.columns-2,desc,sep);
    IF desc^.last=sep THEN EXIT END;
    tty.print('\n');
    IF NOT empty(cmd) THEN
      shell.get_echo(echo); execute(cmd,echo);
      tty.print('\n')
    END
  END
END monitor;

PROCEDURE command_file(VAL cmd: ARRAY OF CHAR);
BEGIN shell.submit(cmd,FALSE)  END command_file;

PROCEDURE profile(VAL name: ARRAY OF CHAR);
BEGIN shell.submit(name,TRUE)    END profile;

PROCEDURE up_tty;
  VAR    s: STRING;
      path: ARRAY [0..79] OF CHAR;
      name: ARRAY [0..31] OF CHAR;
BEGIN
  env.get_str(env.tty,s);
  IF env.done THEN
    str.copy(path,s);
    bio.splitpathname(path,name);
    IF bio.done THEN
      str.print(path,"%s_up.@",name);
      command_file(path)
    END
  END
END up_tty;

VAR old_break: key.BREAK;
   save_break: INTEGER;

PROCEDURE final;
BEGIN
  key.user_break(old_break);
  key.set_break(save_break);
END final;

PROCEDURE empty(no: INTEGER); BEGIN END empty;

PROCEDURE put_user;
  VAR u: usr.USER;
      i: INTEGER;
BEGIN
  tsk.get_attr(tsk.self,tsk.a_user,i);
  usr.unpack(i,u.usr,u.gro,u.priv);
  usr.get_user(u);
  env.put_str("USER"  ,u.name,FALSE);
  env.put_str(env.info,u.name,FALSE)
END put_user;

----------------------------- INIT -----------------------------
                             ------
PROCEDURE make_bump;
  VAR size: INTEGER;
BEGIN
  sta.get(sta.mem_total,size);
  IF size>512*256 THEN size:=32
  ELSE size:=16
  END;
  sle.new(desc,size)
END make_bump;

PROCEDURE usage;
BEGIN
  tty.print(
    '%s\n'
    '  "shell" - interface with system utility (c) KRONOS\n'
    'usage:\n'
    '   shell root=root_dir [profile=file]\n'
    '   shell home=home_dir [profile=file]\n');  --++
  tty.print(
    '   shell <command_file { word }\n'
    '   shell\n'
    '                          Hadrian, Sep 3 1990\n'
            );
END usage;

PROCEDURE submit;
  VAR s: STRING;
      i: INTEGER;
BEGIN
  env.put_str(env.info,arg.words[0],TRUE);
  env.get_str(env.args,s);
  i:=0; str.skip(s,i,' ');
  IF (i<=HIGH(s)) & (s[i]='<') THEN INC(i) END;
  str.sub_str(bump,s,i,HIGH(s)-i+1);
  command_file(bump)
END submit;

VAR s: STRING;

BEGIN
  IF arg.flag('-','h') THEN usage; HALT END;
  IF arg.flag('-','#') THEN
    tty.print('%s\n',version); HALT
  END;
  make_bump;
  IF    arg.string("root",s) THEN
    env.become_ipr;
    home(s)             --++
  ELSIF arg.string("home",s) THEN
    key.nop;
    old_break:=key.state^.ubrk;
    key.user_break(empty);
    shell.hold_break(TRUE);
    env.final(final);
    up_tty; put_user;
    home(s);
    monitor(NULL)
  ELSIF HIGH(arg.words)<0 THEN
    shell.hold_break(TRUE); monitor(033c); shell.hold_break(FALSE)
  ELSIF arg.words[0,0]="<"   THEN submit
  ELSE usage
  END
END shell.
