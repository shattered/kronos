MODULE du; (* Leo 27-Dec-1989 (c) KRONOS *)

IMPORT  tty: Terminal;
IMPORT  std: StdIO;
IMPORT  bio: BIO;
IMPORT  arg: tskArgs;

PROCEDURE pusage;
BEGIN
  std.print('  "du"  disk usage utility program    (c) KRONOS\n');
  std.print("run\n");
  std.print("   du  [path]\n");
  std.print("                                         Leopold, Dec 28 89\n");
END pusage;

PROCEDURE p(VAL s: ARRAY OF CHAR; n: INTEGER);
BEGIN
  IF n>1000 THEN
    std.print("%s%2d,%03dK",s,n DIV 1000,n MOD 1000);
  ELSE
    std.print("%s%03dK",s,n)
  END
END p;

PROCEDURE du(file: bio.FILE; VAL name: ARRAY OF CHAR);
  VAR f,u,t,prc: INTEGER;
BEGIN
  bio.du(file,f,u);
  IF NOT bio.done THEN
    tty.perror(bio.error,'du("%s"): %%s\n',name); HALT(bio.error)
  END;
  t:=f+u;
  t:=t DIV 1024;
  f:=f DIV 1024;
  u:=u DIV 1024;
  std.print('at "%s"',name);
  p('  total: ',t);  prc:=f*100 DIV t;
  p('   free: ',f);  std.print(" (%d%%)",prc);
  p('   used: ',u);  std.print(" (%d%%)\n",100-prc)
END du;

VAR  f: bio.FILE;

BEGIN
  IF arg.flag('-','h') THEN pusage; HALT END;
  IF HIGH(arg.words)<0 THEN
    du(bio.cd,".")
  ELSE
    bio.open(f,arg.words[0],'');
    IF NOT bio.done THEN
      tty.perror(bio.error,'open("%s"): %%s\n',arg.words[0]); HALT(bio.error)
    END;
    du(f,arg.words[0])
  END
END du.
