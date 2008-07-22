MODULE mkdir; (* Leo 18-Dec-89 (c) KRONOS *)

IMPORT  std: Terminal;
IMPORT  bio: BIO;
IMPORT  arg: tskArgs;

PROCEDURE pusage;
BEGIN
  std.print('  "mkdir"  make directories utility program     (c) KRONOS\n');
  std.print("run\n");
  std.print("   mkdir {directory_name}\n");
  std.print("                                    Leopold, Dec 18 89\n");
END pusage;

VAR i: INTEGER;

BEGIN
  IF HIGH(arg.words)<0 THEN pusage; HALT END;
  FOR i:=0 TO HIGH(arg.words) DO
    bio.mkdir(arg.words[i],FALSE);
    IF NOT bio.done THEN
      std.perror(bio.error,'make directory "%s": %%s\n',arg.words[i]);
      HALT(1);
    END
  END
END mkdir.
