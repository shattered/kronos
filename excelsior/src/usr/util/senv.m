MODULE senv; (* Ned 13-Dec-90. (c) KRONOS *)

IMPORT  str: Strings;
IMPORT  bio: BIO;
IMPORT  env: tskEnv;
IMPORT  std: StdIO;
IMPORT args: tskArgs;

CONST end = '%END';

VAR name: ARRAY [0..255] OF CHAR;

PROCEDURE bio_error;
BEGIN
  IF NOT bio.done THEN
    std.perror(bio.error,'open("%s"): %%s\n',name); HALT
  END;
END bio_error;

PROCEDURE pusage;
BEGIN
  std.print(
      '  "senv" save environment utility program (c) KRONOS\n'
      'usage:\n'
      '   senv [-hc] {env_name}\n');
  std.print(
      'writes (appends) values of strings in environment into file\n'
      '   $HOME/profile.@\n\n'
      '    -c  for clearing this information\n'
      '                                   Ned, Dec 13 90\n');

END pusage;

VAR  f: bio.FILE;
     i: INTEGER;
     s: STRING;
  line: ARRAY [0..255] OF CHAR;

BEGIN
  IF args.flag('-','h') THEN pusage; HALT END;
  IF (LEN(args.words)=0) & NOT args.flag('-','c') THEN pusage; HALT END;
  env.get_str('HOME',s);
  IF NOT env.done THEN
    std.perror(env.error,'home directory absent: %%s\n'); HALT
  END;
  str.print(name,'%s/%s',s,'profile.@');
  bio.open(f,name,'rw');
  bio_error;
  bio.check_io(TRUE);
  LOOP
    bio.getstr(f,line,0);
    IF bio.iolen=0 THEN EXIT END;
    IF line=end THEN EXIT END;
  END;
  IF args.flag('-','c') THEN
    bio.cut(f,bio.pos(f)); bio_error;
  ELSE
    IF bio.iolen=0 THEN bio.print(f,'%s\n',end); END;
    FOR i:=0 TO HIGH(args.words) DO
      env.get_str(args.words[i],s);
      IF env.done THEN
        bio.print(f,'%s="%s"\n',args.words[i],s);
      END;
    END;
  END;
  bio.close(f);
  bio_error;
END senv.
