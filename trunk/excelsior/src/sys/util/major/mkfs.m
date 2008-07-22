MODULE mkfs; (* Leo 16-Jan-90 (c) KRONOS *)

IMPORT  std: StdIO;
IMPORT  tty: Terminal;
IMPORT  key: Keyboard;
IMPORT  str: Strings;
IMPORT  bio: BIO;
IMPORT  arg: tskArgs;
IMPORT       Heap;

WITH STORAGE: Heap;

PROCEDURE pusage;
BEGIN
  std.print('  "mkfs"  make file system utility program     (c) KRONOS\n');
  std.print("run\n");
  std.print("   mkfs {block_device_name}  [label_that_may_be_omitted]\n");
  std.print("                                    Leopold, Jan 16 89\n");
END pusage;

VAR d: bio.FILE;
  lab: ARRAY [0..7] OF CHAR;

PROCEDURE query;
  VAR ch: CHAR;
BEGIN
  IF (lab#"") & (lab#"UL") THEN
    tty.print('make file system at "%s", label "%s";  are you sure? ',arg.words[0],lab)
  ELSE
    tty.print('make file system at "%s"; are you sure? ',arg.words[0])
  END;
  LOOP
    key.read(ch);
    IF  (ch='n') OR (ch='N') THEN tty.print("%c\n",ch); HALT   END;
    IF  (ch='y') OR (ch='Y') THEN tty.print("%c\n",ch); RETURN END;
    key.bell(1)
  END
END query;

CONST CANT = "can't";

VAR
   BADS: DYNARR OF INTEGER;
   type: INTEGER;
  block: INTEGER;

BEGIN
  IF HIGH(arg.words)<0 THEN pusage; HALT END;
  bio.open(d,arg.words[0],'wr');
  IF NOT bio.done THEN
    tty.perror(bio.error,'%s open "%s": %%s\n',CANT,arg.words[0]);
    HALT(bio.error)
  END;
  IF HIGH(arg.words)>0 THEN str.copy(lab,arg.words[1]) ELSE lab:="UL" END;
  query;
  NEW(BADS,0);
  bio.fstype(bio.cd,type,block);
  IF NOT bio.done THEN
    tty.perror(bio.error,'%s get fstype at ".": %%s\n',CANT);
    HALT(bio.error)
  END;
  bio.mkfs(d,type,block,lab,BADS);
  IF NOT bio.done THEN
    tty.perror(bio.error,'%s mkfs at "%s": %%s\n',CANT,arg.words[0]);
    HALT(bio.error)
  END
END mkfs.
