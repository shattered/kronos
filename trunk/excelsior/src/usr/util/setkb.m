MODULE setkb; (* Hady. 06-Mar-90. (c) KRONOS *)

IMPORT  bio: BIO;
IMPORT  dkb: defKeyboard;
IMPORT  key: Keyboard;
IMPORT  arg: tskArgs;
IMPORT  tty: Terminal;
IMPORT  sys: SYSTEM;
IMPORT  mem: Heap;
IMPORT  sci: ASCII;
IMPORT  tskEnv;

WITH STORAGE: mem;

VAR CODER: ARRAY CHAR OF CHAR;

PROCEDURE check;
BEGIN
  IF bio.done THEN RETURN END;
  tty.perror(bio.error,"%%s\n"); HALT(1)
END check;

PROCEDURE set_code(from,to: CHAR);
BEGIN CODER[from]:=to END set_code;

PROCEDURE get_coder();
  VAR i,j: INTEGER;
BEGIN
  key.ioctl(dkb._get_ct, CODER);
  IF NOT key.done THEN
    tty.perror(key.error,"get_coder():%%s\n"); HALT(1)
  END
END get_coder;

PROCEDURE set_coder();
BEGIN
  key.ioctl(dkb._set_ct, CODER);
  IF NOT key.done THEN
    tty.perror(key.error,"set_coder():%%s\n"); HALT(1)
  END;
END set_coder;

PROCEDURE init;
  VAR ch: CHAR;
BEGIN
  FOR ch:=MIN(CHAR) TO MAX(CHAR) DO CODER[ch]:=ch END
END init;

PROCEDURE get_char(VAL s: ARRAY OF CHAR;
                   VAR p: INTEGER;
                   VAR c: CHAR): BOOLEAN;
  VAR  del: CHAR;
      no,i: INTEGER;
BEGIN
  WHILE (p<HIGH(s)) & ((s[p]=' ') OR (s[p]=sci.NL)) DO p:=p+1 END;
  IF ((s[p]="'") OR (s[p]='"')) & (p+2<=HIGH(s)) THEN
    del:=s[p]; p:=p+1; c:=s[p]; p:=p+1;
    IF s[p]#del THEN RETURN TRUE
    ELSE p:=p+1
    END;
  ELSIF (s[p]>="0") & (s[p]<="7") THEN c:=0c;
    WHILE (c<MAX(CHAR)) & (p<=HIGH(s)) &
          (s[p]>="0") & (s[p]<="7")  DO
      no:=ORD(s[p])-ORD("0"); c:=CHAR(ORD(c)*10b+no);
      p:=p+1
    END;
    IF c>MAX(CHAR) THEN RETURN TRUE END
  ELSE RETURN TRUE
  END;
  RETURN FALSE
END get_char;

PROCEDURE usage;
BEGIN
  tty.print(
    "    set keyboard translation utility program (c) KRONOS\n"
    "      setkb -f file\n"
    "        load translation table from file\n"
    "      setkb -i file\n"
    "        refuse all translations\n");
  tty.print(
    "      setkb pare { pare }\n\n"
    "    pare = one one\n"
    "    one = "'"char"'' | dig8 { dig8 }\n'
    '    dig8 = "0".."7"\n'
    '                           Hady. 05-Mar-90\n');
  HALT(1)
END usage;

PROCEDURE read;
  VAR file: bio.FILE;
      path: bio.PATHs;
      buff: DYNARR OF CHAR;
     p,eof: INTEGER;
       f,t: CHAR;
BEGIN
  IF HIGH(arg.words)<0 THEN usage END;
  bio.open(file,arg.words[0],"r"); check;
  eof:=bio.eof(file);
  NEW(buff,eof);
  bio.get(file,buff,eof); check;
  p:=0;
  WHILE NOT get_char(buff,p,f) & NOT get_char(buff,p,t) DO
    set_code(f,t)
  END;
  IF p<eof-1 THEN usage END
END read;

PROCEDURE from_args;
  VAR i,p: INTEGER;
      f,t: CHAR;
BEGIN
  i:=0;
  WHILE i<HIGH(arg.words) DO p:=0;
    IF get_char(arg.words[i],p,f) THEN usage END;
    p:=0; i:=i+1;
    IF get_char(arg.words[i],p,t) THEN usage END;
    set_code(f,t); i:=i+1;
  END
END from_args;

BEGIN
  get_coder;
  IF    arg.flag("-","i") THEN init
  ELSIF arg.flag("-","f") THEN read
  ELSIF arg.flag("-","h") THEN usage
  ELSE
    IF HIGH(arg.words)<1 THEN usage END;
    from_args;
  END;
  set_coder();
END setkb.

"." ">"
"," "<"
"*" ":"
":" "*"
