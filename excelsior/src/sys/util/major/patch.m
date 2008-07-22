MODULE patch; (* 19-Dec-88. (c) KRONOS *)

IMPORT args: tskArgs;           IMPORT  sys: SYSTEM;
IMPORT  tty: Terminal;          IMPORT  key: Keyboard;
IMPORT  mem: Heap;              IMPORT  bio: BIO;
IMPORT  err: defErrors;         IMPORT  std: StdIO;
IMPORT  low: lowLevel;          IMPORT  str: Strings;

VAR name: ARRAY [0..127] OF CHAR;
     eof: INTEGER;
    file: bio.FILE;
    buff: sys.ADDRESS;

PROCEDURE help;
BEGIN
  std.print("\n"
           " patch [options] [file_name]\n"
           "       options:\n"
           "  -w   enable write corrected file\n"
           "  -mem show and edit memory\n\n"
          );
  std.print("       in command mode:\n"
           "  0..9,A..F change word contence\n"
           "  /         dereference word\n"
           "  W         write & quit\n"
           "  Q,ESC     quit\n"
           "  LF,DOWN   increment adr\n"
           "  CR,UP     decrement adr\n"
          );
END help;

PROCEDURE edit(): CHAR;
  VAR adr,new: sys.ADDRESS;
       dir,ch: CHAR;
    i,c,x,pos: INTEGER;
          pha: sys.ADDRESS;
BEGIN
  adr:=0; dir:=" ";
  LOOP
    IF adr<0 THEN adr:=0 ELSIF adr>=eof THEN adr:=eof-1 END;
    new:=0; pha:=adr+buff;
    x:=pha^;
    tty.print("\n%c%$8h=%$8h ",dir,adr,x);
    pos:=0;
    LOOP
      key.read(ch);
      IF ("a"<=ch) & (ch<="z") THEN ch:=CAP(ch) END;
      CASE ch OF
      |"0".."9": IF pos<8 THEN
                   new:=INTEGER(new<<4)+ORD(ch)-ORD("0");    INC(pos);
                 END;
      |"A".."F": IF pos<8 THEN
                   new:=INTEGER(new<<4)+ORD(ch)-ORD("A")+10; INC(pos);
                 END;
      |"'",'"' : tty.print(" ");
                 FOR i:=0 TO 3 DO c:=x MOD 256; x:=x>>8;
                   IF c MOD 128 >= 32 THEN ch:=CHAR(c) ELSE ch:='?' END;
                   tty.print("%c",ch);
                 END;
                 IF adr<eof THEN INC(adr); dir:=" "; EXIT END;
      |key.back: IF pos>0 THEN DEC(pos);
                   new:=sys.ADDRESS( (BITSET(new)-{0..3})>>4 );
                 END;
      |key.dw
      ,key.lf : IF pos>0 THEN pha^:=new END;
                IF adr<eof THEN INC(adr); dir:=" "; EXIT END;
      |key.up
      ,key.cr : IF pos>0 THEN pha^:=new END;
                IF adr>0 THEN DEC(adr); dir:="-"; EXIT END;
      |"/"    : IF pos=0 THEN new:=x END; tty.print("/");
                i:=INTEGER(BITSET(new)-{31});
                IF (i>=0) & (i<=eof) THEN adr:=i; dir:=" "; EXIT END;
      |"Q",33c: tty.print("\n"); RETURN "Q"
      |"W"    : tty.print("\n"); RETURN "W"
      |"H"    : help;
      ELSE
      END;
      IF pos>8 THEN pos:=8 END;
      IF pos#0 THEN
        tty.print("\r%c%$8h=%$8h %$*h",dir,adr,x,pos,new);
      ELSE
        tty.print("\r%c%$8h=%$8h ",dir,adr,x);
      END; tty.erase_line(0)
    END;
  END;
END edit;

PROCEDURE query(VAL s: ARRAY OF CHAR): BOOLEAN;
  VAR ch: CHAR;
BEGIN
  tty.print("%s",s);
  REPEAT key.read(ch) UNTIL (CAP(ch)='Y') OR (CAP(ch)='N');
  tty.print("%c\n",ch);
  RETURN CAP(ch)='Y';
END query;

VAR feof: INTEGER;

BEGIN
  IF HIGH(args.words)<0 THEN name:="" ELSE str.copy(name,args.words[0]) END;
  IF args.flag('-','m') & args.flag('-','e') THEN
    buff:=0; eof:=7FFFFFFFh;
    IF edit()#0c THEN END;
    HALT
  END;
  IF args.flag('-','h') OR (name="") THEN help; HALT END;
  bio.open(file,name,"m");
  IF NOT bio.done THEN
    tty.perror(bio.error,'lookup("%s"): %%s\n',name); HALT(bio.error)
  END;
  feof:=bio.eof(file);
  eof:=(feof+3) DIV 4;
  mem.ALLOCATE(buff,eof);
  IF buff=NIL THEN std.print("no memory\n"); HALT(1) END;
  low._zero(buff,eof);
  bio.read(file,buff,feof);
  IF NOT bio.done THEN
    tty.perror(bio.error,'read("%s"): %%s\n',name); HALT(bio.error)
  END;
  IF edit()="W" THEN
    IF args.flag('-','w')
    OR query("You don't use flag -w ; Are you realy sure?")
    THEN
      bio.seek(file,0,0);
      IF NOT bio.done THEN
        tty.perror(bio.error,'seek("%s",0,0): %%s\n',name); HALT(bio.error)
      END;
      bio.write(file,buff,feof);
      IF NOT bio.done THEN
        tty.perror(bio.error,'write("%s"): %%s\n',name); HALT(bio.error)
      END;
    END;
  END;
  bio.close(file);
  IF NOT bio.done THEN
    tty.perror(bio.error,'close("%s"): %%s\n',name); HALT(bio.error)
  END;
END patch.
