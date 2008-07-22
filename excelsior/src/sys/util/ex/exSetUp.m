IMPLEMENTATION MODULE exSetUp; (* Leo 11-Jun-87. (c) KRONOS *)

IMPORT  sys: SYSTEM;
IMPORT  mac: exMacro;
IMPORT  tty: Terminal;
IMPORT kbrd: Keyboard;
IMPORT  err: defErrors;

IMPORT  bio: BIO;
IMPORT  dia: strEditor;
IMPORT heap: Heap;
IMPORT  mcd: defCodes;
IMPORT  env: tskEnv;
IMPORT  lex: Lexicon;

PROCEDURE move(a,b,c: sys.ADDRESS); CODE mcd.move END move;

------------------------  INPUT/OUTPUT  -----------------------
                        ----------------

CONST setup_file = "EX.SETUP";

VAR io: BOOLEAN;

PROCEDURE bottom; BEGIN tty.set_pos(tty.state^.lines-1,0) END bottom;

PROCEDURE error(VAL f: ARRAY OF CHAR; SEQ a: sys.WORD);
  VAR ch: CHAR;
BEGIN
  bottom;
  IF NOT bio.done THEN tty.print("#i/o error: ") ELSE tty.print("#") END;
  tty.print(f,a);
  IF NOT bio.done THEN tty.perror(bio.error," %%s") END;
  tty.set_pos(tty.state^.lines-1,tty.state^.columns-13);
  tty.print("HIT ANY KEY");
  kbrd.read(ch)
END error;

PROCEDURE clear_err;
BEGIN bottom; tty.erase_line(0) END clear_err;

PROCEDURE done;
BEGIN clear_err; tty.print("DONE") END done;

PROCEDURE close(f: bio.FILE);
BEGIN
  bio.close(f);
  IF NOT bio.done THEN error("can't"' close file "%s"',setup_file) END;
END close;

TYPE BUFFER = ARRAY [0..0FFFFh] OF CHAR;
    buf_ptr = POINTER TO BUFFER;

PROCEDURE read_macros(f: bio.FILE);

  VAR buff: buf_ptr;
       ptr: buf_ptr;
       eof: INTEGER;
       i,j: INTEGER;
      type: INTEGER;
      size: INTEGER;
        ch: CHAR;

  PROCEDURE exit;
  BEGIN IF buff#NIL THEN heap.DEALLOCATE(buff,(eof+3) DIV 4) END END exit;

BEGIN buff:=NIL;
  eof:=bio.eof(f);
  IF eof<=256 THEN RETURN END;
  heap.ALLOCATE(buff,(eof+3) DIV 4);
  IF buff=NIL THEN error("no memory for buffer"); RETURN END;
  bio.read(f,buff,eof);
  IF NOT bio.done THEN
    error("can't"' read file "%s"',setup_file); exit; RETURN
  END;
  i:=256;
  REPEAT
    type:=ORD(buff^[i]); INC(i);  ch:=buff^[i]; INC(i);
    size:=ORD(buff^[i]); INC(i,2);
    IF    type=0CCh THEN mac.newGmacro(ch,ptr,size);
    ELSIF type=0BBh THEN mac.newBmacro(ch,ptr,size);
    ELSIF type=088h THEN mac.newSmacro(ch,ptr,size);
    ELSE error('illegal macros in file "%s"',setup_file); exit; RETURN
    END;
    j:=0;
    WHILE size>0 DO ptr^[j]:=buff^[i]; INC(i); INC(j); DEC(size) END;
  UNTIL i>=eof;
  exit;
END read_macros;

PROCEDURE write_macros(f: bio.FILE);

  VAR buff: buf_ptr;
       ptr: buf_ptr;
       eof: INTEGER;
         i: INTEGER;
      type: INTEGER;
      size: INTEGER;
        ch: CHAR;

  PROCEDURE exit;
  BEGIN IF buff#NIL THEN heap.DEALLOCATE(buff,(eof+3) DIV 4) END END exit;

  PROCEDURE put(ch: CHAR; type,size: INTEGER; ptr: buf_ptr);
    VAR j: INTEGER;
  BEGIN
    ASSERT((0<size) & (size<=255));
    ASSERT(i+4+size<eof);
    ASSERT(ptr#NIL);
    buff^[i]:=CHAR(type);  INC(i);
    buff^[i]:=ch;          INC(i);
    buff^[i]:=CHAR(size);  INC(i);
    buff^[i]:=0c;          INC(i);
    j:=0;
    WHILE size>0 DO buff^[i]:=ptr^[j]; INC(i); INC(j); DEC(size) END;
  END put;

BEGIN eof:=256; buff:=NIL;
  FOR ch:=0c TO 377c DO
    mac.getGmacro(ch,ptr,size); eof:=eof+size+4;
    mac.getBmacro(ch,ptr,size); eof:=eof+size+4;
    mac.getSmacro(ch,ptr,size); eof:=eof+size+4;
  END;
  heap.ALLOCATE(buff,(eof+3) DIV 4);
  IF buff=NIL THEN error("no memory for buffer"); RETURN END;
  FOR i:=0 TO 255 DO buff^[i]:=0c END;
  i:=256;
  FOR ch:=0c TO 377c DO
    mac.getGmacro(ch,ptr,size);
    IF size>0 THEN put(ch,0CCh,size,ptr) END;
    mac.getBmacro(ch,ptr,size);
    IF size>0 THEN put(ch,0BBh,size,ptr) END;
    mac.getSmacro(ch,ptr,size);
    IF size>0 THEN put(ch,088h,size,ptr) END;
  END;
  bio.fwrite(f,buff,BYTES(public),i-BYTES(public));
  IF NOT bio.done THEN
    error("can't"' write file "%s"',setup_file); exit; RETURN
  END;
  exit;
END write_macros;

---------------------------  SETUP  ---------------------------
                           ---------
PROCEDURE original;
BEGIN
  WITH public DO
    high  :=tty.state^.lines;           width :=tty.state^.columns;
    adjust:=08;              info  :=TRUE;
    ins   :=TRUE;            bell  :=TRUE;
    upmargin:=2;             dwmargin:=2;
    pagesize:=high-9;
    fmargin:=5;       lmargin:=0;     rmargin:=63;
  END;
END original;

VAR ETC: bio.PATHs;

PROCEDURE recall;
  VAR f: bio.FILE;
BEGIN
  bio.lookup(ETC,f,setup_file,'r');
  IF NOT bio.done THEN
    error("can't"' lookup file "%s"',setup_file); original; RETURN
  END;
  bio.read(f,sys.ADR(public),BYTES(public));
  IF NOT bio.done THEN
    error("can't"' read file "%s"',setup_file);
    original; RETURN
  ELSE
    WITH public DO
      IF (high <4 ) OR (high >tty.state^.lines)   THEN high :=tty.state^.lines   END;
      IF (width<10) OR (width>tty.state^.columns) THEN width:=tty.state^.columns END;
    END;
  END;
  bio.seek(f,0,0);
  IF NOT bio.done THEN
    error('seek error in "%s"',setup_file);
  ELSE read_macros(f);
  END;
  close(f);
END recall;

PROCEDURE save;
  VAR f: bio.FILE;
BEGIN
  bio.create(f,setup_file,'w',0);
  IF NOT bio.done THEN
    error("can't"' create file "%s"',setup_file); RETURN
  END;
  bio.write(f,sys.ADR(public),BYTES(public));
  IF NOT bio.done THEN
    error("can't"' write  file "%s"',setup_file); close(f); RETURN
  END;
  write_macros(f);
  bio.end(f,bio.pos(f));
  IF NOT bio.done THEN
    error("can't"' set eof in file "%s"',setup_file);
  END;
  close(f);
  IF bio.done THEN done END;
END save;

PROCEDURE install;
  VAR i: INTEGER;
   size: INTEGER;
    ptr: buf_ptr;
     ch: CHAR;
    str: ARRAY [0..255] OF CHAR;
    pro: ARRAY [0..7] OF CHAR;
BEGIN
  tty.set_pos(19,15); tty.print("?");
  REPEAT kbrd.read(ch) UNTIL (40c<ch) & (ch<177c) OR (ch=33c);
  IF ch=33c THEN RETURN END;
  tty.set_pos(19,15); tty.print(" %c",ch);
  tty.set_pos(20,6);
  tty.print("+-----------------------------------------------------+");
  tty.set_pos(21,6);
  tty.print("|                                                     |");
  tty.set_pos(22,6);
  tty.print("+-----------------------------------------------------+");
  pro:="  = "; pro[0]:=ch;
  dia.edit_str(pro,str,21,8,58,NIL);
  i:=0;
  WHILE (i<HIGH(str)) & (str[i]#0c) DO INC(i) END;
  size:=i; i:=0;
  mac.newSmacro(ch,ptr,size);
  WHILE size>0 DO ptr^[i]:=str[i]; INC(i); DEC(size) END;
END install;

PROCEDURE show_setup;
BEGIN
  tty.home;
              --01234567890123456789012
  WITH public DO
    tty.print("    use ARROWS to move   \n");
    tty.print("                         \n");
    tty.print(" .---------------------. \n");
    tty.print(" |    EXIT             | \n");                  --  0
    tty.print(" |    LINES     %3d    | \n",high);             --  1
    tty.print(" |    COLUMNS   %3d    | \n",width);            --  2
    tty.print(" |    TAB JUMP  %3d    | \n",adjust);           --  3
    tty.print(" |    UP AREA   %3d    | \n",upmargin);         --  4
    tty.print(" |    DW AREA   %3d    | \n",dwmargin);         --  5
    tty.print(" |    PAGE JUMP %3d    | \n",pagesize);         --  6
    tty.print(" |    INS       %3d    | \n",ins);              --  7
    tty.print(" |    BELL      %3d    | \n",bell);             --  8
    tty.print(" |    INFOLINE  %3d    | \n",info);             --  9
    tty.print(" |    PARAGRAPH %3d    | \n",fmargin);          -- 10
    tty.print(" |    LEFT  MRG %3d    | \n",lmargin);          -- 11
    tty.print(" |    RIGHT MRG %3d    | \n",rmargin);          -- 12
    tty.print(" |    SAVE             | \n");                  -- 13
    tty.print(" |    RECALL           | \n");                  -- 14
    tty.print(" |    ORIGINAL         | \n");                  -- 15
    tty.print(" |    INSTALL          | \n");                  -- 16
    tty.print(" |_____________________| \n");
    tty.print("  CR to select or enter \n");
    tty.print("  ESC to exit           \n");
    tty.print("                        ");
  END;
  clear_err;
END show_setup;

VAR alt: INTEGER;
    key: CHAR;

PROCEDURE select;
BEGIN
  LOOP
    tty.set_pos(3+alt,19);
    kbrd.read(key);
    clear_err;
    tty.set_pos(3+alt,19);
    IF    key=kbrd.up  THEN alt:=alt-1
    ELSIF key=kbrd.dw  THEN alt:=alt+1
    ELSIF key=33c     THEN RETURN
    ELSIF key=15c     THEN RETURN
    ELSIF (alt IN {1..12}) & (ORD(key)-ORD("0") IN {0..9}) THEN RETURN
    ELSE
    END;
    IF alt<0 THEN alt:=16 ELSIF alt>16 THEN alt:=0 END;
  END;
END select;

PROCEDURE num_attr;
  VAR i,n: INTEGER;
BEGIN
  tty.set_pos(3+alt,19); tty.print("=");
  n:=0;
  REPEAT
    i:=ORD(key)-ORD("0");
    IF (i IN {0..9}) & (n*10+i<=99) THEN
      n:=n*10+i;
    ELSIF key=kbrd.del THEN
      n:=n DIV 10;
    END;
    tty.set_pos(3+alt,19); tty.print("=%d ",n);
    tty.set_pos(3+alt,19); tty.print("=%d",n);
    kbrd.read(key);
  UNTIL key=15c;
  tty.set_pos(3+alt,19); tty.print("    ");
  WITH public DO
    CASE alt OF
    | 1: high    :=n
    | 2: width   :=n
    | 3: adjust  :=n
    | 4: upmargin:=n
    | 5: dwmargin:=n
    | 6: pagesize:=n
    | 7: ins     :=n#0;  n:=ORD(n#0);
    | 8: bell    :=n#0;  n:=ORD(n#0);
    | 9: info    :=n#0;  n:=ORD(n#0);
    |10: fmargin :=n
    |11: lmargin :=n
    |12: rmargin :=n
    END;
    IF (high <4 ) OR (high >tty.state^.lines) THEN
      high:=tty.state^.lines
    END;
    IF (width<10) OR (width>tty.state^.columns) THEN
      width:=tty.state^.columns
    END;
  END;
  tty.set_pos(3+alt,16); tty.print("%3d",n);
END num_attr;

PROCEDURE set_up;
BEGIN
  show_setup;
  LOOP
    select;
    IF (key=33c) OR (alt=0) THEN RETURN END;
    IF    alt IN {1..12} THEN num_attr
    ELSIF alt IN {13..15} THEN
      IF    alt=13 THEN save
      ELSIF alt=14 THEN recall;   show_setup;
      ELSIF alt=15 THEN original; show_setup;
      ELSE
      END;
      IF bio.done THEN done END;
    ELSIF alt=16 THEN install;  show_setup;
    END;
  END;
END set_up;

BEGIN
  bio.get_paths(ETC,env.etc);
  IF NOT bio.done THEN
    error("can't"' open paths "%s"',env.etc); ETC:=bio.here
  END;
  recall; alt:=0;
END exSetUp.
