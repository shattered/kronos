MODULE prom; (* Sem 24-Oct-86. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR, ADDRESS;

IMPORT tty : Terminal;
IMPORT key : Keyboard;
IMPORT bio : BIO;
IMPORT arg : tskArgs;
IMPORT inp : strEditor;
IMPORT str : Strings;

FROM Gtek       IMPORT  Read, Write, Clear?, Init;
FROM defCodes   IMPORT  move;

VAR
  rom         : ARRAY [0..4096*2*16-1] OF CHAR;
  rom_size    : INTEGER;
  skew        : INTEGER;
  file_name   : ARRAY [0..255] OF CHAR;
  file        : bio.FILE;
  RomNo       : INTEGER;
  First,Last  : INTEGER;
  err_cnt     : INTEGER;
  cmd_desc    : inp.descriptor;
  command     : ARRAY [0..255] OF CHAR;
  cmd_pos     : INTEGER;

PROCEDURE MOVE(t,f: ADDRESS; s: INTEGER); CODE move END MOVE;

PROCEDURE AskInt(s: ARRAY OF CHAR; VAR i: INTEGER): BOOLEAN;
  VAR ln: ARRAY [0..79] OF CHAR; n: INTEGER;
BEGIN
  inp.read_str(s,ln,cmd_desc,15c); tty.print('\n');
  IF ln='' THEN RETURN TRUE END;
  i:=0; n:=0;
  WHILE (ln[n]>='0')&(ln[n]<='9') DO
    i:=i*10+ORD(ln[n])-ORD('0'); INC(n);
  END;
  RETURN ln[n]#0c;
END AskInt;

PROCEDURE io_chk;
BEGIN
  ASSERT(bio.done);
END io_chk;

PROCEDURE WriteFile;
BEGIN
  IF (First*skew MOD 4) # 0 THEN
    tty.print('Can"t write %4$h-%4$h\n',First,Last); RETURN;
  END;
  inp.read_str('Output file name: ',file_name,cmd_desc,15c); tty.print('\n');
  bio.create(file,file_name,'w',(Last-First+1)*skew); io_chk;
  bio.write(file,ADR(rom)+First*skew DIV 4,(Last-First+1)*skew); io_chk;
  bio.close(file); io_chk;
END WriteFile;

PROCEDURE Change;
  VAR ln,ln1: ARRAY [0..79] OF CHAR; i,j,k: INTEGER; ok: BOOLEAN;
BEGIN
  ln:='0';
  inp.read_str('adr=',ln,cmd_desc,15c);
  str.append(ln,'h');
  j:=0;
  str.iscan(i,ln,j,ok);
  IF NOT ok OR (ln[j]#0c) THEN tty.print('Illegal number\n'); RETURN END;
  LOOP
    IF (i<0) OR (i>HIGH(rom)) THEN EXIT END;
    str.print(ln1,'\radr=%4$h %2$h ',i,rom[i*skew+RomNo]);
    str.print(ln,'%3$h',rom[i*skew+RomNo]);
    inp.read_str(ln1,ln,cmd_desc,15c); tty.print('\n');
    IF ln[0]='.' THEN RETURN END;
    IF ln[0]#0c THEN
      str.append(ln,'h');
      k:=0;
      str.iscan(j,ln,k,ok);
      IF NOT ok OR (ln[k]#0c) THEN tty.print('Illegal number\n'); RETURN END;
      rom[i*skew+RomNo]:=CHAR(j);
    END;
    INC(i);
  END;
END Change;

PROCEDURE err(s: ARRAY OF CHAR);
BEGIN
  tty.print('*******  %s\n',s);
  INC(err_cnt);
END err;

PROCEDURE get(i: INTEGER): INTEGER;
BEGIN
  IF i MOD 128 = 0 THEN tty.print("\r%4$h\r",i) END;
  RETURN INTEGER(rom[i*skew+RomNo]);
END get;

PROCEDURE set(i,j: INTEGER);
BEGIN
  rom[i*skew+RomNo]:=CHAR(j);
  IF i MOD 128 = 0 THEN tty.print("\r%4$h\r",i) END;
END set;

PROCEDURE cmp(i,j: INTEGER);
BEGIN
  IF i MOD 128 = 0 THEN tty.print("\r%4$h\r",i) END;
  IF  rom[i*skew+RomNo]#CHAR(j) THEN
    tty.print('address %4$h, rom %2$h, memory %2$h\n',i,j,rom[i*skew+RomNo]);
    INC(err_cnt);
  END;
END cmp;

PROCEDURE vrf(i,j: INTEGER);
BEGIN
  IF i MOD 128 = 0 THEN tty.print("\r%4$h\r",i) END;
  IF  BITSET(j)*BITSET(rom[i*skew+RomNo])#BITSET(rom[i*skew+RomNo]) THEN
    tty.print('address %4$h, rom %2$h, memory %2$h\n',i,j,rom[i*skew+RomNo]);
    INC(err_cnt);
  END;
END vrf;

PROCEDURE cmd_num(VAR n: INTEGER): BOOLEAN;
  VAR ok: BOOLEAN;
BEGIN
  str.iscan(n,command,cmd_pos,ok);
  IF NOT ok THEN tty.print('Illegal number.\n') END;
  RETURN ok;
END cmd_num;

PROCEDURE cmd_name(): CHAR;
  VAR fr: INTEGER;
BEGIN
  str.skip(command,cmd_pos,' ');
  fr:=cmd_pos;
  str.search(command,cmd_pos,' ');
  IF fr=cmd_pos THEN RETURN 0c END;
  RETURN CAP(command[fr]);
END cmd_name;

VAR
  i,j : INTEGER;
  type: CHAR;

BEGIN
  inp.new(cmd_desc,8);
  IF cmd_desc=NIL THEN tty.print('No memory.\n'); HALT END;
  rom[0]:=377c; rom[1]:=377c; rom[2]:=377c; rom[3]:=377c;
  MOVE(ADR(rom)+1,ADR(rom),SIZE(rom)-1);
  Last:=0; RomNo:=0; First:=0; skew:=8;
  IF HIGH(arg.words)<0 THEN
    file_name:=''; rom_size:=0;
  ELSE
    str.copy(file_name,arg.words[0]);
    bio.open(file,file_name,'r'); io_chk;
    rom_size:=bio.eof(file);
    IF rom_size>BYTES(rom) THEN
      tty.print('Too large file "%s", no memory.\n',file_name); HALT;
    END;
    bio.read(file,ADR(rom),rom_size); io_chk;
    Last:=rom_size DIV skew -1;
    IF Last<0 THEN Last:=0 END;
    bio.close(file); io_chk;
    tty.print('File "%s" loaded.\n',file_name);
  END;
  REPEAT
    tty.print("EPROM SELECTION MENU\n");
    tty.print("    NMOS        NMOS       CMOS        EEPROM     W/ADAPT \n");
    tty.print("A - 2758    G - 2508   L - 27C16   P - 5213   R - 874x-1K \n");
    tty.print("B - 2716    H - 2516   M - 27C32   Q - X2816A S - 874x-2K \n");
    tty.print("C - 2732    I - 2532   N - MC6716  X - 48016  T - 874xH-1K\n");
    tty.print("D - 2732A   J - 2564   O - F27C64  Y - I2816A U - 874xH-2K\n");
    tty.print("E - 2764    K - i68766 0 - I27C64  3 - I2817A V - 8751    \n");
    tty.print("1 - i2764A             8 - F27C256 9 - X2864A W - 8755    \n");
    tty.print("F - 27128\n");
    tty.print("2 - i27128A\n");
    tty.print("Z - i27256\n");
    tty.print("7 - i27512\n");
    tty.print("ENTER SELECTION ->");
    key.read(type);
    type:=CAP(type);
    tty.print("%c\n",type);
  UNTIL Init(type);
  err_cnt:=0;
  LOOP
    IF skew#8 THEN
      tty.print('(skew %d) ',skew);
    END;
    IF err_cnt>0 THEN tty.print('%d errors.\n',err_cnt); err_cnt:=0 END;
    tty.print('%1d [%4$h..%4$h]',RomNo,First,Last);
    inp.read_str(' ?',command,cmd_desc,15c); tty.print('\n'); cmd_pos:=0;
    CASE cmd_name() OF
      'P': Write(First,Last,get,err);
     |'R': Read(First,Last,set,err);
     |'W': WriteFile;
     |'C': Read(First,Last,cmp,err);
     |'V': Read(First,Last,vrf,err);
     |'E': Clear?(err);
     |'S': Change;
     |'B': EXIT;
     |'D': IF cmd_num(i) THEN
             IF (i<1) OR (i>HIGH(rom)) THEN
               tty.print("Out of range\n");
             ELSE
               skew:=i;
             END;
           END;
     |'F': IF cmd_num(i) THEN
             IF (i<0) OR (i>HIGH(rom)) THEN
               tty.print("Out of range\n");
             ELSE
               First:=i;
             END;
           END;
     |'L': IF cmd_num(i) THEN
             IF (i<0) OR (i>HIGH(rom)) THEN
               tty.print("Out of range\n");
             ELSE
               Last:=i;
             END;
           END;
     |'N': IF cmd_num(i) THEN
             IF NOT (i IN {0..7}) THEN
               tty.print('Must be in 0..7\n')
             ELSE
               RomNo:=i;
             END;
           END;
     |'A': Clear?(err);
           IF err_cnt=0 THEN Write(First,Last,get,err) END;
           IF err_cnt=0 THEN Read(First,Last,cmp,err) END;
           IF err_cnt=0 THEN RomNo:=(RomNo+1) MOD 8 END;
     |0c:
    ELSE
      tty.print(' P  programm\n');
      tty.print(' R  read from rom to memory\n');
      tty.print(' N  set rom number\n');
      tty.print(' E  verify rom erasure\n');
      tty.print(' C  compare rom with memory\n');
      tty.print(' W  write memory to file\n');
      tty.print(' S  change data in memory\n');
      tty.print(' V  verify rom\n');
      tty.print(' F  set first adr\n');
      tty.print(' L  set last adr\n');
      tty.print(' D  set skew factor\n');
      tty.print(' A  verify erasure, programm and compare\n');
      tty.print(' B  bye\n');
    END;
    IF RomNo<0 THEN RomNo:=0 END;
    IF RomNo>=skew THEN RomNo:=skew-1 END;
    IF First<0 THEN First:=0 END;
    IF Last<First THEN Last:=First END;
  END;
END prom.
