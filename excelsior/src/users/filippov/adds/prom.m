MODULE prom; (* Sem 24-Oct-86. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  key: Keyboard;
IMPORT  tty: Terminal;
IMPORT  bio: BIO;
IMPORT  arg: tskArgs;
IMPORT  sed: strEditor;
IMPORT  str: Strings;
IMPORT  Gtek;
IMPORT  cod: defCodes;

VAR rom         : ARRAY [0..4096*2*16-1] OF CHAR;
    skew        : INTEGER;
    romSize     : INTEGER;
    Name        : ARRAY [0..79] OF CHAR;
    inp,out     : bio.FILE;
    string      : ARRAY [0..79] OF CHAR;
    RomNo,First,Last: INTEGER;
    err_cnt     : INTEGER;


PROCEDURE MOVE(t,f: SYSTEM.ADDRESS; s: INTEGER); CODE cod.move END MOVE;

PROCEDURE AskInt(s: ARRAY OF CHAR; VAR i: INTEGER): BOOLEAN;
  VAR ln: ARRAY [0..79] OF CHAR; n: INTEGER;
BEGIN
  ln:="";
  sed.read_str(s,ln,NIL); tty.print('\n');
  IF ln[0]=0c THEN RETURN TRUE END;
  i:=0; n:=0;
  WHILE (ln[n]>='0')&(ln[n]<='9') DO
    i:=i*10+ORD(ln[n])-ORD('0'); INC(n);
  END;
  RETURN ln[n]#0c;
END AskInt;

PROCEDURE WriteFile;
BEGIN
  IF (First*skew MOD 4) # 0 THEN
    tty.print('Can"t write %4$h-%4$h\n',First,Last); RETURN ;
  END;
  Name:="";
  sed.read_str('File name: ',Name,NIL); tty.print('\n');
  bio.create(out,Name,'w',0);
  IF NOT bio.done THEN HALT(bio.error) END;
  bio.write(out,SYSTEM.ADR(rom)+First*skew DIV 4,(Last-First+1)*skew);
  IF NOT bio.done THEN HALT(bio.error) END;
  bio.close(out);
  IF NOT bio.done THEN bio.purge(out); HALT(bio.error) END
END WriteFile;

PROCEDURE Change;
  VAR ln,ln1: ARRAY [0..79] OF CHAR; pos,i,j,k: INTEGER; done: BOOLEAN;
BEGIN
  str.print(ln,'0');
  ln:="";
  sed.read_str('adr=',ln,NIL); str.app(ln,'h');
  pos:=0;
  str.iscan(i,ln,pos,done);
  IF NOT done THEN tty.print('Illegal number\n'); RETURN END;
  LOOP
    IF (i<0) OR (i>HIGH(rom)) THEN EXIT END;
    str.print(ln1,'\radr=%4$h %2$h ',i,rom[i*skew+RomNo]);
    str.print(ln,'%3$h',rom[i*skew+RomNo]);
    ln:="";
    sed.read_str(ln1,ln,NIL); tty.print('\n');
    IF ln[0]='.' THEN RETURN END;
    IF ln[0]#0c THEN
      str.app(ln,'h');
      pos:=0;
      str.iscan(j,ln,pos,done);
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

PROCEDURE Cmd(VAR s: ARRAY OF CHAR): CHAR;
  VAR w: ARRAY [0..79] OF CHAR; i,j: INTEGER; c: CHAR;
BEGIN
  i:=0;
  WHILE (i<=HIGH(s))&(s[i]=' ') DO INC(i) END;
  IF i<=HIGH(s) THEN c:=s[i] END;
  FOR j:=0 TO i DO s[j]:=' ' END;
  RETURN CAP(c)
END Cmd;

VAR i,j,pos: INTEGER;
   type: CHAR;
   done: BOOLEAN;

BEGIN
  rom[0]:=0c; rom[1]:=0c; rom[2]:=0c; rom[3]:=0c;
  MOVE(SYSTEM.ADR(rom)+1,SYSTEM.ADR(rom),SIZE(rom)-1);
  Last:=0; RomNo:=0; First:=0; skew:=8;
  IF HIGH(arg.words)>=0 THEN
    Name:=arg.words[0];
    bio.open(inp,Name,'r');        IF NOT bio.done THEN HALT(bio.error) END;
    bio.get(inp,rom,bio.eof(inp)); IF NOT bio.done THEN HALT(bio.error) END;
    Last:=bio.eof(inp) DIV skew -1; IF Last<0 THEN Last:=0 END;
    bio.close(inp);                IF NOT bio.done THEN HALT(bio.error) END;
    tty.print('File %s loaded.\n',Name)
  END;
  REPEAT
    tty.print("EPROM SELECTION MENU\n");
    tty.print("    NMOS         NMOS        CMOS        EEPROM      W/ADAPT \n");
    tty.print("A - 2758     G - 2508    L - 27C16   P - 5213    R - 874x-1K \n");
    tty.print("B - 2716     H - 2516    M - 27C32   Q - X2816A  S - 874x-2K \n");
    tty.print("C - 2732     I - 2532    N - MC6716  X - 48016   T - 874xH-1K\n");
    tty.print("D - 2732A    J - 2564    O - F27C64  Y - I2816A  U - 874xH-2K\n");
    tty.print("E - 2764     K - i68766  0 - I27C64  3 - I2817A  V - 8751    \n");
    tty.print("1 - i2764A               8 - F27C256 9 - X2864A  W - 8755    \n");
    tty.print("F - 27128\n");
    tty.print("2 - i27128A\n");
    tty.print("Z - i27256\n");
    tty.print("7 - i27512\n");
    tty.print("ENTER SELECTION ->");
    key.read(type);
    IF (type>100c) & (type<=177c) THEN type:=CAP(type) END;
    tty.print("%c\n",type);
  UNTIL Gtek.Init(type);
  err_cnt:=0;
  LOOP
    IF skew#8 THEN
      tty.print('(skew %d) ',skew);
    END;
    IF err_cnt>0 THEN tty.print('%d errors.\n',err_cnt); err_cnt:=0 END;
    tty.print('%1d [%4$h..%4$h]',RomNo,First,Last);
    string:="";
    sed.read_str(' ?',string,NIL); tty.print('\n');
    CASE Cmd(string) OF
      'P': Gtek.Write(First,Last,get,err);
     |'R': Gtek.Read(First,Last,set,err);
     |'W': WriteFile;
     |'C': Gtek.Read(First,Last,cmp,err);
     |'V': Gtek.Read(First,Last,vrf,err);
     |'E': Gtek.Clear?(err);
     |'S': Change;
     |'B': EXIT;
     |'D': pos:=0;
           str.iscan(skew,string,pos,done);
           IF NOT done THEN tty.print('Illegal number\n') END;
           IF (skew<1) OR (skew>HIGH(rom)) THEN
             tty.print("Out of range\n"); skew:=8;
           END;
     |'F':
           pos:=0;
           str.iscan(First,string,pos,done);
           IF NOT done THEN tty.print('Illegal number\n') END;
           IF (First<0) OR (First>HIGH(rom)) THEN
             tty.print("Out of range\n"); First:=0;
           END;
     |'L': pos:=0;
           str.iscan(Last,string,pos,done);
           IF NOT done THEN tty.print('Illegal number\n') END;
           IF (Last<0) OR (Last>HIGH(rom)) THEN
             tty.print("Out of range\n"); Last:=0;
           END;
     |'N': pos:=0;
           str.iscan(RomNo,string,pos,done);
           IF NOT done THEN tty.print('Illegal number\n') END;
           IF NOT (RomNo IN {0..7}) THEN
             RomNo:=0; tty.print('Must be in 0..7\n')
           END;
     |'A': Gtek.Clear?(err);
           IF err_cnt=0 THEN Gtek.Write(First,Last,get,err) END;
           IF err_cnt=0 THEN Gtek.Read(First,Last,cmp,err) END;
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
  Gtek.Stop;
END prom.
