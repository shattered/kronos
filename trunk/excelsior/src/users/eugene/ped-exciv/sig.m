MODULE sig; (* 13-Feb-87. (c) KRONOS *)

FROM Terminal   IMPORT  print, Read;
FROM Args       IMPORT  TakeWord, ScanFlags, Flag?;
FROM Pattern    IMPORT  Match;
FROM Model      IMPORT  Object, Objects, Iterate, String, Segment,
                        NewObject, Tie, KillObject, KillList, UnTie, SigTypes;
IMPORT  mm: ModelMisc;
FROM ModelIO    IMPORT  ReadModel, WriteModel;
FROM ModelPbl   IMPORT  Exception?, Reaction, Message;
FROM Edit       IMPORT  ReadString;
FROM Image      IMPORT  image0;
FROM Keyboard   IMPORT  up, dw;
IMPORT StdIO;
IMPORT  mcd: mCodeMnem;

VAR e           : Reaction;
    Name        : ARRAY [0..79] OF CHAR;
    mdl         : Object;
    Patt        : ARRAY [0..79] OF CHAR;
    FindName    : String;
    FindSig     : Object;
    Free        : Object;
    cmds        : ARRAY [0..7] OF ARRAY [0..15] OF CHAR;
    sigs        : ARRAY [0..4999] OF Object;
    sig_no      : INTEGER;
    sig_ptr     : INTEGER;

PROCEDURE Tag(o: Object): Objects;
CODE 0 mcd.lxb END Tag;

PROCEDURE FindCmd(cmd: ARRAY OF CHAR): INTEGER;
  VAR i,j,k: INTEGER;
BEGIN
  j:=-1;
  FOR i:=0 TO HIGH(cmds) DO
    k:=0;
    LOOP
      IF cmds[i,k]=0c THEN
        IF cmd[k]#0c THEN EXIT END;
        IF j>=0 THEN RETURN -2 END;
        j:=i; EXIT;
      ELSE
        IF cmd[k]=0c THEN IF j>=0 THEN RETURN -2 ELSE j:=i; EXIT END END;
        IF cmd[k]=cmds[i,k] THEN INC(k) ELSE EXIT END;
      END;
    END;
  END;
  RETURN j;
END FindCmd;

PROCEDURE DelPin(p: Object; d: INTEGER);
BEGIN
  IF Tag(p)#pin THEN RETURN END;
  IF p^.Signal=FindSig THEN
    p^.Signal:=Free;
    IF Free#NIL THEN Tie(Free^.TiedPins,p) END;
  END;
END DelPin;

PROCEDURE EditSignal;
  VAR cmd: ARRAY [0..79] OF CHAR; i: INTEGER; ch: CHAR;
BEGIN
  cmds[0]:='fixed';
  cmds[1]:='fantom';
  cmds[2]:='power';
  cmds[3]:='bye';
  cmds[4]:='name';
  cmds[5]:='kill';
  cmds[6]:='next';
  cmds[7]:='list';
  LOOP
    print('Сигнал %s.',FindSig^.Name);
    print(' Атрибуты :');
    IF fixed  IN FindSig^.sType THEN print(' fixed') END;
    IF fantom IN FindSig^.sType THEN print(' fantom') END;
    IF power  IN FindSig^.sType THEN print(' power') END;
    print('\n');
    ReadString('>',cmd); print('\n');
    i:=FindCmd(cmd);
    CASE i OF
      0: IF fixed IN FindSig^.sType THEN
           EXCL(FindSig^.sType,fixed)
         ELSE
           INCL(FindSig^.sType,fixed)
         END;
     |1: IF fantom IN FindSig^.sType THEN
           EXCL(FindSig^.sType,fantom)
         ELSE
           INCL(FindSig^.sType,fantom)
         END;
     |2: IF power IN FindSig^.sType THEN
           EXCL(FindSig^.sType,power)
         ELSE
           INCL(FindSig^.sType,power)
         END;
     |3: RETURN
     |4: ReadString('New name:',FindSig^.Name); print('\n');
     |5: IF FindSig=Free THEN
           print('Can not kill signal "..free.."\n');
         ELSE
           print('Kill signal %s? ',FindSig^.Name);
           ch:=Read();
           IF (ch='y') OR (ch='Y') THEN
             print('yes\n');
             UnTie(mdl^.All,FindSig);
             Iterate(mdl^.All,DelPin,0);
             Iterate(FindSig^.TiedPins,DelPin,0);
             KillObject(FindSig^.ChainB);
             KillList  (FindSig^.TiedPins);
             KillObject(FindSig);
             DEC(sig_no);
             IF sig_no=0 THEN
               RETURN
             ELSE
               FOR i:=sig_ptr TO sig_no-1 DO sigs[i]:=sigs[i+1] END;
               IF sig_ptr>=sig_no THEN sig_ptr:=0 END;
               FindSig:=sigs[sig_ptr]; FindName:=FindSig^.Name;
             END;
           ELSE
             print('no\n');
           END;
         END;
     |6: INC(sig_ptr); IF sig_ptr>=sig_no THEN sig_ptr:=0 END;
         FindSig:=sigs[sig_ptr]; FindName:=FindSig^.Name;
     |7: print('--------------------------------------------------------\n');
         FOR i:=0 TO sig_no-1 DO
           print('%-15s ',sigs[i]^.Name);
           IF (i MOD 4)=3 THEN print('\n') END;
         END;
         print('\n');
         print('--------------------------------------------------------\n');
    ELSE
      print('Непонятная команда.\n');
    END;
  END;
END EditSignal;

PROCEDURE FindSignal(o: Object; d: INTEGER);
BEGIN
  IF Tag(o)#signal THEN RETURN END;
  IF o^.Name#FindName THEN RETURN END;
  FindSig:=o;
END FindSignal;

PROCEDURE AppSig(s: Object; d: INTEGER);
BEGIN
  IF Tag(s)#signal THEN RETURN END;
  IF Match(FindName,s^.Name) THEN
    sigs[sig_no]:=s; INC(sig_no);
  END;
END AppSig;

PROCEDURE SigBlock;
  VAR n,k: INTEGER; s: Object;
BEGIN
  sig_no:=0;
  Iterate(mdl^.All,AppSig,0);
  n:=0;
  REPEAT
    k:=0;
    WHILE n+1<sig_no DO
      IF sigs[n]^.Name>sigs[n+1]^.Name THEN
        s:=sigs[n]; sigs[n]:=sigs[n+1]; sigs[n+1]:=s;
        IF n>1 THEN DEC(n) END;
        INC(k);
      ELSE
        INC(n);
      END;
    END;
  UNTIL k=0;
END SigBlock;

PROCEDURE ListSig(o: Object; d: INTEGER);
BEGIN
  IF Tag(o)#signal THEN RETURN END;
  StdIO.print('Signal : %s\n',o^.Name);
  FindSig:=o;
END ListSig;

PROCEDURE check(o: Object; d: INTEGER);
BEGIN
  IF Tag(o)#signal THEN RETURN END;
  mm.StartConductor(o,FALSE);
  WHILE NOT mm.Empty DO
    IF mm.Size=0 THEN
      StdIO.print('Signal %s cFree=%d Ident=%d x0=%d y0=%d x1=%d y1=%d layer=%{}\n',
                   o^.Name,o^.ChainB^.cFree,mm.Ident,mm.X1,mm.Y1,mm.X2,mm.Y2,mm.Layer);
    END;
    mm.NextConductor;
  END;
END check;

BEGIN
  ScanFlags;
  TakeWord(Name);
  IF Name[0]=0c THEN HALT END;
  IF Exception?(e) THEN print('%s',Message); HALT END;
  mdl:=ReadModel(Name);
  image0(FindName,'GND');
  IF Flag?('l') THEN Iterate(mdl^.All,ListSig,0); HALT END;
  IF Flag?('b') THEN Iterate(mdl^.All,check,0); HALT END;
  FindSig:=NIL;
  FindName:='..free..';
  Iterate(mdl^.All,FindSignal,0);
  Free:=FindSig;
  IF Free=NIL THEN print('Warning: can not find signal "..free.."\n') END;
  LOOP
    ReadString('Имя сигнала: ',FindName); print('\n');
    IF FindName[0]=0c THEN EXIT END;
    SigBlock;
    IF sig_no>0 THEN
      sig_ptr:=0; FindSig:=sigs[sig_ptr]; FindName:=FindSig^.Name;
      EditSignal;
    ELSE
      print('Нет сигнала %s\n',FindName);
    END;
  END;
  WriteModel(mdl);
END sig.
