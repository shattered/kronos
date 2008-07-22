MODULE abc; (* Hady. 29-Apr-88. (c) KRONOS *)

FROM Keyboard   IMPORT  gold,  exit,  silver, dw,    up
                      , space, rtab,  ltab,   break, cr
                      , delln, insln, empty;
FROM Args       IMPORT  ScanFlags, Flag?, TakeWord;

IMPORT Strings,  Heap,     Streams
     , Image,    abcVoc,   StdIO
     , ASCII,    Misc,     abcBuf
     , Terminal, abcTerm,  Keyboard
     , Strings,  abcDefs,  SYSTEM
     , Args,     BIO,      FsPublic
     , FileNames;

CONST Show = Terminal.Show;

CONST PMTW = 'ABC: ';
      PMTC = '>>> ';
      NAME = 'vvv';
      SAVE = 'ABC.SAVE';

TYPE Modes = (from_buff, to_buff, to_file, gld);
     Mode  = SET OF Modes;

CONST all = Mode{from_buff, to_buff, to_file, gld};
      short = Mode{to_file, gld};
      dummy = Mode{};

VAR file: Streams.Stream;
    mode: Mode;
   write: BOOLEAN;
   fName: ARRAY [0..31] OF CHAR;

PROCEDURE Break();
  VAR r: INTEGER;
      s: ARRAY [0..79] OF CHAR;
BEGIN
  Terminal.SetCrs(abcDefs.INFO,0);
  Terminal.WriteString('                                         '
                       '                                         ');
  IF file>=0 THEN r:=Streams.Close(file);
    IF r<0 THEN Strings.Str0(s);
      Streams.Why?(r,s); Terminal.print('%s\n',s)
    END;
  END;
  r:=abcVoc.CloseVoc();
  Terminal.SetCrs(abcDefs.INFO-1,0);
  IF r<0 THEN
    Strings.Str0(s); abcDefs.VisError(r,s);
    Terminal.print('\n%s\n',s)
  END;
  Terminal.print('\n\n\n');
  Terminal.SetMode(TRUE);
  HALT
END Break;

PROCEDURE Message(VAL s: ARRAY OF CHAR; VAR mode: BOOLEAN);
(* -mode- FALSE means query message *)
BEGIN
  abcTerm.WriteMess(s); abcTerm.Bell;
  IF mode THEN
    IF abcTerm.REFRESH THEN abcTerm.RefreshWrk END;
    REPEAT UNTIL Keyboard.Pressed?();
  ELSE
    mode:=StdIO.Query('');
  END;
  abcTerm.RefreshInfo;
END Message;

PROCEDURE Check(err: INTEGER): BOOLEAN;
  VAR s: ARRAY [0..79] OF CHAR;
      r: BOOLEAN;
BEGIN
  r:=FALSE;
  IF err<0 THEN
    Strings.Str1(s,'>> ERROR: ');
    abcDefs.VisError(err, s);
    Strings.AppStr(s,' ЭТО СЕРЬЕЗНО ?');
    Message(s,r);
  END;
  RETURN r
END Check;

PROCEDURE CheckHALT(err: INTEGER);
  VAR i: INTEGER;
      s: ARRAY [0..79] OF CHAR;
BEGIN
  IF Check(err) THEN
    Terminal.SetCrs(23,0);
    i:=abcVoc.CloseVoc();
    IF i<0 THEN
      Strings.Str0(s); abcDefs.VisError(i,s);
      Terminal.print('\n%s\n',s);
    END;
  END;
END CheckHALT;

PROCEDURE NewFile(s: ARRAY OF CHAR);
BEGIN
  Strings.Str1(fName,s);
  IF file>=0 THEN CheckHALT(Streams.Close(file)) END;
  IF Strings.Len(fName)=0 THEN file:=-1
  ELSE
    file:=Streams.Open(fName);
    IF file<0 THEN
      file:=Streams.Create(fName); CheckHALT(file);
    ELSE
      CheckHALT(Streams.Seek(file,0,2));
    END;
  END;
  IF file>=0 THEN abcTerm.File!(fName)
  ELSE abcTerm.File!('');
    IF write THEN write:=FALSE; abcTerm.Write!(write) END;
  END;
END NewFile;

PROCEDURE HELP(scr: BOOLEAN);
  VAR r: BOOLEAN;
BEGIN
  IF NOT scr THEN
  Show(' abc [ИмяВыходногоФайла] [-ribq] ');
  Show('   abc v2.3.4  /05-Dec-88/ (c) KRONOS ');
  Show('   КЛЮЧИ: ');
  Show('     "r","i","b" -- выключение роллинга,');
  Show('                   инфо-строки и звонка ');
  Show(' Подсказка в мониторе утилиты:');
  Show('                 <GOLD><GOLD><h><CR>');
  ELSE
  Terminal.Home; Terminal.Clear;
  Show('   В режиме редактирования строки:');
  Show('     функциональные ключи - как обычно, КРОМЕ:');
  Show('    <UP>,<DW>     -- перемещения маркера ');
  Show('     <BRONZE>     -- клавиатура LATIN/КИРИЛЛ ');
  Show('     <SILVER>     -- запись выбранной строки в файл ');
  Show('   <GOLD><GOLD>   -- переход в командный режим ');
  Show('     <GOLD> <d>   -- удаление выбранной строки из словаря');
  Show('     <GOLD> <e>   -- редактирование выбранной строки ');
  Show('    <INS LINE>    -- опустить текущую строку в буфер  ');
  Show('    <DEL LINE>    -- выбрать строку из буфера ');
  Show('  <PULL LEFT>,<PULL RIGHT> -- передвижение выбранной');
  Show('                строки влево и вправо соответственно');
  Show('     <BREAK>      -- выход без записи состояния');
  Show('      <EXIT>      -- выход с записью состояния');
  Show('   В КОМАНДНОМ РЕЖИМЕ: ');
  Show('   sИмяФайла -- направить выдачу в файл -ИмяФайла-');
  Show('       "i","b","r" -- Info, bell, roll on-off ');
  Show('               "h" -- Help; ');
  Show('               "v" -- Показать буфер');
  Show('               "w" -- Запись в файл on_off');
  abcTerm.WriteMess('PRESS <CR>'); abcTerm.Bell;
  REPEAT UNTIL Keyboard.ReadKey()=cr;
  abcTerm.RefreshScreen;
  END;
END HELP;

CONST peekLen = 20;

VAR shNum: INTEGER;

PROCEDURE PeekString(s: ARRAY OF CHAR): BOOLEAN;
  VAR t: ARRAY [0..peekLen+1] OF CHAR;
      r: BOOLEAN;
BEGIN
  Strings.Str1(t,s); Terminal.SetCrs(shNum,78-peekLen);
  Terminal.ClearLine; Terminal.print('| %s',t); INC(shNum);
  IF shNum=abcDefs.INFO THEN r:=TRUE; shNum:=0;
    LOOP
      Message('PRESS <SPACE BAR> TO CONTINUE; <CR> TO STOP',r);
      CASE Keyboard.ReadKey() OF
        |space: RETURN FALSE
        | cr  : RETURN TRUE
      ELSE
      END;
    END;
  ELSE RETURN FALSE;
  END;
END PeekString;

VAR out: Streams.Stream;

PROCEDURE PutSave(VAR s: ARRAY OF CHAR): BOOLEAN;
BEGIN
  Streams.PutS(out,s); Streams.PutC(out,ASCII.NL); RETURN FALSE
END PutSave;

PROCEDURE saveStr(VAL s: ARRAY OF CHAR);
BEGIN Streams.PutS(out,s); Streams.PutC(out,ASCII.NL);
END saveStr;

PROCEDURE SaveContens(): INTEGER; (* >=0 if o'k *)
BEGIN
  out:=Streams.Create(SAVE);
  IF out<0 THEN RETURN out END;
  Streams.PutS(out,fName); Streams.PutC(out,ASCII.NL);
  IF abcTerm.latin?() THEN Streams.PutC(out,'1')
  ELSE Streams.PutC(out,'0')
  END;
  IF abcTerm.Roll?() THEN Streams.PutC(out,'1')
  ELSE Streams.PutC(out,'0')
  END;
  IF abcTerm.Bell?() THEN Streams.PutC(out,'1')
  ELSE Streams.PutC(out,'0')
  END;
  IF write   THEN Streams.PutC(out,'1')
  ELSE Streams.PutC(out,'0')
  END;
  Streams.PutC(out,ASCII.NL);
  abcTerm.SaveScreen(saveStr);
  abcBuf.ShowBuffer(PutSave);
  Streams.PutC(out,ASCII.NL);
  Streams.PutC(out,ASCII.NL);
  Streams.PutC(out,ASCII.NL);
  RETURN Streams.Close(out);
END SaveContens;

PROCEDURE Finish;
BEGIN
  CheckHALT(SaveContens()); Break();
END Finish;

PROCEDURE showBuffer();
  VAR i: INTEGER;
      r: BOOLEAN;
BEGIN
  shNum:=0; abcTerm.REFRESH:=FALSE;
  abcBuf.ShowBuffer(PeekString);
  IF shNum<abcDefs.INFO THEN
    FOR i:=shNum TO abcDefs.INFO DO
      Terminal.SetCrs(i,78-peekLen);
      Terminal.Write('|'); Terminal.ClearLine;
    END;
    r:=TRUE;
    REPEAT Message('PRESS <SPACE BAR>',r)
    UNTIL Keyboard.ReadKey()#empty;
  END;
  abcTerm.REFRESH:=TRUE;
END showBuffer;

PROCEDURE Execute(s: ARRAY OF CHAR);
  VAR r: BOOLEAN;
      c: CHAR;
BEGIN
  c:=s[0];
  Strings.DelCh(s,0);
  CASE c OF
     's': NewFile(s);
    |'b': abcTerm.Bell!(NOT abcTerm.Bell?());
    |'i': abcTerm.Info!(NOT abcTerm.Info?());
    |'r': abcTerm.Roll!(NOT abcTerm.Roll?());
    |'h': HELP(TRUE);
    |'v': showBuffer; abcTerm.RefreshScreen;
    |'q': CheckHALT(SaveContens()); Break();
    |'w': write:=NOT write;
          IF write & (file<0) THEN
            r:=TRUE;
            Message('НЕ ОТКРЫТ ВЫХОДНОЙ ФАЙЛ ! ',r);
            write:=FALSE
          END;
          abcTerm.Write!(write);
  ELSE
    r:=TRUE; Message(' НЕПОНЯТНАЯ КОМАНДА ',r);
  END;
END Execute;

PROCEDURE Command();
  VAR s: ARRAY [0..79] OF CHAR;
      t: ARRAY [0..79] OF CHAR;
   tmpm: Mode;
BEGIN
  abcTerm.SaveWrk;
  tmpm:=mode; mode:=dummy;
  abcTerm.ReadWork(PMTC,s,abcTerm.new);
  WHILE Strings.Len(s)>0 DO
    Strings.GetWord(s,t);
    IF Strings.Len(t)#0 THEN Execute(t) END;
  END;
  mode:=tmpm;
  abcTerm.RestoreWrk;
END Command;

PROCEDURE ToFile();

  CONST oLen = 14;

  VAR r: BOOLEAN;
      s: ARRAY [0..255] OF CHAR;
    o,t: ARRAY [0..255] OF CHAR;
BEGIN
  IF to_file IN mode THEN
    IF file<0 THEN r:=TRUE;
      Message(' НЕ ОТКРЫТ ВЫХОДНОЙ ФАИЛ ! ',r);
      RETURN
    END;
    abcTerm.GetLine(s);
    Strings.GetWord(s,o);
    IF Strings.Len(o)>oLen THEN
      Image.image0(t,'%s%s',o,s)
    ELSE
      Image.image0(t,'%-14.14s%s',o,s)
    END;
    Streams.PutS(file,t); Streams.PutC(file,ASCII.NL);
    abcTerm.MarkString('w');
  END;
END ToFile;

PROCEDURE DelWord();

  CONST LEN = 30;

  VAR w: abcVoc.Word;
      s: ARRAY [0..255] OF CHAR;
      t: ARRAY [0..79] OF CHAR;
      r: BOOLEAN;
    err: INTEGER;

BEGIN
  abcTerm.GetLine(s);
  IF Strings.Len(s)#0 THEN
    Strings.Str1(t,s);
    IF Strings.Len(t)>LEN THEN
      Strings.Truncate(t,LEN-3); Strings.AppStr(t,'...')
    END;
    Strings.AppStr(t,' -- TO DELEET. ARE YOU SHURE ?');
    r:=FALSE;
    Message(t,r);
    IF r THEN
      Strings.GetWord(s,t);
      err:=abcVoc.GetWord(t,w);
      ASSERT(Strings.Len(t)#0,4Bh);
      IF err<0 THEN CheckHALT(err)
      ELSIF err=0 THEN r:=TRUE;
        Message(' ТАКОГО УЖЕ НЕТ ...',r);
      END;
      abcTerm.MarkString('d');
    END;
  END;
END DelWord;

PROCEDURE EditWord;
  VAR s: ARRAY [0..255] OF CHAR;
      w: abcVoc.Word;
    err: INTEGER;
      r: BOOLEAN;
    lat: BOOLEAN;
BEGIN
  abcTerm.GetLine(s);
  IF Strings.Len(s)#0 THEN Strings.GetWord(s,w.org);
    IF Strings.Len(w.org)#0 THEN err:=abcVoc.GetWord(w.org,w);
      IF err<0 THEN CheckHALT(err)
      ELSIF err=0 THEN r:=TRUE;
        Message(' ТАКОГО СЛОВА НЕТ ...',r);
      ELSE
        lat:=abcTerm.latin?(); abcTerm.SaveWrk;
        REPEAT
          abcTerm.ReadWork('',w.org,abcTerm.conf);
        UNTIL Strings.Len(w.org)#0;
        abcTerm.latin!(NOT lat);
        IF Strings.Len(w.org)>28 THEN
          Image.image0(s,'%-25.25s... =',w.org)
        ELSE Image.image0(s,'%s = ',w.org);
        END;
        REPEAT abcTerm.ReadWork(s,w.trn,abcTerm.conf);
        UNTIL Strings.Len(w.trn)#0;
        abcTerm.latin!(lat); Strings.PoolStr(w.org);
        Strings.PoolStr(w.trn);
        err:=abcVoc.PutWord(w); CheckHALT(err);
        abcTerm.RestoreWrk;
      END;
    END;
  END;
END EditWord;

PROCEDURE GoldRead();
  VAR c: CHAR;
BEGIN
  IF gld IN mode THEN
    c:=Keyboard.ReadKey();
    CASE c OF
       gold           : Command;
      |'d','D','д','Д': DelWord;
      |'e','E','е','Е': EditWord;
    ELSE
    END;
  END;
END GoldRead;

PROCEDURE FromBuffer();
  VAR w: abcVoc.Word;
    err: INTEGER;
  lat,r: BOOLEAN;
      s: ARRAY [0..255] OF CHAR;

  PROCEDURE FlashWord(VAR w: abcVoc.Word);
  BEGIN
    IF write THEN
      IF Strings.Len(w.org)>14 THEN
        Image.image0(s,'%s = %s',w.org, w.trn)
      ELSE
        Image.image0(s,'%-14s = %s',w.org, w.trn)
      END;
      Streams.PutS(file,s);
      Streams.PutC(file,ASCII.NL);
      abcTerm.MarkString('w');
    END;
  END FlashWord;

BEGIN
  IF from_buff IN mode THEN
    lat:=abcTerm.latin?();
    REPEAT err:=abcBuf.GetString(s);
      IF err<0 THEN
        Strings.Str0(s); abcDefs.VisError(err,s);
        r:=TRUE; Message(s,r); RETURN
      END;
      Strings.PoolStr(s);
      err:=abcVoc.FindWord(s,w); CheckHALT(err);
      IF err#0 THEN
        Image.image0(s,"%s = %s",w.org, w.trn);
        abcTerm.ShowString(s); FlashWord(w);
      END;
    UNTIL err=0;
    Strings.Str1(w.org,s);
    abcTerm.latin!(Misc.Kiril?(w.org[0]));
    Strings.Str0(w.trn);
    IF Strings.Len(w.org)>25 THEN
      Image.image0(s,'%-25.25s... = ',w.org)
    ELSE Image.image0(s,'%s = ',w.org);
    END;
    abcTerm.SaveWrk;
    abcTerm.ReadWork(s,w.trn,abcTerm.new);
    IF Strings.Len(w.trn)#0 THEN
      Strings.PoolStr(w.trn); CheckHALT(abcVoc.PutWord(w));
      Image.image0(s,'%s = %s',w.org,w.trn);
      abcTerm.ShowString(s); FlashWord(w);
    END;
    abcTerm.RestoreWrk;
    abcTerm.latin!(lat);
  END;
END FromBuffer;

PROCEDURE ToBuffer();
  VAR err: INTEGER;
        s: ARRAY [0..255] OF CHAR;
        r: BOOLEAN;
BEGIN
  IF to_buff IN mode THEN
    Strings.Truncate(abcTerm.str_work,Strings.Len(abcTerm.str_work));
    err:=abcBuf.PutString(abcTerm.str_work);
    IF err<0 THEN abcDefs.VisError(err,s);
       r:=TRUE; Message(s,r)
    END;
    abcTerm.FINAL:=abcTerm.update;
  END;
END ToBuffer;

PROCEDURE ShowWord(VAL w: abcVoc.Word);
  VAR s: ARRAY [0..255] OF CHAR;
BEGIN
  Image.image0(s,'%s = %s', w.org, w.trn);
  abcTerm.WriteStr(s);
END ShowWord;

PROCEDURE WriteWord(VAL w: abcVoc.Word);
  VAR s: ARRAY [0..255] OF CHAR;
BEGIN
  Image.image0(s,'%s = %s', w.org, w.trn);
  abcTerm.ShowString(s);
END WriteWord;

PROCEDURE Regular?(VAL s: ARRAY OF CHAR): BOOLEAN;
  VAR i: INTEGER;
BEGIN i:=0;
  WHILE s[i]#0c DO
    IF (s[i]='*') OR (s[i]='%') THEN RETURN TRUE END;
    INC(i);
  END;
  RETURN FALSE
END Regular?;

VAR Ist: BOOLEAN;
    num: INTEGER;

PROCEDURE ShowStop(VAL w: abcVoc.Word): BOOLEAN;
  VAR s: ARRAY [0..255] OF CHAR;
      k: CHAR;
      r: BOOLEAN;
BEGIN
  Image.image0(s, '%s = %s', w.org, w.trn);
  abcTerm.ShowString(s); INC(num);
  IF num=20 THEN
    Ist:=TRUE; num:=0;
    r:=TRUE;
    Message('PRESS <CR> TO STOP; <SPACE BAR> TO CONTINUE',r);
    LOOP
      k:=Keyboard.ReadKey();
      CASE k OF
        |cr: RETURN TRUE;  |space: RETURN FALSE
      ELSE abcTerm.StandReaction(k)
      END;
    END;
  END;
  RETURN FALSE
END ShowStop;

PROCEDURE NewWord(VAL s: ARRAY OF CHAR);
  VAR w: abcVoc.Word;
      i: INTEGER;
      r: BOOLEAN;
    lat: BOOLEAN;
     st: ARRAY [0..79] OF CHAR;
BEGIN
  Strings.Str1(w.org,s);
  IF Strings.Len(w.org)>28 THEN
    Image.image0(st,'%-25.25s... = ',w.org)
  ELSE Image.image0(st,'%s = ',w.org);
  END;
  lat:=abcTerm.latin?();
  abcTerm.latin!(Misc.Kiril?(w.org[0]));
  abcTerm.ReadWork(st,w.trn,abcTerm.new);
  IF Strings.Len(w.trn)>0 THEN
    i:=abcVoc.PutWord(w); CheckHALT(i);
  END;
  abcTerm.latin!(lat);
END NewWord;

PROCEDURE Monitor();
  VAR tmpm: Mode;
      word: abcVoc.Word;
       err: INTEGER;
      comm: ARRAY [0..255] OF CHAR;
         r: BOOLEAN;
        ch: CHAR;
BEGIN
  Strings.Str0(comm);
  LOOP
    REPEAT
      abcTerm.ReadWork(PMTW, comm, abcTerm.old)
    UNTIL Strings.Len(comm)#0;
    Strings.PoolStr(comm);
    IF Regular?(comm) THEN
      Ist:=TRUE; num:=0; abcTerm.REFRESH:=FALSE;
      Terminal.Home; Terminal.ClearLine;
      tmpm:=mode; mode:=short;
      err:=abcVoc.IterTree(comm,ShowStop);
      mode:=tmpm;
      abcTerm.REFRESH:=TRUE; abcTerm.RefreshWrk;
      abcTerm.RefreshInfo; CheckHALT(err);
    ELSE
      err:=abcVoc.FindWord(comm, word);
      IF err>0 THEN WriteWord(word)
      ELSIF err<0 THEN CheckHALT(err)
      ELSE r:=TRUE;
        Message(' СЛОВО  НЕ НАЙДЕНО; PRESS <SILVER>'
                            ' TO ENTER TRANSLATION ',r);
        IF Keyboard.CurrentIs(silver) THEN
          tmpm:=mode; mode:=short;
          ch:=Keyboard.ReadKey(); NewWord(comm);
          mode:=tmpm;
        END;
      END;
    END;
  END;
END Monitor;

PROCEDURE Start();
  VAR i: INTEGER;
      r: BOOLEAN;
      s: ARRAY [0..79] OF CHAR;
BEGIN
  i:=abcVoc.OpenVoc(NAME);
  IF (i<0) &  Check(i) THEN HALT
  ELSIF i>=0 THEN RETURN
  ELSE
    r:=FALSE;
    Message('>>> СОЗДАТЬ НОВЫЙ СЛОВАРЬ ?',r);
    IF r THEN
      i:=abcVoc.CreateVoc(NAME);
      IF i<0 THEN
        Strings.Str0(s); abcDefs.VisError(i,s);
        Terminal.Home; Terminal.Clear;
        Terminal.print('%s\n',s);
        HALT
      END;
    END;
  END;
END Start;

PROCEDURE getS(VAR s: ARRAY OF CHAR): INTEGER;
BEGIN RETURN Streams.GetS(out,s) END getS;

PROCEDURE RestoreContens();
  VAR t: ARRAY [0..255] OF CHAR;
  i,eof: INTEGER;
BEGIN
  out:=Streams.Open(SAVE);
  IF out<0 THEN RETURN END;
  CheckHALT(Streams.GetS(out,t));
  NewFile(t);
  CheckHALT(Streams.GetS(out,t));
  abcTerm.latin!(t[0]='1');
  abcTerm.Roll!(t[1]='1');
  abcTerm.Bell!(t[2]='1');
  write:=(t[3]='1');
  abcTerm.Write!(write);
  CheckHALT(abcTerm.resScreen(getS));
  CheckHALT(Streams.GetS(out,t));
  WHILE (t[0]#ASCII.EOF)&(t[0]#0c) DO
    CheckHALT(abcBuf.PutString(t));
    CheckHALT(Streams.GetS(out,t));
  END;
END RestoreContens;

MODULE merge; (* Hady. 14-May-88. (c) KRONOS *)

FROM abcVoc     IMPORT  Word, IterTree, OpenVoc, CreateVoc
                      , CloseVoc, PutWord;
FROM abcDefs    IMPORT  VisError;
FROM Streams    IMPORT  Why?;
FROM Strings    IMPORT  GetWord, Len;
FROM Terminal   IMPORT  print;
FROM Args       IMPORT  ScanFlags, TakeParm, Flag?;
FROM Image      IMPORT  image0;

IMPORT Heap, SYSTEM, BIO, FsPublic, FileNames, Strings, Terminal;

MODULE arrays;

  FROM Heap       IMPORT  ALLOCATE, DEALLOCATE;
  FROM SYSTEM     IMPORT  ADDRESS;

  EXPORT NewData, GetData, Update, high;

  CONST blksize = 256;
        arrsize = 16;

  TYPE Block = POINTER TO ARRAY [0..blksize-1] OF ADDRESS;

  VAR arr: ARRAY [0..arrsize-1] OF Block;
      high: INTEGER;

  PROCEDURE NewData(a: ADDRESS): BOOLEAN;
    VAR i: INTEGER;
  BEGIN
    INC(high);
    i:=high DIV blksize;
    IF arr[i]=NIL THEN
      ALLOCATE(arr[i],blksize);
      ASSERT(arr[i]#NIL);
    END;
    arr[i]^[high MOD blksize]:=a;
    RETURN FALSE;
  END NewData;

  PROCEDURE GetData(x: INTEGER; VAR a: ADDRESS);
    VAR i: INTEGER; b: Block;
  BEGIN
    ASSERT(x<=high,4Ah);
    b:=arr[x DIV blksize];
    ASSERT(b#NIL);
    a:=b^[x MOD blksize];
  END GetData;

  PROCEDURE Update();
    VAR i: INTEGER;
  BEGIN
    i:=0;
    WHILE arr[i]#NIL DO
      DEALLOCATE(arr[i],blksize); arr[i]:=NIL; INC(i);
    END;
    high:=-1;
  END Update;

BEGIN
  FOR high:=0 TO arrsize-1 DO arr[high]:=NIL END;
  high:=-1;
END arrays;

MODULE BinTree;

  FROM SYSTEM     IMPORT  ADDRESS;
  FROM Heap       IMPORT  ALLOCATE, DEALLOCATE;
  FROM Strings    IMPORT  Len, Str2;
  FROM Terminal   IMPORT  Read, print;

  IMPORT Word;

  EXPORT NewNode, Iterate, UpdateTree, Tree, noq;

  TYPE Tree = POINTER TO Node;
       Node = RECORD
         key, word: POINTER TO ARRAY [0..255] OF CHAR;
         less, grt: Tree;
       END;

  VAR tree: Tree;

  PROCEDURE LEN(VAL s: ARRAY OF CHAR): INTEGER;
    VAR i: INTEGER;
  BEGIN i:=0;
    WHILE s[i]#0c DO INC(i) END;
    RETURN i
  END LEN;

  VAR noq: BOOLEAN;

  PROCEDURE NewNode(VAR w: Word): BOOLEAN;
    VAR t,n: Tree;
      len,i: INTEGER;

    PROCEDURE ask(one,two: ARRAY OF CHAR): BOOLEAN;
      VAR c: CHAR;
    BEGIN
      IF noq THEN RETURN (LEN(one)>LEN(two))
      ELSE
        print('\n%s =\n%-79.79s\n%-79.79s\nВыбрать первый ?',w.org,one,two);
        LOOP c:=Read();
          CASE c OF
            |'y','Y': print('%c\n',c); RETURN TRUE
            |'n','N': print('%c\n',c); RETURN FALSE
          ELSE END;
        END;
      END;
    END ask;

  BEGIN
    ALLOCATE(t,SIZE(Node)); ASSERT(t#NIL);
    t^.grt:=NIL; t^.less:=NIL;
    IF tree=NIL THEN tree:=t
    ELSE n:=tree;
      LOOP
        IF n^.key^=w.org THEN
          IF n^.word^=w.trn THEN
            DEALLOCATE(t,SIZE(Node)); RETURN FALSE
          ELSE
            IF ask(w.trn,n^.word^) THEN
              Str2(w.trn); len:=Len(w.trn);
              ALLOCATE(t^.word,(len+4) DIV 4); ASSERT(t^.word#NIL);
              FOR i:=0 TO len DO t^.word^[i]:=w.trn[i] END;
              DEALLOCATE(n^.word,(LEN(n^.word^)+4) DIV 4);
              n^.word:=t^.word; DEALLOCATE(t,SIZE(Node));
              RETURN FALSE
            ELSE
              DEALLOCATE(t,SIZE(Node)); RETURN FALSE;
            END;
          END;
        ELSIF w.org<n^.key^ THEN
          IF n^.less=NIL THEN n^.less:=t; EXIT
          ELSE n:=n^.less
          END;
        ELSIF n^.grt=NIL THEN n^.grt:=t; EXIT
        ELSE n:=n^.grt
        END;
      END;
    END;
    Str2(w.org); Str2(w.trn);
    len:=Len(w.org);
    ALLOCATE(t^.key,(len+4) DIV 4); ASSERT(t^.key#NIL);
    FOR i:=0 TO len DO t^.key^[i]:=w.org[i]; END;
    len:=Len(w.trn);
    ALLOCATE(t^.word,(len+4) DIV 4); ASSERT(t^.word#NIL);
    FOR i:=0 TO len DO t^.word^[i]:=w.trn[i] END;
    RETURN FALSE
  END NewNode;

  TYPE nodeProc = PROCEDURE (Tree): BOOLEAN;

  PROCEDURE Iterator(t: Tree; put: nodeProc): BOOLEAN;
  BEGIN
    IF t=NIL THEN RETURN FALSE END;
    IF Iterator(t^.less,put) THEN RETURN TRUE END;
    IF put(t) THEN RETURN TRUE END;
    RETURN Iterator(t^.grt,put);
  END Iterator;

  PROCEDURE Iterate(put: nodeProc);
    VAR r: BOOLEAN;
  BEGIN
    r:=Iterator(tree,put);
  END Iterate;

  PROCEDURE IterKill(VAR t: Tree);
  BEGIN
    IF t=NIL THEN RETURN END;
    IterKill(t^.less);
    IterKill(t^.grt);
    DEALLOCATE(t^.key,(LEN(t^.key^)+4) DIV 4);
    DEALLOCATE(t^.word,(LEN(t^.word^)+4) DIV 4);
    DEALLOCATE(t,SIZE(Node)); t:=NIL;
  END IterKill;

  PROCEDURE UpdateTree();
  BEGIN
    IterKill(tree);
  END UpdateTree;

BEGIN
  tree:=NIL;
END BinTree;

PROCEDURE chkHALT(err: INTEGER);
  VAR s: ARRAY [0..255] OF CHAR;
BEGIN
  IF err<0 THEN
    Strings.Str0(s);
    VisError(err,s);
    Terminal.print('ERRORS: %s',s); ASSERT(FALSE);
  END;
END chkHALT;

PROCEDURE LoadOne(c: CHAR);
  VAR s: ARRAY [0..3] OF CHAR;
BEGIN
  s[0]:=c; s[1]:='*'; s[2]:=0c; s[3]:=3c;
  chkHALT(IterTree(s,NewNode));
  Iterate(NewData);
END LoadOne;

PROCEDURE putWord(t: Tree): INTEGER;
  VAR w: Word;
BEGIN ASSERT(t#NIL);
  Strings.Str1(w.org,t^.key^);
  Strings.Str1(w.trn,t^.word^);
  RETURN PutWord(w);
END putWord;

PROCEDURE StoreOne(from, to: INTEGER);
  VAR i: INTEGER; t: Tree;
BEGIN
  i:=to-from;
  IF i<3 THEN RETURN
  ELSE
    i:=from+(i DIV 2);
    GetData(i,t);
    chkHALT(putWord(t));
    StoreOne(from,i);
    StoreOne(i,to);
  END;
END StoreOne;

PROCEDURE oneCHAR(nm: ARRAY OF CHAR; c: CHAR);
  VAR one: ARRAY [0..79] OF CHAR;
        s: ARRAY [0..79] OF CHAR;
BEGIN
  GetWord(nm,one);
  WHILE Len(one)#0 DO
    chkHALT(OpenVoc(one));
    image0(s,'"%s"',one);
    print('\rReading %-14.14s',s);
    LoadOne(c);
    chkHALT(CloseVoc());
    GetWord(nm,one);
  END;
END oneCHAR;

VAR c: CHAR; count: INTEGER;
    tmp,one,names: ARRAY [0..79] OF CHAR;

BEGIN ScanFlags;
  IF Flag?('m') THEN
    noq:=Flag?('q');
    TakeParm(names);
    count:=0;
    IF Len(names)=0 THEN HALT END;
    FOR c:=141c TO 337c DO
      IF (c<=172c) OR (c>=300c) THEN
        Terminal.print('\r                               CHAR = "%c"\r',c);
        oneCHAR(names,c);
        IF c=141c THEN chkHALT(CreateVoc('tmp'))
        ELSE           chkHALT(OpenVoc('tmp')) END;
        print('\rWriting in "tmp" %d words',high+1);
        count:=count+high+1;
        StoreOne(0,high);
        chkHALT(CloseVoc());
        UpdateTree;
        Update;
      END;
    END;
    print('\nTotal vocabulary volume %d words',count); HALT;
  END;
END merge;

VAR t: ARRAY [0..31] OF CHAR;

BEGIN ScanFlags;
  IF Flag?('#') THEN
    Show('   abc v2.3.4  /05-Dec-88/  (c) KRONOS ')
  END;
  IF Flag?('h') THEN HELP(FALSE) END;
  IF Flag?('h') OR Flag?('#') THEN HALT END;
  abcTerm.InitScreen;
  file:=-1;  write:=FALSE; Strings.Str0(fName);
  Start; RestoreContens(); TakeWord(t);
  IF Strings.Len(t)#0 THEN NewFile(t); END;
  IF Flag?('b') THEN abcTerm.Bell!(FALSE) END;
  IF Flag?('i') THEN abcTerm.Info!(FALSE) END;
  IF Flag?('r') THEN abcTerm.Roll!(FALSE) END;
  Terminal.SetMode(FALSE);
  abcTerm.SetReaction(gold,GoldRead);
  abcTerm.SetReaction(delln,FromBuffer);
  abcTerm.SetReaction(insln,ToBuffer);
  abcTerm.SetReaction(silver,ToFile);
  abcTerm.SetReaction(break,Break);
  abcTerm.SetReaction(exit,Finish);
  mode:=all;
  Monitor;
END abc.
