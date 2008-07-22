MODULE abc; (* Hady. 29-Apr-88. (c) KRONOS *)

FROM Keyboard   IMPORT  gold,  exit,  silver, dw,    up
                      , space, rtab,  ltab,   break, cr
                      , delln, insln, empty;
FROM Args       IMPORT  ScanFlags, Flag?, TakeWord;

IMPORT Strings,  Heap,     Streams
     , Image,    dicFile,  StdIO
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
   dict : dicFile.dictionary;

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
  dicFile.close(dict);
  Terminal.SetCrs(abcDefs.INFO-1,0);
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
    dicFile.close(dict);
    HALT;
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

  VAR s: ARRAY [0..255] OF CHAR;
      t: ARRAY [0..79] OF CHAR;
    wrd: dicFile.text;
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
      Strings.GetWord(s,wrd);
      dicFile.remove(dict,wrd);
      abcTerm.MarkString('d');
    END;
  END;
END DelWord;

PROCEDURE EditWord;
  VAR s: ARRAY [0..255] OF CHAR;
    err: INTEGER;
    wrd: dicFile.text;
    trn: dicFile.text;
      r: BOOLEAN;
    lat: BOOLEAN;
BEGIN
  abcTerm.GetLine(s);
  IF Strings.Len(s)#0 THEN
    Strings.GetWord(s,wrd);
    IF Strings.Len(wrd)#0 THEN
      IF NOT dicFile.get(dict,wrd,trn) THEN
        r:=TRUE; Message(' ТАКОГО СЛОВА НЕТ ...',r);
      ELSE
        lat:=abcTerm.latin?(); abcTerm.SaveWrk;
        REPEAT
          abcTerm.ReadWork('',wrd,abcTerm.conf);
        UNTIL Strings.Len(wrd)#0;
        abcTerm.latin!(NOT lat);
        IF Strings.Len(wrd)>28 THEN
          Image.image0(s,'%-25.25s... =',wrd)
        ELSE
          Image.image0(s,'%s = ',wrd);
        END;
        Strings.Str2(trn);
        abcTerm.ReadWork(s,trn,abcTerm.conf);
        abcTerm.latin!(lat);
        IF Strings.Len(trn)#0 THEN
          Strings.PoolStr(wrd);
          Strings.PoolStr(trn);
          dicFile.put(dict,wrd,trn);
        END;
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
  VAR
    err: INTEGER;
  lat,r: BOOLEAN;
      s: ARRAY [0..255] OF CHAR;
    wrd: dicFile.text;
    trn: dicFile.text;

  PROCEDURE FlashWord;
  BEGIN
    IF write THEN
      IF Strings.Len(wrd)>14 THEN
        Image.image0(s,'%s = %s',wrd,trn)
      ELSE
        Image.image0(s,'%-14s = %s',wrd,trn)
      END;
      Streams.PutS(file,s);
      Streams.PutC(file,ASCII.NL);
      abcTerm.MarkString('w');
    END;
  END FlashWord;

BEGIN
  IF from_buff IN mode THEN
    lat:=abcTerm.latin?();
    REPEAT
      err:=abcBuf.GetString(s);
      IF err<0 THEN
        Strings.Str0(s); abcDefs.VisError(err,s);
        r:=TRUE; Message(s,r); RETURN
      END;
      Strings.PoolStr(s);
      Strings.Str1(wrd,s);
      IF dicFile.get(dict,wrd,trn) THEN
        Image.image0(s,"%s = %s",wrd,trn);
        abcTerm.ShowString(s); FlashWord;
      END;
    UNTIL err=0;
    Strings.Str1(wrd,s);
    abcTerm.latin!(Misc.Kiril?(s[0]));
    Strings.Str0(trn);
    IF Strings.Len(wrd)>25 THEN
      Image.image0(s,'%-25.25s... = ',wrd)
    ELSE
      Image.image0(s,'%s = ',wrd);
    END;
    abcTerm.SaveWrk;
    abcTerm.ReadWork(s,trn,abcTerm.new);
    IF Strings.Len(trn)#0 THEN
      Strings.PoolStr(trn);
      dicFile.put(dict,wrd,trn);
      Image.image0(s,'%s = %s',wrd,trn);
      abcTerm.ShowString(s); FlashWord;
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

PROCEDURE WriteWord(VAL wrd,trn: dicFile.text);
  VAR s: ARRAY [0..255] OF CHAR;
BEGIN
  Image.image0(s,'%s = %s',wrd,trn);
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

PROCEDURE ShowStop(wrd,trn: dicFile.text): BOOLEAN;
  VAR s: ARRAY [0..255] OF CHAR;
      k: CHAR;
      r: BOOLEAN;
BEGIN
  Image.image0(s,'%s = %s',wrd,trn);
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
  VAR i: INTEGER;
      r: BOOLEAN;
    lat: BOOLEAN;
    wrd: dicFile.text;
    trn: dicFile.text;
     st: ARRAY [0..79] OF CHAR;
BEGIN
  Strings.Str1(wrd,s);
  IF Strings.Len(wrd)>28 THEN
    Image.image0(st,'%-25.25s... = ',wrd)
  ELSE
    Image.image0(st,'%s = ',wrd)
  END;
  lat:=abcTerm.latin?();
  abcTerm.latin!(Misc.Kiril?(wrd[0]));
  abcTerm.ReadWork(st,trn,abcTerm.new);
  IF Strings.Len(trn)>0 THEN dicFile.put(dict,wrd,trn) END;
  abcTerm.latin!(lat);
END NewWord;

PROCEDURE Monitor();
  VAR tmpm: Mode;
       err: INTEGER;
      comm: ARRAY [0..255] OF CHAR;
      trn : dicFile.text;
         r: BOOLEAN;
        ch: CHAR;
BEGIN
  Strings.Str0(comm);
  LOOP
    dicFile.flash_cash;
    REPEAT
      abcTerm.ReadWork(PMTW, comm, abcTerm.old)
    UNTIL Strings.Len(comm)#0;
    Strings.PoolStr(comm);
    IF Regular?(comm) THEN
      Ist:=TRUE; num:=0; abcTerm.REFRESH:=FALSE;
      Terminal.Home; Terminal.ClearLine;
      tmpm:=mode; mode:=short;
      dicFile.find(dict,comm,ShowStop);
      mode:=tmpm;
      abcTerm.REFRESH:=TRUE; abcTerm.RefreshWrk;
      abcTerm.RefreshInfo; CheckHALT(err);
    ELSE
      IF dicFile.get(dict,comm,trn) THEN
        WriteWord(comm,trn);
      ELSE
        r:=TRUE;
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
  dicFile.open(dict,NAME);
  IF (i<0) &  Check(i) THEN HALT
  ELSIF i>=0 THEN RETURN
  ELSE
    r:=FALSE;
    Message('>>> СОЗДАТЬ НОВЫЙ СЛОВАРЬ ?',r);
    IF r THEN
      dicFile.create(dict,NAME);
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

VAR count: INTEGER;
    tmp: dicFile.dictionary;

PROCEDURE app_word(in,out: dicFile.text): BOOLEAN;
  VAR r: BOOLEAN; ch: CHAR; trn: dicFile.text;
BEGIN
  IF dicFile.get(tmp,in,trn) THEN
    IF out=trn THEN
      r:=FALSE;
    ELSE
      Terminal.print('%s\n',in);
      Terminal.print('  %-75.75s\n',trn);
      Terminal.print('  %-75.75s\n',out);
      Terminal.print('first?');
      REPEAT ch:=Terminal.Read() UNTIL (ch='y') OR (ch='n');
      Terminal.print('%c\n\n',ch);
      r:=ch='n';
    END;
  ELSE
    r:=TRUE;
  END;
  IF r THEN
    dicFile.put(tmp,in,out);
    INC(count);
    Terminal.print('%d\r',count);
  END;
  RETURN FALSE;
END app_word;

VAR t,name: ARRAY [0..31] OF CHAR;

BEGIN
  Terminal.SetMode(FALSE);
  ScanFlags;
  IF Flag?('#') THEN
    Show('   abc v2.3.4  /05-Dec-88/  (c) KRONOS ')
  END;
  IF Flag?('h') THEN HELP(FALSE) END;
  IF Flag?('h') OR Flag?('#') THEN HALT END;
  IF Flag?('m') THEN
    count:=0;
    dicFile.create(tmp,'tmp');
    LOOP
      TakeWord(name);
      IF name='' THEN EXIT END;
      dicFile.open(dict,name);
      dicFile.rebalance(dict,app_word);
      dicFile.close(dict);
    END;
    dicFile.close(tmp);
    Terminal.print('\nTotal vocabulary volume %d words',count); HALT;
  END;
  abcTerm.InitScreen;
  file:=-1;  write:=FALSE; Strings.Str0(fName);
  Start; RestoreContens(); TakeWord(t);
  IF Strings.Len(t)#0 THEN NewFile(t); END;
  IF Flag?('b') THEN abcTerm.Bell!(FALSE) END;
  IF Flag?('i') THEN abcTerm.Info!(FALSE) END;
  IF Flag?('r') THEN abcTerm.Roll!(FALSE) END;
  abcTerm.SetReaction(gold,GoldRead);
  abcTerm.SetReaction(delln,FromBuffer);
  abcTerm.SetReaction(insln,ToBuffer);
  abcTerm.SetReaction(silver,ToFile);
  abcTerm.SetReaction(break,Break);
  abcTerm.SetReaction(exit,Finish);
  mode:=all;
  Monitor;
END abc.
