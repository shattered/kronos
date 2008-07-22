MODULE abc; (* Hady. 28-Feb-89. (c) KRONOS *)

IMPORT  abuf: abcBuf;
IMPORT  SCR : abcTT;
IMPORT  voc : abcVoc;
IMPORT  sle : abcSLE;
IMPORT  tty : Terminal;
IMPORT  kbr : Keyboard;
IMPORT  strs: Strings;
IMPORT  flio: Streams;
IMPORT  img : Image;
IMPORT  sys : SYSTEM;
IMPORT  shell: myShell;
IMPORT  BIO, FsPublic, ASCII, SYSTEM, Args, Heap, FileNames;

CONST version = 'abc v2.5.0 /11-Mar-89/ (c) KRONOS';

CONST gld = kbr.gold;
      slv = kbr.silver;
      brz = kbr.bronze;

MODULE misc;

  EXPORT QUALIFIED Latin?, Cyrill?, Little, Control?;

  PROCEDURE Latin?(c: CHAR): BOOLEAN;
  BEGIN RETURN ((c>='a')&(c<='z')) OR ((c>='A')&(c<='Z')) END Latin?;

  PROCEDURE Cyrill?(c: CHAR): BOOLEAN;
  BEGIN RETURN (c>=300c)&(c<=377c) END Cyrill?;

  PROCEDURE Control?(c: CHAR): BOOLEAN;
  BEGIN
    RETURN (c<40c) OR ((c>=200c)&(c<240c));
  END Control?;

  PROCEDURE Little(c: CHAR): CHAR;
  BEGIN
    IF    Latin?(c)  THEN RETURN CHAR(BITSET(c)+{5})
    ELSIF Cyrill?(c) THEN RETURN CHAR(BITSET(c)-{5})
    ELSE RETURN c
    END;
  END Little;

END misc;

---------------------ERRORS AND MESSAGES----------------------
                     -------------------

CONST -- ERRORS OF MODULES --
  ExistOpened= -100h;
  NotOpened  = -101h;
  NoSuchWord = -102h;
  NoMemory   = -103h;
  EmptyBuf   = -104h;

PROCEDURE VisError(err: INTEGER; VAR s: ARRAY OF CHAR);
  VAR t: ARRAY [0..79] OF CHAR;
BEGIN
  IF err<0 THEN
    IF    err=NoSuchWord  THEN strs.AppStr(s,'Неизвестное слово')
    ELSIF err=NoMemory    THEN strs.AppStr(s,'Нет памяти в буфферe')
    ELSIF err=EmptyBuf    THEN strs.AppStr(s,'Буффер пуст')
    ELSIF err=NotOpened   THEN
      strs.AppStr(s,'Обращение к неоткрытому словарю')
    ELSIF err=ExistOpened THEN strs.AppStr(s,'Словарь уже открыт')
    ELSE strs.Str0(t); flio.Why?(err,s); strs.AppStr(s,t);
    END;
  END;
END VisError;

VAR mess_char: CHAR;

PROCEDURE query(): BOOLEAN;
BEGIN
  LOOP
    mess_char:=misc.Little(kbr.ReadKey());
    CASE mess_char OF
      | 'y', 'д' : RETURN TRUE
      | 'n', 'н' : RETURN FALSE;
    ELSE tty.Write(07c)
    END;
  END;
END query;

PROCEDURE message(VAL s: ARRAY OF CHAR; VAR mode: BOOLEAN);
(* -mode- FALSE means query message *)
BEGIN
  SCR.message(s);
  IF mode THEN REPEAT UNTIL kbr.Pressed?();
  ELSE mode:=query();
  END;
  SCR.ref_info;
END message;

PROCEDURE Check(err: INTEGER): BOOLEAN;
  VAR s: ARRAY [0..79] OF CHAR;
      r: BOOLEAN;
BEGIN
  r:=FALSE;
  IF err<0 THEN
    strs.Str1(s,'>> ERROR: ');
    VisError(err, s);
    strs.AppStr(s,' ЭТО СЕРЬЕЗНО ?');
    message(s,r);
  END;
  RETURN r
END Check;

PROCEDURE CheckHALT(err: INTEGER);
  VAR i: INTEGER; s: ARRAY [0..79] OF CHAR;
BEGIN
  IF Check(err) THEN tty.SetCrs(23,0);
    i:=voc.CloseVoc();
    IF i<0 THEN
      strs.Str0(s); VisError(i,s); tty.print('\n%s\n',s);
    END; HALT;
  END;
END CheckHALT;

PROCEDURE checkHALT(err: INTEGER);
  VAR s: ARRAY [0..79] OF CHAR;
BEGIN
  IF err<0 THEN strs.Str0(s);  VisError(err,s);
    tty.Home; tty.Clear; tty.print('%s\n',s);
    HALT
  END;
END checkHALT;

PROCEDURE Query(VAL s: ARRAY OF CHAR): BOOLEAN;
  VAR r: BOOLEAN;
BEGIN r:=FALSE; message(s,r); RETURN r END Query;

PROCEDURE Message(VAL s: ARRAY OF CHAR);
VAR r: BOOLEAN;
BEGIN r:=TRUE; message(s,r) END Message;

PROCEDURE mess_wait(VAL s: ARRAY OF CHAR);
  VAR t: ARRAY [0..79] OF CHAR;
BEGIN  img.image0(t,'%s Press any key.',s);
  Message(t); mess_char:=kbr.ReadKey();
END mess_wait;

-----------------STREAMS, SIMPLIFIED & QUICK------------------
                 ---------------------------

MODULE abcSave; (* Hady. 11-Dec-88. (c) KRONOS *)

   IMPORT BIO, tty, FsPublic, img, SYSTEM, ASCII, Query;

   EXPORT QUALIFIED OpenSave,  CreateSave
                  , CloseSave, GetS
                  , PutS     , GetC
                  , PutC;

   CONST NAME = '.ABC.SAVE';

   VAR save : FsPublic.File;
       bNo  : INTEGER;
       cc   : INTEGER;
       eof  : INTEGER;
       buf  : ARRAY [0..4095] OF CHAR;
       name : ARRAY [0..31] OF CHAR;
       write: BOOLEAN;

   PROCEDURE chk(r: BOOLEAN);
     VAR rep,err: ARRAY [0..79] OF CHAR;
   BEGIN
     IF r THEN
       FsPublic.VisFSerr(r,err);
       img.image0(rep,' %s; Это серьезно? ',err);
       IF Query(rep) THEN HALT END;
     END;
   END chk;

   PROCEDURE Min(i,j: INTEGER): INTEGER;
   BEGIN
     IF i<j THEN RETURN i ELSE RETURN j END;
   END Min;

   PROCEDURE CreateSave();
     VAR r: BOOLEAN;
   BEGIN
     r:=BIO.OpenOnDir(BIO.CD(),save,name);
     IF r & r=FsPublic.FileNotFound THEN chk(BIO.Create(save));
       chk(BIO.Link(BIO.CD(),name,save));
     ELSIF r THEN chk(r);
     END;
     write:=TRUE; bNo:=0; eof:=0; cc:=0;
   END CreateSave;

   PROCEDURE OpenSave(): BOOLEAN;
     VAR r: BOOLEAN;
   BEGIN
     r:=BIO.OpenOnDir(BIO.CD(),save,name);
     IF r=FsPublic.FileNotFound THEN RETURN TRUE
     ELSE chk(r)
     END;
     eof:=BIO.GetEof(save);
     chk(BIO.bRead(save,0,SYSTEM.ADR(buf),Min(eof,4096)));
     write:=FALSE; cc:=0; bNo:=0;
     RETURN FALSE;
   END OpenSave;

   PROCEDURE CloseSave();
     VAR r: BOOLEAN;
   BEGIN
     IF write THEN
       chk(BIO.bWrite(save,bNo,SYSTEM.ADR(buf),cc));
       BIO.SetEof(save,eof); chk(BIO.Cut(save));
     END;
     chk(BIO.Close(save)); save:=-1;
   END CloseSave;

   PROCEDURE tryRead();
   BEGIN
     chk(BIO.bRead(save,bNo+1,SYSTEM.ADR(buf),Min((eof-bNo*4096),4096)));
     INC(bNo); cc:=0;
   END tryRead;

   PROCEDURE GetC(): CHAR;
     VAR c: CHAR; pos: INTEGER;
   BEGIN ASSERT(NOT write);
     pos:=bNo*4096+cc;
     IF pos>=eof THEN RETURN ASCII.EOF END;
     c:=buf[cc]; INC(cc);
     IF (cc>4095) & (pos+1<eof) THEN tryRead END;
     RETURN c;
   END GetC;

   PROCEDURE GetS(VAR s: ARRAY OF CHAR);
     VAR i,pos: INTEGER;
   BEGIN ASSERT(NOT write);
     pos:=bNo*4096+cc; i:=0;
     IF pos>=eof THEN s[0]:=ASCII.EOF; RETURN END;
     WHILE (i<HIGH(s)-1)&(pos<eof) DO
       s[i]:=buf[cc]; INC(cc); INC(pos);
       IF (cc>4095)&(pos<eof) THEN tryRead END;
       IF s[i]=ASCII.NL THEN s[i]:=0c; s[HIGH(s)]:=CHAR(i); RETURN END;
       INC(i);
     END;
     s[i]:=0c; s[HIGH(s)]:=CHAR(i);
     IF (pos<eof) THEN
       REPEAT INC(cc); INC(pos);
         IF (cc>4095) & (pos<eof) THEN tryRead END;
       UNTIL (buf[cc]=ASCII.NL) OR (pos=eof);
     END;
   END GetS;

   PROCEDURE tryWrite();
   BEGIN
     chk(BIO.bWrite(save,bNo,SYSTEM.ADR(buf),4096));
     INC(bNo); cc:=0;
   END tryWrite;

   PROCEDURE PutC(c: CHAR);
   BEGIN ASSERT(write);
     buf[cc]:=c; INC(cc); INC(eof);
     IF cc>4095 THEN tryWrite END;
   END PutC;
   
   PROCEDURE PutS(VAL s: ARRAY OF CHAR);
     VAR i: INTEGER;
   BEGIN ASSERT(write);
     i:=0;
     WHILE (i<=HIGH(s))&(s[i]#0c) DO
       buf[cc]:=s[i]; INC(cc); INC(eof);
       IF cc>4095 THEN tryWrite END;
       INC(i);
     END;
   END PutS;

BEGIN save:=-1; img.image0(name,'%s',NAME);
END abcSave;

----------------------------OUTPUT----------------------------
                            ------

VAR   out: flio.Stream;
 out_name: ARRAY [0..32] OF CHAR;

PROCEDURE newFile(name: ARRAY OF CHAR);
BEGIN
  IF out_name[0]#0c THEN
    CheckHALT(flio.Close(out));
  END;
  IF name[0]#0c THEN
    out:=flio.Open(name);
    IF out<0 THEN
      out:=flio.Create(name); CheckHALT(out);
    END;
    CheckHALT(flio.Seek(out,0,2));
  END;
  img.image0(out_name,'%s',name);
  SCR.file(name);
END newFile;

PROCEDURE put_out(s: ARRAY OF CHAR);
BEGIN
  IF out_name[0]=0c THEN mess_wait(' Не открыт выходной файл. ');
  ELSE flio.PutS(out,s); flio.PutC(out,ASCII.NL)
  END;
END put_out;

-----------------------STRINGS EDITORS------------------------
                       ---------------

CONST GOLD   = '***  GOLD  ***';        BRONZE = '*** BRONZE ***';
      SILVER = '*** SILVER ***';        CLEAR  = '              ';
      SPACES = '----------------------------------------'
               '---------------------------------------';
      BLANKS = '                                        ';

VAR patts,
    cmds ,
    trans: sle.BUFF;

CONST cmdPMT = '>>> ';
       myPMT = 'ABC ';

PROCEDURE decor(VAL s: ARRAY OF CHAR; line, col: INTEGER);
  VAR i: INTEGER;
BEGIN
  tty.SetCrs(line,col); i:=0;
  WHILE (i<=HIGH(s))&(s[i]#0c) DO
    tty.Write(' ');
    col:=tty.Reverse(1);
    tty.Write(s[i]);
    col:=tty.Reverse(col);
    INC(i);
  END;
  tty.Write(' ');
END decor;

PROCEDURE decorate;
BEGIN
  tty.Home; tty.ClearLine; tty.print('\n');
            tty.ClearLine; tty.print('\n');
  IF SCR.vt220 THEN
    tty.WriteString(''033c'[4m');
    tty.WriteString(BLANKS);
    tty.WriteString(''033c'[24m');
    tty.WriteString(''033c'#6');
    decor('vocabulary',2,16);
  ELSE
    tty.WriteString(SPACES);
    decor('VOCABULARY',2,46);
  END;
END decorate;

PROCEDURE kill_decor;
BEGIN
  IF SCR.vt220 THEN
    tty.SetCrs(2,0);
    tty.WriteString(''033c'#5'); tty.ClearLine; -- kill decoration
    tty.WriteString(''033c'[1;24r');            -- set roll bounds
  END;
END kill_decor;

PROCEDURE clear_ln; BEGIN tty.Write(015c); tty.ClearLine END clear_ln;

PROCEDURE deleete_word;
  VAR w: voc.Word; r: INTEGER; ask: ARRAY [0..79] OF CHAR;
BEGIN SCR.get_word(w);
  IF w.org='' THEN RETURN END;
  img.image0(ask,'"%s = %-*.*s..." to deleete, are you shure ?',w.org,16,16,w.trn);
  IF Query(ask) THEN
    r:=voc.GetWord(w.org,w);
    IF r<0 THEN CheckHALT(r)
    ELSIF r=0 THEN
      Message(' Такого слова уже нет');
    ELSE SCR.mark('d');
    END;
  END;
END deleete_word;

PROCEDURE silver;
  VAR c: CHAR;
BEGIN SCR.state(SILVER);
  c:=kbr.ReadKey();
  WHILE c#kbr.silver DO
    CASE c OF
      | kbr.up    : SCR.mark_up;
      | kbr.dw    : SCR.mark_dw;
      |kbr.lpull  : SCR.move_left;
      |kbr.rpull  : SCR.move_right;
      |  'w'      : SCR.write_out(put_out); SCR.mark('w');
      |  'd'      : deleete_word;
    ELSE SCR.bell!;
    END;
    c:=kbr.ReadKey();
  END;
  SCR.state(CLEAR); sle.ref_pos;
END silver;

PROCEDURE pool_str(VAR s: ARRAY OF CHAR);
  VAR i,j: INTEGER;
BEGIN
  i:=0; j:=0;
  WHILE (i<HIGH(s))&(s[i]=' ') DO INC(i) END;
  WHILE (i<HIGH(s))&(s[i]# 0c) DO s[j]:=s[i]; INC(i); INC(j) END;
  s[j]:=0c;
END pool_str;

PROCEDURE help;
BEGIN
  IF SCR.vt220 THEN
    tty.SetCrs(SCR.screen-2,0);
  ELSE
    tty.SetCrs(SCR.scr_start,0); tty.Clear;
  END;
  tty.Show('');
  tty.Show('<GOLD>   -- командный режим; ВОЗМОЖНЫ КОМАНДЫ:');
  tty.Show('       "h" -- подсказка');
  tty.Show('       "v" -- визуализация буфера');
  tty.Show('   "b","i" -- включение/выключение звонка, инфо-строки');
  tty.Show('   "e","q" -- выход с/без записи состояния');
  tty.Show('       "!" -- временный выход в shell');
  tty.Show('   "sимя_файла" -- открыть выходной файл "имя_файла"');
  tty.Show('<SILVER> -- выход на экран; ПРИ ЭТОМ:');
  tty.Show('   стрелки -- перемещения маркера;');
  tty.Show('  <SILVER> -- выход из режима;');
  tty.Show('       "w" -- запись маркированной строки в выходной файл;');
  tty.Show('<BRONZE> -- работа с буфером. <ESC> -- выход;');
  tty.Show('');
  tty.Show('');
  tty.Show('');
  tty.Show('');
  tty.Show('');
  tty.Show('');
  mess_wait('               ');
END help;

VAR cnt: INTEGER;
CONST limit  = 19;
      margin = 50;

PROCEDURE show_str(VAL s: ARRAY OF CHAR): BOOLEAN;
  VAR bump: ARRAY [0..31] OF CHAR; i: INTEGER;
BEGIN i:=0;
  WHILE (i<HIGH(s))&(i<HIGH(bump))&(s[i]#0c) DO
    bump[i]:=s[i]; INC(i)
  END;
  bump[i]:=0c;
  tty.SetCrs(SCR.scr_start+cnt,margin);
  tty.WriteString('| ');  tty.WriteString(bump);
  tty.ClearLine;
  INC(cnt);
  IF cnt>limit THEN
    Message(' ESC to cancel, any other key to continue');
    IF kbr.ReadKey()=033c THEN RETURN TRUE END;
    cnt:=0;
  END;
  RETURN FALSE;
END show_str;

PROCEDURE show_buffer;
BEGIN cnt:=0;
  abuf.ShowBuffer(show_str);
  IF cnt<limit THEN
    WHILE cnt<=limit DO
      tty.SetCrs(SCR.scr_start+cnt,margin);
      tty.Write('|'); tty.ClearLine;
      INC(cnt);
    END;
    mess_wait('           ');
  END;
  SCR.refresh;
END show_buffer;

PROCEDURE from_buff(VAR s: ARRAY OF CHAR);
BEGIN CheckHALT(abuf.GetString(s));
  SCR.buff(SCR.buff_depth-1);
END from_buff;

PROCEDURE to_buff(VAL s: ARRAY OF CHAR);
BEGIN CheckHALT(abuf.PutString(s));
  SCR.buff(SCR.buff_depth+1);
END to_buff;

PROCEDURE save_str(VAL s: ARRAY OF CHAR);
BEGIN
  abcSave.PutS(s); abcSave.PutC(ASCII.NL);
END save_str;

PROCEDURE save_buff(VAL s: ARRAY OF CHAR): BOOLEAN;
BEGIN save_str(s); RETURN FALSE END save_buff;

PROCEDURE save_all;
BEGIN
  abcSave.CreateSave;
  save_str(version );
  save_str(out_name);
  SCR.save(save_str);
  abuf.ShowBuffer(save_buff);
  abcSave.CloseSave;
END save_all;

PROCEDURE finish;
BEGIN
  CheckHALT(voc.CloseVoc()); newFile('');
END finish;

PROCEDURE restore_all;
  VAR bump: ARRAY [0..79] OF CHAR;
BEGIN
  IF abcSave.OpenSave() THEN RETURN END;
  abcSave.GetS(bump);
  IF bump#version THEN
    IF Query(' Неправильная версия файла ".ABC.SAVE", это серьезно ? ')
    THEN HALT(1)
    END;
  ELSE
    abcSave.GetS(bump); newFile(bump);
    SCR.restore(abcSave.GetS);
    abcSave.GetS(bump);
    WHILE (bump[0]#ASCII.EOF) DO
      to_buff(bump); abcSave.GetS(bump);
    END;
  END;
  abcSave.CloseSave;
END restore_all;

PROCEDURE Shell;
  VAR pmt,bump: ARRAY [0..255] OF CHAR;
BEGIN kill_decor;
  LOOP
    shell.get_prompt(bump);
    img.image0(pmt,'ABC>>> %s',bump);
    sle.SLE(pmt, bump, SCR.screen, 0, 78, FALSE, 033c);
    tty.print('\n');
    IF sle.last_char=033c THEN EXIT END;
    shell.system(bump,tty.print);
  END;
  decorate;
  SCR.refresh;
END Shell;

PROCEDURE interpret(VAR s: ARRAY OF CHAR);
BEGIN pool_str(s);
  CASE s[0] OF
    |'h','?': help; SCR.refresh;
    |'b'    : SCR.bell(NOT SCR.bell?);
    |'v'    : show_buffer;
    |'e'    : save_all; finish; tty.SetMode(TRUE); HALT;
    |'q'    : finish; tty.SetMode(TRUE); HALT;
    |'s'    : s[0]:=' '; pool_str(s); newFile(s);
    |'!'    : Shell;
  ELSE
    mess_wait('НЕПОНЯТНАЯ КОМАНДА; ');
  END;
END interpret;

PROCEDURE gold;
  VAR cmd: ARRAY [0..79] OF CHAR;
BEGIN SCR.state(GOLD);
  sle.Set(cmds);
  sle.SLE(cmdPMT,cmd,1,0,78,FALSE); clear_ln;
  interpret(cmd);
  SCR.state(CLEAR); sle.ref_pos;
END gold;

PROCEDURE bronze;
  VAR w: voc.Word;
    pat: ARRAY [0..79] OF CHAR;
    pmt: ARRAY [0..31] OF CHAR;
      r: INTEGER;
BEGIN SCR.state(BRONZE); sle.Set(trans);
  LOOP
    r:=abuf.GetString(pat);
    IF r=EmptyBuf THEN mess_wait(' Буфер пуст.   '); EXIT
    ELSIF r<0 THEN CheckHALT(r);
    ELSE
      SCR.buff(SCR.buff_depth-1);
      r:=voc.FindWord(pat,w);
      IF r=0 THEN
        img.image0(pmt,'%s = ',pat);
        sle.SLE(pmt,w.trn,1,0,78,FALSE,033c); clear_ln;
        IF    sle.last_char=033c THEN to_buff(pat); EXIT
        ELSIF    w.trn[0]#0c     THEN CheckHALT(voc.PutWord(w));
        END;
      ELSIF r>0 THEN SCR.show(w);
      ELSE CheckHALT(r);
      END;
    END;
  END;
END bronze;

PROCEDURE inpStr(VAR s: ARRAY OF CHAR);
BEGIN
  LOOP
    sle.Set(patts);
    sle.SLE(myPMT,s,0,SCR.work_line,78,FALSE,kbr.insln, gld,slv,brz);
    clear_ln;
    CASE sle.last_char OF
      |kbr.insln  : to_buff(s);
      |kbr.silver : silver ;
      |kbr.gold   : gold   ;
      |kbr.bronze : bronze ;
    ELSE RETURN
    END;
  END;
END inpStr;

-------------------------TRANSLATIONS-------------------------
                         ------------

PROCEDURE Regular?(VAL s: ARRAY OF CHAR): BOOLEAN;
  VAR i: INTEGER;
BEGIN i:=0;
  WHILE (i<HIGH(s))&(s[i]#0c) DO
    IF (s[i]='*') OR (s[i]='%') THEN RETURN TRUE END;
    INC(i);
  END;
  RETURN FALSE;
END Regular?;

CONST stop_info = ' [ESC] to cancel; any other key to stop';

PROCEDURE show_stop(VAR w: voc.Word): BOOLEAN;
  VAR c: CHAR;
BEGIN
  IF SCR.vt220 THEN SCR.show(w);
    IF kbr.Pressed?() THEN c:=kbr.ReadKey();
      IF c=033c THEN RETURN TRUE
      ELSE mess_wait('         ');
        IF mess_char=033c THEN RETURN TRUE END;
        SCR.message(stop_info);
      END;
    END;
  ELSE
    SCR.show(w); INC(cnt);
    IF cnt=limit THEN cnt:=0;
      Message(' [ESC] to cancel, any other key to continue');
      IF kbr.ReadKey()=033c THEN RETURN TRUE END;
    END;
  END;
  RETURN FALSE;
END show_stop;

PROCEDURE tryToTrans(VAR w: voc.Word);
  VAR i: INTEGER;
BEGIN
  IF Regular?(w.org) THEN
    IF SCR.vt220 THEN
      SCR.message(stop_info);
      CheckHALT(voc.IterTree(w.org,show_stop));
      SCR.ref_info;
    ELSE
      SCR.info(FALSE); cnt:=0;
      CheckHALT(voc.IterTree(w.org,show_stop));
      SCR.info(TRUE);
    END;
  ELSE i:=voc.FindWord(w.org,w); CheckHALT(i);
    IF i=0 THEN Message('Слово не найдено')
    ELSE SCR.show(w);
    END;
  END;
END tryToTrans;

--------------------MERGE OF VOCABULARIES---------------------
                    ---------------------

MODULE merge; (* Hady. 14-May-88. (c) KRONOS *)

FROM voc        IMPORT  Word, IterTree, OpenVoc, CreateVoc
                      , CloseVoc, PutWord;
FROM flio       IMPORT  Why?;
FROM strs       IMPORT  GetWord, Len;
FROM tty        IMPORT  print;
FROM Args       IMPORT  ScanFlags, TakeParm, Flag?;
FROM img        IMPORT  image0;

IMPORT Heap, SYSTEM, BIO, FsPublic, FileNames, strs, tty, VisError;

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
  FROM strs       IMPORT  Len, Str2;
  FROM tty        IMPORT  Read, print;

  IMPORT Word, tty;

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
      VAR c: CHAR; r: BOOLEAN;
    BEGIN
      IF noq THEN RETURN (LEN(one)>LEN(two))
      ELSE
        print('\n%s =\n%-79.79s\n%-79.79s\nВыбрать первый ?',w.org,one,two);
        tty.Write(07c);
        LOOP c:=Read();
          CASE c OF
            |'y','Y': print('%c\r',c); r:=TRUE ; EXIT
            |'n','N': print('%c\r',c); r:=FALSE; EXIT
          ELSE END;
        END;
        tty.Up; tty.Up; tty.Up;
        tty.Clear; tty.Up;
        RETURN r;
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
    strs.Str0(s);
    VisError(err,s);
    tty.print('ERRORS: %s',s); ASSERT(FALSE);
  END;
END chkHALT;

PROCEDURE LoadOne(c: CHAR);
  VAR s: ARRAY [0..3] OF CHAR;
      i: INTEGER; t: Tree;
BEGIN
  s[0]:=c; s[1]:='*'; s[2]:=0c; s[3]:=3c;
  chkHALT(IterTree(s,NewNode));
END LoadOne;

PROCEDURE putWord(t: Tree): INTEGER;
  VAR w: Word;
BEGIN ASSERT(t#NIL);
  strs.Str1(w.org,t^.key^);
  strs.Str1(w.trn,t^.word^);
  RETURN PutWord(w);
END putWord;

PROCEDURE StoreOne(from, to: INTEGER);
  VAR i: INTEGER; t: Tree;
BEGIN
  i:=to-from;
  IF i<3 THEN
    FOR i:=from TO to DO
      GetData(i,t); chkHALT(putWord(t));
    END;
  ELSE i:=from+(i DIV 2);
    GetData(i,t); chkHALT(putWord(t));
    StoreOne(from,i-1);
    StoreOne(i+1,to);
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
  Iterate(NewData);
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
        tty.print('\n                               CHAR = "%c"\r',c);
        oneCHAR(names,c);
        IF c=141c THEN chkHALT(CreateVoc('tmp'))
        ELSE           chkHALT(OpenVoc('tmp')) END;
        print('\rWriting to "tmp" %d words',high+1);
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

---------------------------------------------------------------

PROCEDURE monitor;
  VAR w: voc.Word;
BEGIN
  LOOP
    inpStr(w.org);
    IF w.org[0]#0c THEN tryToTrans(w);
    END;
  END;
END monitor;

CONST myFilesName = 'vvv';

PROCEDURE start;
  VAR i: INTEGER; s: ARRAY [0..79] OF CHAR;
BEGIN
  i:=voc.OpenVoc(myFilesName);
  IF (i<0) &  Check(i) THEN HALT
  ELSIF i>=0 THEN RETURN
  ELSIF Query('>>> СОЗДАТЬ НОВЫЙ СЛОВАРЬ ?') THEN
    checkHALT(voc.CreateVoc(myFilesName));
  END;
END start;

PROCEDURE Help;
BEGIN
  tty.Show('Англо - русский словарь.');
  tty.Show('   abc [file_name] [-h#]');
  tty.Show('   при наличии "file_name" открывается выходной файл');
  tty.Show('   Ключи:');
  tty.Show('     "h" -- подсказка');
  tty.Show('     "#" -- текущая версия');
  tty.Show('');
END Help;

BEGIN Args.ScanFlags;
  IF Args.Flag?('h') THEN Help END;
  IF Args.Flag?('#') THEN tty.Show(version) END;
  IF Args.Flag?('#') OR Args.Flag?('h') THEN HALT END;
  Args.TakeWord(out_name);
  SCR.init_scr; decorate;
  IF out_name[0]#0c THEN newFile(out_name) END;
  out_name:='';
  patts:=sle.New(); cmds:=sle.New(); trans:=sle.New();
  start; restore_all;
  tty.SetMode(FALSE);
  monitor;
END abc.
