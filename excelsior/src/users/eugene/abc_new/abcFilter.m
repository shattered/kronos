MODULE abcFilter; (* Hady. 28-Feb-89. (c) KRONOS *)

(* Leo: one_word.get_word IF changed *)

IMPORT  sys : SYSTEM;
IMPORT  ed  : myEditor;
IMPORT  tty : Terminal;
IMPORT  voc : abcVoc;
IMPORT  buf : abcBuf;
IMPORT  img : Image;
IMPORT  flio: Streams;
IMPORT  kbrd: Keyboard;
IMPORT  abuf: abcBuf;
IMPORT  res : Resource;
IMPORT  Args;

CONST version = 'abcFilter v0.0.1 /16-Mar-89/ (c) KRONOS';
      vt220   = TRUE;

MODULE misc;

  EXPORT QUALIFIED Latin?, Cyrill?, Little, letter?, reg_lett?;

  PROCEDURE Latin?(c: CHAR): BOOLEAN;
  BEGIN c:=CAP(c);
    RETURN (c>='A')&(c<='Z')
  END Latin?;

  PROCEDURE Cyrill?(c: CHAR): BOOLEAN;
  BEGIN RETURN (c>=300c)&(c<=377c) END Cyrill?;

  PROCEDURE Little(c: CHAR): CHAR;
  BEGIN
    IF    Latin?(c)  THEN RETURN CHAR(BITSET(c)+{5})
    ELSIF Cyrill?(c) THEN RETURN CHAR(BITSET(c)-{5})
    ELSE RETURN c
    END;
  END Little;

  PROCEDURE letter?(c: CHAR): BOOLEAN;
  BEGIN
    RETURN ((c>='a')&(c<='z')) OR ((c>='A')&(c<='Z')) OR (c>=300c)
  END letter?;

  PROCEDURE reg_lett?(c: CHAR): BOOLEAN;
  BEGIN
    RETURN letter?(c) OR (c='%') OR (c='*')
  END reg_lett?;

END misc;

---------------------------STRINGS------------------------------
                           -------

PROCEDURE app_str(VAR dest: ARRAY OF CHAR;
                  VAL sour: ARRAY OF CHAR;
                VAR s_dest: INTEGER;
                    h_sour: INTEGER);

  VAR i: INTEGER;

BEGIN i:=0;
  WHILE (s_dest<HIGH(dest)) & (i<h_sour) & (sour[i]#0c) DO
    dest[s_dest]:=sour[i]; INC(i); INC(s_dest)
  END;
END app_str;

PROCEDURE take_word(VAR dest: ARRAY OF CHAR;
                    VAL sour: ARRAY OF CHAR;
                  VAR p_sour: INTEGER): INTEGER;

  VAR i: INTEGER;

BEGIN i:=0;
  WHILE (i<HIGH(dest))     & (p_sour<HIGH(sour)) &
        (sour[p_sour]=' ') & (sour[p_sour]#0c)   DO
    dest[i]:=sour[p_sour]; INC(p_sour); INC(i)
  END;
  WHILE (i<HIGH(dest))     & (p_sour<HIGH(sour)) &
        (sour[p_sour]#' ') & (sour[p_sour]#0c) (*& (sour[p_sour]#';')*) DO
    dest[i]:=sour[p_sour]; INC(p_sour); INC(i)
  END;
  dest[i]:=0c;
  RETURN i;
END take_word;

PROCEDURE is_first?(VAL s: ARRAY OF CHAR; len: INTEGER): BOOLEAN;
  VAR i: INTEGER;
BEGIN i:=0;
  WHILE (i<len) & (s[i]=' ') DO INC(i) END;
  WHILE (i<len) & (s[i]#' ') DO INC(i) END;
  WHILE (i<len) & (s[i]=' ') DO INC(i) END;
  IF (i<len) & (s[i]='=') THEN RETURN TRUE
  ELSE RETURN FALSE
  END;
END is_first?;

----------------------------ERRORS------------------------------
                            ------
CONST -- ERRORS OF MODULES --
  ExistOpened= -100h;
  NotOpened  = -101h;
  NoSuchWord = -102h;
  NoMemory   = -103h;
  EmptyBuf   = -104h;

PROCEDURE vis_error(err: INTEGER; VAR s: ARRAY OF CHAR);
  VAR t: ARRAY [0..79] OF CHAR;
BEGIN
  IF err<0 THEN
    IF    err=NoSuchWord  THEN t:='Неизвестное слово'
    ELSIF err=NoMemory    THEN t:='Нет памяти в буфферe'
    ELSIF err=EmptyBuf    THEN t:='Буффер пуст'
    ELSIF err=NotOpened   THEN
      t:='Обращение к неоткрытому словарю'
    ELSIF err=ExistOpened THEN t:='Словарь уже открыт'
    ELSE img.image0(t,''); flio.Why?(err,t);
    END;
  END;
  img.image(s,' %s; ',t);
END vis_error;

PROCEDURE chk(err: INTEGER);
  VAR rep: ARRAY [0..79] OF CHAR;
BEGIN
  IF err<0 THEN img.image0(rep,''); vis_error(err,rep);
    ed.message(TRUE,' %s',rep); HALT
  END;
END chk;

CONST scr_start = 15;

VAR decor?: BOOLEAN;

PROCEDURE decorate;

  CONST SPACES = '---------------------------------------';
        BLANKS = '                                       ';

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

BEGIN
  IF decor? THEN
    tty.SetCrs(scr_start,0); tty.Clear;
    IF vt220 THEN
      tty.WriteString(''033c'[4m');  -- underline on
      tty.WriteString(BLANKS);
      tty.WriteString(''033c'[24m'); -- underline off
      tty.WriteString(''033c'#6');   -- double width line atribute
      decor('vocabulary',scr_start,15);
    ELSE tty.WriteString(SPACES);
      tty.WriteString(SPACES);
      decor('VOCABULARY',scr_start,46);
    END;
    tty.print('\n'); decor?:=FALSE
  ELSE
    tty.SetCrs(scr_start+1,0); tty.Clear;
  END;
END decorate;

PROCEDURE final;
BEGIN
  IF vt220 & NOT decor? THEN
    tty.SetCrs(scr_start,0); tty.ClearLine;
    tty.WriteString(''033c'#5');
  END;
END final;

VAR q_char: CHAR;

PROCEDURE query(VAL patt,fmt: ARRAY OF CHAR; SEQ arg: sys.WORD);

  PROCEDURE in_patt?(c: CHAR): BOOLEAN;
    VAR i: INTEGER;
  BEGIN i:=0;
    WHILE (i<=HIGH(patt))&(patt[i]#0c) DO
      IF patt[i]=c THEN RETURN TRUE END; INC(i);
    END;
    RETURN FALSE;
  END in_patt?;

BEGIN
  ed.message(FALSE,fmt,arg);
  LOOP  q_char:=misc.Little(kbrd.ReadKey());
    IF in_patt?(q_char) THEN RETURN
    ELSE tty.Write(07c)
    END;
  END;
END query;

VAR t_lin, t_col, t_last: INTEGER;
                   frame: BOOLEAN;


PROCEDURE one_word(VAR s: ARRAY OF CHAR);

  VAR bump: ARRAY [0..255] OF CHAR;
     len,i: INTEGER;

  PROCEDURE get_word;
    VAR l: INTEGER;
  BEGIN
    IF misc.reg_lett?(bump[i]) OR (i>0) & misc.reg_lett?(bump[i-1]) THEN
                             ---------------------------------- Leo
      REPEAT DEC(i)
      UNTIL (i<0) OR (NOT misc.reg_lett?(bump[i]));
      INC(i);
    ELSE
      REPEAT INC(i)
      UNTIL (i>=len) OR misc.reg_lett?(bump[i]);
    END;
    IF i>=len THEN s[0]:=0c; RETURN END;
    l:=0;
    WHILE misc.reg_lett?(bump[i]) & (i<=len) DO
      s[l]:=bump[i]; INC(l); INC(i)
    END; s[l]:=0c;
  END get_word;

BEGIN
  ed.crs_pos(len,i); ed.get(bump,len); get_word;
END one_word;

PROCEDURE regular?(VAR s: ARRAY OF CHAR): BOOLEAN;
  VAR i: INTEGER; reg: BOOLEAN;
BEGIN i:=0; reg:=FALSE;
  WHILE (i<HIGH(s))&(s[i]#0c) DO
    s[i]:=misc.Little(s[i]);
    IF (s[i]='*') OR (s[i]='%') THEN reg:=TRUE END;
    INC(i);
  END;
  RETURN reg;
END regular?;

TYPE show_proc = PROCEDURE (ARRAY OF CHAR, INTEGER);

PROCEDURE vis_word(VAL w: voc.Word; show: show_proc);

  CONST limit = 60; spaces = '        ';

  VAR         bump: ARRAY [0..255] OF CHAR;
         last_word: ARRAY [0..255] OF CHAR;
    p_dest, p_sour: INTEGER;
          last_len: INTEGER;

  PROCEDURE ini_bump(VAL sour: ARRAY OF CHAR);
  BEGIN p_dest:=0;
    WHILE (p_dest<HIGH(bump)) &
          (p_dest<HIGH(sour)) & (sour[p_dest]#0c) DO
      bump[p_dest]:=sour[p_dest]; INC(p_dest)
    END;
  END ini_bump;

  PROCEDURE make_str(VAL sour: ARRAY OF CHAR);
  BEGIN
    IF last_len#0 THEN
      app_str(bump, last_word, p_dest, last_len);
    END;
    last_len:=take_word(last_word, sour, p_sour);
    WHILE (p_dest+last_len<=limit) & (last_len#0) DO
      app_str(bump, last_word, p_dest, last_len);
      last_len:=take_word(last_word, sour, p_sour);
    END;
  END make_str;

BEGIN last_len:=0; p_sour:=0;
  ini_bump(w.org);
  app_str(bump,' = ',p_dest,3);
  make_str(w.trn);
  show(bump,p_dest);
  WHILE last_len#0 DO ini_bump(spaces);
    make_str(w.trn); show(bump,p_dest);
  END;
END vis_word;

PROCEDURE ins_line(VAL s: ARRAY OF CHAR; l: INTEGER);
BEGIN INC(t_lin); INC(t_last);
  ed.jump(t_lin); ed.ins(1); ed.put(s,l);
END ins_line;

PROCEDURE insert_word(VAL w: voc.Word);
BEGIN vis_word(w,ins_line);
END insert_word;

VAR show_all: BOOLEAN;

PROCEDURE tty_show(s: ARRAY OF CHAR; l: INTEGER);
BEGIN s[l]:=0c; tty.print('%s\n',s) END tty_show;

PROCEDURE show_one(VAR w: voc.Word): BOOLEAN;
BEGIN
  IF show_all THEN insert_word(w); RETURN FALSE
  ELSE decorate; vis_word(w,tty_show);
    query('yna'033c'',' Insert [Y/N] ? Cancel [ESC] ? All [a] ?');
    CASE q_char OF
      |'y' : insert_word(w); RETURN FALSE
      |'n' : RETURN FALSE
      |'a' : show_all:=TRUE; insert_word(w); RETURN FALSE
      |033c: RETURN TRUE
    ELSE
    END;
  END;
END show_one;

PROCEDURE show_seq(VAL s: ARRAY OF CHAR);
BEGIN decor?:=TRUE; show_all:=FALSE;
  chk(voc.IterTree(s,show_one));
  IF decor? THEN
    ed.message(TRUE,' ТАКИХ НЕТ ...');
  ELSE
  END;
END show_seq;

PROCEDURE make_one(VAR s: ARRAY OF CHAR): BOOLEAN;
  VAR w: voc.Word; i: INTEGER;
BEGIN
  IF regular?(s) THEN show_seq(s);
  ELSE
    i:=voc.FindWord(s,w);
    IF i<0 THEN chk(i)
    ELSIF i=0 THEN RETURN TRUE;
    ELSE insert_word(w);
    END;
  END;
  RETURN FALSE;
END make_one;

PROCEDURE trans_one;
  VAR s: ARRAY [0..79] OF CHAR;
BEGIN
  one_word(s);
  IF make_one(s) THEN
    ed.message(FALSE,'СЛОВО НЕ НАЙДЕНО: "%s"',s);
  END;
END trans_one;

PROCEDURE go_up(VAR s: ARRAY OF CHAR): BOOLEAN;
  VAR len: INTEGER;
BEGIN
  LOOP ed.get(s,len); s[len]:=0c;
    IF len=0 THEN RETURN TRUE END;
    IF is_first?(s,len) THEN RETURN FALSE END;
    t_lin:=t_lin-1;
    IF t_lin<0 THEN
      t_lin:=0; RETURN TRUE
    ELSE ed.jump(t_lin)
    END;
  END;
END go_up;

PROCEDURE find_fin(): INTEGER;
  VAR       bump: ARRAY [0..255] OF CHAR;
    marg,len,fin: INTEGER;

  PROCEDURE count_spac(): INTEGER;
    VAR i: INTEGER;
  BEGIN i:=0;
    WHILE (i<len) & (bump[i]=' ') DO INC(i) END;
    RETURN i;
  END count_spac;

BEGIN fin:=t_lin+1; marg:=-1;
  LOOP
    IF fin>ed.last() THEN EXIT END;
    ed.jump(fin); ed.get(bump,len);
    IF (len=0) OR (is_first?(bump,len)) THEN EXIT END;
    IF    marg<0 THEN marg:=count_spac()
    ELSIF marg#count_spac() THEN EXIT
    END;
    fin:=fin+1;
  END;
  ed.jump(t_lin); RETURN fin;
END find_fin;

PROCEDURE make_word(VAR w: voc.Word;
                    VAR s: ARRAY OF CHAR;
                      fin: INTEGER);
  VAR p_sour,i: INTEGER;

  PROCEDURE pool_str(VAR s: ARRAY OF CHAR);
    VAR i,j: INTEGER;
  BEGIN i:=0; j:=0;
    WHILE (i<HIGH(s))&(s[i]=' ') DO INC(i) END;
    WHILE (i<HIGH(s))&(s[i]# 0c) DO
      s[j]:=s[i]; INC(i); INC(j)
    END; s[j]:=0c;
  END pool_str;

BEGIN p_sour:=0;
  i:=take_word(w.org,s,p_sour); pool_str(w.org);
  i:=take_word(w.trn,s,p_sour); pool_str(w.trn); ASSERT(w.trn[0]='=');
  i:=0;
  WHILE (i<HIGH(s))&(i<p_sour) DO s[i]:=' '; INC(i) END;
  pool_str(s); p_sour:=0;
  app_str(w.trn,s,p_sour,HIGH(s));
  LOOP
    IF p_sour>=HIGH(w.trn)-1 THEN EXIT END;
    w.trn[p_sour]:=' '; INC(p_sour); t_lin:=t_lin+1;
    IF t_lin>=fin THEN EXIT END;
    ed.jump(t_lin); ed.get(s,i); pool_str(s);
    app_str(w.trn,s,p_sour,i);
  END;
  w.trn[p_sour]:=0c;
END make_word;

PROCEDURE ask_truth(VAL w: voc.Word;
                      fmt: ARRAY OF CHAR;
                  SEQ arg: sys.WORD): BOOLEAN;
BEGIN
  decorate;
  vis_word(w,tty_show);
  query('yn',fmt,arg);
  CASE q_char OF
    |'y' : RETURN TRUE
    |'n' : RETURN FALSE
  ELSE
  END;
END ask_truth;

PROCEDURE inp_one;
  VAR bump: ARRAY [0..255] OF CHAR;
   fin_lin: INTEGER;
       O,W: voc.Word;
BEGIN decor?:=TRUE;
  IF go_up(bump) THEN
    ed.message(FALSE,' ИЗВИНИТЕ, НЕ МОГУ ВЫДЕЛИТЬ СЛОВАРНУЮ СТАТЬЮ');
  ELSE fin_lin:=find_fin();
    make_word(W,bump,fin_lin);
    IF (W.trn[0]=0c) OR (W.org[0]=0c) OR regular?(W.org) THEN
      ed.message(FALSE,'ИЗВИНИТЕ, НЕДОПУСТИМЫЙ ВИД СЛОВАРНОЙ СТАТЬИ');
    ELSIF ask_truth(W,'Insert this one to vocabulary [Y/N] ?') THEN
      IF voc.FindWord(W.org,O)>=0 THEN
        IF ask_truth(O,'There is such in vocabulary, are you shure [Y/N] ?')
        THEN chk(voc.GetWord(W.org,O)); chk(voc.PutWord(W))
        END;
      ELSE chk(voc.PutWord(W))
      END;
    END;
  END;
END inp_one;

PROCEDURE go_dw(VAR s: ARRAY OF CHAR): BOOLEAN;
  VAR len: INTEGER;
BEGIN
  LOOP ed.get(s,len); s[len]:=0c;
    IF is_first?(s,len) THEN RETURN TRUE END;
    t_lin:=t_lin+1;
    IF t_lin>t_last THEN RETURN FALSE ELSE ed.jump(t_lin) END;
  END;
END go_dw;

PROCEDURE inp_frame;
  VAR bump: ARRAY [0..255] OF CHAR;
  cnt, fin: INTEGER;
         W: voc.Word;
       all: BOOLEAN;
BEGIN ed.jump(t_lin); cnt:=0; decor?:=TRUE; all:=FALSE;
  WHILE t_lin<=t_last DO
    IF go_dw(bump) THEN
      fin:=find_fin();
      make_word(W,bump,fin);
      IF (W.trn[0]=0c) OR (W.org[0]=0c) OR regular?(W.org) THEN
        ed.message(FALSE,' НЕДОПУСТИМЫЙ ВИД СЛОВАРНОЙ СТАТЬИ');
      ELSE INC(cnt);
        IF NOT all THEN decorate; vis_word(W,tty_show);
          query(''033c'yna',' Insert to vocabulary [Y/N]; [ESC] to cancel; all [A]?');
          CASE q_char OF
            |'y' : chk(voc.PutWord(W));
            |'a' : all:=TRUE;
            |033c: RETURN
          ELSE
          END;
        ELSE chk(voc.PutWord(W));
        END;
      END;
      t_lin:=fin; ed.jump(t_lin);
    END;
  END;
  IF cnt=0 THEN
    ed.message(FALSE,' НЕ СМОГ ВЫДЕЛИТЬ НИ ОДНОЙ СЛОВАРНОЙ СТАТЬИ В ОБЛАСТИ');
  END;
END inp_frame;

PROCEDURE inp_trans;
BEGIN
  IF frame THEN inp_frame ELSE inp_one END;
END inp_trans;

VAR buff_depth: INTEGER;

PROCEDURE trans_line;
  VAR bump: ARRAY [0..255] OF CHAR;
      word: ARRAY  [0..79] OF CHAR;
      b_cc: INTEGER;
       len: INTEGER;

  PROCEDURE sub_patt();
    VAR i: INTEGER;
  BEGIN i:=0;
    WHILE (b_cc<HIGH(bump)) & (bump[b_cc]#0c) &
               (NOT misc.letter?(bump[b_cc])) DO INC(b_cc)
    END;
    WHILE (b_cc<HIGH(bump)) & (i<HIGH(word)) &
                  (misc.letter?(bump[b_cc])) DO
      word[i]:=bump[b_cc]; INC(i); INC(b_cc)
    END;
    word[i]:=0c;
  END sub_patt;

BEGIN ed.jump(t_lin); ed.get(bump,len);
  IF len#0 THEN b_cc:=0;
    REPEAT sub_patt;
      IF make_one(word) THEN
        chk(abuf.PutString(word)); INC(buff_depth);
      END;
    UNTIL b_cc>=len;
  END;
END trans_line;

PROCEDURE sh_buff(VAL s: ARRAY OF CHAR): BOOLEAN;
  VAR bump: ARRAY [0..79] OF CHAR;
         i: INTEGER;
BEGIN i:=0;
  WHILE (i<HIGH(bump)-3) & (s[i]#0c) DO bump[i]:=s[i]; INC(i) END;
  bump[i]:=' '; INC(i);
  bump[i]:='='; INC(i);
  bump[i]:=0c; ins_line(bump,i);
  RETURN FALSE;
END sh_buff;

PROCEDURE put_buffer; BEGIN abuf.ShowBuffer(sh_buff) END put_buffer;

PROCEDURE trans_frame;
BEGIN
  IF frame THEN
    buff_depth:=0;
    WHILE t_lin<=t_last DO
      trans_line; t_lin:=t_lin+1;
    END;
    t_lin:=t_last;
    IF buff_depth>0 THEN
      query('yn','В БУФЕРЕ [%d] СЛОВ, БУДЕТЕ ПЕРЕВОДИТЬ [Y/N] ?',buff_depth);
      IF q_char='y' THEN put_buffer END;
    END;
  ELSE trans_one;
  END;
END trans_frame;

PROCEDURE Help;
BEGIN
  tty.Show(' ');
  tty.Show(' Фильтр-словарь abcFilter позволяет:');
  tty.Show('   переводить отдельные слова,'     );
  tty.Show('   помеченные курсором: без ключей;');
  tty.Show('   вводить отдельные переводы'      );
  tty.Show('   из редактируемого текста: "-t";' );
  tty.Show('   обрабатывать области:     "-f";' );
  tty.Show(' ');
END Help;

PROCEDURE work_frame;
  VAR i: INTEGER;

  PROCEDURE min(VAR x,y: INTEGER); VAR i: INTEGER;
  BEGIN IF x>y THEN i:=x; x:=y; y:=i END  END min;

BEGIN
  ed.frame(t_lin,t_col,t_last,i); min(t_lin,t_last);
  IF (t_lin<0) OR (t_last<0) THEN
    ed.message(TRUE,'НЕ ПОМЕЧЕНЫ НАЧАЛО И КОНЕЦ ОБЛАСТИ');
    HALT;
  END;
  frame:=TRUE;
END work_frame;

CONST my_files = 'vvv';

BEGIN Args.ScanFlags; frame:=FALSE; decor?:=TRUE;
  IF Args.Flag?('h') THEN Help END;
  IF Args.Flag?('#') THEN tty.Show(version) END;
  IF Args.Flag?('h') OR Args.Flag?('#') THEN HALT END;
  ed.message(FALSE,version);
  IF Args.Flag?('f') THEN work_frame
  ELSE
    ed.crs_pos(t_lin,t_col); t_last:=0;
  END;
  res.Final(final);
  chk(voc.OpenVoc(my_files));
  IF Args.Flag?('t') THEN inp_trans;
  ELSE trans_frame;
  END;
  chk(voc.CloseVoc());
END abcFilter.
