MODULE abc; (* 21-Aug-89. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR;
FROM dicFile    IMPORT  open, close, text, find, get, flash_cash, time,
                        dictionary, put, create, rebalance, remove;

IMPORT  arg : tskArgs;
IMPORT  sle : strEditor;
IMPORT  kbd : Keyboard;
IMPORT  tty : Terminal;
IMPORT  ttf : StdIO;
IMPORT  clc : Time;
IMPORT  str : Strings;

VAR
  cnt     : INTEGER;
  add_cnt : INTEGER;
  dic     : dictionary;
  new     : dictionary;
  key     : text;
  out     : text;
  inh     : BOOLEAN; -- запрещает вывод на терминал словарной статьи
  std     : BOOLEAN; -- выдача на стандартный вывод
  last    : BOOLEAN;
  sle_trn : sle.descriptor;
  sle_cmd : sle.descriptor;

(*
PROCEDURE one_old(VAR w: abcVoc.Word): BOOLEAN;
  VAR ch: CHAR; r: BOOLEAN; inp: text;
BEGIN
  Str1(inp,w.org);
  IF get(dic,inp,out) THEN
    IF out#w.trn THEN
      print('%s\n',w.org);
      print('  %s\n',out);
      print('  %s\n',w.trn);
      print('     first?');
      REPEAT ch:=Read() UNTIL (ch='y') OR (ch='n');
      print('\n');
      r:=ch='n';
    ELSE
      r:=FALSE;
    END;
  ELSE
    r:=TRUE;
  END;
  IF r THEN
    Str1(out,w.trn);
    put(dic,inp,out);
    INC(cnt);
    print('%d\r',cnt);
  END;
  RETURN FALSE;
END one_old;

PROCEDURE get_old;
  PROCEDURE checkHALT(i: INTEGER);
  BEGIN
    IF i>=0 THEN RETURN END;
    print('IO error.\n'); HALT(1);
  END checkHALT;
  VAR nm: ARRAY [0..79] OF CHAR;
BEGIN
  LOOP
    cnt:=0;
    TakeWord(nm);
    IF nm='' THEN RETURN END;
    checkHALT(abcVoc.OpenVoc(nm));
    checkHALT(abcVoc.IterTree('*',one_old));
    checkHALT(abcVoc.CloseVoc());
    print('%8s %4d words\n',nm,cnt);
  END;
END get_old;
*)

PROCEDURE show(inp,out: text): BOOLEAN;
  VAR ln: text; ps,space,line: INTEGER;
  PROCEDURE prt(c: CHAR);
    VAR j: INTEGER;
  BEGIN
    IF ps>78 THEN
      IF space<30 THEN
        ln[ps]:='-'; INC(ps); ln[ps]:=0c;
        IF std THEN
          ttf.print('%s\n',ln); ps:=0;
        ELSE
          tty.print('%s\n',ln);
          FOR j:=0 TO 12 DO ln[j]:=' ' END;
          ln[11]:='|'; ps:=13;
        END;
        space:=-1;
      ELSE
        ln[space]:=0c;
        IF std THEN
          ttf.print('%s\n',ln);
          FOR j:=0 TO ps-space-2 DO ln[j]:=ln[space+1+j] END;
          ps:=ps-space-1;
        ELSE
          tty.print('%s\n',ln);
          FOR j:=0 TO 12 DO ln[j]:=' ' END;
          ln[11]:='|';
          FOR j:=0 TO ps-space-2 DO ln[j+13]:=ln[space+1+j] END;
          ps:=ps-space+12;
        END;
        FOR j:=13 TO ps-1 DO IF ln[j]=' ' THEN space:=ps END END;
      END;
    END;
    IF c=' ' THEN space:=ps END;
    ln[ps]:=c; INC(ps);
  END prt;
  VAR i: INTEGER; ch: CHAR;
BEGIN
  IF last & (time<clc.sys_time(clc.sec)-4*60*60) THEN RETURN FALSE END;
  INC(cnt);
  IF inh THEN RETURN FALSE END;
  ps:=0; space:=-1;
  IF std THEN prt(' '); prt(' ') END;
  i:=0; WHILE inp[i]#0c DO prt(inp[i]); INC(i) END;
  WHILE i<10 DO prt(' '); INC(i) END;
  prt(' '); prt('='); prt(' ');
  i:=0; WHILE out[i]#0c DO prt(out[i]); INC(i) END;
  ln[ps]:=0c;
  IF std THEN
    ttf.print('%s$\n',ln);
  ELSE
    tty.print('%s\n',ln);
    IF (cnt MOD 16)=0 THEN
      tty.print('---------------- press any key for continue ');
      kbd.read(ch);
      tty.print('\r                                            \r');
      IF ch=3c THEN RETURN TRUE END;
      tty.print('\n                         * * * * * *\n\n');
    END;
  END;
  RETURN FALSE;
END show;

PROCEDURE app_time(VAR ss: ARRAY OF CHAR; t: INTEGER);
  CONST MONTHs = "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec ";
  VAR y,m,d,h,mn,s: INTEGER; mon: ARRAY [0..7] OF CHAR;
BEGIN
  clc.unpack(t,y,m,d,h,mn,s);
  str.sub_arr(mon,MONTHs,(m-1)*4,3);
  str.append(ss,"%2d-%s-%02d %2d:%02d.%02d ",d,mon,y-1900,h,mn,s)
END app_time;

PROCEDURE one_new(i,o: text): BOOLEAN;
  VAR r: BOOLEAN; ch: CHAR; t: INTEGER;
BEGIN
  t:=time;
  IF get(dic,i,out) THEN
    IF out#o THEN
      tty.home; tty.erase(0); cnt:=0;
      key:=''; app_time(key,time);
      tty.print('%s\n',key);
      IF show(i,out) THEN END;
      key:=''; app_time(key,t);
      tty.print('\n%s\n',key);
      IF show(i,o) THEN END;
      tty.print('\n  Do you want first translation?');
      REPEAT kbd.read(ch) UNTIL (ch='y') OR (ch='n');
      tty.print('%c\n',ch);
      r:=ch='n';
    ELSE
      r:=FALSE;
    END;
  ELSE
    r:=TRUE;
  END;
  IF r THEN
    put(dic,i,o); INC(add_cnt);
    tty.print('%d\r',add_cnt);
  END;
  RETURN FALSE;
END one_new;

PROCEDURE get_new(n: INTEGER);
BEGIN
  WHILE n<=HIGH(arg.words) DO
    add_cnt:=0;
    IF open(new,arg.words[n]) THEN
      rebalance(new,one_new);
      close(new);
      tty.print('%s %4d words\n',arg.words[n],add_cnt);
    ELSE
      tty.print('Dictionary %s not found.\n',arg.words[n]);
    END;
    INC(n);
  END;
END get_new;

PROCEDURE clear(VAR t: text);
  VAR l: INTEGER;
BEGIN
  l:=0; WHILE t[l]#0c DO INC(l) END;
  WHILE (l>0) & (t[l-1]=' ') DO DEC(l); t[l]:=0c END;
  WHILE t[0]=' ' DO
    l:=0; REPEAT t[l]:=t[l+1]; INC(l) UNTIL t[l-1]=0c;
  END;
END clear;

(*
PROCEDURE get_text;
  VAR
    inp: File;
    nm : FileName;
    buf: ARRAY [0..4095] OF CHAR;
    blk: INTEGER;
    ptr: INTEGER;
    eof: INTEGER;
    ch : CHAR;
  PROCEDURE getc;
  BEGIN
    IF ptr>=4096 THEN
      INC(blk); ptr:=0;
      checkHALT(bRead(inp,blk,ADR(buf),4096),nm);
    END;
    ch:=buf[ptr]; DEC(eof); INC(ptr);
  END getc;
  VAR
    key: text;
    trn: text;
    wrk: text;
    i  : INTEGER;
    r  : BOOLEAN;
BEGIN
  LOOP
    TakeWord(nm);
    IF nm='' THEN EXIT END;
    checkHALT(OpenOnDir(CD(),inp,nm),nm);
    eof:=GetEof(inp); blk:=-1; ptr:=4096;
    LOOP
      REPEAT IF eof=0 THEN EXIT END; getc UNTIL (ch>' ');
      i:=0;
      REPEAT key[i]:=ch; INC(i); getc UNTIL (ch='=');
      key[i]:=0c; clear(key);
      REPEAT getc UNTIL (ch>' ');
      i:=0;
      REPEAT
        IF ch=NL THEN ch:=' ' END;
        trn[i]:=ch; INC(i); getc
      UNTIL (ch='$');
      trn[i]:=0c; clear(trn);
      r:=get(dic,key,wrk);
      IF r & (wrk=trn) THEN
        -- nothing
      ELSIF r THEN
        Home; Clear;
        IF show(key,wrk) THEN END;
        IF show(key,trn) THEN END;
        print('\n  Do you want first translation?');
        REPEAT ch:=Read() UNTIL (ch='y') OR (ch='n');
        print('%c\n',ch);
        IF ch='n' THEN put(dic,key,trn) END;
      ELSE
        Home; Clear;
        IF show(key,trn) THEN END;
        print('\n  Do you want insert this translation?');
        REPEAT ch:=Read() UNTIL (ch='y') OR (ch='n');
        print('%c\n',ch);
        IF ch='y' THEN put(dic,key,trn) END;
      END;
    END;
    checkHALT(Close(inp),nm);
  END;
END get_text;
*)

PROCEDURE expr?(t: text): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  i:=0;
  LOOP
    IF i>HIGH(t) THEN RETURN FALSE END;
    IF t[i]=0c THEN RETURN FALSE END;
    IF t[i]='*' THEN RETURN TRUE END;
    INC(i);
  END;
END expr?;

PROCEDURE try_translate(cmd: text);
  VAR trn: text;
BEGIN
  cnt:=0;
  find(dic,cmd,show);
  IF cnt=0 THEN
    IF expr?(cmd) THEN
      tty.print('Not found words compared with "%s".\n',cmd);
    ELSE
      tty.print('Word "%s" not found, enter translation:\n',cmd);
      sle.read_str('',trn,sle_trn);
      clear(trn);
      IF trn#'' THEN tty.print('\n'); put(dic,cmd,trn)
      ELSE tty.print('\r')
      END;
    END;
  END;
END try_translate;

PROCEDURE Monitor;
  VAR
    cmd,buf,wrd,trn: text; i: INTEGER;
    quit,edit,del: BOOLEAN;
    ch: CHAR;
BEGIN
  cmd:='';
  kbd.set_break(0);
  LOOP
    flash_cash;
    REPEAT
      sle.read_str('',cmd,sle_cmd);
      tty.print('\n');
      clear(cmd);
    UNTIL cmd#'';
    quit:=FALSE; edit:=FALSE; del:=FALSE;
    i:=0; buf:=cmd;
    WHILE (i<=HIGH(buf)) & (buf[i]#0c) DO
      IF buf[i]='/' THEN
        buf[i]:=' '; INC(i);
        WHILE (i<=HIGH(buf)) & (buf[i]#' ') & (buf[i]#0c) DO
          CASE buf[i] OF
            |'q': quit:=TRUE;
            |'e': edit:=TRUE;
            |'d': del :=TRUE;
          ELSE
            tty.print('Illegal key "%c".\n',buf[i]);
          END;
          buf[i]:=' '; INC(i);
        END;
      ELSE INC(i);
      END;
    END;
    clear(buf);
    LOOP
      wrd:=buf; clear(wrd); buf:='';
      IF wrd='' THEN EXIT END;
      IF del & NOT edit THEN
        IF get(dic,wrd,trn) THEN
          tty.print('Delete word "%s", are you sure?',wrd);
          REPEAT kbd.read(ch) UNTIL (ch='y') OR (ch='n');
          tty.print('%c\n',ch);
          IF ch='y' THEN remove(dic,wrd) END;
        ELSE
          tty.print('Word "%s" not found.\n',wrd);
        END;
      ELSIF edit THEN
        IF get(dic,wrd,trn) THEN
          tty.print('Enter new translation:\n');
          sle.read_str('',trn,sle_trn);
          clear(trn);
          IF trn#'' THEN tty.print('\n'); put(dic,wrd,trn)
          ELSE tty.print('\r')
          END;
        ELSE
          tty.print('Word "%s" not found.\n',wrd);
        END;
      ELSE
        try_translate(wrd);
      END;
    END;
    IF quit THEN RETURN END;
  END;
END Monitor;

VAR ch: CHAR; i: INTEGER;

BEGIN
  sle.new(sle_trn,4);
  sle.new(sle_cmd,4);
  IF arg.flag('-','h') THEN
    tty.print('abc -m {dictionary name}\n');
    tty.print('   пополнение словоря из других словарей\n');
    tty.print('abc -mo {dictionary name}\n');
    tty.print('   пополнение словоря из словарей старого формата\n');
    tty.print('abc -s <pattern>\n');
    tty.print('   распечатка словаря\n');
    tty.print('abc -si <pattern>\n');
    tty.print('   подсчет количества словарных статей\n\n');
    tty.print('   вызов без ключей - переход в монитор\n');
    tty.print('   команды монитора:\n');
    tty.print('     \\q выход\n');
    tty.print('     <pattern>\\e редактирование статьи\n');
    tty.print('     <pattern>\\d удаление статьи\n');
    HALT;
  END;
  inh:=FALSE; last:=FALSE; std:=FALSE;
  IF NOT open(dic,'vvv') THEN
    tty.print('Dictionary not found, create?');
    REPEAT kbd.read(ch) UNTIL (ch='y') OR (ch='n');
    tty.print('%c\n',ch);
    IF ch='n' THEN HALT END;
    create(dic,'vvv');
  END;
  IF arg.flag('-','m') THEN
    IF arg.flag('-','o') THEN HALT(50h); --get_old
    ELSIF arg.flag('-','t') THEN HALT(50h); --get_text;
    ELSE get_new(0);
    END;
  ELSIF arg.flag('-','s') THEN
    inh:=arg.flag('-','i'); last:=arg.flag('-','l'); std:=arg.flag('-','t');
    cnt:=0;
    FOR i:=0 TO HIGH(arg.words) DO
      str.print(key,'%s',arg.words[i]);
      find(dic,key,show);
    END;
    tty.print('%4d words\n',cnt);
  ELSE
    Monitor;
  END;
  close(dic);
END abc.
