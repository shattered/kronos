MODULE t; (* 21-Aug-89. (c) KRONOS *)

IMPORT vocVoc;
FROM BIO        IMPORT  checkHALT;
FROM dicFile    IMPORT  open, close, text, find, get,
                        dictionary, put, create, rebalance;
FROM Args       IMPORT  TakeWord, Flag?, ScanFlags;
FROM Terminal   IMPORT  print, Read;

VAR cnt: INTEGER;
    dic: dictionary;
    new: dictionary;
    old: vocVoc.dictionary;
    key: text;
    out: text;

PROCEDURE one_old(): BOOLEAN;
  VAR ch: CHAR; r: BOOLEAN;
BEGIN
  IF get(dic,vocVoc.word,out) THEN
    IF out#vocVoc.trn THEN
      print('%s\n',vocVoc.word);
      print('  %s\n',out);
      print('  %s\n',vocVoc.trn);
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
    put(dic,vocVoc.word,vocVoc.trn);
    INC(cnt);
    print('%d\r',cnt);
  END;
  RETURN FALSE;
END one_old;

PROCEDURE get_old;
  VAR nm: ARRAY [0..79] OF CHAR;
BEGIN
  LOOP
    cnt:=0;
    TakeWord(nm);
    IF nm='' THEN RETURN END;
    checkHALT(vocVoc.open(old,nm),'open');
    vocVoc.key:='*';
    checkHALT(vocVoc.find(old,one_old),'find');
    checkHALT(vocVoc.close(old),'close');
    print('%8s %4d words\n',nm,cnt);
  END;
END get_old;

PROCEDURE one_new(i,o: text): BOOLEAN;
BEGIN
  put(dic,i,o);
  INC(cnt);
  print('%d\r',cnt);
  RETURN FALSE;
END one_new;

PROCEDURE get_new;
  VAR nm: ARRAY [0..79] OF CHAR;
BEGIN
  LOOP
    cnt:=0;
    TakeWord(nm);
    IF nm='' THEN RETURN END;
    open(new,nm);
    rebalance(new,one_new);
    close(new);
    print('%8s %4d words\n',nm,cnt);
  END;
END get_new;


PROCEDURE show(i,o: text): BOOLEAN;
BEGIN
  print('%s = %s\n',i,o);
  INC(cnt);
  RETURN FALSE;
END show;

BEGIN
  ScanFlags;
  IF Flag?('c') THEN create(dic,'vvv'); close(dic); HALT END;
  open(dic,'vvv');
  IF Flag?('m') THEN
    IF Flag?('o') THEN get_old ELSE get_new END;
  ELSE
    TakeWord(key); find(dic,key,show);
  END;
  close(dic);
END t.
