IMPLEMENTATION MODULE exIO; (* Leo 05-Jul-87. (c) KRONOS *)

FROM SYSTEM    IMPORT   ADR, ADDRESS;
FROM ASCII     IMPORT   LF, NL, NUL, HT;
FROM exMem     IMPORT   insert, delete, app, jump, cur, last, put, maxline
                      , adr, size?;
FROM exHead    IMPORT   message, start, finish, alarm;
FROM exSetUp   IMPORT   public;
FROM exMain    IMPORT   inform_on, inform_off;
FROM exScreen  IMPORT   infomode, infomode?, fixpos, push, pop, pos;

IMPORT       Terminal;
IMPORT  str: Strings;
IMPORT  err: defErrors;
IMPORT  bio: BIO;
IMPORT  low: lowLevel;
IMPORT  lex: Lexicon;
IMPORT args: tskArgs;

VAR filename: ARRAY [0..255] OF CHAR;

PROCEDURE msg(VAL s: ARRAY OF CHAR);
BEGIN message(TRUE,"%s: %s",filename,s) END msg;

PROCEDURE check(): BOOLEAN;
  VAR s: ARRAY [0..63] OF CHAR;
BEGIN
  IF NOT bio.done THEN
    lex.perror(s,bio.error,"%%s"); msg(s); RETURN TRUE
  END;
  RETURN FALSE
END check;

PROCEDURE _check;
  VAR s: ARRAY [0..63] OF CHAR;
BEGIN
  IF NOT bio.done THEN
    lex.perror(s,bio.error,"%%s"); msg(s);
  END;
END _check;

TYPE PutProc = PROCEDURE(ARRAY OF CHAR, INTEGER): BOOLEAN;

VAR simp: ARRAY CHAR OF BOOLEAN;
    buff: ARRAY [0..4095] OF CHAR;
    lin : ARRAY [0..256] OF CHAR;

PROCEDURE readfile(f: bio.FILE; resh: PROC; putline: PutProc; VAR sep: CHAR);
  VAR  i: INTEGER;
    eof : INTEGER;
    len : INTEGER;
    co  : INTEGER;
    sco : INTEGER;
    endc: INTEGER;
    r   : BOOLEAN;
    ch  : CHAR;
    ln  : INTEGER;
    sav : INTEGER;
(*$<*) (*$T-*)
BEGIN
  sep:=NL;
  sav:=cur;
  eof:=bio.eof(f); len:=0; co:=HIGH(buff)+1; ln:=0;
  WHILE eof>0 DO
    IF co>HIGH(buff) THEN
      co:=0;
      fixpos(ln);
      IF eof>4096 THEN i:=4096 ELSE i:=eof END;
      bio.read(f,ADR(buff),i);
      IF NOT bio.done THEN _check; RETURN END;
    END;
    ch:=buff[co];
    endc:=eof-1;
    IF NOT simp[ch] THEN
      IF endc>HIGH(buff) THEN endc:=HIGH(buff) END;
      IF (endc-co)>=(HIGH(lin)-len) THEN endc:=HIGH(lin)-len+co-1 END;
      IF co<endc THEN
        sco:=co;
        REPEAT
          lin[len]:=ch; INC(len); INC(co);
          ch:=buff[co];
        UNTIL BITSET(co=endc)+BITSET(simp[ch])#{};
        DEC(eof,co-sco);
      END;
    END;
    CASE ch OF
      |NL,LF,NUL:
         sep:=ch;
         lin[len]:=0c;
         IF putline(lin,len) THEN
            msg("КОНЕЦ ФАЙЛА НЕ ПОМЕСТИЛСЯ В ПАМЯТЬ!");  eof:=0;
         END;
         len:=0; INC(ln);
         IF ln=public.high THEN resh END;
      |HT:
         REPEAT lin[len]:=' '; INC(len) UNTIL (len MOD 8)#0;
      |40c..177c, 240c..377c:
         IF len>=HIGH(lin) THEN
           lin[len]:=0c;
           IF putline(lin,len) THEN
             msg("КОНЕЦ ФАЙЛА НЕ ПОМЕСТИЛСЯ В ПАМЯТЬ!"); eof:=0;
           END;
           len:=0; INC(ln);
           IF ln=public.high THEN resh END;
         END;
         lin[len]:=ch; INC(len);
    ELSE
    END;
    INC(co); DEC(eof);
  END;
  IF len>0 THEN
    lin[len]:=0c;
    IF putline(lin,len) THEN
      msg("КОНЕЦ ФАЙЛА НЕ ПОМЕСТИЛСЯ В ПАМЯТЬ!");
    END;
  END;
  IF ln<public.high THEN resh END;
  jump(last+1); put("",0); (* to recalc last *)
  jump(sav);
(*$>*)
END readfile;

PROCEDURE dummy; BEGIN END dummy;

VAR gloresh: PROC;
    insco,lastins: INTEGER;

PROCEDURE ins(VAL str: ARRAY OF CHAR; len: INTEGER): BOOLEAN;
BEGIN
  IF insco=0 THEN insert(32); insco:=32 END;
  IF alarm THEN RETURN TRUE END;
  put(str,len);
  DEC(insco);
  jump(cur+1); lastins:=cur;
  RETURN alarm
END ins;

PROCEDURE reshins;
  VAR sav: INTEGER;
BEGIN
  IF insco>0 THEN
    sav:=cur; jump(lastins); delete(insco,0,dummy); jump(sav); insco:=0;
  END; gloresh;
END reshins;

PROCEDURE readinsert(f: bio.FILE; resh: PROC; VAR sep: CHAR);
  VAR sav: INTEGER;
BEGIN
  gloresh:=resh;
  insco:=0; lastins:=cur;
  readfile(f,reshins,ins,sep);
  IF insco>0 THEN
    sav:=cur; jump(lastins); delete(insco,0,dummy); jump(sav); insco:=0
  END
END readinsert;

PROCEDURE ReadFile(name: ARRAY OF CHAR; ins: BOOLEAN; resh: PROC;
                VAR sep: CHAR): BOOLEAN;
  VAR f: bio.FILE;
  inf,r: BOOLEAN;
BEGIN
  str.print(filename,'%s',name);
  bio.open(f,filename,'r');
  IF check() THEN RETURN TRUE END;
  inf:=infomode?(); infomode(TRUE);
  push; pos(999,0);
  inform_on("READING");
  IF ins THEN readinsert(f,resh,sep)
  ELSE readfile(f,resh,app,sep)
  END;
  inform_off; pop;
  infomode(inf);
  bio.close(f);
  RETURN check()
END ReadFile;

PROCEDURE writefile(f: bio.FILE; from,to: INTEGER; add: BOOLEAN; nl: CHAR): BOOLEAN;
  VAR
     lin: POINTER TO ARRAY [0..256] OF CHAR;
    buff: ARRAY [0..4095] OF CHAR;
  sav,co: INTEGER;
 ln,i,sz: INTEGER;
(*$<*) (*$T-*)
BEGIN
  sav:=cur;
  IF add THEN
    bio.seek(f,0,2)
  ELSE
    bio.seek(f,0,0)
  END;
  IF NOT bio.done THEN RETURN FALSE END;
  co:=0;
  fixpos(to);
  FOR ln:=from TO to DO
    jump(ln);
    lin:=adr(); sz:=size?();
    IF co+sz<HIGH(buff) THEN
      IF sz<5 THEN
        FOR i:=0 TO sz-1 DO buff[co]:=lin^[i]; INC(co) END;
      ELSE
        low.cmove(ADR(buff),co,lin,0,sz); INC(co,sz);
      END;
      buff[co]:=nl; INC(co);
    ELSIF co+sz=HIGH(buff) THEN
      IF sz<5 THEN
        FOR i:=0 TO sz-1 DO buff[co]:=lin^[i]; INC(co) END;
      ELSE
        low.cmove(ADR(buff),co,lin,0,sz); INC(co,sz);
      END;
      buff[co]:=nl; INC(co);
      bio.put(f,buff,4096);
      IF NOT bio.done THEN RETURN FALSE END;
      co:=0;
    ELSE
      IF i<5 THEN
        i:=0;
        WHILE co<=HIGH(buff) DO buff[co]:=lin^[i]; INC(co); INC(i) END;
      ELSE
        i:=HIGH(buff)-co+1;
        low.cmove(ADR(buff),co,lin,0,i)
      END;
      bio.put(f,buff,4096);
      IF NOT bio.done THEN RETURN FALSE END;
      co:=0;
      IF sz-i<5 THEN
        WHILE i<sz DO buff[co]:=lin^[i]; INC(co); INC(i) END;
      ELSIF i<sz THEN
        low.cmove(ADR(buff),co,lin,i,sz-i);  INC(co,sz-i);
      END;
      buff[co]:=nl; INC(co);
    END;
  END;
  IF co>0 THEN
    bio.put(f,buff,co);
    IF NOT bio.done THEN RETURN FALSE END;
  END;
  fixpos(sav);
  jump(sav);
(*$>*)
  RETURN TRUE
END writefile;

PROCEDURE executable(VAL name: ARRAY OF CHAR): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  IF args.flag('-','x') THEN RETURN TRUE END;
  i:=str.len(name);
  WHILE (i>=0) & (name[i]#'.') DO DEC(i) END;
  IF i<0 THEN RETURN FALSE END;
  INC(i);
  RETURN
     (i<HIGH(name)-1) & (name[i]='@') & (name[i+1]=0c)
  OR (i<HIGH(name)-2) & (name[i]='s') & (name[i+1]='h') & (name[i+2]=0c)
END executable;

PROCEDURE WriteFile(name: ARRAY OF CHAR; from,to: INTEGER;
                    add: BOOLEAN; nl: CHAR): BOOLEAN;
  VAR f: bio.FILE;
   i,dr: INTEGER;
    inf: BOOLEAN;
    cre: BOOLEAN;
     ok: BOOLEAN;
BEGIN
  str.print(filename,'%s',name);
  bio.open(f,filename,'w');
  cre:=NOT bio.done;
  IF cre THEN
    IF add & (bio.error#err.no_entry) THEN _check; RETURN TRUE END;
    bio.create(f,filename,'w',last*32);
    IF NOT bio.done THEN _check; RETURN TRUE END;
  END;
  inf:=infomode?(); infomode(TRUE);
  Terminal.set_cursor(0);
  push;
  inform_on("WRITING");
  ok:=writefile(f,from,to,add,nl);
  _check;
  inform_off; pop;
  Terminal.set_cursor(1);
  infomode(inf);
  IF NOT ok THEN RETURN TRUE END;
  bio.cut(f,bio.pos(f));
  _check;
  IF cre & executable(name) THEN
    bio.chaccess(f,bio.cmask);
    _check;
  END;
  bio.close(f);
  IF bio.done THEN RETURN FALSE END;
  _check;
  RETURN TRUE
END WriteFile;

VAR ch: CHAR;

BEGIN
  FOR ch:=0c TO 377c DO
    simp[ch]:=NOT ((ch>=40c)&(ch<177c) OR (ch>=240c))
  END
END exIO.
