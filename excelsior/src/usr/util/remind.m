MODULE remind; (* Hady. 05-Apr-89. (c) KRONOS *)
               (* Hady. 29-Jan-91. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  tty: Terminal;
IMPORT  key: Keyboard;
IMPORT  env: tskEnv;
IMPORT  arg: tskArgs;
IMPORT  str: Strings;
IMPORT  err: defErrors;
IMPORT  mem: Heap;
IMPORT  BIO, ASCII;
IMPORT  Time;

WITH STORAGE: mem;

CONST default = "REMIND.DAT";

CONST NUL = ASCII.NUL;

---------------------- EDIT TIME FOR DEBUG ---------------------
                      ---------------------

MODULE editor;

  IMPORT  tty, key, img: str, Time;

  EXPORT  write_time, edit_time, monthes, days;

  CONST (* ACHTUNG ! This format must not be changed !!! *)
     time_format = " %3.3s  %3.3s %02d  %02d:%02d.%02d  %04d. ";

(*              0123456789012345678901234567890
                 Thu  Apr 05  16:58.33  1990.            *)

  CONST Margin = 20;
  VAR Line: INTEGER;

  --------------------------- TIMES ------------------------------
                             -------

  VAR time: INTEGER;
       str: ARRAY [0..80] OF CHAR;

  CONST p_mon   = 00;  p_day   = 01;
        p_hour  = 02;  p_min   = 03;
        p_sec   = 04;  p_year  = 05;

  poss = ARRAY OF INTEGER { Margin+06, Margin+10, Margin+14,
                            Margin+17, Margin+20, Margin+24};

  TYPE str12= ARRAY [0..11] OF CHAR;
  VAR monthes: ARRAY [1..12] OF str12;
         days: ARRAY [1..7]  OF str12;

  VAR Mon, Year, Hour, Min, Sec, Day: INTEGER;

  PROCEDURE AppTime(VAR s: ARRAY OF CHAR; time: INTEGER);
    CONST time_format = " %3.3s  %3.3s %02d  %02d:%02d.%02d  %04d. ";
  BEGIN
    Time.unpack(time, Year, Mon, Day, Hour, Min, Sec);
    img.print(s, time_format, days[Time.day(time)],
                            monthes[Mon], Day, Hour, Min, Sec, Year);
  END AppTime;

  PROCEDURE wr_time(tt: BOOLEAN);
    VAR len: INTEGER; b: INTEGER;
  BEGIN
    AppTime(str,time);
    tty.set_pos(Line,Margin); tty.set_reverse(1);
    tty.WriteString(str);
    tty.set_reverse(0); tty.erase_line(0)
  END wr_time;

  PROCEDURE write_time(tim: INTEGER);
  BEGIN time:=tim; wr_time(FALSE) END write_time;

  PROCEDURE edit_time(VAR tim: INTEGER): BOOLEAN;

    VAR pos: INTEGER;

    PROCEDURE max_day(Mon, Year: INTEGER): INTEGER;
    BEGIN
      ASSERT(Mon IN {1..12});
      IF (Mon=2) & (Year MOD 4 = 0) &
         ((Year MOD 100 # 0) OR (Year MOD 400 = 0)) THEN RETURN 29
      ELSIF Mon=2 THEN RETURN 28;
      ELSIF Mon IN {1,3,5,7,8,10,12} THEN RETURN 31
      ELSE  RETURN 30
      END;
    END max_day;

    PROCEDURE sync_time;
    BEGIN
      IF Day>max_day(Mon,Year) THEN
        Day:=max_day(Mon, Year)
      END;
      time:=Time.pack(Year,Mon,Day,Hour,Min,Sec);
      wr_time(TRUE)
    END sync_time;

    PROCEDURE sync;
    BEGIN
      tty.set_pos(Line,poss[pos])
    END sync;

    CONST digits = ARRAY OF CHAR {"0","1","2","3","4",
                                  "5","6","7","8","9"};

    VAR ch: CHAR;

    PROCEDURE edit2(VAR val: INTEGER; l,h: INTEGER);
      VAR d: ARRAY [0..3] OF INTEGER;
          p: INTEGER;

      PROCEDURE check(): BOOLEAN;
        VAR i: INTEGER;
      BEGIN
        i:=d[0]*10+d[1];
        RETURN (i>=l) & (i<=h)
      END check;

    BEGIN p:=0;
      val:=val MOD 100;
      d[0]:=val DIV 10;
      d[1]:=val MOD 10;
      tty.set_reverse(1);
      LOOP key.read(ch);
        CASE ch OF
          |key.right : IF p>0 THEN EXIT END;
                       tty.right(1); p:=1
          |key.left  : IF p=0 THEN EXIT END;
                       tty.left(1) ; p:=0
          |key.up,"+":
             REPEAT d[p]:=(d[p]+1) MOD 10 UNTIL check();
             tty.Write(digits[ORD(d[p])]);  tty.left(1)
          |key.dw,"-":
             REPEAT d[p]:=(d[p]+9) MOD 10 UNTIL check();
             tty.Write(digits[ORD(d[p])]);  tty.left(1)
          |key.cr, 033c: EXIT
        ELSE
        END;
      END;
      tty.set_reverse(0);
      val:=d[0]*10+d[1]
    END edit2;

    PROCEDURE edit_dig(VAR val: INTEGER; l,h: INTEGER);
      VAR old,D: INTEGER;
    BEGIN
      D:=h-l;
      LOOP
        key.read(ch);
        IF    (ch="+") OR (ch=key.up) THEN
          val:=l+((val-l+1) MOD (D+1)); sync_time; sync
        ELSIF (ch="-") OR (ch=key.dw) THEN
          val:=l+((val-l+D) MOD (D+1)); sync_time; sync
        ELSIF (ch=33c)      OR (ch=key.cr)    OR
              (ch=key.left) OR (ch=key.right) THEN EXIT
        END;
      END;
    END edit_dig;

  BEGIN
    time:=tim ; wr_time(TRUE);
    pos:=p_mon; sync;
    LOOP
      CASE pos OF
        |p_mon : edit_dig(Mon,1,12)
        |p_day : edit_dig(Day,1,max_day(Mon,Year))
        |p_hour: edit2(Hour,0,23)
        |p_min : edit2(Min ,0,59)
        |p_sec : edit2(Sec ,0,59)
        |p_year: edit_dig(Year,1986,2017)
      END;
      IF    ch=key.left  THEN pos:=(pos+5) MOD 6; sync
      ELSIF ch=key.right THEN pos:=(pos+1) MOD 6; sync
      ELSIF (ch=key.cr) OR (ch=33c) THEN  EXIT
      END
    END;
    sync_time; tim:=time; RETURN ch#033c
  END edit_time;

BEGIN
  tty.reset;
  Line:=tty.state^.lines-1;

  monthes [1] := "January"  ;   monthes [2] := "February";
  monthes [3] := "March"    ;   monthes [4] := "April";
  monthes [5] := "May"      ;   monthes [6] := "June";
  monthes [7] := "July"     ;   monthes [8] := "August";
  monthes [9] := "September";   monthes[10] := "October";
  monthes[11] := "November" ;   monthes[12] := "December";

  days[1] := "Monday"   ;  days[2] := "Tuesday" ;
  days[3] := "Wednesday";  days[4] := "Thursday";
  days[5] := "Friday"   ;  days[6] := "Saturday";
  days[7] := "Sunday"
END editor;
------------------------ I/O & ERRORS --------------------------
                        --------------

VAR buff: DYNARR OF CHAR;
    pos: INTEGER;

PROCEDURE set_pos(p: INTEGER);
BEGIN ASSERT(p<=HIGH(buff)); pos:=p END set_pos;

PROCEDURE check_bio(VAL op,name: ARRAY OF CHAR): BOOLEAN;
BEGIN
  IF BIO.done THEN RETURN FALSE END;
  IF BIO.error#err.no_entry THEN
    tty.perror(BIO.error,'%s("%s"): %%s\n',op,name);
    HALT
  ELSE RETURN TRUE
  END;
END check_bio;

PROCEDURE open(VAL name: ARRAY OF CHAR): BOOLEAN;
  VAR f: BIO.FILE; r: BOOLEAN;
   dirs: BIO.PATHs;
BEGIN
  BIO.get_paths(dirs,env.etc);
  IF check_bio("open_path",env.etc) THEN RETURN FALSE END;
  BIO.lookup(dirs,f,name,'r');
  IF check_bio("open",name)         THEN RETURN FALSE END;
  pos:=BIO.eof(f); NEW(buff,pos);
  BIO.get(f,buff,pos);
  IF check_bio('read',name) THEN RETURN FALSE END;
  pos:=0; BIO.purge(f);
  RETURN TRUE
END open;

VAR bump: ARRAY [0..255] OF CHAR;
      cc: INTEGER;
     lcc: INTEGER;

PROCEDURE put(c: CHAR);
BEGIN
  bump[cc]:=c; cc:=cc+1;
  IF cc>=HIGH(bump) THEN cc:=HIGH(bump) END;
END put;

PROCEDURE get_char(): CHAR;
  VAR c: CHAR;
BEGIN
  IF pos>=HIGH(buff) THEN c:=ASCII.EOF
  ELSE pos:=pos+1; c:=buff[pos-1]
  END;
  IF c=ASCII.NL THEN put(NUL); cc:=0; INC(lcc);
  ELSE put(c);
  END;
  RETURN c;
END get_char;

PROCEDURE error(VAL fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD); FORWARD;
PROCEDURE unexp(VAL s: ARRAY OF CHAR);
BEGIN error(' Ожидался символ "%s".', s);
END unexp;

-------------------------- STRINGS -----------------------------
                          ---------
TYPE STR = DYNARR OF CHAR;
     STRING = POINTER TO STR;

MODULE strings; (* Hady 03-Apr-89. (c) KRONOS *)

IMPORT  mem;
IMPORT  SYSTEM;
IMPORT  error;
IMPORT  STR, STRING;

EXPORT new_str, kill_str;

PROCEDURE new_str(VAL s: ARRAY OF CHAR): STRING;
  VAR r: STRING; i: INTEGER;
BEGIN
  NEW(r);
  IF r=NIL THEN error(' нет памяти !'); HALT(1) END;
  i:=0;
  WHILE (i<HIGH(s))&(s[i]#0c) DO INC(i) END;
  NEW(r^,i+1);
  FOR i:=0 TO HIGH(r^) DO r^[i]:=s[i] END;
  RETURN r;
END new_str;

PROCEDURE kill_str(VAR s: STRING);
BEGIN
  ASSERT(s#NIL); DISPOSE(s^); DISPOSE(s); s:=NIL
END kill_str;

END strings;

----------------------- SCANER & HASH --------------------------
                       ---------------

MODULE scan; (* Hady. 28-Mar-89. (c) KRONOS *)

IMPORT  STRING, new_str, kill_str;
IMPORT  tty;
IMPORT  sci: ASCII;
IMPORT  mem;
IMPORT  SYSTEM;

EXPORT QUALIFIED hash_sz, never, ident, str, dig, point,
                 comma, colomn, semic, o_brack, minus, equ,
                 UTILIZE, util, get_c, lex, sym, val, final,
                 ini, get_lex, skip_eol, hash, str_id, id_str,
                 kill_id, c_brack, error, ini_hash, clear_hash;

CONST DEBUG = FALSE;

CONST hash_sz = 255;

CONST (* symbols *)
  never = hash_sz+01;     ident = hash_sz+02;
    str = hash_sz+03;       dig = hash_sz+04;
  point = hash_sz+05;     comma = hash_sz+06;
 colomn = hash_sz+07;     semic = hash_sz+08;
o_brack = hash_sz+09;   c_brack = hash_sz+10;
  minus = hash_sz+11;       equ = hash_sz+12;

TYPE   ERROR = PROCEDURE (ARRAY OF CHAR, SEQ SYSTEM.WORD);
     UTILIZE = PROCEDURE (CHAR);
    GET_CHAR = PROCEDURE (): CHAR;

VAR lex: ARRAY [0..255] OF CHAR;
    sym: INTEGER;
    val: INTEGER;

VAR get_c: GET_CHAR;
    error: ERROR;
     util: UTILIZE;
    final: PROC;

------------------------ MISCELANIOUS --------------------------
                        --------------

CONST NUL = sci.NUL;

PROCEDURE letter?(c: CHAR): BOOLEAN;
BEGIN
  RETURN sci.KIND(c)*{sci.cyril,sci.latin}#{}
END letter?;

PROCEDURE digit?(c: CHAR): BOOLEAN; -- десятичная цифра?
BEGIN RETURN (c>='0')&(c<='9') END digit?;

PROCEDURE octal?(c: CHAR): BOOLEAN; -- восьмеричная цифра?
BEGIN RETURN (c>='0')&(c<='7') END octal?;

----------------------- INITIALISATION -------------------------
                       ----------------

PROCEDURE dummyUt(c: CHAR); BEGIN END dummyUt;

PROCEDURE getEOF(): CHAR; BEGIN RETURN sci.EOF END getEOF;

PROCEDURE stop;
BEGIN tty.Show('Неожиданный конец текста!'); HALT
END stop;

---------------------------- HASH ------------------------------
                            ------

CONST step = 17;

VAR table : ARRAY [0..hash_sz] OF STRING;

PROCEDURE ini_hash;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(table) DO table[i]:=NIL END
END ini_hash;

PROCEDURE clear_hash;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(table) DO
    IF table[i]#NIL THEN kill_str(table[i]) END
  END
END clear_hash;

PROCEDURE fhash(VAL s: ARRAY OF CHAR): INTEGER;
BEGIN RETURN ORD(s[0]) END fhash;

PROCEDURE hash(VAL s: ARRAY OF CHAR): INTEGER;
  VAR i,sav: INTEGER;
BEGIN
  i:=fhash(s); sav:=i;
  REPEAT
    IF    table[i]=NIL THEN
      table[i]:=new_str(s); RETURN i
    ELSIF table[i]^=s  THEN
      error(' Повторно использован символ "%s".',s)
    ELSE i:=(i+step) MOD (hash_sz+1)
    END
  UNTIL i=sav;
  RETURN never
END hash;

PROCEDURE str_id(VAL s: ARRAY OF CHAR): INTEGER;
  VAR i,sav: INTEGER;
BEGIN
  i:=fhash(s); sav:=i;
  REPEAT
    IF    table[i]=NIL THEN RETURN never
    ELSIF table[i]^=s  THEN RETURN i
    ELSE i:=(i+step) MOD (hash_sz+1)
    END;
  UNTIL i=sav;
  RETURN never
END str_id;

PROCEDURE id_str(id: INTEGER): STRING;
BEGIN
  IF (id>hash_sz) OR (id<0) THEN RETURN NIL
  ELSE RETURN table[id]
  END
END id_str;

PROCEDURE kill_id(id: INTEGER);
BEGIN
  IF table[id]#NIL THEN kill_str(table[id]) END
END kill_id;

--------------------------- SCANER -----------------------------
                           --------
VAR char: CHAR;

PROCEDURE get_char; BEGIN char:=get_c() END get_char;

PROCEDURE ini; BEGIN get_char END ini;

PROCEDURE skip_eol;
BEGIN
  WHILE char#sci.NL DO get_char END;
END skip_eol;

VAR pos: INTEGER;

PROCEDURE put(c: CHAR);
BEGIN
  IF pos>HIGH(lex) THEN pos:=HIGH(lex) END;
  lex[pos]:=c; INC(pos)
END put;

PROCEDURE get_ident;
BEGIN
  WHILE letter?(char) OR digit?(char) DO put(char); get_char END;
  put(NUL); sym:=ident;
END get_ident;

CONST max_int = 0FFFFFFFh;

PROCEDURE get_digit;
BEGIN val:=0;
  WHILE digit?(char) DO
    IF (max_int DIV 10) < val THEN error('переполнение целого.')
    ELSE val:=(val*10)+(ORD(char)-ORD('0'))
    END;
    put(char); get_char
  END;
  put(NUL); sym:=dig
END get_digit;

PROCEDURE get_string;
  VAR term: CHAR; val: INTEGER; done: BOOLEAN;
BEGIN
  done:=FALSE;
  REPEAT
    IF    (char='"') OR (char="'") THEN term:=char;   get_char;
      WHILE (char#term) & (char#sci.NL) DO put(char) ; get_char END;
      IF char=sci.NL THEN error('незакрытая строка;'); RETURN   END;
      get_char;
    ELSIF octal?(char) THEN val:=0;
      REPEAT
        val:=INTEGER(BITSET(val<<03)+BITSET(ORD(char)-ORD('0')));
        get_char
      UNTIL NOT octal?(char);
      IF char#'c' THEN
        error('должна быть СИМВОЛЬНАЯ константа!'); RETURN
      END;
      put(CHAR(val)); get_char
    ELSIF (char=' ') OR (char=sci.NL) THEN
      WHILE (char=' ') OR (char=sci.NL) DO get_char END
    ELSE done:=TRUE
    END
  UNTIL done
END get_string;

PROCEDURE get_lex;
  VAR done: BOOLEAN;
BEGIN pos:=0; done:=FALSE;
  REPEAT
    IF    letter?(char) THEN get_ident;                 done:=TRUE
    ELSIF  digit?(char) THEN get_digit;                 done:=TRUE
    ELSE
      CASE char OF
        |'"',"'": get_string; sym:=str;                 done:=TRUE
        |sci.EOF: final;                                done:=TRUE
        |  '%'  : REPEAT get_char UNTIL char=sci.NL;
        |  ','  : put(char); get_char; sym:=comma;      done:=TRUE
        |  ':'  : put(char); get_char; sym:=colomn;     done:=TRUE
        |  ';'  : put(char); get_char; sym:=semic;      done:=TRUE
        |  '.'  : put(char); get_char; sym:=point;      done:=TRUE
        |  '('  : put(char); get_char; sym:=o_brack;    done:=TRUE
        |  ')'  : put(char); get_char; sym:=c_brack;    done:=TRUE
        |  '-'  : put(char); get_char; sym:=minus;      done:=TRUE
        |  '='  : put(char); get_char; sym:=equ;        done:=TRUE
      ELSE util(char); get_char
      END
    END
  UNTIL done;
  put(NUL);
  IF DEBUG THEN
    tty.print('*** get_lex: sy:%d lex:"%s"\n',sym,lex);
  END;
END get_lex;

VAR i: INTEGER;

BEGIN
  get_c:=getEOF; util:=dummyUt; final:=stop; error:=tty.print;
  FOR i:=0 TO hash_sz DO table[i]:=NIL END;
END scan;

PROCEDURE error(VAL fmt: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
  VAR pos: INTEGER;
BEGIN pos:=cc;
  tty.print(' Ошибка в строке %d, позиция %d.\n',lcc,cc);
  scan.skip_eol; --put(NUL);
  tty.WriteString(bump);
  tty.print('\n%*s\n', pos+1, ' ^ ');
  tty.print(fmt,args);
  HALT(1); -- ???
END error;

------------------------- PARAMETERS ---------------------------
                         ------------

TYPE PARM = POINTER TO PARMBODY;
     PARMBODY = RECORD
                  next: PARM;
                   val: STRING;
                END;

     CALL = POINTER TO CALLBODY;
     CALLBODY = RECORD
                  name: INTEGER;
                  parm: PARM;
                  next: CALL;
                END;

VAR calls: CALL;
    parms: PARM;

PROCEDURE new_parm(VAL s: ARRAY OF CHAR);
  VAR p,t: PARM;
BEGIN
  NEW(p);
  p^.val:=new_str(s);
  p^.next:=NIL;
  IF parms=NIL THEN parms:=p
  ELSE t:=parms;
    WHILE t^.next#NIL DO t:=t^.next END;
    t^.next:=p
  END;
END new_parm;

PROCEDURE new_call(id: INTEGER);
  VAR c,t: CALL;
BEGIN
  NEW(c);
  c^.name:=id;
  c^.parm:=parms; parms:=NIL;
  c^.next:=NIL;
  IF calls=NIL THEN calls:=c
  ELSE t:=calls;
    WHILE t^.next#NIL DO t:=t^.next END;
    t^.next:=c;
  END;
END new_call;

PROCEDURE kill_call(c: CALL); -- параметры должны быть уничтожены
                              --  внешним способом!!!
  VAR t,k: CALL;
BEGIN
  ASSERT(calls#NIL); k:=NIL;
  IF calls=c THEN k:=calls; calls:=calls^.next
  ELSE t:=calls;
    WHILE (t^.next#NIL) & (t^.next#c) DO t:=t^.next END;
    ASSERT(t^.next#NIL);
    k:=t^.next; t^.next:=t^.next^.next;
  END;
  ASSERT(k#NIL);
  mem.DEALLOCATE(k,SIZE(CALLBODY));
END kill_call;

--------------------------- PARSER -----------------------------
                           --------

(*         12345678|12345678|12345678|12345678|12345678|12345678|12345678|12345678|12345678|12345678|12345678|12345678| *)
CONST Eng="January  February March    April    May      June     July     August   SeptemberOctober  November December   ";
      Rus="Январь   Февраль  Март     Апрель   Май      Июнь     Июль     Август   Сентябрь Октябрь  Ноябрь   Декабрь    ";

PROCEDURE Month?(VAL s: ARRAY OF CHAR): INTEGER;
  VAR i,j: INTEGER; c0,c1,c2: CHAR;

PROCEDURE BOLD(c: CHAR): CHAR;
  VAR mc: BITSET;
BEGIN mc:=BITSET(c);
  EXCL(mc,5); RETURN CHAR(mc)
END BOLD;

BEGIN
  c0:=BOLD(s[0]); c1:=BOLD(s[1]); c2:=BOLD(s[2]);
  FOR i:=0 TO 11 DO j:=i*9;
    IF (c0=BOLD(Eng[j])) & (c1=BOLD(Eng[j+1])) & (c2=BOLD(Eng[j+2])) THEN
      RETURN i+1
    END;
    IF (c0=BOLD(Rus[j])) & (c1=BOLD(Rus[j+1])) & (c2=BOLD(Rus[j+2])) THEN
      RETURN i+1
    END;
  END; RETURN -1;
END Month?;

VAR end, text, case, unk, const: INTEGER;

VAR day, mon, year, week: INTEGER;

PROCEDURE get_date(): BOOLEAN;
  VAR d,y,m: INTEGER;
BEGIN
  IF scan.sym=scan.dig THEN d:=scan.val
  ELSIF (scan.sym=scan.ident) & (scan.str_id(scan.lex)=unk) THEN d:=-1
  ELSE error(' Ожидалось десятичное число ')
  END;
  scan.get_lex;
  IF scan.sym#scan.minus THEN unexp(' - ') END;
  scan.get_lex;
  IF scan.sym=scan.ident THEN
    IF scan.str_id(scan.lex)=unk THEN m:=-1
    ELSE
      m:=Month?(scan.lex);
      IF m<0 THEN error(' Неправильное название месяца.') END;
    END;
  ELSE error(' Ожидался идентификатор.')
  END;
  scan.get_lex;
  IF scan.sym#scan.minus THEN unexp(' - ') END;
  scan.get_lex;
  IF scan.sym=scan.dig THEN y:=scan.val;
    IF y<100 THEN y:=y+1900 END;
  ELSIF (scan.sym=scan.ident) & (scan.str_id(scan.lex)=unk) THEN y:=-1
  ELSE error(' Ожидалось десятичное число ')
  END;
  scan.get_lex;
  RETURN ((day=d) OR (d<0)) & ((mon=m) OR (m<0)) & ((year=y) OR (y<0));
END get_date;

VAR vals: ARRAY [0..scan.hash_sz] OF STRING;
    macs: ARRAY [0..scan.hash_sz] OF INTEGER;

PROCEDURE one_parm;
  VAR done: BOOLEAN; id: INTEGER;
BEGIN
  REPEAT done:=TRUE;
    IF scan.sym=scan.str THEN new_parm(scan.lex); scan.get_lex
    ELSIF scan.sym=scan.ident THEN
      id:=scan.str_id(scan.lex);
      IF id>=scan.never THEN
        error(' невидимый объект %s',scan.lex);
      ELSIF vals[id]#NIL THEN new_parm(vals[id]^); scan.get_lex
      ELSE error(' не могу объяснить, но ошибка');
      END;
    ELSE error(' Ожидалась строчная константа')
    END;
    IF    scan.sym=scan.comma   THEN scan.get_lex; done:=FALSE;
    ELSIF scan.sym#scan.c_brack THEN unexp(' ) ')
    END;
  UNTIL done;
END one_parm;

PROCEDURE get_parms;
BEGIN
  scan.get_lex;
  IF scan.sym=scan.c_brack THEN scan.get_lex; RETURN END;
  REPEAT one_parm UNTIL scan.sym=scan.c_brack;
  scan.get_lex;
END get_parms;

PROCEDURE get_call;
  VAR name: INTEGER;
BEGIN
  IF scan.sym#scan.ident THEN error(' Ожидался идентификатор') END;
  name:=scan.str_id(scan.lex);
  IF name=scan.never THEN name:=scan.hash(scan.lex) END;
  scan.get_lex;
  IF scan.sym=scan.o_brack THEN get_parms END;
  new_call(name);
END get_call;

PROCEDURE variant;
  VAR ok,t: BOOLEAN;
BEGIN
  ok:=get_date();
  WHILE scan.sym#scan.colomn DO
    IF    scan.sym=scan.comma  THEN scan.get_lex
    ELSE  t:=get_date(); ok:=ok OR t;
    END;
  END;
  IF ok THEN
    scan.get_lex;
    WHILE scan.sym#scan.semic DO
      get_call;
      IF    scan.sym=scan.comma THEN scan.get_lex
      ELSIF scan.sym#scan.semic THEN unexp(' ; ')
      END;
    END;
    scan.get_lex;
  ELSE
    REPEAT scan.get_lex UNTIL scan.sym=scan.semic;
    scan.get_lex
  END;
END variant;

PROCEDURE choose;
  VAR i,j: INTEGER;
BEGIN scan.get_lex;
  WHILE (scan.sym#scan.ident) OR (scan.str_id(scan.lex)#end) DO variant END;
  scan.get_lex;
  IF scan.sym#scan.semic THEN unexp(' ; ') END;
END choose;

TYPE LOCAL = POINTER TO LOCBODY;
     LOCBODY = RECORD
                 ident: INTEGER;
                  next: LOCAL;
               END;

VAR locals: LOCAL;

PROCEDURE new_local(id: INTEGER);
  VAR l: LOCAL;
BEGIN mem.ALLOCATE(l, SIZE(LOCBODY));
  IF l=NIL THEN error(' Нет памяти')
  ELSE l^.ident:=id; l^.next:=locals; locals:=l;
  END;
END new_local;

PROCEDURE kill_locals;
  VAR t: LOCAL;
BEGIN
  WHILE locals#NIL DO t:=locals; locals:=locals^.next;
    scan.kill_id(t^.ident);
    IF vals[t^.ident]#NIL THEN kill_str(vals[t^.ident]) END;
    mem.DEALLOCATE(t, SIZE(LOCBODY));
  END;
END kill_locals;

PROCEDURE eval_parms(c: CALL);

  VAR id: INTEGER; p: PARM;

  CONST illg_parm_no = ' Неправильное число параметров';

BEGIN scan.get_lex;
  WHILE scan.sym#scan.c_brack DO
    IF scan.sym#scan.ident THEN error(' Ожидался идентификатор')
    ELSE
      id:=scan.str_id(scan.lex);
      IF id=scan.never THEN id:=scan.hash(scan.lex) END;
      IF vals[id]#NIL THEN
          error(' Ну, я просто не знаю, как это называется !!')
      ELSIF c^.parm=NIL THEN error(illg_parm_no);
      ELSE
        p:=c^.parm;
        vals[id]:=p^.val;
        c^.parm:=p^.next;
        new_local(id);
        mem.DEALLOCATE(p, SIZE(PARMBODY));
      END;
      scan.get_lex;
      IF scan.sym=scan.comma THEN scan.get_lex END;
    END;
  END;
  IF c^.parm#NIL THEN error(illg_parm_no) END;
  scan.get_lex; -- счеркивание закрывающей скобки;
  IF scan.sym#scan.semic THEN unexp(' ; ') END;
END eval_parms;

VAR decor_cal: BOOLEAN;

PROCEDURE decorate(VAL mark: ARRAY OF CHAR);
  VAR b,a: INTEGER;
     hbar: CHAR;
BEGIN
  decor_cal:=TRUE;
  b:=tty.state^.columns;
  IF b>64 THEN b:=64 END;
  hbar:=tty.state^.hbar;
  a:=b DIV 10;
  b:=b-a-str.len(mark);
  tty.print('%.*c%s%.*c',b,hbar,mark,a,hbar);
END decorate;

PROCEDURE show_macro;

  PROCEDURE decor;
    VAR date: ARRAY [0..79] OF CHAR;
  BEGIN
    str.print(date,"mention for %s, %s %d, %d",
              editor.days[week],
              editor.monthes[mon], day, year);
    decorate(date)
  END decor;

  VAR dut: scan.UTILIZE; id: INTEGER;
BEGIN
  decor;
  dut:=scan.util;
  scan.util:=tty.Write;
  scan.get_lex;
  WHILE (scan.sym#scan.ident) OR (scan.str_id(scan.lex)#end) DO
    IF    scan.sym=scan.str   THEN tty.WriteString(scan.lex)
    ELSIF scan.sym=scan.ident THEN
      id:=scan.str_id(scan.lex);
      IF (id<scan.never) & (vals[id]#NIL) THEN
           tty.WriteString(vals[id]^)
      ELSE tty.WriteString(scan.lex)
      END
    ELSE tty.WriteString(scan.lex)
    END;
    scan.get_lex
  END;
  scan.util:=dut;
  scan.get_lex    -- счеркивание символа "END";
END show_macro;

PROCEDURE get_const;
  VAR id: INTEGER;
BEGIN
  scan.get_lex;
  IF scan.sym#scan.ident THEN error(' Ожидался идентификатор');
  ELSE id:=scan.hash(scan.lex); ASSERT(vals[id]=NIL);
    scan.get_lex;
    IF scan.sym#scan.equ THEN unexp(' = ')
    ELSE scan.get_lex;
      IF scan.sym#scan.str THEN error(' Ожидалась строчная константа')
      ELSE vals[id]:=new_str(scan.lex);
      END;
    END;
  END;
  scan.get_lex;
  IF scan.sym#scan.semic THEN unexp (' ; ') END;
  scan.get_lex;
END get_const;

PROCEDURE one_macro;
  VAR pp,id: INTEGER;
      cc, c: CALL;
       made: BOOLEAN;
BEGIN pp:=pos;
--  IF pp>0 THEN pp:=pp-1 END;
  scan.get_lex;
  IF (scan.sym#scan.ident) THEN error(' Ожидался идентификатор.')
  ELSE id:=scan.str_id(scan.lex);
    cc:=calls; made:=FALSE;
    WHILE cc#NIL DO
      IF cc^.name=id THEN c:=cc; cc:=cc^.next;
        IF made THEN set_pos(pp); scan.ini; scan.get_lex END;
        scan.get_lex;
        IF scan.sym=scan.o_brack THEN eval_parms(c)
        ELSIF scan.sym#scan.semic THEN unexp(' ; ')
        END;
        show_macro;
        kill_call(c); kill_locals;
        made:=TRUE;
      ELSE cc:=cc^.next;
      END;
    END;
    IF NOT made THEN
      REPEAT scan.get_lex
      UNTIL (scan.sym=scan.ident) & (scan.str_id(scan.lex)=end);
      scan.get_lex; -- счеркивание символа "END";
    END;
  END;
END one_macro;

PROCEDURE one_block;
BEGIN
  IF    (scan.sym=scan.ident) & (scan.str_id(scan.lex)=text)  THEN one_macro;
  ELSIF (scan.sym=scan.ident) & (scan.str_id(scan.lex)=const) THEN get_const;
  ELSIF (scan.sym=scan.ident) & (scan.str_id(scan.lex)=case)  THEN choose;
  ELSE unexp('TEXT OR CONST OR CHOOSE');
  END;
END one_block;

PROCEDURE main_loop;
BEGIN
--  IF calls=NIL THEN RETURN END;
  scan.get_lex;
  WHILE scan.sym#scan.point DO one_block;
    IF scan.sym=scan.semic THEN scan.get_lex END;
  END;
END main_loop;

------------------------- DEBUG ETC. ---------------------------
                         ------------

PROCEDURE show_calls;

  PROCEDURE show(c: CALL);
    VAR t: PARM; s: STRING;
  BEGIN s:=scan.id_str(c^.name);
    tty.print('%s(',s^);
    t:=c^.parm;
    WHILE t#NIL DO s:=t^.val;
      tty.print('"%s"',s^); t:=t^.next;
      IF t#NIL THEN tty.print(',') END;
    END;
    tty.print(')\n');
  END show;

  VAR t: CALL;

BEGIN
  IF calls=NIL THEN
  ELSE
    tty.print(' ОТСУТСТВУЮТ ОПИСАНИЯ ТЕКСТОВ:\n');
    t:=calls;
    WHILE t#NIL DO show(t); t:=t^.next END;
  END;
END show_calls;

PROCEDURE jubilee?(d,m,y,w: INTEGER);
  VAR i: INTEGER;
BEGIN
  parms:=NIL;  calls:=NIL;  cc:=0;  lcc:=0;
  scan.ini_hash;
  FOR i:=0 TO scan.hash_sz DO vals[i]:=NIL; macs[i]:=-1 END;
  end :=scan.hash('END');        text :=scan.hash('TEXT');
  case:=scan.hash('CHOOSE');     const:=scan.hash('CONST');
  unk :=scan.hash('XX');
  scan.get_c:=get_char;  scan.error:=error;  scan.ini;
  day:=d; mon:=m; year:=y; week:=w;
  main_loop; show_calls;
  DISPOSE(buff); scan.clear_hash
END jubilee?;

PROCEDURE jubilee(d,m,y,w: INTEGER);
  VAR i: INTEGER;
BEGIN
  decor_cal:=FALSE;
  IF open(default) THEN jubilee?(d,m,y,w) END;
  FOR i:=0 TO HIGH(arg.words) DO
    IF open(arg.words[i]) THEN jubilee?(d,m,y,w)
    ELSE
      tty.print('open("%s"): no such entry\n',arg.words[i])
    END
  END;
  IF decor_cal THEN
    decorate(" DON'T FORGET! ");
    tty.print('\n')
  END
END jubilee;


PROCEDURE usage;
BEGIN
  tty.print(
    '  "remind" of events of date utility (c)1991 KRONOS\n'
    'usage:\n'
    '   remind [-dh] { file_name }\n'
    '                           Hadrian, Jan 29, 91\n'
           );
END usage;

VAR stime,y,m,d,h,mn,sc0: INTEGER;

BEGIN
  stime:=Time.time();
  IF arg.flag("-","h") THEN usage; HALT END;
  IF arg.flag("-","d") THEN
    WHILE edit_time(stime) DO
      tty.print('\r'); tty.erase_line(0);
      Time.unpack(stime,y,m,d,h,mn,sc0);
      h:=Time.day(stime);
      jubilee(d,m,y,h)
    END;
    tty.print('\r'); tty.erase_line(0)
  ELSE
    Time.unpack(stime,y,m,d,h,mn,sc0);
    h:=Time.day(stime);
    jubilee(d,m,y,h)
  END;
END remind.
