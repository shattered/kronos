IMPLEMENTATION MODULE mxScan; (* Leo  xx-Xxx-86. (c) KRONOS *)
                              (* Ned  18-Sep-86. (c) KRONOS *)
                           (* mx:Ned  03-Mar-87. (c) KRONOS *)

IMPORT    sys: SYSTEM;
IMPORT   comp: coolDefs;
IMPORT  inter: coolSystem;

WITH STORAGE: inter;

---------------------------------------------------------------

PROCEDURE MOVE(t,f: sys.ADDRESS; sz: INTEGER); CODE 0C0h END MOVE;

--------------------------  OPTIONS  ---------------------------
                          -----------

VAR   base: BITSET;
  optSTACK: inter.QUEUE;

PROCEDURE ini_opts;
BEGIN
  inter.clear(optSTACK); inter.lifo(optSTACK); opts:=base;
END ini_opts;

---------------------------  SCANER  ---------------------------
                           ----------

CONST EOF = 36c; EOL = 0c;

CONST
  MaxReal=REAL(7FFFFFFFh);  (* TYPE REAL=[-MaxReal..MaxReal] *)
  Delta  =REAL(00800000h);  (* Delta / 2.0 = 0.0             *)
                            (* 0.0 = REAL(00000000h)         *)

VAR
  text   : comp.io_ptr;
  next   : CHAR;
  char   : CHAR;
  source : STRING;
  occur  : BOOLEAN; (* TRUE if errors occur in current line *)
  comment: INTEGER; (* first line of comment *)
  col0   : INTEGER;
  line0  : INTEGER;

CONST alpha? = ARRAY OF CHAR {          -- letter OR digit
-------  00  01  02  03  04  05  06  07  08  09  0A  0B  0C  0D  0E  0F
(*00h*)  0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c,
(*10h*)  0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c,
(*20h*)  0c, 1c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c,
(*30h*)  1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 0c, 0c, 0c, 0c, 0c, 1c,
(*40h*)  0c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c,
(*50h*)  1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 0c, 0c, 0c, 0c, 1c,
(*60h*)  1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c,
(*70h*)  1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 0c, 0c, 0c, 0c, 0c,
(*80h*)  0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c,
(*90h*)  0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c,
(*A0h*)  0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c,
(*B0h*)  0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c, 0c,
(*C0h*)  1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c,
(*D0h*)  1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c,
(*E0h*)  1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c,
(*F0h*)  1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 1c, 0c};

PROCEDURE NewLine;
BEGIN
  occur:=FALSE;
  REPEAT
    text^.doio(text);
    IF text^.done THEN
(*$<U+*)
      source^:=text^.buf^
(*$>*)
    ELSE
      IF LEN(source)<2 THEN RESIZE(source,2) END;
      source[0]:=' ';
      source[1]:=0c;
      INC(line0); col0:=0;
      next:=EOF;
      RETURN
    END;
    INC(line0); col0:=0;
    WHILE source[col0]=' ' DO INC(col0) END;
    next:=source[col0]; INC(col0);
  UNTIL next#EOL;
END NewLine;

PROCEDURE getchar0;
BEGIN
  char:=next; next:=source[col0]; INC(col0);
END getchar0;

PROCEDURE getchar;
BEGIN
  char:=next; next:=source[col0]; INC(col0);
  IF char=EOL THEN NewLine END;
END getchar;

PROCEDURE Fraction(v: INTEGER);
  VAR r,f: REAL;
     e,sg: INTEGER;
BEGIN ASSERT(char='.'); getchar0;
  sy:=realval; cVal:=INTEGER(1.); r:=FLOAT(v);
  f:=1.;
  WHILE ORD(char)-ORD('0') IN {0..9} DO f:=f/10.;
    IF f>=Delta THEN
      r:=r + FLOAT(ORD(char)-ORD('0'))*f;
    ELSE err(54)
    END; getchar0
  END;
  IF CAP(char)='E' THEN getchar0; sg:=+1;
    IF char='-' THEN sg:=-1; getchar0 ELSIF char='+' THEN getchar0 END;
    e:=0;
    WHILE ORD(char)-ORD('0') IN {0..9} DO
      IF e<255 THEN e:=e*10+ORD(char)-ORD('0') END;
      getchar0;
    END;
    IF e>40 THEN err(54); RETURN END;
    WHILE e>0 DO
      IF sg>0 THEN
        IF r>MaxReal/10. THEN err(54); RETURN END;
        r:=r*10.
      ELSE
        IF r<Delta THEN err(54); RETURN END;
        r:=r/10.
      END;  DEC(e)
    END;
  END;
  cVal:=INTEGER(r);
END Fraction;

PROCEDURE Number;
  VAR dv,hv,ov   : INTEGER;  ord10,ord16: INTEGER;
      hex,oct,dec: BOOLEAN; c: CHAR;
BEGIN
  dv:=0; hv:=dv; ov:=hv; hex:=TRUE; oct:=hex; dec:=oct;
  sy:=intval;
  LOOP  c:=char;
    ord10:=ORD(c)-ORD('0'); c:=CAP(c);
    IF    ord10 IN {0..7} THEN ord16:=ord10
    ELSIF ord10 IN {8..9} THEN ord16:=ord10; oct:=FALSE;
    ELSE ord16:=ORD(c)-(ORD('A')-10);
      IF ord16 IN {10..15} THEN oct:=FALSE; dec:=FALSE END
    END;
    IF NOT (ord16 IN {0..15}) THEN (* c=CAP(char) *) EXIT END;
    getchar0; (*! ! ! ! ! !*)
    IF oct THEN ov:=ov<<3;
      IF BITSET(ov)*{0..2} # {} THEN oct:=FALSE
      ELSE INC(ov,ord10)
      END; c:=CAP(char);
      IF oct & ((c='B') OR (c='C')) THEN c:=CAP(next);
        IF (ORD(next)-ORD('0') IN {0..9})
        OR (ORD( c )-ORD('A') IN {0..5}) OR (c='H')
        THEN oct:=FALSE
        ELSE c:=CAP(char); EXIT  (* octal terminated *)
        END;
      END;
    END;
    IF dec THEN
      IF dv>(MAX(INTEGER)-ord10) DIV 10  THEN dec:=FALSE
      ELSE dv:=dv*10+ord10
      END
    END;
    IF hex THEN hv:=hv<<4;
      IF BITSET(hv)*{0..3} # {} THEN hex:=FALSE
      ELSE INC(hv,ord16)
      END
    END;
  END; (* LOOP *)
  cVal:=1;
  IF   (char='.') & dec & (next#'.') THEN Fraction(dv)
  ELSIF c='H' THEN getchar0;
    IF hex THEN cVal:=hv ELSE err(54) END;
  ELSIF (c='B') OR (c='C') THEN getchar0;
    IF oct THEN cVal:=ov ELSE err(54) END;
    IF c='C' THEN sy:=charval END;
  ELSE
    IF dec THEN cVal:=dv
    ELSIF hex THEN err(11); cVal:=hv
    ELSE err(54)
    END
  END;
END Number;

PROCEDURE Prags;
  VAR cap: CHAR; n: INTEGER;
BEGIN
  ASSERT(char='$'); getchar;
  LOOP cap:=CAP(char);
    IF    char="<" THEN getchar;  inter.push(optSTACK,opts)
    ELSIF char=">" THEN getchar;
      IF NOT inter.pop(optSTACK,opts) THEN opts:=base END;
    ELSIF char="!" THEN getchar;  ini_opts
    ELSIF char="+" THEN getchar; n:=0;
      WHILE (char>='0') & (char<='9') DO
        n:=n*10+ORD(char)-ORD('0'); getchar
      END;
      INC(add_stk,n);
    ELSIF (cap>='A') & (cap<='Z') THEN getchar;
      IF    char='+' THEN getchar; INCL(opts,ORD(cap)-ORD('A'));
      ELSIF char='-' THEN getchar; EXCL(opts,ORD(cap)-ORD('A'));
      ELSE RETURN
      END;
    ELSE RETURN
    END;
  END;
END Prags;

PROCEDURE Comment;
  VAR c0: INTEGER;
BEGIN
  c0:=comment; comment:=line;
  ASSERT(char='*'); getchar;
  LOOP
    IF char='*'    THEN getchar;
      IF char=')'  THEN comment:=c0; RETURN END;
    ELSIF char='(' THEN getchar;
      IF char='*'  THEN Comment END;
    ELSIF char='$' THEN Prags
    ELSIF char=EOF THEN Fault(66,'%d',comment); RETURN
    ELSIF fault   THEN RETURN
    ELSE getchar
    END
  END;
END Comment;

PROCEDURE QStr;
  VAR i: INTEGER; q: CHAR;
BEGIN i:=0;
  LOOP
    q:=char; getchar0;
    WHILE (char#EOL) & (char#q) DO
      IF i<HIGH(sVal) THEN sVal[i]:=char; INC(i) END;
      getchar0;
    END;
    IF (char=EOL) OR (i+4>=HIGH(sVal)) THEN err(12) END;
    IF char=EOL THEN NewLine END;
    getchar;
    WHILE (char=' ') OR (char=EOL)  DO getchar END;
    WHILE (char>='0') & (char<='9') DO GetSy;
      IF sy#charval THEN err(67) END;
      IF i<HIGH(sVal) THEN sVal[i]:=CHAR(cVal); INC(i) END;
      IF char=EOL THEN NewLine END;
      WHILE (char=' ') OR (char=EOL) DO getchar END;
    END;
    IF (char#'"') & (char#"'") THEN EXIT END;
  END;
  sVal[i]:=0c;
  IF (char=EOL) OR (i+4>=HIGH(sVal)) THEN err(12) END;
  IF i=1 THEN
       sy:=charval; cVal:=ORD(sVal[0]);
  ELSE sy:=string; sLen:=i+1;
    REPEAT sVal[i]:=0c; INC(i) UNTIL (i MOD 4)=0;
  END
END QStr;

------------------------  IDENTIFIERS  -------------------------
                        ---------------

TYPE
  pString = POINTER TO String;
  POOL    = POINTER TO ARRAY [0..1023] OF INTEGER;

VAR
  id_pool : POOL;
  id_queue: inter.QUEUE;
  free    : INTEGER; (* позиция в строковом буфере *)
  ref     : DYNARR OF pString;
  KWids   : ARRAY [0..lastsymbol] OF pString;
  ids_busy: INTEGER; (*[0..noIdents]*)
  ids_lim : INTEGER; (* (noIdents+3) DIV 4 *)

PROCEDURE id_str(d: INTEGER; VAR s: ARRAY OF CHAR);
  VAR i: INTEGER; p: pString;
BEGIN
  p:=ref[d];
  IF p=NIL THEN s:='$******$'; s[1]:=0c; RETURN END;
  IF INTEGER(p)<0 THEN p:=KWids[ABS(INTEGER(p))] END;
  i:=0;
  WHILE (i<HIGH(s)) & (p^[i]#0c) DO s[i]:=p^[i]; INC(i) END;
  s[i]:=0c;
END id_str;

PROCEDURE Hash(VAL str: ARRAY OF CHAR; len,sum: INTEGER): INTEGER;
  VAR p: pString; r,x: INTEGER;
BEGIN
  sum:=sum MOD noIdents;
  r:=1;
  LOOP
    p:=ref[sum];
    IF p=NIL THEN
      len:=(len+3) DIV 4;
      IF free+len>HIGH(id_pool^) THEN
        inter.push(id_queue,id_pool);
        NEW(id_pool); free:=0;
      END;
      p:=sys.ADR(id_pool^[free]); ref[sum]:=p;
      INC(free,len);
      MOVE(p,sys.ADR(str),len);
      INC(ids_busy);
      IF ids_busy>ids_lim THEN Fault(2,'') END;
      RETURN sum
    END;
    IF INTEGER(p)<0 THEN
      x:=ABS(INTEGER(p)); p:=KWids[x];
      IF  p^=str THEN sy:=x; RETURN sum  END;
    ELSIF p^=str THEN        RETURN sum
    END;
    x:=r+r; r:=(x+x+r) MOD (noIdents*4);
    sum:=(sum+(r DIV 4)) MOD noIdents;
  END;
END Hash;

PROCEDURE str_id(VAL str: ARRAY OF CHAR): INTEGER;
  VAR sum: BITSET; len: INTEGER; char: CHAR;
BEGIN
  len:=0; sum:={}; char:=str[0];
  WHILE char#0c DO
    sum:=BITSET(sum<<3) / BITSET(ORD(char)*16+len);
    INC(len); char:=str[len];
  END;
  sum:=BITSET(sum)/BITSET(sum>>8)/BITSET(sum<<8);
  RETURN Hash(str,len+1,INTEGER(sum));
END str_id;

PROCEDURE MakeKW(s: INTEGER; VAL str: ARRAY OF CHAR);
  VAR d: INTEGER;
BEGIN d:=str_id(str);
  KWids[s]:=ref[d]; ref[d]:=pString(-s);
END MakeKW;

CONST sps = ARRAY OF CHAR{
            '=',' ',  '#',' ',  '0','2',
            '<',' ',  '>',' ',  '<','=',   '>','=',
            '*',' ',  '0','8',  '0','9',   '/',' ',
            '-',' ',  '+',' ',  '<','<',   '>','>',
            '1','5',  '1','6',  '1','7',  '1','8',   '1','9',
            '2','0',  '2','1',  '2','2',   '2','3',   '2','4',
            ';',' ',
            '2','6',  '2','7',  '2','8',   '2','9',   '3','0',   '3','1',
            '3','2',  '3','3',  '3','4',   '3','5',   '3','6',
            '^',' ',  '.',' ',  ':',' ',   '[',' ',
            ']',' ',  '(',' ',  ')',' ',   '{',' ',
            '}',' ',  ',',' ',  '.','.',   ':','='};

PROCEDURE vis_sym(s: INTEGER; VAR v: ARRAY OF CHAR);
  VAR i: INTEGER; p: pString;
BEGIN
  IF KWids[s]#NIL THEN p:=KWids[s]; i:=0;
    WHILE (i<HIGH(v)) & (p^[i]#0c) DO v[i]:=p^[i]; INC(i) END;
    v[i]:=0c;
  ELSIF s=ident THEN v:="$IDENT$"
  ELSIF (s=range) OR (s=becomes) OR (s IN {leq,geq,rol,ror}) THEN
      i:=s*2; v[0]:=sps[i]; v[1]:=sps[i+1]; v[2]:=0c;
  ELSIF s<range THEN i:=s*2; v[0]:=sps[i]; v[1]:=0c;
  ELSE inter.sprint(v,'unknown symbol #%d',s);
  END
END vis_sym;

-------------------------  G E T S Y  --------------------------
                         -------------

VAR Str: String;
    sum: BITSET;
  len,i: INTEGER;

PROCEDURE GetSy;
BEGIN
  IF fault THEN sy:=end; RETURN END;
  col:=col0-2; line:=line0;
  LOOP
    CASE char OF
    |'0','1','2','3','4','5','6','7','8','9': Number; RETURN
    |'"',"'": QStr; RETURN
    |'+': sy:=plus  ; getchar0; RETURN
    |'*': sy:=times ; getchar0; RETURN
    |'/': sy:=slash ; getchar0; RETURN
    |'[': sy:=lbr   ; getchar0; RETURN
    |']': sy:=rbr   ; getchar0; RETURN
    |'{': sy:=lbrace; getchar0; RETURN
    |'}': sy:=rbrace; getchar0; RETURN
    |'^': sy:=bar   ; getchar0; RETURN
    |'&': sy:=and   ; getchar0; RETURN
    |'~': sy:=not   ; getchar0; RETURN
    |'|': sy:=sep   ; getchar0; RETURN
    |';': sy:=semic ; getchar0; RETURN
    |',': sy:=coma  ; getchar0; RETURN
    |')': sy:=rpar  ; getchar0; RETURN
    |'=': sy:=equ   ; getchar0; RETURN
    |'#': sy:=neq   ; getchar0; RETURN
    |'-': IF next='-' THEN REPEAT getchar0 UNTIL next=EOL;
          ELSE sy:=minus; getchar0; RETURN
          END
    |':': IF next='=' THEN getchar0; sy:=becomes ELSE sy:=colon END;
          getchar0; RETURN
    |'.': IF next='.' THEN getchar0; sy:=range   ELSE sy:=period END;
          getchar0; RETURN
    |'>': IF next='=' THEN getchar0; sy:=geq
       ELSIF next='>' THEN getchar0; sy:=ror     ELSE sy:=gtr  END;
          getchar0; RETURN
    |'<': IF next='=' THEN getchar0; sy:=leq
       ELSIF next='>' THEN getchar0; sy:=neq
       ELSIF next='<' THEN getchar0; sy:=rol     ELSE sy:=lss  END;
          getchar0; RETURN
    |'(': IF next='*' THEN getchar0; Comment;
          ELSE sy:=lpar; getchar0; RETURN
          END
    |'a'..'z','A'..'Z','ю'..'щ','Ю'..'Щ','_','?','!': sy:=ident;
       len:=0; sum:={};
       REPEAT
         sum:=BITSET(sum<<3) / BITSET(ORD(char)*16+len);
         Str[len]:=char; INC(len);
         char:=next; next:=source[col0]; INC(col0);
       UNTIL alpha?[ORD(char)]=0c;
       Str[len]:=0c;
       sum:=BITSET(sum)/BITSET(sum>>8)/BITSET(sum<<8);
       Id:=Hash(Str,len+1,INTEGER(sum));
       RETURN
    |EOF: Fault(4,''); RETURN
    |EOL: NewLine;
    |' ':
    ELSE err(13)
    END;
    getchar0;
  END;
END GetSy;

------------------------  INIT & EXIT  -------------------------
                        ---------------

PROCEDURE MakeKW2(s0: INTEGER; VAL str0: ARRAY OF CHAR;
                  s1: INTEGER; VAL str1: ARRAY OF CHAR);
BEGIN MakeKW(s0,str0); MakeKW(s1,str1)
END MakeKW2;

PROCEDURE IniKWs;
BEGIN
  MakeKW2(by,"BY",                    if,"IF")
 ;MakeKW2(do,"DO",                    of,"OF")
 ;MakeKW2(in,"IN",                    to,"TO")
 ;MakeKW2(or,"OR",                    code,"CODE")
 ;MakeKW2(for,"FOR",                  div,"DIV")
 ;MakeKW2(end,"END",                  mod,"MOD")
 ;MakeKW2(not,"NOT",                  set,"SET")
 ;MakeKW2(var,"VAR",                  case,"CASE")
 ;MakeKW2(else,"ELSE",                exit,"EXIT")
 ;MakeKW2(from,"FROM",                loop,"LOOP")
 ;MakeKW2(then,"THEN",                type,"TYPE")
 ;MakeKW2(with,"WITH",                array,"ARRAY")
 ;MakeKW2(begin,"BEGIN",              const,"CONST")
 ;MakeKW2(elsif,"ELSIF",              until,"UNTIL")
 ;MakeKW2(while,"WHILE",              export,"EXPORT")
 ;MakeKW2(import,"IMPORT",            module,"MODULE")
 ;MakeKW2(record,"RECORD",            repeat,"REPEAT")
 ;MakeKW2(pointer,"POINTER",          return,"RETURN")
 ;MakeKW2(procedure,"PROCEDURE",      forward,"FORWARD")
 ;MakeKW2(qualified,"QUALIFIED",      definition,"DEFINITION")
 ;MakeKW2(and,"AND",                  seq,"SEQ")
 ;MakeKW2(val,"VAL",                  implementation,"IMPLEMENTATION");
 MakeKW(dynarr,"DYNARR");
 MakeKW(rem,"REM");
END IniKWs;

PROCEDURE IniIds;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(ref)   DO ref[i]:=NIL   END;
  FOR i:=0 TO HIGH(KWids) DO KWids[i]:=NIL END;
  free:=0; Id:=DmId; ids_busy:=0; ids_lim:=(noIdents*6) DIV 7;
  i:=str_id('*1'); i:=str_id('*2');
  ref[DmId]:=sys.ADR(id_pool^[1]);
  IniKWs;
END IniIds;

PROCEDURE ini_scaner;
BEGIN
  occur:=FALSE; noErrors:=0; fault:=FALSE;
  IniIds;
  NEW(source);
  line0:=-1; col0:=0;
  line := 0; col :=0;
  next:=EOL; NewLine;
  getchar0; GetSy;
END ini_scaner;

PROCEDURE exi_scaner;
END exi_scaner;

---------------------------  ERRORS  ---------------------------
                           ----------

CONST fstr = '%s';

PROCEDURE message(no: INTEGER; VAL f: ARRAY OF CHAR; SEQ args: sys.WORD);

  PROCEDURE show(VAL msg: ARRAY OF CHAR);
    VAR s: ARRAY [0..255] OF CHAR; c: INTEGER;
  BEGIN
    inter.sprint(s,f,args);
    IF col0>=2 THEN c:=col0-2 ELSE c:=0 END;
    inter.error(line0,c,source,'%s %s',msg,s);
    INC(noErrors);
    IF ORD('F')-ORD('A') IN opts THEN inter.print("ОТЛАДКА."); HALT(1) END;
    occur:=TRUE;
  END show;

BEGIN
  CASE no OF
  |00: show("")
--|01:
  |02: show("Переполнена хеш таблица (слишком много имен)")
  |03: show("Переполнение стека выражений!")
  |04: show("Неожиданный конец исходного текста!")
--|05:
  |06: show("Ошибка в заголовке модуля");
  |07: show("Ожидался идентификатор");
  |08: show("Должно быть имя блока")
  |09: show("Неправильный (неконстантный) или вырожденный отрезок")
  |10: show("Размах меток оператора выбора > 256 ")
  |11: show("Отсутствует 'h' после шестнадцатеричного")
  |12: show("Незакрытая или слишком длинная строка!")
  |13: show("Непонятный знак игнорируется")
  |14: show("Невидимый объект")
  |15: show("Повторно объявлен")
  |16: show("Нереализовано")
  |17: show("Рекурсивное определение объекта")
  |18: show("Ожидался символ")
  |19: show("Должен быть тип")
  |20: show("Недопустимое преобразование типа")
  |21: show("Типы несовместимы")
  |22: show("Должен быть скалярный тип")
  |23: show("Ошибка в конструкторе типа")
  |24: show("Должен быть простой (1 слово) тип")
  |25: show("Неправильное константное выражение")
  |26: show("Неправильное выражение")
  |27: show("Должна быть переменная")
  |28: show("Должен быть массив")
  |29: show("Не обладает адресом")
  |30: show("Не обладает значением")
--|31:
  |32: show("Должна быть запись")
  |33: show("Слишком сложное условное выражение")
  |34: show("Ожидалось константное выражение")
  |35: show("Ожидался оператор")
  |36: show("Ошибка в описаниях")
  |37: show("Недопустимо в определяющем модуле")
  |38: show("Недопустимое использование формального типа")
  |39: show("Вызов процедуры в выражении")
  |40: show("Переменная цикла должна быть локальной");
  |41: show("EXIT вне LOOP'а")
  |42: show("Это не модуль")
  |43: show("Неправильный вызов")
  |44: show("Выход за границы диапазона")
  |45: show("Такая метка уже была")
  |46: show("Вызов функции в позиции оператора")
  |47: show("Экспорт невозможен. Объект уже объявлен")
  |48: show("Код записывается байтами! [0..0FFh]")
  |49: show("Неправильное число параметров")
  |50: show("Это не процедура")
  |51: show("Должен быть тип указателя")
  |52: show("Должен быть тип множества")
  |53: show("RETURN можно писать только в процедуре")
  |54: show("Переполнение (исчерпание) в константном выражении")
  |55: show("Ошибка в заголовке симфайла")
  |56: show("Некорректная версия симфайла")
  |57: show("Доступ к дескриптору только с ключом $U+ (unsafe)");
  |58: show("Нереализованная процедура")
  |59: show("Скрытый тип должен быть однословным и не литерным")
  |60: show("Слишком большой размер типа")
  |61: show("Некорректная информация в симфайле")
  |62: show("Попытка подсунуть чужой симфайл (не на Модуле-2)")
  |63: show("Повторный FORWARD")
  |64: show("Слишком большая процедура")
  |65: show("Недопустимое использование идентификатора модуля");
  |66: show("Незакрытый коментарий, начавшийся в строке")
  |67: show("Неправильный синтаксис строки")
  |68: show("Разрешено только на уровне единицы компиляции")
  |69: show("Недоступная RTS процедура")
  |70: show("Спецификатор VAL недопустим в описании процедурного типа");
  |71: show("Слишком много параметров");
  |72: show("В CASE нужна хоть одна альтернатива");
--|73: show("Слишком мало памяти для работы компилятора");
  |74: show("Различное размещение параметра");
  |75: show("Переменная цикла не может быть VAR параметром");
  |76: show("Присваивание VAL переменной (Только Для Чтения)");
  |77: show("Разрешено только в определяющем модуле");
  |78: show("Конфликт версий (по времени компиляции)");
  |79: show("Ограничение транслятора: слишком много");
  |80: show("Копирование динмассивов только с ключом $X+");
  ELSE message(0,"Неизвестная ошибка # %d",no);
  END;
END message;

PROCEDURE Err0(n: INTEGER; id: INTEGER);
  VAR v: ARRAY [0..255] OF CHAR;
BEGIN
  IF fault THEN RETURN END;
  IF id>0 THEN id_str(id,v); message(n,' -- "%s"',v);
  ELSE                       message(n,'');
  END;
END Err0;

PROCEDURE err(N: INTEGER);
BEGIN
  IF NOT occur OR (ORD('A')-ORD('A') IN opts) THEN Err0(N,-1) END;
END err;

PROCEDURE err_id(N: INTEGER; id: INTEGER);
BEGIN
  IF NOT occur OR (ORD('A')-ORD('A') IN opts) THEN Err0(N,id) END
END err_id;

PROCEDURE expc(sy: INTEGER);
  VAR v: ARRAY [0..79] OF CHAR;
BEGIN
  IF fault THEN RETURN END;
  IF occur & NOT (ORD('A')-ORD('A') IN opts) THEN RETURN END;
  IF    sy=ident THEN message(7,'');
  ELSE vis_sym(sy,v); message(18,' %s',v);
  END;
END expc;

PROCEDURE Fault(n: INTEGER; VAL format: ARRAY OF CHAR; SEQ args: sys.WORD);
BEGIN
  IF fault THEN RETURN END;
  fault:=TRUE;
  message(n,format,args);
  inter.print("ФАТАЛЬНО. Компиляция прервана");
END Fault;

PROCEDURE io_fault(VAL format: ARRAY OF CHAR; SEQ args: sys.WORD);
BEGIN
  Fault(0,format,args);
END io_fault;

-----------------------  INITIALIZATION  -----------------------
                       ------------------

PROCEDURE Ini(txt: comp.io_ptr);
BEGIN
  text:=txt;
  text^.print:=io_fault;
  NEW(ref,noIdents);
  NEW(id_pool);
  inter.fifo(id_queue);
  add_stk:=0;
  ini_scaner;
END Ini;

PROCEDURE Exi;
  VAR p: POOL;
BEGIN
  exi_scaner;
  ini_opts;
  DISPOSE(ref);
  DISPOSE(id_pool);
  WHILE inter.pop(id_queue,p) DO DISPOSE(p) END;
END Exi;

BEGIN
  text:=NIL;
  line:=0; fault:=FALSE; add_stk:=0;
  inter.lifo(optSTACK);
  opts:={};
  base:=opts;
END mxScan.
