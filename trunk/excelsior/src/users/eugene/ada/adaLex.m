IMPLEMENTATION MODULE adaLex; (* 03-Apr-89. (c) KRONOS *)

FROM SYSTEM     IMPORT  WORD;
FROM Terminal   IMPORT  print;
FROM Image      IMPORT  image0;
FROM ASCII      IMPORT  LF, CR, HT, NL;

IMPORT adaInp;
IMPORT mcd : mCodeMnem;

FROM adaInp     IMPORT  eof;

TYPE
  err_rec=RECORD
    pos  : INTEGER;
    txt  : ARRAY [0..79] OF CHAR;
  END;

VAR
  ch        : CHAR;
  next_ch   : CHAR;
  ch_pos    : INTEGER;
  id        : BOOLEAN;
  aps       : BOOLEAN;
  next_val  : value;
  errs      : ARRAY [0..5] OF err_rec;
  eno       : INTEGER;

PROCEDURE get_ch;
BEGIN
  ch:=next_ch; ch_pos:=adaInp.ch_pos;
  next_ch:=adaInp.get_ch();
END get_ch;

PROCEDURE error_cnt(): INTEGER;
BEGIN
  RETURN eno;
END error_cnt;

PROCEDURE put_errors;
  VAR
    txt  : ARRAY [0..79] OF CHAR;
    pos  : INTEGER;
    len  : INTEGER;
    line : INTEGER;
  PROCEDURE get_txt;
  BEGIN
    get_ch; INC(line); pos:=ch_pos; len:=0;
    WHILE (ch#LF) & (ch#eof) DO
      IF len<HIGH(txt) THEN txt[len]:=ch; INC(len) END;
      get_ch;
    END;
    txt[len]:=0c;
  END get_txt;
  VAR
    e    : err_rec;
    buf  : ARRAY [0..79] OF CHAR;
    i,j,k: INTEGER;
BEGIN
  IF eno=0 THEN RETURN END;
  i:=0;
  WHILE (i<eno-1) DO
    IF errs[i+1].pos<errs[i].pos THEN
      e:=errs[i+1]; errs[i+1]:=errs[i]; errs[i]:=e;
      IF i>0 THEN DEC(i) END;
    ELSE
      INC(i);
    END;
  END;
  adaInp.set_pos(0); get_ch; line:=-1; i:=0;
  REPEAT
    REPEAT get_txt UNTIL (errs[i].pos<=pos+len) OR (ch=eof);
    FOR j:=0 TO HIGH(buf) DO buf[j]:='-' END;
    buf[HIGH(buf)]:=0c;
    WHILE (i<eno) & ((errs[i].pos<=pos+len) OR (ch=eof)) DO
      k:=errs[i].pos-pos;
      print('[%5d:%3$d ] %s.\n',line,k,errs[i].txt);
      IF k<HIGH(buf) THEN buf[k]:='^' END;
      INC(i);
    END;
    print('%s\n%s\n',txt,buf);
  UNTIL (i=eno) OR (ch=eof);
  eno:=0;
END put_errors;

PROCEDURE halt;
BEGIN
  put_errors;
  print('***** Compilation aborted *****\n');
  HALT(1);
END halt;

PROCEDURE error(p: INTEGER; VAL fmt: ARRAY OF CHAR; SEQ args: WORD);
  VAR i,j: INTEGER; str: ARRAY [0..255] OF CHAR; fault: BOOLEAN;
BEGIN
  IF eno>HIGH(errs) THEN halt END;
  i:=0; j:=0; fault:=FALSE;
  WHILE (i<=HIGH(fmt)) & (fmt[i]#0c) DO
    IF j<HIGH(str) THEN
      IF (fmt[i]='%') & (i<HIGH(fmt)) & (fmt[i+1]='.') THEN
        INC(i); fault:=TRUE;
      ELSE
        str[j]:=fmt[i]; INC(j);
      END;
    END;
    INC(i);
  END;
  str[j]:=0c;
  errs[eno].pos:=p;
  image0(errs[eno].txt,str,args);
  INC(eno);
  IF fault THEN halt END;
END error;

PROCEDURE get_ident(): lex_elem;
  VAR d,r: BOOLEAN; c: CHAR; i,n: INTEGER;
BEGIN
  i:=0; n:=0;
  LOOP
    IF (ch>='a') & (ch<='z') THEN c:=CHAR(ORD(ch)-40b) ELSE c:=ch END;
    IF i>=HIGH(next_val.str) THEN
      error(next_pos,'слишком длинный идентификатор%.');
    END;
    d:=(c='_');
    next_val.str[i]:=c; INC(i); get_ch;
    r:=(ch>='0') & (ch<='9') OR (ch='_');
    IF r THEN
      INC(n)
    ELSE
      r:= (ch>='A') & (ch<='Z') OR (ch>='a') & (ch<='z')
    END;
    IF NOT r THEN
      next_val.str[i]:=0c; next_val.len:=i;
      IF d THEN
        error(ch_pos,'такое использование подчеркивания не допустимо');
      END;
      IF aps OR (n>0) THEN RETURN ident END;
      CASE next_val.str[0] OF
        |'A': IF next_val.str='ABORT'     THEN RETURN abort     END;
              IF next_val.str='ABS'       THEN RETURN abs       END;
              IF next_val.str='ACCEPT'    THEN RETURN accept    END;
              IF next_val.str='ACCESS'    THEN RETURN access    END;
              IF next_val.str='ALL'       THEN RETURN all       END;
              IF next_val.str='AND'       THEN RETURN and       END;
              IF next_val.str='ARRAY'     THEN RETURN array     END;
              IF next_val.str='AT'        THEN RETURN at        END;
        |'B': IF next_val.str='BEGIN'     THEN RETURN begin     END;
              IF next_val.str='BODY'      THEN RETURN body      END;
        |'C': IF next_val.str='CASE'      THEN RETURN case      END;
              IF next_val.str='CONSTANT'  THEN RETURN constant  END;
        |'D': IF next_val.str='DECLARE'   THEN RETURN declare   END;
              IF next_val.str='DELAY'     THEN RETURN delay     END;
              IF next_val.str='DELTA'     THEN RETURN delta     END;
              IF next_val.str='DIGITS'    THEN RETURN digits    END;
              IF next_val.str='DO'        THEN RETURN do        END;
        |'E': IF next_val.str='ELSE'      THEN RETURN else      END;
              IF next_val.str='ELSIF'     THEN RETURN elsif     END;
              IF next_val.str='END'       THEN RETURN end       END;
              IF next_val.str='ENTRY'     THEN RETURN entry     END;
              IF next_val.str='EXCEPTION' THEN RETURN exception END;
              IF next_val.str='EXIT'      THEN RETURN exit      END;
        |'F': IF next_val.str='FOR'       THEN RETURN for       END;
              IF next_val.str='FUNCTION'  THEN RETURN function  END;
        |'G': IF next_val.str='GENERIC'   THEN RETURN generic   END;
              IF next_val.str='GOTO'      THEN RETURN goto      END;
        |'I': IF next_val.len#2 THEN RETURN ident END;
              IF next_val.str='IF'        THEN RETURN if        END;
              IF next_val.str='IN'        THEN RETURN in        END;
              IF next_val.str='IS'        THEN RETURN is        END;
        |'L': IF next_val.str='LIMITED'   THEN RETURN limited   END;
              IF next_val.str='LOOP'      THEN RETURN loop      END;
        |'M': IF next_val.str='MOD'       THEN RETURN mod       END;
        |'N': IF next_val.str='NEW'       THEN RETURN new       END;
              IF next_val.str='NOT'       THEN RETURN not       END;
              IF next_val.str='NULL'      THEN RETURN null      END;
        |'O': IF next_val.str='OF'        THEN RETURN of        END;
              IF next_val.str='OR'        THEN RETURN or        END;
              IF next_val.str='OTHERS'    THEN RETURN others    END;
              IF next_val.str='OUT'       THEN RETURN out       END;
        |'P': IF next_val.str='PACKAGE'   THEN RETURN package   END;
              IF next_val.str='PRAGMA'    THEN RETURN pragma    END;
              IF next_val.str='PRIVATE'   THEN RETURN private   END;
              IF next_val.str='PROCEDURE' THEN RETURN procedure END;
        |'R': IF next_val.str='RAISE'     THEN RETURN raise     END;
              IF next_val.str='RANGE'     THEN RETURN range     END;
              IF next_val.str='RECORD'    THEN RETURN record    END;
              IF next_val.str='REM'       THEN RETURN rem       END;
              IF next_val.str='RENAMES'   THEN RETURN renames   END;
              IF next_val.str='RETURN'    THEN RETURN return    END;
              IF next_val.str='REVERSE'   THEN RETURN reverse   END;
        |'S': IF next_val.str='SELECT'    THEN RETURN select    END;
              IF next_val.str='SEPARATE'  THEN RETURN separate  END;
              IF next_val.str='SUBTYPE'   THEN RETURN subtype   END;
        |'T': IF next_val.str='TASK'      THEN RETURN task      END;
              IF next_val.str='TERMINATE' THEN RETURN terminate END;
              IF next_val.str='THEN'      THEN RETURN then      END;
              IF next_val.str='TYPE'      THEN RETURN type      END;
        |'U': IF next_val.str='USE'       THEN RETURN use       END;
        |'W': IF next_val.str='WHEN'      THEN RETURN when      END;
              IF next_val.str='WHILE'     THEN RETURN while     END;
              IF next_val.str='WITH'      THEN RETURN with      END;
        |'X': IF next_val.str='XOR'       THEN RETURN xor       END;
      ELSE RETURN ident;
      END;
      RETURN ident;
    END;
  END;
END get_ident;

PROCEDURE comment;
BEGIN
  WHILE (ch#eof) & (ch#LF) DO get_ch END;
END comment;

PROCEDURE get_number_ovr(): lex_elem;
  VAR dig: INTEGER;
  PROCEDURE dig?(base: INTEGER): BOOLEAN;
  BEGIN
    IF (ch>='A') & (ch<='F') THEN dig:=ORD(ch)-ORD('A')+10
    ELSIF (ch>='a') & (ch<='f') THEN dig:=ORD(ch)-ORD('a')+10
    ELSIF (ch>='0') & (ch<='9') THEN dig:=ORD(ch)-ORD('0')
    ELSE RETURN FALSE;
    END;
    RETURN dig<base;
  END dig?;
  PROCEDURE next_dig?(base: INTEGER): BOOLEAN;
  BEGIN
    IF (next_ch>='A') & (next_ch<='F') THEN dig:=ORD(next_ch)-ORD('A')+10
    ELSIF (next_ch>='a') & (next_ch<='f') THEN dig:=ORD(next_ch)-ORD('a')+10
    ELSIF (next_ch>='0') & (next_ch<='9') THEN dig:=ORD(next_ch)-ORD('0')
    ELSE RETURN FALSE;
    END;
    RETURN dig<base;
  END next_dig?;
  PROCEDURE cardinal(VAR v: long_int; base: INTEGER);
  BEGIN
    long_zero(v);
    IF NOT dig?(base) THEN error(ch_pos,'digit expected'); RETURN END;
    LOOP
      mul_short(v,base); add_short(v,dig); get_ch;
      IF (ch='_') & next_dig?(base) THEN get_ch;
      ELSIF NOT dig?(base) THEN EXIT;
      END;
    END;
  END cardinal;
  PROCEDURE exp(VAR n: long_int; VAR se: BOOLEAN; base: INTEGER);
    VAR v,pos: INTEGER;
  BEGIN
    long_zero(n); n.i[0]:=1; se:=FALSE;
    IF (ch#'E') & (ch#'e') THEN RETURN END;
    get_ch; v:=0;
    IF ch='-' THEN get_ch; se:=TRUE ELSIF ch='+' THEN get_ch END;
    IF NOT dig?(10) THEN error(ch_pos,'digit expected'); RETURN END;
    pos:=ch_pos;
    LOOP
      IF v>1000 THEN error(pos,'too large exponent'); v:=0 END;
      v:=v*10+dig; get_ch;
      IF (ch='_') & next_dig?(base) THEN get_ch;
      ELSIF NOT dig?(base) THEN EXIT;
      END;
    END;
    WHILE v>0 DO mul_short(n,base); DEC(v) END;
  END exp;
  PROCEDURE tail(n: long_int; base: INTEGER; end: CHAR): lex_elem;
    VAR sig: BOOLEAN; m: long_int; fn,fm: long_float;
  BEGIN
    IF (ch='.') & next_dig?(base) THEN
      get_ch; long_zero(m); m.i[0]:=1;
      LOOP
        mul_short(n,base); mul_short(m,base); add_short(n,dig);
        get_ch;
        IF (ch='_') & next_dig?(base) THEN get_ch;
        ELSIF NOT dig?(base) THEN EXIT;
        END;
      END;
      int_float(n,fn); int_float(m,fn); fdiv(fn,fm);
      IF end#' ' THEN
        IF ch#end THEN error(ch_pos,'"%c" expected',end) ELSE get_ch END;
      END;
      exp(m,sig,base); int_float(m,fm);
      IF sig THEN fdiv(fn,fm) ELSE fmul(fn,fm) END;
      next_val.flo:=fn; RETURN real_number;
    END;
    IF end#' ' THEN
      IF ch#end THEN error(ch_pos,'"%c" expected',end) ELSE get_ch END;
    END;
    exp(m,sig,base);
    IF sig THEN div(n,m) ELSE mul(n,m) END;
    next_val.int:=n; RETURN int_number;
  END tail;
  VAR
    n   : long_int;
    base: INTEGER;
    i   : INTEGER;
    c   : CHAR;
    err : BOOLEAN;
BEGIN
  cardinal(n,10);
  IF (ch='#') OR (ch=':') THEN
    c:=ch; get_ch; err:=FALSE;
    FOR i:=1 TO HIGH(n.i) DO err:=err OR (n.i[i]#0) END;
    err:=err OR (n.i[0]<2) OR (n.i[0]>16);
    IF err THEN
      base:=16; error(next_pos,'допустимы основания от 2 до 16');
    ELSE
      base:=n.i[0];
    END;
    cardinal(n,base);
  ELSE
    base:=10; c:=' ';
  END;
  RETURN tail(n,base,c);
END get_number_ovr;

PROCEDURE get_number(): lex_elem;
  VAR l: lex_elem;
BEGIN
  ovr:=FALSE;
  l:=get_number_ovr();
  IF ovr THEN ovr:=FALSE; error(lex_pos,'invalid number') END;
  RETURN l;
END get_number;

PROCEDURE get_string(): lex_elem;
  VAR c: CHAR; i,p: INTEGER;
BEGIN
  ASSERT((ch='"') OR (ch='%'));
  p:=ch_pos; c:=ch; i:=0;
  get_ch;
  LOOP
    IF (ch=LF) OR (ch=eof) THEN error(p,'незакрытая строка'); EXIT END;
    IF ch=c THEN
      get_ch;
      IF ch#c THEN EXIT END;
    END;
    IF i>=HIGH(next_val.str) THEN
      REPEAT get_ch UNTIL (ch=c) OR (ch=eof) OR (ch=LF);
      error(next_pos,'слишком длинная строка'); EXIT;
    END;
    next_val.str[i]:=ch; INC(i); get_ch;
  END;
  next_val.str[i]:=0c; next_val.len:=i;
  RETURN string;
END get_string;

PROCEDURE get_char(): lex_elem;
  VAR c: CHAR;
BEGIN
  get_ch;
  IF (ch=LF) OR (ch=eof) THEN
    error(ch_pos,'странный символьный литерал');
  ELSE
    c:=ch; get_ch;
    IF ch#"'" THEN
      error(ch_pos,'странный символьный литерал');
    ELSE
      get_ch
    END;
  END;
  next_val.str[0]:="'";
  next_val.str[1]:=c;
  next_val.str[2]:="'";
  next_val.str[3]:=0c;
  next_val.len:=3;
  RETURN char;
END get_char;

PROCEDURE get_lex(): lex_elem;
BEGIN
  LOOP
    next_pos:=ch_pos;
    CASE ch OF
      |eof     : RETURN eot;
      |' '     : get_ch; id:=FALSE; aps:=FALSE;
      |LF      : get_ch; id:=FALSE; aps:=FALSE;
      |CR      : get_ch; id:=FALSE; aps:=FALSE;
      |NL      : get_ch; id:=FALSE; aps:=FALSE;
      |HT      : get_ch; id:=FALSE; aps:=FALSE;
      |'&'     : get_ch; RETURN ampersand;
      |'('     : get_ch; RETURN left_parenthesis;
      |')'     : get_ch; RETURN right_parenthesis;
      |'*'     : get_ch;
                 IF ch#'*' THEN RETURN star END;
                 get_ch; RETURN double_star;
      |'+'     : get_ch; RETURN plus;
      |','     : get_ch; RETURN comma;
      |'-'     : get_ch;
                 IF ch#'-' THEN RETURN minus END;
                 comment;
      |'.'     : get_ch;
                 IF ch#'.' THEN RETURN point END;
                 get_ch; RETURN double_dot;
      |'/'     : get_ch;
                 IF ch#'=' THEN RETURN slash END;
                 get_ch; RETURN inequality;
      |':'     : get_ch;
                 IF ch#'=' THEN RETURN colon END;
                 get_ch; RETURN assign;
      |';'     : get_ch; RETURN semicolon;
      |'<'     : get_ch;
                 CASE ch OF
                   |'=': get_ch; RETURN less_equal;
                   |'<': get_ch; RETURN label_left;
                   |'>': get_ch; RETURN box;
                 ELSE RETURN less;
                 END;
      |'='     : get_ch;
                 IF ch#'>' THEN RETURN equal END;
                 get_ch; RETURN arrow;
      |'>'     : get_ch;
                 CASE ch OF
                   |'=': get_ch; RETURN greater_equal;
                   |'>': get_ch; RETURN label_right;
                 ELSE RETURN greater;
                 END;
      |'|','!' : get_ch; RETURN bar;
      |'A'..'Z','a'..'z': RETURN get_ident();
      |'0'..'9': RETURN get_number();
      |'"','%' : RETURN get_string();
      |"'"     : IF id THEN get_ch; RETURN apostrophe END;
                 RETURN get_char();
    ELSE
      error(next_pos,'недопустимый символ: %$2hh',ch); get_ch;
    END;
  END;
END get_lex;

PROCEDURE next;
BEGIN
  sy:=next_sy; val:=next_val; lex_pos:=next_pos;
  next_sy:=get_lex();
  id:=next_sy=ident;
  aps:=next_sy=apostrophe;
END next;

PROCEDURE set_pos(n: INTEGER);
BEGIN
  adaInp.set_pos(n); get_ch; get_ch;
  id:=FALSE; aps:=FALSE;
  next; next;
END set_pos;

BEGIN
  ovr:=FALSE;
  sy:=eot; next_sy:=eot;
  lex_pos:=0; val.len:=0;
  next_pos:=0; next_val.len:=0;
  id:=FALSE; aps:=FALSE; eno:=0; get_ch; get_ch;
END adaLex.
