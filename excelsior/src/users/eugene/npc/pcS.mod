IMPLEMENTATION MODULE pcS; (* Ned 14-Apr-89. (c) KRONOS *)
                           (* Ned 28-Mar-91. (c) KRONOS *)

IMPORT SYSTEM, pcM, pcK;

TYPE LREAL = REAL; (* temporary *)

CONST EOL = 0C;  EOF = 36C;

VAR
  next   : CHAR;
  char   : CHAR;
  source : STRING;
  done   : BOOLEAN;
  occur  : BOOLEAN; (* TRUE if errors occur in current line *)
  comment: LONGINT; (* first line of comment *)
  col0   : INTEGER;
  line0  : INTEGER;
  m2     : BOOLEAN;
  stack  : ARRAY [0..31] OF BITSET;
  sp     : INTEGER;
  base   : BITSET;
  pow    : ARRAY [0..8] OF LREAL;

PROCEDURE NewLine;
BEGIN
  occur:=FALSE;
  REPEAT
    pcM.getstr(source,done);
    IF NOT done THEN
      source[0]:=' ';
      source[1]:=EOL;
      INC(line0); col0:=0;
      next:=EOF; RETURN
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

(*---------------------------------------------------------------*)

PROCEDURE Float(j,i: INTEGER);

 PROCEDURE Ten(e: INTEGER): LREAL;
   VAR k: INTEGER; u: LREAL;
 BEGIN
   k := 0; u := 1.;
   WHILE e > 0 DO
     IF ODD(e) THEN u := pow[k] * u END ;
     e := e DIV 2; INC(k)
   END ;
   RETURN u
 END Ten;

  VAR x,f: LREAL; l,e: INTEGER; last: CHAR; neg: BOOLEAN;
BEGIN
  x:=0.; l:=0;
  WHILE j < i DO (*read int part*)
    IF l < pcM.max_dig THEN
      IF source[j] > "9" THEN err(3) END ;
      x := 10.*x + FLOAT(ORD(source[j])-ORD("0")); INC(l)
    ELSE err(201);
    END;
    INC(j)
  END ;
  l:=0; f:=0.;
  getchar0;
  WHILE ("0"<=char) & (char<="9") DO (*read fraction*)
    IF l < pcM.max_dig THEN
      f := 10.*f + FLOAT(ORD(char)-ORD("0")); INC(l)
    END ;
    getchar0;
  END;
  x := f / Ten(l) + x;  e:=0; neg:=FALSE; last:=CAP(char);
  IF (last="E") OR (last="D") THEN
    getchar0;
    IF    char="-" THEN neg:=TRUE; getchar0
    ELSIF char="+" THEN getchar0
    END ;
    IF ("0"<=char) & (char<="9") THEN
      REPEAT
        e:=e*10 + INTEGER(char)-INTEGER("0");
        getchar0;
      UNTIL (char<"0") OR (char>"9")
    ELSE err(3);
    END
  END;
  IF neg THEN
    IF  e <= pcM.max_exp THEN x := x / Ten(e) ELSE x := 0. END
  ELSIF e <  pcM.max_exp THEN x := Ten(e) * x
  ELSE err(201); x:=0.
  END;
  IF last="D" THEN
    pcM.abort;
(*  lit:=lrlval; double:=x; *)
  ELSE
    lit:=realval;
    IF x <= pcM.max_real THEN real:=x; (* SHORT(x) *)
    ELSE err(201); real:= 1.
    END
  END
END Float;

PROCEDURE Number;
  VAR c,i: INTEGER; ord: LONGINT; last,ch: CHAR;
BEGIN
  c:=col0-2; i:=c;
  REPEAT INC(i) UNTIL pcM.alpha[source[i]]#2C;
  WHILE source[c]='0' DO INC(c) END;
  last:=CAP(source[i-1]);
  char:=source[i];
  IF char#EOL THEN next:=source[i+1] END;
  col0:=i+2;
  IF (char=".") & (next#".") THEN Float(c,i); RETURN END;
  int:=0; lit:=intval;
  IF (last="H") OR (last="X") THEN
    DEC(i);
    IF i-c>pcM.max_hex_dig THEN err(201);
    ELSE
      WHILE c<i DO
        ch:=source[c];
        IF (i-c=pcM.max_hex_dig-1) & (int>7) THEN DEC(int,16) END;
        IF    (ORD(ch)-ORD("0")) IN {0..9} THEN
          ord:=LONGINT(ch)-LONGINT("0")
        ELSIF (ORD(CAP(ch))-ORD("A")) IN {0..5} THEN
          ord:=LONGINT(ch)-(LONGINT("A")-10);
        ELSE err(3)
        END;
        int:=int*10H+ord;
        INC(c);
      END;
    END;
  ELSIF (last='C') OR (last="B") THEN
    DEC(i);
    WHILE c<i DO
      ord:=LONGINT(source[c])-LONGINT("0");
      IF  (ord<0) OR(ord>7) THEN err(3) END;
      IF int>(pcM.max_lint-ord) DIV 10B THEN err(201); int:=0 END;
      int:=int*10B+ord;
      INC(c);
    END;
  ELSE
    WHILE c<i DO
      ord:=LONGINT(source[c])-LONGINT("0");
      IF (ord<0) OR (ord>9) THEN err(3) END;
      IF int>(pcM.max_lint-ord) DIV 10 THEN err(201); int:=0 END;
      int:=int*10+ord;
      INC(c);
    END;
  END;
  IF (last="X") OR (last="C") THEN
    IF int>255 THEN err(201); int:=0 END;
    lit:=charval
  END;
END Number;

PROCEDURE Prags;
  VAR cap: CHAR; no: CARDINAL;
BEGIN
  getchar;
  LOOP
    cap:=CAP(char);
    IF    char="<" THEN getchar; stack[sp]:=opts; INC(sp);
    ELSIF char=">" THEN getchar;
      IF sp>0 THEN DEC(sp); opts:=stack[sp] ELSE  opts:=base END;
    ELSIF char="!" THEN getchar; sp:=0; opts:=base;
    ELSIF (cap>='A') & (cap<='Z') THEN
      CASE cap OF
        |'A': no:=all_errors;
        |'R': no:=index_check;
        |'T': no:=range_check;
        |'P': no:=nil_check;
        |'N': no:=proc_check;
        |'G': no:=type_check;
        |'E': no:=external;
      ELSE RETURN;
      END;
      getchar;
      IF    char='+' THEN getchar; INCL(opts,no);
      ELSIF char='-' THEN getchar; EXCL(opts,no);
      ELSE RETURN
      END;
    ELSE RETURN
    END;
  END;
END Prags;

PROCEDURE Comment;

  PROCEDURE err;
    VAR s: ARRAY [0..15] OF CHAR;
  BEGIN
    s:='';
    pcM.app_num(s,comment);
    Fault(2,s);
  END err;

  VAR c0: LONGINT;
BEGIN
  c0:=comment; comment:=txtpos DIV 10000H;
  getchar;
  LOOP
    IF char='*'    THEN getchar;
      IF char=')'  THEN comment:=c0; RETURN END;
    ELSIF char='(' THEN getchar;
      IF char='*'  THEN Comment END;
    ELSIF char='$' THEN Prags
    ELSIF char=EOF THEN err; RETURN
    ELSIF fault    THEN RETURN
    ELSE getchar
    END
  END;
END Comment;

PROCEDURE QStr(q: CHAR);
  VAR i: INTEGER;
BEGIN
  i:=0; getchar0;
  WHILE (char#EOL) & (char#q) & (i<max_str) DO
    string[i]:=char; INC(i);
    getchar0;
  END;
  string[i]:=0C;
  IF char#EOL THEN
    IF char#q THEN
      err(4);
      WHILE (char#EOL) & (char#q) DO getchar0 END;
    END;
    IF i=1 THEN int:=LONGINT(string[0]); lit:=charval
    ELSE
      len:=i+1; lit:=strval
    END;
    IF char#EOL THEN getchar0 END;
  ELSE err(4);
  END;
END QStr;

(*----------------------------------------------------------------*)

PROCEDURE identifier(VAR sy: Symbol);
  VAR i: INTEGER;
BEGIN
  i:=0;
  REPEAT
    name[i]:=char;
    char:=next; next:=source[col0];
    INC(col0); INC(i);
  UNTIL (i>=pcK.max_name) OR (pcM.alpha[char]=0C);
  name[i]:=0C; len:=i;
  IF i=pcK.max_name THEN
    err(6);
    REPEAT getchar0 UNTIL pcM.alpha[char]=0C;
  END;
  sy:=ident;
END identifier;

PROCEDURE get(VAR sy: Symbol);
BEGIN
  IF fault THEN sy:=end; RETURN END;
  txtpos:=LONGINT(line0)*10000H+LONGINT(col0)-2;
  LOOP
    CASE char OF
    |'0'..'9': Number;     sy:=literal; RETURN
    |'"',"'" : QStr(char); sy:=literal; RETURN
    |'+': getchar0; sy:=plus;   RETURN
    |'*': getchar0; sy:=times;  RETURN
    |'[': getchar0; sy:=lbr;    RETURN
    |']': getchar0; sy:=rbr;    RETURN
    |'{': getchar0; sy:=lbrace; RETURN
    |'}': getchar0; sy:=rbrace; RETURN
    |'/': getchar0; sy:=slash;  RETURN
    |'^': getchar0; sy:=bar;    RETURN
    |'&': getchar0; sy:=and;    RETURN
    |'|': getchar0; sy:=sep;    RETURN
    |';': getchar0; sy:=semic;  RETURN
    |',': getchar0; sy:=coma;   RETURN
    |'=': getchar0; sy:=equ;    RETURN
    |'#': getchar0; sy:=neq;    RETURN
    |')': getchar0; sy:=rpar;   RETURN
    |'~': getchar0; sy:=not;    RETURN
    |'-': IF next='-' THEN REPEAT getchar0 UNTIL next=EOL;
          ELSE getchar0; sy:=minus; RETURN
          END
    |':': getchar0;
          IF char='=' THEN getchar0; sy:=becomes ELSE sy:=colon END;
          RETURN
    |'.': getchar0;
          IF char='.' THEN getchar0; sy:=range   ELSE sy:=period END;
          RETURN
    |'>': getchar0;
          IF    char='=' THEN getchar0; sy:=geq ELSE sy:=gtr END;
          RETURN
    |'<': getchar0;
          IF char='=' THEN getchar0; sy:=leq    ELSE sy:=lss END;
          RETURN
    |'(': getchar0;
          IF char='*' THEN Comment; ELSE sy:=lpar; RETURN END;
    |'_','a'..'z',
     ' ','¡','¢','£','¤','¥','¦','§','¨','©','ª','«','¬','­','®','¯',
     'à','á','â','ã','ä','å','æ','ç','è','é','ê','ë','ì','í','î','ï',
     '€','','‚','ƒ','„','…','†','‡','ˆ','‰','Š','‹','Œ','','Ž','',
     '','‘','’','“','”','•','–','—','˜','™','š','›','œ','','ž','Ÿ':
          identifier(sy); RETURN
    |'A': identifier(sy);
          IF    pcM.str_equ(name,"AND") THEN sy:=and
          ELSIF pcM.str_equ(name,"ARRAY") THEN sy:=array
          END; RETURN
    |'B': identifier(sy);
          IF    pcM.str_equ(name,"BEGIN") THEN sy:=begin
          ELSIF pcM.str_equ(name,"BY") THEN sy:=by
          END; RETURN
    |'C': identifier(sy);
          IF    pcM.str_equ(name,"CONST") THEN sy:=const
          ELSIF pcM.str_equ(name,"CASE")  THEN sy:=case
          ELSIF pcM.str_equ(name,"CODE")  THEN sy:=code;
          END; RETURN
    |'D': identifier(sy);
          IF    pcM.str_equ(name,"DO") THEN sy:=do
          ELSIF pcM.str_equ(name,"DIV") THEN sy:=div
          ELSIF pcM.str_equ(name,"DYNARR") THEN sy:=dynarr
          ELSIF pcM.str_equ(name,"DEFINITION") THEN
            IF m2 THEN sy:=definition END;
          END; RETURN
    |'E': identifier(sy);
          IF    pcM.str_equ(name,"END")   THEN sy:=end
          ELSIF pcM.str_equ(name,"ELSE")  THEN sy:=else
          ELSIF pcM.str_equ(name,"ELSIF") THEN sy:=elsif
          ELSIF pcM.str_equ(name,"EXIT")  THEN sy:=exit
          END; RETURN
    |'F': identifier(sy);
          IF    pcM.str_equ(name,"FOR")     THEN sy:=for
          ELSIF pcM.str_equ(name,"FORWARD") THEN
            IF m2 THEN sy:=forward END;
          END; RETURN
    |'G'..'H': identifier(sy); RETURN
    |'I': identifier(sy);
          IF    pcM.str_equ(name,"IF") THEN sy:=if
          ELSIF pcM.str_equ(name,"IN") THEN sy:=in
          ELSIF pcM.str_equ(name,"IS") THEN sy:=is
          ELSIF pcM.str_equ(name,"IMPORT") THEN sy:=import
          ELSIF m2 & pcM.str_equ(name,"IMPLEMENTATION") THEN sy:=implementation
          END; RETURN
    |'J'..'K': identifier(sy); RETURN
    |'L': identifier(sy);
          IF    pcM.str_equ(name,"LOOP") THEN sy:=loop END; RETURN
    |'M': identifier(sy);
          IF    pcM.str_equ(name,"MOD") THEN sy:=mod
          ELSIF pcM.str_equ(name,"MODULE") THEN sy:=module
          END; RETURN
    |'N': identifier(sy);
          IF    pcM.str_equ(name,"NOT") THEN sy:=not
          ELSIF pcM.str_equ(name,"NIL") THEN sy:=nil
          END; RETURN
    |'O': identifier(sy);
          IF    pcM.str_equ(name,"OR") THEN sy:=or
          ELSIF pcM.str_equ(name,"OF") THEN sy:=of
          END; RETURN
    |'P': identifier(sy);
          IF    pcM.str_equ(name,"PROCEDURE") THEN sy:=procedure
          ELSIF pcM.str_equ(name,"POINTER") THEN sy:=pointer
          END; RETURN
    |'Q': identifier(sy); RETURN
    |'R': identifier(sy);
          IF    pcM.str_equ(name,"RETURN") THEN sy:=return
          ELSIF pcM.str_equ(name,"REPEAT") THEN sy:=repeat
          ELSIF pcM.str_equ(name,"RECORD") THEN sy:=record
          ELSIF pcM.str_equ(name,"REM") THEN sy:=rem
          END; RETURN
    |'S': identifier(sy);
          IF    pcM.str_equ(name,"SET") THEN
            IF m2 THEN sy:=set END;
          ELSIF pcM.str_equ(name,"SEQ") THEN sy:=seq
          END; RETURN
    |'T': identifier(sy);
          IF    pcM.str_equ(name,"THEN") THEN sy:=then
          ELSIF pcM.str_equ(name,"TO")   THEN sy:=to
          ELSIF pcM.str_equ(name,"TYPE") THEN sy:=type
          END; RETURN
    |'U': identifier(sy);
          IF    pcM.str_equ(name,"UNTIL") THEN sy:=until END; RETURN
    |'V': identifier(sy);
          IF    pcM.str_equ(name,"VAR") THEN sy:=var
          ELSIF pcM.str_equ(name,"VAL") THEN
            IF m2 THEN sy:=val END;
          END; RETURN
    |'W': identifier(sy);
          IF    pcM.str_equ(name,"WHILE") THEN sy:=while
          ELSIF pcM.str_equ(name,"WITH")  THEN sy:=with
          END; RETURN
    |'X'..'Z': identifier(sy); RETURN
    |EOL: NewLine;
    |EOF: Fault(5,''); sy:=end; RETURN
    |' ',11C:
    ELSE err(1)
    END;
    char:=next; next:=source[col0]; INC(col0);
  END;
END get;

(*----------------------------------------------------------------*)

PROCEDURE message(no: INTEGER; s: ARRAY OF CHAR);
  VAR msg: ARRAY [0..255] OF CHAR; c: INTEGER;
BEGIN
  IF fault OR occur & NOT (all_errors IN opts) THEN RETURN END;
  pcM.err_msg(no,msg);
  IF s[0]#0C THEN pcM.app(msg,s) END;
  IF pcM.pass2 THEN
    pcM.error(INTEGER(txtpos DIV 10000H),INTEGER(txtpos MOD 10000H),'',msg);
  ELSE
    IF col0>=2 THEN c:=col0-2 ELSE c:=0 END;
    pcM.error(line0,c,source,msg);
    occur:=TRUE;
  END;
  INC(no_errs);
END message;

PROCEDURE err(n: INTEGER);
BEGIN
  message(n,'');
END err;

PROCEDURE err_id(n: INTEGER; name: ARRAY OF CHAR);
  VAR s: ARRAY [0..79] OF CHAR;
BEGIN
  s:=' -- ';
  pcM.app(s,name);
  message(n,s);
END err_id;

PROCEDURE expc(s: Symbol);
  VAR v: ARRAY [0..15] OF CHAR;
BEGIN
  CASE s OF
    |sep    : v:='"|"'
    |semic  : v:='";"'
    |colon  : v:='":"'
    |period : v:='"."'
    |lbr    : v:='"["'
    |rbr    : v:='"]"'
    |lpar   : v:='"("'
    |rpar   : v:='")"'
    |lbrace : v:='"{"'
    |rbrace : v:='"}"'
    |coma   : v:='","'
    |equ    : v:='"="'
    |becomes: v:='":="'
    |range  : v:='".."'
    |do     : v:='"DO"'
    |end    : v:='"END"'
    |of     : v:='"OF"'
    |then   : v:='"THEN"'
    |to     : v:='"TO"'
    |until  : v:='"UNTIL"'
  ELSE        v:='"****"'
  END;
  message(8,v);
END expc;

PROCEDURE Fault(n: INTEGER; msg: ARRAY OF CHAR);
BEGIN
  IF NOT fault THEN message(n,msg); fault:=TRUE END;
  pcM.abort;
END Fault;

(*----------------------------------------------------------------*)

PROCEDURE Ini;
BEGIN
  m2:=NOT pcM.oberon;
  occur:=FALSE; no_errs:=0; fault:=FALSE;
  comment:=-1;
  line0:=-1; col0:=0;
  txtpos:=0;
  next:=EOL; NewLine;
  getchar0;
  sp:=0;
  base:=opts;
END Ini;

VAR i: INTEGER;

BEGIN
  txtpos:=0; fault:=FALSE;
  opts:={index_check,range_check,nil_check};
  base:=opts;
  i := 0; pow[0] := 10.;
  REPEAT pow[i+1] := pow[i] * pow[i]; INC(i)
  UNTIL i = 5;
(*UNTIL i = 8;*)
END pcS.
