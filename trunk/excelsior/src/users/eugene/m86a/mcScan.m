IMPLEMENTATION MODULE mcScan; (* Leo xx-Xxx-86. (c) KRONOS *)
                              (* Ned 18-Sep-86. (c) KRONOS *)
                           (* mx:Ned 03-Mar-87. (c) KRONOS *)
                              (* Ned 11-Aug-90. (c) KRONOS *)

IMPORT    low: lowLevel;
IMPORT    sys: SYSTEM;
IMPORT   comp: coDefs;
IMPORT  inter: pcSystem;
IMPORT    ers: coErrors;
IMPORT    pc : pcTab;

WITH STORAGE: inter;


CONST mem_unit = BYTES(sys.WORD) DIV SIZE(sys.WORD);

--------------------------  OPTIONS  ---------------------------
                          -----------

VAR   base: BITSET;
  optSTACK: inter.QUEUE;

---------------------------  SCANER  ---------------------------
                           ----------

CONST EOF = 36c; EOL = 0c;

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
    ELSE next:=EOF; RETURN
    END;
    INC(line0); col0:=0;
    WHILE source[col0]=' ' DO INC(col0) END;
    next:=source[col0]; INC(col0);
  UNTIL next#EOL;
END NewLine;

PROCEDURE getchar0;
BEGIN
  char:=next;
  IF next#EOF THEN next:=source[col0]; INC(col0) END;
END getchar0;

PROCEDURE getchar;
BEGIN
  char:=next;
  IF next#EOF THEN next:=source[col0]; INC(col0) END;
  IF char=EOL THEN NewLine END;
END getchar;

PROCEDURE Fraction(v: INTEGER);
  VAR r,f,f10,fc,e,sg,ps: INTEGER;
BEGIN
  ps:=line0*10000h+col0;
  ASSERT(char='.'); getchar0;
  sy:=realval;
  pc.gen_const(ps,pc.float,pc.real,1,0,cVal);
  pc.gen_const(ps,pc.float,pc.real,v,0,r);
  pc.gen_const(ps,pc.float,pc.real,1,0,f);
  pc.gen_const(ps,pc.float,pc.real,10,0,f10);
  WHILE ORD(char)-ORD('0') IN {0..9} DO
    pc.gen_const(ps,pc.slash,pc.real,f,f10,f);
    pc.gen_const(ps,pc.float,pc.real,ORD(char)-ORD('0'),0,fc);
    pc.gen_const(ps,pc.star, pc.real,fc,f,fc);
    pc.gen_const(ps,pc.plus, pc.real,fc,r,r);
    getchar0
  END;
  IF CAP(char)='E' THEN
    getchar0; sg:=+1;
    IF char='-' THEN sg:=-1; getchar0 ELSIF char='+' THEN getchar0 END;
    e:=0;
    WHILE ORD(char)-ORD('0') IN {0..9} DO
      IF e<255 THEN e:=e*10+ORD(char)-ORD('0') END;
      getchar0;
    END;
    WHILE e>0 DO
      IF sg>0 THEN pc.gen_const(ps,pc.star,pc.real,r,f10,r);
      ELSE         pc.gen_const(ps,pc.slash,pc.real,r,f10,r);
      END;
      DEC(e)
    END;
  END;
  cVal:=r;
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
    IF hex THEN cVal:=hv ELSE err(ers.overflow) END;
  ELSIF (c='B') OR (c='C') THEN getchar0;
    IF oct THEN cVal:=ov ELSE err(ers.overflow) END;
    IF c='C' THEN sy:=charval END;
  ELSE
    IF dec THEN cVal:=dv
    ELSIF hex THEN err(ers.ill_num); cVal:=hv
    ELSE err(ers.overflow)
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
    ELSIF char="!" THEN getchar;
      inter.clear(optSTACK); inter.lifo(optSTACK); opts:=base;
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
    ELSIF char=EOF THEN Fault(ers.ill_comment,'%d',comment); RETURN
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
    IF (char=EOL) OR (i+4>=HIGH(sVal)) THEN err(ers.ill_string) END;
    IF char=EOL THEN NewLine END;
    getchar;
    WHILE (char=' ') OR (char=EOL)  DO getchar END;
    WHILE (char>='0') & (char<='9') DO GetSy;
      IF sy#charval THEN err(ers.ill_string) END;
      IF i<HIGH(sVal) THEN sVal[i]:=CHAR(cVal); INC(i) END;
      IF char=EOL THEN NewLine END;
      WHILE (char=' ') OR (char=EOL) DO getchar END;
    END;
    IF (char#'"') & (char#"'") THEN EXIT END;
  END;
  sVal[i]:=0c;
  IF (char=EOL) OR (i+4>=HIGH(sVal)) THEN err(ers.ill_string) END;
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
  POOL    = POINTER TO ARRAY [0..4095] OF CHAR;

VAR
  id_pool : POOL;
  id_queue: inter.QUEUE;
  free    : INTEGER; (* позиция в строковом буфере *)
  ref     : DYNARR OF pString;
  KWids   : ARRAY Symbol OF pString;
  ids_busy: INTEGER; (*[0..noIdents]*)
  ids_lim : INTEGER; (* (noIdents+3) DIV 4 *)

PROCEDURE id_str(d: INTEGER; VAR s: ARRAY OF CHAR);
  VAR i: INTEGER; p: pString;
BEGIN
  p:=ref[d];
  IF p=NIL THEN s:='$******$'; s[1]:=0c; RETURN END;
  IF INTEGER(p)<0 THEN p:=KWids[Symbol(ABS(INTEGER(p)))] END;
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
      IF mem_unit=4 THEN len:=INTEGER(BITSET(len+3)-{0..1}) END;
      IF free+len>=LEN(id_pool^) THEN
        inter.push(id_queue,id_pool);
        NEW(id_pool); free:=0;
      END;
      p:=sys.ADDRESS(id_pool) + (free DIV mem_unit);
      ref[sum]:=p; INC(free,len);
      low.move(p,sys.ADR(str),len DIV mem_unit);
      INC(ids_busy);
      IF ids_busy>ids_lim THEN
        Fault(ers.restriction,'too many identifiers')
      END;
      RETURN sum
    END;
    IF INTEGER(p)<0 THEN
      x:=ABS(INTEGER(p)); p:=KWids[Symbol(x)];
      IF  p^=str THEN sy:=Symbol(x); RETURN sum  END;
    ELSIF p^=str THEN                RETURN sum
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

PROCEDURE MakeKW(s: Symbol; VAL str: ARRAY OF CHAR);
  VAR d: INTEGER;
BEGIN d:=str_id(str);
  KWids[s]:=ref[d]; ref[d]:=pString(-INTEGER(s));
END MakeKW;

PROCEDURE vis_sym(sy: Symbol; VAR v: ARRAY OF CHAR);
  VAR i: INTEGER; p: pString;
BEGIN
  IF KWids[sy]#NIL THEN p:=KWids[sy]; i:=0;
    WHILE (i<HIGH(v)) & (p^[i]#0c) DO v[i]:=p^[i]; INC(i) END;
    v[i]:=0c;
    RETURN
  END;
  CASE sy OF
    |ident : v:="$IDENT$"
    |equ   : v:='=';
    |neq   : v:='#';
    |lss   : v:='<';
    |gtr   : v:='>';
    |leq   : v:='<=';
    |geq   : v:='>=';
    |times : v:='*';
    |slash : v:='/';
    |minus : v:='-';
    |plus  : v:='+';
    |rol   : v:='<<';
    |ror   : v:='>>';
    |semic : v:=';';
    |sep   : v:='|';
    |bar   : v:='^';
    |period: v:='.';
    |colon : v:=':';
    |lbr   : v:='[';
    |rbr   : v:=']';
    |lpar  : v:='(';
    |rpar  : v:=')';
    |lbrace: v:='{';
    |rbrace: v:='}';
    |coma  : v:=',';
    |range : v:='..';
    |becomes: v:=':=';
  ELSE inter.sprint(v,'unknown symbol #%d',sy);
  END
END vis_sym;

-------------------------  G E T S Y  --------------------------
                         -------------

VAR Str: String;
    sum: BITSET;
  len,i: INTEGER;

PROCEDURE GetChar(): CHAR;
  VAR c: CHAR;
BEGIN
  col:=col0-2; line:=line0;
  IF char=EOL THEN NewLine END;
  c:=char; getchar0;
  RETURN c;
END GetChar;

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
         char:=next;
         IF next#EOF THEN next:=source[col0]; INC(col0) END;
       UNTIL alpha?[ORD(char)]=0c;
       Str[len]:=0c;
       sum:=BITSET(sum)/BITSET(sum>>8)/BITSET(sum<<8);
       Id:=Hash(Str,len+1,INTEGER(sum));
       RETURN
    |EOF: Fault(ers.unexpc_eof,''); RETURN
    |EOL: NewLine;
    |' ':
    ELSE err(ers.ill_char)
    END;
    getchar0;
  END;
END GetSy;

------------------------  INIT & EXIT  -------------------------
                        ---------------

PROCEDURE MakeKW2(s0: Symbol; VAL str0: ARRAY OF CHAR;
                  s1: Symbol; VAL str1: ARRAY OF CHAR);
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
 MakeKW(packed,"PACKED");
END IniKWs;

PROCEDURE IniIds;
  VAR i: INTEGER; s: Symbol;
BEGIN
  FOR i:=0 TO HIGH(ref)   DO ref[i]:=NIL   END;
  FOR s:=MIN(Symbol) TO MAX(Symbol) DO KWids[s]:=NIL END;
  free:=0; Id:=DmId; ids_busy:=0; ids_lim:=(noIdents*6) DIV 7;
  i:=str_id('*1'); i:=str_id('*2');
  ref[DmId]:=sys.ADR(id_pool);
  IniKWs;
END IniIds;

PROCEDURE ini_scaner;
BEGIN
  occur:=FALSE; noErrors:=0; fault:=FALSE;
  IniIds;
  line0:=-1; col0:=0;
  line := 0; col :=0;
  next:=EOL; NewLine;
  getchar0; GetSy;
END ini_scaner;

PROCEDURE exi_scaner;
END exi_scaner;

---------------------------  ERRORS  ---------------------------
                           ----------

PROCEDURE message(no: ers.T; VAL f: ARRAY OF CHAR; SEQ args: sys.WORD);
  VAR s,msg: ARRAY [0..255] OF CHAR;
BEGIN
  inter.sprint(s,f,args);
  inter.err_msg(no,msg);
  inter.error(line0,col0-2,'%s %s',msg,s);
  INC(noErrors);
  IF ORD('F')-ORD('A') IN opts THEN HALT(1) END;
  occur:=TRUE;
END message;

PROCEDURE gen_error(ps: INTEGER; hlt: BOOLEAN;
                    VAL fmt: ARRAY OF CHAR; SEQ x: sys.WORD);
BEGIN
  inter.error(ps DIV 1000h,ps MOD 1000h,fmt,x);
  INC(noErrors);
  IF ORD('F')-ORD('A') IN opts THEN HALT(1) END;
  occur:=TRUE;
  IF hlt THEN inter.halt END;
END gen_error;

PROCEDURE Err0(n: ers.T; id: INTEGER);
  VAR v: ARRAY [0..255] OF CHAR;
BEGIN
  IF fault THEN RETURN END;
  IF id>0 THEN id_str(id,v); message(n,' -- "%s"',v);
  ELSE                       message(n,'');
  END;
END Err0;

PROCEDURE err(N: ers.T);
BEGIN
  IF NOT occur OR (ORD('A')-ORD('A') IN opts) THEN Err0(N,-1) END;
END err;

PROCEDURE err_id(N: ers.T; id: INTEGER);
BEGIN
  IF NOT occur OR (ORD('A')-ORD('A') IN opts) THEN Err0(N,id) END
END err_id;

PROCEDURE expc(sy: Symbol);
  VAR v: ARRAY [0..79] OF CHAR;
BEGIN
  IF fault THEN RETURN END;
  IF occur & NOT (ORD('A')-ORD('A') IN opts) THEN RETURN END;
  IF    sy=ident THEN message(ers.ident,'');
  ELSE vis_sym(sy,v); message(ers.symbol,' %s',v);
  END;
END expc;

PROCEDURE Err(n: ers.T; VAL format: ARRAY OF CHAR; SEQ args: sys.WORD);
BEGIN
  IF fault THEN RETURN END;
  message(n,format,args);
END Err;

PROCEDURE Fault(n: ers.T; VAL format: ARRAY OF CHAR; SEQ args: sys.WORD);
BEGIN
  IF fault THEN RETURN END;
  fault:=TRUE;
  message(n,format,args);
  inter.halt;
END Fault;

PROCEDURE io_fault(VAL format: ARRAY OF CHAR; SEQ args: sys.WORD);
BEGIN
  Fault(ers.blank,format,args);
END io_fault;

-----------------------  INITIALIZATION  -----------------------
                       ------------------

PROCEDURE Ini(txt: comp.io_ptr);
BEGIN
  pc.error:=gen_error;
  inter.fifo(id_queue);
  inter.lifo(optSTACK); base:=opts;
  text:=txt;
  text^.print:=io_fault;
  NEW(ref,noIdents);
  NEW(id_pool);
  ini_scaner;
END Ini;

PROCEDURE Exi;
  VAR p: POOL;
BEGIN
  exi_scaner;
  DISPOSE(ref);
  DISPOSE(id_pool);
  WHILE inter.pop(id_queue,p) DO DISPOSE(p) END;
  inter.clear(id_queue);
  inter.clear(optSTACK);
END Exi;

BEGIN
  line:=0; fault:=FALSE; add_stk:=0;
  text:=NIL;
  opts:={};
  base:=opts;
  ASSERT(mem_unit IN {1,4});
END mcScan.
