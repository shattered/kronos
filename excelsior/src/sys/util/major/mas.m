MODULE mas; (* Ned 24-Feb-89. (c) KRONOS *)

(* New version of KRONOS micro assembler *)

IMPORT       SYSTEM;
IMPORT       ASCII;
IMPORT  tty: Terminal;
IMPORT  str: Strings;
IMPORT  bio: BIO;
IMPORT  mem: Heap;
IMPORT  arg: tskArgs;

WITH STORAGE: mem;

----------------------------------------------------------------

CONST max_bit = 63;

---------------------------  parms  ---------------------------
                           ---------
VAR
  parms: ARRAY [0..255] OF CHAR;
  p_pos: INTEGER;

PROCEDURE next_name(VAR s: ARRAY OF CHAR): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  IF parms[p_pos]=0c THEN RETURN TRUE END;
  i:=0;
  WHILE parms[p_pos]#ASCII.NL DO
    s[i]:=parms[p_pos]; INC(i); INC(p_pos);
  END;
  s[i]:=0c; INC(p_pos);
  RETURN FALSE
END next_name;

---------------------------  input  ---------------------------
                           ---------

VAR
   source: STRING;
  sou_pos: INTEGER;
     line: INTEGER;
    start: INTEGER;     -- sou_pos of 0 byte of current line

PROCEDURE read_file;
  VAR f: bio.FILE;
   name: ARRAY [0..79] OF CHAR;
BEGIN
  IF next_name(name) THEN
    tty.print('*** unexpected end of text\n'); HALT(1)
  END;
  bio.open(f,name,'r');
  IF NOT bio.done THEN
    tty.perror(bio.error,'"%s" %%s\n',name); HALT(bio.error)
  END;
  RESIZE(source,bio.eof(f));
  bio.get(f,source,BYTES(source));
  bio.close(f);
  sou_pos:=0; line:=0; start:=0;
  tty.print('%-16.16s %d\n',name,BYTES(source));
END read_file;

---------------------------  output  --------------------------
                           ----------

PROCEDURE write_rom(VAL name: ARRAY OF CHAR;
                    VAL cmds: ARRAY OF SYSTEM.WORD;
                       bytes: INTEGER);
  VAR f: bio.FILE;
BEGIN
  tty.print('rom: %s\n',name);
  bio.create(f,name,'w',bytes);
  IF NOT bio.done THEN
    tty.perror(bio.error,'"%s" %%s\n',name); HALT(bio.error)
  END;
  bio.put(f,cmds,bytes);
  bio.close(f);
  IF NOT bio.done THEN
    tty.perror(bio.error,'"%s" %%s\n',name); HALT(bio.error)
  END;
END write_rom;

-------------------------  listing  ----------------------------
                         -----------

VAR list: STRING; list_pos: INTEGER;


PROCEDURE listing(VAL name: ARRAY OF CHAR; adr: INTEGER);
  VAR i: INTEGER;
     s: ARRAY [0..63] OF CHAR;
BEGIN
  str.print(s,"%-24.22s%04h  %05b\n",name,adr,adr);
  i:=0;
  WHILE (i<HIGH(s)) & (s[i]#0c) DO
    IF list_pos>HIGH(list) THEN RESIZE(list,BYTES(list)*2) END;
    list[list_pos]:=s[i];
    i:=i+1; list_pos:=list_pos+1
  END;
END listing;

PROCEDURE write_listing(VAL name: ARRAY OF CHAR);
  VAR f: bio.FILE;
BEGIN
  tty.print('list: %s\n',name);
  bio.create(f,name,'w',list_pos);
  IF NOT bio.done THEN
    tty.perror(bio.error,'"%s" %%s\n',name); HALT(bio.error)
  END;
  bio.put(f,list,list_pos);
  bio.close(f);
  IF NOT bio.done THEN
    tty.perror(bio.error,'"%s" %%s\n',name); HALT(bio.error)
  END;
  DISPOSE(list)
END write_listing;

---------------------------  trees  ---------------------------
                           ---------

TYPE
  name_str = ARRAY [0..31] OF CHAR;
  tree_ptr = POINTER TO tree_rec;
  tree_rec = RECORD
               name: name_str;
               l,r : tree_ptr;
               val : SYSTEM.ADDRESS;
             END;

PROCEDURE new(VAR x: tree_ptr; VAL name: ARRAY OF CHAR);
BEGIN
  NEW(x);
  str.print(x^.name,'%s',name);
  x^.l:=NIL;    x^.r:=NIL;
  x^.val:=NIL;
END new;

PROCEDURE append(VAR root: tree_ptr;
                 VAL name: ARRAY OF CHAR;
                 VAR x   : tree_ptr;
                 ): BOOLEAN;
BEGIN
  IF root=NIL THEN new(root,name); x:=root; RETURN TRUE END;
  x:=root;
  LOOP
    IF name>x^.name THEN
      IF x^.r=NIL THEN new(x^.r,name); x:=x^.r; RETURN TRUE
      ELSE x:=x^.r;
      END;
    ELSIF name=x^.name THEN RETURN FALSE
    ELSE
      IF x^.l=NIL THEN new(x^.l,name); x:=x^.l; RETURN TRUE
      ELSE x:=x^.l;
      END;
    END;
  END;
END append;

PROCEDURE find(VAR root: tree_ptr; VAL name: ARRAY OF CHAR): BOOLEAN;
BEGIN
  LOOP
    IF root=NIL THEN RETURN FALSE END;
    IF root^.name=name THEN RETURN TRUE END;
    IF name>root^.name THEN root:=root^.r ELSE root:=root^.l END;
  END;
END find;

---------------------------  scaner  --------------------------
                           ----------

CONST -- symbols
  inv = 2c;  num = 1c;  id  = 0c;

CONST class = ARRAY OF CHAR {
-------  00  01  02  03  04  05  06  07  08  09  0A  0B  0C  0D  0E  0F
(*00h*) inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,
(*10h*) inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,
(*20h*) inv, id, id, id,inv,inv, id, id, id, id, id, id, id, id, id, id,
(*30h*)  id, id, id, id, id, id, id, id, id, id,inv, id, id,inv, id, id,
(*40h*)  id, id, id, id, id, id, id, id, id, id, id, id, id, id, id, id,
(*50h*)  id, id, id, id, id, id, id, id, id, id, id,inv,inv,inv,inv, id,
(*60h*)  id, id, id, id, id, id, id, id, id, id, id, id, id, id, id, id,
(*70h*)  id, id, id, id, id, id, id, id, id, id, id,inv,inv,inv,inv,inv,
(*80h*) inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,
(*90h*) inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,
(*A0h*) inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,
(*B0h*) inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,inv,
(*C0h*)  id, id, id, id, id, id, id, id, id, id, id, id, id, id, id, id,
(*D0h*)  id, id, id, id, id, id, id, id, id, id, id, id, id, id, id, id,
(*E0h*)  id, id, id, id, id, id, id, id, id, id, id, id, id, id, id, id,
(*F0h*)  id, id, id, id, id, id, id, id, id, id, id, id, id, id, id, id};

VAR sy: CHAR;
   cch: CHAR;
  name: name_str;
   pos: INTEGER;
 value: INTEGER;

PROCEDURE error(VAL msg: ARRAY OF CHAR; SEQ args: SYSTEM.WORD); FORWARD;

PROCEDURE get_char;
BEGIN
  IF sou_pos>HIGH(source) THEN read_file END;
  cch:=source[sou_pos]; INC(sou_pos);
END get_char;

PROCEDURE number;
  VAR n,i,x: INTEGER; dec: BOOLEAN;
    s: ARRAY [0..15] OF INTEGER;
BEGIN n:=0;
  LOOP
    IF    ORD(cch)-ORD('0') IN {0..9} THEN s[n]:=ORD(cch)-ORD('0')
    ELSIF ORD(cch)-ORD('A') IN {0..5} THEN s[n]:=ORD(cch)-ORD('A')+10
    ELSE dec:=(cch='.');
      IF dec THEN get_char END;
      EXIT
    END;
    INC(n); get_char
  END;
  IF dec THEN i:=0; x:=0;
    WHILE i<n DO x:=x*10+s[i]; INC(i) END;
    value:=x;
  ELSE i:=0; x:=0;
    WHILE (i<n) & (s[i]=0) DO INC(i) END;
    ASSERT(n-i<=8,41h);
    WHILE i<n DO x:=x<<4+s[i]; INC(i) END;
    value:=x;
  END;
END number;

PROCEDURE get_sy;
BEGIN
  LOOP
    CASE cch OF
      |'a'..'z','A'..'Z','(',')','<','>':
        pos:=0;
        REPEAT name[pos]:=cch; INC(pos); get_char; UNTIL class[ORD(cch)]#id;
        name[pos]:=0c; sy:=id;  RETURN
      |'0'..'9': number; sy:=num; RETURN
      |'@','$','*','=','{','}','[',']',':','^': sy:=cch; get_char; RETURN
      |' ':
      |'%': REPEAT get_char UNTIL cch=ASCII.NL;
      |ASCII.NL: INC(line); start:=sou_pos;
    ELSE error('unknown symbol: %h',cch); HALT(1)
    END;
    get_char
  END;
END get_sy;

---------------------------  errors  --------------------------
                           ----------

PROCEDURE error(VAL msg: ARRAY OF CHAR; SEQ args: SYSTEM.WORD);
  VAR s: ARRAY [0..255] OF CHAR;
    i,n: INTEGER;
BEGIN
  tty.print(msg,args); tty.print('\n');
  i:=0; n:=start;
  WHILE n<sou_pos DO s[i]:=source[n]; INC(i); INC(n) END;
  s[i]:='$'; INC(i);
  WHILE (n<=HIGH(source)) & (source[n]#ASCII.NL) DO
    s[i]:=source[n]; INC(i); INC(n)
  END; s[i]:=0c;
  tty.print('%3d: %s\n',line,s);
END error;

PROCEDURE err_id(VAL msg,name: ARRAY OF CHAR);
BEGIN error('%s -- %s\n',msg,name);
END err_id;

PROCEDURE expc(c: CHAR); BEGIN error('expected "%c"',c) END expc;

PROCEDURE expc_id;  BEGIN error('expected ident')  END expc_id;
PROCEDURE expc_num; BEGIN error('expected number') END expc_num;

--------------------------  objects  --------------------------
                          -----------

TYPE
  OBJECT      = SYSTEM.ADDRESS;

  bit_ptr     = POINTER TO bit_rec;
  bit_rec     = RECORD
                  bit : INTEGER;
                  inv : BOOLEAN;
                  next: bit_ptr;
                END;

  field_ptr   = POINTER TO field_rec;
  field_rec   = RECORD
                  bits   : bit_ptr;
                  def    : INTEGER;
                  locals : tree_ptr;
                END;

VAR
  fields: tree_ptr;
  consts: tree_ptr;

PROCEDURE declare(VAR t: tree_ptr; o: OBJECT);
  VAR x: tree_ptr;
BEGIN
  IF append(t,name,x) THEN x^.val:=o;
  ELSE err_id('double declaration',name); HALT
  END;
END declare;

PROCEDURE vis?(t: tree_ptr; VAR o: OBJECT): BOOLEAN;
BEGIN
  IF find(t,name) THEN o:=t^.val; RETURN TRUE END;
  RETURN FALSE
END vis?;

PROCEDURE vis(t: tree_ptr; VAR o: OBJECT);
BEGIN
  IF find(t,name) THEN o:=t^.val
  ELSE err_id('invisible object -- ',name); HALT
  END;
END vis;

PROCEDURE get_value(f: field_ptr; VAR c: INTEGER);
BEGIN
  IF sy=id THEN
    IF NOT vis?(f^.locals,c) THEN vis(consts,c) END;
  ELSIF sy=num THEN c:=value
  ELSE error('expected value'); HALT
  END;
END get_value;

------------------------  declarations  -----------------------
                        ----------------

VAR trans: ARRAY [0..255] OF INTEGER;

PROCEDURE locals(VAR root: tree_ptr);
BEGIN get_sy;
  WHILE sy#'}' DO
    IF sy#id  THEN expc_id; HALT END;
    get_sy;
    IF sy#num THEN expc_num; HALT END;
    declare(root,value); get_sy
  END;
  get_sy;
END locals;

PROCEDURE new_field(VAR f: field_ptr);
BEGIN
  NEW(f);
  declare(fields,f);
  f^.locals:=NIL; f^.bits:=NIL; f^.def:=0;
END new_field;

PROCEDURE field_dcl;
  VAR f: field_ptr; b: bit_ptr; n: INTEGER; inv: BOOLEAN;
BEGIN get_sy;
  IF sy#id THEN expc_id; HALT END;
  new_field(f); get_sy;
  LOOP
    inv:=(sy='^');
    IF inv THEN get_sy;
       IF sy#num THEN expc_num; HALT END;
    ELSIF sy#num THEN EXIT
    END;
    n:=trans[value];
    IF n>max_bit THEN error('illegal bit number'); HALT END;
    NEW(b);
    b^.bit:=n; b^.inv:=inv; b^.next:=f^.bits; f^.bits:=b;
    get_sy;
  END;
  IF sy='{' THEN locals(f^.locals) END;
  IF sy='=' THEN get_sy;
    get_value(f,f^.def); get_sy;
  END;
END field_dcl;

PROCEDURE generic_dcl;
  VAR g,f: field_ptr;
BEGIN get_sy;
  IF sy#id THEN expc_id; HALT END;
  new_field(g); get_sy;
  IF sy#id THEN expc_id; HALT END;
  vis(fields,f);
  g^:=f^;
  get_sy;
  IF sy#'=' THEN expc('='); HALT END;
  get_sy;
  get_value(g,g^.def); get_sy;
END generic_dcl;

PROCEDURE declaration;
  CONST msg = 'expected FIELD or GENERIC';
BEGIN
  IF sy#id THEN error(msg); HALT END;
  IF    name='FIELD'   THEN field_dcl
  ELSIF name='GENERIC' THEN generic_dcl
  ELSE error(msg); HALT
  END;
END declaration;

PROCEDURE translate;
  VAR x: ARRAY [0..max_bit] OF INTEGER;
    i,n: INTEGER;
BEGIN
  IF (sy#id) OR (name#'TRANSLATE') THEN RETURN END;
  get_sy; i:=0;
  WHILE sy=num DO x[i]:=value; INC(i); get_sy END;
  n:=0; DEC(i);
  WHILE i>0 DO trans[x[i]]:=n; INC(n); DEC(i) END;
END translate;

--------------------------  commands  -------------------------
                          ------------

TYPE COMMAND = ARRAY [0..max_bit] OF BOOLEAN;

VAR
  def  : COMMAND;
  free : COMMAND;
  cmd  : COMMAND;
  busy : COMMAND;

PROCEDURE app_value(l: bit_ptr; x: INTEGER);
  VAR b: BOOLEAN;
BEGIN
  WHILE l#NIL DO
    IF l^.inv THEN b:=NOT ODD(x) ELSE b:=ODD(x) END;
    IF (busy[l^.bit]=TRUE) & (b#cmd[l^.bit]) THEN
      error('field conflict in bit %d',l^.bit); HALT
    END;
    cmd[l^.bit]:=b; busy[l^.bit]:=TRUE;
    x:=x DIV 2; l:=l^.next;
  END;
END app_value;

PROCEDURE assign;
  VAR f: field_ptr; val: INTEGER;
BEGIN
  IF sy#id THEN expc_id; HALT END;
  vis(fields,f); get_sy;
  IF sy#'=' THEN app_value(f^.bits,f^.def);
  ELSE get_sy;
    get_value(f,val); app_value(f^.bits,val);
    get_sy;
  END;
END assign;

PROCEDURE command;
  VAR i: INTEGER;
BEGIN
  cmd:=def; busy:=free;
  WHILE sy=':' DO get_sy; get_sy END;
  IF sy#'[' THEN expc('['); HALT END;
  get_sy;
  WHILE sy#']' DO assign END;
END command;

----------------------  packed commands  ----------------------
                      -------------------

TYPE PACKED  = ARRAY [0..1] OF INTEGER;

VAR
  parity    : BOOLEAN;
  parity_bit: INTEGER;
  dummy     : PACKED;

PROCEDURE pack_cmd(VAR p: PACKED);
  VAR x,i,sum: INTEGER;
BEGIN
  IF parity THEN sum:=0;
    IF busy[parity_bit] THEN error('not zero parity bit') END;
    FOR i:=0 TO HIGH(cmd) DO INC(sum,ORD(cmd[i])) END;
    cmd[parity_bit]:=NOT ODD(sum);
  END;
  x:=0;
  FOR i:=31 TO 0 BY -1 DO x:=x<<1+ORD(cmd[i]) END;
  p[0]:=x; x:=0;
  FOR i:=max_bit TO 32 BY -1 DO x:=x<<1+ORD(cmd[i]) END;
  p[1]:=x;
IF arg.flag('-','v') THEN
  tty.print('*** %{} %{} ***\n',p[0],p[1]);
END;
END pack_cmd;

--------------------------  options  --------------------------
                          -----------

PROCEDURE default;
BEGIN get_sy;
  IF sy#'[' THEN expc('['); HALT END;
  command; def:=cmd;
  get_sy;
END default;

PROCEDURE set_parity_bit;
BEGIN get_sy;
  IF sy#num THEN expc_num; HALT END;
  parity:=TRUE; parity_bit:=value;
  IF value>max_bit THEN error('illegal value of parity bit'); HALT END;
  get_sy;
END set_parity_bit;

PROCEDURE set_dummy_command;
  VAR i,x: INTEGER;
BEGIN
  IF (sy=id) & (name='DUMMY') THEN
    get_sy;
    IF sy#num THEN expc_num; HALT END;
    dummy[0]:=value; get_sy;
    IF sy#num THEN expc_num; HALT END;
    dummy[1]:=value; get_sy;
  END;
  IF parity THEN
    x:=dummy[0];
    FOR i:=0 TO 31 DO cmd[i]:=ODD(x); x:=x>>1 END;
    x:=dummy[1];
    FOR i:=32 TO max_bit DO cmd[i]:=ODD(x); x:=x>>1 END;
    cmd[parity_bit]:=FALSE;
    pack_cmd(dummy);
  END;
END set_dummy_command;

PROCEDURE options_1;
BEGIN
  LOOP
    IF    sy#id          THEN EXIT
    ELSIF name='DEFAULT' THEN
      REPEAT get_sy UNTIL sy=']'; get_sy;
    ELSIF name='PARITY'  THEN get_sy; get_sy;
    ELSIF name='DUMMY'   THEN get_sy; get_sy; get_sy;
    ELSE EXIT
    END;
  END;
END options_1;

PROCEDURE options_2;
BEGIN
  LOOP
    IF    sy#id          THEN EXIT
    ELSIF name='DEFAULT' THEN default
    ELSIF name='PARITY'  THEN set_parity_bit
    ELSE EXIT
    END;
  END;
  set_dummy_command;
END options_2;

---------------------------  parser  --------------------------
                           ----------
PROCEDURE pass1;
  VAR adr: INTEGER;
BEGIN
  WHILE sy#'$' DO get_sy END;
  get_sy;
  options_1;
  adr:=0;
  list_pos:=0;
  LOOP
    IF sy='$' THEN RETURN
    ELSIF sy=':' THEN get_sy;
      IF sy#id THEN expc_id; HALT END;
      listing(name,adr);
      declare(consts,adr);
      get_sy;
    ELSE INC(adr);
      IF sy#'[' THEN expc('['); HALT END;
      WHILE sy#']' DO get_sy END;
      get_sy;
    END;
  END;
END pass1;

VAR
  commands: ARRAY [0..1024*4] OF PACKED;
  cp      : INTEGER;

PROCEDURE pass2;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(trans) DO trans[i]:=i END;
  translate;
  WHILE sy#'$' DO declaration END;
  get_sy;
  FOR i:=0 TO HIGH(free) DO free[i]:=FALSE END;
  def:=free; dummy[0]:=0FFFFFFFFh; dummy[1]:=0FFFFFFFFh;
  options_2; cp:=0;
  WHILE sy#'$' DO
    cmd:=def; busy:=free;
    WHILE sy=':' DO get_sy; get_sy END;
    IF sy#'[' THEN expc('['); HALT END;
    get_sy;
    IF sy='*' THEN get_sy;
      IF sy#']' THEN expc(']'); HALT END;
      commands[cp]:=dummy;
    ELSE
      WHILE sy#']' DO assign END;
      pack_cmd(commands[cp]);
    END;
    get_sy; INC(cp);
  END;
END pass2;

----------------------------------------------------------------

PROCEDURE change_ext(VAR name: ARRAY OF CHAR; VAL ext: ARRAY OF CHAR);
  VAR i,p: INTEGER;
BEGIN i:=0; p:=-1;
  WHILE (i<=HIGH(name)) & (name[i]#0c) DO
    IF name[i]='.' THEN p:=i END;
    INC(i);
  END;
  IF p<0 THEN name[i]:='.'; p:=i END;
  INC(p); i:=0;
  WHILE (i<=HIGH(ext)) & (ext[i]#0c) DO
    name[p]:=ext[i]; INC(p); INC(i);
  END;
  name[p]:=0c;
END change_ext;

----------------------------------------------------------------

VAR i: INTEGER;
 main: STRING;

BEGIN
  IF HIGH(arg.words)<=0 THEN HALT END;
  bio.check_io(TRUE);
  fields:=NIL;
  consts:=NIL;
  NEW(list,4096);
  NEW(source);
  RESIZE(main,BYTES(arg.words[0]));
  str.copy(main,arg.words[0]);
  str.print(parms,'%s%c',main,ASCII.NL);
  FOR i:=1 TO HIGH(arg.words) DO
    str.append(parms,'%s%c',arg.words[i],ASCII.NL)
  END;

tty.print('-- pass 1\n');
  p_pos:=0; read_file; get_char; get_sy; pass1;
  change_ext(main,'lst');
  write_listing(main);

tty.print('-- pass 2\n');
  p_pos:=0; read_file; get_char; get_sy; pass2;

  change_ext(main,'rom');
  write_rom(main,commands,cp*BYTES(PACKED));
END mas.

----------------------------------------------------------------

                            mas NEWS

1. Поле не более 32 битов. Все числа тоже.
2. Добавлены новые опции:
     -- PARITY - определяет бит четности
     -- DUMMY  - определяет  заполнение пустой команды.
                 пустая команда обозначается [*].
                 Заметим, что команда [ ] не пустая,
                 а она заполнена значением по умолчанию.
3. Часть возможностей старого mas'а временно не реализована,
   например:
     -- запись карты;
     -- двоичные числа;
     -- ????
   Кому надо пусть сделает, или скажет мне.
                                                Ned.

                           GOOD LUCK!

----------------------------------------------------------------

 1. PROGRAM ::
                [ Translation ] { Declaration } "$"
                Options { { Label } Command } "$".
 2. Translation ::
                "TRANSLATE" { number }.
 3. Declaration ::
                FieldDcl | GenericDcl.
 4. FieldDcl ::
                "FIELD" ident Bits [ Locals ] [ "=" Value ].
 5. Bits ::
                { [ "^" ] Number }.
 6. Locals ::
                "{" { LocalDcl } "}"
 7. LocalDcl ::
                ident Value.
 8. GenericDcl ::
                "GENERIC" ident ident "=" Value.
 9. Options ::
                [ "DEFAULT" Command ] [ "PARITY" number ]
                [ "DUMMY" number number ].
10. Label ::
                ":" ident.
11. Command ::
                "[" { ident [ "=" Value ] } "]".
12. Value ::
                Number | ident.
13. Number ::
                Hexidecimal | Decimal.
14. Hexidecimal ::
                digit { digit16 }.
15. Decimal ::
                digit { digit } ".".
16. ident ::
                letter | { char-{"$","=","{","}","[","]",":" }
