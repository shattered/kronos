MODULE mshell; (* Hady. 14-Sep-90. (c) KRONOS *)

IMPORT  shell: Shell,
          exe: Execute,
          env: tskEnv,
          sys: SYSTEM,
           os: osKernel,
          tty: Terminal,
          key: Keyboard,
          str: Strings,
          arg: tskArgs,
          sle: strEditor,
          sci: ASCII,
          mem: Heap,
          bio: BIO;


WITH STORAGE  (NEW: mem.allocate;
           DISPOSE: mem.deallocate;
            RESIZE: mem.reallocate);


CONST DEBUG = FALSE;

CONST NULL = CHAR(-1);

---------------------------- LIBRARY ---------------------------
                            ---------

PROCEDURE comp(VAL patt: ARRAY OF CHAR;
               VAL sour: ARRAY OF CHAR;
                  sf,st: INTEGER): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  IF sf>st THEN RETURN FALSE END;
  i:=0;
  WHILE (i<=HIGH(patt)) & (patt[i]#0c) & (sf<=st) DO
    IF sour[sf]#patt[i] THEN RETURN FALSE END;
    sf:=sf+1; i:=i+1
  END;
  RETURN (sf>st) & ((i>HIGH(patt)) OR (patt[i]=0c))
END comp;

PROCEDURE skip(VAL s: ARRAY OF CHAR; VAR p: INTEGER);
BEGIN
  WHILE (p<=HIGH(s)) & (s[p]=" ") DO p:=p+1 END
END skip;

PROCEDURE skipn(VAL s: ARRAY OF CHAR; VAR p: INTEGER);
BEGIN
  WHILE (p<=HIGH(s)) & (s[p]#" ") & (s[p]#0c) DO
    p:=p+1
  END
END skipn;


TYPE str256 = ARRAY [0..255] OF CHAR;

VAR desc: sle.descriptor;
    bump: str256;

VAR echo: ARRAY [0..7] OF CHAR;

PROCEDURE in_echo(ch: CHAR): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  i:=0;
  WHILE (i<=HIGH(echo)) & (echo[i]#0c) & (echo[i]#ch) DO INC(i) END;
  RETURN (i<=HIGH(echo)) & (echo[i]=ch)
END in_echo;

---------------------------- ERRORS ----------------------------
                            --------

VAR abort: BOOLEAN;

PROCEDURE check_bio(VAL fmt: ARRAY OF CHAR;
                    SEQ arg: sys.WORD): BOOLEAN;
BEGIN
  IF bio.done THEN RETURN FALSE END;
  tty.print(fmt,arg); tty.perror(bio.error," %%s\n");
  RETURN TRUE
END check_bio;

PROCEDURE chk_bio(VAL fmt: ARRAY OF CHAR;
                  SEQ arg: sys.WORD): BOOLEAN;
BEGIN
  abort:=check_bio(fmt,arg); RETURN abort
END chk_bio;

PROCEDURE check_mem(): BOOLEAN;
BEGIN
  IF mem.done THEN RETURN FALSE END;
  tty.perror(mem.error," %%s\n");
  RETURN TRUE
END check_mem;

PROCEDURE chk_mem(): BOOLEAN;
BEGIN
  abort:=check_mem(); RETURN abort
END chk_mem;

PROCEDURE copy(VAR dest: STRING;
               VAL body: ARRAY OF CHAR;
                    len: INTEGER);
  VAR i: INTEGER;
BEGIN
  IF len<0 THEN len:=str.len(body) END;
  NEW(dest,len+1);
  IF chk_mem() THEN RETURN END;
  FOR i:=0 TO len-1 DO dest[i]:=body[i] END;
  dest[len]:=0c
END copy;

----------------------------- I/O ------------------------------
                             -----

VAR  inp: bio.FILE;
     eof: INTEGER;
     pos: INTEGER;
    fpos: INTEGER;
    lpos: INTEGER;
    line: INTEGER;

PROCEDURE init_io(VAL name: ARRAY OF CHAR): BOOLEAN;
  VAR self: os.task_ptr;
      save: INTEGER;
BEGIN
  bio.lookup(exe.etc,inp,name,"x");
  IF chk_bio('lookup("%s")',name) THEN RETURN TRUE END;
  self:=os.self();
  save:=self^.user;
  self^.user:=INTEGER(BITSET(save)+{7});
    bio.chmode(inp,"r");
  self^.user:=save;
  IF chk_bio('chmode("%s")',name) THEN RETURN TRUE END;
  eof:=bio.eof(inp); bump:="";
  pos:=0 ; fpos:=0;
  lpos:=0; line:=0;
  RETURN FALSE
END init_io;

PROCEDURE get_str(): BOOLEAN;
  VAR ch: CHAR;

  PROCEDURE put(ch: CHAR): BOOLEAN;
  BEGIN
    IF pos>HIGH(bump) THEN
      tty.print("\n#error in line %d\n",line);
      tty.print("too long string in file\n");
      RETURN TRUE
   END;
   bump[pos]:=ch; INC(pos);
    RETURN FALSE
  END put;

BEGIN
  fpos:=bio.pos(inp);
  IF fpos>=eof THEN RETURN FALSE END;
  pos:=0;
  REPEAT bio.getch(inp,ch);
  UNTIL chk_bio('get_ch("%s")',bio.ename)
     OR (bio.pos(inp)>=eof)
     OR (ch=sci.NL) OR (ch=sci.LF) OR put(ch);
  IF abort THEN RETURN FALSE END;
  REPEAT DEC(pos) UNTIL (pos<0) OR (bump[pos]#" "); INC(pos);
  bump[pos]:=0c; INC(line);
  pos:=0; RETURN TRUE
END get_str;

CONST (* symbols *)
  _if     = 00;       _then   = 01;
  _else   = 02;       _end    = 03;
  _while  = 04;       _do     = 05;
  _repeat = 06;       _until  = 07;
  _loop   = 08;       _exit   = 09;
  _halt   = 10;       _eof    = 11;
  _equ    = 12;       _neq    = 13;
  _gtr    = 14;       _geq    = 15;
  _lss    = 16;       _leq    = 17;
  _obra   = 18;       _cbra   = 19;
  _num    = 20;       _str    = 21;
  _exe    = 22;       _query  = 23;

  _compare = 32;

  _ops = { _gtr, _lss, _equ, _neq, _geq, _leq };

TYPE blk_ptr = POINTER TO blk_rec;
     blk_rec = RECORD
                  fpos: INTEGER;  -- позиция в файле
                  lpos: INTEGER;  -- позиция в строке
                  lino: INTEGER;  -- номер строки
                active: BOOLEAN;  -- исполнять ли операторы
                   res: INTEGER;  -- result of last operation
                  next: blk_ptr;  -- stack
                   sep: CHAR;     -- separator for _exe block
                  type: INTEGER;  -- что за блок

                  bool: BOOLEAN;  -- for "if","while" blocks
                    op: INTEGER;  -- for "compare" block
                   sub: INTEGER;  -- substate of "compare"
               END;

VAR blk: blk_ptr;

VAR sym: INTEGER;
    num: INTEGER;
    val: str256;   -- necessary HIGH(val)>=HIGH(bump) !!!

PROCEDURE vis_sym(sym: INTEGER);
BEGIN
  CASE sym OF
    |_if     : tty.print("IF ")
    |_else   : tty.print("ELSE ")
    |_while  : tty.print("WHILE ")
    |_repeat : tty.print("REPEAT ")
    |_loop   : tty.print("LOOP ")
    |_halt   : tty.print("HALT ")
    |_equ    : tty.print("= ")
    |_gtr    : tty.print("> ")
    |_lss    : tty.print("< ")
    |_obra   : tty.print("( ")
    |_num    : tty.print("%d ",num)
    |_exe    : tty.print("shell command ")
    |_then   : tty.print("THEN ")
    |_end    : tty.print("END ")
    |_do     : tty.print("DO ")
    |_until  : tty.print("UNTIL ")
    |_exit   : tty.print("EXIT ")
    |_eof    : tty.print("END OF FILE")
    |_neq    : tty.print("# ")
    |_geq    : tty.print(">= ")
    |_leq    : tty.print("<= ")
    |_cbra   : tty.print(") ")
    |_str    : tty.print('"%s"',val)
    |_compare: tty.print('boolean expression')
    |_query  : tty.print('$* ')
  ELSE
    tty.print('U N K N O W N')
  END
END vis_sym;

PROCEDURE error(VAL fmt: ARRAY OF CHAR; SEQ arg: sys.WORD);
BEGIN
  tty.print('\n#error at %d.%d\n',line,lpos);
  tty.print(fmt,arg); tty.print('\n');
  abort:=TRUE
END error;

PROCEDURE unexp(sy: INTEGER);
BEGIN
  tty.print('\n#error at %d.%d\n',line,lpos);
  tty.print('#unexpected symbol: ');
  vis_sym(sy); tty.print('\n');
  abort:=TRUE
END unexp;

PROCEDURE expect(sy: INTEGER): BOOLEAN;
BEGIN
  IF sy#sym THEN
    tty.print('\n#error at %d.%d\n',line,lpos);
    tty.print('#symbol "'); vis_sym(sy);
    tty.print('" was expected\n');
    abort:=TRUE
  END;
  RETURN abort
END expect;

PROCEDURE read_string(VAL pmt: ARRAY OF CHAR;
                      VAR val: ARRAY OF CHAR);
BEGIN
  IF blk^.active THEN
    sle.read_str(pmt,val,desc); tty.print('\n')
  ELSE val:=""
  END
END read_string;

PROCEDURE read_number;
  VAR done: BOOLEAN;
       val: ARRAY [0..31] OF CHAR;
         i: INTEGER;
BEGIN
  IF blk^.active THEN
    REPEAT
      sle.read_str("# ",val,desc); i:=0;
      str.iscan(num,val,i,done);
      IF NOT done THEN tty.print('-- illegal number') END;
      tty.print('\n')
    UNTIL done
  ELSE num:=0
  END
END read_number;

PROCEDURE read_query;
  VAR c: CHAR;
BEGIN
  IF NOT blk^.active THEN num:=ORD(FALSE); RETURN END;
  tty.print('[y/n]?');
  LOOP
    key.read(c); c:=CAP(c);
    IF (c="Y") OR (c="N") THEN
      tty.print('%c\n',c); EXIT
    END;
    key.bell(1)
  END;
  num:=ORD(c="Y")
END read_query;

PROCEDURE get_string;

  VAR sep: CHAR;
      f,i: INTEGER;

BEGIN
  sep:=bump[pos];
  f:=pos+1; i:=0;
  WHILE (f<=HIGH(bump))
      & (bump[f]#sep)
      & (bump[f]#0c ) DO
    ASSERT(i<=HIGH(val)); -- because val body can be got from buff !
    val[i]:=bump[f]; INC(i); INC(f)
  END;
  IF (f>HIGH(bump)) OR (bump[f]#sep) THEN
    error("#close delimiter of string missed");
    RETURN
  END;
  IF i<=HIGH(val) THEN val[i]:=0c END;
  pos:=f+1; sym:=_str
END get_string;

PROCEDURE get_env;
  VAR f,i,no: INTEGER;
           s: STRING;
BEGIN
  f:=pos+1; i:=0;
  WHILE (f<=HIGH(bump))
      & (bump[f]#" ")  & (bump[f]#0c) DO
    ASSERT(i<=HIGH(val)); -- because val body can be got from buff !
    val[i]:=bump[f]; INC(i); INC(f)
  END;
  IF i<=HIGH(val) THEN val[i]:=0c END;
  IF (f<=HIGH(bump)) & (bump[i]#0c) THEN
       pos:=f+1
  ELSE pos:=f
  END;
  IF    val="#"0c THEN read_number;            sym:=_num
  ELSIF val="?"0c THEN read_string("? ", val); sym:=_str
  ELSIF val="*"0c THEN read_query;             sym:=_query
  ELSE
    IF (val[1]=0c) & (val[0]>="1") & (val[0]<="9") THEN
      no:=ORD(val[0])-ORD("0");
      IF no<=HIGH(arg.words) THEN
        IF HIGH(arg.words[no])>HIGH(val) THEN
          error('#too long argument "$%d"',no);
          val:=""; sym:=_str; RETURN
        END;
        str.copy(val,arg.words[no]);
        sym:=_str; RETURN
      END
    END;
    env.get_str(val,s);
    IF env.done THEN
      IF HIGH(s)>HIGH(val) THEN val:="";
        error('#too long string Environment."%s"',val)
      ELSE str.copy(val,s)
      END
    ELSE val:=""
    END;
    sym:=_str
  END
END get_env;

PROCEDURE exe_command;
  VAR f,i,no: INTEGER;
      ch,sep: CHAR;
         inp: ARRAY [0..79] OF CHAR;

  PROCEDURE append(VAL sour: ARRAY OF CHAR);
    VAR j: INTEGER;
  BEGIN
    j:=0;
    WHILE (j<=HIGH(sour)) & (sour[j]#0c) DO
      ASSERT(i<=HIGH(val));
      val[i]:=sour[j]; INC(j); INC(i)
    END
  END append;

BEGIN
  i:=0; f:=pos;
  sep:=blk^.sep;
  WHILE (f<=HIGH(bump))
      & (bump[f]#0c) & (bump[f]#";") & (bump[f]#sep) DO
    ASSERT(i<=HIGH(val));
    ch:=bump[f];
    IF (ch="\") & (f<HIGH(bump)) THEN
      INC(f); ch:=bump[f];
      IF (ch="$") OR (ch=";") OR (ch="\") OR (ch=sep) THEN
      ELSE DEC(f); ch:="\"
      END
    ELSIF (ch="$") & (f<HIGH(bump)) THEN
      INC(f); ch:=NULL;
      IF (bump[f]>="1") & (bump[f]<="9") THEN
        no:=ORD(bump[f])-ORD("0");
        IF no<=HIGH(arg.words) THEN append(arg.words[no]) END
      ELSIF bump[f]="?" THEN
        val[i]:=0c; read_string(val,inp); append(inp)
      ELSE DEC(f); ch:="$"
      END
    END;
    IF ch#NULL THEN val[i]:=ch; INC(i) END;
    INC(f)
  END;
  IF i<=HIGH(val) THEN val[i]:=0c END;
  IF (f<=HIGH(bump)) & (bump[f]#0c) & (bump[f]#sep) THEN
       pos:=f+1
  ELSE pos:=f
  END;
  sym:=_exe
END exe_command;

PROCEDURE get_sy;
  VAR yes: BOOLEAN;
      fin: INTEGER;
       ch: CHAR;
BEGIN
  LOOP
    yes:=FALSE;
    skip(bump,pos);
    IF (pos>HIGH(bump)) OR (bump[pos]=0c) THEN
      IF NOT get_str() THEN
        bump:=""; pos:=0;
        sym:=_eof; yes:=TRUE
      END;
      IF abort THEN EXIT END;
      pos:=0; skip(bump,pos);
      IF (pos<=HIGH(bump)) & (bump[pos]="%") THEN
        IF in_echo("@") THEN tty.print('%s\n',bump) END;
        pos:=HIGH(bump)+1
      END
    ELSE
      lpos:=pos; yes:=TRUE;
      CASE bump[pos] OF
        |"(": sym:=_obra
        |")": sym:=_cbra
        |"=": sym:=_equ
        |"#": sym:=_neq
        |">":
           IF (pos<HIGH(bump)) & (bump[pos+1]="=") THEN
                sym:=_geq; INC(pos)
           ELSE sym:=_gtr
           END
        |"<":
           IF (pos<HIGH(bump)) & (bump[pos+1]="=") THEN
                sym:=_leq; INC(pos)
           ELSE sym:=_lss
           END
        |"'",'"': get_string; DEC(pos)
        |"0".."9":
          str.iscan(num,bump,pos,yes);
          IF yes THEN DEC(pos); sym:=_num
          ELSE error("#illegal number"); EXIT
          END
      ELSE yes:=FALSE
      END;
      IF yes THEN INC(pos); EXIT END;
      IF (blk^.type=_compare) & (bump[pos]="$") THEN
        get_env; EXIT
      END;
      fin:=pos;
      WHILE (fin<=HIGH(bump)) &
            (bump[fin]#" ") & (bump[fin]#0c) &
            (bump[fin]#";") & (bump[fin]#blk^.sep) DO INC(fin)
      END;
      fin:=fin-1; yes:=TRUE;
      IF    comp("IF"    , bump, pos, fin) THEN sym:=_if
      ELSIF comp("THEN"  , bump, pos, fin) THEN sym:=_then
      ELSIF comp("ELSE"  , bump, pos, fin) THEN sym:=_else
      ELSIF comp("END"   , bump, pos, fin) THEN sym:=_end
      ELSIF comp("WHILE" , bump, pos, fin) THEN sym:=_while
      ELSIF comp("DO"    , bump, pos, fin) THEN sym:=_do
      ELSIF comp("REPEAT", bump, pos, fin) THEN sym:=_repeat
      ELSIF comp("UNTIL" , bump, pos, fin) THEN sym:=_until
      ELSIF comp("LOOP"  , bump, pos, fin) THEN sym:=_loop
      ELSIF comp("EXIT"  , bump, pos, fin) THEN sym:=_exit
      ELSIF comp("HALT"  , bump, pos, fin) THEN sym:=_halt
      ELSE yes:=FALSE
      END;
      IF yes THEN pos:=fin+1;
        IF (pos<HIGH(bump)) & (bump[pos]#0c)
         & (bump[pos]#blk^.sep) THEN INC(pos)
        END;
        EXIT
      END;
      exe_command; EXIT
    END;
    IF yes THEN EXIT END
  END;
IF DEBUG THEN
  tty.print('get_sy(');
  vis_sym(sym);
  tty.print(')\n')
END;
END get_sy;

PROCEDURE push(T: INTEGER; sep: CHAR): BOOLEAN;
  VAR b: blk_ptr;
      i: INTEGER;
BEGIN
IF DEBUG THEN
  tty.print('push(');
  vis_sym(T);
  tty.print(', "%c")\n',sep);
END;
  ASSERT(blk#NIL);
  NEW(b);
  IF chk_mem() THEN RETURN TRUE END;
  b^.type:=T;
  b^.active:=blk^.active;
  b^.fpos:=fpos;
  b^.lpos:=lpos;
  b^.lino:=line;

  b^.sep:=blk^.sep;
  IF sep#NULL THEN b^.sep:=sep END;

  b^.op:=-1;
  b^.res:=0;
  b^.next:=blk;
  blk:=b;
  RETURN FALSE
END push;

PROCEDURE pop;
  VAR b: blk_ptr;
      i: INTEGER;
BEGIN
IF DEBUG THEN
  tty.print('pop(');
  vis_sym(blk^.type);
  tty.print(', "%c")\n',blk^.sep);
END;
  ASSERT((blk#NIL) & (blk^.next#NIL));
  b:=blk; blk:=b^.next;
  IF b^.active THEN blk^.res:=b^.res END;
  DISPOSE(b); ASSERT(NOT chk_mem())
END pop;

PROCEDURE comp_str(op: INTEGER; VAL s1,s2: ARRAY OF CHAR): BOOLEAN;
  VAR r: BOOLEAN;
BEGIN
  CASE op OF
    |_equ: r:=(s1=s2)      |_lss: r:=(s1<s2)
    |_neq: r:=(s1#s2)      |_leq: r:=(s1<=s2)
    |_gtr: r:=(s1>s2)      |_geq: r:=(s1>=s2)
  ELSE ASSERT(FALSE)
  END;
  RETURN r
END comp_str;

PROCEDURE comp_num(op: INTEGER; i,j: INTEGER): BOOLEAN;
  VAR r: BOOLEAN;
BEGIN
  CASE op OF
    |_equ: r:=(i=j)      |_lss: r:=(i<j)
    |_neq: r:=(i#j)      |_leq: r:=(i<=j)
    |_gtr: r:=(i>j)      |_geq: r:=(i>=j)
  ELSE ASSERT(FALSE)
  END;
  RETURN r
END comp_num;

PROCEDURE execute(VAL cmd: ARRAY OF CHAR): INTEGER;
BEGIN
  shell.get_echo(echo);
  IF in_echo("@") & blk^.active THEN tty.print('%s\n',cmd) END;
  shell.system(cmd,echo);
  RETURN shell.result
END execute;

PROCEDURE one_block;

  PROCEDURE check_op(): BOOLEAN;
  BEGIN
    IF NOT (sym IN _ops) THEN
      error("#comparing operation missed");
      RETURN TRUE
    END;
    RETURN FALSE
  END check_op;

  PROCEDURE goto(): BOOLEAN;
  BEGIN
    bio.seek(inp,blk^.fpos,0);
    IF get_str() THEN END;
    IF chk_bio('read("%s")',bio.ename) THEN RETURN TRUE END;
    line:=blk^.lino;
    pos :=blk^.lpos;
    get_sy;
    RETURN abort
  END goto;

  PROCEDURE compare;
    VAR s: STRING;
  BEGIN
    IF NOT push(_compare, NULL) THEN get_sy END;
    IF abort THEN RETURN END;
    IF    sym=_str  THEN
      copy(s,val,-1); --
      get_sy;         -- two different sources of errors
      IF abort OR check_op() THEN RETURN END;
      blk^.op:=sym;
      get_sy;
      IF expect(_str) THEN RETURN END;
      blk^.bool:=comp_str(blk^.op,s,val);
      DISPOSE(s); ASSERT(NOT chk_mem());
      blk^.sub:=0
    ELSIF sym=_num  THEN
      blk^.bool:=BOOLEAN(num); get_sy;
      IF abort OR check_op() THEN RETURN END;
      blk^.op:=sym;
      get_sy;
      IF expect(_obra) THEN RETURN END;
      blk^.sub:=1;
      IF push(_exe,")") THEN RETURN END
    ELSIF sym=_obra THEN
      blk^.sub:=2;
      IF push(_exe,")") THEN RETURN END
    ELSIF sym=_query THEN
      blk^.bool:=BOOLEAN(num); blk^.sub:=0
    ELSE unexp(sym)
    END;
    IF NOT abort THEN get_sy END
  END compare;

  PROCEDURE fin_comp;
    VAR bool,get: BOOLEAN;
             res: INTEGER;
  BEGIN
    IF    blk^.sub=1 THEN
      blk^.bool:=comp_num(blk^.op,INTEGER(blk^.bool),blk^.res)
    ELSIF blk^.sub=2 THEN
      IF check_op() THEN RETURN END;
      blk^.op:=sym;
      get_sy;
      IF abort OR expect(_num) THEN RETURN END;
      blk^.bool:=comp_num(blk^.op,blk^.res,num);
      get_sy
    END;
    bool:=blk^.bool;
    pop;
    blk^.bool:=bool;
    get:=TRUE;
    CASE blk^.type OF
      |_if    :
         IF expect(_then) OR push(_exe,NULL) THEN RETURN END;
         blk^.active:=blk^.active & bool
      |_while :
         IF expect( _do ) OR push(_exe,NULL) THEN RETURN END;
         blk^.active:=blk^.active & bool
      |_repeat:
         IF blk^.active & NOT bool THEN
           IF goto() OR push(_exe,NULL) THEN RETURN END
         ELSE
           pop; get:=FALSE
         END
    ELSE
      ASSERT(FALSE)
    END;
    IF get & NOT abort THEN get_sy END
  END fin_comp;

  PROCEDURE else;
    VAR bool: BOOLEAN;
  BEGIN
    pop;
    IF blk^.type#_if THEN unexp(sym); RETURN END;
    bool:=blk^.bool;
    IF push(_exe,NULL) THEN RETURN END;
    blk^.active:=blk^.active & NOT bool;
    get_sy
  END else;

  PROCEDURE exit;
    VAR do: BOOLEAN;
      loop: blk_ptr;
       res: INTEGER;
  BEGIN
    res:=blk^.res; do:=blk^.active; loop:=blk;
    WHILE (loop#NIL) & (loop^.type#_loop) DO
  --    IF loop^.active THEN res:=loop^.res END;
      IF do THEN loop^.active:=FALSE END;
      loop:=loop^.next
    END;
    IF loop=NIL THEN error("EXIT out of LOOP"); RETURN END;
    IF do THEN
      loop^.op:=_exit; loop^.res:=res
    END;
    get_sy
  END exit;

  PROCEDURE end;
  BEGIN
    IF blk^.type=_exe THEN pop END;
    IF    blk^.type=_if THEN
      pop; get_sy -- checking is not necessary
    ELSIF blk^.type=_while THEN
      IF blk^.active & blk^.bool THEN
        IF NOT goto() THEN compare END
      ELSE pop; get_sy
      END
    ELSIF blk^.type=_loop THEN
      IF blk^.active & (blk^.op#_exit) THEN
        IF NOT (goto() OR push(_exe,NULL)) THEN get_sy END;
      ELSE pop; get_sy
      END
    ELSE unexp(sym)
    END
  END end;

  PROCEDURE _execute;
  BEGIN
    IF blk^.active THEN blk^.res:=execute(val) END;
    get_sy
  END _execute;

  PROCEDURE halt;
    VAR no: INTEGER; b: blk_ptr;
  BEGIN
    get_sy; no:=0;
    IF abort THEN RETURN END;
    IF sym=_obra THEN
      get_sy;
      IF abort THEN RETURN END;
      IF sym=_num THEN
        no:=num;
        get_sy;
        IF abort OR expect(_cbra) THEN RETURN END;
        get_sy
      ELSIF push(_exe,")") THEN RETURN
      END
    END;
    IF blk^.active THEN
      b:=blk; ASSERT(b#NIL);
      WHILE b^.next#NIL DO
        b^.active:=FALSE; b:=b^.next
      END;
      b^.active:=FALSE;
      b^.op:=_halt;
      b^.res:=no
    END
  END halt;

BEGIN
  LOOP
    ASSERT(blk#NIL);
    IF abort THEN EXIT END;
    IF    blk^.type=_compare THEN fin_comp
    ELSIF blk^.type=_exe     THEN
      CASE sym OF
        |_exe   : _execute
        |_if    : IF NOT push(_if, NULL) THEN compare END
        |_else  : else
        |_obra  : IF NOT push(_exe,")") THEN get_sy END
        |_cbra  : pop; get_sy
        |_eof   : IF blk^.next=NIL THEN EXIT
                  ELSE unexp(sym)
                  END
        |_end   : end
        |_while : IF NOT push(_while,NULL) THEN compare END
        |_loop  : IF NOT
                    (push(_loop  ,NULL) OR push(_exe,NULL))
                  THEN get_sy
                  END
        |_repeat: IF NOT
                    (push(_repeat,NULL) OR push(_exe,NULL))
                  THEN get_sy
                  END
        |_until : pop;
                  IF blk^.type#_repeat THEN unexp(sym)
                  ELSE compare
                  END;
        |_exit  : exit
        |_halt  : halt
      ELSE unexp(sym)
      END
    ELSE ASSERT(FALSE)
    END
  END
END one_block;

PROCEDURE release;
  VAR b: blk_ptr;
BEGIN
  WHILE blk#NIL DO
    b:=blk; blk:=b^.next;
    DISPOSE(b)
  END
END release;

PROCEDURE submit(VAL name: ARRAY OF CHAR): INTEGER;
  VAR res: INTEGER;
BEGIN
  IF init_io(name) THEN RETURN -1 END;
  NEW(blk);
  IF chk_mem() THEN RETURN -1 END;
  WITH blk^ DO
    fpos:=-1; lpos:=-1; lino:=-1;
    next:=NIL; bool  :=TRUE;
    sep:=NULL; active:=TRUE;
    res:=0; op:=-1;
    type:= _exe;
  END;
  shell.get_echo(echo);
  sym:=-1;
  val:="";
  get_sy;
  one_block;
  IF abort THEN release; RETURN -1 END;
  IF blk^.op=_halt THEN res:=blk^.res ELSE res:=0 END;
  release; bio.close(inp);
  IF chk_bio('close("%s")',bio.ename) THEN END;
  RETURN res
END submit;

PROCEDURE interpret(VAL cname: ARRAY OF CHAR);
  VAR name: str256;
       res: INTEGER;
BEGIN
  str.sub_str(name,cname,1,str.len(cname));
  res:=submit(name);
  IF res>0 THEN HALT(res) END
END interpret;

PROCEDURE parms;
  VAR com: STRING;
BEGIN
  arg.dispose;
  env.get_str(env.args,com);
  IF env.done THEN
                   (*    equ   wds  esc  pre sep *)
    arg.scan_string(com,FALSE,TRUE,FALSE, "", ""  )
  END
END parms;

PROCEDURE usage;
BEGIN
  tty.print(
    '  "mshell" - command files interpreting utility (c) KRONOS\n'
    'usage:\n'
    '   mshell <command_file_name { argument }\n\n'
    '                           Hady, Oct 17 1990.\n')
END usage;

BEGIN
  abort:=FALSE;  blk:=NIL;
  sle.new(desc,16); parms;

  IF   (HIGH(arg.words)<0)
    OR (arg.words[0,0]#"<") THEN usage; HALT
  END;

  interpret(arg.words[0]);

END mshell.
