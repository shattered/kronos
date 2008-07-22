IMPLEMENTATION MODULE pcSystem; (* Leo 05-Jun-88. (c) KRONOS *)
                                (* Ned 04-Mar-90. (c) KRONOS *)
                                (* Ned 17-Nov-90. (c) KRONOS *)

IMPORT  sys: SYSTEM;
IMPORT  str: Strings;
IMPORT  env: tskEnv;
IMPORT  tty: Terminal;
IMPORT   tm: Time;
IMPORT  mem: Heap;
IMPORT  ers: coErrors;

---------------------------  DEBUG  ----------------------------
                           ---------

PROCEDURE xprint(VAL fmt: ARRAY OF CHAR; SEQ args: sys.WORD);
BEGIN tty.print(fmt,args)
END xprint;

--------------------------  SERVICE  ---------------------------
                          -----------

PROCEDURE final(p: PROC);  BEGIN env.final(p) END final;

PROCEDURE time(): INTEGER; BEGIN RETURN tm.time() END time;

PROCEDURE app_time(VAR s: ARRAY OF CHAR; time: INTEGER);
  VAR y,m,d,h,mi,sc: INTEGER;
BEGIN
  tm.unpack(time,y,m,d,h,mi,sc);
  str.append(s,'%$2d-%$2d-%$2d  %$2d:%$2d.%$2d',d,m,y DIV 100,h,mi,sc);
END app_time;

PROCEDURE append(VAR s: ARRAY OF CHAR; VAL f: ARRAY OF CHAR; SEQ args: sys.WORD);
BEGIN str.append(s,f,args) END append;

PROCEDURE sprint(VAR s: ARRAY OF CHAR; VAL f: ARRAY OF CHAR; SEQ args: sys.WORD);
BEGIN str.print(s,f,args) END sprint;

---------------------------------------------------------------

PROCEDURE err_msg(no: ers.T; VAR s: ARRAY OF CHAR);
BEGIN
  CASE no OF
  |ers.blank        : str.copy(s,"")
  |ers.unexpc_eof   : str.copy(s,"Unexpected end of text")

  |ers.ill_num      : str.copy(s,"'h' expected after hexadecimal number")
  |ers.ill_string   : str.copy(s,"Too long string")
  |ers.ill_char     : str.copy(s,"Illegal char")
  |ers.ill_range    : str.copy(s,"Invalid range")
  |ers.ill_comment  : str.copy(s,"Invalid comment from line")
  |ers.ill_type     : str.copy(s,"Invalid type constructor")
  |ers.ill_expr     : str.copy(s,"Invalid expression")
  |ers.ill_decl     : str.copy(s,"Invalid declaration")
  |ers.ill_header   : str.copy(s,"Invalid module header")

  |ers.ident        : str.copy(s,"Identifier expected")
  |ers.symbol       : str.copy(s,"Expected symbol")

  |ers.block_name   : str.copy(s,"Block name expected")

  |ers.invisible    : str.copy(s,"Undeclared object")
  |ers.duplicate    : str.copy(s,"Already declared object")
  |ers.recursive    : str.copy(s,"Recursive object declaration")
  |ers.ill_export   : str.copy(s,"Export impossible")
  |ers.dup_forward  : str.copy(s,"Duplicate forward")

  |ers.type         : str.copy(s,"Type expected")
  |ers.scalar       : str.copy(s,"Must be scalar type")
  |ers.simple       : str.copy(s,"Must be simple type")
  |ers.incompatible : str.copy(s,"Types incompatible")
  |ers.type_transfer: str.copy(s,"Illegal type transfer")
  |ers.ill_hidden   : str.copy(s,"Illegal realization of hidden type")
  |ers.val_in_type  : str.copy(s,"VAL not allowed in procedure type")
  |ers.parm_quan    : str.copy(s,"Number of parameters mismatch")
  |ers.parm_spec    : str.copy(s,"Illegal parameter specification")
  |ers.huge_type    : str.copy(s,"Type size is too large")

  |ers.variable     : str.copy(s,"Must be variable")
  |ers.procedure    : str.copy(s,"Must be procedure")
  |ers.module       : str.copy(s,"Must be module")
  |ers.array        : str.copy(s,"Must be array")
  |ers.record       : str.copy(s,"Must be record")
  |ers.pointer      : str.copy(s,"Must be pointer")
  |ers.set          : str.copy(s,"Must be SET")

  |ers.no_address   : str.copy(s,"Has no address")
  |ers.no_value     : str.copy(s,"Has no value")

  |ers.not_in_def   : str.copy(s,"Not allowed in definition module")
  |ers.only_in_def  : str.copy(s,"Allowed in definition module only")
  |ers.only_in_cu   : str.copy(s,"Allowed at level 0 only")

  |ers.proc_call    : str.copy(s,"Procedure call in expression")
  |ers.func_call    : str.copy(s,"Function call instead statement")
  |ers.bounds_check : str.copy(s,"Index out of range")
  |ers.dup_label    : str.copy(s,"Such label already exist")
  |ers.ill_exit     : str.copy(s,"EXIT outsize LOOP")

  |ers.readonly     : str.copy(s,"Assign to read only variable")
  |ers.empty_case   : str.copy(s,"empty CASE")
  |ers.loop_var     : str.copy(s,"Must be local variable")

  |ers.const_expr   : str.copy(s,"Must be constant expression")
  |ers.overflow     : str.copy(s,"Overflow in constant expression")
  |ers.statement    : str.copy(s,"Must be statement")
  |ers.unimpl_proc  : str.copy(s,"Unrealized procedure")
  |ers.no_RTS       : str.copy(s,"Invisible RTS procedure")

  |ers.sym_header   : str.copy(s,"Invalid sym. file header")
  |ers.sym_vers     : str.copy(s,"Invalid sym. file version")
  |ers.sym_data     : str.copy(s,"Invalid sym. file data")
  |ers.sym_language : str.copy(s,"Invalid sym. file data")
  |ers.sym_times    : str.copy(s,"Version conflict")

  |ers.restriction  : str.copy(s,"Restriction of compiler")
  |ers.unimplement  : str.copy(s,"Unimplemented")
  ELSE str.print(s,"Undefined error %d",no);
  END;
END err_msg;


----------------------------  HEAP  ----------------------------
                            --------

TYPE
  node_ptr = POINTER TO node_rec;
  node_rec = RECORD size: INTEGER; next: node_ptr END;
  slot_ptr = POINTER TO slot_rec;
  slot_rec = RECORD next: slot_ptr END;
  dyn_ptr  = POINTER TO dyn_rec;
  dyn_rec  = RECORD l,r: dyn_ptr; size: INTEGER END;

VAR
  slots: ARRAY [0..31] OF slot_ptr;
  stat : ARRAY [0..31] OF INTEGER;
  nodes: node_ptr;
  dyns : dyn_ptr;
  drec : dyn_rec;

PROCEDURE alloc(VAR a: sys.ADDRESS; size: INTEGER);
  VAR s,l: slot_ptr; n: node_ptr; co,i,sz: INTEGER;
BEGIN
  IF size<=HIGH(slots) THEN
    co:=32;
    INC(stat[size],co);
  ELSE co:=1
  END;
  sz:=size*co+SIZE(n^);
  mem.ALLOCATE(n,sz);
  n^.size:=sz;
  n^.next:=nodes; nodes:=n;
  a:=sys.ADDRESS(n)+SIZE(n^);
  IF size<=HIGH(slots) THEN
    l:=NIL; s:=a+size;
    FOR i:=0 TO co-2 DO
      s^.next:=l; l:=s;
      s:=sys.ADDRESS(s)+size;
    END;
    slots[size]:=l;
  END;
END alloc;

PROCEDURE ALLOCATE(VAR a: sys.ADDRESS; size: INTEGER);
  VAR s: slot_ptr; i: INTEGER; b: sys.ADDRESS;
BEGIN
  IF (size<=HIGH(slots)) & (slots[size]#NIL) THEN
    s:=slots[size]; slots[size]:=s^.next; a:=s;
  ELSE
    alloc(a,size);
  END;
END ALLOCATE;

PROCEDURE DEALLOCATE(VAR a: sys.ADDRESS; size: INTEGER);
  VAR s: slot_ptr;
BEGIN
  IF (a=NIL) OR (size<=0) THEN RETURN END;
  IF size<=HIGH(slots) THEN
    s:=a;
    s^.next:=slots[size]; slots[size]:=s;
  END;
  a:=NIL;
END DEALLOCATE;

PROCEDURE REALLOCATE(VAR a: sys.ADDRESS; VAR hi: INTEGER;
                                      len,bytes: INTEGER);
  VAR d: dyn_ptr; size,whi: INTEGER;
BEGIN
  size:=(len*bytes+3) DIV 4;
  IF len=0 THEN -- DISPOSE
    IF hi<0 THEN a:=NIL; RETURN END;
    ASSERT(a#NIL);
    d:=a-SIZE(d^);
    d^.l^.r:=d^.r;
    d^.r^.l:=d^.l;
    mem.DEALLOCATE(d,d^.size+SIZE(d^));
    a:=NIL; hi:=-1;
  ELSIF hi<0 THEN -- NEW
    IF len=0 THEN a:=NIL; RETURN END;
    mem.ALLOCATE(a,SIZE(d^)+size);
    d:=a;
    a:=a+SIZE(d^); hi:=len-1;
    d^.size:=size;
    d^.l:=dyns;
    d^.r:=dyns^.r;
    d^.l^.r:=d;
    d^.r^.l:=d;
  ELSE -- RESIZE
    d:=a-SIZE(d^);
    IF d^.size>=size THEN
      hi:=len-1; RETURN
    END;
    whi:=d^.size+SIZE(d^) -1;
    mem.REALLOCATE(d,whi,size+SIZE(d^),4);
    d^.size:=size;
    d^.l^.r:=d;
    d^.r^.l:=d;
    a:=sys.ADDRESS(d)+SIZE(d^);
    hi:=len-1;
  END;
END REALLOCATE;

PROCEDURE ini_heap;
  VAR i: INTEGER;
BEGIN
  dyns:=REF(drec,dyn_ptr);
  dyns^.l:=dyns;
  dyns^.r:=dyns;
  dyns^.size:=-1;
  nodes:=NIL;
  FOR i:=0 TO HIGH(slots) DO slots[i]:=NIL END;
  FOR i:=0 TO HIGH(stat)  DO stat[i]:=0 END;
END ini_heap;

PROCEDURE release;
  VAR x: node_ptr; l,d: dyn_ptr;
BEGIN
  WHILE nodes#NIL DO
    x:=nodes; nodes:=nodes^.next;
    mem.DEALLOCATE(x,x^.size);
  END;
  l:=dyns^.r;
  WHILE l#dyns DO
    d:=l; l:=l^.r;
    mem.DEALLOCATE(d,d^.size+SIZE(d^));
  END;
  ini_heap;
END release;

---------------------------  QUEUEs  ---------------------------
                           ----------

WITH STORAGE (NEW: ALLOCATE; DISPOSE: DEALLOCATE);

TYPE
  NODE  = POINTER TO node;
  node  = RECORD
            info: sys.WORD;
            next: NODE;
          END;
  QUEUE = POINTER TO queue;
  queue = RECORD
            fifo: BOOLEAN;
            head: NODE;
            tail: NODE;
          END;

PROCEDURE open(VAR q: QUEUE; fifo: BOOLEAN);
BEGIN NEW(q);
  q^.head:=NIL; q^.tail:=NIL; q^.fifo:=fifo;
END open;

PROCEDURE lifo(VAR q: QUEUE); BEGIN open(q,FALSE) END lifo;

PROCEDURE fifo(VAR q: QUEUE); BEGIN open(q,TRUE ) END fifo;

PROCEDURE clear(VAR q: QUEUE);
  VAR l,n: NODE;
BEGIN
  ASSERT(q#NIL);
  IF q^.head#NIL THEN l:=q^.head;
    WHILE l#NIL DO n:=l; l:=l^.next; DISPOSE(n) END;
  END;
  DISPOSE(q);
END clear;

PROCEDURE push(q: QUEUE; one: sys.WORD);
  VAR n: NODE;
BEGIN ASSERT(q#NIL);
  NEW(n); n^.info:=one; n^.next:=NIL;
  IF    q^.head=NIL THEN q^.head:=n; q^.tail:=n
  ELSIF q^.fifo     THEN q^.tail^.next:=n; q^.tail:=n;
  ELSE(*q^.stack*)       n^.next:=q^.head; q^.head:=n;
  END;
END push;

PROCEDURE pop(q: QUEUE; VAR one: sys.WORD): BOOLEAN;
  VAR n: NODE;
BEGIN
  ASSERT(q#NIL);
  IF q^.head=NIL THEN RETURN FALSE END;
  n:=q^.head; one:=n^.info;
  q^.head:=n^.next;
  DISPOSE(n);
  RETURN TRUE
END pop;

---------------------------------------------------------------

PROCEDURE show;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(stat) DO
    IF stat[i]#0 THEN
      tty.print('%3d: %d\n',i,stat[i]);
    END;
  END;
END show;

PROCEDURE unimplemented; BEGIN ASSERT(FALSE,51h) END unimplemented;

BEGIN
  ini  :=sys.WORD(unimplemented);
  exi  :=sys.WORD(unimplemented);
  error:=sys.WORD(unimplemented);
  print:=sys.WORD(unimplemented);
  halt :=sys.WORD(unimplemented);
  ini_heap;
--  final(show);
END pcSystem.
