IMPLEMENTATION MODULE mxCmd; (* Ned 30-Nov-88. (c) KRONOS *)

IMPORT   sys: SYSTEM;
IMPORT   cod: defCodes;
IMPORT inter: coolSystem;
IMPORT  scan: mxScan;

WITH STORAGE: inter;

CONST
   liX   = 00h;         clX  = 0D0h;
   llwX  = 20h;         slwX  = 30h;
   lgwX  = 40h;         sgwX  = 50h;
   lswX  = 60h;         sswX  = 70h;

--------------------------  SERVICE  --------------------------
                          -----------

TYPE
  process  = POINTER TO RECORD G,L,PC,M,S,H,T: INTEGER END;
  set_ptr  = POINTER TO BITSET;

PROCEDURE active(): process; CODE cod.activ END active;
PROCEDURE tags(): BITSET;    CODE cod.llw 2 END tags;
PROCEDURE getm(): BITSET;    CODE cod.getm  END getm;
PROCEDURE setm(m: BITSET);   CODE cod.setm  END setm;
PROCEDURE savem;             CODE cod.getm cod.slw 3 END savem;

PROCEDURE mask_overflow;
  VAR p: process;
BEGIN
  setm(getm()-{31});
  IF 30 IN tags() THEN savem END;
  p:=active(); p^.T:=0;
END mask_overflow;

PROCEDURE check_overflow(): BOOLEAN;
  VAR p: process; s: set_ptr; T: INTEGER;
BEGIN
  p:=active(); T:=p^.T;
  setm(getm()+{31});
  IF 30 IN tags() THEN savem END;
  IF T=0 THEN RETURN FALSE END;
  IF (T=41h) OR (T=42h) OR (T=43h) THEN RETURN TRUE END;
  ASSERT(FALSE,T);
END check_overflow;

PROCEDURE check (n,l: INTEGER); CODE cod.lib 0FFh cod.chk  cod.drop END check;
PROCEDURE check0(n: INTEGER);   CODE cod.lib 0FFh cod.chkz cod.drop END check0;

------------------------  CODE BUFFER  ------------------------
                        ---------------
TYPE
  code_buf = ARRAY [0..1023] OF CHAR;
  code_ptr = POINTER TO code_buf;

VAR  -- буфер для готового  кода (выходной)
  out_bufs: inter.QUEUE;  -- QUEUE(code_ptr)
  out_co  : INTEGER;         -- счетчик буферов в очереди
  out_buf : code_ptr;        -- текущий буфер
  out_pos : INTEGER;         -- позиция в текущем буфере

VAR  -- буфер для сегментов кода
  cur_buf : STRING;

VAR  -- информация для щелевой оптимизации
  last  : INTEGER;   -- последняя команда сегмента
  action: INTEGER;
  last_l: INTEGER;   -- \ размещение последней переменной
  last_n: INTEGER;   -- /

CONST -- actions
  _no_action = 0; _load = 1; _save = 2; _loadadr = 3; _neq0 = 4;

PROCEDURE ini_code_buf;
BEGIN
  inter.fifo(out_bufs);  out_co :=0;
  NEW(out_buf); out_pos:=0;
  NEW(cur_buf);
  cur_pos:=0;  cur_segm:=0;
  last:=-1; action:=_no_action;
END ini_code_buf;

PROCEDURE exi_code_buf;
BEGIN
  inter.clear(out_bufs);
  DISPOSE(out_buf);
  DISPOSE(cur_buf);
END exi_code_buf;

PROCEDURE out_code_pos(): INTEGER;
BEGIN RETURN out_co*BYTES(code_buf)+out_pos
END out_code_pos;

PROCEDURE close_seg(VAR seg,len: INTEGER);
BEGIN last:=-1;
  IF action=_neq0 THEN DEC(cur_pos,2) END;
  action:=_no_action;
  seg:=cur_segm; len:=cur_pos-cur_segm; cur_segm:=cur_pos;
END close_seg;

PROCEDURE b(n: INTEGER);
BEGIN check0(n);
  IF cur_pos>HIGH(cur_buf) THEN RESIZE(cur_buf,BYTES(cur_buf)+1024) END;
  cur_buf[cur_pos]:=CHAR(n); INC(cur_pos);
END b;

PROCEDURE c(n: INTEGER);
BEGIN last:=n; b(last); action:=_no_action;
END c;

PROCEDURE flush_out_buf;
BEGIN
  inter.push(out_bufs,out_buf); INC(out_co);
  NEW(out_buf); out_pos:=0;
END flush_out_buf;

PROCEDURE save_byte(b: INTEGER);
BEGIN check0(b);
  IF out_pos>HIGH(out_buf^) THEN flush_out_buf END;
  out_buf^[out_pos]:=CHAR(b); INC(out_pos);
END save_byte;

PROCEDURE save_seg(seg,len: INTEGER);
BEGIN
  WHILE len>0 DO
    IF out_pos>HIGH(out_buf^) THEN flush_out_buf END;
    out_buf^[out_pos]:=cur_buf[seg];
    INC(out_pos); INC(seg); DEC(len);
  END;
END save_seg;

PROCEDURE save_jump(uncond,short,back: BOOLEAN; len: INTEGER);
BEGIN save_byte(18h+ORD(uncond)+ORD(short)*2+ORD(back)*4);
  IF len<=255 THEN save_byte(len);
  ELSE save_byte(len MOD 100h); save_byte(len DIV 100h)
  END;
END save_jump;

PROCEDURE write_code(write: WRITE);
  VAR c: code_ptr;
BEGIN
  IF out_pos#0 THEN
    WHILE (out_pos MOD 4)#0 DO save_byte(cod.invld) END;
  END;
  WHILE inter.pop(out_bufs,c) DO
    write(c^,SIZE(code_buf)); DISPOSE(c);
  END;
  IF out_pos#0 THEN write(out_buf^,out_pos DIV 4) END;
END write_code;

---------------------------  UNDO  ---------------------------
                           --------

CONST cmd_len = ARRAY OF CHAR{
          1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,
          2c,3c,5c,1c,2c,2c,2c,3c,3c,3c,2c,2c,3c,3c,1c,1c,
          2c,2c,3c,2c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,
          2c,2c,3c,2c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,
        
          1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,
          1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,
          1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,
          1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,

          1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,
          1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,2c,
          1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,
          1c,1c,1c,1c,1c,1c,2c,2c,7c,7c,7c,1c,1c,1c,2c,2c,
        
          1c,7c,3c,2c,1c,1c,2c,2c,1c,2c,1c,1c,3c,2c,1c,2c,
          1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,
          1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,1c,3c,7c,7c,7c,7c,
          1c,2c,2c,2c,1c,1c,1c,2c,7c,7c,7c,7c,7c,7c,7c,7c};

PROCEDURE undo;
  VAR n: INTEGER;
BEGIN n:=ORD(cmd_len[last]); ASSERT(n IN {1..3,5});
  DEC(cur_pos,n); last:=-1; action:=_no_action;
END undo;

-------------------------  E-STACK  --------------------------
                         -----------

VAR e_stack: ARRAY [0..eStackDepth-1] OF INTEGER;

PROCEDURE ePush;
BEGIN
  IF scan.noErrors#0 THEN RETURN END;
  INC(depth);
  IF depth>eStackDepth THEN scan.Fault(3,'')
  ELSE e_stack[depth-1]:=-1;
  END;
END ePush;

PROCEDURE ePop;
BEGIN
  IF scan.noErrors#0 THEN RETURN END;
  DEC(depth);
  ASSERT(depth>=0)
END ePop;

PROCEDURE set_depth(n: INTEGER);
BEGIN ASSERT(n IN {0..6});
  depth:=n; action:=_no_action;
END set_depth;

PROCEDURE pop_const;
BEGIN ePop;
  IF scan.noErrors#0 THEN RETURN END;
  cur_pos:=e_stack[depth]; ASSERT(cur_pos>=cur_segm);
END pop_const;

--------------------------  COMMANDS  -------------------------
                          ------------

PROCEDURE b4(n: INTEGER);
BEGIN
  b(INTEGER(BITSET(n)*{0..7})); n:=n>>8;
  b(INTEGER(BITSET(n)*{0..7})); n:=n>>8;
  b(INTEGER(BITSET(n)*{0..7})); n:=n>>8;
  b(INTEGER(BITSET(n)*{0..7}));
END b4;

PROCEDURE li(n: INTEGER);
  VAR pos: INTEGER;
BEGIN pos:=cur_pos;
  IF n>=0 THEN
    IF    n<=    0Fh THEN c(liX+n); ePush;
    ELSIF n<=   0FFh THEN c(cod.lib);  b(n); ePush;
    ELSIF n<= 0FFFFh THEN c(cod.lid);  b(n MOD 256); b(n DIV 256); ePush;
    ELSIF n=INTEGER(NIL) THEN c(cod.lin); ePush;
    ELSIF (n=MAX(INTEGER)) & (depth<eStackDepth-1) THEN
      c(liX+2); ePush; c(cod.neg); c(liX+1); ePush; c(cod.ror); ePop;
    ELSE  c(cod.liw); b4(n); ePush;
    END;
  ELSIF (n=MIN(INTEGER)) & (depth<eStackDepth-1) THEN
    c(liX+1); ePush; copt; c(cod.ror); ePop;
  ELSIF -n>0FFFFh THEN c(cod.liw); b4(n); ePush
  ELSE li(-n); c(cod.neg);
  END;
  IF scan.noErrors#0 THEN RETURN END;
  e_stack[depth-1]:=pos;
END li;

PROCEDURE lsta(n: INTEGER);
  VAR pos: INTEGER;
BEGIN pos:=cur_pos;
  c(cod.lsta); b(n MOD 100h); b(n DIV 100h); ePush;
  IF scan.noErrors#0 THEN RETURN END;
  e_stack[depth-1]:=pos;
END lsta;

PROCEDURE cADD(n: INTEGER);
BEGIN
  IF n=0 THEN RETURN
  ELSIF (n>=0) & (n<=0FFh) THEN
    IF (n<=15) & (depth<eStackDepth) THEN
      li(n); c(cod.add); ePop
    ELSE c(cod.lsa); b(n)
    END;
  ELSIF n<0 THEN li(-n); c(cod.sub); ePop;
  ELSE           li( n); c(cod.add); ePop;
  END
END cADD;

PROCEDURE ssw(n: INTEGER);
BEGIN
  IF n<=0Fh THEN c(sswX+n) ELSE c(cod.ssw); b(n) END;
  ePop; ePop;
END ssw;

PROCEDURE lsw(n: INTEGER);
BEGIN
  IF    n<=0Fh  THEN c(lswX+n)
  ELSIF n<=0FFh THEN c(cod.lsw); b(n)
  ELSE  cADD(n); c(lswX+0);
  END
END lsw;

PROCEDURE getbase(level: INTEGER);
BEGIN
  level:=curlevel-level; ASSERT(level>0);
  IF level=1 THEN c(cod.gb1) ELSE c(cod.gb); b(level) END;
  ePush;
END getbase;

PROCEDURE check_nil;
BEGIN
  IF CPU>=1 THEN c(cod.chknil)
  ELSE
    c(cod.lin); ePush; cADD(-1); c(cod.chkz); ePop;
  END;
END check_nil;

PROCEDURE raise(n: INTEGER);
BEGIN li(n); c(cod.trap); ePop;
END raise;

PROCEDURE power2(c: INTEGER; VAR bit: INTEGER): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  IF (c=MIN(INTEGER)) OR (BITSET(c)*BITSET(c-1)#{}) THEN RETURN FALSE END;
  i:=0;
  WHILE (i<BITS(sys.WORD)) & NOT (i IN BITSET(c)) DO INC(i) END;
  bit:=i;
  RETURN TRUE
END power2;

PROCEDURE cMUL(op: INTEGER);
  VAR bit: INTEGER;
BEGIN
  IF NOT power2(op,bit) THEN li(op); c(cod.mul); ePop;
  ELSIF bit=32 THEN c(cod.drop); ePop; li(0);
  ELSIF bit=1  THEN copt; c(cod.add); ePop;
  ELSIF bit#0  THEN li(bit); c(cod.shl); ePop;
  END;
END cMUL;

PROCEDURE cDIV(op: INTEGER; quo: BOOLEAN);
  VAR bit: INTEGER;
BEGIN
  IF NOT power2(op,bit) THEN li(op);
    IF quo & (CPU>=2) THEN c(cod.quot); b(cod.quot_quo) ELSE c(cod.div) END;
    ePop;
  ELSIF bit=32 THEN scan.err(54)
  ELSIF bit#0  THEN li(bit);
    IF quo & (CPU>=2) THEN c(cod.quot); b(cod.quot_shr) ELSE c(cod.shr) END;
    ePop;
  END;
END cDIV;

PROCEDURE cMOD(op: INTEGER; rem: BOOLEAN);
  VAR bit: INTEGER;
BEGIN
  IF NOT power2(op,bit) THEN li(op);
    IF rem & (CPU>=2) THEN c(cod.quot); b(cod.quot_rem) ELSE c(cod.mod) END;
    ePop;
  ELSIF bit=32 THEN scan.err(54)
  ELSE li(op-1);
    IF rem & (CPU>=2) THEN c(cod.quot); b(cod.quot_and) ELSE c(cod.and) END;
    ePop;
  END;
END cMOD;

PROCEDURE logical_not(not_not: BOOLEAN);
  CONST ofs = cod.lss;
    inverse = ARRAY OF INTEGER {
                cod.geq, cod.gtr, cod.leq,
                cod.lss, cod.neq, cod.equ};
  VAR n: INTEGER;
BEGIN n:=last-ofs;
  IF n IN {0..5} THEN undo; c(inverse[n]);
  ELSIF not_not & (last=cod.not) THEN undo;
  ELSE c(cod.not);
  END;
END logical_not;

PROCEDURE move; BEGIN c(cod.move); ePop; ePop; ePop END move;

PROCEDURE lxa(size: INTEGER);
  VAR bit: INTEGER;
BEGIN
  IF size=1 THEN c(cod.add); ePop
  ELSIF power2(size,bit) THEN ASSERT(bit#32); cMUL(size); c(cod.add); ePop
  ELSE li(size); c(cod.lxa); ePop; ePop;
  END;
END lxa;

PROCEDURE copt; BEGIN c(cod.copt); ePush END copt;
PROCEDURE stot; BEGIN c(cod.stot); ePop  END stot;
PROCEDURE lodt; BEGIN c(cod.lodt); ePush END lodt;

PROCEDURE store; BEGIN c(cod.store); depth:=0 END store;
PROCEDURE stofv; BEGIN c(cod.stofv); depth:=0 END stofv;
PROCEDURE lodfv; BEGIN c(cod.lodfv);          END lodfv;

PROCEDURE alloc(n: INTEGER); BEGIN li(n); c(cod.alloc) END alloc;

-----------------------  EXPRESSIONs  ------------------------
                       ---------------

CONST
  commands  = ARRAY OF INTEGER {
     -- int :
                (* = *) cod.equ,   (* # *) cod.neq,   (* IN*) cod.invld,
                (* < *) cod.lss,   (* > *) cod.gtr,   (* <=*) cod.leq,
                (* >=*) cod.geq,   (* * *) cod.mul,   (*DIV*) cod.div,
                (*MOD*) cod.mod,   (* / *)      -4,   (* - *) cod.sub,
                (* + *) cod.add,   (* <<*) cod.rol,   (* >>*) cod.ror,
                (*REM*)      -5,
     -- set :
                (* = *) cod.equ,   (* # *) cod.neq,   (* IN*) cod.in,
                (* < *) cod.invld, (* > *) cod.invld, (* <=*) -2,
                (* >=*) -3,        (* * *) cod.and,   (*DIV*) cod.invld,
                (*MOD*) cod.invld, (* / *) cod.xor,   (* - *) cod.bic,
                (* + *) cod.or,    (* <<*) cod.invld, (* >>*) cod.invld,
                (*REM*) cod.invld,
     -- real:
                (* = *)        -1, (* # *)        -1, (* IN*) cod.invld,
                (* < *)        -1, (* > *)        -1, (* <=*)        -1,
                (* >=*)        -1, (* * *) cod.fmul,  (*DIV*) cod.invld,
                (*MOD*) cod.invld, (* / *) cod.fdiv,  (* - *) cod.fsub,
                (* + *) cod.fadd,  (* <<*) cod.invld, (* >>*) cod.invld,
                (*REM*) cod.invld
              };

PROCEDURE bin_op(sy,mode: INTEGER);
  VAR cmd: INTEGER;
BEGIN
  cmd:=commands[sy+mode*16];
  IF cmd>=0 THEN ASSERT(cmd#cod.invld);
    c(cmd); ePop;
  ELSE
    CASE cmd OF
      |-1: b(cod.fcmp); c(commands[sy+0*16]); ePop;
      |-2:              c(cod.bic); ePop; c(cod.not);
      |-3: c(cod.swap); c(cod.bic); ePop; c(cod.not);
      |-4: IF CPU>=2 THEN c(cod.quot); b(cod.quot_quo) ELSE c(cod.div) END;
           ePop;
      |-5: IF CPU>=2 THEN c(cod.quot); b(cod.quot_rem) ELSE c(cod.mod) END;
           ePop;
    ELSE ASSERT(FALSE);
    END;
  END;
END bin_op;

PROCEDURE comp0(sy: INTEGER);
BEGIN
  IF    sy=scan.equ THEN c(cod.not);
  ELSIF sy=scan.neq THEN li(0); c(cod.neq); ePop; action:=_neq0
  ELSE ASSERT(FALSE)
  END;
END comp0;

PROCEDURE string_cmp(sy: INTEGER);
BEGIN c(cod.comp); bin_op(sy,0);
END string_cmp;

------------------------  LOAD & STORE  -----------------------
                        ----------------

PROCEDURE lgw(n: INTEGER);
BEGIN check(n,2);
  IF n<=0Fh THEN c(lgwX+n) ELSE c(cod.lgw); b(n) END; ePush;
END lgw;

PROCEDURE llw(n: INTEGER);
BEGIN check(n,4);
  IF n<=0Fh THEN c(llwX+n) ELSE c(cod.llw); b(n) END; ePush;
END llw;

PROCEDURE lpw(n: INTEGER); BEGIN c(cod.lpw); b(n); ePush END lpw;

PROCEDURE loadword(l,n: INTEGER);
BEGIN
  IF (action IN {_load,_save}) & (n=last_n) & (l=last_l) THEN
    IF action=_load THEN copt; action:=_load; RETURN
    ELSIF depth<eStackDepth-1 THEN
      undo; ePush; copt; storeword(l,n); RETURN
    END;
  END;
  IF    l<0 THEN c(cod.lew); b(-l); b(n); ePush;
  ELSIF l=0 THEN lgw(n);
  ELSIF l=curlevel THEN
    IF n<=0 THEN lpw(-n) ELSE llw(n) END;
  ELSE getbase(l);
    IF n<=0 THEN li(1-n); c(cod.sub); ePop; lsw(0) ELSE lsw(n) END;
  END;
  last_n:=n; last_l:=l; action:=_load;
END loadword;

PROCEDURE slw(n: INTEGER);
BEGIN check(n,4);
  IF n<=0Fh THEN c(slwX+n) ELSE c(cod.slw); b(n) END; ePop;
END slw;

PROCEDURE sgw(n: INTEGER);
BEGIN check(n,2);
  IF n<=0Fh THEN c(sgwX+n) ELSE c(cod.sgw); b(n) END; ePop;
END sgw;

PROCEDURE spw(n: INTEGER);
BEGIN c(cod.spw); b(n); ePop;
END spw;

PROCEDURE storeword(l,n: INTEGER);
BEGIN
  IF    l<0 THEN c(cod.sew); b(-l); b(n); ePop;
  ELSIF l=0 THEN sgw(n);
  ELSE ASSERT(l=curlevel);
    IF n<=0 THEN spw(-n) ELSE slw(n) END;
  END;
  last_n:=n; last_l:=l; action:=_save;
END storeword;

PROCEDURE loadadr(l,n: INTEGER);
BEGIN
  IF (action=_loadadr) & (last_n=n) & (last_l=l) THEN
    copt; action:=_loadadr; RETURN
  END;
  IF    l<0 THEN c(cod.lea); b(-l); b(n); ePush;
  ELSIF l=0 THEN c(cod.lga); b(n);        ePush;
  ELSIF l=curlevel THEN
    IF n<=0 THEN c(cod.lpa); b(-n) ELSE c(cod.lla); b(n) END; ePush;
  ELSE getbase(l);
    IF n<=0 THEN li(1-n); c(cod.sub); ePop ELSE cADD(n) END;
  END;
  last_n:=n; last_l:=l; action:=_loadadr;
END loadadr;

PROCEDURE loadconst(l,n: INTEGER);
  VAR pos: INTEGER;
BEGIN pos:=cur_pos;
  IF l<0 THEN c(cod.lew); b(-l); b(1); ePush; cADD(n) ELSE lsta(n) END;
  IF scan.noErrors=0 THEN e_stack[depth-1]:=pos END;
END loadconst;

PROCEDURE call(l,n: INTEGER);
BEGIN ASSERT(n>0);
  IF     l<0 THEN c(cod.cx); b(-l); b(n);
  ELSIF (l=0) OR (l=curlevel) THEN
    IF n<=0Fh THEN c(clX+n) ELSE c(cod.cl); b(n) END;
  ELSE ASSERT(l<curlevel);
    getbase(l); c(cod.ci); b(n); ePop;
  END;
END call;

PROCEDURE copy_farr(l,n,hi_n: INTEGER; byte: BOOLEAN; size: INTEGER);
BEGIN ASSERT(l=curlevel);
  IF n<=0 THEN
    lpw(-hi_n);
    IF byte THEN li(2); c(cod.shr); ePop; cADD(1);
    ELSE cADD(1);
      IF size>1 THEN cMUL(size) END;
    END;
    copt; c(cod.alloc); c(cod.swap); stot; copt; lpw(-n); lodt;
    move; spw(-n);
  ELSE
    llw(n); llw(hi_n);
    IF byte THEN c(cod.cpcop); b(n);
    ELSIF size=1 THEN c(cod.pcop); b(n);
    ELSE cADD(1); cMUL(size);
      li(1); c(cod.sub); ePop; c(cod.pcop); b(n);
    END; ePop; ePop;
  END;
END copy_farr;

PROCEDURE copy_struct(l,n: INTEGER; size: INTEGER);
BEGIN ASSERT(l=curlevel);
  IF n<=0 THEN
    lpw(-n);  alloc(size); copt; spw(-n);
    c(cod.swap); li(size); move;
  ELSE llw(n); li(size-1); c(cod.pcop); b(n); ePop; ePop;
  END;
END copy_struct;

----------------------  CODE PROCEDUREs  ----------------------
                      -------------------

VAR -- буфер для кодовых процедур
  code_bodies: STRING;
  code_pos   : INTEGER;

PROCEDURE insert_code_proc(pp: INTEGER);
  VAR len,pos: INTEGER;
BEGIN
  len:=pp MOD 10000h;
  pos:=pp DIV 10000h;
  last:=-1; action:=_no_action;
  WHILE len>0 DO b(ORD(code_bodies[pos])); INC(pos); DEC(len) END;
END insert_code_proc;

PROCEDURE enter_code_proc(VAR pp: INTEGER);
BEGIN pp:=code_pos*10000h+0;
END enter_code_proc;

PROCEDURE app_code_expr(VAR pp: INTEGER; byte: INTEGER);
BEGIN
  IF code_pos>HIGH(code_bodies) THEN
    RESIZE(code_bodies,BYTES(code_bodies)+256)
  END;
  code_bodies[code_pos]:=CHAR(byte); INC(code_pos);
  INC(pp);
END app_code_expr;

----------------------------  POOL  ---------------------------
                            --------

TYPE
  pool_ptr = POINTER TO pool_buf;
  pool_buf = RECORD
               CASE :BOOLEAN OF
                |FALSE: wbuf: ARRAY [0..254 ] OF INTEGER;
                |TRUE : cbuf: ARRAY [0..1019] OF CHAR;
               END;
               next: pool_ptr;
             END;

VAR
  pool_bufs: pool_ptr;
  cur_pool : pool_ptr;
  pool_pos : INTEGER;

PROCEDURE new_pool_buf;
  VAR l,p: pool_ptr;
BEGIN
  NEW(p);
  p^.next:=NIL; cur_pool:=p;
  IF pool_bufs=NIL THEN pool_bufs:=p
  ELSE l:=pool_bufs;
    WHILE l^.next#NIL DO l:=l^.next END;
    l^.next:=p;
  END; pool_pos:=0;
END new_pool_buf;

PROCEDURE ini_pool;
BEGIN pool_bufs:=NIL; pool_ofs:=0; new_pool_buf;
END ini_pool;

PROCEDURE exi_pool;
  VAR l,n: pool_ptr;
BEGIN l:=pool_bufs;
  WHILE l#NIL DO n:=l^.next; DISPOSE(l); l:=n END;
END exi_pool;

PROCEDURE skip(VAR ofs: INTEGER): pool_ptr;
  VAR p: pool_ptr;
BEGIN p:=pool_bufs;
  WHILE ofs>HIGH(p^.wbuf) DO DEC(ofs,SIZE(p^.wbuf)); p:=p^.next END;
  RETURN p
END skip;

PROCEDURE inc_table(ofs,len,add: INTEGER);
  VAR p: pool_ptr;
BEGIN p:=skip(ofs);
  WHILE len>0 DO
    INC(p^.wbuf[ofs],add); DEC(len);
    IF ofs>=HIGH(p^.wbuf) THEN ofs:=0; p:=p^.next ELSE INC(ofs) END;
  END;
END inc_table;

PROCEDURE app_word(w: INTEGER);
BEGIN
  IF pool_pos>HIGH(cur_pool^.wbuf) THEN new_pool_buf END;
  cur_pool^.wbuf[pool_pos]:=w; INC(pool_pos); INC(pool_ofs);
END app_word;

PROCEDURE app_words(len: INTEGER; VAL w: ARRAY OF sys.WORD);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO len-1 DO app_word(w[i]) END;
END app_words;

PROCEDURE app_str(VAL s: ARRAY OF CHAR; len: INTEGER): INTEGER;
  VAR res: INTEGER;
BEGIN res:=pool_ofs; app_words((len+3) DIV 4,s); RETURN res
END app_str;

PROCEDURE get_word(ofs,inx: INTEGER): INTEGER;
  VAR p: pool_ptr;
BEGIN INC(ofs,inx); p:=pool_bufs;
  WHILE ofs>HIGH(p^.wbuf) DO DEC(ofs,SIZE(p^.wbuf)); p:=p^.next END;
  RETURN p^.wbuf[ofs]
END get_word;

PROCEDURE get_byte(ofs,inx: INTEGER): INTEGER;
  VAR p: pool_ptr;
BEGIN INC(ofs,inx DIV 4); p:=skip(ofs);
  RETURN ORD(p^.cbuf[ofs*4+inx MOD 4])
END get_byte;

PROCEDURE iter_bytes(ofs,len: INTEGER; one: PUT_BYTE);
  VAR p: pool_ptr;
BEGIN p:=skip(ofs); ofs:=ofs*4; len:=len*4;
  WHILE len>0 DO
    one(ORD(p^.cbuf[ofs])); INC(ofs); DEC(len);
    IF ofs>HIGH(p^.cbuf) THEN p:=p^.next; ofs:=0 END;
  END;
END iter_bytes;

PROCEDURE put_bytes(start,len: INTEGER; one: GET_BYTE);
  VAR p: pool_ptr; i,ofs: INTEGER;
BEGIN
  p:=pool_bufs; ofs:=start;
  WHILE ofs>HIGH(p^.wbuf) DO
    DEC(ofs,SIZE(p^.wbuf));
    IF p^.next=NIL THEN new_pool_buf END;
    p:=p^.next
  END;
  i:=ofs*4;
  INC(start,len);
  len:=len*4;
  WHILE len>0 DO
    IF i>HIGH(cur_pool^.cbuf) THEN new_pool_buf; i:=0 END;
    cur_pool^.cbuf[i]:=CHAR(one()); INC(i); DEC(len);
  END;
  IF start>pool_ofs THEN pool_ofs:=start; pool_pos:=i DIV 4 END;
END put_bytes;

PROCEDURE write_pool(write: WRITE);
  VAR p: pool_ptr;
BEGIN
  p:=pool_bufs; ASSERT(p#NIL);
  WHILE p^.next#NIL DO
    write(p^.wbuf,SIZE(p^.wbuf)); p:=p^.next
  END;
  IF pool_pos#0 THEN write(p^.wbuf,pool_pos) END;
END write_pool;

--------------------------  CONTROLs  -------------------------
                          ------------

CONST -- atomMode
  _free  = 0;                  -- not used atom
  _dummy = 1;                  -- dummy atom
  _mcode = 2;                  -- m-code segment
  _jump  = 3;                  -- uncoditional jump & exit (loop,case)
  _jump? = 4;   _jumpT = 5;    -- conditional (FALSE & TRUE) jumps
  _and?  = 6;   _or?   = 7;    -- and & or jumps
  _return= 8;                  -- return
  _drop  = 9;                  -- drop command
            ----- control points of structures -----
  _exit  =10;
  _loop  =11;  _while =12;  _do    =13;   _repeat=14;
  _if    =15;  _else  =16;  _for   =17;   _case  =18;
  _cand  =19;   _cor  =20;

TYPE
  xpos_ptr = POINTER TO xpos_rec;
  xpos_rec = RECORD
               pc  : INTEGER;
               line: INTEGER;
               col : INTEGER;
               next: xpos_ptr;
             END;
  atom     = POINTER TO atomdesc;
  POINT    = atom;
  atomdesc = RECORD
               pos : INTEGER; -- используется только при оптимизации
               next: atom;
               CASE mode : INTEGER OF
                 |_mcode     : segm: INTEGER; len : INTEGER; xpos: xpos_ptr;
                 |_jump.._or?: to  : atom;    clen: INTEGER;
                 |_exit      : from: atom;   exits: atom; -- list of exits
                 |_return    :
                 |_dummy     :
                 |_loop,_cor : down: atom;    val : INTEGER;
                 |_free      :
                 |_drop      :
               END;
             END;

CONST
  jumps? = {_jump?,_jumpT,_and?,_or?};
  jumps  = jumps? + {_jump};

VAR
  stack: atom;      -- stack of control structures
  list : atom;      -- head of list of segments
  tail : atom;      -- tail of list of segments
  exits: atom;      -- head of list of exits
  xpos : xpos_ptr;  -- head of list of xpos of current segment

PROCEDURE newatom(m: INTEGER): atom;
  VAR a: atom;
BEGIN
  IF (tail#NIL) & (tail^.next#NIL) THEN
    tail:=tail^.next; ASSERT(tail^.mode=_free);
    a:=tail;
  ELSE
    NEW(a);
    IF list=NIL THEN list:=a; tail:=a
    ELSE tail^.next:=a; tail:=a
    END;
  END;
  a^.pos:=0; a^.mode:=m; a^.next:=NIL;
  RETURN a
END newatom;

PROCEDURE nextatom(): atom;
  VAR a,b: atom;
BEGIN b:=tail; a:=newatom(_free); tail:=b; RETURN a
END nextatom;

PROCEDURE insert_atom(after: atom; mode: INTEGER): atom;
  VAR a: atom;
BEGIN
  NEW(a);
  a^.mode:=mode; a^.pos:=0; a^.next:=after^.next; after^.next:=a;
  RETURN a
END insert_atom;

PROCEDURE mark_pos;
  VAR new: xpos_ptr;
BEGIN
  IF scan.noErrors#0 THEN RETURN END;
  NEW(new);
  new^.pc:=cur_pos-cur_segm;
  new^.line:=scan.line; new^.col:=scan.col;
  new^.next:=xpos; xpos:=new;
END mark_pos;

PROCEDURE rem_xpos(l: xpos_ptr);
  VAR n: xpos_ptr;
BEGIN
  WHILE l#NIL DO n:=l^.next; DISPOSE(l); l:=n END;
END rem_xpos;

PROCEDURE out_xpos(put_xref: PUT_XREF; l: xpos_ptr; proc_no,entry: INTEGER);
  VAR n: xpos_ptr;
BEGIN
  WHILE l#NIL DO n:=l^.next;
    put_xref(proc_no,l^.pc+entry,l^.line,l^.col);
    DISPOSE(l); l:=n
  END;
END out_xpos;

PROCEDURE mcode;
  VAR a: atom; segm,len: INTEGER;
BEGIN close_seg(segm,len);
  IF len=0 THEN rem_xpos(xpos);
  ELSE a:=newatom(_mcode); a^.segm:=segm; a^.len:=len; a^.xpos:=xpos;
  END; xpos:=NIL;
END mcode;

PROCEDURE save_mcode;
  VAR seg,len: INTEGER;
BEGIN
  close_seg(seg,len); save_seg(seg,len);
END save_mcode;

PROCEDURE set_point(VAR p: POINT);
BEGIN mcode; p:=newatom(_dummy);
END set_point;

PROCEDURE insert(a: POINT);
  VAR seg,len: INTEGER;
BEGIN a^.mode:=_mcode; a^.xpos:=NIL; close_seg(a^.segm,a^.len);
END insert;

----------------------------------------------------------------

TYPE
  POSITION = POINTER TO pos_rec;
  pos_rec  = RECORD
               tail: atom;
               xpos: xpos_ptr;
               segm: INTEGER;
               pos : INTEGER;
             END;

PROCEDURE save_pos(VAR pos: POSITION);
BEGIN
  NEW(pos);
  pos^.tail:=tail; pos^.xpos:=xpos;
  pos^.segm:=cur_segm; pos^.pos:=cur_pos;
  action:=_no_action;
END save_pos;

PROCEDURE resume(pos: POSITION);
  VAR x,d: xpos_ptr; l: atom;
BEGIN
  ASSERT(pos^.pos>=pos^.segm);
  cur_segm:=pos^.segm; cur_pos:=pos^.pos;
  tail:=pos^.tail; x:=NIL;
  IF tail=NIL THEN list:=NIL
  ELSE l:=tail^.next;
    WHILE l#NIL DO
      IF l^.mode=_mcode THEN
        IF x=NIL THEN x:=xpos ELSE rem_xpos(xpos) END;
      END;
      l^.mode:=_free; l:=l^.next;
    END;
  END;
  IF x=NIL THEN x:=xpos END;
  WHILE (x#NIL) & (x#pos^.xpos) DO
    d:=x; x:=x^.next; DISPOSE(d);
  END;
  ASSERT(x=pos^.xpos);
  xpos:=pos^.xpos;
END resume;

PROCEDURE release(pos: POSITION);
BEGIN DISPOSE(pos);
END release;

----------------------------------------------------------------

TYPE
  goto_ptr = POINTER TO goto_rec;
  goto_rec = RECORD
               id  : INTEGER;
               fwd : BOOLEAN;
               atom: atom;
               next: goto_ptr;
             END;

VAR labels: goto_ptr;

PROCEDURE mark(n: INTEGER);
  VAR l,p,x: goto_ptr; a: atom;
BEGIN
  mcode;
  a:=newatom(_dummy);
  l:=labels; p:=NIL;
  WHILE l#NIL DO
    IF l^.id=n THEN
      IF NOT l^.fwd THEN scan.err(15); RETURN END;
      x:=l;
      IF p=NIL THEN labels:=l^.next; l:=labels;
      ELSE
        p^.next:=l^.next; l:=p^.next;
      END;
      x^.atom^.to:=a;
      DISPOSE(x);
    ELSE p:=l; l:=l^.next;
    END;
  END;
  NEW(x);
  x^.id:=n;  x^.atom:=a;  x^.fwd:=FALSE;
  x^.next:=labels; labels:=x;
END mark;

PROCEDURE goto(n: INTEGER);
  VAR l,x: goto_ptr; a: atom;
BEGIN
  mcode;
  a:=newatom(_jump); a^.clen:=0; a^.to:=a;
  l:=labels;
  LOOP
    IF l=NIL THEN EXIT END;
    IF l^.id=n THEN
      IF l^.fwd THEN EXIT END;
      a^.to:=l^.atom;
      RETURN
    END;
    l:=l^.next;
  END;
  NEW(x);
  x^.id:=n;  x^.atom:=a;  x^.fwd:=TRUE;
  x^.next:=labels; labels:=x;
END goto;

PROCEDURE check_labels;
  VAR x: goto_ptr;
BEGIN
  WHILE labels#NIL DO
    x:=labels; labels:=x^.next;
    IF x^.fwd THEN
      scan.err_id(14,x^.id);
      x^.atom^.mode:=_dummy;
    END;
    DISPOSE(x);
  END;
END check_labels;

----------------------------------------------------------------

PROCEDURE exit(from: atom);
  VAR a: atom;
BEGIN a:=newatom(_exit); a^.from:=from; a^.exits:=exits; exits:=a;
END exit;

PROCEDURE set_exits(from: atom);
  VAR end,l,p,n: atom;
BEGIN l:=exits; p:=NIL; end:=nextatom();
  WHILE l#NIL DO n:=l^.exits;
    IF l^.from=from THEN
      IF p=NIL THEN exits:=n ELSE p^.exits:=n END;
      l^.mode:=_jump; l^.to:=end; l^.clen:=0; l:=n;
    ELSE p:=l; l:=n;
    END;
  END;
END set_exits;

PROCEDURE return;
  VAR a: atom;
BEGIN mcode; a:=newatom(_return);
END return;

PROCEDURE enterloop;
  VAR a: atom;
BEGIN mcode;
  a:=newatom(_loop); a^.down:=stack; stack:=a;
END enterloop;

PROCEDURE endloop;
  VAR a,loop: atom;
BEGIN
  ASSERT(stack^.mode=_loop);
  loop:=stack; stack:=stack^.down;
  mcode;
  a:=newatom(_jump); a^.clen:=0; a^.to:=loop;
  loop^.mode:=_dummy;
  set_exits(loop);
END endloop;

PROCEDURE exitloop;
  VAR a,loop: atom;
BEGIN
  loop:=stack;
  WHILE (loop#NIL) & (loop^.mode#_loop) DO loop:=loop^.down END;
  IF loop=NIL THEN scan.err(41); RETURN END;
  mcode; exit(loop);
END exitloop;

PROCEDURE enterwhile;
  VAR a: atom;
BEGIN
  mcode;
  a:=newatom(_while); a^.down:=stack; stack:=a;
END enterwhile;

PROCEDURE do(b: INTEGER);
  VAR a: atom;
BEGIN
  ASSERT(stack^.mode=_while);
  IF (b=2) & (ORD('W')-ORD('A') IN scan.opts) THEN logical_not(TRUE) END;
  mcode;
  a:=newatom(_do); a^.val:=b; a^.down:=stack; stack:=a;
END do;

PROCEDURE endwhile;
  VAR a,do,enter: atom;

  PROCEDURE repeat;
    VAR a,free: atom;
  BEGIN
    IF do^.next=NIL THEN                -- пустое тело цикла
      a:=newatom(_dummy);               -- для корректного переноса
    END;
    a:=enter^.next; enter^.next:=do^.next;
    free:=tail^.next;
    WHILE free#NIL DO                   -- на free атомы есть переходы
      tail:=free; tail^.mode:=_dummy; free:=free^.next
    END;
    tail^.next:=a; tail:=do; tail^.next:=NIL;
    enter^.mode:=_jump; enter^.to:=a;   enter^.clen:=0;
    do^.mode:=_jump?; do^.to:=enter^.next; do^.clen:=0;
  END repeat;

  PROCEDURE while;
    VAR a: atom;
  BEGIN
    a:=newatom(_jump); a^.to:=enter;       a^.clen:=0;
    do^.mode:=_jump?; do^.to:=nextatom(); do^.clen:=0;
    enter^.mode:=_dummy;
  END while;

BEGIN
  ASSERT(stack^.mode=_do);
  mcode;
  do:=stack; stack:=stack^.down;
  ASSERT((stack#NIL) & (stack^.mode=_while));
  enter:=stack; stack:=stack^.down;
  IF    do^.val=2 THEN
    IF ORD('W')-ORD('A') IN scan.opts THEN repeat ELSE while END;
  ELSIF do^.val=0 THEN -- WHILE FALSE
-- выкинуть лишнее
    enter^.mode:=_jump; enter^.to:=nextatom(); enter^.clen:=0;
    do^.mode:=_dummy;
  ELSIF do^.val=1 THEN -- WHILE TRUE
    a:=newatom(_jump); a^.clen:=0; a^.to:=enter;
    do^.mode:=_dummy; enter^.mode:=_dummy;
  END;
END endwhile;

PROCEDURE repeat;
  VAR a: atom;
BEGIN
  mcode;
  a:=newatom(_repeat); a^.down:=stack; stack:=a;
END repeat;

PROCEDURE until(b: INTEGER);
  VAR  r,a: atom;
BEGIN
  r:=stack; stack:=stack^.down; r^.mode:=_dummy;
  mcode;
  IF b=1 THEN (* UNTIL TRUE *) RETURN END;
  a:=newatom(_jump); a^.clen:=0; a^.to:=r;
  IF b=2 THEN a^.mode:=_jump? END;
END until;

PROCEDURE if(b: INTEGER);
  VAR a: atom;
BEGIN
  mcode;
  a:=newatom(_if); a^.down:=stack; stack:=a; a^.val:=b;
END if;

PROCEDURE else;
  VAR a: atom;
BEGIN
  ASSERT(stack^.mode=_if);
  mcode;
  a:=newatom(_else); a^.down:=stack; stack:=a;
END else;

PROCEDURE endif;
  VAR end,next,then: atom;
BEGIN
  ASSERT(stack^.mode IN {_if,_else});
  mcode;
  end:=nextatom(); next:=end;
  IF stack^.mode=_else THEN
    next:=stack; stack:=stack^.down;
    next^.mode:=_jump; next^.to:=end; next^.clen:=0;
    next:=next^.next;
  END;
  ASSERT((stack#NIL) &(stack^.mode=_if));
  then:=stack; stack:=stack^.down;
  IF then^.val=1 THEN then^.mode:=_dummy; RETURN END;
  IF then^.val=0 THEN then^.mode:=_jump ELSE then^.mode:=_jump? END;
  then^.to:=next; then^.clen:=0;
END endif;

PROCEDURE entercond(b: INTEGER; and: BOOLEAN);
  VAR a: atom;
BEGIN
  mark_pos;     -- 18-Oct-90
  mcode;
  IF and THEN a:=newatom(_cand) ELSE a:=newatom(_cor) END;
  a^.down:=stack; stack:=a; a^.val:=b;
END entercond;

PROCEDURE exitcond(b: INTEGER; VAR val: INTEGER);
  VAR cond,end: atom;

  PROCEDURE jump(m: INTEGER);
  BEGIN cond^.mode:=m; cond^.to:=end; cond^.clen:=0;
  END jump;

BEGIN
  ASSERT(stack^.mode IN {_cor,_cand});
  val:=2;
  mark_pos;     -- 18-Oct-90
  mcode;
  cond:=stack; stack:=stack^.down; end:=nextatom();
  IF cond^.mode=_cand THEN
    IF cond^.val=2 THEN
      IF    b=2 THEN jump(_and?);
      ELSIF b=1 THEN cond^.mode:=_dummy;
      ELSE           cond^.mode:=_drop;  val:=0;
      END;
    ELSIF cond^.val=0 THEN
      IF b=2 THEN jump(_jump) ELSE cond^.mode:=_dummy END;
      val:=0;
    ELSIF cond^.val=1 THEN cond^.mode:=_dummy;
      IF b#2 THEN val:=b END;
    ELSE ASSERT(FALSE);
    END;
  ELSE
    IF cond^.val=2 THEN
      IF    b=2 THEN jump(_or?);
      ELSIF b=0 THEN cond^.mode:=_dummy;
      ELSE           cond^.mode:=_drop;  val:=1;
      END;
    ELSIF cond^.val=1 THEN
      IF b=2 THEN jump(_jump) ELSE cond^.mode:=_dummy END;
      val:=1;
    ELSIF cond^.val=0 THEN cond^.mode:=_dummy;
      IF b#2 THEN val:=b END;
    ELSE ASSERT(FALSE);
    END;
  END;
  IF val=2 THEN ePush ELSE li(val) END;
END exitcond;

----------------------------  FOR  ----------------------------
                            -------
TYPE
  for_ptr = POINTER TO for_rec;
  for_rec = RECORD
              vaddr: INTEGER;
              bound: INTEGER;
              cfrom: BOOLEAN;        from : INTEGER;
              cto  : BOOLEAN;        to   : INTEGER;
              step : INTEGER;
              next : for_ptr;
            END;

VAR for: for_ptr;

PROCEDURE enterfor(vaddr,baddr: INTEGER);
  VAR new: for_ptr; l,n: INTEGER;
BEGIN
  NEW(new);
  new^.next:=for; for:=new; for^.step:=1;
  for^.vaddr:=vaddr;
  for^.bound:=baddr;
  for^.cfrom:=FALSE; for^.cto:=FALSE;
END enterfor;

PROCEDURE for_from(cons: BOOLEAN; val: INTEGER);
BEGIN
  IF cons THEN for^.from:=val; for^.cfrom:=TRUE;
  ELSE copt;
  END;
  storeword(curlevel,for^.vaddr);
END for_from;

PROCEDURE for_to(cons: BOOLEAN; val: INTEGER);
BEGIN
  IF cons THEN for^.to:=val; for^.cto:=TRUE END;
  IF NOT (for^.cfrom & for^.cto) THEN copt END;
  storeword(curlevel,for^.bound);
END for_to;

PROCEDURE for_step(val: INTEGER);
BEGIN
  for^.step:=val; pop_const;
END for_step;

PROCEDURE for_do;
  VAR a: atom; leq: BOOLEAN;
BEGIN
  IF NOT (for^.cfrom & for^.cto) THEN
    leq:=(for^.step>=0);
    IF for^.cfrom THEN li(for^.from); leq:=NOT leq END;
    IF leq THEN bin_op(scan.leq,0) ELSE bin_op(scan.geq,0) END;
    ePop;
  END;
  mcode;
  a:=newatom(_for); a^.down:=stack; stack:=a;
END for_do;

PROCEDURE endfor(VAR baddr: INTEGER);

  PROCEDURE gen_for;
    VAR up: BOOLEAN; head,a: atom;
  BEGIN
    head:=stack; stack:=stack^.down;
    up:=(for^.step>0);
    IF for^.cfrom & for^.cto THEN
      IF up & (for^.from>for^.to) OR NOT up & (for^.from<for^.to) THEN
        head^.mode:=_jump; head^.clen:=0; head^.to:=nextatom(); RETURN
      ELSE head^.mode:=_dummy
      END;
    END;
    loadword (curlevel,for^.vaddr); cADD(for^.step); copt;
    storeword(curlevel,for^.vaddr); loadword(curlevel,for^.bound);
    IF for^.step>=0 THEN c(cod.gtr) ELSE c(cod.lss) END; ePop;
    mcode;
    a:=newatom(_jump?); a^.to:=head^.next; a^.clen:=0; ePop;
    IF NOT (for^.cfrom & for^.cto) THEN
      head^.mode:=_jump?;  head^.clen:=0; head^.to:=nextatom();
    END;
  END gen_for;

  VAR rem: for_ptr;
BEGIN
  ASSERT(stack^.mode=_for);
  mcode;
  gen_for;
  baddr:=for^.bound;
  rem:=for; for:=for^.next;
  DISPOSE(rem);
END endfor;

----------------------------  CASE  ---------------------------
                            --------

TYPE -- case descriptor
  label_ptr = POINTER TO label_rec;
  label_rec = RECORD
                val : INTEGER;
                alt : atom;
                next: label_ptr;
              END;
  case_ptr  = POINTER TO case_rec;
  case_rec  = RECORD
                const: BOOLEAN;
                val  : INTEGER;
                min  : INTEGER;
                max  : INTEGER;
                else : atom;
                alts : label_ptr;
                table: INTEGER;    -- displacement of case table
                next : case_ptr;
              END;

TYPE -- info for case table correction
  case_tab_ptr  = POINTER TO case_tab_rec;
  case_tab_rec  = RECORD
                    prefix: INTEGER;
                    ofs   : INTEGER;
                    len   : INTEGER;
                    next  : case_tab_ptr;
                  END;

VAR   case: case_ptr;      -- stack of case's
  cur_case: case_ptr;      -- list  of case in current proc
  case_tab: case_tab_ptr;  --

PROCEDURE rem_case(c: case_ptr);
  VAR l,n: label_ptr;
BEGIN l:=c^.alts;
  WHILE l#NIL DO n:=l^.next; DISPOSE(l); l:=n END;
  DISPOSE(c);
END rem_case;

PROCEDURE entercase(cons: BOOLEAN; val: INTEGER; taddr: INTEGER);
  VAR a: atom; new: case_ptr;
BEGIN
  mcode;
  a:=newatom(_case); a^.down:=stack; stack:=a;
  NEW(new);
  new^.else:=NIL; new^.alts:=NIL;
  new^.min:=MAX(INTEGER); new^.max:=MIN(INTEGER);
  new^.table:=taddr;
  new^.const:=cons;
  IF new^.const THEN new^.val:=val END;
  new^.next:=case; case:=new;
  ePop;
END entercase;

PROCEDURE new_label(label: INTEGER);
  VAR l,p,n: label_ptr;
BEGIN
  NEW(n);
  n^.val:=label; n^.alt:=nextatom();
  l:=case^.alts;
  IF l=NIL THEN n^.next:=NIL; case^.alts:=n;
  ELSIF label<l^.val THEN case^.alts:=n; n^.next:=l;
  ELSE p:=NIL;
    WHILE (l#NIL) & (label>l^.val) DO p:=l; l:=l^.next END;
    IF (l#NIL) & (l^.val=label) THEN scan.err(45)
    ELSE n^.next:=l; p^.next:=n;
    END;
  END;
END new_label;

PROCEDURE caselabel(val: INTEGER);
BEGIN
  ASSERT(stack^.mode=_case);
  pop_const;
  IF case^.min>val THEN case^.min:=val END;
  IF case^.max<val THEN case^.max:=val END;
  new_label(val);
END caselabel;

PROCEDURE caserange(v1,v2: INTEGER);
  VAR i: INTEGER;
BEGIN
  ASSERT(stack^.mode=_case);
  pop_const; pop_const;
  IF case^.min>v1 THEN case^.min:=v1 END;
  IF case^.max<v2 THEN case^.max:=v2 END;
  FOR i:=v1 TO v2 DO new_label(i) END;
END caserange;

PROCEDURE exitvariant;
BEGIN
  ASSERT(stack^.mode=_case);
  mcode; exit(stack);
END exitvariant;

PROCEDURE elsecase;
  VAR a: atom;
BEGIN
  ASSERT(stack^.mode=_case);
  mcode; case^.else:=nextatom();
  IF NOT case^.const THEN a:=newatom(_drop) END;
END elsecase;

PROCEDURE endcase;

  PROCEDURE const_case(head: atom; val: INTEGER);
    VAR l: label_ptr;
  BEGIN
    head^.mode:=_jump; head^.clen:=0; head^.to:=case^.else;
    l:=case^.alts;
    WHILE (l#NIL) & (val>l^.val) DO l:=l^.next END;
    IF (l#NIL) & (l^.val=val) THEN head^.to:=l^.alt END;
  END const_case;

  PROCEDURE jump(head: atom);
    VAR a: atom; hi: INTEGER;
  BEGIN ePush; -- case expression
    IF case^.min#0 THEN cADD(-case^.min) END;
    hi:=case^.max-case^.min;
    copt;
    IF CPU>=1 THEN li(hi); c(cod.rchkz); ePop;
    ELSE
      copt; li(0); c(cod.geq); ePop; c(cod.swap);
      li(hi); c(cod.leq); ePop; c(cod.and); ePop;
    END;
    head^.mode:=_mcode; close_seg(head^.segm,head^.len); head^.xpos:=NIL;
    a:=insert_atom(head,_jump?); a^.clen:=0; a^.to:=case^.else; ePop;
    loadword(curlevel,case^.table); c(cod.lxw); ePop;
    IF CPU>=1 THEN c(cod.jump); ePop ELSE stot; c(cod.xit) END;
    a:=insert_atom(a,_mcode); close_seg(a^.segm,a^.len); a^.xpos:=NIL;
  END jump;

  PROCEDURE final;
    VAR c: case_ptr;
  BEGIN c:=case; case:=case^.next;
    IF c^.const THEN rem_case(c)
    ELSE c^.else:=c^.else^.next; -- after drop command
      c^.next:=cur_case; cur_case:=c
    END;
  END final;

  VAR head: atom;
BEGIN
  ASSERT(stack^.mode IN {_case,_else});
  IF case^.else=NIL THEN elsecase; raise(45h) END;
  mcode;
  head:=stack; stack:=stack^.down;
  IF case^.alts=NIL THEN scan.err(72); RETURN END;
  IF case^.max-case^.min>=256 THEN scan.err(10); RETURN END;
  IF case^.const THEN const_case(head,case^.val);
  ELSE jump(head);
  END;
  set_exits(head);
  final;
END endcase;

PROCEDURE flush_case_list;

  PROCEDURE new_table(len: INTEGER);
    VAR tab: case_tab_ptr;
  BEGIN
    NEW(tab);
    tab^.ofs:=pool_ofs; tab^.prefix:=-1; tab^.len:=len;
    tab^.next:=case_tab; case_tab:=tab;
  END new_table;

  PROCEDURE flush_case(c: case_ptr);
    VAR else,ofs,i: INTEGER; l,n: label_ptr;
  BEGIN ASSERT(NOT c^.const);
    new_table(c^.max-c^.min+1); ofs:=pool_ofs;
    l:=c^.alts; else:=c^.else^.pos;
    WHILE l#NIL DO
      app_word(l^.alt^.pos); n:=l^.next;
      IF n#NIL THEN
        FOR i:=l^.val+1 TO n^.val-1 DO app_word(else) END;
      END; l:=n;
    END;
    lsta(ofs); storeword(curlevel,c^.table);
  END flush_case;

  VAR c: case_ptr; seg,len,prefix: INTEGER; t: case_tab_ptr;
BEGIN
  WHILE cur_case#NIL DO
    c:=cur_case; cur_case:=c^.next;
    flush_case(c); rem_case(c);
  END;
  close_seg(seg,len); save_seg(seg,len);
  prefix:=out_code_pos();
  t:=case_tab;
  WHILE (t#NIL) & (t^.prefix<0) DO t^.prefix:=prefix; t:=t^.next END;
END flush_case_list;

PROCEDURE correct_case_tables(no_proc: INTEGER);
  VAR t,n: case_tab_ptr;
BEGIN t:=case_tab;
  WHILE t#NIL DO n:=t^.next;
    inc_table(t^.ofs,t^.len,t^.prefix+no_proc*4);
    DISPOSE(t); t:=n;
  END; case_tab:=NIL;
END correct_case_tables;

--------------------------------------------------------------

PROCEDURE assert(b: INTEGER; one: BOOLEAN);
  VAR a: atom;
BEGIN
  IF b=2 THEN a:=newatom(_jump?); a^.clen:=0 END;
  IF b#1 THEN
    IF one THEN raise(48h) ELSE c(cod.trap) END;
  END;
  IF b=2 THEN mcode; a^.to:=nextatom() END;
  set_depth(0); -- как я устал с ней (глубиной) бороться
END assert;

--------------------------------------------------------------

PROCEDURE clean_atoms;
  VAR l,d: atom; curs: INTEGER;
BEGIN l:=list;
  WHILE l#NIL DO
    IF l^.pos<0 THEN
      IF l^.mode=_mcode THEN rem_xpos(l^.xpos) END; l^.mode:=_dummy
    END;
    l:=l^.next
  END;
  curs:=-1; l:=list;
  WHILE (l#NIL) & (l^.mode=_dummy) DO
    d:=l; l:=l^.next; DISPOSE(d);
  END;
  ASSERT(l#NIL);
  list:=l; tail:=l;
  REPEAT
    INC(curs); l^.pos:=curs; l:=l^.next;
    WHILE (l#NIL) & (l^.mode=_dummy) DO
      d:=l; l:=l^.next; DISPOSE(d);
    END;
    tail^.next:=l;
    IF l#NIL THEN tail:=l END;
  UNTIL l=NIL;
END clean_atoms;

PROCEDURE access_atoms;

  PROCEDURE jump(a: atom); FORWARD;

  PROCEDURE scan_list(a,lim: atom);
    VAR n: atom;
  BEGIN a^.pos:=ABS(a^.pos);
    WHILE a#lim DO ASSERT(a#NIL);
      n:=a^.next;
      IF a^.pos>0 THEN
        CASE a^.mode OF
          |_return      :
          |_jump        : jump(a)
          |_jump?.._or? : jump(a); n^.pos:=ABS(n^.pos)
          |_mcode,_dummy,_drop:    n^.pos:=ABS(n^.pos)
        ELSE ASSERT(FALSE,100h+ORD(a^.mode));
        END;
      END;
      a:=n;
    END;
  END scan_list;

  PROCEDURE jump(a: atom);
    VAR t,x: atom;
  BEGIN
    t:=a^.to;
    WHILE t^.mode=_dummy DO t:=t^.next END;
    a^.to:=t;
    IF (a^.mode=_and?) & (t^.mode=_jump?) THEN
      a^.mode:=_jump?; t:=t^.to;
      WHILE t^.mode=_dummy DO t:=t^.next END;
      a^.to:=t;
    END;
    IF a^.mode IN {_jump,_jump?} THEN
      LOOP
        IF (t=a) OR (t^.mode#_jump) THEN EXIT END;
        x:=t^.to;
          WHILE x^.mode=_dummy DO x:=x^.next END;
        t^.to:=x;
        IF t=x THEN EXIT END;
        t:=t^.to;
      END;
      IF (t^.mode=_return) & (a^.mode=_jump) THEN a^.mode:=_return; RETURN END;
      a^.to:=t;
    END;
    IF (t^.pos<0) & (ABS(a^.pos)>ABS(t^.pos)) THEN scan_list(t,a)
    ELSE t^.pos:=ABS(t^.pos);
    END;
  END jump;

  PROCEDURE variant(VAR t: atom);
    VAR x: atom;
  BEGIN
    WHILE t^.mode=_dummy DO t:=t^.next END;
    LOOP
      IF t^.mode#_jump THEN EXIT END;
      x:=t^.to;
      WHILE x^.mode=_dummy DO x:=x^.next END;
      t^.to:=x;
      IF t=x THEN EXIT END;
      t:=x;
    END;
    t^.pos:=ABS(t^.pos);
  END variant;

  VAR a: atom; i: INTEGER; c: case_ptr; l: label_ptr;
BEGIN a:=list; i:=-1;
  WHILE a#NIL DO a^.pos:=i; DEC(i); a:=a^.next END;
  c:=cur_case;
  WHILE c#NIL DO
    variant(c^.else); l:=c^.alts;
    WHILE l#NIL DO variant(l^.alt); l:=l^.next END;
    c:=c^.next;
  END;
  scan_list(list,NIL);
END access_atoms;

PROCEDURE fool_jumps;
  CONST condjumps = {_jump?,_jumpT};
  VAR a,b,n: atom; i: INTEGER; done: BOOLEAN;
BEGIN a:=list; b:=NIL; i:=0;
  WHILE a#NIL DO
    REPEAT done:=TRUE;
      n:=a^.next;
      IF a^.mode IN jumps THEN
        IF n=a^.to THEN
          IF a^.mode IN jumps? THEN a^.mode:=_drop;
          ELSE -- выкинуть переход на следующию команду
            DISPOSE(a); done:=FALSE;
            IF b=NIL THEN list:=n; a:=list ELSE a:=n; b^.next:=n END;
          END;
        ELSIF (a^.mode IN condjumps) & (n^.mode=_jump) & (a^.to=n^.next) THEN
          a^.to:=n^.to; a^.next:=n^.next;
          IF a^.mode=_jump? THEN a^.mode:=_jumpT ELSE a^.mode:=_jump? END;
          DISPOSE(n);
          n:=a; done:=FALSE;
        END;
      END;
    UNTIL done;
    a^.pos:=i;
    CASE a^.mode OF
      |_mcode       : INC(i,a^.len)
      |_return,_drop: INC(i);
      |_and?,_or?   : INC(i,2);
      |_jump,_jump? : INC(i,2);
      |_jumpT       : INC(i,3);
    ELSE ASSERT(FALSE);
    END;
    b:=a; a:=n;
  END;
END fool_jumps;

PROCEDURE eval_jumps_len;

  PROCEDURE inc_len(a: atom);
    VAR l: atom; pos: INTEGER;
  BEGIN
    l:=list; pos:=a^.pos;
    WHILE l#NIL DO
      IF (l^.mode IN jumps) & ((l^.pos< pos) & (l^.to^.pos>pos)
                            OR (l^.pos>=pos) & (l^.to^.pos<pos))
      THEN
        INC(l^.clen);
        IF l^.clen=256 THEN inc_len(l) END;
      END;
      l:=l^.next;
    END;
  END inc_len;

  VAR a: atom; add: INTEGER;
BEGIN a:=list;
  WHILE a^.next#NIL DO
    IF a^.mode IN jumps THEN
      INC(a^.clen,ABS(a^.to^.pos-a^.next^.pos));
      IF a^.clen>255 THEN inc_len(a) END;
    END;
    a:=a^.next;
  END;
  IF a^.mode#_return THEN ASSERT(a^.mode=_jump);
    INC(a^.clen,a^.pos+2-a^.to^.pos);
    IF a^.clen>255 THEN INC(a^.clen) END;
  END;
  IF cur_case=NIL THEN RETURN END;
  a:=list; add:=0;
  WHILE a#NIL DO INC(a^.pos,add);
     IF (a^.mode IN jumps) & (a^.clen>255) THEN INC(add) END;
     a:=a^.next
  END;
END eval_jumps_len;

---------------------------------------------------------------

TYPE
  cont_ptr = POINTER TO cont_rec;
  cont_rec = RECORD
               segm : INTEGER;
               list : atom;
               tail : atom;
               stack: atom;
               exits: atom;
               cases: case_ptr;
               xpos : xpos_ptr;
               next : cont_ptr;
             END;

VAR cont: cont_ptr;

PROCEDURE ini_structs;
BEGIN
  stack:=NIL; list:=NIL; tail:=NIL; exits:=NIL; xpos:=NIL;
  for:=NIL;   case:=NIL; cur_case:=NIL;
  labels:=NIL;
END ini_structs;

PROCEDURE enter_proc;
  VAR new: cont_ptr;
BEGIN
  mcode;
  NEW(new);
  new^.segm:=cur_segm;
  new^.list:=list; new^.tail:=tail; new^.stack:=stack;
  new^.exits:=exits; new^.cases:=cur_case; new^.xpos:=xpos;
  new^.next:=cont; cont:=new;
  INC(curlevel);
  ini_structs;
END enter_proc;

PROCEDURE exit_proc(p_no,start: INTEGER; put_xref: PUT_XREF);

  PROCEDURE final;
    VAR a,d: atom; c: case_ptr; x: cont_ptr;
  BEGIN a:=list;
    WHILE a#NIL DO d:=a; a:=a^.next; DISPOSE(d) END;
    WHILE cur_case#NIL DO c:=cur_case; cur_case:=c^.next; rem_case(c) END;
    IF p_no#0 THEN
      cur_segm:=cont^.segm;
      list:=cont^.list; tail:=cont^.tail; stack:=cont^.stack;
      exits:=cont^.exits; cur_case:=cont^.cases; xpos:=cont^.xpos;
      x:=cont; cont:=cont^.next;
      DISPOSE(x);
      cur_pos:=cur_segm;
      DEC(curlevel);
    END;
  END final;

  PROCEDURE out_xpos(l: xpos_ptr; proc_no,entry: INTEGER);
    VAR n: xpos_ptr;
  BEGIN
    WHILE l#NIL DO n:=l^.next;
      put_xref(proc_no,l^.pc+entry,l^.line,l^.col);
      DISPOSE(l); l:=n
    END;
  END out_xpos;

  VAR a: atom; entry: INTEGER;
BEGIN
  IF scan.noErrors#0 THEN final; RETURN END;
  check_labels;
  access_atoms; clean_atoms; fool_jumps; eval_jumps_len;
  IF cur_case#NIL THEN flush_case_list END;
  entry:=out_code_pos()-start;
  a:=list;
  WHILE a#NIL DO
    CASE a^.mode OF
      |_jumpT      : save_byte(cod.not);
         save_jump(a^.mode=_jump,a^.clen<=255,a^.to^.pos<=a^.pos,a^.clen);
      |_jump,_jump?:
         save_jump(a^.mode=_jump,a^.clen<=255,a^.to^.pos<=a^.pos,a^.clen);
      |_and?       : save_byte(cod.andjp);
                     IF a^.clen>255 THEN scan.err(33); a^.clen:=0 END;
                     save_byte(a^.clen);
      |_or?        : save_byte(cod.orjp);
                     IF a^.clen>255 THEN scan.err(33); a^.clen:=0 END;
                     save_byte(a^.clen);
      |_drop       : save_byte(cod.drop);
      |_return     : save_byte(cod.rtn);
      |_mcode      : save_seg(a^.segm,a^.len);
                     out_xpos(a^.xpos,p_no,a^.pos+entry);
    ELSE ASSERT(FALSE);
    END;
    a:=a^.next;
  END;
  final;
  ASSERT(xpos=NIL);
END exit_proc;

------------------------  INI & EXI  -------------------------
                        -------------

PROCEDURE Ini;
BEGIN
  depth:=0; curlevel:=0;
  ini_code_buf;
  NEW(code_bodies);
  code_pos:=0;
  ini_pool;
  cont:=NIL; case_tab:=NIL; ini_structs;
END Ini;

PROCEDURE Exi;
BEGIN
  exi_pool;
  DISPOSE(code_bodies);
  exi_code_buf;
END Exi;

PROCEDURE set_cpu;
  PROCEDURE checkm;  CODE cod.getm cod.setm END checkm;
  PROCEDURE tags(): BITSET;  CODE cod.llw 2 END tags;
  PROCEDURE _cpu(): INTEGER; CODE cod.sys 0 END _cpu;
  PROCEDURE _cpm(): INTEGER; CODE cod.sys 2 END _cpm;
BEGIN
  checkm;
  IF    NOT (30 IN tags())       THEN CPU:=2
  ELSIF (_cpu()=5) & (_cpm()=15) THEN CPU:=1
  ELSE                                CPU:=0
  END;
END set_cpu;

BEGIN
  set_cpu;
END mxCmd.
