IMPLEMENTATION MODULE coIO; (*$N+ Ned 04-Mar-90. (c) KRONOS *)

IMPORT   sys: SYSTEM;
IMPORT        ASCII;
IMPORT  comp: coDefs;
IMPORT   str: Strings;
IMPORT   bio: BIO;
IMPORT   lex: Lexicon;
IMPORT   reg: regExpr;
IMPORT   mem: Heap;

WITH STORAGE: mem;

CONST ok = 0;

TYPE
  text_ptr = POINTER TO
               RECORD
                 f  : bio.FILE;
                 buf: STRING;
                 pos: INTEGER;
                 lim: INTEGER;
                 eof: INTEGER;
               END;
  out_ptr  = POINTER TO out_rec;
  out_rec  = RECORD
               expr: reg.EXPR;
               dir : bio.FILE;
               next: out_ptr;
             END;

VAR
  sym: bio.PATHs;
  txt: bio.PATHs;
  out: out_ptr;

PROCEDURE check(VAL s: ARRAY OF CHAR; io: comp.io_ptr);
  VAR msg: ARRAY [0..127] OF CHAR;
BEGIN
  io^.done:=bio.done;
  IF NOT io^.done THEN
    lex.perror(msg,bio.error,'"%s": %%s',s);
    io^.print('%s',msg)
  END
END check;

PROCEDURE next_block(t: text_ptr): BOOLEAN;
  VAR len: INTEGER;
BEGIN
  len:=t^.eof;
  IF len<=0 THEN RETURN TRUE END;
  IF len>4096 THEN len:=4096 END;
  bio.get(t^.f,t^.buf,len);
  t^.lim:=len; DEC(t^.eof,len); t^.pos:=0;
  RETURN FALSE
END next_block;

PROCEDURE get_str(io: comp.io_ptr);
  VAR t: text_ptr; ch: CHAR; i: INTEGER;
BEGIN
  t:=io^.exts;
  i:=0;
  LOOP
    (*$<T-*)
    IF t^.pos>=t^.lim THEN
      IF next_block(t) THEN io^.done:=FALSE; RETURN END;
      check('',io);
    END;
    ch:=t^.buf[t^.pos]; INC(t^.pos);
    IF ch=ASCII.NL THEN EXIT END;
    io^.buf[i]:=ch; INC(i);
    IF i>=HIGH(io^.buf) THEN RESIZE(io^.buf,i+16) END;
    (*$>*)
  END;
  io^.buf[i]:=0c;
  io^.len:=i;
  io^.done:=TRUE;
END get_str;

PROCEDURE open_text(VAL name: ARRAY OF CHAR; io: comp.io_ptr);
  VAR f: bio.FILE; t: text_ptr; len: INTEGER;
BEGIN
  io^.doio:=get_str;
  bio.lookup(txt,f,name,'r');
  check(name,io);
  IF io^.done THEN
    NEW(t); io^.exts:=t;
    t^.f:=f;
    t^.eof:=bio.eof(t^.f);
    IF t^.eof>4096 THEN len:=4096 ELSE len:=t^.eof END;
    NEW(t^.buf,len);
    t^.pos:=0; t^.lim:=-1;
    NEW(io^.buf,256);
  END;
END open_text;

PROCEDURE no_op(io: comp.io_ptr);
BEGIN ASSERT(FALSE);
END no_op;

PROCEDURE open_sym(VAL name: ARRAY OF CHAR; io: comp.io_ptr);
  VAR f: bio.FILE;
BEGIN
  io^.doio:=no_op;
  bio.lookup(sym,f,name,'r');
  check(name,io);
  IF io^.done THEN
    NEW(io^.buf,bio.eof(f));
    bio.get(f,io^.buf,BYTES(io^.buf));
    io^.len:=BYTES(io^.buf);
    check(name,io);
  END;
  bio.close(f);
  IF io^.done THEN check(name,io) END;
END open_sym;

PROCEDURE write(io: comp.io_ptr);
  VAR f: bio.FILE;
BEGIN
  IF NOT io^.done THEN RETURN END;
  f:=bio.FILE(io^.exts);
  bio.put(f,io^.buf,io^.len);
  check('',io);
END write;

PROCEDURE create(VAL name,mode: ARRAY OF CHAR; io: comp.io_ptr);
  VAR f: bio.FILE; x: out_ptr;
BEGIN
  io^.doio:=write;
  x:=out;
  WHILE (x#NIL) & NOT reg.match(x^.expr,name,0) DO x:=x^.next END;
  IF x=NIL THEN
    bio.create(f,name,mode,1);
  ELSE
    bio.fcreate(x^.dir,f,name,mode,1);
  END;
  check(name,io);
  io^.exts:=sys.ADDRESS(f);
END create;

PROCEDURE ini(VAR io  : comp.io_ptr;
              VAL name: ARRAY OF CHAR;
                  kind: INTEGER;
                 print: comp.PRINT);
  VAR s: ARRAY [0..255] OF CHAR;
BEGIN
  NEW(io);
  NEW(io^.buf);
  io^.exts :=NIL;
  io^.kind :=kind;
  io^.print:=print;
  CASE kind OF
    |comp.def   : str.print(s,'%s.d',name);
                  open_text(s,io);
    |comp.imp
    ,comp.main  : str.print(s,'%s.m',name);
                  open_text(s,io);
    |comp.text  : open_text(name,io);
    |comp.sym_in: str.print(s,'%s.sym',name);
                  open_sym(s,io);
    |comp.sym_ou: str.print(s,'%s.sym',name);
                  create(s,'hw',io);
    |comp.ref   : str.print(s,'%s.ref',name);
                  create(s,'hw',io);
    |comp.code  : str.print(s,'%s.cod',name);
                  create(s,'hw',io);
    |comp.mcode : str.print(s,'%s.cod',name);
                  create(s,'w',io);
  ELSE ASSERT(FALSE);
  END;
END ini;

PROCEDURE exi(VAR io: comp.io_ptr);
  VAR f: bio.FILE; t: text_ptr;
BEGIN
  IF io^.kind IN {comp.def,comp.imp,comp.main} THEN
    t:=io^.exts;
    IF t#NIL THEN
      bio.close(t^.f);  check('',io);
      DISPOSE(t^.buf);  DISPOSE(t);    DISPOSE(io^.buf)
    END
  ELSIF io^.kind IN {comp.sym_ou,comp.ref,comp.code,comp.mcode} THEN
    IF io^.exts#NIL THEN
      f:=bio.FILE(io^.exts);
      IF io^.done & (io^.kind IN {comp.code,comp.mcode}) THEN
        bio.chaccess(f,bio.cmask); check('',io)
      END;
      IF io^.done THEN bio.close(f) ELSE bio.purge(f) END;
      check('',io)
    END
  ELSIF io^.kind=comp.sym_in THEN
    DISPOSE(io^.buf)
  END;
  DISPOSE(io)
END exi;

PROCEDURE dispose;
  VAR x: out_ptr;
BEGIN
  IF sym#bio.here THEN bio.close_paths(sym) END;
  WHILE out#NIL DO
    x:=out; out:=x^.next;
    reg.dispose(x^.expr);
    bio.close(x^.dir);
    DISPOSE(x);
  END;
END dispose;

PROCEDURE set(VAL text_path,sym_path,spec: ARRAY OF CHAR; print: comp.PRINT);

  PROCEDURE perror(err: INTEGER; VAL s: ARRAY OF CHAR);
    VAR msg: ARRAY [0..127] OF CHAR;
  BEGIN
    lex.perror(msg,err,'WARNING: "%s" %%s',s);
    print('%s',msg);
  END perror;

  PROCEDURE make(VAL expr,path: ARRAY OF CHAR);
    VAR r: reg.EXPR; f: bio.FILE; x: out_ptr;
  BEGIN
    reg.compile(expr,r);
    IF NOT reg.done THEN perror(reg.error,expr); RETURN END;
    bio.open(f,path,'rw');
    IF NOT bio.done THEN perror(bio.error,path); RETURN END;
    NEW(x);
    x^.expr:=r; x^.dir:=f;
    x^.next:=out; out:=x;
  END make;

  PROCEDURE open(VAR path: bio.PATHs; VAL s: ARRAY OF CHAR): BOOLEAN;
  BEGIN
    IF (HIGH(s)<0) OR (s[0]=0c) THEN RETURN FALSE END;
    bio.open_paths(path,s);
    IF NOT bio.done THEN
      perror(bio.error,s); RETURN FALSE
    END;
    RETURN TRUE
  END open;

  VAR i,n: INTEGER;
    expr,path: ARRAY [0..255] OF CHAR;
BEGIN
  dispose;
  IF NOT open(sym,sym_path ) THEN sym:=bio.here END;
  IF NOT open(txt,text_path) THEN txt:=bio.here END;
  i:=0;
  LOOP
    str.skip(spec,i,' ');
    n:=i;
    str.search(spec,i,'=');
    IF (i>HIGH(spec)) OR (spec[i]#'=') THEN EXIT END;
    str.sub_str(expr,spec,n,i-n);
    n:=i+1;
    str.search(spec,i,' ');
    str.sub_str(path,spec,n,i-n);
    IF path='' THEN EXIT END;
    make(expr,path);
    IF (i>HIGH(spec)) OR (spec[i]#' ') THEN EXIT END;
    INC(i);
  END;
END set;

BEGIN
  sym:=bio.here;
  txt:=bio.here;
  out:=NIL;
END coIO.
