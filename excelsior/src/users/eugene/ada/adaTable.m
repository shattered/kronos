IMPLEMENTATION MODULE adaTable; (* 05-Apr-89. (c) KRONOS *)

IMPORT  lex : adaLex;

FROM SYSTEM     IMPORT  ADR, ADDRESS, WORD;
FROM adaLex     IMPORT  val, lex_pos, error, sy, next_sy;
FROM Heap       IMPORT  ALLOCATE, DEALLOCATE;

IMPORT mCodeMnem, BIO, FsPublic, Image;

FROM Terminal   IMPORT  print;

TYPE
  for_rec=RECORD
    ref : POINTER TO item;
    host: item;
    no  : INTEGER;
  END;

VAR
  tree   : name;
  for_wsp: ARRAY [0..39] OF for_rec;
  for_cnt: INTEGER;

PROCEDURE move(a,b: ADDRESS; c: INTEGER); CODE mCodeMnem.move END move;

PROCEDURE def_name(): name;
  VAR
    i,w: INTEGER;
    s  : POINTER TO ARRAY [0..0FFh] OF INTEGER;
    n,t: name;
    at : POINTER TO name;
BEGIN
  i:=val.len;
  REPEAT val.str[i]:=0c; INC(i) UNTIL (i MOD 4)=0;
  i:=i DIV 4; s:=ADR(val.str);
  ASSERT(i>0);
  t:=tree; at:=ADR(tree);
  WHILE t#NIL DO
    w:=0;
    LOOP
      IF (t^.len<=w) OR (t^.val[w]<s^[w]) THEN
        at:=ADR(t^.fwd); t:=t^.fwd; EXIT;
      ELSIF t^.val[w]>s^[w] THEN
        at:=ADR(t^.bck); t:=t^.bck; EXIT;
      ELSE
        INC(w);
        IF w=i THEN
          IF i=t^.len THEN RETURN t END;
          at:=ADR(t^.bck); t:=t^.bck; EXIT;
        END;
      END;
    END;
  END;
  ALLOCATE(n,4+i); at^:=n;
  IF n=NIL THEN error(lex_pos,'не хватило памяти%.') END;
  n^.fwd:=NIL; n^.bck:=NIL; n^.obj:=NIL; n^.len:=i;
  move(ADR(n^.val),s,i);
  RETURN n;
END def_name;

PROCEDURE find(): name;
  VAR
    i,w: INTEGER;
    s  : POINTER TO ARRAY [0..0FFh] OF INTEGER;
    t  : name;
BEGIN
  i:=val.len;
  REPEAT val.str[i]:=0c; INC(i) UNTIL (i MOD 4)=0;
  i:=i DIV 4; s:=ADR(val.str);
  ASSERT(i>0);
  t:=tree;
  WHILE t#NIL DO
    w:=0;
    LOOP
      IF (t^.len<=w) OR (t^.val[w]<s^[w]) THEN t:=t^.fwd; EXIT;
      ELSIF t^.val[w]>s^[w] THEN t:=t^.bck; EXIT;
      ELSE
        INC(w);
        IF w=i THEN
          IF i=t^.len THEN RETURN t END;
          t:=t^.bck; EXIT;
        END;
      END;
    END;
  END;
  RETURN NIL;
END find;

PROCEDURE find_nm(str: ARRAY OF CHAR): name;
  VAR
    i,w: INTEGER;
    s  : POINTER TO ARRAY [0..0FFh] OF INTEGER;
    t  : name;
BEGIN
  i:=0;
  WHILE str[i]#0c DO INC(i) END;
  (*$T-*)
  REPEAT str[i]:=0c; INC(i) UNTIL (i MOD 4)=0;
  (*$T+*)
  i:=i DIV 4; s:=ADR(str);
  ASSERT(i>0);
  t:=tree;
  WHILE t#NIL DO
    w:=0;
    LOOP
      IF (t^.len<=w) OR (t^.val[w]<s^[w]) THEN t:=t^.fwd; EXIT;
      ELSIF t^.val[w]>s^[w] THEN t:=t^.bck; EXIT;
      ELSE
        INC(w);
        IF w=i THEN
          IF i=t^.len THEN RETURN t END;
          t:=t^.bck; EXIT;
        END;
      END;
    END;
  END;
  RETURN NIL;
END find_nm;

PROCEDURE tie_item(n: name; i: item);
BEGIN
  i^.nm:=n; i^.nm_fwd:=n^.obj; n^.obj:=i;
END tie_item;

PROCEDURE def_item(c: class): item;
  VAR i: item; j: INTEGER;
BEGIN
  ALLOCATE(i,SIZE(i^));
  IF i=NIL THEN error(lex_pos,'не хватило памяти%.') END;
  i^.nm:=NIL;
  i^.nm_fwd:=NIL;
  i^.host:=NIL;
  i^.host_nxt:=NIL;
  i^.host_cnt:=-1;
  i^.md:=c;
  i^.lex_pos:=lex_pos;
  CASE c OF
    |cl_operator:
      i^.op_lex:=lex.null;
      i^.op_dw_no:=0;
      i^.op_val0:=0;
      i^.op_list:=NIL;
    |cl_enumeration:
      i^.en_bits:=0;
      i^.en_signed:=FALSE;
      i^.en_universal:=FALSE;
    |cl_integer:
      i^.in_bits:=0;
      i^.in_signed:=FALSE;
      i^.in_universal:=FALSE;
    |cl_float:
      i^.fl_bits  :=0;
      i^.fl_digits:=0;
      i^.fl_delta :=0;
      i^.fl_universal:=FALSE;
    |cl_array:
      i^.ar_elemtp  :=NIL;
      FOR j:=0 TO HIGH(i^.ar_index) DO i^.ar_index[j]:=NIL END;
      i^.ar_index_no:=0;
      i^.ar_elembits:=0;
    |cl_record:
      i^.re_fields:=NIL;
--      i^.re_descr
      i^.re_descr_no:=0;
      i^.re_bits:=0;
    |cl_field:
      i^.fi_type:=NIL;
      i^.fi_nxt:=NIL;
      i^.fi_bits:=0;
      i^.fi_offs:=0;
      i^.fi_descr:=0;
      i^.fi_case:=0;
    |cl_access:
      i^.ac_type:=NIL;
      i^.ac_universal:=FALSE;
    |cl_task:
    |cl_var:
      i^.va_type:=NIL;
      i^.va_const:=FALSE;
      i^.va_mem:=FALSE;
      i^.va_sz:=0;
      i^.va_rl:=0;
      i^.va_val0:=0;
      i^.va_val1:=0;
      i^.va_val2:=NIL;
    |cl_func:
      i^.fu_import:=FALSE;
      i^.fu_type:=NIL;
      i^.fu_list:=NIL;
      i^.fu_prm:=NIL;
      i^.fu_prm_no:=0;
      i^.fu_var_no:=0;
      i^.fu_reg_no:=0;
      i^.fu_rl:=0;
      i^.fu_val0:=0;
      i^.fu_val1:=0;
      i^.fu_nxt:=NIL;
      i^.fu_inline:=FALSE;
    |cl_prm:
      i^.pr_type:=NIL;
      i^.pr_nxt:=NIL;
      i^.pr_in:=FALSE;
      i^.pr_out:=FALSE;
      i^.pr_bits:=0;
      i^.pr_rl:=0;
      i^.pr_val0:=0;
    |cl_pack:
      i^.pa_import:=FALSE;
      i^.pa_ref:=FALSE;
      i^.pa_list:=NIL;
      i^.pa_use:=NIL;
      i^.pa_uni_int:=NIL;
      i^.pa_uni_flo:=NIL;
      i^.pa_uni_enu:=NIL;
      i^.pa_uni_acc:=NIL;
      i^.pa_var_no:=0;
      i^.pa_priv:=NIL;
      i^.pa_body:=NIL;
      i^.pa_var_no:=0;
    |cl_use:
      i^.us_pack:=NIL;
      i^.us_nxt:=NIL;
    |cl_subtype:
      i^.su_type :=NIL;
      i^.su_first:=0;
      i^.su_last :=0;
      FOR j:=0 TO HIGH(i^.su_descr) DO i^.su_descr[j]:=NIL END;
      i^.su_private:=FALSE;
      i^.su_limited:=FALSE;
    |cl_type:
      i^.ty_type:=NIL;
  END;
  RETURN i;
END def_item;

PROCEDURE clear_table;
  PROCEDURE cl(VAR nm: name);
  BEGIN
    IF nm=NIL THEN RETURN END;
    cl(nm^.bck); cl(nm^.fwd);
    DEALLOCATE(nm,nm^.len+INTEGER(ADR(nm^.val))-INTEGER(nm));
  END cl;
BEGIN
  cl(tree);
END clear_table;

PROCEDURE write_lib(lib: item);
(*
  VAR
    blk  : INTEGER;
    pos  : INTEGER;
    buf  : ARRAY [0..1023] OF INTEGER;
    f    : FsPublic.File;
    fname: FsPublic.FileName;
  PROCEDURE chk(r: BOOLEAN);
    VAR err  : ARRAY [0..79] OF CHAR;
  BEGIN
    IF r THEN
      FsPublic.VisFSerr(r,err);
      error(16,lex_pos,err); HALT(1);
    END;
  END chk;
  PROCEDURE put(n: WORD);
  BEGIN
    IF pos=1024 THEN
      chk(BIO.bWrite(f,blk,ADR(buf),4096));
      INC(blk); pos:=0;
    END;
    buf[pos]:=n; INC(pos);
  END put;
  PROCEDURE put_ref(i,level: item);
    VAR a: ADDRESS; n: INTEGER; h: item;
  BEGIN
    IF i=NIL THEN put(-2); RETURN END;
    h:=i^.host;
    IF (h=NIL) & (i=lib) THEN
      put(-3);
    ELSIF h=NIL THEN
      put(-4);
      ASSERT(i^.nm#NIL);
      a:=ADR(i^.nm^.val);
      put(i^.nm^.len);
      FOR n:=0 TO i^.nm^.len-1 DO put(a^); INC(a) END;
    ELSIF (h^.host=NIL) & (h#lib) THEN
      -- other lib
      put(-1);
      ASSERT(h^.nm#NIL);
      a:=ADR(h^.nm^.val);
      put(h^.nm^.len);
      FOR n:=0 TO h^.nm^.len-1 DO put(a^); INC(a) END;
      put(i^.host_cnt);
    ELSE
      n:=0;
      WHILE h#level DO
        INC(n); level:=level^.host; ASSERT(level#NIL);
      END;
      put(n);
      put(i^.host_cnt);
    END;
  END put_ref;
  PROCEDURE write_item(i: item);
    VAR a: ADDRESS; n: INTEGER; t: item;
  BEGIN
    put(i^.md);
    IF i^.nm=NIL THEN
      put(0);
    ELSE
      a:=ADR(i^.nm^.val);
      put(i^.nm^.len);
      FOR n:=0 TO i^.nm^.len-1 DO put(a^); INC(a) END;
    END;
    put(i^.host_cnt);
    put(i^.private);
    CASE i^.md OF
      |cl_enumeration:
        put(i^.en_char);
        put(i^.en_universal);
      |cl_integer:
        put(i^.in_byte);
        put(i^.in_universal);
      |cl_float:
        put(i^.fl_digits);
        put(i^.fl_delta);
        put(i^.fl_universal);
      |cl_array:
        put_ref(i^.ar_elemtp,i^.host);
        put_ref(i^.ar_index,i^.host);
      |cl_record:
        put_ref(i^.re_fields,i^.host);
      |cl_field:
        put_ref(i^.fi_type,i^.host);
        put_ref(i^.fi_nxt,i^.host);
        put(i^.fi_val0);
      |cl_access:
        put_ref(i^.ac_type,i^.host);
      |cl_task:
      |cl_var:
        put_ref(i^.va_type,i^.host);
        put(i^.va_rl);
        put(i^.va_val0);
        put(i^.va_val1);
        put(i^.va_val2);
      |cl_func:
        ASSERT(NOT i^.fu_import);
        put_ref(i^.fu_type,i^.host);
        t:=i^.fu_list;
        WHILE t#NIL DO
          ASSERT(t^.host=i);
          put(1); write_item(t); t:=t^.host_nxt;
        END;
        put(0);
        put_ref(i^.fu_prm,i);
        put(i^.fu_rl);
        put(i^.fu_val0);
        put(i^.fu_val1);
        --i^.fu_nxt
      |cl_prm:
        put_ref(i^.pr_type,i^.host);
        put_ref(i^.pr_nxt,i^.host);
        put(i^.pr_in);
        put(i^.pr_out);
        put(i^.pr_val0);
      |cl_pack:
        ASSERT(NOT i^.pa_import);
        t:=i^.pa_list;
        WHILE t#NIL DO
          ASSERT(t^.host=i);
          put(1); write_item(t); t:=t^.host_nxt;
        END;
        put(0);
        --i^.pa_use
        put(i^.pa_var_no);
      |cl_use:
        put_ref(i^.us_pack,i^.host);
      |cl_subtype:
        put_ref(i^.su_type,i^.host);
        put(i^.su_first);
        put(i^.su_last);
        put(i^.su_private);
        put(i^.su_limited);
      |cl_type:
        put_ref(i^.ty_type,i^.host);
    END;
  END write_item;
BEGIN
  ASSERT(lib^.nm#NIL);
  ASSERT(lib^.host=NIL);
  ASSERT(lib^.md IN library_class);
  Image.image0(fname,'%s.sym',lib^.nm^.val);
  chk(BIO.Create(f));
  blk:=0; pos:=0;
  write_item(lib);
  BIO.SetEof(f,(blk*1024+pos)*4);
  IF pos>0 THEN chk(BIO.bWrite(f,blk,ADR(buf),4096)) END;
  chk(BIO.Link(BIO.CD(),fname,f));
  chk(BIO.Close(f));
*)
END write_lib;
(*
PROCEDURE forward_ref(VAR ref: item; host: item; no: INTEGER);
BEGIN
  IF for_cnt>HIGH(for_wsp) THEN
    HALT(1);
  ELSE
    for_wsp[for_cnt].ref:=ADR(ref);
    for_wsp[for_cnt].host:=host;
    for_wsp[for_cnt].no:=no;
    INC(for_cnt);
  END;
END forward_ref;

PROCEDURE do_forward;
  VAR t: item;
BEGIN
  WHILE for_cnt>0 DO
    WITH for_wsp[for_cnt-1] DO
      CASE host^.md OF
        |cl_pack: t:=host^.pa_list;
        |cl_func: t:=host^.fu_list;
      END;
      LOOP
        IF t=NIL THEN RETURN END;
        IF t^.host_cnt=no THEN ref^:=t; EXIT END;
        t:=t^.host_nxt;
      END;
    END;
    DEC(for_cnt);
  END;
END do_forward;
*)
PROCEDURE import(nm: name; vis: BOOLEAN): item;
(*
  VAR
    blk  : INTEGER;
    pos  : INTEGER;
    buf  : ARRAY [0..1023] OF INTEGER;
    f    : FsPublic.File;
    fname: FsPublic.FileName;
    lib  : item;
  PROCEDURE chk(r: BOOLEAN);
    VAR err  : ARRAY [0..79] OF CHAR;
  BEGIN
    IF r THEN
      FsPublic.VisFSerr(r,err);
      error(16,lex_pos,err); HALT(1);
    END;
  END chk;
  PROCEDURE get(): WORD;
  BEGIN
    IF pos=1024 THEN
      INC(blk); pos:=0;
      chk(BIO.bRead(f,blk,ADR(buf),4096));
    END;
    INC(pos);
    RETURN buf[pos-1];
  END get;
  PROCEDURE get_ref(VAR ref: item; level: item);
    VAR a: ADDRESS; n: INTEGER; nm: name;
  BEGIN
    n:=get();
    IF n=-4 THEN
      n:=get();
      a:=ADR(string_val); string_len:=n*4;
      WHILE n>0 DO a^:=get(); INC(a); DEC(n) END;
      WHILE (string_len>0) & (string_val[string_len-1]=0c) DO
        DEC(string_len);
      END;
      nm:=def_name();
      ref:=import(nm,FALSE);
      ASSERT(ref#NIL);
      ASSERT(ref^.md=cl_pack);
    ELSIF n=-3 THEN
      ref:=lib;
    ELSIF n=-2 THEN
      ref:=NIL;
    ELSIF n=-1 THEN
      n:=get();
      a:=ADR(string_val); string_len:=n*4;
      WHILE n>0 DO a^:=get(); INC(a); DEC(n) END;
      WHILE (string_len>0) & (string_val[string_len-1]=0c) DO
        DEC(string_len);
      END;
      nm:=def_name();
      n:=get();
      ref:=import(nm,FALSE);
      ASSERT(ref#NIL);
      ASSERT(ref^.md=cl_pack);
      ref:=ref^.pa_list;
      LOOP
        ASSERT(ref#NIL);
        IF ref^.host_cnt=n THEN EXIT END;
        ref:=ref^.host_nxt;
      END;
    ELSE
      ASSERT(n>=0);
      WHILE n>0 DO
        ASSERT(level#NIL);
        level:=level^.host; DEC(n)
      END;
      n:=get();
      CASE level^.md OF
        |cl_pack: ref:=level^.pa_list; |cl_func: ref:=level^.fu_list;
      END;
      LOOP
        IF ref=NIL THEN forward_ref(ref,level,n); EXIT END;
        IF ref^.host_cnt=n THEN EXIT END;
        ref:=ref^.host_nxt;
      END;
    END;
  END get_ref;
  PROCEDURE skip_ref;
    VAR n: INTEGER;
  BEGIN
    n:=get();
    IF n=-4 THEN
      n:=get();
      WHILE n>0 DO IF get()=0 THEN END; DEC(n) END;
    ELSIF n=-3 THEN
    ELSIF n=-2 THEN
    ELSIF n=-1 THEN
      n:=get();
      WHILE n>0 DO IF get()=0 THEN END; DEC(n) END;
      n:=get();
    ELSE
      n:=get();
    END;
  END skip_ref;
  PROCEDURE skip(md: class);
    VAR n: INTEGER;
  BEGIN
    n:=get(); -- name len
    WHILE n>0 DO IF get()=0 THEN END; DEC(n) END;
    n:=get(); -- host_cnt
    n:=get(); -- private
    CASE md OF
      |cl_enumeration: n:=get(); n:=get();
      |cl_integer:     n:=get(); n:=get();
      |cl_float:       n:=get(); n:=get(); n:=get();
      |cl_array:       skip_ref; skip_ref;
      |cl_record:      skip_ref;
      |cl_field:       skip_ref; skip_ref; n:=get();
      |cl_access:      skip_ref;
      |cl_task:
      |cl_var:         skip_ref; n:=get(); n:=get(); n:=get(); n:=get();
      |cl_func:        skip_ref;
        LOOP
          n:=get();
          IF n=0 THEN EXIT END;
          ASSERT(n=1);
          skip(get());
        END;
        skip_ref; n:=get(); n:=get(); n:=get();
      |cl_prm:         skip_ref; skip_ref; n:=get(); n:=get(); n:=get();
      |cl_pack:
        LOOP
          n:=get();
          IF n=0 THEN EXIT END;
          ASSERT(n=1);
          skip(get());
        END;
        n:=get();
      |cl_use:         skip_ref;
      |cl_subtype:     skip_ref; n:=get(); n:=get(); n:=get(); n:=get();
      |cl_type:        skip_ref;
    END;
  END skip;
  PROCEDURE read_prm(host: item; VAR last: item);
    VAR
      a: ADDRESS;
      n: INTEGER;
      t,i: item;
      md: class;
  BEGIN
    md:=get();
    IF md#cl_prm THEN skip(md); RETURN END;
    n:=get();
    IF n#0 THEN
      a:=ADR(string_val); string_len:=n*4;
      WHILE n>0 DO a^:=get(); INC(a); DEC(n) END;
      WHILE (string_len>0) & (string_val[string_len-1]=0c) DO
        DEC(string_len);
      END;
      i^.nm:=def_name();
      tie_item(i^.nm,i);
    END;
    i:=def_item(md);
    i^.host_cnt:=get();
    i^.private:=get();
    ASSERT(host#NIL);
    i^.host:=host;
    IF last#NIL THEN
      last^.host_nxt:=i;
    ELSE
      ASSERT(host^.md=cl_func);
      host^.fu_list:=i;
    END;
    last:=i;
    get_ref(i^.pr_type,host);
    get_ref(i^.pr_nxt,host);
    i^.pr_in:=get();
    i^.pr_out:=get();
    i^.pr_val0:=get();
  END read_prm;
  PROCEDURE read_item(host: item; VAR last: item);
    VAR
      a: ADDRESS;
      n: INTEGER;
      t,i: item;
  BEGIN
    i:=def_item(get());
    n:=get();
    IF n#0 THEN
      a:=ADR(string_val); string_len:=n*4;
      WHILE n>0 DO a^:=get(); INC(a); DEC(n) END;
      WHILE (string_len>0) & (string_val[string_len-1]=0c) DO
        DEC(string_len);
      END;
      i^.nm:=def_name();
      tie_item(i^.nm,i);
    END;
    i^.host_cnt:=get();
    i^.private:=get();
    IF host#NIL THEN
      i^.host:=host;
      IF last#NIL THEN
        last^.host_nxt:=i;
      ELSE
        CASE host^.md OF
          |cl_func  : host^.fu_list:=i;
          |cl_pack  : host^.pa_list:=i;
        END;
      END;
    END;
    last:=i;
    CASE i^.md OF
      |cl_enumeration:
        i^.en_char:=get();
        i^.en_universal:=get();
      |cl_integer:
        i^.in_byte:=get();
        i^.in_universal:=get();
      |cl_float:
        i^.fl_digits:=get();
        i^.fl_delta:=get();
        i^.fl_universal:=get();
      |cl_array:
        get_ref(i^.ar_elemtp,host);
        get_ref(i^.ar_index,host);
      |cl_record:
        get_ref(i^.re_fields,host);
      |cl_field:
        get_ref(i^.fi_type,host);
        get_ref(i^.fi_nxt,host);
        i^.fi_val0:=get();
      |cl_access:
        get_ref(i^.ac_type,host);
      |cl_task:
      |cl_var:
        get_ref(i^.va_type,host);
        i^.va_rl:=get();
        i^.va_val0:=get();
        i^.va_val1:=get();
        i^.va_val2:=get();
      |cl_func:
        i^.fu_import:=TRUE;
        get_ref(i^.fu_type,host);
        t:=NIL;
        LOOP
          n:=get();
          IF n=0 THEN EXIT END;
          ASSERT(n=1);
          read_prm(i,t);
        END;
        get_ref(i^.fu_prm,i);
        i^.fu_rl:=get();
        i^.fu_val0:=get();
        i^.fu_val1:=get();
        i^.fu_nxt:=NIL;
      |cl_prm:
        get_ref(i^.pr_type,host);
        get_ref(i^.pr_nxt,host);
        i^.pr_in:=get();
        i^.pr_out:=get();
        i^.pr_val0:=get();
      |cl_pack:
        i^.pa_import:=FALSE;
        t:=NIL;
        LOOP
          n:=get();
          IF n=0 THEN EXIT END;
          ASSERT(n=1);
          read_item(i,t);
        END;
        i^.pa_var_no:=get();
        do_forward;
      |cl_use:
        get_ref(i^.us_pack,host);
        ASSERT(i^.us_pack#NIL);
        i^.us_nxt:=i^.us_pack^.pa_use;
        i^.us_pack^.pa_use:=i;
      |cl_subtype:
        get_ref(i^.su_type,host);
        i^.su_first:=get();
        i^.su_last:=get();
        i^.su_private:=get();
        i^.su_limited:=get();
      |cl_type:
        get_ref(i^.ty_type,host);
    END;
  END read_item;
  VAR r: BOOLEAN;
BEGIN
  lib:=nm^.obj;
  WHILE (lib#NIL) & NOT (lib^.host=NIL) DO lib:=lib^.nm_fwd END;
  IF lib=NIL THEN
    Image.image0(fname,'%s.sym',nm^.val);
    r:=BIO.OpenOnDir(BIO.CD(),f,fname);
    IF r=FsPublic.FileNotFound THEN RETURN NIL END;
    chk(r);
    blk:=-1; pos:=1024;
    read_item(NIL,lib);
    chk(BIO.Close(f));
    ASSERT(lib#NIL);
    ASSERT(lib^.nm#NIL);
    ASSERT(lib^.host=NIL);
    ASSERT(lib^.md IN library_class);
    do_forward;
    ASSERT(for_cnt=0);
    CASE lib^.md OF
      |cl_func: lib^.fu_import:=TRUE;
      |cl_pack: lib^.pa_import:=TRUE;
    END;
    IF NOT vis THEN
      ASSERT(lib^.md=cl_pack);
      lib^.pa_ref:=TRUE;
    END;
  ELSIF vis & (lib^.md=cl_pack) THEN
    lib^.pa_ref:=FALSE;
  END;
  RETURN lib;
*)
END import;

PROCEDURE read_lib(lib_name: ARRAY OF CHAR): item;
(*
  VAR
    blk  : INTEGER;
    pos  : INTEGER;
    buf  : ARRAY [0..1023] OF INTEGER;
    f    : FsPublic.File;
    fname: FsPublic.FileName;
    lib  : item;
  PROCEDURE chk(r: BOOLEAN);
    VAR err  : ARRAY [0..79] OF CHAR;
  BEGIN
    IF r THEN
      FsPublic.VisFSerr(r,err);
      error(16,lex_pos,err); HALT(1);
    END;
  END chk;
  PROCEDURE get(): WORD;
  BEGIN
    IF pos=1024 THEN
      INC(blk); pos:=0;
      chk(BIO.bRead(f,blk,ADR(buf),4096));
    END;
    INC(pos);
    RETURN buf[pos-1];
  END get;
  PROCEDURE get_ref(VAR ref: item; level: item);
    VAR a: ADDRESS; n: INTEGER; nm: name;
  BEGIN
    n:=get();
    IF n=-4 THEN
      n:=get();
      a:=ADR(string_val); string_len:=n*4;
      WHILE n>0 DO a^:=get(); INC(a); DEC(n) END;
      WHILE (string_len>0) & (string_val[string_len-1]=0c) DO
        DEC(string_len);
      END;
      nm:=def_name();
      ref:=import(nm,FALSE);
      ASSERT(ref#NIL);
      ASSERT(ref^.md=cl_pack);
    ELSIF n=-3 THEN
      ref:=lib;
    ELSIF n=-2 THEN
      ref:=NIL;
    ELSIF n=-1 THEN
      n:=get();
      a:=ADR(string_val); string_len:=n*4;
      WHILE n>0 DO a^:=get(); INC(a); DEC(n) END;
      WHILE (string_len>0) & (string_val[string_len-1]=0c) DO
        DEC(string_len);
      END;
      nm:=def_name();
      n:=get();
      ref:=import(nm,FALSE);
      ASSERT(ref#NIL);
      ASSERT(ref^.md=cl_pack);
      ref:=ref^.pa_list;
      LOOP
        ASSERT(ref#NIL);
        IF ref^.host_cnt=n THEN EXIT END;
        ref:=ref^.host_nxt;
      END;
    ELSE
      ASSERT(n>=0);
      WHILE n>0 DO
        ASSERT(level#NIL);
        level:=level^.host; DEC(n)
      END;
      n:=get();
      CASE level^.md OF
        |cl_pack: ref:=level^.pa_list; |cl_func: ref:=level^.fu_list;
      END;
      LOOP
        IF ref=NIL THEN forward_ref(ref,level,n); EXIT END;
        IF ref^.host_cnt=n THEN EXIT END;
        ref:=ref^.host_nxt;
      END;
    END;
  END get_ref;
  PROCEDURE read_item(host: item; VAR last: item);
    VAR
      a: ADDRESS;
      n: INTEGER;
      t,i: item;
  BEGIN
    i:=def_item(get());
    n:=get();
    IF n#0 THEN
      a:=ADR(string_val); string_len:=n*4;
      WHILE n>0 DO a^:=get(); INC(a); DEC(n) END;
      WHILE (string_len>0) & (string_val[string_len-1]=0c) DO
        DEC(string_len);
      END;
      i^.nm:=def_name();
      tie_item(i^.nm,i);
    END;
    i^.host_cnt:=get();
    i^.private:=get();
    IF host#NIL THEN
      i^.host:=host;
      IF last#NIL THEN
        last^.host_nxt:=i;
      ELSE
        CASE host^.md OF
          |cl_func  : host^.fu_list:=i;
          |cl_pack  : host^.pa_list:=i;
        END;
      END;
    END;
    last:=i;
    CASE i^.md OF
      |cl_enumeration:
        i^.en_char:=get();
        i^.en_universal:=get();
      |cl_integer:
        i^.in_byte:=get();
        i^.in_universal:=get();
      |cl_float:
        i^.fl_digits:=get();
        i^.fl_delta:=get();
        i^.fl_universal:=get();
      |cl_array:
        get_ref(i^.ar_elemtp,host);
        get_ref(i^.ar_index,host);
      |cl_record:
        get_ref(i^.re_fields,host);
      |cl_field:
        get_ref(i^.fi_type,host);
        get_ref(i^.fi_nxt,host);
        i^.fi_val0:=get();
      |cl_access:
        get_ref(i^.ac_type,host);
      |cl_task:
      |cl_var:
        get_ref(i^.va_type,host);
        i^.va_rl:=get();
        i^.va_val0:=get();
        i^.va_val1:=get();
        i^.va_val2:=get();
      |cl_func:
        get_ref(i^.fu_type,host);
        get_ref(i^.fu_prm,host);
        t:=NIL;
        LOOP
          n:=get();
          IF n=0 THEN EXIT END;
          ASSERT(n=1);
          read_item(i,t);
        END;
        i^.fu_rl:=get();
        i^.fu_val0:=get();
        i^.fu_val1:=get();
        i^.fu_nxt:=NIL;
        do_forward;
      |cl_prm:
        get_ref(i^.pr_type,host);
        get_ref(i^.pr_nxt,host);
        i^.pr_in:=get();
        i^.pr_out:=get();
        i^.pr_val0:=get();
      |cl_pack:
        i^.pa_import:=FALSE;
        t:=NIL;
        LOOP
          n:=get();
          IF n=0 THEN EXIT END;
          ASSERT(n=1);
          read_item(i,t);
        END;
        i^.pa_var_no:=get();
        do_forward;
      |cl_use:
        get_ref(i^.us_pack,host);
        ASSERT(i^.us_pack#NIL);
        i^.us_nxt:=i^.us_pack^.pa_use;
        i^.us_pack^.pa_use:=i;
      |cl_subtype:
        get_ref(i^.su_type,host);
        i^.su_first:=get();
        i^.su_last:=get();
        i^.su_private:=get();
        i^.su_limited:=get();
      |cl_type:
        get_ref(i^.ty_type,host);
    END;
  END read_item;
  VAR r: BOOLEAN;
BEGIN
  Image.image0(fname,'%s.sym',lib_name);
  r:=BIO.OpenOnDir(BIO.CD(),f,fname);
  IF r=FsPublic.FileNotFound THEN RETURN NIL END;
  chk(r);
  blk:=-1; pos:=1024;
  lib:=NIL; read_item(NIL,lib);
  chk(BIO.Close(f));
  ASSERT(lib^.nm#NIL);
  ASSERT(lib^.host=NIL);
  ASSERT(lib^.md IN library_class);
  do_forward;
  ASSERT(for_cnt=0);
  RETURN lib;
*)
END read_lib;

PROCEDURE new_item(c: class; host: item; VAR last: item);
  VAR i: item;
BEGIN
  i:=def_item(c);
  IF host#NIL THEN
    i^.host:=host;
    IF last#NIL THEN
      i^.host_cnt:=last^.host_cnt+1; last^.host_nxt:=i;
    ELSE
      i^.host_cnt:=0;
      CASE host^.md OF
        |cl_func    : host^.fu_list:=i;
        |cl_pack    : host^.pa_list:=i;
        |cl_operator: host^.op_list:=i;
      END;
    END;
  END;
  last:=i;
END new_item;

PROCEDURE new_name_item(c: class; host: item; VAR last: item);
-- создает объект класса  c
-- с именем string_val
-- в области описаний объекта host
-- проверяет отсутствие повторного описания
  VAR i: item; nm: name;
BEGIN
  ASSERT((sy=lex.ident)OR(sy=lex.string)OR(sy=lex.char));
  nm:=find();
  IF nm#NIL THEN
    i:=nm^.obj;
    WHILE i#NIL DO
      IF (i^.host=host) & ((i^.md#cl_func) OR (c#cl_func)) THEN
        error(lex_pos,'повторное описание имени %s',val.str)
      END;
      i:=i^.nm_fwd;
    END;
  ELSE
    nm:=def_name();
  END;
  new_item(c,host,last);
  tie_item(nm,last);
END new_name_item;

PROCEDURE check_func(fn: item; list: item): BOOLEAN;
-- сверено со стандартом
  VAR p0,p1: item;
BEGIN
  WHILE list#NIL DO
    IF (list^.fu_type=NIL) & (fn^.fu_type=NIL) OR
       (list^.fu_type^.su_type=fn^.fu_type^.su_type) THEN
      p0:=list^.fu_prm; p1:=fn^.fu_prm;
      LOOP
        IF (p0=NIL) & (p1=NIL) THEN RETURN FALSE END;
        IF (p0=NIL) OR (p1=NIL) OR
           (p0^.pr_type^.su_type#p1^.pr_type^.su_type) THEN EXIT END;
        p0:=p0^.pr_nxt; p1:=p1^.pr_nxt;
      END;
    END;
    list:=list^.fu_nxt;
  END;
  RETURN TRUE;
END check_func;

PROCEDURE find_vis(nm: name; from: item): item;
  CONST with_name=class_set{cl_var,cl_func,cl_prm,cl_pack,cl_subtype};
  VAR fun,use,i,j: item; may_use: BOOLEAN;
BEGIN
  ASSERT((from=NIL) OR (from^.md IN class_set{cl_pack,cl_func,cl_operator}));
  fun:=NIL; use:=NIL; may_use:=TRUE;
  LOOP
    i:=nm^.obj;
    WHILE i#NIL DO
      IF i^.md IN with_name THEN
        IF i^.host=from THEN
          IF i^.md=cl_func THEN
            IF check_func(i,fun) THEN i^.fu_nxt:=fun; fun:=i END;
          ELSIF fun#NIL THEN
            RETURN fun;
          ELSE
            RETURN i;
          END;
        ELSIF may_use & (i^.host#NIL) & (i^.host^.md=cl_pack) THEN
          j:=i^.host^.pa_use;
          WHILE j#NIL DO
            IF j^.host=from THEN
              IF (i^.md=cl_func) & ((use=NIL)OR(use^.md=cl_func)) THEN
                i^.fu_nxt:=use; use:=i
              ELSIF use#NIL THEN
                use:=NIL; may_use:=FALSE;
              ELSE
                use:=i;
              END;
            END;
          END;
        END;
      END;
      i:=i^.nm_fwd;
    END;
    IF from=NIL THEN
      IF (use#NIL) & (use^.md=cl_func) THEN
        j:=NIL;
        WHILE use#NIL DO
          IF NOT check_func(use,j) OR NOT check_func(use,fun) THEN
            RETURN fun;
          END;
          i:=use; use:=i^.fu_nxt; i^.fu_nxt:=j; j:=i;
        END;
        REPEAT i:=j; j:=i^.fu_nxt; i^.fu_nxt:=fun; fun:=i UNTIL j=NIL;
      END;
      IF fun#NIL THEN RETURN fun ELSE RETURN use END;
    END;
    from:=from^.host;
  END;
END find_vis;

PROCEDURE find_extended(cx: item; VAR nm: name): item;
  VAR
    vis: item;
    obj: item;
  PROCEDURE find_obj;
    VAR i: item; dir: BOOLEAN;
  BEGIN
    IF vis=NIL THEN
      obj:=find_vis(nm,cx);
    ELSE
      IF vis^.md=cl_pack THEN
        i:=cx;
        WHILE (i#NIL) & (i#vis) DO i:=i^.host END;
        dir:=(i=vis);
        i:=vis^.pa_list;
        LOOP
          IF NOT dir & (i=vis^.pa_priv) OR (i=NIL) THEN EXIT END;
          IF i^.nm=nm THEN
            IF i^.md=cl_func THEN
              ASSERT((obj=NIL)OR(obj^.md=cl_func));
              i^.fu_nxt:=obj; obj:=i;
            ELSE
              ASSERT((obj=NIL)OR(obj^.md#cl_func));
              obj:=i; EXIT
            END;
          END;
          i:=i^.host_nxt;
        END;
      ELSIF vis^.md=cl_func THEN
        i:=vis^.fu_list;
        LOOP
          IF i=NIL THEN EXIT END;
          IF i^.nm=nm THEN
            IF i^.md=cl_func THEN
              ASSERT((obj=NIL)OR(obj^.md=cl_func));
              i^.fu_nxt:=obj; obj:=i;
            ELSE
              ASSERT((obj=NIL)OR(obj^.md#cl_func));
              obj:=i; EXIT
            END;
          END;
          i:=i^.host_nxt;
        END;
      END;
    END;
  END find_obj;
  PROCEDURE prefix?(): BOOLEAN;
    VAR i,j: item;
  BEGIN
    obj:=NIL; nm:=NIL;
    IF (sy#lex.ident) OR (next_sy#lex.point) THEN RETURN FALSE END;
    nm:=find();
    IF nm=NIL THEN RETURN FALSE END;
    find_obj;
    IF obj=NIL THEN RETURN FALSE END;
    IF obj^.md=cl_func THEN
      i:=cx;
      LOOP
        IF i=NIL THEN RETURN FALSE END;
        j:=obj;
        WHILE (j#NIL) & (j#i) DO j:=j^.fu_nxt END;
        IF j=i THEN vis:=j; lex.next; lex.next; RETURN TRUE END;
        i:=i^.host;
      END;
    ELSIF obj^.md=cl_pack THEN
      vis:=obj; lex.next; lex.next; RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END prefix?;
  VAR i: INTEGER;
BEGIN
  vis:=NIL;
  IF sy=lex.ident THEN WHILE prefix?() DO END END;
  IF nm=NIL THEN
    IF sy=lex.string THEN
      IF next_sy=lex.left_parenthesis THEN
        FOR i:=0 TO val.len-1 DO
          IF (val.str[i]>='a') & (val.str[i]<='z') THEN
            val.str[i]:=CHAR(ORD(val.str[i])-40b);
          END;
        END;
        nm:=find();
      END;
    ELSIF (sy=lex.ident) OR (sy=lex.char) THEN
      nm:=find()
    END;
  END;
  IF (nm#NIL) & (obj=NIL) THEN find_obj END;
  RETURN obj;
END find_extended;

BEGIN
  tree:=NIL;
  for_cnt:=0;
END adaTable.
