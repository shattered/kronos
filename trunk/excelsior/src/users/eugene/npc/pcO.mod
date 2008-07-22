IMPLEMENTATION MODULE pcO; (* Ned 28-Mar-91. (c) KRONOS *)

IMPORT  SYSTEM, pcM, pcK, pcS, pcY;

TYPE SINT = LONGINT; (* for TopSpeed instead of SHORTINT *)

VAR
  super : pcK.OBJECT;  (* super proc    *)
  sysmod: pcK.OBJECT;  (* module SYSTEM *)
  forms : ARRAY [ 0..15] OF pcK.Form;
  modes : ARRAY [16..39] OF SHORTINT;

(*----------------------------------------------------------------*)

PROCEDURE str_comp(s1,s2: pcK.NAME): INTEGER;
  VAR c1,c2: CHAR; i: INTEGER;
BEGIN
  i:=0;
  LOOP
    c1:=s1[i]; c2:=s2[i];
    IF    c1#c2 THEN RETURN INTEGER(c2)-INTEGER(c1)
    ELSIF c1=0C THEN RETURN 0
    END;
    INC(i);
  END;
END str_comp;

(*----------------------------------------------------------------*)

PROCEDURE new_obj(VAR o: pcK.OBJECT; mode: SHORTINT);
BEGIN
  pcM.ALLOCATE(o,SIZE(o^));
  o^:=pcK.null_object;
  o^.mode:=pcK.Mode(mode);
  o^.scope:=level;
  o^.pos:=pcS.txtpos;
END new_obj;

PROCEDURE new_type(VAR t: pcK.STRUCT; mode: pcK.Form);
BEGIN
  pcM.ALLOCATE(t,SIZE(t^));
  t^:=pcK.null_struct;
  t^.mode:=mode;
  t^.pos:=pcS.txtpos;
END new_type;

PROCEDURE rem_type(VAR t: pcK.STRUCT);
  VAR l,x: pcK.OBJECT;
BEGIN
  IF t^.mode IN pcK.Forms{pcK.proctype,pcK.record} THEN
    l:=t^.next;
    WHILE l#NIL DO x:=l; l:=l^.next; pcM.DEALLOCATE(x,SIZE(x^)) END;
  END;
  pcM.DEALLOCATE(t,SIZE(t^));
END rem_type;

(*----------------------------------------------------------------*)

PROCEDURE append(VAR root: pcK.OBJECT; o: pcK.OBJECT): BOOLEAN;
  VAR x: pcK.OBJECT; r: INTEGER;
BEGIN
  IF root=NIL THEN root:=o; RETURN FALSE END;
  x:=root;
  LOOP
    r:=str_comp(x^.name,o^.name);
    IF    r>0 THEN
      IF x^.r#NIL THEN x:=x^.r ELSE x^.r:=o; RETURN FALSE END;
    ELSIF r<0 THEN
      IF x^.l#NIL THEN x:=x^.l ELSE x^.l:=o; RETURN FALSE END;
    ELSE RETURN TRUE
    END;
  END;
END append;

PROCEDURE insert(VAR root: pcK.OBJECT; o: pcK.OBJECT; VAR old: pcK.OBJECT);
  VAR x: pcK.OBJECT; r: INTEGER;
BEGIN
  old:=NIL;
  IF root=NIL THEN root:=o; RETURN END;
  x:=root;
  LOOP
    r:=str_comp(x^.name,o^.name);
    IF    r>0 THEN
      IF x^.r#NIL THEN x:=x^.r ELSE x^.r:=o; RETURN END;
    ELSIF r<0 THEN
      IF x^.l#NIL THEN x:=x^.l ELSE x^.l:=o; RETURN END;
    ELSE old:=x; RETURN
    END;
  END;
END insert;

PROCEDURE find(root: pcK.OBJECT; name: pcK.NAME; VAR o: pcK.OBJECT): BOOLEAN;
  VAR r: INTEGER;
BEGIN
  LOOP
    IF root=NIL THEN RETURN FALSE END;
    r:=str_comp(root^.name,name);
    IF r=0 THEN o:=root; RETURN TRUE END;
    IF r>0 THEN root:=root^.r ELSE root:=root^.l END;
  END;
END find;

(*----------------------------------------------------------------*)

VAR levs: pcK.OBJECT;

PROCEDURE try(name: pcK.NAME; VAR o: pcK.OBJECT): BOOLEAN;
  VAR x: pcK.OBJECT;
BEGIN
  x:=levs;
  WHILE x#NIL DO
    IF find(x^.r,name,o) THEN RETURN TRUE END;
    x:=x^.l;
  END;
  o:=inv_obj;
  RETURN FALSE
END try;

PROCEDURE invis(name: pcK.NAME; VAR o: pcK.OBJECT);
BEGIN
  new_obj(o,pcK.inv); o^.type:=invtype;
  pcM.str_copy(o^.name,name);
  pcS.err_id(20,name);
END invis;

PROCEDURE vis(name: pcK.NAME; VAR o: pcK.OBJECT);
BEGIN
  IF NOT try(name,o) THEN invis(name,o); dcl(o) END;
END vis;

PROCEDURE try_in(name: pcK.NAME; scope: pcK.OBJECT; VAR o: pcK.OBJECT): BOOLEAN;
BEGIN
  IF find(scope^.head^.r,name,o) THEN
    RETURN (o^.mode#pcK.type) OR (pcK.exported IN o^.tags)
  ELSE RETURN FALSE
  END;
END try_in;

PROCEDURE vis_in(name: pcK.NAME; scope: pcK.OBJECT; VAR o: pcK.OBJECT);
BEGIN
  IF find(scope^.head^.r,name,o) THEN
    IF (o^.mode=pcK.type) & NOT (pcK.exported IN o^.tags) THEN
      pcS.err_id(20,name);
      o^.mode:=pcK.type;
    END;
  ELSE invis(name,o); dcl_in(scope,o);
  END;
END vis_in;

PROCEDURE try_in_rec(name: pcK.NAME; rec: pcK.STRUCT; VAR o: pcK.OBJECT): BOOLEAN;
BEGIN
  WHILE rec#NIL DO
    IF find(rec^.locs,name,o) THEN RETURN TRUE END;
    rec:=rec^.base;
  END;
  RETURN FALSE
END try_in_rec;

PROCEDURE vis_in_rec(name: pcK.NAME; rec: pcK.STRUCT; VAR o: pcK.OBJECT);
  VAR old: pcK.OBJECT;
BEGIN
  IF NOT try_in_rec(name,rec,o) THEN
    invis(name,o);
    IF append(rec^.locs,o) THEN END;
  END;
END vis_in_rec;

PROCEDURE dcl(o: pcK.OBJECT);
BEGIN
  IF append(levs^.r,o) THEN
    pcS.err_id(22,o^.name);
  END;
END dcl;

PROCEDURE dcl_in(scope: pcK.OBJECT; o: pcK.OBJECT);
BEGIN
  IF append(scope^.head^.r,o) THEN
    pcS.err_id(22,o^.name);
  END;
END dcl_in;

PROCEDURE dcl_in_rec(rec: pcK.STRUCT; o: pcK.OBJECT);
BEGIN
  IF (o^.mode=pcK.field) & try_in_rec(o^.name,rec^.base,o)
    OR append(rec^.locs,o)
  THEN pcS.err_id(22,o^.name);
  END;
END dcl_in_rec;

(*----------------------------------------------------------------*)

PROCEDURE enter_scope(b: pcK.OBJECT);
  VAR x: pcK.OBJECT;
BEGIN
  new_obj(x,pcK.header);
  b^.head:=x;
  x^.head:=b;
  pcM.str_copy(x^.name,b^.name);
  x^.l:=levs;
  levs:=x;
  scope:=b;
  INC(level);
END enter_scope;

PROCEDURE exit_scope;
BEGIN
  levs:=levs^.l;
  scope:=levs^.head;
  DEC(level);
END exit_scope;

(*----------------------------------------------------------------*)

CONST VERS = 1; (* symbol file version *)

PROCEDURE export(module: pcK.OBJECT; VAR newSF: BOOLEAN; key: LONGINT);

  CONST max_ptr = 31; max_rec = 31;

  VAR
    ptrs  : ARRAY [0..max_ptr] OF pcK.STRUCT;
    ptr_no: INTEGER;
    recs  : ARRAY [0..max_rec] OF pcK.STRUCT;
    rec_no: INTEGER;
    ref_no: INTEGER;
    exp_no: INTEGER;

  PROCEDURE out_struct(type: pcK.STRUCT); FORWARD;

  PROCEDURE out_parms(l: pcK.OBJECT);
  BEGIN
    pcM.put(pcY.plist);
    WHILE l#NIL DO
      out_struct(l^.type);
      CASE l^.mode OF
        |pcK.var   : pcM.put(pcY.valpar)
        |pcK.varpar: pcM.put(pcY.varpar)
        |pcK.seq   : pcM.put(pcY.seq)
        |pcK.varseq: pcM.put(pcY.varseq)
      END;
      pcM.put(l^.type^.ref); pcM.put_name(l^.name);
      put_object(l);
      l:=l^.next;
    END;
  END out_parms;

  PROCEDURE out_fields(l: pcK.OBJECT);

    PROCEDURE field(o: pcK.OBJECT);
    BEGIN
      out_struct(o^.type);
      pcM.put(pcY.field); pcM.put(o^.type^.ref); pcM.put_name(o^.name);
      put_object(o);
      IF pcK.RO_export IN o^.tags THEN pcM.put(pcY.readonly) END;
    END field;

  BEGIN
    WHILE l#NIL DO
      IF l^.mode=pcK.header THEN
        IF l^.l#NIL THEN field(l) END;
        out_fields(l^.r);
      ELSE field(l);
      END;
      l:=l^.next
    END;
  END out_fields;

  PROCEDURE modno(no: INTEGER; VAR mno: SINT);
    VAR obj: pcK.OBJECT;
  BEGIN
    obj:=exts[no];
    IF obj^.mode=0 THEN
      obj^.mode:=pcK.Mode(exp_no);
      mno:=SINT(exp_no); INC(exp_no);
      pcM.put(pcY.module); pcM.put4(obj^.adr);
      pcM.put_name(obj^.name);
    ELSE mno:=SINT(obj^.mode);
    END;
  END modno;

  PROCEDURE out_struct(type: pcK.STRUCT);
    VAR mno: SINT; obj: pcK.OBJECT; base: pcK.STRUCT;
  BEGIN
    IF type^.ref>0 THEN RETURN END;
    base:=type^.base;
    IF type^.mno>0 THEN modno(type^.mno,mno) ELSE mno:=0 END;
    CASE type^.mode OF
      |pcK.range:
         out_struct(base);
         pcM.put(pcY.range); pcM.put(base^.ref); pcM.put(mno);
         pcM.put4(type^.n); pcM.put4(type^.m);
      |pcK.enum:
         pcM.put(pcY.enum);  pcM.put(pcY.undef); pcM.put(mno);
         pcM.put(type^.m);
      |pcK.opaque:
         pcM.put(pcY.opaque); pcM.put(pcY.undef); pcM.put(mno);
      |pcK.pointer:
         pcM.put(pcY.pointer);
         IF base^.ref>0 THEN pcM.put(base^.ref)
         ELSE pcM.put(pcY.undef);
           IF ptr_no>max_ptr THEN pcS.err(224)
           ELSE ptrs[ptr_no]:=type; INC(ptr_no);
           END;
         END;
         pcM.put(mno);
      |pcK.set:
         out_struct(base);
         pcM.put(pcY.set); pcM.put(base^.ref); pcM.put(mno);
         pcM.put(type^.size);
      |pcK.proctype:
         out_struct(base);
         out_parms(type^.next);
         pcM.put(pcY.proctype); pcM.put(base^.ref); pcM.put(mno);
      |pcK.array:
         out_struct(type^.inx); out_struct(base);
         pcM.put(pcY.array); pcM.put(base^.ref); pcM.put(mno);
         pcM.put(type^.inx^.ref); pcM.put4(type^.size);
      |pcK.vector:
         out_struct(base);
         pcM.put(pcY.vector);  pcM.put(base^.ref);  pcM.put(mno);
         pcM.put4(type^.n);    pcM.put4(type^.size);
      |pcK.array_of:
         out_struct(base);
         pcM.put(pcY.array_of);  pcM.put(base^.ref);  pcM.put(mno);
         pcM.put(type^.n);      pcM.put(type^.size);
      |pcK.dynarr:
         out_struct(base);
         pcM.put(pcY.dynarr);  pcM.put(base^.ref);  pcM.put(mno);
         pcM.put(type^.n);     pcM.put(type^.size);
      |pcK.record:
         IF base#NIL THEN out_struct(base) END;
         pcM.put(pcY.flist);
         out_fields(type^.next);
         pcM.put(pcY.record);
         IF base=NIL THEN pcM.put(pcY.undef) ELSE pcM.put(base^.ref) END;
         pcM.put(mno); pcM.put4(type^.size); pcM.put(type^.m);
         IF type^.link#NIL THEN
           IF rec_no>max_rec THEN pcS.err(230)
           ELSE recs[rec_no]:=type; INC(rec_no);
           END;
         END;
    ELSE pcM.abort;
    END;
    put_struct(type);
    IF type^.obj#NIL THEN
      IF def OR (pcK.exported IN type^.obj^.tags) THEN pcM.put(pcY.type)
      ELSE pcM.put(pcY.hdtype)
      END;
      pcM.put(LONGINT(ref_no)); pcM.put(mno); pcM.put_name(type^.obj^.name);
    END;
    IF ref_no>255 THEN pcS.err(225) END;
    type^.ref:=LONGINT(ref_no); INC(ref_no);
    IF type^.mode=pcK.enum THEN
      obj:=type^.next;
      WHILE obj#NIL DO
        pcM.put(pcY.cons); pcM.put(obj^.type^.ref);
        pcM.put_name(obj^.name);
        put_object(obj);
        obj:=obj^.next
      END;
    END;
  END out_struct;

  PROCEDURE out_obj(o: pcK.OBJECT);
    VAR mno: SINT;
  BEGIN
    CASE o^.mode OF
      |pcK.var:
         out_struct(o^.type);
         pcM.put(pcY.var); pcM.put(o^.type^.ref);
         pcM.put_name(o^.name);
         put_object(o);
         IF pcK.RO_export IN o^.tags THEN pcM.put(pcY.readonly) END;
      |pcK.xproc,pcK.cproc,pcK.iproc:
         out_struct(o^.type^.base);
         out_parms(o^.type^.next);
         IF    o^.mode=pcK.xproc THEN pcM.put(pcY.xproc);
         ELSIF o^.mode=pcK.cproc THEN pcM.put(pcY.cproc);
         ELSIF o^.mode=pcK.iproc THEN pcM.put(pcY.iproc);
         END;
         pcM.put(o^.type^.base^.ref);
         IF pcK.external IN o^.tags THEN pcM.put(1) ELSE pcM.put(0) END;
         pcM.put_name(o^.name);
         put_object(o);
      |pcK.cons:
         out_struct(o^.type);
         IF o^.scope>=0 THEN
           pcM.put(pcY.cons); pcM.put(o^.type^.ref);
         ELSE
           pcM.put(pcY.xcons); pcM.put(o^.type^.ref);
           modno(-o^.scope,mno); pcM.put(mno);
         END;
         pcM.put_name(o^.name);
         put_object(o);
    ELSE (* nothing *)
    END;
  END out_obj;

  PROCEDURE compare(VAR newSF: BOOLEAN; VAR key: LONGINT);
    VAR x,vers: SINT; old: LONGINT;
  BEGIN
    pcM.get4(x); pcM.get(vers);
    IF  x#pcY.SYMTAG THEN pcS.err_id(190,cu_name);
    ELSIF vers#VERS  THEN pcS.err_id(191,cu_name);
    ELSE
      pcM.get(x); pcM.get(x); pcM.get(x);
      pcM.get4(old);
      IF pcM.equal(12) THEN key:=old; newSF:=FALSE
      ELSIF NOT newSF THEN pcS.err(193); newSF:=FALSE;
      END;
    END;
  END compare;

  VAR l,o: pcK.OBJECT; i: INTEGER; t: pcK.STRUCT; done: BOOLEAN;
BEGIN
  pcM.create(module^.name,done);
  IF NOT done THEN RETURN END;
  pcM.put4(pcY.SYMTAG); pcM.put(VERS); pcM.put(0); pcM.put(0);
  ref_no:=32; ptr_no:=0; rec_no:=0; exp_no:=1;
  pcM.put(pcY.module); pcM.put4(key);
  pcM.put_name(module^.name);
  l:=module^.head^.next;
  WHILE l#NIL DO
    IF (def OR (pcK.exported IN l^.tags)) & (l^.mode=pcK.type) THEN
      out_struct(l^.type);  o:=l^.type^.obj;
      IF (o#l) & (o#NIL) THEN
        pcM.put(pcY.type); pcM.put(l^.type^.ref);
        pcM.put(0); (* Wirth ??? *)
        pcM.put_name(l^.name);
      END;
    END;
    l:=l^.next
  END;
  l:=module^.head^.next;
  WHILE l#NIL DO
    IF def OR (pcK.exported IN l^.tags) THEN out_obj(l) END;
    l:=l^.next
  END;
  FOR i:=0 TO ptr_no-1 DO
    t:=ptrs[i];
    out_struct(t^.base);
    pcM.put(pcY.linkage); pcM.put(t^.ref); pcM.put(t^.base^.ref);
  END;
  FOR i:=0 TO rec_no-1 DO
    t:=recs[i]; l:=t^.link;
    WHILE l#NIL DO
      o:=l^.head;
      out_struct(o^.type^.base);
      out_parms(o^.type^.next);
      pcM.put(pcY.method);
      pcM.put(t^.ref);
      pcM.put(o^.type^.base^.ref);
      pcM.put_name(l^.name);
      put_object(l);
      l:=l^.next;
    END;
  END;
  pcM.put(pcY.eosf); put_object(module);
  IF def THEN newSF:=TRUE
  ELSIF pcS.no_errs=0 THEN
    pcM.open(module^.name,TRUE,done);
    IF done THEN compare(newSF,key); pcM.close;
    ELSE newSF:=TRUE
    END;
  ELSE newSF:=FALSE
  END;
  pcM.close_new(newSF);
  module^.head^.adr:=key;
END export;

(*----------------------------------------------------------------*)

PROCEDURE read_sym(self: BOOLEAN; VAR head: pcK.OBJECT);
  TYPE STACK = ARRAY [0..7] OF RECORD h,t: pcK.OBJECT END;
  VAR
    a,t,m: SINT;
    mode: SHORTINT;
    i: INTEGER;
    key: LONGINT;
    type: pcK.STRUCT;
    obj,old,l,x: pcK.OBJECT;
    types: ARRAY [0..255]     OF pcK.STRUCT;  ref_no: INTEGER;
    mods : ARRAY [0..max_ext] OF pcK.OBJECT;  mod_no: INTEGER;
    mname: pcK.NAME;
    fields: STACK; flev: INTEGER; fhead,ftail: pcK.OBJECT;
    params: STACK; plev: INTEGER; phead,ptail: pcK.OBJECT;
BEGIN
  FOR ref_no:=0 TO 255 DO types[ref_no]:=NIL END;
  types[pcY.undef    ]:=undef;     types[pcY.shortint]:=shortint;
  types[pcY.integer  ]:=integer;   types[pcY.longint ]:=longint;
  types[pcY.shortcard]:=shortcard; types[pcY.cardinal]:=cardinal;
  types[pcY.shortIC  ]:=shortIC;   types[pcY.IC      ]:=IC;
  types[pcY.real     ]:=real;      types[pcY.longreal]:=longreal;
  types[pcY.boolean  ]:=boolean;   types[pcY.char    ]:=char;
  types[pcY.bitset   ]:=bitset;    types[pcY.byte    ]:=byte;
  types[pcY.word     ]:=word;      types[pcY.addr    ]:=addr;
  types[pcY.niltype  ]:=niltype;
  ref_no:=32; mod_no:=0;
  flev:=0; plev:=0;
  LOOP
    pcM.get(a); mode:=SHORTINT(a);
    IF mode<16 THEN (* struct *)
      new_type(type,forms[mode]);
      pcM.get(a); type^.base:=types[INTEGER(a)];
      pcM.get(a); type^.mno :=-mods[INTEGER(a)]^.scope;
      CASE mode OF
        |pcY.range   : pcM.get4(type^.n); pcM.get4(type^.m);
                       type^.size:=type^.base^.size;
        |pcY.enum    : pcM.get(type^.m);  type^.size:=pcM.byte;
                       type^.n:=0;
        |pcY.proctype: type^.size:=pcM.proctype;
                       type^.next:=phead;
                       DEC(plev);
                       WITH params[plev] DO phead:=h; ptail:=t END;
        |pcY.array   : pcM.get(a); type^.inx:=types[INTEGER(a)];
                       pcM.get4(type^.size);
        |pcY.vector  : pcM.get4(type^.n); pcM.get4(type^.size);
        |pcY.record  : pcM.get4(type^.size); pcM.get(type^.m);
                       IF type^.base=undef THEN type^.base:=NIL;
                       ELSE type^.n:=type^.base^.n+1;
                       END;
                       type^.next:=fhead;
                       l:=fhead;
                       WHILE l#NIL DO dcl_in_rec(type,l); l:=l^.next END;
                       DEC(flev);
                       WITH fields[flev] DO fhead:=h; ftail:=t END;
        |pcY.set     : pcM.get(type^.size);
        |pcY.opaque  : type^.size:=pcM.addr;
        |pcY.pointer : type^.size:=pcM.addr;
        |pcY.array_of: pcM.get(type^.n); pcM.get(type^.size);
        |pcY.dynarr  : pcM.get(type^.n); pcM.get(type^.size);
      ELSE pcM.abort;
      END;
      get_struct(type);
      types[ref_no]:=type; INC(ref_no);
    ELSIF mode<pcY.module THEN
      new_obj(obj,modes[mode]); m:=0;
      pcM.get(t); type:=types[INTEGER(t)];
      IF (mode>=pcY.xproc) & (mode<=pcY.iproc) THEN
        new_type(obj^.type,pcK.proctype);
        obj^.type^.size:=pcM.proctype;
        obj^.type^.base:=type;
        type:=obj^.type;
        type^.next:=phead;
        DEC(plev);
        WITH params[plev] DO phead:=h; ptail:=t END;
        pcM.get(key);
        IF key#0 THEN obj^.tags:={pcK.external}
        ELSIF self THEN obj^.tags:={pcK.forward,pcK.exported}
        END;
      ELSIF (mode=pcY.type) OR (mode=pcY.hdtype) THEN
        pcM.get(m); obj^.type:=type;
        IF type^.obj=NIL THEN type^.obj:=obj END;
        IF mode=pcY.type THEN obj^.tags:={pcK.exported} END;
      ELSIF mode=pcY.xcons THEN
        pcM.get(a); obj^.scope:=mods[INTEGER(a)]^.scope;
        obj^.type:=type;
      ELSE obj^.type:=type;
      END;
      pcM.get_name(obj^.name);
      obj^.scope:=mods[INTEGER(m)]^.scope;
      get_object(obj);
      insert(mods[INTEGER(m)]^.r,obj,old);
      IF old#NIL THEN
        IF obj^.mode=pcK.type THEN
          types[INTEGER(t)]:=old^.type; rem_type(obj^.type)
        END;
        pcM.DEALLOCATE(obj,SIZE(obj^));
      END;
    ELSE
      CASE mode OF
        |pcY.field:
           new_obj(obj,pcK.field);
           pcM.get(t); obj^.type:=types[INTEGER(t)];
           pcM.get_name(obj^.name); INCL(obj^.tags,pcK.exported);
           get_object(obj);
           IF fhead=NIL THEN fhead:=obj ELSE ftail^.next:=obj END;
           ftail:=obj;
        |pcY.method:
           pcM.get(t); type:=types[INTEGER(t)];
           new_obj(obj,pcK.method); obj^.type:=type;
           new_obj(x,pcK.xproc); obj^.head:=x;
           new_type(x^.type,pcK.proctype);
           x^.type^.size:=pcM.proctype;
           pcM.get(t);
           x^.type^.base:=types[INTEGER(t)];
           x^.type^.next:=phead;
           DEC(plev);
           WITH params[plev] DO phead:=h; ptail:=t END;
           pcM.get_name(obj^.name); INCL(obj^.tags,pcK.exported);
           pcM.str_copy(x^.name,obj^.name);
           get_object(obj);
           insert(type^.locs,obj,old);
           IF old#NIL THEN pcM.DEALLOCATE(obj,SIZE(obj^)) END;
        |pcY.varpar,pcY.seq,pcY.varseq,pcY.valpar:
           new_obj(obj,modes[mode]);
           pcM.get(t); obj^.type:=types[INTEGER(t)];
           pcM.get_name(obj^.name);
           obj^.scope:=1;
           get_object(obj);
           IF phead=NIL THEN phead:=obj ELSE ptail^.next:=obj END;
           ptail:=obj;
        |pcY.module :
           pcM.get4(key); pcM.get_name(mname);
(*!!           IF NOT self & (mname=cu_name) THEN pcS.err(24) END;*)
           i:=0;
           WHILE (i<ext_no) & NOT pcM.str_equ(mname,exts[i]^.name) DO
             INC(i)
           END;
           IF i<ext_no THEN
             obj:=exts[i];
             IF self & (i=0) THEN obj^.adr:=key END;
             IF key#obj^.adr THEN pcS.err_id(192,mname); pcS.fault:=TRUE END;
           ELSE
             new_obj(obj,0);
             IF ext_no>max_ext THEN pcS.err(226)
             ELSE exts[ext_no]:=obj; INC(ext_no);
             END;
             pcM.str_copy(obj^.name,mname);
             obj^.scope:=1-ext_no;
             obj^.adr:=key;
           END;
           IF mod_no>max_ext THEN pcS.err(226)
           ELSE mods[mod_no]:=obj; INC(mod_no);
           END;
        |pcY.linkage:
           pcM.get(t); pcM.get(a);
           type:=types[INTEGER(t)];
           IF type^.base=undef THEN type^.base:=types[INTEGER(a)] END;
        |pcY.plist:
           WITH params[plev] DO h:=phead; t:=ptail END; INC(plev);
           phead:=NIL; ptail:=NIL;
        |pcY.flist   :
           WITH fields[flev] DO h:=fhead; t:=ftail END; INC(flev);
           fhead:=NIL; ftail:=NIL;
        |pcY.readonly:
           IF NOT self THEN INCL(obj^.tags,pcK.RO) END;
        |pcY.eosf:
           IF self THEN get_object(mods[0]^.head) END;
           EXIT
      ELSE pcM.abort;
      END;
    END;
  END;
  head:=mods[0];
END read_sym;

PROCEDURE import(o: pcK.OBJECT; imp_name: pcK.NAME; self: BOOLEAN);
  VAR done: BOOLEAN; tag: LONGINT; vers,x: SINT;
BEGIN
  IF pcM.str_equ(imp_name,"SYSTEM") THEN o^.head:=sysmod^.head;
  ELSE
    pcM.open(imp_name,FALSE,done);
    IF done THEN
      pcM.get4(tag); pcM.get(vers);
      IF tag#pcY.SYMTAG THEN pcS.err_id(190,imp_name); pcS.fault:=TRUE
      ELSIF vers#VERS   THEN pcS.err_id(191,imp_name); pcS.fault:=TRUE
      ELSE
        pcM.get(x); pcM.get(x);
        IF self THEN o^.head^.scope:=0 END;
        read_sym(self,o^.head);
        o^.scope:=o^.head^.scope;
      END;
      pcM.close;
    ELSE pcS.fault:=TRUE; INC(pcS.no_errs);
    END;
  END;
END import;

(*----------------------------------------------------------------*)

PROCEDURE standard(mode: SHORTINT; name: pcK.NAME; proc_no: LONGINT);
  VAR o: pcK.OBJECT;
BEGIN
  new_obj(o,mode);
  pcM.str_copy(o^.name,name);
  o^.adr :=proc_no;
  dcl(o);
END standard;

PROCEDURE system(mode: SHORTINT; name: pcK.NAME; proc_no: LONGINT);
  VAR o: pcK.OBJECT; i: CARDINAL;
BEGIN
  new_obj(o,mode);
  pcM.str_copy(o^.name,name);
  o^.adr :=proc_no;
  dcl_in(sysmod,o);
END system;

PROCEDURE Ini;

  PROCEDURE type(t: pcK.STRUCT; name: pcK.NAME);
    VAR o: pcK.OBJECT;
  BEGIN
    new_obj(o,pcK.type); pcM.str_copy(o^.name,name);
    o^.type:=t; o^.tags:={pcK.exported};
    IF t^.obj=NIL THEN t^.obj:=o END;
    dcl(o);
  END type;

  PROCEDURE struct0(VAR t: pcK.STRUCT; m: pcK.Form; r,z: LONGINT);
  BEGIN
    new_type(t,m); t^.ref:=r; t^.size:=z;
  END struct0;

  PROCEDURE struct(VAR t: pcK.STRUCT; m: pcK.Form; r,z: LONGINT; name: pcK.NAME);
  BEGIN
    new_type(t,m); t^.ref:=r; t^.size:=z; type(t,name);
  END struct;

  PROCEDURE const(VAR o: pcK.OBJECT; name: pcK.NAME; t: pcK.STRUCT; val: LONGINT);
  BEGIN
    new_obj(o,pcK.cons); pcM.str_copy(o^.name,name);
    o^.type:=t; o^.scope:=0; o^.adr :=val;
    dcl(o);
  END const;

BEGIN
  cu_name:=''; def:=FALSE; imp:=FALSE;
  ext_no:=0;
  levs:=NIL; level:=-2;
  new_obj(super,pcK.proc);
  new_obj(sysmod,pcK.proc);
  super^.type:=NIL;
  enter_scope(super);

  struct0(invtype,pcK.invtype,pcY.undef  ,1);
  struct0(undef  ,pcK.undef  ,pcY.undef  ,1);
  struct0(niltype,pcK.niltype,pcY.niltype,pcM.addr);
  struct0(shortIC,pcK.shortIC,pcY.shortIC,pcM.byte);
  struct0(IC     ,pcK.IC     ,pcY.IC     ,pcM.byte);

  struct(shortint ,pcK.shortint ,pcY.shortint ,pcM.byte    ,"SHORTINT");
  struct(shortcard,pcK.shortcard,pcY.shortcard,pcM.byte    ,"SHORTCARD");
  struct(integer  ,pcK.integer  ,pcY.integer  ,pcM.integer ,"INTEGER");
  struct(cardinal ,pcK.cardinal ,pcY.cardinal ,pcM.cardinal,"CARDINAL");
  struct(longint  ,pcK.longint  ,pcY.longint  ,pcM.longint ,"LONGINT");
  struct(boolean  ,pcK.boolean  ,pcY.boolean  ,pcM.boolean ,"BOOLEAN");
  struct(char     ,pcK.char     ,pcY.char     ,pcM.byte    ,"CHAR");
  struct(real     ,pcK.real     ,pcY.real     ,pcM.real    ,"REAL");
  struct(longreal ,pcK.longreal ,pcY.longreal ,pcM.longreal,"LONGREAL");
  struct(bitset   ,pcK.bitset   ,pcY.bitset   ,pcM.bitset  ,"BITSET");
  bitset^.base:=shortint;
  type(bitset,"SET");
  struct(byte,pcK.byte,pcY.byte,pcM.byte,"BYTE");

(* MODULE SYSTEM *)
  enter_scope(sysmod);
    struct(addr,pcK.addr,pcY.addr,pcM.addr,"ADDRESS");
    struct(word,pcK.word,pcY.word,pcM.word,"WORD");
    type(byte,"BYTE");
  exit_scope;
(* END SYSTEM *)

  new_obj(inv_obj,pcK.inv); inv_obj^.type:=invtype;
  pcM.str_copy(inv_obj^.name,'*INV*');

  const(false,"FALSE",boolean,0);
  const(true, "TRUE" ,boolean,1);

  standard(pcK.sproc,"INC",pcK.inc);
  standard(pcK.sproc,"DEC",pcK.dec);
  standard(pcK.sproc,"INCL",pcK.incl);
  standard(pcK.sproc,"EXCL",pcK.excl);
  standard(pcK.sproc,"NEW",pcK.new);
  standard(pcK.sproc,"RESIZE",pcK.resize);
  standard(pcK.sproc,"HALT",pcK.halt);
  standard(pcK.sproc,"COPY",pcK.copy);
  standard(pcK.sproc,"ASSERT",pcK.assert);

  standard(pcK.sfunc,"MAX",pcK.max);
  standard(pcK.sfunc,"MIN",pcK.min);
  standard(pcK.sfunc,"ABS",pcK.abs);
  standard(pcK.sfunc,"CAP",pcK.cap);
  standard(pcK.sfunc,"LEN",pcK.len);
  standard(pcK.sfunc,"HIGH",pcK.high);
  standard(pcK.sfunc,"ODD",pcK.odd);
  standard(pcK.sfunc,"SIZE",pcK.size);
  standard(pcK.sfunc,"ASH",pcK.ash);

  system(pcK.sfunc,"ADR",pcK.adr);
END Ini;

PROCEDURE _object(x: pcK.OBJECT); BEGIN pcM.abort END _object;
PROCEDURE _struct(x: pcK.STRUCT); BEGIN pcM.abort END _struct;

VAR i: INTEGER; f: pcK.Form;

BEGIN
  get_object:=_object;
  put_object:=_object;
  put_struct:=_struct;
  get_struct:=_struct;
  FOR i:=0  TO HIGH(forms) DO forms[i]:=pcK.invtype END;
  FOR i:=16 TO HIGH(modes) DO modes[i]:=0 END;
  forms[pcY.range   ]:=pcK.range;
  forms[pcY.enum    ]:=pcK.enum;
  forms[pcY.opaque  ]:=pcK.opaque;
  forms[pcY.pointer ]:=pcK.pointer;
  forms[pcY.set     ]:=pcK.set;
  forms[pcY.proctype]:=pcK.proctype;
  forms[pcY.array   ]:=pcK.array;
  forms[pcY.vector  ]:=pcK.vector;
  forms[pcY.array_of]:=pcK.array_of;
  forms[pcY.dynarr  ]:=pcK.dynarr;
  forms[pcY.record  ]:=pcK.record;
  modes[pcY.cons    ]:=pcK.cons;
  modes[pcY.xcons   ]:=pcK.cons;
  modes[pcY.var     ]:=pcK.var;
  modes[pcY.xproc   ]:=pcK.xproc;
  modes[pcY.iproc   ]:=pcK.iproc;
  modes[pcY.cproc   ]:=pcK.cproc;
  modes[pcY.type    ]:=pcK.type;
  modes[pcY.hdtype  ]:=pcK.type;
  modes[pcY.field   ]:=pcK.field;
  modes[pcY.method  ]:=pcK.method;
  modes[pcY.varpar  ]:=pcK.varpar;
  modes[pcY.seq     ]:=pcK.seq;
  modes[pcY.varseq  ]:=pcK.varseq;
  modes[pcY.valpar  ]:=pcK.var;
END pcO.
