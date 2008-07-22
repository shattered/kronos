IMPLEMENTATION MODULE xRef; (* Ned 29-Apr-89. (c) KRONOS *)
                            (* Ned 07-Oct-89. (c) KRONOS *)

(*$I-U+*)

IMPORT  sys: SYSTEM;
IMPORT  sym: coolSym;
IMPORT  bio: BIO;
IMPORT  mem: Heap;
IMPORT  str: Strings;
IMPORT  err: defErrors;
IMPORT  env: tskEnv;

WITH STORAGE: mem;

----------------------------------------------------------------

CONST MAGIC = 4F5250h; -- PRO

TYPE
  module_ptr = POINTER TO module_rec;
  module_rec = RECORD
                 magic: INTEGER;
                 cu   : cu_ptr;
                 next : module_ptr;
               END;
  PROJECT    = module_ptr;

VAR
  REF: bio.PATHs;
  SYM: bio.PATHs;

----------------------------  OBJs  ---------------------------
                            --------

PROCEDURE new_obj(VAR o: obj_ptr);
BEGIN
  NEW(o); o^.locs:=NIL; o^.next:=NIL; o^.type:=NIL;
END new_obj;

PROCEDURE app_obj(VAR head: obj_ptr; o: obj_ptr);
  VAR l,p: obj_ptr;
BEGIN l:=head;
  IF l=NIL THEN head:=o; o^.next:=NIL; RETURN END;
  WHILE (l#NIL) & (o^.ofs>l^.ofs) DO p:=l; l:=l^.next END;
  IF l=head THEN o^.next:=l; head:=o;
  ELSE p^.next:=o; o^.next:=l;
  END;
END app_obj;

PROCEDURE lookup_external(VAR pro : PROJECT;
                          VAL name: ARRAY OF CHAR;
                          VAR cu: cu_ptr): BOOLEAN;
  VAR l: module_ptr;
BEGIN
  l:=pro;
  WHILE l#NIL DO
    ASSERT(l^.magic=MAGIC);
    IF l^.cu^.names=name THEN cu:=l^.cu; RETURN TRUE END;
    l:=l^.next;
  END;
  NEW(cu);
  cu^.locs:=NIL;
  cu^.def_time:=-1;
  cu^.imp_time:=-1;
  NEW(cu^.types);
  NEW(cu^.exts);
  NEW(cu^.proc_tab);
  NEW(cu^.names,str.len(name)+1); str.copy(cu^.names,name);
  cu^.language:=-1;
  cu^.complete:=FALSE;
  NEW(l);
  l^.magic:=MAGIC;
  l^.cu:=cu;
  l^.next:=pro; pro:=l;
  RETURN FALSE
END lookup_external;

VAR standard: ARRAY [0..31] OF type_ptr;

PROCEDURE ini_types;

  PROCEDURE new_type(mode: INTEGER);
    VAR t: type_ptr;
  BEGIN NEW(t);
    t^.mode:=mode; t^.base:=NIL; t^.id:=-1; t^.modno:=0;
    standard[mode]:=t;
  END new_type;

  PROCEDURE string;
    VAR t: type_ptr;
  BEGIN NEW(t);
    t^.mode:=sym.dynarr; t^.base:=standard[sym.char];
    t^.dim:=1; t^.id:=-1; t^.modno:=0;
    standard[sym.string]:=t;
  END string;

  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(standard) DO standard[i]:=NIL END;
  new_type(sym.addr);
  new_type(sym.word);
  new_type(sym.int);
  new_type(sym.char);
  new_type(sym.real);
  new_type(sym.bool);
  new_type(sym.bitset);
  new_type(sym.nan);
  string;
END ini_types;

PROCEDURE unpack_sym(VAR pro : PROJECT;
                     VAR cu  : cu_ptr;
                     VAL text: ARRAY OF CHAR;
                        t_pos: INTEGER);

                         ----- IO -----

  PROCEDURE get(VAR b: sys.WORD);
  BEGIN b:=ORD(text[t_pos]); INC(t_pos);
  END get;

  PROCEDURE get2(VAR b: INTEGER);
  BEGIN b:=ORD(text[t_pos])+ORD(text[t_pos+1])*100h; INC(t_pos,2);
  END get2;

  PROCEDURE get4(VAR b: INTEGER);
    VAR h,l: INTEGER;
  BEGIN get2(l); get2(h);
    b:=INTEGER( BITSET(l)+BITSET(h<<16) );
  END get4;

  PROCEDURE getX(VAR b: sys.WORD);
  BEGIN b:=ORD(text[t_pos]); INC(t_pos);
    IF    INTEGER(b)>=8 THEN b:=INTEGER(b)-128
    ELSIF INTEGER(b)=0  THEN get(b)
    ELSIF INTEGER(b)=1  THEN get(b); b:=-INTEGER(b);
    ELSIF INTEGER(b)=2  THEN get2(b)
    ELSIF INTEGER(b)=3  THEN get4(b)
    ELSE ASSERT(FALSE)
    END
  END getX;

  PROCEDURE scan_name(VAR s: ARRAY OF CHAR; VAR len: INTEGER);
    VAR i: INTEGER; c: CHAR;
  BEGIN i:=0;
    REPEAT
      c:=text[t_pos]; INC(t_pos); s[i]:=c; INC(i);
    UNTIL c=0c;
    len:=i+1;
  END scan_name;

                        ----- IDs -----

  VAR
    names: STRING;
    n_pos: INTEGER;

  PROCEDURE get_name(VAR id: INTEGER);
    VAR c: CHAR;
  BEGIN id:=n_pos;
    REPEAT
      c:=text[t_pos]; INC(t_pos);
      IF n_pos>HIGH(names) THEN RESIZE(names,names^.HIGH+1025) END;
      names[n_pos]:=c; INC(n_pos);
    UNTIL c=0c;
    IF n_pos=id+1 THEN n_pos:=id; id:=-1 END;
  END get_name;

                       ----- TYPEs -----

  VAR types: TYPEs;
    type_no: INTEGER;

  PROCEDURE new_type(VAR t: type_ptr; mode: INTEGER);
    VAR i,n: INTEGER;
  BEGIN NEW(t);
    t^.mode:=mode; t^.base:=NIL; t^.id:=-1; t^.modno:=0;
    n:=type_no-32;
    IF n>HIGH(types) THEN
      i:=types^.HIGH+1;
      RESIZE(types,types^.HIGH+129);
      FOR i:=i TO types^.HIGH DO types[i]:=NIL END;
    END;
    types[n]:=t; INC(type_no);
  END new_type;

  PROCEDURE get_type(VAR t: type_ptr);
    VAR refno: INTEGER;
  BEGIN getX(refno);
    IF refno<32 THEN t:=standard[refno] ELSE t:=types[refno-32] END;
    ASSERT(t#NIL);
  END get_type;

  PROCEDURE ini_types;
    VAR i: INTEGER;
  BEGIN type_no:=32;
    NEW(types,128);
    FOR i:=0 TO HIGH(types) DO types[i]:=NIL END;
  END ini_types;

                      ------ IMPORT -----

  VAR exts: EXTs;
    ext_no: INTEGER;

  PROCEDURE import;
    VAR ext: cu_ptr; time,unit,x,len: INTEGER;
       name: ARRAY [0..255] OF CHAR;
  BEGIN
    scan_name(name,len);
    IF ext_no=0 THEN
      IF NOT lookup_external(pro,name,ext) THEN ASSERT(FALSE) END;
      ASSERT(NOT ext^.complete);
      n_pos:=len; names^:=cu^.names^;
      get_name(ext^.language);
    ELSE
      IF NOT lookup_external(pro,name,ext) THEN RESIZE(ext^.names,len+1) END;
      scan_name(name,len); -- language
    END;
    getX(time); get(unit); getX(x);
    IF ext^.complete THEN
      IF done & (ext^.def_time#time) THEN
        str.print(note,'time conflict: %s IMPORTs %s',cu^.names,ext^.names);
        error:=err.inconsistency; done:=FALSE;
      END;
    ELSE
      ext^.unit:=unit;
      ext^.def_time:=time;
      IF unit=sym.prog THEN ext^.imp_time:=time END;
      IF (ORD(text[t_pos])=sym.atrs) THEN INC(t_pos);
        get(x); getX(ext^.imp_time);
        WHILE x>1 DO getX(unit) END;
      END;
    END;
    IF ext_no>HIGH(exts) THEN RESIZE(exts,exts^.HIGH+9) END;
    exts[ext_no]:=ext; INC(ext_no);
  END import;

                      ----- CONTEXTs -----

  VAR cur: context_ptr;
     proc_tab: OBJs;
     mods: OBJs;

  PROCEDURE new_context(VAR c: context_ptr);
  BEGIN NEW(c);
    c^.procs:=NIL;        c^.vars :=NIL;
    c^.mods :=NIL;        c^.cons :=NIL;
    c^.xpos :=NIL;
  END new_context;

  PROCEDURE end_proc(no: INTEGER);
    VAR p: obj_ptr;
  BEGIN
    new_obj(p); p^.locs:=cur; p^.ofs:=no; p^.id:=-1;
    proc_tab[no]:=p;
    new_context(cur);
  END end_proc;

  PROCEDURE correspondence(p: parm_ptr; vars: obj_ptr);
    VAR l: obj_ptr;
  BEGIN
    WHILE p#NIL DO l:=vars;
      WHILE (l#NIL) & (l^.ofs#p^.ofs) DO l:=l^.next END;
      IF l#NIL THEN p^.var:=l; l^.parm:=p END;
      p:=p^.next;
    END;
  END correspondence;

  PROCEDURE lookup_proc(id: INTEGER; t: type_ptr; scope,ofs: INTEGER);
    VAR p: obj_ptr;
  BEGIN p:=proc_tab[ofs];
    IF p=NIL THEN new_obj(p); p^.ofs:=ofs; proc_tab[ofs]:=p
    ELSE ASSERT(p^.locs#NIL); correspondence(t^.parms,p^.locs^.vars);
    END;
    p^.id:=id; p^.type:=t; p^.scope:=scope;
    app_obj(cur^.procs,p);
  END lookup_proc;

  PROCEDURE xpos(VAR o: obj_ptr);
    VAR l,p,x: xpos_ptr; no: INTEGER;
  BEGIN ASSERT(o#NIL);
    NEW(x);
    getX(x^.pc); get2(x^.line); get(x^.col);
    l:=o^.locs^.xpos;
    IF l=NIL THEN x^.next:=NIL; o^.locs^.xpos:=x
    ELSE p:=NIL;
      WHILE (l#NIL) & (l^.pc<x^.pc) DO p:=l; l:=l^.next END;
      x^.next:=l;
      IF p=NIL THEN o^.locs^.xpos:=x ELSE p^.next:=x END;
    END;
  END xpos;

  PROCEDURE end_module(no: INTEGER);
    VAR o: obj_ptr; i: INTEGER;
  BEGIN
    new_obj(o); o^.locs:=cur; o^.ofs:=no; o^.id:=-1;
    IF no=0 THEN proc_tab[0]:=o; (* compilation unit *)
    ELSE
      IF no>HIGH(mods) THEN i:=mods^.HIGH+1;
        RESIZE(mods,no+1);
        WHILE i<no DO mods[i]:=NIL; INC(i) END;
      END;
      mods[no]:=o;
      new_context(cur);
    END;
  END end_module;

  PROCEDURE lookup_module(id,no: INTEGER);
    VAR o: obj_ptr;
  BEGIN
    IF (no>HIGH(mods)) OR (mods[no]=NIL) THEN new_obj(o); o^.ofs:=no;
    ELSE o:=mods[no]
    END;
    o^.id:=id;
    app_obj(cur^.mods,o);
  END lookup_module;

  PROCEDURE end_cu(proc_no: INTEGER);
  BEGIN ASSERT(proc_tab[0]#NIL);
    DISPOSE(mods);
    RESIZE(types,type_no-32);
    RESIZE(proc_tab,proc_no);
    RESIZE(exts,ext_no);
    cu:=exts[0];
    cu^.locs:=cur;
    cu^.types^:=types^;
    cu^.proc_tab^ :=proc_tab^;
    cu^.exts^ :=exts^;
    cu^.names^:=names^;
    cu^.complete:=TRUE;
  END end_cu;

  PROCEDURE ini_context;
    VAR i: INTEGER;
  BEGIN
    NEW(proc_tab,256);
    FOR i:=0 TO proc_tab^.HIGH DO proc_tab[i]:=NIL END;
    NEW(mods);
    new_context(cur);
  END ini_context;

                        ----- TIE -----

  PROCEDURE tie_enum(VAR head: enum_ptr; id,val: INTEGER);
    VAR e: enum_ptr;
  BEGIN NEW(e);
    e^.id:=id; e^.val:=val; e^.next:=head; head:=e;
  END tie_enum;

  PROCEDURE tie_parm(VAR head: parm_ptr; p: parm_ptr);
    VAR l: parm_ptr;
  BEGIN l:=head;
    IF l=NIL THEN head:=p
    ELSE
      WHILE l^.next#NIL DO l:=l^.next END;
      l^.next:=p;
    END;
  END tie_parm;

                        ----- SCAN -----

  VAR
    tag: INTEGER;
   x,id: INTEGER;
      o: obj_ptr;
      t: type_ptr;
      p: parm_ptr;
  scope: INTEGER;
    ofs: INTEGER;
 fields: obj_ptr;
  parms: parm_ptr;

BEGIN
  ini_types; ext_no:=0;
  NEW(exts);
  ini_context;
  fields:=NIL; parms:=NIL;
  LOOP tag:=ORD(text[t_pos]); INC(t_pos);
    CASE tag OF
    |sym.enumtype: new_type(t,sym.enumtype); t^.consts:=NIL;
    |sym.range   : new_type(t,sym.range);
                   get_type(t^.base); getX(t^.min); getX(t^.max);
    |sym.array   : new_type(t,sym.array); get_type(t^.inx); get_type(t^.base);
    |sym.openarr : new_type(t,sym.openarr); get_type(t^.base);
    |sym.dynarr  : new_type(t,sym.dynarr);  get_type(t^.base); getX(t^.dim);
    |sym.pointer : new_type(t,sym.pointer);
    |sym.record  : new_type(t,sym.record);  get_type(t^.base); getX(t^.size);
                   t^.fields:=fields; fields:=NIL;
    |sym.set     : new_type(t,sym.set); get_type(t^.base);
    |sym.proctype: new_type(t,sym.proctype); t^.base:=NIL;
                   t^.parms:=parms; parms:=NIL;
    |sym.functype: new_type(t,sym.proctype); get_type(t^.base);
                   t^.parms:=parms; parms:=NIL;
    |sym.hidden  : new_type(t,sym.hidden); getX(x);
    |sym.linkage : get_type(t); get_type(t^.base);
----------------------------------------------------------------
    |sym.parm    : NEW(p); p^.next:=NIL; p^.var:=NIL;
                   get_name(id); get_type(p^.type);
                   getX(p^.ofs); getX(p^.tags);
                   tie_parm(parms,p);
    |sym.field   : new_obj(o);
                   get_name(o^.id); get_type(o^.type); getX(o^.ofs);
                   app_obj(fields,o);

    |sym.enum    : get_name(id); get_type(t); getX(x); getX(x);
                   tie_enum(t^.consts,id,x);
    |sym.struct
    ,sym.sconst  : get_name(id); get_type(t); getX(x);
                   IF x=0 THEN  -- scope
                     new_obj(o);
                     o^.id:=id; o^.type:=t; o^.scope:=0;
                     getX(o^.ofs);
                     app_obj(cur^.cons,o);
                   ELSE getX(x)
                   END;
                   IF tag=sym.struct THEN
                     getX(ofs); ofs:=ofs*4;
                     WHILE ofs>0 DO get(x); DEC(ofs) END;
                   END;

    |sym.const   : get_name(id); get_type(t); getX(x);  -- ignore

    |sym.var     : new_obj(o); get_name(o^.id); get_type(o^.type);
                   getX(o^.scope); getX(o^.ofs); getX(o^.tags);
                   o^.parm:=NIL; app_obj(cur^.vars,o);
    |sym.proc    : get_name(id); get_type(t); getX(scope); getX(ofs);
                   lookup_proc(id,t,scope,ofs);
    |sym.type    : get_name(id); getX(x); getX(scope);
                   IF x>=32 THEN t:=types[x-32]; ASSERT(t#NIL);
                     IF t^.id<0 THEN t^.modno:=scope; t^.id:=id END;
                   END;

    |sym.module   : get_name(id); getX(ofs); lookup_module(id,ofs);
    |sym.import   : import
    |sym.endproc  : getX(x); end_proc(x);
    |sym.endmodule: getX(x); end_module(x);
    |sym.end_CU   : getX(x); end_cu(x); EXIT

    |sym.xpos     : getX(x); xpos(proc_tab[x]);

    |sym.strange  : getX(x); WHILE x>0 DO get (ofs); DEC(x) END;
    |sym.atrs     : get (x); WHILE x>0 DO getX(ofs); DEC(x) END;
    ELSE ASSERT(FALSE,100h+tag);
    END;
  END;
END unpack_sym;

PROCEDURE check_header(VAL text: ARRAY OF CHAR; VAR pos: INTEGER);
  VAR head: POINTER TO sym.header;
BEGIN
  done:=FALSE;
  head:=sys.ADR(text);
  IF head^.magic=sym.MAGIC THEN
    IF head^.vers#sym.VERSION THEN
      str.print(note,'version %d # %d',head^.vers,sym.VERSION);
      error:=err.ill_vers;
    ELSE
      pos:=head^.offset; done:=TRUE
    END;
  ELSIF (ORD(text[0])#0FFh) OR (ORD(text[1])#0FEh) THEN
    error:=err.inconsistency;
  ELSIF ORD(text[2])#2 THEN
    str.print(note,'version %d # %d',text[2],2);
    error:=err.ill_vers;
  ELSE
    pos:=3; done:=TRUE
  END;
END check_header;

PROCEDURE read_cu(VAR pro : PROJECT;
                  VAR cu  : cu_ptr;
                  VAL name: ARRAY OF CHAR;
                       ref: BOOLEAN);

  PROCEDURE read_text(VAL name: ARRAY OF CHAR; VAR text: STRING);
    VAR f: bio.FILE; eof: INTEGER;
  BEGIN
    IF ref THEN
      bio.lookup(REF,f,name,'r');
    ELSE
      bio.lookup(SYM,f,name,'r');
    END;
    IF NOT bio.done THEN error:=bio.error; RETURN END;
    eof:=bio.eof(f);
    IF bio.done THEN
      NEW(text,eof);
      bio.read(f,text^.ADR,eof);
    END;
    IF NOT bio.done THEN error:=bio.error; bio.close(f); RETURN END;
    bio.close(f);
    done:=bio.done;
    IF NOT done THEN error:=bio.error END;
  END read_text;

  VAR fn: ARRAY [0..255] OF CHAR; r,pos: INTEGER;
    text: STRING;

BEGIN
  note[0]:=0c;
  IF lookup_external(pro,name,cu) & cu^.complete THEN done:=TRUE; RETURN END;
  done:=FALSE;
  IF ref THEN str.print(fn,'%s.ref',name);
  ELSE        str.print(fn,'%s.sym',name);
  END;
  NEW(text);
  read_text(fn,text);
  IF done THEN
    check_header(text,pos);
    IF done THEN unpack_sym(pro,cu,text,pos) END;
  END;
  DISPOSE(text);
END read_cu;

PROCEDURE enter_cu(VAR pro : PROJECT;
                   VAR cu  : cu_ptr;
                   VAL name: ARRAY OF CHAR;
                   VAL text: ARRAY OF CHAR);
  VAR pos: INTEGER;
BEGIN
  note[0]:=0c;
  IF lookup_external(pro,name,cu) & cu^.complete THEN done:=TRUE; RETURN END;
  done:=FALSE;
  check_header(text,pos);
  IF done THEN unpack_sym(pro,cu,text,pos) END;
END enter_cu;

----------------------------  EXIT  ---------------------------
                            --------

PROCEDURE exit_cu(cu: cu_ptr);

  PROCEDURE dispose_xpos(l: xpos_ptr);
    VAR d: xpos_ptr;
  BEGIN
    WHILE l#NIL DO d:=l; l:=l^.next; DISPOSE(d) END;
  END dispose_xpos;

  PROCEDURE dispose_context(VAR c: context_ptr); FORWARD;

  PROCEDURE dispose_list(l: obj_ptr; cont: BOOLEAN);
    VAR d: obj_ptr;
  BEGIN
    WHILE l#NIL DO
      IF cont THEN dispose_context(l^.locs) END;
      d:=l; l:=l^.next;
      DISPOSE(d);
    END;
  END dispose_list;

  PROCEDURE dispose_context(VAR c: context_ptr);
  BEGIN
    IF c=NIL THEN RETURN END;
    dispose_list(c^.procs,TRUE);
    dispose_list(c^.mods, TRUE);
    dispose_list(c^.vars,FALSE);
    dispose_list(c^.cons,FALSE);
    dispose_xpos(c^.xpos);
    DISPOSE(c);
  END dispose_context;

  PROCEDURE dispose_types(VAL types: ARRAY OF type_ptr);
    VAR i: INTEGER; t: type_ptr; e,d: enum_ptr; p,x: parm_ptr;
  BEGIN i:=HIGH(types);
    WHILE i>=0 DO
      t:=types[i]; ASSERT(t#NIL);
      CASE t^.mode OF
        |sym.enumtype: e:=t^.consts;
                       WHILE e#NIL DO d:=e; e:=e^.next; DISPOSE(e) END;
        |sym.proctype: p:=t^.parms;
                       WHILE p#NIL DO x:=p; p:=p^.next; DISPOSE(x) END;
        |sym.record  : dispose_list(t^.fields,FALSE);
      ELSE
      END;
      DISPOSE(t);
      DEC(i);
    END;
  END dispose_types;

BEGIN
  note[0]:=0c;
  done:=TRUE;
  IF NOT cu^.complete THEN RETURN END;
  RESIZE(cu^.names,cu^.language); cu^.language:=-1;
  dispose_context(cu^.locs);
  DISPOSE(cu^.exts);
  DISPOSE(cu^.proc_tab);
  dispose_types(cu^.types);
  DISPOSE(cu^.types);
  cu^.complete:=FALSE;
END exit_cu;

PROCEDURE new(VAR pro: PROJECT);
BEGIN
  pro:=NIL;
END new;

PROCEDURE release(VAR pro: PROJECT);
  VAR x: module_ptr;
BEGIN
  note[0]:=0c;
  WHILE pro#NIL DO
    ASSERT(pro^.magic=MAGIC);
    x:=pro; pro:=x^.next;
    exit_cu(x^.cu);
    DISPOSE(x^.cu^.names);
    DISPOSE(x^.cu);
    DISPOSE(x);
  END;
END release;

--------------------  AUXILIARY PROCEDUREs  -------------------
                    ------------------------

PROCEDURE text_pos(l: xpos_ptr; pc: INTEGER; VAR line,col: INTEGER);
  VAR p: xpos_ptr;
BEGIN
  IF l=NIL THEN line:=-1; col:=-1; RETURN END;
  p:=l;
  WHILE (l#NIL) & (l^.pc<pc) DO p:=l; l:=l^.next END;
  IF l=NIL THEN l:=p END;
  line:=l^.line; col:=l^.col;
END text_pos;

PROCEDURE id_str(VAL names: ARRAY OF CHAR; id: INTEGER; VAR s: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN i:=0;
  WHILE (i<=HIGH(s)) & (names[id]#0c) DO
    s[i]:=names[id]; INC(i); INC(id);
  END;
  IF i<=HIGH(s) THEN s[i]:=0c END;
END id_str;

BEGIN
  ini_types;
  main:=NIL;
  bio.get_paths(REF,env.ref);
  IF NOT bio.done THEN REF:=bio.here END;
  bio.get_paths(SYM,env.ref);
  IF NOT bio.done THEN SYM:=bio.here END;
END xRef.
