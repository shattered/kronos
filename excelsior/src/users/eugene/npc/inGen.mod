IMPLEMENTATION MODULE inGen; (* Ned 04-Apr-91. (c) KRONOS *)

IMPORT SYSTEM;
IMPORT pcK, pcM, pcS, pcO, inExp, inSta;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;

CONST
  pc    ::= pcK;
  inter ::= pcM;
  scan  ::= pcS;
  obs   ::= pcO;
  exp   ::= inExp;
  sta   ::= inSta;

TYPE
  SET32 = SET OF [0..31];

CONST
  global_ofs = 0;
  threshold  = 8000H;
  max_type_size = 0FFFFH;
  ext_link   = 12;
  glo_link   =  4;
  loc_link   =  6;

VAR
  proc_no : CARDINAL;
  module  : pc.OBJECT;

PROCEDURE err_ps(ps: LONGINT; n: INTEGER);
  VAR s: LONGINT;
BEGIN
  s:=scan.txtpos; scan.txtpos:=ps; scan.err(n); scan.txtpos:=s;
END err_ps;

(*----------------------------------------------------------------*)
PROCEDURE type_size(type: pc.STRUCT; alloc: BOOLEAN); FORWARD;
(*----------------------------------------------------------------*)

PROCEDURE range(type: pc.STRUCT; VAR l,h: LONGINT);
BEGIN
  CASE type^.mode OF
    |pc.invtype,pc.undef         : l:=0; h:=1;
    |pc.shortint                 : l:=-128; h:=127;
    |pc.char,pc.byte,pc.shortcard: l:=0; h:=255;
    |pc.boolean                  : l:=0; h:=1;
    |pc.integer                  : l:=MIN(INTEGER); h:=MAX(INTEGER);
    |pc.longint                  : l:=MIN(LONGINT); h:=MAX(LONGINT);
    |pc.range,pc.enum            : l:=type^.n; h:=type^.m;
  END;
END range;

PROCEDURE proctype(type: pc.STRUCT; link_size: LONGINT);

  PROCEDURE parm(l: pc.OBJECT; VAR no: LONGINT);
    VAR w: LONGINT;
  BEGIN
    IF l=NIL THEN RETURN END;
    parm(l^.next,no);
    type_size(l^.type,inter.oberon);
    IF l^.type^.mode=pc.array_of THEN w:=l^.type^.size;
    ELSIF inter.oberon & (l^.mode=pc.varpar) & (l^.type^.mode=pc.record) THEN
      w:=8
    ELSIF (l^.mode=pc.varpar) OR (l^.type^.size>2) THEN w:=4
    ELSE w:=2
    END;
    l^.adr:=no; INC(no,w);
  END parm;

  VAR no: LONGINT;
BEGIN
  no:=link_size;
  parm(type^.next,no);
  type^.adr:=no-link_size;
  type^.size:=4;
END proctype;

PROCEDURE wrap(VAR n: LONGINT; sz: LONGINT);
BEGIN
  IF sz>2 THEN WHILE n MOD 4 # 0 DO INC(n) END;
  ELSIF sz>1 THEN WHILE n MOD 2 # 0 DO INC(n) END;
  END;
END wrap;

PROCEDURE type_size(type: pc.STRUCT; alloc: BOOLEAN);

  PROCEDURE array(type: pc.STRUCT);
    VAR l,h: LONGINT;
  BEGIN
    IF type^.mode=pc.array THEN
      type_size(type^.inx,alloc);
      range(type^.inx,l,h); l:=h-l+1;
    ELSE l:=type^.n
    END;
    type^.size:=type^.base^.size*l;
    wrap(type^.size,type^.size);
    IF type^.size>max_type_size THEN err_ps(type^.pos,232) END;
  END array;

  PROCEDURE record(type: pc.STRUCT; do_alloc: BOOLEAN);

    PROCEDURE set_ofs(f: pc.OBJECT; VAR ofs: LONGINT);
    BEGIN
      type_size(f^.type,alloc);
(*      wrap(ofs,f^.type^.size);   *)
      f^.adr:=ofs; INC(ofs,f^.type^.size);
    END set_ofs;

    PROCEDURE fields(l: pc.OBJECT; VAR ofs: LONGINT);

      PROCEDURE variant(x: pc.OBJECT; VAR max: LONGINT);
        VAR ofs0,ofs: LONGINT;
      BEGIN
        ofs0:=max;
        WHILE x#NIL DO
          ofs:=ofs0;
          fields(x^.r,ofs);
          IF ofs>max THEN max:=ofs END;
          x:=x^.next;
        END;
      END variant;

    BEGIN
      WHILE l#NIL DO
        IF l^.mode=pc.header THEN
          IF l^.l#NIL THEN set_ofs(l^.l,ofs) END;
          variant(l^.r,ofs);
        ELSE set_ofs(l,ofs);
        END;
        l:=l^.next
      END;
    END fields;

    VAR ofs: LONGINT;
  BEGIN
    IF type^.base=NIL THEN ofs:=0
    ELSE type_size(type^.base,alloc); ofs:=type^.base^.size;
    END;
    fields(type^.next,ofs);
(*    wrap(ofs,ofs);  *)
    type^.size:=ofs;
    IF do_alloc THEN
      inter.abort;
(*      type^.adr:=cod.scope^.vars; INC(cod.scope^.vars)*)
    END;
    IF type^.size>max_type_size THEN err_ps(type^.pos,232) END;
  END record;

  PROCEDURE set(type: pc.STRUCT);
    VAR l,h: LONGINT;
  BEGIN
    range(type^.base,l,h);
    IF h-l<=7 THEN type^.size:=1;
    ELSIF h-l<=15 THEN type^.size:=2;
    ELSIF h-l<=31 THEN type^.size:=4;
    ELSE err_ps(type^.pos,202); type^.size:=4;
    END;
  END set;

  VAR undef,do_alloc: BOOLEAN;
BEGIN
  undef:=(type^.size<0);
  do_alloc:=alloc & (type^.adr=0) & (type^.mno=0);
  IF NOT (undef OR do_alloc) THEN RETURN END;
  IF do_alloc THEN type^.adr:=-1 (* avoid cycles *) END;
  CASE type^.mode OF
    |pc.invtype,pc.undef    : inter.abort; type^.size:=1;
    |pc.shortint..pc.niltype: (*nothing*)
    |pc.range               : type^.size:=type^.base^.size;
    |pc.set                 : type_size(type^.base,alloc);
                              IF undef THEN set(type) END;
    |pc.enum                :
      IF type^.m-type^.n<=0FFH THEN type^.size:=1;
      ELSIF type^.m-type^.n<=0FFFFH THEN type^.size:=2;
      ELSE type^.size:=4;
      END;
    |pc.opaque              : type^.size:=4;
    |pc.array,pc.vector     : type_size(type^.base,alloc);
                              IF undef THEN array(type) END;
    |pc.array_of,pc.dynarr  : type_size(type^.base,alloc);
                              type^.size:=type^.n*8;
    |pc.record              : record(type,do_alloc);
    |pc.pointer             : type^.size:=4;
                              IF do_alloc THEN type_size(type^.base,alloc) END;
    |pc.proctype            :
      IF undef THEN
        proctype(type,ext_link);
        IF (type^.base^.mode#pc.undef) & (type^.base^.size>4) THEN
          err_ps(type^.pos,43);
        END;
      END;
  ELSE inter.abort;
  END;
END type_size;

PROCEDURE traverse(l: pc.OBJECT; ext: exp.EXT);

  PROCEDURE compare(rts,o: pc.OBJECT);

    PROCEDURE param(VAR p: pc.OBJECT; mode: pc.Mode; type: pc.STRUCT);
    BEGIN
      IF p=NIL THEN scan.err_id(38,o^.name); RETURN END;
      IF p^.mode#mode THEN scan.err_id(39,o^.name)
      ELSIF p^.type#type THEN scan.err_id(30,o^.name)
      END;
      p:=p^.next;
    END param;

    VAR p: pc.OBJECT;
  BEGIN
    IF NOT (o^.mode IN {pc.proc,pc.xproc}) THEN err_ps(o^.pos,55); RETURN END;
    p:=o^.type^.next;
    IF rts^.adr=pc.new THEN
      param(p,pc.varpar,obs.addr);
      param(p,pc.var,obs.longint);
(*      cod.alloc:=o;*)
    ELSIF rts^.adr=pc.dispose THEN
      param(p,pc.varpar,obs.addr);
      param(p,pc.var,obs.longint);
(*      cod.dealloc:=o;*)
    ELSIF rts^.adr=pc.resize THEN
      param(p,pc.varpar,obs.addr);
      param(p,pc.varpar,obs.longint);
      param(p,pc.var,obs.longint);
      param(p,pc.var,obs.longint);
(*      cod.realloc:=o;*)
    ELSE inter.abort
    END;
  END compare;

  PROCEDURE var(l: pc.OBJECT; glo: BOOLEAN; VAR ofs: LONGINT);
  BEGIN
    IF NOT glo THEN
      DEC(ofs,l^.type^.size);
      IF (l^.type^.size>1) & ODD(ofs) THEN DEC(ofs) END;
      l^.adr:=ofs;
      IF ofs<-0FFFFH THEN err_ps(l^.pos,220) END;
    ELSIF l^.type^.size>threshold THEN
      wrap(ofs,4); l^.adr:=ofs; INC(ofs,4); INCL(l^.tags,exp.indirect);
      IF ofs>=10000H THEN err_ps(l^.pos,220) END;
    ELSE
      IF (l^.type^.size>1) & ODD(ofs) THEN INC(ofs) END;
      wrap(ofs,l^.type^.size); l^.adr:=ofs; INC(ofs,l^.type^.size);
      IF ofs>=10000H THEN err_ps(l^.pos,220) END;
    END;
  END var;

BEGIN
  WHILE l#NIL DO
    CASE l^.mode OF
      |pc.cons:
         type_size(l^.type,inter.oberon);
         IF l^.type^.size>=10000H THEN scan.err_id(220,l^.name)
         ELSIF l^.type^.mode IN exp.mem_const THEN

           wrap(e^.n,l^.type^.size); l^.adr:=e^.n; INC(e^.n,l^.type^.size);
           IF l^.type^.size>=10000H THEN err_ps(l^.pos,220) END;
           sta.glo_const(l^.adr,l^.val,l^.type^.size);
         END;
      |pc.var:
         type_size(l^.type,inter.oberon); var(l,l^.scope=0,ext^.n);
      |pc.proc,pc.xproc,pc.iproc:
         l^.adr:=LONGINT(proc_no); INC(proc_no);
         IF l^.mode=pc.proc THEN
           IF l^.scope=0 THEN proctype(l^.type,glo_link);
           ELSE proctype(l^.type,loc_link);
           END;
         ELSE proctype(l^.type,ext_link);
         END;
         NEW(e); e^.n:=0; l^.ext:=e;
         traverse(l^.head^.next,e);
      |pc.cproc: proctype(l^.type,0);
      |pc.sproc: compare(l,l^.head);
      |pc.type : type_size(l^.type,inter.oberon);
    END;
    l:=l^.next;
  END;
END traverse;

PROCEDURE prepare(tree: pc.NODE);
  VAR o,v: pc.OBJECT; ext: exp.EXT; l: pc.NODE; t: pc.STRUCT;
BEGIN
  module:=tree^.obj;
  ext:=module^.ext;
  IF ext=NIL THEN
    NEW(ext);
    module^.ext:=ext;
    ext^.n:=global_ofs;  (* +ORD(inter.oberon); *)
  END;
  proc_no:=0;
  traverse(module^.head^.next,ext);
(*
  l:=tree^.next;
  WHILE l#NIL DO
    t:=l^.type;
    IF t^.mode=pc.record THEN
      IF t^.base#NIL THEN t^.m:=t^.base^.m END;
      IF t^.n>7 THEN scan.txtpos:=t^.pos; scan.err(229) END;
      ASSERT(t^.adr>=global_ofs);
      o:=t^.link;
      WHILE o#NIL DO
        IF pc.redefine IN o^.tags THEN
          IF NOT obs.try_in_rec(o^.name,t^.base,v) THEN ASSERT(FALSE) END;
          o^.adr:=v^.adr;
        ELSE o^.adr:=t^.m; INC(t^.m);
        END;
        o:=o^.next;
      END;
    END;
    l:=l^.next;
  END;
*)
END prepare;

(*----------------------------------------------------------------*)

PROCEDURE put_object(o: pc.OBJECT);
BEGIN
  CASE o^.mode OF
    |pc.module:
       inter.put4(o^.adr);
    |pc.var,pc.varpar,pc.seq,pc.varseq,pc.field:
       inter.put4(o^.adr);
    |pc.xproc,pc.iproc:
       inter.put4(o^.type^.adr);
    |pc.method:
       inter.put(o^.head^.type^.adr);
       inter.put(o^.adr);
    |pc.cproc:
       inter.put4(o^.adr);
       inter.put_bytes(o^.val^,o^.adr);
    |pc.cons:
       inter.put4(o^.adr);
       inter.put4(o^.type^.size);
       inter.put_bytes(o^.val^,o^.type^.size);
  ELSE (* nothing *)
  END;
END put_object;

PROCEDURE get_object(o: pc.OBJECT);
  VAR n: LONGINT;
BEGIN
  CASE o^.mode OF
    |pc.module:
       inter.get4(o^.adr);
    |pc.var,pc.varpar,pc.seq,pc.varseq,pc.field:
       inter.get4(o^.adr);
    |pc.xproc,pc.iproc:
       inter.get4(o^.type^.adr);
    |pc.method:
       inter.get(o^.head^.type^.adr);
       inter.get(o^.adr);
    |pc.cproc:
       inter.get4(o^.adr);
       ALLOCATE(o^.val,o^.adr);
       inter.get_bytes(o^.val^,o^.adr);
    |pc.cons:
       inter.get4(o^.adr);
       inter.get4(n);
       ALLOCATE(o^.val,n);
       inter.get_bytes(o^.val^,n);
  ELSE (* nothing *)
  END;
END get_object;

PROCEDURE put_struct(s: pc.STRUCT);
BEGIN
  IF s^.mode=pc.proctype THEN inter.put4(s^.adr)
  ELSIF s^.mode=pc.record THEN inter.put4(s^.adr)
  END;
END put_struct;

PROCEDURE get_struct(s: pc.STRUCT);
BEGIN
  IF s^.mode=pc.proctype THEN inter.get4(s^.adr)
  ELSIF s^.mode=pc.record THEN inter.get4(s^.adr)
  END;
END get_struct;

(*----------------------------------------------------------------*)

PROCEDURE eval(n: pc.NODE; VAR val: pc.VALUE);

  PROCEDURE str_comp(s1,s2: VALUE): INTEGER;
  BEGIN
    WHILE (s1^.b#0) & (s1^.b=s2^.b) DO
      s1:=ADDRESS(s1)+1; s2:=ADDRESS(s2)+1;
    END;
    IF s1^.b<s2^.b THEN RETURN -1
    ELSIF s1^.b>s2^.b THEN RETURN 1
    ELSE RETURN 0
    END;
  END str_comp;

  PROCEDURE arr_comp(s1,s2: VALUE; sz: LONGINT): BOOLEAN;
  BEGIN
    WHILE sz>0 DO
      IF s1^.b#s2^.b) THEN RETURN FALSE END;
      s1:=ADDRESS(s1)+1; s2:=ADDRESS(s2)+1; DEC(sz);
    END;
    RETURN TRUE;
  END arr_comp;

  PROCEDURE constop(n: pc.NODE; x,y: VALUE; VAR z: VALUE);
    VAR mode: pc.Form;
  BEGIN
    ALLOCATE(z,n^.type^.size);
    CASE n^.sub OF
      |pc.equ:
         IF exp.vm(n^.l^.type)=exp.vm_string THEN
           z^.b:=str_comp(x,y)=0;
         ELSE
           z^.b:=arr_comp(x,y,n^.l^.type^.size);
         END;
      |pc.neq:
         IF exp.vm(n^.l^.type)=exp.vm_string THEN
           z^.b:=str_comp(x,y)#0;
         ELSE
           z^.b:=NOT arr_comp(x,y,n^.l^.type^.size);
         END;
      |pc.lss:
         CASE n^.l^.type^.mode OF
           |pc.real     : z^.b:=x^.r<y^.r;
           |pc.longreal : z^.b:=x^.lr<y^.lr;
           |pc.shortint : z^.b:=x^.si<y^.si;
           |pc.integer  : z^.b:=x^.i<y^.i;
           |pc.longint  : z^.b:=x^.li<y^.li;
           |pc.shortcard: z^.b:=x^.sc<y^.sc;
           |pc.cardinal : z^.b:=x^.c<y^.c;
           |pc.longcard : z^.b:=x^lc<y^.lc;
           |pc.shortIC  : z^.b:=x^.si<y^.si;
           |pc.IC       : z^.b:=x^.i<y^.i;
           |pc.boolean  : z^.b:=x^.b<y^.b;
           |pc.char     : z^.b:=x^.b<y^.b;
           |pc.byte     : z^.b:=x^.b<y^.b;
           |pc.word     : z^.b:=x^.w<y^.w;
           |pc.addr     : z^.b:=x^.a<y^.a;
           |pc.enum     : z^.b:=x^.b<y^.b;
         ELSE
           IF exp.vm(n^.l^.type)=exp.vm_string THEN
             z^.b:=str_comp(x,y)<0;
           ELSE
             err_ps(n^.pos,0);
           END;
         END;
      |pc.leq:
         CASE n^.l^.type^.mode OF
           |pc.real     : z^.b:=x^.r<=y^.r;
           |pc.longreal : z^.b:=x^.lr<=y^.lr;
           |pc.shortint : z^.b:=x^.si<=y^.si;
           |pc.integer  : z^.b:=x^.i<=y^.i;
           |pc.longint  : z^.b:=x^.li<=y^.li;
           |pc.shortcard: z^.b:=x^.sc<=y^.sc;
           |pc.cardinal : z^.b:=x^.c<=y^.c;
           |pc.longcard : z^.b:=x^lc<=y^.lc;
           |pc.shortIC  : z^.b:=x^.si<=y^.si;
           |pc.IC       : z^.b:=x^.i<=y^.i;
           |pc.boolean  : z^.b:=x^.b<=y^.b;
           |pc.char     : z^.b:=x^.b<=y^.b;
           |pc.byte     : z^.b:=x^.b<=y^.b;
           |pc.word     : z^.b:=x^.w<=y^.w;
           |pc.addr     : z^.b:=x^.a<=y^.a;
           |pc.enum     : z^.b:=x^.b<=y^.b;
         ELSE
           IF exp.vm(n^.l^.type)=exp.vm_string THEN
             z^.b:=str_comp(x,y)<=0;
           ELSE
             err_ps(n^.pos,0);
           END;
         END;
         IF mode=pc.real        THEN val:=LONGINT( ext^.real<=e1^.real );
         ELSIF mode=pc.longreal THEN val:=LONGINT( ext^.lrl<=e1^.lrl );
         ELSE val:=LONGINT( val<=v1 );
         END;
      |pc.gtr:
         IF mode=pc.real        THEN val:=LONGINT( ext^.real>e1^.real );
         ELSIF mode=pc.longreal THEN val:=LONGINT( ext^.lrl>e1^.lrl );
         ELSE val:=LONGINT( val>v1 );
         END;
      |pc.geq:
         IF mode=pc.real        THEN val:=LONGINT( ext^.real>=e1^.real );
         ELSIF mode=pc.longreal THEN val:=LONGINT( ext^.lrl>=e1^.lrl );
         ELSE val:=LONGINT( val>=v1 );
         END;
      |pc.in :
         IF (val<0) OR (val>31) THEN val:=0
         ELSE val:=LONGINT( CARDINAL(val) IN SET32(v1));
         END;
      |pc.mul:
         IF mode=pc.real        THEN ext^.real:=ext^.real*e1^.real
         ELSIF mode=pc.longreal THEN ext^.lrl:=ext^.lrl*e1^.lrl;
         ELSE val:=val*v1;
         END;
      |pc.div:
         IF mode=pc.real        THEN ext^.real:=ext^.real/e1^.real
         ELSIF mode=pc.longreal THEN ext^.lrl:=ext^.lrl/e1^.lrl;
         ELSE val:=val DIV v1;
         END;
      |pc.mod: val:=val MOD v1;
      |pc.plus:
         IF mode=pc.real        THEN ext^.real:=ext^.real+e1^.real
         ELSIF mode=pc.longreal THEN ext^.lrl:=ext^.lrl+e1^.lrl;
         ELSE val:=val+v1;
         END;
      |pc.minus:
         IF mode=pc.real        THEN ext^.real:=ext^.real-e1^.real
         ELSIF mode=pc.longreal THEN ext^.lrl:=ext^.lrl-e1^.lrl;
         ELSE val:=val-v1;
         END;
      |pc.and : val:=LONGINT( SET32(val)*SET32(v1) );
      |pc.or  : val:=LONGINT( SET32(val)+SET32(v1) );
      |pc.xor : val:=LONGINT( SET32(val)/SET32(v1) );
      |pc.bic : val:=LONGINT( SET32(val)-SET32(v1) );
      |pc.cand: val:=LONGINT( BOOLEAN(val) & BOOLEAN(v1) );
      |pc.cor : val:=LONGINT( BOOLEAN(val) OR BOOLEAN(v1) );
      |pc.rol : inter.abort;
      |pc.ror : inter.abort;
      |pc.ash : val:=val<<v1;
    END;
  END constop;

  PROCEDURE convert(g,f: pc.Form; VAR val: LONGINT; VAR ext: exp.EXT);
    CONST min_real = VAL(LONGREAL,MIN(REAL));
          max_real = VAL(LONGREAL,MAX(REAL));
          min_ir   = VAL(REAL,MIN(LONGINT));
          max_ir   = VAL(REAL,MAX(LONGINT));
          min_il   = VAL(LONGREAL,MIN(LONGINT));
          max_il   = VAL(LONGREAL,MAX(LONGINT));
    VAR r: REAL; l: LONGREAL;
  BEGIN
    IF f=pc.real THEN
      IF g=pc.longreal THEN ext^.lrl:=VAL(LONGREAL,ext^.real)
      ELSIF g#pc.real THEN
        r:=ext^.real;
        IF (r<min_ir) OR (r>max_ir) THEN scan.err(201)
        ELSE val:=VAL(LONGINT,r);
        END;
      END;
    ELSIF f=pc.longreal THEN
      l:=ext^.lrl;
      IF g=pc.real THEN
        IF (l<min_real) OR (l>max_real) THEN scan.err(201)
        ELSE ext^.real:=VAL(REAL,ext^.lrl)
        END;
      ELSIF g#pc.longreal THEN
        IF (l<min_il) OR (l>max_il) THEN scan.err(201)
        ELSE val:=VAL(LONGINT,l);
        END;
      END;
    ELSIF g IN pc.REALs THEN
      NEW(ext);
      IF g=pc.real THEN ext^.real:=VAL(REAL,val)
      ELSE ext^.lrl:=VAL(LONGREAL,val);
      END;
    ELSIF f>g THEN scan.err(201); val:=1
    END;
  END convert;

  PROCEDURE cunary(n: pc.NODE; VAR val: LONGINT; VAR ext: ADDRESS);
    CONST only_type = {pc.min,pc.max,pc.bits,pc.bytes,pc.size,pc.high,pc.len};
    VAR l,h: LONGINT; type: pc.STRUCT; e: exp.EXT;
  BEGIN
    type:=n^.l^.type;
    IF (n^.sub<=pc.len) & (n^.sub IN only_type) THEN
      type_size(type,FALSE); ext:=NIL;
    ELSE
      eval(n^.l,val,ext)
    END;
    e:=ext;
    CASE n^.sub OF
      |pc.max   : IF type^.mode IN pc.REALs THEN
                    NEW(e);
                    IF type^.mode=pc.real THEN e^.real:=MAX(REAL)
                    ELSE e^.lrl:=MAX(LONGREAL)
                    END;
                  ELSE range(type,l,h); val:=h;
                  END;
      |pc.min   : IF type^.mode IN pc.REALs THEN
                    NEW(e);
                    IF type^.mode=pc.real THEN e^.real:=MIN(REAL)
                    ELSE e^.lrl:=MIN(LONGREAL)
                    END;
                  ELSE range(type,l,h); val:=l;
                  END;
      |pc.bits  : val:=type^.size*8;
      |pc.bytes : val:=type^.size;
      |pc.size  : val:=type^.size;
      |pc.high  : IF type^.mode=pc.vector THEN val:=type^.n-1
                  ELSE range(type^.inx,l,h); val:=h;
                  END;
      |pc.len   : IF type^.mode=pc.vector THEN val:=type^.n
                  ELSE range(type^.inx,l,h); val:=h-l+1;
                  END;
      |pc.conv  : convert(n^.type^.mode,type^.mode,val,e); ext:=e
      |pc.typetran:
                  IF type^.size#n^.type^.size THEN scan.err(34) END;
      |pc.abs:
                  IF type^.mode=pc.real THEN e^.real:=ABS(e^.real);
                  ELSIF type^.mode=pc.longreal THEN e^.lrl:=ABS(e^.lrl);
                  ELSE val:=ABS(val);
                  END;
      |pc.minus:
                  IF type^.mode=pc.real THEN e^.real:=-e^.real;
                  ELSIF type^.mode=pc.longreal THEN e^.lrl:=-e^.lrl;
                  ELSE val:=-val;
                  END;
      |pc.cap   : val:=LONGINT( SET32(val)-SET32{5} );
      |pc.odd   : val:=LONGINT( ODD(val) );
      |pc.not   : val:=LONGINT(val=0);
      |pc.rcheck: range(n^.type,l,h);
                  IF (val<l) OR (val>h) THEN scan.err(122) END;
    END;
  END cunary;

  PROCEDURE set(agg: pc.NODE);
    VAR x: SET32; a,b,l,h: LONGINT; n,rem: pc.NODE;
  BEGIN
    x:=SET32{};
    IF agg^.type^.mode=pc.bitset THEN l:=0; h:=15
    ELSE range(agg^.type^.base,l,h);
    END;
    n:=agg^.l;
    WHILE n#NIL DO
      IF n^.mode=pc.node THEN
        a:=n^.l^.val; b:=n^.r^.val;
        DISPOSE(n^.l); DISPOSE(n^.r);
      ELSE a:=n^.val; b:=a;
      END;
      IF (a<l) OR (b>h) OR (a>b) THEN scan.err(122)
      ELSE x:=x+SET32{CARDINAL(a)..CARDINAL(b)};
      END;
      rem:=n; n:=n^.next; DISPOSE(rem);
    END;
    val:=LONGINT(x); ext:=NIL;
  END set;

  PROCEDURE array(agg: pc.NODE);
    VAR
      i: CARDINAL; n: pc.NODE; e: exp.EXT;
      p: POINTER TO ARRAY [0..03FFEH] OF LONGINT;
  BEGIN
    new_buf(e,agg^.type^.size);
    n:=agg^.l; i:=0; p:=e^.buf;
    WHILE n#NIL DO p^[i]:=n^.val; INC(i); n:=n^.next END;
    val:=-1; ext:=e;
  END array;

  PROCEDURE string(agg: pc.NODE);
    VAR
      i: CARDINAL; n: pc.NODE; e: exp.EXT;
      p: POINTER TO ARRAY [0..0FFFH] OF BYTE;
  BEGIN
    new_buf(e,agg^.type^.size);
    n:=agg^.l; i:=0; p:=e^.buf;
    WHILE n#NIL DO p^[i]:=BYTE(n^.val); INC(i); n:=n^.next END;
    WHILE i<CARDINAL(e^.n) DO p^[i]:=0C; INC(i) END;
    val:=-1; ext:=e;
  END string;

  PROCEDURE index(n: pc.NODE);
    VAR p,p1: PTR; e,e1: exp.EXT; i,j: INTEGER;
  BEGIN
    e1:=n^.l^.ext; p1:=e1^.buf;
    IF n^.type^.mode IN pc.Forms{pc.array..pc.record} THEN
      new_buf(e,n^.type^.size); p:=e^.buf;
      ext:=e; val:=-1;
    ELSE val:=0; p:=ADR(val); ext:=NIL;
    END;
    j:=INTEGER( n^.r^.val*n^.type^.size );
    FOR i:=0 TO INTEGER(n^.type^.size-1) DO p^[i]:=p1^[j+i] END;
    IF    n^.type^.mode=pc.shortint THEN val:=LONGINT(SHORTCARD(val))
    ELSIF n^.type^.mode=pc.integer  THEN val:=LONGINT(CARDINAL(val))
    END;
  END index;

  PROCEDURE codeproc(n: pc.NODE);
    VAR l: pc.NODE; i: INTEGER; e: exp.EXT; p: PTR;
  BEGIN
    l:=n^.l; i:=0;
    WHILE l#NIL DO
      IF l^.type#obs.shortint THEN err_ps(l^.pos,30) END;
      INC(i); l:=l^.next
    END;
    new_buf(e,LONGINT(i)); p:=e^.buf;
    l:=n^.l; i:=0;
    WHILE l#NIL DO p^[i]:=CHR(l^.val); INC(i); l:=l^.next END;
    val:=LONGINT(i); ext:=e;
  END codeproc;

  PROCEDURE concat(n: pc.NODE);
    VAR i,j: INTEGER; v1,v2: LONGINT; e1,e2,e: exp.EXT; t1,t2: pc.STRUCT;
      p,p1,p2: PTR;
  BEGIN
    t1:=n^.l^.type;
    t2:=n^.r^.type;
    eval(n^.l,v1,ext); e1:=ext; p1:=e1^.buf;
    eval(n^.r,v2,ext); e2:=ext; p2:=e2^.buf;
    new_buf(e,n^.type^.n); p:=e^.buf;
    IF t1^.mode=pc.vector THEN
      FOR i:=0 TO INTEGER(t1^.n-2) DO p^[i]:=p1^[i] END;
      i:=INTEGER(t1^.n-1);
    ELSE p^[0]:=CHR(v1); i:=1;
    END;
    IF t2^.mode=pc.vector THEN
      FOR j:=0 TO INTEGER(t2^.n-2) DO p^[i]:=p2^[j]; INC(i) END;
    ELSE p^[i]:=CHR(v2); INC(i);
    END;
    p^[i]:=0C;
    ext:=e; val:=-1;
  END concat;

  VAR mode: pc.Form;
BEGIN
  type_size(n^.type,FALSE);
  CASE n^.mode OF
    |pc.binary   : IF n^.sub=pc.concat THEN concat(n);
                   ELSE
                     eval(n^.l,val,ext);
                     constop(n,val,exp.EXT(ext));
                   END;
                   DISPOSE(n^.l); DISPOSE(n^.r);
    |pc.unary    : cunary(n,val,ext);
                   DISPOSE(n^.l);
    |pc.value    : val:=n^.val; ext:=n^.ext;
    |pc.cons     : val:=n^.obj^.adr; ext:=n^.obj^.ext;
    |pc.index    : index(n);
                   DISPOSE(n^.l); DISPOSE(n^.r);
    |pc.field    : inter.abort;
    |pc.aggregate: mode:=n^.type^.mode;
                   IF    mode IN pc.SETs THEN set(n);
                   ELSIF mode IN pc.Forms{pc.array,pc.vector} THEN
                     IF n^.type^.base^.mode=pc.char THEN string(n)
                     ELSE array(n)
                     END;
                   ELSIF mode=pc.proctype THEN codeproc(n)
                   ELSE inter.abort;
                   END;
  END;
END eval;

PROCEDURE literal(n: pc.NODE);

  PROCEDURE string(n: pc.NODE);
    VAR e: exp.EXT; i: INTEGER;
        p: POINTER TO ARRAY [0..0FFFEH] OF CHAR;
  BEGIN
    new_buf(e,LONGINT(scan.len));
    p:=e^.buf;
    FOR i:=0 TO scan.len-1 DO p^[i]:=scan.string[i] END;
    n^.val:=-1; n^.ext:=e;
  END string;

  VAR e: exp.EXT;
BEGIN
  type_size(n^.type,FALSE);
  CASE scan.lit OF
    |scan.intval : n^.val:=scan.int;
    |scan.charval: n^.val:=scan.int;
    |scan.realval: NEW(e); n^.ext:=e; e^.real:=scan.real;
    |scan.lrlval : inter.abort; NEW(e); n^.ext:=e; (*e^.lrl:=scan.double;*)
    |scan.strval : string(n);
  ELSE inter.abort;
  END;
END literal;

(*----------------------------------------------------------------*)

PROCEDURE ini; END ini;

PROCEDURE compile(tree: pc.NODE; nil,proc,type: BOOLEAN);
BEGIN
  sta.compile(tree,NOT obs.imp,proc_no);
END compile;

END inGen.
