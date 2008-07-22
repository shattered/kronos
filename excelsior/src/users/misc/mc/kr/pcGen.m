IMPLEMENTATION MODULE pcGen; (*$N+ Sem 06-Oct-90. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR, ADDRESS;

IMPORT bio : BIO;
IMPORT tty : Terminal;
IMPORT mcd : defCodes;
IMPORT comp: coolDefs;
IMPORT gen : krSym;

IMPORT tbl : pcTab;
IMPORT mem : pcSystem;
IMPORT exp : pcGenExpr;
IMPORT put : pcGenCmd;
IMPORT des : pcGenDesig;

FROM pcTab      IMPORT  ref;

WITH STORAGE : mem;

-- импорту подлежат: (см. pcTab.d)
--    объекты, обладающие значением
--    конструкторы типов
--    module

VAR
  -- code = info <строковый пул> <код> <мультиглобалы> <внешние>
  -- <мультиглобалы> = { <offset> <size> }
  info  :  RECORD
    version  : INTEGER;   -- версия кодофайла
    def_time : INTEGER;   -- время компиляции definition
    imp_time : INTEGER;   -- время компиляции implementation
    str_size : INTEGER;   -- размер строкового пула
    code_size: INTEGER;   -- размер кода
    min_stack: INTEGER;   -- мин. стек для глоб. мультизначений
    add_stack: INTEGER;   -- дополнительный стек
    glo_size : INTEGER;   -- число глобалов ( + GW0,GW1)
    no_exts  : INTEGER;   -- число внешних  ( + self)
    no_proc  : INTEGER;   -- число процедур
    no_mg    : INTEGER;   -- число мультиглобалов
    reserv_0B: INTEGER;
    compiler : ARRAY [0..3] OF CHAR; -- имя компилятора
    mod_tag  : INTEGER;
    reserv_0E: INTEGER;
    reserv_0F: INTEGER;
  END;

  flow      : exp.node;
  last_flow : POINTER TO exp.node;

  code_len  : INTEGER;

  proc_table: ARRAY [0..255] OF INTEGER;

PROCEDURE move(a,b: ADDRESS; s: INTEGER); CODE mcd.move END move;
PROCEDURE bblt(a: ADDRESS; n: INTEGER;
               b: ADDRESS; m,l: INTEGER); CODE mcd.bblt END bblt;

PROCEDURE copy(VAR f: exp.node);
  VAR l: exp.node;
BEGIN
  IF (f#NIL) & f^.mark & (f^.len<=8) & (f^.cnt<2) THEN
    INC(f^.cnt); l:=exp.new(); l^:=f^; f:=l;
    f^.mark:=FALSE; f^.next:=NIL;
  END;
END copy;

PROCEDURE move_jump(VAR f: exp.node);
  VAR l: exp.node;
BEGIN
  l:=f;
  LOOP
    IF (f=NIL) OR (f^.len>0) OR (f^.md#exp.nm_goto) THEN RETURN END;
    f:=f^.true;
    IF l=f THEN RETURN END;
  END;
END move_jump;

PROCEDURE tie(f: exp.node);
  VAR l: exp.node; i: INTEGER;
BEGIN
  IF (f=NIL) OR f^.mark THEN RETURN END;
  f^.adr:=code_len; INC(code_len,f^.len+f^.res);
  last_flow^:=f; last_flow:=ADR(f^.next); f^.mark:=TRUE;
  CASE f^.md OF
    |exp.nm_goto:
      move_jump(f^.true); copy(f^.true); tie(f^.true);
    |exp.nm_case:
      FOR i:=0 TO HIGH(f^.alts) DO move_jump(f^.alts[i]); tie(f^.alts[i]) END;
      move_jump(f^.false); tie(f^.false);
    |exp.nm_cond:
      move_jump(f^.false); move_jump(f^.true);
      IF (f^.false#NIL) & NOT f^.false^.mark THEN
        tie(f^.false); tie(f^.true);
      ELSIF (f^.true#NIL) & NOT f^.true^.mark THEN
        tie(f^.true); tie(f^.false);
      ELSE
        copy(f^.false);
        IF (f^.false#NIL) & NOT f^.false^.mark THEN
          tie(f^.true); tie(f^.false);
        ELSE
          copy(f^.true); tie(f^.false); tie(f^.true);
        END;
      END;
  END;
END tie;

PROCEDURE gen_jp(f: exp.node);
  VAR fr,pos: INTEGER;
  PROCEDURE b(n: INTEGER);
  BEGIN
    f^.buf[pos]:=CHAR(n); INC(fr); INC(pos);
  END b;
  VAR to: INTEGER; db,cond: BOOLEAN;
BEGIN
  IF f^.md=exp.nm_case THEN f^.res:=0; RETURN END;
  pos:=0; db:=FALSE;
  IF f^.md=exp.nm_cond THEN
    IF f^.false#f^.true THEN
      ASSERT(f^.false#NIL); ASSERT(f^.true#NIL);
      IF f^.false=f^.next THEN
        to:=f^.true^.adr;
        IF f^.cmp>=f^.pos+f^.len-1 THEN
          f^.len:=f^.cmp-f^.pos;
          CASE INTEGER(put.code[f^.cmp]) OF
            |mcd.equ: b(mcd.neq);
            |mcd.neq: b(mcd.equ);
            |mcd.lss: b(mcd.geq);
            |mcd.leq: b(mcd.gtr);
            |mcd.gtr: b(mcd.leq);
            |mcd.geq: b(mcd.lss);
            |mcd.not: -- nop
          END;
        ELSE b(mcd.not);
        END;
      ELSE
        db:=f^.true#f^.next; to:=f^.false^.adr;
      END;
      cond:=TRUE;
    ELSIF (f^.true=NIL) OR (f^.true=f^.next) THEN
      b(mcd.drop); f^.res:=1; RETURN
    ELSE to:=f^.true^.adr; b(mcd.drop); cond:=FALSE;
    END;
  ELSIF (f^.true=NIL) OR (f^.true=f^.next) THEN f^.res:=0; RETURN
  ELSE to:=f^.true^.adr; cond:=FALSE;
  END;
  fr:=f^.adr+f^.len+pos;
  IF ABS(fr+2-to)<100h THEN
    to:=fr+2-to;
    IF to>0 THEN
      IF cond THEN b(mcd.jbsc) ELSE b(mcd.jbs) END; b(to);
    ELSE
      IF cond THEN b(mcd.jfsc) ELSE b(mcd.jfs) END; b(-to);
    END;
  ELSIF ABS(fr+3-to)<10000h THEN
    to:=fr+3-to;
    IF to>0 THEN
      IF cond THEN b(mcd.jblc) ELSE b(mcd.jbl) END; b(to); b(to>>8);
    ELSE
      IF cond THEN b(mcd.jflc) ELSE b(mcd.jfl) END; b(-to); b((-to)>>8);
    END;
  ELSE
    IF cond THEN b(mcd.not); b(mcd.jfsc); b(6) END;
    b(mcd.liw); b(to); b(to>>8); b(to>>16); b(to>>24); b(mcd.jump);
  END;
  IF db THEN
    fr:=f^.adr+f^.len+pos; to:=f^.true^.adr;
    IF to=fr THEN
    ELSIF ABS(fr+2-to)<100h THEN
      to:=fr+2-to;
      IF to>0 THEN b(mcd.jbs); b(to) ELSE b(mcd.jfs); b(-to) END;
    ELSIF ABS(fr+3-to)<10000h THEN
      to:=fr+3-to;
      IF to>0 THEN b(mcd.jbl); b(to); b(to>>8);
      ELSE b(mcd.jfl); b(-to); b((-to)>>8);
      END;
    ELSE
      b(mcd.liw); b(to); b(to>>8); b(to>>16); b(to>>24); b(mcd.jump);
    END;
  END;
  f^.res:=pos;
END gen_jp;

PROCEDURE mark_flow(f: exp.node);
  VAR fail_no: INTEGER; i,k: exp.node; j,n,m: INTEGER;
BEGIN
  REPEAT
    fail_no:=0; i:=f;
    WHILE i#NIL DO
      IF i^.next#NIL THEN
        ASSERT(i^.next^.adr=i^.adr+i^.len+i^.res);
        gen_jp(i);
        j:=i^.adr+i^.len+i^.res-i^.next^.adr;
        IF j#0 THEN
          k:=i^.next;
          WHILE k#NIL DO INC(k^.adr,j); k:=k^.next END;
          INC(fail_no); INC(code_len,j);
        END;
      ELSE
        j:=i^.len+i^.res; gen_jp(i); INC(code_len,i^.len+i^.res-j);
      END;
      i:=i^.next;
    END;
  UNTIL fail_no=0;
  WHILE f#NIL DO
    IF f^.md=exp.nm_case THEN
      ASSERT(f^.false#NIL);
      FOR n:=f^.spos TO f^.spos+f^.slen-1 DO
        IF des.scode[n]<0 THEN
          des.scode[n]:=f^.false^.adr;
        ELSE
          ASSERT(des.scode[n]<=HIGH(f^.alts));
          des.scode[n]:=f^.alts[des.scode[n]]^.adr;
        END;
      END;
    END;
    f:=f^.next;
  END;
END mark_flow;

PROCEDURE put_code(l,m: ref; VAL s: ARRAY OF CHAR);
--
-- m - модуль - хозяин объекта l
-- если l тоже модуль то m - модуль импортирующий l
--
  PROCEDURE import_val(VAR a: gen.access);
  BEGIN
    WITH a DO
      CASE am OF
        |gen.am_G   : lvl:=m^.adr;
        |gen.am_aG  : lvl:=m^.adr;
        |gen.am_STR : lvl:=m^.adr;
      ELSE
      END;
    END;
  END import_val;

  VAR a,b: ADDRESS; i: INTEGER;
BEGIN
  IF l=NIL THEN
--tty.print('import my def\n');
    -- чтение результатов генерации для единицы компиляции
    ASSERT(des.mdls_no>0); ASSERT(SIZE(s)>=SIZE(gen.access_cu));
    move(ADR(des.cu),ADR(s),SIZE(des.cu));
    a:=ADR(s)+SIZE(gen.access_cu);
    i:=des.new_str(des.cu.str_sz);
    b:=ADR(des.scode[i]);
    move(b,a,des.cu.str_sz);
    INC(a,des.cu.str_sz);
    b:=ADR(put.code);
    ASSERT(put.new_code(des.cu.ini_sz)=0);
    move(b,a,(des.cu.ini_sz+3) DIV 4);
    INC(a,(des.cu.ini_sz+3) DIV 4);
    i:=des.new_mg(des.cu.mg_sz);
    b:=ADR(des.mg[i]);
    move(b,a,des.cu.mg_sz*SIZE(des.mg[0]));
    RETURN
  END;
  IF m=NIL THEN exp.error(NIL,'module=NIL') END;
  CASE l^.md OF
    |tbl.var,tbl.const:
--tty.print('import var or const\n');
      ASSERT(SIZE(s)=SIZE(gen.access));
      l^.adr:=des.new_var();
      move(ADR(des.vars[l^.adr]),ADR(s),SIZE(gen.access));
      import_val(des.vars[l^.adr]);
    |tbl.range:
--tty.print('import range\n');
      l^.adr:=des.new_rng();
      move(ADR(des.rngs[l^.adr]),ADR(s),SIZE(s));
      import_val(des.rngs[l^.adr].l);
      import_val(des.rngs[l^.adr].r);
    |tbl.procedure:
--tty.print('import procedure\n');
      ASSERT(SIZE(s)=SIZE(gen.access_proc));
      l^.adr:=des.new_proc();
      move(ADR(des.prcs[l^.adr]),ADR(s),SIZE(s));
      des.prcs[l^.adr].mod:=m^.adr;
    |tbl.module:
--tty.print('import module\n');
      IF l^.nm=m^.nm THEN -- импорт самого себя
        IF des.mdls_no=0 THEN l^.adr:=des.new_mod() END;
        l^.adr:=0;
      ELSE -- импорт библиотечного модуля
        FOR i:=1 TO des.mdls_no-1 DO
          IF des.mdls_nm[i]=l^.nm THEN l^.adr:=i; RETURN END;
        END;
        l^.adr:=des.new_mod();
        IF l^.adr=0 THEN l^.adr:=des.new_mod() END;
      END;
      move(ADR(des.mdls[l^.adr]),ADR(s),SIZE(gen.access_import));
      des.mdls_nm[l^.adr]:=l^.nm;
      des.mdls[l^.adr].offset:=l^.adr;
--      ASSERT(l^.adr=des.mdls[l^.adr].offset);
  ELSE exp.error(l,'import object, illegal mode');
  END;
END put_code;

PROCEDURE get_code(l: ref; VAR s: STRING);
  VAR a,b: ADDRESS; i,ini,an: INTEGER;
BEGIN
  IF l=NIL THEN
    WITH des.cu DO
      str_sz:=des.scnt; ini:=ini_sz; ini_sz:=code_len; mg_sz:=des.mg_no;
      i:=str_sz+(ini_sz+3) DIV 4+mg_sz*SIZE(des.mg[0])+SIZE(gen.access_cu);
    END;
    IF exp.LAST THEN
      s.ADR :=ADR(des.cu);
      s.HIGH:=BYTES(gen.access_cu)-1;
    ELSE
      NEW(s,i*4);
      move(ADR(s),ADR(des.cu),SIZE(gen.access_cu));
      a:=ADR(s)+SIZE(gen.access_cu);
      move(a,ADR(des.scode),des.cu.str_sz);
      INC(a,des.cu.str_sz);
      an:=0;
      bblt(a,an*8,ADR(put.code),0,ini*8);
      INC(an,ini);
      WHILE flow#NIL DO
        bblt(a,an*8,ADR(put.code),flow^.pos*8,flow^.len*8);
        INC(an,flow^.len);
        bblt(a,an*8,ADR(flow^.buf),0,flow^.res*8);
        INC(an,flow^.res);
        flow:=flow^.next;
      END;
      ASSERT(an=des.cu.ini_sz);
      INC(a,(des.cu.ini_sz+3) DIV 4);
      move(a,ADR(des.mg),des.cu.mg_sz*SIZE(des.mg[0]));
    END;
    RETURN
  END;
  CASE l^.md OF
    |tbl.var,tbl.const:
      s.ADR :=ADR(des.vars[l^.adr]);
      s.HIGH:=BYTES(gen.access)-1;
    |tbl.range:
      s.ADR :=ADR(des.rngs[l^.adr]);
      s.HIGH:=BYTES(gen.access_range)-1;
    |tbl.procedure:
      s.ADR :=ADR(des.prcs[l^.adr]);
      s.HIGH:=BYTES(gen.access_proc)-1;
    |tbl.module:
      des.mdls[l^.adr].offset:=l^.adr;
      s.ADR :=ADR(des.mdls[l^.adr]);
      s.HIGH:=BYTES(gen.access_import)-1;
  ELSE exp.error(l,'import object, illegal mode');
  END;
END get_code;

PROCEDURE make_info(def,imp: INTEGER; VAR size: INTEGER);
BEGIN
-- code = info <строковый пул> <код> <мультиглобалы> <внешние>
-- <мультиглобалы> = { <offset> <size> }
  info.version  :=102h;
  info.def_time :=def;
  info.imp_time :=imp;
  info.str_size :=des.scnt;
  info.code_size:=(code_len+3) DIV 4;
  info.min_stack:=0;
  info.add_stack:=0;
  info.glo_size :=des.cu.glo_sz;
  info.no_exts  :=des.mdls_no;
  info.no_proc  :=des.cu.prc_sz;
  info.no_mg    :=des.mg_no;
  info.reserv_0B:=0;
  info.compiler :='mx';
  info.mod_tag  :=0;
  info.reserv_0E:=0;
  info.reserv_0F:=0;
  INC(info.glo_size,info.no_exts);
  size:=info.code_size+info.str_size;
END make_info;

PROCEDURE write_code_file;
  PROCEDURE io_chk;
  BEGIN
    IF bio.done THEN RETURN END;
    tbl.error(0,0,'I/O error, %s',bio.ename); HALT(1);
  END io_chk;
  VAR
    nm  : ARRAY [0..255] OF CHAR;
    ext : ARRAY [0..255] OF CHAR;
    out : bio.FILE;
    i,e : INTEGER;
BEGIN
  tbl.get_name(des.mdls_nm[0],nm);
  i:=0;
  WHILE (i<HIGH(nm)-4) & (nm[i]#0c) DO INC(i) END;
  nm[i]:='.'; INC(i); nm[i]:='c'; INC(i);
  nm[i]:='o'; INC(i); nm[i]:='d'; INC(i);
  REPEAT nm[i]:=0c; INC(i) UNTIL i>HIGH(nm);
  bio.create(out,nm,'hw',(SIZE(info)+info.code_size+info.str_size)*4); io_chk;
  bio.buffers(out,1,4096); io_chk;
  bio.write(out,ADR(info),SIZE(info)*4); io_chk;
  bio.write(out,ADR(des.scode[0]),info.str_size*4); io_chk;
  bio.write(out,ADR(proc_table),info.no_proc*4); io_chk;
  bio.write(out,ADR(put.code),des.cu.ini_sz); io_chk;
  WHILE flow#NIL DO
    ASSERT(flow^.len>=0); ASSERT(flow^.res>=0);
    bio.fwrite(out,ADR(put.code),flow^.pos,flow^.len); io_chk;
    bio.fwrite(out,ADR(flow^.buf),0,flow^.res); io_chk;
    flow:=flow^.next;
  END;
  IF bio.pos(out) MOD 4 # 0 THEN
    i:=-1; bio.fwrite(out,ADR(i),0,4-(bio.pos(out) MOD 4)); io_chk;
  END;
  bio.write(out,ADR(des.mg[0]),info.no_mg*SIZE(des.mg[0])*4); io_chk;
  FOR e:=des.mdls_no-1 TO 1 BY -1 DO
des.mdls[e].def_time:=-1;
    bio.write(out,ADR(des.mdls[e].def_time),4); io_chk;
    tbl.get_name(des.mdls_nm[e],ext); i:=0;
    WHILE (i<HIGH(ext)) & (ext[i]#0c) DO INC(i) END;
    REPEAT ext[i]:=0c; INC(i) UNTIL i MOD 4 =0;
    bio.write(out,ADR(ext),i); io_chk;
  END;
  bio.close(out); io_chk;
END write_code_file;

PROCEDURE mark_flow_graf(f: exp.node);
  VAR fn: exp.node;
BEGIN
  flow:=NIL; last_flow:=ADR(flow);
  code_len:=des.cu.ini_sz+des.cu.prc_sz*4;
  exp.proc:=exp.procs;
  WHILE f#NIL DO
    IF exp.proc^.l^.md=tbl.module THEN
      proc_table[0]:=des.cu.prc_sz*4;
    ELSE
      proc_table[des.prcs[exp.proc^.l^.adr].disp]:=code_len;
    END;
    exp.proc:=exp.proc^.r;
    fn:=f^.next; f^.next:=NIL; tie(f); mark_flow(f); f:=fn;
  END;
END mark_flow_graf;

PROCEDURE enter_host_module(m: ref; def: INTEGER);
  VAR name: ARRAY [0..255] OF CHAR; i,j: INTEGER;
BEGIN
  ASSERT((m#NIL) & (m^.md=tbl.module));
  IF des.mdls_no=0 THEN m^.adr:=des.new_mod() ELSE m^.adr:=0 END;
  IF des.mdls_nm[0]<0 THEN
    des.mdls_nm[0]:=m^.nm;
    des.mdls[0].def_time:=def;
    des.cu.prc_sz:=1;
    des.cu.ini_sz:=0;
    des.cu.glo_sz:=2;
    des.cu.str_sz:=0;
    des.cu.mg_sz :=0;
    tbl.get_name(m^.nm,name);
    i:=0;
    WHILE (i<HIGH(name)) & (name[i]#0c) DO INC(i) END;
    REPEAT name[i]:=0c; INC(i) UNTIL i MOD 4 = 0;
    j:=des.new_str(i DIV 4);
    move(ADR(des.scode[j]),ADR(name),i DIV 4);
  ELSIF des.mdls_nm[0]#m^.nm THEN exp.error(m,'illegal name');
  END;
  exp.proc:=des.new_ref(tbl.proc_body);
  exp.proc^.dw:=m^.dw;
  exp.proc^.l:=m;
  exp.last_proc:=exp.proc;
  exp.procs:=exp.proc;
END enter_host_module;

PROCEDURE gen_code(m: ref; def,imp: INTEGER; VAR size: INTEGER);
  VAR f: exp.node; i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(proc_table) DO proc_table[i]:=0 END;
  exp.LAST:=TRUE;
  enter_host_module(m,def);
  f:=NIL; last_flow:=ADR(f);
  WHILE exp.proc#NIL DO
    last_flow^:=exp.do_flow();
    last_flow:=ADR(last_flow^^.next);
    exp.proc:=exp.proc^.r;
  END;
  mark_flow_graf(f);
  make_info(def,imp,size);
  write_code_file;
END gen_code;

PROCEDURE gen_def(m: ref; def: INTEGER);
  VAR f: exp.node;
BEGIN
  enter_host_module(m,def);
  f:=exp.do_flow();
  IF exp.proc^.r#NIL THEN
    exp.error(exp.proc^.r,'proc_body in definition - unrealized');
  END;
  flow:=NIL; last_flow:=ADR(flow);
  code_len:=des.cu.ini_sz;
  tie(f); mark_flow(f);
END gen_def;

PROCEDURE ini;
BEGIN
  des.vars_no:=0; NEW(des.vars,20);
  des.prcs_no:=0; NEW(des.prcs,20);
  des.mdls_no:=0; NEW(des.mdls,10); NEW(des.mdls_nm,10);
  des.rngs_no:=0; NEW(des.rngs,20);
  des.scnt   :=0; NEW(des.scode,100);
  des.mg_no  :=0; NEW(des.mg,10);
  des.inln_no:=0; NEW(des.inln,10);
  put.cnt    :=0; NEW(put.code,1000);
  exp.LAST:=FALSE;
  tbl.gen_code:=gen_code;
  tbl.gen_def :=gen_def;
  tbl.get_code:=get_code;
  tbl.put_code:=put_code;
END ini;

BEGIN
  tbl.ini_gen :=ini;
END pcGen.
