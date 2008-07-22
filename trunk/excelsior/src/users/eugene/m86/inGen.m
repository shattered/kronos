IMPLEMENTATION MODULE inGen; (*$U+ Sem 06-Oct-90. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR, ADDRESS, WORD;

IMPORT tbl : pcTab;
IMPORT mem : pcSystem;
IMPORT bio : BIO;
IMPORT tty : Terminal;
IMPORT mcd : lowLevel;

IMPORT  inEmul;
IMPORT  inNum;

IMPORT sym : inSym;
IMPORT put : inCmd;
IMPORT vrs : inVars;
IMPORT cns : inConst;
IMPORT flw : inFlow;
IMPORT sta : inStat;


FROM pcTab      IMPORT  ref;

WITH STORAGE : mem;

--    code :
--                       bytes
-- info                  64
-- <глобалы>             info.glob_size
-- <код>                 info.code_size
-- <мультиглобалы>       info.mglo_no * 8
-- <таблица процедур>    info.proc_no * 4
-- <таблица вн.процедур> info.link_no * 2
-- <таблица модулей>

VAR
  info  :  RECORD
    version  : INTEGER;   -- версия кодофайла
    def_time : INTEGER;   -- время компиляции definition
    imp_time : INTEGER;   -- время компиляции implementation
    reserv_03: INTEGER;
    ------------------
    glob_size: INTEGER;   -- размер глобалов
    code_size: INTEGER;   -- размер кода
    mglo_no  : INTEGER;   -- количество мультиглобалов
    exts_no  : INTEGER;   -- количество внешних модулей
    ------------------
    min_stack: INTEGER;   -- мин. стек для глоб. мультизначений
    add_stack: INTEGER;   -- дополнительный стек
    link_no  : INTEGER;
    proc_no  : INTEGER;
    ------------------
    reserv_0C: INTEGER;
    reserv_0D: INTEGER;
    reserv_0E: INTEGER;
    reserv_0F: INTEGER;
  END;

  flow      : flw.node;
  last_flow : POINTER TO flw.node;
  code_len  : INTEGER;
  LAST      : BOOLEAN;
  acs_imp   : sym.access_import;

PROCEDURE error(s: ARRAY OF CHAR; SEQ x: WORD);
BEGIN
  tbl.error(0,0,s,x); HALT(50h);
END error;

PROCEDURE copy(VAR f: flw.node);
  VAR l: flw.node;
BEGIN
  IF (f#NIL) & f^.mark & (f^.len<=8) & (f^.cnt<2) THEN
    INC(f^.cnt); l:=flw.new(); l^:=f^; f:=l;
    f^.mark:=FALSE; f^.next:=NIL;
  END;
END copy;

PROCEDURE move_jump(VAR f: flw.node);
  VAR i: INTEGER;
BEGIN
  i:=10;
  REPEAT
    IF (f=NIL) OR (f^.len>0) OR (f^.md#flw.nm_goto) THEN RETURN END;
    f:=f^.true; DEC(i);
  UNTIL i=0;
END move_jump;

PROCEDURE tie(f: flw.node);
  VAR l: flw.node; i: INTEGER;
BEGIN
  IF (f=NIL) OR f^.mark THEN RETURN END;
  f^.adr:=code_len; INC(code_len,f^.len+f^.tlen);
  last_flow^:=f; last_flow:=ADR(f^.next); f^.mark:=TRUE;
  CASE f^.md OF
    |flw.nm_call,flw.nm_goto:
      move_jump(f^.true); copy(f^.true); tie(f^.true);
    |flw.nm_case:
      move_jump(f^.false); tie(f^.false);
      FOR i:=0 TO HIGH(f^.alts) DO move_jump(f^.alts[i]); tie(f^.alts[i]) END;
    |flw.nm_cond:
      move_jump(f^.false); move_jump(f^.true);
      IF (f^.false#NIL) & NOT f^.false^.mark THEN
        tie(f^.false); tie(f^.true);
      ELSIF (f^.true#NIL) & NOT f^.true^.mark THEN
        tie(f^.true); tie(f^.false);
      ELSE
        copy(f^.false);
        IF (f^.false#NIL) & NOT f^.false^.mark THEN
          tie(f^.false); tie(f^.true);
        ELSE
          copy(f^.true); tie(f^.true); tie(f^.false);
        END;
      END;
  END;
END tie;

PROCEDURE mark_flow(f: flw.node);
  VAR fail_no: INTEGER; i,k: flw.node; j,c,s: INTEGER;
BEGIN
  c:=put.cnt; s:=vrs.scnt;
  REPEAT
    fail_no:=0; i:=f; put.cnt:=c; vrs.scnt:=s;
    WHILE i#NIL DO
      IF i^.next#NIL THEN ASSERT(i^.next^.adr=i^.adr+i^.len+i^.tlen) END;
      j:=flw.gen_jp(i)-i^.tlen;
      IF j#0 THEN
        k:=i^.next;
        WHILE k#NIL DO INC(k^.adr,j); k:=k^.next END;
        INC(fail_no); INC(code_len,j); INC(i^.tlen,j);
      END;
      i:=i^.next;
    END;
  UNTIL fail_no=0;
END mark_flow;

PROCEDURE put_code(l,m: ref; VAL s: ARRAY OF CHAR);
--
-- m - модуль - хозяин объекта l
-- если l тоже модуль то m - модуль импортирующий l
--
  PROCEDURE import_val(VAR a: sym.access);
  BEGIN
    WITH a DO
      CASE am OF
        |sym.am_G   : level:=m^.adr;
        |sym.am_Gimm: level:=m^.adr;
        |sym.am_Gstr: level:=m^.adr;
        |sym.am_aG  : level:=m^.adr;
      ELSE
      END;
    END;
  END import_val;

  VAR a,b: ADDRESS; i,j: INTEGER;
BEGIN
  IF (l=NIL) & (m=NIL) THEN
    -- чтение результатов генерации для единицы компиляции
    ASSERT(SIZE(s)>=SIZE(sym.access_cu));
    mcd.move(ADR(vrs.cu),ADR(s),SIZE(vrs.cu));
    a:=ADR(s)+SIZE(sym.access_cu);
    ASSERT(vrs.new_str(vrs.cu.glo_sz)=0);
    mcd.cmove(ADR(vrs.scode),0,a,0,vrs.cu.glo_sz);
    i:=vrs.cu.glo_sz;
    ASSERT(put.new_code(vrs.cu.ini_sz)=0);
    mcd.cmove(ADR(put.code),0,a,i,vrs.cu.ini_sz);
    INC(i,vrs.cu.ini_sz);
    ASSERT(vrs.new_mg(vrs.cu.mg_sz)=0);
    j:=BYTES(vrs.mg[0]);
    mcd.cmove(ADR(vrs.mg),0,a,i,vrs.cu.mg_sz*j);
    RETURN
  END;
  CASE l^.md OF
    |tbl.var,tbl.const:
      ASSERT(SIZE(s)=SIZE(sym.access));
      l^.adr:=vrs.new_var();
      mcd.move(ADR(vrs.vars[l^.adr]),ADR(s),SIZE(sym.access));
      IF m#NIL THEN import_val(vrs.vars[l^.adr]) END;
    |tbl.range,tbl.subtype,tbl.set,tbl.enumeration,tbl.array,tbl.packed_array:
      ASSERT(SIZE(s)=SIZE(sym.access_range));
      l^.adr:=vrs.new_rng();
      mcd.move(ADR(vrs.rngs[l^.adr]),ADR(s),SIZE(s));
      IF m#NIL THEN
        import_val(vrs.rngs[l^.adr].l);
        import_val(vrs.rngs[l^.adr].r);
      END;
    |tbl.procedure:
      ASSERT(SIZE(s)=SIZE(sym.access_proc));
      l^.adr:=vrs.new_proc();
      mcd.move(ADR(vrs.prcs[l^.adr]),ADR(s),SIZE(sym.access_proc));
      IF m#NIL THEN
        WITH vrs.prcs[l^.adr] DO
          mod:=m^.adr;
          const:=FALSE;
          ASSERT(export);
          ASSERT(mod#0);
        END;
      END;
    |tbl.module:
      ASSERT(SIZE(s)=SIZE(sym.access_import));
      IF m=NIL THEN
        mcd.move(ADR(acs_imp),ADR(s),SIZE(sym.access_import));
        IF acs_imp.no>=vrs.mdls_no THEN
          RESIZE(vrs.mdls,acs_imp.no+1);
          RESIZE(vrs.mdls_nm,acs_imp.no+1);
          vrs.mdls_no:=acs_imp.no+1;
        END;
        vrs.mdls[acs_imp.no]:=acs_imp;
        vrs.mdls_nm[acs_imp.no]:=l^.nm;
        l^.adr:=acs_imp.no;
--tty.print('read import %d\n',l^.adr);
      ELSE
        FOR i:=1 TO vrs.mdls_no-1 DO
          IF vrs.mdls_nm[i]=l^.nm THEN l^.adr:=i; RETURN END;
        END;
        l^.adr:=vrs.new_mod();
        IF l^.adr=0 THEN l^.adr:=vrs.new_mod() END;
        mcd.move(ADR(vrs.mdls[l^.adr]),ADR(s),SIZE(sym.access_import));
        vrs.mdls_nm[l^.adr]:=l^.nm;
        WITH vrs.mdls[l^.adr] DO
          ofs:=vrs.new_str(2);
          no:=l^.adr;
          vrs.scode[ofs+0]:=CHAR(l^.adr);
          vrs.scode[ofs+1]:=CHAR(l^.adr>>8);
        END;
--tty.print('cre import %d\n',l^.adr);
      END;
  ELSE error('import object, illegal mode');
  END;
END put_code;

PROCEDURE get_code(l: ref; VAR s: STRING);
  VAR a,b: ADDRESS; i,ini,an: INTEGER;
BEGIN
  IF l=NIL THEN
    WITH vrs.cu DO
      glo_sz:=vrs.scnt; ini:=ini_sz; ini_sz:=code_len; mg_sz:=vrs.mg_no;
      i:=glo_sz+ini_sz+mg_sz*BYTES(vrs.mg[0])+BYTES(sym.access_cu);
    END;
    IF LAST THEN
      s^.ADR :=ADR(vrs.cu);
      s^.HIGH:=BYTES(sym.access_cu)-1;
    ELSE
      NEW(s,i);
      mcd.move(ADR(s),ADR(vrs.cu),SIZE(sym.access_cu));
      a:=ADR(s)+SIZE(sym.access_cu);
      mcd.cmove(a,0,ADR(vrs.scode),0,vrs.cu.glo_sz);
      an:=vrs.cu.glo_sz;
      mcd.cmove(a,an,ADR(put.code),0,ini);
      INC(an,ini);
      WHILE flow#NIL DO
        mcd.cmove(a,an,ADR(put.code),flow^.pos,flow^.len);
        INC(an,flow^.len);
        mcd.cmove(a,an,ADR(put.code),flow^.tpos,flow^.tlen);
        INC(an,flow^.tlen);
        flow:=flow^.next;
      END;
      ASSERT(an=vrs.cu.glo_sz+vrs.cu.ini_sz);
      i:=BYTES(vrs.mg[0]);
      mcd.cmove(a,an,ADR(vrs.mg),0,vrs.cu.mg_sz*i);
    END;
    RETURN;
  END;
  CASE l^.md OF
    |tbl.var,tbl.const:
      s^.ADR :=ADR(vrs.vars[l^.adr]);
      s^.HIGH:=BYTES(sym.access)-1;
    |tbl.range,tbl.subtype,tbl.set,tbl.enumeration,tbl.array,tbl.packed_array:
      ASSERT(l^.adr>=0);
      ASSERT(l^.adr<vrs.rngs_no);
      s^.ADR :=ADR(vrs.rngs[l^.adr]);
      s^.HIGH:=BYTES(sym.access_range)-1;
    |tbl.procedure:
      IF NOT LAST THEN vrs.prcs[l^.adr].export:=TRUE END;
      s^.ADR :=ADR(vrs.prcs[l^.adr]);
      s^.HIGH:=BYTES(sym.access_proc)-1;
    |tbl.module:
      s^.ADR :=ADR(vrs.mdls[l^.adr]);
      s^.HIGH:=BYTES(sym.access_import)-1;
  ELSE error('export object, illegal mode %d',l^.md);
  END;
END get_code;

PROCEDURE make_info(def,imp: INTEGER; VAR size: INTEGER);
  VAR i: INTEGER;
BEGIN
  info.version  :=103h;
  info.def_time :=def;
  info.imp_time :=imp;
  info.reserv_03:=0;
  --
  info.glob_size:=vrs.scnt;
  info.code_size:=code_len;
  info.mglo_no  :=vrs.mg_no;
  info.exts_no  :=vrs.mdls_no-1;
  --
  info.min_stack:=0;
  info.add_stack:=0;
  info.link_no  :=0;
  FOR i:=0 TO vrs.prcs_no-1 DO
    IF vrs.prcs[i].const THEN INC(info.link_no) END;
  END;
  info.proc_no  :=vrs.cu.prc_sz;
  --
  info.reserv_0C:=0;
  info.reserv_0D:=0;
  info.reserv_0E:=0;
  info.reserv_0F:=0;
  size:=info.code_size;
END make_info;

PROCEDURE write_code_file;
  PROCEDURE io_chk;
  BEGIN
    IF bio.done THEN RETURN END;
    error('I/O error, %s',bio.ename);
  END io_chk;
  VAR
    nm  : ARRAY [0..255] OF CHAR;
    ext : ARRAY [0..255] OF CHAR;
    out : bio.FILE;
    i,e : INTEGER;
BEGIN
  tbl.get_name(vrs.mdls_nm[0],nm);
  i:=0;
  WHILE (i<HIGH(nm)-4) & (nm[i]#0c) DO INC(i) END;
  nm[i]:='.'; INC(i); nm[i]:='o'; INC(i);
  nm[i]:='b'; INC(i); nm[i]:='j'; INC(i);
  REPEAT nm[i]:=0c; INC(i) UNTIL i>HIGH(nm);
  bio.create(out,nm,'hw',BYTES(info)+info.code_size+info.glob_size); io_chk;
  bio.buffers(out,1,4096); io_chk;
  -------------------------------------------------------------------
  --    write info (64 bytes)
  --
  bio.write(out,ADR(info),BYTES(info)); io_chk;
  -------------------------------------------------------------------
  --    write globals (info.glob_size bytes)
  --
  bio.write(out,ADR(vrs.scode),info.glob_size); io_chk;
  -------------------------------------------------------------------
  --    write code (info.code_size bytes)
  --
  bio.write(out,ADR(put.code),vrs.cu.ini_sz); io_chk;
  WHILE flow#NIL DO
    ASSERT(flow^.len>=0); ASSERT(flow^.tlen>=0);
    bio.fwrite(out,ADR(put.code),flow^.pos,flow^.len); io_chk;
    bio.fwrite(out,ADR(put.code),flow^.tpos,flow^.tlen); io_chk;
    flow:=flow^.next;
  END;
  -------------------------------------------------------------------
  --    write multiglobals (info.mglo_no*8 bytes)
  --
  bio.write(out,ADR(vrs.mg),info.mglo_no*BYTES(vrs.mg[0])); io_chk;
  -------------------------------------------------------------------
  --    write procedure table (info.proc_no*4 bytes)
  --
  ASSERT(info.proc_no=LEN(flw.proc_flow));
  i:=2; bio.write(out,ADR(i),4); io_chk;
  FOR i:=1 TO info.proc_no-1 DO
    bio.write(out,ADR(flw.proc_flow[i]^.adr),4); io_chk;
  END;
  -------------------------------------------------------------------
  --    write external procedure table (info.link_no*2 bytes)
  --
  FOR i:=0 TO vrs.prcs_no-1 DO
    IF vrs.prcs[i].const THEN
      bio.write(out,ADR(vrs.prcs[i].ofs),2); io_chk;
    END;
  END;
  -------------------------------------------------------------------
  --    write external modules table
  --
  FOR e:=1 TO vrs.mdls_no-1 DO
    bio.write(out,ADR(vrs.mdls[e].def_time),4); io_chk;
    bio.write(out,ADR(vrs.mdls[e].ofs),4); io_chk;
    tbl.get_name(vrs.mdls_nm[e],ext); i:=0;
    WHILE (i<HIGH(ext)) & (ext[i]#0c) DO INC(i) END;
    ext[i]:=0c; INC(i);
    bio.write(out,ADR(ext),i); io_chk;
  END;
  bio.close(out); io_chk;
END write_code_file;

PROCEDURE enter_host_module(VAR m: ref; def: INTEGER);
  VAR name: ARRAY [0..255] OF CHAR; i,j: INTEGER; p: ref;
BEGIN
  ASSERT((m#NIL) & (m^.md=tbl.module));
  IF vrs.mdls_no=0 THEN m^.adr:=vrs.new_mod() ELSE m^.adr:=0 END;
  IF vrs.mdls_nm[0]<0 THEN
    vrs.mdls_nm[0]:=m^.nm;
    vrs.mdls[0].def_time:=def;
    vrs.cu.prc_sz:=1;
    vrs.cu.ini_sz:=2; put.w(0);
    vrs.cu.glo_sz:=0;
    vrs.cu.mg_sz :=0;
  ELSIF vrs.mdls_nm[0]#m^.nm THEN error('illegal name');
  END;
  NEW(p);
  p^.md:=tbl.proc_body;
  p^.dw:=m^.dw;
  p^.r:=NIL;
  p^.nxt:=NIL;
  p^.adr:=-1;
  p^.pos:=0;
  NEW(p^.l);
  p^.l^.md:=tbl.procedure;
  p^.l^.adr:=vrs.new_proc();
  p^.l^.pos:=0;
  p^.l^.nm:=m^.nm;
  p^.l^.l:=NIL;
  p^.l^.r:=NIL;
  p^.l^.dw:=NIL;
  p^.l^.nxt:=NIL;
  vrs.prcs[p^.l^.adr].export:=TRUE;
  m:=p;
END enter_host_module;

PROCEDURE gen_code(m: ref; def,imp: INTEGER; VAR size: INTEGER);
  VAR i: INTEGER;
BEGIN
  LAST:=TRUE;
  enter_host_module(m,def);
  cns.do_const(m);
  NEW(flw.proc_flow,vrs.cu.prc_sz);
  FOR i:=0 TO HIGH(flw.proc_flow) DO flw.proc_flow[i]:=NIL END;
  code_len:=put.cnt; -- imported code
  sta.gen_code(m);
  flow:=NIL; last_flow:=ADR(flow);
  FOR i:=0 TO HIGH(flw.proc_flow) DO
    IF flw.proc_flow[i]=NIL THEN
      error('procedure %d is not declared\n',i)
    END;
    tie(flw.proc_flow[i]);
  END;
  mark_flow(flow);
  make_info(def,imp,size);
  write_code_file;
END gen_code;

PROCEDURE gen_def(m: ref; def: INTEGER);
BEGIN
  LAST:=FALSE;
  enter_host_module(m,def);
  cns.do_const(m);
  code_len:=put.cnt;
(*
  f:=sta.do_flow();
  IF vrs.proc^.r#NIL THEN
    error('proc_body in definition - unrealized');
  END;
  flow:=NIL; last_flow:=ADR(flow);
  tie(f); mark_flow(f);
*)
END gen_def;

PROCEDURE ini;
BEGIN
  vrs.vars_no:=0; NEW(vrs.vars,20);
  vrs.prcs_no:=0; NEW(vrs.prcs,20);
  vrs.mdls_no:=0; NEW(vrs.mdls,10); NEW(vrs.mdls_nm,10);
  vrs.rngs_no:=0; NEW(vrs.rngs,20);
  vrs.scnt   :=0; NEW(vrs.scode,100);
  vrs.mg_no  :=0; NEW(vrs.mg,10);
  put.cnt    :=0; NEW(put.code,1000);
  inEmul.init;
  tbl.gen_code:=gen_code;
  tbl.gen_def :=gen_def;
  tbl.get_code:=get_code;
  tbl.put_code:=put_code;
  code_len:=0;
END ini;

BEGIN
  tbl.ini_gen :=ini;
END inGen.
