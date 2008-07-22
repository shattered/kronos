IMPLEMENTATION MODULE nsGen; (*$N+ Sem 06-Oct-90. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR, ADDRESS, WORD;

IMPORT tbl : pcTab;
IMPORT mem : pcSystem;
IMPORT mcd : defCodes;
IMPORT bio : BIO;

IMPORT sym : nsSym;
IMPORT put : nsCmd;
IMPORT des : nsDesig;
IMPORT exp : nsExpr;
IMPORT sta : nsStat;

FROM pcTab      IMPORT  ref;
FROM nsExpr     IMPORT  node, node_mode;

WITH STORAGE : mem;

VAR
  -- code = info <глобалы> <код> <multiglobals> <externals>
  info  :  RECORD
    version  : INTEGER;   -- версия кодофайла
    def_time : INTEGER;   -- время компиляции definition
    imp_time : INTEGER;   -- время компиляции implementation
    glob_size: INTEGER;   -- размер глобалов
    code_size: INTEGER;   -- размер кода
    min_stack: INTEGER;   -- мин. стек для глоб. мультизначений
    add_stack: INTEGER;   -- дополнительный стек
    no_exts  : INTEGER;
    no_mg    : INTEGER;
    reserv_09: INTEGER;
    reserv_0A: INTEGER;
    reserv_0B: INTEGER;
    reserv_0C: INTEGER;
    reserv_0D: INTEGER;
    reserv_0E: INTEGER;
    reserv_0F: INTEGER;
  END;

  flow      : node;
  last_flow : POINTER TO node;

  proc_flow : ARRAY [0..255] OF node;

  code_len  : INTEGER;
  case_src  : sym.access;
  TOS       : sym.access;

PROCEDURE move(a,b: ADDRESS; s: INTEGER); CODE mcd.move END move;
PROCEDURE bblt(a1: ADDRESS; s1: INTEGER;
               a2: ADDRESS; s2: INTEGER;
                             s: INTEGER); CODE mcd.bblt END bblt;

PROCEDURE error(s: ARRAY OF CHAR; SEQ x: WORD);
BEGIN
  tbl.error(0,0,s,x); HALT(1);
END error;

PROCEDURE copy(VAR f: node);
  VAR l: node;
BEGIN
  IF (f#NIL) & f^.mark & (f^.len<=8) & (f^.cnt<2) THEN
    INC(f^.cnt); l:=exp.new(); l^:=f^; f:=l;
    f^.mark:=FALSE; f^.next:=NIL;
  END;
END copy;

PROCEDURE move_jump(VAR f: node);
  VAR l: node;
BEGIN
  l:=f;
  LOOP
    IF (f=NIL) OR (f^.len>0) OR (f^.md#nm_goto) THEN RETURN END;
    f:=f^.true;
    IF l=f THEN RETURN END;
  END;
END move_jump;

PROCEDURE tie(f: node);
  VAR l: node; i: INTEGER;
BEGIN
  IF (f=NIL) OR f^.mark THEN RETURN END;
  f^.adr:=code_len; INC(code_len,f^.len+f^.tlen);
  last_flow^:=f; last_flow:=ADR(f^.next); f^.mark:=TRUE;
  CASE f^.md OF
    |nm_call,nm_goto:
      move_jump(f^.true); copy(f^.true); tie(f^.true);
    |nm_case:
      move_jump(f^.false); tie(f^.false);
      FOR i:=0 TO HIGH(f^.alts) DO move_jump(f^.alts[i]); tie(f^.alts[i]) END;
    |nm_cond:
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

PROCEDURE mark_flow(f: node);
  PROCEDURE gen_jp(f: node): INTEGER;
    VAR p: node; c,n,i: INTEGER;
  BEGIN
    f^.tpos:=put.cnt;
    CASE f^.md OF
      |nm_call:
        p:=proc_flow[f^.proc]; ASSERT(p#NIL);
        IF des.prcs[f^.proc].mod#0 THEN
          HALT(1);
        ELSIF des.prcs[f^.proc].lvl<=1 THEN
          put.spr(TOS,put.MD,4); c:=put.cnt; put.put({1});
          put.disp(p^.adr-f^.adr-f^.len-c+f^.tpos);
        ELSE
          put.put({1}); put.disp(p^.adr-f^.adr-f^.len);
        END;
        IF (f^.true#NIL) & (f^.next#f^.true) THEN
          c:=put.cnt; put.put({1,3,5,6,7});
          put.disp(f^.true^.adr-f^.adr-f^.len-c+f^.tpos);
        END;
      |nm_case:
        case_src.rg_x:=f^.proc;
        c:=f^.false^.adr-f^.adr-f^.len;
        FOR n:=0 TO HIGH(f^.alts) DO
          i:=f^.alts[n]^.adr-f^.adr-f^.len;
          IF i>c THEN c:=i END;
        END;
        IF (i>=-80h) & (i<=7Fh) THEN
          case_src.xm:=sym.xm_b;
          put.case(case_src,1);
          put.put2(HIGH(f^.ctbl)+1);
          FOR n:=0 TO HIGH(f^.ctbl) DO
            i:=f^.ctbl[n];
            IF i<0 THEN
              c:=f^.false^.adr-f^.adr-f^.len;
            ELSE
              c:=f^.alts[i]^.adr-f^.adr-f^.len;
            END;
            put.put(c);
          END;
        ELSIF (i>=-8000h) & (i<=7FFFh) THEN
          case_src.xm:=sym.xm_w;
          put.case(case_src,2);
          put.put2(HIGH(f^.ctbl)+1);
          FOR n:=0 TO HIGH(f^.ctbl) DO
            i:=f^.ctbl[n];
            IF i<0 THEN
              c:=f^.false^.adr-f^.adr-f^.len;
            ELSE
              c:=f^.alts[i]^.adr-f^.adr-f^.len;
            END;
            put.put(c); put.put(c>>8);
          END;
        ELSE
          case_src.xm:=sym.xm_d;
          put.case(case_src,4);
          put.put2(HIGH(f^.ctbl)+1);
          FOR n:=0 TO HIGH(f^.ctbl) DO
            i:=f^.ctbl[n];
            IF i<0 THEN
              c:=f^.false^.adr-f^.adr-f^.len;
            ELSE
              c:=f^.alts[i]^.adr-f^.adr-f^.len;
            END;
            put.put(c); put.put(c>>8); put.put(c>>16); put.put(c>>24);
          END;
        END;
      |nm_cond:
        ASSERT(f^.true#NIL); ASSERT(f^.false#NIL);
        IF f^.false=f^.next THEN
          put.put({1,3}+BITSET(f^.flag<<4));
          put.disp(f^.true^.adr-(f^.adr+f^.len));
        ELSIF f^.true=f^.next THEN
          put.put({1,3}+BITSET(f^.flag<<4)/{4});
          put.disp(f^.false^.adr-f^.adr-f^.len);
        ELSE
          put.put({1,3}+BITSET(f^.flag<<4));
          put.disp(f^.true^.adr-f^.adr-f^.len);
          c:=put.cnt-f^.tpos;
          put.put({1,3,5,6,7});
          put.disp(f^.false^.adr-f^.adr-f^.len-c);
        END;
      |nm_goto:
        IF (f^.true#NIL) & (f^.next#f^.true) THEN
          put.put({1,3,5,6,7});
          put.disp(f^.true^.adr-f^.adr-f^.len);
        END;
    END;
    RETURN put.cnt-f^.tpos;
  END gen_jp;
  VAR
    fail_no: INTEGER; i,k: node; j,c: INTEGER;
BEGIN
  c:=put.cnt;
  REPEAT
    fail_no:=0; i:=f; put.cnt:=c;
    WHILE i#NIL DO
      IF i^.next#NIL THEN ASSERT(i^.next^.adr=i^.adr+i^.len+i^.tlen) END;
      j:=gen_jp(i)-i^.tlen;
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
        |sym.am_aSB   : level:=m^.adr;
        |sym.am_aSBimm: level:=m^.adr;
        |sym.am_aaSB  : level:=m^.adr;
      ELSE
      END;
    END;
  END import_val;

  VAR a,b: ADDRESS; i: INTEGER;
BEGIN
  IF l=NIL THEN
--tty.print('import my def\n');
    -- чтение результатов генерации для единицы компиляции
    ASSERT(des.mdls_no>0); ASSERT(SIZE(s)>=SIZE(sym.access_cu));
    move(ADR(des.cu),ADR(s),SIZE(des.cu));
    a:=ADR(s)+SIZE(sym.access_cu);
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
  IF m=NIL THEN error('module=NIL') END;
  CASE l^.md OF
    |tbl.var,tbl.const:
--tty.print('import var or const\n');
      ASSERT(SIZE(s)=SIZE(sym.access));
      l^.adr:=des.new_var();
      move(ADR(des.vars[l^.adr]),ADR(s),SIZE(sym.access));
      import_val(des.vars[l^.adr]);
    |tbl.range,tbl.subtype,tbl.set,tbl.enumeration,tbl.array,tbl.packed_array:
      l^.adr:=des.new_rng();
      move(ADR(des.rngs[l^.adr]),ADR(s),SIZE(s));
      import_val(des.rngs[l^.adr].l);
      import_val(des.rngs[l^.adr].r);
    |tbl.procedure:
--tty.print('import procedure\n');
      ASSERT(SIZE(s)=SIZE(sym.access_proc));
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
      move(ADR(des.mdls[l^.adr]),ADR(s),SIZE(sym.access_import));
      des.mdls_nm[l^.adr]:=l^.nm;
      des.mdls[l^.adr].offset:=l^.adr;
--      ASSERT(l^.adr=des.mdls[l^.adr].offset);
  ELSE error('import object, illegal mode');
  END;
END put_code;

PROCEDURE get_code(l: ref; VAR s: STRING);
  VAR a,b: ADDRESS; i,ini,an: INTEGER;
BEGIN
  IF l=NIL THEN
    WITH des.cu DO
      str_sz:=des.scnt; ini:=ini_sz; ini_sz:=code_len; mg_sz:=des.mg_no;
      i:=str_sz+(ini_sz+3) DIV 4+mg_sz*SIZE(des.mg[0])+SIZE(sym.access_cu);
    END;
    IF sta.LAST THEN
      s.ADR :=ADR(des.cu);
      s.HIGH:=BYTES(sym.access_cu)-1;
    ELSE
      NEW(s,i*4);
      move(ADR(s),ADR(des.cu),SIZE(sym.access_cu));
      a:=ADR(s)+SIZE(sym.access_cu);
      move(a,ADR(des.scode),des.cu.str_sz);
      INC(a,des.cu.str_sz);
      an:=0;
      bblt(a,an*8,ADR(put.code),0,ini*8);
      INC(an,ini);
      WHILE flow#NIL DO
        bblt(a,an*8,ADR(put.code),flow^.pos*8,flow^.len*8);
        INC(an,flow^.len);
        bblt(a,an*8,ADR(put.code),flow^.tpos*8,flow^.tlen*8);
        INC(an,flow^.tlen);
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
      s.HIGH:=BYTES(sym.access)-1;
    |tbl.range,tbl.subtype,tbl.set,tbl.enumeration,tbl.array,tbl.packed_array:
      s.ADR :=ADR(des.rngs[l^.adr]);
      s.HIGH:=BYTES(sym.access_range)-1;
    |tbl.procedure:
      s.ADR :=ADR(des.prcs[l^.adr]);
      s.HIGH:=BYTES(sym.access_proc)-1;
    |tbl.module:
      des.mdls[l^.adr].offset:=l^.adr;
      s.ADR :=ADR(des.mdls[l^.adr]);
      s.HIGH:=BYTES(sym.access_import)-1;
  ELSE error('export object, illegal mode %d',l^.md);
  END;
END get_code;

PROCEDURE make_info(def,imp: INTEGER; VAR size: INTEGER);
BEGIN
  info.version  :=102h;
  info.def_time :=def;
  info.glob_size:=des.scnt*4;
  info.code_size:=code_len;
  info.min_stack:=0;
  info.add_stack:=0;
  info.no_exts  :=des.mdls_no;
  info.no_mg    :=des.mg_no;
  info.reserv_09:=0;
  info.reserv_0A:=0;
  info.reserv_0B:=0;
  info.reserv_0C:=0;
  info.reserv_0D:=0;
  info.reserv_0E:=0;
  info.reserv_0F:=0;
  size:=(info.code_size+3) DIV 4;
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
  tbl.get_name(des.mdls_nm[0],nm);
  i:=0;
  WHILE (i<HIGH(nm)-4) & (nm[i]#0c) DO INC(i) END;
  nm[i]:='.'; INC(i); nm[i]:='o'; INC(i);
  nm[i]:='b'; INC(i); nm[i]:='j'; INC(i);
  REPEAT nm[i]:=0c; INC(i) UNTIL i>HIGH(nm);
  bio.create(out,nm,'hw',BYTES(info)+info.code_size+info.glob_size); io_chk;
  bio.buffers(out,1,4096); io_chk;
  bio.write(out,ADR(info),BYTES(info)); io_chk;
  bio.write(out,ADR(des.scode),info.glob_size); io_chk;
  bio.write(out,ADR(put.code),des.cu.ini_sz); io_chk;
  WHILE flow#NIL DO
    ASSERT(flow^.len>=0); ASSERT(flow^.tlen>=0);
    bio.fwrite(out,ADR(put.code),flow^.pos,flow^.len); io_chk;
    bio.fwrite(out,ADR(put.code),flow^.tpos,flow^.tlen); io_chk;
    flow:=flow^.next;
  END;
  bio.write(out,ADR(des.mg[0]),info.no_mg*BYTES(des.mg[0])); io_chk;
  FOR e:=des.mdls_no-1 TO 1 BY -1 DO
    bio.write(out,ADR(des.mdls[e].def_time),4); io_chk;
    tbl.get_name(des.mdls_nm[e],ext); i:=0;
    WHILE (i<HIGH(ext)) & (ext[i]#0c) DO INC(i) END;
    REPEAT ext[i]:=0c; INC(i) UNTIL i MOD 4 =0;
    bio.write(out,ADR(ext),i); io_chk;
  END;
  bio.close(out); io_chk;
END write_code_file;

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
    des.cu.glo_sz:=0;
--    des.cu.str_sz:=0;
    des.cu.mg_sz :=0;
  ELSIF des.mdls_nm[0]#m^.nm THEN error('illegal name');
  END;
  NEW(des.proc);
  des.proc^.md:=tbl.proc_body;
  des.proc^.dw:=m^.dw;
  des.proc^.l:=m;
  des.proc^.r:=NIL;
  des.proc^.nxt:=NIL;
  des.proc^.adr:=0;
  sta.last_proc:=des.proc;
  sta.procs:=des.proc;
END enter_host_module;

PROCEDURE gen_code(m: ref; def,imp: INTEGER; VAR size: INTEGER);
  VAR f,f1: exp.node; i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(proc_flow) DO proc_flow[i]:=NIL END;
  sta.LAST:=TRUE;
  enter_host_module(m,def);
  code_len:=put.cnt;
  f:=NIL; last_flow:=ADR(f);
  WHILE des.proc#NIL DO
    last_flow^:=sta.do_flow();
    proc_flow[des.proc^.l^.adr]:=last_flow^;
    last_flow:=ADR(last_flow^^.next);
    des.proc:=des.proc^.r;
  END;
  flow:=NIL; last_flow:=ADR(flow);
  WHILE f#NIL DO
    f1:=f^.next; f^.next:=NIL; tie(f); f:=f1;
  END;
  mark_flow(flow);
  make_info(def,imp,size);
  write_code_file;
END gen_code;

PROCEDURE gen_def(m: ref; def: INTEGER);
  VAR f: exp.node;
BEGIN
  enter_host_module(m,def);
  f:=sta.do_flow();
  IF des.proc^.r#NIL THEN
    error('proc_body in definition - unrealized');
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
  sta.LAST:=FALSE;
  tbl.gen_code:=gen_code;
  tbl.gen_def :=gen_def;
  tbl.get_code:=get_code;
  tbl.put_code:=put_code;
END ini;

BEGIN
  tbl.ini_gen :=ini;
  WITH case_src DO
    am:=sym.am_aPC;
    xm:=sym.xm_b;
    rg:=0;
    rg_x:=0;
    n:=0;
    disp:=6;
    level:=0;
  END;
  WITH TOS DO
    am:=sym.am_TOS;
    xm:=sym.xm_off;
    rg:=0;
    rg_x:=0;
    n:=0;
    disp:=0;
    level:=0;
  END;
END nsGen.
