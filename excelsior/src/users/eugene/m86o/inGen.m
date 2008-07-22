IMPLEMENTATION MODULE inGen; (*$U+ Sem 06-Oct-90. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR, ADDRESS, WORD;

IMPORT tbl : pcTab;
IMPORT mem : pcSystem;
IMPORT bio : BIO;
IMPORT mcd : lowLevel;
IMPORT str : Strings;

IMPORT  inNum;

IMPORT sym : inSym;
IMPORT put : inCmd;
IMPORT vrs : inVars;
IMPORT cns : inConst;
IMPORT flw : inFlow;
IMPORT sta : inStat;

IMPORT  tty : Terminal;

FROM pcTab      IMPORT  ref;

WITH STORAGE : mem;

TYPE fixup_rec=RECORD ofs,size,index: INTEGER END;

VAR
  flow      : flw.node;
  last_flow : POINTER TO flw.node;
  code_len  : INTEGER;
  LAST      : BOOLEAN;
  acs_imp   : sym.access_import;
  fixup     : DYNARR OF fixup_rec;

PROCEDURE error(s: ARRAY OF CHAR; SEQ x: WORD);
BEGIN
  tbl.error(0,TRUE,s,x);
END error;

PROCEDURE copy(VAR f: flw.node);
  VAR l: flw.node;
BEGIN
  IF (f#NIL) & (f^.adr>0) & (f^.len<=8) & (f^.cnt<2) THEN
    INC(f^.cnt); l:=flw.new(); l^:=f^; f:=l;
    f^.adr:=-1; f^.next:=NIL;
  END;
END copy;

PROCEDURE move_jump(VAR f: flw.node);
  VAR i: INTEGER;
BEGIN
  i:=10;
  REPEAT
    IF (f=NIL) OR (f^.len>0) OR (f^.md#flw.nm_goto) THEN RETURN END;
    f:=f^.goto; DEC(i);
  UNTIL i=0;
END move_jump;

PROCEDURE tie(f: flw.node);
  VAR l: flw.node; i: INTEGER;
BEGIN
  IF (f=NIL) OR (f^.adr>0) THEN RETURN END;
  f^.adr:=code_len; INC(code_len,f^.len+f^.tlen);
  last_flow^:=f; last_flow:=ADR(f^.next); INC(f^.cnt);
  CASE f^.md OF
    |flw.nm_call,flw.nm_ext_call,flw.nm_goto:
      move_jump(f^.goto); copy(f^.goto); tie(f^.goto);
    |flw.nm_case:
      move_jump(f^.goto); tie(f^.goto);
      FOR i:=0 TO HIGH(f^.ctbl) DO
        move_jump(f^.ctbl[i]); tie(f^.ctbl[i]);
      END;
    |flw.nm_cond:
      ASSERT( (f^.else#NIL) & (f^.goto#NIL) );
      move_jump(f^.else); move_jump(f^.goto);
      IF f^.else^.adr<=0 THEN
        tie(f^.else); tie(f^.goto);
      ELSIF f^.goto^.adr<=0 THEN
        tie(f^.goto); tie(f^.else);
      ELSE
        copy(f^.else);
        IF f^.else^.adr<=0 THEN
          tie(f^.else); tie(f^.goto);
        ELSE
          copy(f^.goto); tie(f^.goto); tie(f^.else);
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
      vrs.prcs_nm[l^.adr]:=l^.nm;
      mcd.move(ADR(vrs.prcs[l^.adr]),ADR(s),SIZE(sym.access_proc));
      IF m#NIL THEN
        WITH vrs.prcs[l^.adr] DO
          mod:=m^.adr; ofs:=-1;
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
      ELSE
        FOR i:=1 TO vrs.mdls_no-1 DO
          IF vrs.mdls_nm[i]=l^.nm THEN l^.adr:=i; RETURN END;
        END;
        l^.adr:=vrs.new_mod();
        IF l^.adr=0 THEN l^.adr:=vrs.new_mod() END;
        mcd.move(ADR(vrs.mdls[l^.adr]),ADR(s),SIZE(sym.access_import));
        vrs.mdls_nm[l^.adr]:=l^.nm;
        WITH vrs.mdls[l^.adr] DO ofs:=-1; no:=l^.adr END;
      END;
  ELSE error('import object, illegal mode');
  END;
END put_code;

PROCEDURE get_code(l: ref; VAR s: STRING);
  VAR a,b: ADDRESS; i,an: INTEGER;
BEGIN
  IF l=NIL THEN
    WITH vrs.cu DO
      glo_sz:=vrs.scnt; ini_sz:=0; mg_sz:=vrs.mg_no;
      i:=glo_sz+mg_sz*BYTES(vrs.mg[0])+BYTES(sym.access_cu);
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
      WHILE flow#NIL DO
        mcd.cmove(a,an,ADR(put.code),flow^.pos,flow^.len);
        INC(an,flow^.len);
        mcd.cmove(a,an,ADR(put.code),flow^.tpos,flow^.tlen);
        INC(an,flow^.tlen);
        flow:=flow^.next;
      END;
      ASSERT(an=vrs.cu.glo_sz);
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

PROCEDURE do_sort(l,r: INTEGER);
  VAR i,j: INTEGER; x: INTEGER; w: fixup_rec;
BEGIN
  i:=l; j:=r; x:=fixup[(l+r) DIV 2].ofs;
  REPEAT
    WHILE fixup[i].ofs<x DO INC(i) END;
    WHILE x<fixup[j].ofs DO DEC(j) END;
    IF i<=j THEN
      w:=fixup[i]; fixup[i]:=fixup[j]; fixup[j]:=w;
      INC(i); DEC(j);
    END;
  UNTIL i>j;
  IF l<j THEN do_sort(l,j) END;
  IF i<r THEN do_sort(i,r) END;
END do_sort;

PROCEDURE make_fixup;
  VAR i,j: INTEGER;
BEGIN
  j:=0;
  FOR i:=0 TO vrs.prcs_no-1 DO
    IF vrs.prcs[i].ofs>=0 THEN
      fixup[j].ofs:=vrs.prcs[i].ofs;
      fixup[j].size:=4;
      fixup[j].index:=j+1;
      INC(j);
    END;
  END;
  FOR i:=0 TO vrs.mdls_no-1 DO
    IF vrs.mdls[i].ofs>=0 THEN
      fixup[j].ofs:=vrs.mdls[i].ofs;
      fixup[j].size:=2;
      fixup[j].index:=j+1;
      INC(j);
    END;
  END;
  FOR i:=0 TO vrs.mg_no-1 DO
    fixup[j].ofs:=vrs.mg[i].offset;
    fixup[j].size:=4;
    fixup[j].index:=j+1;
    INC(j);
  END;
  ASSERT(j=LEN(fixup));
  IF LEN(fixup)=0 THEN RETURN END;
  do_sort(0,LEN(fixup)-1);
END make_fixup;

PROCEDURE write_code_file(main: BOOLEAN);
  VAR
    summ : INTEGER;
    len  : INTEGER;
    r_pos: INTEGER;
    out  : bio.FILE;
    nm   : ARRAY [0..255] OF CHAR;
    fnm  : ARRAY [0..255] OF CHAR;
    pnm  : ARRAY [0..255] OF CHAR;
    enm  : ARRAY [0..255] OF CHAR;
  PROCEDURE io_chk;
  BEGIN
    IF bio.done THEN RETURN END;
    error('I/O error, file: "%s"',fnm);
  END io_chk;
  PROCEDURE byte(n: INTEGER);
  BEGIN
    summ:=(summ+n) MOD 256; INC(len);
    bio.write(out,ADR(n),1);
    IF NOT bio.done THEN io_chk END;
  END byte;
  PROCEDURE byte_seq(VAL s: ARRAY OF CHAR; pos,ln: INTEGER);
     VAR i: INTEGER;
  BEGIN
    ASSERT(ln>=0);
    FOR i:=pos TO pos+ln-1 DO INC(summ,ORD(s[i])); INC(len) END;
    summ:=summ MOD 256;
    bio.fwrite(out,ADR(s),pos,ln); io_chk;
  END byte_seq;
  PROCEDURE rec_start(n: INTEGER);
  BEGIN
    summ:=0; len:=0; byte(n); r_pos:=bio.pos(out); byte(0); byte(0); len:=0;
  END rec_start;
  PROCEDURE rec_end;
    VAR i,j: INTEGER;
  BEGIN
    i:=bio.pos(out); bio.seek(out,r_pos,0);
    j:=len+1; byte(j); byte(j>>8);
    bio.seek(out,i,0); byte(-summ);
  END rec_end;
  PROCEDURE string(VAL s: ARRAY OF CHAR);
    VAR i,j: INTEGER;
  BEGIN
    i:=0; j:=0;
    WHILE (i<=HIGH(s)) & (s[i]#0c) DO INC(i) END;
    byte(i);
    WHILE j<i DO byte(ORD(s[j])); INC(j) END;
  END string;
  PROCEDURE string2(VAL s1,s2: ARRAY OF CHAR);
    VAR i1,i2,j: INTEGER;
  BEGIN
    i1:=0; i2:=0; j:=0;
    WHILE (i1<=HIGH(s1)) & (s1[i1]#0c) DO INC(i1) END;
    WHILE (i2<=HIGH(s2)) & (s2[i2]#0c) DO INC(i2) END;
    byte(i1+i2+1);
    WHILE j<i1 DO byte(ORD(s1[j])); INC(j) END;
    byte(ORD('$')); j:=0;
    WHILE j<i2 DO byte(ORD(s2[j])); INC(j) END;
  END string2;
  PROCEDURE index(n: INTEGER);
  BEGIN
    ASSERT(n>=0);
    IF n<128 THEN byte(n); RETURN END;
    byte(n DIV 256 + 128); byte(n);
  END index;
  VAR e_cnt: INTEGER;
  PROCEDURE write_ename(t: flw.ext_name);
  BEGIN
    IF t=NIL THEN RETURN END;
    write_ename(t^.dw);
    t^.index:=e_cnt; INC(e_cnt); string(t^.nm); index(0);
    write_ename(t^.up);
  END write_ename;
  CONST
    PUBDEF = 090h;     -- public name def
    COMDEF = 0B0h;     -- communal names def
    LOCSYM = 092h;     -- local symbols
    EXTDEF = 08Ch;     -- external names def
    LINNUM = 094h;     -- line number record
    LEDATA = 0A0h;     -- logical enumerated data
    LIDATA = 0A2h;     -- logical iterated data
    THEADR = 080h;     -- T-module header
    LHEADR = 082h;     -- L-module header
    LNAMES = 096h;     -- list of names
    SEGDEF = 098h;     -- segment def
    GRPDEF = 09Ah;     -- group def
    FIXUPP = 09Ch;     -- fixup record
    MODEND = 08Ah;     -- module entry
    COMENT = 088h;     -- comment
  VAR i,n,m,j,k,l: INTEGER; f: flw.node;
BEGIN
  IF vrs.scnt>=10000h THEN
    tbl.error(0,TRUE,'Too large global data segment.');
  END;
  IF code_len>=10000h THEN
    tbl.error(0,TRUE,'Too large code segment.');
  END;
  tbl.get_name(vrs.mdls_nm[0],nm);
  i:=0;
  WHILE (i<HIGH(nm)-4) & (nm[i]#0c) DO fnm[i]:=nm[i]; INC(i) END;
  fnm[i]:='.'; INC(i); fnm[i]:='o'; INC(i);
  fnm[i]:='b'; INC(i); fnm[i]:='j'; INC(i);
  REPEAT fnm[i]:=0c; INC(i) UNTIL i>HIGH(fnm);
  bio.create(out,fnm,'w',1); io_chk;
  bio.buffers(out,2,4096); io_chk;
  ----------------------------------------------------------------------
  --              module header                                       --
  ----------------------------------------------------------------------
  rec_start(THEADR); str.print(enm,'%s.m',nm); string(enm); rec_end;
  ----------------------------------------------------------------------
  --              define code & data segments                         --
  ----------------------------------------------------------------------
  IF main THEN
    rec_start(LNAMES); string('CODE'); string('DATA'); string('STACK'); rec_end;
  ELSE
    rec_start(LNAMES); string('CODE'); string('DATA'); rec_end;
  END;
  rec_start(SEGDEF);
  byte(060h); byte(code_len); byte(code_len>>8); index(1); index(1); index(0);
  rec_end;
  rec_start(SEGDEF);
  byte(060h); byte(vrs.scnt); byte(vrs.scnt>>8); index(2); index(2); index(0);
  rec_end;
  IF main THEN
    rec_start(SEGDEF);
    byte(074h); byte(00); byte(40h); index(3); index(3); index(0);
    rec_end;
  END;
  ----------------------------------------------------------------------
  --              define external names                               --
  ----------------------------------------------------------------------
  rec_start(EXTDEF); j:=0;
  FOR i:=0 TO vrs.prcs_no-1 DO
    IF vrs.prcs[i].ofs>=0 THEN
      tbl.get_name(vrs.mdls_nm[vrs.prcs[i].mod],enm);
      tbl.get_name(vrs.prcs_nm[i],pnm); string2(enm,pnm); index(0); INC(j);
    END;
  END;
  FOR i:=0 TO vrs.mdls_no-1 DO
    IF vrs.mdls[i].ofs>=0 THEN
      tbl.get_name(vrs.mdls_nm[i],enm); string(enm); index(0); INC(j);
    END;
  END;
  FOR i:=0 TO vrs.mg_no-1 DO
    tbl.get_name(vrs.mg[i].name,enm);
    string2(nm,enm); index(0); INC(j);
  END;
  NEW(fixup,j);
  e_cnt:=j+1;
  write_ename(flw.enm_tree);
  rec_end;
  ----------------------------------------------------------------------
  --              define public names (procedures export)             --
  ----------------------------------------------------------------------
  rec_start(PUBDEF);
  index(0); index(1);
  FOR i:=0 TO vrs.prcs_no-1 DO
    IF (vrs.prcs[i].mod=0) & (vrs.prcs[i].export) THEN
      tbl.get_name(vrs.prcs_nm[i],pnm); string2(nm,pnm);
      n:=flw.proc_flow[vrs.prcs[i].no]^.adr;
      byte(n); byte(n>>8); index(0);
    END;
  END;
  rec_end;
  ----------------------------------------------------------------------
  --              define public names (global data export)            --
  ----------------------------------------------------------------------
  rec_start(PUBDEF);
  index(0); index(2); string(nm); byte(0); byte(0); index(0);
  rec_end;
  ----------------------------------------------------------------------
  --              microsoft extensions comment                        --
  ----------------------------------------------------------------------
  rec_start(COMENT); i:=0C0A1h; byte(i>>8); byte(i); rec_end;
  ----------------------------------------------------------------------
  --              library search comment                              --
  ----------------------------------------------------------------------
  FOR n:=1 TO vrs.mdls_no-1 DO
    rec_start(COMENT);
    i:=0C09Fh; byte(i>>8); byte(i);
    tbl.get_name(vrs.mdls_nm[n],enm);
    str.print(pnm,'%s.obj',enm);
    i:=0;
    WHILE pnm[i]#0c DO byte(ORD(pnm[i])); INC(i) END;
    rec_end;
  END;
  rec_start(COMENT);
  i:=0C09Fh; byte(i>>8); byte(i);
  IF tbl.cpu_mode=0 THEN pnm:='rm_rts.obj' ELSE pnm:='pm_rts.obj' END;
  i:=0;
  WHILE pnm[i]#0c DO byte(ORD(pnm[i])); INC(i) END;
  rec_end;
  ----------------------------------------------------------------------
  --              define communal names (multi global data export)    --
  ----------------------------------------------------------------------
  rec_start(COMDEF);
  FOR i:=0 TO vrs.mg_no-1 DO
    tbl.get_name(vrs.mg[i].name,enm);
    string2(nm,enm); index(0); byte(61h);
    byte(88h); n:=vrs.mg[i].size;
    byte(n); byte(n>>8); byte(n>>16); byte(n>>24); byte(1);
  END;
  rec_end;
  ----------------------------------------------------------------------
  --              define local names (local procedures)               --
  ----------------------------------------------------------------------
  rec_start(LOCSYM);
  index(0); index(1);
  FOR i:=0 TO vrs.prcs_no-1 DO
    IF (vrs.prcs[i].mod=0) & NOT vrs.prcs[i].export & (vrs.prcs[i].lvl=1) THEN
      tbl.get_name(vrs.prcs_nm[i],pnm); string2(nm,pnm);
      n:=flw.proc_flow[vrs.prcs[i].no]^.adr;
      byte(n); byte(n>>8); index(0);
    END;
  END;
  rec_end;
  ----------------------------------------------------------------------
  rec_start(LEDATA);
  index(1); byte(0); byte(0); byte(0); byte(0);
  rec_end;
  rec_start(FIXUPP);
  byte(0C8h); byte(0); byte(50h); index(2); byte(0); byte(0);
  rec_end;
  ----------------------------------------------------------------------
  i:=2; j:=0; k:=0; f:=flow;
  LOOP
    ----------------------------------------------------------------------
    --              write code                                          --
    ----------------------------------------------------------------------
    rec_start(LEDATA);
    index(1); byte(i); byte(i>>8); n:=0;
    LOOP
      -- n - record length count, i - record offset count,
      -- j,k,f - current pointer in code list
      IF f=NIL THEN rec_end; EXIT END;
      IF k=0 THEN
        IF f^.len-j+n>400h THEN
          byte_seq(put.code,f^.pos+j,400h-n);
          INC(j,400h-n); n:=400h; rec_end; EXIT;
        ELSE
          byte_seq(put.code,f^.pos+j,f^.len-j);
          INC(n,f^.len-j); k:=1; j:=0;
        END;
      ELSE
        IF f^.tlen-j+n>400h THEN rec_end; EXIT END;
        byte_seq(put.code,f^.tpos+j,f^.tlen-j);
        INC(n,f^.tlen-j); j:=0; k:=0;
        IF f^.md=flw.nm_ext_call THEN
          rec_end;
          rec_start(FIXUPP);
          m:=0CC00h+(f^.adr-i+f^.len+1); byte(m>>8); byte(m);
          byte(52h); index(f^.name^.index); byte(0); byte(0);
          rec_end;
          f:=f^.next; EXIT;
        END;
        f:=f^.next;
      END;
    END;
    IF f=NIL THEN EXIT END;
    INC(i,n);
  END;
  make_fixup;
  i:=0; j:=0;
  LOOP
    -- i - data bytes count, j - fixup count;
    IF i>=vrs.scnt THEN EXIT END;
    ----------------------------------------------------------------------
    --              write global data                                   --
    ----------------------------------------------------------------------
    rec_start(LEDATA);
    index(2); byte(i); byte(i>>8);
    k:=vrs.scnt-i;
    IF k>1024 THEN k:=1024 END;
    l:=j;
    WHILE (l<=HIGH(fixup)) & (fixup[l].ofs+fixup[l].size<=i+k) DO INC(l) END;
    IF (l<=HIGH(fixup)) & (fixup[l].ofs<i+k) THEN k:=fixup[l].ofs-i END;
    ASSERT(k>0);
    byte_seq(vrs.scode,i,k);
    rec_end;
    IF j<l THEN
      ----------------------------------------------------------------------
      --              fixup                                               --
      ----------------------------------------------------------------------
      rec_start(FIXUPP);
      REPEAT
        CASE fixup[j].size OF
          |4: n:=0CC00h+fixup[j].ofs-i; byte(n>>8); byte(n);
              byte(52h); index(fixup[j].index); byte(0); byte(0);
          |2: n:=0C800h+fixup[j].ofs-i; byte(n>>8); byte(n);
              byte(52h); index(fixup[j].index); byte(0); byte(0);
        END;
        INC(j);
      UNTIL j=l;
      rec_end;
    END;
    INC(i,k);
  END;
  DISPOSE(fixup);
  ----------------------------------------------------------------------
  --              write debug info                                    --
  ----------------------------------------------------------------------
  rec_start(LINNUM); f:=flow;
  index(0); index(1);
  WHILE f#NIL DO
    FOR i:=0 TO HIGH(f^.line) DO
      j:=f^.line[i]+(f^.adr<<16);
      byte(j); byte(j>>8); byte(j>>16); byte(j>>24);
    END;
    f:=f^.next;
  END;
  rec_end;
  ----------------------------------------------------------------------
  rec_start(MODEND);
  IF main THEN
    byte(0C1h); byte(0); index(1); index(1); byte(2); byte(0);
  ELSE
    byte(0)
  END;
  rec_end;
  bio.close(out); io_chk;
END write_code_file;

PROCEDURE enter_host_module(VAR m: ref; def: INTEGER);
  VAR p: ref; i: INTEGER;
BEGIN
  ASSERT((m#NIL) & (m^.md=tbl.module));
  IF vrs.mdls_no=0 THEN m^.adr:=vrs.new_mod() ELSE m^.adr:=0 END;
  IF vrs.mdls_nm[0]<0 THEN
    vrs.mdls_nm[0]:=m^.nm; vrs.mdls[0].def_time:=def;
    vrs.cu.prc_sz:=1; vrs.cu.mg_sz:=0; vrs.cu.glo_sz:=0;
  ELSIF vrs.mdls_nm[0]#m^.nm THEN error('illegal name');
  END;
  IF vrs.scnt=0 THEN
    tbl.get_name(m^.nm,vrs.scode); i:=0;
    WHILE vrs.scode[i]#0c DO INC(i) END;
    vrs.scnt:=i+1;
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
  vrs.prcs[p^.l^.adr].mod:=0;
  vrs.prcs_nm[p^.l^.adr]:=m^.nm;
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
  code_len:=2;
  sta.gen_code(m,imp=def);
  flow:=NIL; last_flow:=ADR(flow);
  FOR i:=0 TO HIGH(flw.proc_flow) DO
    IF flw.proc_flow[i]=NIL THEN
      error('procedure %d is not declared\n',i)
    END;
    tie(flw.proc_flow[i]);
  END;
  mark_flow(flow);
  size:=code_len;
  write_code_file(def=imp);
END gen_code;

PROCEDURE gen_def(m: ref; def: INTEGER);
BEGIN
  LAST:=FALSE;
  enter_host_module(m,def);
  cns.do_const(m);
END gen_def;

PROCEDURE ini;
BEGIN
  flw.enm_tree:=NIL;
  vrs.vars_no:=0; NEW(vrs.vars,20);
  vrs.prcs_no:=0; NEW(vrs.prcs,20); NEW(vrs.prcs_nm,20);
  vrs.mdls_no:=0; NEW(vrs.mdls,10); NEW(vrs.mdls_nm,10);
  vrs.rngs_no:=0; NEW(vrs.rngs,20);
  vrs.scnt   :=0; NEW(vrs.scode,100);
  vrs.mg_no  :=0; NEW(vrs.mg,10);
  put.cnt    :=0; NEW(put.code,1000);
  tbl.gen_code:=gen_code;
  tbl.gen_def :=gen_def;
  tbl.get_code:=get_code;
  tbl.put_code:=put_code;
END ini;

BEGIN
  tbl.ini_gen :=ini;
END inGen.
