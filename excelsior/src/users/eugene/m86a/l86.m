MODULE l86; (* Sem 06-Mar-91. (c) KRONOS *)

FROM SYSTEM      IMPORT ADDRESS, ADR;

IMPORT  bio : BIO;
IMPORT  tty : Terminal;
IMPORT  str : Strings;
IMPORT  arg : tskArgs;
IMPORT  mem : pcSystem;

WITH STORAGE : mem;

CONST
  stack_size  = 16*1024;
  modules_lim = 99;
  mglo_lim    = 999;

TYPE
--    code :
--                       bytes
-- info                  64
-- <глобалы>             info.glob_size
-- <код>                 info.code_size
-- <мультиглобалы>       info.mglo_no * 8
-- <таблица процедур>    info.proc_no * 4
-- <таблица вн.процедур> info.link_no * 2
-- <таблица модулей>
  code_ptr = POINTER TO RECORD
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

VAR
  mod    : ARRAY [0..modules_lim] OF code_ptr;
  mod_nm : ARRAY [0..modules_lim] OF ARRAY [0..63] OF CHAR;
  mod_glo: ARRAY [0..modules_lim] OF INTEGER;
  mod_cod: ARRAY [0..modules_lim] OF INTEGER;
  mod_no : INTEGER;

  mg_adr : ARRAY [0..mglo_lim] OF INTEGER;
  mg_size: ARRAY [0..mglo_lim] OF INTEGER;
  mg_no  : INTEGER;

  init   : ARRAY [0..modules_lim] OF INTEGER;
  init_no: INTEGER;

  code   : DYNARR OF CHAR;
  cnt    : INTEGER;

  tbl    : DYNARR OF INTEGER;
  tbl_no : INTEGER;

  hdr    : ARRAY [0..1Bh] OF CHAR;
  relo_ss: INTEGER;
  relo_cs: INTEGER;

  mg_area_size: INTEGER;

PROCEDURE chk_io(VAL nm: ARRAY OF CHAR);
BEGIN
  IF bio.done THEN RETURN END;
  tty.perror(bio.error,'IO error, file "%s", %%s.\n',nm);
  HALT;
END chk_io;

PROCEDURE read_code(n: ARRAY OF CHAR): code_ptr;
  VAR f: bio.FILE; nm: ARRAY [0..63] OF CHAR; src: code_ptr;
BEGIN
  str.print(nm,'%s.obj',n);
  bio.open(f,nm,'r'); chk_io(nm);
  mem.ALLOCATE(src,(bio.eof(f)+3) DIV 4);
  bio.read(f,src,bio.eof(f)); chk_io(nm);
  bio.close(f); chk_io(nm);
  RETURN src;
END read_code;

PROCEDURE read_module(nm: ARRAY OF CHAR; time,mod_up: INTEGER);
  PROCEDURE chk_time(mod_dw: INTEGER);
    VAR c: code_ptr;
  BEGIN
    c:=mod[mod_dw];
    IF mod_up>=0 THEN
      IF c^.def_time=c^.imp_time THEN
        tty.print('Import from program module "%s".\n',mod_nm[mod_dw]);
        HALT;
      ELSIF c^.def_time#time THEN
        tty.print('Version conflict, import from "%s to "%s".\n'
                  'Try to recompile "%s.m".\n',
                  mod_nm[mod_dw],mod_nm[mod_up],mod_nm[mod_up]); HALT;
      END;
    ELSIF c^.def_time#c^.imp_time THEN
      tty.print('Warning: "%s" is not program module.\n',mod_nm[mod_dw]);
    END;
  END chk_time;
  VAR
    p    : POINTER TO ARRAY [0..0FFFFFFh] OF CHAR;
    m    : INTEGER;
    i,j,k: INTEGER;
    c    : code_ptr;
    imp  : DYNARR OF RECORD nm: ARRAY [0..79] OF CHAR; tim: INTEGER END;
BEGIN
  FOR i:=0 TO mod_no-1 DO IF mod_nm[i]=nm THEN chk_time(i); RETURN END END;
  m:=mod_no; INC(mod_no);
  str.print(mod_nm[m],'%s',nm);
  mod[m]:=read_code(nm); c:=mod[m]; p:=ADDRESS(c);
  IF c^.version#103h THEN
    tty.print('Illegal code version: "%s".\n',nm); HALT;
  END;
  chk_time(m);
  tty.print('%-20s: %5d code, %5d global;\n',nm,c^.code_size,c^.glob_size);
  j:=64+c^.glob_size+c^.code_size+c^.mglo_no*8+c^.proc_no*4+c^.link_no*2;
  NEW(imp,c^.exts_no);
  FOR i:=0 TO c^.exts_no-1 DO
    imp[i].tim:=ORD(p^[j])+ORD(p^[j+1])*100h+
                ORD(p^[j+2])*10000h+ORD(p^[j+3])*1000000h;
    INC(j,8); k:=0;
    WHILE p^[j]#0c DO imp[i].nm[k]:=p^[j]; INC(j); INC(k) END;
    INC(j); imp[i].nm[k]:=0c;
  END;
  FOR i:=0 TO HIGH(imp) DO read_module(imp[i].nm,imp[i].tim,m) END;
  DISPOSE(imp);
  init[init_no]:=m; INC(init_no);
END read_module;

PROCEDURE byte(n: INTEGER);
BEGIN
  IF cnt>=LEN(code) THEN RESIZE(code,cnt+50000) END;
  code[cnt]:=CHAR(n); INC(cnt);
END byte;

PROCEDURE word(n: INTEGER);
BEGIN
  IF cnt>=LEN(code)-1 THEN RESIZE(code,cnt+50000) END;
  code[cnt]:=CHAR(n); INC(cnt);
  code[cnt]:=CHAR(n>>8); INC(cnt);
END word;

PROCEDURE item(n: INTEGER);
  VAR l: INTEGER;
BEGIN
  IF tbl_no>=LEN(tbl) THEN RESIZE(tbl,tbl_no+100) END;
  l:=n MOD 1000h;
  n:=(n-l)*1000h+l;
  tbl[tbl_no]:=n; INC(tbl_no);
END item;

PROCEDURE get_word(p: ADDRESS; n: INTEGER): INTEGER;
  VAR pp: POINTER TO ARRAY [0..0FFFFh] OF CHAR;
BEGIN
  pp:=p;
  RETURN ORD(pp^[n])+ORD(pp^[n+1])<<8+ORD(pp^[n+2])<<16+ORD(pp^[n+3])<<24;
END get_word;

PROCEDURE alloc_modules;
  VAR
    c      : code_ptr;
    m,i,j,n: INTEGER;
    p      : POINTER TO ARRAY [0..0FFFFFFh] OF CHAR;
BEGIN
  FOR m:=0 TO mod_no-1 DO
    c:=mod[m]; p:=ADDRESS(c);
    WHILE cnt MOD 16 # 0 DO byte(0) END;
    mod_glo[m]:=cnt; i:=64;
    IF cnt+c^.glob_size>LEN(code) THEN RESIZE(code,cnt+c^.glob_size+50000) END;
    FOR j:=0 TO c^.glob_size-1 DO
      code[cnt]:=p^[i]; INC(cnt); INC(i);
    END;
    WHILE cnt MOD 16 # 0 DO byte(0) END;
    mod_cod[m]:=cnt;
    IF cnt+c^.code_size>LEN(code) THEN RESIZE(code,cnt+c^.code_size+50000) END;
    FOR j:=0 TO c^.code_size-1 DO
      code[cnt]:=p^[i]; INC(cnt); INC(i);
    END;
    FOR j:=0 TO c^.mglo_no-1 DO
      n:=get_word(p,i);
      INC(i,4);
      mg_adr[mg_no]:=mod_glo[m]+n;
      n:=get_word(p,i);
      mg_size[mg_no]:=n;
      INC(i,4);
      INC(mg_no);
    END;
    i:=mod_glo[m] DIV 10h; j:=mod_cod[m];
    code[j]:=CHAR(i); code[j+1]:=CHAR(i>>8);
    item(j);
  END;
END alloc_modules;

PROCEDURE procedure_ip(md,pn: INTEGER): INTEGER;
  VAR
    c    : code_ptr;
    p    : POINTER TO ARRAY [0..0FFFFFFh] OF CHAR;
    j    : INTEGER;
BEGIN
  c:=mod[md]; p:=ADDRESS(c);
  j:=64+c^.glob_size+c^.code_size+c^.mglo_no*8+pn*4;
  RETURN ORD(p^[j])+ORD(p^[j+1])*256;
END procedure_ip;

PROCEDURE set_mod_refs;
  VAR
    c    : code_ptr;
    m,md : INTEGER;
    pn   : INTEGER;
    i,j,k: INTEGER;
    p    : POINTER TO ARRAY [0..0FFFFFFh] OF CHAR;
    imp  : DYNARR OF RECORD
      nm  : ARRAY [0..79] OF CHAR;
      ofs : INTEGER;
      no  : INTEGER;
    END;
BEGIN
  FOR m:=0 TO mod_no-1 DO
    c:=mod[m]; p:=ADDRESS(c);
    j:=64+c^.glob_size+c^.code_size+c^.mglo_no*8+c^.proc_no*4+c^.link_no*2;
    NEW(imp,c^.exts_no);
    FOR i:=0 TO c^.exts_no-1 DO
      INC(j,4);
      imp[i].ofs:=get_word(p,j);
      INC(j,4); k:=0;
      WHILE p^[j]#0c DO imp[i].nm[k]:=p^[j]; INC(j); INC(k) END;
      INC(j); imp[i].nm[k]:=0c;
    END;
    FOR i:=0 TO HIGH(imp) DO
      j:=0;
      WHILE mod_nm[j]#imp[i].nm DO INC(j) END;
      imp[i].no:=j;
      IF imp[i].ofs>=0 THEN
        k:=mod_glo[m]+imp[i].ofs;
        code[k+0]:=CHAR(mod_glo[j] DIV 10h);
        code[k+1]:=CHAR(mod_glo[j] DIV 1000h);
        item(imp[i].ofs+mod_glo[m]);
      END;
    END;
    j:=64+c^.glob_size+c^.code_size+c^.mglo_no*8+c^.proc_no*4;
    FOR i:=0 TO c^.link_no-1 DO
      k :=ORD(p^[j])+ORD(p^[j+1])*256; INC(j,2);
      item(k+2+mod_glo[m]);
      k :=mod_glo[m]+k;
      pn:=ORD(code[k+0])+ORD(code[k+1])*256;
      md:=ORD(code[k+2])+ORD(code[k+3])*256;
      IF md=0 THEN md:=m ELSE md:=imp[md-1].no END;
      pn:=procedure_ip(md,pn);
      md:=mod_cod[md] DIV 10h;
      code[k+0]:=CHAR(pn); code[k+1]:=CHAR(pn>>8);
      code[k+2]:=CHAR(md); code[k+3]:=CHAR(md>>8);
    END;
    DISPOSE(imp);
  END;
END set_mod_refs;

PROCEDURE make_ini_code;
  VAR m,md: INTEGER;
BEGIN
  WHILE cnt MOD 16 # 0 DO byte(0) END;
  relo_cs:=cnt DIV 10h;
  FOR m:=0 TO init_no-1 DO
    md:=init[m];
    byte(9Ah);  -- call far
    word(procedure_ip(md,0));
    item(cnt);
    word(mod_cod[md] DIV 10h);
  END;
  byte(0B8h); word(4C00h); -- mov ax,4C00h
  byte(0CDh); byte(021h);  -- int 21h
  WHILE cnt MOD 16 # 0 DO byte(0) END;
END make_ini_code;

PROCEDURE alloc_mg;
  VAR adr,i,a,n: INTEGER;
BEGIN
  adr:=cnt;
  FOR i:=0 TO mg_no-1 DO
    a:=mg_adr[i];
    n:=adr MOD 8000h;
    code[a+0]:=CHAR(n);
    code[a+1]:=CHAR(n>>8);
    INC(a,2); n:=adr DIV 8000h * 800h;
    code[a+0]:=CHAR(n);
    code[a+1]:=CHAR(n>>8);
    item(a);
    INC(adr,mg_size[i]);
  END;
  WHILE adr MOD 10h # 0 DO INC(adr) END;
  mg_area_size:=adr-cnt;
END alloc_mg;

PROCEDURE make_hdr;
  VAR
    n,i     : INTEGER;
    page_cnt: INTEGER;
    hdr_size: INTEGER;
    min_mem : INTEGER;
    max_mem : INTEGER;
BEGIN
  relo_ss:=(cnt+mg_area_size) DIV 10h;
  hdr_size:=(1Ch+tbl_no*4 + 15) DIV 10h;
  page_cnt:=(hdr_size*16+cnt+511) DIV 512;
  min_mem:=(mg_area_size+stack_size+15) DIV 10h;
  max_mem:=min_mem;
  hdr[0]:=CHAR(5Ah); hdr[1]:=CHAR(4Dh);
  n:=page_cnt*512-hdr_size*16-cnt;
  hdr[2] :=CHAR(n);        hdr[3] :=CHAR(n>>8);
  hdr[4] :=CHAR(page_cnt); hdr[5] :=CHAR(page_cnt>>8);
  hdr[6] :=CHAR(tbl_no);   hdr[7] :=CHAR(tbl_no>>8);
  hdr[8] :=CHAR(hdr_size); hdr[9] :=CHAR(hdr_size>>8);
  hdr[10]:=CHAR(min_mem);  hdr[11]:=CHAR(min_mem>>8);
  hdr[12]:=CHAR(max_mem);  hdr[13]:=CHAR(max_mem>>8);
  hdr[14]:=CHAR(relo_ss);  hdr[15]:=CHAR(relo_ss>>8);
  hdr[16]:=CHAR(stack_size); hdr[17]:=CHAR(stack_size>>8);
  hdr[18]:=0c;             hdr[19]:=0c;
  hdr[20]:=0c;             hdr[21]:=0c;
  hdr[22]:=CHAR(relo_cs);  hdr[23]:=CHAR(relo_cs>>8);
  hdr[24]:=CHAR(1Ch);      hdr[25]:=0c;
  hdr[26]:=0c;             hdr[27]:=0c;
  WHILE (tbl_no*4+BYTES(hdr)) MOD 16 # 0 DO item(0) END;
  WHILE (cnt+BYTES(hdr)+tbl_no*4) MOD 512 # 0 DO byte(0) END;
  n:=0;
  FOR i:=0 TO HIGH(hdr) BY 2 DO
    n:=n+ORD(hdr[i])+ORD(hdr[i+1])*256;
  END;
  FOR i:=0 TO tbl_no-1 DO
    n:=(n+tbl[i]) MOD 10000h;
    n:=(n+tbl[i] DIV 10000h) MOD 10000h;
  END;
  FOR i:=0 TO cnt-1 BY 2 DO
    n:=(n+ORD(code[i])+ORD(code[i+1])*256) MOD 10000h;
  END;
  n:=-n;
  hdr[18]:=CHAR(n); hdr[19]:=CHAR(n>>8);
END make_hdr;

VAR
  f       : bio.FILE;
  onm     : ARRAY [0..63] OF CHAR;

BEGIN
  mg_no:=0;
  mod_no:=0;
  init_no:=0;
  cnt:=0; NEW(code);
  tbl_no:=0; NEW(tbl);
  IF LEN(arg.words)#1 THEN tty.print('l86 <code file name>\n'); HALT END;
  read_module(arg.words[0],0,-1);
  ASSERT(init_no=mod_no);
  alloc_modules;
  set_mod_refs;
  make_ini_code;
  alloc_mg;
  make_hdr;
  str.print(onm,'%s.exe',arg.words[0]);
  bio.create(f,onm,'w',1); chk_io(onm);
  bio.write(f,ADR(hdr),BYTES(hdr)); chk_io(onm);
  bio.write(f,ADR(tbl),tbl_no*4); chk_io(onm);
  bio.write(f,ADR(code),cnt); chk_io(onm);
  bio.close(f); chk_io(onm);
  tty.print('-----------------------------\n');
  tty.print('code                : %6d b\n',cnt);
  tty.print('global data         : %6d b\n',mg_area_size);
  tty.print('relocate table      : %6d \n',tbl_no);
END l86.
