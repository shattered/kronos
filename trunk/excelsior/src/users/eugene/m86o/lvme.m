MODULE lvme; (* Sem 06-Mar-91. (c) KRONOS *)

FROM SYSTEM      IMPORT ADDRESS, ADR;

IMPORT  bio : BIO;
IMPORT  tty : Terminal;
IMPORT  str : Strings;
IMPORT  arg : tskArgs;
IMPORT  mem : Heap;

WITH STORAGE : mem;

CONST
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

  mg_seg : ARRAY [0..mglo_lim] OF INTEGER;
  mg_siz : ARRAY [0..mglo_lim] OF INTEGER;
  mg_no  : INTEGER;

  init   : ARRAY [0..modules_lim] OF INTEGER;
  init_no: INTEGER;

  out    : bio.FILE;
  out_nm : ARRAY [0..79] OF CHAR;

  free   : INTEGER;

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

PROCEDURE word(a: ADDRESS; i: INTEGER): INTEGER;
  VAR p: POINTER TO ARRAY [0..0FFFFFFh] OF CHAR;
BEGIN
  p:=a;
  RETURN ORD(p^[i])+ORD(p^[i+1])<<8+ORD(p^[i+2])<<16+ORD(p^[i+3])<<24;
END word;

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
    m,n  : INTEGER;
    i,j,k: INTEGER;
    c    : code_ptr;
    imp  : DYNARR OF RECORD nm: ARRAY [0..79] OF CHAR; tim: INTEGER END;
BEGIN
  FOR i:=0 TO mod_no-1 DO IF mod_nm[i]=nm THEN chk_time(i); RETURN END END;
  m:=mod_no; INC(mod_no);
  str.print(mod_nm[m],'%s',nm);
  mod[m]:=read_code(nm); c:=mod[m]; p:=ADDRESS(c);
  IF c^.version#303h THEN
    tty.print('Illegal code version: "%s".\n',nm); HALT;
  END;
  chk_time(m);
  tty.print('%-20s: %5d code, %5d global;\n',nm,c^.code_size,c^.glob_size);
  IF c^.glob_size>0 THEN
    mod_glo[m]:=free;
    n:=64+c^.glob_size;
    p^[n]:=CHAR(free*8); INC(n);
    p^[n]:=CHAR(free DIV 20h);
    INC(free);
  ELSE
    mod_glo[m]:=0; n:=64+c^.glob_size; p^[n]:=0c; INC(n); p^[n]:=0c;
  END;
  mod_cod[m]:=free; INC(free);
  i:=64+c^.glob_size+c^.code_size;
  FOR j:=0 TO c^.mglo_no-1 DO
    mg_seg[mg_no]:=free;
    n:=64+word(p,i); INC(i,4);
    p^[n]:=0c; INC(n);
    p^[n]:=0c; INC(n);
    p^[n]:=CHAR(free*8); INC(n);
    p^[n]:=CHAR(free DIV 20h);
    mg_siz[mg_no]:=word(p,i); INC(i,4);
    INC(mg_no); INC(free);
  END;
  j:=64+c^.glob_size+c^.code_size+c^.mglo_no*8+c^.proc_no*4+c^.link_no*2;
  NEW(imp,c^.exts_no);
  FOR i:=0 TO c^.exts_no-1 DO
    imp[i].tim:=word(p,j);
    INC(j,8); k:=0;
    WHILE p^[j]#0c DO imp[i].nm[k]:=p^[j]; INC(j); INC(k) END;
    INC(j); imp[i].nm[k]:=0c;
  END;
  FOR i:=0 TO HIGH(imp) DO read_module(imp[i].nm,imp[i].tim,m) END;
  DISPOSE(imp);
  init[init_no]:=m; INC(init_no);
END read_module;

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
      imp[i].ofs:=word(p,j);
      INC(j,4); k:=0;
      WHILE p^[j]#0c DO imp[i].nm[k]:=p^[j]; INC(j); INC(k) END;
      INC(j); imp[i].nm[k]:=0c;
    END;
    FOR i:=0 TO HIGH(imp) DO
      j:=0;
      WHILE mod_nm[j]#imp[i].nm DO INC(j) END;
      imp[i].no:=j;
      IF imp[i].ofs>=0 THEN
        k:=64+imp[i].ofs;
        p^[k+0]:=CHAR(mod_glo[j]*8);
        p^[k+1]:=CHAR(mod_glo[j] DIV 20h);
      END;
    END;
    j:=64+c^.glob_size+c^.code_size+c^.mglo_no*8+c^.proc_no*4;
    FOR i:=0 TO c^.link_no-1 DO
      k :=64+ORD(p^[j])+ORD(p^[j+1])*256; INC(j,2);
      pn:=ORD(p^[k+0])+ORD(p^[k+1])*100h;
      md:=ORD(p^[k+2])+ORD(p^[k+3])*100h;
      IF md=0 THEN md:=m ELSE md:=imp[md-1].no END;
      pn:=procedure_ip(md,pn);
      md:=mod_cod[md]*8;
      p^[k+0]:=CHAR(pn); p^[k+1]:=CHAR(pn>>8);
      p^[k+2]:=CHAR(md); p^[k+3]:=CHAR(md>>8);
    END;
    DISPOSE(imp);
  END;
END set_mod_refs;

PROCEDURE make_ini_code;
  VAR m,i,md: INTEGER;
BEGIN
  i:=1; bio.write(out,ADR(i),4); chk_io(out_nm);
  i:=1; bio.write(out,ADR(i),4); chk_io(out_nm);
  i:=init_no*5+5; bio.write(out,ADR(i),4); chk_io(out_nm);
  FOR m:=0 TO init_no-1 DO
    md:=init[m];
    i:=9Ah; bio.write(out,ADR(i),1); chk_io(out_nm); -- call far
    i:=procedure_ip(md,0); bio.write(out,ADR(i),2); chk_io(out_nm);
    i:=mod_cod[md]*8; bio.write(out,ADR(i),2); chk_io(out_nm);
  END;
  i:=4C00B8h; bio.write(out,ADR(i),3); chk_io(out_nm); -- mov ax,4C00h
  i:=21CDh; bio.write(out,ADR(i),2); chk_io(out_nm); -- int 21h
END make_ini_code;

PROCEDURE write_modules;
  VAR i,n: INTEGER; c: code_ptr;
BEGIN
  FOR i:=0 TO mod_no-1 DO
    c:=mod[i];
    IF mod_glo[i]>0 THEN
      n:=0;
      bio.write(out,ADR(mod_glo[i]),4); chk_io(out_nm);
      bio.write(out,ADR(n),4); chk_io(out_nm);
      bio.write(out,ADR(c^.glob_size),4); chk_io(out_nm);
      bio.fwrite(out,c,64,c^.glob_size); chk_io(out_nm);
    END;
    IF mod_cod[i]>0 THEN
      n:=1;
      bio.write(out,ADR(mod_cod[i]),4); chk_io(out_nm);
      bio.write(out,ADR(n),4); chk_io(out_nm);
      bio.write(out,ADR(c^.code_size),4); chk_io(out_nm);
      bio.fwrite(out,c,64+c^.glob_size,c^.code_size); chk_io(out_nm);
    END;
  END;
END write_modules;

PROCEDURE write_mg;
  VAR i,n: INTEGER;
BEGIN
  n:=2;
  FOR i:=0 TO mg_no-1 DO
    bio.write(out,ADR(mg_seg[i]),4); chk_io(out_nm);
    bio.write(out,ADR(n),4); chk_io(out_nm);
    bio.write(out,ADR(mg_siz[i]),4); chk_io(out_nm);
  END;
END write_mg;

VAR i: INTEGER;

BEGIN
  mg_no:=0;
  mod_no:=0;
  init_no:=0;
  free:=2;
  IF LEN(arg.words)#1 THEN tty.print('lvme <code file name>\n'); HALT END;
  str.print(out_nm,'%s.vme',arg.words[0]);
  bio.create(out,out_nm,'w',1); chk_io(out_nm);
  read_module(arg.words[0],0,-1);
  ASSERT(init_no=mod_no);
  set_mod_refs;
  make_ini_code;
  write_modules;
  write_mg;
  i:=-1;
  bio.write(out,ADR(i),4); chk_io(out_nm);
  bio.close(out); chk_io(out_nm);
END lvme.
