MODULE link; (* Sem 21-Feb-91. (c) KRONOS *)

IMPORT        SYSTEM;
IMPORT  bio : BIO;
IMPORT  mem : Heap;
IMPORT  cdf : defCode;
IMPORT  cod : defCodes;
IMPORT  tty : Terminal;
IMPORT  key : Keyboard;
IMPORT  str : Strings;
IMPORT  arg : tskArgs;
IMPORT  tim : Time;

WITH STORAGE : mem;

CONST
  main_code = ARRAY OF INTEGER {
  0000000AEh,  000000008h,  0341308C9h,  000153513h,  0B56134B5h,
  089101035h,  0642536B5h,  06039B588h,  0E409143Ah,  01AA2002Ah,
  060292613h,  02638B588h,  070886028h,  014E40914h,  0181FE50Ah,
  088652526h,  03A6039B5h,  02AE40914h,  0181AA200h,  088602926h,
  060282438h,  037890289h,  070602728h,  014E40914h,  01D1FE50Ah,
  088662526h,  03A6039B5h,  02AE40914h,  0151AA200h,  088602926h,
  0E4091438h,  0C8602928h,  0E4091470h,  01FE50A14h,  06725261Ah,
  06039B588h,  0E409143Ah,  01AA2002Ah,  060292614h,  0E83BB588h,
  0A413C8CEh,  0E40914B1h,  01FE50A14h,  000D1CA19h,  0FFFFFFCAh};

VAR
  all    : BOOLEAN;
  bin    : bio.PATHs;
  mod    : ARRAY [0..99] OF cdf.code_ptr;
  mod_pos: ARRAY [0..99] OF INTEGER; -- DFT item offset
  mod_no : INTEGER;
  init   : ARRAY [0..99] OF INTEGER; -- DFT item offset
  init_no: INTEGER;
  mg     : ARRAY [0..999] OF RECORD pos,size: INTEGER END;
  mg_no  : INTEGER;
  ext    : ARRAY [0..99] OF ARRAY [0..79] OF CHAR;
  ext_tim: ARRAY [0..99] OF INTEGER;
  ext_no : INTEGER;
  eref   : ARRAY [0..999] OF INTEGER;
  eref_no: INTEGER;
  rel    : ARRAY [0..999] OF INTEGER;
  rel_no : INTEGER;
  out    : bio.FILE;
  onm    : ARRAY [0..79] OF CHAR;
  main_nm: ARRAY [0..15] OF CHAR;
  mstk   : INTEGER;
  astk   : INTEGER;



PROCEDURE chk_io(VAL nm: ARRAY OF CHAR);
BEGIN
  IF bio.done THEN RETURN END;
  tty.perror(bio.error,'IO error, file "%s", %%s.\n',nm);
  HALT(bio.error);
END chk_io;

PROCEDURE read_code(nm: ARRAY OF CHAR): cdf.code_ptr;
  VAR fnm: ARRAY [0..79] OF CHAR; p: cdf.code_ptr; f: bio.FILE;
BEGIN
  str.print(fnm,'%s.cod',nm);
  bio.lookup(bin,f,fnm,'r'); chk_io(fnm);
  mem.ALLOCATE(p,(bio.eof(f)+3) DIV 4);
  bio.read(f,p,bio.eof(f));  chk_io(fnm);
  bio.close(f);              chk_io(fnm);
  INC(mstk,p^.min_stack);
  INC(astk,p^.add_stack);
  RETURN p;
END read_code;

PROCEDURE write_str(c: cdf.code_ptr);
BEGIN
  bio.write(out,SYSTEM.ADDRESS(c)+cdf.info_size,c^.str_size*4); chk_io(onm);
END write_str;

PROCEDURE find_module(nm: ARRAY OF CHAR; VAR e: BOOLEAN): INTEGER;
  VAR i: INTEGER; p: POINTER TO ARRAY [0..79] OF CHAR;
BEGIN
  FOR i:=0 TO mod_no-1 DO
    p:=SYSTEM.ADDRESS(mod[i])+cdf.info_size;
    IF p^=nm THEN e:=FALSE; RETURN i END;
  END;
  FOR i:=0 TO ext_no-1 DO
    IF ext[i]=nm THEN e:=TRUE; RETURN i END;
  END;
  ASSERT(FALSE);
END find_module;

PROCEDURE write_import(c: cdf.code_ptr);
  VAR p: POINTER TO ARRAY [0..79] OF CHAR; i,j,me: INTEGER; e: BOOLEAN;
BEGIN
  p:=SYSTEM.ADDRESS(c)+cdf.info_size+c^.str_size+c^.code_size+c^.no_mg*2;
  FOR i:=1 TO c^.no_exts-1 DO
    p:=SYSTEM.ADDRESS(p)+1; -- skip compilation time
    me:=find_module(p^,e);
    IF e THEN
      eref[eref_no]:=bio.pos(out) DIV 4; INC(eref_no);
      bio.write(out,SYSTEM.ADR(me),4); chk_io(onm);
    ELSE
      rel[rel_no]:=bio.pos(out) DIV 4; INC(rel_no);
      bio.write(out,SYSTEM.ADR(mod_pos[me]),4); chk_io(onm);
    END;
    j:=0;
    WHILE p^[j]#0c DO INC(j) END;
    REPEAT INC(j) UNTIL j MOD 4 = 0;
    p:=SYSTEM.ADDRESS(p) + j DIV 4;
  END;
END write_import;

PROCEDURE write_glo(c: cdf.code_ptr; f,str: INTEGER);
  VAR i,j: INTEGER; a: SYSTEM.ADDRESS;
BEGIN
  rel[rel_no]:=bio.pos(out) DIV 4; INC(rel_no);
  bio.write(out,SYSTEM.ADR(f),4); chk_io(onm);
  rel[rel_no]:=bio.pos(out) DIV 4; INC(rel_no);
  bio.write(out,SYSTEM.ADR(str),4); chk_io(onm);
  a:=SYSTEM.ADDRESS(c)+cdf.info_size+c^.str_size+c^.code_size;
  FOR i:=0 TO c^.no_mg-1 DO
    mg[mg_no].pos:=bio.pos(out) DIV 4 + INTEGER(a^) - 2; INC(a);
    mg[mg_no].size:=a^; INC(a);
    INC(mg_no);
  END;
  j:=0;
  FOR i:=2 TO c^.glo_size-c^.no_exts-1 DO
    bio.write(out,SYSTEM.ADR(j),4); chk_io(onm);
  END;
END write_glo;

PROCEDURE write_code(c: cdf.code_ptr);
BEGIN
  bio.write(out,SYSTEM.ADDRESS(c)+cdf.info_size+c^.str_size,c^.code_size*4);
  chk_io(onm);
END write_code;

PROCEDURE sure(): BOOLEAN;
  VAR ch: CHAR;
BEGIN
  LOOP
    key.read(ch);
    IF (CAP(ch)='Y') THEN tty.print('%c\n',ch); RETURN TRUE  END;
    IF (CAP(ch)='N') THEN tty.print('%c\n',ch); RETURN FALSE END;
    IF (CAP(ch)='A') THEN tty.print('%c\n',ch); all:=TRUE; RETURN TRUE END
  END
END sure;

PROCEDURE write_module(nm: ARRAY OF CHAR; time: INTEGER);
  VAR
    rd : BOOLEAN;
    p  : POINTER TO ARRAY [0..79] OF CHAR;
    i,j: INTEGER;
    ch : CHAR;
    c  : cdf.code_ptr;
    tst: bio.FILE;
    fnm: ARRAY [0..79] OF CHAR;
    pt : POINTER TO INTEGER;
    imp: DYNARR OF RECORD nm: ARRAY [0..79] OF CHAR; tim: INTEGER END;
   m,ps: INTEGER;
 header: ARRAY [0..15] OF INTEGER;
BEGIN
  FOR i:=0 TO mod_no-1 DO
    p:=SYSTEM.ADDRESS(mod[i])+cdf.info_size;
    IF p^=nm THEN RETURN END;
  END;
  FOR i:=0 TO ext_no-1 DO
    IF ext[i]=nm THEN RETURN END;
  END;
  str.print(fnm,"%s.cod",nm);
  bio.lookup(bin,tst,fnm,'r');
  IF bio.done THEN
    bio.get(tst,header,BYTES(header));
    bio.close(tst);
    c:=SYSTEM.ADR(header);
    IF c^.tag#0 THEN
      rd:=FALSE
    ELSIF arg.flag('-','q') OR (time=0) THEN
      tty.print('%s\n',nm);  rd:=TRUE
    ELSE
      tty.print('%-20s ? ',nm);
      rd:=sure()
    END
  ELSE
    rd:=FALSE
  END;
  IF rd THEN
    m:=mod_no;
    mod[m]:=read_code(nm);
    c:=mod[m];
    INC(mod_no);
    IF time#0 THEN
      IF c^.def_time#time THEN
        tty.print('Time conflict in module "%s".\n',nm); HALT;
      END;
    ELSIF c^.def_time#c^.imp_time THEN
      tty.print('I can link program modules only.\n'); HALT;
    END;
    mod_pos[m]:=bio.pos(out) DIV 4;
    bio.seek(out,(1+c^.str_size+c^.glo_size+c^.code_size)*4,1);
    -- 1 added for DFT item
    pt:=SYSTEM.ADDRESS(c)+cdf.info_size+c^.str_size+c^.code_size+c^.no_mg*2;
    NEW(imp,c^.no_exts-1);
    FOR i:=0 TO c^.no_exts-2 DO
      p:=SYSTEM.ADDRESS(pt)+1; -- skip compilation time
      str.print(imp[i].nm,'%s',p^);
      imp[i].tim:=pt^;
      j:=0;
      WHILE p^[j]#0c DO INC(j) END;
      REPEAT INC(j) UNTIL j MOD 4 = 0;
      pt:=SYSTEM.ADDRESS(p) + j DIV 4;
    END;
    FOR i:=HIGH(imp) TO 0 BY -1 DO write_module(imp[i].nm,imp[i].tim) END;
    DISPOSE(imp);
    ps:=bio.pos(out);
    bio.seek(out,mod_pos[m]*4,0); chk_io(onm);
      -- DFT item
    j:=mod_pos[m]+1+c^.str_size+c^.no_exts;
    rel[rel_no]:=mod_pos[m]; INC(rel_no);
    bio.write(out,SYSTEM.ADR(j),4); chk_io(onm);
      -- string pool
    write_str(mod[m]);
      -- LDFT (import)
    write_import(mod[m]);
      -- LDFT (myself)
    rel[rel_no]:=bio.pos(out) DIV 4; INC(rel_no);
    bio.write(out,SYSTEM.ADR(mod_pos[m]),4); chk_io(onm);
      -- globals
    write_glo(mod[m],mod_pos[m]+1+c^.str_size+c^.glo_size,mod_pos[m]+1);
      -- code
    write_code(mod[m]);
    bio.seek(out,ps,0); chk_io(onm);
    init[init_no]:=m; INC(init_no);
  ELSE
    str.print(ext[ext_no],'%s',nm);
    ext_tim[ext_no]:=time;
    INC(ext_no);
  END;
END write_module;

PROCEDURE write_tables;
  VAR
    ps,r_pos,e_pos,m_pos,g_pos,i: INTEGER;
    p: POINTER TO ARRAY [0..79] OF CHAR;
BEGIN
  IF arg.flag('-','i') THEN tty.print('initialization oder:\n') END;
  FOR i:=0 TO init_no-1 DO
    p:=SYSTEM.ADDRESS(mod[init[i]])+cdf.info_size;
    IF arg.flag('-','i') THEN tty.print('%s\n',p^) END;
    init[i]:=mod_pos[init[i]];
  END;
  r_pos:=bio.pos(out) DIV 4;
  bio.write(out,SYSTEM.ADR(rel_no),4); chk_io(onm);
  bio.write(out,SYSTEM.ADR(rel),rel_no*4); chk_io(onm);
  e_pos:=bio.pos(out) DIV 4;
  bio.write(out,SYSTEM.ADR(eref_no),4); chk_io(onm);
  bio.write(out,SYSTEM.ADR(eref),eref_no*4); chk_io(onm);
  g_pos:=bio.pos(out) DIV 4;
  bio.write(out,SYSTEM.ADR(mg_no),4); chk_io(onm);
  bio.write(out,SYSTEM.ADR(mg),mg_no*8); chk_io(onm);
  m_pos:=bio.pos(out) DIV 4;
  bio.write(out,SYSTEM.ADR(init_no),4); chk_io(onm);
  bio.write(out,SYSTEM.ADR(init),init_no*4); chk_io(onm);
  ps:=bio.pos(out); bio.seek(out,cdf.info_size*4,0); chk_io(onm);
  bio.write(out,SYSTEM.ADR(main_nm),16); chk_io(onm);
  bio.write(out,SYSTEM.ADR(r_pos),4); chk_io(onm);
  bio.write(out,SYSTEM.ADR(e_pos),4); chk_io(onm);
  bio.write(out,SYSTEM.ADR(g_pos),4); chk_io(onm);
  bio.write(out,SYSTEM.ADR(m_pos),4); chk_io(onm);
  bio.seek(out,ps,0); chk_io(onm);
END write_tables;

PROCEDURE write_main_code;
  PROCEDURE adr(VAL s: ARRAY OF INTEGER): SYSTEM.ADDRESS; CODE cod.drop END adr;
BEGIN
  bio.write(out,adr(main_code),SIZE(main_code)*4); chk_io(onm);
END write_main_code;

PROCEDURE write_main_import;
  VAR i,j: INTEGER;
BEGIN
  FOR i:=ext_no-1  TO 0 BY -1 DO
    bio.write(out,SYSTEM.ADR(ext_tim[i]),4); chk_io(onm);
    j:=0;
    WHILE ext[i][j]#0c DO INC(j) END;
    REPEAT ext[i][j]:=0c; INC(j) UNTIL j MOD 4 = 0;
    bio.write(out,SYSTEM.ADR(ext[i]),j); chk_io(onm);
  END;
END write_main_import;

PROCEDURE write_main_info;
  VAR info: cdf.code_rec; ps: INTEGER; i,sz: INTEGER;
BEGIN
  sz:=0;
  FOR i:=0 TO mg_no-1 DO INC(sz,mg[i].size) END;
  info.vers:=BITSET(102h);
  info.def_time:=tim.time();
  info.imp_time:=info.def_time;
  info.str_size:=bio.pos(out) DIV 4 - cdf.info_size;
  info.code_size:=SIZE(main_code);
  info.min_stack:=mstk+sz;
  info.add_stack:=astk;
  info.glo_size:=ext_no+1+2;
  info.no_exts:=ext_no+1;
  info.no_proc:=2;
  info.no_mg:=0;
  info.size:=0;
  info.language:='mx';
  info.tag:=0;
  info.usercode:={};
  info.unused__F:=0;
  ps:=bio.pos(out); bio.seek(out,0,0);      chk_io(onm);
  bio.write(out,SYSTEM.ADR(info),cdf.info_size*4); chk_io(onm);
  bio.seek(out,ps,0);                       chk_io(onm)
END write_main_info;

PROCEDURE help;
BEGIN
  tty.print('link [-aiq] <program_name> <root_module_name>\n')
END help;

VAR i: INTEGER; s: STRING;  ss: ARRAY [0..255] OF CHAR;

PROCEDURE extention(VAR fname: ARRAY OF CHAR; VAL ext: ARRAY OF CHAR);
  VAR i,j,k: INTEGER;
BEGIN
  i:=str.len(fname);
  j:=str.len(ext);
  IF i>j THEN
    k:=0; DEC(i,j);
    WHILE (j>0) & (fname[i]=ext[k]) DO DEC(j); INC(i); INC(k) END;
    IF j=0 THEN RETURN END
  END;
  str.append(fname,ext)
END extention;

BEGIN
  all:=FALSE;
  IF HIGH(arg.words)<1 THEN help; HALT END;
  mod_no:=0;
  mg_no:=0;
  ext_no:=0;
  eref_no:=0;
  rel_no:=0;
  init_no:=0;
  IF arg.flag('-','a') THEN
    bio.get_paths(bin,'BIN');    chk_io('BIN');
  ELSE
    bin:=bio.here
  END;

  str.copy(onm,arg.words[0]);
  extention(onm,".cod");
  str.sub_str(main_nm,onm,0,str.len(onm)-4);
  mstk:=0;
  astk:=0;
  bio.create(out,onm,'rw',0); chk_io(onm);
  bio.buffers(out,4,4096); chk_io(onm);
  bio.seek(out,24*4,0);
  write_module(arg.words[1],0);
  write_tables;
  write_main_info;
  write_main_code;
  write_main_import;
  tty.print('"%s" %dK,%03d bytes\n',main_nm,bio.eof(out) DIV 1000
                                           ,bio.eof(out) MOD 1000);
  bio.close(out); chk_io(onm)
END link.
