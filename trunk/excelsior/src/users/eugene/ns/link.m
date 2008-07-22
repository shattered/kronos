MODULE link; (* 17-Oct-90. (c) KRONOS *)

FROM SYSTEM     IMPORT  WORD, ADDRESS, ADR;

IMPORT bio : BIO;
IMPORT tty : Terminal;
IMPORT arg : tskArgs;
IMPORT str : Strings;
IMPORT mem : pcSystem;
IMPORT put : nsCmd;

FROM nsSym       IMPORT access, adr_mode, index_mode;
FROM nsCmd       IMPORT cpu_reg;

WITH STORAGE : mem;

CONST
  base_adr = 1000h;

VAR
  info  : POINTER TO RECORD
    version  : INTEGER;   -- версия кодофайла
    def_time : INTEGER;   -- время компиляции definition
    imp_time : INTEGER;   -- время компиляции implementation
    glob_size: INTEGER;   -- размер глобалов
    code_size: INTEGER;   -- размер кода
    min_stack: INTEGER;   -- мин. стек для глоб. мультизначений
    add_stack: INTEGER;   -- дополнительный стек
    reserv_07: INTEGER;
    reserv_08: INTEGER;
    reserv_09: INTEGER;
    reserv_0A: INTEGER;
    reserv_0B: INTEGER;
    reserv_0C: INTEGER;
    reserv_0D: INTEGER;
    reserv_0E: INTEGER;
    reserv_0F: INTEGER;
  END;
  eof  : INTEGER;
  TOS  : access;
  IMM  : access;
  REG  : access;

PROCEDURE ini;
BEGIN
  WITH IMM DO am:=am_imm; xm:=xm_off; rg:=0; rg_x:=0; disp:=0; level:=0 END;
  WITH REG DO am:=am_RG; xm:=xm_off; rg_x:=0; n:=0; disp:=0; level:=0 END;
  WITH TOS DO am:=am_TOS; xm:=xm_off; rg:=0; rg_x:=0; disp:=0; level:=0 END;
END ini;

PROCEDURE linker;
  VAR i,sb,mod,c_sz,g_sz: INTEGER;
BEGIN
  info:=ADR(put.code);
  c_sz:=info^.code_size;
  g_sz:=info^.glob_size;
  put.cnt:=0;
  put.spr(TOS,MD,4);
  mod:=base_adr+BYTES(info^)-8;
  sb :=base_adr+BYTES(info^);
  IMM.n:=mod; put.lpr(IMM,MD,4);
  IMM.n:=sb;  put.lpr(IMM,SB,4);
  i:=BYTES(info^)+g_sz-put.cnt; put.put({1}); put.disp(i);
  put.put({1,4}); put.put(0);
  WHILE put.cnt<BYTES(info^)-8 DO put.put(0) END;
  put.put2(sb); put.put2(sb>>16);
  put.put2(0); put.put2(0);
  eof:=BYTES(info^)+g_sz+c_sz;
END linker;

VAR
  w  : ARRAY [0..255] OF CHAR;
  nm : ARRAY [0..255] OF CHAR;
  inp: bio.FILE;

PROCEDURE io_chk;
BEGIN
  IF NOT bio.done THEN
    tty.perror(bio.error,'"%s": %%s.\n',nm); HALT(1)
  END;
END io_chk;

BEGIN
  IF HIGH(arg.words)<0 THEN HALT END;
  str.copy(w,arg.words[0]);
  str.print(nm,'%s.obj',w);
  bio.open(inp,nm,'r'); io_chk;
  eof:=bio.eof(inp);
  NEW(put.code,eof);
  bio.read(inp,ADR(put.code),eof); io_chk;
  bio.close(inp); io_chk;
  ini;
  linker;
  str.print(nm,'%s.exe',w);
  bio.create(inp,nm,'w',eof); io_chk;
  bio.write(inp,ADR(put.code),eof); io_chk;
  bio.close(inp); io_chk;
END link.
