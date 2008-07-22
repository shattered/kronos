IMPLEMENTATION MODULE visCode; (* Ned 07-Oct-89. (c) KRONOS *)

IMPORT  sys: SYSTEM;
IMPORT  def: defCode;
IMPORT  mcd: defCodes;
IMPORT  err: defErrors;
IMPORT  str: Strings;
IMPORT  mem: Heap;

(*$U+*)

WITH STORAGE (NEW    : mem.allocate;
              DISPOSE: mem.deallocate;
              RESIZE : mem.reallocate);

PROCEDURE externals(ofs,lim: sys.ADDRESS; exts: EXTs);
  VAR i,n: INTEGER; p: POINTER TO ARRAY [0..31] OF CHAR;
BEGIN
  FOR i:=0 TO HIGH(exts) DO exts[i]:=NIL END;
  IF HIGH(exts)<1 THEN RETURN END;
  n:=HIGH(exts);
  exts[n]:=ofs;
  p:=ofs+1;
  DEC(n);
  WHILE n>0 DO
    i:=0;
    WHILE (p^[i]#0c) DO INC(i) END;
    INC(ofs,i DIV 4+2);
    ASSERT(ofs+1<lim);
    exts[n]:=ofs;
    p:=ofs+1;
    DEC(n);
  END;
END externals;

PROCEDURE connect(VAR c: code_ptr; VAR base: ARRAY OF sys.WORD);
  VAR a: def.code_ptr; ofs: sys.ADDRESS;
BEGIN
  done:=FALSE;
  a:=sys.ADR(base);
  IF a^.vers*def.vers_mask#def.cur_vers THEN
    error:=err.ill_vers; RETURN
  END;
  NEW(c);
  IF c=NIL THEN error:=err.no_memory; RETURN END;
  c^.vers    :=INTEGER(a^.vers*def.vers_mask);
  c^.def_time:=a^.def_time;
  c^.imp_time:=a^.imp_time;
  c^.no_glo  :=a^.glo_size-a^.no_exts;
  c^.tag     :=a^.tag;
  c^.min_stack:=a^.min_stack;
  c^.add_stack:=a^.add_stack;

  c^.compiler^.ADR :=sys.ADR(a^.language);
  c^.compiler^.HIGH:=HIGH(a^.language);

  ofs:=sys.ADR(base)+16;
  c^.structs^.ADR :=ofs;           c^.strings^.ADR:=ofs;
  c^.structs^.HIGH:=a^.str_size-1; c^.strings^.HIGH:=a^.str_size*4-1;

  c^.name^:=c^.strings^;
  c^.name^.HIGH:=str.len(c^.name)-1;

  ofs:=ofs+a^.str_size;
  c^.proc_tab^.ADR :=ofs;
  c^.proc_tab^.HIGH:=a^.no_proc-1;

  c^.code^.ADR :=ofs;
  c^.code^.HIGH:=a^.code_size*4-1;

  ofs:=ofs+a^.code_size;
  c^.multi_glo^.ADR :=ofs;
  c^.multi_glo^.HIGH:=a^.no_mg-1;

  ofs:=ofs+a^.no_mg*SIZE(GLOBAL);

  NEW(c^.exts,a^.no_exts);
  IF c^.exts^.ADR=NIL THEN DISPOSE(c); error:=err.no_memory; RETURN END;
  externals(ofs,sys.ADR(base)+SIZE(base),c^.exts);
  done:=TRUE; --++ vik 16.04.90
END connect;

PROCEDURE disconnect(VAR c: code_ptr);
BEGIN ASSERT(c#NIL);
  DISPOSE(c^.exts);
  DISPOSE(c);
END disconnect;

-------------------------  COMMANDs  ---------------------------
                         ------------


VAR cmd_mnem: ARRAY [0..256*6-1] OF CHAR;

CONST longs = ARRAY OF CHAR {
  CHAR(mcd.lib),  1c,  CHAR(mcd.lid),   2c,  CHAR(mcd.liw),  4c,
  CHAR(mcd.lla),  1c,  CHAR(mcd.lga),   1c,  CHAR(mcd.lsa),  1c,
  CHAR(mcd.llw),  1c,  CHAR(mcd.lgw),   1c,  CHAR(mcd.lsw),  1c,
  CHAR(mcd.slw),  1c,  CHAR(mcd.sgw),   1c,  CHAR(mcd.ssw),  1c,
  CHAR(mcd.lea),  2c,  CHAR(mcd.lew),   2c,  CHAR(mcd.sew),  2c,
  CHAR(mcd.jflc), 2c,  CHAR(mcd.jfl),   2c,  CHAR(mcd.jfsc), 1c,
  CHAR(mcd.jfs),  1c,  CHAR(mcd.jblc),  2c,  CHAR(mcd.jbl),  2c,
  CHAR(mcd.jbsc), 1c,  CHAR(mcd.jbs),   1c,
  CHAR(mcd.ffct), 1c,  CHAR(mcd.cpcop), 1c,  CHAR(mcd.pcop), 1c,
  CHAR(mcd.for1), 3c,  CHAR(mcd.for2),  3c,  CHAR(mcd.entc), 2c,
  CHAR(mcd.orjp), 1c,  CHAR(mcd.andjp), 1c,  CHAR(mcd.lpc),  2c,
  CHAR(mcd.lsta), 2c,  CHAR(mcd.gb),    1c,  CHAR(mcd.entr), 1c,
  CHAR(mcd.quot), 1c,
  CHAR(mcd.cx),   2c,  CHAR(mcd.ci),    1c,  CHAR(mcd.cl),   1c,
  CHAR(mcd.lpa),  1c,  CHAR(mcd.lpw),   1c,  CHAR(mcd.spw),  1c };

PROCEDURE fill_cmds;

  VAR m: INTEGER;

  PROCEDURE app(VAL s: ARRAY OF CHAR);
    VAR i: INTEGER;
  BEGIN i:=0;
    WHILE (s[i]#0c) DO cmd_mnem[m]:=s[i]; INC(m); INC(i) END;
  END app;

  VAR i: INTEGER;

BEGIN
  FOR i:=0 TO 255 DO cmd_len[i]:=0c END;
  FOR i:=0 TO HIGH(longs) BY 2 DO cmd_len[ORD(longs[i])]:=longs[i+1] END;
  m:=0;
  app("LI0   LI1   LI2   LI3   LI4   LI5   LI6   LI7   ");
  app("LI8   LI9   LI0A  LI0B  LI0C  LI0D  LI0E  LI0F  ");
  app("LIB   LID   LIW   LIN   LLA   LGA   LSA   LEA   ");
  app("JFLC  JFL   JFSC  JFS   JBLC  JBL   JBSC  JBS   ");
  app("LLW   LGW   LEW   LSW   LLW4  LLW5  LLW6  LLW7  ");
  app("LLW8  LLW9  LLW0A LLW0B LLW0C LLW0D LLW0E LLW0F ");
  app("SLW   SGW   SEW   SSW   SLW4  SLW5  SLW6  SLW7  ");
  app("SLW8  SLW9  SLW0A SLW0B SLW0C SLW0D SLW0E SLW0F ");

  app("LXB   LXW   LGW2  LGW3  LGW4  LGW5  LGW6  LGW7  ");
  app("LGW8  LGW9  LGW0A LGW0B LGW0C LGW0D LGW0E LGW0F ");
  app("SXB   SXW   SGW2  SGW3  SGW4  SGW5  SGW6  SGW7  ");
  app("SGW8  SGW9  SGW0A SGW0B SGW0C SGW0D SGW0E SGW0F ");
  app("LSW0  LSW1  LSW2  LSW3  LSW4  LSW5  LSW6  LSW7  ");
  app("LSW8  LSW9  LSW0A LSW0B LSW0C LSW0D LSW0E LSW0F ");
  app("SSW0  SSW1  SSW2  SSW3  SSW4  SSW5  SSW6  SSW7  ");
  app("SSW8  SSW9  SSW0A SSW0B SSW0C SSW0D SSW0E SSW0F ");

  app("RESET QUIT  GETM  SETM  TRAP  TRA   TR    IDLE  ");
  app("ADD   SUB   MUL   DIV   SHL   SHR   ROL   ROR   ");
  app("INP   OUT   *92*  TRB   *94*  *95*  *96*  *97*  ");
  app("FADD  FSUB  FMUL  FDIV  FCMP  FABS  FNEG  FFCT  ");
  app("LSS   LEQ   GTR   GEQ   EQU   NEQ   ABS   NEG   ");
  app("OR    AND   XOR   BIC   IN    BIT   NOT   MOD   ");
  app("DECS  DROP  LODFV STORE STOFV COPT  CPCOP PCOP  ");
  app("FOR1  FOR2  ENTC  XIT   ADDPC JUMP  ORJP  ANDJP ");

  app("MOVE  CHKNILLSTA  COMP  GB    GB1   CHK   CHKZ  ");
  app("ALLOC ENTR  RTN   NOP   CX    CI    CF    CL    ");
  app("CL0   CL1   CL2   CL3   CL4   CL5   CL6   CL7   ");
  app("CL8   CL9   CL0A  CL0B  CL0C  CL0D  CL0E  CL0F  ");
  app("INCL  EXCL  INL   QUOT  INC1  DEC1  INC   DEC   ");
  app("STOT  LODT  LXA   LPC   BBU   BBP   BBLT  PDX   ");
  app("SWAP  LPA   LPW   SPW   SSWU  RCHK  RCHKZ CM    ");
  app("*F8*  BMG   ACTIV USR   SYS   NII   DOT   INVLD ");

END fill_cmds;

PROCEDURE vis_command(n: INTEGER; VAR s: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN n:=n*6;
  FOR i:=0 TO 5 DO s[i]:=cmd_mnem[n]; INC(n) END;
  s[6]:=0c;
END vis_command;

BEGIN fill_cmds;
  done:=TRUE; error:=0;
END visCode.
