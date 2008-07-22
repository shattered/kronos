IMPLEMENTATION MODULE inExp;

IMPORT pcK,inCmd,pcS,pcM;

FROM inCmd IMPORT adr_mode;

CONST
  cmd ::= inCmd;
  pc  ::= pcK;

TYPE
  byte     = SET OF [0..07];
  word     = SET OF [0..15];
  dword    = SET OF [0..31];
  am_set   = SET OF cmd.adr_mode;

CONST
  ovr_check = TRUE;
  on_stack  = am_set{cmd.am_STK,cmd.am_aSTK};
  am_const  = am_set{cmd.am_imm,cmd.am_Gimm,cmd.am_Limm};
  sgn_types = pc.Forms{pc.shortint,pc.integer,pc.longint};

VAR
  const  : cmd.access;

PROCEDURE error(n: INTEGER);
BEGIN
  pcS.err(n); pcM.abort;
END error;

PROCEDURE sign(t: pc.STRUCT): BOOLEAN;
BEGIN
  IF t^.mode=pc.range THEN t:=t^.base END;
  RETURN t^.mode IN sgn_types;
END sign;

PROCEDURE pos(txt_pos: LONGINT): CARDINAL;
BEGIN
  RETURN CARDINAL(txt_pos>>16);
END pos;

PROCEDURE imm(VAR a: cmd.access);
BEGIN
  IF a.am IN am_const THEN RETURN END;
  error(87);
END imm;

PROCEDURE min(type: pc.STRUCT; VAR a: cmd.access);
BEGIN
  a:=const;
  CASE type^.mode OF
    |pc.invtype,pc.undef : a.n:=0;
    |pc.shortint         : a.n:=-128;
    |pc.shortcard        : a.n:=0;
    |pc.char,pc.byte     : a.n:=0;
    |pc.boolean          : a.n:=0;
    |pc.integer          : a.n:=-8000H;
    |pc.cardinal,pc.word : a.n:=0;
    |pc.longint          : a.n:=MIN(LONGINT);
    |pc.range,pc.enum    : a.n:=type^.n;
  ELSE
    IF type^.size<4 THEN a.n:=0 ELSE a.n:=MIN(LONGINT) END;
  END;
END min;

PROCEDURE max(type: pc.STRUCT; VAR a: cmd.access);
BEGIN
  a:=const;
  CASE type^.mode OF
    |pc.invtype,pc.undef : a.n:=1;
    |pc.shortint         : a.n:=127;
    |pc.shortcard        : a.n:=255;
    |pc.char,pc.byte     : a.n:=255;
    |pc.boolean          : a.n:=1;
    |pc.integer          : a.n:=7FFFH;
    |pc.cardinal,pc.word : a.n:=0FFFFH;
    |pc.longint          : a.n:=7FFFFFFFH;
    |pc.range,pc.enum    : a.n:=type^.m;
  ELSE
    IF type^.size=1 THEN a.n:=0FFH;
    ELSIF type^.size=2 THEN a.n:=0FFFFH;
    ELSE a.n:=MAX(LONGINT)
    END;
  END;
END max;

PROCEDURE min_max(type: pc.STRUCT; VAR fr,to: LONGINT);
BEGIN
  CASE type^.mode OF
    |pc.invtype,pc.undef : fr:=0; to:=1;
    |pc.shortint         : fr:=-128; to:=127;
    |pc.shortcard        : fr:=0; to:=255;
    |pc.char,pc.byte     : fr:=0; to:=255;
    |pc.boolean          : fr:=0; to:=1;
    |pc.integer          : fr:=MIN(INTEGER); to:=MAX(INTEGER);
    |pc.cardinal,pc.word : fr:=0; to:=0FFFFH;
    |pc.longint          : fr:=MIN(LONGINT); to:=MAX(LONGINT);
    |pc.range,pc.enum    : fr:=type^.n; to:=type^.m;
  ELSE
    IF type^.size=1 THEN fr:=0; to:=0FFH;
    ELSIF type^.size=2 THEN fr:=0; to:=0FFFFH;
    ELSE fr:=MIN(LONGINT); to:=MAX(LONGINT);
    END;
  END;
END min_max;

PROCEDURE low_s(o: pc.STRUCT; VAR a: cmd.access);
BEGIN
  IF o^.mode=pc.array THEN min(o^.inx,a); RETURN END;
  IF NOT (o^.mode IN pc.ARRs) THEN error(0) END;
  a:=const; a.n:=0;
END low_s;

PROCEDURE low(o: pc.NODE; VAR a: cmd.access);
BEGIN
  low_s(o^.type,a);
END low;

PROCEDURE add_offset(VAR a: cmd.access; offset: LONGINT);
BEGIN
  CASE a.am OF
    |am_imm  : error(0);
    |am_abs  : a.disp:=a.disp+offset;
    |am_aSTK : a.disp:=a.disp+offset;
    |am_G    : a.disp:=a.disp+offset;
    |am_L    : a.disp:=a.disp+offset;
    |am_Gimm : a.disp:=a.disp+offset; a.am:=am_G;
    |am_Limm : a.disp:=a.disp+offset; a.am:=am_L;
    |am_aG   : a.disp:=a.disp+offset;
    |am_aL   : a.disp:=a.disp+offset;
  ELSE error(0);
  END;
END add_offset;

PROCEDURE high_s(o: pc.STRUCT; VAR a: cmd.access);
BEGIN
  IF o^.mode=pc.array THEN max(o^.inx,a); RETURN END;
  IF o^.mode#pc.vector THEN error(233) END;
  a:=const; a.n:=o^.n-1;
END high_s;

PROCEDURE high(l: pc.NODE; VAR a: cmd.access);
BEGIN
  IF l^.type^.mode IN pc.Forms{pc.dynarr,pc.array_of} THEN
    designator(l,a); add_offset(a,4);
  ELSE
    high_s(l^.type,a);
  END;
END high;

PROCEDURE len(u: pc.NODE; VAR a: cmd.access);
  VAR to: cmd.access;
BEGIN
  IF u^.type^.mode IN pc.Forms{pc.dynarr,pc.array_of} THEN
    designator(u,a); add_offset(a,4); cmd.load(a,4);
    const.n:=1; cmd.cmd_stk_m(cmd.c_add,const); a.am:=am_STK;
  ELSE
    low_s(u^.type,a); high_s(u^.type,to);
    IF a.am#cmd.am_imm THEN error(233) END;
    a.n:=to.n-a.n+1;
  END;
END len;

PROCEDURE adr(u: pc.NODE; VAR a: cmd.access);
BEGIN
  designator(u,a);
  CASE a.am OF
    |am_imm   : error(0);
    |am_abs   : a.am:=am_imm; a.n:=a.disp;
    |am_aSTK  : cmd.load_adr(a); a.am:=am_STK;
    |am_G     : cmd.load_adr(a); a.am:=am_STK;
    |am_L     : cmd.load_adr(a); a.am:=am_STK;
    |am_Gimm  : cmd.load_adr(a); a.am:=am_STK;
    |am_Limm  : cmd.load_adr(a); a.am:=am_STK;
    |am_aG    : IF a.disp=0 THEN a.am:=am_G; a.disp:=a.n;
                ELSE cmd.load_adr(a); a.am:=am_STK;
                END;
    |am_aL    : IF a.disp=0 THEN a.am:=am_L; a.disp:=a.n;
                ELSE cmd.load_adr(a); a.am:=am_STK;
                END;
  ELSE error(233);
  END;
END adr;

PROCEDURE adr_u(u: pc.NODE; VAR a: cmd.access);
BEGIN
  IF u^.type^.mode IN pc.Forms{pc.dynarr,pc.array_of} THEN designator(u,a);
  ELSE adr(u,a);
  END;
END adr_u;

PROCEDURE bytes_u(u: pc.NODE; VAR a: cmd.access);
BEGIN
  IF u^.type^.mode IN pc.Forms{pc.dynarr,pc.array_of} THEN
    designator(u,a); add_offset(a,4);
    IF u^.type^.base^.size#1 THEN
      cmd.load(a,4); const.n:=u^.type^.base^.size;
      cmd.imul(const); a.am:=cmd.am_STK;
    END;
  ELSE
    a:=const; a.n:=u^.type^.size;
  END;
END bytes_u;

PROCEDURE min_size(min,max: LONGINT; VAR sz: CARDINAL; VAR sg: BOOLEAN);
BEGIN
  sg:=min<0;
  IF sg THEN
    IF (min>=-128) & (max<=127) THEN sz:=1;
    ELSIF (min>=MIN(INTEGER)) & (max<=MAX(INTEGER)) THEN sz:=2
    ELSE sz:=4
    END;
  ELSIF max<=0FFH THEN sz:=1
  ELSIF max<=0FFFFH THEN sz:=2
  ELSE sz:=4
  END;
END min_size;

PROCEDURE rcheck_cmd(fr_min,fr_max,to_min,to_max: LONGINT);
  VAR sz,sv: CARDINAL; sg: BOOLEAN;
BEGIN
  min_size(fr_min,fr_max,sz,sg);
  IF (to_min>fr_max) OR (to_max<fr_min) THEN error(0); (* range check *) END;
  IF (sz=4) & (to_min>=0) & (to_max<=0FFFFH) THEN
    IF cmd.pos[cmd.stk-1]=cmd.da THEN
      cmd.b(cmd.and_wrm); cmd.b(cmd.md_reg+cmd.DX*8+cmd.DX);
    ELSE
      cmd.b(cmd.and_wrm); cmd.b(cmd.md_reg+cmd.BX*8+cmd.BX);
    END;
    cmd.b(cmd.je); cmd.b(3); cmd.b(cmd.int); cmd.b(4); cmd.b(cmd.nop);
    sz:=2; sg:=FALSE;
    IF fr_min<0 THEN fr_min:=0 END;
    IF fr_max>0FFFFH THEN fr_max:=0FFFFH END;
  END;
  sv:=cmd.siz[cmd.stk-1]; cmd.siz[cmd.stk-1]:=sz;
  IF sz>sv THEN error(233) END;
  IF to_min>fr_min THEN
    const.n:=to_min;
    IF sz=4 THEN
      cmd.copt; cmd.cmd_stk_m(cmd.c_sub,const); cmd.drop;
    ELSE
      cmd.cmd_stk_m(cmd.c_cmp,const);
    END;
    IF sg THEN cmd.b(cmd.jge) ELSE cmd.b(cmd.jnb) END;
    cmd.b(3); cmd.b(cmd.int); cmd.b(4); cmd.b(cmd.nop);
  END;
  IF to_max<fr_max THEN
    IF sz=4 THEN
      const.n:=to_max+1; cmd.copt; cmd.cmd_stk_m(cmd.c_sub,const); cmd.drop;
      IF sg THEN cmd.b(cmd.jl) ELSE cmd.b(cmd.jb) END;
    ELSE
      const.n:=to_max; cmd.cmd_stk_m(cmd.c_cmp,const);
      IF sg THEN cmd.b(cmd.jle) ELSE cmd.b(cmd.jbe) END;
    END;
    cmd.b(3); cmd.b(cmd.int); cmd.b(4); cmd.b(cmd.nop);
  END;
  cmd.siz[cmd.stk-1]:=sv;
END rcheck_cmd;

PROCEDURE range_check(n: pc.NODE; VAR a: cmd.access;
                      to_t: pc.STRUCT; chk: BOOLEAN);
  VAR
    fr_sg: BOOLEAN;
    fr_min,fr_max,to_min,to_max: LONGINT;
    fr_sz: CARDINAL; fr_t: pc.STRUCT;
BEGIN
  min_max(to_t,to_min,to_max);
  WHILE
    (n^.mode=pc.unary) & (n^.sub=pc.rcheck) OR
    (n^.mode=pc.unary) & (n^.sub=pc.conv) &
    (n^.l^.type^.mode IN pc.SCALARs) & (n^.type^.mode IN pc.SCALARs) DO
    min_max(n^.type,fr_min,fr_max);
    IF fr_min>to_min THEN to_min:=fr_min END;
    IF fr_max<to_max THEN to_max:=fr_max END;
    n:=n^.l;
  END;
  expression(n,a); fr_t:=n^.type;
  IF a.am IN am_const THEN
    IF (a.n<to_min) OR (a.n>to_max) THEN error(0) END;
    RETURN;
  END;
  fr_sg:=sign(fr_t); fr_sz:=CARDINAL(fr_t^.size);
  min_max(fr_t,fr_min,fr_max);
  IF (to_min>fr_max) OR (to_max<fr_min) THEN error(0); (* range check *) END;
  IF chk & ((to_min>fr_min) OR (to_max<fr_max)) THEN
    cmd.load(a,fr_sz); a.am:=cmd.am_STK;
    rcheck_cmd(fr_min,fr_max,to_min,to_max);
  END;
  IF (a.am=cmd.am_STK) OR (fr_t^.size<to_t^.size) THEN
    cmd.load(a,fr_sz); a.am:=cmd.am_STK;
    cmd.set_size(CARDINAL(to_t^.size),fr_sg);
  END;
END range_check;

PROCEDURE bounds_check(n: pc.NODE; VAR a: cmd.access;
                       to_t: pc.STRUCT; chk: BOOLEAN; VAR v_sz: CARDINAL);
  VAR
    fr_min,fr_max,to_min,to_max,base,m: LONGINT;
    fr_sz: CARDINAL;
    fr_sg: BOOLEAN;
    i: CARDINAL; fr_t: pc.STRUCT;
BEGIN
  CASE to_t^.mode OF
    |pc.array : min_max(to_t^.inx,to_min,to_max);
    |pc.vector: to_min:=0; to_max:=to_t^.n-1;
    |pc.set   : min_max(to_t^.base,to_min,to_max);
    |pc.bitset: to_min:=0; to_max:=15;
  ELSE error(233);
  END;
  base:=to_min;
  WHILE
    (n^.mode=pc.unary) & (n^.sub=pc.rcheck) OR
    (n^.mode=pc.unary) & (n^.sub=pc.conv) &
    (n^.l^.type^.mode IN pc.SCALARs) & (n^.type^.mode IN pc.SCALARs) DO
    min_max(n^.type,fr_min,fr_max);
    IF fr_min>to_min THEN to_min:=fr_min END;
    IF fr_max<to_max THEN to_max:=fr_max END;
    n:=n^.l;
  END;
  expression(n,a); fr_t:=n^.type;
  min_max(fr_t,fr_min,fr_max);
  IF a.am IN am_const THEN
    IF (a.n<to_min) OR (a.n>to_max) THEN a.n:=base; error(0) END;
    a.n:=a.n-base; v_sz:=4; RETURN;
  END;
  v_sz:=CARDINAL(fr_t^.size);
  IF a.am#cmd.am_STK THEN cmd.load(a,v_sz); a.am:=cmd.am_STK END;
  IF chk THEN rcheck_cmd(fr_min,fr_max,to_min,to_max) END;
  IF to_max>fr_max THEN m:=fr_max-base ELSE m:=to_max-base END;
  IF fr_sg THEN
    IF m<=7FH THEN i:=1;
    ELSIF m<=7FFFH THEN i:=2;
    ELSE i:=4;
    END;
  ELSIF m<=0FFH THEN i:=1
  ELSIF m<=0FFFFH THEN i:=2;
  ELSE i:=4;
  END;
  IF v_sz<i THEN cmd.set_size(i,fr_sg); v_sz:=i END;
  IF base#0 THEN const.n:=base; cmd.cmd_stk_m(cmd.c_sub,const) END;
END bounds_check;

PROCEDURE index_mul(a_sz,v_sz,r_sz: CARDINAL; imm: LONGINT);
  (* a_sz  significant size *)
  (* v_sz  valid size       *)
  (* r_sz  result size      *)
  VAR ax,dx: CARDINAL;
BEGIN
  IF (imm=2) OR (imm=4) OR (imm=8) THEN
    IF cmd.pos[cmd.stk-1]=cmd.da THEN ax:=cmd.AX; dx:=cmd.DX;
    ELSE ax:=cmd.CX; dx:=cmd.BX;
    END;
    IF (v_sz<r_sz) & (v_sz=1) THEN cmd.b(cmd.mov_ahi+ax); cmd.b(0); v_sz:=2 END;
    IF v_sz<r_sz THEN cmd.b(cmd.mov_axi+dx); cmd.w(0); v_sz:=4 END;
    REPEAT
      CASE r_sz OF
        |1: cmd.b(cmd.shift_bm1); cmd.b(cmd.md_reg+ax+cmd.s_shl);
        |2: cmd.b(cmd.shift_wm1); cmd.b(cmd.md_reg+ax+cmd.s_shl);
        |4: cmd.b(cmd.shift_wm1); cmd.b(cmd.md_reg+ax+cmd.s_shl);
            cmd.b(cmd.shift_wm1); cmd.b(cmd.md_reg+dx+cmd.s_rcl);
      ELSE error(0);
      END;
      imm:=imm>>1;
    UNTIL imm=1;
  ELSIF imm#1 THEN
    IF (a_sz=4) OR (imm>=10000H) THEN
      const.n:=imm; cmd.load(const,4); cmd.call_rts(5);
    ELSIF (a_sz=2) OR (imm>=100H) THEN
      const.n:=imm; cmd.load(const,2); cmd.pop_reg(2);
      cmd.b(cmd.grp1_wm); cmd.b(cmd.md_reg+cmd.CX+cmd.g1_mul);
    ELSE
      const.n:=imm; cmd.load(const,1); cmd.pop_reg(2);
      cmd.b(cmd.grp1_bm); cmd.b(cmd.md_reg+cmd.CL+cmd.g1_mul);
    END;
    DEC(cmd.stk); cmd.pos[cmd.stk-1]:=cmd.da;
  END;
  cmd.siz[cmd.stk-1]:=r_sz;
END index_mul;

PROCEDURE index_dynarr(n: pc.NODE; VAR a: cmd.access);
  VAR desc,desc1,desc2: cmd.access; i,dx: CARDINAL;
BEGIN
  IF n^.sub#pc.rcheck THEN
    i:=CARDINAL(n^.r^.type^.size);
    expression(n^.r,a); cmd.load(a,i);
    index_mul(i,i,4,n^.l^.type^.base^.size);
    designator_u(n^.l,a); cmd.load(a,4);
    cmd.swap; cmd.add_adr(cmd.top,4,sign(n^.r^.type));
    a.am:=cmd.am_aSTK; a.disp:=0;
  ELSE
    designator(n^.l,desc);
    IF desc.am=cmd.am_aSTK THEN
      cmd.alloc_tmp_var(desc1,4); cmd.store(desc1);
      desc1.n:=desc1.disp; desc1.disp:=0;
      IF a.am=cmd.am_L THEN desc1.am:=cmd.am_aL;
      ELSIF a.am=cmd.am_G THEN desc1.am:=cmd.am_aG;
      ELSE error(233);
      END;
    ELSE
      desc1:=desc;
    END;
    desc2:=desc1; add_offset(desc2,4);
    expression(n^.r,a); cmd.load(a,CARDINAL(n^.r^.type^.size));
    cmd.set_size(4,sign(n^.r^.type)); cmd.pop_reg(1);
    IF cmd.pos[cmd.stk-1]=cmd.da THEN dx:=cmd.DX ELSE dx:=cmd.BX END;
    cmd.b(cmd.and_wrm); cmd.b(cmd.md_reg+dx+dx*8);
    cmd.b(cmd.jl); cmd.b(0); i:=cmd.cnt;
    cmd.copt; cmd.load(desc2,4);
    cmd.swap; cmd.cmd_stk_m(cmd.c_sub,cmd.top); cmd.drop;
    cmd.b(cmd.jge); cmd.b(3);
    cmd.code[i-1]:=byte(cmd.cnt-i);
    cmd.b(cmd.int); cmd.b(4); cmd.b(cmd.nop);
    index_mul(4,4,4,n^.l^.type^.base^.size);
    cmd.load(desc1,4); cmd.swap;
    cmd.add_adr(cmd.top,4,sign(n^.r^.type));
    a.am:=cmd.am_aSTK; a.disp:=0;
  END;
END index_dynarr;

PROCEDURE index(i: pc.NODE; VAR a: cmd.access);
  VAR r_sz,v_sz: CARDINAL; e: cmd.access;
BEGIN
  IF i^.l^.type^.mode IN pc.Forms{pc.dynarr,pc.array_of} THEN
    index_dynarr(i,a);
  ELSE
    bounds_check(i^.r,e,i^.l^.type,i^.sub=pc.rcheck,v_sz);
    IF e.am IN am_const THEN
      designator(i^.l,a); add_offset(a,e.n*i^.l^.type^.base^.size);
    ELSE
      cmd.load(e,CARDINAL(i^.r^.type^.size));
      IF i^.l^.type^.size<=0FFH THEN r_sz:=1;
      ELSIF i^.l^.type^.size<=0FFFFH THEN r_sz:=2;
      ELSE r_sz:=4;
      END;
      index_mul(cmd.siz[cmd.stk-1],v_sz,r_sz,i^.l^.type^.base^.size);
      adr(i^.l,a); cmd.load(a,4); cmd.swap;
      cmd.add_adr(cmd.top,r_sz,FALSE); a.am:=am_aSTK; a.disp:=0;
    END;
  END;
END index;

PROCEDURE field(l: pc.NODE; VAR a: cmd.access);
BEGIN
  designator(l^.l,a); add_offset(a,l^.obj^.adr);
END field;

PROCEDURE deref(l: pc.NODE; VAR a: cmd.access);
BEGIN
  expression(l^.l,a);
  CASE a.am OF
    |am_imm   : a.am:=am_abs; a.disp:=a.n; a.n:=0;
    |am_Gimm  : a.am:=am_abs; a.disp:=a.n; a.n:=0;
    |am_Limm  : a.am:=am_abs; a.disp:=a.n; a.n:=0;
    |am_STK   : a.am:=am_aSTK; a.disp:=0;
    |am_G     : a.am:=am_aG; a.n:=a.disp; a.disp:=0;
    |am_L     : a.am:=am_aL; a.n:=a.disp; a.disp:=0;
  ELSE cmd.load(a,4); a.am:=am_aSTK; a.disp:=0;
  END;
END deref;

PROCEDURE gen_access(o: pc.OBJECT; VAR a: cmd.access);
  TYPE dword=SET OF [0..31];
  VAR e: EXT; ofs: CARDINAL;
BEGIN
  CASE o^.mode OF
    |pc.var :
      IF o^.scope>0 THEN
        a.am:=am_L; a.level:=o^.scope; a.disp:=o^.adr;
      ELSE
        IF indirect IN o^.tags THEN
          a.am:=am_aG; a.n:=o^.adr; a.disp:=0;
        ELSE
          a.am:=am_G; a.disp:=o^.adr;
        END;
        a.mod:=CARDINAL(-o^.scope);
      END;
    |pc.varpar:
      a.am:=am_aL; a.level:=o^.scope; a.n:=o^.adr; a.disp:=0;
    |pc.seq,pc.varseq:
      a.am:=am_L; a.level:=o^.scope; a.disp:=o^.adr;
    |pc.xproc:
      cmd.new_proc_const(o,ofs);
      a.am:=am_G; a.disp:=LONGINT(ofs); a.mod:=0;
    |pc.cons:
      IF o^.type^.mode IN mem_const THEN
        IF o^.scope>0 THEN
          a.am:=am_G; a.mod:=0; a.disp:=o^.adr;
        ELSE
          a.am:=am_G; a.disp:=o^.adr;
          a.mod:=CARDINAL(-o^.scope);
        END;
      ELSIF o^.type^.mode=pc.real THEN
        a.am:=am_imm; e:=o^.ext; a.n:=LONGINT(dword(e^.real));
      ELSE
        a.am:=am_imm; a.n:=o^.adr;
      END;
  ELSE error(0);
  END;
END gen_access;

PROCEDURE make_bitset(v_sz,r_sz: CARDINAL);
BEGIN
  cmd.alloc_reg(r_sz); cmd.pop_reg(2);
  IF cmd.pos[cmd.stk-2]=cmd.bc THEN cmd.pos[cmd.stk-2]:=cmd.da;
  ELSE cmd.b(cmd.xchg_cx);
  END;
  CASE r_sz OF
    |1: cmd.b(cmd.mov_ali); cmd.b(1);
        cmd.b(cmd.shift_bmc); cmd.b(cmd.md_reg+cmd.AL+cmd.s_shl);
    |2: cmd.b(cmd.mov_axi); cmd.w(1);
        cmd.b(cmd.shift_wmc); cmd.b(cmd.md_reg+cmd.AX+cmd.s_shl);
    |4: IF v_sz=1 THEN cmd.b(cmd.mov_chi); cmd.b(0) END;
        cmd.b(cmd.mov_axi); cmd.w(1);
        cmd.b(cmd.mov_dxi); cmd.w(0);
        cmd.b(cmd.jcxz); cmd.b(6);
        cmd.b(cmd.shift_wm1); cmd.b(cmd.md_reg+cmd.AX+cmd.s_shl);
        cmd.b(cmd.shift_wm1); cmd.b(cmd.md_reg+cmd.DX+cmd.s_rcl);
        cmd.b(cmd.loop); cmd.b(word(-6));
  ELSE error(0);
  END;
  cmd.siz[cmd.stk-2]:=r_sz; DEC(cmd.stk);
END make_bitset;

(*
PROCEDURE bitset_aggregate(l: ref; VAR res: cmd.access; sz: INTEGER);
  VAR fr,to,nsz,i,j: INTEGER; e: pc.ref; a,b: cmd.access;
BEGIN
  IF sz>4 THEN error(l,assert) END;
  min(l^.l^.dw,a); imm?(l^.l^.dw,a); fr:=a.n;
  max(l^.l^.dw,a); imm?(l^.l^.dw,a); to:=a.n;
  bytes(l^.l^.dw,a); imm?(l^.l^.dw,a); nsz:=a.n;
  res.am:=cmd.am_imm; res.n:=0;
  e:=l^.r;
  WHILE e#NIL DO
    IF e^.md=pc.range THEN
      IF (e^.l^.md=pc.number) & (e^.r^.md=pc.number) THEN
        i:=e^.l^.val; j:=e^.r^.val;
        IF (i<fr) OR (i>to) THEN error(e,range_error)
        ELSIF (j<fr) OR (j>to) THEN error(e,range_error)
        ELSIF i<=j THEN res.n:=INTEGER(BITSET(res.n)+{i-fr..j-fr});
        END;
      ELSE
        expression(e^.l,a); cmd.load(a,nsz); check_range(l^.l);
        expression(e^.r,a); cmd.load(a,nsz); check_range(l^.l);
        cmd.save; cmd.external_call(l^.pos,'$BstRng');
        DEC(cmd.stk); cmd.pos[cmd.stk-1]:=cmd.da;
        cmd.siz[cmd.stk-1]:=sz;
        IF res.am=cmd.am_STK THEN cmd.cmd_stk_m(cmd.c_or,cmd.top);
        ELSE res.am:=cmd.am_STK
        END;
      END;
    ELSE
      expression(e,a);
      IF a.am IN am_const THEN
        IF (a.n<fr) OR (a.n>to) THEN error(e,range_error)
        ELSE res.n:=INTEGER(BITSET(res.n)+{i-fr});
        END;
      ELSE
        cmd.load(a,nsz);
        check_range(l^.l);
        make_bitset(sz);
        IF res.am=cmd.am_STK THEN cmd.cmd_stk_m(cmd.c_or,cmd.top);
        ELSE res.am:=cmd.am_STK
        END;
      END;
    END;
    e:=e^.nxt;
  END;
  IF (res.am=cmd.am_STK) & (res.n#0) THEN
    const.n:=res.n; cmd.cmd_stk_m(cmd.c_or,const);
  END;
END bitset_aggregate;

PROCEDURE aggregate(l: ref; VAR res: cmd.access);
  VAR
    v,e     : ref;
    of,pos  : INTEGER;
    a,ea    : cmd.access;
    sz,esz,b: cmd.access;
BEGIN
  bytes(l^.l,sz);
  IF NOT (sz.am IN am_const) THEN
    error(l,'aggregate size must be constant')
  END;
  IF sz.n<=0 THEN
    -- ¯–€®©  £ž¥£  ¤®«¦¥­ «¥¦ ‹ ¯®  ¡€®«­®¬–  ¤ž¥€– NIL
    res.disp:=0; res.am:=am_abs; RETURN;
  END;
  IF (sz.n<=4) & (l^.l^.md=pc.set) THEN
    bitset_aggregate(l,res,sz.n); RETURN
  END;
  alloc_var(res,sz.n);
  e:=l^.r;
  IF (l^.l^.md=pc.array) OR (l^.l^.md=pc.packed_array) THEN
    bytes(l^.l^.r,esz); imm?(l,esz);
    a:=res;
    WHILE e#NIL DO
      expression(e,ea);
      cmd.load_store(a,ea,esz.n);
      e:=e^.nxt;
      add_offset(a,esz.n);
    END;
  ELSE error(l,unrealized);
  END;
END aggregate;
*)

PROCEDURE gen_value(l: pc.NODE; VAR a: cmd.access);
  VAR
    ofs,i: CARDINAL; e: EXT;
    pr   : POINTER TO LONGREAL;
    pb,pd: POINTER TO ARRAY [0..0FFFEH] OF BYTE;
BEGIN
  IF l^.mode#pc.value THEN error(233) END;
  IF l^.type^.mode=pc.longreal THEN
    a.am:=am_G; a.mod:=0; e:=l^.ext;
    cmd.new_glo_const(CARDINAL(l^.type^.size),pr,ofs);
    a.disp:=LONGINT(ofs); pr^:=e^.lrl;
  ELSIF l^.type^.mode=pc.real THEN
    a.am:=am_imm; e:=l^.ext; a.n:=LONGINT(dword(e^.real));
  ELSIF l^.type^.mode IN mem_const THEN
    a.am:=am_G; a.mod:=0; e:=l^.ext;
    cmd.new_glo_const(CARDINAL(l^.type^.size),pb,ofs);
    a.disp:=LONGINT(ofs); pd:=e^.buf;
    FOR i:=0 TO CARDINAL(l^.type^.size)-1 DO pb^[i]:=pd^[i] END;
  ELSE
    a.am:=am_imm; a.n:=l^.val;
  END;
END gen_value;

PROCEDURE designator(u: pc.NODE; VAR a: cmd.access);
BEGIN
  CASE u^.mode OF
    |pc.proc      : gen_access(u^.obj,a);
    |pc.var       : gen_access(u^.obj,a);
    |pc.cons      : gen_access(u^.obj,a);
(*    |pc.aggregate : aggregate(u,a);   *)
    |pc.index     : index(u,a);
    |pc.deref     : deref(u,a);
    |pc.field     : field(u,a);
    |pc.value     : gen_value(u,a);
(*    |pc.guard     : type_test(u,u^.obj^.type,TRUE,FALSE); *)
(*    |pc.uguard    : type_test(u,u^.obj^.type,TRUE,TRUE);  *)
  ELSE error(0);
  END;
END designator;

PROCEDURE designator_u(u: pc.NODE; VAR a: cmd.access);
BEGIN
  designator(u,a);
  IF u^.type^.mode IN pc.Forms{pc.dynarr,pc.array_of} THEN
    CASE a.am OF
      |am_imm   : a.am:=am_abs; a.disp:=a.n; a.n:=0;
      |am_Gimm  : a.am:=am_abs; a.disp:=a.n; a.n:=0;
      |am_Limm  : a.am:=am_abs; a.disp:=a.n; a.n:=0;
      |am_STK   : a.am:=am_aSTK; a.disp:=0;
      |am_G     : a.am:=am_aG; a.n:=a.disp; a.disp:=0;
      |am_L     : a.am:=am_aL; a.n:=a.disp; a.disp:=0;
    ELSE cmd.load(a,4); a.am:=am_aSTK; a.disp:=0;
    END;
  END;
END designator_u;

PROCEDURE power_2(v: LONGINT; VAR i: CARDINAL): BOOLEAN;
  VAR s: LONGINT;
BEGIN
  IF v<=0 THEN RETURN FALSE END;
  i:=30;
  LOOP
    s:=LONGINT(dword{i});
    IF v>=s THEN RETURN v=s END;
    DEC(i);
  END;
END power_2;

PROCEDURE load(l: pc.NODE);
  VAR a: cmd.access;
BEGIN
  expression(l,a);
  IF a.am#am_STK THEN cmd.load(a,CARDINAL(l^.type^.size)) END;
END load;

PROCEDURE load_2_swap(l,r: pc.NODE; VAR a: cmd.access);
  VAR aa: cmd.access;
BEGIN
  IF l^.type^.size#r^.type^.size THEN error(233) END;
  expression(l,aa); expression(r,a);
  IF aa.am=cmd.am_STK THEN RETURN END;
  IF NOT (a.am IN on_stack) & NOT (aa.am IN am_const) THEN
    cmd.load(aa,CARDINAL(l^.type^.size));
  ELSE
    IF a.am#cmd.am_STK THEN cmd.load(a,CARDINAL(l^.type^.size)) END;
    IF aa.am=cmd.am_aSTK THEN cmd.swap END;
    a:=aa;
  END;
END load_2_swap;

PROCEDURE load_2(l,r: pc.NODE; VAR a: cmd.access);
  VAR aa: cmd.access;
BEGIN
  IF l^.type^.size#r^.type^.size THEN error(233) END;
  expression(l,aa);
  IF aa.am=cmd.am_aSTK THEN
    cmd.load(aa,CARDINAL(l^.type^.size)); aa.am:=cmd.am_STK
  END;
  expression(r,a);
  IF aa.am=cmd.am_STK THEN RETURN END;
  cmd.load(aa,CARDINAL(l^.type^.size));
  IF (a.am=cmd.am_STK) OR (a.am=cmd.am_aSTK) THEN cmd.swap END;
END load_2;

PROCEDURE bic(a: cmd.access);
BEGIN
  IF a.am IN am_const THEN
    a.n:=LONGINT(dword(a.n)/dword{0..31});
    cmd.cmd_stk_m(cmd.c_and,a); RETURN;
  END;
  IF a.am#cmd.am_STK THEN cmd.load(a,cmd.siz[cmd.stk-1]) END;
  cmd.pop_reg(2);
  IF cmd.pos[cmd.stk-1]=cmd.da THEN
    CASE cmd.siz[cmd.stk-1] OF
      |1: cmd.b(cmd.grp1_bm); cmd.b(cmd.md_reg+cmd.AL+cmd.g1_not);
          cmd.b(cmd.and_brm); cmd.b(cmd.md_reg+cmd.AL+cmd.CL*8);
      |2: cmd.b(cmd.grp1_wm); cmd.b(cmd.md_reg+cmd.AX+cmd.g1_not);
          cmd.b(cmd.and_wrm); cmd.b(cmd.md_reg+cmd.AX+cmd.CX*8);
      |4: cmd.b(cmd.grp1_wm); cmd.b(cmd.md_reg+cmd.AX+cmd.g1_not);
          cmd.b(cmd.and_wrm); cmd.b(cmd.md_reg+cmd.AX+cmd.CX*8);
          cmd.b(cmd.grp1_wm); cmd.b(cmd.md_reg+cmd.DX+cmd.g1_not);
          cmd.b(cmd.and_wrm); cmd.b(cmd.md_reg+cmd.DX+cmd.BX*8);
    ELSE error(0);
    END;
  ELSE
    CASE cmd.siz[cmd.stk-1] OF
      |1: cmd.b(cmd.grp1_bm); cmd.b(cmd.md_reg+cmd.CL+cmd.g1_not);
          cmd.b(cmd.and_brm); cmd.b(cmd.md_reg+cmd.CL+cmd.AL*8);
      |2: cmd.b(cmd.grp1_wm); cmd.b(cmd.md_reg+cmd.CX+cmd.g1_not);
          cmd.b(cmd.and_wrm); cmd.b(cmd.md_reg+cmd.CX+cmd.AX*8);
      |4: cmd.b(cmd.grp1_wm); cmd.b(cmd.md_reg+cmd.CX+cmd.g1_not);
          cmd.b(cmd.and_wrm); cmd.b(cmd.md_reg+cmd.CX+cmd.AX*8);
          cmd.b(cmd.grp1_wm); cmd.b(cmd.md_reg+cmd.BX+cmd.g1_not);
          cmd.b(cmd.and_wrm); cmd.b(cmd.md_reg+cmd.BX+cmd.DX*8);
    ELSE error(0);
    END;
  END;
  DEC(cmd.stk);
END bic;

PROCEDURE set_z;
BEGIN
  IF cmd.siz[cmd.stk-1]>2 THEN
    IF cmd.pos[cmd.stk-1]=cmd.da THEN
      cmd.b(cmd.or_wrm); cmd.b(cmd.md_reg+cmd.DX+cmd.AX*8);
    ELSE
      cmd.b(cmd.or_wrm); cmd.b(cmd.md_reg+cmd.BX+cmd.CX*8);
    END;
  END;
  DEC(cmd.stk);
END set_z;

PROCEDURE vm(l: pc.STRUCT): val_mode;
  PROCEDURE str(r: pc.STRUCT): val_mode;
  BEGIN
    IF r^.mode=pc.char    THEN RETURN vm_string END;
    IF r^.mode=pc.range   THEN RETURN str(r^.base) END;
    RETURN vm_undef;
  END str;
BEGIN
  CASE l^.mode OF
    |pc.shortint : RETURN vm_integer;
    |pc.integer  : RETURN vm_integer;
    |pc.longint  : RETURN vm_integer;
    |pc.shortcard: RETURN vm_cardinal;
    |pc.cardinal : RETURN vm_cardinal;
    |pc.real     : RETURN vm_real;
    |pc.longreal : RETURN vm_real;
    |pc.boolean  : RETURN vm_boolean;
    |pc.char     : RETURN vm_cardinal;
    |pc.bitset   : RETURN vm_set;
    |pc.byte     : RETURN vm_cardinal;
    |pc.word     : RETURN vm_cardinal;
    |pc.addr     : RETURN vm_address;
    |pc.niltype  : RETURN vm_address;
    |pc.range    : RETURN vm(l^.base);
    |pc.enum     : RETURN vm_cardinal;
    |pc.opaque   : RETURN vm_undef;
    |pc.pointer  : RETURN vm_address;
    |pc.set      : RETURN vm_set;
    |pc.proctype : RETURN vm_address;
    |pc.array    : RETURN str(l^.base);
    |pc.vector   : RETURN str(l^.base);
    |pc.array_of : RETURN str(l^.base);
    |pc.dynarr   : RETURN str(l^.base);
    |pc.record   : RETURN vm_undef;
  ELSE error(0);
  END;
END vm;

PROCEDURE gen_flags_set(l: pc.NODE; VAR c: cmd.condition);
  VAR a: cmd.access;
BEGIN
  CASE c OF
    |cmd.c_le:
      load_2(l^.l,l^.r,a); bic(a); set_z; c:=cmd.c_z;
    |cmd.c_ge:
      load_2(l^.r,l^.l,a); bic(a); set_z; c:=cmd.c_z;
    |cmd.c_z,cmd.c_nz:
      load_2_swap(l^.l,l^.r,a);
      IF cmd.siz[cmd.stk-1]=4 THEN cmd.cmd_stk_m(cmd.c_xor,a); set_z;
      ELSE cmd.cmd_stk_m(cmd.c_sub,a); DEC(cmd.stk);
      END;
  ELSE error(233);
  END;
END gen_flags_set;

PROCEDURE gen_flags_cardinal(l: pc.NODE; VAR c: cmd.condition);
  VAR a: cmd.access;
BEGIN
  CASE c OF
    |cmd.c_l :
      load_2(l^.l,l^.r,a); cmd.cmd_stk_m(cmd.c_sub,a); cmd.drop; c:=cmd.c_c;
    |cmd.c_ge:
      load_2(l^.l,l^.r,a); cmd.cmd_stk_m(cmd.c_sub,a); cmd.drop; c:=cmd.c_nc;
    |cmd.c_le:
      load_2(l^.l,l^.r,a);
      IF cmd.siz[cmd.stk-1]=4 THEN
        cmd.load(a,4); cmd.swap; cmd.cmd_stk_m(cmd.c_sub,cmd.top); c:=cmd.c_nc;
      ELSE
        cmd.cmd_stk_m(cmd.c_sub,a); c:=cmd.c_cz;
      END;
      cmd.drop;
    |cmd.c_g :
      load_2(l^.l,l^.r,a);
      IF cmd.siz[cmd.stk-1]=4 THEN
        cmd.load(a,4); cmd.swap; cmd.cmd_stk_m(cmd.c_sub,cmd.top); c:=cmd.c_c;
      ELSE
        cmd.cmd_stk_m(cmd.c_sub,a); c:=cmd.c_ncz;
      END;
      cmd.drop;
    |cmd.c_z,cmd.c_nz:
      load_2_swap(l^.l,l^.r,a);
      IF cmd.siz[cmd.stk-1]=4 THEN cmd.cmd_stk_m(cmd.c_xor,a); set_z;
      ELSE cmd.cmd_stk_m(cmd.c_sub,a); DEC(cmd.stk);
      END;
  ELSE error(0);
  END;
END gen_flags_cardinal;

PROCEDURE gen_flags_address(l: pc.NODE; VAR c: cmd.condition);
BEGIN
  gen_flags_cardinal(l,c);
END gen_flags_address;

PROCEDURE gen_flags_integer(l: pc.NODE; VAR c: cmd.condition);
  VAR aa,a: cmd.access;
BEGIN
  expression(l^.l,aa); expression(l^.r,a);
  IF aa.am#cmd.am_STK THEN
    IF NOT (a.am IN on_stack) & NOT (aa.am IN am_const) THEN
      cmd.load(aa,CARDINAL(l^.l^.type^.size));
    ELSE
      IF a.am#cmd.am_STK THEN cmd.load(a,CARDINAL(l^.r^.type^.size)) END;
      IF aa.am=cmd.am_aSTK THEN cmd.swap END;
      a:=aa;
      CASE c OF
        |cmd.c_l : c:=cmd.c_g;      |cmd.c_ge: c:=cmd.c_le;
        |cmd.c_le: c:=cmd.c_ge;     |cmd.c_g : c:=cmd.c_l;
      ELSE
      END;
    END;
  END;
  CASE c OF
    |cmd.c_l : cmd.cmd_stk_m(cmd.c_sub,a); cmd.drop;
    |cmd.c_ge: cmd.cmd_stk_m(cmd.c_sub,a); cmd.drop;
    |cmd.c_le:
      IF cmd.siz[cmd.stk-1]=4 THEN
        cmd.load(a,4); cmd.swap; cmd.cmd_stk_m(cmd.c_sub,cmd.top); c:=cmd.c_ge;
      ELSE
        cmd.cmd_stk_m(cmd.c_sub,a);
      END;
      cmd.drop;
    |cmd.c_g :
      IF cmd.siz[cmd.stk-1]=4 THEN
        cmd.load(a,4); cmd.swap; cmd.cmd_stk_m(cmd.c_sub,cmd.top); c:=cmd.c_l;
      ELSE
        cmd.cmd_stk_m(cmd.c_sub,a);
      END;
      cmd.drop;
    |cmd.c_z,cmd.c_nz:
      IF cmd.siz[cmd.stk-1]=4 THEN cmd.cmd_stk_m(cmd.c_xor,a); set_z;
      ELSE cmd.cmd_stk_m(cmd.c_sub,a); DEC(cmd.stk);
      END;
  ELSE error(0);
  END;
END gen_flags_integer;

PROCEDURE gen_flags_real(l: pc.NODE; VAR c: cmd.condition);
  VAR sz: CARDINAL; a: cmd.access;
BEGIN
  sz:=CARDINAL(l^.l^.type^.size);
  expression(l^.l,a); cmd.fpp_load(cmd.f_mov,a,sz);
  expression(l^.r,a); cmd.fpp_load(cmd.f_cmp,a,sz);
  const.n:=41H; cmd.cmd_stk_m(cmd.c_and,const);
  CASE c OF
    |cmd.c_l : const.n:=1; cmd.cmd_stk_m(cmd.c_cmp,const); c:=cmd.c_z;
    |cmd.c_le: c:=cmd.c_nz;
    |cmd.c_ge: const.n:=1; cmd.cmd_stk_m(cmd.c_cmp,const); c:=cmd.c_nz;
    |cmd.c_g : c:=cmd.c_z;
    |cmd.c_z : const.n:=40H; cmd.cmd_stk_m(cmd.c_cmp,const); c:=cmd.c_z;
    |cmd.c_nz: const.n:=40H; cmd.cmd_stk_m(cmd.c_cmp,const); c:=cmd.c_nz;
  ELSE error(0);
  END;
  DEC(cmd.stk);
END gen_flags_real;

PROCEDURE gen_flags_string(l: pc.NODE; VAR c: cmd.condition);
  VAR a: cmd.access;
BEGIN
  adr_u(l^.l,a); cmd.load(a,4);
  adr_u(l^.r,a); cmd.load(a,4);
  cmd.call_rts(13); DEC(cmd.stk,2);
  CASE c OF
    |cmd.c_l : c:=cmd.c_c;      |cmd.c_ge: c:=cmd.c_nc;
    |cmd.c_le: c:=cmd.c_cz;     |cmd.c_g : c:=cmd.c_ncz;
  ELSE
  END;
END gen_flags_string;

PROCEDURE gen_flags(l: pc.NODE; VAR c: cmd.condition);
BEGIN
  CASE vm(l^.l^.type) OF
    |vm_integer : gen_flags_integer (l,c);
    |vm_real    : gen_flags_real    (l,c);
    |vm_cardinal: gen_flags_cardinal(l,c);
    |vm_boolean : gen_flags_cardinal(l,c);
    |vm_set     : gen_flags_set     (l,c);
    |vm_string  : gen_flags_string  (l,c);
    |vm_address : gen_flags_address (l,c);
  ELSE error(0);
  END;
END gen_flags;

PROCEDURE gen_compare(l: pc.NODE; VAR a: cmd.access; c: cmd.condition);
  VAR i: CARDINAL;
BEGIN
  gen_flags(l,c);
  const.n:=1; cmd.load(const,1);
  cmd.b(cmd.jo+INTEGER(c)); cmd.b(0);
  i:=cmd.cnt;
  cmd.drop; const.n:=0; cmd.load(const,1);
  cmd.code[i-1]:=byte(cmd.cnt-i);
  a.am:=cmd.am_STK;
END gen_compare;

PROCEDURE gen_compare_condition
  (l: pc.NODE; c: cmd.condition; then,else: cmd.node);
BEGIN
  gen_flags(l,c);
  cmd.block^.md:=cmd.nm_cond;
  cmd.block^.goto:=then;
  cmd.block^.else:=else;
  cmd.block^.flag:=c;
END gen_compare_condition;

PROCEDURE gen_in_condition(l: pc.NODE; then,else: cmd.node);
  TYPE set32=SET OF [0..31];
  VAR a,b,c: cmd.access; lsz,rsz: CARDINAL; sg: BOOLEAN; fr,to: LONGINT;
BEGIN
  expression(l^.l,a); expression(l^.r,c);
  IF (a.am IN on_stack) & (c.am IN on_stack) THEN cmd.swap END;
  sg:=sign(l^.l^.type);
  lsz:=CARDINAL(l^.l^.type^.size);
  rsz:=CARDINAL(l^.r^.type^.size);
  min(l^.r^.type,b); imm(b); fr:=b.n;
  IF a.am=cmd.am_imm THEN
    max(l^.r^.type,b); imm(b); to:=b.n;
    IF (a.n<fr) OR (a.n>to) THEN a.n:=0
    ELSE a.n:=LONGINT(set32{CARDINAL(a.n-fr)})
    END;
  ELSE
    cmd.load(a,lsz); a.am:=cmd.am_STK;
    IF c.am IN on_stack THEN cmd.pop_reg(2) END;
    cmd.cmd_stk_m(cmd.c_sub,b);
    IF sg THEN cmd.block^.flag:=cmd.c_ge;
    ELSE cmd.block^.flag:=cmd.c_nc;
    END;
    cmd.block^.else:=else; cmd.block^.goto:=cmd.new_node();
    cmd.block^.md:=cmd.nm_cond; cmd.start(cmd.block^.goto);
    IF lsz=4 THEN
      cmd.siz[cmd.stk-1]:=2; cmd.block^.flag:=cmd.c_z;
      cmd.block^.else:=else; cmd.block^.goto:=cmd.new_node();
      cmd.block^.md:=cmd.nm_cond; cmd.start(cmd.block^.goto);
    END;
    max(l^.r^.type,b); imm(b); to:=b.n;
    b.n:=to-fr; cmd.cmd_stk_m(cmd.c_cmp,b);
    cmd.block^.flag:=cmd.c_cz;
    cmd.block^.else:=else; cmd.block^.goto:=cmd.new_node();
    cmd.block^.md:=cmd.nm_cond; cmd.start(cmd.block^.goto);
    make_bitset(rsz,lsz);
  END;
  IF a.am=cmd.am_STK THEN
    IF c.am=cmd.am_aSTK THEN cmd.swap END;
    cmd.cmd_stk_m(cmd.c_and,c);
  ELSE cmd.load(c,rsz); cmd.cmd_stk_m(cmd.c_and,a);
  END;
  IF rsz>2 THEN
    IF cmd.pos[cmd.stk-1]=cmd.da THEN
      cmd.b(cmd.or_wrm); cmd.b(cmd.md_reg+cmd.DX+cmd.AX*8);
    ELSE
      cmd.b(cmd.or_wrm); cmd.b(cmd.md_reg+cmd.BX+cmd.CX*8);
    END;
  END;
  cmd.drop;
  cmd.block^.md:=cmd.nm_cond; cmd.block^.flag:=cmd.c_nz;
  cmd.block^.else:=else; cmd.block^.goto:=then;
END gen_in_condition;

PROCEDURE gen_condition(cond: pc.NODE; then,else: cmd.node);
  VAR h: cmd.node;
BEGIN
  CASE cond^.sub OF
    |pc.not   : gen_condition(cond^.l,else,then);
    |pc.cor   : h:=cmd.new_node(); gen_condition(cond^.l,then,h);
                cmd.start(h); gen_condition(cond^.r,then,else);
    |pc.cand  : h:=cmd.new_node(); gen_condition(cond^.l,h,else);
                cmd.start(h); gen_condition(cond^.r,then,else);
    |pc.lss   : gen_compare_condition(cond,cmd.c_l,then,else);
    |pc.leq   : gen_compare_condition(cond,cmd.c_le,then,else);
    |pc.gtr   : gen_compare_condition(cond,cmd.c_g,then,else);
    |pc.geq   : gen_compare_condition(cond,cmd.c_ge,then,else);
    |pc.equ   : gen_compare_condition(cond,cmd.c_z,then,else);
    |pc.neq   : gen_compare_condition(cond,cmd.c_nz,then,else);
    |pc.in    : gen_in_condition(cond,then,else);
  ELSE
    load(cond); cmd.set_flag_z;
    cmd.block^.md:=cmd.nm_cond;
    cmd.block^.flag:=cmd.c_nz;
    cmd.block^.goto:=then;
    cmd.block^.else:=else;
  END;
END gen_condition;

PROCEDURE gen_neg(l: pc.NODE; VAR a: cmd.access);
  VAR ax,dx,sz: CARDINAL; ln: LONGINT;
BEGIN
  expression(l^.l,a);
  CASE vm(l^.type) OF
    |vm_cardinal:
    |vm_integer :
      sz:=CARDINAL(l^.l^.type^.size);
      cmd.load(a,sz); a.am:=cmd.am_STK; cmd.pop_reg(1);
      IF cmd.pos[cmd.stk-1]=cmd.da THEN ax:=cmd.AX; dx:=cmd.DX;
      ELSE ax:=cmd.CX; dx:=cmd.BX;
      END;
      CASE sz OF
        |1: cmd.b(cmd.grp1_bm); cmd.b(cmd.md_reg+ax+cmd.g1_neg);
        |2: cmd.b(cmd.grp1_wm); cmd.b(cmd.md_reg+ax+cmd.g1_neg);
        |4: cmd.b(cmd.grp1_wm); cmd.b(cmd.md_reg+dx+cmd.g1_neg);
            cmd.b(cmd.grp1_wm); cmd.b(cmd.md_reg+ax+cmd.g1_neg);
            cmd.b(cmd.imm_wbm); cmd.b(cmd.md_reg+dx+cmd.i_sbb); cmd.b(0);
      END;
      IF ovr_check THEN cmd.b(cmd.into); cmd.b(cmd.nop) END;
    |vm_real:
      sz:=CARDINAL(l^.l^.type^.size); cmd.fpp_load(cmd.f_mov,a,sz);
      cmd.fpp_ucmd(cmd.u_neg); a.am:=cmd.am_FPP;
    |vm_set :
      sz:=CARDINAL(l^.type^.size); cmd.load(a,sz);
      min(l^.type,a); imm(a); ln:=a.n;
      max(l^.type,a); imm(a); ln:=a.n-ln;
      const.n:=LONGINT(dword{0..CARDINAL(ln)}); a.am:=cmd.am_STK;
      cmd.cmd_stk_m(cmd.c_xor,const);
  ELSE error(0);
  END;
END gen_neg;

PROCEDURE gen_minus(l: pc.NODE; VAR a: cmd.access);
  VAR sz: CARDINAL;
BEGIN
  CASE vm(l^.l^.type) OF
    |vm_integer :
      load_2(l^.l,l^.r,a); cmd.cmd_stk_m(cmd.c_sub,a); a.am:=cmd.am_STK;
      IF ovr_check THEN cmd.b(cmd.into); cmd.b(cmd.nop) END;
    |vm_cardinal:
      load_2(l^.l,l^.r,a); cmd.cmd_stk_m(cmd.c_sub,a); a.am:=cmd.am_STK;
    |vm_address :
      load_2(l^.l,l^.r,a);
      cmd.sub_adr(a,CARDINAL(l^.r^.type^.size),sign(l^.r^.type));
      a.am:=cmd.am_STK;
    |vm_real    :
      sz:=CARDINAL(l^.type^.size);
      expression(l^.l,a); cmd.fpp_load(cmd.f_mov,a,sz);
      expression(l^.r,a); cmd.fpp_load(cmd.f_sub,a,sz);
      a.am:=cmd.am_FPP;
  ELSE error(233);
  END;
END gen_minus;

PROCEDURE gen_bic(l: pc.NODE; VAR a: cmd.access);
BEGIN
  load_2(l^.l,l^.r,a); bic(a); a.am:=cmd.am_STK;
END gen_bic;

PROCEDURE gen_plus(l: pc.NODE; VAR a: cmd.access);
  VAR sz: CARDINAL; b: cmd.access;
BEGIN
  CASE vm(l^.l^.type) OF
    |vm_integer :
      load_2_swap(l^.l,l^.r,a); cmd.cmd_stk_m(cmd.c_add,a);
      a.am:=cmd.am_STK;
      IF ovr_check THEN cmd.b(cmd.into); cmd.b(cmd.nop) END;
    |vm_cardinal:
      load_2_swap(l^.l,l^.r,a); cmd.cmd_stk_m(cmd.c_add,a);
      a.am:=cmd.am_STK;
    |vm_address :
      load_2(l^.l,l^.r,a);
      cmd.add_adr(a,CARDINAL(l^.r^.type^.size),sign(l^.r^.type));
      a.am:=cmd.am_STK;
    |vm_real    :
      sz:=CARDINAL(l^.type^.size);
      expression(l^.l,a); expression(l^.r,b);
      IF a.am=cmd.am_FPP THEN cmd.fpp_load(cmd.f_add,b,sz);
      ELSIF b.am=cmd.am_FPP THEN cmd.fpp_load(cmd.f_add,a,sz);
      ELSE cmd.fpp_load(cmd.f_mov,b,sz); cmd.fpp_load(cmd.f_add,a,sz);
      END;
      a.am:=cmd.am_FPP;
  ELSE error(233);
  END;
END gen_plus;

PROCEDURE gen_or(l: pc.NODE; VAR a: cmd.access);
BEGIN
  load_2_swap(l^.l,l^.r,a); cmd.cmd_stk_m(cmd.c_or,a); a.am:=cmd.am_STK;
END gen_or;

PROCEDURE gen_cor(l: pc.NODE; VAR a: cmd.access);
  VAR then,else,next: cmd.node; sz: CARDINAL;
BEGIN
  then:=cmd.new_node(); else:=cmd.new_node(); next:=cmd.new_node();
  gen_condition(l,then,else);
  cmd.start(then); const.n:=1;
  cmd.load(const,1); cmd.drop; cmd.block^.goto:=next;
  cmd.start(else); const.n:=0;
  cmd.load(const,1); cmd.block^.goto:=next;
  cmd.start(next); a.am:=cmd.am_STK;
END gen_cor;

PROCEDURE cmul;
BEGIN
  IF cmd.siz[cmd.stk-1]=4 THEN
    cmd.call_rts(5);
  ELSE
    cmd.pop_reg(2);
    IF cmd.siz[cmd.stk-1]=2 THEN
      cmd.b(cmd.grp1_wm); cmd.b(cmd.md_reg+cmd.CX+cmd.g1_mul);
    ELSE
      cmd.b(cmd.grp1_bm); cmd.b(cmd.md_reg+cmd.CL+cmd.g1_mul);
    END;
  END;
  DEC(cmd.stk); cmd.pos[cmd.stk-1]:=cmd.da;
END cmul;

PROCEDURE gen_mul(l: pc.NODE; VAR a: cmd.access);
  VAR sz: CARDINAL; b: cmd.access;
BEGIN
  CASE vm(l^.type) OF
    |vm_integer :
      load_2_swap(l^.l,l^.r,a); cmd.imul(a); a.am:=cmd.am_STK;
      IF ovr_check THEN cmd.b(cmd.into); cmd.b(cmd.nop) END;
    |vm_cardinal:
      load_2_swap(l^.l,l^.r,a); cmd.load(a,cmd.siz[cmd.stk-1]);
      cmul; a.am:=cmd.am_STK;
    |vm_real    :
      sz:=CARDINAL(l^.type^.size);
      expression(l^.l,a); expression(l^.r,b);
      IF a.am=cmd.am_FPP THEN cmd.fpp_load(cmd.f_mul,b,sz);
      ELSIF b.am=cmd.am_FPP THEN cmd.fpp_load(cmd.f_mul,a,sz);
      ELSE cmd.fpp_load(cmd.f_mov,b,sz); cmd.fpp_load(cmd.f_mul,a,sz);
      END;
      a.am:=cmd.am_FPP;
  ELSE error(233);
  END;
END gen_mul;

PROCEDURE gen_and(l: pc.NODE; VAR a: cmd.access);
BEGIN
  load_2_swap(l^.l,l^.r,a); cmd.cmd_stk_m(cmd.c_and,a); a.am:=cmd.am_STK;
END gen_and;

PROCEDURE gen_cand(l: pc.NODE; VAR a: cmd.access);
  VAR then,else,next: cmd.node;
BEGIN
  then:=cmd.new_node(); else:=cmd.new_node(); next:=cmd.new_node();
  gen_condition(l,then,else);
  cmd.start(then); const.n:=1;
  cmd.load(const,1); cmd.drop; cmd.block^.goto:=next;
  cmd.start(else); const.n:=0;
  cmd.load(const,1); cmd.block^.goto:=next;
  cmd.start(next); a.am:=cmd.am_STK;
END gen_cand;

PROCEDURE iquot;
BEGIN
  IF cmd.siz[cmd.stk-1]=4 THEN
    cmd.call_rts(6);
  ELSE
    cmd.pop_reg(2);
    IF cmd.pos[cmd.stk-1]=cmd.da THEN cmd.b(cmd.xchg_cx) END;
    IF cmd.siz[cmd.stk-1]=2 THEN
      cmd.b(cmd.cwd);
      cmd.b(cmd.grp1_wm); cmd.b(cmd.md_reg+cmd.CX+cmd.g1_idiv);
    ELSE
      cmd.b(cmd.cbw);
      cmd.b(cmd.grp1_bm); cmd.b(cmd.md_reg+cmd.CL+cmd.g1_idiv);
    END;
  END;
  DEC(cmd.stk); cmd.pos[cmd.stk-1]:=cmd.da;
END iquot;

PROCEDURE cdiv;
BEGIN
  IF cmd.siz[cmd.stk-1]=4 THEN
    cmd.call_rts(7);
  ELSE
    cmd.pop_reg(2);
    IF cmd.pos[cmd.stk-1]=cmd.da THEN cmd.b(cmd.xchg_cx) END;
    IF cmd.siz[cmd.stk-1]=2 THEN
      cmd.b(cmd.mov_dxi); cmd.w(0);
      cmd.b(cmd.grp1_wm); cmd.b(cmd.md_reg+cmd.CX+cmd.g1_div);
    ELSE
      cmd.b(cmd.mov_ahi); cmd.b(0);
      cmd.b(cmd.grp1_bm); cmd.b(cmd.md_reg+cmd.CL+cmd.g1_div);
    END;
  END;
  DEC(cmd.stk); cmd.pos[cmd.stk-1]:=cmd.da;
END cdiv;

PROCEDURE gen_slash(l: pc.NODE; VAR a: cmd.access);
  VAR sz: CARDINAL;
BEGIN
  CASE vm(l^.type) OF
    |vm_integer :
      load_2(l^.l,l^.r,a); cmd.load(a,cmd.siz[cmd.stk-1]);
      iquot; a.am:=cmd.am_STK;
      IF ovr_check THEN cmd.b(cmd.into); cmd.b(cmd.nop) END;
    |vm_cardinal:
      load_2(l^.l,l^.r,a); cmd.load(a,cmd.siz[cmd.stk-1]);
      cdiv; a.am:=cmd.am_STK;
    |vm_real    :
      sz:=CARDINAL(l^.type^.size);
      expression(l^.l,a); cmd.fpp_load(cmd.f_mov,a,sz);
      expression(l^.r,a); cmd.fpp_load(cmd.f_div,a,sz);
      a.am:=cmd.am_FPP;
  ELSE error(0);
  END;
END gen_slash;

PROCEDURE gen_xor(l: pc.NODE; VAR a: cmd.access);
BEGIN
  load_2_swap(l^.l,l^.r,a); cmd.cmd_stk_m(cmd.c_xor,a);
  a.am:=cmd.am_STK;
END gen_xor;

PROCEDURE gen_div(l: pc.NODE; VAR a: cmd.access);
BEGIN
  CASE vm(l^.type) OF
    |vm_integer :
      load_2(l^.l,l^.r,a); cmd.idiv(a); a.am:=cmd.am_STK;
      IF ovr_check THEN cmd.b(cmd.into); cmd.b(cmd.nop) END;
    |vm_cardinal:
      load_2(l^.l,l^.r,a); cmd.load(a,cmd.siz[cmd.stk-1]);
      cdiv; a.am:=cmd.am_STK;
  ELSE error(0);
  END;
END gen_div;

PROCEDURE irem;
BEGIN
  IF cmd.siz[cmd.stk-1]=4 THEN
    cmd.call_rts(8);
  ELSE
    cmd.pop_reg(2);
    IF cmd.pos[cmd.stk-1]=cmd.da THEN cmd.b(cmd.xchg_cx) END;
    IF cmd.siz[cmd.stk-1]=2 THEN
      cmd.b(cmd.cwd);
      cmd.b(cmd.grp1_wm); cmd.b(cmd.md_reg+cmd.CX+cmd.g1_idiv);
      cmd.b(cmd.mov_wrm); cmd.b(cmd.md_reg+cmd.DX+cmd.AX*8);
    ELSE
      cmd.b(cmd.cbw);
      cmd.b(cmd.grp1_bm); cmd.b(cmd.md_reg+cmd.CL+cmd.g1_idiv);
      cmd.b(cmd.mov_brm); cmd.b(cmd.md_reg+cmd.AH+cmd.AL*8);
    END;
  END;
  DEC(cmd.stk); cmd.pos[cmd.stk-1]:=cmd.da;
END irem;

PROCEDURE imod(VAR a: cmd.access);
  VAR i: CARDINAL;
BEGIN
  IF (a.am IN am_const) & power_2(a.n,i) THEN
    IF i=0 THEN DEC(cmd.stk); a.n:=0; a.am:=cmd.am_imm; RETURN END;
    a.n:=LONGINT(dword{0..i-1}); a.am:=cmd.am_imm;
    cmd.cmd_stk_m(cmd.c_and,a); a.am:=cmd.am_STK;
  ELSE
    cmd.save; cmd.stot(a,cmd.siz[cmd.stk-1]);
    CASE cmd.siz[cmd.stk-1] OF
      |1: cmd.call_rts(9);
      |2: cmd.call_rts(10);
      |4: cmd.call_rts(11);
    ELSE error(0);
    END;
    cmd.pos[cmd.stk-1]:=cmd.da; a.am:=cmd.am_STK;
  END;
END imod;

PROCEDURE cmod;
BEGIN
  IF cmd.siz[cmd.stk-1]=4 THEN
    cmd.call_rts(12);
  ELSE
    cmd.pop_reg(2);
    IF cmd.pos[cmd.stk-1]=cmd.da THEN cmd.b(cmd.xchg_cx) END;
    IF cmd.siz[cmd.stk-1]=2 THEN
      cmd.b(cmd.mov_dxi); cmd.w(0);
      cmd.b(cmd.grp1_wm); cmd.b(cmd.md_reg+cmd.CX+cmd.g1_div);
      cmd.b(cmd.mov_wrm); cmd.b(cmd.md_reg+cmd.DX+cmd.AX*8);
    ELSE
      cmd.b(cmd.mov_ahi); cmd.b(0);
      cmd.b(cmd.grp1_bm); cmd.b(cmd.md_reg+cmd.CL+cmd.g1_div);
      cmd.b(cmd.mov_brm); cmd.b(cmd.md_reg+cmd.AH+cmd.AL*8);
    END;
  END;
  DEC(cmd.stk); cmd.pos[cmd.stk-1]:=cmd.da;
END cmod;

PROCEDURE gen_rem(l: pc.NODE; VAR a: cmd.access);
BEGIN
  CASE vm(l^.type) OF
    |vm_integer :
      load_2(l^.l,l^.r,a); cmd.load(a,cmd.siz[cmd.stk-1]);
      irem; a.am:=cmd.am_STK;
      IF ovr_check THEN cmd.b(cmd.into); cmd.b(cmd.nop) END;
    |vm_cardinal:
      load_2(l^.l,l^.r,a); cmd.load(a,cmd.siz[cmd.stk-1]);
      cmod; a.am:=cmd.am_STK;
  ELSE error(0);
  END;
END gen_rem;

PROCEDURE gen_mod(l: pc.NODE; VAR a: cmd.access);
BEGIN
  CASE vm(l^.type) OF
    |vm_integer :
      load_2(l^.l,l^.r,a); imod(a);
      IF ovr_check THEN cmd.b(cmd.into); cmd.b(cmd.nop) END;
    |vm_cardinal:
      load_2(l^.l,l^.r,a); cmd.load(a,cmd.siz[cmd.stk-1]);
      cmod; a.am:=cmd.am_STK;
  ELSE error(0);
  END;
END gen_mod;

PROCEDURE gen_bits_u(l: pc.NODE; VAR a: cmd.access);
BEGIN
  bytes_u(l^.l,a);
  IF a.am IN am_const THEN
    a.n:=a.n*8; a.am:=cmd.am_imm;
  ELSE
    cmd.load(a,4); a.am:=cmd.am_STK;
    const.n:=8; cmd.imul(const);
  END;
END gen_bits_u;

PROCEDURE gen_type_transfer(l: pc.NODE; VAR a: cmd.access);
BEGIN
  expression(l^.l,a);
  IF l^.l^.type^.mode=pc.dynarr THEN error(233); RETURN END;
  IF l^.l^.type^.mode=pc.array_of THEN error(233); RETURN END;
  IF l^.type^.mode=pc.dynarr THEN error(233); RETURN END;
  IF l^.type^.size#l^.l^.type^.size THEN error(34) END;
END gen_type_transfer;

PROCEDURE gen_type_conversion(l: pc.NODE; VAR a: cmd.access);
  VAR sz: CARDINAL;
BEGIN
  IF l^.l^.type^.mode IN pc.Forms{pc.dynarr,pc.array_of} THEN
    error(34);
  ELSIF (l^.l^.type^.mode IN pc.SCALARs) & (l^.type^.mode IN pc.SCALARs) THEN
    range_check(l^.l,a,l^.type,TRUE);
  ELSIF (l^.l^.type^.mode IN pc.SCALARs) & (l^.type^.mode IN pc.REALs) THEN
    expression(l^.l,a);
    sz:=CARDINAL(l^.l^.type^.size);
    IF NOT sign(l^.l^.type) THEN
      cmd.load(a,sz); cmd.set_size(4,FALSE); sz:=4; a.am:=am_STK;
    END;
    cmd.fpp_load_integer(cmd.f_mov,a,sz);
    a.am:=cmd.am_FPP;
  ELSIF (l^.l^.type^.mode IN pc.REALs) & (l^.type^.mode IN pc.SCALARs) THEN
    expression(l^.l,a);
    cmd.fpp_load(cmd.f_mov,a,CARDINAL(l^.l^.type^.size));
    sz:=CARDINAL(l^.type^.size);
    cmd.alloc_tmp_var(a,sz);
    cmd.fpp_store_integer(a,sz);
  ELSE error(34);
  END;
END gen_type_conversion;

PROCEDURE gen_range_check(rr: pc.NODE; VAR a: cmd.access);
BEGIN
  range_check(rr^.l,a,rr^.type,TRUE);
END gen_range_check;

PROCEDURE gen_not(l: pc.NODE; VAR a: cmd.access);
  VAR i: CARDINAL;
BEGIN
  expression(l^.l,a); cmd.load(a,1);
  cmd.set_flag_z;
  const.n:=1; cmd.load(const,1);
  cmd.b(cmd.je); cmd.b(0);
  i:=cmd.cnt;
  cmd.drop; const.n:=0; cmd.load(const,1);
  cmd.code[i-1]:=byte(cmd.cnt-i);
  a.am:=cmd.am_STK;
END gen_not;

PROCEDURE gen_in(l: pc.NODE; VAR a: cmd.access);
  VAR then,else,next: cmd.node;
BEGIN
  next:=cmd.new_node(); then:=cmd.new_node(); else:=cmd.new_node();
  gen_in_condition(l,then,else);
  cmd.start(then); const.n:=1;
  cmd.load(const,1); cmd.drop; cmd.block^.goto:=next;
  cmd.start(else); const.n:=0;
  cmd.load(const,1); cmd.block^.goto:=next; cmd.start(next);
  a.am:=cmd.am_STK;
END gen_in;

PROCEDURE gen_abs(l: pc.NODE; VAR a: cmd.access);
  VAR ax,dx,sz: CARDINAL;
BEGIN
  expression(l^.l,a);
  CASE vm(l^.type) OF
    |vm_cardinal:
    |vm_integer :
      sz:=CARDINAL(l^.type^.size);
      cmd.load(a,sz); a.am:=cmd.am_STK; cmd.pop_reg(1);
      IF cmd.pos[cmd.stk-1]=cmd.da THEN ax:=cmd.AX; dx:=cmd.DX;
      ELSE ax:=cmd.CX; dx:=cmd.BX;
      END;
      CASE sz OF
        |1: cmd.b(cmd.and_brm); cmd.b(cmd.md_reg+ax+ax*8);
            cmd.b(cmd.jns); cmd.b(2);
            cmd.b(cmd.grp1_bm); cmd.b(cmd.md_reg+ax+cmd.g1_neg);
        |2: cmd.b(cmd.and_wrm); cmd.b(cmd.md_reg+ax+ax*8);
            cmd.b(cmd.jns); cmd.b(2);
            cmd.b(cmd.grp1_wm); cmd.b(cmd.md_reg+ax+cmd.g1_neg);
        |4: cmd.b(cmd.and_wrm); cmd.b(cmd.md_reg+dx+dx*8);
            cmd.b(cmd.jns); cmd.b(7);
            cmd.b(cmd.grp1_wm); cmd.b(cmd.md_reg+dx+cmd.g1_neg);
            cmd.b(cmd.grp1_wm); cmd.b(cmd.md_reg+ax+cmd.g1_neg);
            cmd.b(cmd.imm_wbm); cmd.b(cmd.md_reg+dx+cmd.i_sbb); cmd.b(0);
      ELSE error(0);
      END;
      IF ovr_check THEN cmd.b(cmd.into); cmd.b(cmd.nop) END;
    |vm_real   :
      sz:=CARDINAL(l^.type^.size); cmd.fpp_load(cmd.f_mov,a,sz);
      cmd.fpp_ucmd(cmd.u_abs); a.am:=cmd.am_FPP;
  ELSE error(0);
  END;
END gen_abs;

PROCEDURE worder(l: pc.NODE; VAR a: cmd.access);
BEGIN
  CASE vm(l^.type) OF
    |vm_integer : load(l); cmd.set_size(4,TRUE);  a.am:=cmd.am_STK;
    |vm_cardinal: load(l); cmd.set_size(4,FALSE); a.am:=cmd.am_STK;
    |vm_boolean : load(l); cmd.set_size(4,FALSE); a.am:=cmd.am_STK;
    |vm_set     : load(l); cmd.set_size(4,FALSE); a.am:=cmd.am_STK;
    |vm_real    : expression(l,a);
    |vm_address : expression(l,a);
    |vm_string  : adr_u(l,a);
    |vm_undef   : adr_u(l,a);
  ELSE error(0);
  END;
END worder;

PROCEDURE gen_rot(l: pc.NODE; VAR a: cmd.access);
  VAR lsz,ax,dx,cx,i: CARDINAL;
BEGIN
  lsz:=CARDINAL(l^.l^.type^.size); expression(l^.l,a); cmd.load(a,lsz);
  IF lsz=4 THEN
    expression(l^.r,a);
    IF a.am IN am_const THEN
      IF l^.sub=pc.ror THEN a.n:=-a.n END;
      a.n:=a.n MOD 32;
      cmd.pop_reg(1);
      IF a.n>=16 THEN
        IF cmd.pos[cmd.stk-1]=cmd.da THEN cmd.b(cmd.xchg_dx);
        ELSE cmd.b(cmd.xchg_wmr); cmd.b(cmd.md_reg+cmd.CX*8+cmd.BX);
        END;
        a.n:=a.n-16;
      END;
      IF a.n<=3 THEN
        IF cmd.pos[cmd.stk-1]=cmd.da THEN ax:=cmd.AX; dx:=cmd.DX;
        ELSE ax:=cmd.CX; dx:=cmd.BX;
        END;
        FOR i:=1 TO CARDINAL(a.n) DO
          cmd.b(cmd.shift_wm1); cmd.b(cmd.md_reg+ax+cmd.s_shl);
          cmd.b(cmd.shift_wm1); cmd.b(cmd.md_reg+dx+cmd.s_rcl);
          cmd.b(cmd.imm_bm); cmd.b(cmd.md_reg+ax+cmd.i_adc); cmd.b(0);
        END;
        a.am:=cmd.am_STK; RETURN;
      END;
    END;
    cmd.load(a,1); cmd.pop_reg(2);
    IF cmd.pos[cmd.stk-1]=cmd.da THEN ax:=cmd.CX; dx:=cmd.BX; cx:=cmd.AX;
    ELSE ax:=cmd.AX; dx:=cmd.DX; cx:=cmd.CX;
    END;
    IF NOT (a.am IN am_const) THEN
      IF l^.sub=pc.ror THEN
        cmd.b(cmd.grp1_bm); cmd.b(cmd.md_reg+cx+cmd.g1_neg);
      END;
      cmd.b(cmd.imm_bm); cmd.b(cmd.md_reg+cx+cmd.i_and); cmd.b(1FH);
      cmd.b(cmd.je); cmd.b(11);
    END;
    cmd.b(cmd.shift_wm1); cmd.b(cmd.md_reg+ax+cmd.s_shl);
    cmd.b(cmd.shift_wm1); cmd.b(cmd.md_reg+dx+cmd.s_rcl);
    cmd.b(cmd.imm_bm); cmd.b(cmd.md_reg+ax+cmd.i_adc); cmd.b(0);
    cmd.b(cmd.grp2_b); cmd.b(cmd.md_reg+cx+cmd.g2_dec);
    cmd.b(cmd.jne); cmd.b(word(-11));
    DEC(cmd.stk);
  ELSE
    load(l^.r); cmd.pop_reg(2);
    IF cmd.pos[cmd.stk-1]=cmd.da THEN cmd.b(cmd.xchg_cx) END;
    IF lsz=1 THEN cmd.b(cmd.shift_bmc) ELSE cmd.b(cmd.shift_wmc) END;
    IF l^.sub=pc.ror THEN cmd.b(cmd.md_reg+cmd.s_ror);
    ELSE cmd.b(cmd.md_reg+cmd.s_rol);
    END;
    DEC(cmd.stk); cmd.pos[cmd.stk-1]:=cmd.da;
  END;
  a.am:=cmd.am_STK;
END gen_rot;

PROCEDURE gen_ash(l: pc.NODE; VAR a: cmd.access);
  VAR lsz,ax,dx,cx,i: CARDINAL; sg: BOOLEAN;
BEGIN
  error(0);
(*
  lsz:=CARDINAL(l^.l^.type^.size); expression(l^.l,a); cmd.load(a,lsz);
  sg:=sign(l^.l^.type);
  IF lsz=4 THEN
    expression(l^.r,a);
    IF a.am IN am_const THEN
      a.n:=a.n MOD 256;
      cmd.pop_reg(1);
      IF a.n>=16 THEN
        IF cmd.pos[cmd.stk-1]=cmd.da THEN cmd.b(cmd.xchg_dx);
        ELSE cmd.b(cmd.xchg_wmr); cmd.b(cmd.md_reg+cmd.CX*8+cmd.BX);
        END;
        IF sg THEN cmd.b(cmd.cwd);
        ELSIF cmd.pos[cmd.stk-1]=cmd.da THEN cmd.b(cmd.mov_dxi); c.w(0);
        ELSE cmd.b(cmd.mov_bxi); cmd.w(0);
        END;
        a.n:=a.n-16;
      END;
      IF a.n<=3 THEN
        IF cmd.pos[cmd.stk-1]=cmd.da THEN ax:=cmd.AX; dx:=cmd.DX;
        ELSE ax:=cmd.CX; dx:=cmd.BX;
        END;
        FOR i:=1 TO CARDINAL(a.n) DO
          cmd.b(cmd.shift_wm1); cmd.b(cmd.md_reg+ax+cmd.s_shl);
          cmd.b(cmd.shift_wm1); cmd.b(cmd.md_reg+dx+cmd.s_rcl);
          cmd.b(cmd.imm_bm); cmd.b(cmd.md_reg+ax+cmd.i_adc); cmd.b(0);
        END;
        a.am:=cmd.am_STK; RETURN;
      END;
    END;
    cmd.load(a,1); cmd.pop_reg(2);
    IF cmd.pos[cmd.stk-1]=cmd.da THEN ax:=cmd.CX; dx:=cmd.BX; cx:=cmd.AX;
    ELSE ax:=cmd.AX; dx:=cmd.DX; cx:=cmd.CX;
    END;
    IF NOT (a.am IN am_const) THEN
      IF rg THEN cmd.b(cmd.grp1_bm); cmd.b(cmd.md_reg+cx+cmd.g1_neg) END;
      cmd.b(cmd.imm_bm); cmd.b(cmd.md_reg+cx+cmd.i_and); cmd.b(1FH);
      cmd.b(cmd.je); cmd.b(11);
    END;
    cmd.b(cmd.shift_wm1); cmd.b(cmd.md_reg+ax+cmd.s_shl);
    cmd.b(cmd.shift_wm1); cmd.b(cmd.md_reg+dx+cmd.s_rcl);
    cmd.b(cmd.imm_bm); cmd.b(cmd.md_reg+ax+cmd.i_adc); cmd.b(0);
    cmd.b(cmd.grp2_b); cmd.b(cmd.md_reg+cx+cmd.g2_dec);
    cmd.b(cmd.jne); cmd.b(word(-11));
    DEC(cmd.stk);
  ELSE
    load(l^.r); cmd.pop_reg(2);
    IF cmd.pos[cmd.stk-1]=cmd.da THEN cmd.b(cmd.xchg_cx) END;
    IF lsz=1 THEN cmd.b(cmd.shift_bmc) ELSE cmd.b(cmd.shift_wmc) END;
    IF l^.sub=pc.ror THEN cmd.b(cmd.md_reg+cmd.s_ror);
    ELSE cmd.b(cmd.md_reg+cmd.s_rol);
    END;
    DEC(cmd.stk); cmd.pos[cmd.stk-1]:=cmd.da;
  END;
  a.am:=cmd.am_STK;
*);
END gen_ash;

PROCEDURE gen_odd(l: pc.NODE; VAR a: cmd.access);
BEGIN
  expression(l^.l,a); cmd.load(a,1); a.am:=cmd.am_STK;
  const.n:=1; cmd.cmd_stk_m(cmd.c_and,const);
END gen_odd;

PROCEDURE gen_cap(l: pc.NODE; VAR a: cmd.access);
BEGIN
  expression(l^.l,a); cmd.load(a,1); a.am:=cmd.am_STK;
  const.n:=0DFH; cmd.cmd_stk_m(cmd.c_and,const);
END gen_cap;

PROCEDURE gen_unary(n: pc.NODE; VAR a: cmd.access);
BEGIN
  CASE n^.sub OF
    |pc.bits    : gen_bits_u(n,a);
    |pc.bytes   : bytes_u(n^.l,a);
    |pc.size    : bytes_u(n^.l,a);
    |pc.high    : high(n^.l,a);
    |pc.len     : len(n^.l,a);
(*    |pc.is      : design(n^.l,x); type_test(x,n^.obj^.type,FALSE,FALSE); *)
    |pc.conv    : gen_type_conversion(n,a);
    |pc.typetran: gen_type_transfer(n,a);
    |pc.abs     : gen_abs(n,a);
    |pc.minus   : gen_neg(n,a);
    |pc.cap     : gen_cap(n,a);
    |pc.odd     : gen_odd(n,a);
    |pc.not     : gen_not(n,a);
    |pc.rcheck  : gen_range_check(n,a);
    |pc.adr     : adr_u(n^.l,a);
  ELSE error(0);
  END;
END gen_unary;

PROCEDURE gen_binary(n: pc.NODE; VAR a: cmd.access);
BEGIN
  CASE n^.sub OF
    |pc.equ  : gen_compare(n,a,cmd.c_z);
    |pc.neq  : gen_compare(n,a,cmd.c_nz);
    |pc.lss  : gen_compare(n,a,cmd.c_l);
    |pc.leq  : gen_compare(n,a,cmd.c_le);
    |pc.gtr  : gen_compare(n,a,cmd.c_g);
    |pc.geq  : gen_compare(n,a,cmd.c_ge);
    |pc.in   : gen_in(n,a);
    |pc.mul  : gen_mul(n,a);
    |pc.div  : gen_div(n,a);
    |pc.mod  : gen_mod(n,a);
    |pc.plus : gen_plus(n,a);
    |pc.minus: gen_minus(n,a);
    |pc.and  : gen_and(n,a);
    |pc.or   : gen_or(n,a);
    |pc.xor  : gen_xor(n,a);
    |pc.bic  : gen_bic(n,a);
    |pc.cand : gen_cand(n,a);
    |pc.cor  : gen_cor(n,a);
    |pc.rol  : gen_rot(n,a);
    |pc.ror  : gen_rot(n,a);
    |pc.ash  : gen_ash(n,a);
  ELSE error(233);
  END;
END gen_binary;

PROCEDURE expression(l: pc.NODE; VAR a: cmd.access);
BEGIN
  CASE l^.mode OF
    |pc.binary: gen_binary(l,a);
    |pc.unary : gen_unary(l,a);
    |pc.call  : gen_call(l); a.am:=cmd.am_STK;
  ELSE designator_u(l,a);
  END;
END expression;

PROCEDURE gen_call_error(l: pc.NODE);
BEGIN
  error(233);
END gen_call_error;

BEGIN
  WITH const DO am:=am_imm; disp:=0; level:=0 END;
  gen_call:=gen_call_error;
END inExp.
