IMPLEMENTATION MODULE inCmds; (* Sem 26-Feb-91. (c) KRONOS *)

IMPORT  pc  : pcTab;
IMPORT  c   : inCmd;
IMPORT  sym : inSym;
IMPORT  vrs : inVars;
IMPORT  eml : inEmul;
IMPORT  fpp : inFPP;
IMPORT  des : inDesig;

IMPORT  tty : Terminal;

TYPE
  am_set=SET OF sym.adr_mode;

CONST
  cop_0=ARRAY OF INTEGER {88h,00b,50b,10b,60b,40b,70b};
  cop_1=ARRAY OF INTEGER {88h,20b,30b,10b,60b,40b,70b};

  am_const = am_set{sym.am_imm,sym.am_Gimm,sym.am_Limm};

PROCEDURE show_stk;
  VAR i: INTEGER;
BEGIN
  tty.print('stack:\n');
  FOR i:=0 TO stk-1 DO
    tty.print('%d ',siz[i]);
    IF pos[i]=da THEN tty.print('dx ax\n');
    ELSIF pos[i]=bc THEN tty.print('bx cx\n');
    ELSE tty.print('save\n');
    END;
  END;
  tty.print('.......\n');
END show_stk;

PROCEDURE alloc_reg(sz: INTEGER);
  VAR n: INTEGER;
BEGIN
  ASSERT(stk>=0);
  n:=stk-1;
  LOOP
    IF (n<0) OR (pos[n]=stack) THEN
      siz[stk]:=sz; pos[stk]:=da; INC(stk); RETURN
    ELSIF pos[n]=da THEN
      LOOP
        DEC(n);
        IF (n<0) OR (pos[n]=stack) THEN
          siz[stk]:=sz; pos[stk]:=bc; INC(stk); RETURN
        ELSIF pos[n]=bc THEN
          IF siz[n]=4 THEN c.b(c.push_bx) END; c.b(c.push_cx); pos[n]:=stack;
          siz[stk]:=sz; pos[stk]:=bc; INC(stk); RETURN
        ELSE ASSERT(FALSE);
        END;
      END;
    ELSIF pos[n]=bc THEN
      LOOP
        DEC(n);
        IF (n<0) OR (pos[n]=stack) THEN
          siz[stk]:=sz; pos[stk]:=da; INC(stk); RETURN
        ELSIF pos[n]=da THEN
          IF siz[n]=4 THEN c.b(c.push_dx) END; c.b(c.push_ax); pos[n]:=stack;
          siz[stk]:=sz; pos[stk]:=da; INC(stk); RETURN
        ELSE ASSERT(FALSE);
        END;
      END;
    END;
    DEC(n);
  END;
END alloc_reg;

PROCEDURE pop_reg(n: INTEGER);
BEGIN
  ASSERT(n IN {1,2});
  ASSERT(n<=stk);
  IF pos[stk-1]=stack THEN
    c.b(c.pop_ax);
    IF siz[stk-1]=4 THEN c.b(c.pop_dx) END;
    pos[stk-1]:=da;
  END;
  IF (n>1) & (pos[stk-2]=stack) THEN
    IF pos[stk-1]=bc THEN
      c.b(c.pop_ax);
      IF siz[stk-2]=4 THEN c.b(c.pop_dx) END;
      pos[stk-2]:=da;
    ELSE
      c.b(c.pop_cx);
      IF siz[stk-2]=4 THEN c.b(c.pop_bx) END;
      pos[stk-2]:=bc;
    END;
  END;
END pop_reg;

PROCEDURE alloc_swap_reg(sz: INTEGER);
BEGIN
  pop_reg(1); alloc_reg(sz); swap;
END alloc_swap_reg;

PROCEDURE movi(n: INTEGER);
BEGIN
  IF pos[stk-1]=da THEN
    CASE siz[stk-1] OF
      |1: c.b(c.mov_ali); c.b(n);
      |2: c.b(c.mov_axi); c.w(n);
      |4: c.b(c.mov_axi); c.w(n); c.b(c.mov_dxi); c.w(n>>16);
    END;
  ELSE
    CASE siz[stk-1] OF
      |1: c.b(c.mov_cli); c.b(n);
      |2: c.b(c.mov_cxi); c.w(n);
      |4: c.b(c.mov_cxi); c.w(n); c.b(c.mov_bxi); c.w(n>>16);
    END;
  END;
END movi;

PROCEDURE set_size(sz: INTEGER; sg: BOOLEAN);
  VAR i: INTEGER;
BEGIN
  i:=siz[stk-1]; siz[stk-1]:=sz;
  IF i>=sz THEN RETURN END;
  pop_reg(1);
  IF sg THEN
    IF pos[stk-1]#da THEN
      IF (siz[stk-1]=4) OR (stk>1) & (pos[stk-2]=da) & (siz[stk-2]=4) THEN
        c.b(c.xchg_wmr); c.b(c.md_reg+c.BX+c.DX*8);
      END;
      c.b(c.xchg_cx);
      pos[stk-1]:=da;
      IF (stk>1) & (pos[stk-2]=da) THEN pos[stk-2]:=bc END;
    END;
    IF i=1 THEN c.b(c.cbw); i:=2 END;
    IF (i=2) & (sz=4) THEN c.b(c.cwd); i:=4 END;
  ELSE
    IF i=1 THEN
      IF pos[stk-1]=da THEN c.b(c.mov_ahi); c.b(0)
      ELSE c.b(c.mov_chi); c.b(0);
      END;
      i:=2;
    END;
    IF (i=2) & (sz=4) THEN
      IF pos[stk-1]=da THEN c.b(c.mov_dxi); c.w(0);
      ELSE c.b(c.mov_bxi); c.w(0);
      END;
    END;
  END;
END set_size;

PROCEDURE put_access(am,sr,ofs,reg: INTEGER; SEQ cop: INTEGER);
  VAR i: INTEGER; sgo: BOOLEAN;
BEGIN
  sgo:=(am IN {2,3,6}) & (sr#2) OR (am IN {0,1,4,5,7,8}) & (sr#3);
  IF sgo THEN c.b(46b+sr*8) END;
  FOR i:=0 TO HIGH(cop) DO c.b(cop[i]) END;
  ofs:=ofs MOD 10000h;
  IF am=8 THEN c.b(006b+reg); c.w(ofs)
  ELSIF (am#6) & (ofs=0) THEN c.b(am+reg);
  ELSIF (ofs<80h) OR (ofs>=0FF80h) THEN c.b(am+reg+100b); c.b(ofs);
  ELSE c.b(am+reg+200b); c.w(ofs);
  END;
END put_access;

PROCEDURE put_cmd(VAL d: cmd_desc; am,sr,ofs: INTEGER);
  PROCEDURE put_ofs(rr,n: INTEGER);
  BEGIN
    n:=n MOD 10000h;
    IF am=8 THEN c.b(006b+rr); c.w(n)
    ELSIF d.cop=c_cmp THEN c.b(am+rr+200b); c.w(n)
    ELSIF (am#6) & (n=0) THEN c.b(am+rr);
    ELSIF (n<80h) OR (n>=0FF80h) THEN c.b(am+rr+100b); c.b(n);
    ELSE c.b(am+rr+200b); c.w(n);
    END;
  END put_ofs;
  VAR n,x1,x0: INTEGER; ax,sgo: BOOLEAN;
BEGIN
  ASSERT((ofs>=0) & (ofs<=10000h-d.size));
  sgo:=(am IN {2,3,6}) & (sr#2) OR (am IN {0,1,4,5,7,8}) & (sr#3);
  x0:=cop_0[ORD(d.cop)]; x1:=cop_1[ORD(d.cop)];
  IF (d.mode=m_rm) OR (d.mode=m_mr) THEN
    ax:=(d.cop=c_mov) & (d.r0=0) & (am=8);
    n:=ORD(d.size>1)+ORD(d.mode=m_rm)*2;
    IF (d.size>2) & (d.cop=c_cmp) THEN
      IF sgo THEN c.b(46b+sr*8) END;
      c.b(x1+n); put_ofs(d.r1*8,ofs+2); c.b(c.jne);
      IF sgo THEN c.b(5) ELSE c.b(4) END;
    END;
    IF sgo THEN c.b(46b+sr*8) END;
    IF ax THEN c.b(c.mov_alm+ORD(d.size>1)+ORD(d.mode=m_mr)*2); c.w(ofs);
    ELSE c.b(x0+n); put_ofs(d.r0*8,ofs);
    END;
    IF (d.size>2) & (d.cop#c_cmp) THEN
      IF sgo THEN c.b(46b+sr*8) END;
      c.b(x1+n); put_ofs(d.r1*8,ofs+2);
    END;
  ELSIF (d.mode=m_mi) & (d.cop=c_mov) THEN
    IF sgo THEN c.b(46b+sr*8) END;
    CASE d.size OF
      |1: c.b(c.mov_bmi); put_ofs(0,ofs); c.b(d.val);
      |2: c.b(c.mov_wmi); put_ofs(0,ofs); c.w(d.val);
      |4: c.b(c.mov_wmi); put_ofs(0,ofs); c.w(d.val);
          IF sgo THEN c.b(46b+sr*8) END;
          c.b(c.mov_wmi); put_ofs(0,ofs+2); c.w(d.val>>16);
    END;
  ELSIF d.mode=m_sm THEN
    ASSERT(d.cop=c_mov);
    IF sgo THEN c.b(46b+sr*8) END;
    CASE d.size OF
      |2: c.b(c.grp2_w); put_ofs(c.g2_push,ofs);
      |4: c.b(c.grp2_w); put_ofs(c.g2_push,ofs+2);
          IF sgo THEN c.b(46b+sr*8) END;
          c.b(c.grp2_w); put_ofs(c.g2_push,ofs);
    END;
  ELSIF (d.mode=m_mi) & (d.cop=c_add) & (d.size<4) & (d.val=1) THEN
    IF sgo THEN c.b(46b+sr*8) END;
    CASE d.size OF
      |1: c.b(c.grp2_b); put_ofs(c.g2_inc,ofs);
      |2: c.b(c.grp2_w); put_ofs(c.g2_inc,ofs);
    END;
  ELSIF (d.mode=m_mi) & (d.cop=c_sub) & (d.size<4) & (d.val=1) THEN
    IF sgo THEN c.b(46b+sr*8) END;
    CASE d.size OF
      |1: c.b(c.grp2_b); put_ofs(c.g2_dec,ofs);
      |2: c.b(c.grp2_w); put_ofs(c.g2_dec,ofs);
    END;
  ELSIF d.mode=m_mi THEN
    CASE d.size OF
      |1: IF (d.cop=c_xor) & (BITSET(d.val)*{0..7}={}) THEN RETURN END;
          IF (d.cop=c_or ) & (BITSET(d.val)*{0..7}={}) THEN RETURN END;
          IF (d.cop=c_and) & (BITSET(d.val)*{0..7}={0..7}) THEN RETURN END;
          IF sgo THEN c.b(46b+sr*8) END;
          c.b(c.imm_bm); put_ofs(x0,ofs); c.b(d.val);
      |2: IF (d.cop=c_xor) & (BITSET(d.val)*{0..15}={}) THEN RETURN END;
          IF (d.cop=c_or ) & (BITSET(d.val)*{0..15}={}) THEN RETURN END;
          IF (d.cop=c_and) & (BITSET(d.val)*{0..15}={0..15}) THEN RETURN END;
          IF sgo THEN c.b(46b+sr*8) END;
          c.b(c.imm_wm); put_ofs(x0,ofs); c.w(d.val);
      |4: IF d.cop=c_cmp THEN
            IF sgo THEN c.b(46b+sr*8) END;
            c.b(c.imm_wm); put_ofs(x1,ofs+2); c.w(d.val>>16); c.b(c.jne);
            IF sgo THEN c.b(7); c.b(46b+sr*8) ELSE c.b(6) END;
            c.b(c.imm_wm); put_ofs(x0,ofs); c.w(d.val);
          ELSE
            IF (d.cop=c_xor) & (BITSET(d.val)*{0..15}={}) THEN
            ELSIF (d.cop=c_or ) & (BITSET(d.val)*{0..15}={}) THEN
            ELSIF (d.cop=c_and) & (BITSET(d.val)*{0..15}={0..15}) THEN
            ELSE
              IF sgo THEN c.b(46b+sr*8) END;
              c.b(c.imm_wm); put_ofs(x0,ofs); c.w(d.val);
            END;
            IF (d.cop=c_xor) & (BITSET(d.val)*{16..31}={}) THEN RETURN END;
            IF (d.cop=c_or ) & (BITSET(d.val)*{16..31}={}) THEN RETURN END;
            IF (d.cop=c_and) & (BITSET(d.val)*{16..31}={16..31}) THEN RETURN END;
            IF sgo THEN c.b(46b+sr*8) END;
            c.b(c.imm_wm); put_ofs(x1,ofs+2); c.w(d.val>>16);
          END;
    END;
  ELSE ASSERT(FALSE);
  END;
END put_cmd;

PROCEDURE adr_carry(r0,r1: INTEGER);
BEGIN
  IF pc.cpu_mode=0 THEN
    c.b(c.jns); c.b(8);
    c.b(c.imm_wm); c.b(c.md_reg+r0+c.i_and); c.w(7FFFh);
    c.b(c.imm_wm); c.b(c.md_reg+r1+c.i_add); c.w(800h);
  ELSE
    c.b(c.jns); c.b(7);
    c.b(c.imm_wm); c.b(c.md_reg+r0+c.i_and); c.w(7FFFh);
    c.b(c.imm_wbm); c.b(c.md_reg+r1+c.i_add); c.b(8h);
  END;
END adr_carry;

PROCEDURE cmd_imm(VAL d: cmd_desc; n: INTEGER);
  VAR n0,n1: INTEGER;
BEGIN
  IF d.mode=m_sm THEN
    IF d.size=4 THEN c.b(c.push_wi); c.w(n>>16) END;
    c.b(c.push_wi); c.w(n); RETURN
  ELSIF d.mode=m_ri THEN n:=d.val
  ELSE ASSERT(d.mode=m_rm);
  END;
  IF d.cop=c_mov THEN
    CASE d.size OF
      |1: c.b(c.mov_ali+d.r0); c.b(n);
      |2: c.b(c.mov_axi+d.r0); c.w(n);
      |4: c.b(c.mov_axi+d.r0); c.w(n); c.b(c.mov_axi+d.r1); c.w(n>>16);
    END;
  ELSE
    CASE siz[stk-1] OF
      |1: n:=n MOD 100h;
          IF (d.cop=c_xor) & (n=0) THEN RETURN END;
          IF (d.cop=c_or ) & (n=0) THEN RETURN END;
          IF (d.cop=c_and) & (n=0FFh) THEN RETURN END;
          c.b(c.imm_bm); c.b(c.md_reg+d.r0+cop_0[ORD(d.cop)]); c.b(n);
      |2: n:=n MOD 10000h;
          IF (d.cop=c_xor) & (n=0) THEN RETURN END;
          IF (d.cop=c_or ) & (n=0) THEN RETURN END;
          IF (d.cop=c_and) & (n=0FFFFh) THEN RETURN END;
          IF (n<=7Fh) OR (n>=0FF80h) THEN
            c.b(c.imm_wbm); c.b(c.md_reg+d.r0+cop_0[ORD(d.cop)]); c.b(n);
          ELSE
            c.b(c.imm_wm); c.b(c.md_reg+d.r0+cop_0[ORD(d.cop)]); c.w(n);
          END;
      |4: IF d.cop=c_cmp THEN
            c.b(c.imm_wm); c.b(c.md_reg+d.r1+cop_1[ORD(d.cop)]); c.w(n>>16);
            c.b(c.jne); c.b(4);
            c.b(c.imm_wm); c.b(c.md_reg+d.r0+cop_0[ORD(d.cop)]); c.w(n);
          ELSE
            n0:=n MOD 10000h; n1:=(n>>16) MOD 10000h;
            IF (d.cop=c_xor) & (n0=0) THEN
            ELSIF (d.cop=c_or ) & (n0=0) THEN
            ELSIF (d.cop=c_and) & (n0=0FFFFh) THEN
            ELSIF (n0<=7Fh) OR (n0>=0FF80h) THEN
              c.b(c.imm_wbm); c.b(c.md_reg+d.r0+cop_0[ORD(d.cop)]); c.b(n0);
            ELSE
              c.b(c.imm_wm); c.b(c.md_reg+d.r0+cop_0[ORD(d.cop)]); c.w(n0);
            END;
            IF (d.cop=c_xor) & (n1=0) THEN RETURN END;
            IF (d.cop=c_or ) & (n1=0) THEN RETURN END;
            IF (d.cop=c_and) & (n1=0FFFFh) THEN RETURN END;
            IF (n1<=7Fh) OR (n1>=0FF80h) THEN
              c.b(c.imm_wbm); c.b(c.md_reg+d.r1+cop_1[ORD(d.cop)]); c.b(n1);
            ELSE
              c.b(c.imm_wm); c.b(c.md_reg+d.r1+cop_1[ORD(d.cop)]); c.w(n1);
            END;
          END;
    END;
  END;
END cmd_imm;

PROCEDURE mov_es_si(VAR ofs: INTEGER);
  VAR sg: INTEGER;
BEGIN
  IF pc.cpu_mode=0 THEN sg:=ofs DIV 8000h * 800h;
  ELSE sg:=ofs DIV 8000h * 8;
  END;
  IF (sg>=-128) & (sg<=127) THEN
    c.b(c.imm_wbm); c.b(c.md_reg+c.SI+c.i_add); c.b(sg);
  ELSE
    c.b(c.imm_wm); c.b(c.md_reg+c.SI+c.i_add); c.w(sg);
  END;
  c.b(c.mov_srm); c.b(c.md_reg+c.SI+c.ES*8);
  ofs:=ofs MOD 8000h;
END mov_es_si;

PROCEDURE mov_es_ds(VAR ofs: INTEGER);
BEGIN
  c.b(c.mov_msr); c.b(c.md_reg+c.SI+c.DS*8);
  mov_es_si(ofs);
END mov_es_ds;

PROCEDURE mov_es_external_ds(level,size: INTEGER; VAR disp: INTEGER);
  VAR ofs: INTEGER;
BEGIN
  ASSERT(level>0);
  ofs:=vrs.mdls[level].ofs;
  IF (ofs>=0) & (ofs<=0FFFEh) THEN
    IF (disp>=0) & (disp<=10000h-size) THEN
      c.b(c.mov_srm); c.b(c.md_abs+c.ES*8); c.w(ofs);
    ELSE
      c.b(c.mov_wrm); c.b(c.md_abs+c.SI*8); c.w(ofs);
      mov_es_si(disp);
    END;
  ELSE
    c.b(c.mov_msr); c.b(c.md_reg+c.SI+c.DS*8);
    mov_es_si(ofs);
    c.b(46b+c.ES*8);
    IF (disp>=0) & (disp<=10000h-size) THEN
      c.b(c.mov_srm); c.b(c.md_abs+c.ES*8); c.w(ofs);
    ELSE
      c.b(c.mov_wrm); c.b(c.md_abs+c.SI*8); c.w(ofs);
      mov_es_si(disp);
    END;
  END;
END mov_es_external_ds;

PROCEDURE cmd_stk(size: INTEGER; VAR am,sr,ofs: INTEGER);
  VAR ax,dx,sg: INTEGER;
BEGIN
  DEC(stk);
  IF (ofs<0) OR (ofs>8001h-size) THEN
    IF pos[stk]=da THEN ax:=c.AX; dx:=c.DX
    ELSIF pos[stk]=bc THEN ax:=c.CX; dx:=c.BX
    ELSE c.b(c.pop_si); ax:=c.SI; c.b(c.pop_di); dx:=c.DI;
    END;
    IF pc.cpu_mode=0 THEN sg:=ofs DIV 8000h * 800h;
    ELSE sg:=ofs DIV 8000h * 8;
    END;
    ofs:=ofs MOD 8000h;
    IF (sg<-128) OR (sg>127) THEN
      c.b(c.imm_wm); c.b(c.md_reg+dx+c.i_add); c.w(sg);
    ELSIF sg#0 THEN
      c.b(c.imm_wbm); c.b(c.md_reg+dx+c.i_add); c.b(sg);
    END;
    IF ofs > 8001h-size THEN
      ASSERT(size=4);
      c.b(c.imm_wm); c.b(c.md_reg+ax+c.i_add); c.w(ofs); ofs:=0;
      adr_carry(ax,dx);
    END;
  ELSE
    IF pos[stk]=da THEN ax:=c.AX; dx:=c.DX
    ELSIF pos[stk]=bc THEN ax:=c.CX; dx:=c.BX
    ELSE c.b(c.pop_si); ax:=c.SI; c.b(c.pop_es); dx:=-1;
    END;
  END;
  IF dx>=0 THEN c.b(c.mov_srm); c.b(c.md_reg+dx+c.ES*8) END;
  IF ax#c.SI THEN c.b(c.mov_wrm); c.b(c.md_reg+ax+c.SI*8) END;
  am:=c.rm_si; sr:=c.ES;
END cmd_stk;

PROCEDURE cmd_ass(si: BOOLEAN; ofs0,size: INTEGER; VAR am,sr,ofs: INTEGER);
  VAR sg,rr: INTEGER;
BEGIN
  ofs0:=ofs0 MOD 10000h;
  IF si THEN rr:=c.rm_si; c.b(46b+c.SS*8) ELSE rr:=c.rm_bp END;
  IF (ofs>=0) & (ofs<=8001h-size) THEN
    c.b(c.les);
    IF (ofs0<80h) OR (ofs0>=0FF80h) THEN c.b(c.md_b+rr+c.SI*8); c.b(ofs0);
    ELSE c.b(c.md_w+rr+c.SI*8); c.w(ofs0);
    END;
  ELSE
    IF pc.cpu_mode=0 THEN sg:=ofs DIV 8000h * 800h;
    ELSE sg:=ofs DIV 8000h * 8;
    END;
    ofs:=ofs MOD 8000h;
    c.b(c.mov_wrm); c.b(c.md_w+rr+c.DI*8); c.w(ofs0+2);
    IF (sg<-128) OR (sg>127) THEN
      c.b(c.imm_wm); c.b(c.md_reg+c.DI+c.i_add); c.w(sg);
    ELSIF sg#0 THEN
      c.b(c.imm_wbm); c.b(c.md_reg+c.DI+c.i_add); c.b(sg);
    END;
    IF si THEN c.b(46b+c.SS*8) END;
    c.b(c.mov_wrm); c.b(c.md_w+rr+c.SI*8); c.w(ofs0);
    IF ofs <= 8001h-size THEN
      c.b(c.mov_srm); c.b(c.md_reg+c.DI+c.ES*8);
    ELSE
      ASSERT(size=4);
      c.b(c.imm_wm); c.b(c.md_reg+c.SI+c.i_add); c.w(ofs); ofs:=0;
      adr_carry(c.SI,c.DI);
      c.b(c.mov_srm); c.b(c.md_reg+c.DI+c.ES*8);
    END;
  END;
  am:=c.rm_si; sr:=c.ES;
END cmd_ass;

PROCEDURE parm_ofs(VAR ofs: INTEGER; lvl: INTEGER);
  VAR p: pc.ref;
BEGIN
  p:=proc;
  LOOP
--tty.print('%d %d %d\n',lvl,vrs.prcs[p^.adr].lvl,vrs.prcs[p^.adr].no);
    ASSERT(p^.md=pc.procedure);
    ASSERT(vrs.prcs[p^.adr].mod=0);
    IF vrs.prcs[p^.adr].lvl=lvl THEN EXIT END;
    ASSERT(vrs.prcs[p^.adr].slink);
--p:=p;
    ASSERT(NOT vrs.prcs[p^.adr].export);
    p:=p^.r;
  END;
  IF vrs.prcs[p^.adr].export THEN INC(ofs,8);
  ELSIF vrs.prcs[p^.adr].slink THEN INC(ofs,6);
  ELSE INC(ofs,4);
  END;
END parm_ofs;

PROCEDURE gen_access(VAL a: sym.access; size: INTEGER; VAR am,sr,ofs: INTEGER);
  VAR sg,i,ofs0: INTEGER;
BEGIN
  CASE a.am OF
    |sym.am_imm,sym.am_Gimm,sym.am_Limm: am:=9; ofs:=a.n;
    |sym.am_abs:
      IF pc.cpu_mode=0 THEN sg:=a.disp DIV 8000h * 800h;
      ELSE sg:=a.disp DIV 8000h * 8;
      END;
      c.b(c.mov_sii); c.w(sg);
      c.b(c.mov_srm); c.b(c.md_reg+c.ES*8+c.SI);
      am:=8; sr:=c.ES; ofs:=a.disp MOD 8000h;
    |sym.am_G,sym.am_Gstr:
      am:=8; ofs:=a.disp;
      IF a.level=0 THEN
        IF (ofs>=0) & (ofs<=10000h-size) THEN sr:=c.DS;
        ELSE mov_es_ds(ofs); sr:=c.ES;
        END;
      ELSE
        mov_es_external_ds(a.level,size,ofs); sr:=c.ES;
      END;
    |sym.am_L,sym.am_PB2,sym.am_PB4,sym.am_PB8:
      ASSERT(a.level>=1);
      ofs:=a.disp;
      IF a.am=sym.am_PB2 THEN parm_ofs(ofs,a.level) END;
      IF a.am=sym.am_PB4 THEN parm_ofs(ofs,a.level) END;
      IF a.am=sym.am_PB8 THEN parm_ofs(ofs,a.level) END;
      ofs:=ofs MOD 10000h;
      IF a.level=vrs.prcs[proc^.adr].lvl THEN
        am:=c.rm_bp; sr:=c.SS;
      ELSE
        ASSERT(vrs.prcs[proc^.adr].slink);
        ASSERT(NOT vrs.prcs[proc^.adr].export);
        c.b(c.mov_wrm); c.b(c.md_b+c.SI*8+c.rm_bp); c.b(4);
        FOR i:=a.level+2 TO vrs.prcs[proc^.adr].lvl DO
          c.b(46b+c.SS*8); c.b(c.mov_wrm); c.b(c.md_b+c.SI*8+c.rm_si); c.b(4);
        END;
        am:=c.rm_si; sr:=c.SS;
      END;
    |sym.am_aG:
      ofs:=a.disp; ofs0:=a.n;
      IF (ofs>=0) & (ofs<=8001h-size) THEN
        IF a.level#0 THEN
          mov_es_external_ds(a.level,4,ofs0); c.b(c.seg_es);
        ELSIF (ofs0<0) OR (ofs0>0FFFCh) THEN
          mov_es_ds(ofs0); c.b(c.seg_es);
        END;
        c.b(c.les); c.b(c.md_abs+c.SI*8); c.w(ofs0);
        am:=c.rm_si; sr:=c.ES;
      ELSE
        IF a.level#0 THEN
          mov_es_external_ds(a.level,4,ofs0);
          c.b(c.seg_es); c.b(c.mov_wrm); c.b(c.md_abs+c.SI*8); c.w(ofs0);
          c.b(c.seg_es); c.b(c.mov_wrm); c.b(c.md_abs+c.DI*8); c.w(ofs0+2);
        ELSIF (ofs0<0) OR (ofs0>0FFFCh) THEN
          mov_es_ds(ofs0);
          c.b(c.seg_es); c.b(c.mov_wrm); c.b(c.md_abs+c.SI*8); c.w(ofs0);
          c.b(c.seg_es); c.b(c.mov_wrm); c.b(c.md_abs+c.DI*8); c.w(ofs0+2);
        ELSE
          c.b(c.mov_wrm); c.b(c.md_abs+c.SI*8); c.w(ofs0);
          c.b(c.mov_wrm); c.b(c.md_abs+c.DI*8); c.w(ofs0+2);
        END;
        IF pc.cpu_mode=0 THEN sg:=ofs DIV 8000h * 800h;
        ELSE sg:=ofs DIV 8000h * 8;
        END;
        ofs:=ofs MOD 8000h;
        IF (sg<-128) OR (sg>127) THEN
          c.b(c.imm_wm); c.b(c.md_reg+c.DI+c.i_add); c.w(sg);
        ELSIF sg#0 THEN
          c.b(c.imm_wbm); c.b(c.md_reg+c.DI+c.i_add); c.b(sg);
        END;
        IF ofs > 8001h-size THEN
          c.b(c.imm_wm); c.b(c.md_reg+c.SI+c.i_add); c.w(ofs); ofs:=0;
          adr_carry(c.SI,c.DI);
        END;
        c.b(c.mov_srm); c.b(c.md_reg+c.DI+c.ES*8);
        am:=c.rm_si; sr:=c.ES;
      END;
    |sym.am_aSTK: ofs:=a.disp; cmd_stk(size,am,sr,ofs);
    |sym.am_aL,sym.am_aPB:
      ASSERT(a.level>=1);
      ofs0:=a.n; ofs:=a.disp;
      IF a.am=sym.am_aPB THEN parm_ofs(ofs0,a.level) END;
      IF a.level=vrs.prcs[proc^.adr].lvl THEN
        cmd_ass(FALSE,ofs0,size,am,sr,ofs);
      ELSE
        ASSERT(vrs.prcs[proc^.adr].slink);
        ASSERT(NOT vrs.prcs[proc^.adr].export);
        c.b(c.mov_wrm); c.b(c.md_b+c.SI*8+c.rm_bp); c.b(4);
        FOR i:=a.level+2 TO vrs.prcs[proc^.adr].lvl DO
          c.b(46b+c.SS*8); c.b(c.mov_wrm); c.b(c.md_b+c.SI*8+c.rm_si); c.b(4);
        END;
        cmd_ass(TRUE,ofs0,size,am,sr,ofs);
      END;
  ELSE tty.print('gen access, am = %d\n',a.am); HALT;
  END;
END gen_access;

PROCEDURE gen_cmd(VAL d: cmd_desc; VAL a: sym.access);
  VAR am,sr,ofs: INTEGER;
BEGIN
  gen_access(a,d.size,am,sr,ofs);
  IF am=9 THEN cmd_imm(d,ofs);
  ELSE put_cmd(d,am,sr,ofs);
  END;
END gen_cmd;

PROCEDURE copt;
BEGIN
  alloc_reg(siz[stk-1]); pop_reg(2);
  IF pos[stk-1]=da THEN
    CASE siz[stk-1] OF
      |1: c.b(c.mov_brm); c.b(c.md_reg+c.CL+c.AL*8);
      |2: c.b(c.mov_wrm); c.b(c.md_reg+c.CX+c.AX*8);
      |4: c.b(c.mov_wrm); c.b(c.md_reg+c.CX+c.AX*8);
          c.b(c.mov_wrm); c.b(c.md_reg+c.BX+c.DX*8);
    END;
  ELSE
    CASE siz[stk-1] OF
      |1: c.b(c.mov_brm); c.b(c.md_reg+c.AL+c.CL*8);
      |2: c.b(c.mov_wrm); c.b(c.md_reg+c.AX+c.CX*8);
      |4: c.b(c.mov_wrm); c.b(c.md_reg+c.AX+c.CX*8);
          c.b(c.mov_wrm); c.b(c.md_reg+c.DX+c.BX*8);
    END;
  END;
END copt;

PROCEDURE cmd_stk_stk(cc: cmd_cop);
BEGIN
  pop_reg(2); DEC(stk);
  IF pos[stk]=da THEN
    CASE siz[stk] OF
      |1: c.b(cop_0[ORD(cc)]);   c.b(c.md_reg+c.CL+c.AL*8);
      |2: c.b(cop_0[ORD(cc)]+1); c.b(c.md_reg+c.CX+c.AX*8);
      |4: c.b(cop_0[ORD(cc)]+1); c.b(c.md_reg+c.CX+c.AX*8);
          c.b(cop_1[ORD(cc)]+1); c.b(c.md_reg+c.BX+c.DX*8);
    END;
  ELSE
    CASE siz[stk] OF
      |1: c.b(cop_0[ORD(cc)]);   c.b(c.md_reg+c.AL+c.CL*8);
      |2: c.b(cop_0[ORD(cc)]+1); c.b(c.md_reg+c.AX+c.CX*8);
      |4: c.b(cop_0[ORD(cc)]+1); c.b(c.md_reg+c.AX+c.CX*8);
          c.b(cop_1[ORD(cc)]+1); c.b(c.md_reg+c.DX+c.BX*8);
    END;
  END;
END cmd_stk_stk;

PROCEDURE swap_no_pop;
  VAR r: reg; s: INTEGER;
BEGIN
  r:=pos[stk-1]; pos[stk-1]:=pos[stk-2]; pos[stk-2]:=r;
  s:=siz[stk-1]; siz[stk-1]:=siz[stk-2]; siz[stk-2]:=s;
END swap_no_pop;

PROCEDURE cmd_stk_m(cc: cmd_cop; a: sym.access);
  VAR d: cmd_desc;
BEGIN
  IF a.am=sym.am_STK THEN cmd_stk_stk(cc); RETURN END;
  IF a.am=sym.am_FPP THEN des.alloc_var(a,4); fpp.store(a,4) END;
  d.cop:=cc; d.size:=siz[stk-1]; d.mode:=m_rm;
  IF a.am=sym.am_aSTK THEN
    d.size:=siz[stk-2];
    IF pos[stk-2]=da THEN d.r0:=c.AX; d.r1:=c.DX
    ELSIF pos[stk-2]=bc THEN d.r0:=c.CX; d.r1:=c.BX
    ELSIF pos[stk-1]=da THEN
      d.r0:=c.CX; d.r1:=c.BX; pos[stk-2]:=bc; c.b(c.pop_cx);
      IF siz[stk-2]=4 THEN c.b(c.pop_bx) END;
    ELSE
      d.r0:=c.AX; d.r1:=c.DX; pos[stk-2]:=da; c.b(c.pop_ax);
      IF siz[stk-1]=4 THEN c.b(c.pop_dx) END;
    END;
  ELSIF pos[stk-1]=da THEN d.r0:=c.AX; d.r1:=c.DX
  ELSIF pos[stk-1]=bc THEN d.r0:=c.CX; d.r1:=c.BX
  ELSE
    d.r0:=c.AX; d.r1:=c.DX; pos[stk-1]:=da; c.b(c.pop_ax);
    IF siz[stk-1]=4 THEN c.b(c.pop_dx) END;
  END;
  gen_cmd(d,a);
END cmd_stk_m;

PROCEDURE cmd_m_stk(cc: cmd_cop; a: sym.access);
  VAR d: cmd_desc;
BEGIN
  IF a.am=sym.am_STK THEN swap; cmd_stk_stk(cc); RETURN END;
  IF a.am=sym.am_FPP THEN des.alloc_var(a,4); fpp.store(a,4) END;
  ASSERT(a.am#sym.am_imm);
  pop_reg(1); d.cop:=cc; d.size:=siz[stk-1]; d.mode:=m_mr;
  IF pos[stk-1]=da THEN d.r0:=c.AX; d.r1:=c.DX
  ELSE                  d.r0:=c.CX; d.r1:=c.BX
  END;
  IF a.am=sym.am_aSTK THEN swap_no_pop END;
  gen_cmd(d,a);
END cmd_m_stk;

PROCEDURE cmd_m_imm(cc: cmd_cop; VAL a: sym.access; val,sz: INTEGER);
  VAR d: cmd_desc;
BEGIN
  IF a.am=sym.am_STK THEN
    ASSERT(siz[stk-1]=sz);
    pop_reg(1); d.cop:=cc; d.size:=sz; d.mode:=m_ri; d.val:=val;
    IF pos[stk-1]=da THEN d.r0:=c.AX; d.r1:=c.DX
    ELSE                  d.r0:=c.CX; d.r1:=c.BX
    END;
  ELSE
    ASSERT(a.am#sym.am_imm);
    d.cop:=cc; d.size:=sz; d.mode:=m_mi; d.val:=val;
  END;
  gen_cmd(d,a);
END cmd_m_imm;

PROCEDURE load(VAL a: sym.access; sz: INTEGER);
BEGIN
  IF a.am=sym.am_STK THEN RETURN END;
  alloc_reg(sz);
  IF a.am=sym.am_aSTK THEN swap END;
  cmd_stk_m(c_mov,a);
END load;

PROCEDURE store(VAL to: sym.access);
BEGIN
  cmd_m_stk(c_mov,to); DEC(stk);
END store;

PROCEDURE load_store(VAL to,fr: sym.access; sz: INTEGER);
  VAR d: cmd_desc;
BEGIN
  IF (fr.am=sym.am_imm) OR (fr.am=sym.am_Gimm) OR (fr.am=sym.am_Limm) THEN
    d.cop:=c_mov; d.size:=sz; d.val:=fr.n; d.mode:=m_mi; gen_cmd(d,to);
  ELSE load(fr,sz); store(to);
  END;
END load_store;

PROCEDURE swap;
  VAR r: reg; s: INTEGER;
BEGIN
  pop_reg(2);
  r:=pos[stk-1]; pos[stk-1]:=pos[stk-2]; pos[stk-2]:=r;
  s:=siz[stk-1]; siz[stk-1]:=siz[stk-2]; siz[stk-2]:=s;
END swap;

PROCEDURE set_flag_z;
BEGIN
  pop_reg(1);
  IF pos[stk-1]=da THEN
    CASE siz[stk-1] OF
      |1: c.b(c.and_brm); c.b(c.md_reg+c.AL+c.AL*8);
      |2: c.b(c.and_wrm); c.b(c.md_reg+c.AX+c.AX*8);
      |4: c.b(c.or_wrm); c.b(c.md_reg+c.DX+c.AX*8);
    END;
  ELSE
    CASE siz[stk-1] OF
      |1: c.b(c.and_brm); c.b(c.md_reg+c.CL+c.CL*8);
      |2: c.b(c.and_wrm); c.b(c.md_reg+c.CX+c.CX*8);
      |4: c.b(c.or_wrm); c.b(c.md_reg+c.BX+c.CX*8);
    END;
  END;
  DEC(stk);
  ASSERT(stk>=0);
END set_flag_z;

PROCEDURE add_adr_const(ofs: INTEGER);
  VAR sg,ax,dx: INTEGER;
BEGIN
  IF ofs=0 THEN RETURN END;
  pop_reg(1);
  IF pc.cpu_mode=0 THEN sg:=ofs DIV 8000h * 800h;
  ELSE sg:=ofs DIV 8000h * 8;
  END;
  ofs:=ofs MOD 8000h;
  IF pos[stk-1]=da THEN ax:=c.AX; dx:=c.DX ELSE ax:=c.CX; dx:=c.BX END;
  IF ofs#0 THEN
    IF (ofs<=7Fh) OR (ofs>=0FF80h) THEN
      c.b(c.imm_wbm); c.b(c.md_reg+ax+c.i_add); c.b(ofs);
    ELSE
      c.b(c.imm_wm); c.b(c.md_reg+ax+c.i_add); c.w(ofs);
    END;
    IF pc.cpu_mode=0 THEN
      c.b(c.jns); c.b(8);
      c.b(c.imm_wm); c.b(c.md_reg+dx+c.i_add); c.w(800h);
    ELSE
      c.b(c.jns); c.b(7);
      c.b(c.imm_wbm); c.b(c.md_reg+dx+c.i_add); c.b(8);
    END;
    c.b(c.imm_wm); c.b(c.md_reg+ax+c.i_and); c.w(7FFFh);
  END;
  IF sg=0 THEN RETURN
  ELSIF (sg>=-128) & (sg<=127) THEN
    c.b(c.imm_wbm); c.b(c.md_reg+dx+c.i_add); c.b(sg);
  ELSE
    c.b(c.imm_wm); c.b(c.md_reg+dx+c.i_add); c.w(sg);
  END;
END add_adr_const;

PROCEDURE sub_adr_const(ofs: INTEGER);
  VAR sg,ax,dx: INTEGER;
BEGIN
  IF ofs=0 THEN RETURN END;
  pop_reg(1);
  IF pc.cpu_mode=0 THEN sg:=ofs DIV 8000h * 800h;
  ELSE sg:=ofs DIV 8000h * 8;
  END;
  ofs:=ofs MOD 8000h;
  IF pos[stk-1]=da THEN ax:=c.AX; dx:=c.DX ELSE ax:=c.CX; dx:=c.BX END;
  IF ofs#0 THEN
    IF (ofs<=7Fh) OR (ofs>=0FF80h) THEN
      c.b(c.imm_wbm); c.b(c.md_reg+ax+c.i_sub); c.b(ofs);
    ELSE
      c.b(c.imm_wm); c.b(c.md_reg+ax+c.i_sub); c.w(ofs);
    END;
    IF pc.cpu_mode=0 THEN
      c.b(c.jns); c.b(8);
      c.b(c.imm_wm); c.b(c.md_reg+dx+c.i_sub); c.w(800h);
    ELSE
      c.b(c.jns); c.b(7);
      c.b(c.imm_wbm); c.b(c.md_reg+dx+c.i_sub); c.b(8);
    END;
    c.b(c.imm_wm); c.b(c.md_reg+ax+c.i_and); c.w(7FFFh);
  END;
  IF sg=0 THEN RETURN
  ELSIF (sg>=-128) & (sg<=127) THEN
    c.b(c.imm_wbm); c.b(c.md_reg+dx+c.i_sub); c.b(sg);
  ELSE
    c.b(c.imm_wm); c.b(c.md_reg+dx+c.i_sub); c.w(sg);
  END;
END sub_adr_const;

PROCEDURE load_adr(VAL a: sym.access);
  VAR sg,ax,dx,ofs,i: INTEGER; d: cmd_desc;
BEGIN
  IF a.am=sym.am_aSTK THEN add_adr_const(a.disp); RETURN END;
  alloc_reg(4);
  IF pos[stk-1]=da THEN ax:=c.AX; dx:=c.DX ELSE ax:=c.CX; dx:=c.BX END;
  d.mode:=m_rm; d.size:=4; d.cop:=c_mov; d.r0:=ax; d.r1:=dx;
  CASE a.am OF
    |sym.am_abs :
      IF pc.cpu_mode=0 THEN sg:=a.disp DIV 8000h * 800h
      ELSE sg:=a.disp DIV 8000h * 8;
      END;
      movi(sg*10000h+a.disp MOD 8000h);
    |sym.am_G,sym.am_Gimm,sym.am_Gstr:
      IF pc.cpu_mode=0 THEN sg:=a.disp DIV 8000h * 800h
      ELSE sg:=a.disp DIV 8000h * 8;
      END;
      IF pos[stk-1]=da THEN
        c.b(c.mov_axi); c.w(a.disp MOD 8000h);
      ELSE
        c.b(c.mov_cxi); c.w(a.disp MOD 8000h);
      END;
      IF a.level=0 THEN
        c.b(c.mov_msr); c.b(c.md_reg+dx+c.DS*8);
      ELSE
        ofs:=vrs.mdls[a.level].ofs;
        IF (ofs>=0) & (ofs<=0FFFEh) THEN
          c.b(c.mov_wrm); c.b(c.md_abs+dx*8); c.w(ofs);
        ELSE
          mov_es_ds(ofs); c.b(46b+c.ES*8);
          c.b(c.mov_wrm); c.b(c.md_abs+dx*8); c.w(ofs MOD 8000h);
        END;
      END;
      IF (sg<-128) OR (sg>127) THEN
        c.b(c.imm_wm); c.b(c.md_reg+dx+c.i_add); c.w(sg);
      ELSIF sg#0 THEN
        c.b(c.imm_wbm); c.b(c.md_reg+dx+c.i_add); c.b(sg);
      END;
    |sym.am_L,sym.am_Limm,sym.am_PB2,sym.am_PB4,sym.am_PB8:
      ofs:=a.disp;
      IF a.am=sym.am_PB2 THEN parm_ofs(ofs,a.level) END;
      IF a.am=sym.am_PB4 THEN parm_ofs(ofs,a.level) END;
      IF a.am=sym.am_PB8 THEN parm_ofs(ofs,a.level) END;
      IF a.level=vrs.prcs[proc^.adr].lvl THEN
        c.b(c.mov_msr); c.b(c.md_reg+dx+c.SS*8); c.b(c.lea);
        IF (ofs>=-128) & (ofs<=127) THEN c.b(c.md_b+c.rm_bp+ax*8); c.b(ofs);
        ELSE c.b(c.md_w+c.rm_bp+ax*8); c.w(ofs);
        END;
      ELSE
        ASSERT(vrs.prcs[proc^.adr].slink);
        ASSERT(NOT vrs.prcs[proc^.adr].export);
        c.b(c.mov_wrm); c.b(c.md_b+c.SI*8+c.rm_bp); c.b(4);
        FOR i:=a.level+2 TO vrs.prcs[proc^.adr].lvl DO
          c.b(46b+c.SS*8); c.b(c.mov_wrm); c.b(c.md_b+c.SI*8+c.rm_si); c.b(4);
        END;
        c.b(c.mov_msr); c.b(c.md_reg+dx+c.SS*8); c.b(c.lea);
        IF (ofs>=-128) & (ofs<=127) THEN c.b(c.md_b+c.rm_si+ax*8); c.b(ofs);
        ELSE c.b(c.md_w+c.rm_si+ax*8); c.w(ofs);
        END;
      END;
    |sym.am_aG:
      IF a.level=0 THEN
        IF (a.n>=0) & (a.n<=10000h-4) THEN put_cmd(d,8,c.DS,a.n);
        ELSE ofs:=a.n; mov_es_ds(ofs); put_cmd(d,8,c.ES,ofs);
        END;
      ELSE
        ofs:=vrs.mdls[a.level].ofs;
        IF (ofs>=0) & (ofs<=0FFFEh) THEN
          IF (a.n>=0) & (a.n<=10000h-4) THEN
            c.b(c.mov_srm); c.b(c.md_abs+c.ES*8); c.w(ofs);
            put_cmd(d,8,c.ES,a.n);
          ELSE
            c.b(c.mov_wrm); c.b(c.md_abs+c.SI*8); c.w(ofs);
            ofs:=a.n; mov_es_si(ofs); put_cmd(d,8,c.ES,ofs);
          END;
        ELSE
          mov_es_ds(ofs); c.b(46b+c.ES*8);
          IF (a.n>=0) & (a.n<=10000h-4) THEN
            c.b(c.mov_srm); c.b(c.md_abs+c.ES*8); c.w(ofs);
            put_cmd(d,8,c.ES,a.n);
          ELSE
            c.b(c.mov_wrm); c.b(c.md_abs+c.SI*8); c.w(ofs);
            ofs:=a.n; mov_es_si(ofs); put_cmd(d,8,c.ES,ofs);
          END;
        END;
      END;
      add_adr_const(a.disp);
    |sym.am_aL,sym.am_aPB:
      ASSERT(a.level>=1);
      ofs:=a.n;
      IF a.am=sym.am_aPB THEN parm_ofs(ofs,a.level) END;
      IF a.level=vrs.prcs[proc^.adr].lvl THEN
        put_cmd(d,c.rm_bp,c.SS,ofs MOD 10000h);
      ELSE
        ASSERT(vrs.prcs[proc^.adr].slink);
        ASSERT(NOT vrs.prcs[proc^.adr].export);
        c.b(c.mov_wrm); c.b(c.md_b+c.SI*8+c.rm_bp); c.b(4);
        FOR i:=a.level+2 TO vrs.prcs[proc^.adr].lvl DO
          c.b(46b+c.SS*8); c.b(c.mov_wrm); c.b(c.md_b+c.SI*8+c.rm_si); c.b(4);
        END;
        put_cmd(d,c.rm_si,c.SS,ofs MOD 10000h);
      END;
      add_adr_const(a.disp);
  ELSE
tty.print('load adr, mode %d\n',a.am); HALT(50h);
  END;
END load_adr;

PROCEDURE neg;
  VAR ax,dx: INTEGER;
BEGIN
  pop_reg(1);
  IF pos[stk-1]=da THEN ax:=c.AX; dx:=c.DX;
  ELSE ax:=c.CX; dx:=c.BX;
  END;
  CASE siz[stk-1] OF
    |1: c.b(c.grp1_bm); c.b(c.md_reg+ax+c.g1_neg);
    |2: c.b(c.grp1_wm); c.b(c.md_reg+ax+c.g1_neg);
    |4: c.b(c.grp1_wm); c.b(c.md_reg+dx+c.g1_neg);
        c.b(c.grp1_wm); c.b(c.md_reg+ax+c.g1_neg);
        c.b(c.imm_wbm); c.b(c.md_reg+dx+c.i_sbb); c.b(0);
  END;
END neg;

PROCEDURE shl_c(n: INTEGER);
  VAR i,ax,dx: INTEGER;
BEGIN
  pop_reg(1);
  IF pos[stk-1]=da THEN ax:=c.AX; dx:=c.DX;
  ELSE ax:=c.CX; dx:=c.BX;
  END;
  CASE siz[stk-1] OF
    |1: FOR i:=0 TO n-1 DO c.b(c.shift_bm1); c.b(c.md_reg+ax+c.s_shl) END;
    |2: FOR i:=0 TO n-1 DO c.b(c.shift_wm1); c.b(c.md_reg+ax+c.s_shl) END;
    |4: IF n>=16 THEN
          c.b(c.mov_wrm); c.b(c.md_reg+dx*8+ax);
          IF ax=c.AX THEN c.b(c.mov_axi) ELSE c.b(c.mov_cxi) END;
          c.w(0); n:=n-16;
        END;
        FOR i:=0 TO n-1 DO
          c.b(c.shift_wm1); c.b(c.md_reg+ax+c.s_shl);
          c.b(c.shift_wm1); c.b(c.md_reg+dx+c.s_rcl);
        END;
  END;
END shl_c;

PROCEDURE sar_c(n: INTEGER);
  VAR i,ax,dx: INTEGER;
BEGIN
  pop_reg(1);
  IF pos[stk-1]=da THEN ax:=c.AX; dx:=c.DX;
  ELSE ax:=c.CX; dx:=c.BX;
  END;
  CASE siz[stk-1] OF
    |1: FOR i:=0 TO n-1 DO c.b(c.shift_bm1); c.b(c.md_reg+ax+c.s_sar) END;
    |2: FOR i:=0 TO n-1 DO c.b(c.shift_wm1); c.b(c.md_reg+ax+c.s_sar) END;
    |4: IF (n>=16) & (ax=c.AX) THEN
          c.b(c.mov_wrm); c.b(c.md_reg+ax*8+dx);
          c.b(c.cwd); n:=n-16;
        END;
        FOR i:=0 TO n-1 DO
          c.b(c.shift_wm1); c.b(c.md_reg+dx+c.s_sar);
          c.b(c.shift_wm1); c.b(c.md_reg+ax+c.s_rcr);
        END;
  END;
END sar_c;

PROCEDURE power_2(n: INTEGER; VAR i: INTEGER): BOOLEAN;
  VAR k: INTEGER;
BEGIN
  IF n<=0 THEN RETURN FALSE END;
  i:=30;
  LOOP
    k:=INTEGER({i});
    IF n>=k THEN RETURN n=k END;
    DEC(i);
  END;
END power_2;

PROCEDURE idiv(VAL a: sym.access);
  VAR i: INTEGER;
BEGIN
  IF a.am=sym.am_aSTK THEN load(a,siz[stk-2])
  ELSIF a.am IN am_const THEN
    IF power_2(a.n,i) THEN sar_c(i); RETURN END;
    IF power_2(-a.n,i) THEN sar_c(i); neg; RETURN END;
    load(a,siz[stk-1]);
  ELSIF a.am#sym.am_STK THEN load(a,siz[stk-1])
  END;
  save;
  CASE siz[stk-1] OF
    |1: eml.emul_call('idiv1');
    |2: eml.emul_call('idiv2');
    |4: eml.emul_call('idiv4');
  END;
  DEC(stk); pos[stk-1]:=da;
END idiv;

PROCEDURE imul(VAL a: sym.access);
  VAR i: INTEGER;
BEGIN
  IF a.am=sym.am_aSTK THEN load(a,siz[stk-2])
  ELSIF a.am IN am_const THEN
    IF a.n=0 THEN DEC(stk); load(a,siz[stk]); RETURN END;
    IF power_2(a.n,i) THEN shl_c(i); RETURN END;
    IF power_2(-a.n,i) THEN shl_c(i); neg; RETURN END;
    load(a,siz[stk-1]);
  ELSIF a.am#sym.am_STK THEN load(a,siz[stk-1])
  END;
  IF siz[stk-1]=4 THEN
    save; eml.emul_call('imul4'); DEC(stk); pos[stk-1]:=da; siz[stk-1]:=4;
  ELSE
    pop_reg(2);
    IF siz[stk-1]=2 THEN
      c.b(c.grp1_wm); c.b(c.md_reg+c.CX+c.g1_imul);
      DEC(stk); pos[stk-1]:=da;
    ELSE
      c.b(c.grp1_bm); c.b(c.md_reg+c.CL+c.g1_imul);
      DEC(stk); pos[stk-1]:=da;
    END;
  END;
  ASSERT(stk>=0);
END imul;

PROCEDURE add_adr(VAL a: sym.access);
  VAR ax,dx,cx,bx,sg,ofs,sz: INTEGER;
BEGIN
  CASE a.am OF
    |sym.am_STK :
    |sym.am_imm,sym.am_Gimm,sym.am_Limm: add_adr_const(a.n); RETURN;
  ELSE load(a,4);
  END;
  pop_reg(2);
  sz:=siz[stk-1];
  ASSERT(siz[stk-2]=4);
  ASSERT(pos[stk-1]#pos[stk-2]);
  IF pos[stk-2]=da THEN
    ax:=c.AX; dx:=c.DX; cx:=c.CX; bx:=c.BX;
    IF sz=1 THEN c.b(c.mov_chi); c.b(0) END;
  ELSE
    ax:=c.CX; dx:=c.BX; cx:=c.AX; bx:=c.DX;
    IF sz=1 THEN c.b(c.mov_ahi); c.b(0) END;
  END;
  IF sz>1 THEN
    c.b(c.and_wrm); c.b(c.md_reg+cx+cx*8);
    c.b(c.jns);
    IF pc.cpu_mode=0 THEN
      c.b(8); c.b(c.imm_wm); c.b(c.md_reg+c.i_add+dx); c.w(800h);
    ELSE
      c.b(7); c.b(c.imm_wbm); c.b(c.md_reg+c.i_add+dx); c.b(8h);
    END;
    c.b(c.imm_wm); c.b(c.md_reg+c.i_and+cx); c.w(7FFFh);
  END;
  c.b(c.add_wrm); c.b(c.md_reg+ax*8+cx);
  c.b(c.jns);
  IF pc.cpu_mode=0 THEN
    c.b(8); c.b(c.imm_wm); c.b(c.md_reg+c.i_add+dx); c.w(800h);
  ELSE
    c.b(7); c.b(c.imm_wbm); c.b(c.md_reg+c.i_add+dx); c.b(8h);
  END;
  c.b(c.imm_wm); c.b(c.md_reg+c.i_and+ax); c.w(7FFFh);
  IF sz>2 THEN
    IF pc.cpu_type>=1 THEN
      c.b(c.shift_wmi); c.b(c.md_reg+bx+c.s_shl);
      IF pc.cpu_mode=0 THEN c.b(12) ELSE c.b(4) END;
    ELSE
      IF pos[stk-2]=bc THEN c.b(c.push_cx) END;
      c.b(c.mov_cli);
      IF pc.cpu_mode=0 THEN c.b(12) ELSE c.b(4) END;
      c.b(c.shift_wmc); c.b(c.md_reg+bx+c.s_shl);
      IF pos[stk-2]=bc THEN c.b(c.pop_cx) END;
    END;
    c.b(c.add_wrm); c.b(c.md_reg+dx*8+bx);
  END;
  DEC(stk);
  ASSERT(stk>=0);
END add_adr;

PROCEDURE sub_adr(VAL a: sym.access);
  VAR ax,dx,cx,bx,sg,ofs,sz: INTEGER;
BEGIN
  CASE a.am OF
    |sym.am_STK :
    |sym.am_imm,sym.am_Gimm,sym.am_Limm: sub_adr_const(a.n); RETURN;
  ELSE load(a,4);
  END;
  pop_reg(2);
  sz:=siz[stk-1];
  ASSERT(siz[stk-2]=4);
  ASSERT(pos[stk-1]#pos[stk-2]);
  IF pos[stk-2]=da THEN
    ax:=c.AX; dx:=c.DX; cx:=c.CX; bx:=c.BX;
    IF sz=1 THEN c.b(c.mov_chi); c.b(0) END;
  ELSE
    ax:=c.CX; dx:=c.BX; cx:=c.AX; bx:=c.DX;
    IF sz=1 THEN c.b(c.mov_ahi); c.b(0) END;
  END;
  IF sz>1 THEN
    c.b(c.and_wrm); c.b(c.md_reg+cx+cx*8);
    c.b(c.jns);
    IF pc.cpu_mode=0 THEN
      c.b(8); c.b(c.imm_wm); c.b(c.md_reg+c.i_sub+dx); c.w(800h);
    ELSE
      c.b(7); c.b(c.imm_wbm); c.b(c.md_reg+c.i_sub+dx); c.b(8h);
    END;
    c.b(c.imm_wm); c.b(c.md_reg+c.i_and+cx); c.w(7FFFh);
  END;
  c.b(c.sub_wrm); c.b(c.md_reg+ax*8+cx);
  c.b(c.jns);
  IF pc.cpu_mode=0 THEN
    c.b(8); c.b(c.imm_wm); c.b(c.md_reg+c.i_sub+dx); c.w(800h);
  ELSE
    c.b(7); c.b(c.imm_wbm); c.b(c.md_reg+c.i_sub+dx); c.b(8h);
  END;
  c.b(c.imm_wm); c.b(c.md_reg+c.i_and+ax); c.w(7FFFh);
  IF sz>2 THEN
    IF pc.cpu_type>=1 THEN
      c.b(c.shift_wmi); c.b(c.md_reg+bx+c.s_shl);
      IF pc.cpu_mode=0 THEN c.b(12) ELSE c.b(4) END;
    ELSE
      IF pos[stk-2]=bc THEN c.b(c.push_cx) END;
      c.b(c.mov_cli);
      IF pc.cpu_mode=0 THEN c.b(12) ELSE c.b(4) END;
      c.b(c.shift_wmc); c.b(c.md_reg+bx+c.s_shl);
      IF pos[stk-2]=bc THEN c.b(c.pop_cx) END;
    END;
    c.b(c.sub_wrm); c.b(c.md_reg+dx*8+bx);
  END;
  DEC(stk);
  ASSERT(stk>=0);
END sub_adr;

PROCEDURE drop;
BEGIN
  pop_reg(1); DEC(stk);
  ASSERT(stk>=0);
END drop;

PROCEDURE save;
BEGIN
  IF (stk>1) & (pos[stk-2]=da) THEN
    ASSERT(pos[stk-2]#pos[stk-1]);
    IF siz[stk-2]=4 THEN c.b(c.push_dx) END;
    c.b(c.push_ax); pos[stk-2]:=stack;
  ELSIF (stk>1) & (pos[stk-2]=bc) THEN
    ASSERT(pos[stk-2]#pos[stk-1]);
    IF siz[stk-2]=4 THEN c.b(c.push_bx) END;
    c.b(c.push_cx); pos[stk-2]:=stack;
  END;
  IF (stk>0) & (pos[stk-1]=da) THEN
    IF siz[stk-1]=4 THEN c.b(c.push_dx) END;
    c.b(c.push_ax); pos[stk-1]:=stack;
  ELSIF (stk>0) & (pos[stk-1]=bc) THEN
    IF siz[stk-1]=4 THEN c.b(c.push_bx) END;
    c.b(c.push_cx); pos[stk-1]:=stack;
  END;
END save;

PROCEDURE stot(a: sym.access; sz: INTEGER);
  VAR d: cmd_desc;
BEGIN
  IF sz=1 THEN load(a,sz); a.am:=sym.am_STK END;
  IF (pc.cpu_type<1) &
     ((a.am=sym.am_imm) OR (a.am=sym.am_Gimm) OR (a.am=sym.am_Limm)) THEN
    load(a,sz); a.am:=sym.am_STK
  END;
  IF a.am#sym.am_STK THEN
    d.cop:=c_mov; d.mode:=m_sm; d.size:=sz; gen_cmd(d,a);
  ELSE
    ASSERT((stk=1) OR (pos[stk-2]=stack));
    DEC(stk);
    ASSERT(stk>=0);
    IF pos[stk]=da THEN
      IF siz[stk]=4 THEN c.b(c.push_dx) END;
      c.b(c.push_ax);
    ELSIF pos[stk]=bc THEN
      IF siz[stk]=4 THEN c.b(c.push_bx) END;
      c.b(c.push_cx);
    END;
  END;
END stot;

PROCEDURE func_ret(sz: INTEGER);
BEGIN
  alloc_reg(sz);
END func_ret;

PROCEDURE hold_out;
BEGIN
  ASSERT(stk=1);
  pop_reg(1); DEC(stk);
  ASSERT(stk>=0);
  IF pos[stk]=bc THEN
    IF siz[stk]=4 THEN c.b(c.xchg_wmr); c.b(c.md_reg+c.DX+c.BX*8) END;
    c.b(c.xchg_cx);
  END;
END hold_out;

PROCEDURE mov_c(sz: INTEGER);
  VAR ax,dx,cx,bx: INTEGER;
BEGIN
  pop_reg(2);
  ASSERT(stk>=2);
  IF sz<=0 THEN DEC(stk,2); RETURN END;
  c.b(c.push_ds);
  IF pos[stk-2]=da THEN ax:=c.AX; dx:=c.DX; cx:=c.CX; bx:=c.BX
  ELSE ax:=c.CX; dx:=c.BX; cx:=c.AX; bx:=c.DX
  END;
  c.b(c.mov_wrm); c.b(c.md_reg+cx+c.SI*8);
  c.b(c.mov_srm); c.b(c.md_reg+bx+c.DS*8);
  c.b(c.mov_wrm); c.b(c.md_reg+ax+c.DI*8);
  c.b(c.mov_srm); c.b(c.md_reg+dx+c.ES*8);
  c.b(c.cld);
  IF sz>=8000h THEN
    c.b(c.mov_axi); c.w(sz DIV 8000h);
    c.b(c.and_wrm); c.b(c.md_reg+c.AX+c.AX*8);
    c.b(c.mov_cxi); c.w(4000h); c.b(c.rep_z); c.b(c.movs_w);
    IF pc.cpu_mode=0 THEN
      c.b(c.imm_wm); c.b(c.md_reg+bx+c.i_add); c.w(800h);
      c.b(c.imm_wm); c.b(c.md_reg+dx+c.i_add); c.w(800h);
    ELSE
      c.b(c.imm_wbm); c.b(c.md_reg+bx+c.i_add); c.b(8h);
      c.b(c.imm_wbm); c.b(c.md_reg+dx+c.i_add); c.b(8h);
    END;
    c.b(c.mov_srm); c.b(c.md_reg+bx+c.DS*8);
    c.b(c.mov_srm); c.b(c.md_reg+dx+c.ES*8);
    c.b(c.imm_wm); c.b(c.md_reg+c.SI+c.i_and); c.w(7FFFh);
    c.b(c.imm_wm); c.b(c.md_reg+c.DI+c.i_and); c.w(7FFFh);
    c.b(c.dec_ax);
    IF pc.cpu_mode=0 THEN c.b(c.jne); c.b(-28) ELSE c.b(c.jne); c.b(-26) END;
    sz:=sz MOD 8000h;
  ELSIF sz>0 THEN c.b(c.xor_wrm); c.b(c.md_reg+c.AX+c.AX*8);
  END;
  IF sz>0 THEN
    IF ODD(sz) THEN
      c.b(c.mov_cxi); c.w(sz); c.b(c.rep); c.b(c.movs_b);
    ELSE
      c.b(c.mov_cxi); c.w(sz DIV 2); c.b(c.rep); c.b(c.movs_w);
    END;
  END;
  c.b(c.pop_ds);
  DEC(stk,2);
  ASSERT(stk>=0);
END mov_c;

PROCEDURE move;
  VAR ax,dx,cx,bx: INTEGER;
BEGIN
  pop_reg(2);
  ASSERT(stk>=3);
  IF pos[stk-2]=da THEN ax:=c.AX; dx:=c.DX; cx:=c.CX; bx:=c.BX
  ELSE ax:=c.CX; dx:=c.BX; cx:=c.AX; bx:=c.DX
  END;
  c.b(c.mov_wrm); c.b(c.md_reg+cx+c.SI*8);
  c.b(c.mov_wrm); c.b(c.md_reg+ax+c.DI*8);
  c.b(c.pop_cx);
  c.b(c.pop_ax);
  c.b(c.push_ds);
  c.b(c.mov_srm); c.b(c.md_reg+bx+c.DS*8);
  c.b(c.mov_srm); c.b(c.md_reg+dx+c.ES*8);
  c.b(c.cld);
  c.b(c.shift_wm1); c.b(c.md_reg+c.AX+c.s_shl);
  c.b(c.and_wrm); c.b(c.md_reg+c.CX+c.CX*8);
  c.b(c.jns); c.b(5);
  c.b(c.imm_wm); c.b(c.md_reg+c.CX+c.i_and); c.w(7FFFh);
  c.b(c.inc_ax);
  c.b(c.and_wrm); c.b(c.md_reg+c.AX+c.AX*8);
  c.b(c.je);
  IF pc.cpu_mode=0 THEN c.b(30) ELSE c.b(28) END;
  c.b(c.push_cx);
  c.b(c.mov_cxi); c.w(4000h); c.b(c.rep_z); c.b(c.movs_w);
  IF pc.cpu_mode=0 THEN
    c.b(c.imm_wm); c.b(c.md_reg+bx+c.i_add); c.w(800h);
    c.b(c.imm_wm); c.b(c.md_reg+dx+c.i_add); c.w(800h);
  ELSE
    c.b(c.imm_wbm); c.b(c.md_reg+bx+c.i_add); c.b(8h);
    c.b(c.imm_wbm); c.b(c.md_reg+dx+c.i_add); c.b(8h);
  END;
  c.b(c.mov_srm); c.b(c.md_reg+bx+c.DS*8);
  c.b(c.mov_srm); c.b(c.md_reg+dx+c.ES*8);
  c.b(c.imm_wm); c.b(c.md_reg+c.SI+c.i_and); c.w(7FFFh);
  c.b(c.imm_wm); c.b(c.md_reg+c.DI+c.i_and); c.w(7FFFh);
  c.b(c.dec_ax);
  IF pc.cpu_mode=0 THEN c.b(c.jne); c.b(-28) ELSE c.b(c.jne); c.b(-26) END;
  c.b(c.pop_cx);
  c.b(c.rep); c.b(c.movs_b);
  c.b(c.pop_ds);
  DEC(stk,3);
END move;

PROCEDURE cmps;
BEGIN
  pop_reg(2);
  ASSERT(stk>=2);
  c.b(c.push_ds);
  IF pos[stk-1]=da THEN
    c.b(c.mov_wrm); c.b(c.md_reg+c.AX+c.SI*8);
    c.b(c.mov_srm); c.b(c.md_reg+c.DX+c.DS*8);
    c.b(c.mov_wrm); c.b(c.md_reg+c.CX+c.DI*8);
    c.b(c.mov_srm); c.b(c.md_reg+c.BX+c.ES*8);
  ELSE
    c.b(c.mov_wrm); c.b(c.md_reg+c.CX+c.SI*8);
    c.b(c.mov_srm); c.b(c.md_reg+c.BX+c.DS*8);
    c.b(c.mov_wrm); c.b(c.md_reg+c.AX+c.DI*8);
    c.b(c.mov_srm); c.b(c.md_reg+c.DX+c.ES*8);
  END;
  c.b(c.mov_chi); c.b(0); c.b(c.cld);
  c.b(c.mov_brm); c.b(c.md_0+c.rm_si+c.CL*8);
  c.b(c.jcxz); c.b(5);
  c.b(c.cmps_b);
  c.b(c.je); c.b(-7);
  c.b(c.jmp_s); c.b(1);
  c.b(c.cmps_b);
  c.b(c.pop_ds);
  DEC(stk,2);
  ASSERT(stk>=0);
END cmps;

BEGIN
  stk:=0; top.am:=sym.am_STK; proc:=NIL;
END inCmds.
