IMPLEMENTATION MODULE inCmd; (* Sem 26-Feb-91. (c) KRONOS *)

IMPORT pcS,pcM;

TYPE
  am_set = SET OF adr_mode;
  arr_7  = ARRAY cmd_cop OF CARDINAL;
  byte   = SET OF [0..7];
  word   = SET OF [0..15];
  dword  = SET OF [0..31];
  cmd_desc = RECORD
    cop  : cmd_cop;
    mode : cmd_mode;
    r0,r1: CARDINAL;
    size : CARDINAL;
    val  : LONGINT;
  END;

CONST
  cop_0 = arr_7 (88H,00B,50B,10B,60B,40B,70B);
  cop_1 = arr_7 (88H,20B,30B,10B,60B,40B,70B);

  am_const = am_set{am_imm,am_Gimm,am_Limm};

  too_large_code = 0;
  assert         = 233;

VAR
  last_line: CARDINAL;
  node_mem : ARRAY [0..07FFEH] OF BYTE;
  busy_mem : CARDINAL;

PROCEDURE error(n: INTEGER);
BEGIN
  pcS.err(n); pcM.abort;
END error;

PROCEDURE b(n: WORD);
BEGIN
  IF cnt>HIGH(code) THEN error(too_large_code) END;
  code[cnt]:=byte(n); INC(cnt);
END b;

PROCEDURE w(n: WORD);
BEGIN
  IF cnt+1>HIGH(code) THEN error(too_large_code) END;
  code[cnt]:=byte(n); INC(cnt);
  code[cnt]:=byte(CARDINAL(n)>>8); INC(cnt);
END w;

PROCEDURE new_node(): node;
  VAR r: node;
BEGIN
  r:=ADR(node_mem[busy_mem]);
  IF busy_mem>SIZE(node_mem)-SIZE(r^) THEN error(0) END;
  busy_mem:=busy_mem+SIZE(r^);
  WITH r^ DO
    goto :=NIL;  md   :=nm_goto;
    else :=NIL;  line :=NIL;
    pos  :=0;    len  :=0;
    tpos :=0;    tlen :=0;
    adr  :=0;    cnt  :=0;
    next :=NIL;  fixup:=NIL;
  END;
  RETURN r;
END new_node;

PROCEDURE finish;
BEGIN
  IF block#NIL THEN
    block^.len:=cnt-block^.pos;
    block:=NIL; last_line:=0;
  END;
END finish;

PROCEDURE start(n: node);
BEGIN
  finish; block:=n; block^.pos:=cnt;
END start;

PROCEDURE remove_nodes;
BEGIN
  cnt:=0; busy_mem:=0; block:=NIL; last_line:=0;
END remove_nodes;

PROCEDURE gen_jump(f: node; ofs: CARDINAL): INTEGER;
  PROCEDURE jump(to: CARDINAL);
    VAR i: LONGINT;
  BEGIN
    i:=LONGINT(to)-LONGINT(f^.adr+f^.len+(cnt-f^.tpos));
    IF (i<=129) & (i>=-126) THEN b(jmp_s); b(word(i-2));
    ELSE b(jmp); w(word(i-3));
    END;
  END jump;
  VAR
    p      : node;
    c,n,j  : CARDINAL;
    ptr    : POINTER TO ARRAY [0..3FFEH] OF CARDINAL;
    i      : LONGINT;
BEGIN
  f^.tpos:=cnt;
  CASE f^.md OF
    |nm_case:
      FOR n:=0 TO f^.clen-1 DO f^.jtbl^[n]:=f^.ctbl^[n]^.adr+ofs END;
    |nm_cond:
      IF f^.else=f^.goto THEN
        IF f^.next#f^.goto THEN jump(f^.goto^.adr) END;
      ELSIF f^.else=f^.next THEN
        i:=LONGINT(f^.goto^.adr)-LONGINT(f^.adr+f^.len);
        IF (i<=129) & (i>=-126) THEN
          b(jo+ORD(f^.flag)); b(word(i-2));
        ELSE
          b(jo+CARDINAL(BITSET(f^.flag)/{0})); b(3);
          b(jmp); w(word(i-5));
        END;
      ELSIF f^.goto=f^.next THEN
        i:=LONGINT(f^.else^.adr)-LONGINT(f^.adr+f^.len);
        IF (i<=129) & (i>=-126) THEN
          b(jo+CARDINAL(BITSET(f^.flag)/{0})); b(word(i-2));
        ELSE
          b(jo+ORD(f^.flag)); b(3);
          b(jmp); w(word(i-5));
        END;
      ELSE
        i:=LONGINT(f^.goto^.adr)-LONGINT(f^.adr+f^.len);
        IF (i<=129) & (i>=-126) THEN
          b(jo+ORD(f^.flag)); b(word(i-2));
          jump(f^.else^.adr);
        ELSE
          i:=LONGINT(f^.else^.adr)-LONGINT(f^.adr+f^.len);
          IF (i<=129) & (i>=-126) THEN
            b(jo+CARDINAL(BITSET(f^.flag)/{0})); b(word(i-2));
            jump(f^.goto^.adr);
          ELSE
            (* both jump are long *)
            b(jo+ORD(f^.flag)); b(3);
            b(jmp); w(word(i-5));
            jump(f^.goto^.adr);
          END;
        END;
      END;
    |nm_goto:
      IF (f^.goto#NIL) & (f^.next#f^.goto) THEN jump(f^.goto^.adr) END;
  END;
  RETURN cnt-f^.tpos;
END gen_jump;

PROCEDURE mark(line: CARDINAL);
  VAR l: LINE;
BEGIN
  IF line=last_line THEN RETURN END;
  last_line:=line;
  l:=ADR(node_mem[busy_mem]);
  IF busy_mem>SIZE(node_mem)-SIZE(l^) THEN error(0) END;
  busy_mem:=busy_mem+SIZE(l^);
  l^.no:=line;
  l^.next:=block^.line;
  IF l^.next#NIL THEN l^.ofs:=cnt-block^.pos ELSE l^.ofs:=0 END;
  block^.line:=l;
END mark;

PROCEDURE fixup;
  VAR f: FIXUP;
BEGIN
  f:=ADR(node_mem[busy_mem]);
  IF busy_mem>SIZE(node_mem)-SIZE(f^) THEN error(0) END;
  busy_mem:=busy_mem+SIZE(f^);
  f^.md:=fm_rts;
  f^.obj:=NIL;
  f^.ofs:=cnt-block^.pos;
  f^.next:=block^.fixup;
  block^.fixup:=f;
END fixup;

PROCEDURE alloc_reg(sz: CARDINAL);
  VAR n: INTEGER;
BEGIN
  n:=INTEGER(stk)-1;
  IF (n<0) OR (pos[n]=stack) THEN
    siz[stk]:=sz; pos[stk]:=da; INC(stk); RETURN
  ELSIF pos[n]=da THEN
    DEC(n);
    IF (n<0) OR (pos[n]=stack) THEN
      siz[stk]:=sz; pos[stk]:=bc; INC(stk); RETURN
    ELSIF pos[n]=bc THEN
      IF siz[n]=4 THEN b(push_bx) END; b(push_cx); pos[n]:=stack;
      siz[stk]:=sz; pos[stk]:=bc; INC(stk); RETURN
    END;
  ELSIF pos[n]=bc THEN
    DEC(n);
    IF (n<0) OR (pos[n]=stack) THEN
      siz[stk]:=sz; pos[stk]:=da; INC(stk); RETURN
    ELSIF pos[n]=da THEN
      IF siz[n]=4 THEN b(push_dx) END; b(push_ax); pos[n]:=stack;
      siz[stk]:=sz; pos[stk]:=da; INC(stk); RETURN
    END;
  END;
  error(assert);
END alloc_reg;

PROCEDURE pop_reg(n: CARDINAL);
BEGIN
  IF n>stk THEN error(assert) END;
  IF pos[stk-1]=stack THEN
    b(pop_ax);
    IF siz[stk-1]=4 THEN b(pop_dx) END;
    pos[stk-1]:=da;
  END;
  IF (n>1) & (pos[stk-2]=stack) THEN
    IF pos[stk-1]=bc THEN
      b(pop_ax);
      IF siz[stk-2]=4 THEN b(pop_dx) END;
      pos[stk-2]:=da;
    ELSE
      b(pop_cx);
      IF siz[stk-2]=4 THEN b(pop_bx) END;
      pos[stk-2]:=bc;
    END;
  END;
END pop_reg;

PROCEDURE set_size(sz: CARDINAL; sg: BOOLEAN);
  VAR i: CARDINAL;
BEGIN
  i:=siz[stk-1]; siz[stk-1]:=sz;
  IF i>=sz THEN RETURN END;
  pop_reg(1);
  IF sg THEN
    IF pos[stk-1]=bc THEN
      IF (siz[stk-1]=4) OR (stk>1) & (pos[stk-2]=da) & (siz[stk-2]=4) THEN
        b(xchg_wmr); b(md_reg+BX+DX*8);
      END;
      b(xchg_cx);
      pos[stk-1]:=da;
      IF (stk>1) & (pos[stk-2]=da) THEN pos[stk-2]:=bc END;
    END;
    IF i=1 THEN b(cbw); i:=2 END;
    IF (i=2) & (sz=4) THEN b(cwd) END;
  ELSE
    IF i=1 THEN
      IF pos[stk-1]=da THEN b(mov_ahi); b(0)
      ELSE b(mov_chi); b(0);
      END;
      i:=2;
    END;
    IF (i=2) & (sz=4) THEN
      IF pos[stk-1]=da THEN b(mov_dxi); w(0);
      ELSE b(mov_bxi); w(0);
      END;
    END;
  END;
END set_size;

PROCEDURE put_access(am,sr,ofs,reg,cop: CARDINAL);
  VAR sgo: BOOLEAN;
BEGIN
  sgo:=(am IN {2,3,6}) & (sr#2) OR (am IN {0,1,4,5,7,8}) & (sr#3);
  IF sgo THEN b(seg_es+sr*8) END;
  b(cop);
  IF am=8 THEN b(006B+reg); w(ofs)
  ELSIF (am#6) & (ofs=0) THEN b(am+reg);
  ELSIF (ofs<80H) OR (ofs>=0FF80H) THEN b(am+reg+100B); b(ofs);
  ELSE b(am+reg+200B); w(ofs);
  END;
END put_access;

PROCEDURE put_access2(am,sr,ofs,reg,cop1,cop2: CARDINAL);
  VAR sgo: BOOLEAN;
BEGIN
  sgo:=(am IN {2,3,6}) & (sr#2) OR (am IN {0,1,4,5,7,8}) & (sr#3);
  IF sgo THEN b(seg_es+sr*8) END;
  b(cop1); b(cop2);
  IF am=8 THEN b(006B+reg); w(ofs)
  ELSIF (am#6) & (ofs=0) THEN b(am+reg);
  ELSIF (ofs<80H) OR (ofs>=0FF80H) THEN b(am+reg+100B); b(ofs);
  ELSE b(am+reg+200B); w(ofs);
  END;
END put_access2;

PROCEDURE put_cmd(d: cmd_desc; am,sr,ofs: CARDINAL);
  PROCEDURE put_ofs(rr,n: CARDINAL);
  BEGIN
    IF am=8 THEN b(006B+rr); w(n)
    ELSIF d.cop=c_cmp THEN b(am+rr+200B); w(n)
    ELSIF (am#6) & (n=0) THEN b(am+rr);
    ELSIF (n<80H) OR (n>=0FF80H) THEN b(am+rr+100B); b(n);
    ELSE b(am+rr+200B); w(n);
    END;
  END put_ofs;
  VAR n,x1,x0: INTEGER; ax,sgo: BOOLEAN;
BEGIN
  IF ofs>0FFFFH-d.size+1 THEN error(assert) END;
  sgo:=(am IN {2,3,6}) & (sr#2) OR (am IN {0,1,4,5,7,8}) & (sr#3);
  x0:=cop_0[d.cop]; x1:=cop_1[d.cop];
  IF (d.mode=m_rm) OR (d.mode=m_mr) THEN
    ax:=(d.cop=c_mov) & (d.r0=0) & (am=8);
    n:=ORD(d.size>1)+ORD(d.mode=m_rm)*2;
    IF (d.size>2) & (d.cop=c_cmp) THEN error(assert) END;
    IF sgo THEN b(seg_es+sr*8) END;
    IF ax THEN b(mov_alm+ORD(d.size>1)+ORD(d.mode=m_mr)*2); w(ofs);
    ELSE b(x0+n); put_ofs(d.r0*8,ofs);
    END;
    IF d.size>2 THEN
      IF sgo THEN b(seg_es+sr*8) END;
      b(x1+n); put_ofs(d.r1*8,ofs+2);
    END;
  ELSIF (d.mode=m_mi) & (d.cop=c_mov) THEN
    IF sgo THEN b(seg_es+sr*8) END;
    CASE d.size OF
      |1: b(mov_bmi); put_ofs(0,ofs); b(word(d.val));
      |2: b(mov_wmi); put_ofs(0,ofs); w(word(d.val));
      |4: b(mov_wmi); put_ofs(0,ofs); w(word(d.val));
          IF sgo THEN b(seg_es+sr*8) END;
          b(mov_wmi); put_ofs(0,ofs+2); w(word(d.val>>16));
    END;
  ELSIF d.mode=m_sm THEN
    IF d.cop#c_mov THEN error(assert) END;
    IF sgo THEN b(seg_es+sr*8) END;
    CASE d.size OF
      |2: b(grp2_w); put_ofs(g2_push,ofs);
      |4: b(grp2_w); put_ofs(g2_push,ofs+2);
          IF sgo THEN b(seg_es+sr*8) END;
          b(grp2_w); put_ofs(g2_push,ofs);
    END;
  ELSIF (d.mode=m_mi) & (d.cop=c_add) & (d.size<4) & (d.val=1) THEN
    IF sgo THEN b(seg_es+sr*8) END;
    CASE d.size OF
      |1: b(grp2_b); put_ofs(g2_inc,ofs);
      |2: b(grp2_w); put_ofs(g2_inc,ofs);
    END;
  ELSIF (d.mode=m_mi) & (d.cop=c_sub) & (d.size<4) & (d.val=1) THEN
    IF sgo THEN b(seg_es+sr*8) END;
    CASE d.size OF
      |1: b(grp2_b); put_ofs(g2_dec,ofs);
      |2: b(grp2_w); put_ofs(g2_dec,ofs);
    END;
  ELSIF d.mode=m_mi THEN
    CASE d.size OF
      |1: IF (d.cop=c_xor) & (byte(d.val)=byte{}) THEN RETURN END;
          IF (d.cop=c_or ) & (byte(d.val)=byte{}) THEN RETURN END;
          IF (d.cop=c_and) & (byte(d.val)=byte{0..7}) THEN RETURN END;
          IF sgo THEN b(seg_es+sr*8) END;
          b(imm_bm); put_ofs(x0,ofs); b(word(d.val));
      |2: IF (d.cop=c_xor) & (word(d.val)=word{}) THEN RETURN END;
          IF (d.cop=c_or ) & (word(d.val)=word{}) THEN RETURN END;
          IF (d.cop=c_and) & (word(d.val)=word{0..15}) THEN RETURN END;
          IF sgo THEN b(seg_es+sr*8) END;
          b(imm_wm); put_ofs(x0,ofs); w(word(d.val));
      |4: IF    (d.cop=c_xor) & (word(d.val)=word{}) THEN
          ELSIF (d.cop=c_or ) & (word(d.val)=word{}) THEN
          ELSIF (d.cop=c_and) & (word(d.val)=word{0..15}) THEN
          ELSE
            IF sgo THEN b(seg_es+sr*8) END;
            b(imm_wm); put_ofs(x0,ofs); w(word(d.val));
          END;
          IF (d.cop=c_xor) & (word(d.val>>16)=word{}) THEN RETURN END;
          IF (d.cop=c_or ) & (word(d.val>>16)=word{}) THEN RETURN END;
          IF (d.cop=c_and) & (word(d.val>>16)=word{0..15}) THEN RETURN END;
          IF sgo THEN b(seg_es+sr*8) END;
          b(imm_wm); put_ofs(x1,ofs+2); w(word(d.val>>16));
    END;
  ELSE error(assert);
  END;
END put_cmd;

PROCEDURE inc_address_carry(r1: CARDINAL);
BEGIN
  IF cpu_mode=0 THEN
    b(jnb); b(4); b(imm_wm); b(md_reg+r1+i_add); w(1000H);
  ELSE
    b(jnb); b(3); b(imm_wbm); b(md_reg+r1+i_add); b(8H);
  END;
END inc_address_carry;

PROCEDURE dec_address_carry(r1: CARDINAL);
BEGIN
  IF cpu_mode=0 THEN
    b(jnb); b(4); b(imm_wm); b(md_reg+r1+i_sub); w(1000H);
  ELSE
    b(jnb); b(3); b(imm_wbm); b(md_reg+r1+i_sub); b(8H);
  END;
END dec_address_carry;

PROCEDURE inc_address_low(r0,r1,ofs: CARDINAL);
BEGIN
  IF (ofs>=80H) & (ofs<0FF80H) THEN
    b(imm_wm); b(md_reg+r0+i_add); w(ofs); inc_address_carry(r1);
  ELSIF ofs#0 THEN
    b(imm_wbm); b(md_reg+r0+i_add); b(ofs); inc_address_carry(r1);
  END;
END inc_address_low;

PROCEDURE dec_address_low(r0,r1,ofs: CARDINAL);
BEGIN
  IF (ofs>=80H) & (ofs<0FF80H) THEN
    b(imm_wm); b(md_reg+r0+i_sub); w(ofs); dec_address_carry(r1);
  ELSIF ofs#0 THEN
    b(imm_wbm); b(md_reg+r0+i_sub); b(ofs); dec_address_carry(r1);
  END;
END dec_address_low;

PROCEDURE inc_address_high(r1: CARDINAL; ofs: LONGINT);
  VAR sg: INTEGER;
BEGIN
  sg:=INTEGER(ofs>>16);
  IF cpu_mode=0 THEN sg:=sg*1000H ELSE sg:=sg*8 END;
  IF (sg>=128) OR (sg<-128) THEN
    b(imm_wm); b(md_reg+r1+i_add); w(sg);
  ELSIF sg#0 THEN
    b(imm_wbm); b(md_reg+r1+i_add); b(word(sg));
  END;
END inc_address_high;

PROCEDURE dec_address_high(r1: CARDINAL; ofs: LONGINT);
  VAR sg: INTEGER;
BEGIN
  sg:=INTEGER(ofs>>16);
  IF cpu_mode=0 THEN sg:=sg*1000H ELSE sg:=sg*8 END;
  IF (sg>=128) OR (sg<-128) THEN
    b(imm_wm); b(md_reg+r1+i_sub); w(sg);
  ELSIF sg#0 THEN
    b(imm_wbm); b(md_reg+r1+i_sub); b(word(sg));
  END;
END dec_address_high;

PROCEDURE cmd_imm(d: cmd_desc; n1,n2: CARDINAL);
BEGIN
  IF d.mode=m_sm THEN
    IF d.size=4 THEN b(push_wi); w(n2) END;
    b(push_wi); w(n1); RETURN
  ELSIF d.mode=m_ri THEN
    n1:=CARDINAL(word(d.val)); n2:=CARDINAL(word(d.val>>16));
  ELSIF d.mode#m_rm THEN error(assert);
  END;
  IF d.cop=c_mov THEN
    CASE d.size OF
      |1: b(mov_ali+d.r0); b(n1);
      |2: IF n1=0 THEN b(xor_wrm); b(md_reg+d.r0+d.r0*8);
          ELSE b(mov_axi+d.r0); w(n1);
          END;
      |4: IF n1=0 THEN b(xor_wrm); b(md_reg+d.r0+d.r0*8);
          ELSE b(mov_axi+d.r0); w(n1);
          END;
          IF n2=0 THEN b(xor_wrm); b(md_reg+d.r1+d.r1*8);
          ELSE b(mov_axi+d.r1); w(n2);
          END;
    END;
  ELSE
    CASE siz[stk-1] OF
      |1: IF (d.cop=c_xor) & (byte(n1)=byte{}) THEN RETURN END;
          IF (d.cop=c_or ) & (byte(n1)=byte{}) THEN RETURN END;
          IF (d.cop=c_and) & (byte(n1)=byte{0..7}) THEN RETURN END;
          b(imm_bm); b(md_reg+d.r0+cop_0[d.cop]); b(n1);
      |2: IF (d.cop=c_xor) & (n1=0) THEN RETURN END;
          IF (d.cop=c_or ) & (n1=0) THEN RETURN END;
          IF (d.cop=c_and) & (n1=0FFFFH) THEN RETURN END;
          IF (n1<=7FH) OR (n1>=0FF80H) THEN
            b(imm_wbm); b(md_reg+d.r0+cop_0[d.cop]); b(n1);
          ELSE
            b(imm_wm); b(md_reg+d.r0+cop_0[d.cop]); w(n1);
          END;
      |4: IF d.cop=c_cmp THEN error(assert) END;
          IF    (d.cop=c_xor) & (n1=0) THEN
          ELSIF (d.cop=c_or ) & (n1=0) THEN
          ELSIF (d.cop=c_and) & (n1=0FFFFH) THEN
          ELSIF (n1<=7FH) OR (n1>=0FF80H) THEN
            b(imm_wbm); b(md_reg+d.r0+cop_0[d.cop]); b(n1);
          ELSE
            b(imm_wm); b(md_reg+d.r0+cop_0[d.cop]); w(n1);
          END;
          IF (d.cop=c_xor) & (n2=0) THEN RETURN END;
          IF (d.cop=c_or ) & (n2=0) THEN RETURN END;
          IF (d.cop=c_and) & (n2=0FFFFH) THEN RETURN END;
          IF (n2<=7FH) OR (n2>=0FF80H) THEN
            b(imm_wbm); b(md_reg+d.r1+cop_1[d.cop]); b(n2);
          ELSE
            b(imm_wm); b(md_reg+d.r1+cop_1[d.cop]); w(n2);
          END;
    END;
  END;
END cmd_imm;

PROCEDURE mov_es_si(ofs: LONGINT);
BEGIN
  inc_address_high(SI,ofs);
  b(mov_srm); b(md_reg+SI+ES*8);
END mov_es_si;

PROCEDURE mov_es_ds(ofs: LONGINT);
BEGIN
  b(mov_msr); b(md_reg+SI+DS*8);
  mov_es_si(ofs);
END mov_es_ds;

PROCEDURE mov_es_external_ds(mod: CARDINAL; disp: LONGINT);
BEGIN
  b(seg_cs);
  IF (disp>=0) & (disp<10000H) THEN
    b(mov_srm); b(md_abs+ES*8);
    fixup; block^.fixup^.md:=fm_dft;
    w(mod*2);
  ELSE
    b(mov_wrm); b(md_abs+SI*8);
    fixup; block^.fixup^.md:=fm_dft;
    w(mod*2);
    mov_es_si(disp);
  END;
END mov_es_external_ds;

PROCEDURE cmd_stk(size: CARDINAL; disp: LONGINT; VAR am,sr,ofs: CARDINAL);
  VAR ax,dx,sg: CARDINAL;
BEGIN
  DEC(stk);
  IF disp#0 THEN
    IF pos[stk]=da THEN ax:=AX; dx:=DX
    ELSIF pos[stk]=bc THEN ax:=CX; dx:=BX
    ELSE b(pop_si); ax:=SI; b(pop_di); dx:=DI;
    END;
    inc_address_high(dx,disp);
    inc_address_low(ax,dx,CARDINAL(word(disp)));
  ELSE
    IF pos[stk]=da THEN ax:=AX; dx:=DX
    ELSIF pos[stk]=bc THEN ax:=CX; dx:=BX
    ELSE b(pop_si); ax:=SI; b(pop_es); dx:=0FFFFH;
    END;
  END;
  IF dx#0FFFFH THEN b(mov_srm); b(md_reg+dx+ES*8) END;
  IF ax#SI THEN b(mov_wrm); b(md_reg+ax+SI*8) END;
  am:=rm_si; sr:=ES; ofs:=0;
END cmd_stk;

PROCEDURE cmd_ass(si: BOOLEAN; ofs0,size: CARDINAL; disp: LONGINT;
                  VAR am,sr,ofs: CARDINAL);
  VAR sg,rr: CARDINAL;
BEGIN
  IF si THEN rr:=rm_si; b(seg_ss) ELSE rr:=rm_bp END;
  IF disp=0 THEN
    b(les);
    IF (ofs0<80H) OR (ofs0>=0FF80H) THEN b(md_b+rr+SI*8); b(ofs0);
    ELSE b(md_w+rr+SI*8); w(ofs0);
    END;
  ELSE
    b(mov_wrm); b(md_w+rr+DI*8); w(ofs0+2);
    inc_address_high(DI,disp);
    IF si THEN b(seg_ss) END;
    b(mov_wrm); b(md_w+rr+SI*8); w(ofs0);
    inc_address_low(SI,DI,CARDINAL(word(disp)));
    b(mov_srm); b(md_reg+DI+ES*8);
  END;
  am:=rm_si; sr:=ES; ofs:=0;
END cmd_ass;

PROCEDURE gen_access(a: access; size: CARDINAL; VAR am,sr,ofs: CARDINAL);
  VAR i,ofs0: CARDINAL; sg: INTEGER;
BEGIN
  CASE a.am OF
    |am_imm,am_Gimm,am_Limm:
      am:=9; ofs:=CARDINAL(word(a.n)); sr:=CARDINAL(word(a.n>>16));
    |am_abs:
      sg:=INTEGER(a.disp>>16);
      IF cpu_mode=0 THEN sg:=sg*1000H ELSE sg:=sg*8 END;
      ofs:=CARDINAL(word(a.disp));
      b(mov_sii); w(word(sg));
      b(mov_srm); b(md_reg+ES*8+SI);
      am:=8; sr:=ES;
    |am_G:
      am:=8; ofs:=CARDINAL(word(a.disp));
      IF a.level=0 THEN
        IF (a.disp>=0) & (a.disp<10000H) THEN sr:=DS;
        ELSE mov_es_ds(a.disp); sr:=ES;
        END;
      ELSE
        mov_es_external_ds(a.mod,a.disp); sr:=ES;
      END;
    |am_L:
      ofs:=CARDINAL(word(a.disp));
      IF a.level=level THEN
        am:=rm_bp; sr:=SS;
      ELSE
        b(mov_wrm); b(md_b+SI*8+rm_bp); b(4);
        FOR i:=a.level+2 TO level DO
          b(seg_ss); b(mov_wrm); b(md_b+SI*8+rm_si); b(4);
        END;
        am:=rm_si; sr:=SS;
      END;
    |am_aG:
      ofs:=CARDINAL(word(a.disp)); ofs0:=CARDINAL(word(a.n));
      IF a.disp=0 THEN
        IF a.mod#0 THEN
          mov_es_external_ds(a.mod,a.n); b(seg_es);
        ELSIF (a.n<0) OR (a.n>0FFFFH) THEN
          mov_es_ds(a.n); b(seg_es);
        END;
        b(les); b(md_abs+SI*8); w(ofs0);
        am:=rm_si; sr:=ES;
      ELSE
        IF a.mod#0 THEN
          mov_es_external_ds(a.mod,a.n);
          b(seg_es); b(mov_wrm); b(md_abs+SI*8); w(ofs0);
          b(seg_es); b(mov_wrm); b(md_abs+DI*8); w(ofs0+2);
        ELSIF (a.n<0) OR (a.n>0FFFFH) THEN
          mov_es_ds(a.n);
          b(seg_es); b(mov_wrm); b(md_abs+SI*8); w(ofs0);
          b(seg_es); b(mov_wrm); b(md_abs+DI*8); w(ofs0+2);
        ELSE
          b(mov_wrm); b(md_abs+SI*8); w(ofs0);
          b(mov_wrm); b(md_abs+DI*8); w(ofs0+2);
        END;
        inc_address_high(DI,a.disp);
        inc_address_low(SI,DI,ofs);
        b(mov_srm); b(md_reg+DI+ES*8);
        am:=rm_si; sr:=ES; ofs:=0;
      END;
    |am_aSTK: cmd_stk(size,a.disp,am,sr,ofs);
    |am_aL:
      ofs0:=CARDINAL(word(a.n));
      IF a.level=level THEN
        cmd_ass(FALSE,ofs0,size,a.disp,am,sr,ofs);
      ELSE
        b(mov_wrm); b(md_b+SI*8+rm_bp); b(4);
        FOR i:=a.level+2 TO level DO
          b(seg_ss); b(mov_wrm); b(md_b+SI*8+rm_si); b(4);
        END;
        cmd_ass(TRUE,ofs0,size,a.disp,am,sr,ofs);
      END;
  ELSE error(assert);
  END;
END gen_access;

PROCEDURE gen_cmd(d: cmd_desc; a: access);
  VAR am,sr,ofs: CARDINAL;
BEGIN
  gen_access(a,d.size,am,sr,ofs);
  IF am=9 THEN cmd_imm(d,ofs,sr);
  ELSE put_cmd(d,am,sr,ofs);
  END;
END gen_cmd;

PROCEDURE copt;
BEGIN
  alloc_reg(siz[stk-1]); pop_reg(2);
  IF pos[stk-1]=da THEN
    CASE siz[stk-1] OF
      |1: b(mov_brm); b(md_reg+CL+AL*8);
      |2: b(mov_wrm); b(md_reg+CX+AX*8);
      |4: b(mov_wrm); b(md_reg+CX+AX*8);
          b(mov_wrm); b(md_reg+BX+DX*8);
    END;
  ELSE
    CASE siz[stk-1] OF
      |1: b(mov_brm); b(md_reg+AL+CL*8);
      |2: b(mov_wrm); b(md_reg+AX+CX*8);
      |4: b(mov_wrm); b(md_reg+AX+CX*8);
          b(mov_wrm); b(md_reg+DX+BX*8);
    END;
  END;
END copt;

PROCEDURE cmd_stk_stk(cc: cmd_cop);
BEGIN
  pop_reg(2); DEC(stk);
  IF pos[stk]=da THEN
    CASE siz[stk] OF
      |1: b(cop_0[cc]);   b(md_reg+CL+AL*8);
      |2: b(cop_0[cc]+1); b(md_reg+CX+AX*8);
      |4: IF cc=c_cmp THEN error(assert) END;
          b(cop_0[cc]+1); b(md_reg+CX+AX*8);
          b(cop_1[cc]+1); b(md_reg+BX+DX*8);
    END;
  ELSE
    CASE siz[stk] OF
      |1: b(cop_0[cc]);   b(md_reg+AL+CL*8);
      |2: b(cop_0[cc]+1); b(md_reg+AX+CX*8);
      |4: IF cc=c_cmp THEN error(assert) END;
          b(cop_0[cc]+1); b(md_reg+AX+CX*8);
          b(cop_1[cc]+1); b(md_reg+DX+BX*8);
    END;
  END;
END cmd_stk_stk;

PROCEDURE swap_no_pop;
  VAR r: reg; s: INTEGER;
BEGIN
  r:=pos[stk-1]; pos[stk-1]:=pos[stk-2]; pos[stk-2]:=r;
  s:=siz[stk-1]; siz[stk-1]:=siz[stk-2]; siz[stk-2]:=s;
END swap_no_pop;

PROCEDURE cmd_stk_m(cc: cmd_cop; a: access);
  VAR d: cmd_desc;
BEGIN
  IF a.am=am_STK THEN cmd_stk_stk(cc); RETURN END;
  IF a.am=am_FPP THEN alloc_tmp_var(a,4); fpp_store(a,4) END;
  d.cop:=cc; d.size:=siz[stk-1]; d.mode:=m_rm;
  IF a.am=am_aSTK THEN
    d.size:=siz[stk-2];
    IF pos[stk-2]=da THEN d.r0:=AX; d.r1:=DX
    ELSIF pos[stk-2]=bc THEN d.r0:=CX; d.r1:=BX
    ELSIF pos[stk-1]=da THEN
      d.r0:=CX; d.r1:=BX; pos[stk-2]:=bc; b(pop_cx);
      IF siz[stk-2]=4 THEN b(pop_bx) END;
    ELSE
      d.r0:=AX; d.r1:=DX; pos[stk-2]:=da; b(pop_ax);
      IF siz[stk-1]=4 THEN b(pop_dx) END;
    END;
  ELSIF pos[stk-1]=da THEN d.r0:=AX; d.r1:=DX
  ELSIF pos[stk-1]=bc THEN d.r0:=CX; d.r1:=BX
  ELSE
    d.r0:=AX; d.r1:=DX; pos[stk-1]:=da; b(pop_ax);
    IF siz[stk-1]=4 THEN b(pop_dx) END;
  END;
  gen_cmd(d,a);
END cmd_stk_m;

PROCEDURE cmd_m_stk(cc: cmd_cop; a: access);
  VAR d: cmd_desc;
BEGIN
  IF a.am=am_STK THEN swap; cmd_stk_stk(cc); RETURN END;
  IF a.am=am_FPP THEN alloc_tmp_var(a,4); fpp_store(a,4) END;
  IF a.am=am_imm THEN error(assert) END;
  pop_reg(1); d.cop:=cc; d.size:=siz[stk-1]; d.mode:=m_mr;
  IF pos[stk-1]=da THEN d.r0:=AX; d.r1:=DX
  ELSE                  d.r0:=CX; d.r1:=BX
  END;
  IF a.am=am_aSTK THEN swap_no_pop END;
  gen_cmd(d,a);
END cmd_m_stk;

PROCEDURE cmd_m_imm(cc: cmd_cop; a: access; val: LONGINT; sz: CARDINAL);
  VAR d: cmd_desc;
BEGIN
  IF a.am=am_STK THEN
    IF siz[stk-1]#sz THEN error(assert) END;
    pop_reg(1); d.cop:=cc; d.size:=sz; d.mode:=m_ri; d.val:=val;
    IF pos[stk-1]=da THEN d.r0:=AX; d.r1:=DX
    ELSE                  d.r0:=CX; d.r1:=BX
    END;
  ELSE
    IF a.am=am_imm THEN error(assert) END;
    d.cop:=cc; d.size:=sz; d.mode:=m_mi; d.val:=val;
  END;
  gen_cmd(d,a);
END cmd_m_imm;

PROCEDURE load(a: access; sz: CARDINAL);
BEGIN
  IF a.am=am_STK THEN RETURN END;
  alloc_reg(sz);
  IF a.am=am_aSTK THEN swap END;
  cmd_stk_m(c_mov,a);
END load;

PROCEDURE store(to: access);
BEGIN
  cmd_m_stk(c_mov,to); DEC(stk);
END store;

PROCEDURE load_store(to,fr: access; sz: CARDINAL);
  VAR d: cmd_desc;
BEGIN
  IF (fr.am=am_imm) OR (fr.am=am_Gimm) OR (fr.am=am_Limm) THEN
    d.cop:=c_mov; d.size:=sz; d.val:=fr.n; d.mode:=m_mi; gen_cmd(d,to);
  ELSE
    load(fr,sz); store(to);
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
      |1: b(or_brm); b(md_reg+AL+AL*8);
      |2: b(or_wrm); b(md_reg+AX+AX*8);
      |4: b(or_wrm); b(md_reg+DX+AX*8);
    END;
  ELSE
    CASE siz[stk-1] OF
      |1: b(or_brm); b(md_reg+CL+CL*8);
      |2: b(or_wrm); b(md_reg+CX+CX*8);
      |4: b(or_wrm); b(md_reg+BX+CX*8);
    END;
  END;
  DEC(stk);
END set_flag_z;

PROCEDURE add_adr_const(ofs: LONGINT);
  VAR ax,dx: CARDINAL;
BEGIN
  pop_reg(1);
  IF pos[stk-1]=da THEN ax:=AX; dx:=DX ELSE ax:=CX; dx:=BX END;
  inc_address_low(ax,dx,CARDINAL(word(ofs)));
  inc_address_high(dx,ofs);
END add_adr_const;

PROCEDURE sub_adr_const(ofs: LONGINT);
  VAR ax,dx: CARDINAL;
BEGIN
  pop_reg(1);
  IF pos[stk-1]=da THEN ax:=AX; dx:=DX ELSE ax:=CX; dx:=BX END;
  dec_address_low(ax,dx,CARDINAL(word(ofs)));
  dec_address_high(dx,ofs);
END sub_adr_const;

PROCEDURE load_adr(a: access);
  VAR ax,dx,ofs,i: CARDINAL; sg: INTEGER; d: cmd_desc;
BEGIN
  IF a.am=am_aSTK THEN add_adr_const(a.disp); RETURN END;
  alloc_reg(4);
  IF pos[stk-1]=da THEN ax:=AX; dx:=DX ELSE ax:=CX; dx:=BX END;
  d.mode:=m_rm; d.size:=4; d.cop:=c_mov; d.r0:=ax; d.r1:=dx;
  CASE a.am OF
    |am_abs :
      sg:=INTEGER(a.disp>>16);
      IF cpu_mode=0 THEN sg:=sg*1000H ELSE sg:=sg*8 END;
      cmd_imm(d,CARDINAL(word(a.disp)),CARDINAL(word(sg)));
    |am_G,am_Gimm:
      IF pos[stk-1]=da THEN b(mov_axi); w(word(a.disp));
      ELSE                  b(mov_cxi); w(word(a.disp));
      END;
      IF a.mod=0 THEN
        b(mov_msr); b(md_reg+dx+DS*8);
      ELSE
        b(seg_cs); b(mov_wrm); b(md_abs+dx*8);
        fixup; block^.fixup^.md:=fm_dft; w(a.mod*2);
      END;
      inc_address_high(dx,a.disp);
    |am_L,am_Limm:
      ofs:=CARDINAL(word(a.disp));
      IF a.level=level THEN
        b(mov_msr); b(md_reg+dx+SS*8); b(lea);
        IF (ofs>=80H) & (ofs<0FF80H) THEN b(md_w+rm_bp+ax*8); w(ofs);
        ELSE b(md_b+rm_bp+ax*8); b(ofs);
        END;
      ELSE
        b(mov_wrm); b(md_b+SI*8+rm_bp); b(4);
        FOR i:=a.level+2 TO level DO
          b(seg_ss); b(mov_wrm); b(md_b+SI*8+rm_si); b(4);
        END;
        b(mov_msr); b(md_reg+dx+SS*8); b(lea);
        IF (ofs>=80H) & (ofs<0FF80H) THEN b(md_w+rm_si+ax*8); w(ofs);
        ELSE b(md_b+rm_si+ax*8); b(ofs);
        END;
      END;
    |am_aG:
      ofs:=CARDINAL(word(a.n));
      IF a.mod=0 THEN
        IF (a.n>=0) & (a.n<10000H) THEN put_cmd(d,8,DS,ofs);
        ELSE mov_es_ds(a.n); put_cmd(d,8,ES,ofs);
        END;
      ELSE
        mov_es_external_ds(a.mod,a.n);
        put_cmd(d,8,ES,ofs);
      END;
      add_adr_const(a.disp);
    |am_aL:
      IF a.level=level THEN
        put_cmd(d,rm_bp,SS,CARDINAL(word(a.n)));
      ELSE
        b(mov_wrm); b(md_b+SI*8+rm_bp); b(4);
        FOR i:=a.level+2 TO level DO
          b(seg_ss); b(mov_wrm); b(md_b+SI*8+rm_si); b(4);
        END;
        put_cmd(d,rm_si,SS,CARDINAL(word(a.n)));
      END;
      add_adr_const(a.disp);
  END;
END load_adr;

PROCEDURE stot_adr(a: access);
  VAR ofs,i: CARDINAL; sg: INTEGER; d: cmd_desc;
BEGIN
  CASE a.am OF
    |am_aSTK:
      add_adr_const(a.disp); DEC(stk);
      IF    pos[stk]=da THEN b(push_dx); b(push_ax);
      ELSIF pos[stk]=bc THEN b(push_bx); b(push_cx);
      END;
    |am_abs :
      sg:=INTEGER(a.disp>>16);
      IF cpu_mode=0 THEN sg:=sg*1000H ELSE sg:=sg*8 END;
      IF cpu_type=0 THEN error(0);
      ELSE b(push_wi); w(word(sg)); b(push_wi); w(word(a.disp));
      END;
    |am_G,am_Gimm:
      IF (a.disp<0) OR (a.disp>=10000H) THEN error(0) END;
      IF a.mod=0 THEN b(push_ds);
      ELSE
        b(seg_cs); b(grp2_w); b(md_abs+g2_push);
        fixup; block^.fixup^.md:=fm_dft;
        w(a.mod*2);
      END;
      IF cpu_type=0 THEN error(0);
      ELSE b(push_wi); w(word(a.disp));
      END;
    |am_L,am_Limm:
      b(push_ss);
      ofs:=CARDINAL(word(a.disp));
      IF a.level=level THEN
        b(lea);
        IF (ofs>=80H) & (ofs<0FF80H) THEN b(md_w+rm_bp+SI*8); w(ofs);
        ELSE b(md_b+rm_bp+SI*8); b(ofs);
        END;
      ELSE
        b(mov_wrm); b(md_b+SI*8+rm_bp); b(4);
        FOR i:=a.level+2 TO level DO
          b(seg_ss); b(mov_wrm); b(md_b+SI*8+rm_si); b(4);
        END;
        IF (ofs>=80H) & (ofs<0FF80H) THEN
          b(imm_wm); b(md_reg+i_add+SI); w(ofs);
        ELSE
          b(imm_wbm); b(md_reg+i_add+SI); b(ofs);
        END;
      END;
      b(push_si);
    |am_aG:
      IF (a.n<0) OR (a.n>=10000H) THEN error(0) END;
      IF a.disp#0 THEN
        load_adr(a); DEC(stk);
        IF    pos[stk]=da THEN b(push_dx); b(push_ax);
        ELSIF pos[stk]=bc THEN b(push_bx); b(push_cx);
        END;
      ELSE
        ofs:=CARDINAL(word(a.n));
        IF a.mod=0 THEN
          b(grp2_w); b(md_abs+g2_push); w(ofs+2);
          b(grp2_w); b(md_abs+g2_push); w(ofs);
        ELSE
          mov_es_external_ds(a.mod,a.n);
          b(seg_es); b(grp2_w); b(md_abs+g2_push); w(ofs+2);
          b(seg_es); b(grp2_w); b(md_abs+g2_push); w(ofs);
        END;
      END;
    |am_aL:
      IF a.disp#0 THEN
        load_adr(a); DEC(stk);
        IF    pos[stk]=da THEN b(push_dx); b(push_ax);
        ELSIF pos[stk]=bc THEN b(push_bx); b(push_cx);
        END;
      ELSE
        ofs:=CARDINAL(word(a.n));
        IF a.level=level THEN
          b(grp2_w); b(md_w+g2_push+rm_bp); w(ofs+2);
          b(grp2_w); b(md_w+g2_push+rm_bp); w(ofs);
        ELSE
          b(mov_wrm); b(md_b+SI*8+rm_bp); b(4);
          FOR i:=a.level+2 TO level DO
            b(seg_ss); b(mov_wrm); b(md_b+SI*8+rm_si); b(4);
          END;
          b(seg_ss); b(grp2_w); b(md_w+g2_push+rm_si); w(ofs+2);
          b(seg_ss); b(grp2_w); b(md_w+g2_push+rm_si); w(ofs);
        END;
      END;
  ELSE error(0);
  END;
END stot_adr;

PROCEDURE neg;
  VAR ax,dx: CARDINAL;
BEGIN
  pop_reg(1);
  IF pos[stk-1]=da THEN ax:=AX; dx:=DX ELSE ax:=CX; dx:=BX END;
  CASE siz[stk-1] OF
    |1: b(grp1_bm); b(md_reg+ax+g1_neg);
    |2: b(grp1_wm); b(md_reg+ax+g1_neg);
    |4: b(grp1_wm); b(md_reg+dx+g1_neg);
        b(grp1_wm); b(md_reg+ax+g1_neg);
        b(imm_wbm); b(md_reg+dx+i_sbb); b(0);
  END;
END neg;

PROCEDURE shl_c(n: CARDINAL);
  VAR i,ax,dx: CARDINAL;
BEGIN
  pop_reg(1);
  IF pos[stk-1]=da THEN ax:=AX; dx:=DX ELSE ax:=CX; dx:=BX END;
  CASE siz[stk-1] OF
    |1: FOR i:=1 TO n DO b(shift_bm1); b(md_reg+ax+s_shl) END;
    |2: FOR i:=1 TO n DO b(shift_wm1); b(md_reg+ax+s_shl) END;
    |4: IF n>=16 THEN
          b(mov_wrm); b(md_reg+dx*8+ax);
          IF ax=AX THEN b(mov_axi) ELSE b(mov_cxi) END;
          w(0); n:=n-16;
        END;
        FOR i:=1 TO n DO
          b(shift_wm1); b(md_reg+ax+s_shl);
          b(shift_wm1); b(md_reg+dx+s_rcl);
        END;
  END;
END shl_c;

PROCEDURE sar_c(n: CARDINAL);
  VAR i,ax,dx: CARDINAL;
BEGIN
  pop_reg(1);
  IF pos[stk-1]=da THEN ax:=AX; dx:=DX ELSE ax:=CX; dx:=BX END;
  CASE siz[stk-1] OF
    |1: FOR i:=1 TO n DO b(shift_bm1); b(md_reg+ax+s_sar) END;
    |2: FOR i:=1 TO n DO b(shift_wm1); b(md_reg+ax+s_sar) END;
    |4: IF (n>=16) & (ax=AX) THEN
          b(mov_wrm); b(md_reg+ax*8+dx);
          b(cwd); n:=n-16;
        END;
        FOR i:=1 TO n DO
          b(shift_wm1); b(md_reg+dx+s_sar);
          b(shift_wm1); b(md_reg+ax+s_rcr);
        END;
  END;
END sar_c;

PROCEDURE power_2(n: LONGINT; VAR i: CARDINAL): BOOLEAN;
  VAR k: LONGINT;
BEGIN
  IF n<=0 THEN RETURN FALSE END;
  i:=30;
  LOOP
    k:=LONGINT(dword{i});
    IF n>=k THEN RETURN n=k END;
    DEC(i);
  END;
END power_2;

PROCEDURE call_rts(n: CARDINAL);
BEGIN
  save; b(call_id); fixup; block^.fixup^.rts:=n; w(0); w(0);
END call_rts;

PROCEDURE idiv(a: access);
  VAR i: CARDINAL;
BEGIN
  IF a.am=am_aSTK THEN load(a,siz[stk-2])
  ELSIF a.am IN am_const THEN
    IF power_2(a.n,i) THEN sar_c(i); RETURN END;
    IF power_2(-a.n,i) THEN sar_c(i); neg; RETURN END;
    load(a,siz[stk-1]);
  ELSIF a.am#am_STK THEN load(a,siz[stk-1])
  END;
  CASE siz[stk-1] OF
    |1: call_rts(1);
    |2: call_rts(2);
    |4: call_rts(3);
  END;
  DEC(stk); pos[stk-1]:=da;
END idiv;

PROCEDURE imul(a: access);
  VAR i: CARDINAL;
BEGIN
  IF a.am=am_aSTK THEN load(a,siz[stk-2])
  ELSIF a.am IN am_const THEN
    IF a.n=0 THEN drop; load(a,siz[stk]); RETURN END;
    IF power_2(a.n,i) THEN shl_c(i); RETURN END;
    IF power_2(-a.n,i) THEN shl_c(i); neg; RETURN END;
    load(a,siz[stk-1]);
  ELSIF a.am#am_STK THEN load(a,siz[stk-1])
  END;
  IF siz[stk-1]=4 THEN
    call_rts(4); DEC(stk); pos[stk-1]:=da; siz[stk-1]:=4;
  ELSE
    pop_reg(2);
    IF siz[stk-1]=2 THEN
      b(grp1_wm); b(md_reg+CX+g1_imul);
      DEC(stk); pos[stk-1]:=da;
    ELSE
      b(grp1_bm); b(md_reg+CL+g1_imul);
      DEC(stk); pos[stk-1]:=da;
    END;
  END;
END imul;

PROCEDURE add_adr(a: access; sz: CARDINAL; sg: BOOLEAN);
  VAR ax,dx,cx,bx: CARDINAL;
BEGIN
  CASE a.am OF
    |am_STK :
    |am_imm,am_Gimm,am_Limm: add_adr_const(a.n); RETURN;
  ELSE load(a,sz); IF sg THEN set_size(4,TRUE) END;
  END;
  pop_reg(2);
  IF siz[stk-2]#4 THEN error(assert) END;
  IF pos[stk-2]=da THEN
    ax:=AX; dx:=DX; cx:=CX; bx:=BX;
    IF sz=1 THEN b(mov_chi); b(0) END;
  ELSE
    ax:=CX; dx:=BX; cx:=AX; bx:=DX;
    IF sz=1 THEN b(mov_ahi); b(0) END;
  END;
  b(add_wrm); b(md_reg+ax*8+cx);
  inc_address_carry(dx);
  IF sz>2 THEN
    IF cpu_type>=1 THEN
      b(shift_wmi); b(md_reg+bx+s_shl);
      IF cpu_mode=0 THEN b(12) ELSE b(3) END;
    ELSE
      IF pos[stk-2]=bc THEN b(push_cx) END;
      b(mov_cli);
      IF cpu_mode=0 THEN b(12) ELSE b(3) END;
      b(shift_wmc); b(md_reg+bx+s_shl);
      IF pos[stk-2]=bc THEN b(pop_cx) END;
    END;
    b(add_wrm); b(md_reg+dx*8+bx);
  END;
  DEC(stk);
END add_adr;

PROCEDURE sub_adr(a: access; sz: CARDINAL; sg: BOOLEAN);
  VAR ax,dx,cx,bx: CARDINAL;
BEGIN
  CASE a.am OF
    |am_STK :
    |am_imm,am_Gimm,am_Limm: sub_adr_const(a.n); RETURN;
  ELSE load(a,sz); IF sg THEN set_size(4,TRUE) END;
  END;
  pop_reg(2);
  IF sz#4 THEN error(assert) END;
  IF pos[stk-2]=da THEN
    ax:=AX; dx:=DX; cx:=CX; bx:=BX;
    IF sz=1 THEN b(mov_chi); b(0) END;
  ELSE
    ax:=CX; dx:=BX; cx:=AX; bx:=DX;
    IF sz=1 THEN b(mov_ahi); b(0) END;
  END;
  b(sub_wrm); b(md_reg+ax*8+cx);
  dec_address_carry(dx);
  IF sz>2 THEN
    IF cpu_type>=1 THEN
      b(shift_wmi); b(md_reg+bx+s_shl);
      IF cpu_mode=0 THEN b(12) ELSE b(3) END;
    ELSE
      IF pos[stk-2]=bc THEN b(push_cx) END;
      b(mov_cli);
      IF cpu_mode=0 THEN b(12) ELSE b(3) END;
      b(shift_wmc); b(md_reg+bx+s_shl);
      IF pos[stk-2]=bc THEN b(pop_cx) END;
    END;
    b(sub_wrm); b(md_reg+dx*8+bx);
  END;
  DEC(stk);
END sub_adr;

PROCEDURE drop;
BEGIN
  pop_reg(1); DEC(stk);
END drop;

PROCEDURE save;
BEGIN
  IF (stk>1) & (pos[stk-2]=da) THEN
    IF siz[stk-2]=4 THEN b(push_dx) END;
    b(push_ax); pos[stk-2]:=stack;
  ELSIF (stk>1) & (pos[stk-2]=bc) THEN
    IF siz[stk-2]=4 THEN b(push_bx) END;
    b(push_cx); pos[stk-2]:=stack;
  END;
  IF (stk>0) & (pos[stk-1]=da) THEN
    IF siz[stk-1]=4 THEN b(push_dx) END;
    b(push_ax); pos[stk-1]:=stack;
  ELSIF (stk>0) & (pos[stk-1]=bc) THEN
    IF siz[stk-1]=4 THEN b(push_bx) END;
    b(push_cx); pos[stk-1]:=stack;
  END;
END save;

PROCEDURE stot(a: access; sz: CARDINAL);
  VAR d: cmd_desc;
BEGIN
  IF sz=1 THEN load(a,sz); a.am:=am_STK END;
  IF (cpu_type<1) &
     ((a.am=am_imm) OR (a.am=am_Gimm) OR (a.am=am_Limm)) THEN
    load(a,sz); a.am:=am_STK
  END;
  IF a.am#am_STK THEN
    d.cop:=c_mov; d.mode:=m_sm; d.size:=sz; gen_cmd(d,a);
  ELSE
    IF (stk#1) & (pos[stk-2]#stack) THEN error(assert) END;
    DEC(stk);
    IF pos[stk]=da THEN
      IF siz[stk]=4 THEN b(push_dx) END;
      b(push_ax);
    ELSIF pos[stk]=bc THEN
      IF siz[stk]=4 THEN b(push_bx) END;
      b(push_cx);
    END;
  END;
END stot;

PROCEDURE func_ret(sz: CARDINAL);
BEGIN
  alloc_reg(sz);
  IF pos[stk-1]#da THEN error(assert) END;
END func_ret;

PROCEDURE hold_out;
BEGIN
  IF stk#1 THEN error(assert) END;
  pop_reg(1); DEC(stk);
  IF pos[stk]=bc THEN
    IF siz[stk]=4 THEN b(xchg_wmr); b(md_reg+DX+BX*8) END;
    b(xchg_cx);
  END;
END hold_out;

PROCEDURE put_fpp_cmd(cop,am,sr,ofs: CARDINAL);
  VAR rr: CARDINAL; sgo: BOOLEAN;
BEGIN
  b(wait);
  sgo:=(am IN {2,3,6}) & (sr#2) OR (am IN {0,1,4,5,7,8}) & (sr#3);
  IF sgo THEN b(seg_es+sr*8) END;
  b(cop);
  rr:=cop>>8;
  IF am=8 THEN b(006B+rr); w(ofs)
  ELSIF (am#6) & (ofs=0) THEN b(am+rr);
  ELSIF (ofs<80H) OR (ofs>=0FF80H) THEN b(am+rr+100B); b(ofs);
  ELSE b(am+rr+200B); w(ofs);
  END;
END put_fpp_cmd;

PROCEDURE gen_fpp_cmd(cop,size: CARDINAL; a: access);
  VAR am,sr,ofs: CARDINAL; ptr: POINTER TO LONGINT;
BEGIN
  IF a.am=am_imm THEN
    new_glo_const(size,ptr,ofs); a.disp:=LONGINT(ofs);
    ptr^:=a.n; a.am:=am_G; a.mod:=0;
  END;
  IF a.am=am_Gimm THEN a.am:=am_G END;
  IF a.am=am_Limm THEN a.am:=am_L END;
  gen_access(a,size,am,sr,ofs);
  put_fpp_cmd(cop,am,sr,ofs);
END gen_fpp_cmd;

PROCEDURE calc_fpp_flags;
  VAR a: access; am,sr,ofs,ax: CARDINAL;
BEGIN
  DEC(stk);
  alloc_reg(1);
  IF pos[stk-1]=da THEN ax:=AX ELSE ax:=CX END;
  alloc_tmp_var(a,2);
  gen_access(a,2,am,sr,ofs);
  put_access2(am,sr,ofs,70B,09BH,0DDH);
  put_access2(am,sr,ofs+1,ax*8,09BH,mov_brm);
END calc_fpp_flags;

PROCEDURE fpp_load(cc: fpp_cop; a: access; sz: CARDINAL);
  VAR cop: CARDINAL;
BEGIN
  IF a.am=am_FPP THEN
    CASE cc OF
      |f_mov: RETURN
      |f_add: b(9BH); b(0DEH); b(0C1H);
      |f_mul: b(9BH); b(0DEH); b(0C9H);
      |f_sub: b(9BH); b(0DEH); b(0E9H);
      |f_div: b(9BH); b(0DEH); b(0F9H);
      |f_cmp: b(9BH); b(0DEH); b(0D9H); calc_fpp_flags;
    END;
    DEC(fpp_stk);
  ELSIF a.am=am_STK THEN
    IF sz#4 THEN error(assert) END;
    alloc_tmp_var(a,sz); store(a); fpp_load(cc,a,sz);
  ELSE
    IF (sz#4) & (sz#8) THEN error(assert) END;
    CASE cc OF
      |f_mov: cop:=0D9H+ORD(sz=8)*4; INC(fpp_stk);
      |f_add: cop:=0D8H+ORD(sz=8)*4;
      |f_mul: cop:=08D8H+ORD(sz=8)*4;
      |f_sub: cop:=20D8H+ORD(sz=8)*4;
      |f_div: cop:=30D8H+ORD(sz=8)*4;
      |f_cmp: cop:=18D8H+ORD(sz=8)*4;
    END;
    gen_fpp_cmd(cop,sz,a);
    IF cc=f_cmp THEN calc_fpp_flags END;
  END;
END fpp_load;

PROCEDURE fpp_store(a: access; sz: CARDINAL);
  VAR cop: CARDINAL;
BEGIN
  IF a.am=am_FPP THEN
  ELSIF a.am=am_STK THEN
    IF sz#4 THEN error(assert) END;
    alloc_tmp_var(a,sz); fpp_store(a,sz); load(a,sz);
  ELSE
    cop:=18D9H+ORD(sz=8)*4;
    gen_fpp_cmd(cop,sz,a); DEC(fpp_stk);
    b(wait);
  END;
END fpp_store;

PROCEDURE fpp_load_integer(cc: fpp_cop; a: access; sz: CARDINAL);
  VAR cop: CARDINAL;
BEGIN
  IF a.am=am_FPP THEN
    CASE cc OF
      |f_mov: RETURN
      |f_add: b(9BH); b(0DEH); b(0C1H);
      |f_mul: b(9BH); b(0DEH); b(0C9H);
      |f_sub: b(9BH); b(0DEH); b(0E9H);
      |f_div: b(9BH); b(0DEH); b(0F9H);
      |f_cmp: b(9BH); b(0DEH); b(0D9H); calc_fpp_flags;
    END;
    DEC(fpp_stk);
  ELSIF a.am=am_STK THEN
    IF sz#4 THEN error(assert) END;
    alloc_tmp_var(a,sz); store(a); fpp_load_integer(cc,a,sz);
  ELSE
    IF (sz#2) & (sz#4) THEN error(assert) END;
    CASE cc OF
      |f_mov: cop:= 0DBH+ORD(sz=2)*4; INC(fpp_stk);
      |f_add: cop:= 0DAH+ORD(sz=2)*4;
      |f_mul: cop:=08DAH+ORD(sz=2)*4;
      |f_sub: cop:=20DAH+ORD(sz=2)*4;
      |f_div: cop:=30DAH+ORD(sz=2)*4;
      |f_cmp: cop:=18DAH+ORD(sz=2)*4;
    END;
    gen_fpp_cmd(cop,sz,a);
    IF cc=f_cmp THEN calc_fpp_flags END;
  END;
END fpp_load_integer;

PROCEDURE fpp_store_integer(a: access; sz: CARDINAL);
  VAR cop: CARDINAL;
BEGIN
  IF a.am=am_FPP THEN
  ELSIF a.am=am_STK THEN
    IF (sz#2) & (sz#4) THEN error(assert) END;
    alloc_tmp_var(a,sz); fpp_store_integer(a,sz); load(a,sz);
  ELSE
    CASE sz OF
      |2: cop:=18DFH;
      |4: cop:=18DBH;
      |8: cop:=38DFH;
    END;
    gen_fpp_cmd(cop,sz,a); DEC(fpp_stk);
    b(wait);
  END;
END fpp_store_integer;

PROCEDURE fpp_ucmd(cc: fpp_ucop);
BEGIN
  CASE cc OF
    |u_abs: b(09BH); b(0D9H); b(0E1H);
    |u_neg: b(09BH); b(0D9H); b(0E0H);
  END;
END fpp_ucmd;

BEGIN
  stk:=0; fpp_stk:=0; top.am:=am_STK; level:=0; busy_mem:=0;
  block:=NIL; last_line:=0;
  cpu_type:=1; cpu_mode:=1;
END inCmd.
