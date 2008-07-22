IMPLEMENTATION MODULE inAsm; (*$U+ Sem 12-Mar-91. (c) KRONOS *)

FROM SYSTEM      IMPORT WORD, ADR;

IMPORT  c   : inCmd;
IMPORT  pc  : pcTab;
IMPORT  str : Strings;
IMPORT  vrs : inVars;
IMPORT  sym : inSym;
IMPORT  pcSystem;

WITH STORAGE : pcSystem;

TYPE
  string = ARRAY [0..15] OF CHAR;

VAR
  src   : DYNARR OF CHAR;
  s_pos : INTEGER;
  s_buf : ARRAY [0..1023] OF CHAR;
  tbl   : DYNARR OF RECORD
    name : string;
    val  : INTEGER;
  END;
  tbl_sz: INTEGER;
  cmd   : string;
  fail  : INTEGER;
  gen   : BOOLEAN;

PROCEDURE error(VAL s: ARRAY OF CHAR; SEQ x: WORD);
  VAR tmp: ARRAY [0..79] OF CHAR;
BEGIN
  str.print(tmp,s,x);
  pc.error(0,0,'%s - %s',s_buf,tmp); HALT(50h);
END error;

PROCEDURE get_line;
  VAR i: INTEGER;
BEGIN
  i:=0;
  WHILE (s_pos<=HIGH(src)) & (src[s_pos]>=' ') & (src[s_pos]#'%') DO
    IF i<HIGH(s_buf) THEN s_buf[i]:=src[s_pos]; INC(i) END;
    INC(s_pos);
  END;
  INC(s_pos);
  s_buf[i]:=0c;
END get_line;

PROCEDURE def(): INTEGER;
BEGIN
  IF LEN(tbl)<=tbl_sz THEN RESIZE(tbl,LEN(tbl)+10) END;
  INC(tbl_sz); RETURN tbl_sz-1;
END def;

PROCEDURE find(VAL s: ARRAY OF CHAR): INTEGER;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO tbl_sz-1 DO
    IF tbl[i].name=s THEN RETURN i END;
  END;
  RETURN -1;
END find;

PROCEDURE dig(pos: INTEGER): BOOLEAN;
BEGIN
  RETURN (s_buf[pos]>='0') & (s_buf[pos]<='9');
END dig;

PROCEDURE d_number(VAR p,v: INTEGER): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  i:=p;
  WHILE s_buf[i]=' ' DO INC(i) END;
  IF NOT dig(i) THEN RETURN FALSE END;
  v:=0;
  REPEAT
    v:=(ORD(s_buf[i])-ORD('0')+v*10) MOD 10000h; INC(i);
  UNTIL NOT dig(i);
  p:=i; RETURN TRUE;
END d_number;

PROCEDURE h_number(VAR p,v: INTEGER): BOOLEAN;
  VAR i,n: INTEGER; ch: CHAR;
BEGIN
  i:=p;
  WHILE s_buf[i]=' ' DO INC(i) END;
  IF NOT dig(i) THEN RETURN FALSE END;
  n:=0; ch:=s_buf[p];
  REPEAT
    IF ch>='a' THEN n:=(ORD(ch)-ORD('a')+10+n*16) MOD 10000h;
    ELSE            n:=(ORD(ch)-ORD('0')+n*16) MOD 10000h;
    END;
    INC(i); ch:=s_buf[i];
    IF (ch>='A') & (ch<='F') THEN ch:=CHAR(ORD(ch)-ORD('A')+ORD('a')) END;
  UNTIL ((ch<'0') OR (ch>'9')) & ((ch<'a') OR (ch>'f'));
  IF (s_buf[i]='h') OR (s_buf[i]='H') THEN p:=i+1; v:=n; RETURN TRUE END;
  RETURN FALSE;
END h_number;

PROCEDURE letter(pos: INTEGER): BOOLEAN;
BEGIN
  IF (s_buf[pos]>='A') & (s_buf[pos]<='Z') THEN RETURN TRUE END;
  IF (s_buf[pos]>='a') & (s_buf[pos]<='z') THEN RETURN TRUE END;
  IF s_buf[pos]>=300c THEN RETURN TRUE END;
  RETURN (s_buf[pos]='?') OR (s_buf[pos]='_');
END letter;

PROCEDURE ident(VAR pos: INTEGER; VAR nm: ARRAY OF CHAR): BOOLEAN;
  VAR i,j: INTEGER;
BEGIN
  j:=pos;
  WHILE s_buf[j]=' ' DO INC(j) END;
  IF NOT letter(j) THEN RETURN FALSE END;
  i:=0;
  REPEAT
    IF i<HIGH(nm) THEN nm[i]:=s_buf[j]; INC(i) END; INC(j);
  UNTIL NOT letter(j) & NOT dig(j);
  nm[i]:=0c; pos:=j; RETURN TRUE;
END ident;

PROCEDURE name_number(VAR p,v: INTEGER): BOOLEAN;
  VAR i: INTEGER; nm: string; var: pc.ref;
BEGIN
  IF NOT ident(p,nm) THEN RETURN FALSE END;
  var:=pc.find_var(nm);
  IF (var=NIL) OR (var^.md#pc.const) THEN
    RETURN FALSE;
--    error('constant "%s" not found',nm); v:=0;
  ELSIF (vrs.vars[var^.adr].am=sym.am_Gimm) OR
        (vrs.vars[var^.adr].am=sym.am_imm) THEN
    v:=vrs.vars[var^.adr].n;
  ELSE error('"%s" has unsuitable access mode',nm); v:=0;
  END;
  RETURN TRUE;
END name_number;

PROCEDURE label_number(VAR p,v: INTEGER): BOOLEAN;
  PROCEDURE ap(VAR i: INTEGER): BOOLEAN;
  BEGIN
    WHILE s_buf[i]=' ' DO INC(i) END;
    IF s_buf[i]="'" THEN INC(i); RETURN TRUE ELSE RETURN FALSE END;
  END ap;
  VAR i,var: INTEGER; nm,s: string;
BEGIN
  i:=p;
  IF NOT (ident(i,nm) & ap(i) & ident(i,s) & (s='OFS')) THEN RETURN FALSE END;
  v:=0;
  IF gen THEN
    var:=find(nm);
    IF var<0 THEN error('label "%s" not found',nm); v:=0;
    ELSE v:=tbl[var].val;
    END;
  END;
  p:=i;
  RETURN TRUE;
END label_number;

PROCEDURE expression_a(VAR p,v: INTEGER): BOOLEAN;
BEGIN
  RETURN h_number(p,v) OR d_number(p,v) OR
         label_number(p,v) OR name_number(p,v);
END expression_a;

PROCEDURE expression(VAR p,v: INTEGER): BOOLEAN;
  VAR i,v1: INTEGER; r: BOOLEAN;
BEGIN
  IF NOT expression_a(p,v) THEN RETURN FALSE END;
  i:=p;
  LOOP
    v1:=0;
    IF s_buf[i]='+' THEN INC(i); r:=expression_a(i,v1);
    ELSIF s_buf[i]='-' THEN INC(i); r:=expression_a(i,v1); v1:=-v1;
    ELSE EXIT;
    END;
    IF NOT r THEN EXIT END;
    v:=v+v1; p:=i;
  END;
  RETURN TRUE;
END expression;

PROCEDURE offset_expression(VAR pos,ofs: INTEGER): BOOLEAN;
  VAR o,i: INTEGER; r: BOOLEAN;
BEGIN
  i:=pos;
  WHILE s_buf[i]=' ' DO INC(i) END;
  LOOP
    o:=0;
    IF s_buf[i]='+' THEN INC(i); r:=expression_a(i,o)
    ELSIF s_buf[i]='-' THEN INC(i); r:=expression_a(i,o); o:=-o;
    ELSE EXIT;
    END;
    IF NOT r THEN EXIT END;
    pos:=i; ofs:=ofs+o;
  END;
  RETURN TRUE;
END offset_expression;

PROCEDURE adr_expression(VAR p,sz,ofs: INTEGER): BOOLEAN;
  VAR nm: string; i,o: INTEGER; a: BOOLEAN; var: pc.ref;
BEGIN
  i:=p; o:=0;
  LOOP
    IF NOT ident(i,nm) THEN p:=i; ofs:=o; RETURN TRUE END;
    IF nm='BYTE' THEN
      IF (sz>0) & (sz#1) THEN RETURN FALSE END;
      sz:=1;
    ELSIF nm='WORD' THEN
      IF (sz>0) & (sz#2) THEN RETURN FALSE END;
      sz:=2;
    ELSE EXIT
    END;
  END;
  a:=nm='ADR';
  IF a & NOT ident(i,nm) THEN RETURN FALSE END;
  var:=pc.find_var(nm);
  IF (var=NIL) OR (var^.md#pc.var) THEN
    RETURN FALSE;
--    error('variable "%s" not found',nm);
  ELSIF NOT a & (
        (vrs.vars[var^.adr].am=sym.am_G) OR
        (vrs.vars[var^.adr].am=sym.am_Gimm) OR
        (vrs.vars[var^.adr].am=sym.am_Gstr)) THEN
    o:=vrs.vars[var^.adr].disp;
  ELSIF a & (vrs.vars[var^.adr].am=sym.am_aG) THEN
    o:=vrs.vars[var^.adr].n;
  ELSE error('"%s" has unsuitable access mode',nm); o:=0;
  END;
  IF NOT offset_expression(i,o) THEN RETURN FALSE END;
  ofs:=o; p:=i; RETURN TRUE;
END adr_expression;

PROCEDURE reg8_access(VAR p,rg: INTEGER): BOOLEAN;
  VAR s: string; i: INTEGER;
BEGIN
  i:=p;
  IF NOT ident(i,s) THEN RETURN FALSE END;
  IF s='al' THEN rg:=0; p:=i; RETURN TRUE END;
  IF s='cl' THEN rg:=1; p:=i; RETURN TRUE END;
  IF s='dl' THEN rg:=2; p:=i; RETURN TRUE END;
  IF s='bl' THEN rg:=3; p:=i; RETURN TRUE END;
  IF s='ah' THEN rg:=4; p:=i; RETURN TRUE END;
  IF s='ch' THEN rg:=5; p:=i; RETURN TRUE END;
  IF s='dh' THEN rg:=6; p:=i; RETURN TRUE END;
  IF s='bh' THEN rg:=7; p:=i; RETURN TRUE END;
  RETURN FALSE;
END reg8_access;

PROCEDURE reg16_access(VAR p,rg: INTEGER): BOOLEAN;
  VAR s: string; i: INTEGER;
BEGIN
  i:=p;
  IF NOT ident(i,s) THEN RETURN FALSE END;
  IF s='ax' THEN rg:=0; p:=i; RETURN TRUE END;
  IF s='cx' THEN rg:=1; p:=i; RETURN TRUE END;
  IF s='dx' THEN rg:=2; p:=i; RETURN TRUE END;
  IF s='bx' THEN rg:=3; p:=i; RETURN TRUE END;
  IF s='sp' THEN rg:=4; p:=i; RETURN TRUE END;
  IF s='bp' THEN rg:=5; p:=i; RETURN TRUE END;
  IF s='si' THEN rg:=6; p:=i; RETURN TRUE END;
  IF s='di' THEN rg:=7; p:=i; RETURN TRUE END;
  RETURN FALSE;
END reg16_access;

PROCEDURE reg_access(VAR p,sz,rg: INTEGER): BOOLEAN;
BEGIN
  IF (sz IN {0,1}) & reg8_access(p,rg) THEN sz:=1; RETURN TRUE END;
  IF (sz IN {0,2}) & reg16_access(p,rg) THEN sz:=2; RETURN TRUE END;
  RETURN FALSE;
END reg_access;

PROCEDURE segreg_access(VAR p,rg: INTEGER): BOOLEAN;
  VAR s: string; i: INTEGER;
BEGIN
  i:=p;
  IF NOT ident(i,s) THEN RETURN FALSE END;
  IF s='es' THEN rg:=0; p:=i; RETURN TRUE END;
  IF s='cs' THEN rg:=1; p:=i; RETURN TRUE END;
  IF s='ss' THEN rg:=2; p:=i; RETURN TRUE END;
  IF s='ds' THEN rg:=3; p:=i; RETURN TRUE END;
  RETURN FALSE;
END segreg_access;

PROCEDURE mem_access(VAR p,sz,am,ofs: INTEGER): BOOLEAN;
  VAR i,n: INTEGER; bx,bp,si,di: BOOLEAN;
BEGIN
  i:=p;
  WHILE s_buf[i]=' ' DO INC(i) END;
  IF NOT adr_expression(i,sz,ofs) THEN RETURN FALSE END;
  IF s_buf[i]#'[' THEN p:=i; am:=8; RETURN TRUE END;
  INC(i); bx:=FALSE; bp:=FALSE; si:=FALSE; di:=FALSE;
  LOOP
    IF (s_buf[i]='b') & (s_buf[i+1]='x') THEN
      IF bx OR bp THEN RETURN FALSE END;
      bx:=TRUE; INC(i,2);
    ELSIF (s_buf[i]='b') & (s_buf[i+1]='p') THEN
      IF bx OR bp THEN RETURN FALSE END;
      bp:=TRUE; INC(i,2);
    ELSIF (s_buf[i]='s') & (s_buf[i+1]='i') THEN
      IF si OR di THEN RETURN FALSE END;
      si:=TRUE; INC(i,2);
    ELSIF (s_buf[i]='d') & (s_buf[i+1]='i') THEN
      IF si OR di THEN RETURN FALSE END;
      di:=TRUE; INC(i,2);
    ELSE
      IF NOT expression(i,n) THEN RETURN FALSE END;
      ofs:=ofs+n; EXIT;
    END;
    IF s_buf[i]#'+' THEN
      IF NOT offset_expression(i,ofs) THEN RETURN FALSE END;
      EXIT;
    ELSE INC(i)
    END;
  END;
  IF s_buf[i]#']' THEN RETURN FALSE END;
  INC(i);
  IF    bx & si THEN am:=0;
  ELSIF bx & di THEN am:=1;
  ELSIF bx      THEN am:=7;
  ELSIF bp & si THEN am:=2;
  ELSIF bp & di THEN am:=3;
  ELSIF bp      THEN am:=6;
  ELSIF si      THEN am:=4
  ELSIF di      THEN am:=5
  ELSE am:=8
  END;
  ofs:=ofs MOD 10000h; p:=i; RETURN TRUE;
END mem_access;

PROCEDURE mem_reg_access(VAR p,sz,am,rg,ofs: INTEGER): BOOLEAN;
BEGIN
  IF reg_access(p,sz,rg) THEN am:=9; RETURN TRUE END;
  RETURN mem_access(p,sz,am,ofs);
END mem_reg_access;

PROCEDURE ?0(VAR ps: INTEGER; VAL nm: ARRAY OF CHAR; v: INTEGER): BOOLEAN;
BEGIN
  IF cmd=nm THEN c.b(v); RETURN TRUE END;
  RETURN FALSE;
END ?0;

PROCEDURE ?0w(VAR ps: INTEGER; VAL nm: ARRAY OF CHAR; v: INTEGER): BOOLEAN;
BEGIN
  IF cmd=nm THEN c.w(v); RETURN TRUE END;
  RETURN FALSE;
END ?0w;

PROCEDURE s_word(VAR s: ARRAY OF CHAR; VAR i: INTEGER): BOOLEAN;
  VAR j: INTEGER;
BEGIN
  WHILE s_buf[i]=' ' DO INC(i) END;
  j:=0;
  WHILE (s_buf[i]>' ') & (s_buf[i]#';') & (s_buf[i]#',') & (s_buf[i]#':') DO
    s[j]:=s_buf[i]; INC(i); INC(j);
  END;
  s[j]:=0c;
  RETURN j#0;
END s_word;

PROCEDURE comma(VAR i: INTEGER): BOOLEAN;
BEGIN
  WHILE s_buf[i]=' ' DO INC(i) END;
  IF s_buf[i]#',' THEN RETURN FALSE END;
  INC(i);
  RETURN TRUE;
END comma;

PROCEDURE end(VAR i: INTEGER): BOOLEAN;
BEGIN
  WHILE s_buf[i]=' ' DO INC(i) END;
  IF s_buf[i]=';' THEN RETURN TRUE END;
  IF s_buf[i]=0c  THEN RETURN TRUE END;
  RETURN FALSE;
END end;

PROCEDURE ?1mr(VAR ps: INTEGER; VAL nm: ARRAY OF CHAR; v: INTEGER): BOOLEAN;
  VAR
    i,j,sz : INTEGER;
    r      : BOOLEAN;
    am,ofs : INTEGER;
    rg1,rg2: INTEGER;
BEGIN
  IF cmd#nm THEN RETURN FALSE END;
  i:=ps; sz:=0;
  r:=mem_reg_access(i,sz,am,rg1,ofs) & comma(i) &
     reg_access(i,sz,rg2) & end(i) & (sz IN {1,2});
  IF NOT r THEN RETURN FALSE END;
  ps:=i;
  c.b(v+ORD(sz=2));
  IF am=8 THEN c.b(c.md_abs+rg2*8); c.w(ofs);
  ELSIF am=9 THEN c.b(c.md_reg+rg2*8+rg1);
  ELSIF (ofs<=7Fh) OR (ofs>=0FF80h) THEN c.b(c.md_b+rg2*8+am); c.b(ofs);
  ELSE c.b(c.md_w+rg2*8+am); c.w(ofs);
  END;
  RETURN TRUE;
END ?1mr;

PROCEDURE ?1msr(VAR ps: INTEGER; VAL nm: ARRAY OF CHAR; v: INTEGER): BOOLEAN;
  VAR
    i,j,sz : INTEGER;
    r      : BOOLEAN;
    am,ofs : INTEGER;
    rg1,rg2: INTEGER;
BEGIN
  IF cmd#nm THEN RETURN FALSE END;
  i:=ps; sz:=0;
  r:=mem_reg_access(i,sz,am,rg1,ofs) & comma(i) &
     segreg_access(i,rg2) & end(i) & (sz IN {0,2});
  IF NOT r THEN RETURN FALSE END;
  ps:=i;
  c.b(v);
  IF am=8 THEN c.b(c.md_abs+rg2*8); c.w(ofs);
  ELSIF am=9 THEN c.b(c.md_reg+rg2*8+rg1);
  ELSIF (ofs<=7Fh) OR (ofs>=0FF80h) THEN c.b(c.md_b+rg2*8+am); c.b(ofs);
  ELSE c.b(c.md_w+rg2*8+am); c.w(ofs);
  END;
  RETURN TRUE;
END ?1msr;

PROCEDURE ?1rm(VAR ps: INTEGER; VAL nm: ARRAY OF CHAR; v: INTEGER): BOOLEAN;
  VAR
    i,j,sz : INTEGER;
    r      : BOOLEAN;
    am,ofs : INTEGER;
    rg1,rg2: INTEGER;
BEGIN
  IF cmd#nm THEN RETURN FALSE END;
  i:=ps; sz:=0;
  r:=reg_access(i,sz,rg1) & comma(i) &
     mem_reg_access(i,sz,am,rg2,ofs) & end(i) & (sz IN {1,2});
  IF NOT r THEN RETURN FALSE END;
  ps:=i;
  c.b(v+ORD(sz=2));
  IF am=8 THEN c.b(c.md_abs+rg1*8); c.w(ofs);
  ELSIF am=9 THEN c.b(c.md_reg+rg1*8+rg2);
  ELSIF (ofs<=7Fh) OR (ofs>=0FF80h) THEN c.b(c.md_b+rg1*8+am); c.b(ofs);
  ELSE c.b(c.md_w+rg1*8+am); c.w(ofs);
  END;
  RETURN TRUE;
END ?1rm;

PROCEDURE ?1rm16(VAR ps: INTEGER; VAL nm: ARRAY OF CHAR; v: INTEGER): BOOLEAN;
  VAR
    i,j,sz : INTEGER;
    r      : BOOLEAN;
    am,ofs : INTEGER;
    rg1,rg2: INTEGER;
BEGIN
  IF cmd#nm THEN RETURN FALSE END;
  i:=ps; sz:=0;
  r:=reg_access(i,sz,rg1) & comma(i) &
     mem_reg_access(i,sz,am,rg2,ofs) & end(i) & (sz=2);
  IF NOT r THEN RETURN FALSE END;
  ps:=i;
  c.b(v);
  IF am=8 THEN c.b(c.md_abs+rg1*8); c.w(ofs);
  ELSIF am=9 THEN c.b(c.md_reg+rg1*8+rg2);
  ELSIF (ofs<=7Fh) OR (ofs>=0FF80h) THEN c.b(c.md_b+rg1*8+am); c.b(ofs);
  ELSE c.b(c.md_w+rg1*8+am); c.w(ofs);
  END;
  RETURN TRUE;
END ?1rm16;

PROCEDURE ?1srm(VAR ps: INTEGER; VAL nm: ARRAY OF CHAR; v: INTEGER): BOOLEAN;
  VAR
    i,j,sz : INTEGER;
    r      : BOOLEAN;
    am,ofs : INTEGER;
    rg1,rg2: INTEGER;
BEGIN
  IF cmd#nm THEN RETURN FALSE END;
  i:=ps; sz:=0;
  r:=segreg_access(i,rg1) & comma(i) &
     mem_reg_access(i,sz,am,rg2,ofs) & end(i) & (sz IN {0,2});
  IF NOT r THEN RETURN FALSE END;
  ps:=i;
  c.b(v);
  IF am=8 THEN c.b(c.md_abs+rg1*8); c.w(ofs);
  ELSIF am=9 THEN c.b(c.md_reg+rg1*8+rg2);
  ELSIF (ofs<=7Fh) OR (ofs>=0FF80h) THEN c.b(c.md_b+rg1*8+am); c.b(ofs);
  ELSE c.b(c.md_w+rg1*8+am); c.w(ofs);
  END;
  RETURN TRUE;
END ?1srm;

PROCEDURE ?1m(VAR ps: INTEGER; VAL nm: ARRAY OF CHAR;
              v,vv: INTEGER; ww: BOOLEAN): BOOLEAN;
  VAR
    i,j,sz : INTEGER;
    r      : BOOLEAN;
    am,ofs : INTEGER;
    rg     : INTEGER;
BEGIN
  IF cmd#nm THEN RETURN FALSE END;
  i:=ps;
  IF ww THEN sz:=2 ELSE sz:=0 END;
  r:=mem_reg_access(i,sz,am,rg,ofs) & end(i) & (sz IN {1,2});
  IF NOT r THEN RETURN FALSE END;
  ps:=i;
  c.b(v+ORD(sz=2));
  IF am=8 THEN c.b(c.md_abs+vv*8); c.w(ofs);
  ELSIF am=9 THEN c.b(c.md_reg+vv*8+rg);
  ELSIF (ofs<=7Fh) OR (ofs>=0FF80h) THEN c.b(c.md_b+vv*8+am); c.b(ofs);
  ELSE c.b(c.md_w+vv*8+am); c.w(ofs);
  END;
  RETURN TRUE;
END ?1m;

PROCEDURE ?1pm(VAR ps: INTEGER; VAL nm: ARRAY OF CHAR;
              p,v,vv: INTEGER): BOOLEAN;
  VAR
    i,j,sz : INTEGER;
    r      : BOOLEAN;
    am,ofs : INTEGER;
    rg     : INTEGER;
BEGIN
  IF cmd#nm THEN RETURN FALSE END;
  i:=ps; sz:=0;
  r:=mem_reg_access(i,sz,am,rg,ofs) & end(i) & (sz IN {0,2});
  IF NOT r THEN RETURN FALSE END;
  ps:=i;
  c.b(p); c.b(v);
  IF am=8 THEN c.b(c.md_abs+vv*8); c.w(ofs);
  ELSIF am=9 THEN c.b(c.md_reg+vv*8+rg);
  ELSIF (ofs<=7Fh) OR (ofs>=0FF80h) THEN c.b(c.md_b+vv*8+am); c.b(ofs);
  ELSE c.b(c.md_w+vv*8+am); c.w(ofs);
  END;
  RETURN TRUE;
END ?1pm;

PROCEDURE ?1m_far(VAR ps: INTEGER; VAL nm: ARRAY OF CHAR;
                  v,vv: INTEGER): BOOLEAN;
  VAR i: INTEGER; s: string;
BEGIN
  i:=ps;
  IF (nm=cmd) & ident(i,s) & (s='far') & ?1m(i,nm,v,vv,TRUE) THEN
    ps:=i; RETURN TRUE;
  END;
  RETURN FALSE;
END ?1m_far;

PROCEDURE ?a_imm(VAR ps: INTEGER; VAL nm: ARRAY OF CHAR; v: INTEGER): BOOLEAN;
  VAR
    i,j,sz : INTEGER;
    r      : BOOLEAN;
    rg,imm : INTEGER;
BEGIN
  IF cmd#nm THEN RETURN FALSE END;
  i:=ps; sz:=0;
  r:=reg_access(i,sz,rg) & (rg=0) & comma(i) &
     expression(i,imm) & end(i) & (sz IN {1,2});
  IF NOT r THEN RETURN FALSE END;
  ps:=i;
  c.b(v+ORD(sz=2));
  IF sz=2 THEN c.w(imm) ELSE c.b(imm) END;
  RETURN TRUE;
END ?a_imm;

PROCEDURE ?m_imm(VAR ps: INTEGER; VAL nm: ARRAY OF CHAR;
                 v,vv: INTEGER): BOOLEAN;
  VAR
    i,j,sz : INTEGER;
    r      : BOOLEAN;
    am,ofs : INTEGER;
    rg,imm : INTEGER;
BEGIN
  IF cmd#nm THEN RETURN FALSE END;
  i:=ps; sz:=0;
  r:=mem_reg_access(i,sz,am,rg,ofs) & comma(i) &
     expression(i,imm) & end(i) & (sz IN {1,2});
  IF NOT r THEN RETURN FALSE END;
  ps:=i; imm:=imm MOD 10000h;
  IF (sz=2) & (imm<=7Fh) & (imm>=0FF80h) THEN
    c.b(v+3);
    IF am=8 THEN c.b(c.md_abs+vv*8); c.w(ofs);
    ELSIF am=9 THEN c.b(c.md_reg+vv*8+rg);
    ELSIF (ofs<=7Fh) OR (ofs>=0FF80h) THEN c.b(c.md_b+vv*8+am); c.b(ofs);
    ELSE c.b(c.md_w+vv*8+am); c.w(ofs);
    END;
    c.b(imm);
  ELSE
    c.b(v+ORD(sz=2));
    IF am=8 THEN c.b(c.md_abs+vv*8); c.w(ofs);
    ELSIF am=9 THEN c.b(c.md_reg+vv*8+rg);
    ELSIF (ofs<=7Fh) OR (ofs>=0FF80h) THEN c.b(c.md_b+vv*8+am); c.b(ofs);
    ELSE c.b(c.md_w+vv*8+am); c.w(ofs);
    END;
    IF sz=2 THEN c.w(imm) ELSE c.b(imm) END;
  END;
  RETURN TRUE;
END ?m_imm;

PROCEDURE ?l16(VAR ps: INTEGER; VAL nm: ARRAY OF CHAR; v: INTEGER): BOOLEAN;
  VAR
    lb     : string;
    r      : BOOLEAN;
    i,n    : INTEGER;
BEGIN
  IF cmd#nm THEN RETURN FALSE END;
  i:=ps;
  r:=ident(i,lb) & end(i);
  IF NOT r THEN RETURN FALSE END;
  ps:=i;
  IF NOT gen THEN c.b(v); c.w(0); RETURN TRUE END;
  n:=find(lb);
  IF n<0 THEN error('label "%s" not found',lb); n:=0;
  ELSE n:=tbl[n].val;
  END;
  n:=n-c.cnt-3; c.b(v); c.w(n); RETURN TRUE;
END ?l16;

PROCEDURE ?l16far(VAR ps: INTEGER; VAL nm: ARRAY OF CHAR; v: INTEGER): BOOLEAN;
  PROCEDURE colon(VAR i: INTEGER): BOOLEAN;
  BEGIN
    WHILE s_buf[i]=' ' DO INC(i) END;
    IF s_buf[i]=':' THEN INC(i); RETURN TRUE ELSE RETURN FALSE END;
  END colon;
  VAR
    lb     : string;
    r      : BOOLEAN;
    i,hi,lo: INTEGER;
BEGIN
  IF cmd#nm THEN RETURN FALSE END;
  i:=ps;
  r:=ident(i,lb) & (lb='far') &
     expression(i,hi) & colon(i) & expression(i,lo) & end(i);
  IF NOT r THEN RETURN FALSE END;
  ps:=i; c.b(v); c.w(lo); c.w(hi); RETURN TRUE;
END ?l16far;

PROCEDURE ?l8(VAR ps: INTEGER; VAL nm: ARRAY OF CHAR; v: INTEGER): BOOLEAN;
  VAR
    lb     : string;
    r      : BOOLEAN;
    i,n    : INTEGER;
BEGIN
  IF cmd#nm THEN RETURN FALSE END;
  i:=ps;
  r:=ident(i,lb) & end(i);
  IF NOT r THEN RETURN FALSE END;
  ps:=i;
  IF NOT gen THEN c.b(v); c.b(0); RETURN TRUE END;
  n:=find(lb);
  IF n<0 THEN error('label "%s" not found',lb); n:=0;
  ELSE n:=tbl[n].val;
  END;
  n:=(n-c.cnt-2) MOD 10000h;
  IF (n>7Fh) & (n<0FF80h) THEN RETURN FALSE END;
  c.b(v); c.b(n);
  RETURN TRUE;
END ?l8;

PROCEDURE ?0r16(VAR ps: INTEGER; VAL nm: ARRAY OF CHAR; v: INTEGER): BOOLEAN;
  VAR
    r   : BOOLEAN;
    rg,i: INTEGER;
BEGIN
  IF cmd#nm THEN RETURN FALSE END;
  i:=ps;
  r:=reg16_access(i,rg) & end(i);
  IF NOT r THEN RETURN FALSE END;
  ps:=i; c.b(v+rg); RETURN TRUE;
END ?0r16;

PROCEDURE ?0ax_r16(VAR ps: INTEGER; VAL nm: ARRAY OF CHAR; v: INTEGER): BOOLEAN;
  VAR
    r        : BOOLEAN;
    rg1,rg2,i: INTEGER;
BEGIN
  IF cmd#nm THEN RETURN FALSE END;
  i:=ps;
  r:=reg16_access(i,rg1) & comma(i) & reg16_access(i,rg2) & end(i) &
     ((rg1=0) OR (rg2=0));
  IF NOT r THEN RETURN FALSE END;
  IF rg1=0 THEN rg1:=rg2 END;
  ps:=i; c.b(v+rg1); RETURN TRUE;
END ?0ax_r16;

PROCEDURE ?0sr16(VAR ps: INTEGER; VAL nm: ARRAY OF CHAR; v: INTEGER): BOOLEAN;
  VAR
    r      : BOOLEAN;
    rg,i   : INTEGER;
BEGIN
  IF cmd#nm THEN RETURN FALSE END;
  i:=ps;
  r:=segreg_access(i,rg) & end(i);
  IF NOT r THEN RETURN FALSE END;
  ps:=i; c.b(v+rg*8); RETURN TRUE;
END ?0sr16;

PROCEDURE ?shift(VAR ps: INTEGER; VAL nm: ARRAY OF CHAR; v: INTEGER): BOOLEAN;
  VAR
    r       : BOOLEAN;
    am,ofs  : INTEGER;
    rg,i,num: INTEGER;
    sz,j    : INTEGER;
BEGIN
  IF cmd#nm THEN RETURN FALSE END;
  i:=ps; sz:=0;
  r:=mem_reg_access(i,sz,am,rg,ofs) & comma(i) & (sz IN {1,2});
  IF NOT r THEN RETURN FALSE END;
  j:=i;
  IF expression(j,num) & (num=1) & end(j) THEN i:=j; c.b(0D0h+ORD(sz=2));
  ELSIF reg8_access(i,num) & (num=1) & end(i) THEN c.b(0D2h+ORD(sz=2));
  ELSE RETURN FALSE
  END;
  ps:=i;
  IF am=8 THEN c.b(c.md_abs+v*8); c.w(ofs);
  ELSIF am=9 THEN c.b(c.md_reg+v*8+rg);
  ELSIF (ofs<=7Fh) OR (ofs>=0FF80h) THEN c.b(c.md_b+v*8+am); c.b(ofs);
  ELSE c.b(c.md_w+v*8+am); c.w(ofs);
  END;
  RETURN TRUE;
END ?shift;

PROCEDURE ?return(VAR ps: INTEGER): BOOLEAN;
  VAR
    s     : string;
    i,num : INTEGER;
BEGIN
  IF cmd#'ret' THEN RETURN FALSE END;
  i:=ps;
  IF ident(i,s) & (s='far') THEN
    IF end(i) THEN
      ps:=i; c.b(0CBh); RETURN TRUE;
    ELSIF expression(i,num) & end(i) THEN
      ps:=i; c.b(0CAh); c.w(num); RETURN TRUE;
    ELSE RETURN FALSE
    END;
  END;
  i:=ps;
  IF end(i) THEN ps:=i; c.b(0C3h); RETURN TRUE END;
  i:=ps;
  IF expression(i,num) & end(i) THEN
    ps:=i; c.b(0C2h); c.w(num); RETURN TRUE;
  END;
  RETURN FALSE
END ?return;

PROCEDURE ?ib(VAR ps: INTEGER; VAL nm: ARRAY OF CHAR; v: INTEGER): BOOLEAN;
  VAR num,i: INTEGER;
BEGIN
  i:=ps;
  IF (cmd=nm) & expression(i,num) & end(i) & (num>=0) & (num<=255) THEN
    ps:=i; c.b(v); c.b(num); RETURN TRUE;
  ELSE RETURN FALSE
  END;
END ?ib;

PROCEDURE ?iw(VAR ps: INTEGER; VAL nm: ARRAY OF CHAR; v: INTEGER): BOOLEAN;
  VAR num,i: INTEGER;
BEGIN
  i:=ps;
  IF (cmd=nm) & expression(i,num) & end(i) THEN
    ps:=i; c.b(v); c.w(num); RETURN TRUE;
  ELSE RETURN FALSE
  END;
END ?iw;

PROCEDURE ?inp(VAR ps: INTEGER): BOOLEAN;
  VAR num,i: INTEGER; s: string;
BEGIN
  IF cmd#'inp' THEN RETURN FALSE END;
  i:=ps;
  IF ident(i,s) & (s='ax') & comma(i) &
     expression(i,num) & end(i) & (num>=0) & (num<100h) THEN
    ps:=i; c.b(0E5h); c.b(num); RETURN TRUE;
  END;
  i:=ps;
  IF ident(i,s) & (s='al') & comma(i) &
        expression(i,num) & end(i) & (num>=0) & (num<100h) THEN
    ps:=i; c.b(0E4h); c.b(num); RETURN TRUE;
  END;
  i:=ps;
  IF ident(i,s) & (s='ax') & comma(i) &
        ident(i,s) & (s='dx') & end(i) THEN
    ps:=i; c.b(0EDh); RETURN TRUE;
  END;
  i:=ps;
  IF ident(i,s) & (s='al') & comma(i) &
        ident(i,s) & (s='dx') & end(i) THEN
    ps:=i; c.b(0ECh); RETURN TRUE;
  END;
   RETURN FALSE
END ?inp;

PROCEDURE ?out(VAR ps: INTEGER): BOOLEAN;
  VAR num,i: INTEGER; s: string;
BEGIN
  IF cmd#'out' THEN RETURN FALSE END;
  i:=ps;
  IF ident(i,s) & (s='ax') & comma(i) &
     expression(i,num) & end(i) & (num>=0) & (num<100h) THEN
    ps:=i; c.b(0E7h); c.b(num); RETURN TRUE;
  END;
  i:=ps;
  IF ident(i,s) & (s='al') & comma(i) &
        expression(i,num) & end(i) & (num>=0) & (num<100h) THEN
    ps:=i; c.b(0E6h); c.b(num); RETURN TRUE;
  END;
  i:=ps;
  IF ident(i,s) & (s='ax') & comma(i) &
        ident(i,s) & (s='dx') & end(i) THEN
    ps:=i; c.b(0EFh); RETURN TRUE;
  END;
  i:=ps;
  IF ident(i,s) & (s='al') & comma(i) &
        ident(i,s) & (s='dx') & end(i) THEN
    ps:=i; c.b(0EEh); RETURN TRUE;
  END;
  RETURN FALSE
END ?out;

PROCEDURE ?db(VAR ps: INTEGER): BOOLEAN;
  VAR n: INTEGER;
BEGIN
  IF cmd#'db' THEN RETURN FALSE END;
  LOOP
    IF NOT expression(ps,n) THEN error('bad expression'); n:=0 END;
    c.b(n);
    IF NOT comma(ps) THEN EXIT END;
  END;
  IF NOT end(ps) THEN error('bad expression') END;
  RETURN TRUE;
END ?db;

PROCEDURE command;
  PROCEDURE ill;
  BEGIN error('illegal instruction');
  END ill;
  VAR i,n: INTEGER; r: BOOLEAN;
BEGIN
  i:=0;
  LOOP
    IF NOT s_word(cmd,i) THEN RETURN END;
    WHILE s_buf[i]=' ' DO INC(i) END;
    IF s_buf[i]#':' THEN EXIT END;
    INC(i);
    n:=find(cmd);
    IF gen THEN
      IF tbl[n].val#c.cnt THEN INC(fail); tbl[n].val:=c.cnt END
    ELSE
      IF n>=0 THEN ill; RETURN END;
      n:=def();
      tbl[n].name:=cmd;
      tbl[n].val :=c.cnt;
    END;
  END;
  CASE cmd[0] OF
    |'a': r:=?0(i,'aaa',37h) OR ?0w(i,'aad',0AD5h) OR ?0w(i,'aam',0AD4h) OR
             ?0(i,'aas',3Fh) OR ?1mr(i,'adc',10h) OR ?1rm(i,'adc',12h) OR
             ?a_imm(i,'adc',14h) OR ?m_imm(i,'adc',80h,2) OR
             ?1mr(i,'add',00h) OR ?1rm(i,'add',02h) OR
             ?a_imm(i,'add',04h) OR ?m_imm(i,'add',80h,0) OR
             ?1mr(i,'and',20h) OR ?1rm(i,'and',22h) OR
             ?a_imm(i,'and',24h) OR ?m_imm(i,'and',80h,4);
    |'c': r:=?l16(i,'call',0E8h) OR ?1m(i,'call',0FEh,2,TRUE) OR
             ?1m_far(i,'call',0FEh,3) OR
             ?0(i,'cbw',098h) OR ?0(i,'clc',0F8h) OR ?0(i,'cld',0FCh) OR
             ?0(i,'cli',0FAh) OR ?0(i,'cmc',0F5h) OR
             ?1mr(i,'cmp',38h) OR ?1rm(i,'cmp',3Ah) OR
             ?a_imm(i,'cmp',3Ch) OR ?m_imm(i,'cmp',80h,7) OR
             ?0(i,'cwd',099h) OR ?0(i,'cmps',0A6h) OR ?0(i,'cmpsw',0A7h) OR
             ?l16far(i,'call',9Ah);
    |'d': r:=?0(i,'daa',027h) OR ?0(i,'das',02Fh) OR
             ?0r16(i,'dec',48h) OR ?1m(i,'dec',0FEh,1,FALSE) OR
             ?1m(i,'div',0F6h,6,FALSE) OR ?db(i);
    |'h': r:=?0(i,'halt',0F4h);
    |'i': r:=?1m(i,'idiv',0F6h,7,FALSE) OR ?1m(i,'imul',0F6h,5,FALSE) OR
             ?0r16(i,'inc',40h) OR ?1m(i,'inc',0FEh,0,FALSE) OR
             ?0(i,'iret',0CFh) OR ?ib(i,'int',0CDh) OR
             ?0(i,'into',0CEh) OR ?inp(i);
    |'j': r:=?l8(i,'ja',  077h) OR ?l8(i,'jae', 073h) OR ?l8(i,'jb',  072h) OR
             ?l8(i,'jbe', 076h) OR ?l8(i,'jc',  072h) OR ?l8(i,'je',  074h) OR
             ?l8(i,'jg',  07Fh) OR ?l8(i,'jge', 07Dh) OR ?l8(i,'jl',  07Ch) OR
             ?l8(i,'jle', 07Eh) OR ?l8(i,'jna', 076h) OR ?l8(i,'jnae',072h) OR
             ?l8(i,'jnb' ,073h) OR ?l8(i,'jnbe',077h) OR ?l8(i,'jnc' ,073h) OR
             ?l8(i,'jne' ,075h) OR ?l8(i,'jng' ,07Eh) OR ?l8(i,'jnge',07Ch);
     r:=r OR ?l8(i,'jnl' ,07Dh) OR ?l8(i,'jnle',07Fh) OR ?l8(i,'jno' ,071h) OR
             ?l8(i,'jnp' ,07Bh) OR ?l8(i,'jns' ,079h) OR ?l8(i,'jnz' ,075h) OR
             ?l8(i,'jo'  ,070h) OR ?l8(i,'jp'  ,07Ah) OR ?l8(i,'jpe' ,07Ah) OR
             ?l8(i,'jpo' ,07Bh) OR ?l8(i,'js'  ,078h) OR ?l8(i,'jz'  ,074h) OR
             ?l8(i,'jcxz',0E3h) OR ?l8(i,'jmp' ,0EBh) OR ?l16(i,'jmp',0E9h) OR
             ?l16far(i,'jmp',0EAh) OR
             ?1m(i,'jmp',0FEh,4,TRUE) OR ?1m_far(i,'jmp',0FEh,5);
    |'l': r:=?0(i,'lahf',09Fh) OR ?1rm16(i,'lds',0C5h) OR
             ?1rm16(i,'les',0C4h) OR
             ?1rm16(i,'lea',08Dh) OR ?0(i,'lock',0F0h) OR
             ?l8(i,'loope',0E1h) OR ?l8(i,'loopne',0E0h) OR
             ?l8(i,'loopz',0E1h) OR ?l8(i,'loopnz',0E0h) OR
             ?l8(i,'loop',0E2h) OR ?0(i,'lods',0ACh) OR ?0(i,'lodsw',0ADh) OR
             ?1pm(i,'lmsw',0Fh,1,6) OR ?1pm(i,'lldt',0Fh,0,2) OR
             ?1pm(i,'lgdt',0Fh,1,2) OR ?1pm(i,'lidt',0Fh,1,3);
    |'m': r:=?1mr(i,'mov',088h) OR ?1rm(i,'mov',08Ah) OR
             ?m_imm(i,'mov',0C6h,0) OR
             ?1msr(i,'mov',08Ch) OR ?1srm(i,'mov',08Eh) OR
             ?1m(i,'mul',0F6h,4,FALSE) OR ?0(i,'movs',0A4h) OR
             ?0(i,'movsw',0A5h);
    |'n': r:=?1m(i,'neg',0F6h,3,FALSE) OR ?0(i,'nop',90h) OR
             ?1m(i,'not',0F6h,2,FALSE);
    |'o': r:=?1mr(i,'or',08h) OR ?1rm(i,'or',0Ah) OR
             ?a_imm(i,'or',0Ch) OR ?m_imm(i,'or',80h,1) OR ?out(i);
    |'p': r:=?0sr16(i,'pop',07h) OR ?0sr16(i,'push',06h) OR
             ?0r16(i,'pop',58h) OR ?1m(i,'pop',08Eh,0,TRUE) OR
             ?0r16(i,'push',50h) OR ?1m(i,'push',0FEh,6,TRUE) OR
             ?0(i,'popf',09Dh) OR ?0(i,'pushf',09Ch) OR ?iw(i,'push',68h);
    |'r': r:=?shift(i,'rcl',2) OR ?shift(i,'rcr',3) OR
             ?shift(i,'rol',0) OR ?shift(i,'ror',1) OR
             ?0(i,'rep',0F3h) OR ?0(i,'repe',0F3h) OR
             ?0(i,'repz',0F3h) OR ?0(i,'repne',0F2h) OR
             ?0(i,'repnz',0F2h) OR ?return(i);
    |'s': r:=?0(i,'sahf',09Eh) OR ?0sr16(i,'seg',46b) OR
             ?shift(i,'sal',4) OR ?shift(i,'shl',4) OR
             ?shift(i,'sar',7) OR ?shift(i,'shr',5) OR
             ?1mr(i,'sbb',18h) OR ?1rm(i,'sbb',1Ah) OR
             ?a_imm(i,'sbb',1Ch) OR ?m_imm(i,'sbb',80h,3) OR
             ?0(i,'stc',0F9h) OR ?0(i,'std',0FDh) OR ?0(i,'sti',0FBh) OR
             ?1mr(i,'sub',28h) OR ?1rm(i,'sub',2Ah);
     r:=r OR ?a_imm(i,'sub',2Ch) OR ?m_imm(i,'sub',80h,5) OR
             ?0(i,'scas',0AEh) OR ?0(i,'scasw',0AFh) OR
             ?0(i,'stos',0AAh) OR ?0(i,'stosw',0ABh) OR
             ?1pm(i,'smsw',0Fh,1,4) OR ?1pm(i,'sgdt',0Fh,1,0) OR
             ?1pm(i,'sidt',0Fh,1,1) OR ?1pm(i,'sldt',0Fh,0,0);
    |'t': r:=?1mr(i,'test',84h) OR ?1rm(i,'test',84h) OR
             ?a_imm(i,'test',0A8h) OR ?m_imm(i,'test',0F6h,0);
    |'w': r:=?0(i,'wait',09Bh);
    |'x': r:=?0ax_r16(i,'xchg',90h) OR ?0(i,'xlat',0D7h) OR
             ?0(i,'xlatb',0D7h) OR
             ?1rm(i,'xchg',86h) OR ?1mr(i,'xchg',86h) OR
             ?1mr(i,'xor',30h) OR ?1rm(i,'xor',32h) OR
             ?a_imm(i,'xor',34h) OR ?m_imm(i,'xor',80h,6);
  ELSE r:=FALSE;
  END;
  IF NOT r THEN ill END;
END command;

PROCEDURE gen_code(VAL s: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  src^.ADR:=ADR(s);
  src^.HIGH:=HIGH(s);
  s_pos:=0; tbl_sz:=0; NEW(tbl); i:=c.cnt; gen:=FALSE;
  WHILE (s_pos<=HIGH(src)) & (src[s_pos]#0c) DO
    get_line; command;
  END;
  REPEAT
    s_pos:=0; c.cnt:=i; fail:=0; gen:=TRUE;
    WHILE (s_pos<=HIGH(src)) & (src[s_pos]#0c) DO
      get_line; command;
    END;
  UNTIL fail=0;
  DISPOSE(tbl);
END gen_code;

END inAsm.
