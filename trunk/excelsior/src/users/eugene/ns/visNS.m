IMPLEMENTATION MODULE visNS; (* Ned 04-Jan-89. (c) KRONOS *)

(* NS32032 re-assembler *)

IMPORT ou: StdIO;

VAR get_byte: GET_BYTE;

----------------------------  CODE  ---------------------------
                            --------

VAR cp0 : INTEGER; -- pos of instruction first byte
  gen_no: INTEGER;
  scales: BITSET;
  index: ARRAY [0..1] OF INTEGER;
  case_sz: INTEGER;
  case_no: INTEGER;
  case_pc: INTEGER;

PROCEDURE next(): INTEGER;
BEGIN INC(cp); RETURN get_byte();
END next;

PROCEDURE disp(): INTEGER;
  VAR n: INTEGER;
BEGIN n:=next();
  IF n<128 THEN
    IF n<64 THEN RETURN n ELSE RETURN n-128 END;
  ELSIF n DIV 64=2 THEN n:=(n MOD 64)*100h+next();
    IF n<2000h THEN RETURN n ELSE RETURN n-4000h END;
  ELSE ASSERT(n DIV 64=3);
    n:=(((n MOD 64)*100h+next())*100h+next())*100h+next();
    IF n<2000000h THEN RETURN n ELSE RETURN n-40000000h END;
  END;
END disp;

PROCEDURE vis_disp;
  VAR n: INTEGER;
BEGIN n:=next();
  IF n<128 THEN
    IF n<64 THEN ou.print('%3$#h',n) ELSE ou.print('-%3$#h',80h-n) END;
  ELSIF n DIV 64=2 THEN n:=(n MOD 64)*100h+next();
    IF n<2000h THEN ou.print('%5$#h',n)
    ELSE ou.print('-%5$#h',4000h-n)
    END;
  ELSE ASSERT(n DIV 64=3);
    n:=(((n MOD 64)*100h+next())*100h+next())*100h+next();
    IF n<2000000h THEN ou.print('%9$#h',n)
    ELSE ou.print('-%9$#h',40000000h-n)
    END;
  END;
END vis_disp;

PROCEDURE set_scale(gen1,gen2: INTEGER);
BEGIN
  IF gen1-1Ch IN {0..3} THEN index[0]:=next(); INCL(scales,0); END;
  IF gen2-1Ch IN {0..3} THEN index[1]:=next(); INCL(scales,1); END;
END set_scale;

----------------------  ADDRESSING MODEs  ---------------------
                      --------------------

TYPE access_class = (_read,_write,_rmw,_addr,_regaddr,
                     _fread,_fwrite,_frmw,_fregaddr);

PROCEDURE vis_mode(x: INTEGER; class: access_class);

  PROCEDURE mem_rel(VAL s: ARRAY OF CHAR);
    VAR d1,d2: INTEGER;
  BEGIN d1:=disp(); d2:=disp();
    ou.print('%d(%d(%s))',d2,d1,s);
  END mem_rel;

BEGIN
  CASE x OF
    | 0.. 7: IF    class=_addr     THEN ou.print('(R%d)',x);
             ELSIF class=_fread    THEN ou.print('F%d',x);
             ELSIF class=_fwrite   THEN ou.print('F%d',x);
             ELSIF class=_frmw     THEN ou.print('F%d',x);
             ELSIF class=_fregaddr THEN ou.print('F%d',x);
             ELSE                       ou.print('R%d',x);
             END;
    | 8..0Fh: vis_disp; ou.print('(R%d)',x MOD 8);
    | 10h  :  mem_rel('FP');
    | 11h  :  mem_rel('SP');
    | 12h  :  mem_rel('SB');
    | 14h  : ou.print('res_mode_14')
    | 15h  : ou.print('@'); ou.print('%#h',disp());
    | 16h  : ou.print('EXT('); vis_disp; ou.print(')+'); vis_disp;
    | 17h  : IF    class=_read  THEN ou.print('pop');
             ELSIF class=_write THEN ou.print('push');
             ELSE                    ou.print('tos');
             END;
    | 18h  : vis_disp; ou.print('(FP)');
    | 19h  : vis_disp; ou.print('(SP)');
    | 1Ah  : vis_disp; ou.print('(SB)');
    | 1Bh  : x:=disp(); ou.print('*%+d   [%#h]',x,x+cp0);
  ELSE ou.print('illegal_mode');
  END;
END vis_mode;

PROCEDURE scaled_index(x,n: INTEGER);
  CONST scale = 'BWDQ';
BEGIN
  vis_mode(n DIV 8,_addr);
  ou.print('[R%d:%c]',n MOD 8,scale[x-1Ch]);
END scaled_index;

PROCEDURE gen(x: INTEGER; class: access_class);
BEGIN
  IF gen_no IN scales THEN scaled_index(x,index[gen_no])
  ELSE vis_mode(x,class);
  END;
  INC(gen_no);
  ou.print('  ');
END gen;

PROCEDURE gen_read(x,i: INTEGER);

  PROCEDURE imm4(): INTEGER;
    VAR w: BITSET;
  BEGIN w:=BITSET(next());
    w:=BITSET(w<<8); w:=w+BITSET(next());
    w:=BITSET(w<<8); w:=w+BITSET(next());
    w:=BITSET(w<<8); w:=w+BITSET(next());
    RETURN INTEGER(w);
  END imm4;

  VAR n: INTEGER;
BEGIN
  IF x#14h THEN gen(x,_read); RETURN END;
  -- immediate: most significant byte first!
  CASE i OF
    |0: n:=next();
        ou.print('$0%$2hh ',n);
    |1: n:=next()*100h+next();
        ou.print('$0%$4hh ',n);
    |3: n:=imm4();
        ou.print('$0%$8hh ',n);
  ELSE ASSERT(FALSE);
  END;
  INC(gen_no);
END gen_read;

PROCEDURE gen_fread(x: INTEGER; f: BOOLEAN);

  PROCEDURE imm4(): INTEGER;
    VAR w: BITSET;
  BEGIN w:=BITSET(next());
    w:=BITSET(w<<8); w:=w+BITSET(next());
    w:=BITSET(w<<8); w:=w+BITSET(next());
    w:=BITSET(w<<8); w:=w+BITSET(next());
    RETURN INTEGER(w);
  END imm4;

  VAR n: INTEGER;
BEGIN
  IF x#14h THEN gen(x,_fread); RETURN END;
  -- immediate: most significant byte first!
  CASE f OF
    |TRUE : n:=imm4(); ou.print('$%$8h ',n);
    |FALSE: n:=imm4(); ou.print('$%$8h',n);
            n:=imm4(); ou.print('$%$8h ',n);
  END;
  INC(gen_no);
END gen_fread;

------------------------  MNEMONICs  -------------------------
                        -------------

PROCEDURE mnem(VAL s: ARRAY OF CHAR);
BEGIN ou.print('%-8.8s ',s);
END mnem;

PROCEDURE int_mnem(VAL s: ARRAY OF CHAR; i: INTEGER);
  VAR m: ARRAY [0..15] OF CHAR; n: INTEGER;
BEGIN n:=0;
  WHILE s[n]#0c DO m[n]:=s[n]; INC(n) END;
  CASE i OF
    |0: m[n]:='b'
    |1: m[n]:='w'
    |3: m[n]:='d'
  ELSE ASSERT(FALSE);
  END;
  m[n+1]:=0c;
  mnem(m);
END int_mnem;

PROCEDURE float_mnem(VAL s: ARRAY OF CHAR; f: BOOLEAN);
  VAR m: ARRAY [0..15] OF CHAR; n: INTEGER;
BEGIN n:=0;
  WHILE s[n]#0c DO m[n]:=s[n]; INC(n) END;
  IF f THEN m[n]:='f' ELSE m[n]:='l' END;
  m[n+1]:=0c;
  mnem(m);
END float_mnem;

PROCEDURE mnem_cond(c: CHAR; cond,i: INTEGER);
  CONST conds = 'eqnecscchilsgtlefsfclohsltger ??';
  VAR s: ARRAY [0..15] OF CHAR;
BEGIN
  s[0]:=c;
  s[1]:=conds[cond*2+0];
  s[2]:=conds[cond*2+1];
  s[3]:=0c;
  IF i IN {0,1,3} THEN int_mnem(s,i)
  ELSE mnem(s)
  END;
END mnem_cond;

--------------------------  COMMANDS  -------------------------
                          ------------

PROCEDURE vis_branch;
  VAR x: INTEGER;
BEGIN x:=disp();
  ou.print('*%+d   [%4#$h]',x,x+cp0);
END vis_branch;

PROCEDURE branch(cond: INTEGER);
BEGIN mnem_cond('b',cond,-1); vis_branch;
END branch;

PROCEDURE exec_2(n: INTEGER);

  PROCEDURE inverse(x: BITSET): BITSET;
    VAR i: INTEGER; w: BITSET;
  BEGIN w:={};
    FOR i:=0 TO 7 DO
      IF i IN x THEN INCL(w,7-i) END;
    END; RETURN w
  END inverse;

BEGIN
  CASE n OF
    |  0: mnem('bsr');  vis_branch;
    |  1: mnem('ret');  vis_disp;
    |  2: mnem('cxp');  vis_disp;
    |  3: mnem('rxp');  vis_disp;
    |  4: mnem('rett'); vis_disp;
    |  5: mnem('reti');
    |  6: mnem('save');    ou.print('%{}',next());
    |  7: mnem('restore'); ou.print('%{}',inverse(BITSET(next())));
    |  8: mnem('enter');   ou.print('%{}  ',next()); vis_disp;
    |  9: mnem('exit');    ou.print('%{}',inverse(BITSET(next())));
    |0Ah: mnem('nop');
    |0Bh: mnem('wait');
    |0Ch: mnem('dia');
    |0Dh: mnem('flag');
    |0Eh: mnem('svc');
    |0Fh: mnem('bpt');
  ELSE ASSERT(FALSE);
  END;
END exec_2;

PROCEDURE float_cmd_BE;
  VAR n,c,src,dest: INTEGER; f: BOOLEAN;
BEGIN
  n:=next(); f:=ODD(n);
  c:=(n DIV 2) MOD 32;
  dest:=n DIV 64;
  n:=next(); src:=n DIV 8;
  dest:=dest+(n MOD 8)*4;
  set_scale(src,dest);
  CASE c OF
    |  0: float_mnem('add',f); gen_fread(src,f); gen(dest,_frmw);
    |  2: float_mnem('mov',f); gen_fread(src,f); gen(dest,_fwrite);
    |  4: float_mnem('cmp',f); gen_fread(src,f); gen_fread(dest,f);
    |  8: float_mnem('sub',f); gen_fread(src,f); gen(dest,_frmw);
    |0Ah: float_mnem('neg',f); gen_fread(src,f); gen(dest,_fwrite);
    |10h: float_mnem('div',f); gen_fread(src,f); gen(dest,_frmw);
    |18h: float_mnem('mul',f); gen_fread(src,f); gen(dest,_frmw);
    |1Ah: float_mnem('abs',f); gen_fread(src,f); gen(dest,_fwrite);
  ELSE ASSERT(FALSE);
  END;
END float_cmd_BE;

PROCEDURE int_cmd_2E;
  VAR n,i,reg,gen1,gen2: INTEGER; index: BOOLEAN;
BEGIN
  n:=next(); i:=n MOD 4; reg:=(n DIV 8) MOD 8; gen2:=n DIV 64;
  index:=2 IN BITSET(n);
  n:=next(); gen1:=n DIV 8; INC(gen2,(n MOD 8)*4);
  set_scale(gen1,gen2);
  IF index THEN int_mnem('index',i);
    ou.print('R%d  ',reg); gen_read(gen1,i); gen_read(gen2,i);
  ELSE int_mnem('ext',i);
    ou.print('R%d  ',reg); gen(gen1,_regaddr); gen(gen2,_write); vis_disp;
  END;
END int_cmd_2E;

PROCEDURE int_cmd_AE;
  VAR n,i,reg,gen1,gen2: INTEGER;
BEGIN
  n:=next(); i:=n MOD 4; reg:=(n DIV 8) MOD 8; gen2:=n DIV 64;
  IF 2 IN BITSET(n) THEN ASSERT(FALSE) END;
  n:=next(); gen1:=n DIV 8; INC(gen2,(n MOD 8)*4);
  set_scale(gen1,gen2);
  int_mnem('ins',i);
  ou.print('R%d  ',reg); gen_read(gen1,i); gen(gen2,_regaddr);
  vis_disp;
END int_cmd_AE;

PROCEDURE int_cmd_EE;
  VAR n,i,reg,gen1,gen2: INTEGER;
BEGIN
  n:=next(); i:=n MOD 4; reg:=(n DIV 8) MOD 8; gen2:=n DIV 64;
  IF 2 IN BITSET(n) THEN ASSERT(FALSE) END;
  n:=next(); gen1:=n DIV 8; INC(gen2,(n MOD 8)*4);
  set_scale(gen1,gen2);
  int_mnem('check',i);
  ou.print('R%d  ',reg); gen(gen1,_addr); gen_read(gen2,i);
END int_cmd_EE;

PROCEDURE int_cmd_4E;
  VAR n,c,i,src,dest: INTEGER;
BEGIN
  n:=next(); i:=n MOD 4;
  c:=(n DIV 4) MOD 16; dest:=n DIV 64;
  n:=next();
  src:=n DIV 8; INC(dest,(n MOD 8)*4);
  set_scale(src,dest);
  CASE c OF
    |  0: int_mnem('rot',i);   gen_read(src,0); gen(dest,_rmw);
    |  1: int_mnem('ash',i);   gen_read(src,0); gen(dest,_rmw);
    |  2: int_mnem('cbit',i);  gen_read(src,i); gen(dest,_regaddr);
    |  3: int_mnem('cbiti',i); gen_read(src,i); gen(dest,_regaddr);
    |  5: int_mnem('lsh',i);   gen_read(src,0); gen(dest,_rmw);
    |  6: int_mnem('sbit',i);  gen_read(src,i); gen(dest,_regaddr);
    |  7: int_mnem('sbiti',i); gen_read(src,i); gen(dest,_regaddr);
    |  8: int_mnem('neg',i);   gen_read(src,i); gen(dest,_write);
    |  9: int_mnem('not',i);   gen_read(src,i); gen(dest,_write);
--  |0Bh: (* SUBPi  *)
    |0Ch: int_mnem('abs',i);   gen_read(src,i); gen(dest,_write);
    |0Dh: int_mnem('com',i);   gen_read(src,i); gen(dest,_write);
    |0Eh: int_mnem('ibiti',i); gen_read(src,i); gen(dest,_regaddr);
--  |0Fh: (* ADDPi  *)
  ELSE ASSERT(FALSE,100h+c);
  END;
END int_cmd_4E;

PROCEDURE int_cmd_CE;
  VAR n,c,i,src,dest: INTEGER;
BEGIN
  n:=next(); i:=n MOD 4;
  c:=(n DIV 4) MOD 16; dest:=n DIV 64;
  n:=next();
  src:=n DIV 8; INC(dest,(n MOD 8)*4);
  set_scale(src,dest);
  CASE c OF
    |  0: int_mnem('movm',i); gen(src,_addr); gen(dest,_addr); vis_disp;
    |  1: int_mnem('cmpm',i); gen(src,_addr); gen(dest,_addr); vis_disp;
    |  2: int_mnem('inss',i); gen_read(src,i); gen(dest,_regaddr);
          n:=next(); ou.print('%d:%d',n DIV 32,n MOD 32+1);
    |  3: int_mnem('exts',i); gen_read(src,i); gen(dest,_regaddr);
          n:=next(); ou.print('%d:%d',n DIV 32,n MOD 32+1);
    |  4: IF i=0 THEN
            mnem('movxbw'); gen_read(src,0);  gen(dest,_write);
          ELSE ASSERT(FALSE);
          END;
    |  5: IF i=0 THEN
            mnem('movzbw'); gen_read(src,0);  gen(dest,_write);
          ELSE ASSERT(FALSE);
          END;
    |  6: IF i=0 THEN
            mnem('movzbd'); gen_read(src,0);  gen(dest,_write);
          ELSIF i=1 THEN
            mnem('movzwd'); gen_read(src,1);  gen(dest,_write);
          ELSE ASSERT(FALSE);
          END;
    |  7: IF i=0 THEN
            mnem('movxbd'); gen_read(src,0);  gen(dest,_write);
          ELSIF i=1 THEN
            mnem('movxwd'); gen_read(src,1);  gen(dest,_write);
          ELSE ASSERT(FALSE);
          END;
    |  8: int_mnem('mul',i); gen_read(src,i); gen(dest,_rmw);
    |  9: int_mnem('mei',i); gen_read(src,i); gen(dest,_rmw);
    |0Bh: int_mnem('dei',i); gen_read(src,i); gen(dest,_rmw);
    |0Ch: int_mnem('quo',i); gen_read(src,i); gen(dest,_rmw);
    |0Dh: int_mnem('rem',i); gen_read(src,i); gen(dest,_rmw);
    |0Eh: int_mnem('mod',i); gen_read(src,i); gen(dest,_rmw);
    |0Fh: int_mnem('div',i); gen_read(src,i); gen(dest,_rmw);
  ELSE ASSERT(FALSE,100h+c);
  END;
END int_cmd_CE;

PROCEDURE int_cmd_0E;

  PROCEDURE str_opt(n: INTEGER);
  BEGIN
    IF ODD(n) THEN ou.print('B') END;
    n:=(n DIV 2) MOD 4; ASSERT(n IN {0,1,3});
    IF    n=1 THEN ou.print('W')
    ELSIF n=1 THEN ou.print('U')
    END;
  END str_opt;

  VAR n,c,i: INTEGER; x: BOOLEAN;
BEGIN
  n:=next(); i:=n MOD 4;
  c:=(n DIV 4) MOD 32; x:=(7 IN BITSET(n));
  n:=next();
  CASE c OF
    |0: IF x THEN ASSERT(i=0); mnem('movst') ELSE int_mnem('movs',i) END;
        str_opt(n);
    |1: IF x THEN ASSERT(i=0); mnem('cmpst') ELSE int_mnem('cmps',i) END;
        str_opt(n);
    |2: ASSERT(i=3); mnem('setcfg');
        IF x THEN ou.print('I') END;
        IF 0 IN BITSET(n) THEN ou.print('F') END;
        IF 1 IN BITSET(n) THEN ou.print('M') END;
        IF 2 IN BITSET(n) THEN ou.print('C') END;
    |3: IF x THEN ASSERT(i=0); mnem('skpst') ELSE int_mnem('skps',i) END;
        str_opt(n);
  ELSE ASSERT(FALSE,100h+c);
  END;
END int_cmd_0E;

PROCEDURE int_cmd_1E;

  PROCEDURE mmr(x: INTEGER);
  BEGIN
    CASE x OF
      |00h: ou.print('bpr0');
      |01h: ou.print('bpr1');
      |04h: ou.print('pf0');
      |05h: ou.print('pf1');
      |08h: ou.print('sc');
      |0Ah: ou.print('msr');
      |0Bh: ou.print('bcnt');
      |0Ch: ou.print('ptb0');
      |0Dh: ou.print('ptb1');
      |0Fh: ou.print('eia');
    ELSE ASSERT(FALSE,100h+x);
    END;
    ou.print('  ');
  END mmr;

  VAR n,c,x,gen1: INTEGER;
BEGIN
  n:=next(); c:=n MOD 128; x:=n DIV 128;
  n:=next(); gen1:=n DIV 8; x:=x+(n MOD 8)*2;
  set_scale(gen1,gen1);
  CASE c OF
    |03h: mnem('rdval'); gen(gen1,_addr);
    |07h: mnem('wrval'); gen(gen1,_addr);
    |0Bh: mnem('lmr'); mmr(x); gen_read(gen1,3);
    |0Fh: mnem('smr'); mmr(x); gen(gen1,_write);
  ELSE ASSERT(FALSE);
  END;
END int_cmd_1E;

PROCEDURE int_cmd_3E;
BEGIN
  ou.print('3E');
--ASSERT(FALSE);
END int_cmd_3E;

PROCEDURE int_cmd_6E;
BEGIN
  ou.print('6E');
--ASSERT(FALSE);
END int_cmd_6E;

PROCEDURE exec_E(n: INTEGER);
BEGIN
  CASE n OF
    |  0: int_cmd_0E;
    |  1: int_cmd_1E;
    |  2: int_cmd_2E;
    |  3: int_cmd_3E;
    |  4: int_cmd_4E;
    |  6: int_cmd_6E;
    |0Ah: int_cmd_AE;
    |0Bh: float_cmd_BE;
    |0Ch: int_cmd_CE;
    |0Eh: int_cmd_EE;
  ELSE ASSERT(FALSE,100h+n);
  END;
END exec_E;

PROCEDURE int_cmd_4(n: INTEGER);
  VAR i,src,dest,c,o1,o2,s: INTEGER;
BEGIN
  s:=n;
  i:=n MOD 4;  n:=n DIV 4;
  c:=n MOD 16; dest:=n DIV 16;
  n:=next();
  src:=n DIV 8; INC(dest,n MOD 8 * 4);
  set_scale(src,dest);
  CASE c OF
    |  0: int_mnem('add',i);  gen_read(src,i); gen(dest,_rmw);
    |  1: int_mnem('cmp',i);  gen_read(src,i); gen_read(dest,i);
    |  2: int_mnem('bic',i);  gen_read(src,i); gen(dest,_rmw);
    |  4: int_mnem('addc',i); gen_read(src,i); gen(dest,_rmw);
    |  5: int_mnem('mov',i);  gen_read(src,i); gen(dest,_write);
    |  6: int_mnem('or',i);   gen_read(src,i); gen(dest,_rmw);
    |  8: int_mnem('sub',i);  gen_read(src,i); gen(dest,_rmw);
    |  9: IF i=3 THEN
            mnem('addr'); gen(src,_addr); gen(dest,_write);
          ELSE
            ou.print('%$2h %$2h',s,n);
          END;
    |0Ah: int_mnem('and',i);  gen_read(src,i); gen(dest,_rmw);
    |0Ch: int_mnem('subc',i); gen_read(src,i); gen(dest,_rmw);
    |0Dh: int_mnem('tbit',i); gen_read(src,i); gen(dest,_regaddr);
    |0Eh: int_mnem('xor',i);  gen_read(src,i); gen(dest,_rmw);
    |0Fh: ASSERT(FALSE);
  ELSE (* 3,7,0Bh, 0Fh *) ASSERT(FALSE);
  END;
END int_cmd_4;

PROCEDURE int_cmd_5_7(c,src,i: INTEGER);
  VAR sz,len: INTEGER;
BEGIN
  ASSERT(NOT ODD(c),100h+c); c:=c DIV 2;
  CASE c OF
    |0: IF i=3 THEN mnem('cxpd'); gen(src,_addr);
        ELSE ASSERT(FALSE)
        END;
    |1: ASSERT(i IN {0,1});
        int_mnem('bicpsr',i); gen_read(src,i);
    |2: IF i=3 THEN mnem('jump'); gen(src,_addr);
        ELSE ASSERT(FALSE)
        END;
    |3: ASSERT(i IN {0,1});
        int_mnem('bispsr',i); gen_read(src,i);
    |5: int_mnem('adjsp',i);  gen_read(src,i);
    |6: IF i=3 THEN mnem('jsr'); gen(src,_addr);
        ELSE ASSERT(FALSE)
        END;
    |7: int_mnem('case',i); gen_read(src,i);
        IF (0 IN scales) & (index[0] DIV 8 = 1Bh) THEN
          case_sz:=src-1Ch;
          case_no:=next()+next()*256;
          case_pc:=cp0;
        END;
  ELSE ASSERT(FALSE,100h+c);
  END;
END int_cmd_5_7;

PROCEDURE p_reg(n: INTEGER);
BEGIN
  CASE n OF
    | 00h: ou.print('UPSR');
    | 08h: ou.print('FP');
    | 09h: ou.print('SP');
    | 0Ah: ou.print('SB');
    | 0Dh: ou.print('PSR');
    | 0Eh: ou.print('INTBASE');
    | 0Fh: ou.print('MOD');
  ELSE ASSERT(FALSE);
  END;
  ou.print('  ');
END p_reg;

PROCEDURE int_cmd_5(n: INTEGER);
  VAR i,quick,q,dest,c,s: INTEGER;
BEGIN
  s:=n;
  i:=n MOD 4;  n:=n DIV 16;
  c:=n MOD 8; quick:=n DIV 8;
  n:=next();
  dest:=n DIV 8; INC(quick,(n MOD 8) * 2);
  IF quick<8 THEN q:=quick ELSE q:=quick-16 END;
  set_scale(dest,0);
  CASE c OF
    |  0: int_mnem('addq',i); ou.print('%d  ',q); gen(dest,_write);
    |  1: int_mnem('cmpq',i); ou.print('%d  ',q); gen(dest,_write);
    |  2: int_mnem('spr',i);  p_reg(quick);       gen(dest,_write);
    |  3: mnem_cond('s',quick,i);                 gen(dest,_write);
    |  4: int_mnem('acb',i);  ou.print('%d  ',q); gen(dest,_rmw); vis_branch;
    |  5: int_mnem('movq',i); ou.print('%d  ',q); gen(dest,_write);
    |  6: int_mnem('lpr',i);  p_reg(quick);       gen_read(dest,i);
    |  7: IF ODD(quick) THEN
            ou.print('%$2h %$2h',s,n);
          ELSE
            int_cmd_5_7(quick,dest,i);
          END;
  END;
END int_cmd_5;

PROCEDURE vis_ins;
  VAR n: INTEGER;
BEGIN
  cp0:=cp; gen_no:=0; scales:={};
  ou.print('%$#4h: ',cp0);
  IF case_no>0 THEN
    CASE case_sz OF
      |0:
        n:=next();
        IF 7 IN BITSET(n) THEN n:=INTEGER(BITSET(n)+{7..31}) END;
      |1:
        n:=next()+next()*256;
        IF 15 IN BITSET(n) THEN n:=INTEGER(BITSET(n)+{15..31}) END;
    END;
    DEC(case_no);
    ou.print('%5d [%4hh]',n,case_pc+n);
  ELSE
    n:=next();
    CASE n MOD 16 OF
      |  2: exec_2(n DIV 16);
      |  6: ou.print('%$2h',n); --ASSERT(FALSE);
      |0Ah: branch(n DIV 16);
      |0Eh: exec_E(n DIV 16);
      |0Ch,0Dh,0Fh: int_cmd_5(n);
    ELSE            int_cmd_4(n);
    END;
  END;
END vis_ins;

PROCEDURE ini(cp0: INTEGER; get: GET_BYTE);
BEGIN cp:=cp0; get_byte:=get; case_no:=0;
END ini;

END visNS.
