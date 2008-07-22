MODULE sk86; (* Ned 02-Nov-88. (c) KRONOS *)

IMPORT  sys: SYSTEM;
IMPORT  sym: coSym;
IMPORT  env: tskEnv;
IMPORT  str: Strings;
IMPORT time: Time;
IMPORT  bio: BIO;
IMPORT   ou: Terminal;
IMPORT args: tskArgs;
IMPORT   kr: inSym;
IMPORT  mem: Heap;

WITH STORAGE: mem;

VAR sou: bio.FILE;
     fn: ARRAY [0..255] OF CHAR;
   name: ARRAY [0..255] OF CHAR;
   xpos: BOOLEAN;

PROCEDURE g(): INTEGER;
  VAR c: CHAR;
BEGIN
  bio.read(sou,sys.ADR(c),1);
  RETURN ORD(c)
END g;

PROCEDURE g2(): INTEGER; BEGIN RETURN g()+g()*100h END g2;

PROCEDURE g4(): INTEGER;
BEGIN RETURN INTEGER(BITSET(g2())+BITSET(g2()<<16)) END g4;

PROCEDURE gX(): INTEGER;
  VAR b: INTEGER;
BEGIN
  b:=g();
  IF    b>=8 THEN RETURN b-128
  ELSIF b=0  THEN RETURN g()
  ELSIF b=1  THEN RETURN -g()
  ELSIF b=2  THEN RETURN g2()
  ELSIF b=3  THEN RETURN g4()
  ELSE ASSERT(FALSE)
  END
END gX;

PROCEDURE gName(VAR s: ARRAY OF CHAR);
  VAR i: INTEGER; c: CHAR;
BEGIN i:=0; c:=CHAR(g());
  WHILE (i<HIGH(s)) & (c#0c) DO
    s[i]:=c; c:=CHAR(g()); INC(i)
  END;
  ASSERT(c=0c);
  s[i]:=c;
END gName;

----------------------------------------------------------------

VAR
  line  : ARRAY [0..255] OF CHAR;
  lpos  : INTEGER;
  impNo : INTEGER;
  TypeNo: INTEGER;

PROCEDURE initVis;
BEGIN
  TypeNo:=32; lpos:=0;
  str.image(line,lpos,'     ');
END initVis;

PROCEDURE pr(VAL format: ARRAY OF CHAR; SEQ x: sys.WORD);
BEGIN str.image(line,lpos,format,x);
END pr;

PROCEDURE nl;
BEGIN
  ou.print('%s\n',line); lpos:=0;
  str.image(line,lpos,'     ');
END nl;

PROCEDURE WriteTime(t: INTEGER);
  VAR s: ARRAY [0..63] OF CHAR;
      y,d,m,ho,mi,se: INTEGER;
BEGIN
  time.unpack(t,y,d,m,ho,mi,se);
  str.print(s,"%02d-%02d-%02d %02d:%02d.%02d"
             ,d,m,y MOD 100,ho,mi,se);
  pr('%s',s);
END WriteTime;

PROCEDURE ws (VAL s: ARRAY OF CHAR); BEGIN pr('%s',s)      END ws;
PROCEDURE tag(VAL s: ARRAY OF CHAR); BEGIN pr('%-10.8s',s) END tag;
PROCEDURE atr(VAL s: ARRAY OF CHAR); BEGIN pr('%-5.5s=',s) END atr;

PROCEDURE t(t: INTEGER);
BEGIN
  CASE t OF
    |sym.int    : ws('INT   ');
    |sym.uint   : ws('u INT ');
    |sym.bool   : ws('BOOL  ');
    |sym.char   : ws('CHAR  ');
    |sym.bitset : ws('BITS  ');
    |sym.real   : ws('REAL  ');
    |sym.addr   : ws('ADDR  ');
    |sym.word   : ws('WORD  ');
    |sym.nan    : ws('NAN   ');
  ELSE pr('%3#h   ',t);
  END;
END t;

PROCEDURE type; BEGIN atr('type'); t(gX()) END type;
PROCEDURE base; BEGIN atr('base'); t(gX()) END base;

PROCEDURE skip_to(n: INTEGER);
  VAR i: INTEGER;
BEGIN
  FOR i:=str.len(line) TO n DO str.image(line,lpos,' ') END;
END skip_to;

PROCEDURE vname;
BEGIN skip_to(54); pr("'%s'",name);
END vname;

PROCEDURE vX(VAL s: ARRAY OF CHAR);
BEGIN atr(s); pr('%3d  ',gX()); END vX;

PROCEDURE typetag(VAL s: ARRAY OF CHAR);
BEGIN lpos:=0; pr('%3$#h  ',TypeNo); tag(s); INC(TypeNo) END typetag;

PROCEDURE importno;
BEGIN lpos:=0; pr('%3$#h  ',impNo); INC(impNo) END importno;

PROCEDURE id; BEGIN gName(name) END id;

---------------------------------------------------------------

PROCEDURE vis_args;
  VAR i,n: INTEGER;
BEGIN n:=g();
  nl; pr("options [%d]: ",n); nl;
  FOR i:=1 TO n DO pr(" %08h ",gX()) END;
END vis_args;

---------------------------------------------------------------

PROCEDURE show_val(VAR a: kr.access);
BEGIN
  pr(' [ ');
  CASE a.am OF
    |kr.am_imm : pr('imm  %$8h',a.n);
    |kr.am_abs : pr('abs  %d[%d]',a.n,a.disp);
    |kr.am_G   : pr('glo  ds[%d]',a.disp);
    |kr.am_Gstr: pr('str  ds[%d]',a.disp);
    |kr.am_L   : pr('loc  ss[bp%+d], lvl %d',a.disp,a.level);
    |kr.am_Gimm: pr('glo  ds[%d], imm %d',a.disp,a.n);
    |kr.am_Limm: pr('loc  ss[bp%+d], lvl %d, imm %d',a.disp,a.level,a.n);
    |kr.am_aG  : pr('glo^ ds[%d][%d]',a.n,a.disp);
    |kr.am_aL  : pr('loc^ ss[bp%+d][%d], lvl %d',a.n,a.disp,a.level);
    |kr.am_PB2 : pr('prm2 %d, lvl %d',a.disp,a.level);
    |kr.am_PB4 : pr('prm4 %d, lvl %d',a.disp,a.level);
    |kr.am_PB8 : pr('prm8 %d, lvl %d',a.disp,a.level);
    |kr.am_aPB : pr('prm^ %d,%d, lvl %d',a.n,a.disp,a.level);
  ELSE pr('***** %d *****',a.am);
  END;
  pr(' ] ');
END show_val;

PROCEDURE skip(n: INTEGER);
  VAR c: CHAR;
BEGIN
  WHILE n>0 DO
    bio.read(sou,sys.ADR(c),1); DEC(n)
  END;
END skip;

PROCEDURE access_val;
  VAR a: kr.access; n: INTEGER;
BEGIN
  n:=gX();
  IF n#BYTES(a) THEN pr('[ *** %d *** ]',n); skip(n) END;
  bio.get(sou,a,BYTES(a));
  show_val(a);
END access_val;

PROCEDURE access_range;
  VAR a: kr.access_range; n: INTEGER;
BEGIN
  n:=gX();
  IF n#BYTES(a) THEN pr('[ *** %d *** ]',n); skip(n) END;
  bio.get(sou,a,BYTES(a)); show_val(a.l); show_val(a.r);
  pr(' [ sz=%d, sg=%d ]',a.size,a.sign);
END access_range;

PROCEDURE access_proc;
  VAR a: kr.access_proc; n: INTEGER;
BEGIN
  n:=gX();
  IF n#BYTES(a) THEN pr('[ *** %d *** ]',n); skip(n) END;
  bio.get(sou,a,BYTES(a));
  nl;
  pr('[ mod=%d ofs=%d lev=%d no=%d loc_sz=%d export=%d slink=%d ] '
    ,a.mod,a.ofs,a.lvl,a.no,a.loc_sz,a.export,a.slink);
END access_proc;

PROCEDURE access_import;
  VAR a: kr.access_import; n: INTEGER;
BEGIN
  n:=gX();
  IF n#BYTES(a) THEN pr('[ *** %d *** ]',n); skip(n) END;
  bio.get(sou,a,BYTES(a));
  pr('[ no %2d, ofs %4d]',a.no,a.ofs);
END access_import;

PROCEDURE access_cu(all: BOOLEAN);

  VAR a: kr.access_cu; n: INTEGER;
      w: DYNARR OF CHAR;
      p: INTEGER;
BEGIN
  nl;
  n:=gX();
  IF n>=BYTES(a) THEN
    bio.get(sou,a,BYTES(a));
    pr('[ prc=%d glo=%d ini=%d mg=%d tail=%d ]'
       ,a.prc_sz,a.glo_sz,a.ini_sz,a.mg_sz,n-BYTES(a));
    DEC(n,BYTES(a));
    IF all & (n>0) THEN
      NEW(w,n); bio.get(sou,w,n); p:=0;
      nl; pr('GLOBALS: '); n:=a.glo_sz;
      WHILE n>0 DO pr(' %02h',w[p]); INC(p); DEC(n); END;
      IF a.ini_sz>0 THEN
        nl; pr('CODE: '); n:=a.ini_sz;
        WHILE n>0 DO pr(' %02h',w[p]); INC(p); DEC(n); END;
      END;
      IF a.mg_sz>0 THEN
        nl; pr('MGLO: '); n:=a.mg_sz;
        WHILE n>0 DO
          pr(' %d,%d ',ORD(w[p])+ORD(w[p+1])*256,ORD(w[p+4])+ORD(w[p+5])*256);
          INC(p,8); DEC(n);
        END;
      END;
      DISPOSE(w);
    ELSE skip(n);
    END;
  ELSE
    pr('[ *** %d *** ]',n); skip(n);
  END;
END access_cu;

--------------------------- ВНЕШНИЕ ----------------------------

PROCEDURE import;
  VAR kind,time: INTEGER; pl: ARRAY [0..255] OF CHAR;
    s: ARRAY [0..15] OF CHAR;
BEGIN
  importno; id; gName(pl); time:=gX(); kind:=g();
  CASE kind OF
    |sym.def : s:='def'
    |sym.imp : s:='imp'
    |sym.prog: s:='prog'
  ELSE str.print(s,'*%d*',kind);
  END;
  pr('IMPORT %s %s; ',s,name);
  access_import;
  pr('(* %s. ',pl); WriteTime(time); pr(' *)');
END import;

PROCEDURE header;
  VAR kind,time: INTEGER; pl: ARRAY [0..255] OF CHAR;
BEGIN
  impNo:=1; lpos:=0; id; gName(pl); time:=gX(); kind:=g();
  CASE kind OF
    |0: pr('DEFINITION ');
    |1: pr('IMPLEMENTATION ');
    |2:
  ELSE  pr('%d ',kind)
  END;
  pr('MODULE %s; ',name);
  access_import;
  pr('(* %s. ',pl); WriteTime(time); pr(' *)');
END header;

PROCEDURE vKind;
  VAR i,co: INTEGER; k: BITSET;
BEGIN co:=0; k:=BITSET(gX());
  ws('{');
  FOR i:=0 TO BITS(k)-1 DO
    IF i IN k THEN
      IF co#0 THEN ws(',') END;
      CASE i OF
        |sym.varparam: ws('var');
        |sym.seqparam: ws('seq');
        |sym.readonly: ws('RO');
      ELSE pr('%d',i);
      END; INC(co);
    END;
  END;
  ws('}  ');
END vKind;

PROCEDURE scan;
  VAR byte,n: INTEGER; nl?: BOOLEAN;
BEGIN
 nl?:=TRUE;
 LOOP byte:=g();
  IF byte=sym.atrs THEN vis_args; byte:=g() END;
  IF nl? THEN nl END;
  nl?:=TRUE;
  CASE byte OF

   |sym.enumtype: typetag('enumtype'); access_range;
   |sym.range   : typetag('range'); base; access_range;
   |sym.newrange: typetag('new range'); base; access_range;
   |sym.array   : typetag('array'); pr('index=%3#h ',gX()); base; access_range;
-- |sym.i_array : typetag('array'); vX('len'); base;
   |sym.openarr : typetag('openarr'); base;
   |sym.dynarr  : typetag('dynarr');  base; vX('dim');
   |sym.pointer : typetag('pointer');
   |sym.record  : typetag('record'); base;
   |sym.set     : typetag('set'); base; access_range;
   |sym.proctype: typetag('proctype');
   |sym.functype: typetag('functype'); base;
   |sym.hidden  : typetag('hidden');
   |sym.linkage : tag('linkage'); pr('ptr=%3#h  ',gX()); base;
   |sym.parm    : id; tag('parm'); type; vKind; access_val; vname;
   |sym.field   : id; tag('field'); type;       access_val; vname;

   |sym.enum    : id; tag('enum');   type; vX('modno');  access_val;  vname;
   |sym.const   : id; tag('const');  type; vX('modno');  access_val;  vname;
   |sym.var     : id; tag('var');    type; vKind;        access_val;  vname;
   |sym.proc    : id; tag('proc');   type; vX('pno');    access_proc; vname;
   |sym.type    : id; tag('type');   type; vX('modno');            vname;
   |sym.module  : id; tag('module');       vX('modno');            vname;
   |sym.import  : import
   |sym.endproc : tag('endproc');  vX('pno');
   |sym.endmodule:tag('endmod ');  vX('modno');
   |sym.packed  :tag('packed ');
   |sym.xpos    : IF xpos THEN
                    tag('xpos');
                    pr('code=[%3$#h,%4$#h]  text=[%4d,%2d]'
                       ,gX(),gX(),g2(),g());
                  ELSE n:=gX(); n:=gX(); n:=g2(); n:=g(); nl?:=FALSE;
                  END;
   |sym.end_CU  : tag('end_CU'); access_cu(TRUE);
  ELSE
    IF byte#sym.eosf THEN
      ou.print('Некорректный тег в симфайле: %#h\n',byte);
    END; RETURN
  END;
 END;
END scan;

PROCEDURE vis_sym;
  VAR head: sym.header;
BEGIN
  bio.get(sou,head,BYTES(head));
  IF head.magic#sym.MAGIC THEN
    ou.print('Некорректный заголовок симфайла %s\n',fn); RETURN
  END;
  IF head.vers#sym.VERSION THEN
    ou.print('ПРЕДУПРЕЖДЕНИЕ: Некорректная версия симфайла %s (%d вместо %d)\n'
                ,fn,head.vers,sym.VERSION);
  ELSE ou.print('версия симфайла: %d\n',head.vers);
  END;
  bio.seek(sou,head.offset,0);
  IF g()#sym.import THEN
    ou.print('Некорректный заголовок симфайла %s\n',fn); RETURN
  END;
  initVis; header; scan;
  ou.print('\n');
END vis_sym;

VAR e: ARRAY [0..7] OF CHAR;
    p: bio.PATHs;

BEGIN
  IF HIGH(args.words)<0 THEN HALT END;
  xpos:=NOT args.flag('-','x');
  IF args.flag('-','r') THEN
    bio.get_paths(p,env.ref);
    e:='ref';
  ELSE
    bio.get_paths(p,env.sym);
    e:='sym';
  END;
  str.print(fn,'%s.%s',args.words[0],e);
--  bio.check_io(TRUE);
  bio.lookup(p,sou,fn,'r');
  IF NOT bio.done THEN
    ou.print('IO error, %s\n',fn);
    HALT;
  END;
  vis_sym;
  bio.close(sou);
  IF NOT bio.done THEN
    ou.print('IO error, %s\n',fn);
  END;
END sk86.
