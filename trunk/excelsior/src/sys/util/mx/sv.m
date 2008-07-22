MODULE sv; (* Ned 02-Nov-88. (c) KRONOS *)

IMPORT  sys: SYSTEM;
IMPORT  sym: coolSym;
IMPORT  env: tskEnv;
IMPORT  str: Strings;
IMPORT time: Time;
IMPORT  bio: BIO;
IMPORT   ou: StdIO;
IMPORT args: tskArgs;

VAR sou: bio.FILE;
     fn: ARRAY [0..255] OF CHAR;
   name: ARRAY [0..255] OF CHAR;
   vers: INTEGER;
   xpos: BOOLEAN;

PROCEDURE g(): INTEGER;
  VAR n: INTEGER;
BEGIN
  n:=0;
  bio.read(sou,sys.ADR(n),1);
  ASSERT(bio.done);
  RETURN n
END g;

PROCEDURE g2(): INTEGER; BEGIN RETURN g()+g()*100h END g2;

PROCEDURE g4(): INTEGER;
BEGIN RETURN INTEGER(BITSET(g2())+BITSET(g2()<<16)) END g4;

PROCEDURE gX(): INTEGER;
  VAR b: INTEGER;
BEGIN b:=g();
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
BEGIN TypeNo:=32; lpos:=0;
  str.image(line,lpos,'     ');
END initVis;

PROCEDURE pr(VAL format: ARRAY OF CHAR; SEQ x: sys.WORD);
BEGIN str.image(line,lpos,format,x);
END pr;

PROCEDURE nl;
BEGIN ou.print('%s\n',line); lpos:=0;
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

--------------------------- ВНЕШНИЕ ----------------------------

PROCEDURE import;
  VAR kind,time: INTEGER; pl: ARRAY [0..255] OF CHAR;
    s: ARRAY [0..15] OF CHAR;
BEGIN importno; id; gName(pl); time:=gX(); kind:=g();
  CASE kind OF
    |sym.def : s:='def'
    |sym.imp : s:='imp'
    |sym.prog: s:='prog'
  ELSE str.print(s,'*%d*',kind);
  END;
  pr('IMPORT %s %s; ',s,name);
  skip_to(30); pr('(* [%d] %s. ',gX(),pl); WriteTime(time); pr(' *)');
END import;

PROCEDURE header;
  VAR kind,time: INTEGER; pl: ARRAY [0..255] OF CHAR;
BEGIN impNo:=1; lpos:=0; id; gName(pl); time:=gX(); kind:=g();
  CASE kind OF
    |0: pr('DEFINITION ');
    |1: pr('IMPLEMENTATION ');
    |2:
  ELSE  pr('%d ',kind)
  END;
  pr('MODULE %s; ',name);
  skip_to(30); pr('(* [%d] %s. ',gX(),pl); WriteTime(time); pr(' *)');
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

PROCEDURE disp;
BEGIN pr('[%d,%d]  ',gX(),gX());
END disp;

PROCEDURE scan;
  VAR byte,n: INTEGER; nl?: BOOLEAN;
BEGIN
 LOOP byte:=g();
  IF byte=sym.atrs THEN vis_args; byte:=g() END;
  IF nl? THEN nl END;
  nl?:=TRUE;
  CASE byte OF

   |sym.enumtype: typetag('enumtype');
   |sym.range   : typetag('range'); base; vX('min'); vX('max');
   |sym.array   : typetag('array'); pr('index=%3#h  ',gX()); base;
-- |sym.i_array : typetag('array'); vX('len'); base;
   |sym.openarr : typetag('openarr'); base;
   |sym.dynarr  : typetag('dynarr');  base; vX('dim');
   |sym.pointer : typetag('pointer');
   |sym.record  : typetag('record'); base; vX('size');
   |sym.set     : typetag('set'); base;
   |sym.proctype: typetag('proctype');
   |sym.functype: typetag('functype'); base;
   |sym.hidden  : typetag('hidden'); vX('size');
   |sym.linkage : tag('linkage'); pr('ptr=%3#h  ',gX()); base;
   |sym.parm    : id; tag('parm'); type; pr('[%d]  ',gX()); vKind; vname;
   |sym.field   : id; tag('field'); type; vX('ofs');               vname;

   |sym.enum    : id; tag('enum');   type; vX('modno'); vX('val'); vname;
   |sym.const   : id; tag('const');  type; vX('val');              vname;
   |sym.struct  : id; tag('struct'); type; disp;
                  n:=gX(); pr('(%d bytes)',n);
                  WHILE n>0 DO byte:=g(); DEC(n) END;              vname;
   |sym.sconst  : id; tag('sconst'); type; disp;                   vname;
   |sym.var     : id; tag('var');    type; disp; vKind;            vname;
   |sym.proc    : id; tag('proc');   type; disp;                   vname;
   |sym.type    : id; tag('type');   type; vX('modno');            vname;
   |sym.module  : id; tag('module');       vX('modno');            vname;
   |sym.import  : import
   |sym.endproc : tag('endproc');  vX('pno');
   |sym.endmodule:tag('endmod ');  vX('modno');
   |sym.xpos    : IF xpos THEN
                    tag('xpos');
                    pr('code=[%3$#h,%4$#h]  text=[%4d,%2d]'
                       ,gX(),gX(),g2(),g());
                  ELSE n:=gX(); n:=gX(); n:=g2(); n:=g(); nl?:=FALSE;
                  END;
   |sym.end_CU  : tag('end_CU'); vX('procs'); vX('vars');
  ELSE
    IF byte#sym.eosf THEN
      ou.print('Некорректный тег в симфайле: %#h\n',byte);
    END; RETURN
  END;
 END;
END scan;

PROCEDURE vis_sym;
  VAR head: sym.header;
      vers: INTEGER;
BEGIN
  bio.get(sou,head,BYTES(head));
  IF head.magic#sym.MAGIC THEN
    ou.print('Некорректный заголовок симфайла %s\n',fn); RETURN
  END;
  IF head.vers#sym.VERSION THEN
    ou.print('ПРЕДУПРЕЖДЕНИЕ: Некорректная версия симфайла %s (%d вместо %d)\n'
                ,fn,head.vers,sym.VERSION);
  ELSE ou.print('версия симфайла: %d\n',vers);
  END;
  bio.seek(sou,head.offset,0);
  IF g()#sym.import THEN
    ou.print('Некорректный заголовок симфайла %s\n',fn); RETURN
  END;
  initVis; header; scan;
  ou.print('\n');
END vis_sym;

VAR w: ARRAY [0..79] OF CHAR;
    e: ARRAY [0..7] OF CHAR;
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
  bio.check_io(TRUE);
  bio.lookup(p,sou,fn,'r');
  IF NOT bio.done THEN
    ou.perror(bio.error,'%s %%s\n',fn);
    HALT;
  END;
  vis_sym;
  bio.close(sou);
  IF NOT bio.done THEN
    ou.perror(bio.error,'%s %%s\n',fn);
  END;
END sv.
