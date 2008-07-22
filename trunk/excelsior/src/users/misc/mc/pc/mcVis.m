IMPLEMENTATION MODULE mcVis; (* 18-Oct-90. (c) KRONOS *)

IMPORT   pc: pcTab;
IMPORT  tty: Terminal;
IMPORT  scan: mcScan;

PROCEDURE vis_tree(m: pc.ref);

  VAR n: INTEGER;

  PROCEDURE one(i: pc.ref);

    PROCEDURE lst(n: pc.ref);
    BEGIN
      WHILE n#NIL DO one(n); n:=n^.nxt END;
    END lst;

    PROCEDURE num(m: pc.ref);
      VAR j: INTEGER;
    BEGIN
      FOR j:=0 TO n-1 DO tty.print('  ') END;
      tty.print('->%$5h\n',m);
    END num;

    PROCEDURE nam(i: pc.ref);
      VAR name: ARRAY [0..63] OF CHAR;
    BEGIN
      IF i^.nm>0 THEN scan.id_str(i^.nm,name); tty.print('  "%s"',name) END;
      tty.print('\n');
    END nam;

    VAR j: INTEGER; t: pc.ref;
  BEGIN
    IF i=NIL THEN RETURN END;
    FOR j:=0 TO n-1 DO tty.print('  ') END;
    INC(n);
    tty.print('%$5h ',i);
    CASE i^.md OF
      |pc.module       : tty.print('module\n');     lst(i^.dw);
      |pc.procedure    : tty.print('procedure'); nam(i); num(i^.l);
      |pc.inline       : tty.print('inline');    nam(i); num(i^.l);
      |pc.proc_body    : tty.print('proc_body\n');  num(i^.l); lst(i^.dw);
      |pc.code_body    : tty.print('code_body\n');  num(i^.l); lst(i^.dw);
      |pc.var          : tty.print('var   ');
                         IF    i^.l^.md=pc.var_prm THEN
                           tty.print('VAR'); t:=i^.l^.dw;
                         ELSIF i^.l^.md=pc.var_prm THEN
                           tty.print('VAL'); t:=i^.l^.dw;
                         ELSE t:=i^.l;
                         END;
                         nam(i);
                         num(t);
      |pc.usage        : tty.print('usage \n');     num(i^.dw); num(i^.r);
      |pc.enumeration  : tty.print('enumeration\n');
      |pc.const        : tty.print('const '); nam(i);    one(i^.dw); num(i^.l);
      |pc.integer      : tty.print('integer\n');
      |pc.boolean      : tty.print('boolean\n');
      |pc.dynarr       : tty.print('dynarr \n');                   num(i^.r);
      |pc.packed_dynarr: tty.print('packed_dynarr \n');            num(i^.r);
      |pc.packed_array_of: tty.print('parray_of \n');                num(i^.r);
      |pc.array_of     : tty.print('array_of \n');                 num(i^.r);
      |pc.array        : tty.print('array \n');        num(i^.l);  num(i^.r);
      |pc.packed_array : tty.print('packed array \n'); num(i^.l);  num(i^.r);
      |pc.real         : tty.print('real  \n');
      |pc.char         : tty.print('char  \n');
      |pc.set          : tty.print('set   \n');     num(i^.dw);
      |pc.record       : tty.print('record\n');     lst(i^.dw);
      |pc.packed_record: tty.print('packed_record\n');     lst(i^.dw);
      |pc.pointer      : tty.print('pointer\n');    num(i^.dw);
      |pc.profile      : tty.print('profile\n');    lst(i^.dw); one(i^.l);
      |pc.block        : tty.print('block \n');     lst(i^.dw);
      |pc.assign       : tty.print('assign\n');     one(i^.l); one(i^.r);
      |pc.call         : tty.print('call  \n');     one(i^.l); lst(i^.r);
      |pc.select       : tty.print('select\n');     lst(i^.l); lst(i^.r);
      |pc.if           : tty.print('if\n'); ASSERT(i^.dw#NIL);
                                            one(i^.dw); lst(i^.l); lst(i^.r);
      |pc.case         : tty.print('case  \n'); one(i^.dw); lst(i^.r); lst(i^.l);
      |pc.loop         : tty.print('loop\n'); lst(i^.l); one(i^.r); lst(i^.dw);
      |pc.exit         : tty.print('exit  \n'); num(i^.dw);
      |pc.return       : tty.print('return\n'); one(i^.dw);
      |pc.equal        : tty.print('=     \n'); one(i^.l); one(i^.r);
      |pc.inequality   : tty.print('#     \n'); one(i^.l); one(i^.r);
      |pc.less         : tty.print('<     \n'); one(i^.l); one(i^.r);
      |pc.less_equal   : tty.print('<=    \n'); one(i^.l); one(i^.r);
      |pc.greater      : tty.print('>     \n'); one(i^.l); one(i^.r);
      |pc.greater_equal: tty.print('>=    \n'); one(i^.l); one(i^.r);
      |pc.in           : tty.print('in    \n'); one(i^.l); one(i^.r);
      |pc.plus         : tty.print('+     \n'); one(i^.l); one(i^.r);
      |pc.minus        : tty.print('-     \n'); one(i^.l); one(i^.r);
      |pc.star         : tty.print('*     \n'); one(i^.l); one(i^.r);
      |pc.slash        : tty.print('/     \n'); one(i^.l); one(i^.r);
      |pc.div          : tty.print('div   \n'); one(i^.l); one(i^.r);
      |pc.mod          : tty.print('mod   \n'); one(i^.l); one(i^.r);
      |pc.rem          : tty.print('rem   \n'); one(i^.l); one(i^.r);
      |pc.index        : tty.print('index \n'); num(i^.dw); one(i^.l); one(i^.r);
      |pc.field        : tty.print('field \n'); one(i^.l); one(i^.r);
      |pc.aggregate    : tty.print('aggregate\n'); num(i^.l); lst(i^.r);
      |pc.range        : tty.print('range \n'); one(i^.l); one(i^.r);
      |pc.subtype      : tty.print('subtype\n'); one(i^.l); one(i^.r);
      |pc.number       : tty.print('number %$8h\n',i^.val); num(i^.dw);
      |pc.nil          : tty.print('nil\n');
      |pc.string       : tty.print('string\n'); num(i^.dw);
      |pc.worder       : tty.print('worder\n'); num(i^.dw); one(i^.r);
      |pc.trunc        : tty.print('trunc \n'); num(i^.dw); one(i^.r);
      |pc.float        : tty.print('float \n'); num(i^.dw); one(i^.r);
      |pc.min          : tty.print('min   \n'); num(i^.dw); num(i^.r);
      |pc.max          : tty.print('max   \n'); num(i^.dw); num(i^.r);
      |pc.size_check   : tty.print('size_check\n');  num(i^.dw); one(i^.r);
      |pc.range_check  : tty.print('range_check\n'); num(i^.dw); one(i^.r);
      |pc.const_check  : tty.print('const_check\n'); num(i^.dw); one(i^.r);
      |pc.adr          : tty.print('adr   \n'); num(i^.dw); one(i^.r);
      |pc.size         : tty.print('size  \n'); num(i^.dw); one(i^.r);
      |pc.bytes        : tty.print('bytes \n'); num(i^.dw); one(i^.r);
      |pc.bits         : tty.print('bits  \n'); num(i^.dw); one(i^.r);
      |pc.high         : tty.print('high  \n'); num(i^.dw); one(i^.r);
      |pc.len          : tty.print('len   \n'); num(i^.dw); one(i^.r);
      |pc.assert       : tty.print('assert\n'); one(i^.l); one(i^.r);
      |pc.program_check: tty.print('program_check\n'); one(i^.l); one(i^.r);
      |pc.incl         : tty.print('incl  \n'); one(i^.l); one(i^.r);
      |pc.excl         : tty.print('excl  \n'); one(i^.l); one(i^.r);
      |pc.deref        : tty.print('^     \n'); num(i^.dw); one(i^.l);
      |pc.rol          : tty.print('rol   \n');
      |pc.ror          : tty.print('ror   \n');
      |pc.not          : tty.print('not   \n'); one(i^.r);
      |pc.goto         : tty.print('goto\n'); num(i^.dw); one(i^.l); one(i^.r);
      |pc.continue     : tty.print('continue\n');
      |pc.inc          : tty.print('inc   \n');  one(i^.l); one(i^.r);
      |pc.dec          : tty.print('inc   \n');  one(i^.l); one(i^.r);
      |pc.new          : tty.print('new\n'); one(i^.dw); one(i^.l); one(i^.r);
      |pc.dispose      : tty.print('dispose\n'); one(i^.dw); one(i^.l);
      |pc.resize       : tty.print('resize\n'); one(i^.dw); one(i^.l); one(i^.r);
    ELSE
      tty.print('*** %d ***\n',i^.md);
    END;
    DEC(n);
  END one;
BEGIN
  n:=0; one(m);
END vis_tree;

END mcVis.
