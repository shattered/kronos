MODULE pm;                              (* Andy 27-Apr-89. (c) KRONOS *)
                                        (* Andy 11-May-89. (c) KRONOS *)
                                        (* Andy 19-Dec-89. (c) KRONOS *)
                                        (* Andy 11-Jan-90. (c) KRONOS *)
                                        (* Andy 12-Mar-90. (c) KRONOS *)

----------------------- Project Manager --------------------------------
                        ---------------

                IMPORT     sys: SYSTEM;
                IMPORT     bio: BIO;
                IMPORT     arg: tskArgs;
                IMPORT     str: Strings;
                IMPORT     key: Keyboard;
                IMPORT     tty: Terminal;
                IMPORT    heap: Heap;
                IMPORT   shell: Shell;
                IMPORT  editor: myEditor;
                IMPORT     sle: strEditor;
                IMPORT     env: tskEnv;
                IMPORT    walk: fsWalk;
                IMPORT     std: StdIO;
                IMPORT  errmsg: Lexicon;
                IMPORT      mx: mxPars;
                IMPORT    mxio: coolIO;
                IMPORT      dc: coolDefs;
                IMPORT      tm: Time;
                IMPORT          ASCII;

(*$X+*)

TYPE MSG_PRINT= PROCEDURE (INTEGER, ARRAY OF CHAR, SEQ sys.WORD);

TYPE pMSG= POINTER TO MSG;
      MSG= RECORD line,col: INTEGER;
                       msg: STRING;
                      next: pMSG;
           END;

PROCEDURE msg1(dmode: INTEGER; VAL fmt: ARRAY OF CHAR; SEQ args: sys.WORD);
                                                       FORWARD;

PROCEDURE msg2(dmode: INTEGER; VAL fmt: ARRAY OF CHAR; SEQ args: sys.WORD);
                                                       FORWARD;

PROCEDURE low_info(dmode: BITSET; VAL fmt: ARRAY OF CHAR; SEQ args: sys.WORD);
                                                       FORWARD;

CONST -- display modes
   d_norm= 0;    d_rev= 3;
  d_inten= 1;   d_line= 4;
    d_dim= 2;

---------------------------- Memory ------------------------------------
                             ------
PROCEDURE getmem(VAR a: sys.ADDRESS; sz: INTEGER);
BEGIN heap.allocate(a,sz);
  IF a=NIL THEN
      low_info({d_inten},'Not enough memory in heap, sorry...'); HALT
  END;
END getmem;

PROCEDURE extend(VAR a: sys.ADDRESS; VAR high: INTEGER;
                                         len,bsize: INTEGER);
BEGIN heap.reallocate(a,high,len,bsize);
  IF high<(len-1) THEN
      low_info({d_inten},'Not enough memory in heap, sorry...'); HALT
  END;
END extend;

WITH STORAGE (    NEW: getmem;
              DISPOSE: heap.deallocate;
               RESIZE: extend);
------------------------------------------------------------------------

MODULE comp;            (* Andy 15-Feb-90. (c) KRONOS *)
                        (* Andy 10-Mar-90. (c) KRONOS *)

                IMPORT  sys, mx, mxio, dc, str, tm, arg, env;
                IMPORT  pMSG, msg1, msg2;

EXPORT QUALIFIED OK?, line, time, iotime, files, code,
                      LINE, TIME, IOTIME,
                 del_list,
                 init_compiler, compile, GetImportList;

VAR OK?: BOOLEAN; -- result of last compilation

VAR opts: BITSET;
     cpu: INTEGER;

VAR
  text: dc.io_ptr;
 super: PROCEDURE (dc.io_ptr);

VAR
  errs: INTEGER;      errlim: INTEGER;
  line: INTEGER;        LINE: INTEGER;
  time: INTEGER;        TIME: INTEGER;
iotime: INTEGER;      IOTIME: INTEGER;
 files: INTEGER;        code: INTEGER;

---------------------------------------------------------------
PROCEDURE stop(text: dc.io_ptr); BEGIN text^.done:=FALSE END stop;

VAR err_list: pMSG;

PROCEDURE add_list(l,c: INTEGER; VAL msg: ARRAY OF CHAR; VAR list: pMSG);
  VAR el,ptr: pMSG;
BEGIN
  NEW(el); el^.line:=l; el^.col:=c; el^.next:=NIL;
  NEW(el^.msg,str.len(msg)+1);
  str.copy(el^.msg,msg);
  IF list=NIL THEN list:=el; RETURN END;
  ptr:=list; WHILE ptr^.next#NIL DO ptr:=ptr^.next END;
  ptr^.next:=el;
END add_list;

PROCEDURE del_list(VAR list: pMSG);
  VAR ptr,ptr1: pMSG;
BEGIN ptr:=list;
  WHILE ptr#NIL DO
      ptr1:=ptr^.next; DISPOSE(ptr^.msg); DISPOSE(ptr);
      ptr:=ptr1;
  END;
  list:=NIL;
END del_list;

PROCEDURE print(VAL format: ARRAY OF CHAR; SEQ arg: sys.WORD);
BEGIN msg1(0,format,arg) END print;

PROCEDURE error(l,c: INTEGER; VAL s,format: ARRAY OF CHAR; SEQ arg: sys.WORD);
  VAR message: ARRAY [0..79] OF CHAR;
BEGIN
  IF errs<errlim THEN
    INC(errs);
    IF errs=1 THEN msg1(2,'ERRORS DETECTED') END;
    str.print(message,format,arg);
    add_list(l,c,message,err_list);
    IF errs=errlim THEN text^.doio:=stop END;
  END;
END error;
---------------------------------------------------------------

PROCEDURE result;
BEGIN
  OK?:=(errs=0);
  IF NOT OK? THEN RETURN END;
  INC(TIME,time);       INC(IOTIME,iotime);
  INC(LINE,line);       INC(files);
  INC(code,mx.code);
END result;

PROCEDURE ini(VAR xxxx: dc.io_ptr;
              VAL name: ARRAY OF CHAR;
                  unit: INTEGER;
                 print: dc.PRINT);
BEGIN
  IF (unit=dc.code) OR (unit=dc.sym_ou) THEN
      time:=tm.sys_time(tm.milisec)-time;
  END;
  mxio.ini(xxxx,name,unit,print);
END ini;

PROCEDURE exi(VAR x: dc.io_ptr);
BEGIN
  IF x^.kind=dc.sym_in THEN time:=tm.sys_time(tm.milisec) END;
  mxio.exi(x);
END exi;

PROCEDURE get_str(text: dc.io_ptr);
BEGIN
  super(text);
  INC(line);
  IF line MOD 64 = 0 THEN msg2(0,'%4d',line) END;
END get_str;

PROCEDURE parser;
BEGIN
  errs:=0;
  line:=0;
  super:=text^.doio;
  text^.doio:=get_str;
  iotime:=tm.sys_time(tm.milisec);
  time  :=iotime;
  mx.compile(text,ini,exi,error,print,opts,cpu);
  IF errs>0 THEN time:=tm.sys_time(tm.milisec)-time END;
  iotime:=tm.sys_time(tm.milisec)-iotime-time;
  result;
END parser;

PROCEDURE init_compiler(user_opts: BITSET);
  VAR sym: STRING;
      out: STRING;
BEGIN
  LINE:=0; TIME:=0; IOTIME:=0; code:=0; files:=0;
  IF arg.string(env.sym,sym) THEN END;
  IF arg.string('mxOUT',out) THEN END;
  mxio.set('',sym,out,print);
  IF NOT arg.number('mxERRLIM',errlim) THEN errlim:=12 END;
  IF NOT arg.number('mxCPU'   ,cpu   ) THEN cpu:=mx.cpu END;
  err_list:=NIL;
  opts:=user_opts;
END init_compiler;

PROCEDURE compile(VAL module: ARRAY OF CHAR; VAR list: pMSG);
BEGIN
  err_list:=list;
  mxio.ini(text,module,dc.text,print);
  IF text^.done THEN
    parser;
    mxio.exi(text);
  END;
  list:=err_list;
END compile;

VAR _error: BOOLEAN;

PROCEDURE il_error(line,col: INTEGER;
                   VAL src: ARRAY OF CHAR;
                   VAL fmt: ARRAY OF CHAR; SEQ args: sys.WORD);
BEGIN _error:=TRUE;
END il_error;

PROCEDURE il_print(VAL fmt: ARRAY OF CHAR; SEQ args: sys.WORD);
BEGIN
END il_print;

PROCEDURE GetImportList(VAL name: ARRAY OF CHAR; diagn: dc.PRINT;
                        put: mx.ONE_MODULE;
                        VAR iores: BOOLEAN; VAR mxres: BOOLEAN);
BEGIN
  _error:=FALSE;
  iores :=FALSE;
  mxio.ini(text,name,dc.text,diagn);
  IF NOT text^.done THEN iores:=TRUE; RETURN END;
  mx.get_import(text,il_error,il_print,put);
  mxio.exi(text);
  mxres:=_error;
END GetImportList;

BEGIN err_list:=NIL;
      text:=NIL;
END comp;

CONST header= 'Modula-2 Project Manager  (c) KRONOS';

TYPE  ModName= ARRAY [0..31] OF CHAR;

      ModKind= (mdef, mimp, mtext);
      ModKinds= SET OF ModKind;

     pModItem= POINTER TO ModItem;
      ModItem= RECORD name: ModName; -- имя модуля в файловой системе
                     lname: ModName; -- собственное имя модуля
                      path: ARRAY [0..255] OF CHAR;
                       len: INTEGER;
                      kind: ModKind;
                    status: INTEGER;
                       atr: BITSET;
                    d_mode: BITSET;
                  msg_list: pMSG;
               END;

CONST -- Флаги атрибутов модуля
          done?= 0;     sorted?= 6;
         ready?= 1;
      selected?= 2;
      for_comp?= 3;
       comp_ok?= 4;
      comp_err?= 5;

CONST SPACE = ' ';
      GOLD  = key.f1;
      SILVER= key.f10;
      BRONZE= key.f2;

---------------------------- Screen ------------------------------------
                             ------
CONST HeaderLine=  0;  NamesInLine=  4;
      StatusLine=  1;    NameWidth= 19;
        NameLine=  3;    NameField= 18;
        LastLine= 20;
        InfoLine= 22;
       Info1Line= 23;

        FirstPos=  1;

        MaxNames= (LastLine-NameLine+1)*NamesInLine;

TYPE System= ARRAY [0..MaxNames-1] OF ModItem;

VAR project: System;
      items: INTEGER;

PROCEDURE norm;
BEGIN tty.set_color(0);
      tty.set_blinking(0);
      tty.set_reverse(0);
      tty.set_underline(0);
END norm;

VAR has_low?: BOOLEAN;

PROCEDURE mode(modes: BITSET);
BEGIN
  IF  d_inten IN modes THEN
     IF has_low? THEN tty.set_color(+1)
                 ELSE tty.set_blinking(1)
     END;
  ELSIF d_dim IN modes THEN
     IF has_low? THEN tty.set_color(-1)
                 ELSE tty.set_color(+1)
     END;
  END;
  IF d_rev  IN modes THEN tty.set_reverse(1) END;
  IF d_line IN modes THEN tty.set_underline(1) END;
END mode;

PROCEDURE c_on;  BEGIN tty.set_cursor(1)  END c_on;
PROCEDURE c_off; BEGIN tty.set_cursor(0) END c_off;

PROCEDURE pos?(inx: INTEGER; VAR line: INTEGER; VAR col: INTEGER);
BEGIN
  line:= (inx DIV NamesInLine)+NameLine;
   col:= (inx MOD NamesInLine)*NameWidth+FirstPos;
END pos?;

PROCEDURE pick_item(no: INTEGER);
  VAR l,c: INTEGER;
BEGIN pos?(no,l,c); tty.set_pos(l,c);
  mode(project[no].d_mode+{d_rev});
  tty.print('%*s',project[no].len,project[no].name);
  norm;
END pick_item;

PROCEDURE light_item(no: INTEGER);
  VAR l,c: INTEGER;
BEGIN pos?(no,l,c); tty.set_pos(l,c);
  mode(project[no].d_mode+{d_inten});
  tty.print('%*s',project[no].len,project[no].name);
  norm;
END light_item;

PROCEDURE select_item(no: INTEGER);
  VAR l,c: INTEGER;
BEGIN pos?(no,l,c); tty.set_pos(l,c);
  mode(project[no].d_mode+{d_line});
  tty.print('%*s',project[no].len,project[no].name);
  norm;
END select_item;

PROCEDURE show_item(no: INTEGER);
  VAR l,c: INTEGER;
BEGIN pos?(no,l,c); tty.set_pos(l,c);
  mode(project[no].d_mode);
  tty.print('%*s',project[no].len,project[no].name);
  norm;
END show_item;

PROCEDURE draw;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO items-1 DO show_item(i) END;
END draw;

PROCEDURE clear_field;
  VAR i: INTEGER;
BEGIN
  FOR i:=NameLine TO LastLine DO
       tty.set_pos(i,0); tty.erase_line(0);
  END;
END clear_field;

PROCEDURE high_info(dmode: BITSET; wait?, clear?: BOOLEAN;
                           VAL fmt: ARRAY OF CHAR; SEQ args: sys.WORD);
  VAR k: CHAR;
BEGIN
  tty.set_pos(StatusLine,0); tty.erase_line(0);
  mode(dmode);
  tty.print(fmt,args);
  IF wait? THEN tty.set_pos(StatusLine,tty.state^.columns-15);
       tty.print('PRESS KEY...'); key.read(k);
       IF clear? THEN norm;
            tty.set_pos(StatusLine,0); tty.erase_line(0); RETURN
       END;
  END;
  norm;
END high_info;

PROCEDURE low_info(dmode: BITSET; VAL fmt: ARRAY OF CHAR; SEQ args: sys.WORD);
BEGIN
  tty.set_pos(InfoLine,0);  tty.erase_line(0);
  tty.set_pos(Info1Line,0); tty.erase_line(0);
  tty.set_pos(InfoLine,0);
  mode(dmode); tty.print(fmt,args); norm;
END low_info;

PROCEDURE msg1(dmode: INTEGER; VAL fmt: ARRAY OF CHAR; SEQ args: sys.WORD);
  VAR d: BITSET;
BEGIN d:= {d_norm};
  IF dmode=1 THEN d:={d_inten} ELSIF dmode=2 THEN d:={d_norm,d_rev} END;
  tty.set_pos(InfoLine,0);  tty.erase_line(0);
  mode(d); tty.print(fmt,args); norm;
END msg1;

PROCEDURE msg2(dmode: INTEGER; VAL fmt: ARRAY OF CHAR; SEQ args: sys.WORD);
  VAR d: BITSET;
BEGIN d:= {d_norm};
  IF dmode=1 THEN d:={d_inten} ELSIF dmode=2 THEN d:={d_norm,d_rev} END;
  tty.set_pos(Info1Line,0);  tty.erase_line(0);
  mode(d); tty.print(fmt,args); norm;
END msg2;

PROCEDURE clear_low;  BEGIN low_info({d_norm},' \n ') END clear_low;
PROCEDURE clear_high; BEGIN high_info({d_norm},FALSE,FALSE,'  ') END clear_high;

VAR bell?: BOOLEAN;
PROCEDURE bell; BEGIN IF bell? THEN tty.Write(ASCII.BEL); tty.Write(ASCII.BEL) END;
END bell;

PROCEDURE show_blink(VAL fmt: ARRAY OF CHAR; SEQ args: sys.WORD);
  PROCEDURE wait(d: INTEGER); VAR i: INTEGER; BEGIN FOR i:=0 TO d DO END END wait;
BEGIN
  low_info({d_inten},fmt,args); wait(30000); clear_low; wait(4000);
  low_info({d_inten},fmt,args); wait(30000); clear_low;
END show_blink;

PROCEDURE showDONE; BEGIN clear_high; clear_low; show_blink('DONE')
END showDONE;

PROCEDURE GetKey(SEQ alts: CHAR):CHAR;
  VAR k: CHAR; i: INTEGER;
BEGIN
  LOOP key.read(k);
    FOR i:=0 TO HIGH(alts) DO IF k=alts[i] THEN RETURN k END END;
    bell;
  END;
END GetKey;

PROCEDURE NextKey(VAR curr: INTEGER):CHAR;
  VAR k: CHAR;
  PROCEDURE s; BEGIN show_item(curr) END s;
  PROCEDURE p; BEGIN pick_item(curr) END p;
BEGIN p;
  LOOP key.read(k);
       CASE k OF
        |key.up:    s; curr:= (curr+items*4-4) MOD items; p;
        |key.dw:    s; curr:= (curr+4) MOD items;         p;
        |key.left:  s; curr:= (curr+items-1) MOD items;   p;
        |key.right: s; curr:= (curr+1) MOD items;         p;
       ELSE RETURN k END;
  END;
END NextKey;

PROCEDURE start;
BEGIN norm;
  tty.home; tty.erase(2);
  c_off;
  tty.set_pos(HeaderLine,40);
  mode({d_line}); tty.print('%s',header); norm;
  tty.set_pos(NameLine-1,0);
  tty.print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~');
  tty.set_pos(LastLine+1,0);
  tty.print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~');
END start;

CONST   ready= 0;

PROCEDURE add_list(VAL fname,mname: ARRAY OF CHAR);

  VAR ext: ARRAY [0..15] OF CHAR;

  PROCEDURE get_ext(VAR s: ARRAY OF CHAR; VAR ext: ARRAY OF CHAR);
    VAR l,i: INTEGER;
  BEGIN
    l:=str.len(s);
    i:=l-1;
    WHILE (i>=0) & (s[i]#'.') DO DEC(i) END;
    IF i<0 THEN ext[0]:=0c; RETURN END;
    str.sub_str(ext,s,i+1,l-i-1);
    s[i]:=0c;
  END get_ext;

  VAR i: INTEGER;

BEGIN
  IF items>HIGH(project) THEN
    low_info({d_inten},'No more then %d files in project, sorry ...',
                       HIGH(project)+1);
    HALT;
  END;
  FOR i:=0 TO items-1 DO
    IF project[i].name=mname THEN
      low_info({d_inten},'Two modules with name %s in project, sorry ...',
                         mname);
      HALT;
    END;
  END;
  WITH project[items] DO
    str.copy(path,fname);
    str.copy(name,mname);
    len:=str.len(name);
    atr:= {}; status:= ready; d_mode:= {d_norm};
    msg_list:= NIL;
    str.copy(lname,mname);
    get_ext(lname,ext);
    IF    ext='d'0c THEN kind:=mdef
    ELSIF ext='m'0c THEN kind:=mimp
    ELSE str.append(lname,'.%s',ext); kind:=mtext
    END;
  END;
  INC(items);
END add_list;

PROCEDURE err_msg(VAL msg: ARRAY OF CHAR);
BEGIN low_info({d_inten},'%s',msg); HALT
END err_msg;

PROCEDURE err_print(VAL fmt: ARRAY OF CHAR; SEQ args: sys.WORD);
BEGIN low_info({d_inten},fmt,args);
END err_print;

PROCEDURE find_module(mkind: ModKind; VAL nm: ARRAY OF CHAR;
                             VAR no: INTEGER): BOOLEAN; FORWARD;

PROCEDURE warning(VAL lname: ARRAY OF CHAR);
  VAR x: CHAR;
BEGIN
  tty.set_pos(StatusLine,0); tty.erase_line(0);
  mode({d_norm,d_line}); tty.print("WARNING");        norm;
  mode({d_norm});        tty.print(": there is no ");
  mode({d_inten});       tty.print("%s.m",lname);     norm;
  mode({d_norm});        tty.print(" for ");
  mode({d_inten});       tty.print("%s.d",lname);     norm;
  mode({d_norm});        tty.print(" in project");
  tty.set_pos(StatusLine,tty.state^.columns-15);
  tty.print('PRESS KEY...');
  key.read(x);
  tty.set_pos(StatusLine,0); tty.erase_line(0);
END warning;

PROCEDURE order_dir;
  VAR tmp: ModItem; no: INTEGER;
  VAR i,j: INTEGER;
BEGIN
  FOR i:=items-2 TO 0 BY -1 DO
     FOR j:=0 TO i DO
        IF project[j].name>project[j+1].name THEN
            tmp:= project[j]; project[j]:= project[j+1]; project[j+1]:= tmp;
        END;
     END;
  END;
  FOR i:=0 TO items-1 DO WITH project[i] DO
     IF (kind=mdef) AND NOT find_module(mimp,lname,no) THEN warning(lname) END;
  END END;
END order_dir;

PROCEDURE collect;

  PROCEDURE chk;
    VAR msg: ARRAY [0..79] OF CHAR;
  BEGIN
    IF walk.done THEN RETURN END;
    errmsg.perror(msg,walk.error,'Iterating directory: %%s');
    err_msg(msg);
  END chk;

  PROCEDURE one(VAL pattern: ARRAY OF CHAR);
    VAR tree: walk.TREE;
        name: ARRAY [0..63] OF CHAR;
       fname: ARRAY [0..255] OF CHAR;
        mode: BITSET;
  BEGIN
    walk.walk(tree,pattern,TRUE); chk;
    WHILE walk.next_dir(tree) DO
      WHILE walk.next_entry(tree,name,mode) DO
        walk.fpath(tree,fname);
        add_list(fname,name);
      END;
      chk;
    END;
    chk;
    walk.dispose(tree);
  END one;

  VAR i: INTEGER;
BEGIN
  high_info({d_norm},FALSE,FALSE,'Reading directory...');
  items:=0;
  IF LEN(arg.words)<=0 THEN one('*.?')
  ELSE
    FOR i:=0 TO HIGH(arg.words) DO one(arg.words[i]) END;
  END;
  IF items<=0 THEN
    low_info({d_inten},'No modules in project, sorry ...');
    HALT;
  END;
  order_dir;
  clear_high;
END collect;

------------------- Import Graph Information ---------------------------
                    ------------------------
TYPE   IROW= DYNARR OF BITSET; -- представление множества модулей
                               -- в виде шкалы
     IGRAPH= DYNARR OF IROW;

VAR Graph: IGRAPH;

CONST b= BITS(BITSET);

PROCEDURE Edit(no: INTEGER); FORWARD;

PROCEDURE mk_graph(VAR graph: IGRAPH);
  VAR row_sz,i,j: INTEGER;
BEGIN ASSERT(items>0);
  NEW(graph,items);
  row_sz:=(items+b-1) DIV b;
  FOR i:=0 TO HIGH(graph) DO
     NEW(graph[i],row_sz);
     FOR j:=0 TO row_sz-1 DO graph[i,j]:={} END;
  END;
END mk_graph;

PROCEDURE del_graph(VAR graph: IGRAPH);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(graph) DO DISPOSE(graph[i]) END;
  DISPOSE(graph); NEW(graph);
END del_graph;

PROCEDURE copy_graph(VAL from: IGRAPH; VAR to: IGRAPH);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(from) DO to[i]:=from[i] END;
END copy_graph;

PROCEDURE find_module(mkind: ModKind; VAL nm: ARRAY OF CHAR;
                                            VAR no: INTEGER): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO items-1 DO WITH project[i] DO
     IF (kind=mkind) AND (nm=lname) THEN no:=i; RETURN TRUE END;
  END END;
  no:= -1; RETURN FALSE
END find_module;

PROCEDURE find_name(VAL nm: ARRAY OF CHAR; VAR no: INTEGER): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO items-1 DO
     IF project[i].name=nm THEN no:=i; RETURN TRUE END;
  END;
  no:= -1; RETURN FALSE
END find_name;

VAR curr_row: INTEGER;

PROCEDURE clean_row;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(Graph[curr_row]) DO
       Graph[curr_row,i]:={};
  END;
END clean_row;

PROCEDURE incl_link(VAL nm: ARRAY OF CHAR);
  VAR no: INTEGER;
BEGIN
----  low_info({d_norm},'%s',nm);
  IF find_module(mdef,nm,no) THEN
      INCL(Graph[curr_row,no DIV b],no MOD b);
  END;
END incl_link;

PROCEDURE done_off;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO items-1 DO WITH project[i] DO
       EXCL(d_mode,d_dim); INCL(d_mode,d_norm);
  END END;
END done_off;

PROCEDURE set_norm;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO items-1 DO WITH project[i] DO
       d_mode:= d_mode-{d_dim,d_inten}+{d_norm};
  END END;
END set_norm;

PROCEDURE _diagn(err: INTEGER; VAL name: ARRAY OF CHAR);
  VAR msg: ARRAY [0..79] OF CHAR;
BEGIN errmsg.perror(msg,err,'%s: %%s',name);
      low_info({d_inten},"%s",msg);
END _diagn;

PROCEDURE dprint(VAL fmt: ARRAY OF CHAR; SEQ args: sys.WORD);
BEGIN low_info({d_inten},fmt,args);
END dprint;

PROCEDURE mk_one_row(no: INTEGER): BOOLEAN;
  VAR r,ior: BOOLEAN;
BEGIN
  LOOP
     curr_row:=no;
     comp.GetImportList(project[no].path,dprint,incl_link,ior,r);
     IF ior   THEN RETURN TRUE  END;
     IF NOT r THEN RETURN FALSE END;
     low_info({d_inten},'Bad import section in module %s\n'
                        '           Edit/Abort? (e/ESC)',
                        project[no].path);
     CASE GetKey('e','E',ASCII.ESC) OF
      |'e','E':   Edit(no);
                  high_info({d_inten},FALSE,FALSE,'Creating import graph...');
                  clean_row;
      |ASCII.ESC: RETURN TRUE
     ELSE ASSERT(FALSE) END;
  END;
END mk_one_row;

PROCEDURE MakeGraph():BOOLEAN;
  VAR k: CHAR; i: INTEGER;
BEGIN
  high_info({d_inten},FALSE,FALSE,'Creating import graph...');
  del_graph(Graph);
   mk_graph(Graph);
  FOR i:=0 TO items-1 DO WITH project[i] DO
     IF (kind=mdef) OR (kind=mimp) THEN
         INCL(d_mode,d_inten); show_item(i);
         IF mk_one_row(i) THEN
             high_info({d_inten,d_line},TRUE,TRUE,
                       'Import graph generation aborted...');
             clear_low;
             set_norm; draw;
             del_graph(Graph);
             RETURN FALSE
         END;
         d_mode:= d_mode-{d_inten}+{d_dim}; show_item(i);
     END;
  END END;
  clear_low;
  high_info({d_norm},TRUE,TRUE,'Import graph created...');
  set_norm; draw;
  RETURN TRUE
END MakeGraph;

VAR GraphName: ARRAY [0..63] OF CHAR;

VAR gs: bio.FILE; --stm.STREAM;
VAR gn_desc: sle.descriptor;

CONST separ= '#'0c;
      Separ= '###';

PROCEDURE clear_gact;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO items-1 DO WITH project[i] DO
      EXCL(atr,ready?); d_mode:= d_mode-{d_dim,d_inten}+{d_norm};
  END END;
  draw;
END clear_gact;

PROCEDURE diagn(err: INTEGER); BEGIN _diagn(err,GraphName) END diagn;

PROCEDURE read_graph(VAR unknown: ARRAY OF CHAR): BOOLEAN;
  VAR nm: ARRAY [0..127] OF CHAR;
      no,i: INTEGER;
  CONST b= BITS(BITSET);
BEGIN
  LOOP
       bio.getstr(gs,nm,0);
       IF nm=Separ THEN EXIT END;
       IF NOT find_name(nm,no) THEN str.copy(unknown,nm); RETURN FALSE END;
       light_item(no);
       INCL(project[no].atr,ready?);
       LOOP
            bio.getstr(gs,nm,0);
            IF nm=separ THEN EXIT END;
            IF NOT find_name(nm,i) THEN
                str.copy(unknown,nm); RETURN FALSE
            END;
            INCL(Graph[no,i DIV b],(i MOD b));
       END;
       show_item(no);
  END;
  FOR i:=0 TO items-1 DO WITH project[i] DO
     IF (kind=mdef) OR (kind=mimp) THEN
         IF NOT (ready? IN atr) THEN str.copy(unknown,name); RETURN FALSE END;
     END;
  END END;
  RETURN TRUE
END read_graph;

PROCEDURE ReadGraph(online: BOOLEAN);

  PROCEDURE close;
  BEGIN bio.close(gs); IF NOT bio.done THEN diagn(bio.error) END;
  END close;

  PROCEDURE quit; BEGIN del_graph(Graph); clear_low; clear_high END quit;
  PROCEDURE warn; BEGIN high_info({d_inten},TRUE,TRUE,'Import graph not generated...') END warn;

  VAR err_name: ARRAY [0..127] OF CHAR;
BEGIN
  high_info({d_norm},FALSE,FALSE,'Reading import graph from file...');
  del_graph(Graph); mk_graph(Graph);
  IF online THEN
      c_on;
      gn_desc^.how:=sle.show;
      sle.edit_str('Enter file name >>',GraphName,InfoLine,0,50,
                                                  gn_desc,ASCII.ESC);
      c_off;
      IF gn_desc^.last=ASCII.ESC THEN quit; RETURN END;
  END;
  bio.open(gs,GraphName,'r');
  IF NOT bio.done THEN diagn(bio.error); warn; quit; RETURN END;
  IF NOT read_graph(err_name) THEN
       low_info({d_inten},
                'Import graph/Project conflict: name %s not found',err_name);
       warn; quit; clear_gact; close;
  ELSE clear_gact; close; IF online THEN showDONE END END;
END ReadGraph;

PROCEDURE gput(VAL s: ARRAY OF CHAR);
BEGIN
  bio.putstr(gs,s,0); IF NOT bio.done THEN RETURN END;
  bio.putch(gs,ASCII.NL);
END gput;

PROCEDURE write_graph;
  CONST b= BITS(BITSET);
  VAR k: ModKind;
      i,j: INTEGER;
BEGIN
  FOR i:=0 TO items-1 DO
     k:=project[i].kind;
     IF (k=mdef) OR (k=mimp) THEN
         light_item(i);
         gput(project[i].name); IF NOT bio.done THEN RETURN END;
         FOR j:=0 TO items-1 DO
            IF (j MOD b) IN Graph[i,j DIV b] THEN
                gput(project[j].name);
                IF NOT bio.done THEN RETURN END;
            END;
         END;
         gput(separ);           IF NOT bio.done THEN RETURN END;
         show_item(i);
     END;
  END;
  gput(Separ);
END write_graph;

PROCEDURE WriteGraph;
  PROCEDURE chk;
  BEGIN diagn(bio.error);
    high_info({d_inten},TRUE,TRUE,'Import graph NOT written!');
    clear_low; clear_high;
  END chk;
BEGIN
  high_info({d_norm},FALSE,FALSE,'Writing import graph to file...');
  IF HIGH(Graph)<0 THEN
      high_info({d_inten,d_line},TRUE,TRUE,'No import graph generated yet!');
      RETURN
  END;
  c_on;
  gn_desc^.how:=sle.show;
  sle.edit_str('Enter file name >>',GraphName,InfoLine,0,50,
                                              gn_desc,ASCII.ESC);
  c_off;
  IF gn_desc^.last=ASCII.ESC THEN clear_low; clear_high; RETURN END;
  bio.create(gs,GraphName,'wh',0);   IF NOT bio.done THEN chk; RETURN END;
  write_graph;                       IF NOT bio.done THEN chk; bio.purge(gs); RETURN END;
  bio.close(gs);                     IF NOT bio.done THEN chk; RETURN END;
  showDONE;
END WriteGraph;

---------------------------- Logic -------------------------------------
                             -----
TYPE COMPTASK= RECORD -- задание на компиляцию
                  cnt: INTEGER;
                 list: ARRAY [0..MaxNames-1] OF INTEGER;
               END;

PROCEDURE mk_row(VAR row: IROW);
  VAR i: INTEGER;
BEGIN
  NEW(row,(items+b-1) DIV b);
  FOR i:=0 TO HIGH(row) DO row[i]:={} END;
END mk_row;

PROCEDURE del_row(VAR row: IROW); BEGIN DISPOSE(row); NEW(row) END del_row;

PROCEDURE add_row(VAL el: IROW; VAR sum: IROW);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(el) DO sum[i]:=sum[i]+el[i] END;
END add_row;

PROCEDURE make_sum(VAL set: IROW; VAL matrix: IGRAPH;
                                     VAR res: IROW);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO items-1 DO
     IF (i MOD b) IN set[i DIV b] THEN add_row(matrix[i],res) END;
  END;
END make_sum;

PROCEDURE trans_graph(VAL matrix: IGRAPH; VAR res: IGRAPH; diag?: BOOLEAN);
  VAR i,j: INTEGER;
BEGIN
  FOR i:=0 TO items-1 DO
     FOR j:=0 TO items-1 DO
        IF (i MOD b) IN matrix[j,i DIV b] THEN
            INCL(res[i,j DIV b],j MOD b);
        END;
     END;
     IF diag? THEN INCL(res[i,i DIV b],i MOD b) END;
  END;
END trans_graph;

PROCEDURE comp?(VAL row1,row2: IROW): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(row1) DO
       IF row1[i]#row2[i] THEN RETURN FALSE END;
  END;
  RETURN TRUE
END comp?;

PROCEDURE empty?(VAL row: IROW): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(row) DO
       IF row[i]#{} THEN RETURN FALSE END;
  END;
  RETURN TRUE
END empty?;

PROCEDURE move_impl(VAR set: IROW; VAR iset: IROW);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO items-1 DO
     IF ((i MOD b) IN set[i DIV b]) AND (project[i].kind=mimp) THEN
         INCL(iset[i DIV b],i MOD b);
         EXCL( set[i DIV b],i MOD b);
     END;
  END;
END move_impl;

PROCEDURE add_impl(VAL set: IROW; VAR iset: IROW);
  VAR no,i: INTEGER;
BEGIN
  FOR i:=0 TO items-1 DO
     IF (i MOD b) IN set[i DIV b] THEN
         WITH project[i] DO
            IF (kind=mdef) AND find_module(mimp,lname,no) THEN
                INCL(iset[no DIV b],no MOD b);
            END;
         END;
     END;
  END;
END add_impl;

PROCEDURE get_set(VAR set: IROW): BOOLEAN;
  VAR empty?: BOOLEAN;
  VAR i: INTEGER;
BEGIN empty?:= TRUE;
  FOR i:=0 TO items-1 DO WITH project[i] DO
       IF (selected? IN atr) AND (kind IN ModKinds{mdef,mimp}) THEN
           INCL(set[i DIV b],i MOD b); empty?:= FALSE;
       END;
  END END;
  RETURN empty?
END get_set;

PROCEDURE find_leaf(VAR set: IROW; VAL table: IGRAPH): INTEGER;
  PROCEDURE chk?(no: INTEGER):BOOLEAN;
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO HIGH(set) DO
         IF table[no,i]*set[i]#{} THEN RETURN FALSE END;
    END;
    RETURN TRUE
  END chk?;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO items-1 DO
     IF (i MOD b) IN set[i DIV b] THEN
          EXCL(set[i DIV b],i MOD b);
          IF chk?(i) THEN RETURN i ELSE INCL(set[i DIV b],i MOD b) END;
     END;
  END;
  RETURN -1
END find_leaf;

PROCEDURE put_task(no: INTEGER; VAR task: COMPTASK);
BEGIN
  task.list[task.cnt]:=no; INC(task.cnt);
  WITH project[no] DO INCL(atr,for_comp?); INCL(d_mode,d_line) END;
  show_item(no);
END put_task;

PROCEDURE order_defs(VAR set: IROW; VAR task: COMPTASK):BOOLEAN;
  VAR no: INTEGER;
BEGIN
  WHILE NOT empty?(set) DO
      no:= find_leaf(set,Graph);
      IF no= -1 THEN RETURN TRUE END;
      put_task(no,task);
  END;
  RETURN FALSE
END order_defs;

PROCEDURE list_imps(VAL set: IROW; VAR task: COMPTASK);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO items-1 DO
     IF (i MOD b) IN set[i DIV b] THEN put_task(i,task) END;
  END;
END list_imps;

PROCEDURE CompileTask(VAL task: COMPTASK); FORWARD;
PROCEDURE WriteCompileTask(VAL task: COMPTASK); FORWARD;

PROCEDURE off;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO items-1 DO WITH project[i] DO
      EXCL(atr,for_comp?);
      IF NOT (selected? IN atr) THEN EXCL(d_mode,d_line) END;
      show_item(i);
  END END;
END off;

PROCEDURE MakeRecompileTask(write: BOOLEAN);
  VAR task: COMPTASK; table: IGRAPH;
      init,imps,res: IROW;
  PROCEDURE exit;
  BEGIN del_row(init); del_row(imps); del_row(res); del_graph(table);
  END exit;
BEGIN
  high_info({d_norm},FALSE,FALSE,'Recompiling...');
  task.cnt:= 0;
  mk_row(init); mk_row(res); mk_row(imps); mk_graph(table);
  IF get_set(init) THEN
      high_info({d_inten},TRUE,TRUE,'No modules selected!');
      exit; RETURN
  END;
  IF HIGH(Graph)<0 THEN
      IF MakeGraph() THEN
           high_info({d_norm},FALSE,FALSE,'Recompiling...');
      ELSE high_info({d_inten},TRUE,TRUE,'Recompilation aborted: no import graph');
           exit; RETURN
      END;
  END;
  move_impl(init,imps);
  trans_graph(Graph,table,TRUE);
  LOOP
       make_sum(init,table,res);
       move_impl(res,imps);
       add_impl(res,imps);
       IF comp?(init,res) THEN EXIT END;
       init:=res;
  END;
  IF order_defs(init,task) THEN
      high_info({d_inten},TRUE,TRUE,'Def-modules import cycle detected!');
      exit; off; RETURN
  END;
  list_imps(imps,task); exit;
  IF write THEN
    WriteCompileTask(task);
  ELSE
    CompileTask(task);
  END;
END MakeRecompileTask;

PROCEDURE MakeGenerateTask(write: BOOLEAN);
  VAR task: COMPTASK;
      init,imps,res: IROW;
  PROCEDURE exit;
  BEGIN del_row(init); del_row(imps); del_row(res);
  END exit;
BEGIN
  high_info({d_norm},FALSE,FALSE,'Generating...');
  task.cnt:= 0;
  mk_row(init); mk_row(res); mk_row(imps);
  IF get_set(init) THEN
      high_info({d_inten},TRUE,TRUE,'No modules selected!');
      exit; RETURN
  END;
  IF HIGH(Graph)<0 THEN
      IF MakeGraph() THEN
           high_info({d_norm},FALSE,FALSE,'Generating...');
      ELSE high_info({d_inten},TRUE,TRUE,'Generation aborted: no import graph');
           exit; RETURN
      END;
  END;
  LOOP
       make_sum(init,Graph,res); add_row(init,res);
       imps:=res; add_impl(res,imps); res:=imps;
       IF comp?(init,res) THEN EXIT END;
       init:=res;
  END;
  del_row(imps); mk_row(imps);
  move_impl(res,imps);
  IF order_defs(res,task) THEN
      high_info({d_inten},TRUE,TRUE,'Def-modules import cycle detected!');
      exit; off; RETURN
  END;
  list_imps(imps,task); exit;
  IF write THEN
    WriteCompileTask(task);
  ELSE
    CompileTask(task);
  END;
END MakeGenerateTask;

PROCEDURE change_graph(VAL reord: COMPTASK);
  VAR new: IGRAPH; i1,j1,i,j: INTEGER;
BEGIN
  mk_graph(new);
  FOR i:=0 TO items-1 DO
      FOR j:=0 TO items-1 DO
          IF (j MOD b) IN Graph[i,j DIV b] THEN
              i1:= reord.list[i]; j1:= reord.list[j];
              INCL(new[i1,j1 DIV b],j1 MOD b);
          END;
      END;
  END;
  copy_graph(new,Graph);
  del_graph(new);
END change_graph;

PROCEDURE MakeSorting;
  VAR task,reord: COMPTASK; new: System;
      init: IROW; bnd,no,i: INTEGER;
  PROCEDURE exit; BEGIN del_row(init) END exit;
  PROCEDURE h; BEGIN high_info({d_norm},FALSE,FALSE,'Ordering project...') END h;
  PROCEDURE put(no: INTEGER);
  BEGIN reord.list[no]:= bnd; new[bnd]:= project[no]; INC(bnd);
        INCL(project[no].atr,sorted?);
  END put;
BEGIN h;
  task.cnt:=0; bnd:=0; mk_row(init);
  IF HIGH(Graph)<0 THEN
      IF MakeGraph() THEN h;
      ELSE high_info({d_inten},TRUE,TRUE,'Ordering aborted: no import graph');
           exit; RETURN
      END;
  END;
  FOR i:=0 TO items-1 DO
     IF project[i].kind=mdef THEN INCL(init[i DIV b],i MOD b) END;
  END;
  IF order_defs(init,task) THEN
      high_info({d_inten},TRUE,TRUE,'Def-modules import cycle detected!');
      exit; off; RETURN
  END;
  exit; off;
  FOR i:=0 TO task.cnt-1 DO
     put(task.list[i]);
     WITH project[task.list[i]] DO
         IF find_module(mimp,lname,no) THEN put(no) ELSE warning(lname); h END;
     END;
  END;
  FOR i:=0 TO items-1 DO WITH project[i] DO
     IF (kind=mimp) AND NOT (sorted? IN atr) THEN put(i) END;
  END END;
  FOR i:=0 TO items-1 DO WITH project[i] DO
     IF NOT (sorted? IN atr) THEN put(i) END;
  END END;
  ASSERT(bnd=items);
  project:= new;
  clear_field; draw; clear_high;
  high_info({d_norm},FALSE,FALSE,'Generating new import graph...');
  change_graph(reord);
  clear_high; showDONE;
END MakeSorting;

PROCEDURE total;
BEGIN
  comp.TIME:=comp.TIME DIV 1000;
  IF comp.TIME<=0 THEN comp.TIME:=1 END;
  comp.IOTIME:=comp.IOTIME DIV 1000;
  IF comp.IOTIME<=0 THEN comp.IOTIME:=1 END;

  low_info({d_norm},'--------------------------------- TOTAL ---------------------------------\n'
    "files %d lines %d cpu%$2d:%$2d io%$2d:%$2d speed %d l/m code %d w",
     comp.files, comp.LINE,
     comp.TIME   DIV 60, comp.TIME   MOD 60,
     comp.IOTIME DIV 60, comp.IOTIME MOD 60,
     comp.LINE*60 DIV comp.TIME, comp.code);
END total;

PROCEDURE comp_abort;
  VAR i: INTEGER;
BEGIN
  clear_low; clear_high;
  FOR i:=0 TO items-1 DO WITH project[i] DO
      comp.del_list(msg_list);
      atr:= atr-{for_comp?,comp_ok?,comp_err?};
      d_mode:= d_mode-{d_inten,d_dim}+{d_norm};
      IF NOT (selected? IN atr) THEN EXCL(d_mode,d_line) END;
      show_item(i);
  END END;
END comp_abort;

PROCEDURE FinishCompileTask(fst_err: INTEGER); FORWARD;

VAR opts: BITSET;

PROCEDURE edit_opts;
  VAR _opts: BITSET;
  PROCEDURE pos(p: INTEGER); BEGIN tty.set_pos(Info1Line,8+p*2) END pos;
  PROCEDURE on(opt: INTEGER);
  BEGIN mode({d_rev}); tty.Write(CHAR(ORD('A')+opt)); norm;
        tty.Write(' '); pos(opt); INCL(_opts,opt);
  END on;
  PROCEDURE off(opt: INTEGER);
  BEGIN tty.print('%c ',CHAR(ORD('A')+opt)); pos(opt); EXCL(_opts,opt);
  END off;
  VAR i: INTEGER; k: CHAR;
BEGIN _opts:=opts;
  low_info({d_norm},'------------------- SET COMPILER OPTIONS -------------------');
  FOR i:=0 TO 25 DO pos(i); IF i IN _opts THEN on(i) ELSE off(i) END END;
  pos(0); i:=0; c_on;
  LOOP key.read(k);
       CASE k OF
        |key.right:      i:=(i+1)  MOD 26; pos(i);
        |key.left:       i:=(i+25) MOD 26; pos(i);
        |SPACE:          IF i IN _opts THEN off(i) ELSE on(i) END;
        |key.cr,key.lf:  opts:=_opts; EXIT
        |ASCII.ESC,key.break: EXIT
       ELSE bell END;
  END;
  c_off; clear_low;
END edit_opts;

PROCEDURE comp_result(VAL name: ARRAY OF CHAR);
BEGIN
  comp.time:=comp.time DIV 1000;
  IF comp.time<=0 THEN comp.time:=1 END;
  comp.iotime:=comp.iotime DIV 1000;
  IF comp.iotime<=0 THEN comp.iotime:=1 END;
  low_info({d_norm},"%s: lines %d time %$2dcp + %$2dio",
           name,comp.line,comp.time,comp.iotime);
END comp_result;

PROCEDURE WriteCompileTask(VAL task: COMPTASK);
  VAR i: INTEGER; f: bio.FILE; def: BOOLEAN;
      s: STRING; no: INTEGER;
BEGIN
  high_info({d_norm},FALSE,FALSE,'Preparing file "pmcomp.@"...');
  bio.create(f,'pmcomp.@','w',0);
  bio.print(f,'mx');
  def:=FALSE;
  FOR i:=0 TO task.cnt-1 DO
     no:=task.list[i];
     pick_item(no);
     WITH project[no] DO
       IF kind=mdef THEN
         def:=TRUE;
       ELSIF def THEN
         def:=FALSE;
         bio.print(f,'\nmx');
       END;
       bio.print(f,' %s',path);
       d_mode:= d_mode-{d_inten,d_dim}+{d_norm};
       IF NOT (selected? IN atr) THEN EXCL(d_mode,d_line) END;
     END;
     show_item(no);
  END;
  bio.print(f,'\n');
  bio.close(f);
  high_info({d_norm},TRUE,TRUE,'File "pmcomp.@" prepared');
END WriteCompileTask;

PROCEDURE CompileTask(VAL task: COMPTASK);
  VAR no: INTEGER; flag: BOOLEAN; k: CHAR;
  PROCEDURE taskOK?(VAR fst_err:INTEGER):BOOLEAN;
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO items-1 DO WITH project[i] DO
       IF comp_err? IN atr THEN ASSERT(kind=mimp);
           fst_err:= i; RETURN FALSE
       END;
    END END;
    fst_err:= -1; RETURN TRUE
  END taskOK?;
  VAR i: INTEGER;
BEGIN
  low_info({d_norm},'PRESS ANY KEY TO START OR ESC TO ABORT TASK');
  key.read(k); clear_low;
  IF (k=ASCII.ESC) OR (k=key.break) THEN clear_high; off; RETURN END;
  comp.init_compiler(opts);
  FOR i:=0 TO task.cnt-1 DO
     no:= task.list[i];
     LOOP
         pick_item(no);
         WITH project[no] DO comp.del_list(msg_list);
             comp.compile(path,msg_list);
         END;
         IF comp.OK? THEN
              comp_result(project[no].name);
              WITH project[no] DO
                    atr:= atr-{for_comp?,comp_err?}+{comp_ok?};
                    d_mode:= d_mode-{d_norm,d_inten}+{d_dim};
              END;
              show_item(no); EXIT
         ELSE low_info({d_inten},"%s: errors detected",project[no].path);
              WITH project[no] DO
                  atr:= atr-{for_comp?}+{comp_err?};
                  d_mode:= d_mode-{d_norm,d_dim}+{d_inten};
                  show_item(no);
                  IF kind=mimp THEN EXIT END;
                  msg2(0,' Edit module/Abort compilation (e/ESC)?');
                  CASE GetKey('e','E',ASCII.ESC) OF
                   |'e','E':  flag:= TRUE;
                              WHILE flag DO
                                Edit(no);
                                high_info({d_norm},FALSE,FALSE,'Recompiling...');
                                msg2(0,' Edit again/Continue/Compile&Continue/Abort compilation (e/c/m/ESC)?');
                                CASE GetKey('e','E','c','C','m','M',ASCII.ESC) OF
                                 |'e','E'  : (* Nothing *)
                                 |'c','C'  : atr:= atr-{for_comp?,comp_err?}+{comp_ok?};
                                             d_mode:= d_mode-{d_norm,d_inten}+{d_dim};
                                             show_item(no);
                                             clear_low; EXIT
                                 |'m','M'  : clear_low; flag:= FALSE;
                                 |ASCII.ESC: comp_abort; RETURN
                                ELSE ASSERT(FALSE) END;
                              END;
                   |ASCII.ESC:
                              comp_abort; RETURN
                  ELSE ASSERT(FALSE) END;
              END;
         END;
     END;
  END;
  clear_low; total;
  IF taskOK?(no) THEN
      high_info({d_norm},TRUE,TRUE,'Compilation completed');
      comp_abort; RETURN
  END;
  FinishCompileTask(no);
  comp_abort;
END CompileTask;

PROCEDURE FinishCompileTask(fst_err: INTEGER);
  VAR curr: INTEGER; k: CHAR;
  PROCEDURE p; BEGIN pick_item(curr) END p;
  PROCEDURE s; BEGIN show_item(curr) END s;
  PROCEDURE l;
  BEGIN low_info({d_norm},' Edit/Compile/Mark/Unmark/Abort (e/m/+/-/ESC)?') END l;
  PROCEDURE h;
  BEGIN high_info({d_norm},FALSE,FALSE,'Please correct errors...') END h;
  PROCEDURE compile;
  BEGIN
    WITH project[curr] DO
        IF atr*{comp_err?,comp_ok?}={} THEN bell; RETURN END;
        high_info({d_norm},FALSE,FALSE,'Compiling...'); clear_low;
        comp.del_list(msg_list);
        comp.compile(name,msg_list);
        IF comp.OK? THEN
             comp_result(name);
             atr:= atr-{for_comp?,comp_err?}+{comp_ok?};
             d_mode:= d_mode-{d_norm,d_inten}+{d_dim};
             high_info({d_norm},TRUE,TRUE,'Compiled OK');
        ELSE low_info({d_inten},"%s: errors detected",name);
             atr:= atr-{for_comp?,comp_ok?}+{comp_err?};
             d_mode:= d_mode-{d_norm,d_dim}+{d_inten};
             high_info({d_inten},TRUE,TRUE,'Compilation failed');
        END;
    END;
  END compile;
BEGIN
  high_info({d_inten},TRUE,TRUE,'Compilation errors detected...');
  clear_low;
  h; curr:= fst_err; p; l;
  LOOP k:= NextKey(curr);
       CASE k OF
        |'e','E':   Edit(curr); h; l; p;
        |'m','M':   compile; clear_low; h; l; p;
        |'-':       WITH project[curr] DO
                       IF atr*{comp_err?,comp_ok?}#{} THEN
                           comp.del_list(msg_list);
                           atr:= atr-{comp_err?}+{comp_ok?};
                           d_mode:= d_mode-{d_inten}+{d_dim};
                       ELSE bell END;
                    END; p;
        |'+':       WITH project[curr] DO
                       IF atr*{comp_err?,comp_ok?}#{} THEN
                           atr:= atr-{comp_ok?}+{comp_err?};
                           d_mode:= d_mode-{d_dim}+{d_inten};
                       ELSE bell END;
                    END; p;
        |ASCII.ESC: RETURN
       ELSE bell END;
  END;
END FinishCompileTask;

(***********************************************************************
--------------------------- Commands -----------------------------------
                            --------
(*
   -- Параметры --
   $F -- имя текущего файла
   $N -- языковое имя текущего модуля
   $P -- текущий набор образцов
   $[string] -- часть строки вводится интерактивно
             -- с выдачей prompt'а string
   -- Команды --
   #C -- очистить экран перед запуском задачи
   #R -- перерисовать картинку после окончания задачи
   #D -- прочесть директорию после окончания задачи
   #W -- возврат в pm по нажатию любой клавиши
*)

PROCEDURE ReadDir; FORWARD;

VAR com_desc: sle.descriptor;
VAR clear_screen?, refresh?, read_dir?, wait?: BOOLEAN;

PROCEDURE commands(VAR cmd: ARRAY OF CHAR);
BEGIN
  clear_screen?:= FALSE;
  refresh?:= FALSE;
  read_dir?:= FALSE;
  wait?:= FALSE;
  LOOP WHILE cmd[0]=' ' DO str.delete(cmd,0,1) END;
       IF cmd[0] # '#' THEN RETURN END;
       CASE cmd[1] OF
        |'C':   clear_screen?:= TRUE; str.delete(cmd,0,2);
        |'R':   refresh?:= TRUE;      str.delete(cmd,0,2);
        |'D':   read_dir?:= TRUE;     str.delete(cmd,0,2);
        |'W':   wait?:= TRUE;         str.delete(cmd,0,2);
       ELSE RETURN END;
  END;
END commands;

PROCEDURE interpret(curr: INTEGER; VAL com: ARRAY OF CHAR);
  VAR cmd,res: ARRAY [0..255] OF CHAR;
      prompt: ARRAY [0..79] OF CHAR; cmd1: ARRAY [0..127] OF CHAR;
      i,n: INTEGER; x: CHAR;
BEGIN
  str.print(cmd,'%s',com); str.print(res,'%s','');
  commands(cmd);
  i:=0;
  LOOP
      CASE cmd[i] OF
       |'$': CASE cmd[i+1] OF
              |'F':  str.append(res,'%s',project[curr].path);  INC(i,2);
              |'N':  str.append(res,'%s',project[curr].lname); INC(i,2);
              |'P':  INC(i,2);
                     FOR n:=0 TO HIGH(args.words) DO
                       str.append(res,'%s ',arg.words[n]);
                     END;
              |'[':  INC(i,2); str.print(prompt,'%s','');
                     LOOP
                       IF (cmd[i]=']') OR (cmd[i]=0c) THEN EXIT END;
                       IF (cmd[i]='\') AND (cmd[i+1]#0c) THEN
                            str.append(prompt,'%c',cmd[i+1]); INC(i,2);
                       ELSE str.append(prompt,'%c',cmd[i]);   INC(i);
                       END;
                     END;
                     IF cmd[i]=']' THEN INC(i) END;
                     clear_low; c_on;
                     com_desc^.how:=sle.empty;
                     sle.edit_str(prompt,cmd1,InfoLine,0,78,
                                              com_desc,ASCII.ESC);
                     c_off; clear_low;
                     IF com_desc^.last=ASCII.ESC THEN RETURN END;
                     str.append(res,'%s',cmd1);
             ELSE str.append(res,'%c',cmd[i]); INC(i) END;
       |'\': IF cmd[i+1]=0c THEN str.append(res,'%c',cmd[i]); EXIT
             ELSE str.append(res,'%c',cmd[i+1]); INC(i,2) END;
       |0c:  EXIT
      ELSE   str.append(res,'%c',cmd[i]); INC(i) END;
  END;
  IF clear_screen? THEN tty.home; tty.erase(2) END;
  c_on;
  shell.print:=tty.print;
  shell.system(res,TRUE);
  c_off;
  IF wait? THEN tty.print('\nPRESS ANY KEY TO RETURN TO "PM"...');
      key.read(x)
  END;
  IF refresh? THEN start; draw END;
  IF read_dir? THEN ReadDir END;
END interpret;

TYPE  COM_STRING= ARRAY [0..127] OF CHAR;
     pCOM_STRING= POINTER TO COM_STRING;

VAR com_table: ARRAY [0c..377c] OF pCOM_STRING;

VAR cmd: COM_STRING;

PROCEDURE init_commands;
  VAR c: CHAR;
BEGIN
  FOR c:=0c TO 377c DO com_table[c]:=NIL END;
END init_commands;

PROCEDURE edit_com(com: CHAR);
  VAR prompt: ARRAY [0..15] OF CHAR;
BEGIN
  msg2(0,''); str.print(prompt,'%c ::= ',com);
  IF com_table[com]#NIL THEN str.print(cmd,'%s',com_table[com]^)
                        ELSE str.print(cmd,'%s','')
  END;
  com_desc^.how:=sle.show;
  c_on; sle.edit_str(prompt,cmd,Info1Line,0,75,com_desc,ASCII.ESC); c_off;
  IF com_desc^.last=ASCII.ESC THEN clear_high;
      show_blink('Definition aborted'); RETURN
  END;
  IF com_table[com]=NIL THEN NEW(com_table[com]) END;
  str.print(com_table[com]^,'%s',cmd);
END edit_com;

PROCEDURE DefineCommand;
  VAR k: CHAR;
  PROCEDURE valid?(k: CHAR): BOOLEAN;
    VAR sort: BITSET;
  BEGIN sort:=ASCII.KIND(k);
        RETURN sort*{ASCII.cyril,ASCII.latin,ASCII.digit,ASCII.special}#{}
  END valid?;
BEGIN clear_low; clear_high;
  high_info({d_norm},FALSE,FALSE,'Defining shell command...');
  msg1(0,'------------------------------------------------------------------------');
  msg2(0,'ENTER KEY TO DEFINE (ESC TO ABORT)==>');
  key.read(k);
  IF k=ASCII.ESC THEN clear_low; clear_high; RETURN END;
  IF NOT valid?(k) THEN clear_high; show_blink('Invalid key'); RETURN END;
  edit_com(k);
  clear_low; clear_high;
END DefineCommand;
------------------------------------------------------------------------
***********************************************************************)

VAR curr_edit: INTEGER;

PROCEDURE put_msg;
  VAR msg: pMSG;
BEGIN
  msg:= project[curr_edit].msg_list;
  WHILE msg#NIL DO
      editor.mark(msg^.line,msg^.col,msg^.msg);
      msg:= msg^.next;
  END;
END put_msg;

PROCEDURE Edit(no: INTEGER);
  VAR cmd: ARRAY [0..127] OF CHAR;
BEGIN
  high_info({d_norm},FALSE,FALSE,'Editing...');
  curr_edit:= no; editor.start:= put_msg;
  str.print(cmd,'{+myEditor}ex %s',project[no].path);
  shell.print:=err_print;
  shell.system(cmd,{});
  comp.del_list(project[no].msg_list);
  start; draw;
END Edit;

VAR sh_desc: sle.descriptor;

PROCEDURE Shell;
  VAR pmt: ARRAY [0..47] OF CHAR;
      pro: ARRAY [0..47] OF CHAR;
      command: ARRAY [0..255] OF CHAR;
      l,c: INTEGER; xxx: BOOLEAN;
      echo: BITSET;
BEGIN
  l:=tty.state^.lines-1; c:=tty.state^.columns-2;
  LOOP
    shell.get_prompt(pmt,xxx,xxx);
    str.print(pro,'PM >>> %s',pmt);
    sle.edit_str(pro,command,l,0,c,sh_desc,ASCII.ESC);
    tty.print("\n");
    IF sh_desc^.last=ASCII.ESC THEN EXIT END;
    IF command#"" THEN
      shell.print:=tty.print;
      shell.get_echo(echo);
      shell.system(command,echo)
    END;
  END;
END Shell;

PROCEDURE unselect;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO items-1 DO WITH project[i] DO
      IF selected? IN atr THEN
          EXCL(atr,selected?); EXCL(d_mode,d_line);
          show_item(i);
      END;
  END END;
END unselect;

PROCEDURE ReadDir;
BEGIN
  clear_field; collect; draw;
  IF LEN(Graph)>0 THEN del_graph(Graph) END;
END ReadDir;

PROCEDURE help;
  VAR c: CHAR;
BEGIN
  tty.home; tty.erase(0);
  tty.print('\n');
  tty.print('   e       - edit module\n');
  tty.print('   r       - read directory\n');
  tty.print('   q       - quit PM\n');
  tty.print('  CR,SPACE - select/unselect module\n');
  tty.print('  ESC      - unselect all\n');
  tty.print('\n');
  tty.print('   F2 r    - read  links file\n');
  tty.print('   F2 w    - write links file\n');
  tty.print('   F2 g    - make  links graph\n');
  tty.print('\n');
  tty.print('   F0 r    - recompile selected task\n');
  tty.print('   F0 g    - generate  selected task\n');
  tty.print('   F0 s    - sorting according to links graph\n');
  tty.print('   F0 o    - set compiler options\n');
  tty.print('\n');
  tty.print('   F0 PgDw - shell (ESC - return to PM)\n');
  tty.print('\n');
  tty.print('   F5 r    - write batch file for recompilation\n');
  tty.print('   F5 g    - write batch file for generation\n');
  tty.print('\n');
  tty.print('Small and capital letters are equal\n');
  tty.print('\n');
  tty.print('PRESS ANY KEY...');
  key.read(c);
  start; draw;
END help;

PROCEDURE monitor;
  VAR k,k1: CHAR; curr: INTEGER;
  PROCEDURE s; BEGIN show_item(curr) END s;
  PROCEDURE p; BEGIN pick_item(curr) END p;
BEGIN
  curr:= 0; p;
  LOOP k:= NextKey(curr);
       CASE k OF
        |'e','E':   Edit(curr); p;
        |key.f5 :
                    key.read(k1);
                    CASE k1 OF
                     |'r','R':  s; MakeRecompileTask(TRUE);     p;
                     |'g','G':  s; MakeGenerateTask(TRUE);      p;
                    ELSE (* Nothing *) END;
        |SILVER:
                    key.read(k1);
                    CASE k1 OF
                     |key.pgdw: c_on; Shell; c_off; start; draw; p;
                     |'r','R':  s; MakeRecompileTask(FALSE);    p;
                     |'g','G':  s; MakeGenerateTask(FALSE);     p;
                     |'s','S':  s; MakeSorting; curr:=0; p;
                     |'o','O':  edit_opts;
                    ELSE (* Nothing *) END;
        |BRONZE:
                    key.read(k1);
                    CASE k1 OF
                     |'g','G':   s; IF MakeGraph() THEN END; p;
                     |'w','W':   WriteGraph; p;
                     |'r','R':   ReadGraph(TRUE); p;
                    ELSE (* Nothing *) END;
        |GOLD:      help;
                  (*
                    key.read(k1);
                    IF k1=GOLD THEN DefineCommand
                    ELSE IF com_table[k1]#NIL THEN
                              interpret(curr,com_table[k1]^);
                              IF curr>=items THEN curr:= items-1 END; p;
                         ELSE bell END;
                    END;
                  *)
        |key.cr,key.lf,SPACE:
                    WITH project[curr] DO
                        atr:= atr/{selected?}; d_mode:= d_mode/{d_line};
                    END;
                    p;
        |'r','R':   ReadDir; curr:=0; p;
        |ASCII.ESC: unselect;
        |'q','Q':   HALT
        |key.break: HALT(50h)
        |ASCII.BEL: bell?:= NOT bell?;
                    IF bell? THEN show_blink('BELL ON')
                             ELSE show_blink('BELL OFF')
                    END;
       ELSE bell END;
  END;
END monitor;

PROCEDURE pusage;
BEGIN
  std.print(
      '  "pm"  project manager utility program (c) KRONOS\n'
      'usage:\n'
      '   pm [+E] [-h] {file_tree}\n'
      '                                   Andy, 16-Nov-90\n');
END pusage;

PROCEDURE Version;
BEGIN
  std.print("Project Manager v2.3.4 16-Nov-90 Andy (c) KRONOS\n");
END Version;

PROCEDURE finish;
BEGIN
  sle.dispose(gn_desc);
(*  sle.dispose(com_desc); *)
  sle.dispose(sh_desc);
  norm;
  tty.set_cursor(1);
  tty.set_pos(24,0);
END finish;

PROCEDURE set_opts;
  VAR x: STRING; i: INTEGER; c,s: CHAR;
BEGIN
  opts:=mx.opts;
  IF arg.string('mxFLAGS',x) THEN
    i:=0;
    LOOP
      IF (i>=HIGH(x)) OR (x[i]=0c) THEN EXIT END;
      c:=CAP(x[i]); INC(i);
      s:=x[i];      INC(i);
      IF (c<'A') OR (c>'Z') THEN EXIT END;
      IF    s='+' THEN INCL(opts,ORD(c)-ORD('A'));
      ELSIF s='-' THEN EXCL(opts,ORD(c)-ORD('A'))
      ELSE EXIT
      END;
    END;
  END;
END set_opts;

VAR s: STRING;

BEGIN env.final(finish);
  IF arg.flag('-','#') THEN Version; HALT END;
  IF arg.flag('-','h') THEN pusage;  HALT END;

  tty.set_color(-1);
  has_low?:=tty.done;
  tty.reset;

  bell?:= TRUE;
  NEW(Graph);
  GraphName:='pm.links';
  gs:=bio.null;
  sle.new(gn_desc,  8);
(*  sle.new(com_desc, 8); *)
  sle.new(sh_desc, 16);
  set_opts;
(*  init_commands; *)

  start;
  collect;
  IF arg.flag('+','E') THEN
      IF arg.string("links",s) THEN str.copy(GraphName,s) END;
      ReadGraph(FALSE);
      MakeSorting;
  END;
  draw;
  monitor;
END pm.
