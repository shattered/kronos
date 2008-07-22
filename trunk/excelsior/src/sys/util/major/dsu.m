MODULE dsu; (* Leo 18-May-90. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT       Heap;
IMPORT  cdf: defCode;
IMPORT  cod: defCodes;
IMPORT  low: lowLevel;

IMPORT  bio: BIO;
IMPORT  tim: Time;
IMPORT  std: StdIO;
IMPORT  env: tskEnv;
IMPORT  arg: tskArgs;
IMPORT  str: Strings;
IMPORT  tty: Terminal;
IMPORT  key: Keyboard;
IMPORT  err: defErrors;
IMPORT  req: defRequest;
IMPORT  sta: Statistics;

WITH STORAGE: Heap; (* be sure that no_memory cause HALT! *)


VAR    F: bio.FILE;        name: ARRAY [0..63] OF CHAR;
     dev: bio.FILE;         spc: req.REQUEST;
  spec_h: BOOLEAN;        tty_h: BOOLEAN;
  disk_h: BOOLEAN;      undef_h: BOOLEAN;

PROCEDURE fs_stat;
  VAR i: INTEGER;
    d,c: INTEGER;
BEGIN
  sta.get(sta.fs_chsize,i);
  std.print("\nCASH %dKB\n",i DIV 256);
  sta.get(sta.fs_dkwrite,i);
  std.print("write: %6d sectors\n",i);
  sta.get(sta.fs_dkread,d);
  sta.get(sta.fs_chread,c);
  std.print("read from:\n");
  i:=d*100 DIV (d+c);
  std.print("disk : %6d sectors (%2d%%)\n",d,i);
  std.print("cash : %6d sectors (%2d%%)\n",c,100-i);
  std.print("\n");
END fs_stat;

PROCEDURE bio_error(op: ARRAY OF CHAR);
BEGIN
  tty.perror(bio.error,'%s("%s") %%s\n',op,name)
END bio_error;

PROCEDURE show_head(kind: BITSET);

  PROCEDURE p(VAR hd: BOOLEAN; VAL h: ARRAY OF CHAR);
  BEGIN
    IF hd THEN RETURN END; hd:=TRUE;
    tty.set_underline(1); std.print(h); tty.set_underline(0)
  END p;

BEGIN
  IF    kind*bio.is_disk#{} THEN
    p(disk_h, "\nDISK          cyls hd min max  ss  rs   secs rt prec\n")
  ELSIF kind*bio.is_tty#{}  THEN
    p(tty_h,  "\nSERIAL        baud xon  xoff  on off  par stops     \n")
  ELSIF kind*bio.is_spec#{} THEN
    p(spec_h, "\nSPECIAL\n")
  ELSIF kind={}             THEN
    p(undef_h,"\nUNDEF\n")
  END
END show_head;

PROCEDURE show_dev(d: bio.FILE);
  VAR r: req.REQUEST;
BEGIN
  WITH r DO
    show_head(bio.kind(d));
    IF bio.kind(d)*bio.is_spec#{} THEN std.print('%-12s\n',name); RETURN END;
    low.zero(r);
    r.op:=req.GET_SPEC;
    bio.doio(d,r);
    IF NOT bio.done THEN bio_error('get_spec'); RETURN END;
    IF bio.kind(d)*bio.is_disk#{} THEN
      std.print('%-12s %5d %2d %2d  %2d %4d %3d %6d %2d %4d',name
                ,cyls,heads,minsec,maxsec,secsize,ressec,dsecs,rate,precomp);
      std.print("  {");
      IF req.ready  *dmode={} THEN std.print(" nrdy")  END;
      IF req.wint   *dmode#{} THEN std.print(" hrd")  END;
      IF req.floppy *dmode#{} THEN std.print(" flp")  END;
      IF req.wpro   *dmode#{} THEN std.print(" wpro") END;
      IF req.fmttrk *dmode#{} THEN std.print(" fmtT") END;
      IF req.fmtsec *dmode#{} THEN std.print(" fmtS") END;
      IF req.fmtunit*dmode#{} THEN std.print(" fmtU") END;
      std.print(" }\n")
    ELSIF bio.kind(d)*bio.is_tty#{} THEN
      std.print('%-12s %5d %03bc %03bc %3d %3d'
                ,name,baud,xon,xoff,limxon,limxoff);
      IF (req.parNO+req.parODD+req.parEVEN)*smode#{} THEN
        IF req.parNO  *smode#{} THEN std.print("  no  ")  END;
        IF req.parODD *smode#{} THEN std.print("  odd ")  END;
        IF req.parEVEN*smode#{} THEN std.print(" even ")  END
      ELSE
        std.print("   ?  ")
      END;
      IF (req.stops1+req.stops2+req.stops1_5)*smode#{} THEN
        IF req.stops1  *smode#{} THEN std.print("  1  ")  END;
        IF req.stops2  *smode#{} THEN std.print("  2  ")  END;
        IF req.stops1_5*smode#{} THEN std.print(" 1.5 ")  END;
      ELSE
        std.print("  ?  ")
      END;
      IF (req.raw+req.sync)*smode#{} THEN
        std.print("     {");
        IF req.raw *smode#{} THEN std.print(" raw")   END;
        IF req.sync*smode#{} THEN std.print(" sync")  END;
        std.print(" }")
      END;
      std.print("\n")
    END
  END
END show_dev;

PROCEDURE list;

  PROCEDURE entry(kind: BITSET);
    VAR d: bio.FILE;
  BEGIN
    bio.fopen((dev),d,name,'');
    IF NOT bio.done THEN
      IF bio.error#err.no_entry THEN bio_error('open'); RETURN END;
      IF kind={} THEN show_head({}); std.print('%-8s\n',name) END
    ELSIF kind*bio.kind(d)#{} THEN
      show_dev(d); bio.close(d)
    END
  END entry;

  PROCEDURE _list(dir: bio.FILE; kind: BITSET);
    VAR mode: BITSET;
  BEGIN
    bio.dir_walk(dir,0);
    WHILE bio.get_entry(dir,name,mode) DO
      IF mode*bio.e_esc#{} THEN entry(kind) END
    END;
    bio.end_walk(dir)
  END _list;

BEGIN
  _list(bio.cd,{});
  IF NOT bio.equal(dev,bio.cd) THEN _list(dev,{})          END;
  _list(bio.cd,bio.is_spec);
  IF NOT bio.equal(dev,bio.cd) THEN _list(dev,bio.is_spec) END;
  _list(bio.cd,bio.is_tty);
  IF NOT bio.equal(dev,bio.cd) THEN _list(dev,bio.is_tty)  END;
  _list(bio.cd,bio.is_disk);
  IF NOT bio.equal(dev,bio.cd) THEN _list(dev,bio.is_disk) END
END list;

PROCEDURE open(VAL n: ARRAY OF CHAR);
  CONST DEV=bio.is_tty+bio.is_disk+bio.is_spec;
BEGIN
  str.copy(name,n);
  bio.open(F,name,'Xdc');
  IF bio.done & (bio.kind(F)*DEV#{}) THEN RETURN END;
  IF bio.done THEN bio.close(F) END;
  bio.fopen(dev,F,name,'X');
  IF NOT bio.done THEN bio_error('open'); HALT(bio.error) END;
  str.print(name,"/dev/%s",n)
END open;

PROCEDURE reopen;
BEGIN
  bio.close(F);
  IF NOT bio.done THEN bio_error('open'); HALT(bio.error) END;
  open(name)
END reopen;

PROCEDURE get_spec;
BEGIN
  low.zero(spc);
  spc.op:=req.GET_SPEC;
  bio.doio(F,spc);
  IF NOT bio.done THEN bio_error('get_spec'); HALT(bio.error) END;
  IF (bio.kind(F)*bio.is_disk={}) OR (spc.secsize=1<<spc.ssc) THEN RETURN END;
  tty.print("\nWARNING: ssc=%d secsize=%d (must be %d); corrected!\n",
                        spc.ssc,spc.secsize,1<<spc.ssc);
  spc.secsize:=1<<spc.ssc
END get_spec;

PROCEDURE lb(x: INTEGER): INTEGER;
  VAR i: INTEGER;
BEGIN
  i:=0;
  WHILE x>1 DO INC(i); x:=x DIV 2 END;
  RETURN i
END lb;

PROCEDURE set_spec;
BEGIN
  spc.ssc:=lb(spc.secsize);
  spc.op :=req.SET_SPEC;
  bio.doio(F,spc);
  IF NOT bio.done THEN bio_error('set_spec'); HALT(bio.error) END;
  reopen
END set_spec;

PROCEDURE sure(): BOOLEAN;
  VAR ch: CHAR;
BEGIN
  tty.print("are you sure?");
  key.read(ch);
  IF ORD(ch) MOD 128>=32 THEN tty.print("%c",ch) END;
  tty.print("\n");
  IF (ch=03c) OR (ch=33c) THEN HALT(50h) END;
  RETURN CAP(ch)='Y'
END sure;

VAR res_sectors: INTEGER;

PROCEDURE restore_res_sectors;
BEGIN
  IF (res_sectors<0) OR (F=bio.null) THEN RETURN END;
  low.zero(spc);
  spc.op:=req.GET_SPEC;
  bio.doio(F,spc);
  IF NOT bio.done THEN RETURN END;
  spc.ressec:=res_sectors;
  spc.op:=req.SET_SPEC;
  bio.doio(F,spc);
  IF bio.done THEN res_sectors:=-1 END
END restore_res_sectors;

PROCEDURE mark;
  VAR r: req.REQUEST;
     rs: INTEGER;
    ptr: SYSTEM.ADDRESS;
    buf: ARRAY [0..4095] OF CHAR;
    lab: ARRAY [0..15] OF CHAR;
BEGIN
  restore_res_sectors;
  get_spec;
  IF spc.ressec#0 THEN res_sectors:=spc.ressec; spc.ressec:=0; set_spec END;
  IF res_sectors>=0 THEN rs:=res_sectors ELSE rs:=0 END;
  bio.seek(F,0,0);
  IF NOT bio.done THEN bio_error('seek(0)'); HALT(bio.error) END;
  bio.get(F,buf,spc.secsize);
  IF NOT bio.done THEN bio_error('read_sector0'); HALT(bio.error) END;

  lab[ 0]:="X";
  lab[ 1]:="D";
  lab[ 2]:="0";
  lab[ 3]:=0c;
  lab[ 4]:=CHAR(spc.cyls MOD 256);
  lab[ 5]:=CHAR(spc.cyls DIV 256);
  lab[ 6]:=CHAR(spc.minsec);
  lab[ 7]:=CHAR(spc.maxsec);
  lab[ 8]:=CHAR(spc.heads);
  lab[ 9]:=CHAR(spc.ssc);
  lab[10]:=CHAR(rs);           (* NOTE: not ressec! *)
  lab[11]:=0c;
  ptr :=SYSTEM.ADR(lab)+3;
  ptr^:=tim.time();
  low.move(SYSTEM.ADR(buf)+2,SYSTEM.ADR(lab),SIZE(lab));

  bio.seek(F,0,0);
  IF NOT bio.done THEN bio_error('seek(0)'); HALT(bio.error) END;
  bio.put(F,buf,spc.secsize);
  IF NOT bio.done THEN bio_error('read_sector0'); HALT(bio.error) END;
  restore_res_sectors
END mark;

PROCEDURE verify;

  VAR r: req.REQUEST;
    i,l: INTEGER;
    buf: STRING;
    len: INTEGER;
    eof: INTEGER;
    prc: INTEGER;
   bump: ARRAY [0..15] OF CHAR;
   secs: INTEGER;
 le_pos: INTEGER; (* last error position *)
 errors: INTEGER;
retries: INTEGER;

  PROCEDURE perror;
    VAR h,t,s: INTEGER;
  BEGIN
    s:=i DIV spc.secsize;
    IF (spc.cyls<=0) OR (spc.heads<=0) OR (secs<=0) THEN
      std.perror(bio.error,'"%s" sector: %d  verify error: %%s\n',name,s)
    ELSE
      t:=s DIV secs;    h:=t MOD spc.heads;
      s:=s MOD secs;    t:=t DIV spc.heads;
      std.perror(bio.error,'"%s" t%04d h%d s%02d verify error: %%s\n'
                          ,name,t,h,s)
    END
  END perror;

BEGIN
  IF bio.kind(F)*bio.is_disk={} THEN
    tty.print('"%s" is not disk device\n',name); RETURN
  END;
  get_spec;
  IF NOT env.ipr() THEN
    tty.set_cursor(0);
    tty.print('verifying "%s"\n',name)
  END;
  secs:=spc.maxsec-spc.minsec+1;
  len :=secs*spc.secsize*spc.heads;
  IF (len>128*1024) OR (len<4*1024) THEN len:=64*1024 END;
  NEW(buf,len);
  errors:=0;         retries:=0;
  IF spc.ressec>0 THEN
    res_sectors:=spc.ressec; spc.ressec:=0; set_spec
  END;
  eof:=bio.eof(F);   le_pos:=-999999;
  i:=0; prc:=-1;
  LOOP
    IF ABS(i-le_pos)<len*2 THEN l:=spc.secsize ELSE l:=len END;
    IF i+l>eof THEN l:=eof-i END;
    IF l<=0    THEN EXIT END;
    bio.seek(F,i,0);
    IF NOT bio.done  THEN bio_error('seek') END;
    IF prc#i DIV (eof DIV 100) THEN
      prc:=i DIV (eof DIV 100);
      str.print(bump,"%2d%%",i DIV (eof DIV 100));
      env.put_str(env.info,bump,TRUE);
      IF NOT env.ipr() THEN tty.print("%s\r",bump) END;
    END;
    bio.get(F,buf,l);
    IF NOT bio.done THEN
      IF le_pos#i THEN le_pos:=i; l:=0; INC(retries) END;
      IF l=spc.secsize THEN perror; INC(errors) END
    END;
    i:=i+l
  END;
  IF (errors+retries>0) OR NOT env.ipr() THEN
    tty.print('verifying completed');
    IF    errors>0  THEN tty.print(' with %d errors',errors)
    ELSIF retries>0 THEN tty.print(' %d sectors read after retry',retries)
    END;
    tty.print('\n'); tty.set_cursor(1)
  END;
  restore_res_sectors
END verify;

PROCEDURE format;
   CONST fmt  = '"%s" t%04d.h%1d %|6s error: %%s\n';
   CONST fmts = '"%s" t%04d.h%1d.s%02d %|6s error: %%s\n';
   VAR  r: req.REQUEST;
        i: INTEGER;
      trk: INTEGER;
      sav: STRING;
     fbuf: STRING;
     secs: INTEGER;
     bump: ARRAY [0..15] OF CHAR;
     soft: BOOLEAN;
   errors: INTEGER;

  PROCEDURE final;
    VAR bads: DYNARR OF INTEGER;
  BEGIN
    IF (errors>0) OR NOT env.ipr() THEN
      tty.print('formatting completed');
      IF errors>0 THEN tty.print(' with %d errors',errors) END;
      tty.print('\n');
    END;
    IF arg.flag('-','f') THEN mark END;
    restore_res_sectors;
    IF arg.flag('-','f') & NOT arg.flag('-','s') THEN
      NEW(bads,0);
      bio.mkfs(F,1,4096,"UL",bads);
      IF NOT bio.done THEN bio_error("mkfs") END
    END;
    IF arg.flag('-','v') THEN verify END
  END final;

  PROCEDURE prepare;
  BEGIN
    r.ofs:=-1;    r.len:=0;
    r.pos:= 0;    r.buf:=NIL;
    r.op :=req.FORMAT;
    bio.doio(F,r);
    IF (r.res=err.not_enough) & (r.len>0) THEN
      NEW(fbuf,r.len);
      r.ofs:=-1;    r.len:=BYTES(fbuf);
      r.pos:= 0;    r.buf:=SYSTEM.ADR(fbuf);
      r.op :=req.FORMAT;
      bio.doio(F,r)
    ELSE
      NEW(fbuf,0)
    END;
    IF bio.done THEN RETURN END;
    std.perror(bio.error,'"%s" prepare error: %%s\n',name); HALT(bio.error)
  END prepare;

  PROCEDURE unit;
  BEGIN
    r.ofs:=0;    r.len:=BYTES(fbuf);
    r.pos:=0;    r.buf:=SYSTEM.ADR(fbuf);
    r.op :=req.FORMAT;
    bio.doio(F,r);
    IF NOT bio.done THEN INC(errors);
      std.perror(bio.error,fmt,name,0,0,'format'); HALT(bio.error)
    END;
    final;
  END unit;

  PROCEDURE read;
    VAR i: INTEGER; a: SYSTEM.ADDRESS;
  BEGIN
    bio.seek(F,trk*secs*spc.secsize,0);
    IF NOT bio.done THEN bio_error("seek"); HALT(bio.error) END;
    bio.get(F,sav,BYTES(sav));
    IF bio.done THEN RETURN END;
    (* per sector retry: *)
    bio.seek(F,trk*secs*spc.secsize,0);
    IF NOT bio.done THEN bio_error("seek"); HALT(bio.error) END;
    a:=SYSTEM.ADR(sav);
    FOR i:=spc.minsec TO spc.maxsec DO
      bio.read(F,a,spc.secsize);
      IF NOT bio.done THEN
        std.perror(bio.error,fmts,name,trk DIV spc.heads,trk MOD spc.heads,i,'read')
      END;
      INC(a,spc.secsize DIV 4)
    END
  END read;

  PROCEDURE write;
    VAR i: INTEGER; a: SYSTEM.ADDRESS;
  BEGIN
    bio.seek(F,trk*secs*spc.secsize,0);
    IF NOT bio.done THEN bio_error("seek"); HALT(bio.error) END;
    bio.put(F,sav,BYTES(sav));
    IF bio.done THEN RETURN END;
    (* per sector retry: *)
    bio.seek(F,trk*secs*spc.secsize,0);
    a:=SYSTEM.ADR(sav);
    FOR i:=spc.minsec TO spc.maxsec DO
      bio.write(F,a,spc.secsize);
      IF NOT bio.done THEN
        std.perror(bio.error,fmts,name,trk DIV spc.heads,trk MOD spc.heads,i,'write')
      END;
      INC(a,spc.secsize DIV 4)
    END
  END write;

  VAR fS,fT,fU: BOOLEAN; wdt: INTEGER;

BEGIN
  IF bio.kind(F)*bio.is_disk={} THEN
    tty.print('"%s" is not disk device\n',name); RETURN
  END;
  get_spec;   show_dev(F);
  secs:=spc.maxsec-spc.minsec+1;
  fS:=spc.dmode*req.fmtsec #{};
  fT:=spc.dmode*req.fmttrk #{};
  fU:=spc.dmode*req.fmtunit#{};
  IF NOT (fS OR fU OR fT) THEN
    tty.print('FORMAT not available for "%s"\n',name); RETURN
  END;
  IF ORD(fS)+ORD(fT)+ORD(fU)>1 THEN
    tty.print('too many format posibilities for "%s"\n',name); RETURN
  END;
  soft:=arg.flag('-','s');
  IF    fU THEN soft:=FALSE;
  ELSIF fT THEN soft:=soft & (secs*spc.secsize <= 128*1024);
  END;
  tty.set_color(+1);
  tty.WriteLn;
  IF    soft THEN tty.print('SOFT ')
  ELSIF arg.flag('-','s') THEN
    tty.print('WARNING: SOFT format denided! Whole unit format, only\n')
  END;
  tty.print('FORMAT "%s"; ',name);
  tty.set_color(0);
  IF NOT sure() THEN RETURN END;
  errors:=0;
  tty.print('formatting "%s"\n',name);
  tty.set_cursor(0);
  prepare;
  IF spc.ressec>0 THEN res_sectors:=spc.ressec; spc.ressec:=0; set_spec END;
  IF spc.dmode*(req.fmtsec+req.fmttrk+req.fmtunit)=req.fmtunit THEN
    unit; RETURN
  END;
  IF soft THEN NEW(sav,secs*spc.secsize) END;
  WITH spc DO
    trk:=cyls; wdt:=0;
    WHILE trk>0 DO INC(wdt); trk:=trk DIV 10 END;
    trk:=0;
    WHILE trk<cyls*heads DO
      IF (trk MOD heads = 0)  THEN
        str.print(bump,"t%$*d h0..%1d",wdt,trk DIV heads,heads-1);
        env.put_str(env.info,bump,TRUE);
        IF NOT env.ipr() THEN tty.print("%s\r",bump) END
      END;
      IF soft THEN read END;
      r.op :=req.FORMAT;
      r.pos:=0;          r.len:=BYTES(fbuf);
      r.ofs:=trk*secs;   r.buf:=SYSTEM.ADR(fbuf);
      bio.doio(F,r);
      IF NOT bio.done THEN INC(errors);
        std.perror(bio.error,fmt,name,trk DIV heads,trk MOD heads,'format')
      END;
      IF soft THEN write END;
      INC(trk)
    END
  END;
  final;
  IF NOT env.ipr() THEN tty.set_cursor(1) END
END format;

PROCEDURE clear;
   VAR  r: req.REQUEST;
        i: INTEGER;
      trk: INTEGER;
     time: INTEGER;
     secs: INTEGER;
     soft: BOOLEAN;
   errors: INTEGER;
BEGIN
  IF bio.kind(F)*bio.is_disk={} THEN
    tty.print('"%s" is not disk device\n',name); RETURN
  END;
  get_spec;
  secs:=spc.maxsec-spc.minsec+1;
  time:=tim.sys_time(tim.sec)+30;
  IF NOT env.ipr() THEN tty.set_cursor(0) END;
  REPEAT
    WITH spc DO
      trk:=0;
      WHILE trk<cyls*heads DO
        IF NOT env.ipr() THEN tty.print("%d  \r",trk DIV heads) END;
        r.op :=req.SEEK;
        r.pos:=0;          r.len:=0;
        r.ofs:=trk*secs;   r.buf:=NIL;
        bio.doio(F,r);
        INC(trk,heads)
      END
    END
  UNTIL time<tim.sys_time(tim.sec);
  IF NOT env.ipr() THEN tty.set_cursor(1) END
END clear;

PROCEDURE power_off;

  PROCEDURE quit; CODE cod.li0 cod.setm cod.idle END quit;

  VAR r: req.REQUEST;
      d: bio.FILE;
   mode: BITSET;
     no: INTEGER;
  files: ARRAY [0..63] OF bio.FILE;
  names: ARRAY [0..63] OF ARRAY [0..31] OF CHAR;

  PROCEDURE collect;
  BEGIN
    bio.fopen(d,files[no],name,'');
    IF bio.done THEN str.copy(names[no],name); INC(no) END
  END collect;

  PROCEDURE park_disks;
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO no-1 DO
      IF bio.kind(files[i])*bio.is_disk#{} THEN
        bio.doio(files[i],r);
        IF bio.done THEN std.print('device "%s" powered off\n',names[i])
        ELSE             std.perror(r.res,'"%s" %%s\n',names[i])
        END
      END
    END
  END park_disks;

  PROCEDURE park_others;
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO no-1 DO
      IF bio.kind(files[i])*bio.is_disk={} THEN bio.doio(files[i],r) END
    END
  END park_others;

BEGIN
  r.op:=req.POWER_OFF; r.buf:=NIL;
  r.ofs:=0;            r.len:=0;
  no:=0;
  bio.dir_walk(bio.cd,0);
  bio.dir_walk(dev,0);
  LOOP
    IF     bio.get_entry(bio.cd,name,mode) THEN d:=bio.cd
    ELSIF  bio.get_entry(dev,   name,mode) THEN d:=dev
    ELSE EXIT
    END;
    IF mode*bio.e_esc#{} THEN collect END
  END;
  bio.end_walk(dev);
  bio.end_walk(bio.cd);
  tim.delay(5,tim.sec);
  park_disks;
  park_others;
  tim.delay(1,tim.sec);
  quit
END power_off;

PROCEDURE dsk_spec;

  VAR t,s,ss,h,r,v,rt,pc,fs,ls: INTEGER;  something: BOOLEAN;

  PROCEDURE number(VAL s: ARRAY OF CHAR; VAR i: INTEGER);
  BEGIN
    IF arg.number(s,i) THEN something:=TRUE ELSE i:=-1 END
  END number;

BEGIN
  something:=FALSE;
  number('t'   ,t );
  number('s'   ,s );
  number('h'   ,h );
  number('v'   ,v );
  number('fs'  ,fs);
  number('ls'  ,ls);
  number('ss'  ,ss);
  number('rs'  ,r );
  number('rate',rt);
  number('prec',pc);
  IF NOT something THEN RETURN END;
  IF (ls>=0) & (fs>=0) THEN
    IF (s>=0) & (s#ls-fs+1) OR (ls<fs) THEN
      tty.print("illegal sectors specification\n"); HALT(err.bad_parm)
    END
  END;
  get_spec;
  show_dev(F);
  (* order of next statments is signiificant *)
  IF t>0   THEN spc.cyls   :=t              END;
  IF fs>=0 THEN spc.minsec :=fs             END;
  IF ls>=0 THEN spc.maxsec :=ls             END;
  IF s>0   THEN spc.maxsec :=spc.minsec+s-1 END;
  IF h>0   THEN spc.heads  :=h              END;
  IF ss>0  THEN spc.secsize:=ss             END;
  IF r>=0  THEN spc.ressec :=r              END;
  IF v>=0  THEN spc.dsecs  :=v              END;
  IF rt>0  THEN spc.rate   :=rt             END;
  IF pc>0  THEN spc.precomp:=pc             END;
  set_spec;
  show_dev(F);
  HALT
END dsk_spec;

PROCEDURE pusage;
BEGIN
  std.print(
      '  "dsu"  device setup utility program (c) KRONOS\n\n'
      '         setup device attributes at "." & "/dev" directories\n'
      'usage:\n'
      '   dsu [name] [-hltvpfFs] {change_spec_equation}\n');
  std.print(
      'WARNING: Be sure, that no other tasks are operating on device now!\n\n');
  std.print(
      '  -c   clear heads (for floppy drives only)\n'
      '  -l   device   list     -f   format and mark\n'
      '  -v   verify   disk     -sf  save     format\n'
      '  -t   fs statistics     -F   format     disk\n'
      '  -p   power     off     -m   mark       disk\n');
  std.print(
      '  s=sectors     "sectors per track"\n'
      '  fs=sector_no  "number of first sector"\n'
      '  ls=sector_no  "number of last  sector"\n'
      '  t=tracks      "tracks  on  disk"\n'
      '  h=heads       "heads"\n'
      '  v=volsize     "number of sectors on volume"\n');
  std.print(
      '  ss=secsize    "sector  size (bytes)"\n'
      '  rs=ressecs    "number of reserved sectors"\n'
      '  rate=rate     "heads stepping rate"\n'
      '  prec=track_no "track to start precompensation"\n');
  std.print(
      '                                   Leopold, Dec 07 90\n')
END pusage;

VAR i: INTEGER;
   bn: STRING;

BEGIN
  F:=bio.null;
  res_sectors:=-1;
  env.final(restore_res_sectors);
  IF arg.flag('-','h')    THEN pusage;    HALT END;
  tty_h :=FALSE;    disk_h :=FALSE;
  spec_h:=FALSE;    undef_h:=FALSE;
  bio.open(dev,"/dev",'rx');
  IF NOT bio.done THEN name:="/dev"; bio_error("open"); HALT(bio.error) END;
  IF arg.flag('-','t')    THEN fs_stat;   HALT END;
  IF arg.flag('-','l')    THEN list;      HALT END;
  IF arg.flag('-','p')    THEN power_off; HALT END;
  IF HIGH(arg.words)<0    THEN pusage;    HALT END;
  open(arg.words[0]);
  IF arg.flag('-','c')          THEN clear;    HALT END;
  IF arg.flag('-','f')          THEN format;   HALT END;
  IF arg.flag('-','F')          THEN format;   HALT END;
  IF arg.flag('-','m')          THEN mark;     HALT END;
  IF arg.flag('-','v')          THEN verify;   HALT END;
  IF bio.kind(F)*bio.is_disk#{} THEN dsk_spec       END;
  show_dev(F)
END dsu.
