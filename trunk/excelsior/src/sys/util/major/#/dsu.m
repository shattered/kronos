MODULE dsu; (* Leo 18-May-90. (c) KRONOS *)
            (* vik 24-Dec-90. (c) KRONOS ++ttyWindows*)

IMPORT       SYSTEM;            IMPORT       Heap;
IMPORT  cdf: defCode;           IMPORT  cod: defCodes;
IMPORT  low: lowLevel;

IMPORT  bio: BIO;               IMPORT  tim: Time;
IMPORT  env: tskEnv;            IMPORT  arg: tskArgs;
IMPORT  str: Strings;           IMPORT  tty: Terminal;
IMPORT  key: Keyboard;          IMPORT  err: defErrors;
IMPORT  req: defRequest;        IMPORT  sta: Statistics;
IMPORT  wnd: ttyWindows;        IMPORT  lex: Lexicon;
IMPORT menu: ttyMenus;          IMPORT ascii: ASCII;

WITH STORAGE: Heap; (* be sure that no_memory cause HALT! *)


VAR    F: bio.FILE;        name: ARRAY [0..63] OF CHAR;
     dev: bio.FILE;      s_name: ARRAY [0..63] OF CHAR;
     spc: req.REQUEST;     soft: BOOLEAN;
   tty_h: BOOLEAN;       spec_h: BOOLEAN;
  disk_h: BOOLEAN;      undef_h: BOOLEAN;

VAR  ERR: ARRAY [0..79] OF CHAR;
      ch: CHAR;
    show: BOOLEAN;
    done: BOOLEAN;
dev_list: ARRAY [0..255] OF STRING;

VAR  scr, vrf, fmt_w, stat, err_w, num_w: wnd.WINDOW;
     main,  m_dev, m_soft, m_disk,
     m_tty, m_flp, m_par,  m_stop: menu.MENU;

VAR t,s,ss,h,r,v,rt,pc,fs,ls: INTEGER;  something: BOOLEAN;
VAR PAR, stp: INTEGER;

CONST NOT_SPEC=bio.is_tty+bio.is_disk;

PROCEDURE show_error(err: ARRAY OF CHAR);
BEGIN
  IF wnd.closed(err_w) THEN wnd.open(err_w); wnd.ontop(err_w) END;
  wnd.print(err_w,err); done:=FALSE;
END show_error;

PROCEDURE fs_stat;
  VAR i: INTEGER;
    d,c: INTEGER;
BEGIN
  wnd.era(stat,2); wnd.open(stat);  wnd.ontop(stat); wnd.setpos(stat,0,0);
  sta.get(sta.fs_chsize,i);
  wnd.print(stat,"\nCASH %dKB\n",i DIV 256);
  sta.get(sta.fs_dkwrite,i);
  wnd.print(stat,"write: %6d sectors\n",i);
  sta.get(sta.fs_dkread,d);
  sta.get(sta.fs_chread,c);
  wnd.print(stat,"read from:\n");
  i:=d*100 DIV (d+c);
  wnd.print(stat,"disk : %6d sectors (%2d%%)\n",d,i);
  wnd.print(stat,"cash : %6d sectors (%2d%%)",c,100-i);
  IF show THEN key.read(ch); wnd.close(stat) END;
END fs_stat;

PROCEDURE bio_error(op: ARRAY OF CHAR);
BEGIN
  lex.perror(ERR,bio.error,'%s("%s") %%s\n',op,s_name);
  show_error(ERR);
END bio_error;

PROCEDURE false_h;
BEGIN tty_h:=FALSE; disk_h:=FALSE; spec_h:=FALSE; undef_h:=FALSE END false_h;

PROCEDURE show_head(kind: BITSET);

  PROCEDURE p(VAR hd: BOOLEAN; VAL h: ARRAY OF CHAR);
  BEGIN
    IF hd THEN RETURN END; hd:=TRUE;
    wnd.underline(scr,1); wnd.print(scr,h); wnd.underline(scr,0)
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
  IF NOT wnd.top(scr) THEN wnd.ontop(scr) END;
  WITH r DO
    show_head(bio.kind(d));
    IF bio.kind(d)*bio.is_spec#{} THEN
      wnd.print(scr,'%-12s\n',s_name); RETURN
    END;
    low.zero(r);
    r.op:=req.GET_SPEC;
    bio.doio(d,r);
    IF NOT bio.done THEN bio_error('get_spec'); RETURN END;
    IF bio.kind(d)*bio.is_disk#{} THEN
      wnd.print(scr,'%-12s %5d %2d %2d  %2d %4d %3d %6d %2d %4d',s_name
                    ,cyls,heads,minsec,maxsec,secsize,ressec,dsecs
                    ,rate,precomp);
      wnd.print(scr,"  {");
      IF req.ready  *dmode={} THEN wnd.print(scr," nrdy") END;
      IF req.wint   *dmode#{} THEN wnd.print(scr," hrd")  END;
      IF req.floppy *dmode#{} THEN wnd.print(scr," flp")  END;
      IF req.wpro   *dmode#{} THEN wnd.print(scr," wpro") END;
      IF req.fmttrk *dmode#{} THEN wnd.print(scr," fmtT") END;
      IF req.fmtsec *dmode#{} THEN wnd.print(scr," fmtS") END;
      IF req.fmtunit*dmode#{} THEN wnd.print(scr," fmtU") END;
      wnd.print(scr," }\n")
    ELSIF bio.kind(d)*bio.is_tty#{} THEN
      wnd.print(scr,'%-12s %5d %03bc %03bc %3d %3d'
                    ,s_name,baud,xon,xoff,limxon,limxoff);
      IF (req.parNO+req.parODD+req.parEVEN)*smode#{} THEN
        IF req.parNO  *smode#{} THEN wnd.print(scr,"  no  ")  END;
        IF req.parODD *smode#{} THEN wnd.print(scr,"  odd ")  END;
        IF req.parEVEN*smode#{} THEN wnd.print(scr," even ")  END
      ELSE
        wnd.print(scr,"   ?  ")
      END;
      IF (req.stops1+req.stops2+req.stops1_5)*smode#{} THEN
        IF req.stops1  *smode#{} THEN wnd.print(scr,"  1  ")  END;
        IF req.stops2  *smode#{} THEN wnd.print(scr,"  2  ")  END;
        IF req.stops1_5*smode#{} THEN wnd.print(scr," 1.5 ")  END;
      ELSE
        wnd.print(scr,"  ?  ")
      END;
      IF (req.raw+req.sync)*smode#{} THEN
        wnd.print(scr,"     {");
        IF req.raw *smode#{} THEN wnd.print(scr," raw")   END;
        IF req.sync*smode#{} THEN wnd.print(scr," sync")  END;
        wnd.print(scr," }")
      END;
      wnd.print(scr,"\n")
    END
  END;
END show_dev;

PROCEDURE open(VAL n: ARRAY OF CHAR);
  CONST DEV=bio.is_tty+bio.is_disk+bio.is_spec;
BEGIN
  wnd.era(scr,2); wnd.setpos(scr,2,0);
  str.copy(name,n);
  str.print(s_name,"/dev/%s",n);
  bio.open(F,name,'X');
  IF bio.done & (bio.kind(F)*DEV#{}) THEN RETURN END;
  IF bio.done THEN bio.close(F) END;
  bio.fopen(dev,F,name,'X');
  IF NOT bio.done THEN bio_error('open'); key.read(ch);
    IF ch=ascii.ESC THEN wnd.era(scr,2); wnd.close(scr) END
  END;
END open;

PROCEDURE list;
 VAR nm: ARRAY [0..63] OF CHAR;

  PROCEDURE entry(kind: BITSET);
    VAR d: bio.FILE;
  BEGIN
    bio.fopen((dev),d,s_name,'');
    IF NOT bio.done THEN
      IF bio.error#err.no_entry THEN bio_error('*open'); RETURN END;
      IF kind={} THEN show_head({}); wnd.print(scr,'%-8s\n',name) END
    ELSIF kind*bio.kind(d)#{} THEN
      show_dev(d); bio.close(d)
    END
  END entry;

  PROCEDURE _list(dir: bio.FILE; kind: BITSET);
    VAR mode: BITSET;
  BEGIN
    bio.dir_walk(dir,0);
    WHILE bio.get_entry(dir,s_name,mode) DO
      IF mode*bio.e_esc#{} THEN entry(kind) END
    END;
    bio.end_walk(dir)
  END _list;

BEGIN
  wnd.era(scr,2);   wnd.open(scr);
  wnd.ontop(scr);  wnd.setpos(scr,2,0);
  nm:=name;
  bio.open(dev,"/dev",'rx');
  IF NOT bio.done THEN name:="/dev"; bio_error("open"); RETURN END;
  false_h;
  _list(bio.cd,{});
  IF dev#bio.cd THEN _list(dev,{})          END;
  _list(bio.cd,bio.is_spec);
  IF dev#bio.cd THEN _list(dev,bio.is_spec) END;
  _list(bio.cd,bio.is_tty);
  IF dev#bio.cd THEN _list(dev,bio.is_tty)  END;
  _list(bio.cd,bio.is_disk);
  IF dev#bio.cd THEN _list(dev,bio.is_disk) END;
  IF show THEN key.read(ch);
    IF ch=ascii.ESC THEN wnd.close(scr) END;
    IF nm#'' THEN open(nm) END;
  END;
END list;

PROCEDURE get_spec;
BEGIN
  low.zero(spc);
  spc.op:=req.GET_SPEC;
  bio.doio(F,spc);
  IF NOT bio.done THEN bio_error('get_spec'); END;
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
  IF bio.kind(F)*bio.is_disk#{} THEN spc.ssc:=lb(spc.secsize) END;
  spc.op :=req.SET_SPEC;
  bio.doio(F,spc);
  IF NOT bio.done THEN bio_error('set_spec'); key.read(ch) END
END set_spec;

PROCEDURE sure(w: wnd.WINDOW): BOOLEAN;
BEGIN
  wnd.print(w,"are you sure?");
  key.read(ch);
  IF ORD(ch) MOD 128>=32 THEN wnd.print(w,"%c",ch) END;
  wnd.print(w,"\n");
  IF ch=ascii.BREAK THEN HALT(50h) END;
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
  IF NOT bio.done THEN bio_error('seek(0)'); RETURN END;
  bio.get(F,buf,spc.secsize);
  IF NOT bio.done THEN bio_error('read_sector0'); RETURN END;

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
   secs: INTEGER;
 le_pos: INTEGER; (* last error position *)
 errors: INTEGER;
retries: INTEGER;
  l_eof: INTEGER; -- for show %% of verifying

  PROCEDURE perror;
    VAR h,t,s: INTEGER;
  BEGIN
    s:=i DIV spc.secsize;
    IF (spc.cyls<=0) OR (spc.heads<=0) OR (secs<=0) THEN
      lex.perror(ERR,bio.error,'"%s" sector: %d  verify error: %%s\n',s_name,s)
    ELSE
      t:=s DIV secs;    h:=t MOD spc.heads;
      s:=s MOD secs;    t:=t DIV spc.heads;
      lex.perror(ERR,bio.error,'"%s" t%04d h%d s%02d verify error: %%s\n'
                               ,s_name,t,h,s)
    END;
    IF NOT wnd.top(err_w) THEN wnd.ontop(err_w) END;
    IF errors>0 THEN wnd.open(err_w); wnd.frameprint(err_w,0,10,' ERRORs ') END;
    wnd.print(err_w,ERR);
  END perror;

BEGIN
  get_spec;
  wnd.era(vrf,2);       wnd.open(vrf);
  wnd.setpos(vrf,0,0); wnd.ontop(vrf);
  wnd.frameprint(vrf,0,3,' verifying "%s" ',s_name);
  wnd.print(vrf,'\n');
  secs:=spc.maxsec-spc.minsec+1;
  len :=secs<<spc.ssc;
  IF secs<<spc.ssc>2*1024*1024 THEN len:=len*spc.heads END;
  IF (len>64*1024) OR (len<4*1024)  THEN len:=16*1024  END;
  NEW(buf,len);
  errors:=0;
  eof:=bio.eof(F);   le_pos:=-999999; retries:=0;
  IF spc.ressec>0 THEN
    res_sectors:=spc.ressec; INC(eof,spc.ressec*spc.secsize);
    spc.ressec:=0; set_spec
  END;
  i:=0; l_eof:=eof DIV 100;
  LOOP
    IF ABS(i-le_pos)<len*2 THEN l:=spc.secsize ELSE l:=len END;
    IF i+l>eof THEN l:=eof-i END;
    IF l<=0    THEN EXIT END;
    bio.seek(F,i,0);
    IF NOT bio.done THEN bio_error('seek') END;
    wnd.print(vrf,"  %2d%%  %hh\r",i DIV l_eof,i);
    bio.get(F,buf,l);
    IF NOT bio.done THEN
      IF le_pos#i THEN le_pos:=i; l:=0; INC(retries) END;
      IF l=spc.secsize THEN perror; INC(errors) END
    END;
    i:=i+l;
    IF key.ready()>0 THEN key.read(ch);
      IF ch=ascii.ESC THEN EXIT END;
    END;
  END;
  wnd.print(vrf,'verifying completed\n');
  IF    errors>0  THEN wnd.print(vrf,' with %d errors',errors)
  ELSIF retries>0 THEN wnd.print(vrf,' %d sectors read after retry',retries)
  END;
  restore_res_sectors;
  IF show THEN key.read(ch); wnd.close(vrf); wnd.close(err_w) END;
END verify;

PROCEDURE format;
   CONST fmt = '"%s" t%04d.h%02d %|6s error: %%s\n';
   VAR  r: req.REQUEST;
      trk: INTEGER;
      fbuf: STRING;
      sav: STRING;
     secs: INTEGER;
   errors: INTEGER;
     done: BOOLEAN;

  PROCEDURE final;
  BEGIN
    wnd.print(fmt_w,'formatting completed');
    IF errors>0 THEN wnd.print(fmt_w,' with %d errors',errors) END;
    wnd.print(fmt_w,'\n');
    IF arg.flag('-','f') THEN mark END;
    restore_res_sectors;
    verify
  END final;

  PROCEDURE prepare;
  BEGIN
    r.ofs:=-1;    r.len:=0;
    r.pos:= 0;    r.buf:=NIL;
    r.op :=req.FORMAT;
    bio.doio(F,r);
    IF (r.res=err.not_enough) & (r.len>0) THEN NEW(fbuf,r.len);
      r.ofs:=-1;    r.len:=BYTES(fbuf);
      r.pos:= 0;    r.buf:=SYSTEM.ADR(fbuf);
      r.op :=req.FORMAT;
      bio.doio(F,r)
    ELSE NEW(fbuf,0)
    END;
    IF bio.done THEN RETURN END;
    lex.perror(ERR,bio.error,'"%s" prepare error: %%s\n',s_name);
    show_error(ERR);
    done:=FALSE;
  END prepare;

  PROCEDURE unit;
  BEGIN
    IF soft THEN
      wnd.print(err_w,"Save format impossible!"
                      "Whole disk will be formating!\n");
      IF NOT sure(err_w) THEN RETURN END;
    END;
    r.ofs:=0;    r.len:=BYTES(fbuf);
    r.pos:=0;    r.buf:=SYSTEM.ADR(fbuf);
    r.op :=req.FORMAT;
    bio.doio(F,r);
    IF NOT bio.done THEN INC(errors);
      lex.perror(ERR,bio.error,fmt,s_name,0,0,'format'); --HALT(bio.error);
      show_error(ERR);
      done:=FALSE; RETURN
    END;
    final;
  END unit;

  PROCEDURE read;
  BEGIN
    bio.seek(F,trk*secs*spc.secsize,0);
    IF NOT bio.done THEN bio_error("seek"); --HALT(bio.error)
      done:=FALSE; RETURN
    END;
    bio.get(F,sav,BYTES(sav));
    IF NOT bio.done THEN
      lex.perror(ERR,bio.error,fmt,s_name,trk DIV spc.heads,
                                          trk MOD spc.heads,'read');
      show_error(ERR);
    END
  END read;

  PROCEDURE write;
  BEGIN
    bio.seek(F,trk*secs*spc.secsize,0);
    IF NOT bio.done THEN bio_error("seek"); --HALT(bio.error)
      done:=FALSE; RETURN
    END;
    bio.put(F,sav,BYTES(sav));
    IF NOT bio.done THEN
      lex.perror(ERR,bio.error,fmt,s_name,trk DIV spc.heads,
                                          trk MOD spc.heads,'write');
      show_error(ERR);
    END
  END write;

BEGIN
  false_h;           errors:=0;
  wnd.era(err_w,2);  wnd.setpos(err_w,0,0);
  get_spec;          show_dev(F);
  IF show THEN key.read(ch);
    IF ch=ascii.ESC THEN RETURN END;
  END;
  wnd.era(fmt_w,2);  wnd.open(fmt_w);
  wnd.ontop(fmt_w); wnd.setpos(fmt_w,0,0);
  tty.set_color(+1); wnd.print(fmt_w,'\n');
  IF soft THEN       wnd.print(fmt_w,'SOFT ') END;
                     wnd.print(fmt_w,'FORMAT "%s"; ',s_name);
  tty.set_color(0);
  IF NOT sure(fmt_w) THEN wnd.close(fmt_w); RETURN END;
  secs:=spc.maxsec-spc.minsec+1;
  wnd.print(fmt_w,'formatting "%s"\n',s_name);
  prepare;
  IF NOT done THEN key.read(ch); wnd.close(err_w); RETURN END;
  IF spc.ressec>0 THEN res_sectors:=spc.ressec; spc.ressec:=0; set_spec END;
  IF spc.dmode*req.fmtunit#{} THEN unit; RETURN END;
  IF soft THEN NEW(sav,secs*spc.secsize) END;
  wnd.cursor(fmt_w,0);
  WITH spc DO
    trk:=0;
    WHILE trk<cyls*heads DO
      IF trk MOD heads = 0 THEN wnd.print(fmt_w,"cyl %d\r",trk DIV heads) END;
      IF soft THEN read;
        IF NOT done THEN key.read(ch); wnd.close(err_w); RETURN END;
      END;
      r.op :=req.FORMAT;
      r.pos:=0;         r.len:=BYTES(fbuf);
      r.ofs:=trk*secs;  r.buf:=SYSTEM.ADR(fbuf);
      bio.doio(F,r);
      IF NOT bio.done THEN INC(errors);
        lex.perror(ERR,bio.error,fmt,s_name,trk DIV heads,
                                            trk MOD heads,'format');
        show_error(ERR);
      END;
      IF soft THEN write END;
      INC(trk)
    END
  END;
  final;
  IF show THEN key.read(ch);
    IF ch=ascii.ESC THEN wnd.close(fmt_w) END;
  END;
END format;

PROCEDURE power_off;

  PROCEDURE idle; CODE cod.li0 cod.setm cod.idle END idle;

  VAR r: req.REQUEST;
      d: bio.FILE;
   mode: BITSET;

  PROCEDURE park_disk;
  BEGIN
    bio.fopen((dev),d,s_name,'x');
    IF NOT bio.done THEN RETURN END;
    IF bio.kind(d)*bio.is_disk={} THEN RETURN END;
    bio.doio(d,r);
    IF bio.done THEN
      wnd.print(scr,'device "%s" powered off\n',s_name)
    ELSE
      lex.perror(ERR,r.res,'"%s" %s power off: %%s\n',s_name,"don't");
      show_error(ERR);
    END;
    bio.close(d)
  END park_disk;

  PROCEDURE park_other;
  BEGIN
    bio.fopen((dev),d,s_name,'x');
    IF NOT bio.done THEN RETURN END;
    IF bio.kind(d)*bio.is_disk#{} THEN bio.doio(d,r) END;
    bio.close(d)
  END park_other;

BEGIN
  wnd.era(scr,2);    wnd.open(scr);  wnd.ontop(scr);
  tty.set_color(+1); wnd.print(scr,'\nPARK ALL ');
  tty.set_color(0);
  IF NOT sure(scr) THEN RETURN END;
  r.op:=req.POWER_OFF; r.buf:=NIL;
  r.ofs:=0;            r.len:=0;

  bio.dir_walk(bio.cd,0);
  WHILE bio.get_entry(bio.cd,s_name,mode) DO
    IF mode*bio.e_esc#{} THEN park_disk END
  END;
  bio.end_walk(bio.cd);
  bio.dir_walk(dev,0);
  WHILE bio.get_entry(dev,s_name,mode) DO
    IF mode*bio.e_esc#{} THEN park_disk END
  END;
  bio.end_walk(dev);

  tim.delay(2,tim.sec);

  bio.dir_walk(bio.cd,0);
  WHILE bio.get_entry(bio.cd,s_name,mode) DO
    IF mode*bio.e_esc#{}THEN park_other END;
  END;
  bio.end_walk(bio.cd);
  bio.dir_walk(dev,0);
  WHILE bio.get_entry(dev,s_name,mode) DO
    IF mode*bio.e_esc#{}THEN park_other END;
  END;
  bio.end_walk(dev);
  idle
END power_off;

PROCEDURE read_num(l,n: INTEGER): INTEGER;
  VAR i,pos: INTEGER;
      num  : INTEGER;
      done : BOOLEAN;
      st   : ARRAY [0..7] OF CHAR;

  PROCEDURE pr(r: BOOLEAN);
  BEGIN
    IF r THEN wnd.print(num_w,'\r') END;
    IF bio.kind(F)*bio.is_disk#{} THEN wnd.print(num_w,'%8d',num);
    ELSE                               wnd.print(num_w,'%5d',num);
    END;
  END pr;

BEGIN pos:=0; i:=0; ch:=' ';
  wnd.cursor( num_w,1); wnd.setpos(num_w,l,0); wnd.eraln(num_w,0);
  REPEAT wnd.writech(num_w,ch); key.read(ch); st[i]:=ch; INC(i);
  UNTIL (i=HIGH(st)) OR (ch=ascii.ESC) OR (ch=ascii.CR) OR (ch=ascii.SPACE);
  wnd.cursor( num_w,0);
  IF ch=ascii.ESC THEN num:=n; pr(TRUE); RETURN n END;
  st[i]:=0c;
  str.iscan(num,st,pos,done);
  IF NOT done THEN num:=n END;
  wnd.setpos(num_w,l,0); wnd.eraln(num_w,0);
  wnd.underline(num_w,1);
  pr(FALSE);
  wnd.underline(num_w,0);
  RETURN num;
END read_num;

PROCEDURE ask_disk(l,c: INTEGER);

  PROCEDURE s_per_track;
  BEGIN
     wnd.setpos(num_w,0,0);
     wnd.print(num_w,'%8d',spc.maxsec-spc.minsec+1);
  END s_per_track;

  PROCEDURE ini_num;
  BEGIN
    wnd.ontop(num_w); wnd.resize(num_w,8,10);
    wnd.era(num_w,2);  wnd.move (num_w, l+2,c+36);
    wnd.open(num_w);   wnd.setpos(num_w,0,0);
    wnd.print(num_w,'%8d\n',spc.maxsec-spc.minsec+1);
    wnd.print(num_w,'%8d\n',spc.minsec);
    wnd.print(num_w,'%8d\n',spc.maxsec);
    wnd.print(num_w,'%8d\n',spc.cyls);
    wnd.print(num_w,'%8d\n',spc.heads);
    wnd.print(num_w,'%8d\n',spc.dsecs);
    wnd.print(num_w,'%8d\n',spc.secsize);
    wnd.print(num_w,'%8d\n',spc.ressec);
    wnd.print(num_w,'%8d\n',spc.rate);
    wnd.print(num_w,'%8d',spc.precomp);
  END ini_num;

BEGIN
  done:=FALSE;
  menu.on_top(m_disk);    menu.open(m_disk,l,c);
  ini_num;
  LOOP
    menu.select(m_disk);
    IF NOT menu.done THEN menu.close(m_disk); wnd.close(num_w); RETURN END;
    CASE menu.alt(m_disk) OF
      | 0: EXIT;
      | 1: s_per_track;
      | 2: spc.minsec:=read_num(1,spc.minsec); s_per_track;
      | 3: spc.maxsec:=read_num(2,spc.maxsec); s_per_track;
      | 4: spc.cyls:=read_num(3,spc.cyls);
      | 5: spc.heads:=read_num(4,spc.heads);
      | 6: spc.dsecs:=read_num(5,spc.dsecs);
      | 7: spc.secsize:=read_num(6,spc.secsize);
      | 8: spc.ressec:=read_num(7,spc.ressec);
      | 9: spc.rate:=read_num(8,spc.rate);
      |10: spc.precomp:=read_num(9,spc.precomp);
    ELSE
    END;
  END;
  menu.close(m_disk); wnd.close(num_w);
  done:=TRUE;
END ask_disk;

PROCEDURE ask_tty;
 VAR  l,c: INTEGER;

  PROCEDURE read_char(l: INTEGER; c:CHAR): CHAR;
    VAR i,pos: INTEGER;
        done : BOOLEAN;
        num  : INTEGER;
        st   : ARRAY [0..5] OF CHAR;
  BEGIN pos:=0; i:=0; ch:=0c;
    wnd.cursor( num_w,1); wnd.setpos(num_w,l,0); wnd.eraln(num_w,0);
    REPEAT wnd.writech(num_w,ch); key.read(ch);
      st[i]:=ch; INC(i);
    UNTIL (i=HIGH(st)) OR (ch=ascii.ESC) OR (ch=ascii.CR);
    wnd.cursor( num_w,0);
    IF ch=ascii.ESC THEN wnd.print(num_w,'\r %03bc',c); RETURN c END;
    st[i-1]:=0c;
    str.iscan(num,st,pos,done);
    IF (NOT done) OR (num<0) OR (num>255) THEN ch:=c ELSE ch:=CHAR(num) END;
    wnd.setpos(num_w,l,0); wnd.eraln(num_w,0);
    wnd.underline(num_w,1);
    wnd.print(num_w,' %03bc',ch);
    wnd.underline(num_w,0);
    RETURN ch;
  END read_char;

  PROCEDURE ini_num;
  BEGIN
    wnd.ontop(num_w); wnd.resize(num_w,8,8);
    wnd.era(num_w,2);  wnd.move (num_w, l+2,c+8);
    wnd.open(num_w);   wnd.setpos(num_w,0,0);
    wnd.print(num_w,'%5d\n',spc.baud);
    wnd.print(num_w,' %03bc\n',spc.xon);
    wnd.print(num_w,' %03bc\n',spc.xoff);
    wnd.print(num_w,'%5d\n',spc.limxon);
    wnd.print(num_w,'%5d\n',spc.limxoff);
    IF (req.parNO+req.parODD+req.parEVEN)*spc.smode#{} THEN
      IF req.parNO  *spc.smode#{} THEN wnd.print(num_w,"   no\n"); PAR:=0  END;
      IF req.parODD *spc.smode#{} THEN wnd.print(num_w,"  odd\n"); PAR:=1  END;
      IF req.parEVEN*spc.smode#{} THEN wnd.print(num_w," even\n"); PAR:=2  END;
    ELSE  wnd.print(num_w,"   ?\n")
    END;
    IF (req.stops1+req.stops2+req.stops1_5)*spc.smode#{} THEN
      IF req.stops0  *spc.smode#{} THEN wnd.print(num_w,"    0"); stp:=0  END;
      IF req.stops1  *spc.smode#{} THEN wnd.print(num_w,"    1"); stp:=1  END;
      IF req.stops2  *spc.smode#{} THEN wnd.print(num_w,"    2"); stp:=2  END;
      IF req.stops1_5*spc.smode#{} THEN wnd.print(num_w,"  1.5"); stp:=3  END;
    ELSE  wnd.print(num_w,"   ?")
    END;
  END ini_num;

  PROCEDURE par;
  BEGIN
    menu.on_top(m_par); menu.popup(m_par,l+8,c+8);
    IF NOT menu.done THEN RETURN END;
    spc.smode:=spc.smode-req.parNO-req.parODD-req.parEVEN;
    PAR:=menu.alt(m_par);
    wnd.setpos(num_w,5,0); wnd.underline(num_w,1);
    CASE PAR OF
     |0: spc.smode:=spc.smode+req.parNO;   wnd.print(num_w,'   no');
     |1: spc.smode:=spc.smode+req.parODD;  wnd.print(num_w,'  odd');
     |2: spc.smode:=spc.smode+req.parEVEN; wnd.print(num_w,' even');
    END;
    wnd.underline(num_w,0);
  END par;

  PROCEDURE stops;
  BEGIN
    menu.on_top(m_stop); menu.popup(m_stop,l+8,c+8);
    IF NOT menu.done THEN RETURN END;
    spc.smode:=spc.smode-req.stops0-req.stops1-req.stops1_5-req.stops2;
    stp:=menu.alt(m_stop);
    wnd.setpos(num_w,6,0); wnd.underline(num_w,1);
    CASE stp OF
     |0: spc.smode:=spc.smode+req.stops0;   wnd.print(num_w,'    0');
     |1: spc.smode:=spc.smode+req.stops1;   wnd.print(num_w,'    1');
     |2: spc.smode:=spc.smode+req.stops1_5; wnd.print(num_w,'  1.5');
     |3: spc.smode:=spc.smode+req.stops2;   wnd.print(num_w,'    2');
    END;
    wnd.underline(num_w,0);
  END stops;

BEGIN
  done:=FALSE;   l:=2; c:=64;
  menu.on_top(m_tty);    menu.open(m_tty,l,c);
  ini_num;
  LOOP
    menu.select(m_tty);
    IF NOT menu.done THEN menu.close(m_tty); wnd.close(num_w); RETURN END;
    CASE menu.alt(m_tty) OF
      | 0: EXIT;
      | 1: spc.baud:=   read_num (0,spc.baud);
      | 2: spc.xon:=    read_char(1,spc.xon);
      | 3: spc.xoff:=   read_char(2,spc.xoff);
      | 4: spc.limxon:= read_num (3,spc.limxon);
      | 5: spc.limxoff:=read_num (4,spc.limxoff);
      | 6: par;
      | 7: stops;
    ELSE
    END;
  END;
  menu.close(m_tty); wnd.close(num_w);
  done:=TRUE;
END ask_tty;

PROCEDURE flp_spec;
BEGIN
  done:=FALSE;
  menu.on_top(m_flp); menu.open(m_flp,2,40);
  menu.select(m_flp);
  IF NOT menu.done THEN menu.close(m_flp); RETURN END;
  WITH spc DO
    CASE menu.alt(m_flp) OF
      |0:  cyls:=80; secsize:=1024; ressec:=10; heads:=2;
           maxsec:=minsec+4;
      |1:  cyls:=80; secsize:=1024; ressec:=0 ;
           maxsec:=minsec+4;
      |2:  cyls:=40; secsize:=1024; ressec:=0 ;
           maxsec:=minsec+4;
      |3:  cyls:=40; secsize:=512 ; ressec:=0 ;
           maxsec:=minsec+8;
      |4:  cyls:=80; secsize:=512 ; ressec:=0 ;
           maxsec:=minsec+8;
      |5:  ask_disk(4,32); menu.close(m_flp); RETURN
    END;
  END;
  menu.close(m_flp);
  done:=TRUE;
END flp_spec;

PROCEDURE ask_spec;
BEGIN get_spec;
  IF NOT bio.done THEN RETURN END;
  wnd.era(scr,2);
  IF wnd.closed(scr) THEN wnd.open(scr) END;
  wnd.setpos(scr,2,0);
  IF bio.kind(F)*bio.is_disk#{} THEN
    IF req.floppy*spc.dmode#{} THEN flp_spec
    ELSE ask_disk(2,35)
    END;
  ELSIF bio.kind(F)*bio.is_tty#{}  THEN ask_tty
  ELSE  show_error('Uncnown devise!'); RETURN
  END;
  IF done THEN set_spec; false_h; show_dev(F) END;
END ask_spec;

PROCEDURE dsk_spec(): BOOLEAN;

  PROCEDURE number(VAL s: ARRAY OF CHAR; VAR i: INTEGER);
  BEGIN
    IF arg.number(s,i) THEN something:=TRUE ELSE i:=-1 END
  END number;

BEGIN something:=FALSE;
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
  IF something THEN
    IF (ls>=0) & (fs>=0) THEN
      IF (s>=0) & (s#ls-fs+1) OR (ls<fs) THEN
        wnd.print(scr,"illegal sectors specification\n"); RETURN TRUE
      END
    END;
    get_spec; false_h; show_dev(F);
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
    set_spec; show_dev(F);
  END;
  IF arg.flag('-','f')
  OR arg.flag('-','F') THEN format END;
  IF arg.flag('-','m') THEN mark   END;
  IF arg.flag('-','v') THEN verify END;
  RETURN something OR arg.flag('-','f') OR arg.flag('-','F')
                   OR arg.flag('-','m') OR arg.flag('-','v')
END dsk_spec;

PROCEDURE pusage;
BEGIN
  wnd.era(scr,2);  wnd.open(scr);
  wnd.ontop(scr); wnd.setpos(scr,0,0);
  wnd.print(scr,
      '  "dsu"  device setup utility program (c) KRONOS\n\n'
      '  setup device attributes at "." & "/dev" directories\n');
  wnd.print(scr,
      'usage:\n   dsu [name] [-hltvpfFs] {change_spec_equation}\n'
      'WARNING: Be sure, that no other tasks are operating on device now!\n');
  wnd.print(scr,
    '\n  -l   device   list     -f   format and mark\n'
      '  -v   verify   disk     -sf  save     format\n'
      '  -t   fs statistics     -F   format     disk\n'
      '  -p   power     off     -m   mark       disk\n');
  wnd.print(scr,
      '  s=sectors     "sectors per track"\n'
      '  fs=sector_no  "number of first sector"\n'
      '  ls=sector_no  "number of last  sector"\n'
      '  t=tracks      "tracks  on  disk"\n'
      '  h=heads       "heads"\n'
      '  v=volsize     "number of sectors on volume"\n');
  wnd.print(scr,
      '  ss=secsize    "sector  size (bytes)"\n'
      '  rs=ressecs    "number of reserved sectors"\n'
      '  rate=rate     "heads stepping rate"\n'
      '  prec=track_no "track to start precompensation"\n');
  wnd.print(scr,
      '                                   Leopold, Dec 07 90\n');
  IF show THEN key.read(ch);
    IF ch=ascii.ESC THEN wnd.close(scr) END;
  END;
END pusage;

PROCEDURE final;
BEGIN restore_res_sectors END final;

PROCEDURE ini_windows;
BEGIN
  scr:=wnd.full; wnd.open(scr);

  wnd.new(num_w);          wnd.frame(num_w,0);        wnd.cursor(num_w,0);

  wnd.new(err_w);          wnd.move(err_w,8,0);       wnd.resize(err_w,75,13);
  wnd.frame(err_w,1);      wnd.cursor(err_w,0);

  wnd.new(vrf);            wnd.move(vrf,1,30);        wnd.resize(vrf,40,5);
  wnd.frame(vrf,1);        wnd.cursor(vrf,0);

  wnd.new(fmt_w);          wnd.move(fmt_w,1,20);      wnd.resize(fmt_w,45,5);
  wnd.frame(fmt_w,1);      wnd.cursor(fmt_w,0);

  wnd.new(stat);           wnd.move(stat,1,10);       wnd.resize(stat,30,7);
  wnd.frame(stat,1);       wnd.cursor(stat,0);
END ini_windows;

PROCEDURE ini_disk_menu;
BEGIN
  menu.set_alt(m_disk, 0,'x',TRUE,"[x] s a v e  &  e X i t");
  menu.set_alt(m_disk, 1,'p',TRUE,"[p] sectors Per track");
  menu.set_alt(m_disk, 2,'f',TRUE,"[f] number of First sector");
  menu.set_alt(m_disk, 3,'l',TRUE,"[l] number of Last  sector");
  menu.set_alt(m_disk, 4,'d',TRUE,"[d] tracks  on  Disk");
  menu.set_alt(m_disk, 5,'h',TRUE,"[h] Heads           ");
  menu.set_alt(m_disk, 6,'v',TRUE,"[v] number of sectors on Volume");
  menu.set_alt(m_disk, 7,'s',TRUE,"[s] sector  Size (bytes)");
  menu.set_alt(m_disk, 8,'r',TRUE,"[r] number of Reserved sectors");
  menu.set_alt(m_disk, 9,'e',TRUE,"[e] hEads stepping rate");
  menu.set_alt(m_disk,10,'t',TRUE,"[t] Track to start precompensation");
  menu.selector(m_disk,key.left);
  menu.selector(m_disk,key.right);
  menu.selector(m_disk,ascii.SPACE);
END ini_disk_menu;

PROCEDURE ini_tty_menu;
BEGIN
  menu.set_alt(m_tty, 0,'v',TRUE,"saVe");
  menu.set_alt(m_tty, 1,'b',TRUE,"Baud");
  menu.set_alt(m_tty, 2,'f',TRUE,"xon");
  menu.set_alt(m_tty, 3,'l',TRUE,"xoff");
  menu.set_alt(m_tty, 4,'d',TRUE,"on");
  menu.set_alt(m_tty, 5,'h',TRUE,"off");
  menu.set_alt(m_tty, 6,'p',TRUE,"Par");
  menu.set_alt(m_tty, 7,'s',TRUE,"Stops");
  menu.selector(m_tty,key.left);
  menu.selector(m_tty,key.right);
  menu.selector(m_tty,ascii.SPACE);
END ini_tty_menu;

PROCEDURE ini_par;
BEGIN
   menu.set_alt( m_par,0,'n',TRUE,"No");
   menu.set_alt( m_par,1,'o',TRUE,"Odd");
   menu.set_alt( m_par,2,'e',TRUE,"Even");
   menu.selector(m_par,ascii.SPACE);
END ini_par;

PROCEDURE ini_stop;
BEGIN
   menu.set_alt( m_stop,0,'0',TRUE,"  0");
   menu.set_alt( m_stop,1,'1',TRUE,"  1");
   menu.set_alt( m_stop,2,'5',TRUE,"1.5");
   menu.set_alt( m_stop,3,'2',TRUE,"  2");
   menu.selector(m_stop,ascii.SPACE);
END ini_stop;

(*$X+*)
PROCEDURE ini_dev;
  VAR   d: bio.FILE;
        i: INTEGER;
     mode: BITSET;
BEGIN i:=0;
  menu.new(m_dev, menu.updown, 4);  menu.frame(m_dev, 1);
  bio.dir_walk(dev,0);
  WHILE bio.get_entry(dev,name,mode) DO
    IF mode*bio.e_esc#{} THEN
      bio.fopen((dev),d,name,'');
      IF bio.done & (NOT_SPEC*bio.kind(d)#{}) THEN
        NEW(dev_list[i],str.len(name)+1);
        IF Heap.done THEN
          menu.set_alt(m_dev,i,' ',TRUE,'%s',name);
          str.copy(dev_list[i],name);
          INC(i);
        END;
      END;
      bio.close(d);
    END
  END;
  bio.end_walk(dev);
END ini_dev;
(*$X-*)

PROCEDURE ini_flp;
BEGIN
   menu.set_alt( m_flp,0,'0',TRUE,'Labtam - 3000 format 5.25"');
   menu.set_alt( m_flp,1,'0',TRUE,'WorkStation WS2.6 (80 tracks)');
   menu.set_alt( m_flp,2,'0',TRUE,'WorkStation WS2.6 (40 tracks)');
   menu.set_alt( m_flp,3,'0',TRUE,'IBM-PC 360 KB');
   menu.set_alt( m_flp,4,'0',TRUE,'IBM-PC 720 KB');
   menu.set_alt( m_flp,5,'O',TRUE,'Other');
END ini_flp;

PROCEDURE ini_menus;
BEGIN
  menu.new(main,menu.barline,7);  menu.frame(main,0);
  menu.set_alt(main,0,'h',TRUE,' Help');
  menu.set_alt(main,1,'f',TRUE,' Form');
  menu.set_alt(main,2,'t',TRUE,' sTat');
  menu.set_alt(main,3,'s',TRUE,' Save');
  menu.set_alt(main,4,'l',TRUE,' List');
  menu.set_alt(main,5,'p',TRUE,' Power');
  menu.set_alt(main,6,'v',TRUE,' Verif');
  menu.set_alt(main,7,'m',TRUE,' Mark');
  menu.set_alt(main,8,'e',TRUE,' spEc');
  menu.set_alt(main,9,'d',TRUE,'  Dev');
  menu.selector(main,key.dw);

  ini_dev;
  menu.new(m_soft,menu.barline,5);  menu.frame(m_soft,1);
  menu.set_alt(m_soft,0,'t',TRUE,' ON');
  menu.set_alt(m_soft,1,'f',TRUE,' OFF');
  menu.new(m_disk,menu.updown,34);  menu.frame(m_disk,1);   ini_disk_menu;
  menu.new(m_tty, menu.updown, 5);  menu.frame(m_tty, 1);   ini_tty_menu;
  menu.new(m_par, menu.updown, 4);  menu.frame(m_par, 1);   ini_par;
  menu.new(m_stop,menu.updown, 3);  menu.frame(m_stop,1);   ini_stop;
  menu.new(m_flp, menu.updown,30);  menu.frame(m_flp, 1);   ini_flp;
END ini_menus;

PROCEDURE dev_select;
BEGIN
  menu.on_top(m_dev);
  menu.popup(m_dev,1,73);
  IF menu.done THEN
    open(dev_list[menu.alt(m_dev)]);
    wnd.era(scr,2); wnd.setpos(scr,2,0);
    wnd.open(scr); false_h; show_dev(F); key.read(ch);
    IF ch=ascii.ESC THEN wnd.close(scr) END;
  ELSE name:='';
  END;
END dev_select;

PROCEDURE soft_select;
BEGIN
  IF soft THEN menu.set_select(m_soft,0) ELSE menu.set_select(m_soft,1) END;
  menu.on_top(m_soft);
  menu.popup(m_soft,1,20);
  IF menu.done THEN
    CASE menu.alt(m_soft) OF
     |0: soft:=TRUE;
     |1: soft:=FALSE;
    END;
  END;
END soft_select;

PROCEDURE select;
BEGIN
  menu.open(main,0,0);
  LOOP
    menu.on_top(main);
    menu.select(main);
    IF NOT menu.done THEN EXIT END;
    CASE menu.alt(main) OF
     |0: pusage;
     |1: format;
     |2: fs_stat;
     |3: soft_select;
     |4: list;
     |5: power_off;
     |6: verify;
     |7: mark;
     |8: ask_spec;
     |9: dev_select;
    END;
  END;
END select;

BEGIN
  show:=FALSE;
  F:=bio.null;
  res_sectors:=-1;
  bio.open(dev,"/dev",'rx');
  IF NOT bio.done THEN name:="/dev"; bio_error("open"); HALT(bio.error) END;
  ini_windows;
  ini_menus;
  env.final(final);
  IF arg.flag('-','h')    THEN pusage;    HALT END;
  false_h;
  IF arg.flag('-','t')    THEN fs_stat;   HALT END;
  IF arg.flag('-','l')    THEN list;      HALT END;
  IF arg.flag('-','p')    THEN power_off; HALT END;
  IF HIGH(arg.words)<0 THEN menu.open(main,0,0); menu.on_top(main); dev_select
  ELSE name:=arg.words[0]
  END;
  open(name);
  soft:=arg.flag('-','s');
  IF dsk_spec() THEN HALT END;
  show:=TRUE; select;
END dsu.
