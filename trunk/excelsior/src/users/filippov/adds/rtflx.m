MODULE rtflx; (* Max  7 April 1987. *)

IMPORT  SYSTEM, ASCII;
IMPORT  err: defErrors;
IMPORT  arg: tskArgs;
IMPORT  str: Strings;
IMPORT  mcd: defCodes;
IMPORT  tim: Time;
IMPORT  env: tskEnv;
IMPORT  std: StdIO;
IMPORT  tty: Terminal;
IMPORT  dia: strEditor;
IMPORT  key: Keyboard;
IMPORT  bio: BIO;
IMPORT  req: defRequest;
IMPORT  low: lowLevel;
IMPORT  reg: regExpr;
IMPORT  Heap;

FROM SYSTEM     IMPORT  ADDRESS,WORD,ADR;

WITH STORAGE: Heap;

-------------------------- USER IN/OUT -------------------------
                          -------------

PROCEDURE TakeParm(VAR s: ARRAY OF CHAR);
BEGIN
  s:="";
  IF HIGH(arg.words)<0 THEN RETURN END;
  arg.pack_words(0,HIGH(arg.words));
  str.copy(s,arg.words[0]);
END TakeParm;

PROCEDURE PoolStr(VAR s: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  i:=0;
  WHILE (i<=HIGH(s)) & (s[i]=" ") DO INC(i) END;
  str.delete(s,0,i);
END PoolStr;

PROCEDURE GetWord(VAR from,to: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  to:="";
  PoolStr(from);
  IF from[0]=0c THEN RETURN END;
  i:=0;
  WHILE (i<=HIGH(from)) & (from[i]#" ") & (from[i]#0c)  DO INC(i) END;
  str.sub_str(to,from,0,i);
  str.delete(from,0,i);
END GetWord;

VAR user_buf: dia.descriptor;

PROCEDURE ReadString(VAL prompt: ARRAY OF CHAR; VAR s: ARRAY OF CHAR);
BEGIN
  tty.set_pos(tty.state^.lines-1,0); tty.erase_line(0);
  dia.edit_str(prompt,s,tty.state^.lines-1,0,tty.state^.columns-2,user_buf,033c);
  IF user_buf^.last=033c THEN s:="" END;
END ReadString;

PROCEDURE AskInt(s: ARRAY OF CHAR; VAR n: INTEGER): BOOLEAN;
  VAR p: INTEGER; yes: BOOLEAN;
BEGIN
  ReadString(s,s);
  p:=0; str.iscan(n,s,p,yes);
  RETURN NOT yes
END AskInt;

PROCEDURE QueryLn(VAL s: ARRAY OF CHAR): BOOLEAN;
  VAR ch: CHAR;
BEGIN
  tty.WriteString(s);
  REPEAT
    key.read(ch); ch:=CAP(ch)
  UNTIL (ch='Y') OR (ch='N');
  tty.print('%c\n',ch);
  RETURN (ch='Y')
END QueryLn;

PROCEDURE move(t,f: ADDRESS; sz: INTEGER); CODE mcd.move END move;

CONST RTEOF = 32c;
      eol=0c;
      empty=1000b;     (*status words of entry's*)
      permanent=2000b;
      EndMark=4000b;
      CrData=13411b;   (* Некoторая дата *)

TYPE PDPword=[0..0FFFFh];
     RTuname=ARRAY [0..10] OF CHAR;
     RTpname=ARRAY [0..2 ] OF PDPword;

---------------------------- ERRORS ----------------------------
                            --------

PROCEDURE bio_check;
BEGIN
  IF bio.done THEN RETURN END;
  tty.perror(bio.error,'%%s: "%s"\n',bio.ename);
  HALT(bio.error);
END bio_check;

PROCEDURE MSGerr(k:INTEGER);
BEGIN
  CASE k OF
    0: std.print('Неправильная команда.\n');
   |1: std.print('Ошибка чтения.\n');
   |3: std.print('Нет имени носителя.\n');
   |4: std.print('Файл не найден.\n');
   |5: std.print('Не могу создать файл\n');
   |6: std.print('Не могу записать\n');
   |7: std.print('Не могу закрыть файл\n');
   |8: std.print('Такой файл уже есть\n');
   |9: std.print('Переполнены Сегменты директории\n');
   |10:std.print('Не могу узнать размер файла\n');
   |11:std.print('Неверное имя устройства\n');
   |12:std.print('Неправильная или не RT-11-директория\n');
   |13:std.print('Файла и так нет\n');
   |14:std.print('Неверное имя RT-11-файла\n');
   |15:std.print('Пустое имя файла\n');
  ELSE ;
  END;
END MSGerr;

------------------------- RT-11 IN/OUT -------------------------
                         --------------

CONST blk_size = 512;

VAR dname: ARRAY [0..31] OF CHAR;
    ds: BOOLEAN;
    disk: bio.FILE;
    save: req.REQUEST;

PROCEDURE restore_format;
BEGIN
  IF save.op=req.SET_SPEC THEN
    bio.doio(disk,save); save.op:=req.NOP
  END;
  bio.close(disk); bio_check;
END restore_format;

PROCEDURE open_disk;
  VAR r: req.REQUEST;
BEGIN
  bio.open(disk,dname,"rw"); bio_check;
  IF bio.is_disk*bio.kind(disk)={} THEN
    tty.print('"%s" is not disk device\n',dname); HALT(1)
  END;
  r.op:=req.GET_SPEC;
  bio.doio(disk,r); bio_check;
  r.op:=req.SET_SPEC;
  save:=r;
  WITH r DO
    ssc    :=9;
    secsize:=blk_size;
    cyls   :=80;
    IF ds THEN heads:=2 ELSE heads:=1 END;
    minsec :=1;
    maxsec :=10;
    ressec :=0;
    dsecs  :=ssc*cyls*heads;
  END;
  bio.doio(disk,r); bio_check;
  bio.close(disk);
  bio.open(disk,dname,"rw"); bio_check;
  IF bio.is_disk*bio.kind(disk)={} THEN
    tty.print('"%s" is not disk device\n',dname); HALT(1)
  END;
  env.final(restore_format);
END open_disk;

PROCEDURE check(res: INTEGER; VAL op: ARRAY OF CHAR);
BEGIN
  IF res=err.ok THEN RETURN END;
  std.perror(res,"%s: %%s\n",op);
END check;

PROCEDURE rt_read(block: INTEGER; buf: ADDRESS);
BEGIN
  bio.seek(disk,block*blk_size,0); bio_check;
  bio.read(disk,buf,blk_size);     bio_check;
END rt_read;

PROCEDURE rt_write(block: INTEGER; buf: ADDRESS);
BEGIN
  bio.seek (disk,block*blk_size,0); bio_check;
  bio.write(disk,buf,blk_size);     bio_check;
END rt_write;

VAR rt_buf: ARRAY [0..blk_size*2-1] OF CHAR;
    rt_pos: INTEGER;

PROCEDURE getC(VAR pos: INTEGER): INTEGER;
BEGIN
  INC(pos); RETURN ORD(rt_buf[pos-1])
END getC;

PROCEDURE GetW(): PDPword;
BEGIN
  RETURN getC(rt_pos)+getC(rt_pos)*256
END GetW;

PROCEDURE getW(pos: INTEGER): PDPword;
BEGIN
  RETURN getC(pos)+getC(pos)*256
END getW;

PROCEDURE putC(c: INTEGER; VAR pos: INTEGER);
  VAR ch: CHAR;
BEGIN
  c:=c MOD 256;
  rt_buf[pos]:=CHR(c); INC(pos)
END putC;

PROCEDURE PutW(w: PDPword; pos:INTEGER);
  VAR c,p:INTEGER;
BEGIN
  putC(w MOD 256,pos); putC(w DIV 256,pos);
END PutW;

PROCEDURE Skip(k: INTEGER; VAR w: PDPword);
  VAR i: INTEGER;
BEGIN
  FOR i:=1 TO k DO w:=GetW(); END;
END Skip;

----------------- Radix (* Лопатин 14-Мая-86 *) ----------------
                 -------------------------------

TYPE str3 = ARRAY [0..2] OF CHAR;

PROCEDURE R50(ch: CHAR): INTEGER;
BEGIN
  IF (ORD(ch) >= ORD('A')) & (ORD(ch) <= ORD('Z')) THEN
    RETURN ORD(ch)-ORD('A')+1
  END ;
  IF (ORD(ch) >= ORD('0')) & (ORD(ch) <= ORD('9')) THEN
    RETURN ORD(ch)-ORD('0')+30
  END ;
  CASE ch OF
    ' ': RETURN 0
  | '$': RETURN 27
  | '.': RETURN 28
  ELSE   RETURN 29
  END
END R50;

CONST ASC = ARRAY OF CHAR {
               " ","A","B","C","D","E","F","G",
               "H","I","J","K","L","M","N","O",
               "P","Q","R","S","T","U","V","W",
               "X","Y","Z","$",".","?","0","1",
               "2","3","4","5","6","7","8","9"};

CONST base = 50b;

PROCEDURE asc2rdx(с: str3): INTEGER;
BEGIN
  RETURN (R50(с[0])*base + R50(с[1]))*base + R50(с[2]);
END asc2rdx;

PROCEDURE rdx2asc(val: INTEGER; VAR с: str3);
  VAR i: INTEGER;
BEGIN
  FOR i := 2 TO 0 BY -1 DO
    с[i] := ASC[val MOD base]; val := val DIV base
  END;
END rdx2asc;

PROCEDURE rt2exc(Thr: RTpname; VAR F: RTuname);
(* RADIX-имя Thr распаковывает в F *)
  VAR C: str3; i: INTEGER;
BEGIN
  rdx2asc(Thr[0],C); FOR i:=0 TO 2 DO F[i]:=C[i] END;
  rdx2asc(Thr[1],C); FOR i:=3 TO 5 DO F[i]:=C[i-3] END;
  F[6]:='.';
  rdx2asc(Thr[2],C); FOR i:=7 TO 9 DO F[i]:=C[i-7] END;
  F[10]:=eol;
END rt2exc;

PROCEDURE Cap(VAR F: RTuname);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO 9 DO
    IF ASCII.cyril IN ASCII.KIND(F[i]) THEN F[i]:='Z';
    ELSE F[i]:=ASCII.CAPITAL(F[i]);
    END;
  END;
END Cap;

PROCEDURE exc2rt(VAR Thr:RTpname; F:RTuname);
  VAR C: str3; i: INTEGER;
BEGIN (* Имя F запаковывает в RADIX-имя Thr *)
  Cap(F);
  FOR i:=0 TO 2 DO C[i]  :=F[i] END; Thr[0]:=asc2rdx(C);
  FOR i:=3 TO 5 DO C[i-3]:=F[i] END; Thr[1]:=asc2rdx(C);
  FOR i:=7 TO 9 DO C[i-7]:=F[i] END; Thr[2]:=asc2rdx(C);
END exc2rt;

PROCEDURE app_time(VAR s: ARRAY OF CHAR);
  VAR t,y,m,i: INTEGER;
      n: ARRAY [0..11] OF CHAR;
BEGIN
  t:=tim.time();
  tim.unpack(t,y,m,i,i,i,i);
  CASE m OF
    | 1: str.copy(n,"JAN");
    | 2: str.copy(n,"FEB");
    | 3: str.copy(n,"MAR");
    | 4: str.copy(n,"APR");
    | 5: str.copy(n,"MAY");
    | 6: str.copy(n,"JUN");
    | 7: str.copy(n,"JUL");
    | 8: str.copy(n,"AUG");
    | 9: str.copy(n,"SEP");
    |10: str.copy(n,"OCT");
    |11: str.copy(n,"NOV");
    |12: str.copy(n,"DEC");
  END;
  y:=y MOD 100;
  str.print(s,'(c) KRONOS  %s-%d',n,y);
END app_time;

PROCEDURE init(DEV: ARRAY OF CHAR);

  PROCEDURE puts(VAR pos: INTEGER; SEQ w: PDPword);
    VAR i: INTEGER;
  BEGIN
    FOR i:=0 TO HIGH(w) DO PutW(w[i],pos); INC(pos,2) END;
  END puts;

  PROCEDURE putw(w: PDPword; VAR pos: INTEGER);
  BEGIN
    putC(w MOD 256,pos); putC(w DIV 256,pos);
  END putw;

(* Инициализация носителя в RT-11 *)
  VAR hStr: ARRAY [0..23] OF CHAR;
     ds,sz: INTEGER;
         p: ADDRESS;
         i: INTEGER;
       pos: INTEGER;
BEGIN
  tty.WriteString(DEV);
  IF NOT QueryLn('-инициализация. Вы уверены? ') THEN RETURN END;
  IF AskInt('Количество сегментов в оглавлении [2]? ',ds) THEN ds:=2 END;
  std.WriteLn;

  (* boot-block : *)
  pos:=0;
  low.fill(rt_buf,0);
  p:=ADR(rt_buf);

  puts(pos,
       000240b, 012706b, 001000b, 004067b,
       000044b, 000015b, 047517b, 026524b,
       026056b);

  app_time(hStr);

  FOR i:=0 TO 17 DO putC(ORD(hStr[i]),pos) END;

  puts(pos,
       005015b, 000200b, 105737b, 177564b,
       100375b, 112037b, 177566b, 100372b,
       000777b);

  rt_write(0,p);
  (* end boot-block *)

  (* home-block : *)
  low.fill(rt_buf,0);
  pos:=233*2;

  puts(pos,
       000001b,0000006b,107251b,046052b,
       041101b,0051117b,052101b,051117b,
       025131b,0042524b,046522b,020056b,
       054523b,0052123b,046505b,042504b,
       051103b,0030524b,040461b,025040b,
       025040b,0025040b,025040b);
  rt_write(1,p);
  (* end home-block *)

  (* first Directory segment : *)
  ds:=ds MOD 32; pos:=0;

  putw(ds,pos); putw(0h,pos); putw(1h,pos); putw(0h,pos); putw(6+ds*2,pos);
  putw(empty,pos);
  FOR sz:=0 TO 2 DO putw(0h,pos); END;
  sz:=bio.eof(disk) DIV 512;
  putw(sz-6-ds*2,pos); putw(0h,pos); putw(CrData,pos); putw(EndMark,pos);
  REPEAT putw(0h,pos) UNTIL pos=512;
  rt_write(6,p);
END init;

TYPE DSdesc = RECORD
                ssno: INTEGER; (* Число Directory Segments *)
                next: INTEGER; (* =0, если сегмент последний, иначе - номеру следующего *)
                curs: INTEGER; (* номер сегмента, который RT-11 считает текущим *)
                extr: INTEGER; (* число Extra-байтов в Entry *)
                fblo: INTEGER; (* номер блока, начинающего первый Entry сегмента *)
              END;

(*
TYPE DSegment = RECORD
                  buf : ARRAY [0..blk_size*2-1] OF CHAR;
                  pos : INTEGER;
                  ssno: INTEGER; (* Число Directory Segments *)
                  next: INTEGER; (* =0, если сегмент последний, иначе - номеру следующего *)
                  curs: INTEGER; (* номер сегмента, который RT-11 считает текущим *)
                  extr: INTEGER; (* число Extra-байтов в Entry *)
                  fblo: INTEGER; (* номер блока, начинающего первый Entry сегмента *)
                END;

*)
TYPE DEntry = RECORD
                info: INTEGER;
                name: RTuname;
                len : INTEGER;
              END;

PROCEDURE read_dir_seg(block: INTEGER; VAR desc: DSdesc): BOOLEAN;
  VAR a: ADDRESS;
BEGIN
  a:=ADR(rt_buf);
  rt_read(block  ,a); INC(a,blk_size DIV 4);
  rt_read(block+1,a);

  rt_pos:=0;
  WITH desc DO
    ssno:=GetW();
    next:=GetW();
    curs:=GetW();
    extr:=GetW() DIV 2;
    fblo:=GetW();
  END;

  IF (desc.fblo=0) OR (desc.ssno=0) THEN
    MSGerr(12); RETURN TRUE
  END;
  RETURN FALSE
END read_dir_seg;

PROCEDURE read_dir_ent(VAL desc: DSdesc; VAR e: DEntry);
  VAR rtn: RTpname; w: PDPword;
BEGIN

  e.info:=GetW();
  IF e.info=EndMark THEN
    RETURN
  END;
  IF e.info=empty THEN
    Skip(3,w); e.name[0]:=0c;
  ELSE
    rtn[0]:=GetW(); rtn[1]:=GetW(); rtn[2]:=GetW();
    rt2exc(rtn,e.name);
  END;
  e.len:=GetW();
  Skip(2+desc.extr,w);
END read_dir_ent;

PROCEDURE directory(full: BOOLEAN);

  VAR desc: DSdesc;
       ent: DEntry;
     dsize: INTEGER;   free,busy,file: INTEGER;
     block: INTEGER;
      flag: BOOLEAN;
        cc: INTEGER;

BEGIN
  free:=0; busy:=0; file:=0; cc:=0;
  dsize:=(bio.eof(disk) DIV blk_size)+1;
  block:=6;
  WHILE block#0 DO
    IF read_dir_seg(block,desc) THEN RETURN END;
    IF full THEN
      std.print('Directory segment %d\n\n',(block-4) DIV 2);
      cc:=0;
    END;

    read_dir_ent(desc,ent);
    WHILE ent.info#EndMark DO

      IF full THEN std.WriteString('  ')
      ELSE
        IF cc MOD 3 = 0 THEN std.WriteString('  ')
        ELSE                 std.WriteString('       ')
        END;
      END;

      flag:=ent.info=empty;
      IF flag THEN
        IF full   THEN std.WriteString("< UNUSED >") END;
        INC(free,ent.len)
      ELSE
        std.WriteString(ent.name);
        INC(busy,ent.len); INC(file);
      END;
      IF NOT flag OR full THEN std.print('%6d',ent.len) END;
      IF full THEN
        std.print('  START: %6d',desc.fblo)
      END;
      desc.fblo:=desc.fblo+ent.len;

      INC(cc);
      IF full THEN
        IF cc MOD 2 = 0 THEN std.WriteLn END;
      ELSE
        IF cc MOD 3 = 0 THEN std.WriteLn END;
      END;

      read_dir_ent(desc,ent);
    END;
    block:=desc.next;
    IF block#0 THEN block:=block*2+4 END;
  END;
  IF NOT full & ((cc MOD 3)=0) THEN std.WriteLn END;
  std.print("\n%4d Files %6d Blocks "
            "\n%6d Free blocks\n",file,busy,free);
END directory;

VAR --N: ARRAY [0..15] OF CHAR;
    query: BOOLEAN;
    ascii: BOOLEAN;

---------------------------- RT DISK ---------------------------
                            ---------

PROCEDURE search(name: RTuname; VAR blk,pos,start,len: INTEGER): BOOLEAN;
  VAR d: DSdesc;
      e: DEntry;
BEGIN
  Cap(name);
  WHILE blk#0 DO
    IF read_dir_seg(blk,d) THEN RETURN TRUE END;
    pos:=rt_pos; read_dir_ent(d,e);
    WHILE e.info#EndMark DO
      IF e.info=empty THEN
      ELSIF e.name=name THEN
        start:=d.fblo; len:=e.len; RETURN FALSE
      END;
      d.fblo:=d.fblo+e.len;
      pos:=rt_pos; read_dir_ent(d,e);
    END;
    blk:=d.next;
    IF blk#0 THEN blk:=blk*2+4 END;
  END;
  RETURN TRUE
END search;

PROCEDURE repack(VAL ename: ARRAY OF CHAR; VAR name: RTuname);
  VAR g,p: INTEGER;
BEGIN
  FOR p:=0 TO HIGH(name) DO name[p]:=" " END;
  p:=0; g:=0;
  WHILE (g<=HIGH(ename)) & (p<6) & (ename[g]#'.') & (ename[g]#0c) DO
    name[p]:=ename[g]; INC(p); INC(g);
  END;
  name[6]:="."; p:=6;
  name[10]:=0c;
  WHILE (g<HIGH(ename)) & (ename[g]#".") & (ename[g]#0c) DO INC(g) END;
  IF (g>=HIGH(ename)) OR (ename[g]#".") THEN RETURN END;
  WHILE (g<HIGH(ename)) & (p<10) & (ename[g]#0c) DO
    name[p]:=ename[g]; INC(g); INC(p)
  END;
  IF name[0]=" " THEN name[0]:=0c END;
END repack;

PROCEDURE Del(devn: ARRAY OF CHAR);
  VAR Name: RTuname; a: ADDRESS;

   blk,pos: INTEGER;
   sta,len: INTEGER;

BEGIN
  repack(devn,Name);
  IF Name[0]=0c THEN RETURN END;
  blk:=6;
  IF search(Name,blk,pos,sta,len) THEN MSGerr(13);
  ELSE
    PutW(empty,pos);
    a:=ADR(rt_buf); rt_write(blk  ,a);
    a:=a+128;       rt_write(blk+1,a);
  END;
END Del;

TYPE bytep=POINTER TO ARRAY[0..511] OF CHAR;

PROCEDURE cpf(devn,EFile: ARRAY OF CHAR; ascii: BOOLEAN);
(* Копирует файл Name с устройства DEV в файл EFile *)
  VAR Name: RTuname;
     req,i: INTEGER;
         s: bio.FILE;
         a: ADDRESS;
        p1: bytep;
   blk,pos: INTEGER;
   sta,len: INTEGER;

BEGIN
  repack(devn,Name);
  IF Name[0]=0c THEN RETURN END;
  blk:=6;
  IF search(Name,blk,pos,sta,len) THEN MSGerr(4); RETURN END;
  bio.create(s,EFile,"w",4);
  IF NOT bio.done THEN MSGerr(5); RETURN END;
  a:=ADR(rt_buf);
  WHILE len>0 DO
    rt_read(sta,a);
    IF ascii & (len=1) THEN
      req:=0;
      WHILE (req<blk_size) & ((len>1) OR (rt_buf[req]#RTEOF)) DO
        IF rt_buf[req]=ASCII.LF THEN rt_buf[req]:=ASCII.NL END;
        INC(req)
      END;
    ELSE req:=blk_size
    END;
    bio.write(s, a, req);
    IF NOT bio.done THEN MSGerr(6); RETURN END;
    DEC(len); INC(sta);
  END;
  bio.close(s); IF NOT bio.done THEN MSGerr(7) END;
END cpf;

PROCEDURE cpt(EFile,devn: ARRAY OF CHAR; ascii: BOOLEAN);

  VAR blk,pos: INTEGER; (* dir entry pos *)
      sta,len: INTEGER; (* free pos      *)
      eos_pos: INTEGER; (* pos for EndMark in dir segment *)
         desc: DSdesc;

  PROCEDURE allocate(length: INTEGER): BOOLEAN;

    VAR b: INTEGER;
        p: INTEGER;
        e: DEntry;
     save: INTEGER;

  BEGIN
    b:=6;
    WHILE b#0 DO
      IF read_dir_seg(b,desc) THEN RETURN TRUE END;
      p:=rt_pos; read_dir_ent(desc,e);
      WHILE e.info#EndMark DO
        IF e.info#empty THEN
        ELSE
          IF    e.len=length THEN
            sta:=desc.fblo;
            blk:=b; eos_pos:=0;
            pos:=p; len:=e.len;
            RETURN FALSE
          ELSIF e.len>length THEN
            save:=rt_pos;
              blk:=b;
              pos:=p;
              sta:=desc.fblo;
              len:=e.len;
              REPEAT p:=rt_pos; read_dir_ent(desc,e) UNTIL e.info=EndMark;
              IF p+14+desc.extr<blk_size-1 THEN eos_pos:=p; RETURN FALSE END;
            rt_pos:=save;
          END;
        END;
        desc.fblo:=desc.fblo+e.len;
        p:=rt_pos; read_dir_ent(desc,e);
      END;
    END;
    RETURN TRUE
  END allocate;

   VAR Name: RTuname;
        rtn: RTpname;
          p: ADDRESS;
          w: PDPword;
        ptr: POINTER TO ARRAY [0..511] OF CHAR;
   i,length: INTEGER;
          j: INTEGER;
          s: bio.FILE;
    eof,req: INTEGER;

  PROCEDURE copy;
    VAR out: bio.FILE;
        buf: POINTER TO ARRAY [0..4095] OF CHAR;
  req,i,eof: INTEGER;
        sta: INTEGER;
  BEGIN
    bio.create(out,"","m",4); bio_check;
    bio.buffers(out,4,4096);
    eof:=bio.eof(s); NEW(buf);
    WHILE eof>0 DO
      req:=BYTES(buf^);
      IF req>eof THEN req:=eof END;
      bio.read(s,buf,req); bio_check;
      i:=0; sta:=0;
      WHILE i<req DO
        IF buf^[i]=ASCII.NL THEN
          IF sta<i THEN
            bio.fwrite(out,buf,sta,i-sta);
          END;
          bio.putch(out,ASCII.CR);
          bio.putch(out,ASCII.LF);
          sta:=i+1;
        END;
        INC(i);
      END;
      IF sta<i THEN
        bio.fwrite(out,buf,sta,i-sta);
        sta:=i+1
      END;
      DEC(eof,req);
    END;
    bio.close(s);
    bio.seek(out,0,0);
    s:=out;
    DISPOSE(buf);
  END copy;

BEGIN
  repack(devn,Name); IF Name[0]=eol THEN RETURN END;

  i:=6;
  IF NOT search(Name,i,j,j,j) THEN MSGerr(8); RETURN END;

  bio.open(s,EFile,'r');
  IF NOT bio.done THEN std.print('%s ',EFile); MSGerr(4); RETURN END;
  IF ascii THEN copy END;

  eof:=bio.eof(s); length:=(eof DIV 512)+1;

  IF allocate(length) THEN MSGerr(9); RETURN END;
  IF eos_pos#0 THEN
    FOR i:=eos_pos TO pos BY -2 DO w:=getW(i); PutW(w,i+14+desc.extr) END;
    PutW(len-length,pos+22+desc.extr);
  END;
  PutW(permanent,pos);
  exc2rt(rtn,Name);
  FOR i:=1 TO 3 DO PutW(rtn[i-1],pos+i*2) END;
  PutW(length,pos+4*2); PutW(CrData,pos+6*2);
  p:=ADR(rt_buf); rt_write(blk  ,p);
  INC(p,128);     rt_write(blk+1,p);
  p:=ADR(rt_buf);
  FOR i:=sta TO sta+length-1 DO
    low.fill(rt_buf,0);
    req:=blk_size; IF req>eof THEN req:=eof END;
    bio.read(s, p, req);
    len:=bio.iolen;
    IF NOT bio.done OR (len<req) THEN MSGerr(1) END;
    DEC(eof,req);
    IF ascii THEN
      IF len<512 THEN ptr^[len]:=RTEOF END;
    END;
    rt_write(i,p);
  END;
  bio.close(s);
  IF NOT bio.done THEN MSGerr(7) END;
END cpt;

VAR pen: INTEGER; ch: CHAR;
    text: ARRAY [0..15] OF CHAR;

PROCEDURE dump;
  VAR i: INTEGER;
BEGIN
  std.Write('|');
  FOR i:=0 TO 15 DO ch:=text[i];
    IF (ORD(ch)<40b) OR ((ORD(ch)>177b) AND (ORD(ch)<240b)) THEN ch:='.'; END;
    std.Write(ch);
  END; std.WriteLn; pen:=0;
END dump;

PROCEDURE Dump();
(* дамп DEV в абсолютных блоках *)
  VAR buf: ARRAY [0..511] OF CHAR;
  VAR i: INTEGER;
BEGIN
  WHILE NOT AskInt('Номер блока? ',i) DO
    rt_read(i,ADR(buf));
    std.WriteLn; FOR i:=0 TO 15 DO text[i]:=40c; END; pen:=0;
    FOR i:=0 TO 510 BY 2 DO
      IF ((i MOD 16)=0) AND (i#0) THEN dump; END;
      text[pen]:=buf[i]; text[pen+1]:=buf[i+1]; INC(pen,2);
      std.Write( CHAR((ORD(buf[i+1]) DIV 128)+ORD('0')) );
      std.Write( CHAR(((ORD(buf[i+1]) DIV 16) MOD 8)+ORD('0')) );
      std.Write( CHAR(((ORD(buf[i+1]) DIV  2) MOD 8)+ORD('0')) );
      std.Write( CHAR((ORD(buf[i+1]) MOD 2)*4+(ORD(buf[i]) DIV 64)+ORD('0') ) );
      std.Write( CHAR(((ORD(buf[i]) DIV 8) MOD 8)+ORD('0')) );
      std.Write( CHAR((ORD(buf[i]) MOD 8)+ORD('0')) );
      std.Write(' ');
    END;
    dump; std.WriteLn();
  END;
END Dump;

PROCEDURE reg_err(VAL s: ARRAY OF CHAR): BOOLEAN;
BEGIN
  IF reg.done THEN RETURN FALSE END;
  std.perror(reg.error,"%s: %%s\n%.*c*\n",s,reg.epos," ");
  RETURN TRUE
END reg_err;

PROCEDURE cptAll(VAL e,r: ARRAY OF CHAR);
  VAR cd: bio.FILE;
    name: ARRAY [0..31] OF CHAR;
    mode: BITSET;
    expr: reg.EXPR;
     res: ARRAY [0..31] OF CHAR;
    copy: BOOLEAN;

  CONST file = bio.e_file+bio.e_hidden;

BEGIN
  reg.compile(e,expr);
  IF reg_err(e) THEN RETURN END;
  cd:=bio.cd;
  bio.dir_walk(cd,bio.s_none); bio_check;
  WHILE bio.get_entry(cd,name,mode) DO
    IF (mode<=file) & reg.match(expr,name,0) THEN
      reg.substitute(expr,name,r,res);
      std.print('%-20s',res);
      IF query THEN copy:=QueryLn("? ") ELSE copy:=TRUE; std.print('\n') END;
      IF copy THEN cpt(name,res,ascii) END;
    END;
  END;
  bio.end_walk(cd);
  reg.dispose(expr);
END cptAll;

PROCEDURE ScanRTflags(VAR s: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  query:=TRUE; ascii:=FALSE;
  FOR i:=0 TO str.len(s)-2 DO
    IF (s[i]='/') & (s[i+1]='q') THEN
      query:=FALSE;  s[i]:=' '; s[i+1]:=' ';
    END;
    IF (s[i]='/') & (s[i+1]='a') THEN
      ascii:=TRUE;  s[i]:=' '; s[i+1]:=' ';
    END;
  END;
END ScanRTflags;

PROCEDURE commands;
BEGIN
  std.print(
    'commands:\n'
    '  dir[/f]             - directory [/full]\n'
    '  ini                 - initialize RT11 disk\n',
    '  del file_name       - delete RT11 file\n');
  std.print(
    '  cpf[/a/q] sou dest  - copy RT11 file "sou" to Excelsior "dest"\n'
    '  cpt[/a/q] sou dest  - copy Excelsior file(s) to RT11 disk\n'
    '  dump                - dump of RT11 disk\n'
    '  help                - commands help\n'
    '  bye                 - exit of utility\n');
  std.print(
    'keys:\n'
    '  /q - no query\n'
    '  /a - ascii files copy\n');
END commands;

VAR ttline,devn,line,com,EFile:ARRAY [0..127] OF CHAR;

PROCEDURE inter;
  VAR i: INTEGER;
BEGIN
    ScanRTflags(line);
    GetWord(line,com);
    IF    com='dir'   THEN directory(FALSE);
    ELSIF com='dir/f' THEN directory(TRUE );
    ELSIF com='ini'   THEN
      GetWord(line,devn); init(devn);
    ELSIF com='del'   THEN GetWord(line,devn); Del(devn);
    ELSIF com='cpf'   THEN
      GetWord(line,devn); GetWord(line,EFile); cpf(devn,EFile,ascii);
    ELSIF com='cpt'   THEN
      GetWord(line,EFile); GetWord(line,devn); cptAll(EFile,devn);
    ELSIF com='dump'  THEN Dump();
    ELSIF com='help'  THEN commands;
    ELSIF com='bye'   THEN restore_format; HALT
    ELSIF com[0]=eol  THEN
    ELSE MSGerr(0);
    END;
END inter;

PROCEDURE help;
BEGIN
  std.print(
    '  rtflx v2.0  RT11 FLexible disks eXchange (c)1986-92 KRONOS\n'
    'usage:\n'
    '  rtflx [ dx=disk ] [ command parms ] [-ds|-h]\n'
    '    (default disk "/dev/fd0")\n'
    '  -ds - double sided floppy\n');
  commands;
  std.print('\n'
    '             Good luck to use it!\n'
    '      Лопатин 14-Мая-86  Jat 05-Oct-86\n'
    '          Sem 23-Oct-86  Max 07-Apr-87\n'
    '         Hady 02-Jun-92.\n');
END help;

PROCEDURE InitIO;
  VAR s: STRING;
BEGIN
  ds:=arg.flag("-","d") & arg.flag("-","s");
  str.copy(dname,"/dev/fd0");
  IF arg.string("dx",s) THEN str.copy(dname,s) END;
  open_disk;
END InitIO;

BEGIN
  dia.new(user_buf,16);
  IF arg.flag("-",'h') THEN help; HALT END;
  InitIO;
  tty.nop;
  query:=NOT arg.flag('-','q') OR NOT std.is_tty(std.in);
  ttline:="";
  TakeParm(line); PoolStr(line);
  IF line[0]#0c THEN inter; restore_format; HALT END;
  LOOP
    line:=""; com:=""; devn:="";
    ReadString('flx>',ttline); str.copy(line,ttline);
    std.WriteLn;
    inter;
  END;
END rtflx.
