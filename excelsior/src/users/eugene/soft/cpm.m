MODULE cpm; (* Sem 12-Jul-88. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADR;
FROM Terminal   IMPORT  print;

IMPORT  bio : BIO;
IMPORT  str : Strings;

CONST
  block_sz = 2048;
  sec_sz   = 1024;
  sec_oder = ARRAY OF INTEGER { 1, 4, 7, 2, 5, 8, 3, 6};

TYPE
  String=ARRAY [0..11] OF CHAR;
  Extent=RECORD
    Rcnt: INTEGER;
    Ref : ARRAY [0..7] OF INTEGER;
  END;
  Entry=RECORD
    User: INTEGER;
    Mode: BITSET;
    Name: String;
    Ext : String;
    Ent : ARRAY [0..31] OF Extent;
  END;

VAR
  dir: ARRAY [0..99] OF Entry;
  cnt: INTEGER;
  drv: bio.FILE;
  sec   : ARRAY [0..sec_sz-1] OF CHAR;
  sec_no: INTEGER;

PROCEDURE io_chk;
BEGIN
  IF NOT bio.done THEN
    print('IO error.\n'); HALT;
  END;
END io_chk;

PROCEDURE rd_sec(n: INTEGER);
BEGIN
  n:=n DIV SIZE(sec_oder) * SIZE(sec_oder) + sec_oder[n MOD SIZE(sec_oder)]-1;
  bio.seek(drv,n*sec_sz,0); io_chk;
  bio.read(drv,ADR(sec),BYTES(sec)); io_chk;
END rd_sec;

PROCEDURE Find(us: INTEGER; nm,ext: String): INTEGER;
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO cnt-1 DO
    IF (dir[i].Name=nm)&(dir[i].Ext=ext)&(dir[i].User=us) THEN RETURN i END;
  END;
  RETURN -1;
END Find;

PROCEDURE ReadDir;
  VAR
    i,j,k  : INTEGER;
    e,l,s  : INTEGER;
    nm,ex  : String;
    us     : INTEGER;
BEGIN
  cnt:=0;
  FOR s:=0 TO 4096 DIV sec_sz - 1 DO
    rd_sec(s);
    FOR i:=0 TO sec_sz DIV 32 - 1 DO
      IF ORD(sec[i*32])<16 THEN
        us:=ORD(sec[i*32]);
        FOR j:=0 TO 7 DO nm[j]:=CHAR(BITSET(sec[i*32+j+1])*{0..6}) END;
        nm[8]:=0c;
        FOR j:=0 TO 2 DO ex[j]:=CHAR(BITSET(sec[i*32+j+9])*{0..6}) END;
        ex[3]:=0c;
        j:=7; WHILE (j>=0)&(nm[j]=' ') DO nm[j]:=0c; DEC(j) END;
        j:=2; WHILE (j>=0)&(ex[j]=' ') DO ex[j]:=0c; DEC(j) END;
        j:=Find(us,nm,ex);
        IF j<0 THEN
          IF cnt>HIGH(dir) THEN print('Too many files...\n'); HALT(1) END;
          j:=cnt; INC(cnt);
          FOR l:=0 TO HIGH(dir[j].Ent) DO
            dir[j].Ent[l].Rcnt:=0;
            FOR k:=0 TO HIGH(dir[j].Ent[l].Ref) DO dir[j].Ent[l].Ref[k]:=0 END;
          END;
          dir[j].Name:=nm;
          dir[j].Ext :=ex;
          dir[j].User:=us;
          dir[j].Mode:={};
          IF sec[i*32+ 9]>=200c THEN INCL(dir[j].Mode,0) END;
          IF sec[i*32+10]>=200c THEN INCL(dir[j].Mode,1) END;
          IF sec[i*32+11]>=200c THEN INCL(dir[j].Mode,2) END;
        END;
        e:=ORD(sec[i*32+12]);
        FOR k:=0 TO HIGH(dir[j].Ent[e].Ref) DO
          dir[j].Ent[e].Ref[k]:=
            ORD(sec[i*32+16+k*2])+ORD(sec[i*32+17+k*2])*256;
        END;
        dir[j].Ent[e].Rcnt:=ORD(sec[i*32+15]);
        IF dir[j].Ent[e].Rcnt>80h THEN dir[j].Ent[e].Rcnt:=80h END;
      END;
    END;
  END;
END ReadDir;

PROCEDURE UpdateDirEntry(n: INTEGER);
  VAR i,j,k,e: INTEGER;
      buf: ARRAY [0..4095] OF CHAR;
      nm,ex: String;
      us : INTEGER;
  PROCEDURE Update(e: INTEGER);
    VAR i,j,k,e0: INTEGER;
  BEGIN
    WITH dir[n] DO
      FOR i:=0 TO 4096 DIV 32 -1 DO
        IF (ORD(buf[i*32])=User)&
            ((BITSET(e)-{0})=(BITSET(buf[i*32+12])-{0})) THEN
          FOR j:=0 TO 7 DO nm[j]:=CHAR(BITSET(buf[i*32+j+1])*{0..6}) END;
          FOR j:=0 TO 2 DO ex[j]:=CHAR(BITSET(buf[i*32+j+9])*{0..6}) END;
          nm[8]:=0c; ex[3]:=0c;
          j:=7; WHILE (j>=0)&(nm[j]=' ') DO nm[j]:=0c; DEC(j) END;
          j:=2; WHILE (j>=0)&(ex[j]=' ') DO ex[j]:=0c; DEC(j) END;
          IF (nm=Name)&(ex=Ext) THEN
            IF 0 IN Mode THEN
              buf[i*32+09]:=CHAR(BITSET(buf[i*32+09])+{7});
            ELSE
              buf[i*32+09]:=CHAR(BITSET(buf[i*32+09])-{7});
            END;
            IF 1 IN Mode THEN
              buf[i*32+10]:=CHAR(BITSET(buf[i*32+10])+{7});
            ELSE
              buf[i*32+10]:=CHAR(BITSET(buf[i*32+10])-{7});
            END;
            IF 2 IN Mode THEN
              buf[i*32+11]:=CHAR(BITSET(buf[i*32+11])+{7});
            ELSE
              buf[i*32+11]:=CHAR(BITSET(buf[i*32+11])-{7});
            END;
            buf[i*32+12]:=CHAR(e);
            buf[i*32+15]:=CHAR(Ent[e].Rcnt);
            FOR k:=0 TO 3 DO
              IF NOT ODD(e) THEN
                buf[i*32+16+k*2]:=CHAR(Ent[e].Ref[k]);
                buf[i*32+17+k*2]:=CHAR(Ent[e].Ref[k]>>8);
              ELSE
                buf[i*32+24+k*2]:=CHAR(Ent[e].Ref[k]);
                buf[i*32+25+k*2]:=CHAR(Ent[e].Ref[k]>>8);
              END;
            END;
            RETURN
          END;
        END;
      END;
      FOR i:=0 TO 4096 DIV 32 -1 DO
        IF buf[i*32]>=16c THEN

          RETURN
        END;
      END;
      print('Directory overflow...\n'); HALT(1);
    END;
  END Update;

BEGIN
  bio.fread(drv,ADR(buf),0,4096);
  IF NOT bio.done THEN HALT(bio.error) END;
  FOR e:=0 TO HIGH(dir[n].Ent) DO
    IF (e=0)OR(dir[n].Ent[e].Rcnt>0) THEN Update(e) END;
  END;
  bio.fwrite(drv,ADR(buf),0,4096);
  IF NOT bio.done THEN HALT(bio.error) END;
END UpdateDirEntry;

PROCEDURE ListRef(n: INTEGER);
  VAR i,j: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(dir[n].Ent) DO
    FOR j:=0 TO HIGH(dir[n].Ent[i].Ref) DO
      print(' %3d',dir[n].Ent[i].Ref[j]);
    END;
  END;
END ListRef;

PROCEDURE ListHead(n: INTEGER);
  CONST nn=160; VAR i: INTEGER;
BEGIN
  FOR i:=0 TO nn-1 DO sec[i]:=' ' END;
  rd_sec(dir[n].Ent[0].Ref[0]*(block_sz DIV sec_sz));
  FOR i:=0 TO nn-1 DO
    IF (sec[i]<' ') OR (sec[i]>177c) THEN sec[i]:='.' END;
  END;
  sec[nn]:=0c;
  print('%s\n',sec);
END ListHead;

PROCEDURE ListCPM;
  VAR i: INTEGER;
BEGIN
  print('Directory for CPM media:\n');
  FOR i:=0 TO cnt-1 DO
    print('   %-8s.%3s  %2d  ',dir[i].Name,dir[i].Ext,dir[i].User);
    IF 0 IN dir[i].Mode THEN print(' RO') ELSE print(' RW') END;
    IF 1 IN dir[i].Mode THEN print(' SYS') ELSE print(' DIR') END;
    IF 2 IN dir[i].Mode THEN print(' archive') END;
    print('\n');
  END;
  print('%2d files found.\n',cnt);
END ListCPM;

PROCEDURE read_record(n,r: INTEGER; VAR pos: INTEGER): BOOLEAN;
  VAR ref,ent,s,r_s,s_b: INTEGER;
BEGIN
  ref:=r DIV 16;
  ent:=ref DIV 8;
  ref:=ref MOD 8;
  r:=r MOD 16;
  r_s:=sec_sz DIV (block_sz DIV 16);
  s_b:=block_sz DIV sec_sz;
  IF dir[n].Ent[ent].Rcnt<=ref*16+r THEN RETURN FALSE END;
  s:=dir[n].Ent[ent].Ref[ref]*s_b+r DIV r_s;
  pos:=(r MOD r_s)*(block_sz DIV 16);
  IF sec_no#s THEN rd_sec(s); sec_no:=s END;
  RETURN TRUE;
END read_record;

PROCEDURE read_file(n: INTEGER);
  VAR fnm: ARRAY [0..79] OF CHAR; b,ps: INTEGER; f: bio.FILE;
BEGIN
  str.print(fnm,'%s.%s',dir[n].Name,dir[n].Ext);
  print('%s\n',fnm);
  bio.create(f,fnm,'w',0); io_chk;
  bio.buffers(f,1,4096); io_chk;
  b:=0; sec_no:=-1;
  LOOP
    IF NOT read_record(n,b,ps) THEN EXIT END;
    bio.write(f,ADR(sec)+ps DIV 4,block_sz DIV 16); io_chk;
    INC(b);
  END;
  bio.close(f); io_chk;
END read_file;

VAR i: INTEGER;

BEGIN
  bio.open(drv,'/dev/dk0','rc'); io_chk;
  ReadDir;
  FOR i:=0 TO cnt-1 DO read_file(i) END;
END cpm.
