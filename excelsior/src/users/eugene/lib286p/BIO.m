IMPLEMENTATION MODULE BIO; (* Sem 09-Feb-91. (c) KRONOS *)

FROM SYSTEM      IMPORT ADDRESS, ADR, WORD;

IMPORT  def: def_GDT;
IMPORT  err: defErrors;
IMPORT  low: lowLevel;
IMPORT  str: Strings;
IMPORT  mem: Heap;

WITH STORAGE : mem;

CONST
  NULL  = NIL;
  mFILE = 3334h;

TYPE
  CAR16 = {0..0FFFFh};
  file_string = ARRAY [0..63] OF CHAR;
  file_modes = (fm_read,fm_hidden,fm_system,fm_label,fm_dir,fm_archive);
  file_mode  = SET OF file_modes;
  fcb = RECORD
    mag : INTEGER;
    id  : CAR16;
    md  : file_mode;
    name: file_string;
  END;
  FILE  = POINTER TO fcb;
  PATHs = FILE;

VAR
  gdt_s     : def.selector;
  dos_s     : def.selector;
  walk_activ: BOOLEAN;

PROCEDURE adr(w: ADDRESS): INTEGER;
  PROCEDURE real_adr(): INTEGER;
  CODE
        mov si,[bp+6]
        and si,0FFF8h
        mov es,gdt_s
        seg es
        mov dx,[si+2]
        seg es
        mov bl,[si+4]
        mov bh,0
        add dx,[bp+4]
        adc bx,0
        mov ax,dx
        and ax,0Fh
        shr bx,1 % rcr dx,1
        shr bx,1 % rcr dx,1
        shr bx,1 % rcr dx,1
        shr bx,1 % rcr dx,1
        and bx,bx
        jz ok
        int 04h
    ok:
  END real_adr;
BEGIN
  RETURN real_adr();
END adr;

PROCEDURE exit(VAR f: FILE; rem: BOOLEAN);
BEGIN
  IF done THEN RETURN END;
  CASE error MOD 10000h OF
    | 1: error:=err.inv_op;
    | 2: error:=err.no_entry;
    | 3: error:=err.no_entry;      -- path not found
    | 4: error:=err.fsys_full;     -- too many open files
    | 5: error:=err.ill_access;    -- access denided
    | 6: error:=err.bad_desc;      -- invalid handle
    | 7: error:=err.bad_desc;      -- memory ctrl block destroyed
    | 8: error:=err.no_memory;     -- insufficient memory
    | 9: error:=err.bad_parm;      -- invalid memory block address
    |10: error:=err.inconsistency; -- invalid environment
    |11: error:=err.ill_desc;      -- invalid format
    |12: error:=err.ill_access;    -- invalid access code
    |13: error:=err.inconsistency; -- invalid data
    |15: error:=err.no_entry;      -- invalid drive specified
    |16: error:=err.ill_access;    -- can't remove current dir
    |17: error:=err.unsuitable;    -- not same device
    |18: error:=err.no_entry;      -- no more matching files
  ELSE error:=err.inv_op;
  END;
  IF (f#NULL) & (f^.mag=mFILE) THEN
    str.copy(ename,f^.name);
    IF rem THEN f^.mag:=0; DISPOSE(f) END;
  ELSE ename:='*** invalid ***'
  END;
END exit;

PROCEDURE set_name(f: FILE; path: ARRAY OF CHAR);
  PROCEDURE get_dev(): CHAR;
  CODE mov ah,19h % int 21h % add al,41h
  END get_dev;
  PROCEDURE get_cd(d: CHAR; str: ADDRESS);
  CODE
        mov ah,47h
        pop si
        pop bx
        mov ds,dos_s
        mov [0],bx
        pop dx
        sub dl,40h
        int 21h
        seg cs
        mov ds,[0]
  END get_cd;
  VAR dev: CHAR; cd,tail,word: file_string; i,j,k,n: INTEGER;
BEGIN
  i:=0;
  WHILE path[i]#0c DO IF path[i]='/' THEN path[i]:='\' END; INC(i) END;
  i:=0;
  WHILE path[i]=' ' DO INC(i) END;
  IF (path[i]#0c) & (path[i+1]=':') THEN dev:=path[i]; INC(i,2);
  ELSE dev:=get_dev();
  END;
  IF dev>='a' THEN dev:=CHAR(ORD(dev)-20h) END;
  WHILE path[i]=' ' DO INC(i) END;
  IF path[i]='\' THEN INC(i); cd:='' ELSE get_cd(dev,adr(ADR(cd))) END;
  j:=0;
  WHILE path[i]#0c DO
    k:=0;
    WHILE (path[i]#0c) & (path[i]#'\') DO
      word[k]:=path[i]; INC(k); INC(i);
    END;
    word[k]:=0c;
    IF (word='') OR (word='.') THEN
    ELSIF (word='..') & (j>0) THEN
      REPEAT DEC(j) UNTIL (j=0) OR (tail[j]='\');
    ELSE
      IF j>0 THEN tail[j]:='\'; INC(j) END;
      k:=0;
      WHILE word[k]#0c DO tail[j]:=word[k]; INC(k); INC(j) END;
    END;
  END;
  tail[j]:=0c;
  IF cd='' THEN str.print(f^.name,'%c:\\%s',dev,tail);
  ELSIF tail='' THEN str.print(f^.name,'%c:\\%s',dev,cd);
  ELSE str.print(f^.name,'%c:\\%s\\%s',dev,cd,tail);
  END;
END set_name;

PROCEDURE open(VAR f: FILE; VAL path,mode: ARRAY OF CHAR);
  PROCEDURE get_attr(nm: ADDRESS): INTEGER;
  CODE
        pop dx
        pop bx
        mov ds,dos_s
        mov [0],bx
        mov ax,4300h
        int 21h
        mov dx,0FFFFh
        jc ok
        mov dx,0
        mov ax,cx
    ok: seg cs
        mov ds,[0]
  END get_attr;
  PROCEDURE open_code(nm: ADDRESS; md: CHAR): INTEGER;
  CODE
        pop ax
        pop dx
        pop bx
        mov ds,dos_s
        mov [0],bx
        mov ah,3Dh
        int 21h
        mov dx,0
        jnc ok
        mov dx,0FFFFh
    ok: seg cs
        mov ds,[0]
  END open_code;
  VAR md: CHAR; i: INTEGER; r,w,a: BOOLEAN; nm_buf: file_string;
BEGIN
  r:=FALSE; w:=FALSE; a:=FALSE; i:=0;
  WHILE mode[i]#0c DO
    md:=mode[i];
    r:=r OR (md='r') OR (md='m');
    w:=w OR (md='w') OR (md='m');
    a:=a OR (md='a');
    INC(i);
  END;
  IF r & w THEN md:=2c
  ELSIF w THEN md:=1c
  ELSE md:=0c;
  END;
  NEW(f);
  f^.mag:=mFILE;
  set_name(f,path);
  nm_buf:=f^.name;
  error:=get_attr(adr(ADR(nm_buf)));
  done:=error>=0;
  IF done THEN
    f^.md:=file_mode(error);
    IF NOT (fm_dir IN f^.md) THEN
      error:=open_code(adr(ADR(nm_buf)),md);
      done:=error>=0;
      f^.id:=CAR16(error);
    END;
  END;
  exit(f,TRUE);
  IF a & done THEN seek(f,2,0) END;
END open;

PROCEDURE create(VAR f: FILE; VAL path,mode: ARRAY OF CHAR;
                                      size: INTEGER);
  PROCEDURE cre_code(nm: ADDRESS; md: file_mode): INTEGER;
  CODE
        pop cx
        mov ch,0
        pop dx
        pop bx
        mov ds,dos_s
        mov [0],bx
        mov ah,3Ch
        int 21h
        mov dx,0
        jnc ok
        mov dx,0FFFFh
    ok: seg cs
        mov ds,[0]
  END cre_code;
  VAR i: INTEGER; h: BOOLEAN; nm_buf: file_string;
BEGIN
  h:=FALSE; i:=0;
  WHILE mode[i]#0c DO
    h:=h OR (mode[i]='h');
    INC(i);
  END;
  NEW(f);
  f^.mag:=mFILE;
  f^.md:=file_mode{};
  IF h THEN INCL(f^.md,fm_hidden) END;
  set_name(f,path);
  nm_buf:=f^.name;
  error:=cre_code(adr(ADR(nm_buf)),f^.md);
  done:=error>=0;
  f^.id:=CAR16(error);
  exit(f,TRUE);
END create;

PROCEDURE fcreate(cd: FILE; VAR file: FILE; VAL path,mode: ARRAY OF CHAR;
                                                       size: INTEGER);
  VAR name: file_string;
BEGIN
  IF (cd=NULL) OR (cd^.mag#mFILE) THEN error:=6; done:=FALSE; exit(cd,FALSE)
  ELSIF NOT (fm_dir IN cd^.md) THEN error:=6; done:=FALSE; exit(cd,FALSE)
  ELSE
    str.print(name,'%s\%s',cd^.name,path);
    create(file,name,mode,size);
  END;
END fcreate;

PROCEDURE chaccess(file: FILE; mask: BITSET);
BEGIN
END chaccess;

PROCEDURE close(VAR f: FILE);
  PROCEDURE close_code(f: CAR16): INTEGER;
  CODE
        pop bx
        mov ah,3Eh
        int 21h
        mov dx,0
        jnc ok
        mov dx,0FFFFh
    ok:
  END close_code;
BEGIN
  IF (f=NULL) OR (f^.mag#mFILE) THEN error:=6; done:=FALSE;
  ELSIF fm_dir IN f^.md THEN done:=TRUE;
  ELSE error:=close_code(f^.id); done:=error>=0;
  END;
  exit(f,FALSE);
  DISPOSE(f);
END close;

PROCEDURE purge(VAR file: FILE);
BEGIN
  close(file);
END purge;

PROCEDURE cut(file: FILE; size: INTEGER);
BEGIN
  end(file,size);
END cut;

PROCEDURE end(f: FILE; size: INTEGER);
  PROCEDURE end_code(f: CAR16; size: INTEGER): INTEGER;
  CODE
        pop dx
        pop cx
        pop bx
        mov ax,4200h
        int 21h
        jc err
        xor cx,cx
        mov ah,40h
        int 21h
        jc err
        mov dx,0
        jmp ok
    err:mov dx,0FFFFh
    ok:
  END end_code;
BEGIN
  IF (f=NULL) OR (f^.mag#mFILE) THEN
    error:=6; done:=FALSE;
  ELSIF fm_dir IN f^.md THEN
    error:=5; done:=FALSE;
  ELSE
    error:=end_code(f^.id,size); done:=error>=0;
  END;
  exit(f,FALSE);
END end;

PROCEDURE extend(file: FILE; size: INTEGER);
BEGIN
  end(file,size);
END extend;

PROCEDURE seek(f: FILE; pos,mode: INTEGER);
  PROCEDURE seek_code(f: CAR16; p: INTEGER; m: CHAR): INTEGER;
  CODE
        pop ax
        pop dx
        pop cx
        pop bx
        mov ah,42h
        int 21h
        mov dx,0
        jnc ok
        mov dx,0FFFFh
    ok:
  END seek_code;
BEGIN
  IF (f=NULL) OR (f^.mag#mFILE) THEN error:=6; done:=FALSE;
  ELSIF fm_dir IN f^.md THEN error:=5; done:=FALSE;
  ELSE error:=seek_code(f^.id,pos,CHAR(mode)); done:=error>=0;
  END;
  exit(f,FALSE);
END seek;

PROCEDURE eof(f: FILE): INTEGER;
  PROCEDURE eof_code(f: CAR16): INTEGER;
  CODE
        pop bx
        xor cx,cx
        xor dx,dx
        mov ax,4201h
        int 21h
        jc err
        push ax
        push dx
        xor cx,cx
        xor dx,dx
        mov ax,4202h
        int 21h
        jc err
        mov si,ax
        mov di,dx
        pop cx
        pop dx
        mov ax,4200h
        int 21h
        jc err
        mov ax,si
        mov dx,di
        jmp ok
    err:xor ax,ax
        xor dx,dx
    ok:
  END eof_code;
  VAR i: INTEGER;
BEGIN
  IF (f=NULL) OR (f^.mag#mFILE) THEN error:=6; done:=FALSE;
  ELSIF fm_dir IN f^.md THEN error:=5; done:=FALSE; i:=0;
  ELSE i:=eof_code(f^.id); done:=TRUE;
  END;
  exit(f,FALSE);
  RETURN i;
END eof;

PROCEDURE read(f: FILE; mem: ADDRESS; len: INTEGER);
  PROCEDURE read_code(f: CAR16; a: ADDRESS; l: CAR16): INTEGER;
  CODE
        pop cx
        pop dx
        pop bx
        mov ds,dos_s
        mov [0],bx
        pop bx
        mov ah,3Fh
        int 21h
        mov dx,0
        jnc ok
        mov dx,0FFFFh
    ok: seg cs
        mov ds,[0]
  END read_code;
  VAR l,e: INTEGER; buf: ARRAY [0..4095] OF CHAR;
BEGIN
  IF (f=NULL) OR (f^.mag#mFILE) THEN error:=6; done:=FALSE;
  ELSIF fm_dir IN f^.md THEN error:=5; done:=FALSE;
  ELSE
    done:=TRUE;
    WHILE len>0 DO
      IF len>BYTES(buf) THEN l:=BYTES(buf) ELSE l:=len END;
      e:=read_code(f^.id,adr(ADR(buf)),CAR16(l));
      low.move(mem,ADR(buf),l);
      mem:=mem+l; DEC(len,l);
      IF (e<0) & done THEN error:=e; done:=FALSE END;
    END;
  END;
  exit(f,FALSE);
END read;

PROCEDURE write(f: FILE; mem: ADDRESS; len: INTEGER);
  PROCEDURE write_code(f: CAR16; a: ADDRESS; l: CAR16): INTEGER;
  CODE
        pop cx
        pop dx
        pop bx
        mov ds,dos_s
        mov [0],bx
        pop bx
        mov ah,40h
        int 21h
        mov dx,0
        jnc ok
        mov dx,0FFFFh
    ok: seg cs
        mov ds,[0]
  END write_code;
  VAR l,e: INTEGER; buf: ARRAY [0..4095] OF CHAR;
BEGIN
  IF (f=NULL) OR (f^.mag#mFILE) THEN error:=6; done:=FALSE;
  ELSIF fm_dir IN f^.md THEN error:=5; done:=FALSE;
  ELSE
    done:=TRUE;
    WHILE len>0 DO
      IF len>BYTES(buf) THEN l:=BYTES(buf) ELSE l:=len END;
      low.move(ADR(buf),mem,l);
      e:=write_code(f^.id,adr(ADR(buf)),CAR16(l));
      mem:=mem+l; DEC(len,l);
      IF (e<0) & done THEN error:=e; done:=FALSE END;
    END;
  END;
  exit(f,FALSE);
END write;

PROCEDURE fwrite(f: FILE; a: ADDRESS; p,sz: INTEGER);
BEGIN
  write(f,a+p,sz);
END fwrite;

PROCEDURE get(file: FILE; VAR data: ARRAY OF WORD; len: INTEGER);
BEGIN
  read(file,ADR(data),len);
END get;

PROCEDURE put(file: FILE; VAL data: ARRAY OF WORD; len: INTEGER);
BEGIN
  write(file,ADR(data),len);
END put;

PROCEDURE buffers(f: FILE; no,sz: INTEGER);
BEGIN
END buffers;

PROCEDURE dir_walk(f: FILE; sort_kind: INTEGER);
  PROCEDURE start_walk(nm: ADDRESS): INTEGER;
  CODE
        mov ah,4Eh
        pop dx
        pop bx
        mov ds,dos_s
        mov [0],bx
        mov cx,17h
        int 21h
        mov dx,0
        jnc ok
        cmp ax,18
        mov dx,0FFFFh
        jne ok
        mov ah,62h
        int 21h
        mov ds,bx ;  error !!!!!!!!!
        mov BYTE[9Eh],0
        mov ax,0
        mov dx,0
    ok: seg cs
        mov ds,[0]
  END start_walk;
  VAR nm: file_string;
BEGIN
  IF (f=NULL) OR (f^.mag#mFILE) THEN error:=6; done:=FALSE;
  ELSIF NOT (fm_dir IN f^.md) THEN error:=5; done:=FALSE;
  ELSIF walk_activ THEN error:=4; done:=FALSE;
  ELSE
    str.print(nm,'%s\*.*',f^.name);
    error:=start_walk(adr(ADR(nm)));
    done:=error>=0;
  END;
  IF NOT done THEN exit(f,FALSE) END;
END dir_walk;

PROCEDURE get_entry(f: FILE; VAR name: ARRAY OF CHAR;
                             VAR mode: BITSET): BOOLEAN;
  PROCEDURE name_adr(): ADDRESS;
  CODE
        mov ah,62h
        int 21h
        mov dx,bx
        mov ax,9Eh
  END name_adr;
  PROCEDURE attr(): file_mode;
  CODE
        mov ah,62h
        int 21h
        mov es,bx
        seg es
        mov al,[95h]
  END attr;
  PROCEDURE next(): INTEGER;
  CODE
        push ds
        mov ah,62h
        int 21h
        mov ds,bx
        mov dx,80h
        mov ah,4Fh
        int 21h
        mov dx,0
        jnc ok
        cmp ax,18
        mov dx,0FFFFh
        jne ok
        mov ah,62h
        int 21h
        mov ds,bx
        mov BYTE[9Eh],0
        mov ax,0
        mov dx,0
    ok: pop ds
  END next;
  VAR c: file_mode;
BEGIN
  str.print(name,'%s',name_adr());
  IF name='' THEN RETURN FALSE END;
  c:=attr();
  mode:={};
  IF fm_hidden IN c THEN mode:=mode+e_hidden END;
  IF fm_system IN c THEN mode:=mode+e_sys END;
  IF fm_dir IN c THEN mode:=mode+e_dir END;
  error:=next();
  done:=error>=0;
  IF NOT done THEN exit(f,FALSE) END;
  RETURN TRUE;
END get_entry;

PROCEDURE restart_walk(cd: FILE);
BEGIN
  end_walk(cd);
  dir_walk(cd,0);
END restart_walk;

PROCEDURE end_walk (f: FILE);
BEGIN
  exit(f,FALSE);
END end_walk;

PROCEDURE file_name(f: FILE; VAR name: ARRAY OF CHAR);
BEGIN
  done:=(f#NULL) & (f^.mag=mFILE);
  IF done THEN str.print(name,'%s',f^.name)
  ELSE error:=6;
  END;
  exit(f,FALSE);
END file_name;

PROCEDURE close_paths(VAR dirs: PATHs);
BEGIN
  close(dirs);
END close_paths;

PROCEDURE get_paths(VAR dirs: PATHs; VAL envname: ARRAY OF CHAR);
BEGIN
  NEW(dirs);
  dirs^:=cd^;
  done:=TRUE;
END get_paths;

PROCEDURE open_paths(VAR dirs: PATHs; VAL envname: ARRAY OF CHAR);
BEGIN
  NEW(dirs);
  dirs^:=cd^;
  done:=TRUE;
END open_paths;

PROCEDURE lookup(dirs: PATHs; VAR file: FILE; VAL name,mode: ARRAY OF CHAR);
BEGIN
  open(file,name,mode);
END lookup;

BEGIN
  walk_activ:=FALSE;
  gdt_s:=def.gdt_s;
  dos_s:=def.dos_s;
  NEW(cd);
  cd^.mag:=mFILE;
  cd^.md:=file_mode{fm_dir};
  set_name(cd,'');
  NEW(here);
  here^:=cd^;
  cmask:={};
END BIO.
