IMPLEMENTATION MODULE libBIO; (* Sem 09-Feb-91. (c) KRONOS *)

FROM SYSTEM      IMPORT ADDRESS;

IMPORT  io : BIO;
IMPORT  os : osKernel;
IMPORT  lex: Lexicon;
IMPORT  err: libCrash;

VAR
  lock : os.mutex_rec;
  str  : ARRAY [0..79] OF CHAR;

PROCEDURE enter;
BEGIN
  os.acquire(lock);
END enter;

PROCEDURE exit;
BEGIN
  IF NOT io.done THEN
    lex.perror(str,io.error,'%%s');
    err.raise('Неудача при доступе к файлу.\n'
              'Файл   : "%s".\n'
              'Причина: "%s".',io.ename,str);
  END;
  os.release(lock);
END exit;

PROCEDURE open(VAR file: FILE; VAL path,mode: ARRAY OF CHAR);
BEGIN
  enter; io.open(file,path,mode); exit;
END open;

PROCEDURE create(VAR file: FILE; VAL path,mode: ARRAY OF CHAR;
                                      size: INTEGER);
BEGIN
  enter; io.create(file,path,mode,size); exit;
END create;

PROCEDURE close(VAR file: FILE);
BEGIN
  enter; io.close(file); exit;
END close;

PROCEDURE purge(VAR file: FILE);
BEGIN
  enter; io.purge(file); exit;
END purge;

PROCEDURE cut(file: FILE; size: INTEGER);
BEGIN
  enter; io.cut(file,size); exit;
END cut;

PROCEDURE end(file: FILE; size: INTEGER);
BEGIN
  enter; io.end(file,size); exit;
END end;

PROCEDURE extend(file: FILE; size: INTEGER);
BEGIN
  enter; io.extend(file,size); exit;
END extend;

PROCEDURE seek(file: FILE; pos,mode: INTEGER);
BEGIN
  enter; io.seek(file,pos,mode); exit;
END seek;

PROCEDURE eof(file: FILE): INTEGER;
  VAR i: INTEGER;
BEGIN
  enter; i:=io.eof(file); exit; RETURN i;
END eof;

PROCEDURE read(file: FILE; buf: ADDRESS; len: INTEGER);
BEGIN
  enter; io.read(file,buf,len); exit;
END read;

PROCEDURE write(file: FILE; buf: ADDRESS; len: INTEGER);
BEGIN
  enter; io.write(file,buf,len); exit;
END write;

PROCEDURE fread(file: FILE; buf: ADDRESS; pos,len: INTEGER);
BEGIN
  enter; io.fread(file,buf,pos,len); exit;
END fread;

PROCEDURE fwrite(file: FILE; buf: ADDRESS; pos,len: INTEGER);
BEGIN
  enter; io.fwrite(file,buf,pos,len); exit;
END fwrite;

PROCEDURE fopen(cd: FILE; VAR file: FILE; VAL path,mode: ARRAY OF CHAR);
BEGIN
  enter; io.fopen(cd,file,path,mode); exit;
END fopen;

PROCEDURE fcreate(cd: FILE; VAR file: FILE; VAL path,mode: ARRAY OF CHAR;
                                                       size: INTEGER);
BEGIN
  enter; io.fcreate(cd,file,path,mode,size); exit;
END fcreate;

BEGIN
  os.ini_mutex(lock);
END libBIO.
