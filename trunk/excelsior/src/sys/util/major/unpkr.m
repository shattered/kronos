MODULE unpkr; (* John 28-Mar-1990 (c) KRONOS *)

-- Extract pkr data from a disk image to a file.
-- later the files could be combined together to
-- form a valid pkr archive.

IMPORT bio: BIO;
IMPORT tty: Terminal;
IMPORT arg: tskArgs;

PROCEDURE pusage;
BEGIN
  tty.print('unpkr pkr_file destination_file\n');
END pusage;

PROCEDURE check_io(i: INTEGER);
BEGIN
  IF NOT bio.done THEN
    tty.perror(bio.error,'\n"%s" %%s "%s"\n',arg.words[i],bio.ename);
    HALT
  END
END check_io;

PROCEDURE copy(p: bio.FILE; d: bio.FILE);
  VAR buf: ARRAY [0..4095] OF CHAR;
      lab: ARRAY [0..3] OF INTEGER;
      size,no,disks,l: INTEGER;
BEGIN
  bio.seek(p,4096-BYTES(lab),0); check_io(0);
  bio.get (p,lab,BYTES(lab)  );  check_io(0);

  size :=lab[0];
  no   :=lab[1] MOD 100h;
  disks:=(lab[1] MOD 10000h) DIV 100h;
  tty.print('This is disk %d [%d]. It has %d bytes of pkr data\n', no, disks, size);

  bio.extend(d,bio.pos(d)+size); check_io(1);
  WHILE size>0 DO
    IF size>BYTES(buf) THEN l:=BYTES(buf) ELSE l:=size END;
    bio.get(p,buf,l); check_io(0);
    bio.put(d,buf,l); check_io(1);
    DEC(size,l)
  END
END copy;

VAR pkr: bio.FILE;
    dst: bio.FILE;

BEGIN
  IF HIGH(arg.words)<0 THEN pusage; HALT END;

  bio.open(pkr,arg.words[0],'r'); check_io(0);
  bio.create(dst,arg.words[1],'w',0); check_io(1);

  copy(pkr,dst);

  bio.close(pkr);
  bio.close(dst);
END unpkr.
