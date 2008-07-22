MODULE t; (*  15-Nov-92. (c) KRONOS *)

IMPORT  BIO, tty: Terminal, SYSTEM;

VAR f: BIO.FILE;

CONST size=6*1024*1024;

BEGIN
  BIO.create( f, "/DATA.NET","rw",size );
  IF NOT BIO.done THEN HALT(BIO.error) END;
  tty.print(" eof = %d\n",BIO.eof(f));
  BIO.end(f,size);
  IF NOT BIO.done THEN HALT(BIO.error) END;
  tty.print(" eof = %d\n",BIO.eof(f));
  BIO.seek(f,-2,2);
  IF NOT BIO.done THEN HALT(BIO.error) END;
  BIO.write(f, SYSTEM.ADR(f),2);
  IF NOT BIO.done THEN HALT(BIO.error) END;
  BIO.close(f);
  IF NOT BIO.done THEN HALT(BIO.error) END;
END t.
