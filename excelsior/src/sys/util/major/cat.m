MODULE cat;     (* Andy 15-Jan-90. (c) KRONOS *)

                IMPORT     bio: BIO;
                IMPORT     std: StdIO;
                IMPORT    args: tskArgs;
                IMPORT    defs: defErrors;
                IMPORT     tty: Terminal;   -- FOR ERRORS REPORT ONLY

VAR in,out: bio.FILE;

PROCEDURE serr(VAL info: ARRAY OF CHAR);
BEGIN
  tty.perror(bio.error,'\ncat: %s - %%s\n',info); HALT(bio.error);
END serr;

PROCEDURE cat_file;
  CONST bsz=512;
  VAR  buf: ARRAY [0..bsz-1] OF CHAR;
      size,req: INTEGER;
BEGIN
  size:=bio.eof(in);        IF NOT bio.done THEN serr('get length') END;
  WHILE size>0 DO
     IF size>bsz THEN req:=bsz ELSE req:=size END;
     bio.get(in,buf,req);  IF NOT bio.done THEN serr('read')  END;
     bio.put(out,buf,req); IF NOT bio.done THEN serr('write') END;
     DEC(size,req)
  END;
END cat_file;

PROCEDURE cat_serial;
  VAR ch: CHAR;
BEGIN
  LOOP
      bio.getch(in,ch);
      IF NOT bio.done THEN
          IF bio.error=defs.no_data THEN RETURN END;
          serr('read');
      END;
      bio.putch(out,ch);
      IF NOT bio.done THEN serr('write') END;
  END;
END cat_serial;

PROCEDURE help;
BEGIN
  std.print('    "cat"  concatenate files utility programm    (c) KRONOS\n'
            'usage:\n'
            '     cat  {file_name} [-h]\n');
END help;

VAR i: INTEGER;

BEGIN
  IF args.flag('-','h') THEN help; HALT END;
  in :=std.in;
  out:=std.out;
  IF HIGH(args.words)<0 THEN
    IF std.is_tty(in) THEN cat_serial ELSE cat_file END
  ELSE
    FOR i:=0 TO HIGH(args.words) DO
      bio.open(in,args.words[i],'r');
      IF NOT bio.done THEN serr(args.words[i]) END;
      IF std.is_tty(in) THEN cat_serial ELSE cat_file END;
      bio.close(in);
      IF NOT bio.done THEN serr(args.words[i]) END
    END
  END
END cat.
