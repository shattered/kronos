MODULE txc; (* Leg 02-May-90. (c) KRONOS *)

IMPORT  bio: BIO;
IMPORT  arg: tskArgs;
IMPORT  asc: ASCII;
IMPORT  tty: Terminal;
IMPORT  std: StdIO;

CONST pc = ARRAY OF CHAR {
  000c,001c,002c,003c,004c,005c,006c,007c,010c,011c,012c,013c,014c,015c,
  016c,017c,020c,021c,022c,023c,024c,025c,026c,027c,030c,031c,032c,033c,
  034c,035c,036c,037c,040c,041c,042c,043c,044c,045c,046c,047c,050c,051c,
  052c,053c,054c,055c,056c,057c,060c,061c,062c,063c,064c,065c,066c,067c,
  070c,071c,072c,073c,074c,075c,076c,077c,100c,101c,102c,103c,104c,105c,
  106c,107c,110c,111c,112c,113c,114c,115c,116c,117c,120c,121c,122c,123c,
  124c,125c,126c,127c,130c,131c,132c,133c,134c,135c,136c,137c,140c,141c,
  142c,143c,144c,145c,146c,147c,150c,151c,152c,153c,154c,155c,156c,157c,
  160c,161c,162c,163c,164c,165c,166c,167c,170c,171c,172c,173c,174c,175c,
  176c,177c,200c,201c,202c,203c,204c,205c,206c,207c,210c,211c,212c,213c,
  214c,215c,216c,217c,220c,221c,222c,223c,224c,225c,226c,227c,230c,231c,
  232c,233c,234c,235c,236c,237c,240c,241c,242c,243c,244c,245c,246c,247c,
  250c,251c,252c,253c,254c,255c,256c,257c,260c,261c,262c,263c,264c,265c,
  266c,267c,270c,271c,272c,273c,274c,275c,276c,277c,356c,240c,241c,346c,
  244c,245c,344c,243c,345c,250c,251c,252c,253c,254c,255c,256c,257c,357c,
  340c,341c,342c,343c,246c,242c,354c,353c,247c,350c,355c,351c,347c,352c,
  236c,200c,201c,226c,204c,205c,224c,203c,225c,210c,211c,212c,213c,214c,
  215c,216c,217c,237c,220c,221c,222c,223c,206c,202c,234c,233c,207c,230c,
  235c,231c,227c,232c};

CONST kr = ARRAY OF CHAR {
  000c,001c,002c,003c,004c,005c,006c,007c,010c,011c,012c,013c,014c,015c,
  016c,017c,020c,021c,022c,023c,024c,025c,026c,027c,030c,031c,032c,033c,
  034c,035c,036c,037c,040c,041c,042c,043c,044c,045c,046c,047c,050c,051c,
  052c,053c,054c,055c,056c,057c,060c,061c,062c,063c,064c,065c,066c,067c,
  070c,071c,072c,073c,074c,075c,076c,077c,100c,101c,102c,103c,104c,105c,
  106c,107c,110c,111c,112c,113c,114c,115c,116c,117c,120c,121c,122c,123c,
  124c,125c,126c,127c,130c,131c,132c,133c,134c,135c,136c,137c,140c,141c,
  142c,143c,144c,145c,146c,147c,150c,151c,152c,153c,154c,155c,156c,157c,
  160c,161c,162c,163c,164c,165c,166c,167c,170c,171c,172c,173c,174c,175c,
  176c,177c,341c,342c,367c,347c,344c,345c,366c,372c,351c,352c,353c,354c,
  355c,356c,357c,360c,362c,363c,364c,365c,346c,350c,343c,376c,373c,375c,
  377c,371c,370c,374c,340c,361c,301c,302c,327c,307c,304c,305c,326c,332c,
  311c,312c,313c,314c,315c,316c,317c,320c,260c,261c,262c,263c,264c,265c,
  266c,267c,270c,271c,272c,273c,274c,275c,276c,277c,300c,301c,302c,303c,
  304c,305c,306c,307c,310c,311c,312c,313c,314c,315c,316c,317c,320c,321c,
  322c,323c,324c,325c,326c,327c,330c,331c,332c,333c,334c,335c,336c,337c,
  322c,323c,324c,325c,306c,310c,303c,336c,333c,335c,337c,331c,330c,334c,
  300c,321c,360c,361c,362c,363c,364c,365c,366c,367c,370c,371c,372c,373c,
  374c,375c,376c,377c};

PROCEDURE check;
BEGIN
  IF bio.done THEN RETURN END;
  tty.perror(bio.error,'"%s": %%s\n',bio.ename); HALT(1)
END check;

VAR r_fil: bio.FILE;
    r_buf: ARRAY [0..4096*4-1] OF CHAR;
    r_pos: INTEGER;
    r_bp : INTEGER;
    r_eof: INTEGER;

CONST r_hb=HIGH(r_buf)+1;

PROCEDURE init_read;
  VAR len: INTEGER;
BEGIN
  r_bp:=0; r_pos:=0; r_eof:=bio.eof(r_fil);
  len:=r_eof-r_pos; IF len>r_hb THEN len:=r_hb END;
  bio.get(r_fil,r_buf,len); check
END init_read;

PROCEDURE get(VAR ch: CHAR): BOOLEAN;
  VAR len: INTEGER;
BEGIN
  IF r_pos=r_eof THEN RETURN FALSE END;
  ch:=r_buf[r_bp]; INC(r_bp); INC(r_pos);
  IF r_bp>HIGH(r_buf) THEN
    len:=r_eof-r_pos;
    IF len>r_hb THEN len:=r_hb END;
    bio.get(r_fil,r_buf,len); check; r_bp:=0
  END;
  RETURN TRUE
END get;

VAR w_fil: bio.FILE;
    w_buf: ARRAY [0..4096*4-1] OF CHAR;
    w_bp : INTEGER;

CONST w_hb=HIGH(w_buf);

PROCEDURE init_write;
BEGIN w_bp:=0 END init_write;

PROCEDURE put(ch: CHAR);
BEGIN
  w_buf[w_bp]:=ch; INC(w_bp);
  IF w_bp>w_hb THEN bio.put(w_fil,w_buf,w_hb); check; w_bp:=0 END;
END put;

PROCEDURE finish;
BEGIN
  IF w_bp#0 THEN bio.put(w_fil,w_buf,w_bp); w_bp:=0; check END
END finish;

PROCEDURE help;
BEGIN
  std.print(
  '    "txc"  text converter utility   (c) KRONOS\n'
  ' usage:\n'
  '     txc input_file_name [output_file_name] -F|-T -p\n\n'
  '       -F  From any text form to KRONOS text form\n');
  std.print(
  '       -T  from KRONOS text form To any text form\n\n'
  '       -p  converting KRONOS <--> IBM PC\n\n'
  '                                         Leg, 02-05-90\n');
  HALT;
END help;

PROCEDURE kr_pc;
  VAR c: CHAR;
BEGIN
  WHILE get(c) DO
    IF (c=asc.NL) OR (c=12c) OR (c=0c) OR ((c=15c) & (r_buf[r_bp]#12c)) THEN
      put(15c); c:=12c;
    ELSIF c>177c THEN c:=pc[ORD(c)]
    END;
    put(c)
  END;
END kr_pc;

PROCEDURE pc_kr;
  VAR c: CHAR;
BEGIN
  WHILE get(c) DO
    IF c>177c THEN c:=kr[ORD(c)]
    ELSIF (c=0c) OR (c=12c) THEN c:=asc.NL
    ELSIF c=15c THEN
      IF (r_buf[r_bp]=12c) & get(c) THEN END;
      c:=asc.NL
    END;
    put(c)
  END;
END pc_kr;

PROCEDURE conv(to: PROC);
  VAR i: INTEGER;
BEGIN
  i:=0;
  IF HIGH(arg.words)>0 THEN i:=1 END;
  bio.open(r_fil,arg.words[0],'r');     check;
  bio.create(w_fil,arg.words[i],'w',0); check;
  init_read; init_write;
  to; finish;
  bio.close(w_fil); check;
  bio.close(r_fil);
END conv;

VAR To,From: BOOLEAN;

BEGIN
  IF HIGH(arg.words)<0 THEN help END;
  To  :=arg.flag('-','T');
  From:=arg.flag('-','F');
  IF To & From THEN
    tty.print('Illegal flags combination: -T & -F\n');
    HALT
  END;
  IF    To THEN
    IF arg.flag('-','p') THEN conv(kr_pc)
    ELSE help
    END;
  ELSIF From THEN
    IF arg.flag('-','p') THEN conv(pc_kr)
    ELSE help
    END;
  ELSE help
  END;
END txc.
