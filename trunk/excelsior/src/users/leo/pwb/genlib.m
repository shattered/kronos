MODULE genlib; (* 13-Feb-87. (c) KRONOS *)

IMPORT  tty: Terminal;
IMPORT  pcb: pwbWorld;
IMPORT  lex: Lexicon;
IMPORT  str: Strings;

VAR mdl: pcb.MODEL;
      p: pcb.SIGNAL;
    Name: ARRAY [0..31] OF CHAR;

PROCEDURE dip(nm: ARRAY OF CHAR; size: INTEGER; no: INTEGER);
  VAR i: INTEGER; top: pcb.TOPOLOGY;  short: BOOLEAN;
BEGIN
  tty.Show(nm);
  pcb.new_mdl(mdl,nm);
  pcb.mdl_set_size(mdl,(no DIV 2-1)*96,96*size);
  pcb.open(mdl,FALSE);
  FOR i:=0 TO no-1 DO
    str.print(Name,'%d',i+1);
    pcb.new_sig(p,mdl,FALSE,{},Name);
    IF i<(no DIV 2) THEN
      top.x1:=i*96; top.y1:=0;
    ELSE
      top.x1:=(no-i-1)*96; top.y1:=96*size;
    END;
    top.x2:=top.x1; top.y2:=top.y1;
    top.size:=25; top.vsize:=1; top.layer:={0..1};
    top.fixed:=TRUE;
    pcb.ins_range(p,top,TRUE,short); ASSERT(NOT short);
  END;
  pcb.close(mdl,FALSE);
  str.print(Name,"%s.pkg",nm);
  pcb.write_mdl(mdl,Name);
  IF NOT pcb.done THEN lex.perror(Name,pcb.error,'%%s'); tty.print('%s\n',Name) END;
END dip;

PROCEDURE sip(nm: ARRAY OF CHAR; size: INTEGER; no: INTEGER);
  VAR i: INTEGER; top: pcb.TOPOLOGY; short: BOOLEAN;
BEGIN
  tty.Show(nm);
  pcb.new_mdl(mdl,nm);
  pcb.mdl_set_size(mdl,(no-1)*96,96*size);
  pcb.open(mdl,FALSE);
  FOR i:=0 TO no-1 DO
    str.print(Name,'%d',i+1);
    pcb.new_sig(p,mdl,FALSE,{},Name);
    top.x1:=i*96; top.y1:=0;
    top.x2:=top.x1; top.y2:=top.y1;
    top.size:=25; top.vsize:=1; top.layer:={0..1};
    top.fixed:=TRUE;
    pcb.ins_range(p,top,TRUE,short);
  END;
  pcb.close(mdl,FALSE);
  str.print(Name,"%s.pkg",nm);
  pcb.write_mdl(mdl,Name);
  IF NOT pcb.done THEN lex.perror(Name,pcb.error,'%%s'); tty.print('%s\n',Name) END;
END sip;

PROCEDURE matrix(nm: ARRAY OF CHAR; pins,lines: INTEGER);
  VAR l,k: INTEGER; top: pcb.TOPOLOGY; short: BOOLEAN;
BEGIN
  tty.Show(nm);
  pcb.new_mdl(mdl,nm);
  pcb.mdl_set_size(mdl,(pins-1)*96,(lines-1)*96);
  pcb.open(mdl,FALSE);
  FOR l:=0 TO lines-1 DO
    FOR k:=0 TO pins-1 DO
      str.print(Name,'%d',l*pins+k+1);
      pcb.new_sig(p,mdl,FALSE,{},Name);
      top.x1:=k*96; top.y1:=(lines-1-l)*96;
      top.x2:=top.x1; top.y2:=top.y1;
      top.size:=25; top.vsize:=1; top.layer:={0..1};
      top.fixed:=TRUE;
      pcb.ins_range(p,top,TRUE,short);
    END;
  END;
  pcb.close(mdl,FALSE);
  str.print(Name,"%s.pkg",nm);
  pcb.write_mdl(mdl,Name);
  IF NOT pcb.done THEN lex.perror(Name,pcb.error,'%%s'); tty.print('%s\n',Name) END;
END matrix;

PROCEDURE rh(nm: ARRAY OF CHAR; size: INTEGER);
  VAR i: INTEGER; top: pcb.TOPOLOGY;  short: BOOLEAN;
BEGIN
  tty.Show(nm);
  pcb.new_mdl(mdl,nm);
  pcb.mdl_set_size(mdl,96*size,96);
  pcb.open(mdl,FALSE);
  FOR i:=0 TO 1 DO
    str.print(Name,'%d',i+1);
    pcb.new_sig(p,mdl,FALSE,{},Name);
    top.x1:=i*size*96; top.y1:=0;
    top.x2:=top.x1; top.y2:=top.y1;
    top.size:=25; top.vsize:=1; top.layer:={0..1};
    top.fixed:=TRUE;
    pcb.ins_range(p,top,TRUE,short);
  END;
  pcb.close(mdl,FALSE);
  str.print(Name,"%s.pkg",nm);
  pcb.write_mdl(mdl,Name);
  IF NOT pcb.done THEN lex.perror(Name,pcb.error,'%%s'); tty.print('%s\n',Name) END;
END rh;

PROCEDURE pln(nm: ARRAY OF CHAR; size: INTEGER; no: INTEGER);
  VAR i: INTEGER; top: pcb.TOPOLOGY;  short: BOOLEAN;
BEGIN
  tty.Show(nm);
  pcb.new_mdl(mdl,nm);
  pcb.mdl_set_size(mdl,((no DIV 2 + 1) DIV 2 - 1)*96,96*size);
  pcb.open(mdl,FALSE);
  FOR i:=0 TO no-1 DO
    str.print(Name,'%d',i+1);
    pcb.new_sig(p,mdl,FALSE,{},Name);
    IF i<(no DIV 2) THEN
      top.x1:=i*48; top.y1:=((i+1) MOD 2)*96;
    ELSE
      top.x1:=(no-i-1)*48; top.y1:=96*(size-i MOD 2);
    END;
    top.x2:=top.x1; top.y2:=top.y1;
    top.size:=25; top.vsize:=1; top.layer:={0..1};
    top.fixed:=TRUE;
    pcb.ins_range(p,top,TRUE,short);
  END;
  pcb.close(mdl,FALSE);
  str.print(Name,"%s.pkg",nm);
  pcb.write_mdl(mdl,Name);
  IF NOT pcb.done THEN lex.perror(Name,pcb.error,'%%s'); tty.print('%s\n',Name) END;
END pln;

BEGIN
  sip('SIP8',1,8);
  matrix('SNP59',32,3);
  dip('DIP8',3,8);
  dip('DIP14',3,14);
  dip('DIP16',3,16);
  dip('DIP18',3,18);
  dip('DIP20',3,20);
  dip('DIP24',6,24);
  dip('DIP28',6,28);
  dip('DIP40',6,40);
  sip('SIP10',1,10);
  pln('PLN42',10,42);
  pln('PLN48',10,48);
  sip('MLT125',1,2);
  sip('STR3',1,3);
  sip('PIN',1,1);
  rh('RH',4);
  sip('STR32',1,32);
  dip('DIP64',9,64);
  dip('CON6',1,6);
  dip('DIP18W',4,18);
END genlib.
