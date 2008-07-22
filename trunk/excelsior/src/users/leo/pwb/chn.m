MODULE chain; (* 08-Feb-87. (c) KRONOS *)

FROM SYSTEM    IMPORT   ADDRESS;
FROM Chain     IMPORT   Execute;
IMPORT  str: Strings;
IMPORT  pcb: pedModel;
IMPORT  arg: tskArgs;
IMPORT  tty: Terminal;
IMPORT  std: StdIO;
IMPORT  key: Keyboard;

VAR Name1,Name2: ARRAY [0..31] OF CHAR;
    model: pcb.MDL;
    Poz: INTEGER;

PROCEDURE mk(n: INTEGER): INTEGER;
BEGIN
  RETURN n*25000 DIV 960
END mk;

PROCEDURE pce;
BEGIN
  IF NOT pcb.done THEN HALT(pcb.error) END
END pce;

PROCEDURE VisChips;
  VAR cit,sit: pcb.ITERMDL;
     epin,sig: pcb.SIGNAL;
         chip: pcb.MDL;
         type: pcb.MDL;
           tp: BITSET;
        ln,en: STRING;
        x,y,r: INTEGER;
BEGIN
  pcb.mdl_get_size(model,x,y); pce;
  std.print(': %d %d\n',mk(x),mk(y));
  IF pcb.first_chip(chip,cit,model) THEN
    REPEAT
      pce;
      pcb.chip_extract(chip,ln,en); pce;
      std.print('%15s',ln);
      pcb.chip_type(chip,type);     pce;
      pcb.mdl_extract(type,ln);     pce;
      std.print(' - %-15s',ln);
      pcb.chip_pos(chip,x,y,r);     pce;
      std.print(' : %d %d %d\n',mk(x),mk(y),r);
      Poz:=0;
      IF pcb.first_epin(epin,sit,type) THEN
        REPEAT
          pce;
          pcb.sig_extract(epin,tp,en); pce;
          pcb.tied_to(sig,chip,en);    pce;
          pcb.sig_extract(sig,tp,ln);  pce;
          IF Poz>3 THEN Poz:=0; std.WriteLn END;
          std.print('%3s-%-15s',en,ln);
          INC(Poz)
        UNTIL NOT pcb.next_epin(epin,sit)
      END;
      pce;
      IF Poz>0 THEN std.print('\n') END;
      std.print(';\n')
    UNTIL NOT pcb.next_chip(chip,cit)
  END;
  pce;
  std.print('$\n')
END VisChips;

PROCEDURE Help;
BEGIN
  tty.WriteLn;
  tty.Show('chain <file name> [-cvit]'); HALT
END Help;

VAR create, vis, insert, trees: BOOLEAN;
    c: CHAR;

PROCEDURE VisTrees;
  VAR i,Poz: INTEGER;
      it : pcb.ITERMDL;
      sit: pcb.ITERSIG;
      name,l_n,e_n: STRING;
      sig : pcb.SIGNAL;
      chip: pcb.MDL;
      y: BITSET;
BEGIN
  IF pcb.first_sig(sig,it,model) THEN
    REPEAT
      pcb.sig_extract(sig,y,name);
      std.print('%s\n',name);
      Poz:=0;
      IF pcb.first_tied(name,chip,sit,sig) THEN
        REPEAT
          pcb.chip_extract(chip,l_n,e_n);
          IF Poz>3 THEN Poz:=0; std.WriteLn END;
          IF Poz=0 THEN INC(Poz); std.WriteString('        ') END;
          std.print('%4s %-14s',name,l_n);
          INC(Poz);
        UNTIL NOT pcb.next_tied(name,chip,sit);
      END;
      IF Poz>0 THEN std.WriteLn END;
      FOR i:=0 TO 67 DO std.Write('-') END; std.WriteLn;
    UNTIL NOT pcb.next_sig(sig,it);
  END;
END VisTrees;

PROCEDURE qwery(VAL str: ARRAY OF CHAR): BOOLEAN;
BEGIN
  tty.print('%s',str);
  LOOP
    key.read(c);
    IF CAP(c)='N' THEN RETURN FALSE END;
    IF CAP(c)='Y' THEN EXIT END;
  END;
  tty.print('%c\n',c); RETURN TRUE
END qwery;

BEGIN
  create:=arg.flag('-','c'); vis:=arg.flag('-','v');
  insert:=arg.flag('-','i'); trees:=arg.flag('-','t');
  IF HIGH(arg.words)<0 THEN Help; HALT END;
  Name1:=arg.words[0]; Name2:=Name1; str.append(Name1,'.con');
  IF create THEN
    IF NOT qwery('Create new model ?') THEN HALT END;
    pcb.new_mdl(model,Name2); ASSERT(pcb.done);
  ELSE
    pcb.read_mdl(model,Name2);
    IF NOT pcb.done THEN tty.perror(pcb.error,'%s %%s\n',Name2); HALT END;
  END;
  IF create OR insert OR NOT (vis OR trees) THEN
    IF Execute(model,Name1)=0 THEN
      IF NOT qwery('Write modified model ?') THEN HALT END;
      pcb.write_mdl(model);          ASSERT(pcb.done);
    END;
  END;
  IF vis   THEN VisChips END;
  IF trees THEN VisTrees END;
END chain.
