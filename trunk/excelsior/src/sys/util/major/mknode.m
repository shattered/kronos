MODULE mknode; (* Leo 27-Dec-89. (c) KRONOS *)
               (* Leg 23-Nov-90. (c) KRONOS *)

IMPORT  bio: BIO;
IMPORT  arg: tskArgs;
IMPORT  std: StdIO;
IMPORT  tty: Terminal;

PROCEDURE pusage;
BEGIN
  std.print('  "mknode"  make device node utility program    (c) KRONOS\n');
  std.print('run\n');
  std.print('   mknode   path device_name ["pro="pro0 | {pro1}]\n');
  std.print('\n');
  std.print('   pro0 = P U R W X R W X R W X \n');
  std.print('   P    = ("P" | "p" | "-" | "+")\n');
  std.print('   U    = ("U" | "u" | "-" | "+")\n');
  std.print('   R    = ("R" | "r" | "-" | "+")\n');
  std.print('   W    = ("W" | "w" | "-" | "+")\n');
  std.print('   X    = ("X" | "x" | "-" | "+")\n');
  std.print('   pro1 = ("o"|"a"|"g") {("+"|"-") ("r"|"w"|"x"|"p"|"u")}\n');
  std.print('\n');
  std.print("                                     Leg, 23-Nov-90\n");
END pusage;

PROCEDURE unpack_pro(VAR s: ARRAY OF CHAR; p: BITSET);
  VAR i,j: INTEGER;
BEGIN
  s:="purwxrwxrwx";
  IF p*bio.run_priv={} THEN s[0]:='-' END;
  IF p*bio.run_uid ={} THEN s[1]:='-' END;
  FOR j:=0 TO 2 DO
    FOR i:=0 TO 2 DO
      IF i IN p THEN s[j*3+2+2-i]:='-' END
    END;
    p:=p>>4
  END
END unpack_pro;

PROCEDURE cap(ch: CHAR): CHAR;
BEGIN
  IF (ch>=141c) & (ch<=172c) THEN RETURN CHAR(ORD(ch)-40b) END;
  IF (ch>=300c) & (ch<=337c) THEN RETURN CHAR(ORD(ch)+40b) END;
  RETURN ch
END cap;

CONST PATT = 'purwxrwxrwx';

PROCEDURE pars_I(VAR pro: BITSET; w: ARRAY OF CHAR);
  VAR i,j: INTEGER;
      set: BITSET;
      run: BITSET;
      len: INTEGER;
      pos: INTEGER;

  PROCEDURE ilg;
  BEGIN
    tty.print('  Illegal specification           : "%s"\n'
              '  must be "%c" or "%c" or "-" or "+":  %*c\n',
                 w,PATT[pos],cap(PATT[pos]),pos+1,'^');
    HALT(1)
  END ilg;

BEGIN
  len:=0; WHILE (len<=HIGH(w)) & (w[len]#0c) DO INC(len) END;
  IF len<11 THEN pos:=len; ilg END;
  set:={0..11}; run:={}; pos:=0;
  IF (cap(w[pos])=cap(PATT[pos])) OR (w[pos]='+') THEN
    run:=run+bio.run_priv
  ELSIF  w[pos] #'-' THEN ilg END; INC(pos);
  IF (cap(w[pos])=cap(PATT[pos])) OR (w[pos]='+')  THEN
    run:=run+bio.run_uid
  ELSIF  w[pos] #'-' THEN ilg END; INC(pos);
  FOR i:=0 TO 2 DO
    FOR j:=0 TO 2 DO
      CASE j OF
        |0: IF (cap(w[pos])=cap(PATT[pos])) OR (w[pos]='+') THEN
              set:=set-bio.own_read
            ELSIF  w[pos] #'-' THEN ilg END; INC(pos);
        |1: IF (cap(w[pos])=cap(PATT[pos])) OR (w[pos]='+') THEN
              set:=set-bio.own_write
            ELSIF  w[pos] #'-' THEN ilg END; INC(pos);
        |2: IF (cap(w[pos])=cap(PATT[pos])) OR (w[pos]='+') THEN
              set:=set-bio.own_exec
            ELSIF  w[pos] #'-' THEN ilg END; INC(pos);
      END;
    END;
    set:=set>>4;
  END;
  set:=set<<12;
  pro:=run+set;
END pars_I;

PROCEDURE pars_II(VAR pro: BITSET; w: ARRAY OF CHAR);
  VAR x: BOOLEAN;
    i,j: INTEGER;
    set: BITSET;
    run: BITSET;
  shift: INTEGER;

  PROCEDURE ilg;
  BEGIN tty.print('chmode: illegal specification "%s"\n',w); HALT(1) END ilg;

BEGIN
  IF HIGH(w)<2 THEN ilg END;
  CASE w[0] OF
    |'o': shift:=0
    |'g': shift:=1
    |'a': shift:=2
  ELSE
    ilg
  END;
  IF (w[1]#'+') & (w[1]#'-') THEN ilg END;
  x:=(w[1]='-');
  set:={}; run:={};
  j:=2;
  WHILE (j<=HIGH(w)) & (w[j]#0c) DO
    CASE cap(w[j]) OF
      |'R': set:=set+bio.own_read
      |'W': set:=set+bio.own_write
      |'X': set:=set+bio.own_exec
      |'P': run:=run+bio.run_priv
      |'U': run:=run+bio.run_uid
    ELSE
      ilg
    END;
    INC(j)
  END;
  set:=set<<(shift*4);
  IF x THEN pro:=pro-run ELSE pro:=pro+run END;
  IF x THEN pro:=pro+set ELSE pro:=pro-set END;
END pars_II;

VAR i: INTEGER;
 mask: BITSET;
  pro: ARRAY [0..15] OF CHAR;
    s: STRING;

BEGIN
  IF HIGH(arg.words)<1 THEN pusage; HALT END;
  IF arg.flag('-','h') THEN pusage; HALT END;
  IF arg.string('pro',s) THEN pars_I(mask,s)
  ELSE
    mask:=bio.own_read+bio.own_write+bio.own_exec
         +bio.gro_read+bio.gro_write+bio.gro_exec
         +bio.oth_read+bio.oth_write+bio.oth_exec;
    mask:={0..11}/mask;
    FOR i:=2 TO HIGH(arg.words) DO pars_II(mask,arg.words[i]) END;
  END;
  bio.chcmask(mask);
  IF NOT bio.done THEN
    unpack_pro(pro,mask);
    tty.perror(bio.error,'set_mask("%s"): %%s\n',pro); HALT(bio.error)
  END;
  bio.mknode(arg.words[0],arg.words[1]);
  IF NOT bio.done THEN
    tty.perror(bio.error,'mknode("%s","%s"): %%s\n',arg.words[0],arg.words[1]);
    HALT(bio.error)
  END
END mknode.
