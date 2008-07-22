MODULE re; (* Ned 20-Dec-88. (c) KRONOS *)

(* NS32032 re-assembler *)

IMPORT      SYSTEM;
IMPORT arg: tskArgs;
IMPORT ou : StdIO;
IMPORT tty: Terminal;
IMPORT in : BIO;
IMPORT ns : visNS;
IMPORT mem: Heap;

WITH STORAGE : mem;

VAR
  -- code = info <глобалы> <код>
  info  :  RECORD
    version  : INTEGER;   -- версия кодофайла
    def_time : INTEGER;   -- время компиляции definition
    imp_time : INTEGER;   -- время компиляции implementation
    glob_size: INTEGER;   -- размер глобалов
    code_size: INTEGER;   -- размер кода
    min_stack: INTEGER;   -- мин. стек для глоб. мультизначений
    add_stack: INTEGER;   -- дополнительный стек
    reserv_07: INTEGER;
    reserv_08: INTEGER;
    reserv_09: INTEGER;
    reserv_0A: INTEGER;
    reserv_0B: INTEGER;
    reserv_0C: INTEGER;
    reserv_0D: INTEGER;
    reserv_0E: INTEGER;
    reserv_0F: INTEGER;
  END;
  glo : DYNARR OF INTEGER;
  inp : in.FILE;
  str : ARRAY [0..255] OF CHAR;
  len : INTEGER;

PROCEDURE chk_in;
BEGIN
  IF NOT in.done THEN tty.print('IO error.\n'); HALT(1) END;
END chk_in;

PROCEDURE get(): INTEGER;
  VAR n: INTEGER;
BEGIN
  n:=0; in.read(inp,SYSTEM.ADR(n),1); chk_in;
  str[len]:=CHAR(n); INC(len);
  RETURN n;
END get;

VAR i: INTEGER;

BEGIN
  IF HIGH(arg.words)<0 THEN
    tty.print('re {<file name>}\n'); HALT;
  END;
  i:=0;
  REPEAT
    in.open(inp,arg.words[i],'r'); chk_in;
    IF arg.flag('-','r') THEN
      ns.ini(0F80000h,get);
      LOOP
        len:=0;
        ns.vis_ins;
        ou.print(' %');
        FOR i:=0 TO len-1 DO ou.print(' %$2h',str[i]) END;
        ou.print(' ');
        FOR i:=0 TO len-1 DO
          IF (str[i]>=' ') & (str[i]<='z') THEN
            ou.print('%c',str[i])
          ELSE
            ou.print('.')
          END;
        END;
        ou.print('\n');
      END;
    ELSE
      in.read(inp,SYSTEM.ADR(info),BYTES(info)); chk_in;
      ou.print('globals: %4d bytes\n',info.glob_size);
      ou.print('code   : %4d bytes\n',info.code_size);
      NEW(glo,(info.glob_size+3) DIV 4);
      in.read(inp,SYSTEM.ADR(glo),info.glob_size); chk_in;

      FOR i:=0 TO HIGH(glo) DO
        ou.print('%$6h: %$8h\n',i*4,glo[i]);
      END;
      ns.ini(0,get);
      WHILE ns.cp<info.code_size DO
        len:=0; ns.vis_ins; ou.print('\n');
      END;

    END;
    in.close(inp); INC(i);
  UNTIL i>HIGH(arg.words);
END re.
