IMPLEMENTATION MODULE Terminal; (* Ned 25-Aug-89. (c) KRONOS *)

FROM SYSTEM      IMPORT WORD, ADDRESS, ADR;
IMPORT  fmt: Formats;
IMPORT  def: def_GDT;

TYPE
  COLOR = SET OF [0..7];
  CAR16 = def.CAR16;
  BYTE  = def.CAR8;

CONST
  LINES=25;

VAR
  color : COLOR;
  page  : BYTE;
  mode  : BYTE;
  width : BYTE;
  gdt_s : def.selector;
  dos_s : def.selector;

PROCEDURE ws(adr: INTEGER; len: CAR16; color: COLOR);
CODE
        mov bh,page
        mov ah,03h
        int 10h
        pop cx
        mov bl,cl
        pop cx
        pop si
        pop ax
        mov es,dos_s
        seg es
        mov [2],ax
        push bp
        mov bp,si
        mov ax,1301h
        int 10h
        pop bp
END ws;

PROCEDURE ws_sp(len: BYTE);
CODE
        mov bh,page
        mov ah,03h
        int 10h
        mov ax,0920h
        mov bl,color
        pop cx
        mov ch,0
        int 10h
END ws_sp;

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

PROCEDURE Write(c: CHAR);
BEGIN
  ws(adr(ADR(c)),1,color);
END Write;

PROCEDURE WriteString(s: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  IF (HIGH(s)<0) OR (s[0]=0c) THEN RETURN END;
  i:=0;
  REPEAT i:=i+1 UNTIL (i>HIGH(s)) OR (s[i]=0c);
  ws(adr(ADR(s)),CAR16(i),color);
END WriteString;

PROCEDURE write(s: ARRAY OF CHAR; pos,len: INTEGER);
BEGIN
  IF pos>=len THEN RETURN END;
  ws(adr(ADR(s[pos])),CAR16(len-pos),color);
END write;

PROCEDURE write_x(x: WORD; s: ARRAY OF CHAR; pos,len: INTEGER);
BEGIN
  IF pos>=len THEN RETURN END;
  ws(adr(ADR(s[pos])),CAR16(len-pos),color);
END write_x;

PROCEDURE print(VAL format: ARRAY OF CHAR; SEQ args: WORD);
BEGIN
  fmt.format(0,write_x,format,args)
END print;

PROCEDURE WriteLn;
BEGIN
  write("" 15c 12c,0,2)
END WriteLn;

PROCEDURE Show(s: ARRAY OF CHAR);
BEGIN
  WriteString(s); WriteLn
END Show;

PROCEDURE set_pos(line,col: INTEGER);
  PROCEDURE pos(x,y: BYTE);
  CODE
        mov bh,page
        mov ah,02h
        pop cx
        mov dh,cl
        pop cx
        mov dl,cl
        int 10h
  END pos;
BEGIN
  IF line<0 THEN line:=0
  ELSIF line>=LINES THEN line:=LINES-1
  END;
  IF col<0 THEN col:=0
  ELSIF col>INTEGER(width) THEN col:=INTEGER(width)
  END;
  pos(BYTE(col),BYTE(line));
END set_pos;

PROCEDURE get_pos(VAR line,col: INTEGER);
  PROCEDURE pos(): CAR16;
  CODE
        mov bh,page
        mov ah,03h
        int 10h
        mov ax,dx
  END pos;
  VAR i: CAR16;
BEGIN
  col:=INTEGER(i) MOD 100h;
  line:=INTEGER(i>>8) MOD 100h;
END get_pos;

PROCEDURE home;
BEGIN
  set_pos(0,0);
END home;

PROCEDURE bottom;
BEGIN
  set_pos(LINES-1,0);
END bottom;

PROCEDURE repeat(ch: CHAR; times: INTEGER);
  VAR i: INTEGER;
BEGIN
  FOR i:=0 TO times-1 DO Write(ch) END;
END repeat;

PROCEDURE erase(how: INTEGER);
  VAR l,c,i: INTEGER;
BEGIN
  IF NOT (how IN {0..2}) THEN how:=0 END;
  get_pos(l,c);
  CASE how OF
    |0: erase_line(0);
        FOR i:=l+1 TO LINES-1 DO set_pos(i,0); ws_sp(width) END;
    |1: erase_line(1);
        FOR i:=0 TO l-1 DO set_pos(i,0); ws_sp(width) END;
    |2: FOR i:=0 TO LINES-1 DO set_pos(i,0); ws_sp(width) END;
  END;
  set_pos(l,c);
END erase;

PROCEDURE erase_line(how: INTEGER);
  VAR l,c: INTEGER;
BEGIN
  get_pos(l,c);
  CASE how OF
    |1: set_pos(l,0); ws_sp(BYTE(c)); set_pos(l,c);
    |2: set_pos(l,0); ws_sp(width); set_pos(l,c);
  ELSE ws_sp(width-BYTE(c));
  END;
END erase_line;

PROCEDURE set_cursor(n: INTEGER);
BEGIN
END set_cursor;

PROCEDURE set_color(c: INTEGER);
BEGIN
  color:=color-COLOR{0..3}+COLOR(c)*COLOR{0..3};
END set_color;

PROCEDURE set_back (c: INTEGER);
BEGIN
  color:=color-COLOR{4..6}+COLOR(c<<4)*COLOR{4..6};
END set_back;

PROCEDURE set_blinking (ON_OFF: INTEGER);
BEGIN
  IF ON_OFF=0 THEN EXCL(color,7) ELSE INCL(color,7) END;
END set_blinking;

PROCEDURE set_something(ON_OFF: INTEGER);
BEGIN
  IF ON_OFF=0 THEN EXCL(color,7) ELSE INCL(color,7) END;
END set_something;

PROCEDURE get_mode;
CODE
        mov ah,0Fh
        int 10h
        mov mode,al
        mov width,ah
        mov page,bh
END get_mode;

BEGIN
  get_mode;
  color:=COLOR{0..2};
  gdt_s:=def.gdt_s;
  dos_s:=def.dos_s;
END Terminal.
