IMPLEMENTATION MODULE StdIO;

IMPORT Strings, MSDOS, lowLevel;

(*# save *)
(*# debug(vid=>full) *)
(*# call(o_a_copy=>off,o_a_size=>on) *)

CONST
  str ::= Strings;
  dos ::= MSDOS;
  low ::= lowLevel;

PROCEDURE read (data: ADDRESS; pos,len: CARDINAL);
  VAR i: CARDINAL; err: BOOLEAN;
BEGIN
  i:=dos.fread(0,[Seg(data):Ofs(data)+pos],len,err)
END read;

PROCEDURE write(data: ADDRESS; pos,len: CARDINAL);
  VAR i,seg,ofs: CARDINAL; err: BOOLEAN;
BEGIN
  seg:=Seg(data^);
  ofs:=Ofs(data^)+pos;
  data:=[seg:ofs];
  i:=dos.fwrite(1,data,len,err)
END write;

PROCEDURE get(VAR data: ARRAY OF BYTE; pos,len: CARDINAL);
BEGIN
  read(ADR(data),pos,len)
END get;

PROCEDURE put(    data: ARRAY OF BYTE; pos,len: CARDINAL);
BEGIN
  write(ADR(data),pos,len)
END put;


PROCEDURE getc(VAR ch: BYTE);
BEGIN
  read(ADR(ch),0,1)
END getc;

PROCEDURE putc(    ch: BYTE);
BEGIN
  write(ADR(ch),0,1)
END putc;

PROCEDURE getw(VAR w: WORD);
BEGIN
  read(ADR(w),0,2)
END getw;

PROCEDURE putw(    w: WORD);
BEGIN
  write(ADR(w),0,2)
END putw;

PROCEDURE get32(VAR w: LONGWORD);
BEGIN
  read(ADR(w),0,4)
END get32;

PROCEDURE put32(    w: LONGWORD);
BEGIN
  write(ADR(w),0,4)
END put32;

PROCEDURE gets(VAR data: ARRAY OF CHAR);
  VAR i: CARDINAL;
     ch: CHAR;
BEGIN
  i:=0;
  LOOP
    getc(ch);
    IF    ch=15C THEN (* nothing *)
    ELSIF ch=12C THEN EXIT
    ELSIF ch=36C THEN EXIT
    ELSIF i<HIGH(data) THEN
      data[i]:=ch; INC(i)
    END
  END;
  IF i<=HIGH(data) THEN data[i]:=0C END
END gets;

PROCEDURE puts(data: ARRAY OF CHAR);
  VAR i: CARDINAL;
BEGIN
  i:=str.len(data);
  write(ADR(data),0,i)
END puts;


(*# save, call(var_arg=>on,c_conv=>on,reg_param=>()) *)
PROCEDURE print(fmt: ARRAY OF CHAR);
(*# restore *)
  VAR seg,ofs: CARDINAL;  bump: ARRAY [0..511] OF CHAR;
BEGIN
  seg:=low.ss();  ofs:=low.bp()+14;
  str.format(bump,fmt,[seg:ofs]);
  puts(bump)
END print;

(*# restore *)
END StdIO.