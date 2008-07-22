IMPLEMENTATION MODULE exHead; (* Leo 28-Jun-87. (c) KRONOS *)

IMPORT  tty: Terminal;
IMPORT  dia: strEditor;
IMPORT  img: Strings;
IMPORT  key: Keyboard;

FROM SYSTEM     IMPORT  WORD;
FROM exScreen   IMPORT  pushandposinfo, pushandclearinfo, pop, showinfo
                      , infoline_pos?, nocolumns;

VAR desc: dia.descriptor;

PROCEDURE ReadCommand;
BEGIN
  dia.edit_str(prompt,command,Home,0,width-1,desc,33c);
  IF desc^.last=33c THEN command:="" END;
END ReadCommand;


PROCEDURE message(wait: BOOLEAN; VAL fmt: ARRAY OF CHAR; SEQ args: WORD);
  VAR str: ARRAY [0..127] OF CHAR;
  ln,i,cl: INTEGER;
        k: CHAR;
BEGIN
  pushandposinfo; ln:=infoline_pos?;
  tty.set_pos(ln,0);
  img.print(str,">>> "); img.append(str,fmt,args);
  i:=0;
  REPEAT
    IF (str[i]=12c) OR (str[i]=15c) THEN str[i]:='\' END;
    i:=i+1;
  UNTIL str[i-1]=0c;
  cl:=nocolumns-1;
  tty.print("%-*.*s",cl,cl,str); tty.erase_line(0);
  IF wait THEN
    tty.set_pos(ln,cl-14);
    tty.print(" PRESS ANY KEY"); key.read(k);
    tty.print("\r"); tty.erase_line(0);
    showinfo;
  END;
  pop;
END message;

PROCEDURE ask(VAL s: ARRAY OF CHAR): CHAR;
  VAR k: CHAR;
BEGIN
  pushandposinfo;
  tty.set_color(+1); tty.print("<<< %s",s); tty.set_color(0);
  tty.erase_line(0); key.read(k);
  tty.print("\r"); tty.erase_line(0);
  showinfo;
  pop;
  RETURN k;
END ask;

PROCEDURE readstr(VAL s: ARRAY OF CHAR; VAR r: ARRAY OF CHAR);
BEGIN
  pushandclearinfo;
  img.print(prompt,"<<< %s",s);
  img.print(command,"%s",r); Home:=infoline_pos?; ReadCommand;
  img.print(r,"%s",command);
  tty.print("\r"); tty.erase_line(0);
  showinfo;
  pop;
END readstr;

PROCEDURE start(s: ARRAY OF CHAR);
BEGIN
  pushandposinfo;
  tty.print("%s",s); tty.erase_line(0);
  pop;
END start;

PROCEDURE finish;
BEGIN
  pushandposinfo; showinfo; pop
END finish;

BEGIN
  alarm:=FALSE;
  Home:=tty.state^.lines-1;  width:=tty.state^.columns-16;
  dia.new(desc,16);
END exHead.
