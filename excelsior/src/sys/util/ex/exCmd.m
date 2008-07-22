IMPLEMENTATION MODULE exCmd; (* Leo 05-Jul-87. (c) KRONOS *)
                             (* Ned 16-Nov-87. (c) KRONOS *)

IMPORT exMem, exMain, exHead;

IMPORT   arg: tskArgs;
IMPORT   sed: strEditor;
IMPORT  term: Terminal;
IMPORT  kbrd: Keyboard;
IMPORT  face: myEditor;
IMPORT   str: Strings;
IMPORT   mac: exMacro;
IMPORT   bio: BIO;
IMPORT  time: Time;
IMPORT shell: Shell;
IMPORT   env: tskEnv;
IMPORT   lex: Lexicon;
IMPORT        ASCII;

FROM SYSTEM     IMPORT  WORD;
FROM exScreen   IMPORT  push, pop, posl?, infomode, infoline_pos?
                      , pushandclearinfo, showinfo;
FROM exIO       IMPORT  ReadFile, WriteFile;
FROM exHead     IMPORT  readstr, message, ask, UPPG;
FROM exMain     IMPORT  RefreshScreen, SetName, GetName, inform_on, inform_off
                      , MarkBegin, MarkEnd, Where?, MarkLine, ResetFrame
                      , DelFrame, InsFrame, PutFrame, EraFrame, MovFrame
                      , JumpTo, JumpBegin, JumpEnd, JumpToPattern, Replace
                      , GetBegin, GetEnd, RestoreLine, frame?, UpdateInfo;
FROM exMacro    IMPORT  out?;
FROM exSetUp    IMPORT  public;
FROM exMem      IMPORT  cur, jump, last, delete, alloc;

TYPE String=ARRAY [0..127] OF CHAR;

VAR pattern,  command: String;
             reg_patt: BOOLEAN;
                  SEP: CHAR;
             MainName: String;
    top0, frm0, rect0: BOOLEAN;

PROCEDURE SetMain(Name: ARRAY OF CHAR);
  VAR s: ARRAY [0..31] OF CHAR;
BEGIN
  str.print(MainName,'%s',Name);
  bio.splitpathname(Name,s); SetName(s);
  env.put_str(env.info,MainName,TRUE)
END SetMain;

PROCEDURE resh;
  VAR sav: INTEGER;
BEGIN sav:=cur; jump(0); RefreshScreen(TRUE); jump(sav)
END resh;

PROCEDURE reshpage;
  VAR sav: INTEGER;
BEGIN sav:=cur; RefreshScreen(TRUE); jump(sav)
END reshpage;

PROCEDURE dummyresh; END dummyresh;

PROCEDURE pcc(VAL name: ARRAY OF CHAR): BOOLEAN;
  VAR i: INTEGER;
BEGIN
  i:=str.len(name)-2;
  WHILE (i>0) & (name[i]#'.') DO DEC(i) END;
  RETURN (i>0) & (name[i]='.') & (name[i+1]='c') & (name[i+2]=0c)
END pcc;

PROCEDURE FirstRead;
BEGIN
  IF MainName[0]=0c THEN RETURN END;
  IF NOT face.onread(MainName) THEN MainName:=""; RETURN END;
  exMain.alarm_mode(FALSE);
  kbrd.set_break(1);
  IF ReadFile(MainName,FALSE,resh,SEP) THEN
    IF pcc(MainName) THEN SEP:=ASCII.LF END
  END;
  kbrd.set_break(0);
  exMain.alarm_mode(TRUE);
END FirstRead;

PROCEDURE MainWrite(): BOOLEAN;
BEGIN
  IF MainName[0]=0c THEN
    message(TRUE,"NO OUTPUT FILE NAME"); RETURN TRUE
  END;
  IF arg.flag('-','l') & arg.flag('-','f') THEN SEP:=ASCII.LF END;
  IF arg.flag('-','n') & arg.flag('-','l') THEN SEP:=ASCII.NL END;
  RETURN WriteFile(MainName,0,last,FALSE,SEP);
END MainWrite;

PROCEDURE write_and_exit(): BOOLEAN;
BEGIN
  IF NOT face.onwrite(MainName,TRUE) THEN RETURN FALSE END;
  RETURN NOT MainWrite()
END write_and_exit;

PROCEDURE write_file;
BEGIN
  IF NOT face.onwrite(MainName,TRUE) THEN RETURN END;
  IF MainWrite() THEN END;
END write_file;

PROCEDURE GetFile(name: ARRAY OF CHAR);
  VAR k: CHAR;
BEGIN
  IF name[0]=0c THEN RETURN END;
  IF NOT face.onread(name) THEN RETURN END;
  IF MainName[0]#0c THEN
    REPEAT k:=CAP(ask(" С О Х Р А Н И Т Ь    Ф А Й Л ? "));
    UNTIL (k='Y') OR (k='N');
    IF k='Y' THEN
      IF NOT face.onwrite(MainName,TRUE) THEN RETURN END;
      IF MainWrite() THEN RETURN END;
    END;
  END;
  JumpTo(0,0);
  jump(0);
  delete(last+1,last+1,dummyresh);
  ResetFrame; RefreshScreen(TRUE);
  SetMain(name);
  FirstRead;
  JumpTo(0,0);
END GetFile;

PROCEDURE InsFile(name: ARRAY OF CHAR);
  VAR sav: INTEGER;  x: CHAR;
BEGIN
  sav:=cur;
  IF ReadFile(name,TRUE,reshpage,x) THEN END;
  jump(sav);
END InsFile;

PROCEDURE EmitFile(name: ARRAY OF CHAR);
  VAR sav,l0,l1,c0,c1: INTEGER; add: BOOLEAN;
BEGIN
  sav:=cur;
  GetBegin(l0,c0);
  GetEnd  (l1,c1);
  IF (l0<0) OR (l1<0) THEN
    message(TRUE,"NO BEGIN/END MARKS"); RETURN
  END;
  add:=(name[0]='>');
  IF add THEN str.delete(name,0,1) END;
  IF arg.flag('-','l') & arg.flag('-','f') THEN SEP:=ASCII.LF END;
  IF arg.flag('-','n') & arg.flag('-','l') THEN SEP:=ASCII.NL END;
  IF WriteFile(name,l0,l1,add,SEP) THEN END;
  jump(sav);
END EmitFile;

PROCEDURE Find(s: ARRAY OF CHAR; top?,infrm?,rect?: BOOLEAN);
BEGIN
  IF s[0]=0c THEN RETURN END;
  IF rect? THEN infrm?:=TRUE END;
  top0:=top?;  frm0:=infrm?;  rect0:=rect?;
  str.print(pattern,'%s',s);
  inform_on("SEARCH");
  JumpToPattern(pattern,top?,infrm?,rect?);
  inform_off;
END Find;

PROCEDURE End(s: ARRAY OF CHAR; top: BOOLEAN);
  VAR p: String;
BEGIN str.print(p,'END %s',s); Find(p,top,FALSE,FALSE) END End;

PROCEDURE Proc(s: ARRAY OF CHAR; top: BOOLEAN);
  VAR p: String;
BEGIN str.print(p,"PROCEDURE %s",s); Find(p,top,FALSE,FALSE) END Proc;

PROCEDURE Rep(s: ARRAY OF CHAR; top?,infrm?,rect?: BOOLEAN);
  VAR q: CHAR; i,j: INTEGER;
    pat: String; rep: String;
BEGIN
  IF s[0]=0c THEN RETURN END;
  q:=s[0];
  i:=1;
  WHILE (i<HIGH(s)) & (s[i]#0c) & (s[i]#q) DO
   pat[i-1]:=s[i]; INC(i);
  END;
  IF s[i]#q THEN
    message(TRUE,"NOT PAIRED DELIMITER"); RETURN
  END;
  pat[i-1]:=0c;
  INC(i);
  j:=0;
  WHILE (i<HIGH(s)) & (s[i]#0c) DO rep[j]:=s[i]; INC(i); INC(j) END;
  rep[j]:=0c;
  IF rect? THEN infrm?:=TRUE END;
  top0:=top?;  frm0:=infrm?;  rect0:=rect?;
  inform_on("REPLACE");
  Replace(pat,rep,top?,infrm?,rect?);
  inform_off;
END Rep;

PROCEDURE FindNext;
BEGIN
  IF pattern[0]=0c THEN RETURN END;
  inform_on("SEARCH");
  JumpToPattern(pattern,FALSE,frm0,rect0);
  inform_off;
END FindNext;

PROCEDURE wrong;
BEGIN message(TRUE,"UNKNOWN COMMAND") END wrong;

PROCEDURE Jump(VAL s: ARRAY OF CHAR);
  VAR i,l,lin,col: INTEGER; done: BOOLEAN;
BEGIN
  Where?(lin,col); (* to determine current column & line *)
  (* + - relative jump *)
  (* n.m   n==line  m==column  otherwise jump to current column *)
  i:=0;
  str.skip(s,i,' ');
  IF s[i]#'.' THEN
    IF (s[i]#'+') & (s[i]#'-') THEN lin:=0 END;
    str.iscan(l,s,i,done);
    INC(lin,l);
    IF NOT done THEN wrong; RETURN END;
  END;
  IF s[i]='.' THEN
    INC(i);
    str.iscan(col,s,i,done);
    IF NOT done THEN wrong; RETURN END;
  END;
  JumpTo(lin,col);
END Jump;

PROCEDURE ReadString(line: INTEGER;
                     VAL prompt: ARRAY OF CHAR; VAR s: ARRAY OF CHAR);
BEGIN
  str.print(exHead.prompt,'%s',prompt);
  exHead.Home:=line;
  exHead.ReadCommand;
  str.print(s,'%s',exHead.command);
END ReadString;

PROCEDURE Command;   (* called with PUSHED position *)
  VAR sav: String;
BEGIN
  push;
  term.print("\r"); term.erase_line(0);
  str.print(sav,'%s',command);
  exMain.break_mode(FALSE);
  ReadString(posl?,">>> ",sav);
  exMain.break_mode(TRUE);
  pop;
  RestoreLine;
  Do(sav);
END Command;

PROCEDURE Do(VAL sav: ARRAY OF CHAR);
  VAR cmd: String;
     vert: BOOLEAN;
     rect: BOOLEAN;
  top,frm: BOOLEAN;
      com: CHAR;
    small: BOOLEAN;

BEGIN
  IF sav[0]=0c THEN RETURN END;
  str.print(cmd,'%s',sav);
  frm:=FALSE; rect:=FALSE; top:=FALSE; vert:=FALSE;
  LOOP
    IF    (cmd[0]=" ") THEN             str.delete(cmd,0,1)
    ELSIF (cmd[0]="$") THEN frm :=TRUE; str.delete(cmd,0,1)
    ELSIF (cmd[0]="#") THEN rect:=TRUE; str.delete(cmd,0,1)
    ELSIF (cmd[0]="=") THEN vert:=TRUE; str.delete(cmd,0,1);  rect:=TRUE;
    ELSIF (cmd[0]="!") THEN top :=TRUE; str.delete(cmd,0,1)
    ELSE EXIT
    END;
  END;
  WHILE (cmd[0]=' ') DO str.delete(cmd,0,1) END;
  com:=cmd[0];
  small:=("a"<=com) & (com<="z");
  IF small THEN com:=CAP(com) END;
  IF (com#'-') & (com#'+') & NOT (ORD(com)-ORD("0") IN {0..9}) THEN
    str.delete(cmd,0,1)
  END;
  CASE com OF
  |'0'..'9'
  ,'+', '-'
      : Jump(cmd);
  |'F': Find(cmd,top,frm,rect);
  |'R': Rep (cmd,top,frm,rect);
  |'<': InsFile(cmd);
  |'>': EmitFile(cmd);
  |'N': FindNext;
  |'D': DelFrame(rect);
  |'O': PutFrame(rect);
  |'I': InsFrame(rect,vert);
  |'C': EraFrame(rect);
  |'M': MovFrame(rect,vert);
  |'[': MarkBegin;
  |']': MarkEnd;
  |'.': MarkLine
  |'/': ResetFrame
  |'J': IF    cmd[0]='[' THEN JumpBegin
        ELSIF cmd[0]='.' THEN JumpBegin
        ELSIF cmd[0]=']' THEN JumpEnd
        ELSE wrong
        END;
  |'S': SetMain(cmd);
  |'W': IF MainWrite() THEN END;
  |'G': GetFile(cmd);
  |'E': End(cmd,top);
  |'P': Proc(cmd,top);
  |'@': --Interpret(cmd,TRUE);
  ELSE
    wrong;
  END;
  str.print(command,'%s',sav);
END Do;

VAR desc: sed.descriptor;

PROCEDURE Shell;
  VAR  cmd: ARRAY [0..127] OF CHAR;
       pmt: ARRAY  [0..79] OF CHAR;
      name: ARRAY  [0..31] OF CHAR;
      echo: BITSET;
BEGIN
  infomode(FALSE);
  str.copy(cmd,MainName);
  bio.splitpathname(cmd,name);
  shell.hold_break(TRUE);
  exMain.break_mode(FALSE);
  cmd:="";
  LOOP
    kbrd.set_break(1);
    infomode(FALSE);
    shell.get_prompt(cmd, desc^.ins, desc^.bel);
    str.print(pmt,"%s >>> %s",name,cmd);
    sed.edit_str(pmt, command,
                 term.state^.lines-1, 0, term.state^.columns-2,
                 desc, 33c);
    IF desc^.last=33c THEN EXIT END;
    term.print("\n");
    IF command#"" THEN
      shell.get_echo(echo);
      shell.system(command,echo);
      term.print('\n')
    END;
  END;
  shell.hold_break(FALSE);
  exMain.break_mode(TRUE);
  kbrd.set_break(0);
  infomode(public.info);
  RefreshScreen(TRUE);
END Shell;

PROCEDURE ref_scr; BEGIN RefreshScreen(TRUE) END ref_scr;

PROCEDURE last_ln(): INTEGER;
BEGIN RETURN exMem.last END last_ln;

PROCEDURE del(no: INTEGER);
BEGIN exMem.delete(no,public.high+1,ref_scr) END del;

PROCEDURE get_main_name(VAR s: ARRAY OF CHAR);
BEGIN str.print(s,'%s',MainName) END get_main_name;

TYPE
  marker = POINTER TO MARK;
  MARK   = RECORD
             next: marker;
             pred: marker;
               no: INTEGER;
              l,c: INTEGER;
              msg: ARRAY [0..63] OF CHAR;
           END;

VAR
  mark_list : marker;
  marks     : INTEGER;

PROCEDURE mark(ln,cl: INTEGER; VAL fmt: ARRAY OF CHAR; SEQ args: WORD);

  PROCEDURE marks_over;
  BEGIN
    message(TRUE,"NO MEMORY FOR MARKER");
    JumpTo(ln,cl); message(TRUE,fmt,args);
  END marks_over;

  VAR node: marker;

BEGIN
  alloc(node,SIZE(node^));
  IF node=NIL THEN marks_over; RETURN END;
  node^.l:=ln; node^.c:=cl;
  node^.no:=marks;
  str.print(node^.msg,fmt,args);
  IF mark_list=NIL THEN
    mark_list:=node;
    node^.pred:=node;
    node^.next:=node;
  ELSE
    node^.pred:=mark_list;
    node^.next:=mark_list^.next;
    node^.pred^.next:=node;
    node^.next^.pred:=node;
    mark_list:=node;
  END;
  INC(marks)
END mark;

PROCEDURE clear_marks;
  VAR next: marker;
BEGIN
  IF mark_list=NIL THEN RETURN END;
  mark_list^.pred^.next:=NIL;
  WHILE mark_list#NIL DO
    next:=mark_list^.next;
    alloc(mark_list,-SIZE(mark_list^));
    mark_list:=next;
  END;
  marks:=0;
END clear_marks;

PROCEDURE fill_my_editor;
BEGIN
  face.last:=last_ln;           face.crs_pos:=exMain.Where?;
  face.jump:=exMem.jump;        face.refresh:=ref_scr;
  face.get :=exMem.get;         face.goto   :=exMain.JumpTo;
  face.put :=exMem.put;         face.frame  :=exMain.frame?;
  face.app :=exMem.app;         face.message:=exHead.message;
  face.del :=del;               face.f_name :=get_main_name;
  face.ins :=exMem.insert;      face.mark   :=mark;
  face.adr :=exMem.adr;         face.size   :=exMem.size?;
  clear_marks;
END fill_my_editor;

PROCEDURE WaitUntilPressed;
BEGIN
  REPEAT time.delay(2,time.tick) UNTIL kbrd.ready()#0
END WaitUntilPressed;

PROCEDURE show_mark(up: BOOLEAN);
  VAR int: INTEGER;
BEGIN
  IF marks=0 THEN RETURN END;
  IF up THEN mark_list:=mark_list^.pred;
  ELSE       mark_list:=mark_list^.next;
  END;
  IF (mark_list^.l>=0) & (mark_list^.c>=0) THEN
    JumpTo(mark_list^.l,mark_list^.c);
  END;
  int:=term.state^.color;
  term.set_color(term.state^.max_color);
  message(FALSE,"%-59.59s %d <<< %d",mark_list^.msg,mark_list^.no,marks);
  term.set_color(int);
  WaitUntilPressed;
  UpdateInfo;
END show_mark;

PROCEDURE f_print(VAL fmt: ARRAY OF CHAR; SEQ args: WORD);
BEGIN exHead.message(TRUE,fmt,args) END f_print;

PROCEDURE Filter(k: CHAR);
  VAR buff: DYNARR OF CHAR;
       ptr: POINTER TO ARRAY [0..0] OF CHAR;
      size: INTEGER;
      save: shell.PRINT;
BEGIN
  mac.getSmacro(k,ptr,size);
  IF size=0 THEN RETURN END;
(*$<U+*)
  buff^.ADR:=ptr; buff^.HIGH:=size-1;
(*$>*)
  fill_my_editor;
  UpdateInfo;
  kbrd.set_break(1);
  save:=shell.print;
  shell.print:=f_print;
  shell.hold_break(TRUE);
  shell.system(buff,shell._trap+shell._break+shell._ipr);
  shell.hold_break(FALSE);
  shell.print:=save;
  kbrd.set_break(0);
  RefreshScreen(TRUE);
  IF marks>0 THEN show_mark(FALSE)
  ELSE
    WaitUntilPressed;
    UpdateInfo
  END
END Filter;

BEGIN
  str.print(pattern,''); str.print(command,''); str.print(MainName,'');
  mark_list:=NIL;       marks  :=0;
  sed.new(desc,16);
  SEP:=ASCII.NL;
  IF arg.flag('-','l') & arg.flag('-','f') THEN SEP:=ASCII.LF END;
  IF arg.flag('-','n') & arg.flag('-','l') THEN SEP:=ASCII.NL END;
END exCmd.
