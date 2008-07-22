MODULE dpm; (*$X+  21-Jan-91. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT  mem: Heap;
IMPORT  str: Strings;
IMPORT  cpd: CPD;
IMPORT  bio: BIO;
IMPORT  tim: Time;
IMPORT  rnd: Random;
IMPORT  fnt: Fonts;
IMPORT  pup: pmPUP;
IMPORT draw: pmDraw;

WITH STORAGE: mem;

PROCEDURE demoBMG(x,y,w,h: INTEGER); FORWARD;

VAR bug: pup.DEBUG;
  saveP: draw.PALETTE;

PROCEDURE text(txt0,txt1,txt2: ARRAY OF CHAR);
  VAR t: draw.TOOL;
      c: BITSET;
      s: draw.COLOR;
    rgb: ARRAY [0..31] OF INTEGER;
  i,j,k: INTEGER;
   swap: INTEGER;
BEGIN
  t.zX:=25;
  t.zY:=23;
  WITH t.clip DO y:=25; x:=23; w:=390; h:=60 END;
  pup.panel(t.clip,t.clip,TRUE,TRUE);
  t.zX:=t.clip.x;  t.clip.x:=0;
  t.zY:=t.clip.y;  t.clip.y:=0;
  t.mask:={0..3}; t.back:={}; t.mode:=draw.xor;
  t.color:={1,3}/pup.normal;
  draw.print(draw.scr,t,10,30,fnt.font,txt0);
  draw.print(draw.scr,t,10, 0,fnt.font,txt2);
  c:={0,1,3};
  t.color:=c/pup.normal;
  draw.print(draw.scr,t,10,15,pup.font,txt1);
  FOR i:=0 TO 2*4*4-1 DO rnd.random(j); rgb[i]:=j MOD 64 END;
  s:=draw.palette[INTEGER(c)];
  FOR i:=0 TO 2*4*4-1 DO
    j:=rgb[i];
    draw.paint(c,j MOD 4,j DIV 4 MOD 4,j DIV 16 MOD 4);
    tim.delay(2,tim.tick);
  END;
  draw.paint(c,s.r,s.g,s.b);
END text;

PROCEDURE print(VAL fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);
BEGIN
  pup.gprint(bug,fmt,arg)
END print;

PROCEDURE test_msg;
BEGIN
  text('To send any of this message to the screen, you should call:',
       '       pmPUP.message( xcenter, ycenter, format, args );',
       'and nothing more!   (Try mouse, buttons on it and keysboard)');
  pup.pushtimeout(15000);
  print('pup.message(200,240,"Nice...");');
  pup.message(200,240,"Nice, nice, very nice / So many different people in the same device!");
  pup.poptimeout; pup.pushtimeout(8000);
  print('pup.message(500,-10,"Hello World!");');
  pup.message(500,-10,"Hello World!");
  print('pup.message(-10,-10,"Hello There!");');
  pup.message(-10,-10,"Hello There!");
  print('pup.message(500,500,"Hello to Everybody!");');
  pup.message(500,500,"Hello to Everybody!");
  print('pup.message(-10,500,"Hello Freinds!");');
  pup.message(-10,500,"Hello Freinds!");
  pup.poptimeout; pup.pushtimeout(7000);
  print('pup.message(240,180,"Hello, ...")');
  pup.message(240,180,"Hello, I'am Presentation Manager!");
  print('pup.message(200,240,"Nice...");');
  pup.poptimeout; pup.pushtimeout(15000);
  pup.message(200,240,"Nice, nice, very nice / So many different people in the same device!");
  pup.poptimeout
END test_msg;

PROCEDURE test_diabox;
  VAR s: ARRAY [0..1024] OF CHAR;
BEGIN
  print('');
  text('To create and open dialog box on the screen and read',
       '  pmPUP.diabox(xc, yc, width, enterstr, promptformat, args );',
       'text form keyboard application should easy call. (Go for!)');
  pup.pushtimeout(30000);
  print('pup.diabox(240,180,200,str,"text:");');
  pup.diabox(240,180,200,s,"text:");
  IF pup.time<=0 THEN print('TIME OUT') END;
  print('str="%s"',s);
  print('pup.confirm(340,80,100,s,"");');
  s:="Are you realy sure? ";
  pup.confirm(340,80,100,s,"");
  IF pup.time<=0 THEN print('TIME OUT') END;
  print('str="%s"',s);
  pup.poptimeout; pup.pushtimeout(6000);
  pup.wait;
  pup.poptimeout;
END test_diabox;

PROCEDURE test_buttons;
  VAR i: INTEGER;
      b: ARRAY [0..17] OF draw.BLOCK;
      s: ARRAY [0..11] OF draw.BLOCK;
BEGIN
  print("");
  text('Applications could call:',
       '          pmPUP.button( block, pressed );',
       'to create and manupulate buttons on the screen.');
  FOR i:=0 TO HIGH(b) DO
    WITH b[i] DO w:=19; h:=19; y:=290-i MOD 9*24; x:=427+24*ORD(i>=9) END
  END;
  pup.pushtimeout(500);
  print("pmPUP.button(b,FALSE);");
  FOR i:=0 TO HIGH(b)      DO          pup.button(b[i],FALSE) END;
  print("pmPUP.button(b,TRUE);");
  FOR i:=0 TO HIGH(b) BY 2 DO pup.wait; pup.button(b[i],TRUE)  END;
  FOR i:=1 TO HIGH(b) BY 2 DO
    pup.wait; pup.button(b[i-1],FALSE);  pup.button(b[i],TRUE)
  END;
  print("pmPUP.button(b,FALSE);");
  FOR i:=1 TO HIGH(b) BY 2 DO pup.wait; pup.button(b[i],FALSE)  END;
  pup.poptimeout; pup.pushtimeout(4000); pup.wait;
  text('And this procedures could be used for switches:',
       ' pmPUP.button(block,FALSE);  pmPUP.switch(block, pressed);',
       '');
  pup.poptimeout; pup.pushtimeout(500);
  FOR i:=0 TO HIGH(s) DO
    WITH s[i] DO w:=16; h:=16; x:=140+i*20; y:=330 END
  END;
  print("");
  print("pmPUP.button(b,FALSE);  pmPUP.switch(b,FALSE);");
  FOR i:=0 TO HIGH(s) DO pup.button(s[i],FALSE); pup.switch(s[i],FALSE) END;
  print("pmPUP.switch(b,TRUE);");
  FOR i:=0 TO HIGH(s) BY 2 DO pup.wait; pup.switch(s[i],TRUE)   END;
  print("pmPUP.switch(b,FALSE);");
  FOR i:=1 TO HIGH(s) BY 2 DO
    pup.wait; pup.switch(s[i-1],FALSE);  pup.switch(s[i],TRUE)
  END;
  print("pmPUP.switch(b,TRUE);");
  FOR i:=1 TO HIGH(s) BY 2 DO pup.wait; pup.switch(s[i],FALSE)  END;
  pup.poptimeout; pup.pushtimeout(4000);
  pup.wait;
  pup.poptimeout; pup.pushtimeout(12000);
  text('Pointed cursor to one of the blocks presents buttons, may be',
       '   no := pmPUP.inblocks( x, y, button1, button2, button3 );',
       'easy tested by calling this procedure.');
  pup.wait;
  pup.poptimeout;
END test_buttons;

PROCEDURE test_rollers;
  VAR r: pup.ROLLER;
      i: INTEGER;
    txt: pup.TEXT;
BEGIN
  text('The collection of named objects may be created by call',
       '       pmPUP.rnew( roller, x, y, w, h, exits, "title" );',
       'the result will be the "roller".');
  pup.pushtimeout(9000);
  print('');
  pup.rnew(r,70,220,105,100,{},"Rollerman");   print('pmPUP.rnew(r,70,220,105,100,{}," The Rollerman");');
  pup.ropen(r);                                print('pmPUP.ropen(r);');
  pup.wait;
  text('Names of objects to select, may be associated with roller by call',
       '          pmPUP.rsettext( roller, text, top, alt );',
       'and, if roller was previosly openned, they appears on the screen.');
  NEW(txt,12);
  FOR i:=0 TO HIGH(txt) DO NEW(txt[i],32) END;
  str.print(txt[ 0]," Window");
  str.print(txt[ 1]," Vindue");
  str.print(txt[ 2]," Venster");
  str.print(txt[ 3]," Ikkuna");
  str.print(txt[ 4]," Fene`tre");
  str.print(txt[ 5]," Fenster");
  str.print(txt[ 6]," Finestra");
  str.print(txt[ 7]," Vindu");
  str.print(txt[ 8]," Janela");
  str.print(txt[ 9]," Ventana");
  str.print(txt[10]," Fonster");
  str.print(txt[11]," Окошко");
  pup.rsettext(r,txt,-1,-1);       print('pmPUP.rsettext(r,txt,-1,-1);');
  pup.wait;
  text('To select one of objects, application should call',
       '          pmPUP.rselect( roller );',
       '(try mouse and keyboard to select).');
  pup.poptimeout; pup.pushtimeout(5000);
  REPEAT
    print('pmPUP.rselect(r);');
    pup.rselect(r);
    print('pmPUP.rselected(r)=%d;',pup.rselected(r));
    print('pmPUP.ralt(r)=%d;',pup.ralt(r));
    IF pup.time=0 THEN print("pmPUP.time=0  (TIMEOUT)") END;
  UNTIL (pup.time=0) OR NOT pup.rselected(r);
  pup.poptimeout; pup.pushtimeout(1000);
  pup.wait;
  text('To close roller application should call:',
       '          pmPUP.rclose( roller );',
       '');
  pup.poptimeout; pup.pushtimeout(3000);
  pup.wait;
  pup.rclose(r);
  text('','','If roller wasn`t opened before select it will be "popuped"');
  pup.poptimeout; pup.pushtimeout(2000);
  pup.wait;
  pup.poptimeout; pup.pushtimeout(5000);
  REPEAT
    print('pmPUP.rselect(r);');
    pup.rselect(r);
    print('pmPUP.rselected(r)=%d;',pup.rselected(r));
    print('pmPUP.ralt     (r)=%d;',pup.ralt(r));
    IF pup.time=0 THEN print("pmPUP.time=0  (TIMEOUT)") END;
  UNTIL (pup.time=0) OR NOT pup.rselected(r);
  pup.poptimeout; pup.pushtimeout(1000);
  pup.wait;
  pup.poptimeout;
  pup.rdispose(r);
  FOR i:=0 TO HIGH(txt) DO DISPOSE(txt[i]) END; DISPOSE(txt);
END test_rollers;

PROCEDURE test_direx;
  VAR d: pup.DIREX;
      s: ARRAY [0..127] OF CHAR;
      f: bio.FILE;
BEGIN
  text('Direxs are very simple case of rollers, ussing for file selection',
       '     pmPUP.dnew( direx, x, y, w, h, exits, cd, pattern, items );',
       'try mouse and keyboard.');
  print('');
  print('pup.dnew(d,350,150,110,200,pup.xlf+pup.xrg,".","*",pup.standard);');
  pup.dnew(d,350,150,110,200,pup.xlf+pup.xrg,".","*",pup.standard);
  print('pup.dopen(d);');
  pup.dopen(d);
  LOOP
    pup.poptimeout; pup.pushtimeout(10000);
    print('pmPUP.dselect(d);');
    pup.dselect(d);
    print('pmPUP.dselected(d)=%d;',pup.dselected(d));
    IF pup.dselected(d) THEN
      pup.dfilename(d,s);
      print('pmPUP.dfilename(d,s); s=%s;',s);
      pup.dfullname(d,s);
      print('pmPUP.dfullname(d,s); s=%s;',s);
      pup.dopenfile(d,f,'r');
      IF pup.done   THEN print("pmPUP.openfile(d,f) DONE"); bio.close(f)
      ELSE              print("pmPUP.openfile(d,f) ERROR")
      END
  ;pup.zoom;
    END;
    IF pup.time=0 THEN print("pmPUP.time=0  (TIMEOUT)") END;
    IF (pup.time=0) OR (pup.ch=33c) OR ({1,2}*cpd.state^.keys#{}) THEN EXIT END;
    pup.poptimeout; pup.pushtimeout(1000); pup.wait;
  END;
  pup.poptimeout;
  pup.ddispose(d);
END test_direx;

PROCEDURE test_debug;
  VAR g: pup.DEBUG;
BEGIN
  text('Before your application will be ready for SALE,',
       '        pmPUP.gnew( g, x, y, w, h );   pmPUP.open( g, x, y, w, h );',
       'you have to DEBUG it. What`s good idea to open debug window!');
  pup.gnew(g,300,200,200,60);
  pup.pushtimeout(8000);
  pup.wait;     pup.gopen(g);

  pup.poptimeout;        pup.pushtimeout(2000);
  pup.wait;     pup.gprint(g,"and you could easy call:");
  pup.wait;     pup.gprint(g,'   pmPUP.gprint(g,"Hello World!");');
  pup.wait;     pup.gprint(g,'to produce output:');
  pup.wait;     pup.gprint(g,"          Hello World!");
  pup.poptimeout;        pup.pushtimeout(4000);
  pup.wait;     pup.gdispose(g);
  text('To close debug window you should easy call:',
       '           pmPUP.gclose( g );   pmPUP.gdispose( g );',
       '');
  pup.poptimeout; pup.pushtimeout(4000);
  pup.wait;
  pup.poptimeout
END test_debug;

PROCEDURE conversation(n: INTEGER);
  VAR p: pup.POPUP;
      g: pup.DEBUG;
BEGIN
  pup.pushtimeout(30000);
  CASE n OF
  |0: (* popup *)
  |1: (* menu  *)
  |2: test_debug
  |3: test_rollers
  |4: test_direx
  |5: test_msg
  |6: test_diabox
  |7: text('ZOOM is very usefull for applications designer programmer',
           '                  pmPUP.zoom;',
           'allow you to look at the dot structure of the screen.');
      print(''); print('pmPUP.zoom;'); print('');
      pup.zoom;      pup.poptimeout;     RETURN
  |8: test_buttons;  pup.poptimeout;     RETURN
  ELSE
  END;
--pup.pushfont(fnt.font);
  pup.gnew(g,40,30,400,290);
  pup.gopen(g);
  CASE n OF
  |0: pup.gprint(g,"        Sample of POPUP usage program:");
      pup.gprint(g,"");
      pup.gprint(g,"");
      pup.gprint(g," VAR p: POPUP;");
      pup.gprint(g,"   .....  ");
      pup.gprint(g,' pup.pnew(p,290,170,180,140);');
      pup.gprint(g,"   .....  ");
      pup.gprint(g,' pup.popen(p);');
      pup.gprint(g,'');
      pup.gprint(g,'   (* usage area (290,170) (180x140) *)');
      pup.gprint(g,'');
      pup.gprint(g,' pup.pclose(p); (* restore previouse contence *)');
      pup.gprint(g,"   .....  ");
      pup.gprint(g,' pup.pdispose(p);');
      pup.gprint(g,'');
      pup.gprint(g,'');
      pup.gprint(g,'');
      pup.poptimeout; pup.pushtimeout(8000);
      pup.wait;
      pup.pnew(p,290,170,180,140);
      pup.popen(p);
      demoBMG(290,170,180,140);
      pup.poptimeout; pup.pushtimeout(3000);
      pup.wait;
      pup.pdispose(p);
      pup.poptimeout; pup.pushtimeout(30000);

  |1: pup.gprint(g,"         Sample of MENU usage program:");
      pup.gprint(g,"");
      pup.gprint(g," VAR m: MENU;");
      pup.gprint(g,"   .....  ");
      pup.gprint(g,' pup.mnew(m,10,260,70,90,pup.xup+pup.xdw+pup.xlf+pup.xrg," MENU");');
      pup.gprint(g,' pup.mprint(m,0," PopUp");  pup.mhotkey(m,0,"P",TRUE);');
      pup.gprint(g,' pup.mprint(m,1," Menu");   pup.mhotkey(m,1,"M",TRUE);');
      pup.gprint(g,' pup.mprint(m,2," debuG");  pup.mhotkey(m,2,"G",TRUE);');
      pup.gprint(g,' pup.mprint(m,3," Roller"); pup.mhotkey(m,3,"R",TRUE);');
      pup.gprint(g,' pup.mprint(m,4," Direx");  pup.mhotkey(m,4,"D",TRUE);');
      pup.gprint(g,"   .....  ");
      pup.gprint(g,' pup.mselect(m);');
      pup.gprint(g,' IF pup.mselected(m) THEN');
      pup.gprint(g,'   CASE pup.malt(m) OF');
      pup.gprint(g,'     ....');
      pup.gprint(g,'   END');
      pup.gprint(g,' END;');

  |2: pup.gprint(g,"         Sample of DEBUG usage program:");
      pup.gprint(g,"");
      pup.gprint(g,"");
      pup.gprint(g," VAR g: DEBUG;");
      pup.gprint(g,"");
      pup.gprint(g,"   .....  ");
      pup.gprint(g,"");
      pup.gprint(g,' pup.gnew(g,0,0,200,100);');
      pup.gprint(g,' pup.gopen(g);');
      pup.gprint(g,"");
      pup.gprint(g,'   .....  ');
      pup.gprint(g,"");
      pup.gprint(g,' pup.gprint(g,"Hello World!");');
      pup.gprint(g,"");
      pup.gprint(g,'   .....  ');
      pup.gprint(g,"");
      pup.gprint(g,' pup.gclose(g);');
      pup.gprint(g,"");

  |3: pup.gprint(g,"         Sample of ROLLER usage program:");
      pup.gprint(g,"");
      pup.gprint(g," VAR r: ROLLER;");
      pup.gprint(g,"   .....  ");
      pup.gprint(g,' pup.rnew(r,10,260,70,90,pup.xlf+pup.xrg," ROLLER");');
      pup.gprint(g,' NEW(txt,10);');
      pup.gprint(g,' NEW(txt[0],32); str.print(txt[0],"Apple");');
      pup.gprint(g,' NEW(txt[1],32); str.print(txt[1],"Orange");');
      pup.gprint(g,' NEW(txt[2],32); str.print(txt[2],"Banana");');
      pup.gprint(g,"   .....  ");
      pup.gprint(g,' pup.rsettext(r,txt,0,0);');
      pup.gprint(g,' pup.rselect(r);');
      pup.gprint(g,' IF pup.rselected(r) THEN');
      pup.gprint(g,'   CASE pup.ralt(r) OF');
      pup.gprint(g,'     ....');
      pup.gprint(g,'   END');
      pup.gprint(g,' END;');


  |4: pup.gprint(g,"         Sample of DIREX usage program:");
      pup.gprint(g,'');
      pup.gprint(g,' VAR d: pup.DIREX;');
      pup.gprint(g,'');
      pup.gprint(g,' pup.dnew(d,100,100,90,160,{},".","*",pup.standrd);');
      pup.gprint(g,'');
      pup.gprint(g,'  ......');
      pup.gprint(g,' LOOP');
      pup.gprint(g,'   pup.dselect(d);');
      pup.gprint(g,'   IF pup.dselected(d) THEN');
      pup.gprint(g,'     pup.dopenfile(d,f,"rwx");');
      pup.gprint(g,'     IF pup.done THEN EXIT END;');
      pup.gprint(g,'    END;');
      pup.gprint(g,'  END;');
      pup.gprint(g,'  (* work with selected file *) ');
      pup.gprint(g,'   ......');
      pup.gprint(g,'  pup.ddispose(d);');

  |5,6:
      pup.gprint(g,"         Sample of MESSAGE & DIALOGBOX usage program:");
      pup.gprint(g,"");
      pup.gprint(g,"");
      pup.gprint(g,"");
      pup.gprint(g,"");
      pup.gprint(g,'    pup.message(240,180,"Hello Everybody!");');
      pup.gprint(g,"");
      pup.gprint(g,"");
      pup.gprint(g,"");
      pup.gprint(g,'    pup.perror(bio.error,240,180,"open file %s error %%s",name);');
      pup.gprint(g,"");
      pup.gprint(g,"");
      pup.gprint(g,"");
      pup.gprint(g,'    pup.diabox(240,180,95,str,"Are you realy sure?");');
      pup.gprint(g,"");
      pup.gprint(g,"");
      pup.gprint(g,"");
  END;
  pup.gprint(g,'                                                   press any key');
--pup.popfont;
  pup.wait(0c);
  pup.poptimeout;
  pup.gdispose(g);
  CASE n OF
  |0: (* popup *)
  |1: (* menu  *)
  |2: test_debug
  |3: test_rollers
  |4: test_direx
  |5: test_msg
  |6: test_diabox
  |7: print(''); print('pmPUP.zoom;'); print('');
      pup.zoom;   pup.poptimeout;     RETURN
  ELSE
  END;
END conversation;

PROCEDURE begin;
  VAR t: draw.TOOL;
BEGIN
  t.zX:=0;
  t.zY:=0;
  WITH t.clip DO x:=0; y:=0; w:=480; h:=360 END;
  pup.panel(t.clip,t.clip,TRUE,FALSE);
  WITH t.clip DO INC(x,8); INC(y,8); DEC(w,60); DEC(h,40) END;
  pup.panel(t.clip,t.clip,FALSE,TRUE);
  t.mask:={0..3}; t.color:={1}; t.mode:=draw.rep;  t.back:={};
  draw.fill(draw.scr,t,t.clip,32,{});
  t.mask:={1}; t.color:={1}; t.mode:=draw.rep;  t.back:={};
  draw.fill(draw.scr,t,t.clip,10,{0..9},{0,5},{0,5},{0,5},{0,5});
  t.mask:={0..3}; t.color:={1}; t.mode:=draw.rep;  t.back:={};
  --pup.pushfont(fnt.font);
  pup.gnew(bug,45,90,310,120);
  --pup.popfont;
  pup.gopen(bug);
  print("This`s the window for trace");
  print("");
END begin;

PROCEDURE end;
BEGIN
  pup.gdispose(bug);
  text('                All good wishes to PM users!',
       '                                      T H E   E N D',
       '                                            press any key');
  pup.pushtimeout(10000); pup.wait(0c); pup.poptimeout
END end;

PROCEDURE test_menus;
  VAR m: pup.MENU; s: BOOLEAN;
BEGIN
  text('Menus are very simple case of rollers',
       '       pmPUP.mnew( menu, x, y, w, h, exits, "title" );',
       'try mouse, keyboard and hotkeys P,M,G,R,D.');
  print('');
  print('pup.mnew(m,10,217,80,135,pup.xup+pup.xdw+pup.xlf+pup.xrg," MENU");');
  pup.mnew(m,10,217,80,135,pup.xup+pup.xdw+pup.xlf+pup.xrg," MENU");
  pup.mprint(m,0," PopUp");     pup.mhotkey(m,0,"P",TRUE);
  pup.mprint(m,1," Menu");      pup.mhotkey(m,1,"M",TRUE);
  pup.mprint(m,2," debuG");     pup.mhotkey(m,2,"G",TRUE);
  pup.mprint(m,3," Roller");    pup.mhotkey(m,3,"R",TRUE);
  pup.mprint(m,4," Direx");     pup.mhotkey(m,4,"D",TRUE);
  pup.mprint(m,5," meSsage");   pup.mhotkey(m,5,"S",TRUE);
  pup.mprint(m,6," diaboX");    pup.mhotkey(m,6,"X",TRUE);
  pup.mprint(m,7," Zoom  ");    pup.mhotkey(m,7,"Z",TRUE);
  pup.mprint(m,8," Buttons");   pup.mhotkey(m,8,"B",TRUE);
  pup.mprint(m,9," Quit  ");    pup.mhotkey(m,9,"Q",TRUE);
  print('pup.mopen(m);');
  pup.mopen(m);  s:=FALSE;
  LOOP
    pup.poptimeout; pup.pushtimeout(10000);
    print('pmPUP.mselect(m);');
    pup.mselect(m);
    print('pmPUP.mselected(m)=%d;',pup.mselected(m));
    print('pmPUP.malt     (m)=%d',pup.malt(m));
    print('pmPUP.ch=%03bc"%c"  pmPUP.mx=%d pmPUP.my=%d',pup.ch,pup.ch,pup.mx,pup.my);
    IF pup.time=0 THEN print("pmPUP.time=0  (TIMEOUT)") END;
    IF (pup.time=0) OR (pup.ch=33c) OR ({1,2}*cpd.state^.keys#{}) THEN EXIT END;
    pup.poptimeout; pup.pushtimeout(1000); pup.wait;
    IF pup.mselected(m) THEN
      s:=TRUE;
      IF pup.malt(m)=9 THEN end; HALT END;
      conversation(pup.malt(m))
    END
  END;
  pup.poptimeout;
  pup.mdispose(m);
  IF s THEN RETURN END;
  test_debug;
  test_buttons;
  test_msg;
  test_diabox;
  test_rollers;
  test_direx;
END test_menus;


PROCEDURE demoBMG(X,Y,W,H: INTEGER);
(*
  VAR
     rnd: ARRAY [0..255] OF INTEGER;
     wrk: draw.BLOCK;
    tool: draw.TOOL;

  PROCEDURE init_random;
    VAR i,r: INTEGER;
  BEGIN r:=153;
    FOR i:=0 TO HIGH(rnd) DO
      r:=(r*15383+587) MOD 8000h;
      rnd[i]:=r*wrk.w  DIV 8000h;
    END
  END init_random;

  PROCEDURE clear_wrk;
  BEGIN
    tool.mask:={0..3}; tool.color:={0..3}; tool.mode:=draw.rep; tool.clip:=wrk;
    draw.set_tool(tool);  draw.fill(32,0)
  END clear_wrk;

  PROCEDURE test1;
  BEGIN
    tool.color:={0..3};
    draw.set_tool(tool);  draw.fill(2,1,-1);
    tim.delay(1,tim.sec);
  END test1;

  PROCEDURE test0; (* dot *)
    VAR i,j,n: INTEGER;
  BEGIN
    draw.set_mode(draw.xor);
    FOR n:=0 TO 5 DO
      FOR j:=0 TO 5 DO
        FOR i:=j+n*5 TO HIGH(rnd)-3 BY 3 DO
          draw.set_color(BITSET(rnd[i] MOD 13+1));
          draw.dot(wrk.x+rnd[i+1],wrk.y+rnd[i+2])
        END
      END
    END
  END test0;

  PROCEDURE test3;
    VAR i,j,n: INTEGER;
  BEGIN
    clear_wrk;
    draw.set_mode(draw.xor);
    FOR n:=0 TO 1 DO
      FOR i:=-2 TO tool.clip.w+2 BY 2 DO
        draw.set_color(BITSET(i MOD 5+1));
        draw.line(wrk.x+i,wrk.y-2,wrk.x+wrk.w+2-i,wrk.y+wrk.h+2)
      END;
      FOR i:=-2 TO tool.clip.h+2 BY 2 DO
        draw.set_color(BITSET(i MOD 5+1));
        draw.line(wrk.x-2,wrk.y+i,wrk.x+wrk.w+2,wrk.y+wrk.h+2-i)
      END;
      tim.delay((1-n),tim.sec)
    END
  END test3;

  PROCEDURE test6;
    VAR xc,yc,i,j,n: INTEGER;
  BEGIN
    clear_wrk;
    draw.set_mode(draw.xor);
    FOR n:=0 TO 1 DO
      FOR i:=0 TO HIGH(rnd)-4 BY 4 DO
        draw.set_color(BITSET(rnd[i] MOD 13+1));
        draw.circle(wrk.x+rnd[i+1],wrk.y+rnd[i+2],rnd[i+3] DIV 4)
      END;
      tim.delay((1-n),tim.sec)
    END;
  END test6;

  PROCEDURE test8;
    VAR xc,yc,i,j,n: INTEGER;
  BEGIN
    clear_wrk;
    draw.set_mode(draw.xor);
    FOR n:=0 TO 1 DO
      FOR i:=0 TO HIGH(rnd)-4 BY 4 DO
        draw.set_color(BITSET(rnd[i] MOD 13+1));
        draw.circlef(wrk.x+rnd[i+1],wrk.y+rnd[i+2],rnd[i+3] DIV 4)
      END;
      tim.delay((1-n),tim.sec)
    END
  END test8;

  PROCEDURE test9;
    VAR xc,yc,i,j,n: INTEGER;
  BEGIN
    clear_wrk;
    draw.set_mode(draw.xor);
    FOR n:=0 TO 1 DO
      FOR i:=0 TO HIGH(rnd)-7 BY 7 DO
        draw.set_color(BITSET(rnd[i] MOD 13+1));
        draw.trif(wrk.x+rnd[i+1],wrk.y+rnd[i+2]
                 ,wrk.x+rnd[i+3],wrk.y+rnd[i+4]
                 ,wrk.x+rnd[i+5],rnd[i+6]+wrk.y)
      END;
      tim.delay((1-n),tim.sec)
    END
  END test9;

  PROCEDURE test10;
    VAR xc,yc,i,j,n: INTEGER;
  BEGIN
    clear_wrk;
    draw.set_mode(draw.xor);
    FOR n:=0 TO 1 DO
      FOR i:=0 TO HIGH(rnd)-4 BY 5 DO
        draw.set_color(BITSET(rnd[i] MOD 13+1));
        draw.rect(wrk.x+rnd[i+1],wrk.y+rnd[i+2],wrk.x+rnd[i+3],wrk.y+rnd[i+4])
      END;
      tim.delay((1-n),tim.sec)
    END
  END test10;

  VAR i: INTEGER;

BEGIN
  FOR i:=0 TO HIGH(saveP) DO
    WITH saveP[i] DO draw.paint(i,r,g,b) END
  END;
  tool.zX:=X;
  tool.zY:=Y;
  tool.mask:={0..3};
  tool.color:=tool.mask;
  tool.clip.x:=0; tool.clip.w:=W;
  tool.clip.y:=0; tool.clip.h:=H;  tool.mode:=draw.rep;
  draw.set_tool(tool);
  draw.fill(32,0);
  draw.set_mode(draw.or);
  draw.set_color(6b);
  draw.fill(9,0,30, 30, 30, 30,  0,  0,  0,0);
  draw.set_color(2b);
  draw.fill(9,0,64,192,192,192,192,254,252,0);
  tool.color:=tool.mask;  tool.mode:=draw.rep;

  wrk:=tool.clip;
  INC(wrk.x,10); INC(wrk.y,10); DEC(wrk.w,20); DEC(wrk.h,20);
  tool.clip:=wrk;
  draw.set_tool(tool);
  draw.frame(wrk.x,wrk.y,wrk.x+wrk.w-1,wrk.y+wrk.h-1);
  INC(wrk.x,2); INC(wrk.y,2); DEC(wrk.w,4); DEC(wrk.h,4);
  tool.clip:=wrk;
  draw.set_tool(tool);
  draw.fill(32,0);
  init_random;
  tool.clip:=wrk;
  draw.set_tool(tool);
  test0;  test1;
  test3;  test6;
  test8;  test9;  test10;
  pup.setcolors;
*)
END demoBMG;

BEGIN
  fnt.unpack(fnt.font);
  NEW(saveP,HIGH(draw.palette)+1);
  saveP:=draw.palette;
  pup.setcolors;
  LOOP
    begin;
      test_menus;
    end
  END
END dpm.
