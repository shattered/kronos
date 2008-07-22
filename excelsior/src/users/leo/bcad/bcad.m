MODULE bcad; (* brd 15-Jan-91. (c) KRONOS *)

IMPORT  bpm: bcPM;
IMPORT  dft: bcDraft;
IMPORT  tol: bcTool;
IMPORT  set: bcSet;
IMPORT  bas: bcBase;
IMPORT  fil: bcFile;
IMPORT  dim: bcDim;
IMPORT  pup: pmPUP;

  TYPE title = ARRAY [0..15] OF CHAR;

  VAR tdft,tset,tdim,tedt,tscr,tfil,tmod: bpm.TABLET;
                                  X,Y,hf: INTEGER;
                                    base: bpm.BAR;
                                     but: ARRAY [0..6] OF bpm.BLOCK;

PROCEDURE _close(T: bpm.TABLET); BEGIN bpm.tonbottom(T); bpm.bonbottom(base) END _close;

PROCEDURE _open(T: bpm.TABLET);  BEGIN bpm.bontop(base); bpm.tontop(T) END _open;

PROCEDURE _dispose;
BEGIN
  bpm.tdispose(tdft);
  bpm.tdispose(tset);
  bpm.tdispose(tdim);
  bpm.tdispose(tedt);
  bpm.tdispose(tscr);
  bpm.tdispose(tfil);
  bpm.bdispose(base)
END _dispose;

PROCEDURE _move;
  VAR dx,dy: INTEGER;
         wn: bpm.WINDOW;
          i: INTEGER;
BEGIN
   bpm.bwindow(base,wn); dx:=wn^.x-X; dy:=wn^.y-Y;
   IF (dx=0) & (dy=0) THEN  RETURN END;
   X:=X+dx; Y:=Y+dy;
   FOR i:=0 TO HIGH(but) DO but[i].x:=but[i].x+dx END;
   bpm.twindow(tdft,wn); bpm.tmove(tdft,but[0].x,Y-wn^.h-1);
   bpm.twindow(tedt,wn); bpm.tmove(tedt,but[1].x,Y-wn^.h-1);
   bpm.twindow(tscr,wn); bpm.tmove(tscr,but[2].x,Y-wn^.h-1);
   bpm.twindow(tfil,wn); bpm.tmove(tfil,but[3].x,Y-wn^.h-1);
   bpm.twindow(tset,wn); bpm.tmove(tset,but[4].x,Y-wn^.h-1);
   bpm.twindow(tdim,wn); bpm.tmove(tdim,but[5].x,Y-wn^.h-1);
   bpm.twindow(tmod,wn); bpm.tmove(tmod,but[6].x,Y-wn^.h-1);
END _move;

PROCEDURE init_rus;
  VAR wn: bpm.WINDOW;
       i: INTEGER;
       s: ARRAY [0..6] OF STRING;
BEGIN
  bpm.newstr(s[0],'Примитивы');
  bpm.newstr(s[1],'Изменения');
  bpm.newstr(s[2],'Экран');
  bpm.newstr(s[3],'Файлы');
  bpm.newstr(s[4],'Установки');
  bpm.newstr(s[5],'Размеры');
  bpm.newstr(s[6],'Модель');

  bpm.bnew(base,s,X,Y,bpm.xsel,'');

  bpm.bhotkey(base,0,"п",TRUE);
  bpm.bhotkey(base,1,"и",TRUE);
  bpm.bhotkey(base,2,"э",TRUE);
  bpm.bhotkey(base,3,"ф",TRUE);
  bpm.bhotkey(base,4,"у",TRUE);
  bpm.bhotkey(base,5,"р",TRUE);
  bpm.bhotkey(base,6,"м",TRUE);

  bpm.bwindow(base,wn);
  FOR i:=0 TO HIGH(but) DO
    bpm.bblocks(base,i,but[i]);
    but[i].x:=but[i].x+wn^.inner.zX+X
  END;

  bpm.tnew(tdft,1,8,but[0].x,Y-hf*9,100,hf,bpm.xsel+bpm.xup,' Примитивы');
  bpm.tprint(tdft,0,"Прямая       "); bpm.thotkey(tdft,0,"п",TRUE);
  bpm.tprint(tdft,1,"Окружность   "); bpm.thotkey(tdft,1,"о",TRUE);
  bpm.tprint(tdft,2,"Дуга         "); bpm.thotkey(tdft,2,"д",TRUE);
  bpm.tprint(tdft,3,"Ломаная      "); bpm.thotkey(tdft,3,"л",TRUE);
  bpm.tprint(tdft,4,"прямоуГольник"); bpm.thotkey(tdft,4,"г",TRUE);
  bpm.tprint(tdft,5,"Текст        "); bpm.thotkey(tdft,5,"т",TRUE);
  bpm.tprint(tdft,6,"Штриховка    "); bpm.thotkey(tdft,6,"ш",TRUE);
  bpm.tprint(tdft,7,"переРисовать "); bpm.thotkey(tdft,7,"р",TRUE);

  bpm.tnew(tscr,1,7,but[2].x,Y-hf*8,120,hf,bpm.xsel+bpm.xup,' Экран');
  bpm.tprint(tscr,0,"увеличить Окно ");  bpm.thotkey(tscr,0,"о",TRUE);
  bpm.tprint(tscr,1,"Показать все   ");  bpm.thotkey(tscr,1,"п",TRUE);
  bpm.tprint(tscr,2,"поКазать окно");    bpm.thotkey(tscr,2,"к",TRUE);
  bpm.tprint(tscr,3,'Масштаб %-3f   ',bas.cview^.scale);
                                         bpm.thotkey(tscr,3,"м",TRUE);
  bpm.tprint(tscr,4,"Центр экрана  ");   bpm.thotkey(tscr,4,"ц",TRUE);
  bpm.tprint(tscr,5,'новый Вид      ');  bpm.thotkey(tscr,5,"в",TRUE);
  bpm.tprint(tscr,6,'Уменшить вид   ');  bpm.thotkey(tscr,6,"у",TRUE);

  bpm.tnew(tedt,1,9,but[1].x,Y-hf*10,140,hf,bpm.xsel+bpm.xup,' Изменения ');
  bpm.tprint(tedt,0,"Удалить     ");        bpm.thotkey(tedt,0,"у",TRUE);
  bpm.tprint(tedt,1,"Сдвинуть    ");        bpm.thotkey(tedt,1,"с",TRUE);
  bpm.tprint(tedt,2,"Копировать  ");        bpm.thotkey(tedt,2,"к",TRUE);
  bpm.tprint(tedt,3,"Зеркало     ");        bpm.thotkey(tedt,3,"з",TRUE);
  bpm.tprint(tedt,4,"Поворот     ");        bpm.thotkey(tedt,4,"п",TRUE);
  bpm.tprint(tedt,5,"пеРерисовать");        bpm.thotkey(tedt,5,"р",TRUE);
  bpm.tprint(tedt,6,"редактор Ломаных ");   bpm.thotkey(tedt,6,"л",TRUE);
  bpm.tprint(tedt,7,"собрать Группу ");     bpm.thotkey(tedt,7,"г",TRUE);
  bpm.tprint(tedt,8,"разоБрать группу ");   bpm.thotkey(tedt,8,"б",TRUE);

  bpm.tnew(tfil,1,7,but[3].x,Y-hf*8,120,hf,bpm.xsel+bpm.xup,' Файлы');
  bpm.tprint(tfil,0,'Запись файла');      bpm.thotkey(tfil,0,"з",TRUE);
  bpm.tprint(tfil,1,'Чтение файла');      bpm.thotkey(tfil,1,"ч",TRUE);
  bpm.tprint(tfil,2,'запись Макроса');    bpm.thotkey(tfil,2,"м",TRUE);
  bpm.tprint(tfil,3,'чтение маКроса');    bpm.thotkey(tfil,3,"к",TRUE);
  bpm.tprint(tfil,4,'Выход с записью');   bpm.thotkey(tfil,4,"в",TRUE);
  bpm.tprint(tfil,5,'выХод без записи');  bpm.thotkey(tfil,5,"х",TRUE);
  bpm.tprint(tfil,6,'Плоттер    ');       bpm.thotkey(tfil,6,"п",TRUE);

  bpm.tnew(tset,1,6,but[4].x,Y-hf*7,95,hf,bpm.xsel+bpm.xup,' Установки');
  bpm.tprint(tset,0,"Цвет      ");  bpm.thotkey(tset,0,"ц",TRUE);
  bpm.tprint(tset,1,"Тип линий ");  bpm.thotkey(tset,1,"т",TRUE);
  bpm.tprint(tset,2,"Шаг       ");  bpm.thotkey(tset,2,"ш",TRUE);
  bpm.tprint(tset,3,"Сетка     ");  bpm.thotkey(tset,3,"с",TRUE);
  bpm.tprint(tset,4,"сЛои      ");  bpm.thotkey(tset,4,"л",TRUE);
  bpm.tprint(tset,5,"English   ");  bpm.thotkey(tset,5,"e",TRUE);

  bpm.tnew(tdim,1,6,but[5].x,Y-hf*7,125,hf,bpm.xsel+bpm.xup,' Размеры');
  bpm.tprint(tdim,0,'Горизонтальный');  bpm.thotkey(tdim,0,"г",TRUE);
  bpm.tprint(tdim,1,'Вертикальный  ');  bpm.thotkey(tdim,1,"в",TRUE);
  bpm.tprint(tdim,2,'Радиус        ');  bpm.thotkey(tdim,2,"р",TRUE);
  bpm.tprint(tdim,3,'Диаметр       ');  bpm.thotkey(tdim,3,"д",TRUE);
  bpm.tprint(tdim,4,'Угол          ');  bpm.thotkey(tdim,4,"у",TRUE);
  bpm.tprint(tdim,5,'Наклонный     ');  bpm.thotkey(tdim,5,"н",TRUE);
  bpm.tdisable(tdim,2);
  bpm.tdisable(tdim,3);
  bpm.tdisable(tdim,5);

  bpm.tnew(tmod,1,5,but[6].x,Y-hf*6,115,hf,bpm.xsel+bpm.xup,' Модель');
  bpm.tprint(tmod,0,'новая Модель   ');  bpm.thotkey(tmod,0,"м",TRUE);
  bpm.tprint(tmod,1,'Прочесть модель');  bpm.thotkey(tmod,1,"п",TRUE);
  bpm.tprint(tmod,2,'Записать модель');  bpm.thotkey(tmod,2,"з",TRUE);
  bpm.tprint(tmod,3,'запиСать все   ');  bpm.thotkey(tmod,3,"с",TRUE);
  bpm.tprint(tmod,4,'уДалить модель ');  bpm.thotkey(tmod,4,"д",TRUE);
END init_rus;

PROCEDURE init_eng;
  VAR wn: bpm.WINDOW;
       s: ARRAY [0..6] OF STRING;
       i: INTEGER;
BEGIN
  bpm.newstr(s[0],'Picture');
  bpm.newstr(s[1],'Edit');
  bpm.newstr(s[2],'View');
  bpm.newstr(s[3],'Files');
  bpm.newstr(s[4],'Settings');
  bpm.newstr(s[5],'Dimension');
  bpm.newstr(s[6],'Model');

  bpm.bnew(base,s,X,Y,bpm.xsel,'');

  bpm.bhotkey(base,0,"p",TRUE);
  bpm.bhotkey(base,1,"e",TRUE);
  bpm.bhotkey(base,2,"v",TRUE);
  bpm.bhotkey(base,3,"f",TRUE);
  bpm.bhotkey(base,4,"s",TRUE);
  bpm.bhotkey(base,5,"d",TRUE);
  bpm.bhotkey(base,6,"m",TRUE);

  bpm.bwindow(base,wn);
  FOR i:=0 TO HIGH(but) DO
    bpm.bblocks(base,i,but[i]);
    but[i].x:=but[i].x+wn^.inner.zX+X
  END;

  bpm.tnew(tdft,1,8,but[0].x,Y-hf*9,80,hf,bpm.xsel+bpm.xup,' Picture');
  bpm.tprint(tdft,0,"Line   "); bpm.thotkey(tdft,0,"l",TRUE);
  bpm.tprint(tdft,1,"Circle "); bpm.thotkey(tdft,1,"c",TRUE);
  bpm.tprint(tdft,2,"Arca3  "); bpm.thotkey(tdft,2,"a",TRUE);
  bpm.tprint(tdft,3,"Pline  "); bpm.thotkey(tdft,3,"p",TRUE);
  bpm.tprint(tdft,4,"Box    "); bpm.thotkey(tdft,4,"b",TRUE);
  bpm.tprint(tdft,5,"Text   "); bpm.thotkey(tdft,5,"t",TRUE);
  bpm.tprint(tdft,6,"Hatch  "); bpm.thotkey(tdft,6,"h",TRUE);
  bpm.tprint(tdft,7,"Refresh"); bpm.thotkey(tdft,7,"r",TRUE);

  bpm.tnew(tscr,1,7,but[2].x,Y-hf*8,100,hf,bpm.xsel+bpm.xup,' View ');
  bpm.tprint(tscr,0,"zoom Window ");  bpm.thotkey(tscr,0,"w",TRUE);
  bpm.tprint(tscr,1,"zoom All    ");  bpm.thotkey(tscr,1,"a",TRUE);
  bpm.tprint(tscr,2,"zoom Dynamix");  bpm.thotkey(tscr,2,"d",TRUE);
  bpm.tprint(tscr,3,'Scale %-3f',bas.cview^.scale);
                                      bpm.thotkey(tscr,3,"s",TRUE);
  bpm.tprint(tscr,4,"Center screen"); bpm.thotkey(tscr,4,"c",TRUE);
  bpm.tprint(tscr,5,'New view     '); bpm.thotkey(tscr,5,"n",TRUE);
  bpm.tprint(tscr,6,'Unzoom views '); bpm.thotkey(tscr,6,"u",TRUE);

  bpm.tnew(tedt,1,9,but[1].x,Y-hf*10,90,hf,bpm.xsel+bpm.xup,' Edit ');
  bpm.tprint(tedt,0,"Delete      ");  bpm.thotkey(tedt,0,"d",TRUE);
  bpm.tprint(tedt,1,"Shift       ");  bpm.thotkey(tedt,1,"s",TRUE);
  bpm.tprint(tedt,2,"Copy        ");  bpm.thotkey(tedt,2,"c",TRUE);
  bpm.tprint(tedt,3,"Mirror      ");  bpm.thotkey(tedt,3,"m",TRUE);
  bpm.tprint(tedt,4,"Turn        ");  bpm.thotkey(tedt,4,"t",TRUE);
  bpm.tprint(tedt,5,"Refresh     ");  bpm.thotkey(tedt,5,"r",TRUE);
  bpm.tprint(tedt,6,"edit Pline  ");  bpm.thotkey(tedt,6,"p",TRUE);
  bpm.tprint(tedt,7,"make Group  ");  bpm.thotkey(tedt,7,"g",TRUE);
  bpm.tprint(tedt,8,"remake grOup");  bpm.thotkey(tedt,8,"o",TRUE);

  bpm.tnew(tfil,1,7,but[3].x,Y-hf*8,90,hf,bpm.xsel+bpm.xup,' Files');
  bpm.tprint(tfil,0,'Write      ');  bpm.thotkey(tfil,0,"w",TRUE);
  bpm.tprint(tfil,1,'Read       ');  bpm.thotkey(tfil,1,"r",TRUE);
  bpm.tprint(tfil,2,'Save macros');  bpm.thotkey(tfil,2,"m",TRUE);
  bpm.tprint(tfil,3,'Load macros');  bpm.thotkey(tfil,3,"l",TRUE);
  bpm.tprint(tfil,4,'Exit      ');   bpm.thotkey(tfil,4,"e",TRUE);
  bpm.tprint(tfil,5,'Quit      ');   bpm.thotkey(tfil,5,"q",TRUE);
  bpm.tprint(tfil,6,'Plot in file'); bpm.thotkey(tfil,6,"p",TRUE);

  bpm.tnew(tset,1,6,but[4].x,Y-hf*7,80,hf,bpm.xsel+bpm.xup,' Settings');
  bpm.tprint(tset,0,"Color     ");  bpm.thotkey(tset,0,"c",TRUE);
  bpm.tprint(tset,1,"Type  line");  bpm.thotkey(tset,1,"t",TRUE);
  bpm.tprint(tset,2,"Step      ");  bpm.thotkey(tset,2,"s",TRUE);
  bpm.tprint(tset,3,"Grid      ");  bpm.thotkey(tset,3,"g",TRUE);
  bpm.tprint(tset,4,"Layer     ");  bpm.thotkey(tset,4,"l",TRUE);
  bpm.tprint(tset,5,"Русский   ");  bpm.thotkey(tset,5,"р",TRUE);

  bpm.tnew(tdim,1,6,but[5].x,Y-hf*7,85,hf,bpm.xsel+bpm.xup,' Dimension');
  bpm.tprint(tdim,0,'Horizontal');  bpm.thotkey(tdim,0,"h",TRUE);
  bpm.tprint(tdim,1,'Vertical  ');  bpm.thotkey(tdim,1,"v",TRUE);
  bpm.tprint(tdim,2,'Radius    ');  bpm.thotkey(tdim,2,"r",TRUE);
  bpm.tprint(tdim,3,'Diameter  ');  bpm.thotkey(tdim,3,"d",TRUE);
  bpm.tprint(tdim,4,'Angle     ');  bpm.thotkey(tdim,4,"a",TRUE);
  bpm.tprint(tdim,5,'Other     ');  bpm.thotkey(tdim,5,"o",TRUE);
  bpm.tdisable(tdim,2);
  bpm.tdisable(tdim,3);
  bpm.tdisable(tdim,5);

  bpm.tnew(tmod,1,5,but[6].x,Y-hf*6,95,hf,bpm.xsel+bpm.xup,' Model');
  bpm.tprint(tmod,0,'new Model    ');   bpm.thotkey(tmod,0,"m",TRUE);
  bpm.tprint(tmod,1,'Read model   ');   bpm.thotkey(tmod,1,"r",TRUE);
  bpm.tprint(tmod,2,'Write model  ');   bpm.thotkey(tmod,2,"w",TRUE);
  bpm.tprint(tmod,3,'write All    ');   bpm.thotkey(tmod,3,"a",TRUE);
  bpm.tprint(tmod,4,'Dispose model');   bpm.thotkey(tmod,4,"d",TRUE);
END init_eng;

PROCEDURE _init;
BEGIN
  IF bas.ENG THEN  init_rus ELSE init_eng END;
  bpm.bopen(base);
  bas.ENG:= NOT bas.ENG
END _init;

PROCEDURE draw(VAR sel: INTEGER);
BEGIN
  bpm.topen(tdft);
  LOOP
    bpm.tselect(tdft); bas.wmonitor; _move;
    IF NOT bpm.tselected(tdft) THEN
      sel:=bpm.bbutton(base,bpm.mx,bpm.my);
      bpm.bchoose(base,sel);
      bpm.tclose(tdft); EXIT
    END;
    CASE bpm.talt(tdft) OF
       0:  _close(tdft); dft.line;     _open(tdft)
      |1:  _close(tdft); dft.circle;   _open(tdft)
      |2:  _close(tdft); dft.arca;     _open(tdft)
      |3:  _close(tdft); dft.pline;    _open(tdft)
      |4:  _close(tdft); dft.box;      _open(tdft)
      |5:  _close(tdft); dft.text;     _open(tdft)
      |6:  _close(tdft); dft.hatch;    _open(tdft)
      |7:  bas.vredraw(bas.cview)
    ELSE END;
    bpm.tunselect(tdft)
  END
END draw;

PROCEDURE tools(VAR sel: INTEGER);
BEGIN
  bpm.topen(tedt);
  LOOP
    bpm.tselect(tedt);
    IF NOT bpm.tselected(tedt) THEN
      sel:=bpm.bbutton(base,bpm.mx,bpm.my);
      bpm.bchoose(base,sel);
      bpm.tclose(tedt); EXIT
    END;
    CASE bpm.talt(tedt) OF
       0:  _close(tedt); tol.delete;     _open(tedt)
      |1:  _close(tedt); tol.shift;      _open(tedt)
      |2:  _close(tedt); tol.copy;       _open(tedt)
      |3:  _close(tedt); tol.mirror;     _open(tedt)
      |4:  _close(tedt); tol.turn;       _open(tedt)
      |5:  bas.vredraw(bas.cview)
      |6:  _close(tedt); tol.ed_pline;   _open(tedt)
      |7:  _close(tedt); tol.make_group; _open(tedt)
      |8:  _close(tedt); tol.dest_group; _open(tedt)
    ELSE END;
    bpm.tunselect(tedt)
  END
END tools;

PROCEDURE options(VAR sel: INTEGER);
BEGIN
  bpm.topen(tset);
  LOOP
    bpm.tselect(tset);
    IF NOT bpm.tselected(tset) THEN
      sel:=bpm.bbutton(base,bpm.mx,bpm.my);
      bpm.bchoose(base,sel);
      bpm.tclose(tset); EXIT
    END;
    CASE bpm.talt(tset) OF
       0: set.color(but[4].x,Y-36)
      |1: set.type_line (but[4].x,Y-36)
      |2: set.step(but[4].x+20,Y-40)
      |3: set.grid(but[4].x,Y-50)
      |4: set.layers(but[4].x,Y-50)
      |5: _dispose; _init; sel:=4; RETURN
    ELSE  END;
    bpm.tunselect(tset)
  END
END options;

PROCEDURE files(VAR sel: INTEGER);
 VAR  x,y: INTEGER;
BEGIN
  x:=but[3].x+10; y:=but[3].y-15+Y;
  bpm.topen(tfil);
  LOOP
    bpm.tselect(tfil);
    IF NOT bpm.tselected(tfil) THEN
      sel:=bpm.bbutton(base,bpm.mx,bpm.my);
      bpm.bchoose(base,sel);
      bpm.tclose(tfil); EXIT
    END;
    CASE bpm.talt(tfil) OF
       0: fil.save(x,y)
      |1: IF fil.load(x,y)  THEN  bas.mrefresh  END
      |2: _close(tfil); fil.save_block(x,y); _open(tfil)
      |3: _close(tfil); IF fil.load_block(x,y) THEN bas.mrefresh END; _open(tfil)
      |4: fil.exit(bas.SW DIV 2,bas.SH DIV 2);
      |5: fil.quit(bas.SW DIV 2,bas.SH DIV 2);
      |6: fil.plot(x,y);
    ELSE END;
    bpm.tunselect(tfil)
  END
END files;

PROCEDURE screen(VAR sel: INTEGER);
VAR s1,s3,s3_: ARRAY [0..31] OF CHAR;
BEGIN
  IF bas.ENG THEN
     s1:= 'Scale';
     s3:= 'sTep by grid';  s3_:= 'sTep off        ';
  ELSE
     s1:= 'Масштаб';
     s3:= 'Шаг по сетке ';     s3_:= 'Шаг выкл.      ';
  END;
  bpm.tprint(tscr,3,'%s %-3f   ',s1,bas.cview^.scale);
  bpm.topen(tscr);
  LOOP
    bpm.tselect(tscr);
    IF NOT bpm.tselected(tscr) THEN
      sel:=bpm.bbutton(base,bpm.mx,bpm.my);
      bpm.bchoose(base,sel);
      bpm.tclose(tscr); EXIT
    END;
    WITH bas.cview^ DO
      CASE bpm.talt(tscr) OF
         0: _close(tscr); tol.zoom_w;
            bpm.tprint(tscr,3,'%s %-3f   ',s1,scale); _open(tscr)
        |1: tol.zoom_a; bpm.tprint(tscr,3,'%s %-3f   ',s1,scale)
        |2: _close(tscr); tol.zoom_d;
            bpm.tprint(tscr,3,'%s %-3f   ',s1,scale);
            _open(tscr)
        |3: tol.scale; bpm.tprint(tscr,3,'%s %-3f   ',s1,scale);
        |4: _close(tscr); tol.center; _open(tscr); bpm.tunselect(tscr)
        |5: bas.create_view; bpm.tontop(tscr)
        |6: bas.unzoom_views
      ELSE END
    END;
    bpm.tunselect(tscr)
  END
END screen;

PROCEDURE model(VAR sel: INTEGER);
VAR  x,y: INTEGER;
BEGIN
  x:=but[6].x+10; y:=but[6].y-15+Y;
  bpm.topen(tmod);
  LOOP
    bpm.tselect(tmod);
    IF NOT bpm.tselected(tmod) THEN
      sel:=bpm.bbutton(base,bpm.mx,bpm.my);
      bpm.bchoose(base,sel);
      bpm.tclose(tmod); EXIT
    END;
    CASE bpm.talt(tmod) OF
       0: fil.create_model(x,y-40); bpm.tontop(tmod)
      |1: fil.read_model(x,y)
      |2: fil.write_model(x+20,y-40)
      |3: fil.write_all(x+10,y-40)
      |4: fil.remove_model(x+20,y-40);
    ELSE  END;
    bpm.tunselect(tmod)
  END
END model;

PROCEDURE dimension(VAR sel: INTEGER);
BEGIN
  bpm.topen(tdim);
  LOOP
    bpm.tselect(tdim);
    IF NOT bpm.tselected(tdim) THEN
      sel:=bpm.bbutton(base,bpm.mx,bpm.my);
      bpm.bchoose(base,sel);
      bpm.tclose(tdim);  EXIT
    END;
    CASE bpm.talt(tdim) OF
       0: _close(tdim); dim.horizontal; _open(tdim)
      |1: _close(tdim); dim.vertical;   _open(tdim)
      |2: _close(tdim); dim.radius;     _open(tdim)
      |3: _close(tdim); dim.diametr;    _open(tdim)
      |4: _close(tdim); dim.angle;      _open(tdim)
      |5:
    ELSE END;
    bpm.tunselect(tdim)
  END
END dimension;

PROCEDURE monitor;
 VAR select: INTEGER;
BEGIN
  select:=-1;
  bpm.bopen(base);
  LOOP
    IF select=-1 THEN
      bpm.bunselect(base); bpm.bselect(base);
      bas.wmonitor; _move
    ELSE bpm.bchoose(base,select) END;
    IF bpm.bselected(base) THEN
      CASE bpm.balt(base) OF
        0: draw(select)
       |1: tools(select)
       |2: screen(select)
       |3: files(select)
       |4: options(select)
       |5: dimension(select)
       |6: model(select)
      ELSE bpm.bunselect(base); select:=-1 END
    END;
    bpm.bontop(base)
  END
END monitor;

BEGIN
  hf:= bpm.font^.H+6;
  Y:=bas.SH-hf; X:=50;
  init_eng;
  bas.ENG:=TRUE;
  monitor
END bcad.
