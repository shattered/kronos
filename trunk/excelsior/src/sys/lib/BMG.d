DEFINITION MODULE BMG; (* nick 02-Mar-90. (c) KRONOS *)

IMPORT  SYSTEM, defScreen, defBMG, defFont;

TYPE
  TOOL  = defScreen.TOOL;   BMD    = defBMG.BMD;
  BLOCK = defScreen.BLOCK;  BITMAP = defBMG.BITMAP;

CONST
  rep = defScreen.rep;          xor = defScreen.xor;
  or  = defScreen.or;           bic = defScreen.bic;

---------------------- BitBlock Procedures ---------------------
                      ---------------------

PROCEDURE cross(VAR des: BLOCK; blk0,blk1: BLOCK);

PROCEDURE bblt(des: BITMAP; dtool: TOOL;
               x,y: INTEGER;
               sou: BITMAP; stool: TOOL; block: BLOCK);

---------------  Graphic Primitive Procedures  -----------------
               --------------------------------

PROCEDURE erase(bmd: BITMAP);

PROCEDURE fill (bmd: BITMAP;  t: TOOL;  block: BLOCK;
                  w: INTEGER; SEQ pattern: SYSTEM.WORD);

PROCEDURE pattern(bmd: BITMAP;  t: TOOL; block: BLOCK;
                  w,h: INTEGER; pattern: ARRAY OF SYSTEM.WORD);

PROCEDURE grid(bmd: BITMAP; t: TOOL; block: BLOCK; xstep,ystep: INTEGER);

PROCEDURE dot  (bmd: BITMAP; t: TOOL; X,Y: INTEGER);

PROCEDURE line (bmd: BITMAP; t: TOOL; X0,Y0,X1,Y1: INTEGER);
PROCEDURE dline(bmd: BITMAP; t: TOOL; X0,Y0,X1,Y1: INTEGER;
                                        VAR r: SYSTEM.WORD);
PROCEDURE rect (bmd: BITMAP; t: TOOL; X0,Y0,X1,Y1: INTEGER);
PROCEDURE frame(bmd: BITMAP; t: TOOL; X0,Y0,X1,Y1: INTEGER);
PROCEDURE arc  (bmd: BITMAP; t: TOOL; X0,Y0,xa,ya,xb,yb,r: INTEGER);
PROCEDURE arc3 (bmd: BITMAP; t: TOOL; x0,y0,x1,y1,x2,y2: INTEGER);

PROCEDURE circle (bmd: BITMAP; t: TOOL; X,Y,R: INTEGER);
PROCEDURE circlef(bmd: BITMAP; t: TOOL; X,Y,R: INTEGER);
PROCEDURE ring   (bmd: BITMAP; t: TOOL; x0,y0,r1,r2: INTEGER);

PROCEDURE ellipse0 (bmd: BITMAP; t: TOOL; xc,yc,rx,ry: INTEGER);
PROCEDURE ellipse0f(bmd: BITMAP; t: TOOL; xc,yc,rx,ry: INTEGER);

PROCEDURE polyline0(bmd: BITMAP; t: TOOL; SEQ xy: INTEGER);
PROCEDURE polyline1(bmd: BITMAP; t: TOOL;     xy: ARRAY OF INTEGER);

PROCEDURE trif(bmd: BITMAP; t: TOOL; x0,y0,x1,y1,x2,y2: INTEGER);

----------------------------------------------------------------

PROCEDURE scroll(bmd: BITMAP; tool: TOOL; x,y: INTEGER);

PROCEDURE offset(bmd: BITMAP; x,y,layer: INTEGER;
             VAR adr: SYSTEM.ADDRESS; VAR bitoffset: INTEGER);

----------------------------------------------------------------

PROCEDURE lenght(fnt: defFont.FONT; fmt: ARRAY OF CHAR;
                                SEQ arg: SYSTEM.WORD): INTEGER;

PROCEDURE width (fnt: defFont.FONT; fmt: ARRAY OF CHAR;
                                SEQ arg: SYSTEM.WORD): INTEGER;

PROCEDURE margin(fnt: defFont.FONT; fmt: ARRAY OF CHAR;
                                SEQ arg: SYSTEM.WORD): INTEGER;

PROCEDURE write(bmd: BITMAP;  tool: TOOL;  x,y: INTEGER;  fnt: defFont.FONT;
                str: ARRAY OF CHAR;    pos,len: INTEGER);

PROCEDURE xwrite(bmd: BITMAP;  tool: TOOL;  x,y: INTEGER;  fnt: defFont.FONT;
                 str: ARRAY OF CHAR;    pos,len: INTEGER): INTEGER;

PROCEDURE print(bmd: BITMAP;  tool: TOOL;  x,y: INTEGER;  fnt: defFont.FONT;
                fmt: ARRAY OF CHAR;    SEQ arg: SYSTEM.WORD);

PROCEDURE xprint(bmd: BITMAP;  tool: TOOL;  x,y: INTEGER;  fnt: defFont.FONT;
                 fmt: ARRAY OF CHAR;    SEQ arg: SYSTEM.WORD): INTEGER;

PROCEDURE writech(bmd: BITMAP; tool: TOOL; x,y: INTEGER;
                  fnt: defFont.FONT;        ch: CHAR);

END BMG.

----------------------------------------------------------------


PROCEDURE scroll(bmd: BITMAP; tool: TOOL; x,y: INTEGER);
--------------------------------------------------------
     Rectangle region clipped in tool scrolls;
     point (0,0) goes to (x,y)
     Free area filled by calling rect(bmd,tool)

PROCEDURE offset(bmd: BITMAP; x,y,layer: INTEGER;
                 VAR adr: SYSTEM.ADDRESS; VAR off: INTEGER);
------------------------------------------------------------
     Вычисляет  адрес  слова  и номер бита в нем, соответствующие
координатам x,y

PROCEDURE cross(VAR des: BLOCK; blk0,blk1: BLOCK);
--------------------------------------------------
     Процедура  cross вычисляет пересечение двух блоков blk0,
blk1  и  параметры результирующего прямоугольника пересечения
заносит в дескриптор блока des. Если же пересечение пусто, то
ширина и высота в des будут равны 0.


PROCEDURE bblt(des: BITMAP; dblk: BLOCK;
               sou: BITMAP; sblk: BLOCK; mode: INTEGER);
--------------------------------------------------------
     bblt (bIT bLOCK tRANSFER) - процедура пересылки битового
блока-источника  sblk  из  битовой  карты sou в блок-приемник
dblk  в  битовой  карте des, с совмещением левых нижних углов
блоков  иточника и приемника. Прерсылка осуществляется только
между  двумя битовыми слоями, заданными значениями полей BASE
в  дескрипторах битовых карт sou и des. В которые должны быть
предварительно  занесены  соответствующие значения из массива
базовых  адресов  слоев  битовой карты. Как и все графические
операции,  bblt  выполняется  с  определенной модой опeрации,
которая задается значением параметра mode.

PROCEDURE overlap(des: BITMAP; VAR dblk: BLOCK;
                  sou: BITMAP; VAR sblk: BLOCK);
------------------------------------------------
     Результатом  работы  этой  процедуры  являются  координаты и
размеры  блоков  источника  и  премника,  отсеченные  по границам
битовой карты в соответсвии с семантикой процедуры bblt. В случае
если  высота  и ширина одного из этих блоков равны 0, то вызывать
затем  bblt не бессмысленен. Так как один из этих блоков лежит за
пределами своей битовой карты.

PROCEDURE erase(bmd: BITMAP; tool: TOOL);
-----------------------------------------
     Процедура   erase  предназначена  для  полного  стирания
определенного  набора  слоев в битовой карте. Конкретные слои
назначенные  для  стирания  задаются  полем  color в описании
иструмента tool.

PROCEDURE fill(bmd: BITMAP; tool: TOOL;
                 w: INTEGER; SEQ pattern: SYSTEM.WORD);
-------------------------------------------------------
PROCEDURE pattern(bmd: BITMAP; tool: TOOL;
                    w: INTEGER; pattern: ARRAY OF SYSTEM.WORD);
---------------------------------------------------------------

     Обе  эти  процедуры, заполняют прямоугольною область экрана,
битовым шаблоном цвета color, шириной w и высотой определяемой по
правилу:

   (Размер  pattern  в  словах)  DIV (ширина w, в словах)

        /   (HIGH(pattern)+1) DIV ((w+31) DIV 32)   /

     В  качестве прямогольника для заполнения берется область
задаваемая  прямоугольником отсечения clip в описании инстру-
мента.

----------------------------------------------------------------

     Во всех ниже описанных процедурах bmd - указатель на битовую
карту,  где будет рисоваться заданный примитив, tool - инструмент
рисования.

PROCEDURE dot(...  x,y: INTEGER);   -   Постановка точки.

PROCEDURE line(...  x0,y0,x1,y1: INTEGER);
    Рисование отрезка прямой по координатам его концов.

PROCEDURE dline(... X0,Y0,X1,Y1: INTEGER; VAR r: SYSTEM.WORD);
    Рисование отрезка пунктирной линиии по координатам ее концов.
    По рисованию каждой точки "r" проворачивается на один бит r:=r>>1.
    (Точки соответствующие 0 в "r" не рисуются (даже в rep моде!))

PROCEDURE rect (...  x0,y0,x1,y1: INTEGER);
PROCEDURE frame(...  x0,y0,x1,y1: INTEGER);
    Рисование прямоугольника и рамки, соответсвенно, по коор-
динатам их диагональных вершин.

PROCEDURE arc(...  xc,yc,xa,ya,xb,yb,r: INTEGER);

     Рисование  дуги  окружности  с  центром  в точке (xc,yc)
радиуса r, от луча [(xc,yc),(xa,ya)) к лучу [(xc,yc),(xb,yb))

PROCEDURE arc3(...   x0,y0,x1,y1,x2,y2: INTEGER);

    Рисование  дуги окружности от точки (x0,y0) через (x1,y1)
до точки (x2,y2).

PROCEDURE        circle(...   xc,yc,rad: INTEGER);
PROCEDURE filled_circle(...  xc,yc,rad: INTEGER);

     Рисование  окружности  и круга: xc, yc - координаты цен-
тра, rad - радиус.

PROCEDURE ring_circle(... xc,yc,r1,r2: INTEGER);

     Рисование  круглого кольца по координатам центра xc, yc,
внутреннему и внешнему радиусам.

PROCEDURE ellipse(...   xc,yc,rx,ry: INTEGER);
PROCEDURE filled_ellipse(...   xc,yc,rx,ry: INTEGER);

     Элипс  и  закрашенный элипс: xc, yc - координаты центра;
rx,  ry - величины полуосей параллельных осям X и Y, соответ-
ственно.


PROCEDURE polyline0(...   SEQ xy: INTEGER);
PROCEDURE polyline1(...       xy: ARRAY OF INTEGER);

     Рисование ломаной линии заданной набором точек. Первой в
паре  лежит координата по X, второй - координата по Y. Если в
последовательности  или массиве количество элементов нечетно,
то последний элемент при рисовании непринимается во внимание.

PROCEDURE trif(... x0,y0,x1,y1,x2,y2: INTEGER);

    Рисование  закрашенного  треугольника по координатам трех
его вершин.

----------------------------------------------------------------

     Набор  процедур,  позволяющих  отобразить отдельный символ и
строку  символов, заданным шрифтом fnt на битовой карте, заданной
параметром bmd, в позиции (x,y) инструментом tool.

     Координаты  левого  нижнего  угла  знакоместа, при рисовании
символа,  и минимального прямоугольника охватывающего строку, при
отображении   строки  символов  исчисляются  относительно  левого
нижнего   угла  области  отсечения,  определяемой  полем  clip  в
описании инструмента.

PROCEDURE  write(... str: ARRAY OF CHAR; pos,len: INTEGER);
PROCEDURE xwrite(... str: ARRAY OF CHAR;
                               pos,len: INTEGER): INTEGER;

     Отображают  из  строки  str  с  позиции pos len символов
шрифтом fnt. Процедура xwrite возвращает горизонтальную коор-
динату  точки,  следующей непосредственно за последней точкой
выведенной строки.


PROCEDURE  print(... f: ARRAY OF CHAR; SEQ arg: SYSTEM.WORD);
PROCEDURE xprint(... f: ARRAY OF CHAR;
                         SEQ arg: SYSTEM.WORD): INTEGER;

     Печатают  значения  параметров  arg в формате заданном в
строке  f. Процедура xwrite возвращает горизонтальную коорди-
нату точки, следующей непосредственно за последней точкой вы-
веденной строки.


PROCEDURE writech(...  x,y: INTEGER; fnt: FONT; ch: CHAR);
    Отображает символ ch из шрифта fnt в позиции (x,y).


PROCEDURE lenght(fnt: FONT; fmt: ARRAY OF CHAR;
                        SEQ arg: SYSTEM.WORD): INTEGER;

    Returns the next pixel position for calling next print.


PROCEDURE width (fnt: FONT; fmt: ARRAY OF CHAR;
                        SEQ arg: SYSTEM.WORD): INTEGER;


     Подсчитывает  ширину  области в пикселах, которую займет
строка,  отображенная  шрифтом fnt, при печати значений пара-
метров arg в формате заданном в строке fmt.

PROCEDURE margin (fnt: FONT; fmt: ARRAY OF CHAR;
                         SEQ arg: SYSTEM.WORD): INTEGER;

     Returns offset that must be added to the start print
position of the text (moving to the right), cause character
may be elongated to the left.


PROCEDURE grid(bmg,t,b,xs,ys)

     Draw a greed started at b.x,b.y with step xs,ys cliped by t.clip
