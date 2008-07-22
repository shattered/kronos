DEFINITION MODULE defScreen; (* nick 04-Oct-90. (c) KRONOS *)

IMPORT  SYSTEM;

TYPE
  COLOR   = RECORD
              r: INTEGER;       (* intensity of red   beam *)
              g: INTEGER;       (* intensity of green beam *)
              b: INTEGER;       (* intensity of blue  beam *)
            END;

  PALETTE = DYNARR OF COLOR;

CONST           (* screen kind *)
   bitmap = 444D4224h; (* $BMD *)
   pelmap = 44504124h; (* $APD *)
   other  = 48544F24h; (* $OTH *)

TYPE
  STATE   = POINTER TO STATUS;
  STATUS  = RECORD
              type : INTEGER;  (* type of screen device              *)
              kind : INTEGER;  (* kind of implementation             *)
              W,H  : INTEGER;  (* width = W pixells, height = H pix  *)
              bpp  : INTEGER;  (* bIT pER pIXELL                     *)
              ldcx : INTEGER;  (* curren left down corner X position *)
              ldcy : INTEGER;  (* curren left down corner Y position *)
              dx   : INTEGER;  (* step by horizontal shifter         *)
              dy   : INTEGER;  (* step by vertical   shifter         *)
              xpix : INTEGER;  (* distance between pixells at X-axis *)
              ypix : INTEGER;  (* distance between pixells at Y-axis *)
              RGB  : INTEGER;  (* range of beam's intensity          *)
              pal  : PALETTE;  (* current palette                    *)
              ext  : INTEGER;  (* some extra descriptor              *)
            END;

  BLOCK   = RECORD x,y,w,h: INTEGER END;

  TOOL    = RECORD
              mode : INTEGER;    (* operation mode        *)
              mask : BITSET;     (* write mask            *)
              color: BITSET;     (* color/foreground      *)
              back : BITSET;     (* for text background   *)
              clip : BLOCK;      (* clipping rectangle    *)
              zX,zY: INTEGER;    (* (0,0) abs coordinates *)
            END;

CONST           (*  operation modes *)
   rep = 0;  or  = 1;  reverse = 4;
   xor = 2;  bic = 3;  normal = 0;

CONST           (* Driver Control Codes *)
   CONTROL  = 01h;
  _init     = 02h;
  _set_ldcx = 03h;
  _set_ldcy = 04h;
  _set_rgb  = 05h;
  _get_rgb  = 06h;
  _refresh  = 07h;
  _refreshw = 08h;

END defScreen.

(**************************************************************)

     В модуле defScreen вводятся базовыe типы для описания харак-
теристик  графических дисплеев, их особенностей, а также задаются
коды стандартных команд для програм-драйверов этих устройств.

     Буфер  кадра определяется как  двумерный  массив размером
ScrWidth на ScrHeight  n-разрядных слов, кодирующих цвет точек
экрана.

  Screen = ARRAY [0..ScrWidth-1][0..ScrHeight-1]
             OF ARRAY [0..n-1] OF BIT

  Mode        - двуместная побитовая логическая функция
                между содержимым экрана и заданным цветом
                операции Color
  Mask        - маска записи

     Операция постановки точки с координатами (X,Y) цвета Color и
маской записи Mask определяется так:

  Screen[X,Y]:=(Screen[X,Y]-mask) +
               (Screen[X,Y] Mode Color)*Mask .


----------------------------- TYPES ----------------------------
                             -------

BLOCK
-----
        x,y - горизонтальная и вертикальная координаты нижнего
              левого угла прямоугольника,
        w,h - его ширина и высота.

TOOL
----
        mask  -  маска записи  /см. 3.2.1/;
        color - "рабочий" цвет для рисования графических,
                 примитивов /линия, круг и т.д./, а также
                 цвет рисования символов;
        back  -  цвет фона, на котором рисуются символы.
        clip  -  описание прямоугольника, определяющего
                 область отсечения.
        mode  -  тип операции /см 1.1/, в данной версии
                 может принимать следующие значения:

         CONST
            rep = 0; (* destinator := sourse                    *)
            or  = 1; (* destinator := destinator OR  sourse     *)
            xor = 2; (* destinator := destinator XOR sourse     *)
            bic = 3; (* destinator := destinator AND NOT sourse *)

            normal  = 0; (* зарезервировано *)
            reverse = 4; (* зарезервировано *)

     Все координаты должны отсчитываться относительно точки (0,0)
области отсечения, а постановку на экран точки с координатами вы-
ходящими за переделы области отсечения игнорировать.

NOTE !!!!
     Область  отсечения  никогда  не  должна  выходить за пределы
экрана,и ее линейные размеры должны быть сторого больше нуля.


------------------------ COLOR, PALETTE ------------------------
                        ----------------

     COLOR  - описывает соотношение интесивностей основных цветов
                r - красный /rED/
                g - зеленый /gREEN/
                b - синий   /bLUE/

     PALETTE задает соответствие между кодом цвета, т.е. индексом
в массиве, и соотношением основных цветов.

SCREEN
------
type      - число определяющее тип устройства, например:
            type=0 => устройство IGD480
bpp       - число бит на точку
ldcx,ldcy - текущее положение отображаемой области, координа-
            тах буфера кадра, а
W и H     - ее ширина и высота в элементах изображения,
dx,dy     - шаг изменения положения  отображаемой области бу-
            фера кадра,
xpix,ypix - соотношение расстояний между элементами изображе-
            ния по вертикали и горизонтали, соотвественно, на
            физическом экране,
pal       - текущая палитра,
RGB       - максимальное  значение  интенсивностей  красного,
            синего и зеленого цветов,
desc      - ссылка на  дескриптор представления буфера кадра,
            например, указатель на переменную типа BMD,
kind      - вид конкретной реализации буфера кадра, в  данной
            версии модуля может принимать следующие значения:

bitmap = 444D4224h; (* $BMD *)
pelmap = 44504124h; (* $APD *)
other  = 48544F24h; (* $OTH *)

--------------------- DRIVER CONTROL CODES ---------------------
                     ----------------------


 CONTROL  = 01h; --
_init     = 02h; -- команда инициализации драйвера,
_set_ldcx = 03h; -- установка горизонтальной координаты поло-
                    жения отображаемой области буфера кадра,
_set_ldcy = 04h; -- установка вертикальной координаты положе-
                    ния отображаемой области буфера кадра,
_set_rgb  = 05h; -- запись нового содержимого палитры,
_get_rgb  = 06h; -- чтение текущего содержимого палитры.
_refresh  = 07h; -- refresh area
_refreshw = 08h; -- refresh area (rounded to word by x-direction)

================================================================

  (***********************************************************)
  (*  Screen  is                                             *)
  (*       ARRAY [0..ScrWidth-1][0..ScrHeight-1] OF          *)
  (*         ARRAY [0..n-1] OF BIT                           *)
  (*  mode - operation                                       *)
  (*                                                         *)
  (*  Screen[X,Y]:=(Screen[X,Y]-mask) +                      *)
  (*               (Screen[X,Y] mode color)*mask             *)
  (*                                                         *)
  (***********************************************************)
