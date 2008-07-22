DEFINITION MODULE def31dg; (* nick 01-Aug-90. (c) KRONOS *)

IMPORT  Vectors;

CONST X=0; Y=1; Z=2; W=3;
      a =70;
      b =10;
      SS=360;
      iSCALE = 1024;

TYPE
  VECTOR   = Vectors.VECTOR;
  TOP      = ARRAY [0..3] OF INTEGER;
  INT      = RECORD
               val: INTEGER;
               ref: INTEGER;
             END;

  POLYGON  = RECORD
               color: INTEGER;                 --- цвет треугольника
               intc : INTEGER;                 --- интенсивность в центре
               norm : VECTOR;
               tops : ARRAY [0..2] OF INTEGER; --- индекс вершины
               refs : ARRAY [0..2] OF INTEGER; --- next in list of polygons
               case : INTEGER; (* tops location case         *)
               next : INTEGER; (* next in image scan line    *)
               ref  : INTEGER; (* reference to display range *)
             END;

  OBJECT = RECORD
             name : STRING;             --- имя об'екта
             scale: INTEGER;            --- текущий масштаб отображения
             box  : TOP;                --- размеры охватывающего бокса
             cen  : TOP;                --- координаты центра об'екта
             tri  : INTEGER;            --- количество  треугольников
             top  : INTEGER;            --- количество  вершин
             poly : DYNARR OF POLYGON;  --- дескрипторы треугольников
             body : DYNARR OF TOP;      --- вершины в об'ектных координатах
             ints : DYNARR OF INT;      --- интенсивность в вершинах
             norms: DYNARR OF VECTOR;   --- усредненная нормаль в вершине
             image: DYNARR OF TOP;      --- вершины в координатах наблюдателя
          END;


END def31dg.
