DEFINITION MODULE bcMath; (* brd 31-Mar-91. (c) KRONOS *)

IMPORT   def: bcDef;

PROCEDURE max(SEQ a: REAL): REAL;
PROCEDURE min(SEQ a: REAL): REAL;

PROCEDURE dist (x1,y1,x2,y2: REAL): REAL;
PROCEDURE dist1(top1,top2: def.VERTEX): REAL;

PROCEDURE calc_arc(VAR xc,yc,r: REAL; top1,top2,top3:def.VERTEX): BOOLEAN;
  (* по трем точкам дуги определяет ее центр и радиус *)

PROCEDURE angle_1(top1,top2,top3: def.VERTEX): REAL;
  (* угол при вершине треугольника top,top1,top2 в радианах *)

PROCEDURE angle_0(top,top1: def.VERTEX): REAL;
  (* угол между горизонтом и прямой top,top1 в радианах *)

PROCEDURE angle_(top,top1,top2: def.VERTEX): REAL;
  (* угол между прямой top,top1 и прямой top,top2
     pпротив часовой стрелки в радианах, от 0 до 2Pi *)

PROCEDURE angle0(top,top1: def.VERTEX): REAL;
  (* угол между горизонтом и прямой top,top1
     против часовой стрелки в градусах, от 0 до 360 *)

PROCEDURE angle1(top,top1,top2: def.VERTEX): REAL;
  (* угол при вершине треугольника top,top1,top2
     в градусах, от 0 до 180 *)

PROCEDURE angle(top,top1,top2: def.VERTEX): REAL;
  (* угол между прямой top,top1 и прямой top,top2
     pпротив часовой стрелки в градусах, от 0 до 360 *)

----------------------------- SPLIN ----------------------------
                             -------
PROCEDURE pro(x,y: ARRAY OF REAL; VAR pro: ARRAY OF REAL);

     (* по массиву узлов x,y рассчитывает производные в них *)

PROCEDURE splin(x,y,m: ARRAY OF REAL; X: REAL; VAR Y: REAL);

     (*   по   массиву   узлов<x,y>   и  производным  в  них  <m>
рассчитывает значение сплайна *)

PROCEDURE long(x,y:ARRAY OF REAL; VAR t:ARRAY OF REAL);

     (*  рассчитывает  значение  параметра t для узлов ломаной на
плоскости,  параметром  в  узле  принимается  сумма расстояний по
прямой между текущей и предыдущей точками *)

PROCEDURE t_pro(x,y,t: ARRAY OF REAL; VAR pro_x,pro_y: ARRAY OF REAL);

     (*  рассчитывает  в узлах (x,y) ломаной значения производных
pro_x,pro_y по параметру t *)

PROCEDURE  t_splin(x,y,pro_x,pro_y,t: ARRAY OF REAL; T: REAL; VAR X,Y:REAL);

     (*  для  параметра  Т  рассчитывает  координаты  точки (X,Y)
параметрически    заданной    кривой    (x,y)-    массив    узлов
аппроксимируемой   ломаной,   pro_x,pro_y -массивы призводных  по
парамеиру в узлах, t- массив значений параметра в узлах *)

------------------------- INTEGER CALC -------------------------

PROCEDURE muldiv(a,b,c: INTEGER): INTEGER; (* a*b/c *)

PROCEDURE sqrt(x: INTEGER): INTEGER;

---------------------------- matrix ----------------------------

TYPE VECT = def.VERTEX;
     MATR = ARRAY [0..2] OF VECT;

PROCEDURE MxM(VAR c: MATR; a,b: MATR);
PROCEDURE VxM(VAR c: VECT; a: VECT; b: MATR);
PROCEDURE MxV(VAR c: MATR; a: VECT; b: MATR);

END bcMath.
