DEFINITION MODULE vePbl; (* Sem 17-Feb-88. (c) KRONOS *)

FROM Model      IMPORT  Object;

VAR X,Y    : INTEGER; -- абсолютные координаты курсора
    WndX,
    WndY   : INTEGER; -- абсолютные координаты окна
    curpX,
    Scale  : INTEGER; -- масштаб окна
    curpY  : INTEGER; -- абсолютные координаты текущего рисунка
    Mdl    : Object;  -- редактируемая модель
    Root   : Object;  -- корень текущего рисунка
    CurPict: Object;  -- текущий рисунок

END vePbl.
