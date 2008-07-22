DEFINITION MODULE cdsHash; (* 09-Oct-87. (c) KRONOS *)

FROM Model      IMPORT  Object, Objects;

PROCEDURE Init(size: INTEGER);
(* Создает хэш-таблицу дла size объектов *)

PROCEDURE Insert(o: Object);
(* Помещает объект в таблицу,
   если в таблице был объект с таким именем и тегом то он из нее удаляется.
   При переполнении таблицы возникает Exception *)

PROCEDURE LookUp(name: ARRAY OF CHAR; tag: Objects): Object;
(* Ищет объект по его имени и тегу, если не находит то возвращает NIL *)

END cdsHash.
