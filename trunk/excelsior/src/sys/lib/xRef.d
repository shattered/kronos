DEFINITION MODULE xRef; (* Ned 29-Apr-89. (c) KRONOS *)

IMPORT  sym: coolSym;

VAL done: BOOLEAN;
   error: INTEGER;
   note : ARRAY [0..63] OF CHAR;

----------------------------------------------------------------

TYPE
  PROJECT;
  cu_ptr      = POINTER TO cu_rec;        -- compilation unit
  context_ptr = POINTER TO context_rec;
  obj_ptr     = POINTER TO obj_rec;
  type_ptr    = POINTER TO type_rec;


TYPE
  TYPEs       = DYNARR OF type_ptr;
  OBJs        = DYNARR OF obj_ptr;
  EXTs        = DYNARR OF cu_ptr;

---------------------------  TYPEs  ---------------------------
                           ---------

TYPE
  enum_ptr  = POINTER TO enum_rec;
  enum_rec  = RECORD
                id  : INTEGER;
                val : INTEGER;
                next: enum_ptr;
              END;

  parm_ptr  = POINTER TO parm_rec;
  parm_rec  = RECORD
                type: type_ptr;
                tags: BITSET;
                ofs : INTEGER;
                var : obj_ptr;  -- соответствующий локал
                next: parm_ptr;
              END;

  type_rec  = RECORD
                id   : INTEGER;
                modno: INTEGER;
                base : type_ptr;
                CASE mode: INTEGER OF
                  | sym.range   :  min,max: INTEGER;
                  | sym.array   :  inx    : type_ptr;
                  | sym.record  :  fields : obj_ptr;
                                   size   : INTEGER;
                  | sym.dynarr  :  dim    : INTEGER;
                  | sym.enumtype:  consts : enum_ptr;
                  | sym.proctype:  parms  : parm_ptr;
                END;
              END;

--------------------------  OBJECTs  --------------------------
                          -----------

TYPE
  xpos_ptr    = POINTER TO xpos_rec;
  xpos_rec    = RECORD
                  pc  : INTEGER;
                  line: INTEGER;
                  col : INTEGER;
                  next: xpos_ptr;
                END;

TYPE
  obj_rec     = RECORD
                  id   : INTEGER;
                  tags : BITSET;
                  type : type_ptr;
                  scope: INTEGER;
                  ofs  : INTEGER;
                  next : obj_ptr;
                  CASE :BOOLEAN OF
                    |FALSE: locs : context_ptr;
                                   (* for proc's & modules *)
                    |TRUE : parm : parm_ptr;
                                   (* for var *)
                  END;
                END;

  context_rec = RECORD
                  procs: obj_ptr;
                  mods : obj_ptr;
                  vars : obj_ptr;
                  cons : obj_ptr;
                  xpos : xpos_ptr;    -- only for proc
                END;

  cu_rec      = RECORD
                  complete: BOOLEAN;
                  unit    : INTEGER;  -- kind of compilation unit
                  def_time: INTEGER;  -- def time
                  imp_time: INTEGER;  -- imp time
                  exts    : EXTs;
                  types   : TYPEs;
                  proc_tab: OBJs;     -- procno -> obj_ptr
                  locs    : context_ptr;
                  names   : STRING;   -- names[0..] - cu name
                  language: INTEGER;  -- index of language
                                      -- name in names
                END;

----------------------------------------------------------------

VAR main: PROJECT;   -- список всех модулей

PROCEDURE new    (VAR pro: PROJECT);
PROCEDURE release(VAR pro: PROJECT);
(* Удаляет все модули, освобождает всю память. *)

PROCEDURE read_cu(VAR pro: PROJECT;
                  VAR  cu: cu_ptr;
                     name: ARRAY OF CHAR;
                      ref: BOOLEAN);
(* Чтение и разбор файла для модуля с именем name *)

PROCEDURE enter_cu(VAR pro: PROJECT;
                   VAR  cu: cu_ptr;
                      name: ARRAY OF CHAR;
                      text: ARRAY OF CHAR);
(* Разбор симфайла из text для модуля с именем name *)

PROCEDURE exit_cu(cu: cu_ptr);
(* Удаляет информацию о модуле. *)

PROCEDURE text_pos(xpos: xpos_ptr; pc: INTEGER;
                         VAR line,col: INTEGER);
(* Выдает текстовую позицию *)

PROCEDURE id_str(names: ARRAY OF CHAR;
                    id: INTEGER;
              VAR name: ARRAY OF CHAR);
(* Выдает имя по иденту *)

(***************************************************************

PROCEDURE read_cu(
-----------------
                 VAR pro: PROJECT;
                 VAR  cu: cu_ptr;
                    name: ARRAY OF CHAR;
                     ref: BOOLEAN);

     Ищет модуль с именем name в проекте. Если не находит то ищет
     Ищет модуль с именем name в проекте. Если не находит то ищет
файл  с  именем  name.sym или name.ref (если ref=TRUE). то читает
реффайл,  иначе симфайл. Поиск файла идет соответственно по путям
поиска реффайлов или симфайлов.

ОШИБКИ:
        файловые ошибки;
        ill_vers        - некорректная версия файла;
                          note - содержит сообщение.
        inconsistency   - контроль времени компиляции;
                          при этой ошибке построение структуры
                          единицы компиляции не прекращается.
                          note - содержит сообщение о первом
                          конфликте при разборе этого модуля.


PROCEDURE enter_cu(
-------------------
                  VAR pro: PROJECT;
                  VAR  cu: cu_ptr;
                     name: ARRAY OF CHAR;
                     text: ARRAY OF CHAR);

     Ищет  модуль  с  именем  name  в проекте. Если не находит то
разбирает симфайл из text для модуля с именем name.

ОШИБКИ:
        ill_vers        - некорректная версия файла;
                          note - содержит сообщение.
        inconsistency   - контроль времени компиляции;
                          при этой ошибке построение структуры
                          единицы компиляции не прекращается.
                          note - содержит сообщение о первом
                          конфликте при разборе этого модуля.

PROCEDURE exit_cu(cu: cu_ptr);
-----------------

     Удаляет  информацию  о  модуле. Дескриптор cu остается (NOT
cu^.complete).

PROCEDURE new(VAR pro: PROJECT);

     Создает    новый   проект.   Проект   main   создается   при
инициализации модуля.

PROCEDURE release;
-----------------

     Удаляет все модули из проекта, освобождает всю память.

PROCEDURE text_pos(
-------------------
                   xpos: xpos_ptr; pc: INTEGER;
                         VAR line,col: INTEGER);

     Выдает  текстовую  позицию  по  pc и нужному списку позиций
(xpos).

usage:
       text_pos(cu^.ptab[proc_no]^.locs^.xpos,pc,line,col);

PROCEDURE id_str(
-----------------
                 names: ARRAY OF CHAR;
                    id: INTEGER;
              VAR name: ARRAY OF CHAR);

     Выдает имя объекта по иденту.

usage:
       id_str(cu^.names^,id,name);

---------------------------------------------------------------

     Стандарт  симфайла в системе определяет модуль coolSym. Этот
модуль   содержит  описание  структуры  симфайла.  Данный  модуль
использует константы определенные в модуле coolSym. А именно:
     - номера типов ( type_rec.mode );
     - номера битов в множестве признаков (obj_rec.tags);
     - виды единиц компиляции (cu_rec.unit).

     Основным  понятием  в  образе  реф-файла  является  единица
компиляции  (cu_ptr). Единица компиляции (ЕК) может не содержать
информации  о  модуле  (если  NOT  complete)  и  в  этом  случае
единственной  информацией  о  ней является имя модуля, в массиве
names.   Получить  информацю  о  ЕК  можно  с  помощью  процедур
read_cu и enter_cu. После прочтение ЕК содержит:
     1.  образ импорта - массив ссылок на внешние модули (exts).
По традиции в нулевом элементе массива лежит ссылка на себя.
     2.  массив типов модуля (types). В этом массиве собраны все
типовые  значения  объявленные в модуле (без стандартных типов),
как именнованые, так и анонимные. У именнованых типов выставлены
поля  id>=0  и mod_no - номер модуля в списке внешних, в котором
впервые объявлен этот тип.
     3.  процедурная  таблица  (proc_tab). Содержит указатели на
все процедуры в ЕК (в том числе на нулевую).
     4.  массив  имен (names). Содержит все имена объектов ЕК, в
том  числе  и имена импортированных внешних объектов, кроме имен
самих импортируемых ЕК.
     5.  указатель  на  контекст  (locs).  Все объекты реф-файла
попадают  в  контекст  области  действия,  в  которой  они  были
описаны.  Заметим,  что  объектами  не являются типы и константы
(кроме структурных).
     6. Вид единицы компиляции (def,imp,prog).
     7.  Время компиляции определяющего модуля (для программного
модуля  -  совпадает  с  временем  компиляции  модуля)  и  время
компиляции реализации (для определяющего модуля = -1).

ЗАМЕЧАНИЕ:
   После чтения ЕК все массивы содержат только значимые элементы,
т.е. они обрезаны.

Для объектов:

     Пара    (scope,ofs)   определяет   размещение   переменной,
процедуры  или константы. Scope - номер уровня вложенности (если
>=0)  и  номер  внешнего  модуля, если <=0 (т.е. номер в массиве
exts).  Ofs  -  номер процедуры, номер переменной (локальной или
глобальной) и смещение структурной константы в строковом пуле.


ПРИМЕР:

MODULE main;

IMPORT slave;

MODULE module;

PROCEDURE proc;
  VAR local: INTEGER;
END proc;

END module;

VAR global: INTEGER;

END main.

cu:
   exts : длина 2, содержит ссылку на себя и
          ссылку на cu_ptr для slave.
   types: длина 1, содержит ссылку на анонимный
          процедурный тип (для proc).
   names: содержит: main Modula-X module proc local global
          в не знаю каком порядке, что неважно. Поле id
          для объектов содержит индекс начала имени
          в массиве names.
   ptab : длина 2, содержит процедуры 0 и 1 (proc).
   locs :
          vars : global
          procs: NIL
          mods : module ^.locs:
          cons : NIL        vars : NIL;
                            procs: proc   ^.locs:
                            mods : NIL        vars : local
                            cons : NIL        procs: NIL
                                                 mods : NIL
                                                 cons : NIL


Примечание:
     Так   как   модуль   достаточно   универсален,   то  в  его
использование  надо быть достаточно осторожным. Например, вместо
реф-файла  можно  читать  сим-файл.  При  этом  естественно  все
контексты пусты (o^.locs=NIL).
Вместо
  x:=ptab[n]^.locs^.xpos;
необходимо использовать:
  p:=ptab[n];
  IF (p=NIL) OR (p^.locs=NIL)  THEN .....; RETURN END;
  x:=p^.locs^.xpos;

     Компилируйте модули использующие данный с ключом -N (опцией
$N+), которая включает контроль указателей на NIL.

***************************************************************)

END xRef.
