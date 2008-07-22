DEFINITION MODULE mxGen; (* Ned 01-Jun-88. (c) KRONOS *)

IMPORT  SYSTEM, obs: mxObj;

TYPE ObjPtr; -- генерационный объект

TYPE                -- (proc_no,   pc,   line,    col  );
  PUT_XREF = PROCEDURE (INTEGER,INTEGER,INTEGER,INTEGER);

VAL
  proc_no : INTEGER;
  local_no: INTEGER;

--------------------  ПОСТРОЕНИЕ ОБЪЕКТОВ  --------------------
                    -----------------------

PROCEDURE genObj(VAR x: ObjPtr; o: obs.ObjPtr);
(* Строит генерационный объект по объекту. *)

PROCEDURE gen_const(VAR o: ObjPtr; t: obs.TypePtr; val: INTEGER);
(* Делает константу со значением val и типом t *)

PROCEDURE gen_string(VAR o: ObjPtr; t: obs.TypePtr;
                         s: ARRAY OF CHAR; len: INTEGER);
(* Делает строковую константу. *)

-------------------  СТРУКТУРНЫЕ КОНСТАНТЫ  -------------------
                   -------------------------

PROCEDURE create_open_array(VAR o: ObjPtr; base: obs.TypePtr);
                                           ----
PROCEDURE close_open_array(     o: ObjPtr; type: obs.TypePtr);
PROCEDURE app_elem(array,expr: ObjPtr);

---------------------  АТРИБУТЫ ОБЪЕКТОВ  ---------------------
                     ---------------------

PROCEDURE get_type(o: ObjPtr; VAR type: obs.TypePtr);
(* Возвращает в type тип объекта. *)

PROCEDURE type?(o: ObjPtr): BOOLEAN;
(* Выдает TRUE, если объект есть тип. *)

PROCEDURE standard_proc(o: ObjPtr; VAR no: INTEGER): BOOLEAN;
(* Если объект - стандартная процедура, то возвращает ее номер
   и выдает TRUE.
*)

-------------------------  ВЫРАЖЕНИЯ  -------------------------
                         -------------

PROCEDURE apply(o1,o2: ObjPtr; sy: INTEGER);
(* Выполняет бинарную операцию. Результат в o1. *)

PROCEDURE applyNOT(o: ObjPtr); (* Выполняет операцию NOT. *)
PROCEDURE applyNEG(o: ObjPtr); (* Выполняет операцию NEG. *)

PROCEDURE typetransfer(o,ex: ObjPtr);
(* Преобразование типа o(ex). Результат в -o-. *)

PROCEDURE constexpr(o: ObjPtr; VAR val: INTEGER; VAR type: obs.TypePtr);
(* Выдает значение и тип константы.
   Если не константа, то сообщает об ошибке.
*)

PROCEDURE const_dcl(o: ObjPtr; const: obs.ObjPtr);
(* Устанавливает атрибуты type, scope, addr у константы 'const' *)

-------------------  КОНСТРУКТОР МНОЖЕСТВА  -------------------
                   -------------------------

PROCEDURE enterset(o: ObjPtr);
PROCEDURE setelem (o,e: ObjPtr);
PROCEDURE setrange(o,bou1,bou2: ObjPtr);
PROCEDURE  exitset(o: ObjPtr);

------------------------  ОБОЗНАЧЕНИЯ  ------------------------
                        ---------------

PROCEDURE deref (o: ObjPtr);                    -- a^
PROCEDURE index0(o: ObjPtr);                    -- a[
PROCEDURE index1(o,ex: ObjPtr);                 -- a[e]
PROCEDURE access(o: ObjPtr; fieldID: INTEGER);  -- a.f

PROCEDURE load_value(o: ObjPtr);
(*  Загружает значение обозначения. *)

-------------------------  ОПЕРАТОРЫ  -------------------------
                         -------------

                       ----- вызовы -----

PROCEDURE enterProcCall(proc: ObjPtr);
PROCEDURE  exitProcCall(VAR res: ObjPtr);
(* Вход/выход в процедурный вызов.
   Для функции exitProcCall возвращает в res результат функции.
*)

PROCEDURE param(o: ObjPtr; parm: obs.ObjPtr);
(* Генерация параметров *)

PROCEDURE enter_seq;
PROCEDURE seq_param(o: ObjPtr; parm: obs.ObjPtr);
PROCEDURE end_seq;
(* Генерация последовательности параметров *)

            ----- вызовы стандартных процедур -----

CONST    -- СТАНДАРТНЫЕ ПРОЦЕДУРЫ --
  _inc  =-1;     _incl  =-2;       _halt   =-3;
  _dec  =-4;     _excl  =-5;       _assert =-6;
  _new  =-7;     _resize=-8;       _dispose=-9;

  _adr  = 0;     _odd   = 1;       _ord    = 2;
  _min  = 3;     _trunc = 4;       _cap    = 5;
  _max  = 6;     _float = 7;       _chr    = 8;
  _size = 9;     _bits  =10;       _bytes  =11;
  _high =12;     _abs   =13;       _ref    =14;
  _len  =15;

PROCEDURE enterStandardCall(proc: ObjPtr);
PROCEDURE  exitStandardCall(VAR res: ObjPtr);
(* Вход/выход в вызов стандартной процедуры.
   Для функции exitStandardCall возвращает результат в res.
*)

PROCEDURE standard_param(o: ObjPtr);
(* Генерация параметров стандартной процедуры. *)

                    ----- присваивание -----

PROCEDURE assign0(left: ObjPtr);        -- left:=
PROCEDURE assign1(left,right: ObjPtr);  -- left:=right

                ----- структуры управления -----

PROCEDURE enterloop;
PROCEDURE exitloop;
PROCEDURE endloop;

PROCEDURE enterwhile;
PROCEDURE do(o: ObjPtr);
PROCEDURE endwhile;

PROCEDURE repeat;
PROCEDURE until(o: ObjPtr);

PROCEDURE enterfor(v: obs.ObjPtr);
PROCEDURE for_from(o: ObjPtr);
PROCEDURE for_to  (o: ObjPtr);
PROCEDURE for_step(o: ObjPtr);
PROCEDURE for_do;
PROCEDURE endfor;

PROCEDURE if(o: ObjPtr);
PROCEDURE else;
PROCEDURE endif;

PROCEDURE entercase(o: ObjPtr);
PROCEDURE caselabel(o: ObjPtr);
PROCEDURE caserange(o1,o2: ObjPtr);
PROCEDURE exitvariant;
PROCEDURE elsecase;
PROCEDURE endcase;

PROCEDURE return;
PROCEDURE freturn(o: ObjPtr; restype: obs.TypePtr);

PROCEDURE entercond(o: ObjPtr; and: BOOLEAN);
PROCEDURE exitcond (o: ObjPtr): ObjPtr;

PROCEDURE mark(n: INTEGER);
PROCEDURE goto(n: INTEGER);

PROCEDURE enterwith(o: ObjPtr; v: obs.ObjPtr);
(* o -- designator; v -- temp variable
   Устаналивает тип и размещение для v.
*)

PROCEDURE endwith(v: obs.ObjPtr);

--------------------------  ОПИСАНИЯ  -------------------------
                          ------------

PROCEDURE dclProc(proc: obs.ObjPtr);
PROCEDURE dclVar (var : obs.ObjPtr);
(* Определяет размещение объекта (уровень, смещение) *)

PROCEDURE dclParm(var : obs.ObjPtr; parm: obs.ObjPtr);
(* Определяет размещение переменной-параметра *)

PROCEDURE enterProc(proc: obs.ObjPtr);
PROCEDURE  exitProc(proc: obs.ObjPtr; put_xref: PUT_XREF);
(* Вход/выход из процедуры *)

PROCEDURE ProcEqu(RealProc,PseudoProc: obs.ObjPtr);
(* Определяет эквивалентность процедур с данными номерами.
   Они ссылаются на один и тот же код.
*)

PROCEDURE enterCodeProc(proc: obs.ObjPtr);
PROCEDURE  exitCodeProc(proc: obs.ObjPtr);
(* Вход/выход из кодовой процедуры *)

PROCEDURE code_expr(proc: obs.ObjPtr; o: ObjPtr);
(* Добавляет выражение к коду кодовой процедуры *)

-------------------------  ПАРАМЕТРЫ  -------------------------
                         -------------

PROCEDURE scanParms(plist: obs.ObjPtr);
(* Определение размещений параметров и способа передачи. *)

--------------------  РАБОТА С СИМФАЙЛАМИ  --------------------
                    -----------------------

PROCEDURE set_counts(proc_co,var_co: INTEGER);
(* Определяет счетчики процедур и (глобальных) переменных. *)

PROCEDURE extProc  (proc  : obs.ObjPtr; extno,ofs: INTEGER);
PROCEDURE extVar   (var   : obs.ObjPtr; extno,ofs: INTEGER);
(* Определяет размещение внешнего объекта (модуль, смещение) *)

TYPE GET_BYTE = PROCEDURE (): INTEGER;

PROCEDURE extStruct(struct: obs.ObjPtr; extno,ofs: INTEGER;
                       len: INTEGER; get: GET_BYTE);
(* Определяет размещение структуры (модуль, смещение) и ее тело. *)

TYPE PUT_BYTE = PROCEDURE (INTEGER);

PROCEDURE get_struct(o: obs.ObjPtr; one: PUT_BYTE);
(* Выдает содержимое структурной константы. *)

TYPE WORDs = DYNARR OF SYSTEM.WORD;

PROCEDURE put_access(s: ARRAY OF SYSTEM.WORD);
PROCEDURE get_access(VAR s: WORDs);

--------------------  ОФОРМЛЕНИЕ КОДОФАЙЛА  -------------------
                    ------------------------

PROCEDURE GenCode(name: ARRAY OF CHAR;
              put_xref: PUT_XREF;
              def_time: INTEGER;
              imp_time: INTEGER;
                   tag: INTEGER;
          VAR codesize: INTEGER);
(* Возвращает размер кода *)

-----------------------  ИНИЦИАЛИЗАЦИЯ  -----------------------
                       -----------------

PROCEDURE Ini(name: ARRAY OF CHAR; cpu: INTEGER);
PROCEDURE Exi;

PROCEDURE cpu(): INTEGER;

END mxGen.
