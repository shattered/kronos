DEFINITION MODULE cdsLoader; (* Leo 29-Jan-88. (c) KRONOS *)

FROM SYSTEM     IMPORT  ADDRESS;

TYPE
      DFT;      (*    D A T A   F R A M E  T A B L E    *)

CONST   (*      Р Е З У Л Ь Т А Т Ы     О П Е Р А Ц И Й         *)

  ok=0; dftoverflow=1; geterror=2; nomemory=3; wrongtime=4;


VAL SYS: DFT; (*  системная DFT Excelsior                 *)
    TSK: DFT; (*  DFT задачи                              *)

PROCEDURE lookupModule(dft: DFT; name: ARRAY OF CHAR; VAR G: ADDRESS): BOOLEAN;
(* Ищет модуль в DFT. Если находит, заносит значение его  *)
(* глобального регистра в -G- и возвращает TRUE.          *)
(* Иначе возвращает FALSE, значение -G- не изменяется.    *)


TYPE

allocProc=(*    addr     size *)
PROCEDURE (VAR ADDRESS, INTEGER);
(* При size>0 пытается отвести непрерывнй кусок памяти    *)
(* размером в size слов. Возвращает NIL в случае неудачи. *)
(* При size<0 утилизует ранее отведенный кусок размером в *)
(* ABS(size) слов.                                        *)

getProc=
         (*    name         code        size    allocate  *)
PROCEDURE(ARRAY OF CHAR,VAR ADDRESS,VAR INTEGER,allocProc,
      VAR ARRAY OF CHAR): BOOLEAN;
     (*   cause       *)

(* Если удалось "добыть" кодофайл для -name- размещает    *)
(* его в памяти пользуясь процедурой -allocate-, выдает   *)
(* его CodePtr в -code- и возвращает TRUE. Иначе FALSE    *)
(* и причину неудачи в текстовом виде в -cause-.          *)

(* ВНИМАНИЕ! -get- возвращает размер выделенной памяти в  *)
(* -size-. Именно кусок code..code+size-1 будет утилизован*)
(* в случае неудачнойй загрузки                           *)

lookupProc=
         (*    name         glo   *)
PROCEDURE(ARRAY OF CHAR,VAR ADDRESS): BOOLEAN;
(* Процедура поиска во внешних DFT (см. ниже)             *)

PROCEDURE Load(VAR  dft: DFT;          (* результирующая -dft-               *)
               VAR name: ARRAY OF CHAR;(* имя загружаемого модуля/сообщение  *)
                 lookup: lookupProc;   (* процедура поиска во внешних DFT    *)
                    get: getProc;      (* процедура размещения кода модуля   *)
                  alloc: allocProc;    (* процедура выделения памяти         *)
                dftsize: INTEGER;      (* максимальный размер дерева импорта *)
                   ): INTEGER;         (* результат загрузки                 *)

(* В случае неудачи (результат#ok) возвращает причину     *)
(* неудачи в текстовом виде в масиве -name-, поэтому Мы   *)
(* рекомендуем описывать -name- как минимум 80 байтовым:  *)
(*      name: ARRAY [0..N] OF CHAR;  HIGH(name)>=79       *)

(* Инициализация формируется как отдельная структура      *)
(* внутри -dft- и может быть исполнена как процедура      *)
(* с помощью -Call(dft)-.                                 *)

(* Процедура -lookup- используется для поиска модуля во   *)
(* внешних DFT. Мы рекомендуем такую форму:               *)
(*   PROCEDURE lookup(name,G): BOOLEAN;                   *)
(*   BEGIN                                                *)
(*     IF lookupModule(SYS,name,G) THEN RETURN TRUE END;  *)
(*                                                        *)
(*     IF lookupModule(MY0,name,G) THEN RETURN TRUE END;  *)
(*     IF lookupModule(MY1,name,G) THEN RETURN TRUE END;  *)
(*     . . .                                              *)
(*                                                        *)
(*     RETURN FALSE;                                      *)
(*   END lookup;                                          *)
(* NB! Процедура -lookup- используется дважды в процессе  *)
(* загрузки (при чтении кодофалов, и при размещении гло-  *)
(* балов. Поэтому она должна обеспечивать стабильный      *)
(* (не меняющийся с начала и до конца загрузки) результат *)
(* поиска для каждого модуля.                             *)

(* Процедура -alloc- сопоставленная -dft-                 *)
(* Используется процедурой -get- для размещения кода,     *)
(* а также, вызывается для размещения глобальных областей *)
(* инициализируемых модулей, после того как загружены все *)
(* кодофайлы.                                             *)

(* В случае неудачной загрузки -alloc- вызывается для     *)
(* "утилизации" использованной памяти                     *)

PROCEDURE UnLoad(dft: DFT);
(* разгружает -dft- освобождая занятую кодом и глобалами  *)
(* память, после чего удаляет -dft- освобождая занятую ей *)
(* память.                                                *)

PROCEDURE Call(dft: DFT): ADDRESS;
(* Вызывает модули из -dft- в порядке обхода дерева       *)
(* импорта. Циклически зависимые модули инициализируются  *)
(* в произвольном порядке отностительно друг друга, но    *)
(* весь цикл в правильном порядке относительно остального *)
(* дерева.                                                *)
(* Возвращает новое значение S регистра                   *)

TYPE
                 (*proc0*)
iterProc=PROCEDURE(PROC);

PROCEDURE Order(dft: DFT; ip: iterProc);
(* Может быть использована для генерации кода "встроенной"*)
(* инициализации.                                         *)
(* NB! Вы НЕ можете вызывать -proc0- на вашем стеке прямо *)
(* из итератора, т.к. -proc0- использует команды ALLOC!   *)
(*                 B E    C A R E F U L L !               *)

END cdsLoader.
