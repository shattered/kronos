DEFINITION MODULE pcTab; (* 02-Oct-90. (c) KRONOS *)

IMPORT  SYSTEM;

TYPE
  mode = (      --              dw              l               r

-- объекты, выдающие значение
nil,            --      конструктор
number,         --      конструктор
string,         --      конструктор
usage,          --      конструктор                     объект

-- объекты обладающие значением
procedure,      --                      профиль
inline,         --                      профиль
var,            --                      конструктор
const,          --      выражение       конструктор

-- конструкторы типов
enumeration,    --      константы
boolean,        --
char,           --
range,          --      конструктор     выражение-MIN   выражение MAX
subtype,        --      конструктор     выражение-MIN   выражение MAX
integer,        --
real,           --
set,            --      конструктор
array,          --                      конст. идекса   конст. элемента
dynarr,         --                                      конст. элемента
dynarr_desc,    --
array_of,       --                                      конст. элемента
record,         --      поля
packed_array,   --                      конст. идекса   конст. элемента
packed_dynarr,  --                                      конст. элемента
packed_array_of,--                                      конст. элемента
packed_record,  --      поля
pointer,        --      конструктор
profile,        --      параметры       конст. результата
val_prm,        --      конструктор
var_prm,        --      конструктор

-- операции
const_check,    --      конст. результат                выражение
size_check,     --      конст. результат                выражение
range_check,    --      конст. результат                выражение
high,           --      конст.результ.                  объект
low,            --      конст.результ.                  объект
len,            --      конст.результ.                  объект
bits,           --      конст.результ.                  объект
min,            --      конст.результ.                  объект
max,            --      конст.результ.                  объект
adr,            --      конст.результ.                  объект
size,           --      конст.результ.                  объект
bytes,          --      конст.результ.                  объект
abs,            --      конст.результ.                  выражение
odd,            --      конст.результ.                  выражение
cap,            --      конст.результ.                  выражение
equal,          --      конст.результ.  выражение       выражение
inequality,     --      конст.результ.  выражение       выражение
less,           --      конст.результ.  выражение       выражение
less_equal,     --      конст.результ.  выражение       выражение
greater,        --      конст.результ.  выражение       выражение
greater_equal,  --      конст.результ.  выражение       выражение
in,             --      конст.результ.  выражение       выражение
plus,           --      конст.результ.  выражение       выражение
minus,          --      конст.результ.  выражение       выражение
star,           --      конст.результ.  выражение       выражение
slash,          --      конст.результ.  выражение       выражение
div,            --      конст.результ.  выражение       выражение
mod,            --      конст.результ.  выражение       выражение
rem,            --      конст.результ.  выражение       выражение
rol,            --      конст.результ.  выражение       выражение
ror,            --      конст.результ.  выражение       выражение
not,            --      конст.результ.                  выражение
index,          --      конст.результ.  объект          индекс
field,          --      конст.результ.  объект          поле
trunc,          --      конст.результ.                  выражение
float,          --      конст.результ.                  выражение
worder,         --      конст.результ.                  выражение
deref,          --      конст.результ.  объект
aggregate,      --      конст.результ.  конст.значения  список

-- операторы
module,         --      операторы
proc_body,      --      операторы       процедура
code_body,      --      выражения       процедура
block,          --      операторы
assign,         --                      объект          выражение
call,           --      конст.результ.  объект          параметры
select,         --                      диапазоны       операторы
if,             --      выражение       then-операторы  else-операторы
case,           --      выражение       select's        else-операторы
loop,           --      операторы       условие         условие
exit,           --      loop
return,         --      выражение
incl,           --                      объект          выражение
excl,           --                      объект          выражение
goto,           --      оператор
continue,       --
inc,            --                      объект          выражение
dec,            --                      объект          выражение
program_check,  --                      выражение      [выражение]
assert,         --                      выражение      [выражение]
halt,           --                                     [выражение]
new,            --      процедура       выражение      [выражение]
dispose,        --      процедура       выражение
resize          --      процедура       выражение       выражение
  );

  ref  =  POINTER TO node;

  node = RECORD
    nm  : INTEGER;
    md  : mode;
    nxt : ref;
    dw  : ref;
    l   : ref;
    r   : ref;
    adr : INTEGER;
    CASE :INTEGER OF
      |0: val : INTEGER;
      |1: pos : INTEGER;
      |2: str : STRING;
    END;
  END;

VAR
                      (*cu deftime imptime     codesize *)
  ini_gen  : PROCEDURE ();
  gen_def  : PROCEDURE (ref,INTEGER);
  gen_code : PROCEDURE (ref,INTEGER,INTEGER,VAR INTEGER);
  get_code : PROCEDURE (ref,VAR STRING);
  put_code : PROCEDURE (ref,ref,ARRAY OF CHAR);
  gen_const: PROCEDURE (mode,mode,INTEGER,INTEGER,VAR INTEGER);

VAR
  get_name: PROCEDURE (INTEGER, VAR ARRAY OF CHAR);
  find_var: PROCEDURE (ARRAY OF CHAR): ref;
  error   : PROCEDURE (INTEGER,INTEGER,ARRAY OF CHAR,SEQ SYSTEM.WORD);
  chk_nil : BOOLEAN;
  ini_nil : BOOLEAN;
  cpu_type: INTEGER;
  cpu_mode: INTEGER;

END pcTab.
