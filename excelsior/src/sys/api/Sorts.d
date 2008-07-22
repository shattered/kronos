DEFINITION MODULE Sorts; (* Ned 03-Mar-90. (c) KRONOS *)

IMPORT  SYSTEM;

TYPE
  COMP  = PROCEDURE (SYSTEM.WORD,INTEGER,INTEGER): INTEGER;
  COMPW = PROCEDURE (SYSTEM.WORD,SYSTEM.WORD): INTEGER;
  SWAP  = PROCEDURE (SYSTEM.WORD,INTEGER,INTEGER);

PROCEDURE quick(x: SYSTEM.WORD; len: INTEGER; comp: COMP; swap: SWAP);
PROCEDURE heap (x: SYSTEM.WORD; len: INTEGER; comp: COMP; swap: SWAP);

PROCEDURE quickw(VAR x: ARRAY OF SYSTEM.WORD; len: INTEGER; comp: COMPW);
PROCEDURE heapw (VAR x: ARRAY OF SYSTEM.WORD; len: INTEGER; comp: COMPW);

PROCEDURE str_comp(s1,s2: ARRAY OF CHAR): INTEGER;
PROCEDURE abc_comp(s1,s2: ARRAY OF CHAR): INTEGER;
PROCEDURE ABC_comp(s1,s2: ARRAY OF CHAR): INTEGER;

(*************************************************************

     Модуль  предоставляет  операции  сортировки  произвольных
структур  двумя  методами: быстрая сортировка и пираминдальная
сортировка   (см.   Н.Вирт,   Алгоритмы+Структуры   Данных   =
Программы, стр.89-99). Для каждого из этих методов реализованы
по две процедуры:
        - сортировка массивов слов;
        - сортировка произвольных структур.

Сравнение скорости работы процедур (на случайных данных):

        quickw < quick < heapw < heap.

     Пираминдальная  сортировка  хороша  тем,  что скорость ее
работы  практически  не  зависит от данных. Быстрая сортировка
может работать довольно медленно на некоторых данных.

---------------------------------------------------------------

Процедуры сравнения:

  COMP  = PROCEDURE (SYSTEM.WORD,INTEGER,INTEGER): INTEGER;
  COMPW = PROCEDURE (SYSTEM.WORD,SYSTEM.WORD): INTEGER;

Процедуры сравнения должны возвращать
        <0, если первый аргумент < второго
         0, если первый аргумент = второму
        >0, если первый аргумент > второго


PROCEDURE quick & heap
----------------------
     (x: SYSTEM.WORD; len: INTEGER; comp: COMP; swap: SWAP);

     Сортировка  произвольных  структур.  Первый  параметр  не
используется   собственно   процедурами  сортировки  и  просто
передается  процедурам сравнения и обмена. Процедуры сортируют
индексы в диапазоне [0..len-1].


PROCEDURE quickw & heapw
------------------------
     (VAR x: ARRAY OF SYSTEM.WORD; len: INTEGER; comp: COMPW);

     Сортировка массива слов в диапазоне [0..len-1]. Процедуре
сравнения передаются элементы массива.

---------------------------------------------------------------

     Операции   сравнения  строк.  Строки  ДОЛЖНЫ  завершаться
символом 0с.

PROCEDURE str_comp(s1,s2: ARRAY OF CHAR): INTEGER;
--------------------------------------------------

     Сравнение   строк.   Порядок   на   литерах  определяется
стандартом принятым в системе (ДКОИ-8).

PROCEDURE abc_comp(s1,s2: ARRAY OF CHAR): INTEGER;

     Сравнение  строк.  Буквы  как  русского, так и латинского
алфавита распологаются в алфавитном порядке. Порядок остальных
символов определяется стандартом.

PROCEDURE ABC_comp(s1,s2: ARRAY OF CHAR): INTEGER;

     Сравнение  строк.  Буквы  как  русского, так и латинского
алфавита распологаются в алфавитном порядке. Порядок остальных
символов определяется стандартом. Большие и маленькие буквы не
различаются.

*************************************************************)

END Sorts.
