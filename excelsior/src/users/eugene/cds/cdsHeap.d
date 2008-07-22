DEFINITION MODULE cdsHeap; (* Sem 10-May-87. (c) KRONOS *)

FROM SYSTEM    IMPORT ADDRESS;

PROCEDURE   Allocate    (VAR a: ADDRESS; words: CARDINAL);
(* Захват куска памяти длинной words слов.
   В случае неудачи возникает exception MemoryOverflow *)

PROCEDURE   HardAllocate(VAR a: ADDRESS; words: CARDINAL);
(* Захват куска памяти длинной words слов.
   При необходимости мспользует резервную память.
   В случае неудачи возникает exception MemoryOverflow *)

PROCEDURE   Deallocate  (VAR a: ADDRESS; words: CARDINAL);
(* Освобождение памяти *)

PROCEDURE ShowMem;
(* Печать списка свободной памяти на терминал - только для отладки! *)

END cdsHeap.
