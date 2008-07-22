DEFINITION MODULE Random; (* Hady. 10-Dec-90. (c) KRONOS *)

IMPORT  SYSTEM;

PROCEDURE random(VAR x: SYSTEM.WORD);
PROCEDURE rand16(VAR x: SYSTEM.WORD);

PROCEDURE init  (x: SYSTEM.WORD);
PROCEDURE init16(x: SYSTEM.WORD);

END Random.

PROCEDURE random
          ------
  Выдает псевдопроизвольное слово с большим циклом

PROCEDURE rand16
          ------
  Выдает псевдопроизвольное слово с меньшим  циклом, но быстрее

PROCEDURE init
          ----

PROCEDURE init16
          ----
