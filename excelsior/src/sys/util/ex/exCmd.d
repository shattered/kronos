DEFINITION MODULE exCmd; (* Leo 05-Jul-87. (c) KRONOS *)

PROCEDURE Shell;

PROCEDURE SetMain(Name: ARRAY OF CHAR);

PROCEDURE FirstRead;
PROCEDURE MainWrite(): BOOLEAN;

PROCEDURE write_and_exit(): BOOLEAN;
PROCEDURE write_file;

PROCEDURE Command;

PROCEDURE FindNext;

PROCEDURE Do(command: ARRAY OF CHAR);

PROCEDURE Filter(k: CHAR);

PROCEDURE show_mark(up: BOOLEAN);

PROCEDURE fill_my_editor;

END exCmd.
