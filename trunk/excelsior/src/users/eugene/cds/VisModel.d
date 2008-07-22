DEFINITION MODULE VisModel; (* Sem 13-Sep-86. (c) KRONOS *)

FROM Model     IMPORT   Object, List, Objects;
FROM StdIO     IMPORT   Stream;

VAR Output: Stream;

PROCEDURE VisObject(o: Object);

END VisModel.
