DEFINITION MODULE pedTools; (* Sem 13-May-87. (c) KRONOS *)

TYPE ToolRec=RECORD
  Size  : INTEGER;
  VSize : INTEGER;
  DSize : INTEGER;
  DefX  : INTEGER;
  DefY  : INTEGER;
  VDefX : INTEGER;
  VDefY : INTEGER;
  Magic : INTEGER;
END;

VAR Tools: ARRAY [0..31] OF ToolRec;

PROCEDURE ToolsMenu;

END pedTools.
