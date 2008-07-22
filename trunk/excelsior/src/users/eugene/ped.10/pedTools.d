DEFINITION MODULE pedTools; (* Sem 13-May-87. (c) KRONOS *)

TYPE ToolRec=RECORD
  Size  : CARDINAL;
  VSize : CARDINAL;
  DSize : CARDINAL;
  DefX  : CARDINAL;
  DefY  : CARDINAL;
  VDefX : CARDINAL;
  VDefY : CARDINAL;
  Magic : CARDINAL;
END;

VAR Tools: ARRAY [0..31] OF ToolRec;

PROCEDURE ToolsMenu;

END pedTools.
