IMPLEMENTATION MODULE myEditor; (* Leo 26-Jan-89. (c) KRONOS *)

IMPORT  sys: SYSTEM;

TYPE WORD = sys.WORD;

PROCEDURE start0; END start0;

PROCEDURE write0(VAL name: ARRAY OF CHAR; exit: BOOLEAN): BOOLEAN;
BEGIN RETURN TRUE END write0;

PROCEDURE read0(VAL name: ARRAY OF CHAR): BOOLEAN;
BEGIN RETURN TRUE END read0;

PROCEDURE break0(): BOOLEAN;
BEGIN RETURN TRUE END break0;

PROCEDURE empty;
BEGIN
  ASSERT(FALSE,51h);
END empty;

BEGIN
  crs_pos:=WORD(empty);    last:=WORD(empty);
  refresh:=WORD(empty);    jump:=WORD(empty);
  goto   :=WORD(empty);    get :=WORD(empty);
  frame  :=WORD(empty);    put :=WORD(empty);
  message:=WORD(empty);    app :=WORD(empty);
  mark   :=WORD(empty);    del :=WORD(empty);
  adr    :=WORD(empty);    ins :=WORD(empty);
  f_name :=WORD(empty);    size:=WORD(empty);
  start  :=start0;         onread :=read0;
  onwrite:=write0;         onbreak:=break0;
END myEditor.
