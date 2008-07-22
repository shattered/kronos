DEFINITION MODULE bcFile; (* brd 31-Jan-91 *)

----------------------- For current model ----------------------

PROCEDURE save(x,y: INTEGER);
PROCEDURE load(x,y: INTEGER): BOOLEAN;

PROCEDURE save_block(x,y: INTEGER);
PROCEDURE load_block(x,y: INTEGER): BOOLEAN;

PROCEDURE exit(x,y: INTEGER);
PROCEDURE quit(x,y: INTEGER);

PROCEDURE save_DXF(x,y: INTEGER);
PROCEDURE load_DXF(x,y: INTEGER): BOOLEAN;

PROCEDURE plot(x,y: INTEGER);

------------------------ For other model -----------------------

PROCEDURE read_model  (x,y: INTEGER);
PROCEDURE write_model (x,y: INTEGER);
PROCEDURE write_all   (x,y: INTEGER);
PROCEDURE create_model(x,y: INTEGER);
PROCEDURE remove_model(x,y: INTEGER);
  (* This operation write/read model with context editor *)

END bcFile.
