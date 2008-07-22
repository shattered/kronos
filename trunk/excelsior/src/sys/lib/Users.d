DEFINITION MODULE Users;  (* Leo 28-Feb-90. (c) KRONOS *)


VAL no_users: INTEGER;
   no_groups: INTEGER;

TYPE
  USER = RECORD
            usr : INTEGER;
            gro : INTEGER;
            priv: BOOLEAN;
            name: ARRAY [0..7] OF CHAR;
            pass: ARRAY [0..7] OF CHAR;
            done: BOOLEAN;
         END;

PROCEDURE get_user (VAR u: USER);
PROCEDURE set_user (VAR u: USER;     run: ARRAY OF CHAR);

PROCEDURE get_group(VAR u: USER);
PROCEDURE set_group(VAR u: USER);

PROCEDURE find     (VAR u: USER);
PROCEDURE get_shell(VAR u: USER; VAR run: ARRAY OF CHAR);

PROCEDURE pack  (u, g : INTEGER;                       priv: BOOLEAN): INTEGER;
PROCEDURE unpack(ucode: INTEGER; VAR u,g: INTEGER; VAR priv: BOOLEAN);

PROCEDURE user(): INTEGER;

PROCEDURE su(ucode: INTEGER): BOOLEAN;

END Users.
