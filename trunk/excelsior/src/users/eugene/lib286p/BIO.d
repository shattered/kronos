DEFINITION MODULE BIO; (* Sem 09-Feb-91. (c) KRONOS *)

FROM SYSTEM      IMPORT ADDRESS, WORD;

TYPE
  FILE ;
  PATHs;

VAL
  cd   : FILE;
  here : PATHs;
  cmask: BITSET;

  error: INTEGER;
  done : BOOLEAN;
  ename: ARRAY [0..69] OF CHAR;

PROCEDURE open  (VAR file: FILE; path,mode: ARRAY OF CHAR);
PROCEDURE create(VAR file: FILE; path,mode: ARRAY OF CHAR;
                                      size: INTEGER);

PROCEDURE fcreate(cd: FILE; VAR file: FILE; path,mode: ARRAY OF CHAR;
                                                       size: INTEGER);

PROCEDURE chaccess(file: FILE; mask: BITSET);

PROCEDURE close (VAR file: FILE);
PROCEDURE purge (VAR file: FILE);

PROCEDURE cut   (file: FILE; size: INTEGER);
PROCEDURE end   (file: FILE; size: INTEGER);
PROCEDURE extend(file: FILE; size: INTEGER);
PROCEDURE eof   (file: FILE): INTEGER;

PROCEDURE seek (file: FILE; pos,mode: INTEGER);

PROCEDURE read (file: FILE; buf: ADDRESS; len: INTEGER);
PROCEDURE write(file: FILE; buf: ADDRESS; len: INTEGER);
PROCEDURE fwrite(file: FILE; buf: ADDRESS; pos,len: INTEGER);

PROCEDURE get(file: FILE; VAR data: ARRAY OF WORD; len: INTEGER);
PROCEDURE put(file: FILE;     data: ARRAY OF WORD; len: INTEGER);

PROCEDURE buffers(file: FILE; no,sz: INTEGER);

CONST
   s_none     = 00;
   s_reverse  = 01;
   s_dirfwd   = 02;
   s_name     = 04;
   s_ext      = 08;
   s_time     = 16;
   s_cre      = 32;
   s_eof      = 64;

PROCEDURE dir_walk(cd: FILE; sort_kind: INTEGER);

PROCEDURE get_entry(cd: FILE; VAR name: ARRAY OF CHAR;
                              VAR mode: BITSET): BOOLEAN;
CONST -- file entry modes:
  e_file   = { };
  e_dir    = {0};
  e_hidden = {1};
  e_esc    = {2};
  e_sys    = {3};

PROCEDURE restart_walk(cd: FILE);

PROCEDURE end_walk (cd: FILE);

PROCEDURE file_name(f: FILE; VAR name: ARRAY OF CHAR);

PROCEDURE close_paths(VAR dirs: PATHs);
PROCEDURE get_paths  (VAR dirs: PATHs; nm: ARRAY OF CHAR);
PROCEDURE open_paths (VAR dirs: PATHs; nm: ARRAY OF CHAR);

PROCEDURE lookup(dirs: PATHs; VAR file: FILE; name,mode: ARRAY OF CHAR);

END BIO.
