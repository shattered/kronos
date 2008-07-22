DEFINITION MODULE osFiles; (* Leo 02-Jan-86. (c) KRONOS *)
                           (* Leo 24-Aug-89. (c) KRONOS *)
                           (* Ned 19-Sep-89. (c) KRONOS *)

IMPORT       SYSTEM;
IMPORT   os: osKernel;
IMPORT  req: defRequest;


CONST -- disk modes
  sys      = {4};

CONST -- open file modes
  nodelay  =  {3};
  nocash   =  {4};
  wait_wr  =  {5};

    disk   =  {6};
    tty    =  {7};
    spec   =  {8};
  special  =  disk+tty+spec;

    fsys   =  {9};
    hidden = {10};

TYPE
  PORT;
  FILE;
  DOIO = PROCEDURE (VAR req.REQUEST);

CONST (* dnode kind: *)

  d_del    = {0};
  d_file   = {1};
  d_dir    = {2};
  d_hidden = {3};
  d_esc    = {4};
  d_sys    = {5};

  d_entry  = d_esc + d_file + d_dir; (* none empty entry *)

TYPE
  dnode =
  RECORD
    name: ARRAY [0..31] OF CHAR;
    rfe0: ARRAY [0..15] OF CHAR;
    inod: INTEGER;
    kind: BITSET;
    rfe1: ARRAY [0..1] OF INTEGER;
  END;

VAL
  root: FILE;
  null: FILE;

  (* statistics *)
  chsize : INTEGER;
  dkwrite: INTEGER;
  dkread : INTEGER;
  chread : INTEGER;

CONST
  a_eof   = 0;
  a_pro   = 1;
  a_ctime = 2;
  a_wtime = 3;
  a_links = 4;
  a_mode  = 5;
  a_inode = 6;

PROCEDURE getattr(f: FILE; atr: INTEGER; VAR val: SYSTEM.WORD);
PROCEDURE setattr(f: FILE; atr: INTEGER;     val: SYSTEM.WORD);

PROCEDURE isdir(f: FILE): BOOLEAN;

PROCEDURE state(f: FILE): BITSET;

PROCEDURE usrlock  (f: FILE; t: INTEGER): INTEGER;
PROCEDURE usrunlock(f: FILE);

PROCEDURE mount  (dir: FILE; name: ARRAY OF CHAR;   dev: FILE;
                             info: ARRAY OF CHAR;
                          VAR lab: ARRAY OF CHAR;    ro: BOOLEAN): INTEGER;

PROCEDURE unmount(dir: FILE; name: ARRAY OF CHAR; flush: INTEGER): INTEGER;

PROCEDURE copy_fname(VAR name: ARRAY OF CHAR; fname: ARRAY OF CHAR): INTEGER;

PROCEDURE open  (dir: FILE;          VAR f: FILE;
                name: ARRAY OF CHAR; state: BITSET): INTEGER;

PROCEDURE create(dir: FILE;          VAR f: FILE;
                size: INTEGER;       state: BITSET): INTEGER;

PROCEDURE open_dev(VAR f: FILE; device: ARRAY OF CHAR; state: BITSET): INTEGER;
(* state <= nodelay+nocash+wait_wr *)

PROCEDURE close (f: FILE): INTEGER;
PROCEDURE reopen(f: FILE): INTEGER;
PROCEDURE flush (f: FILE): INTEGER;
(* close (write_inode) + open for dir & file
   erase cash for blocked
*)

PROCEDURE make_dir(VAR d: FILE; where: FILE): INTEGER;

PROCEDURE move_dir(from: FILE; fname: ARRAY OF CHAR;
                     to: FILE; tname: ARRAY OF CHAR; hid: BOOLEAN): INTEGER;

PROCEDURE read (f: FILE; fpos: INTEGER;     buf: SYSTEM.ADDRESS;
                         bpos: INTEGER; VAR len: INTEGER): INTEGER;

PROCEDURE write(f: FILE; fpos: INTEGER;     buf: SYSTEM.ADDRESS;
                         bpos: INTEGER; VAR len: INTEGER): INTEGER;

PROCEDURE doio(f: FILE; VAR r: req.REQUEST): INTEGER;

PROCEDURE link  (dir,file: FILE; name: ARRAY OF CHAR; hid: BOOLEAN): INTEGER;
PROCEDURE unlink(dir     : FILE; name: ARRAY OF CHAR): INTEGER;

PROCEDURE du(dir: FILE; VAR free,used: INTEGER): INTEGER;

PROCEDURE cut   (f: FILE; size: INTEGER): INTEGER;
PROCEDURE extend(f: FILE; size: INTEGER): INTEGER;

PROCEDURE make_fs(disk: FILE;
                 label: ARRAY OF CHAR;
                  bads: ARRAY OF INTEGER): INTEGER;

PROCEDURE make_node(d: FILE; VAR f: FILE; ref: ARRAY OF CHAR): INTEGER;


PROCEDURE define_driver(name,host: ARRAY OF CHAR; drn: INTEGER;
                             mode: BITSET;       doio: DOIO): INTEGER;

PROCEDURE remove_driver(name: ARRAY OF CHAR): INTEGER;

END osFiles.
