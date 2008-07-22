DEFINITION MODULE osFileSystem; (* Igo 16-Feb-92. (c) KRONOS *)

IMPORT  SYSTEM;
IMPORT  req: defRequest;
IMPORT  fs : osFiles;
IMPORT  os : osKernel;

VAL FMAGIC : INTEGER;

TYPE

     _FILE  = POINTER TO _file ;

     FS     = POINTER TO fsdesc;

     _file  = RECORD
                fmagic : INTEGER;
                fsid   : FS;
              END;


     GETATTR   = PROCEDURE ( _FILE , INTEGER , VAR SYSTEM.WORD );
     SETATTR   = PROCEDURE ( _FILE , INTEGER ,     SYSTEM.WORD );
     ISDIR     = PROCEDURE ( _FILE ): BOOLEAN;
     STATE     = PROCEDURE ( _FILE ): BITSET;
     USRLOCK   = PROCEDURE ( _FILE , INTEGER ): INTEGER;
     USRUNLOCK = PROCEDURE ( _FILE );
     OPEN      = PROCEDURE ( _FILE , VAR _FILE , ARRAY OF CHAR , BITSET ): INTEGER;
     CREATE    = PROCEDURE ( _FILE , VAR _FILE , INTEGER ,       BITSET ): INTEGER;
     OPENDEV   = PROCEDURE ( VAR _FILE , ARRAY OF CHAR , BITSET ): INTEGER;
     CLOSE     = PROCEDURE ( _FILE ): INTEGER;
     REOPEN    = PROCEDURE ( _FILE ): INTEGER;
     FLUSH     = PROCEDURE ( _FILE ): INTEGER;
     MAKEDIR   = PROCEDURE ( VAR _FILE , _FILE ): INTEGER;
     MOVEDIR   = PROCEDURE ( _FILE , ARRAY OF CHAR ,
                             _FILE , ARRAY OF CHAR , BOOLEAN ): INTEGER;
     READDIR   = PROCEDURE ( _FILE , VAR SYSTEM.ADDRESS, VAR INTEGER ): INTEGER;
     READ      = PROCEDURE ( _FILE , INTEGER , SYSTEM.ADDRESS ,
                                    INTEGER , VAR INTEGER ): INTEGER;

     WRITE     = PROCEDURE ( _FILE , INTEGER , SYSTEM.ADDRESS ,
                                    INTEGER , VAR INTEGER ): INTEGER;
     fDOIO     = PROCEDURE ( _FILE , req.REQUEST ): INTEGER;
     LINK      = PROCEDURE ( _FILE , _FILE , ARRAY OF CHAR , BOOLEAN ): INTEGER;
     UNLINK    = PROCEDURE ( _FILE , ARRAY OF CHAR ): INTEGER;
     DU        = PROCEDURE ( _FILE , VAR INTEGER , VAR INTEGER ): INTEGER;
     CUT       = PROCEDURE ( _FILE , INTEGER ): INTEGER;
     EXTEND    = PROCEDURE ( _FILE , INTEGER ): INTEGER;
     MAKEFS    = PROCEDURE ( _FILE , ARRAY OF CHAR , ARRAY OF INTEGER ): INTEGER;
     MAKENODE  = PROCEDURE ( _FILE , VAR _FILE , ARRAY OF CHAR): INTEGER;
     GETFH     = PROCEDURE ( _FILE , VAR fs.FHANDLE );
     OPENFH    = PROCEDURE ( VAR _FILE , VAR fs.FHANDLE ): INTEGER;
     MOUNT     = PROCEDURE ( _FILE , ARRAY OF CHAR , _FILE ,
                                     ARRAY OF CHAR ,
                                     ARRAY OF CHAR ,
                                 VAR ARRAY OF CHAR , BOOLEAN ): INTEGER;
     MOUNT0    = PROCEDURE ( VAR _FILE , _FILE , _FILE ,
                                     ARRAY OF CHAR ,
                                 VAR ARRAY OF CHAR , BOOLEAN ): INTEGER;
     UNMOUNT   = PROCEDURE ( _FILE , ARRAY OF CHAR , INTEGER ): INTEGER;
     UNMOUNT0  = PROCEDURE ( _FILE ,                 INTEGER ): INTEGER;

     fsdesc = RECORD
                getattr    : GETATTR;
                setattr    : SETATTR;
                isdir      : ISDIR;
                state      : STATE;
                usrlock    : USRLOCK;
                usrunlock  : USRUNLOCK;
                open       : OPEN;
                create     : CREATE;
                close      : CLOSE;
                reopen     : REOPEN;
                flush      : FLUSH;
                makedir    : MAKEDIR;
                movedir    : MOVEDIR;
                readdir    : READDIR;
                read       : READ;
                write      : WRITE;
                doio       : fDOIO;
                link       : LINK;
                unlink     : UNLINK;
                du         : DU;
                cut        : CUT;
                extend     : EXTEND;
                makenode   : MAKENODE;
                getfh      : GETFH;
                mount      : MOUNT;
                unmount    : UNMOUNT;
                unmount0   : UNMOUNT0;
              END;

    fsdesc0 = RECORD
                fsid    : FS;
                openfh  : OPENFH;
                makefs  : MAKEFS;
                mount0  : MOUNT0;
                halt    : os.signal_ptr;
              END;

-------------------------- _FILESYSTEMS -------------------------
                          -------------

PROCEDURE define_filesys(name: ARRAY OF CHAR; fs: fsdesc0): INTEGER;
(* define filesystem *)

PROCEDURE remove_filesys(name: ARRAY OF CHAR): INTEGER;
(* remove filesystem *)


PROCEDURE makefs(disk: _FILE;
               fsname: ARRAY OF CHAR;
                label: ARRAY OF CHAR;
                 bads: ARRAY OF INTEGER): INTEGER;


PROCEDURE mount0(VAR root: _FILE; dev,host: _FILE;
                   fsname: ARRAY OF CHAR;
                   info  : ARRAY OF CHAR;
               VAR lab   : ARRAY OF CHAR; ro: BOOLEAN): INTEGER;

PROCEDURE unmount0(dir: _FILE; flush: INTEGER): INTEGER;
(* unmount filesystem ( "dir" is root directory of unmounted fsys) *)

PROCEDURE openfile(dir: _FILE; VAR f: _FILE;
                  name: ARRAY OF CHAR; state: BITSET): INTEGER;

PROCEDURE openfhandle(VAR f: _FILE; VAR fh: fs.FHANDLE): INTEGER;

PROCEDURE opendev (VAR f:_FILE; dev: ARRAY OF CHAR; mode: BITSET): INTEGER;

PROCEDURE  mountdev(f:_FILE): INTEGER;
PROCEDURE umountdev(f:_FILE): INTEGER;

PROCEDURE getroot(): _FILE;

----------------------------- CASH -----------------------------
                             ------

VAR
      (* statistics *)
      chsize : INTEGER;
      dkwrite: INTEGER;
      dkread : INTEGER;
      chread : INTEGER;

---------------------------- DEVICES ---------------------------
                            ---------

CONST

    disk   =  fs.disk;
    tty    =  fs.tty;
    spec   =  fs.spec;
  special  =  disk+tty+spec;

PROCEDURE define_driver(name,host: ARRAY OF CHAR; drn: INTEGER;
                             mode: BITSET;       doio: fs.DOIO): INTEGER;

PROCEDURE remove_driver(name: ARRAY OF CHAR): INTEGER;

END osFileSystem.
