DEFINITION MODULE nfs; (* Igo 04-Jan-92. (c) KRONOS *)

IMPORT  SYSTEM;

TYPE ADDRESS = SYSTEM.ADDRESS;

CONST MAXDATA    = 8192;
(* The maximum number of bytes of data in a READ or WRITE request *)

CONST MAXPATHLEN = 1024;
(* The maximum number of bytes in pathname argument *)

CONST MAXNAMLEN  = 256;
(* The maximum number of bytes in file name argument *)

CONST COOKIESIZE = 4;
(* The size in bytes of opaque "cookie" passed by READDIR *)

CONST FHSIZE     = 32;
(* The size in bytes of the opaque file handle *)

(* type of file *)

CONST NFNON = 0; (* non-file *)
      NFREG = 1; (* regular file *)
      NFDIR = 2; (* directory    *)
      NFBLK = 3; (* block-special device *)
      NFCHR = 4; (* character-special device *)
      NFLNK = 5; (* symbolic link *)

(* enum stat *)

CONST NFS_OK             = 0;
      NFSERR_PERM        = 1;  (* Not owner *)
      NFSERR_NOENT       = 2;  (* no such file or directory *)
      NFSERR_IO          = 5;  (* some sort of hard error   *)
      NFSERR_NXIO        = 6;  (* no such device or address *)
      NFSERR_ACCES       = 13; (* permission denied         *)
      NFSERR_EXIST       = 17; (* file exist                *)
      NFSERR_NODEV       = 19; (* no such device            *)
      NFSERR_NOTDIR      = 20; (* Not a directory           *)
      NFSERR_ISDIR       = 21; (* is a directory            *)
      NFSERR_FBIG        = 27; (* file too large            *)
      NFSERR_NOSPC       = 28; (* no space left on device   *)
      NFSERR_ROFS        = 30; (* read only filesystem      *)
      NFSERR_NAMETOOLONG = 63; (* filename toolong          *)
      NFSERR_NOTEMPTY    = 66; (* directory notempty        *)
      NFSERR_DQUOT       = 69; (* disk quota exceeded       *)
      NFSERR_STALE       = 70; (* invalid fhandle           *)
      NFSERR_WFLUSH      = 99;


TYPE timeval  = RECORD
                   seconds: INTEGER; (* must be unsigned --++ *)
                  useconds: INTEGER; (* must be unsigned --++ *)
                END;

     fattr    = RECORD
                  type   : INTEGER; (* file type *)
                  mode   : INTEGER;
                  nlink  : INTEGER; (* number of hard links *)
                  uid    : INTEGER; (* user  identefication number *)
                  gid    : INTEGER; (* group identefication number *)
                  size   : INTEGER; (* size in bytes of the file   *)
                  blocksize
                         : INTEGER; (* size in bytes of the block of the file *)
                  rdev   : INTEGER; (* device number if it type is NFCHR or NFBLK *)
                  blocks : INTEGER; (* number of blocks the file takes up on disk *)
                  fsid   : INTEGER; (* filesystem identefier *)
                  fileid : INTEGER; (* the number uniquely identifies the file within a filesystem *)
                  atime  : timeval; (* last access time *)
                  mtime  : timeval; (* last modification *)
                  ctime  : timeval; (* time when status of file was last changed *)
                END;
     (* mode is the access mode encoded as a set of a bits

        0040000 this is a directory;              type should be NFDIR
        0020000 this is a character special file; type should be NFCHR
        0060000 this is a block     special file; type should be NFBLK ?????
        0100000 this is a regular           file; type should be NFREG
        0120000 this is a sym-link          file; type should be NFLNK
        0140000 this is a named socket          ; type should be NFNON
        0004000 set user id on execution
        0002000 set group id on execution
        0001000 save swapped text even after use
        0000400 read permission for owner
        0000200 write permission for owner
        0000100 execute and search permission for owner
        0000040 read permission for group
        0000020 write permission for group
        0000010 execute and search permission for group
        0000004 read permission for others
        0000002 write permission for others
        0000001 execute and search permission for others

     *)

     attrstat  = RECORD
                   CASE state: INTEGER OF
                   |NFS_OK: attributes: fattr
                   END
                 END;

     sattr     = RECORD
                   mode : INTEGER;
                   uid  : INTEGER;
                   gid  : INTEGER;
                   size : INTEGER;
                   atime: timeval;
                   mtime: timeval;
                 END;
     (* A size of zero means the file should be truncated.
        A value of -1 indicates the field that should be ignored *)


     fhandle   = ARRAY [0..FHSIZE-1] OF CHAR;
     filename  = ARRAY [0..31      ] OF CHAR;
     path      = ARRAY [0..31      ] OF CHAR;
     nfscookie = ARRAY [0..3] OF CHAR;

     sattrargs = RECORD
                   file      : fhandle;
                   attributes: sattr
                 END;

     diropargs = RECORD
                   dir : fhandle;
                   name: filename
                 END;

     diropres  = RECORD
                   CASE state: INTEGER OF
                   |NFS_OK: file      : fhandle;
                            attributes: fattr
                   END
                 END;

  readlinkres  = RECORD
                   CASE stat: INTEGER OF
                   |NFS_OK: data: path
                   END
                 END;

      readargs = RECORD
                   file      : fhandle;
                   offset    : INTEGER;
                   count     : INTEGER;
                   totalcount: INTEGER; (* unused *)
                 END;

      readres  = RECORD
                   CASE state: INTEGER OF
                   |NFS_OK: attributes: fattr;
                            buf       : ADDRESS; (* filled bye caller *)
                            bpos      : INTEGER;
                   END
                 END;

    writeargs  = RECORD
                   file       : fhandle;
                   beginoffset: INTEGER; (* unused *)
                   offset     : INTEGER;
                   totalcount : INTEGER; (* unused *)
                   buf        : ADDRESS;
                   bpos       : INTEGER;
                   bsz        : INTEGER;
                 END;

    createargs = RECORD
                   where     : diropargs;
                   attributes: sattr
                 END;

    renameargs = RECORD
                   from: diropargs;
                   to  : diropargs;
                 END;

      linkargs = RECORD
                   from: fhandle;
                   to  : diropargs;
                 END;

   symlinkargs = RECORD
                   from      : diropargs;
                   to        : path;
                   attributes: sattr;
                 END;

   readdirargs = RECORD
                   dir   : fhandle;
                   cookie: nfscookie;
                   count : INTEGER;
                 END;

    entry      = RECORD
                   fileid: INTEGER;
                   mode  : INTEGER;
                   name  : filename;
                   cookie: nfscookie
                 END;

    readdirres = RECORD
                   CASE state: INTEGER OF
                   |NFS_OK: entries: DYNARR OF entry;
                            eof    : BOOLEAN;
                   END
                 END;

    statfsres  = RECORD
                   CASE state: INTEGER OF
                   |NFS_OK: tsize : INTEGER; (* optimum transfer size      *)
                            bsize : INTEGER; (* block size in bytes        *)
                            blocks: INTEGER; (* total number of blocks     *)
                            bfree : INTEGER; (* number of free blocks      *)
                            bavail: INTEGER  (* number of available blocks *)
                   END
                 END;

    nfa        = RECORD
                   iadr: INTEGER; (* ip address of target *)
                   tout: INTEGER; (* timeout              *)
                 END;

PROCEDURE null(): INTEGER; (* 00 *)

(* 01 *)
PROCEDURE getattr(VAR ia: nfa; VAR f: fhandle;   VAR attr: attrstat): INTEGER;

(* 02 *)
PROCEDURE setattr(VAR ia: nfa; VAR a: sattrargs; VAR attr: attrstat): INTEGER;

(* 03 *)
PROCEDURE root; (* empty *)

(* 04 *)
PROCEDURE lookup(VAR ia: nfa; VAR a: diropargs; VAR r: diropres): INTEGER;

(* 05 *)
PROCEDURE readlink(VAR ia: nfa; VAR f: fhandle; VAR r: readlinkres): INTEGER;

(* 06 *)
PROCEDURE read(VAR ia: nfa; VAR a: readargs; VAR r: readres): INTEGER;

(* 07 *)
PROCEDURE writecash; (* empty *)

(* 08 *)
PROCEDURE write(VAR ia: nfa; VAR a: writeargs; VAR attr: attrstat): INTEGER;

(* 09 *)
PROCEDURE create(VAR ia: nfa; VAR a: createargs ; VAR r: diropres): INTEGER;

(* 10 *)
PROCEDURE remove(VAR ia: nfa; VAR a: diropargs  ; VAR r: INTEGER ): INTEGER;

(* 11 *)
PROCEDURE rename(VAR ia: nfa; VAR a: renameargs ; VAR r: INTEGER ): INTEGER;

(* 12 *)
PROCEDURE link  (VAR ia: nfa; VAR a: linkargs   ; VAR r: INTEGER ): INTEGER;

(* 13 *)
PROCEDURE symlink(VAR ia: nfa; VAR a: symlinkargs; VAR r: INTEGER): INTEGER;

(* 14 *)
PROCEDURE mkdir  (VAR ia: nfa; VAR a: createargs; VAR r: diropres): INTEGER;

(* 15 *)
PROCEDURE rmdir  (VAR ia: nfa; VAR a: diropargs ; VAR r: INTEGER ): INTEGER;

(* 16 *)
PROCEDURE readdir(VAR ia: nfa; VAR a: readdirargs; VAR r: readdirres): INTEGER;

(* 17 *)
PROCEDURE statfs (VAR ia: nfa; VAR a: fhandle; VAR r: statfsres): INTEGER;


----------------------------- MOUNT ----------------------------
                             -------
TYPE
        fhstatus = RECORD
                     CASE status: INTEGER OF
                     |0: directory: fhandle
                     END
                   END;

       MOUNTLIST = POINTER TO mountlist;
       mountlist = RECORD
                     hostname : filename;
                     directory: filename;
                     next     : MOUNTLIST;
                   END;

      GROUPS     = POINTER TO groups;
      groups     = RECORD
                     grname: filename;
                     next  : GROUPS
                   END;

      EXPORTLIST = POINTER TO exportlist;
      exportlist = RECORD
                     filesys: filename;
                     group  : groups;
                     next   : EXPORTLIST
                   END;

(* 00 *)
PROCEDURE mnull;

(* 01 *)
PROCEDURE mount(VAR ia: nfa; VAR dirpath: ARRAY OF CHAR; VAR r: fhstatus): INTEGER;

(* 02 *)
PROCEDURE mdump(VAR ia: nfa; VAR ml: MOUNTLIST): INTEGER;

(* 03 *)
PROCEDURE unmount(VAR ia: nfa; VAR dirpath: ARRAY OF CHAR): INTEGER;

(* 04 *)
PROCEDURE unmountall(VAR ia: nfa): INTEGER;

(* 05 *)
PROCEDURE export(VAR ia: nfa; VAR el: exportlist): INTEGER;

END nfs.
