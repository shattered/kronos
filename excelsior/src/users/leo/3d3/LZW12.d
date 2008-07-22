DEFINITION MODULE LZW12; (* Leo 20-Jun-91. (c) KRONOS *)

IMPORT  BIO;

(* Implementation uses Heap and Files *)

VAL done: BOOLEAN;
   error: INTEGER;

PROCEDURE pack  (unpacked,packed: BIO.FILE; in: INTEGER; VAR out: INTEGER;
                                                             mem: BOOLEAN);
PROCEDURE unpack(packed,unpacked: BIO.FILE;              VAR out: INTEGER);

PROCEDURE copy(from,to: BIO.FILE; len: INTEGER);

(*
   Files for "pack" and "unpack" must be openned or created
   before call.
   PACK compressed -in- bytes from file
   -unpacked- starting from the current file position
   and
   write compressed image to file
   -packed- starting from the current file position
   variable -out- is the number of bytes outputed to file -packed-.
   Parameter "mem" defines the memory model used by packing
   (it has influence to unpack algorithms too!).
     If mem=TRUE max amount of available memory used (about 540KB),
   and it may improve speed of compression very mach. But there may
   be a problem to unpack this file on computer with small
   memory.
     If mem=FALSE min amount of memory used for compression,
   speed and compression ratio may be not good for large files,
   but files may be later unpack on a small memory computers.

   UNPACK decompressed sequence of bytes from file
   -packed- starting from the current file position
   (note: the compressed portion of information is self defined,
    so it's no need to specify -in- bytes lenght)
   and
   write compressed image to file
   -unpacked- starting from the current file position
   variable -out- is the number of bytes outputed to file -unpacked-.

   Cause, LZW0.pack adds about 16 bytes internal info for each file,
   it's not recomended to compress files < 128 byte long
*)

END LZW12.

(*
 * Source:
 *   compress.c - File compression ala IEEE Computer, June 1984.
 *
 * Algorithm: from "A Technique for High Performance Data Compression",
 *   Terry A. Welch, IEEE Computer Vol 17, No 6 (June 1984), pp 8-19.
 *
 * Authors: (decvax)  compress.c,v 4.0 85/07/30 12:50:00 joe Release
 *   Spencer W. Thomas
 *   Jim McKie
 *   Steve Davies
 *   Ken Turkowski
 *   James A. Woods
 *   Joe Orost
 *
 * Translated to Modula-2 by
 *   Andy Denisov May 1991
 * Reimplementation and optimization
 *   Leopold  20-Jun-91
 *)
