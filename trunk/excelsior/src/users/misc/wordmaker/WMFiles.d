DEFINITION MODULE WMFiles;
(* Text file handling, os dependent *)
(*This module handles two types of files: files and temporary files
  for files:
A: tFNSet, tOpen, Read_Line, .., tCloseUnchanged
B: tFNSet, tCreate, Write_Line, .., tClose
C: tFNSet, File_Delete
Temporary files are used to implement FIFO buffers:
D: TempFile_Set, Write_Line, .., TempFile_Revert, Read_Line, .., File_Delete *)

VAR  SrcFName: ARRAY [0..80] OF CHAR;

PROCEDURE tFNSet( fno: CARDINAL; mess: ARRAY OF CHAR );
PROCEDURE tFNGet( fno: CARDINAL; VAR mess: ARRAY OF CHAR );
PROCEDURE tOpen(fno: CARDINAL): BOOLEAN;
(*Open file for read. True on error *)
PROCEDURE tCreate(fno:CARDINAL): BOOLEAN;
(* If file exists, rename it to backup file. Delete previous backup
file. True if disk write error *)
PROCEDURE tClose( fno: CARDINAL): BOOLEAN;
(*Close previously created & linked file. True on error*)
PROCEDURE tCloseUnchanged(fno: CARDINAL): BOOLEAN;
(*Closes file opened for read*)
PROCEDURE Write_Line( fno: CARDINAL; VAR line: ARRAY OF CHAR ): BOOLEAN;
(* Write a line to file. True if disk write error *)
PROCEDURE Read_Line( fno: CARDINAL; VAR ln: ARRAY OF CHAR ): BOOLEAN;
(* Read next line from file, false if eof *)
PROCEDURE TempFile_Set(fno: CARDINAL);
PROCEDURE TempFile_Revert(fno: CARDINAL): BOOLEAN;
PROCEDURE File_Delete(fno: CARDINAL): BOOLEAN;
PROCEDURE IsDirectory(f: CARDINAL): BOOLEAN;
PROCEDURE PrintDir(f,lines: CARDINAL): BOOLEAN;

END WMFiles.
