DEFINITION MODULE Statistics; (* Leo 28-Feb-90. (c) KRONOS *)

IMPORT  SYSTEM;

CONST
  os_vers    = 101h;    (* system version no      *)
  os_runtime = 102h;    (* runtime in seconds     *)

  mem_top    = 201h;    (* main memory top        *)
  mem_core   = 202h;    (* occupied by system     *)
  mem_total  = 203h;    (* total memory           *)
  mem_free   = 204h;    (* total free             *)

  fs_chsize  = 301h;    (* file cash in words     *)
  fs_dkwrite = 302h;    (* total writen sectors   *)
  fs_dkread  = 303h;    (* sectors read from disk *)
  fs_chread  = 304h;    (* sectors read from cash *)

PROCEDURE get(attr: INTEGER; VAR val: SYSTEM.WORD);

END Statistics.
