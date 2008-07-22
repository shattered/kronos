IMPLEMENTATION MODULE pcTab; (* 03-Oct-90. (c) KRONOS *)

IMPORT  SYSTEM;

PROCEDURE unimplemented; BEGIN ASSERT(FALSE,51h) END unimplemented;

BEGIN
  gen_code :=SYSTEM.WORD(unimplemented);
  gen_def  :=SYSTEM.WORD(unimplemented);
  get_code :=SYSTEM.WORD(unimplemented);
  put_code :=SYSTEM.WORD(unimplemented);
  ini_gen  :=SYSTEM.WORD(unimplemented);
  get_name :=SYSTEM.WORD(unimplemented);
  find_var :=SYSTEM.WORD(unimplemented);
  error    :=SYSTEM.WORD(unimplemented);
  gen_const:=SYSTEM.WORD(unimplemented);
  chk_nil  :=FALSE;
  ini_nil  :=TRUE;
  cpu_type :=0;
  cpu_mode :=0;
END pcTab.
