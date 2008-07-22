DEFINITION MODULE defErrors; (* Ned 16-Oct-89. (c) KRONOS *)
                             (* Leo 03-Nov-89. (c) KRONOS *)

(*  Модуль содержит определение кодов ошибок OS Excelsior *)

CONST

  ok = 0;  (* см. Примечания *)

                    --- common errors ---

  no_memory          =  80h;    -- нет памяти
  not_enough         =  81h;    -- недостаток ресурса
  busy               =  82h;    -- ресурс занят
  bad_name           =  83h;    -- плохое имя
  bad_parm           =  84h;    -- плохой параметр
  inv_op             =  85h;    -- неверная (нереализованая) операции
  ipted_op           =  86h;    -- прерванная операции
  bad_desc           =  87h;    -- испорченный дескриптор
  ill_desc           =  88h;    -- непригодный в данном случае дескриптор
  sec_vio            =  89h;    -- security violation
  su_only            =  8Ah;    -- for super user (white man) only
  inconsistency      =  8Bh;    -- несогласованность данных
  ill_vers           =  8Ch;    -- некоректная версия данных
  duplicate          =  8Dh;    -- объект с таким именем уже есть
  unsuitable         =  8Eh;    -- неподходящий объект
  no_entry           =  8Fh;    -- нет такого
  undef              =  90h;    -- [еще|уже] неопределенный объект
  ill_access         =  91h;    -- неправильный метод доступа к объекту

                  --- file system errors ---

  is_dir             = 0A0h;
  xcross             = 0A1h;
  not_dir            = 0A2h;
  no_data            = 0A3h;
  bad_fsys           = 0A4h;
  too_large          = 0A5h;
  not_blocked        = 0A6h;
  no_space           = 0A7h;
  fsys_full          = 0A8h;

                       --- i/o errors ---

  io_error           =  80000001h;

    not_ready        =  80000101h;    -- { 8}
    time_out         =  80000201h;    -- { 9}
    write_pro        =  80000401h;    -- {10}
    seek_err         =  80000801h;    -- {11}
    inv_dma          =  80001001h;    -- {12}
    data_crc         =  80002001h;    -- {13}
    head_crc         =  80004001h;    -- {14}
    miss_sid         =  80008001h;    -- {15} sector id not found
    miss_did         =  80010001h;    -- {16} data   id not found
    hw_fail          =  80020001h;    -- {17} hard_ware failure
    seek_0           =  80040001h;    -- {18} seek track 0 incomplete
    bad_block        =  80080001h;    -- {19} marked bad_block found
    inv_dad          =  80100001h;    -- {20} invalid device address
    chk_err          =  80200001h;    -- {21} data compare error
    ecc_err          =  80400001h;    -- {22} non correctable ECC error
    prog_err         =  80800001h;    -- {23} device programming error
    unsafe           =  81000001h;    -- {24} unsafe device detected
    par_err          =  82000001h;    -- {25} parity error
    frm_err          =  84000001h;    -- {26} frame error
    over_run         =  88000001h;    -- {27} data over run
    write_fail       =  90000001h;    -- {28} write_fail

                          --- traps ---

  quit               = 02h;   --  QUIT
  mem_vio            = 03h;   --  memory violation                       
  power_crash        = 04h;   --  power crash                            
  inv_instr          = 07h;   --  unimplemented instruction              
  call_ipt           = 08h;   --  call interrupt                         
  rtn_ipt            = 09h;   --  return interrupt
  trace_ipt          = 0Bh;   --  trace  interrupt
  pstk_overflow      = 40h;   --  P-stack overflow
  int_overflow       = 41h;   --  integer overflow
  real_overflow      = 42h;   --  real overflow
  real_underflow     = 43h;   --  real underflow
  addr_underflow     = 44h;   --  address underflow
  case_error         = 45h;   --  CASE without ELSE
  rtn_error          = 46h;   --  return from function without result
  halt               = 47h;   --  HALT
  assert             = 48h;   --  ASSERT
  instr_0FF          = 49h;   --  invalid instruction
  bounds_check       = 4Ah;   --  bounds check
  HW_assert          = 4Bh;   --  hardware ASSERT
  estk_overflow      = 4Ch;   --  E-stack over/underflow
  abort              = 4Dh;   --  ABORT
  heap_abort         = 4Eh;   --  no memory in heap
  inv_parm           = 4Fh;   --  illegal parameter
  break              = 50h;   --  BREAK (^C)
  unimp_proc         = 51h;   --  unimplemented procedure
  obsolete_proc      = 52h;   --  obsolete procedure
  no_resource        = 53h;   --  resource exhausted
  undef_exception    = 54h;   --  unexpected exception

(***************************************************************

--------------------  П Р И М Е Ч А Н И Я  ---------------------
                    -----------------------

     Модуль  содержит  определение  кодов  ошибок  OS  Excelsior.
Диапазон   01h..7Fh  содержит  номера  аппаратных  и  программных
прерываний. Диапазон 80h..0FFh содержит номера ошибок ОС.
     Ошибки  ввода/вывода нижнего уровня имеют выставленный 31-й
бит  в  коде  ошибки.  Биты  [8..30]  используются  для передачи
информации о причине ошибки.

ЗАМЕЧАНИЕ:
     Не   следует   порождать   ошибки   код   которых  содержит
выставленный  в  1 31-ой  бит,  т.к. они могут в дальнейшем быть
проинтерпритированны как ошибки в/в.


-----------------------------  OK  -----------------------------
                             ------
                   --------------------------
                   -- И все хорошо, а-а,   --
                   -- И все хорошо, а-а... --
                   --                 ДДТ  --
                   --------------------------

***************************************************************)

END defErrors.
