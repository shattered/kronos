DEFINITION MODULE def_GDT; (* Sem 07-May-91. (c) KRONOS *)

FROM SYSTEM      IMPORT ADDRESS;

TYPE
  selector = {0..0FFFFh};
  CAR16    = {0..0FFFFh};
  CAR8     = {0..0FFh};
  tss_body = RECORD
    my_selector: selector;
    stk        : ARRAY [0..2] OF RECORD sp: CAR16; ss: selector END;
    entry_point: CAR16;
    flags      : CAR16;
    ax,cx      : CAR16;
    dx,bx      : CAR16;
    sp,bp      : CAR16;
    si,di      : CAR16;
    es,cs      : selector;
    ss,ds      : selector;
    ldt        : selector;
  END;
  tss = POINTER TO tss_body;
  s_modes = (
    sm_access,  -- к сегменту был доступ
    sm_type0,
    sm_type1,
    sm_type2,
    sm_data,    -- сегмент кода или данных
    sm_dpl0,    -- уровень приоритета, бит 0
    sm_dpl1,    -- уровень приоритета, бит 1
    sm_avail    -- размещен в физической памяти
  );
  s_mode = SET OF s_modes;
  selector_body = RECORD
    limit      : CAR16;
    base       : CAR16;
    base_h     : CAR8;
    mode       : s_mode;
    res        : CAR16;
  END;

CONST
  tss_avail   = s_mode{sm_type0};
  tss_busy    = s_mode{sm_type0,sm_type1};
  code_read   = s_mode{sm_type0,sm_type2,sm_data};
  code_common = s_mode{sm_type0,sm_type1,sm_type2,sm_data};
  data_write  = s_mode{sm_type0,sm_data};
  data_back   = s_mode{sm_type0,sm_type1,sm_data};
  trap_entry  = s_mode{sm_type0,sm_type1};

---- descriptor positions in global table
  gdt_i = 100h; -- global descriptors table
  idt_i = 101h; -- interrupt descriptors table
  stk_i = 102h; -- stack of main process
  -- 103h, 104h -- stack extension
  vga_i = 105h; -- VGA descriptor
  dos_i = 106h; -- descriptor for save real mode DS,ES
  env_i = 107h; -- descriptor for DOS enviroment string
  prm_i = 108h; -- descriptor for DOS parameters string
  oth_i = 109h; -- other descriptors
  glo_i =    0; -- globals & codes of main program
  mem_i = 200h; -- first segment of non structured memory
  top_i = 2FFh; -- last segment of non structured memory

  gdt_s = selector(gdt_i*8);
  idt_s = selector(idt_i*8);
  stk_s = selector(stk_i*8);
  vga_s = selector(vga_i*8);
  dos_s = selector(dos_i*8);
  env_s = selector(env_i*8);
  prm_s = selector(prm_i*8);
  glo_s = selector(glo_i*8);
  mem_s = selector(mem_i*8);
  top_s = selector(top_i*8);

  gdt_a = ADDRESS(gdt_i*80000h);
  idt_a = ADDRESS(idt_i*80000h);
  stk_a = ADDRESS(stk_i*80000h);
  vga_a = ADDRESS(vga_i*80000h);
  dos_a = ADDRESS(dos_i*80000h);
  env_a = ADDRESS(env_i*80000h);
  prm_a = ADDRESS(prm_i*80000h);
  glo_a = ADDRESS(glo_i*80000h);
  mem_a = ADDRESS(mem_i*80000h);
  top_a = ADDRESS(top_i*80000h);

END def_GDT.
