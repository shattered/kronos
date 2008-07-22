MODULE vme; (*  03-May-91. (c) KRONOS *)

FROM SYSTEM      IMPORT ADR, ADDRESS;

IMPORT  def : def_GDT;

TYPE
  CAR16 = def.CAR16;

CONST
  my_cs_i  = def.oth_i+0;
  my_ds_i  = def.oth_i+1;
  my_ss_i  = def.oth_i+2;
  tsk_ss_i = def.stk_i;
  my_ds_pc = def.selector(my_ds_i*8);
  my_cs_pc = def.selector(my_cs_i*8);

  idt_desc_ofs = def.idt_i*8;
  gdt_desc_ofs = def.gdt_i*8;

  code_md = def.code_read+def.s_mode{def.sm_avail};
  data_md = def.data_write+def.s_mode{def.sm_avail};

VAR
  idt       : ARRAY [0..255] OF def.selector_body;
  gdt       : ARRAY [0..def.top_i] OF def.selector_body;
  int_pc    : CAR16;
  save      : ARRAY [0..7] OF CAR16;
  idt_desc_r: def.selector_body;
  my_sp_r   : CAR16;
  my_es_r   : CAR16;
  my_ss_r   : CAR16;
  my_cs_p   : def.selector;
  my_ds_p   : def.selector;
  my_ss_p   : def.selector;

  tsk_cs_p  : def.selector;
  tsk_ss_p  : def.selector;
  tsk_ds_p  : def.selector;
  tsk_sp_p  : CAR16;
  tsk_ip_p  : CAR16;

  task_stack: ARRAY [0..7FFFh] OF CHAR;
  task_args : ARRAY [0..255] OF CHAR;
  task_env  : ARRAY [0..255] OF CHAR;

  sv_ds_es  : INTEGER;

PROCEDURE write(c: CHAR);
CODE pop dx % mov ah,02h % int 21h
END write;

PROCEDURE read(): CHAR;
CODE mov ah,08h % int 21h
END read;

PROCEDURE ws(VAL s: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  i:=0;
  WHILE s[i]#0c DO write(s[i]); INC(i) END;
END ws;

PROCEDURE write_hex(v,n: INTEGER);
  VAR i: INTEGER;
BEGIN
  WHILE n>0 DO
    DEC(n);
    i:=(v>>(n*4)) MOD 10h;
    IF i>9 THEN i:=i+7 END;
    write(CHAR(ORD('0')+i));
  END;
END write_hex;

PROCEDURE run_task;
  PROCEDURE run;
  CODE
        db 09Bh,0DBh,0E3h,09Bh
        mov WORD my_es_r,0
        mov my_ss_r,ss
        mov my_sp_r,sp
        call x2
x2:     pop si
        add si,x3'OFS-x2'OFS
        seg cs
        mov [si-4],si
        cli
        les si,ADR gdt
        seg es
        lidt [si+idt_desc_ofs]
        seg es
        lgdt [si+gdt_desc_ofs]
        smsw ax
        or ax,1
        lmsw ax
        jmp far my_cs_pc:0
x3:     mov ss,tsk_ss_p
        mov sp,tsk_sp_p
        push tsk_cs_p
        push tsk_ip_p
        mov es,tsk_ds_p
        mov ds,tsk_ds_p
        sti
        ret far
  END run;
BEGIN
  run;
END run_task;

PROCEDURE exit_trap;
  VAR
    ds,ss,sp,pc,ds_r,ss_r,ax: INTEGER;
    ds_a,ss_a: POINTER TO ARRAY [0..0FFFFh] OF CHAR;
BEGIN
  ds  :=INTEGER(save[1]) DIV 8;
  ss  :=INTEGER(save[2]) DIV 8;
  sp  :=INTEGER(save[4]);
  ax  :=INTEGER(save[5]);
  ss_r:=INTEGER(gdt[ss].base) + INTEGER(gdt[ss].base_h)*10000h;
  ss_a:=ADDRESS(ss_r MOD 10h + ss_r DIV 10h * 10000h);
  ds_r:=INTEGER(gdt[ds].base) + INTEGER(gdt[ds].base_h)*10000h;
  ds_a:=ADDRESS(ds_r MOD 10h + ds_r DIV 10h * 10000h);
  pc  :=ORD(ss_a^[sp])+ORD(ss_a^[sp+1])*100h;
  ax:=ax MOD 100h;
  IF (ax=0) OR (ax=47h) THEN HALT END;
  ws('module ');
  ws(ds_a^);
  ws(', exit code ');
  write_hex(ax MOD 100h,2);
  ws(', pc ');
  write_hex(pc,4);
  ws(''15c 12c);
  HALT;
END exit_trap;

PROCEDURE int_trap;
  PROCEDURE trap_code;
  CODE
        call l
    l:  pop si
        add si,e'OFS-l'OFS
        mov int_pc,si
        add si,x1'OFS-e'OFS
        seg cs
        mov [si-2],cs
        seg cs
        mov [si-4],si
        add si,x2'OFS-x1'OFS
        seg cs
        mov [si-4],si
        pop bp
        pop ds
        ret far
    e:  call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop%
        call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop%
        call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop%
        call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop%
        call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop%
        call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop%
        call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop%
        call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop%
        call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop%
        call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop%
        call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop%
        call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop%
        call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop%
        call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop%
        call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop%
        call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop% call ok%nop%
    ok:
        push ds
        push my_ds_pc
        pop ds
        mov save[0],si
        pop si
        mov save[2],si ; ds
        pop si
        mov save[4],ss
        mov save[6],es
        mov save[8],sp
        mov save[10],ax
        mov save[12],bx
        mov save[14],cx
        mov bx,sp
        seg ss
        mov cl,[bx+4]
        pushf
        pop bx
        mov ch,bh
        mov ss,my_ss_p
        mov es,my_ds_p

        db 66h,0Fh,20h,0C0h ; mov eax,cr0
        and ax,0FFFEh
        db 66h,0Fh,22h,0C0h ; mov cr0,eax
        jmp far 0:0
    x1: seg cs
        mov ds,[0]
        mov es,my_es_r
        mov ss,my_ss_r
        mov sp,my_sp_r
        lidt idt_desc_r
        sub si,int_pc
        sub si,3 ; si - address of trap vector
        cmp si,10h
        jne l8
        mov BYTE save[10],4Ah
        jmp exit
    l8: cmp si,28h
        jc l6
        cmp si,38h
        jnc l6
        shr si,1   ; protection traps
        shr si,1
        mov save[10],si
        add WORD save[8],2
        jmp exit
    l6: cmp si,84h
        jne l9
        cmp BYTE save[11],4Ch
        jne l9
        jmp exit ; exit to DOS trap
    l9: push cx
        push cs
        call do_int
        seg cs % mov ds,[0]
        mov save[00],si
        mov save[10],ax
        mov save[12],bx
        mov save[14],cx
        pushf % pop cx ; cx = new values of flags
        les si,ADR gdt
        seg es % lidt [si+idt_desc_ofs]
        seg es % lgdt [si+gdt_desc_ofs]
        smsw si % or si,1 % lmsw si
        jmp far my_cs_pc:0
    x2: mov si,my_ds_p
        mov ds,si
        mov ss,save[4]
        mov es,save[6]
        mov sp,save[8]
        mov bx,sp
        seg ss
        mov [bx+4],cl
        mov si,save[00]
        mov ax,save[10]
        mov bx,save[12]
        mov cx,save[14]
        mov ds,save[02]
        iret

  do_int:
  ; si address of interrupt vector
        push 0 % pop ds
        push [si+2]
        push [si+0]
        seg cs % mov ds,[0]
        mov si,save[00]
        mov ax,save[10]
        mov bx,save[12]
        mov cx,save[14]
        mov es,sv_ds_es[2]
        mov ds,sv_ds_es[0]
        ret far

  exit: sti
  END trap_code;
BEGIN
  trap_code;
  exit_trap;
END int_trap;

PROCEDURE init_IDT;
  VAR i: INTEGER; p: PROC;
BEGIN
  p:=int_trap; p();
  FOR i:=0 TO HIGH(idt) DO
    idt[i].limit :=int_pc+CAR16(i)*4;
    idt[i].base  :=CAR16(my_cs_pc);
    idt[i].base_h:=0;
    idt[i].mode  :=def.trap_entry+
                   def.s_mode{def.sm_avail,def.sm_dpl0,def.sm_dpl1};
    idt[i].res   :=0;
  END;
END init_IDT;

PROCEDURE r_ph(a: ADDRESS): INTEGER;
BEGIN
  RETURN INTEGER(BITSET(a>>12)*{4..19})+INTEGER(BITSET(a)*{0..15});
END r_ph;

PROCEDURE set_gdt(no,adr,size: INTEGER; md: def.s_mode);
BEGIN
  WITH gdt[no] DO
    limit :=CAR16(size-1);
    base  :=CAR16(adr);
    base_h:=def.CAR8(adr>>16);
    mode  :=md;
    res   :=0;
  END;
END set_gdt;

PROCEDURE init_GDT;
  PROCEDURE get_ss(): INTEGER;
  CODE mov ax,ss % mov dx,0
  END get_ss;
  PROCEDURE get_ds(): INTEGER;
  CODE mov ax,ds % mov dx,0
  END get_ds;
  VAR i,cs: INTEGER;
BEGIN
  FOR i:=0 TO HIGH(gdt) DO set_gdt(i,0,0,def.s_mode{}) END;
  cs:=INTEGER(BITSET(int_trap>>16)*{0..15});
  set_gdt(def.idt_i,r_ph(ADR(idt)),BYTES(idt),data_md);
  set_gdt(def.gdt_i,r_ph(ADR(gdt)),BYTES(gdt),data_md);
  set_gdt(my_cs_i,cs*16,10000h,code_md);
  set_gdt(my_ds_i,get_ds()*16,10000h,data_md);
  set_gdt(my_ss_i,get_ss()*16,10000h,data_md);
  set_gdt(tsk_ss_i,r_ph(ADR(task_stack)),BYTES(task_stack),data_md);
  set_gdt(def.vga_i,0A0000h,10000h,data_md);
  set_gdt(def.dos_i,r_ph(ADR(sv_ds_es)),4,data_md);
  set_gdt(def.prm_i,r_ph(ADR(task_args)),BYTES(task_args),data_md);
  set_gdt(def.env_i,r_ph(ADR(task_env)),BYTES(task_env),data_md);
  FOR i:=def.mem_i TO def.top_i DO
IF i>=def.mem_i+32 THEN
    set_gdt(i,100000h+(i-def.mem_i)*8000h,10000h,data_md);
END;
  END;
END init_GDT;

PROCEDURE load_task(nm: ARRAY OF CHAR);
  PROCEDURE open(nm: ADDRESS): INTEGER;
  CODE
        pop dx
        pop ds
        mov ax,3D00h
        int 21h
        mov dx,0
        jnc ok
        mov dx,0FFFFh
    ok:
        seg cs
        mov ds,[0]
  END open;
  PROCEDURE close(f: CAR16);
  CODE
        pop bx
        pop cx
        mov ah,3Eh
        int 21h
  END close;
  PROCEDURE read(f: CAR16; a: ADDRESS; l: CAR16): INTEGER;
  CODE
        mov si,ds
        pop cx
        pop dx
        pop ds
        pop bx
        mov ah,3Fh
        int 21h
        mov dx,0
        jnc ok
        mov dx,0FFFFh
    ok: mov ds,si
  END read;
  PROCEDURE alloc(sz: CAR16): CAR16;
  CODE
        pop bx
        mov ah,48h
        int 21h
        jnc ok
        mov ax,0
    ok:
  END alloc;
  PROCEDURE chk(n: INTEGER);
  BEGIN
    IF n<0 THEN ws('file read error'15c 12c); HALT END;
  END chk;
  VAR f,no,t,sz: INTEGER; a: ADDRESS;
BEGIN
  f:=open(ADR(nm));
  IF f<0 THEN ws('file not found'15c 12c); HALT END;
  LOOP
    chk(read(CAR16(f),ADR(no),4));
    IF no<0 THEN EXIT END;
    chk(read(CAR16(f),ADR(t),4));
    chk(read(CAR16(f),ADR(sz),4));
    IF sz>=10000h THEN ws('bad file structure'15c 12c); HALT END;
    IF no>HIGH(gdt) THEN ws('bad file structure'15c 12c); HALT END;
    a:=INTEGER(alloc(CAR16((sz+15) DIV 16)))<<16;
    IF a=ADDRESS(0) THEN ws('isufficient memory'15c 12c); HALT END;
    CASE t OF
      |0: chk(read(CAR16(f),a,CAR16(sz)));
          set_gdt(no,r_ph(a),sz,data_md);
      |1: chk(read(CAR16(f),a,CAR16(sz)));
          set_gdt(no,r_ph(a),sz,code_md);
      |2: set_gdt(no,r_ph(a),sz,data_md);
    ELSE ws('bad file structure'15c 12c); HALT;
    END;
  END;
  close(CAR16(f));
  tsk_cs_p :=def.selector(8);
  tsk_ds_p :=my_ds_p;
  tsk_ss_p :=def.selector(tsk_ss_i*8);
  tsk_sp_p :=CAR16(BYTES(task_stack));
  tsk_ip_p :=0;
  sv_ds_es :=0;
END load_task;

PROCEDURE read_ln(VAR nm: ARRAY OF CHAR);
  PROCEDURE arg(): ADDRESS;
  CODE
        mov ah,62h
        int 21h
        mov dx,bx
        mov ax,80h
  END arg;
  VAR i,j,n: INTEGER; p: POINTER TO ARRAY [0..0FFh] OF CHAR;
BEGIN
  p:=arg(); i:=1; j:=0; n:=ORD(p^[0]);
  WHILE (i<=n) & (p^[i]=' ') DO INC(i) END;
  WHILE (i<=n) & (p^[i]#' ') DO nm[j]:=p^[i]; INC(i); INC(j) END;
  WHILE (i<=n) & (p^[i]=' ') DO INC(i) END;
  nm[j]:='.'; INC(j); nm[j]:='v'; INC(j);
  nm[j]:='m'; INC(j); nm[j]:='e'; INC(j);
  nm[j]:=0c; j:=0;
  WHILE (i<=n) DO
    IF j<HIGH(task_args) THEN task_args[j]:=p^[i]; INC(j) END; INC(i);
  END;
  task_args[j]:=0c;
END read_ln;

PROCEDURE init_run;
  VAR nm: ARRAY [0..31] OF CHAR;
BEGIN
  idt_desc_r.limit :=3FFh;
  idt_desc_r.base  :=0;
  idt_desc_r.base_h:=0;
  idt_desc_r.mode  :=def.s_mode{};
  idt_desc_r.res   :=0;
  my_ds_p:=my_ds_pc;
  my_cs_p:=my_cs_pc;
  my_ss_p:=def.selector(my_ss_i*8);
  init_GDT;
  init_IDT;
  task_env:='';
  read_ln(nm);
  load_task(nm);
  run_task;
END init_run;

BEGIN
  init_run;
END vme.
