IMPLEMENTATION MODULE lowLevel; (* Sem 29-Nov-89. (c) KRONOS *)

FROM SYSTEM      IMPORT WORD, ADDRESS, ADR;

PROCEDURE bblt_rep_code;
CODE
        mov si,[bp+16]
        mov ds,[bp+18]
        mov di,[bp+24]
        mov es,[bp+26]
        mov ax,[bp+12]
        mov cl,03h
        and WORD[bp+12],7h
        shr ax,cl
        add si,ax
        mov ax,[bp+20]
        and WORD[bp+20],7h
        shr ax,cl
        add di,ax
        mov bl,0FFh
        mov dx,[bp+8]
        mov cx,8h
        sub cx,dx
        jc ll2    ; - переход если пишется больше одного байта
        shr bl,cl
ll2:
        sub dx,8
        add dx,[bp+20]
        jge ll3
        mov dx,0
ll3:
        mov [bp+8],dx
        mov cl,[bp+20]
        shl bl,cl  ; bl - маска записи первого байта приемника
        sub cl,[bp+12]
        and cl,7h  ; сl    - разность офсетов
        mov dh,0FFh
        shl dh,cl
        mov dl,dh
        not dl     ; dl, dh - маски записи
        test bl,dl
        jz  ll1
        mov al,[si]
        rol al,cl
        inc si
ll1:    mov ah,[si]
        rol ah,cl
        inc si
        and al,dl
        mov ch,ah
        and ah,dh
        or  ah,al
        xchg al,ch
        and ah,bl
        not bl
        seg es
        and bl,[di]
        or  ah,bl
        seg es
        mov [di],ah
        inc di
        mov bx,[bp+8]
        shr bx,1
        shr bx,1
        shr bx,1
        jz back_loop
main_loop:
        ; dl,dh - маски записи
        ; сl    - разность офсетов
        ; al    - последний прочитанный и сдвинутый байт источника
        ; сh    - используется для временного хранения
        ; bx    - счетчик байтов
        mov ah,[si]
        rol ah,cl
        inc si
        and al,dl
        mov ch,ah
        and ah,dh
        or  ah,al
        xchg al,ch
        seg es
        xchg ah,[di]
        inc di
        dec bx
        jnz main_loop

; осталось записать неполный байт
back_loop:
        ; dl,dh - маски записи
        ; сl    - разность офсетов
        ; al    - последний прочитанный и сдвинутый байт источника
        mov bl,[bp+8]
        and bl,7h
        jz exit
        cmp cl,bl
        jnc bl1
        mov ah,[si]
        rol ah,cl
        and al,dl
        and ah,dh
        or  al,ah
bl1:    mov cl,bl
        mov dl,0FFh
        shl dl,cl
        seg es
        mov ah,[di]
        and ah,dl
        not dl
        and al,dl
        or  ah,al
        seg es
        mov [di],ah
exit:
END bblt_rep_code;

PROCEDURE bblt_or_code;
CODE
        mov si,[bp+16]
        mov ds,[bp+18]
        mov di,[bp+24]
        mov es,[bp+26]
        mov ax,[bp+12]
        mov cl,03h
        and WORD[bp+12],7h
        shr ax,cl
        add si,ax
        mov ax,[bp+20]
        and WORD[bp+20],7h
        shr ax,cl
        add di,ax
        mov bl,0FFh
        mov dx,[bp+8]
        mov cx,8h
        sub cx,dx
        jc ll2    ; - переход если пишется больше одного байта
        shr bl,cl
ll2:
        sub dx,8
        add dx,[bp+20]
        jge ll3
        mov dx,0
ll3:
        mov [bp+8],dx
        mov cl,[bp+20]
        shl bl,cl  ; bl - маска записи первого байта приемника
        sub cl,[bp+12]
        and cl,7h  ; сl    - разность офсетов
        mov dh,0FFh
        shl dh,cl
        mov dl,dh
        not dl     ; dl, dh - маски записи
        test bl,dl
        jz  ll1
        mov al,[si]
        rol al,cl
        inc si
ll1:    mov ah,[si]
        rol ah,cl
        inc si
        and al,dl
        mov ch,ah
        and ah,dh
        or  ah,al
        xchg al,ch
        and ah,bl
        seg es
        or [di],ah
        inc di
        mov bx,[bp+8]
        shr bx,1
        shr bx,1
        shr bx,1
        jz back_loop
main_loop:
        ; dl,dh - маски записи
        ; сl    - разность офсетов
        ; al    - последний прочитанный и сдвинутый байт источника
        ; сh    - используется для временного хранения
        ; bx    - счетчик байтов
        mov ah,[si]
        rol ah,cl
        inc si
        and al,dl
        mov ch,ah
        and ah,dh
        or  ah,al
        xchg al,ch
        seg es
        or [di],ah
        inc di
        dec bx
        jnz main_loop

; осталось записать неполный байт
back_loop:
        ; dl,dh - маски записи
        ; сl    - разность офсетов
        ; al    - последний прочитанный и сдвинутый байт источника
        mov bl,[bp+8]
        and bl,7h
        jz exit
        cmp cl,bl
        jnc bl1
        mov ah,[si]
        rol ah,cl
        and al,dl
        and ah,dh
        or  al,ah
bl1:    mov cl,bl
        mov dl,0FFh
        shl dl,cl
        not dl
        and al,dl
        seg es
        or  [di],al
exit:
END bblt_or_code;

PROCEDURE bblt_xor_code;
CODE
        mov si,[bp+16]
        mov ds,[bp+18]
        mov di,[bp+24]
        mov es,[bp+26]
        mov ax,[bp+12]
        mov cl,03h
        and WORD[bp+12],7h
        shr ax,cl
        add si,ax
        mov ax,[bp+20]
        and WORD[bp+20],7h
        shr ax,cl
        add di,ax
        mov bl,0FFh
        mov dx,[bp+8]
        mov cx,8h
        sub cx,dx
        jc ll2    ; - переход если пишется больше одного байта
        shr bl,cl
ll2:
        sub dx,8
        add dx,[bp+20]
        jge ll3
        mov dx,0
ll3:
        mov [bp+8],dx
        mov cl,[bp+20]
        shl bl,cl  ; bl - маска записи первого байта приемника
        sub cl,[bp+12]
        and cl,7h  ; сl    - разность офсетов
        mov dh,0FFh
        shl dh,cl
        mov dl,dh
        not dl     ; dl, dh - маски записи
        test bl,dl
        jz  ll1
        mov al,[si]
        rol al,cl
        inc si
ll1:    mov ah,[si]
        rol ah,cl
        inc si
        and al,dl
        mov ch,ah
        and ah,dh
        or  ah,al
        xchg al,ch
        and ah,bl
        seg es
        xor [di],ah
        inc di
        mov bx,[bp+8]
        shr bx,1
        shr bx,1
        shr bx,1
        jz back_loop
main_loop:
        ; dl,dh - маски записи
        ; сl    - разность офсетов
        ; al    - последний прочитанный и сдвинутый байт источника
        ; сh    - используется для временного хранения
        ; bx    - счетчик байтов
        mov ah,[si]
        rol ah,cl
        inc si
        and al,dl
        mov ch,ah
        and ah,dh
        or  ah,al
        xchg al,ch
        seg es
        xor [di],ah
        inc di
        dec bx
        jnz main_loop

; осталось записать неполный байт
back_loop:
        ; dl,dh - маски записи
        ; сl    - разность офсетов
        ; al    - последний прочитанный и сдвинутый байт источника
        mov bl,[bp+8]
        and bl,7h
        jz exit
        cmp cl,bl
        jnc bl1
        mov ah,[si]
        rol ah,cl
        and al,dl
        and ah,dh
        or  al,ah
bl1:    mov cl,bl
        mov dl,0FFh
        shl dl,cl
        not dl
        and al,dl
        seg es
        xor  [di],al
exit:
END bblt_xor_code;

PROCEDURE bblt_bic_code;
CODE
        mov si,[bp+16]
        mov ds,[bp+18]
        mov di,[bp+24]
        mov es,[bp+26]
        mov ax,[bp+12]
        mov cl,03h
        and WORD[bp+12],7h
        shr ax,cl
        add si,ax
        mov ax,[bp+20]
        and WORD[bp+20],7h
        shr ax,cl
        add di,ax
        mov bl,0FFh
        mov dx,[bp+8]
        mov cx,8h
        sub cx,dx
        jc ll2    ; - переход если пишется больше одного байта
        shr bl,cl
ll2:
        sub dx,8
        add dx,[bp+20]
        jge ll3
        mov dx,0
ll3:
        mov [bp+8],dx
        mov cl,[bp+20]
        shl bl,cl  ; bl - маска записи первого байта приемника
        sub cl,[bp+12]
        and cl,7h  ; сl    - разность офсетов
        mov dh,0FFh
        shl dh,cl
        mov dl,dh
        not dl     ; dl, dh - маски записи
        test bl,dl
        jz  ll1
        mov al,[si]
        rol al,cl
        inc si
ll1:    mov ah,[si]
        rol ah,cl
        inc si
        and al,dl
        mov ch,ah
        and ah,dh
        or  ah,al
        xchg al,ch
        and ah,bl
        not ah
        seg es
        and [di],ah
        inc di
        mov bx,[bp+8]
        shr bx,1
        shr bx,1
        shr bx,1
        jz back_loop
main_loop:
        ; dl,dh - маски записи
        ; сl    - разность офсетов
        ; al    - последний прочитанный и сдвинутый байт источника
        ; сh    - используется для временного хранения
        ; bx    - счетчик байтов
        mov ah,[si]
        rol ah,cl
        inc si
        and al,dl
        mov ch,ah
        and ah,dh
        or  ah,al
        xchg al,ch
        not ah
        seg es
        and [di],ah
        inc di
        dec bx
        jnz main_loop

; осталось записать неполный байт
back_loop:
        ; dl,dh - маски записи
        ; сl    - разность офсетов
        ; al    - последний прочитанный и сдвинутый байт источника
        mov bl,[bp+8]
        and bl,7h
        jz exit
        cmp cl,bl
        jnc bl1
        mov ah,[si]
        rol ah,cl
        and al,dl
        and ah,dh
        or  al,ah
bl1:    mov cl,bl
        mov dl,0FFh
        shl dl,cl
        not dl
        and al,dl
        not al
        seg es
        and [di],al
exit:
END bblt_bic_code;

PROCEDURE move_code(dest,sou: ADDRESS; size: INTEGER);
CODE
        pop cx
        pop bx
        pop si
        pop ax
        pop di
        pop dx
        push ds
        shl bx,1

        and cx,cx
        jns l1
        inc bx
        and cx,7FFFh
l1:     and bx,bx
        jz l2

        push cx
loop:   mov ds,ax ; source
        mov es,dx ; dest
        mov cx,4000h
        cld
        repz
        movsw
        add ax,8h
        add dx,8h
        and si,7FFFh
        and di,7FFFh
        dec bx
        jnz loop
        pop cx

l2:     jcxz exit
        mov ds,ax
        mov es,dx
        rep
        movs
exit:   pop ds
END move_code;

PROCEDURE fill_code(dest: ADDRESS; size: INTEGER; val: CHAR);
CODE
        pop ax
        mov ah,al
        pop cx
        pop bx
        pop di
        pop dx
        shl bx,1

        cld
        and cx,cx
        jns l1
        inc bx
        and cx,7FFFh
l1:     and bx,bx
        jz l2

        push cx
loop:   mov es,dx ; dest
        mov cx,4000h
        repz
        stosw
        add dx,8h
        and di,7FFFh
        dec bx
        jnz loop
        pop cx

l2:     jcxz exit
        mov es,dx
        rep
        stos
exit:
END fill_code;

PROCEDURE move (to,fr: ADDRESS; sz: INTEGER);
BEGIN
  IF sz<=0 THEN RETURN END;
  move_code(to,fr,sz);
END move;

PROCEDURE bblt_rep(to: ADDRESS; to_ofs: INTEGER;
                   fr: ADDRESS; fr_ofs: INTEGER;
                   bits: INTEGER);
BEGIN
  IF bits<=0 THEN RETURN END;
  ASSERT(bits<8000h);
  bblt_rep_code;
END bblt_rep;

PROCEDURE bblt_or (to: ADDRESS; to_ofs: INTEGER;
                   fr: ADDRESS; fr_ofs: INTEGER;
                   bits: INTEGER);
BEGIN
  IF bits<=0 THEN RETURN END;
  ASSERT(bits<8000h);
  bblt_or_code;
END bblt_or;

PROCEDURE bblt_xor(to: ADDRESS; to_ofs: INTEGER;
                   fr: ADDRESS; fr_ofs: INTEGER;
                   bits: INTEGER);
BEGIN
  IF bits<=0 THEN RETURN END;
  ASSERT(bits<8000h);
  bblt_xor_code;
END bblt_xor;

PROCEDURE bblt_bic(to: ADDRESS; to_ofs: INTEGER;
                   fr: ADDRESS; fr_ofs: INTEGER;
                   bits: INTEGER);
BEGIN
  IF bits<=0 THEN RETURN END;
  ASSERT(bits<8000h);
  bblt_bic_code;
END bblt_bic;

PROCEDURE _zero(adr: ADDRESS; size: INTEGER);
BEGIN
  IF size>0 THEN fill_code(adr,size,0c) END
END _zero;

PROCEDURE _fill(adr: ADDRESS; size: INTEGER; val: WORD);
BEGIN
  IF size>0 THEN fill_code(adr,size,CHAR(val)) END
END _fill;

PROCEDURE zero(VAR area: ARRAY OF WORD);
BEGIN
  IF HIGH(area)>=0 THEN fill_code(ADR(area),SIZE(area),0) END
END zero;

PROCEDURE fill(VAR area: ARRAY OF WORD; val: WORD);
BEGIN
  IF HIGH(area)>=0 THEN fill_code(ADR(area),SIZE(area),CHAR(val)) END
END fill;

PROCEDURE cmove(des: ADDRESS; des_ofs: INTEGER;
                sou: ADDRESS; sou_ofs: INTEGER; bytes: INTEGER);
BEGIN
  move(des+des_ofs,sou+sou_ofs,bytes);
END cmove;

PROCEDURE quit;
CODE
        mov ax,4C47h
        int 21h
END quit;

PROCEDURE QUIT;
BEGIN
  quit
END QUIT;

BEGIN
  cpu_type:=2;
  cpu_mode:=1;
END lowLevel.
