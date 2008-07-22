?imul4
        push bp
        mov bp,sp
        push si
        xor si,si
        mov dx,[bp+0Ah]
        and dx,dx
        jnl l456
        not si
        neg dx
        neg WORD[bp+08h]
        sbb dx,0
        mov [bp+0Ah],dx
l456:   mov dx,[bp+06h]
        and dx,dx
        jg l46B
        je l478
        not si
        neg dx
        neg WORD[bp+04h]
        sbb dx,0
        je l478
l46B:   cmp WORD[bp+0Ah],0
        jne l4A0
        mov ax,dx
        mul WORD[bp+08h]
        jmp l482
l478:   mov ax,[bp+0Ah]
        and ax,ax
        je l482
        mul WORD[bp+04h]
l482:   jb l4A0
        mov dx,[bp+08h]
        mov bp,[bp+04h]
        xchg bp,ax
        mul dx
        add dx,bp
        jb l4A0
        js l4A0
        add ax,si
        adc dx,si
        xor ax,si
        xor dx,si
l49B:   pop si
        pop bp
        ret 8
l4A0:   mov al,40h
        add al,al
        mov ax,0FFFFh
        mov dx,07FFFh
        jmp l49B

?cmod4
        push bp
        mov bp,sp
        mov ax,[bp+04h]
        or  ax,[bp+06h]
        jz ovr
        mov cx,[bp+04h]
        mov bx,[bp+06h]
        xor si,si
        and bx,bx
        js div
shft:   inc si
        shl cx,1
        rcl bx,1
        and bx,bx
        jns shft
div:    mov ax,[bp+08h]
        mov dx,[bp+0Ah]
div3:   cmp dx,bx
        jnz div1
        cmp ax,cx
div1:   jc div2
        sub ax,cx
        sbb dx,bx
div2:   and si,si
        jz exit
        dec si
        shr bx,1
        rcr cx,1
        jmp div3
ovr:    mov al,40h
        add al,al
        mov ax,0
        mov dx,ax
exit:   pop bp
        ret 8h

?imod4
        pop si
        pop cx
        pop bx
        pop ax
        pop dx
        push si
        and bx,bx
        jns l1
        neg bx
        neg cx
        sbb bx,0
        neg dx
        neg ax
        sbb dx,0
        call imod4_sub
        neg dx
        neg ax
        sbb dx,0
        ret
l1:     jnz imod4_sub
        and cx,cx
        jnz imod4_sub
ovr:    mov al,40h
        add al,al
        mov ax,0
        mov dx,ax
        ret

imod4_sub:
        xor si,si
        and bx,bx
        js div
shft:   inc si
        shl cx,1
        rcl bx,1
        and bx,bx
        jns shft
div:    and dx,dx
        jns div3
        neg dx
        neg ax
        sbb dx,0
div6:   cmp dx,bx
        jnz div4
        cmp ax,cx
div4:   jc div5
        sub ax,cx
        sbb dx,bx
div5:   and si,si
        jz div7
        dec si
        shr bx,1
        rcr cx,1
        jmp div6
div7:   mov si,ax
        or  si,dx
        jz exit
        sub cx,ax
        sbb bx,dx
        mov ax,cx
        mov dx,bx
exit:   ret
div3:   cmp dx,bx
        jnz div1
        cmp ax,cx
div1:   jc div2
        sub ax,cx
        sbb dx,bx
div2:   and si,si
        jz exit
        dec si
        shr bx,1
        rcr cx,1
        jmp div3

?idiv4
        pop si
        pop cx
        pop bx
        pop ax
        pop dx
        push si
        and bx,bx
        js l1
        jnz idiv4_sub
        and cx,cx
        jnz idiv4_sub
        mov al,40h
        add al,al
        mov ax,0FFFFh
        mov dx,07FFFh
        ret

l1:     neg bx
        neg cx
        sbb bx,0
        neg dx
        neg ax
        sbb dx,0
idiv4_sub:
        push bp
        xor di,di
        xor bp,bp
        xor si,si
        and bx,bx
        js div
shft:   inc si
        shl cx,1
        rcl bx,1
        and bx,bx
        jns shft
div:    and dx,dx
        jns div3
        ; делимое < 0
        neg dx
        neg ax
        sbb dx,0
div6:   cmp dx,bx
        jnz div4
        cmp ax,cx
div4:   jc div5
        sub ax,cx
        sbb dx,bx
        inc di
div5:   and si,si
        jz div7
        dec si
        shr bx,1
        rcr cx,1
        shl di,1
        rcl bp,1
        jmp div6
div7:   neg bp
        neg di
        sbb bp,0
        mov si,ax
        or  si,dx
        jz exit
        sub di,1
        sbb bp,0
        jmp exit

div3:   ; делимое >= 0
        cmp dx,bx
        jnz div1
        cmp ax,cx
div1:   jc div2
        sub ax,cx
        sbb dx,bx
        inc di
div2:   and si,si
        jz exit
        dec si
        shr bx,1
        rcr cx,1
        shl di,1
        rcl bp,1
        jmp div3
exit:   mov ax,di
        mov dx,bp
        pop bp
        ret

?iquot4
        pop si
        pop cx
        pop bx
        pop ax
        pop dx
        push si
        xor si,si
        and cx,cx
        jnz l0
        and bx,bx
        jz ovr
l0:     and bx,bx
        jns l1
        neg bx
        neg cx
        sbb bx,0
        not si
l1:     and dx,dx
        jns l2
        neg dx
        neg ax
        sbb dx,0
        not si
l2:     push si
        call div_sub
        pop si
        add ax,si
        adc dx,si
        xor ax,si
        xor dx,si
        ret

ovr:    mov al,64
        add al,al
        mov ax,0FFFFh
        mov dx,07FFFh
        ret

div_sub:
        push bp
        xor di,di
        xor bp,bp
        xor si,si
        and bx,bx
        js div
shft:   inc si
        shl cx,1
        rcl bx,1
        and bx,bx
        jns shft
div:    cmp dx,bx
        jnz div1
        cmp ax,cx
div1:   jc div2
        sub ax,cx
        sbb dx,bx
        inc di
div2:   and si,si
        jz exit
        dec si
        shr bx,1
        rcr cx,1
        shl di,1
        rcl bp,1
        jmp div
exit:   mov ax,di
        mov dx,bp
        pop bp
        ret

?irem4
        pop si
        pop cx
        pop bx
        pop ax
        pop dx
        push si
        xor si,si
        and cx,cx
        jnz l0
        and bx,bx
        jz ovr
l0:     and bx,bx
        jns l1
        neg bx
        neg cx
        sbb bx,0
l1:     and dx,dx
        jns l2
        neg dx
        neg ax
        sbb dx,0
        not si
l2:     push si
        call div_sub
        pop si
        add ax,si
        adc dx,si
        xor ax,si
        xor dx,si
        ret

ovr:    mov al,64
        add al,al
        mov ax,0h
        mov dx,0h
        ret

div_sub:
        xor si,si
        and bx,bx
        js div
shft:   inc si
        shl cx,1
        rcl bx,1
        and bx,bx
        jns shft
div:    cmp dx,bx
        jnz div1
        cmp ax,cx
div1:   jc div2
        sub ax,cx
        sbb dx,bx
div2:   and si,si
        jz exit
        dec si
        shr bx,1
        rcr cx,1
        jmp div
exit:   ret

?setrng
        push bp
        mov bp,sp
        mov ax,[bp+8]
        sub ax,[bp+4]
        mov ax,[bp+10]
        sbb ax,[bp+6]
        jl zero
        mov ax,0FFFFh
        mov dx,ax
        mov cx,[bp+8]
        jcxz l2
l1:     shl ax,1 % rcl dx,1
        dec cx
        jnz l1
l2:     push ax
        push dx
        mov ax,1
        mov dx,0
        mov cx,[bp+4]
        jcxz l4
l3:     stc
        rcl ax,1 % rcl dx,1
        dec cx
        jnz l3
l4:     pop cx
        and dx,cx
        pop cx
        and ax,cx
        pop bp
        ret
zero:   mov ax,0
        mov dx,0
        ret 8
