#include "preCompiled.h"

typedef void (__cdecl *_PVFV)(void);

// pointers to initialization functions

extern "C"
{
// C initializers
#pragma data_seg(".CRT$XIA")
_PVFV __xi_a[] = { NULL };

#pragma data_seg(".CRT$XIZ")
_PVFV __xi_z[] = { NULL };


// C++ initializers
#pragma data_seg(".CRT$XCA")
_PVFV __xc_a[] = { NULL };
#pragma data_seg(".CRT$XCZ")
_PVFV __xc_z[] = { NULL };


// C pre-terminators
#pragma data_seg(".CRT$XPA")
_PVFV __xp_a[] = { NULL };
#pragma data_seg(".CRT$XPZ")
_PVFV __xp_z[] = { NULL };

// C terminators
#pragma data_seg(".CRT$XTA")
_PVFV __xt_a[] = { NULL };
#pragma data_seg(".CRT$XTZ")
_PVFV __xt_z[] = { NULL };

#pragma data_seg()  /* reset */

#pragma comment(linker, "/merge:.CRT=.data")
};


static 
void _initterm(_PVFV * pfbegin, _PVFV * pfend)
{
    // walk the table of function pointers from the bottom up, until
    // the end is encountered.  Do not skip the first entry.  The initial
    // value of pfbegin points to the first valid entry.  Do not try to
    // execute what pfend points to.  Only entries before pfend are valid.
    while (pfbegin < pfend)
    {
        if (*pfbegin != NULL)
            (**pfbegin)();
        ++pfbegin;
    }
}


struct ExitNode
{
    ExitNode*   next;
    _PVFV       func;
};

ExitNode* pExitList = NULL;

extern "C" int atexit(_PVFV exitfunc)
{
    ExitNode* pNode = (ExitNode*)GlobalAllocPtr(GPTR, sizeof(ExitNode));
    if (pNode == NULL)
        return -1;
    pNode->next = pExitList;
    pNode->func = exitfunc;
    pExitList = pNode;
    return 0;
}


int main();

extern "C" void mainCRTStartup(void)
{
    // do initializations
    _initterm( __xi_a, __xi_z );
    // do C++ initializations
    _initterm( __xc_a, __xc_z );
    UINT uExitCode = main();
    // do pre-terminators
    _initterm(__xp_a, __xp_z);
    // do terminators
    _initterm(__xt_a, __xt_z);
    while (pExitList != NULL)
    {
        ExitNode* pNode = pExitList;
        pExitList = pExitList->next;
        pNode->next = NULL;
        pNode->func();
        pNode->func = NULL;
        GlobalFreePtr(pNode);
    }
    ExitProcess(uExitCode);
}

///////////////////////////////////////////////////////////////////////////////
// Misc stupid functions:

// placeholder for pure functions:
extern "C" int _purecall(void)
{
    return 0;
}


extern "C" int _fltused = 0x9875;

extern "C" __declspec(naked)
int _ftol(float /*f*/)
{
    _asm 
    {
        push        ebp
        mov         ebp,esp
        add         esp,0F4h
        wait
        fnstcw      word ptr [ebp-2]
        wait
        mov         ax,word ptr [ebp-2]
        or          ah,0Ch
        mov         word ptr [ebp-4],ax
        fldcw       word ptr [ebp-4]
        fistp       qword ptr [ebp-0Ch]
        fldcw       word ptr [ebp-2]
        mov         eax,dword ptr [ebp-0Ch]
        mov         edx,dword ptr [ebp-8]
        leave
        ret
    }
}

//  _chkstk
//  Purpose:
//      Provide stack checking on procedure entry. Method is to simply probe
//      each page of memory required for the stack in descending order. 
//  Entry:
//      EAX = size of local frame
//  Exit:
//      ESP = new stackframe, if successful
//  Uses:
//      EAX

extern "C" __declspec(naked)
void _chkstk(void)
{
    #define _PAGESIZE_ 1000h
    _asm
    {
        push    ecx                     
        cmp     eax, _PAGESIZE_
        lea     ecx, [esp] + 8
        jb      short lastpage
probepages:
        sub     ecx, _PAGESIZE_
        sub     eax, _PAGESIZE_
        test    dword ptr [ecx], eax
        cmp     eax, _PAGESIZE_
        jae     short probepages
lastpage:
        sub     ecx, eax        
        mov     eax, esp
        test    dword ptr [ecx],eax
        mov     esp, ecx
        mov     ecx, dword ptr [eax]
        mov     eax, dword ptr [eax + 4]
        push    eax                     
        ret
    }
}

void* operator new(unsigned int n)
{
    return ::HeapAlloc(::GetProcessHeap(), HEAP_ZERO_MEMORY, n);
}


void operator delete(void* p)
{
    ::HeapFree(::GetProcessHeap(), 0, p);
}


extern "C"
{

__declspec(naked) 
qword _allshl(qword, word)
{
    _asm
    {
        cmp     cl, 64
        jae     short L2
        // Handle shifts of between 0 and 31 bits
        cmp     cl, 32
        jae     short L1
        shld    edx,eax,cl
        shl     eax,cl
        ret
L1:     // Handle shifts of between 32 and 63 bits
        mov     edx,eax
        xor     eax,eax
        and     cl,31
        shl     edx,cl
        ret
L2:     // return 0 in edx:eax
        xor     eax,eax
        xor     edx,edx
        ret
    }

}


__declspec(naked) 
qword _aullshr(qword, word)

{
    _asm
    {
        // Handle shifts of 64 bits or more (if shifting 64 bits or more, the result
        // depends only on the high order bit of edx).
        cmp     cl,64
        jae     short L2
        // Handle shifts of between 0 and 31 bits
        cmp     cl, 32
        jae     short L1
        shrd    eax,edx,cl
        shr     edx,cl
        ret
L1:     // Handle shifts of between 32 and 63 bits
        mov     eax,edx
        xor     edx,edx
        and     cl,31
        shr     eax,cl
        ret
L2:     // return 0 in edx:eax
        xor     eax,eax
        xor     edx,edx
        ret
    }
}

__declspec(naked) 
qword _allshr(qword, word)
{
    _asm
    {
        cmp     cl,64
        jae     L1

        cmp     cl, 32
        jae     L2
        shrd    eax,edx,cl
        sar     edx,cl
        ret
L2:
        mov     eax,edx
        sar     edx,31
        and     cl,31
        sar     eax,cl
        ret
L1:
        sar     edx,31
        mov     eax,edx
        ret
    }
}


#define hiWord(x) [x + 4]
#define loWord(x) [x + 0]

__declspec(naked) 
qword _allmul(qword, qword) // same for signed/unsigned
{
    _asm
    {
//A     EQU   [esp + 4]       ; stack address of a
//B     EQU   [esp + 12]      ; stack address of b
//
//      AHI, BHI : upper 32 bits of A and B
//      ALO, BLO : lower 32 bits of A and B
//
//      ALO * BLO
//      ALO * BHI
// +    BLO * AHI
// ---------------------
//
// swap LO/HI for little endian:
// LOWORD  equ    [0]
// HIWORD  equ    [4]
#define A       esp + 4
#define B       esp + 12
        mov     eax,hiWord(A)
        mov     ecx,hiWord(B)
        or      ecx,eax             //  test for both hiwords zero.
        mov     ecx,loWord(B)
        jnz     short _allmul_hard  //  both are zero, just mult ALO and BLO

        mov     eax,loWord(A)
        mul     ecx

        ret     16                  //  callee restores the stack

_allmul_hard:
        push    ebx

    //  must redefine A and B since esp has been altered
        #undef A
        #undef B
        #define A2      esp + 8
        #define B2      esp + 16

        mul     ecx                 //  eax has AHI, ecx has BLO, so AHI * BLO
        mov     ebx,eax             //  save result

        mov     eax,loWord(A2)
        mul     dword ptr hiWord(B2) // ALO * BHI
        add     ebx,eax             //  ebx = ((ALO * BHI) + (AHI * BLO))

        mov     eax,loWord(A2)      //  ecx = BLO
        mul     ecx                 //  so edx:eax = ALO*BLO
        add     edx,ebx             //  now edx has all the LO*HI stuff

        pop     ebx

        ret     16                  //  callee restores the stack
    }
#undef A2
#undef B2
}

};
