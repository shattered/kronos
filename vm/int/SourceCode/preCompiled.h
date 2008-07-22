///////////////////////////////////////////////////////////////////
// preCompiled.h
#pragma once
#pragma warning(disable:4100) // unreferenced formal parameter
#pragma warning(disable:4201) // nonstandard extension used : nameless struct/union
#pragma warning(disable:4514) // unreferenced inline function has been removed
#pragma warning(disable:4214) // nonstandard extension used : bit field types other than int
#pragma warning(disable:4115) // named type definition in parentheses
#pragma warning(disable:4711) // function selected for automatic inline expansion
#pragma warning(disable:4710) // function not expanded

#define STRICT
//#define NOGDI
#define WIN32_LEAN_AND_MEAN
#define _CRT_SECURE_NO_WARNINGS

#include <Windows.h>
#include <WindowsX.h>
#include <WinIOctl.h>

#pragma warning(default:4100) // unreferenced formal parameter
#pragma warning(default:4201) // nonstandard extension used : nameless struct/union
#pragma warning(default:4214) // nonstandard extension used : bit field types other than int

#pragma intrinsic(memcmp, memcpy, memset, strcmp, strcpy, strlen)

#define unused(x) ((void)x)

typedef unsigned __int64 qword;
typedef __int64 qlong;
typedef BYTE  byte;
typedef DWORD dword;
typedef WORD  word;
#define null  NULL

inline 
int   abs(int x) { return x >= 0 ? x : -x; }
inline 
qlong qabs(qlong x) { return x >= 0 ? x : -x; }

#ifdef _DEBUG
    #define ODS(x) OutputDebugString(x)
    #define trace  _trace
    void   _trace(const char* fmt, ...);
    #define assert(exp) (void)( (exp) || (__assert(#exp, __FILE__, __LINE__), 0) )
    inline int __assert(const char* exp, const char* file, int line)
    {
        trace("\nassert(%s) failed in %s.%d\n", exp, file, line);
        _asm int 3;
        return 0;
    }
#else
    #define assert(x)
    #define ODS(x) {}
    #define trace  (void)

#endif

enum
{
    K = 1024
};

#ifndef _DEBUG
#if _MSC_VER < 1300
#pragma optimize("awsgy", on)
// a - assume no aliasing (e.g.  int x; int* p = x;  x = 1; *p = 2; x++; x==?)
// w - assume no aliasing accross functions borders;
// s - favor small size
// g - global opt OK
// y - optimize frame pointer
#endif
#endif



//
///////////////////////////////////////////////////////////////////
