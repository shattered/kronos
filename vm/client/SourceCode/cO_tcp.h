#pragma once

#include "cO_win32_display.h"

class cO_tcp
{
public:
    cO_tcp(dword so, cO_win32_display &d);
    virtual ~cO_tcp();

    int write(const byte *buf, int bytes);
private:
    dword so;
    cO_win32_display *pd;

    // network reader thread:-
    dword tcpReader();
    HANDLE thread;

    static dword __stdcall tcpWorker(void *pv);
};
