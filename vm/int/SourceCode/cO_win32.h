#pragma once

#include "SIO.h"
#include "cO_win32_display.h"

class cO_win32 : public SIOOutbound
{
public:
    // SIOOutbound methods:
    virtual int  busyRead();
    virtual void write(char *ptr, int bytes);
    virtual void writeChar(char ch);
    virtual void onKey(bool bDown, int nVirtKey, int lKeyData, int ch);

    cO_win32();
    virtual ~cO_win32();

private:
    cO_win32_display d;
    HANDLE stdIn;
    char  szVK[8];

    // keyboard stream:-
    dword WINAPI kbdReader();
    HANDLE thread;
    HANDLE go;
    bool   reading;

    int  decode(word vkChar);

    static dword __stdcall kbdWorker(void *pv);
};
