#include "preCompiled.h"
#include <winsock2.h>
#include "cO_tcp.h"


dword __stdcall cO_tcp::tcpWorker(void *pv)
{
    cO_tcp *p = (cO_tcp *)pv;
    return p->tcpReader();
}


dword cO_tcp::tcpReader()
{
    char ch;

    while (recv(so, &ch, 1, 0) != SOCKET_ERROR)
    {
        pd->writeChar(ch);
    }
    return 0;
}


cO_tcp::cO_tcp(dword _so, cO_win32_display &d) : so(_so), pd(&d), thread(0)
{
    dword id;

    thread = CreateThread(NULL, 4096, tcpWorker, (void *)this, 0, &id);
    if (thread == null)
    {
        delete this;
        return;
    }

    if (SetThreadPriority(thread, THREAD_PRIORITY_TIME_CRITICAL) == 0)
    {
        delete this;
        return;
    }
}   


cO_tcp::~cO_tcp()
{
    if (thread != null)
        TerminateThread(thread, 0);
    if (thread != null)
        CloseHandle(thread);
}


int cO_tcp::write(const byte *buf, int bytes)
{
    return send(so, (const char *)buf, bytes, 0);
}
