#include "preCompiled.h"
#include <winsock2.h>
#include "cO_win32_display.h"
#include "cO_win32_keyboard.h"
#include "cO_tcp.h"

dword connectTo(const char *server, word port)
{
    WSADATA data;
    word nVersion = MAKEWORD(2,1);

    if (WSAStartup(nVersion, &data) != 0)
    {
        dword dw = WSAGetLastError();
        trace("connectTo(WSAStartup): %d [%08X]\n", dw, dw);
        return INVALID_SOCKET;
    }

    HOSTENT *he = gethostbyname(server);
    if (he == NULL)
    {
        long ipAddr = inet_addr(server);

        if (ipAddr != INADDR_NONE)
            he = gethostbyaddr((const char *)&ipAddr, sizeof(ipAddr), AF_INET);
    }

    if (he == NULL)
    {
        // host not found
        dword dw = WSAGetLastError();
        trace("connectTo(gethost): %d [%08X]\n", dw, dw);
        return INVALID_SOCKET;
    }

    dword so = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);

    if (so == INVALID_SOCKET)
    {
        // not enough memory?
        dword dw = WSAGetLastError();
        trace("connectTo(socket): %d [%08X]\n", dw, dw);
        return so;
    }

    SOCKADDR_IN sin;
    sin.sin_family = AF_INET;
    sin.sin_port   = htons(port);
    sin.sin_addr.S_un.S_addr = *(dword *)(he->h_addr_list[0]);

    if (connect(so, (sockaddr *)&sin, sizeof(sin)) == SOCKET_ERROR)
    {
        closesocket(so);

        // can not connect to the server
        dword dw = WSAGetLastError();
        trace("connectTo(connect): %d [%08X]\n", dw, dw);
        return INVALID_SOCKET;
    }
    return so;
}

int atoi(const char *s)
{
    int n = 0;

    while (*s >= '0' && *s <= '9')
    {
        n = n*10 + *s++ - '0';
    }
    return n;
}

char* skipword(const char* pStr)
{
    while (pStr != null && *pStr != 0 && *pStr != ' ')
    {
        if (*pStr == '"')
        {
            pStr++;
            while (*pStr != 0 && *pStr != '"') pStr++;
            if (*pStr != '"')
                return null;
            pStr++;
        }
        else
            pStr++;
    }
    return (char*)pStr;
}

char* skipspaces(const char* pStr)
{
    while (pStr != null && *pStr == ' ')
        pStr++;
    return (char*)pStr;
}


void ParseArgs(char **psz, word *port)
{
    char* pCommandLine = GetCommandLine();
    char *p = skipword(pCommandLine);
    p = skipspaces(p);

    if (p != null && *p != 0)
    {
        char* q = skipword(p);
        if (q != null && *q != 0) { *q = 0; q++; }

        if (*p == '"') p++;
        if (strlen(p) > 0 && p[strlen(p)-1] == '"') p[strlen(p)-1] = 0;
        if (strlen(p) > 0)
            *psz = p;

        if (q != null && *q != 0) p = skipspaces(q);
        else p = null;
    }

    if (p != null && *p != 0)
    {
        char* q = skipword(p);
        if (q != null && *q != 0) { *q = 0; q++; }

        if (*p == '"') p++;
        if (strlen(p) > 0 && p[strlen(p)-1] == '"') p[strlen(p)-1] = 0;
        if (strlen(p) > 0)
            *port = (word)atoi(p);
    }
}


int main() // int ac, char *av[])
{
    char *szServer = "localhost";
    word port = 8086;

    ParseArgs(&szServer, &port);

    dword so = connectTo(szServer, port);
    if (so == INVALID_SOCKET)
        return 1;

    cO_win32_display d;
    cO_win32_keyboard k;
    cO_tcp n(so, d);

    int ch;

    do
    {
        ch = k.read();
        if (ch != 0)
            n.write((byte *)&ch, 1);
    } while (ch != 0);

    return 0;
}
