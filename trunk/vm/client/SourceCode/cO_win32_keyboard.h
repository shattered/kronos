#pragma once

class cO_win32_keyboard
{
public:
    cO_win32_keyboard();
    virtual ~cO_win32_keyboard();

    int read();

    enum { EMPTY = 512 };

private:
    HANDLE stdIn;
    char  szVK[8];

    int  decode(word vkChar);
};
