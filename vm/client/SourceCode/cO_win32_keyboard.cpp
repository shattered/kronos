#include "preCompiled.h"
#include "cO_win32_keyboard.h"


int cO_win32_keyboard::read()
{
    int ch = EMPTY;

    if (szVK[0] == 0)
    {
        INPUT_RECORD Buffer;
        dword nRead = 0;

        if (!ReadConsoleInput(stdIn, &Buffer, 1, &nRead) || nRead < 1)
        {
            dword dw = GetLastError();
            trace("ReadConsoleInput: %d [%08X]\n", dw, dw);
        }
        else if ((Buffer.EventType & KEY_EVENT) != 0 && Buffer.Event.KeyEvent.bKeyDown)
        {
            if (!decode(Buffer.Event.KeyEvent.wVirtualKeyCode))
            {
                szVK[0] = char(Buffer.Event.KeyEvent.uChar.AsciiChar & 0xFF);
//              trace("ch=%02x\n", szVK[0]);
                szVK[1] = 0;
            }
            else
            {
//              trace("ch=%02x%02x\n", szVK[0],szVK[1]);
            }
        }
    }

    if (szVK[0] != 0)
    {
        ch = szVK[0];
        strcpy(szVK, &szVK[1]);
    }
    return ch;
}


cO_win32_keyboard::cO_win32_keyboard() : stdIn(0)
{
    memset(szVK, 0, sizeof szVK);
    stdIn  = ::GetStdHandle(STD_INPUT_HANDLE);

    DWORD dwMode = 0;
    GetConsoleMode(stdIn, &dwMode);
    dwMode &= ~(ENABLE_PROCESSED_INPUT|ENABLE_LINE_INPUT|
                ENABLE_ECHO_INPUT|ENABLE_WINDOW_INPUT|ENABLE_MOUSE_INPUT);

    SetConsoleMode(stdIn, dwMode);
}


cO_win32_keyboard::~cO_win32_keyboard()
{
}


int cO_win32_keyboard::decode(word vkChar)
{
    switch (vkChar)
    {
        case VK_ESCAPE: strcpy(szVK, "\033\033");   break;
        case VK_UP:     strcpy(szVK, "\033A");      break;
        case VK_DOWN:   strcpy(szVK, "\033B");      break;
        case VK_LEFT:   strcpy(szVK, "\033D");      break;
        case VK_RIGHT:  strcpy(szVK, "\033C");      break;
        case VK_INSERT: strcpy(szVK, "\233R");      break;
        case VK_DELETE: strcpy(szVK, "\233S");      break;
        case VK_HOME:   strcpy(szVK, "\233G");      break;
        case VK_END:    strcpy(szVK, "\233O");      break;
        case VK_PRIOR:  strcpy(szVK, "\033?\156");  break;
        case VK_NEXT:   strcpy(szVK, "\033?\115");  break;
        case VK_F1:     strcpy(szVK, "\033P");      break;
        case VK_F2:     strcpy(szVK, "\033Q");      break;
        case VK_F3:     strcpy(szVK, "\033R");      break;
        case VK_F4:     strcpy(szVK, "\033S");      break;
        case VK_F5:     strcpy(szVK, "\033?\160");  break;
        case VK_F6:     strcpy(szVK, "\033?\161");  break;
        case VK_F7:     strcpy(szVK, "\033?\162");  break;
        case VK_F8:     strcpy(szVK, "\033?\163");  break;
        case VK_F9:     strcpy(szVK, "\033?\164");  break;
        case VK_F10:    strcpy(szVK, "\033?\165");  break;

        default:
            return false;
    }
    return true;
}
