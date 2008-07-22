#include "preCompiled.h"
#include "cO_win32_display.h"

///////////////////////////////////////////////////////////////////////////////

const int tr[256] = {
0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,
0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F,
0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2E,0x2F,
0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3A,0x3B,0x3C,0x3D,0x3E,0x3F,
0x40,0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0x49,0x4A,0x4B,0x4C,0x4D,0x4E,0x4F,
0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59,0x5A,0x5B,0x5C,0x5D,0x5E,0x5F,
0x60,0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6A,0x6B,0x6C,0x6D,0x6E,0x6F,
0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,0x78,0x79,0x7A,0x7B,0x7C,0x7D,0x7E,0x7F,
0xD0,0xD1,0xD2,0xD3,0xD4,0xD5,0xD6,0xD7,0xD8,0xD9,0xDA,0xDB,0xDC,0xDD,0xDE,0xDF,
0x90,0x91,0x92,0x93,0x94,0x95,0x96,0x97,0x98,0x99,0x9A,0x9B,0x9C,0x9D,0x9E,0x9F,
0xC0,0xC1,0xC2,0xC3,0xC4,0xC5,0xC6,0xC7,0xC8,0xC9,0xCA,0xCB,0xCC,0xCD,0xCE,0xCF,
0xB0,0xB1,0xB2,0xB3,0xB4,0xB5,0xB6,0xB7,0xB8,0xB9,0xBA,0xBB,0xBC,0xBD,0xBE,0xBF,
0xEE,0xA0,0xA1,0xE6,0xA4,0xA5,0xE4,0xA3,0xE5,0xA8,0xA9,0xAA,0xAB,0xAC,0xAD,0xAE,
0xAF,0xEF,0xE0,0xE1,0xE2,0xE3,0xA6,0xA2,0xEC,0xEB,0xA7,0xE8,0xED,0xE9,0xE7,0xEA,
0x9E,0x80,0x81,0x96,0x84,0x85,0x94,0x83,0x95,0x88,0x89,0x8A,0x8B,0x8C,0x8D,0x8E,
0x8F,0x9F,0x90,0x91,0x92,0x93,0x86,0x82,0x9C,0x9B,0x87,0x98,0x9D,0x99,0x97,0x9A};


cO_win32_display::cO_win32_display() :
    stdOut(0),
    bInEsc(false),
    H(50), W(80),
    nEscCount(0),
//  fColor(FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE),
//  bColor(BACKGROUND_RED | BACKGROUND_GREEN | BACKGROUND_BLUE),
    fColor(FOREGROUND_GREEN),
    bColor(BACKGROUND_GREEN),
    wForeground(fColor),
    wBackground(bColor)
{
    stdOut = ::GetStdHandle(STD_OUTPUT_HANDLE);

    DWORD dwMode;
    GetConsoleMode(stdOut, &dwMode);
    dwMode &= ~ENABLE_WRAP_AT_EOL_OUTPUT;
    SetConsoleMode(stdOut, dwMode);
    SetConsoleOutputCP(866);
    SetFileApisToANSI();
    SetConsoleTitle("Kronos VM. Excelsior iV. Copyright (c) 1984-2001 Kronos Group.");

    cord.X = 0;
    cord.Y = 0;
    SetConsoleCursorPosition(stdOut, cord);

    CONSOLE_SCREEN_BUFFER_INFO csbi;
    GetConsoleScreenBufferInfo(stdOut, &csbi);

    COORD size = csbi.dwSize;

    if (size.X < W || size.Y < H)
    {
        if (size.X < W)
            size.X = W;
        if (size.Y < H)
            size.Y = H;
        SetConsoleScreenBufferSize(stdOut, size);
        GetConsoleScreenBufferInfo(stdOut, &csbi);
    }
    // CODE PAGES
    // 1251 - Windows Cyrillic
    // 866  - MS/DOS Cyrillic
    // 20866 - koi8-R
    // 21866 - koi8-U (Ukranian)
    // 28595 - ISO Cyrillic

    dword cp = ::GetConsoleOutputCP();
//  trace("::GetConsoleOutputCP()=%d\n", cp);
    BOOL b = ::SetConsoleOutputCP(866);
//  trace("::SetConsoleOutputCP(866)=%d\n", b);
    cp = ::GetConsoleCP();
//  trace("::GetConsoleCP()=%d\n", cp);
    b = ::SetConsoleCP(20866);

    SMALL_RECT win;
    win.Top = 0;
    win.Left = 0;
    win.Bottom = (short)(H - 1);
    win.Right = (short)(W - 1);

    SetConsoleWindowInfo(stdOut, true, &win);
    GetConsoleScreenBufferInfo(stdOut, &csbi);

    size.X = W; size.Y = H;
    SetConsoleScreenBufferSize(stdOut, size);
    GetConsoleScreenBufferInfo(stdOut, &csbi);
    Erase();
}


cO_win32_display::~cO_win32_display()
{
}


void cO_win32_display::EraseChars(int n)
{
    COORD c = cord;
    dword nWritten = 0;
    FillConsoleOutputCharacterA(stdOut, ' ', n, c, &nWritten);
    FillConsoleOutputAttribute(stdOut, (WORD)(FOREGROUND_GREEN), n, c, &nWritten);
}

void cO_win32_display::EraseLine()
{
    dword nWritten = 0;
    FillConsoleOutputCharacterA(stdOut, ' ', W - cord.X, cord, &nWritten);
    FillConsoleOutputAttribute(stdOut, (WORD)(FOREGROUND_GREEN), W - cord.X, cord, &nWritten);
}


void cO_win32_display::Erase()
{
    COORD cl = cord;
    EraseLine();
    cord.X = 0;
    for (cord.Y = (short)(cord.Y + 1); cord.Y < H; cord.Y++)
        EraseLine();
    cord = cl;
}

// scroll screen contents to the right by dx chars and down by dy lines
// both dx and dy can be negative.
void cO_win32_display::ScrollScreen(int dx, int dy, SMALL_RECT *rcScroll)
{
    SMALL_RECT rcClip = {0, 0, W, H};
    COORD origin = {(short)(rcScroll->Left + dx), (short)(rcScroll->Top + dy)};
    CHAR_INFO Fill;
    Fill.Attributes = (WORD)(FOREGROUND_GREEN);
    Fill.Char.AsciiChar = ' ';
    ScrollConsoleScreenBuffer(stdOut, rcScroll, &rcClip, origin, &Fill);
}

void cO_win32_display::InsertChars(int n)
{
    SMALL_RECT rc = {cord.X, cord.Y, W, cord.Y};
    ScrollScreen(n, 0, &rc);
}

void cO_win32_display::DeleteChars(int n)
{
    SMALL_RECT rc = {(short)(cord.X+n), cord.Y, W, cord.Y};
    ScrollScreen(-n, 0, &rc);
}


void cO_win32_display::InsertLines(int n)
{
    SMALL_RECT rc = {0, cord.Y, W, H};
    ScrollScreen(0, n, &rc);
}


void cO_win32_display::DeleteLines(int n)
{
    SMALL_RECT rc = {0, (short)(cord.Y+n), W, H};
    ScrollScreen(0, (int)-n, &rc);
}


void cO_win32_display::write(char *ptr, int bytes)
{
    while (bytes--)
        writeChar(*ptr++);
}

int number(char* p)
{
    int n = 0;
    while (*p >= '0' && *p <= '9')
        n = n*10 + (*p++ - '0');

    if (n == 0)
        n = 1;
    return n;
}


void number2(char *p, int *n1, int *n2)
{
    int n = 0;
    while (*p >= '0' && *p <= '9')
        n = n*10 + (*p++ - '0');
    if (n == 0)
        n = 1;
    *n1 = n;

    n = 0;
    if (*p == ';')
    {
        p += 1;

        while (*p >= '0' && *p <= '9')
            n = n*10 + (*p++ - '0');
    }
    if (n == 0)
        n = 1;
    *n2 = n;
}


void cO_win32_display::writeChar(char ch)
{
    // ignore Ctrl+0
    if (ch == 'O' - 'A' + 1)
        return;

    bool bSetPos = false;
    if (ch != 033 && !bInEsc)
    {
        dword nWritten = 0;
        WriteConsole(stdOut, &tr[(byte)ch], 1, &nWritten, NULL);
        if (ch == '\r')
            cord.X = 0;
        else if (ch == '\n')
        {
            if (cord.Y < H-1)
                cord.Y++;
        }
        else
            cord.X++;
//      if (ch < ' ') trace("^%c", ch+'A'-1); else trace("%c", ch);
    }
    else if (ch == 033)
    {
//      trace("ESC");
        bInEsc = true;
        nEscCount = 0;
        memset(szEsc, 0, sizeof szEsc);
    }
    else 
    {
//      trace("%c", ch);

        bInEsc = false;

        if (nEscCount == 0)
        {
            bSetPos = true;
            switch (ch)
            {
                case 'A':   if (cord.Y > 0)     cord.Y--;   break;
                case 'B':   if (cord.Y < H - 1) cord.Y++;   break;
                case 'C':   if (cord.X < W - 1) cord.X++;   break;
                case 'D':   if (cord.X > 0)     cord.X--;   break;
                case 'H':   cord.X = cord.Y = 0;            break;
                case 'K':   EraseLine();                    break;
                case 'J':   Erase();                        break;

                case 'Y':
                case '[':
                    szEsc[nEscCount++] = ch;
                    bSetPos = false;
                    bInEsc = true;
                    break; // more to follow
                default:
                    bSetPos = false;
                    break;
            }
        }
        else
        {
            if (nEscCount >= sizeof(szEsc) - 1)
            {
                bSetPos = false;
            }
            else if (szEsc[0] == 'Y')
            {
                szEsc[nEscCount++] = ch;

                if (nEscCount < 3)
                {
                    bInEsc = true;
                    bSetPos = false;
                }
                else
                {
                    cord.Y = (short)(szEsc[1] - ' ');
                    cord.X = (short)(szEsc[2] - ' ');
                    bSetPos = true;
                }
            }
            else
            {
                szEsc[nEscCount++] = ch;

                bInEsc = false;
                bSetPos = true;

//              assert(szEsc[0] == '[');
                switch(ch)
                {
                case 'A':   if (cord.Y > 0)     cord.Y--;   break;
                case 'B':   if (cord.Y < H - 1) cord.Y++;   break;
                case 'D':   if (cord.X > 0)     cord.X--;   break;
                case 'C':   if (cord.X < W - 1) cord.X++;   break;
                case 'X':   EraseChars(number(&szEsc[1]));  break;
                case 'K':   EraseLine();                    break;
                case '@':   InsertChars(number(&szEsc[1])); break;
                case 'P':   DeleteChars(number(&szEsc[1])); break;
                case 'L':   InsertLines(number(&szEsc[1])); break;
                case 'M':   DeleteLines(number(&szEsc[1])); break;

                case 'H':   // set cursor position
                    {
                        int x, y;
                        number2(&szEsc[1], &y, &x);

                        if (x > W) x = W;
                        if (y > H) y = H;

                        cord.X = (short)(x - 1);
                        cord.Y = (short)(y - 1);
                    }
                    break;

                case 'T':   // scroll down
                    {
                        SMALL_RECT rc = {0,0,W,H};
                        ScrollScreen(0, number(&szEsc[1]), &rc);
                        cord.X = 0; cord.Y = 0;
                    }
                    break;
                case 'S':   // scroll up
                    {
                        SMALL_RECT rc = {0,0,W,H};
                        // Scroll up
                        ScrollScreen(0, -number(&szEsc[1]), &rc);
                        cord.X = 0; cord.Y = (short)(H - 1);
                    }
                    break;

                case 'r':   // inverse display
                    if (szEsc[1] == '0')
                    {
                        wForeground = fColor;
                        wBackground = 0;
                    }
                    else
                    {
                        wForeground = 0;
                        wBackground = bColor;
                    }
                    SetConsoleTextAttribute(stdOut, (WORD)(wBackground | wForeground));
                    break;

                case 'u':   // underline
                    if (szEsc[1] == '0')
                    {
                        fColor &= ~FOREGROUND_INTENSITY;
                        bColor &= ~BACKGROUND_INTENSITY;
                    }
                    else
                    {
                        if (fColor != 0) fColor |= FOREGROUND_INTENSITY;
                        if (bColor != 0) bColor |= BACKGROUND_INTENSITY;
                    }

                    if (wForeground != 0)
                        wForeground = fColor;
                    if (wBackground != 0)
                        wBackground = bColor;
                    SetConsoleTextAttribute(stdOut, (WORD)(wBackground | wForeground));
                    break;

                default:
                    // only digits separated by ';' allowed (at least in the subset we support):
                    bInEsc = (ch >= '0' && ch <= '9' || ch == ';');
                    bSetPos = false;
                    break;
                }
            }
        }

//      if (!bInEsc) trace("\n");
    }
    if (bSetPos)
        SetConsoleCursorPosition(stdOut, cord);
}
