#pragma once

class cO_win32_display
{
public:
    virtual void write(char *ptr, int bytes);
    virtual void writeChar(char ch);

    cO_win32_display();
    virtual ~cO_win32_display();

private:
    HANDLE stdOut;

    bool  bInEsc;
    COORD cord;
    short H;
    short W;
    char  szEsc[256];
    int   nEscCount;

    WORD fColor; // = FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE;
    WORD bColor; // = BACKGROUND_RED | BACKGROUND_GREEN | BACKGROUND_BLUE;

    WORD wForeground; // = fColor;
    WORD wBackground; // = bColor;

    int Number(char *p);
    void EraseLine();
    void Erase();
    void EraseChars(int n);
    void InsertChars(int n);
    void DeleteChars(int n);
    void InsertLines(int n);
    void DeleteLines(int n);
    void ScrollScreen(int dx, int dy, SMALL_RECT *p);
};
