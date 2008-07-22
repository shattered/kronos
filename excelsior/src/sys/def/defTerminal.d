DEFINITION MODULE defTerminal; (* Ned 23-Aug-89. (c) KRONOS *)

TYPE
  BARS  =  ARRAY [0..2],[0..2] OF CHAR;
  STATE =  RECORD
             type     : INTEGER;        hbar : CHAR;
             lines    : INTEGER;        vbar : CHAR;
             columns  : INTEGER;        bars : BARS;
             min_color: INTEGER;        back : INTEGER;
             max_color: INTEGER;        color: INTEGER;
             fonts    : INTEGER;        font : INTEGER;
             screens  : INTEGER;        scr  : INTEGER;

             cursor   : INTEGER;        awp   : INTEGER;
             something: INTEGER;        raw   : INTEGER;
             blinking : INTEGER;        smooth: INTEGER;
             reverse  : INTEGER;        cinter: INTEGER;
             underline: INTEGER;
           END;


CONST  (* M.S.B. of REQUEST.op when L.S.B. of it = CONTROL *)

             --- изменение состояния драйвера ---

  _info         = 00h;      -- (POINTER TO POINTER TO STATE)
  _reset        = 01h;
  _restore      = 02h;      -- (STATE)

  _raw          = 08h;      -- (ON_OFF: BOOLEAN) - raw output mode
  _autowrap     = 09h;      -- (ON_OFF: BOOLEAN)
  _smooth_scroll= 0Ah;      -- (ON_OFF: BOOLEAN)
  _screen       = 0Bh;      -- (no: INTEGER)

                 --- операции над экраном ---

  _up           = 11h;      -- count
  _down         = 12h;      -- count
  _left         = 13h;      -- count
  _right        = 14h;      -- count
  _home         = 15h;      --
  _bottom       = 16h;      --
  _erase        = 17h;      -- [0..2] 0 - к концу, 1 - к началу, 2 - все
  _erase_line   = 18h;      -- [0..2] 0 - к концу, 1 - к началу, 2 - все
  _erase_chars  = 19h;      -- count
  _repeat       = 1Ah;      -- char, count
  _set_pos      = 1Bh;      -- (line,col: INTEGER)
  _roll_up      = 1Ch;      -- count
  _roll_down    = 1Dh;      -- count
  _scroll_up    = 1Eh;      -- count
  _scroll_down  = 1Fh;      -- count
  _ins_char     = 20h;      -- count
  _del_char     = 21h;      -- count
  _ins_line     = 22h;      -- count
  _del_line     = 23h;      -- count
  _cursor       = 24h;      -- (on_off: BOOLEAN);
  _reverse      = 25h;      -- (on_off: BOOLEAN);
  _underline    = 26h;      -- (on_off: BOOLEAN);
  _blinking     = 27h;      -- (on_off: INTEGER);
  _something    = 28h;      -- (on_off: INTEGER);
  _cinter       = 29h;      -- (on_off: INTEGER);
  _color        = 2Ah;      -- (no    : INTEGER);
  _background   = 2Bh;      -- (no    : INTEGER);
                            -- <0   - пониженная интенсивность
                            -- =0   - нормальная интенсивность
                            -- >0   - повышенная интенсивность
  _font         = 2Ch;      -- no
  _load_font    = 2Dh;      -- no,from,to,ADR(font),SIZE(font)

  _set_attr     = 30h;      -- (no: INTEGER;     val: INTEGER);
  _get_attr     = 31h;      -- (no: INTEGER; VAR val: INTEGER);

END defTerminal.
