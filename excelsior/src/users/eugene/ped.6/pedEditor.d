DEFINITION MODULE pedEditor; (* Sem 06-Mar-87. (c) KRONOS *)

                IMPORT  SYSTEM, Model, Windows;

TYPE EdtContext;

     PblContext = POINTER TO RECORD
                    Layer  : INTEGER;
                    CursorX: INTEGER;
                    CursorY: INTEGER;
                    Fixed  : BOOLEAN;
                    Signal : Model.Object;
                    Chip   : Model.Object;
                    LastPin: Model.Object;
                    Boxed  : BOOLEAN;
                    BoxX1,BoxX2,BoxY1,BoxY2: INTEGER;
                    ScaleX ,ScaleY ,WindowX,WindowY: INTEGER;
                    WindowW,WindowE,WindowS,WindowN: INTEGER;
                    ExtPin: BOOLEAN;
                    check_on: BOOLEAN;
                  END;

     Sheet     = POINTER TO RECORD
                   mdl          : Model.Object;
                   wnd          : Windows.window;
                   PublicContext: PblContext;
                   EditorContext: EdtContext;
                   ScreenContext: SYSTEM.ADDRESS;
                 END;

PROCEDURE Editor(s: Sheet);

END pedEditor.
