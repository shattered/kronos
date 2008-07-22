DEFINITION MODULE bcBase; (* brd 10-Jan-91. (c) KRONOS *)

IMPORT  def: bcDef;
IMPORT  obj: bcObj;
IMPORT  txt: bcText;
IMPORT  bmt: bcMath;
IMPORT  wnd: pmWnd;

TYPE CURSOR  = PROCEDURE(INTEGER,INTEGER);
     MONITOR = PROCEDURE(): BOOLEAN;
      COLOR  = INTEGER;

---------------------------- COLORS ----------------------------

CONST
      black= 0;         gray=  8;
        red= 1;        red_l=  9;
      green= 2;      green_l= 10;
     yellow= 3;     yellow_l= 11;
       blue= 4;       blue_l= 12;
       cyan= 5;       cyan_l= 13;
  pale_blue= 6;  pale_blue_l= 14;
      white= 7;      white_l= 15;

VAR  bckg,rubb,mark: COLOR;

----------------------------------------------------------------------------

TYPE VMODEL;
     CONTEXT;

     VIEW = POINTER TO View;

     View = RECORD
        Xc,Yc,Zc: INTEGER; (* number for coordinat system              *)
       X_lf,Y_dw: REAL;    (* coordinate corner screen window at board *)
           scale: REAL;    (* scale                                    *)
          matrix: bmt.MATR;(*  matrix turn and shift                   *)
           vmode: BOOLEAN; (* TRUE 3D  FALSE 2D                        *)
           model: VMODEL;
            cntx: CONTEXT;
            wind: wnd.WINDOW;
            icon: BOOLEAN;
            tool: wnd.TOOL;
         END;

(* wind^.obj = VIEW *)

     CONTEXT= POINTER TO Context;

     Context= RECORD
      gstep_x,gstep_y: REAL;    (* step grid at board                   *)
      cstep_x,cstep_y: REAL;    (* step cursor at board                 *)
          c_X,c_Y,c_Z: REAL;    (* current coordinate cursor at board   *)
                sX,sY:INTEGER;  (* screen coordinate cursor             *)
            max_scale: REAL;    (*                                      *)
                QUERY: BOOLEAN; (* TRUE query in prompt area FALSE not  *)
                COORD: BOOLEAN; (* on/off indicate coord                *)
                  PAN: BOOLEAN; (* on/off soft scroll                   *)
                 GRID: BOOLEAN; (* TRUE grid on;                        *)
                 STEP: BOOLEAN; (* step on/off                          *)
              by_grid: BOOLEAN; (* TRUE step by grid                    *)
                 mask: DYNARR OF  INTEGER; (* mask for layer            *)
                 work: INTEGER; (* work layer                           *)
               END;

  AREA= RECORD
          x,X: REAL;
          y,Y: REAL;
          z,Z: REAL
        END;

  VMODEL = POINTER TO vmodel;
  vmodel = RECORD
             name: ARRAY [0..31] OF CHAR;
            fname: ARRAY [0..255] OF CHAR;
            mbody: obj.Model;
            views: DYNARR OF VIEW;
             area: AREA  (* size work area  at board    *)
          END;

VAR  x_old1,y_old1,x_old,y_old: INTEGER; (* for rubber  cursors          *)
       x_mrk,y_mrk,X_mrk,Y_mrk: REAL;    (* size area  with mark objects *)
                         ENG  : BOOLEAN; (* TRUE  english FALSE russian  *)

VAR   cview: VIEW;             (* current view *)

VAL  models: DYNARR OF VMODEL;
      SW,SH: INTEGER;
        Max: REAL;
      error: INTEGER;
       done: BOOLEAN;

---------------------------- cursors ---------------------------

PROCEDURE cross      (x,y: INTEGER);
PROCEDURE cross_line (x,y: INTEGER);
PROCEDURE cross_vline(x,y: INTEGER);
PROCEDURE cross_hline(x,y: INTEGER);
PROCEDURE cross_cir  (x,y: INTEGER);
PROCEDURE cross_box  (x,y: INTEGER);
PROCEDURE cross_arc  (x,y: INTEGER);
PROCEDURE marker     (x,y: INTEGER);  (* circle r= 3         *)

PROCEDURE set_marked(base: def.VERTEX);
PROCEDURE cross_marked(x,y: INTEGER);   (* move marked objects *)
PROCEDURE end_marked;

-------------------------- calc coord --------------------------

PROCEDURE b_s(v: VIEW; t: def.VERTEX; VAR x,y :INTEGER); (* board to screen *)
PROCEDURE s_b(v: VIEW; x,y: INTEGER; VAR t: def.VERTEX); (* screen to board *)

---------------------- set AREA in VMODEL ----------------------
VAR null_area: AREA;

PROCEDURE extr (m: VMODEL; t: def.VERTEX);
PROCEDURE extr1(m: VMODEL; t: def.VERTEX; r:REAL);

PROCEDURE extr_lin(m: VMODEL; l: def.Line);
PROCEDURE extr_pln(m: VMODEL; p: def.Pline);

PROCEDURE extr_cir(m: VMODEL; c: def.Circ);
PROCEDURE extr_arc(m: VMODEL; a: def.Arc);
PROCEDURE extr_ell(m: VMODEL; a: def.Ellips);

PROCEDURE extr_txt(m: VMODEL; t: def.Text);

PROCEDURE extr_srf(m: VMODEL; s: def.Surf);
PROCEDURE extr_grp(m: VMODEL; g: def.Group);

------------------------ key & mous read -----------------------

PROCEDURE rd_numb (prompt,confirm: ARRAY OF CHAR; VAR x:REAL): BOOLEAN;
  (*  dialine *)
PROCEDURE rd_numb1(prompt: ARRAY OF CHAR; VAR r:REAL; x,y: INTEGER): BOOLEAN;
  (*  diabox  *)
PROCEDURE rd_point(prompt,confirm: ARRAY OF CHAR; VAR p: def.VERTEX): BOOLEAN;


CONST long={0}; vlong={1}; hlong={2};
VAL null_monitor: MONITOR;

PROCEDURE read_long (top: def.VERTEX; prompt: ARRAY OF CHAR; typ: BITSET;
                     crs: CURSOR; monitor: MONITOR; VAR r: REAL): BOOLEAN;

PROCEDURE read_point(prompt: ARRAY OF CHAR; crs: CURSOR; monitor: MONITOR;
                                           VAR v: def.VERTEX): BOOLEAN;

(* if monitor() THEN read_point=FALSE *)

PROCEDURE read_angle(VAR top,top1,top2: def.VERTEX): BOOLEAN;
(* top1 -vertex of angle *)

PROCEDURE  read_text(VAR top: def.VERTEX; VAR text: def.Text): BOOLEAN;

PROCEDURE make_str(f: txt.PFONT; VAR txt: def.Text);

(* сооружает text.pict по заданным параметрам текста *)

----------------------------------------------------------------

PROCEDURE step;    (* on/off step by grid *)

----------------------------- show -----------------------------

PROCEDURE show_coord;

PROCEDURE xshow_vector(v: VIEW; top1,top2: def.VERTEX; c: COLOR);

(* show primitives in all views for model *)

PROCEDURE xshow_line(model: VMODEL; lin: def.Line; c: COLOR);
PROCEDURE xshow_cir (model: VMODEL; cir: def.Circ; c: COLOR);
PROCEDURE xshow_arc (model: VMODEL; arc: def.Arc;  c: COLOR);

PROCEDURE xshow_pline (model: VMODEL; pln: def.Pline;  c: COLOR);
PROCEDURE xshow_ellips(model: VMODEL; ell: def.Ellips; c: COLOR);

PROCEDURE xshow_text (model: VMODEL; txt: def.Text;      c: COLOR);
PROCEDURE xshow_surf(model: VMODEL; srf: def.Surf);

PROCEDURE xshow_group(model: VMODEL; grp: def.Group; c: COLOR);

PROCEDURE show_line(model: VMODEL; lin: def.Line);
PROCEDURE show_cir (model: VMODEL; cir: def.Circ);
PROCEDURE show_arc (model: VMODEL; arc: def.Arc);

PROCEDURE show_pline (model: VMODEL; pln: def.Pline);
PROCEDURE show_ellips(model: VMODEL; ell: def.Ellips);

PROCEDURE show_text (model: VMODEL; txt: def.Text);

PROCEDURE show_surf(model: VMODEL; srf: def.Surf);

PROCEDURE show_group(model: VMODEL; grp: def.Group);

PROCEDURE show_mrk(view: VIEW);

PROCEDURE show_name(v: VIEW);

PROCEDURE  show_grid(view: VIEW);
PROCEDURE erase_grid(view: VIEW);

PROCEDURE mredraw(model: VMODEL);
PROCEDURE vredraw(view: VIEW);

PROCEDURE mrefresh;  (*  refresh current model  in all views *)
PROCEDURE vrefresh;  (*  refresh current model  in VMAIN     *)

---------------------------- models ----------------------------

PROCEDURE new_model(VAR new: VMODEL);
PROCEDURE remove_model(mod: VMODEL);

----------------------------- views ----------------------------

PROCEDURE new_view(model: VMODEL);
PROCEDURE make_view(model: VMODEL; VAR v: VIEW);
PROCEDURE dispose_view(view: VIEW);
PROCEDURE create_view;

------------------------ window manager ------------------------

PROCEDURE wmonitor;
PROCEDURE unzoom_views;  (* for all screen resized windows *)

END bcBase.
