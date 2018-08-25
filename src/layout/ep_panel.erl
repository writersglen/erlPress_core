%%% ==========================================================================
%%% ep_panel.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:      MIT
%%%   File:         ep_panel.erl
%%%   Description:  Create panel map 
%%%
%%%                 Panels wrap boxes (ep_box.erl) in tupleis to facilitate 
%%%                 page design and layout functions
%%%
%%%                 Panels "contain" content elements, either text or images.
%%%
%%%                 Sets of panels are composed into a "page grids" to
%%%                  organize content and direct "eye path." 
%%%
%%%                 Position = {X,Y}
%%%                 X = upper left X coordinate of panel in points  
%%%                 Y = upper left Y coordinate of panel in points
%%%                 Size = {Width, Height}
%%%                 Width = width of box in points
%%%                 Height = height of box in pnts 
%%%                 id = {page number, panel index, name}
%%%
%%%   IMPORTANT NOTE:
%%%
%%%                 PDF assumes that 0, 0 XY coordinates are at lower-left
%%%                 This seems unnatural for folks who read top-to_bottom
%%%                 So for convenience, we'll assume that 0, 0 XY is
%%%                 at upper-left of the panel.  This means that y will 
%%%                 have to be inverted relative to paper stock height when we 
%%%                 print.
%%% @end
%%% ==========================================================================


-module (ep_panel).

-export([create/3, panel/3]).
-export([get_id/1, get_page_number/1, get_panel_index/1, get_panel_name/1]).
-export([get_name_string/1]).
-export([get_position/1, get_text_position/1, get_size/1, get_radius/1, get_content_cursor/1]).
-export([get_available/1, consumed/2, will_fit/3, get_available_lines/3]).
-export([get_border/1]).
-export([get_border_style/1, get_border_color/1, get_margin/1]).
-export([get_measure/1, get_indent/1, get_rot/1]).
-export([get_jump_prompt/1, get_typestyle/1, get_li_fill/1]).

-export([update_id/2, update_position/2, update_size/2, update_radius/2]).
-export([update_content_cursor/2]).
-export([update_border/2, update_border_style/2, update_border_color/2]).
-export([update_background_color/2, update_margin/2, update_rot/2]).
-export([update_jump_prompt/2]).
-export([update_typestyle/2, update_li_fill/2, update_panel/3, break/1]).
-export([one_line_space/3]).
-export([reveal/1]).
-export([default_panel/0]).


-define(ID, {1, 1, top}).
-define(POSITION, {72, 72}).
-define(SIZE, {350, 500}).
-define(RADIUS, 10).
-define(NEXT_LINE, 72).
-define(BORDER, 0).
-define(BORDER_STYLE, solid).
-define(BORDER_COLOR, white).
-define(BACKGROUND_COLOR, white). % eg_pdf_op:color(gainsboro)).
-define(MARGIN, 10).
-define(INDENT, 20).
-define(TYPESTYLE, justify_report).
-define(LI_FILL, black).
-define(ROT, 0).
-define(JUMP_PROMPT, "No jump").



% -compile(export_all).

-include("../../include/ep.hrl").

%% ***********************************************************
%% Create panel map 
%% ***********************************************************

%% @doc Create panel map

-spec create(ID       :: tuple(),
             Position :: tuple(),
             Size     :: tuple()) -> map().

create(ID, Position, Size) ->
    #{ id                => ID
     , position          => Position 
     , size              => Size
     , radius            => ?RADIUS
     , content_cursor    => init_content_cursor(Position) 
     , border            => ?BORDER
     , border_style      => ?BORDER_STYLE
     , border_color      => ?BORDER_COLOR
     , background_color  => ?BACKGROUND_COLOR
     , margin            => ?MARGIN
     , typestyle         => ?TYPESTYLE
     , li_fill           => ?LI_FILL
     , indent            => ?INDENT
     , rot               => ?ROT
     , jump_prompt       => ?JUMP_PROMPT
     }. 


init_content_cursor(Position) ->
   {_X, Y} = Position,
   Y.


%% ***********************************************************
%% Display panel 
%% ***********************************************************

panel(PDF, Job, PanelMap) ->
   % PanelName       = get_panel_name(PanelMap),
   Position        = ep_job:flip_box(Job, PanelMap),
   Size            = maps:get(size, PanelMap),
   
   Radius          = maps:get(radius, PanelMap),
   Border          = maps:get(border, PanelMap),
   BorderStyle     = maps:get(border_style, PanelMap),
   BorderColor     = maps:get(border_color, PanelMap),
   BackgroundColor = maps:get(background_color, PanelMap),
   eg_pdf:set_line_width(PDF, Border),
   eg_pdf:set_dash(PDF, BorderStyle),
   eg_pdf:set_stroke_color(PDF, BorderColor),
   eg_pdf:set_fill_color(PDF, BackgroundColor),
   eg_pdf:round_rect(PDF, Position, Size, Radius),
   eg_pdf:path(PDF, fill_stroke),
   ok.


%% ***********************************************************
%% Get panel id 
%% ***********************************************************

%% @doc Get panel id

-spec get_id(PanelMap :: map()) -> tuple().

get_id(PanelMap) ->
   maps:get(id, PanelMap).


%% ***********************************************************
%% Get page number 
%% ***********************************************************

%% @doc Get page number

-spec get_page_number(PanelMap :: map()) -> tuple().

get_page_number(PanelMap) ->
   PageNumber = maps:get(id, PanelMap),
   element(1, PageNumber).


%% ***********************************************************
%% Get panel index 
%% ***********************************************************

%% @doc Get panel index

-spec get_panel_index(PanelMap :: map()) -> tuple().

get_panel_index(PanelMap) ->
   PanelIndex= maps:get(id, PanelMap),
   element(2, PanelIndex).


%% ***********************************************************
%% Get panel name 
%% ***********************************************************

%% @doc Get panel name  

-spec get_panel_name(PanelMap :: map()) -> tuple().

get_panel_name(PanelMap) ->
   PanelName= maps:get(id, PanelMap),
   element(3, PanelName).


%% ***********************************************************
%% Get panel name as string
%% ***********************************************************

%% @doc Get panel name as string

-spec get_name_string(PanelMap :: map()) -> string().

get_name_string(PanelMap) ->
   ID = ep_panel:get_id(PanelMap),
   List = tuple_to_list(ID),
   List1 = lists:join("-", List),
   lists:concat(List1).


%% ***********************************************************
%% Get position 
%% ***********************************************************

%% @doc Get position

-spec get_position(PanelMap :: map()) -> tuple().

get_position(PanelMap) ->
   maps:get(position, PanelMap).


%% ***********************************************************
%% Get text position 
%% ***********************************************************

%% @doc Get text position

-spec get_text_position(PanelMap :: map()) -> tuple().

get_text_position(PanelMap) ->
   {X, _Y} = maps:get(position, PanelMap),
   Cursor = maps:get(content_cursor, PanelMap),
   ep_lib:impose_text({X, Cursor}, letter).


%% ***********************************************************
%% Get size 
%% ***********************************************************

%% @doc Get size

-spec get_size(PanelMap :: map()) -> tuple().

get_size(PanelMap) ->
   maps:get(size, PanelMap).


%% ***********************************************************
%% Get radius 
%% ***********************************************************

%% @doc Get radius

-spec get_radius(PanelMap :: map()) -> tuple().

get_radius(PanelMap) ->
   maps:get(radius, PanelMap).


%% ***********************************************************
%% Get next line 
%% ***********************************************************

%% @doc Get next line

-spec get_content_cursor(PanelMap :: map()) -> integer().

get_content_cursor(PanelMap) ->
   maps:get(content_cursor, PanelMap).


%% ***********************************************************
%% get_available/1 - Return available space in pixels 
%% ***********************************************************

%% @doc Return available space in panel in pixels 

-spec get_available(PanelMap :: map()) -> integer().

get_available(PanelMap) ->
   {_X, Y} = maps:get(position, PanelMap),
   {_Width, Height} = maps:get(size, PanelMap),
   Cursor           = maps:get(content_cursor, PanelMap),
   (Y + Height) - Cursor.


%% ***********************************************************
%% consumed/2 - Return vertical space consumed by lines 
%% ***********************************************************


%% @doc Return vertical space in panel consumed by lines 

-spec consumed(Tag   :: atom(),
               Lines :: list()) -> integer().

consumed(Tag, Lines) ->
   Leading  = ep_report_sty:report_leading(Tag),
   Leading * length(Lines).


%% ***********************************************************
%% will_fit/3 - Returns true if lines fit in panel 
%% ***********************************************************


%% @doc Returns true if lines fit in panel

-spec will_fit(Tag      :: atom(),
               Lines    :: list(),
               PanelMap :: map()) -> boolean().

will_fit(Tag, Lines, PanelMap) ->
    Consumed = ep_panel:consumed(Tag, Lines),
    Available = ep_panel:get_available(PanelMap),
    Available >= Consumed.

   
%% ***********************************************************
%% get_avialable/3 - Return available lines in panel 
%% ***********************************************************

%% @doc Given tag, return available lines in panel 

-spec get_available_lines(TypeStyle  :: atom(),
                          Tag        :: atom(),
                          PanelMap   :: map()) -> integer().

get_available_lines(TypeStyle, Tag, PanelMap) ->
   Available = get_available(PanelMap),
   Leading   = ep_typespec:leading(TypeStyle, Tag),
   Available div Leading.


%% ***********************************************************
%% ***********************************************************
%% Get border 
%% ***********************************************************

%% @doc Get border

-spec get_border(PanelMap :: map()) -> tuple().

get_border(PanelMap) ->
   maps:get(border, PanelMap).


%% ***********************************************************
%% Get border style 
%% ***********************************************************

%% @doc Get border style

-spec get_border_style(PanelMap :: map()) -> tuple().

get_border_style(PanelMap) ->
   maps:get(border_style, PanelMap).


%% ***********************************************************
%% Get border color 
%% ***********************************************************

%% @doc Get border color

-spec get_border_color(PanelMap :: map()) -> tuple().

get_border_color(PanelMap) ->
   maps:get(border_color, PanelMap).


%% ***********************************************************
%% Get margin 
%% ***********************************************************

%% @doc Get margin

-spec get_margin(PanelMap :: map()) -> tuple().

get_margin(PanelMap) ->
   maps:get(margin, PanelMap).


%% ***********************************************************
%% Get measure 
%% ***********************************************************

%% @doc Get measure 

-spec get_measure(PanelMap :: map()) -> integer().

get_measure(PanelMap) ->
   {Width, _Height} =maps:get(size, PanelMap),
   Margin  = maps:get(margin, PanelMap),
   Width - (Margin * 2).


%% ***********************************************************
%% Get indent 
%% ***********************************************************

%% @doc Get indent 

-spec get_indent(PanelMap :: map()) -> integer().

get_indent(PanelMap) ->
   maps:get(indent, PanelMap).


%% ***********************************************************
%% Get rot 
%% ***********************************************************

%% @doc Get rot; text rotation angle  

-spec get_rot(PanelMap :: map()) -> integer().

get_rot(PanelMap) ->
   maps:get(rot, PanelMap).


%% ***********************************************************
%% Get jump prompt 
%% ***********************************************************

%% @doc Get jump prompt 

-spec get_jump_prompt(PanelMap :: map()) -> tuple().

get_jump_prompt(PanelMap) ->
   maps:get(jump_prompt, PanelMap).


%% ***********************************************************
%% Get typestyle 
%% ***********************************************************


%% @doc Get typestyle 

-spec get_typestyle(PanelMap :: map()) -> tuple().

get_typestyle(PanelMap) ->
   maps:get(typestyle, PanelMap).


%% ***********************************************************
%% Get li fill color 
%% ***********************************************************


%% @doc Get li fill color 

-spec get_li_fill(PanelMap :: map()) -> tuple().

get_li_fill(PanelMap) ->
   maps:get(li_fill, PanelMap).


%% ***********************************************************
%% Update id 
%% ***********************************************************

%% @doc Update panel id

-spec update_id(Id       :: tuple(),
                PanelMap :: map()) -> map().

update_id(ID, PanelMap) ->
   maps:put(id, ID, PanelMap).


%% ***********************************************************
%% Update 
%% ***********************************************************

%% @doc Update position

-spec update_position(Position       :: tuple(),
                      PanelMap :: map()) -> map().

update_position(Position, PanelMap) ->
   maps:put(id, Position, PanelMap).


%% ***********************************************************
%% Update size 
%% ***********************************************************

%% @doc Update size

-spec update_size(Size     :: tuple(),
                  PanelMap :: map()) -> map().

update_size(Size, PanelMap) ->
   maps:put(size, Size, PanelMap).


%% ***********************************************************
%% Update radius 
%% ***********************************************************

%% @doc Update radius

-spec update_radius(Radius   :: integer(),
                    PanelMap :: map()) -> map().

update_radius(Radius, PanelMap) ->
   maps:put(radius, Radius, PanelMap).


%% ***********************************************************
%% Update next line 
%% ***********************************************************

%% @doc Update next line 

-spec update_content_cursor(Pixels    :: integer(),
                            PanelMap  :: map()) -> map().

update_content_cursor(Pixels, PanelMap) ->
   Cursor  = get_content_cursor(PanelMap),
   Cursor1 = Cursor + Pixels,
   maps:put(content_cursor, Cursor1, PanelMap).


%% @doc Update content cursor by one line

-spec one_line_space(Tag       :: atom(),
                     TypeStyle :: atom(),
                     PanelMap  :: map()) -> map().

one_line_space(Tag, TypeStyle, PanelMap) ->
   Pixels = ep_typespec:leading(TypeStyle, Tag),
   update_content_cursor(Pixels, PanelMap).

%% ***********************************************************
%% Update border 
%% ***********************************************************

%% @doc Update border

-spec update_border(Border   :: tuple(),
                    PanelMap :: map()) -> map().

update_border(Border, PanelMap) ->
   maps:put(border, Border, PanelMap).


%% ***********************************************************
%% Update border style 
%% ***********************************************************

%% @doc Update border style

-spec update_border_style(BorderStyle  :: tuple(),
                          PanelMap     :: map()) -> map().

update_border_style(BorderStyle, PanelMap) ->
   maps:put(border_style, BorderStyle, PanelMap).


%% ***********************************************************
%% Update border color 
%% ***********************************************************

%% @doc Update border color

-spec update_border_color(BorderColor  :: tuple(),
                          PanelMap     :: map()) -> map().

update_border_color(BorderColor, PanelMap) ->
   maps:put(border_color, BorderColor, PanelMap).


%% ***********************************************************
%% Update background color 
%% ***********************************************************

%% @doc Update background color

-spec update_background_color(BackgroundColor  :: tuple(),
                              PanelMap         :: map()) -> map().

update_background_color(BackgroundColor, PanelMap) ->
   maps:put(background_color, BackgroundColor, PanelMap).


%% ***********************************************************
%% Update margin 
%% ***********************************************************

%% @doc Update margin

-spec update_margin(Margin   :: tuple(),
                    PanelMap :: map()) -> map().

update_margin(Margin, PanelMap) ->
   maps:put(margin, Margin, PanelMap).


%% ***********************************************************
%% Update rot 
%% ***********************************************************

%% @doc Update rot 

-spec update_rot(Rot   :: integer(),
                 PanelMap :: map()) -> map().

update_rot(Rot, PanelMap) ->
   maps:put(rot, Rot, PanelMap).


%% ***********************************************************
%% Update jump prompt 
%% ***********************************************************

%% @doc Update jump prompt 

-spec update_jump_prompt(JumpPrompt  :: tuple(),
                         PanelMap    :: map()) -> map().

update_jump_prompt(JumpPrompt, PanelMap) ->
   maps:put(jump_prompt, JumpPrompt, PanelMap).


%% ***********************************************************
%% Update typestyle 
%% ***********************************************************

%% @doc Update typestyle 

-spec update_typestyle(TypeStyle  :: atom(),
                       PanelMap   :: map()) -> map().

update_typestyle(TypeStyle, PanelMap) ->
   maps:put(typestyle, TypeStyle, PanelMap).


%% ***********************************************************
%% Update li fill color 
%% ***********************************************************

%% @doc Update li fill color 

-spec update_li_fill(LiFill    :: atom(),
                     PanelMap  :: map()) -> map().

update_li_fill(LiFill, PanelMap) ->
   maps:put(li_fill, LiFill, PanelMap).


%% ***********************************************************
%% Update panel 
%% ***********************************************************

%% @doc Update content cursor (NextLine) in panel

-spec update_panel(Tag      :: atom(),
                   Lines    :: list(),
                   PanelMap :: map()) -> map().


update_panel(br, _Lines, PanelMap) ->
   break(PanelMap);

update_panel(Tag, Lines, PanelMap) ->
   Consumed = consumed(Tag, Lines),
   ep_panel:update_content_cursor(Consumed, PanelMap).


%% @doc Advance content cursor (NextLine) in panel

-spec break(PanelMap :: map()) -> tuple().

break(PanelMap) ->
   TypeStyle  = ep_panel:get_typestyle(PanelMap),
   FontSize   = ep_typespec:fontsize(TypeStyle, p),
   ep_panel:update_content_cursor(FontSize, PanelMap).
   

%% ***********************************************************
%% reveal/1 -- make panelmap displayble 
%% ***********************************************************

%% @doc Make panelmap displayable

-spec reveal(PanelMap :: map()) -> map().

reveal(PanelMap) ->
    PanelMap1 = ep_panel:update_border(1, PanelMap),
    PanelMap2 = ep_panel:update_border_style(solid, PanelMap1),
    ep_panel:update_border_color(black, PanelMap2).




%% ***********************************************************
%% Default panel 
%% ***********************************************************

%% @doc Create default panel 

-spec default_panel() -> map().

default_panel() ->
   Id   = {1, 1, top},
   Position = {72, 72},
   Size     = {450, 300},
   create(Id, Position, Size).

