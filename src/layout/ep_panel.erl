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

-export([
    break/1,
    consumed/2,
    create/3,
    default_panel/0,
    get_available/1,
    get_available_lines/3,
    get_border/1,
    get_border_color/1,
    get_border_style/1,
    get_content_cursor/1,
    get_id/1,
    get_indent/1,
    get_jump_prompt/1,
    get_li_fill/1,
    get_margin/1,
    get_measure/1,
    get_name_string/1,
    get_page_number/1,
    get_panel_index/1, get_panel_name/1,
    get_position/1,
    get_radius/1,
    get_rot/1,
    get_size/1,
    get_text_position/1,
    get_typestyle/1,
    one_line_space/3,
    panel/3,
    reveal/1,
    update_background_color/2,
    update_border/2, update_border_color/2, update_border_style/2,
    update_content_cursor/2,
    update_id/2,
    update_jump_prompt/2,
    update_li_fill/2,
    update_margin/2,
    update_panel/3,
    update_position/2,
    update_radius/2,
    update_rot/2,
    update_size/2,
    update_typestyle/2,
    will_fit/3
]).


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


-include("ep.hrl").
-include("ep_erltypes.hrl").

%% ***********************************************************
%% Create panel map 
%% ***********************************************************

%% @doc Create panel map
-spec create(ID :: ep_panel_id(), Position :: xy(), Size :: xy()) -> ep_panel().
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


%% @private
-spec init_content_cursor({number(), Y}) -> Y when Y :: number().
init_content_cursor(Position) ->
   {_X, Y} = Position,
   Y.


%% ***********************************************************
%% Display panel 
%% ***********************************************************

-spec panel(pdf_server_pid(), ep_job(), ep_panel()) -> ok.
panel(PDF, Job, PanelMap) ->
    %% PanelName = get_panel_name(PanelMap),
    Position = ep_job:flip_box(Job, PanelMap),
    Size     = maps:get(size, PanelMap),

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


%% @doc Get panel id
-spec get_id(ep_panel()) -> ep_panel_id().
get_id(#{id := I}) -> I.


%% @doc Get page number
-spec get_page_number(ep_panel()) -> integer().
get_page_number(#{id := {PgNum, _PanelIndex, _PanelName}}) -> PgNum.


%% @doc Get panel index
-spec get_panel_index(ep_panel()) -> integer().
get_panel_index(#{id := {_PgNum, PanelIndex, _PanelName}}) -> PanelIndex.


%% @doc Get panel name
-spec get_panel_name(ep_panel()) -> string().
get_panel_name(#{id := {_PgNum, _PanelIndex, PanelName}}) -> PanelName.


%% @doc Get panel name as string: PageNumber-PanelIndex-"PanelName"
-spec get_name_string(ep_panel()) -> string().
get_name_string(#{id := {PgNum, PanelIndex, PanelName}}) ->
    lists:flatten([erlang:integer_to_list(PgNum), "-",
                   erlang:integer_to_list(PanelIndex), "-",
                   PanelName]).


%% @doc Get position
-spec get_position(ep_panel()) -> xy().
get_position(#{position := P}) -> P.


%% @doc Get text position
-spec get_text_position(ep_panel()) -> xy().
get_text_position(#{position := {X, _},
                    content_cursor := Cursor}) ->
    ep_lib:impose_text({X, Cursor}, letter).


%% @doc Get size
-spec get_size(ep_panel()) -> xy().
get_size(#{size := S}) -> S.


%% @doc Get radius
-spec get_radius(ep_panel()) -> number().
get_radius(#{radius := R}) -> R.


%% @doc Get next line
-spec get_content_cursor(ep_panel()) -> points().
get_content_cursor(#{content_cursor := CC}) -> CC.


%% @doc Return available space in panel in pixels (remaining to the bottom).
-spec get_available(ep_panel()) -> integer().
get_available(#{position := {_X, Y},
                size := {_W, Height},
                content_cursor := Cursor}) ->
    (Y + Height) - Cursor.


%% @doc Return vertical space in panel consumed by lines
-spec consumed(Tag :: atom(), Lines :: list()) -> integer().
consumed(Tag, Lines) ->
    Leading = ep_report_sty:report_leading(Tag),
    Leading * length(Lines).


%% @doc Returns true if lines fit in panel
-spec will_fit(Tag :: atom(), Lines :: list(), ep_panel()) -> boolean().
will_fit(Tag, Lines, PanelMap) ->
    Consumed = ep_panel:consumed(Tag, Lines),
    Available = ep_panel:get_available(PanelMap),
    Available >= Consumed.


%% @doc Given tag, return available lines in panel 
-spec get_available_lines(TypeStyle :: atom(), Tag :: atom(), ep_panel())
                         -> integer().
get_available_lines(TypeStyle, Tag, PanelMap) ->
    Available = get_available(PanelMap),
    Leading = ep_typespec:leading(TypeStyle, Tag),
    Available div Leading.


%% @doc Get border
-spec get_border(ep_panel()) -> any().
get_border(#{border := B}) -> B.


%% @doc Get border style
-spec get_border_style(ep_panel()) -> atom().
get_border_style(#{border_style := BS}) -> BS.


%% @doc Get border color
-spec get_border_color(ep_panel()) -> color().
get_border_color(#{border_color := BC}) -> BC.


%% @doc Get margin
-spec get_margin(ep_panel()) -> points().
get_margin(#{margin := M}) -> M.


%% @doc Get measure 
-spec get_measure(ep_panel()) -> integer().
get_measure(#{size := {Width, _H},
              margin := Margin}) ->
    Width - (Margin * 2).


%% @doc Get indent
-spec get_indent(ep_panel()) -> points().
get_indent(#{indent := I}) -> I.


%% @doc Get rot; text rotation angle
-spec get_rot(ep_panel()) -> number().
get_rot(#{rot := R}) -> R.


%% @doc Get jump prompt
-spec get_jump_prompt(ep_panel()) -> string().
get_jump_prompt(#{jump_prompt := JP}) -> JP.


%% @doc Get typestyle 
-spec get_typestyle(ep_panel()) -> atom().
get_typestyle(#{typestyle := TS}) -> TS.


%% @doc Get li fill color 
-spec get_li_fill(ep_panel()) -> color().
get_li_fill(#{li_fill := LF}) -> LF.


%% @doc Update panel id
-spec update_id(Id :: ep_panel_id(), ep_panel()) -> ep_panel().
update_id(ID, PanelMap) ->
    maps:put(id, ID, PanelMap).


%% @doc Update position
-spec update_position(Position :: xy(), ep_panel()) -> ep_panel().
update_position(Position, PanelMap) ->
    maps:put(id, Position, PanelMap).


%% @doc Update size
-spec update_size(Size :: xy(), ep_panel()) -> ep_panel().
update_size(Size, PanelMap) ->
    maps:put(size, Size, PanelMap).


%% @doc Update radius
-spec update_radius(Radius :: points(), ep_panel()) -> ep_panel().
update_radius(Radius, PanelMap) ->
    maps:put(radius, Radius, PanelMap).


%% @doc Update next line
-spec update_content_cursor(points(), ep_panel()) -> ep_panel().
update_content_cursor(Pixels, PanelMap) ->
    Cursor = get_content_cursor(PanelMap),
    Cursor1 = Cursor + Pixels,
    maps:put(content_cursor, Cursor1, PanelMap).


%% @doc Update content cursor by one line
-spec one_line_space(Tag :: atom(), TypeStyle :: atom(), ep_panel()) -> ep_panel().
one_line_space(Tag, TypeStyle, PanelMap) ->
    Pixels = ep_typespec:leading(TypeStyle, Tag),
    update_content_cursor(Pixels, PanelMap).


%% @doc Update border
-spec update_border(Border :: number(), ep_panel()) -> ep_panel().
update_border(Border, PanelMap) ->
    maps:put(border, Border, PanelMap).


%% @doc Update border style
-spec update_border_style(BorderStyle :: atom(), ep_panel()) -> ep_panel().
update_border_style(BorderStyle, PanelMap) ->
    maps:put(border_style, BorderStyle, PanelMap).


%% @doc Update border color
-spec update_border_color(color(), ep_panel()) -> ep_panel().
update_border_color(BorderColor, PanelMap) ->
    maps:put(border_color, BorderColor, PanelMap).


%% @doc Update background color
-spec update_background_color(color(), ep_panel()) -> ep_panel().
update_background_color(BackgroundColor, PanelMap) ->
    maps:put(background_color, BackgroundColor, PanelMap).


%% @doc Update margin
-spec update_margin(Margin :: points(), ep_panel()) -> ep_panel().
update_margin(Margin, PanelMap) ->
    maps:put(margin, Margin, PanelMap).


%% @doc Update rot
-spec update_rot(Rot :: number(), ep_panel()) -> ep_panel().
update_rot(Rot, PanelMap) ->
    maps:put(rot, Rot, PanelMap).


%% @doc Update jump prompt
-spec update_jump_prompt(JumpPrompt :: string(), ep_panel()) -> ep_panel().
update_jump_prompt(JumpPrompt, PanelMap) ->
    maps:put(jump_prompt, JumpPrompt, PanelMap).


%% @doc Update typestyle
-spec update_typestyle(TypeStyle :: atom(), ep_panel()) -> ep_panel().
update_typestyle(TypeStyle, PanelMap) ->
    maps:put(typestyle, TypeStyle, PanelMap).


%% @doc Update li fill color
-spec update_li_fill(LiFill :: atom(), PanelMap :: ep_panel()) -> ep_panel().
update_li_fill(LiFill, PanelMap) ->
    maps:put(li_fill, LiFill, PanelMap).


%% @doc Update content cursor (NextLine) in panel
-spec update_panel(Tag :: atom(), Lines :: list(), ep_panel()) -> ep_panel().
update_panel(br, _Lines, PanelMap) ->
    break(PanelMap);

update_panel(Tag, Lines, PanelMap) ->
    Consumed = consumed(Tag, Lines),
    ep_panel:update_content_cursor(Consumed, PanelMap).


%% @doc Advance content cursor (NextLine) in panel
-spec break(ep_panel()) -> ep_panel().
break(PanelMap) ->
    TypeStyle = ep_panel:get_typestyle(PanelMap),
    FontSize = ep_typespec:fontsize(TypeStyle, p),
    ep_panel:update_content_cursor(FontSize, PanelMap).
   

%% @doc Make panelmap displayable
-spec reveal(ep_panel()) -> ep_panel().
reveal(PanelMap) ->
    PanelMap1 = ep_panel:update_border(1, PanelMap),
    PanelMap2 = ep_panel:update_border_style(solid, PanelMap1),
    ep_panel:update_border_color(black, PanelMap2).


%% @doc Create default panel
-spec default_panel() -> ep_panel().
default_panel() ->
    Id = {1, 1, top},
    Position = {72, 72},
    Size = {450, 300},
    create(Id, Position, Size).
