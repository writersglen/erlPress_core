%%% ==========================================================================

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:      MIT
%%%   File:         ep_bezier.erl
%%%   Description:  Display bezier curves 
%%% @end

%%% ==========================================================================


-module (ep_bezier).

-export ([create/4]). 
-export([bezier/3]).

-export([
    color/1,
    control1/1,
    control2/1,
    features/1,
    format/1,
    from/1,
    to/1,
    update_color/2,
    update_control1/2,
    update_control2/2,
    update_format/2,
    update_from/2,
    update_to/2,
    update_width/2,
    width/1
]). % a..z

-include("ep.hrl").
-include("ep_erltypes.hrl").

-define(DEFAULT_WIDTH, 1).
-define(DEFAULT_DASH, solid).
-define(DEFAULT_COLOR, black).
-define(DEFAULT_FORMAT, letter).


%% @doc Create bezier map
-spec create(Pt1 :: xy(), Pt2 :: xy(), Pt3 :: xy(), Pt4 :: xy()) -> ep_bezier().
create(Pt1, Pt2, Pt3, Pt4) ->
   #{ from         => Pt1 
    , pt2          => Pt2 
    , pt3          => Pt3 
    , to           => Pt4   
    , width        => ?DEFAULT_WIDTH
    , color        => ?DEFAULT_COLOR
    }.


%% @doc Bezier to pdf
-spec bezier(pdf_server_pid(), ep_job(), ep_bezier()) -> pdf_server_pid().
bezier(PDF, Job, BezierMap) ->
    PaperStock      = maps:get(paper_stock, Job),
    PagePositions = ep_job:page_positions(Job, 1),
    [Position | _] = PagePositions,
    From            = maps:get(from, BezierMap),
    Pt2             = maps:get(pt2, BezierMap),
    Pt3             = maps:get(pt3, BezierMap),
    To              = maps:get(to,   BezierMap),
    Width           = maps:get(width, BezierMap),
    Color           = maps:get(color, BezierMap),
    FromA           = ep_lib:impose_xy(From, Position, PaperStock),
    Pt2A            = ep_lib:impose_xy(Pt2, Position, PaperStock),
    Pt3A            = ep_lib:impose_xy(Pt3, Position, PaperStock),
    ToA             = ep_lib:impose_xy(To, Position, PaperStock),
    eg_pdf:save_state(PDF),
    eg_pdf:move_to(PDF, From),
    eg_pdf:set_line_width(PDF, Width),
    eg_pdf:bezier(PDF, FromA, Pt2A, Pt3A, ToA),
    eg_pdf:set_stroke_color(PDF, Color),
    eg_pdf:path(PDF, stroke),
    eg_pdf:restore_state(PDF),
    PDF.


%% @doc Return start-of-bezier coordinates
-spec from(ep_bezier()) -> xy().
from(#{from := F}) -> F.


%% @doc Return return control point 1 
-spec control1(ep_bezier()) -> xy().
control1(#{control1 := C1}) -> C1.


%% @doc Return return control point 2 
-spec control2(ep_bezier()) -> xy().
control2(#{control2 := C2}) -> C2.


%% @doc Return end-of-bezier coordinates
-spec to(ep_bezier()) -> xy().
to(#{to := T}) -> T.


%% @doc Return width of bezier 
-spec width(ep_bezier()) -> points().
width(#{width := W}) -> W.


%% @doc Return color of bezier 
%%      Colors: white, silver, gray, black, maroon, red, fuschia,
%%      purple, lime, green, olive, yellow, navy, blue, teal, aqua 
-spec color(ep_bezier()) -> color().
color(#{color := C}) -> C.


%% @doc Return page format 
-spec format(ep_bezier()) -> page_format().
format(#{format := F}) -> F.


%% @doc Return style of bezier; e.g. width, dash, color 
-spec features(ep_bezier()) -> {points(), color()}.
features(BezierMap) ->
    Width  = width(BezierMap),
    Color  = color(BezierMap),
    {Width, Color}.


%% doc Update beinning-of-line coordinates
-spec update_from(From :: xy(), ep_bezier()) -> ep_bezier().
update_from(From, BezierMap) ->
    maps:put(from, From, BezierMap).


%% doc Update control point 1 
-spec update_control1(Control1 :: xy(), ep_bezier()) -> ep_bezier().
update_control1(Control1, BezierMap) ->
    maps:put(control1, Control1, BezierMap).


%% doc Update control point 2
-spec update_control2(Control2 :: xy(), ep_bezier()) -> ep_bezier().
update_control2(Control2, BezierMap) ->
    maps:put(control1, Control2, BezierMap).


%% doc Update end-of-bezier coordinates
-spec update_to(To :: xy(), ep_bezier()) -> ep_bezier().
update_to(To, BezierMap) ->
    maps:put(to, To, BezierMap).


%% @doc Update width of bezier
-spec update_width(Width :: points(), ep_bezier()) -> ep_bezier().
update_width(Width, BezierMap) ->
    maps:put(width, Width, BezierMap).


%% @doc Update color of line: e.g. white, silver, gray, 
%%      black, maroon, red, fuschia, purple, lime, green, 
%%      olive, yellow, navy, blue, teal, aqua 
-spec update_color(color(), ep_bezier()) -> ep_bezier().
update_color(Color, BezierMap) ->
    maps:put(color, Color, BezierMap).


%% @doc Update page format 
%%      SEE:  rp(ep_format:formats().
-spec update_format(Format :: page_format(), ep_bezier()) -> ep_bezier().
update_format(Format, BezierMap) ->
    maps:put(format, Format, BezierMap).
