%%% ==========================================================================
%%% ep_line.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:      MIT
%%%   File:         ep_line.erl
%%%   Description:  Display a line 
%%% @end

%%% ==========================================================================


-module (ep_line).

-export([
    color/1,
    coordinates/1,
    create/2,
    dash/1,
    features/1,
    format/1,
    from/1,
    line/3,
    to/1,
    update_color/2,
    update_dash/2,
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


%% @doc Create line map
-spec create(From :: xy(), To :: xy()) -> ep_line().
create(From, To) ->
   #{ from         => From
    , to           => To
    , width        => ?DEFAULT_WIDTH
    , dash         => ?DEFAULT_DASH
    , color        => ?DEFAULT_COLOR
    }.


%% @doc Draw line to PDF
-spec line(PDF :: pdf_server_pid(), ep_job(), ep_line()) -> ok.
line(PDF, Job, LineMap) ->
    {PaperStock, PagePosition} = ep_job:stock_position(Job),
    From            = maps:get(from, LineMap),
    To              = maps:get(to,   LineMap),
    Width           = maps:get(width, LineMap),
    Dash            = maps:get(dash, LineMap),
    Color           = maps:get(color, LineMap),
    From1           = ep_lib:impose_xy(From, PagePosition, PaperStock),
    To1             = ep_lib:impose_xy(To, PagePosition,  PaperStock),
    eg_pdf:save_state(PDF),
    eg_pdf:move_to(PDF, From1),
    eg_pdf:set_stroke_color(PDF, Color),
    eg_pdf:set_dash(PDF, Dash),
    eg_pdf:set_line_width(PDF, Width),
    eg_pdf:line(PDF, From1, To1),
    eg_pdf:restore_state(PDF),
    ok.
    

%% @doc Return start-of-line coordinates
-spec from(ep_line()) -> xy().
from(#{from := F}) -> F.


%% @doc Return end-of-line coordinates
-spec to(ep_line()) -> xy().
to(#{to := T}) -> T.


%% @doc Return width of line
-spec width(ep_line()) -> points().
width(#{width := W}) -> W.


%% @doc Return style of line; e.g. solid, dash, dot, dashdot 
-spec dash(ep_line()) -> atom().
dash(#{dash := D}) -> D.


%% @doc Return color of line 
%%      Colors: white, silver, gray, black, maroon, red, fuschia,
%%      purple, lime, green, olive, yellow, navy, blue, teal, aqua 
-spec color(ep_line()) -> color().
color(#{color := C}) -> C.


%% @doc Return page format 
-spec format(ep_line()) -> page_format().
format(#{format := F}) -> F.


%% @doc Return beginning and end coordinates of line 
-spec coordinates(ep_line()) -> {xy(), xy()}.
coordinates(LineMap) ->
    From = from(LineMap),
    To   = to(LineMap),
    {From, To}.


%% @doc Return style of line; e.g. width, dash, color 
-spec features(ep_line()) -> {points(), atom(), color()}.
features(Line) ->
    Width = width(Line),
    Dash  = dash(Line),
    Color = color(Line),
    {Width, Dash, Color}.


%% doc Update beginning-of-line coordinates
-spec update_from(From :: xy(), ep_line()) -> ep_line().
update_from(From, LineMap) ->
    maps:put(from, From, LineMap).


%% doc Update end-of-line coordinates
-spec update_to(To :: xy(), ep_line()) -> ep_line().
update_to(To, LineMap) ->
    maps:put(to, To, LineMap).


%% @doc Update width of line
-spec update_width(Width :: points(), ep_line()) -> ep_line().
update_width(Width, LineMap) ->
    maps:put(width, Width, LineMap).


%% @doc Update style of line: e.g. solid, dash, dot, dashdot
-spec update_dash(Dash :: atom(), ep_line()) -> ep_line().
update_dash(Dash, LineMap) ->
    maps:put(type, Dash, LineMap).


%% @doc Update color of line: e.g. white, silver, gray, 
%%      black, maroon, red, fuschia, purple, lime, green, 
%%      olive, yellow, navy, blue, teal, aqua 
-spec update_color(color(), ep_line()) -> ep_line().
update_color(Color, LineMap) ->
    maps:put(color, Color, LineMap).


%% @doc Update page format 
%%      SEE:  rp(ep_format:formats().
-spec update_format(Format  :: page_format(), ep_line()) -> ep_line().
update_format(Format, LineMap) ->
    maps:put(format, Format, LineMap).
