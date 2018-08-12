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

-export ([create/2]). 
-export([line/3]).

-export([from/1, to/1, width/1, dash/1, color/1, format/1]).
-export ([coordinates/1, features/1]). 
-export ([update_from/2, update_to/2]).
-export ([update_width/2, update_dash/2, update_color/2, update_format/2]).

-include("../../include/ep.hrl").

-define(DEFAULT_WIDTH, 1).
-define(DEFAULT_DASH, solid).
-define(DEFAULT_COLOR, black).
-define(DEFAULT_FORMAT, letter).


%% ***********************************************************
%% Create line map 
%% ***********************************************************

%% @doc Create line map

-spec create(From :: tuple(),
             To   :: tuple()) -> map().

create(From, To) ->
   #{ from         => From
    , to           => To
    , width        => ?DEFAULT_WIDTH
    , dash         => ?DEFAULT_DASH
    , color        => ?DEFAULT_COLOR
    }.

%% ***********************************************************
%% Line to pdf  
%% ***********************************************************

%% @doc Draw line

-spec line(PDF     :: identifier(),
           Job     :: map(),
           LineMap :: map()) -> ok.

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
    
%% ***********************************************************
%% Get line attributes 
%% ***********************************************************


%% @doc Return start-of-line coordinates

-spec from(LineMap :: map()) -> tuple().

from(LineMap) ->
   maps:get(from, LineMap).


%% @doc Return end-of-line coordinates

-spec to(LineMap :: map()) -> tuple().

to(LineMap) ->
   maps:get(to, LineMap).


%% @doc Return width of line 

-spec width(LineMap :: map()) -> integer().

width(LineMap) ->
   maps:get(width, LineMap).


%% @doc Return style of line; e.g. solid, dash, dot, dashdot 

-spec dash(LineMap :: map()) -> integer().

dash(LineMap) ->
   maps:get(dash, LineMap).


%% @doc Return color of line 
%%      Colors: white, silver, gray, black, maroon, red, fuschia,
%%      purple, lime, green, olive, yellow, navy, blue, teal, aqua 

-spec color(LineMap :: map()) -> integer().

color(LineMap) ->
   maps:get(color, LineMap).


%% @doc Return page format 

-spec format(LineMap :: map()) -> integer().

format(LineMap) ->
   maps:get(format, LineMap).


%% @doc Return beginning and end coordinates of line 

-spec coordinates(LineMap :: map()) -> integer().


coordinates(LineMap) ->
    From    = from(LineMap),
    To      = to(LineMap),
    {From, To}.


%% @doc Return style of line; e.g. width, dash, color 

-spec features(LineMap :: map()) -> integer().

features(Line) ->
    Width  = width(Line),
    Dash   = dash(Line),
    Color  = color(Line),
    {Width, Dash, Color}.


%% ***********************************************************
%% Update line attributes 
%% ***********************************************************

%% doc Update beinning-of-line coordinates

-spec update_from(From    :: tuple(),
                  LineMap :: map()) -> map().

update_from(From, LineMap) ->
    maps:put(from, From, LineMap).


%% doc Update end-of-line coordinates

-spec update_to(To       :: tuple(),
                LineMap  :: map()) -> map().

update_to(To, LineMap) ->
    maps:put(to, To, LineMap).


%% @doc Update width of line

-spec update_width(Width   :: integer(),
                   LineMap :: map()) -> map().

update_width(Width, LineMap) ->
    maps:put(width, Width, LineMap).


%% @doc Update style of line: e.g. solid, dash, dot, dashdot

-spec update_dash(Dash    :: atom(),
                  LineMap :: map()) -> map().

update_dash(Dash, LineMap) ->
    maps:put(type, Dash, LineMap).


%% @doc Update color of line: e.g. white, silver, gray, 
%%      black, maroon, red, fuschia, purple, lime, green, 
%%      olive, yellow, navy, blue, teal, aqua 

-spec update_color(Color   :: atom(),
                   LineMap :: map()) -> map().

update_color(Color, LineMap) ->
    maps:put(color, Color, LineMap).


%% @doc Update page format 
%%      SEE:  rp(ep_format:formats(). 

-spec update_format(Format  :: atom(),
                    LineMap :: map()) -> map().

update_format(Format, LineMap) ->
    maps:put(format, Format, LineMap).


    

