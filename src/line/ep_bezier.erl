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

-export([from/1, control1/1, control2/1, to/1]).
-export([width/1, color/1, format/1]).
-export ([update_from/2, update_control1/2, update_control2/2, update_to/2]).
-export ([update_width/2, update_color/2, update_format/2]).
-export([features/1]).

-include("../../include/ep.hrl").

-define(DEFAULT_WIDTH, 1).
-define(DEFAULT_DASH, solid).
-define(DEFAULT_COLOR, black).
-define(DEFAULT_FORMAT, letter).


%% ***********************************************************
%% Create bezier map 
%% ***********************************************************

%% @doc Create bezier map

-spec create(Pt1 :: tuple(),
             Pt2 :: tuple(),
             Pt3 :: tuple(),
             Pt4 :: tuple()) -> map().

create(Pt1, Pt2, Pt3, Pt4) ->
   #{ from         => Pt1 
    , pt2          => Pt2 
    , pt3          => Pt3 
    , to           => Pt4   
    , width        => ?DEFAULT_WIDTH
    , color        => ?DEFAULT_COLOR
    }.


%% ***********************************************************
%% Bezier to pdf  
%% ***********************************************************

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

    


%% ***********************************************************
%% Get bezier attributes 
%% ***********************************************************


%% @doc Return start-of-bezier coordinates

-spec from(BezierMap :: map()) -> tuple().

from(BezierMap) ->
   maps:get(from, BezierMap).


%% @doc Return return control point 1 

-spec control1(BezierMap :: map()) -> tuple().

control1(BezierMap) ->
   maps:get(control1, BezierMap).


%% @doc Return return control point 2 

-spec control2(BezierMap :: map()) -> tuple().

control2(BezierMap) ->
   maps:get(control2, BezierMap).


%% @doc Return end-of-bezier coordinates

-spec to(BezierMap :: map()) -> tuple().

to(BezierMap) ->
   maps:get(to, BezierMap).


%% @doc Return width of bezier 

-spec width(BezierMap :: map()) -> integer().

width(BezierMap) ->
   maps:get(width, BezierMap).


%% @doc Return color of bezier 
%%      Colors: white, silver, gray, black, maroon, red, fuschia,
%%      purple, lime, green, olive, yellow, navy, blue, teal, aqua 

-spec color(BezierMap :: map()) -> integer().

color(BezierMap) ->
   maps:get(color, BezierMap).


%% @doc Return page format 

-spec format(BezierMap :: map()) -> integer().

format(BezierMap) ->
   maps:get(format, BezierMap).


%% @doc Return style of bezier; e.g. width, dash, color 

-spec features(BezierMap :: map()) -> integer().

features(BezierMap) ->
    Width  = width(BezierMap),
    Color  = color(BezierMap),
    {Width, Color}.


%% ***********************************************************
%% Update bezier attributes 
%% ***********************************************************

%% doc Update beinning-of-line coordinates

-spec update_from(From   :: tuple(),
                  BezierMap :: map()) -> map().

update_from(From, BezierMap) ->
    maps:put(from, From, BezierMap).


%% doc Update control point 1 

-spec update_control1(Control1   :: tuple(),
                      BezierMap :: map()) -> map().

update_control1(Control1, BezierMap) ->
    maps:put(control1, Control1, BezierMap).


%% doc Update control point d 

-spec update_control2(Control2   :: tuple(),
                      BezierMap :: map()) -> map().

update_control2(Control2, BezierMap) ->
    maps:put(control1, Control2, BezierMap).


%% doc Update end-of-bezier coordinates

-spec update_to(To   :: tuple(),
                BezierMap :: map()) -> map().

update_to(To, BezierMap) ->
    maps:put(to, To, BezierMap).


%% @doc Update width of bezier 

-spec update_width(Width   :: integer(),
                   BezierMap :: map()) -> map().

update_width(Width, BezierMap) ->
    maps:put(width, Width, BezierMap).


%% @doc Update color of line: e.g. white, silver, gray, 
%%      black, maroon, red, fuschia, purple, lime, green, 
%%      olive, yellow, navy, blue, teal, aqua 

-spec update_color(Color   :: atom(),
                   BezierMap :: map()) -> map().

update_color(Color, BezierMap) ->
    maps:put(color, Color, BezierMap).


%% @doc Update page format 
%%      SEE:  rp(ep_format:formats(). 

-spec update_format(Format  :: atom(),
                    BezierMap :: map()) -> map().

update_format(Format, BezierMap) ->
    maps:put(format, Format, BezierMap).


