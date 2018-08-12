%%==========================================================================
%%% ep_paper_stock.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:      MIT
%%%   File:         ep_circle.erl
%%%   Description:  Shape primitives 
%%% @end

%%% ==========================================================================


-module (ep_circle).

-export ([create/2]). 
-export ([circle/3]).

-export ([center/1, radius/1, border_style/1, border_color/1]).
-export ([fill_color/1, format/1]). 
-export ([border_specs/1, colors/1]). 
-export ([update_center/3]).
-export ([update_radius/2, update_border/2, update_border_style/2]).
-export ([update_border_color/2, update_fill_color/2, update_format/2]).

%% -compile(export_all).

-include("../../include/ep.hrl").

-define(BORDER, 1).
-define(BORDER_COLOR, black).
-define(BORDER_STYLE, solid).
-define(FILL_COLOR, white).

%% Border style options: solid, dash, dot, dashdot
%% Color options: white, silver, gray, black, maroon, red, fuschia,
%%                purple, lime, green, olive, yellow, navy, blue, teal, aqua 
%% Format options: N> rp(ep_format:formats()). 


%% ***********************************************************
%% Create circle map 
%% ***********************************************************

%% @doc Create circle map

-spec create(Center :: integer(),
             Radius  :: integer()) -> map().

create(Center, Radius) ->
   #{ center        => Center
    , radius        => Radius 
    , border        => ?BORDER
    , border_style  => ?BORDER_STYLE
    , border_color  => ?BORDER_COLOR
    , fill_color    => ?FILL_COLOR
    }.


%% ***********************************************************
%% circle/3  
%% ***********************************************************

%% @doc Draw circle

-spec circle(PDF      :: identifier(),
             Job      :: map(),
             CircleMap :: map()) -> ok.

circle(PDF, Job, CircleMap) ->
    {PaperStock, PagePosition} = ep_job:stock_position(Job),
    Center     = maps:get(center, CircleMap),
    Center1    = ep_lib:impose_xy(Center, PagePosition, PaperStock),
    Radius     = maps:get(radius, CircleMap),
    {Border, BorderStyle, BorderColor} = border_specs(CircleMap),
    FillColor = fill_color(CircleMap),
    eg_pdf:save_state(PDF),
    eg_pdf:set_line_width(PDF, Border),
    eg_pdf:set_dash(PDF, BorderStyle),
    eg_pdf:set_stroke_color(PDF, BorderColor),
    eg_pdf:set_fill_color(PDF, FillColor),
    eg_pdf:circle(PDF, Center1, Radius),
    eg_pdf:path(PDF, fill_stroke),
    eg_pdf:restore_state(PDF),
    ok.



%% ***********************************************************
%% Get circle attributes 
%% ***********************************************************


%% @doc Return center coordinates 

% -spec center(CircleMap :: map()) -> tuple().

center(CircleMap) ->
   maps:get(center, CircleMap).


%% @doc Return radius 

-spec radius(CircleMap :: map()) -> tuple().

radius(CircleMap) ->
   maps:get(radius, CircleMap).


%% @doc Return border  

-spec border(CircleMap :: map()) -> tuple().

border(CircleMap) ->
   maps:get(border, CircleMap).


%% @doc Return border style 

-spec border_style(CircleMap :: map()) -> tuple().

border_style(CircleMap) ->
   maps:get(border_style, CircleMap).


%% @doc Return border color 

-spec border_color(CircleMap :: map()) -> tuple().

border_color(CircleMap) ->
   maps:get(border_color, CircleMap).


%% @doc Return fill color 

-spec fill_color(CircleMap :: map()) -> tuple().

fill_color(CircleMap) ->
   maps:get(fill_color, CircleMap).


%% @doc Return page format 

-spec format(CircleMap :: map()) -> tuple().

format(CircleMap) ->
   maps:get(format, CircleMap).


%% @doc Return border specifications 

-spec border_specs(CircleMap :: map()) -> tuple().

border_specs(CircleMap) ->
    Border = border(CircleMap),
    Style  = border_style(CircleMap),
    Color  = border_color(CircleMap),
    {Border, Style, Color}.


%% @doc Return border and fill colors 

-spec colors(CircleMap :: map()) -> tuple().

colors(CircleMap) ->
    BorderColor  = border_color(CircleMap),
    FillColor    = fill_color(CircleMap),
    {BorderColor, FillColor}.



%% ***********************************************************
%% Update circle parameters 
%% ***********************************************************

%% Border style: solid, dash, dot, dashdot
%% Colors: white, silver, gray, black, maroon, red, fuschia,
%%         purple, lime, green, olive, yellow, navy, blue, teal, aqua 


%% @doc Update center coordinates 

-spec update_center(CenterX   :: integer(),
                    CenterY   :: integer(),
                    CircleMap :: map()) -> tuple().

update_center(CenterX, CenterY, CircleMap) ->
    maps:put(center, {CenterX, CenterY}, CircleMap).


%% @doc Update radius 

-spec update_radius(Radius    :: integer(),
                    CircleMap :: map()) -> tuple().

update_radius(Radius, CircleMap) ->
    maps:put(radius, Radius, CircleMap).


%% @doc Update border 

-spec update_border(Border    :: integer(),
                    CircleMap :: map()) -> tuple().

update_border(Border, CircleMap) ->
    maps:put(border, Border, CircleMap).


%% @doc Update border style 

-spec update_border_style(BorderStyle :: integer(),
                          CircleMap   :: map()) -> tuple().

update_border_style(BorderStyle, CircleMap) ->
    maps:put(border_style, BorderStyle, CircleMap).


%% @doc Update border color 

-spec update_border_color(BorderColor :: integer(),
                          CircleMap   :: map()) -> tuple().

update_border_color(BorderColor, CircleMap) ->
    maps:put(border_color, BorderColor, CircleMap).

%% @doc Update fill color 

-spec update_fill_color(FillColor :: integer(),
                        CircleMap :: map()) -> tuple().

update_fill_color(FillColor, CircleMap) ->
    maps:put(fill_color, FillColor, CircleMap).


%% @doc Update format 

-spec update_format(Format    :: integer(),
                    CircleMap :: map()) -> tuple().

update_format(Format, CircleMap) ->
    maps:put(format, Format, CircleMap).



