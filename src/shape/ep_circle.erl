%%==========================================================================
%%% ep_circle.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:      MIT
%%%   File:         ep_circle.erl
%%%   Description:  Shape primitives 
%%% @end

%%% ==========================================================================


-module(ep_circle).

-export([
    border_color/1,
    border_specs/1,
    border_style/1,
    center/1,
    circle/3,
    colors/1,
    create/2,
    fill_color/1,
    format/1,
    radius/1,
    update_border/2,
    update_border_color/2,
    update_border_style/2,
    update_center/3,
    update_fill_color/2,
    update_format/2,
    update_radius/2
]).

-include("ep.hrl").
-include("ep_erltypes.hrl").

-define(BORDER, 1).
-define(BORDER_COLOR, black).
-define(BORDER_STYLE, solid).
-define(FILL_COLOR, white).


%% @doc Create circle map
-spec create(Center :: xy(), Radius :: points()) -> ep_circle().
create(Center, Radius) ->
   #{ center        => Center
    , radius        => Radius 
    , border        => ?BORDER
    , border_style  => ?BORDER_STYLE
    , border_color  => ?BORDER_COLOR
    , fill_color    => ?FILL_COLOR
    }.


%% @doc Draw circle
-spec circle(pdf_server_pid(), ep_job(), ep_circle()) -> ok.
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


%% @doc Return center coordinates
-spec center(ep_circle()) -> xy().
center(#{center := C}) -> C.


%% @doc Return radius
-spec radius(ep_circle()) -> points().
radius(#{radius := R}) -> R.


%% @doc Return border
-spec border(ep_circle()) -> points().
border(#{border := B}) -> B.


%% @doc Return border style
-spec border_style(ep_circle()) -> line_style().
border_style(#{border_style := BS}) -> BS.


%% @doc Return border color
-spec border_color(ep_circle()) -> color().
border_color(#{border_color := BC}) -> BC.


%% @doc Return fill color
-spec fill_color(ep_circle()) -> color().
fill_color(#{fill_color := FC}) -> FC.


%% @doc Return page format
%% FIXME: the field does not exist in the constructor?
-spec format(ep_circle()) -> page_format().
format(#{format := F}) -> F.


%% @doc Return border specifications
-spec border_specs(ep_circle()) -> {points(), line_style(), color()}.
border_specs(CircleMap) ->
    Border = border(CircleMap),
    Style  = border_style(CircleMap),
    Color  = border_color(CircleMap),
    {Border, Style, Color}.


%% @doc Return border and fill colors 
-spec colors(ep_circle()) -> {color(), color()}.
colors(CircleMap) ->
    BorderColor  = border_color(CircleMap),
    FillColor    = fill_color(CircleMap),
    {BorderColor, FillColor}.


%% @doc Update center coordinates
-spec update_center(CenterX :: points(), CenterY :: points(), ep_circle())
                   -> ep_circle().
update_center(CenterX, CenterY, CircleMap) ->
    maps:put(center, {CenterX, CenterY}, CircleMap).


%% @doc Update radius 
-spec update_radius(Radius :: points(), ep_circle()) -> tuple().
update_radius(Radius, CircleMap) ->
    maps:put(radius, Radius, CircleMap).


%% @doc Update border 
-spec update_border(Border :: points(), ep_circle()) -> ep_circle().
update_border(Border, CircleMap) ->
    maps:put(border, Border, CircleMap).


%% @doc Update border style
-spec update_border_style(BorderStyle :: line_style(), ep_circle()) -> ep_circle().
update_border_style(BorderStyle, CircleMap) ->
    maps:put(border_style, BorderStyle, CircleMap).


%% @doc Update border color 
-spec update_border_color(BorderColor :: color(), ep_circle()) -> ep_circle().
update_border_color(BorderColor, CircleMap) ->
    maps:put(border_color, BorderColor, CircleMap).


%% @doc Update fill color
-spec update_fill_color(FillColor :: color(), ep_circle()) -> ep_circle().
update_fill_color(FillColor, CircleMap) ->
    maps:put(fill_color, FillColor, CircleMap).


%% @doc Update format 
-spec update_format(Format :: page_format(), ep_circle()) -> ep_circle().
update_format(Format, CircleMap) ->
    maps:put(format, Format, CircleMap).
