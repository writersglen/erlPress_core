%%%==========================================================================
%%% ep_ellipse.erl
%%%
%%% @copyright  2017 Lloyd R. Prentice
%%% @author     Lloyd R. Prentice
%%% @version   .01
%%% @doc
%@%      License:      MIT
%%%      File:         ep_ellipse.erl
%%%       Description: Layout primitives 
%%% @end
%%%==========================================================================


-module (ep_ellipse).

-export ([
    axes/1,
    border_color/1,
    border_specs/1,
    border_style/1,
    center/1,
    colors/1,
    create/2,
    ellipse/3,
    ellipse_specs/1,
    fill_color/1,
    format/1,
    update_border/2,
    update_border_color/2,
    update_border_style/2,
    update_center/3,
    update_dimensions/3,
    update_fill_color/2,
    update_format/2
]). % a..z


-include("ep.hrl").
-include("ep_erltypes.hrl").

-define(DEFAULT_BORDER, 1).
-define(DEFAULT_BORDER_COLOR, black).
-define(DEFAULT_BORDER_STYLE, solid).
-define(DEFAULT_FILL_COLOR, white).
-define(DEFAULT_FORMAT, letter).


%% @doc Create ellipse map
-spec create(Center :: xy(), Axes :: xy1_xy2()) -> ep_ellipse().

create(Center, Axes) ->
   #{ center         => Center
    , axes           => Axes 
    , border         => ?DEFAULT_BORDER
    , border_style   => ?DEFAULT_BORDER_STYLE
    , border_color   => ?DEFAULT_BORDER_COLOR 
    , fill_color     => ?DEFAULT_FILL_COLOR 
    }.


-spec ellipse(pdf_server_pid(), ep_job(), ep_ellipse()) -> ok.
ellipse(PDF, Job, EllipseMap) ->
    {PaperStock, PagePosition} = ep_job:stock_position(Job),
    Center  = maps:get(center, EllipseMap),
    Center1 = ep_lib:impose_xy(Center,
                               PagePosition,
                               PaperStock),
    Axes        = maps:get(axes, EllipseMap),
    Border      = maps:get(border, EllipseMap),
    BorderColor = maps:get(border_color, EllipseMap),
    BorderStyle = maps:get(border_style, EllipseMap),
    FillColor   = maps:get(fill_color, EllipseMap),
    eg_pdf:save_state(PDF),
    eg_pdf:set_line_width(PDF, Border),
    eg_pdf:set_dash(PDF, BorderStyle),
    eg_pdf:set_stroke_color(PDF, BorderColor),
    eg_pdf:set_fill_color(PDF, FillColor),
    eg_pdf:ellipse(PDF, Center1, Axes),
    eg_pdf:path(PDF, fill_stroke),
    eg_pdf:restore_state(PDF),
    ok.


%% @doc Return center coordinates
-spec center(ep_ellipse()) -> xy().
center(#{center := C}) -> C.


%% @doc Return axes 
-spec axes(ep_ellipse()) -> xy1_xy2().
axes(#{axes := A}) -> A.


%% @doc Return border  
-spec border(ep_ellipse()) -> points().
border(#{border := B}) -> B.


%% @doc Return border style 
-spec border_style(ep_ellipse()) -> line_style().
border_style(#{border_style := BS}) -> BS.


%% @doc Return border color
-spec border_color(ep_ellipse()) -> color().
border_color(#{border_color := BC}) -> BC.


%% @doc Return fill color 
-spec fill_color(ep_ellipse()) -> color().
fill_color(#{fill_color := FC}) -> FC.


%% @docc Return page format 
-spec format(ep_ellipse()) -> page_format().
format(#{format := F}) -> F.


%% @doc Return ellipse specifications 
-spec ellipse_specs(ep_ellipse()) -> {number(), number(), xy(), xy()}.
ellipse_specs(EllipseMap) ->
    {CenterX, CenterY} = center(EllipseMap),
    {XAxis, YAxis} = axes(EllipseMap),
    {CenterX, CenterY, XAxis, YAxis}.


%% @doc Return border specifications 
-spec border_specs(ep_ellipse()) -> {points(), line_style(), color()}.
border_specs(EllipseMap) ->
    Border = border(EllipseMap),
    Style  = border_style(EllipseMap),
    Color  = border_color(EllipseMap),
    {Border, Style, Color}.


%% @doc Return border and fill colors 
-spec colors(ep_ellipse()) -> tuple().
colors(EllipseMap) ->
    BorderColor  = border_color(EllipseMap),
    FillColor    = fill_color(EllipseMap),
    {BorderColor, FillColor}.


%% @doc Update center coordinates 
-spec update_center(CenterX :: number(), CenterY :: number(), ep_ellipse())
                   -> ep_ellipse().
update_center(CenterX, CenterY, EllipseMap) ->
    maps:put(center, {CenterX, CenterY}, EllipseMap).


%% @doc Update dimensions 
-spec update_dimensions(XAxis :: xy(), YAxis :: xy(), ep_ellipse()) -> ep_ellipse().
update_dimensions(XAxis, YAxis, EllipseMap) ->
    maps:put(axes, {XAxis, YAxis}, EllipseMap).


%% @doc Update border 
-spec update_border(points(), ep_ellipse()) -> tuple().
update_border(Border, EllipseMap) ->
    maps:put(border, Border, EllipseMap).


%% @doc Update ellipse style 
-spec update_border_style(BorderStyle :: line_style(), ep_ellipse()) -> ep_ellipse().
update_border_style(BorderStyle, EllipseMap) ->
    maps:put(border_style, BorderStyle, EllipseMap).


%% @doc Update border color 
-spec update_border_color(color(), ep_ellipse()) -> ep_ellipse().
update_border_color(BorderColor, EllipseMap) ->
    maps:put(border_color, BorderColor, EllipseMap).


%% @doc Update fill color
-spec update_fill_color(color(), ep_ellipse()) -> ep_ellipse().
update_fill_color(FillColor, EllipseMap) ->
    maps:put(fill_color, FillColor, EllipseMap).


%% @doc Update format 
-spec update_format(Format :: integer(), ep_ellipse()) -> ep_ellipse().
update_format(Format, EllipseMap) ->
    maps:put(format, Format, EllipseMap).
