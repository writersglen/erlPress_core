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

-export ([create/2]).
-export ([ellipse/3]).

-export ([center/1, axes/1, border_style/1, border_color/1]). 
-export ([fill_color/1, format/1]).
-export ([ellipse_specs/1, border_specs/1, colors/1]). 
-export ([update_center/3]).
-export ([update_dimensions/3, update_border/2, update_border_style/2]).
-export ([update_border_color/2, update_fill_color/2, update_format/2]).

%% -compile(export_all).

-include("../../include/ep.hrl").

-define(DEFAULT_BORDER, 1).
-define(DEFAULT_BORDER_COLOR, black).
-define(DEFAULT_BORDER_STYLE, solid).
-define(DEFAULT_FILL_COLOR, white).
-define(DEFAULT_FORMAT, letter).

%% Border style options: solid, dash, dot, dashdot
%% Color options: white, silver, gray, black, maroon, red, fuschia,
%%                purple, lime, green, olive, yellow, navy, blue, teal, aqua 
%% Format options: N> rp(ep_format:formats()). 


%% ***********************************************************
%% Create ellipse map 
%% ***********************************************************

%% @doc Create ellipse map

-spec create(Center   :: tuple(),
             Axes     :: tuple()) -> map().

create(Center, Axes) ->
   #{ center         => Center
    , axes           => Axes 
    , border         => ?DEFAULT_BORDER
    , border_style   => ?DEFAULT_BORDER_STYLE
    , border_color   => ?DEFAULT_BORDER_COLOR 
    , fill_color     => ?DEFAULT_FILL_COLOR 
    }. 

%% ***********************************************************
%% ellipse/3  
%% ***********************************************************

ellipse(PDF, Job, EllipseMap) ->
    {PaperStock, PagePosition} = ep_job:stock_position(Job),
    Center                   = maps:get(center, EllipseMap),
    Center1                  = ep_lib:impose_xy(Center, 
                                                PagePosition, 
                                                PaperStock),
    Axes                     = maps:get(axes, EllipseMap),
    Border                   = maps:get(border, EllipseMap),
    BorderColor              = maps:get(border_color, EllipseMap),
    BorderStyle              = maps:get(border_style, EllipseMap),
    FillColor                = maps:get(fill_color, EllipseMap),
    eg_pdf:save_state(PDF),
    eg_pdf:set_line_width(PDF, Border),
    eg_pdf:set_dash(PDF, BorderStyle),
    eg_pdf:set_stroke_color(PDF, BorderColor),
    eg_pdf:set_fill_color(PDF, FillColor),
    eg_pdf:ellipse(PDF, Center1, Axes),
    eg_pdf:path(PDF, fill_stroke),
    eg_pdf:restore_state(PDF),
    ok.


%% ***********************************************************
%% Get ellipse attributes  
%% ***********************************************************


%% @doc Return center coordinates 

-spec center(EllipseMap :: map()) -> tuple().

center(EllipseMap) ->
   maps:get(center, EllipseMap).


%% @doc Return axes 

-spec axes(EllipseMap :: map()) -> tuple().

axes(EllipseMap) ->
   maps:get(axes, EllipseMap).

%% @doc Return border  

-spec border(EllipseMap :: map()) -> tuple().

border(EllipseMap) ->
   maps:get(border, EllipseMap).


%% @doc Return border style 

-spec border_style(EllipseMap :: map()) -> tuple().

border_style(EllipseMap) ->
   maps:get(border_style, EllipseMap).


%% @doc Return border color 

-spec border_color(EllipseMap :: map()) -> tuple().

border_color(EllipseMap) ->
   maps:get(border_color, EllipseMap).


%% @doc Return fill color 

-spec fill_color(EllipseMap :: map()) -> tuple().

fill_color(EllipseMap) ->
   maps:get(fill_color, EllipseMap).




%% @docc Return page format 

-spec format(EllipseMap :: map()) -> tuple().

format(EllipseMap) ->
   maps:get(format, EllipseMap).


%% @doc Return ellipse specifications 

-spec ellipse_specs(EllipseMap :: map()) -> tuple().

ellipse_specs(EllipseMap) ->
    {CenterX, CenterY} = center(EllipseMap),
    {XAxis, YAxis} = axes(EllipseMap),
    {CenterX, CenterY, XAxis, YAxis}.


%% @doc Return border specifications 

-spec border_specs(EllipseMap :: map()) -> tuple().

border_specs(EllipseMap) ->
    Border = border(EllipseMap),
    Style  = border_style(EllipseMap),
    Color  = border_color(EllipseMap),
    {Border, Style, Color}.

%% @doc Return border and fill colors 

-spec colors(EllipseMap :: map()) -> tuple().

colors(EllipseMap) ->
    BorderColor  = border_color(EllipseMap),
    FillColor    = fill_color(EllipseMap),
    {BorderColor, FillColor}.


%% ***********************************************************
%% Update ellipse parameters 
%% ***********************************************************

%% Border style: solid, dash, dot, dashdot
%% Colors: white, silver, gray, black, maroon, red, fuschia,
%%         purple, lime, green, olive, yellow, navy, blue, teal, aqua 

%% @doc Update center coordinates 

-spec update_center(CenterX :: integer(),
                    CenterY :: integer(),
                    EllipseMap :: map()) -> tuple().

update_center(CenterX, CenterY, EllipseMap) ->
    maps:put(center, {CenterX, CenterY}, EllipseMap).


%% @doc Update dimensions 

-spec update_dimensions(XAxis      :: integer(),
                        YAxis      :: integer(),
                        EllipseMap :: map()) -> tuple().

update_dimensions(XAxis, YAxis, EllipseMap) ->
    maps:put(axes, {XAxis, YAxis}, EllipseMap).


%% @doc Update border 

-spec update_border(Border     :: integer(),
                    EllipseMap :: map()) -> tuple().

update_border(Border, EllipseMap) ->
    maps:put(border, Border, EllipseMap).


%% @doc Update ellipse style 

-spec update_border_style(BorderStyle :: integer(),
                          EllipseMap  :: map()) -> tuple().

update_border_style(BorderStyle, EllipseMap) ->
    maps:put(border_style, BorderStyle, EllipseMap).


%% @doc Update border color 

-spec update_border_color(BorderColor :: integer(),
                          EllipseMap  :: map()) -> tuple().

update_border_color(BorderColor, EllipseMap) ->
    maps:put(border_color, BorderColor, EllipseMap).

%% @doc Update fill color 

-spec update_fill_color(FillColor  :: integer(),
                        EllipseMap :: map()) -> tuple().

update_fill_color(FillColor, EllipseMap) ->
    maps:put(fill_color, FillColor, EllipseMap).


%% @doc Update format 

-spec update_format(Format     :: integer(),
                    EllipseMap :: map()) -> tuple().

update_format(Format, EllipseMap) ->
    maps:put(format, Format, EllipseMap).



