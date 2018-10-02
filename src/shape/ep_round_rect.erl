%%% ==========================================================================
%%% ep_round_rect.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:      MIT
%%%   File:         ep_round_rect.erl
%%%   Description:  Display rectangle with rounded corners 
%%% @end

%%% ==========================================================================


-module (ep_round_rect).

-export([
    create/3,
    round_rect/3
]).

-include("ep.hrl").
-include("ep_erltypes.hrl").

-define(BORDER, 1).
-define(BORDER_COLOR, black).
-define(BORDER_STYLE, solid).
-define(BACKGROUND_COLOR, white).

%% ***********************************************************
%% Create round_rect map 
%% ***********************************************************

%% @doc Create round_rect map
-spec create(Position :: xy(), Size :: xy(), Radius :: points()) -> ep_round_rect().
create(Position, Size, Radius) ->
   #{ position          => Position 
    , size              => Size 
    , radius            => Radius
    , border            => ?BORDER
    , border_style      => ?BORDER_STYLE
    , border_color      => ?BORDER_COLOR
    , background_color  => ?BACKGROUND_COLOR
    }.


%% @doc Display rectangle
-spec round_rect(pdf_server_pid(), ep_job(), ep_round_rect()) -> ok.
round_rect(PDF, Job, RoundRectMap) ->
   Position        = ep_job:flip_y(Job, RoundRectMap), 
   Size            = maps:get(size, RoundRectMap),
   Radius          = maps:get(radius, RoundRectMap), 
   Border          = maps:get(border, RoundRectMap),
   BorderStyle     = maps:get(border_style, RoundRectMap),
   BorderColor     = maps:get(border_color, RoundRectMap),
   BackgroundColor = maps:get(background_color, RoundRectMap),
   eg_pdf:set_line_width(PDF, Border),
   eg_pdf:set_dash(PDF, BorderStyle),
   eg_pdf:set_stroke_color(PDF, BorderColor),
   eg_pdf:set_fill_color(PDF, BackgroundColor), 
   eg_pdf:round_rect(PDF, Position, Size, Radius),
   eg_pdf:path(PDF, fill_stroke),
   ok.
