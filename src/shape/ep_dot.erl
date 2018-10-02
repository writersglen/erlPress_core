%%T==========================================================================
%%% ep_dot.erl
%%%
%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version    .01
%%% @doc
%%%     License:      MIT
%%%     File:         ep_dot.erl
%%%     Description:  Layout primitives 
%%% @end
%%%==========================================================================

-module(ep_dot).

-export([
    create/1,
    dot/3
]).

-define(DEFAULT_RADIUS, 5).

-include("ep.hrl").
-include("ep_erltypes.hrl").


%% @doc Create dot map
-spec create(Center :: xy()) -> map().
create(Center) ->
   #{ center         => Center
    , radius         => ?DEFAULT_RADIUS
    , color          => black
    , border         => 1
    , border_type    => solid
    , border_color   =>  black
    }.


%% ***********************************************************
%% dot/3  
%% ***********************************************************

%% @doc Display dot

-spec dot(PDF    :: identifier(),
          Job    :: map(),
          DotMap :: map()) -> ok.


dot(PDF, Job, DotMap) ->
    {PaperStock, PagePosition} = ep_job:stock_position(Job),
    Center      = maps:get(center, DotMap),
    Center1     = ep_lib:impose_xy(Center, PagePosition, PaperStock), 
    Color       = maps:get(color, DotMap),
    Radius      = maps:get(radius, DotMap),
    Border      = maps:get(border, DotMap),
    BorderType  = maps:get(border_type, DotMap),
    BorderColor = maps:get(border_color, DotMap),
    eg_pdf:save_state(PDF),
    eg_pdf:set_line_width(PDF, Border),
    eg_pdf:set_dash(PDF, BorderType),
    eg_pdf:set_stroke_color(PDF, BorderColor),
    eg_pdf:set_fill_color(PDF, Color),
    eg_pdf:circle(PDF, Center1, Radius),
    eg_pdf:path(PDF, fill_stroke),
    eg_pdf:restore_state(PDF),
    ok.




