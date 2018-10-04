%%% *********************************************************

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:
%%%   File:         ep_metrics.erl
%%%   Description:  Print media metrics conversion 
%%% @end

%%% ==========================================================================

-module(ep_metrics).

-export([
    to_picas/1,
    to_points/1
]).

-include("ep_erltypes.hrl").


%% @doc Convert {Width, Height} in inches to picas
-spec to_picas(Dimensions :: xy()) -> xy().
to_picas(Dimensions) ->
    {Width, Height} = Dimensions,
    Width1 = round(Width * 6),
    Height1 = round(Height * 6),
    {Width1, Height1}.


%% @doc Convert page type dimensions in inches to points
-spec to_points(Dimensions :: xy()) -> xy().
to_points(Dimensions) ->
    {Width, Height} = Dimensions,
    Width1 = round(Width * 72),
    Height1 = round(Height * 72),
    {Width1, Height1}.
