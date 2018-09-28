%%% ==========================================================================
%%% ep_grid.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:
%%%   File:         ep_grid.erl
%%%   Description:  Display grid 
%%% @end

%%% ==========================================================================


-module (ep_grid).

-export ([
    create/2,
    grid/3
]).

-include("ep.hrl").

%% ***********************************************************
%% create 
%% ***********************************************************

%% @doc Create grid map

-spec create(XList :: list(),
             YList :: list()) -> map().

create(XList, YList) ->
    #{ xlist    => XList
     , ylist    => YList
     }.


%% ***********************************************************
%% grid/3  
%% ***********************************************************

%% @doc Display grid

-spec grid(PDF     :: identifier(),
           Job     :: map(),
           GridMap :: #{xlist => list(), ylist => list()}) -> ok.

grid(PDF, Job, GridMap) ->
    {PaperStock, _PagePosition} = ep_job:stock_position(Job),

    XList = maps:get(xlist, GridMap),
    YList = maps:get(ylist, GridMap),
    YList1 = [ep_lib:v_flip(Y, PaperStock) || Y <- YList],

    eg_pdf:save_state(PDF),
    eg_pdf:set_line_width(PDF, 1),
    eg_pdf:set_dash(PDF, solid),
    eg_pdf:set_stroke_color(PDF, black),
    eg_pdf:set_fill_color(PDF, black),
    eg_pdf:grid(PDF, XList, YList1),
    eg_pdf:path(PDF, fill_stroke),
    eg_pdf:restore_state(PDF),
    ok.

