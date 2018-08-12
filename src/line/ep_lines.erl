%%%==========================================================================
%%% ep_lines.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:      MIT
%%%   File:         ep_lines.erl
%%%   Description:  Display parallel lines 
%%% @end

%%% ==========================================================================


-module (ep_lines).

-export ([create/1, lines/3]).

% -compile(export_all).

-include("../../include/ep.hrl").

%% ***********************************************************
%% create 
%% ***********************************************************


create(LineList) ->
    #{ lines   => LineList
     }.


%% ***********************************************************
%% lines/3  
%% ***********************************************************

lines(PDF, Job, LinesMap) ->
    {PaperStock, PagePosition} = ep_job:stock_position(Job),
    LineList    = maps:get(lines, LinesMap),
    LineList1   = ep_lib:impose_lines(LineList, PagePosition, PaperStock),
    eg_pdf:save_state(PDF),
    eg_pdf:set_line_width(PDF, 1),
    eg_pdf:set_dash(PDF, solid),
    eg_pdf:set_stroke_color(PDF, black),
    eg_pdf:set_fill_color(PDF, black),
    eg_pdf:lines(PDF, LineList1),
    eg_pdf:path(PDF, fill_stroke),
    eg_pdf:restore_state(PDF).

