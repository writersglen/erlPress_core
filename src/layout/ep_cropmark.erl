%%==========================================================================
%% ep_cropmark.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:      MIT
%%%   File:         ep_cropmark.erl
%%%   Description:  Page imposition: Display cropmark  
%%% @end

%%% ==========================================================================


-module (ep_cropmark).

-export ([create/1, cropmark/3]).

% -compile(export_all).

-include("../../include/ep.hrl").

%% ***********************************************************
%% create cropmark map 
%% ***********************************************************

%% @doc Create cropmark map

-spec create(Position :: tuple()) -> map().

create(Position) ->
   #{position    =>  Position
    }.


%% ***********************************************************
%% dot/3  
%% ***********************************************************

%% @doc Create cropmark

-spec cropmark(PDF :: identifier(),
               Job :: map(),
               CropmarkMap :: map()) -> ok. 


cropmark(PDF, Job, CropmarkMap) ->
    {X1, Y1}    = ep_job:flip_y(Job, CropmarkMap),
    HFrom       = {(X1 - 10), Y1},  
    HTo         = {(X1 + 10), Y1},  
    VFrom       = {X1, (Y1 - 10)},  
    VTo         = {X1, (Y1 + 10)}, 
    eg_pdf:save_state(PDF),
    eg_pdf:set_line_width(PDF, 1),
    eg_pdf:set_dash(PDF, solid),
    eg_pdf:set_stroke_color(PDF, black),
    eg_pdf:set_fill_color(PDF, black),
    eg_pdf:line(PDF, HFrom, HTo),
    eg_pdf:line(PDF, VFrom, VTo),
    eg_pdf:path(PDF, fill_stroke),
    eg_pdf:restore_state(PDF),
    ok.

