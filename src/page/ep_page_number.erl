%%% ==========================================================================
%%% ep_page_number.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:
%%%   File:         ep_page_number.erl
%%%   Description:  Display page number 
%%% @end

%%% ==========================================================================


-module (ep_page_number).

-export ([create/2]). 
-export([page_number/3]).

% -compile(export_all).

-include("../../include/ep.hrl").

-define(FONT, "Times-Roman").
-define(FONT_SIZE, 14).

%% Examples:  1, page 1, Page 1

%% ***********************************************************
%% Create page number map 
%% ***********************************************************

%% @doc Create page number map 

-spec create(From :: tuple(),
             Text :: string()) -> map().

create(From, Text) ->
   #{ from         =>  From
    , text          => Text 
    , font          => ?FONT 
    , font_size     => ?FONT_SIZE
    }.


%% ***********************************************************
%% Page number to PDF  
%% ***********************************************************

%% @doc Page number to PDF

-spec page_number(PDF            :: identifier(),
                  Job            :: map(),
                  PpageNumberMap :: map()) -> ok.  


page_number(PDF, Job, PageNumberMap) ->
    {X, Y}       = ep_job:flip_y(Job, PageNumberMap),
    PageNumber   = maps:get(page_number, PageNumberMap),
    PageNumber1  = integer_to_list(PageNumber),
    Text         = maps:get(text, PageNumberMap),
    Text1        = Text ++ PageNumber1,
    eg_pdf:save_state(PDF),
    eg_pdf:begin_text(PDF),
    eg_pdf:set_font(PDF, ?FONT, ?FONT_SIZE),
    eg_pdf:set_text_pos(PDF, X, Y),
    eg_pdf:text(PDF, Text1),
    eg_pdf:end_text(PDF),
    eg_pdf:restore_state(PDF), 
    ok.

