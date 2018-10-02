%%% ==========================================================================
%% ep_pagination.erl
%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:
%%%   File:         ep_pagination.erl
%%%   Description:  Pagination functions 
%%% @end

%%% ==========================================================================

-module(ep_pagination).

-export([
    first_page/1,
    next_page/1,
    pageno/1
]).

-include("ep.hrl").
-include("ep_erltypes.hrl").


%% @doc Set first page of galley
-spec first_page(pdf_server_pid()) -> ok.
first_page(PDF) ->
    eg_pdf:set_pagesize(PDF, a4),
    eg_pdf:set_author(PDF, "LRP"),
    eg_pdf:set_title(PDF, "Test Elements"),
    eg_pdf:set_subject(PDF, "erlPrest work-in-progress"),
    eg_pdf:set_keywords(PDF, "Erlang, PDF, erlPress"),
    eg_pdf:new_page(PDF),
    eg_pdf:set_page(PDF, 1),
    pageno(PDF),
    ok.


%% @doc Set next page of galley
-spec next_page(pdf_server_pid()) -> ok.
next_page(PDF) ->
    CurrentPage = eg_pdf:get_page_no(PDF),
    NextPage = CurrentPage + 1,
    eg_pdf:new_page(PDF),
    eg_pdf:set_page(PDF, NextPage),
    pageno(PDF).


%% @doc Display page_number
%% NOTE: This is an alternative function to
%%       ep_page_number:create/2; ep_page_number:page_number/3
%%       We should probably pick one
-spec pageno(PDF :: identifier()) -> ok.
pageno(PDF) ->
    eg_pdf:begin_text(PDF),
    eg_pdf:set_font(PDF, "Times-Roman", 11),
    A = eg_pdf:get_page_no(PDF),
    Str = "Page " ++ eg_pdf_op:n2s(A),
    Width = eg_pdf:get_string_width(PDF, "Times-Roman", 11, Str),
    case A rem 2 of
        0 ->
            eg_pdf:set_text_pos(PDF, 100, 50);
        1 ->
            eg_pdf:set_text_pos(PDF, 510 - Width, 50)
    end,
    eg_pdf:text(PDF, "Page " ++ eg_pdf_op:n2s(A)),
    eg_pdf:end_text(PDF),
    ok.
