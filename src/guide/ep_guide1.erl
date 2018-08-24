%%% ==========================================================================
%%% ep_guide1.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:      MIT
%%%   File:         ep_guide1.erl
%%%   Description:  Programmers guide 
%%% @end
%%%
%%% NOTE: Work-in-progress
%%% ==========================================================================


-module(ep_guide1).

-export([run/0]).


run()->
    Job = ep_job:create("erlPress: Programmer's Guide Pt 1", "LRP"),

    PDF = eg_pdf:new(),

    ep_page:first_page(PDF),  % ****** Page1

    % ********** Page 1 makeup
    

    ep_page:next_page(PDF),    % ****** Page 2
    
    % ********** Page 2 makeup


    ep_page:next_page(PDF),    % ****** Page 3

    % ********** Page 3 makeup


    ep_page:next_page(PDF),    % ****** Page 4

    % ********** Page 4 makeup


    ep_page:next_page(PDF),    % ****** Page 5

    % ********** Page 5 makeup 

    ep_page:next_page(PDF),    % ****** Page 6

    % ********** Page 6 makeup 

ep_job:save_job(PDF).
