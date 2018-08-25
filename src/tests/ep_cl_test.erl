%%% ==========================================================================
%%% ep_cl_test.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:      MIT
%%%   File:         ep_cl_test.erl
%%%   Description:  PDF test harness 
%%% @end

%%% NOTE: This is work-in-progress!!!!!!!
%%% ==========================================================================


-module(ep_cl_test).

-export([run/0]).

-define(OUTFILE, "cl_test").
-define(PANEL_NAME, {1, 1, cl}).
-define(TYPESTYLE, ragged_report). 


run()->
    Job = ep_job:create("erlPress: CL Test", "LRP"),
    OutFile = "./pdf/galleys/" ++ ?OUTFILE ++ ".pdf", 

    PDF = eg_pdf:new(),

     PanelName                    = ?PANEL_NAME, 
     Copy                         = ep_sample_text:cl(),
     Position                     = {72, 72},
     Size                         = {450, 300},
     PanelMap1                    = ep_panel:create(PanelName, Position, Size),
     PanelMap2                    = ep_panel:reveal(PanelMap1),
     PanelMap3                    = ep_panel:update_typestyle(?TYPESTYLE, PanelMap2),
     {Paste, _Gap, PanelMap4}     = ep_copy_block:text(Copy, PanelMap3),
     ok                           = ep_paste_lib:paste_panel(PDF, Job, PanelMap4),
     ok                           = ep_paste_lib:paste(PDF, Paste, [], PanelMap4),

     ep_job:save_job(PDF, OutFile).

