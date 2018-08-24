%%% ==========================================================================
%%% ep_ul_test.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:      MIT
%%%   File:         ep_ul_test.erl
%%%   Description:  PDF test harness 
%%% @end

%%% NOTE: This is work-in-progress!!!!!!!
%%% ==========================================================================


-module(ep_ul_test).

-export([run/0]).

-define(OUTFILE, "ul_test").
-define(PANEL_NAME, {1, 1, ul}).
-define(TYPESTYLE, ragged_report). 


run()->
    Job = ep_job:create("erlPress: UL Test", "LRP"),
    OutFile = "./pdf/galleys/" ++ ?OUTFILE ++ ".pdf", 

    PDF = eg_pdf:new(),

     PanelNameF                    = ?PANEL_NAME, 
     CopyF                         = ep_sample_text:ul(),
     PositionF                     = {72, 72},
     SizeF                         = {450, 300},
     PanelMapF1                    = ep_panel:create(PanelNameF, PositionF, SizeF),
     PanelMapF2                    = ep_panel:reveal(PanelMapF1),
     PanelMapF3                    = ep_panel:update_typestyle(?TYPESTYLE, PanelMapF2),
     {PasteF, _Spill, _PanelMapF4} = ep_copy_block:text(CopyF, PanelMapF3),
     ok                            = ep_paste_lib:paste_panel(PDF, Job, PanelMapF3),
     ok                            = ep_paste_lib:paste(PDF, PasteF, [], PanelMapF3),

     ep_job:save_job(PDF, OutFile).

