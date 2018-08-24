%%% ==========================================================================
%%% ep_cdoc_test.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:      MIT
%%%   File:         ep_cdoc_test.erl
%%%   Description:  PDF test harness 
%%% @end

%%% ==========================================================================


-module(ep_cdoc_test).

-export([run/0]).


run()->
    Job = ep_job:create("erlPress: Test", "LRP"),
    OutFile = "./pdf/galleys/" ++ ?MODULE_STRING ++ ".pdf", 

    PDF = eg_pdf:new(),

     PanelNameF                    = {1, 3, code}, 
     CopyF                         = ep_sample_text:code_listing(),
     PositionF                     = {72, 72},
     SizeF                         = {450, 300},
     PanelMapF1                    = ep_panel:create(PanelNameF, PositionF, SizeF),
     PanelMapF2                    = ep_panel:reveal(PanelMapF1),
     PanelMapF3                    = ep_panel:update_typestyle(cdoc, PanelMapF2),
     {PasteF, _Spill, _PanelMapF4} = ep_copy_block:text(CopyF, PanelMapF3),
     ok                            = ep_paste_lib:paste_panel(PDF, Job, PanelMapF3),
     ok                            = ep_paste_lib:paste(PDF, PasteF, [], PanelMapF3),

     ep_job:save_job(PDF, OutFile).

