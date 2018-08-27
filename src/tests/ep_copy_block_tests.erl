%%% ==========================================================================
%%% ep_copy_block_tests.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:      MIT
%%%   File:         ep_copy_block_tests.erl
%%%   Description:  PDF test harnesses 
%%%
%%%   ep_copy_block.erl test functions tests all erlPress_core 
%%%   typestyles. It also provides many programming exmples.
%%% @end
%%% ==========================================================================


-module(ep_copy_block_tests).

-export([test1/0, test2/0, test3/0, test4/0, test5/0]).
-export([test6/0, test7/0, test8/0, test9/0, test10/0, test11/0]).


-define(OUTFILE, "ul_test").
-define(PANEL_NAME, {1, 1, ul}).
-define(POSITION, {72, 50}).
-define(SIZE, {450, 680}).
-define(TYPESTYLE, ragged_report). 


test1() ->
    Job = ep_job:create("erlPress: times_14  Test", "LRP"),
    OutFile = "./pdf/galleys/" ++ "test1"  ++ ".pdf",
    PDF = eg_pdf:new(),
    PanelName                     = {1, 1, "test1"},
    Copy                          = ep_sample_text:times_14(),
    TypeStyle                     = justify_report,
    PanelMap                      = ep_panel:create(PanelName, ?POSITION, ?SIZE),
    PanelMap1                     = ep_panel:reveal(PanelMap),
    PanelMap2                     = ep_panel:update_typestyle(TypeStyle, PanelMap1),
    {Paste, Gap, PanelMap3}       = ep_copy_block:text(Copy, PanelMap2),
    ok                            = ep_paste_lib:paste_panel(PDF, Job, PanelMap3),
    ok                            = ep_paste_lib:paste(PDF, Paste, Gap, PanelMap3),
    ep_job:save_job(PDF, OutFile).

test2() ->
    Job = ep_job:create("erlPress: helvetica_10 Test", "LRP"),
    OutFile = "./pdf/galleys/" ++ "test2"  ++ ".pdf",
    PDF = eg_pdf:new(),
    PanelName                     = {1, 1, "test2"},
    Copy                          = ep_sample_text:helvetica_10(),
    TypeStyle                     = justify_report_hv,
    PanelMap                      = ep_panel:create(PanelName, ?POSITION, ?SIZE),
    PanelMap1                     = ep_panel:reveal(PanelMap),
    PanelMap2                     = ep_panel:update_typestyle(TypeStyle, PanelMap1),
    {Paste, Gap, PanelMap3}       = ep_copy_block:text(Copy, PanelMap2),
    ok                            = ep_paste_lib:paste_panel(PDF, Job, PanelMap3),
    ok                            = ep_paste_lib:paste(PDF, Paste, Gap, PanelMap3),
    ep_job:save_job(PDF, OutFile).


test3() ->
    Job = ep_job:create("erlPress: centered_report  Test", "LRP"),
    OutFile = "./pdf/galleys/" ++ "test3"  ++ ".pdf",
    PDF = eg_pdf:new(),
    PanelName                     = {1, 1, "test3"},
    Copy                          = ep_sample_text:times_14(),
    TypeStyle                     = centered_report,
    PanelMap                      = ep_panel:create(PanelName, ?POSITION, ?SIZE),
    PanelMap1                     = ep_panel:reveal(PanelMap),
    PanelMap2                     = ep_panel:update_typestyle(TypeStyle, PanelMap1),
    {Paste, Gap, PanelMap3}       = ep_copy_block:text(Copy, PanelMap2),
    ok                            = ep_paste_lib:paste_panel(PDF, Job, PanelMap3),
    ok                            = ep_paste_lib:paste(PDF, Paste, Gap, PanelMap3),
    ep_job:save_job(PDF, OutFile).


test4() ->
    Job = ep_job:create("erlPress: preformatted  Test", "LRP"),
    OutFile = "./pdf/galleys/" ++ "test4"  ++ ".pdf",
    PDF = eg_pdf:new(),
    PanelName                     = {1, 1, "test4"},
    Copy                          = ep_sample_text:the_road_not_taken(),
    TypeStyle                     = preformatted_report,
    PanelMap                      = ep_panel:create(PanelName, ?POSITION, ?SIZE),
    PanelMap1                     = ep_panel:reveal(PanelMap),
    PanelMap2                     = ep_panel:update_typestyle(TypeStyle, PanelMap1),
    {Paste, Gap, PanelMap3}       = ep_copy_block:text(Copy, PanelMap2),
    ok                            = ep_paste_lib:paste_panel(PDF, Job, PanelMap3),
    ok                            = ep_paste_lib:paste(PDF, Paste, Gap, PanelMap3),
    ep_job:save_job(PDF, OutFile).


test5() ->
    Job = ep_job:create("erlPress: preformatted  Test", "LRP"),
    OutFile = "./pdf/galleys/" ++ "test5"  ++ ".pdf",
    PDF = eg_pdf:new(),
    PanelName                     = {1, 1, "test5"},
    Copy                          = ep_sample_text:code_listing(),
    TypeStyle                     = cdoc,
    PanelMap                      = ep_panel:create(PanelName, ?POSITION, ?SIZE),
    PanelMap1                     = ep_panel:reveal(PanelMap),
    PanelMap2                     = ep_panel:update_typestyle(TypeStyle, PanelMap1),
    {Paste, Gap, PanelMap3}       = ep_copy_block:text(Copy, PanelMap2),
    ok                            = ep_paste_lib:paste_panel(PDF, Job, PanelMap3),
    ok                            = ep_paste_lib:paste(PDF, Paste, Gap, PanelMap3),
    ep_job:save_job(PDF, OutFile).



test6() ->
    Job = ep_job:create("erlPress: ragged_report Test", "LRP"),
    OutFile = "./pdf/galleys/" ++ "test6"  ++ ".pdf",
    PDF = eg_pdf:new(),
    PanelName                     = {1, 1, "test6"},
    Copy                          = ep_sample_text:times_14(),
    TypeStyle                     = ragged_report,
    Size                          = {310, 500},
    PanelMap                      = ep_panel:create(PanelName, ?POSITION, Size),
    PanelMap1                     = ep_panel:reveal(PanelMap),
    PanelMap2                     = ep_panel:update_typestyle(TypeStyle, PanelMap1),
    {Paste, Gap, PanelMap3}       = ep_copy_block:text(Copy, PanelMap2),
    ok                            = ep_paste_lib:paste_panel(PDF, Job, PanelMap3),
    ok                            = ep_paste_lib:paste(PDF, Paste, Gap, PanelMap3),
    ep_job:save_job(PDF, OutFile).


test7() ->
    Job = ep_job:create("erlPress: article Test", "LRP"),
    OutFile = "./pdf/galleys/" ++ "test7"  ++ ".pdf",
    PDF = eg_pdf:new(),
    PanelName                     = {1, 1, "test7"},
    Copy                          = ep_sample_text:article(),
    TypeStyle                     = justify_report,
    PanelMap                      = ep_panel:create(PanelName, ?POSITION, ?SIZE),
    PanelMap1                     = ep_panel:reveal(PanelMap),
    PanelMap2                     = ep_panel:update_typestyle(TypeStyle, PanelMap1),
    {Paste, Gap, PanelMap3}       = ep_copy_block:text(Copy, PanelMap2),
    ok                            = ep_paste_lib:paste_panel(PDF, Job, PanelMap3),
    ok                            = ep_paste_lib:paste(PDF, Paste, Gap, PanelMap3),
    ep_job:save_job(PDF, OutFile).


test8() ->
    Job = ep_job:create("erlPress: UL Test", "LRP"),
    OutFile = "./pdf/galleys/" ++ "test8"  ++ ".pdf",
    PDF = eg_pdf:new(),
    PanelName                     = {1, 1, "test8"},
    Copy                          = ep_sample_text:ul(),
    TypeStyle                     = justify_report,
    PanelMap                      = ep_panel:create(PanelName, ?POSITION, ?SIZE),
    PanelMap1                     = ep_panel:reveal(PanelMap),
    PanelMap2                     = ep_panel:update_typestyle(TypeStyle, PanelMap1),
    {Paste, Gap, PanelMap3}       = ep_copy_block:text(Copy, PanelMap2),
    ok                            = ep_paste_lib:paste_panel(PDF, Job, PanelMap3),
    ok                            = ep_paste_lib:paste(PDF, Paste, Gap, PanelMap3),
    ep_job:save_job(PDF, OutFile).


test9() ->
    Job = ep_job:create("erlPress: CL Test", "LRP"),
    OutFile = "./pdf/galleys/" ++ "test9"  ++ ".pdf",
    PDF = eg_pdf:new(),
    PanelName                     = {1, 1, "test9"},
    Copy                          = ep_sample_text:cl(),
    TypeStyle                     = justify_report,
    PanelMap                      = ep_panel:create(PanelName, ?POSITION, ?SIZE),
    PanelMap1                     = ep_panel:reveal(PanelMap),
    PanelMap2                     = ep_panel:update_typestyle(TypeStyle, PanelMap1),
    {Paste, Gap, PanelMap3}       = ep_copy_block:text(Copy, PanelMap2),
    ok                            = ep_paste_lib:paste_panel(PDF, Job, PanelMap3),
    ok                            = ep_paste_lib:paste(PDF, Paste, Gap, PanelMap3),
    ep_job:save_job(PDF, OutFile).


test10()->
    Job = ep_job:create("erlPress: OL Test", "LRP"),
    OutFile = "./pdf/galleys/" ++ "test10" ++ ".pdf",
    PDF = eg_pdf:new(),
     PanelName                    = {1, 1, "test10"},
     Copy                         = ep_sample_text:ol(),
     Position                     = {72, 72},
     Size                         = {450, 300},
     PanelMap1                    = ep_panel:create(PanelName, Position, Size),
     PanelMap2                    = ep_panel:reveal(PanelMap1),
     PanelMap3                    = ep_panel:update_typestyle(?TYPESTYLE, PanelMap2),
     {Paste, _Spill, _PanelMap4} = ep_copy_block:text(Copy, PanelMap3),
     ok                            = ep_paste_lib:paste_panel(PDF, Job, PanelMap3),
     ok                            = ep_paste_lib:paste(PDF, Paste, [], PanelMap3),

     ep_job:save_job(PDF, OutFile).


test11() ->
    Job = ep_job:create("erlPress: erlpress Test", "LRP"),
    OutFile = "./pdf/galleys/" ++ "test11"  ++ ".pdf",
    PDF = eg_pdf:new(),
    PanelName                     = {1, 1, "test11"},
    Copy                          = ep_sample_text:erlpress(),
    TypeStyle                     = justify_report,
    PanelMap                      = ep_panel:create(PanelName, ?POSITION, ?SIZE),
    PanelMap1                     = ep_panel:reveal(PanelMap),
    PanelMap2                     = ep_panel:update_typestyle(TypeStyle, PanelMap1),
    {Paste, Gap, PanelMap3}       = ep_copy_block:text(Copy, PanelMap2),
    ok                            = ep_paste_lib:paste_panel(PDF, Job, PanelMap3),
    ok                            = ep_paste_lib:paste(PDF, Paste, Gap, PanelMap3),
    ep_job:save_job(PDF, OutFile).


