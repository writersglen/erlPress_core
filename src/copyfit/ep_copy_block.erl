%%% ==========================================================================
%%% ep_copy_block.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:      MIT
%%%   File:         ep_copy_block.erl
%%%   Description:  Typseset a block of text
%%% @end

%%% ==========================================================================


-module (ep_copy_block).

-export([text/2, run/0]).
-export([test1/0, test2/0, test3/0, test4/0, test5/0, test6/0, test7/0]).
-export([test8/0, test9/0, test10/0]).

% -compile([export_all]).

%% NOTE: These parameters are for testing purposes

-define(OUTFILE, "text_test").
-define(PANEL_NAME, {1, 1, text}).
-define(COPY, ep_sample_text:times_14()).
-define(POSITION, {72, 50}).
-define(SIZE, {450, 680}).
-define(TYPESTYLE, justify_report).


%%% ==========================================================================
%%% ==========================================================================
%%% text/2 - Typeset and display a block of copy 
%%% ==========================================================================
%%% ==========================================================================


%% @doc Typeset and display a block of text 

-spec text(Copy     :: list(),
           PanelMap :: map()) -> list().


text(Copy, PanelMap) ->
   XML  = ep_xml_lib:parse_xml(Copy),
   Gap  = ep_copyfit:gap(PanelMap),
   {Paste, Gap1, XML1, PanelMap1} = text([], Gap, XML, PanelMap),
   text(Paste, Gap1, XML1, PanelMap1).


text([], Gap, XML, PanelMap) ->
   copyfit_text([], Gap, XML, PanelMap);
 
text(Paste, Gap, [], PanelMap) ->
    {Paste, Gap, PanelMap};

text(Paste, Gap, XML, PanelMap) ->
    io:format("Out of space. Spill: ~p~n~n", [XML]),
    {Paste, Gap, PanelMap}.


%% @doc Recurse through Xml; accumulate lines as we go
%%      NOTE: Paste is a list of copy elements in rich_text format
%%            Gap is space between last rich_text copy element and
%%            the bottom of the panel. XML is a list copy elements; 
%%            e.g. Xml, PanelMap specifies a panel. 

-spec copyfit_text(Paste     :: list(),
                   Gap        :: integer(),
                   XML        :: list(),
                   PanelMap   :: map()) -> list().

% text(Paste, Gap, [], PanelMap) ->
%   {Paste, Gap, [], PanelMap};

copyfit_text(Paste, Gap, XML, PanelMap) ->
   [X | MoreXML]        = XML,
   Xml                  = element(2, X),
   Tag                  = ep_copyfit:get_tag(Xml),
   Lines                = ep_copyfit:get_lines(Tag, Xml, PanelMap),
   SpaceAvailable       = Gap >= 0,
   CopyAvailable        = length(XML) > 1,
   case SpaceAvailable of
      true  -> Gap1     = ep_copyfit:close_gap(Gap, Tag, Lines, PanelMap),
               Paste1   = [{Tag, Lines} | Paste],
               case CopyAvailable of
                  true  -> copyfit_text(Paste1, Gap1, MoreXML, PanelMap);
                  false -> {Paste1, Gap1, [], PanelMap}
               end;
      false -> {Paste,  Gap, MoreXML, PanelMap}
   end.


%%% ==========================================================================
%%% text/2 - Test harnesses 
%%% NOTE: For now, assumes copy fits into panel
%%% ==========================================================================



run() ->
    Job = ep_job:create("erlPress: Copy Block Test", "LRP"),
    OutFile = "./pdf/galleys/" ++ ?OUTFILE ++ ".pdf",
    PDF = eg_pdf:new(),
    PanelName                     = ?PANEL_NAME,
    Copy                          = ?COPY,
    Position                      = ?POSITION,
    Size                          = ?SIZE,
    PanelMap                      = ep_panel:create(PanelName, Position, Size),
    PanelMap1                     = ep_panel:reveal(PanelMap),
    PanelMap2                     = ep_panel:update_typestyle(?TYPESTYLE, PanelMap1),
    {Paste, Gap, PanelMap3}       = text(Copy, PanelMap2),
    ok                            = ep_paste_lib:paste_panel(PDF, Job, PanelMap3),
    ok                            = ep_paste_lib:paste(PDF, Paste, Gap, PanelMap3),
    ep_job:save_job(PDF, OutFile).



test1() ->
    Job = ep_job:create("erlPress: times_14  Test", "LRP"),
    OutFile = "./pdf/galleys/" ++ "test1"  ++ ".pdf",
    PDF = eg_pdf:new(),
    PanelName                     = ?PANEL_NAME,
    Copy                          = ep_sample_text:times_14(),
    TypeStyle                     = justify_report,
    PanelMap                      = ep_panel:create(PanelName, ?POSITION, ?SIZE),
    PanelMap1                     = ep_panel:reveal(PanelMap),
    PanelMap2                     = ep_panel:update_typestyle(TypeStyle, PanelMap1),
    {Paste, Gap, PanelMap3}       = text(Copy, PanelMap2),
    ok                            = ep_paste_lib:paste_panel(PDF, Job, PanelMap3),
    ok                            = ep_paste_lib:paste(PDF, Paste, Gap, PanelMap3),
    ep_job:save_job(PDF, OutFile).



test2() ->
    Job = ep_job:create("erlPress: helvetica_10 Test", "LRP"),
    OutFile = "./pdf/galleys/" ++ "test2"  ++ ".pdf",
    PDF = eg_pdf:new(),
    PanelName                     = ?PANEL_NAME,
    Copy                          = ep_sample_text:times_14(),
    TypeStyle                     = justify_report_hv,
    PanelMap                      = ep_panel:create(PanelName, ?POSITION, ?SIZE),
    PanelMap1                     = ep_panel:reveal(PanelMap),
    PanelMap2                     = ep_panel:update_typestyle(TypeStyle, PanelMap1),
    {Paste, Gap, PanelMap3}       = text(Copy, PanelMap2),
    ok                            = ep_paste_lib:paste_panel(PDF, Job, PanelMap3),
    ok                            = ep_paste_lib:paste(PDF, Paste, Gap, PanelMap3),
    ep_job:save_job(PDF, OutFile).


test3() ->
    Job = ep_job:create("erlPress: preformatted  Test", "LRP"),
    OutFile = "./pdf/galleys/" ++ "test3"  ++ ".pdf",
    PDF = eg_pdf:new(),
    PanelName                     = ?PANEL_NAME,
    Copy                          = ep_sample_text:the_road_not_taken(),
    TypeStyle                     = preformatted_report,
    PanelMap                      = ep_panel:create(PanelName, ?POSITION, ?SIZE),
    PanelMap1                     = ep_panel:reveal(PanelMap),
    PanelMap2                     = ep_panel:update_typestyle(TypeStyle, PanelMap1),
    {Paste, Gap, PanelMap3}       = text(Copy, PanelMap2),
    ok                            = ep_paste_lib:paste_panel(PDF, Job, PanelMap3),
    ok                            = ep_paste_lib:paste(PDF, Paste, Gap, PanelMap3),
    ep_job:save_job(PDF, OutFile).


test4() ->
    Job = ep_job:create("erlPress: centered_report  Test", "LRP"),
    OutFile = "./pdf/galleys/" ++ "test4"  ++ ".pdf",
    PDF = eg_pdf:new(),
    PanelName                     = ?PANEL_NAME,
    Copy                          = ep_sample_text:times_14(),
    TypeStyle                     = centered_report,
    PanelMap                      = ep_panel:create(PanelName, ?POSITION, ?SIZE),
    PanelMap1                     = ep_panel:reveal(PanelMap),
    PanelMap2                     = ep_panel:update_typestyle(TypeStyle, PanelMap1),
    {Paste, Gap, PanelMap3}       = text(Copy, PanelMap2),
    ok                            = ep_paste_lib:paste_panel(PDF, Job, PanelMap3),
    ok                            = ep_paste_lib:paste(PDF, Paste, Gap, PanelMap3),
    ep_job:save_job(PDF, OutFile).



test5() ->
    Job = ep_job:create("erlPress: ragged_report Test", "LRP"),
    OutFile = "./pdf/galleys/" ++ "test5"  ++ ".pdf",
    PDF = eg_pdf:new(),
    PanelName                     = ?PANEL_NAME,
    Copy                          = ep_sample_text:times_14(),
    TypeStyle                     = ragged_report,
    PanelMap                      = ep_panel:create(PanelName, ?POSITION, ?SIZE),
    PanelMap1                     = ep_panel:reveal(PanelMap),
    PanelMap2                     = ep_panel:update_typestyle(TypeStyle, PanelMap1),
    {Paste, Gap, PanelMap3}       = text(Copy, PanelMap2),
    ok                            = ep_paste_lib:paste_panel(PDF, Job, PanelMap3),
    ok                            = ep_paste_lib:paste(PDF, Paste, Gap, PanelMap3),
    ep_job:save_job(PDF, OutFile).


test6() ->
    Job = ep_job:create("erlPress: code Test", "LRP"),
    OutFile = "./pdf/galleys/" ++ "test6"  ++ ".pdf",
    PDF = eg_pdf:new(),
    PanelName                     = ?PANEL_NAME,
    Copy                          = ep_sample_text:code_listing(),
    TypeStyle                     = cdoc,
    PanelMap                      = ep_panel:create(PanelName, ?POSITION, ?SIZE),
    PanelMap1                     = ep_panel:reveal(PanelMap),
    PanelMap2                     = ep_panel:update_typestyle(TypeStyle, PanelMap1),
    {Paste, Gap, PanelMap3}       = text(Copy, PanelMap2),
    ok                            = ep_paste_lib:paste_panel(PDF, Job, PanelMap3),
    ok                            = ep_paste_lib:paste(PDF, Paste, Gap, PanelMap3),
    ep_job:save_job(PDF, OutFile).


test7() ->
    Job = ep_job:create("erlPress: article Test", "LRP"),
    OutFile = "./pdf/galleys/" ++ "test7"  ++ ".pdf",
    PDF = eg_pdf:new(),
    PanelName                     = ?PANEL_NAME,
    Copy                          = ep_sample_text:article(),
    TypeStyle                     = justify_report,
    PanelMap                      = ep_panel:create(PanelName, ?POSITION, ?SIZE),
    PanelMap1                     = ep_panel:reveal(PanelMap),
    PanelMap2                     = ep_panel:update_typestyle(TypeStyle, PanelMap1),
    {Paste, Gap, PanelMap3}       = text(Copy, PanelMap2),
    ok                            = ep_paste_lib:paste_panel(PDF, Job, PanelMap3),
    ok                            = ep_paste_lib:paste(PDF, Paste, Gap, PanelMap3),
    ep_job:save_job(PDF, OutFile).


test8() ->
    Job = ep_job:create("erlPress: UL Test", "LRP"),
    OutFile = "./pdf/galleys/" ++ "test8"  ++ ".pdf",
    PDF = eg_pdf:new(),
    PanelName                     = ?PANEL_NAME,
    Copy                          = ep_sample_text:ul(),
    TypeStyle                     = justify_report,
    PanelMap                      = ep_panel:create(PanelName, ?POSITION, ?SIZE),
    PanelMap1                     = ep_panel:reveal(PanelMap),
    PanelMap2                     = ep_panel:update_typestyle(TypeStyle, PanelMap1),
    {Paste, Gap, PanelMap3}       = text(Copy, PanelMap2),
    ok                            = ep_paste_lib:paste_panel(PDF, Job, PanelMap3),
    ok                            = ep_paste_lib:paste(PDF, Paste, Gap, PanelMap3),
    ep_job:save_job(PDF, OutFile).


test9() ->
    Job = ep_job:create("erlPress: CL Test", "LRP"),
    OutFile = "./pdf/galleys/" ++ "test9"  ++ ".pdf",
    PDF = eg_pdf:new(),
    PanelName                     = ?PANEL_NAME,
    Copy                          = ep_sample_text:cl(),
    TypeStyle                     = justify_report,
    PanelMap                      = ep_panel:create(PanelName, ?POSITION, ?SIZE),
    PanelMap1                     = ep_panel:reveal(PanelMap),
    PanelMap2                     = ep_panel:update_typestyle(TypeStyle, PanelMap1),
    {Paste, Gap, PanelMap3}       = text(Copy, PanelMap2),
    ok                            = ep_paste_lib:paste_panel(PDF, Job, PanelMap3),
    ok                            = ep_paste_lib:paste(PDF, Paste, Gap, PanelMap3),
    ep_job:save_job(PDF, OutFile).


test10() ->
    Job = ep_job:create("erlPress: erlpress Test", "LRP"),
    OutFile = "./pdf/galleys/" ++ "test10"  ++ ".pdf",
    PDF = eg_pdf:new(),
    PanelName                     = ?PANEL_NAME,
    Copy                          = ep_sample_text:erlpress(),
    TypeStyle                     = justify_report,
    PanelMap                      = ep_panel:create(PanelName, ?POSITION, ?SIZE),
    PanelMap1                     = ep_panel:reveal(PanelMap),
    PanelMap2                     = ep_panel:update_typestyle(TypeStyle, PanelMap1),
    {Paste, Gap, PanelMap3}       = text(Copy, PanelMap2),
    ok                            = ep_paste_lib:paste_panel(PDF, Job, PanelMap3),
    ok                            = ep_paste_lib:paste(PDF, Paste, Gap, PanelMap3),
    ep_job:save_job(PDF, OutFile).








