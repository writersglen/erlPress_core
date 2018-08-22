%%% ==========================================================================
%%% ep_copy_block.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:      MIT
%%%   File:         ep_text_block.erl
%%%   Description:  Typseset a block of text
%%% @end

%%% ==========================================================================



-module (ep_copy_block).

-export([text_copy/2, run/0]).

%% NOTE: These parameters are for testing purposes

-define(OUTFILE, "text_copy_test").
-define(PANEL_NAME, {1, 1, text_copy}).
-define(COPY, ep_sample_text:cl()).
-define(POSITION, {72, 20}).
-define(SIZE, {450, 750}).
-define(TYPESTYLE, justify_report).


%%% ==========================================================================
%%% ==========================================================================
%%% copy_blk/2 - Typeset and display a block of copy 
%%% ==========================================================================
%%% ==========================================================================


%% @doc Typeset and display a block of text 

-spec text_copy(Copy     :: list(),
                PanelMap :: map()) -> list().


text_copy(Copy, PanelMap) ->
   XML                     = ep_xml_lib:parse_xml(Copy),
   Gap                     = ep_copyfit:gap(PanelMap),
   {Paste, Gap1, PanelMap1} = text_copy([], Gap, XML, PanelMap),
   {Paste, Gap1, PanelMap1}.


text_copy(Paste, Gap, PanelMap) ->
   {Paste, Gap, PanelMap}.

%% @doc Recurse through Xml; accumulate lines as we go
%%      NOTE: Paste is a list of copy elements in rich_text format
%%            Gap is space between last rich_text copy element and
%%            the bottom of the panel. XML is a list copy elements; 
%%            e.g. Xml, PanelMap specifies a panel. 

-spec text_copy(Paste     :: list(),
               Gap        :: integer(),
               XML        :: list(),
               PanelMap   :: map()) -> list().

text_copy(Paste, Gap, [], PanelMap) ->
   {Paste, Gap, PanelMap};

text_copy(Paste, Gap, XML, PanelMap) ->
   [X | MoreXML]        = XML,
   Xml                  = element(2, X),
   Tag                  = ep_copyfit:get_tag(Xml),
   Lines                = ep_copyfit:get_lines(Tag, Xml, PanelMap),
   SpaceAvailable       = Gap >= 0,
   case SpaceAvailable of
      true  -> Paste1   = [{Tag, Lines} | Paste],
               Gap1     = ep_copyfit:close_gap(Gap, Tag, Lines, PanelMap),
               text_copy(Paste1, Gap1, MoreXML, PanelMap);
      false -> io:format("Panel overflow: XML: ~p~n", [XML]), 
               text_copy(Paste,  Gap, PanelMap)
   end.


%%% ==========================================================================
%%% copy_blk/2 - Test harness 
%%% NOTE: For now, assumes copy fits into panel
%%% ==========================================================================



run() ->
    io:format("Entering ep_copy_block:run()~n"),
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
    io:format("I'm here!~n"),
    {Paste, Gap, PanelMap3}       = text_copy(Copy, PanelMap2),
    io:format("Gap: ~p~n~n", [Gap]),
    ok                            = ep_paste_lib:paste_panel(PDF, Job, PanelMap3),
    ok                            = ep_paste_lib:paste(PDF, Paste, Gap, PanelMap3),
    ep_job:save_job(PDF, OutFile).

















