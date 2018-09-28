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

-export([text/2]).

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
    XML = ep_xml_lib:parse_xml(Copy),
    Gap = ep_copyfit:gap(PanelMap),
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


copyfit_text(Paste, Gap, XML, PanelMap) ->
    [X | MoreXML] = XML,
    Xml            = element(2, X),
    Tag            = ep_copyfit:get_tag(Xml),
    Lines          = ep_copyfit:get_lines(Tag, Xml, PanelMap),
    SpaceAvailable = Gap >= 0,
    CopyAvailable  = length(XML) > 1,
    case SpaceAvailable of
        true -> Gap1 = ep_copyfit:close_gap(Gap, Tag, Lines, PanelMap),
            Paste1 = [{Tag, Lines} | Paste],
            case CopyAvailable of
                true -> copyfit_text(Paste1, Gap1, MoreXML, PanelMap);
                false -> {Paste1, Gap1, [], PanelMap}
            end;
        false -> {Paste, Gap, MoreXML, PanelMap}
    end.
