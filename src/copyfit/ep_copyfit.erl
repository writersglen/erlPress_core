%%% ==========================================================================
%%% ep_copyfit.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:      MIT
%%%   File:         ep_copyfit.erl
%%%   Description:  Copyfitting functions 
%%% @end

%%% ==========================================================================


-module (ep_copyfit).

-export([get_tag/1, get_lines/3, move_content_cursor/2, gap/1, close_gap/4]).

% -compile([export_all]).


%% @doc Given content elment, return tag 

-spec get_tag(Xml :: tuple()) -> atom().

get_tag(Xml) ->
   element(1, Xml).


%% @doc Convert Xml to copyfit richText.

-spec get_lines(Tag        :: atom(),
                Xml        :: list(),
                PanelMap   :: map()) -> list().

get_lines(Tag, Xml, PanelMap) ->
   case Tag of
      ul  ->  List  = element(3, Xml),
              get_richText(List, PanelMap);
      ol  ->  List  = element(3, Xml),
              get_richText(List, PanelMap);
      cl  ->  List  = element(3, Xml),
              get_richText(List, PanelMap);
      br   -> break_rt();
      _    -> ep_xml_lib:xml2lines(Xml, PanelMap)
   end.


%% @doc get_lines/3 helper - Convert list of Xml copy elements into richText

-spec get_richText(List     :: list(),
                   PanelMap :: list()) -> list().

get_richText(List, PanelMap) ->
    [ep_xml_lib:rich_text(Item, PanelMap) || Item <- List].


%% @doc get_lines/3 helper

break_rt() ->
     [{richText, [{space,3000, {face,eg_font_13,12,0,{0,0,0},true}}]}].


%% @doc Update content cursor in panel map

-spec move_content_cursor(Adjust   :: integer(),
                          PanelMap :: map()) -> map().

move_content_cursor(Adjust, PanelMap) ->
    ep_panel:update_content_cursor(Adjust, PanelMap).


%% @doc Available space in panel

-spec gap(PanelMap :: map()) -> integer(). % pixels 

gap(PanelMap) ->
   ep_panel:get_available(PanelMap).


%% ep_article.erl will eventually need functions
%% along these lines

%%  slice/N    - yet to be developed
%%  backfill/N - yet to be developed
%%  shim/N     - yet to be developed


%% @doc copyfit/copyfit_text/3 helper

-spec close_gap(Gap            :: integer(),
                Tag            :: atom(),
                Lines          :: list(),
                PanelMap       :: map()) -> integer().

close_gap(Gap, Tag, Lines, PanelMap) ->
   Needs = needs(Tag, Lines, PanelMap),
   Gap - Needs.


%% @doc close_gap/4 helper 

-spec needs(Tag      :: atom(),
            Lines    :: list(),
            PanelMap :: map()) -> integer().

needs(Tag, Lines, PanelMap) ->
    TypeStyle = ep_panel:get_typestyle(PanelMap),
    Leading   = ep_typespec:leading(TypeStyle, Tag),
    Leading * length(Lines).





