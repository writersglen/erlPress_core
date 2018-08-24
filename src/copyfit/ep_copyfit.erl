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

% -export([get_tag/1, get_lines/3, gap/1, will_fit/2, close_gap/4]).

-compile([export_all]).


%% @doc Given content elment, return tag 

-spec get_tag(Xml :: tuple()) -> atom().

get_tag(Xml) ->
   element(1, Xml).


%% @doc Convert Xml to copyfit richText.

-spec get_lines(Tag        :: atom(),
                Xml        :: list(),
                PanelMap   :: map()) -> list().

get_lines(Tag, Xml, PanelMap) ->
   io:format("Tag: ~p~n", [Tag]),
   case Tag of
      ul  ->  List  = element(3, Xml),
              get_richText(List, PanelMap);
      br   -> break_rt();
      _    -> ep_xml_lib:xml2lines(Xml, PanelMap)
   end.

%% @doc Convert list of Xml copy elements into richText

-spec get_richText(List     :: list(),
                   PanelMap :: list()) -> list().

get_richText(List, PanelMap) ->
    [ep_xml_lib:rich_text(Item, PanelMap) || Item <- List].


break_rt() ->
     [{richText, [{space,3000, {face,eg_font_13,12,0,{0,0,0},true}}]}].



move_content_cursor(Adjust, PanelMap) ->
    ep_panel:update_content_cursor(Adjust, PanelMap).



%% @doc Available space in panel

-spec gap(PanelMap :: map()) -> integer(). % pixels 

gap(PanelMap) ->
   ep_panel:get_available(PanelMap).


will_fit(Gap, PanelMap) ->
    TypeStyle = ep_panel:get_typestyle(PanelMap),
    Leading   = ep_typespec:leading(TypeStyle, p),
    Gap div Leading.

%%  backfill/N - yet to be developed
%   shim/N     - yet to be developed

close_gap(Gap, Tag, Lines, PanelMap) ->
   Needs = needs(Tag, Lines, PanelMap),
   Gap - Needs.



%% @doc Return vertical panel space required by lines

-spec lines_need(Tag      :: atom(),
                 Lines    :: list(),
                 PanelMap :: map()) -> integer().

lines_need(p, Lines, PanelMap) ->
   needs(p, Lines, PanelMap);

lines_need(Tag, Lines, PanelMap) ->
   Needs    = needs(Tag, Lines, PanelMap),
   TypeStyle = ep_panel:get_typestyle(PanelMap),
   Leading   = ep_typespec:leading(TypeStyle, Tag),
   Padding  = Leading * 2,
   Needs + Padding.


%% @doc lines_need/3 helper 

needs(Tag, Lines, PanelMap) ->
    TypeStyle = ep_panel:get_typestyle(PanelMap),
    Leading   = ep_typespec:leading(TypeStyle, Tag),
    Leading * length(Lines).


