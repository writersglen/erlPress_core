%%% ==========================================================================
%%% ep_article.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:      MIT
%%%   File:         ep_article.erl
%%%   Description:  Copyfit text that spans panels within a page and
%%%                 across pages
%%%
%%%   NOTE: This is work under development. Stay tuned. 
%%% @end

%%% ==========================================================================


-module (ep_article).

-export([
    article/2,
    beads/3,
    run/0,
    top_off/6
]).

%%% ==========================================================================
%%% ==========================================================================
%%% article/2 - Copyfit article that spans list of panels (Beads)
%%% ==========================================================================
%%% ==========================================================================


%% @doc  Convert copy to XML and call article/3 
%%       NOTE: Copy is tagged text; Beads is a list of panels.

-spec article(Copy  :: list(),
              Beads :: list()) -> list().

article(Copy, Beads) ->
   XML   = ep_xml_lib:parse_xml(Copy),
   Paste = article([], XML, Beads),
   Paste.    % Next, display panel and paste-up copy


%%% ==========================================================================
%%% article/2 - Test harness 
%%% NOTE: For now, assumes copy fits into panel
%%% ==========================================================================


run() ->
   Copy     = ep_sample_text:article(),
   PanelMap = ep_panel:create({1, 1, test}, {72, 20}, {460, 705}),
   Beads    = [PanelMap],
   article(Copy, Beads).


%%% ==========================================================================
%%% article/2 - Helpers 
%%% ==========================================================================


%% @doc  Copyfit article that spans list of panels (Beads)
%% @private
-spec article(Paste :: list(),
              XML   :: list(),
              Beads :: list()) -> ok | no_space.

article(Paste, [], []) ->
   Paste; 

article(Paste, [], Beads) ->
   LeftOverBeads = length(Beads),
   io:format("~p More beads than needed~n~n", [LeftOverBeads]), 
   Paste;

article(Paste, XML, []) ->
   LeftOverXML = length(XML),
   io:format("~p lines spill over space available~n~n", [LeftOverXML]), 
   Paste;

article(Paste, XML, Beads) ->
   beads(Paste, XML, Beads).



%% @doc Recurse through Beads; copyfit as we go 

-spec beads(Paste :: list(),
            XML   :: list(),
            Beads :: list()) -> list().

beads(Paste, [], []) -> 
   article(Paste, [], []);

beads(Paste, [], Beads) -> 
   article(Paste, [], Beads);

beads(Paste, XML, []) -> 
   article(Paste, XML, []);
   
beads(Paste, XML, Beads) ->
   [PanelMap | MoreBeads] = Beads,
   Gap =    ep_copyfit:gap(PanelMap),
   Paste1 = panel(Paste, Gap, XML, PanelMap),
   case MoreBeads == [] of
      true  -> beads(Paste1, [], []);
      false -> beads([Paste1 | Paste], [], MoreBeads)
   end.
    

%%% ==========================================================================
%%% beads/3 - Helpers 
%%% ==========================================================================



%% @doc Recurse through Xml; accumulate lines as we go
%%      NOTE: Paste is a list of copy elements in rich_text format
%%            Gap is space between last rich_text copy element and
%%            the bottom of the panel. XML is a list copy elements; 
%%            e.g. Xml, PanelMap specifies a panel. 
%% @private
-spec panel(Paste      :: list(),
            Gap        :: integer(),
            XML        :: list(),
            PanelMap   :: map()) -> list().

panel(Paste, _Gap, [], _PanelMap) ->
   Paste;

panel(Paste, Gap, XML, PanelMap) ->
   [X | MoreXML]        = XML,
   Xml                  = element(2, X),
   Tag                  = ep_copyfit:get_tag(Xml),
   Lines                = ep_copyfit:get_lines(Tag, Xml, PanelMap),
   SpaceAvailable       = Gap >= 0,
   case SpaceAvailable of
      true  -> Paste1   = [Lines | Paste],
               Gap1     = ep_copyfit:close_gap(Gap, Tag, Lines, PanelMap),
               panel(Paste1, Gap1, MoreXML, PanelMap); 
      false -> top_off(Paste, Tag, Lines, Gap, Xml, PanelMap)
   end.



%%% ==========================================================================
%%% ==========================================================================
%%% NOTE: All functions from here down support copyfitting across beads and 
%%%       are sill under development
%%% ==========================================================================
%%% ==========================================================================


%% @doc Reduce or elminiate gap at bottom of panel
%%      NOTE: top_off/5 is still under devlelpment
%%            It's only called wehn copy exceeds space
%%            in panel
%%      ISSUES to be resolved: 
%%             Last line in panel can't be hyphenated
%%             Last line in panel can't be headline 
%%             Spillover lines need to be converted back
%%             into XML and appended to yet-to-processed XML  

-spec top_off(Paste         :: list(),
              Tag           :: atom(),
              Lines         :: list(),
              Gap           :: integer(),
              Xml           :: list(),
              PanelMap      :: map()) -> map().

top_off(Paste, Tag, Lines, Gap, Xml, PanelMap) ->
   io:format("top-off/5 - Lines: ~p~n~n", [Lines]),
   WillFit  = ep_panel:will_fit(Tag, Lines, PanelMap),
   case WillFit > 0 of
      true   -> Spill  = Xml,   % backfill/N
                Gap1   = Gap,
                Paste1 = Paste;
      false  -> Spill  = Xml,   % shim/N
                Gap1   = Gap,
                Paste1 = Paste
    end,
    {Paste1, Gap1, Spill}.
   
   
%%% ==========================================================================
%%% Paste functions 
%%% ==========================================================================


% paste_panel(_Paste, Gap, PanelMap) -> 
%   Available = gap(PanelMap),
%   io:format("Available space: ~p~n", [Available]),
%   Pixels    = Available - Gap,
%   Cursor1   = ep_panel:get_content_cursor(PanelMap),
%   io:format("Cursor1: ~p~n", [Cursor1]),
%   PanelMap1 = ep_panel:update_content_cursor(Pixels, PanelMap),
%   Cursor2   = ep_panel:get_content_cursor(PanelMap1),
%   io:format("Cursor2: ~p~n", [Cursor2]),
   % Paste panel
   % paste copy
%   NameString = ep_panel:get_name_string(PanelMap1),
%   Gap1      = gap(PanelMap1),
%   io:format("Pasting panel ~p~n", [NameString]),
%   io:format("Gap: ~p~n~n", [Gap1]),
%   ok.



%% ep_paste_lib:paste_panel/3
%% ep_paste_lib:paste_copy/3
%% ep_paste_lib:paste/4

%% Need to explicate Cost parameter
