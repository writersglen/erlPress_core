%%% ==========================================================================
%%% ep_xml_lib.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:      MIT
%%%   File:         ep_xml_lib.erl
%%%   Description:  XML functions 
%%% @end

%%% ==========================================================================


-module (ep_xml_lib).

% -export([parse_xml/1]).
% -export([fit_xml/2, line_specs/2, get_tag/1, impose_cost/3, vacancies/2]). 
% -export([reduce_cost/2, reposition_content_cursor/2]).


-compile(export_all).


%% ***********************************************************
%% ***********************************************************
%% XML functions 
%%
%% NOTE: XML is a list of Xml content elements.
%%       Xml is an xml-tagged content elment.
%%
%% XML:   [{xml,{h1,[],[{raw,"erlPress_core.01"}]}},...]
%% Xml:   {xml,{p,[], [{raw,"User applications...}
%% ***********************************************************
%% ***********************************************************

%% @doc Return XML

-spec parse_xml(TaggedText :: list()) -> list().  % [{xml, Tag, [], XML]

parse_xml(TaggedText) ->
    XML = eg_xml_lite:parse_all_forms(TaggedText),
    lists:reverse(XML).


%% ***********************************************************
%% fit_xml/1 - Copyfit XML 
%% ***********************************************************


%% @doc fit xml copy into panel

-spec fit_xml(XML      :: list(),
              PanelMap :: map()) -> tuple().

fit_xml(XML, PanelMap) ->
   fit_xml([], XML, PanelMap, continue).


fit_xml(Content, [], PanelMap, _Continue) ->
    {Content, [], PanelMap};


fit_xml(Content, Spill, PanelMap, filled) ->
    {Content, Spill, PanelMap};


fit_xml(Content, XML, PanelMap, continue) ->
     [X | Spill] = XML,
     Xml                        = element(2, X),
     Tag                        = get_tag(Xml),
     {Lines, Size, Continue}    = fit(Xml, Tag, PanelMap),
     case Continue of
        true  -> Content2 = [{Tag, Lines} | Content],
                 PanelMap1 = ep_panel:update_content_cursor(Size, PanelMap),  
                 fit_xml(Content2, Spill, PanelMap1, continue);
        false -> fit_xml(Content, Spill, PanelMap, filled)
     end.  


%% ***********************************************************
%% fit_xml/1 - helpers
%% ***********************************************************

%% @doc Copyfit Xml

-spec fit(Xml     :: tuple(),
          Tag     :: atom(),
          PanelMap :: map()) -> tuple().

fit(_Xml, br, PanelMap) ->
    Lines      = [],
    Available  = ep_panel:get_available(PanelMap),
    TypeStyle  = ep_panel:get_typestyle(PanelMap),
    Size       = ep_typespec:fontsize(TypeStyle, br),
    Continue   = Available >= Size,
    {Lines, Size, Continue};

    
fit(Xml, Tag, PanelMap) -> 
     Lines        = xml2lines(Xml, PanelMap),
     io:format(" $$$$$$$$$$$$ Length Lines:: ~p~n", [length(Lines)]),
     Available    = ep_panel:get_available(PanelMap),
     io:format(" $$$$$$$$$$$$ Available: ~p~n", [Available]),
     TypeStyle    = ep_panel:get_typestyle(PanelMap),
     io:format(" $$$$$$$$$$$$ TypeStyle: ~p~n", [TypeStyle]),
     Leading      = ep_typespec:leading(TypeStyle, Tag),
     io:format(" $$$$$$$$$$$$ Leading: ~p~n", [Leading]),
     Size         = length(Lines) * Leading,
     io:format(" $$$$$$$$$$$$ Size: ~p~n~n", [Size]),
     Continue     = Available >= Size,
     {Lines, Size, Continue}.


%% @doc Given content elment, return tag 

-spec get_tag(Xml :: tuple()) -> atom().

get_tag(Xml) ->
   element(1, Xml).



%% @doc Transform Xml into lines copyfitted into panel

-spec xml2lines(Xml      :: tuple(),
                PanelMap :: map()) -> list().

xml2lines(Xml, PanelMap) ->
   Tag      = get_tag(Xml),
   RichText = rich_text(Xml, PanelMap),
   get_lines(Tag, RichText, PanelMap).



%% @doc rich_text/2 helper 

-spec rich_text(Xml      :: tuple(),
                PanelMap :: map()) -> list().

rich_text(Xml, PanelMap) ->
   TypeStyle         = ep_panel:get_typestyle(PanelMap),
   Tag               = element(1, Xml),
   FontMap           = ep_typespec:get_fontmap(TypeStyle, Tag),
   Norm              = normalise_xml(Xml, FontMap),
   {_, _, RichText}  = Norm,
   RichText.


%% @doc rich_text/2 helper 

-spec normalise_xml(Xml :: list(), FontMap :: list()) -> list().

normalise_xml(Xml, FontMap) ->
    eg_xml2richText:normalise_xml(Xml, FontMap).



%% @doc Transform xml to lines to fit panel

-spec get_lines(Tag      :: atom(),
                RichText :: list(),
                PanelMap :: list()) -> list().

get_lines(Tag, RichText, PanelMap) ->
   {Widths, _Offsets} = line_specs(Tag, PanelMap),
   TypeStyle          = ep_panel:get_typestyle(PanelMap),
   Justify            = ep_typespec:justify(TypeStyle, Tag),
   MaybeLines         = ep_line_break:break_richText(RichText, {Justify, Widths}),
   Lines              = lines(MaybeLines),
   Lines.
            

%% @doc Return number of lines that fit in panel 

-spec vacancies(Tag      :: list(),
                PanelMap :: map()) -> integer().

vacancies(br, PanelMap) ->
   vacancies(p, PanelMap);

vacancies(Tag, PanelMap) ->
   TypeStyle = ep_panel:get_typestyle(PanelMap),
   ep_panel:get_available_lines(TypeStyle, Tag, PanelMap).


%% @doc Return line widths and offsets for a given panel

-spec line_specs(Tag         :: atom(),
                 PanelMap    :: map()) -> tuple().

line_specs(Tag, PanelMap) ->
    Measure   = ep_panel:get_measure(PanelMap),
    Margin    = ep_panel:get_margin(PanelMap),
    TypeStyle = ep_panel:get_typestyle(PanelMap),
    Indent    = ep_typespec:indent(TypeStyle, Tag),
    Vacancies = vacancies(Tag, PanelMap),
    case Tag of
        p     -> Widths  = [Measure - Indent|lists:duplicate(Vacancies - 1, Measure)],
                 Offsets = [Margin + Indent|lists:duplicate(Vacancies - 1, Margin)];
        br    -> Widths  = [Measure|lists:duplicate(Vacancies - 1, Margin)],
                 Offsets = [Margin|lists:duplicate(Vacancies - 1, Margin)];
        li    -> Widths  = [Measure - Indent|lists:duplicate(Vacancies - 1, Measure - Indent)],
                 Offsets = [Margin + Indent|lists:duplicate(Vacancies - 1, Margin + Indent)];
        ci    -> Widths  = [Measure - Indent|lists:duplicate(Vacancies - 1, Measure - Indent)],
                 Offsets = [Margin + Indent|lists:duplicate(Vacancies - 1, Margin + Indent)];
        _     -> Widths  = [Measure|lists:duplicate(Vacancies - 1, Margin)],
                 Offsets = [Margin|lists:duplicate(Vacancies - 1, Margin)]
    end,
    {Widths, Offsets}.


impose_cost(_Adjust, 0, PanelMap) ->
    {0, PanelMap};

impose_cost(Adjust, Cost, PanelMap) ->
    Cost1     = reduce_cost(Adjust, Cost),
    PanelMap1 = reposition_content_cursor(Adjust, PanelMap),
    {Cost1, PanelMap1}.


reduce_cost(Cost, Adjust) ->
    Cost + Adjust.

reposition_content_cursor(Adjust, PanelMap) ->
    Cursor  = ep_panel:get_content_cursor(PanelMap),
    Cursor1 = Cursor + Adjust,
    ep_panel:update_content_cursor(Cursor1, PanelMap).


%% @doc Verify that we have valid lines 

-spec lines(MaybeLines :: tuple()) -> list().

lines(impossible) ->
   io:format("Cannot break line; are widths ok?~n");

lines({Lines, _, _}) ->
    Lines.



