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

-export([
    fit_xml/2,
    get_Xml/1,
    line_specs/2,
    parse_xml/1,
    rich_text/2,
    vacancies/2,
    xml2lines/2
]).

-include("ep_erltypes.hrl").


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
-spec parse_xml(TaggedText :: list()) -> eg_xml().  % [{xml, Tag, [], XML]
parse_xml(TaggedText) ->
    XML = eg_xml_lite:parse_all_forms(TaggedText),
    lists:reverse(XML).


%% @doc Return line widths and offsets for a given panel
-spec line_specs(Tag :: atom(), ep_panel()) -> {_Widths, _Offsets}.
line_specs(Tag, PanelMap) ->
    Measure   = ep_panel:get_measure(PanelMap),
    Margin    = ep_panel:get_margin(PanelMap),
    TypeStyle = ep_panel:get_typestyle(PanelMap),
    Indent    = ep_typespec:indent(TypeStyle, Tag),
    Vacancies = vacancies(Tag, PanelMap),

    DupFn = fun(N) -> lists:duplicate(Vacancies - 1, N) end,
    case Tag of
        p ->
            Widths1 = [Measure - Indent | DupFn(Measure)],
            Offsets1 = [Margin + Indent | DupFn(Margin)],
            {Widths1, Offsets1};
        br ->
            Widths2 = [Measure | DupFn(Margin)],
            Offsets2 = [Margin | DupFn(Margin)],
            {Widths2, Offsets2};
        ul ->
            Widths3 = [Measure - Indent | DupFn(Measure - Indent)],
            Offsets3 = [Margin | DupFn(Margin)],
            {Widths3, Offsets3};
        ol ->
            Widths4 = [Measure - Indent | DupFn(Measure - Indent)],
            Offsets4 = [Margin | DupFn(Margin)],
            {Widths4, Offsets4};
        cl ->
            Widths5 = [Measure - Indent | DupFn(Measure - Indent)],
            Offsets5 = [Margin | DupFn(Margin)],
            {Widths5, Offsets5};
        li ->
            Widths6 = [Measure - Indent | DupFn(Measure - Indent)],
            Offsets6 = [Margin + Indent | DupFn(Margin + Indent)],
            {Widths6, Offsets6};
        ci ->
            Widths7 = [Measure - Indent | DupFn(Measure - Indent)],
            Offsets7 = [Margin + Indent | DupFn(Margin + Indent)],
            {Widths7, Offsets7};

        _Other ->
            Widths = [Measure | DupFn(Margin)],
            Offsets = [Margin | DupFn(Margin)],
            {Widths, Offsets}
    end.


%% @doc Return number of lines that fit in panel 
-spec vacancies(Tag :: atom(), ep_panel()) -> integer().
vacancies(br, PanelMap) ->
    vacancies(p, PanelMap);

vacancies(Tag, PanelMap) ->
    TypeStyle = ep_panel:get_typestyle(PanelMap),
    ep_panel:get_available_lines(TypeStyle, Tag, PanelMap).


%% @doc fit xml copy into panel
-spec fit_xml(XML :: eg_xml(), PanelMap :: map()) -> tuple().
fit_xml(XML, PanelMap) ->
  fit_xml([], XML, PanelMap, continue).


%% @private
fit_xml(Content, [], PanelMap, _Continue) ->
   {Content, [], PanelMap};

fit_xml(Content, Spill, PanelMap, filled) ->
   {Content, Spill, PanelMap};

fit_xml(Content, XML, PanelMap, continue) ->
    [X | Spill] = XML,

    Xml = element(2, X),
    Tag = get_tag(Xml),
    {Lines, Size, Continue} = fit_lines(Tag, Xml, PanelMap),

    case Continue of
        true -> Content2 = [{Tag, Lines} | Content],
            PanelMap1 = ep_panel:update_content_cursor(Size, PanelMap),
            fit_xml(Content2, Spill, PanelMap1, continue);
        false -> fit_xml(Content, Spill, PanelMap, filled)
    end.  


get_Xml(XML) ->
   [X | _Spill] = XML,
   element(2, X).


%% @doc Copyfit Xml
-spec fit_lines(Tag :: atom(), Xml :: eg_xml(), ep_panel()) -> tuple().
fit_lines(_Tag, [], PanelMap) ->
    Lines = [],
    {Size, Continue} = space_required(br, Lines, PanelMap),
    {Lines, Size, Continue};

fit_lines(Tag, Xml, PanelMap) ->
    case Tag of
        ul ->
            Item = element(2, Xml),
            List = element(3, Item),
            Lines = get_rich_text(List, PanelMap);
        ol ->
            Item = element(2, Xml),
            List = element(3, Item),
            Lines = get_rich_text(List, PanelMap);
        cl ->
            Item = element(2, Xml),
            List = element(3, Item),
            Lines = get_rich_text(List, PanelMap);
        _ ->
            Lines = xml2lines(Xml, PanelMap)
    end,
    {Size, Continue} = space_required(Tag, Lines, PanelMap),
    {Lines, Size, Continue}.


-spec get_rich_text(list(), ep_panel()) -> list().
get_rich_text(List, PanelMap) ->
    [ep_xml_lib:rich_text(Item, PanelMap) || Item <- List].     


%% @doc Given content elment, return tag 
-spec get_tag(Xml :: eg_xmlform()) -> atom().
get_tag(Xml) ->
   element(1, Xml).


space_required(Tag, Lines, PanelMap) ->
    Available = ep_panel:get_available(PanelMap),
    TypeStyle = ep_panel:get_typestyle(PanelMap),
    Leading   = ep_typespec:leading(TypeStyle, Tag),
    Size      = length(Lines) * Leading,
    Continue  = Available >= Size,
    {Size, Continue}.
   

%% @doc Transform Xml into lines copyfitted into panel
-spec xml2lines(Xml :: eg_xmlform(), ep_panel()) -> list().
xml2lines(Xml, PanelMap) ->
    Tag = get_tag(Xml),
    RichText = rich_text(Xml, PanelMap),
    get_lines(Tag, RichText, PanelMap).


%% @doc rich_text/2 helper 
-spec rich_text(Xml :: eg_xmlform(), ep_panel()) -> list().
rich_text(Xml, PanelMap) ->
   TypeStyle         = ep_panel:get_typestyle(PanelMap),
   Tag               = element(1, Xml),
   FontMap           = ep_typespec:get_fontmap(TypeStyle, Tag),
   Norm              = normalise_xml(Xml, FontMap),
   {_, _, RichText}  = Norm,
   RichText.


%% @doc rich_text/2 helper 
-spec normalise_xml(Xml :: eg_xml(), FontMap :: list()) -> list().
normalise_xml(Xml, FontMap) ->
    eg_xml2richText:normalise_xml(Xml, FontMap).


%% @doc Transform xml to lines to fit panel
-spec get_lines(Tag :: atom(), RichText :: list(), ep_panel()) -> list().

get_lines(Tag, RichText, PanelMap) ->
    {Widths, _Offsets} = line_specs(Tag, PanelMap),
    TypeStyle   = ep_panel:get_typestyle(PanelMap),
    Justify     = ep_typespec:justify(TypeStyle, Tag),
    MaybeLines  = ep_line_break:break_richText(RichText, {Justify, Widths}),
    lines(MaybeLines).
            

%% @doc Verify that we have valid lines 
-spec lines(MaybeLines :: tuple() | impossible) -> list().
lines(impossible) ->
   io:format("Cannot break line; are widths ok?~n");

lines({Lines, _, _}) ->
    Lines.
