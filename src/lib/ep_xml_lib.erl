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
% -export([fit_xml/2, line_specs/2, vacancies/2]). 


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
        ul    -> Widths  = [Measure - Indent|lists:duplicate(Vacancies - 1, Measure - Indent)],
                 Offsets = [Margin + Indent|lists:duplicate(Vacancies - 1, Margin + Indent)];
        li    -> Widths  = [Measure - Indent|lists:duplicate(Vacancies - 1, Measure - Indent)],
                 Offsets = [Margin + Indent|lists:duplicate(Vacancies - 1, Margin + Indent)];
        ci    -> Widths  = [Measure - Indent|lists:duplicate(Vacancies - 1, Measure - Indent)],
                 Offsets = [Margin + Indent|lists:duplicate(Vacancies - 1, Margin + Indent)];
        _     -> Widths  = [Measure|lists:duplicate(Vacancies - 1, Margin)],
                 Offsets = [Margin|lists:duplicate(Vacancies - 1, Margin)]
    end,
    {Widths, Offsets}.



%% @doc Return number of lines that fit in panel 

-spec vacancies(Tag      :: list(),
                PanelMap :: map()) -> integer().

vacancies(br, PanelMap) ->
   vacancies(p, PanelMap);

vacancies(Tag, PanelMap) ->
   TypeStyle = ep_panel:get_typestyle(PanelMap),
   ep_panel:get_available_lines(TypeStyle, Tag, PanelMap).




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
    {Lines, Size, Continue}    = fit_lines(Tag, Xml, PanelMap),


    case Continue of
       true  -> Content2 = [{Tag, Lines} | Content],
                PanelMap1 = ep_panel:update_content_cursor(Size, PanelMap),  
                fit_xml(Content2, Spill, PanelMap1, continue);
       false -> fit_xml(Content, Spill, PanelMap, filled)
    end.  


get_Xml(XML) ->
   [X | _Spill] = XML,
   element(2, X).




%% ***********************************************************
%% fit_xml/1 - helpers
%% ***********************************************************

%% @doc Copyfit Xml

-spec fit_lines(Tag      :: atom(),
                Xml      :: tuple(),
                PanelMap :: map()) -> tuple().

fit_lines(_Tag, [], PanelMap) ->
    Lines      = [],
    {Size, Continue} = space_required(br, Lines, PanelMap),
    {Lines, Size, Continue};


fit_lines(Tag, Xml, PanelMap) ->
   case Tag of
      ul  ->  Item  = element(2, Xml),
              List  = element(3, Item),
              Lines = get_rich_text(List, PanelMap);
      _    -> Lines = xml2lines(Xml, PanelMap)
   end,
   {Size, Continue}  = space_required(Tag, Lines, PanelMap),
   {Lines, Size, Continue}.


get_rich_text(List, PanelMap) ->
    [ep_xml_lib:rich_text(Item, PanelMap) || Item <- List].     


%% @doc Given content elment, return tag 

-spec get_tag(Xml :: tuple()) -> atom().

get_tag(Xml) ->
   element(1, Xml).




space_required(Tag, Lines, PanelMap) ->
   Available    = ep_panel:get_available(PanelMap),
   TypeStyle    = ep_panel:get_typestyle(PanelMap),
   Leading      = ep_typespec:leading(TypeStyle, Tag),
   Size         = length(Lines) * Leading,
   Continue     = Available >= Size,
   {Size, Continue}.
   


   
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
            





%% @doc Verify that we have valid lines 

-spec lines(MaybeLines :: tuple()) -> list().

lines(impossible) ->
   io:format("Cannot break line; are widths ok?~n");

lines({Lines, _, _}) ->
    Lines.



