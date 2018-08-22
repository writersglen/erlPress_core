%%% ==========================================================================
%%% ep_paste_lib.erl

%%% @author     aloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:      MIT
%%%   File:         ep_paste_lib.erl 
%%%   Description:  XML-to-PDF paste up
%%% @end

%%% ==========================================================================



-module (ep_paste_lib).

% -export([create/3, one_line/4]).
% -export ([text_lines/3, next_text_line/4, end_text_lines/1]). 
% -export([test_sample/2]).

-compile(export_all).

-define(TEXT_COLOR, black).


%% ***********************************************************
%% ***********************************************************
%% Paste functions 
%% ***********************************************************
%% ***********************************************************


%% ***********************************************************
%% paste_panel/3  
%% ***********************************************************


%% @doc Paste panel

-spec paste_panel(PDF   :: identifier(),
                  Job   :: map(),
                  PanelMap :: map()) -> ok.

paste_panel(PDF, Job, PanelMap) ->
    ep_panel:panel(PDF, Job, PanelMap).


%% ***********************************************************
%% paste_copy/3 - 
%%
%% NOTE: Gap parameter supports articles-and-beads which is
%%       on the roadmap   
%% ***********************************************************


%% @doc Paste copy; write out PDF 


paste_copy(Paste, Gap, PanelMap) ->
    PDF       = eg_pdf:new(),
    Job       = ep_job:create("Trial Paste", "LRP"),
    NameString = "text",
    ok        = paste_panel(PDF, Job, PanelMap),

    ok        = paste(PDF, Paste, Gap, PanelMap),
    OutFile   = "./pdf/galleys/" ++
                 ?MODULE_STRING ++
                 "_" ++
                 NameString ++
                 "_test.pdf",
    save(PDF, OutFile).



%% @doc Paste copy; write out PDF 


paste_copy(Paste, Gap, PanelMap, NameString) ->
    PDF       = eg_pdf:new(),
    Job       = ep_job:create("Trial Paste", "LRP"),
    ok        = paste_panel(PDF, Job, PanelMap),

    ok        = paste(PDF, Paste, Gap, PanelMap),
    OutFile   = "./pdf/galleys/" ++
                 ?MODULE_STRING ++
                 "_" ++
                 NameString ++
                 "_test.pdf",
    save(PDF, OutFile).


%% ***********************************************************
%% paste/4 
%% ***********************************************************


%% @doc Paste text elements into panel

-spec paste(PDF      :: identifier(),
            Paste    :: list(),
            Gap      :: integer(),
            PanelMap :: map()) -> ok.


paste(PDF, Paste, Gap, PanelMap) ->
   io:format("Entering paste/4 - Paste: ~p~n~n", [Paste]),
    paste_elements(PDF, Paste, Gap, PanelMap).


paste_elements(_PDF, [], _Gap,  _PanelMap) ->
    ok;

paste_elements(PDF, Paste, Gap, PanelMap) ->
   io:format("Entering paste_elements/4 - Paste: ~p~n~n", [Paste]),
    [Paste1 | MorePaste] = Paste,
    Tag                = element(1, Paste1),
    Lines              = element(2, Paste1),
    WillFit   = ep_panel:will_fit(Tag, Lines, PanelMap), 
    case WillFit of
       true  -> {Gap1, PanelMap1} = paste_up(PDF, Tag, Lines, Gap, PanelMap),
                Paste2             = MorePaste,
                % Recurse
                paste_elements(PDF, Paste2, Gap1, PanelMap1);
       false -> paste_elements(PDF, [], Gap, PanelMap)
     end.


%% ***********************************************************
%% paste/4 helpers
%% ***********************************************************


%% @doc Paste up content element 

-spec paste_up(PDF            :: identifier(),
               Tag            :: atom(),
               Lines          :: list(),
               Gap            :: tuple(),
               PanelMap       :: map()) -> tuple().

paste_up(PDF, Tag, Lines, Gap, PanelMap) ->
   io:format("Entering paste_up/4 - Lines: ~p~n~n", [Lines]),
   io:format("Entering paste_up/4 - Gap: ~p~n~n", [Gap]),
    {Gap1, PanelMap1} = paste_lines(PDF, Tag, Lines, Gap, PanelMap),
    ok                 = paste_if_ci(PDF, Tag, PanelMap),
    {Gap1, PanelMap1}.


%% @doc Paste lines into panel

-spec paste_lines(PDF     :: identifier(),
                  Tag     :: atom(),
                  Lines   :: list(),
                  Gap     :: integer(),
                  PanelMap :: map()) -> tuple().

paste_lines(_PDF, br, _Lines, Gap, PanelMap) ->
    {Gap, PanelMap};

paste_lines(PDF, ul, Lines, Gap, PanelMap) ->
    PanelMap1          = maybe_line_space(ul, PanelMap),
    PanelMap2          = paste_li_list(PDF, ul, Lines, PanelMap1),
    {Gap, PanelMap2};
    

paste_lines(PDF, Tag, Lines, Gap, PanelMap) ->
    {Gap1, PanelMap1}  = maybe_adjust_gap(Gap, PanelMap),
   io:format("Entering paste_lines/5 - Gap1: ~p~n", [Gap1]),
    {Widths, Offsets}  = ep_xml_lib:line_specs(Tag, PanelMap1),
    PanelMap2          = maybe_line_space(Tag, PanelMap1),
    ok                 = maybe_paste_li_symbol(PDF, Tag, PanelMap2),
   io:format("Entering paste_lines/5 - Lines: ~p~n", [Lines]),
   io:format("Entering paste_lines/5 - Tag: ~p~n", [Tag]),
    Code               = pdf_code(PDF, Tag, Lines, Widths, Offsets, PanelMap2),
    ok                 = paste(PDF, Code),
    PanelMap3          = ep_panel:update_panel(Tag, Lines, PanelMap2),
    PanelMap4          = maybe_line_space(Tag, PanelMap3),
    {Gap1, PanelMap4}.


paste_li_list(_PDF, ul, [], PanelMap) ->
   PanelMap;

paste_li_list(PDF, ul, Lines, PanelMap) ->
    [Linez | Rest]  = Lines,
    ok              = paste_li(PDF, ul, Linez, PanelMap),
    TypeStyle       = ep_panel:get_typestyle(PanelMap),
    Leading         = ep_typespec:leading(TypeStyle, li),
    PanelMap1       = ep_panel:update_content_cursor(Leading, PanelMap),
    paste_li_list(PDF, ul, Rest, PanelMap1).
   
    

paste_li(PDF, Tag, Lines, PanelMap) ->
    {Widths, Offsets}  = ep_xml_lib:line_specs(Tag, PanelMap),
    ok                 = maybe_paste_li_symbol(PDF, Tag, PanelMap),
    Code               = pdf_code(PDF, Tag, [Lines], Widths, Offsets, PanelMap),
    ok                 = paste(PDF, Code).



maybe_line_space(Tag, PanelMap) ->
   case Tag of
      ul  -> TypeStyle = ep_panel:get_typestyle(PanelMap),
             ep_panel:one_line_space(ul, TypeStyle, PanelMap);
      _   -> PanelMap
   end.


maybe_paste_li_symbol(PDF, Tag, PanelMap) ->
   case Tag of
      ul -> paste_dot(PDF, PanelMap);
      _  -> ok
   end.


paste_dot(PDF, PanelMap) ->
   Margin     = ep_panel:get_margin(PanelMap),
   TypeStyle  = ep_panel:get_typestyle(PanelMap),
   LiFill     = ep_panel:get_li_fill(PanelMap),
   Indent     = ep_typespec:indent(TypeStyle, p) div 2,
   Radius     = 2,
   Diff       = ep_typespec:leading(TypeStyle, ul) - Radius,
   {X, Y}     = ep_panel:get_text_position(PanelMap),
   TextX      = X + Margin + (2 * Indent),
   TextY      = Y - Diff,
   eg_pdf:save_state(PDF),
   eg_pdf:set_line_width(PDF, 1),
   eg_pdf:set_dash(PDF, solid),
   eg_pdf:set_stroke_color(PDF, black),
   eg_pdf:set_fill_color(PDF, LiFill),
   eg_pdf:circle(PDF, {TextX, TextY}, Radius),
   eg_pdf:path(PDF, fill_stroke),
   eg_pdf:restore_state(PDF),
   ok.



paste_if_ci(PDF, Tag, PanelMap) ->
   TypeStyle = ep_panel:get_typestyle(PanelMap),
   Size      = ep_typespec:fontsize(TypeStyle, Tag) * 0.80,
   Margin    = ep_panel:get_margin(PanelMap),
   Diff      = ep_typespec:leading(TypeStyle, Tag),
   case Tag of
      ci  -> {TextX, TextY} = ep_panel:get_text_position(PanelMap),
             DiffX = TextX + Margin,
             DiffY = TextY  - Diff,
             eg_pdf:save_state(PDF),
             eg_pdf:set_line_width(PDF, 1),
             eg_pdf:set_dash(PDF, solid),
             eg_pdf:set_stroke_color(PDF, black),
             eg_pdf:set_fill_color(PDF, white),
             eg_pdf:rectangle(PDF, {DiffX, DiffY}, {Size, Size}),
             eg_pdf:path(PDF, fill_stroke),
             eg_pdf:restore_state(PDF),
             ok;
        _  -> ok
    end.

%% @doc Transform lines to PDF code

-spec pdf_code(PDF            :: identifier(),
               Tag            :: atom(),
               Lines          :: list(),
               Widths         :: list(),
               Offsets        :: list(),
               PanelMap       :: map()) -> string().


pdf_code(PDF, Tag, Lines, Widths, Offsets, PanelMap) ->
   {TextX, TextY}                       = text_placement(Tag, PanelMap),
   Rot                                  = ep_panel:get_rot(PanelMap),
   TypeStyle                            = ep_panel:get_typestyle(PanelMap),
   Justify                              = ep_typespec:justify(TypeStyle, Tag),
   Leading                              = ep_typespec:leading(TypeStyle, Tag),
   Code = ep_richText2pdf:richText2pdf(PDF,
                                       TextX,
                                       TextY,
                                       Justify,
                                       Rot,
                                       Lines,
                                       Leading,
                                       Widths,
                                       Offsets),
    Code.



text_placement(ul, PanelMap) ->
   Indent = ep_panel:get_indent(PanelMap),
   {X, Y} =  ep_panel:get_text_position(PanelMap),
   {X + (2 * Indent) , Y};

text_placement(_Tag, PanelMap) ->
    ep_panel:get_text_position(PanelMap).


%% @doc Append PDF code to text stream in PDF

-spec paste(PDF  :: identifier(),
            Code :: string()) -> ok.

paste(PDF, Code) ->
  eg_pdf:begin_text(PDF),
  eg_pdf:append_stream(PDF, Code),
  eg_pdf:end_text(PDF),
  ok.



save(PDF, OutFile) ->
    ep_job:save_job(PDF, OutFile).


save(PDF) ->
    OutFile = "./pdf/galleys/" ++ ?MODULE_STRING ++ ".trial.pdf",
    ep_job:save_job(PDF, OutFile).


%%% ==========================================================================
%%% Spill functions 
%%% ==========================================================================



maybe_adjust_gap(Gap, PanelMap) ->
    case  Gap of
       []   -> Gap1              = [],
               PanelMap1          = PanelMap;
       _    -> {Gap1, PanelMap1} = adjust_gap(-1, Gap, PanelMap)
    end,
   {Gap1, PanelMap1}.


adjust_gap(_Adjust, [], PanelMap) ->
    {0, PanelMap};

adjust_gap(Adjust, Gap, PanelMap) ->
    Gap1      = new_gap(Adjust, Gap),
    PanelMap1 = move_content_cursor(Adjust, PanelMap),
    {Gap1, PanelMap1}.


new_gap(Gap, Adjust) ->
    Gap + Adjust.

move_content_cursor(Adjust, PanelMap) ->
    ep_panel:update_content_cursor(Adjust, PanelMap).

