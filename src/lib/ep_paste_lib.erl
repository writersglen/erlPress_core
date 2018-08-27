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
%% paste/4 
%% ***********************************************************


%% @doc Paste text elements into panel

-spec paste(PDF      :: identifier(),
            Paste    :: list(),
            Gap      :: integer(),
            PanelMap :: map()) -> ok.


paste(PDF, Paste, Gap, PanelMap) ->
    paste_elements(PDF, Paste, Gap, PanelMap).


paste_elements(_PDF, [], _Gap,  _PanelMap) ->
    ok;

paste_elements(PDF, Paste, Gap, PanelMap) ->
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
    {Gap1, PanelMap1} = paste_lines(PDF, Tag, Lines, Gap, PanelMap),
    {Gap1, PanelMap1}.


%% @doc Paste lines into panel

-spec paste_lines(PDF     :: identifier(),
                  Tag     :: atom(),
                  Lines   :: list(),
                  Gap     :: integer(),
                  PanelMap :: map()) -> tuple().

paste_lines(_PDF, br, _Lines, Gap, PanelMap) ->
    TypeStyle = ep_panel:get_typestyle(PanelMap),
    Leading   = ep_typespec:leading(TypeStyle, br),
    PanelMap1 = ep_copyfit:move_content_cursor(Leading, PanelMap), 
    {Gap, PanelMap1};

paste_lines(PDF, ul, Lines, Gap, PanelMap) ->
    PanelMap1          = maybe_line_space(ul, PanelMap),
    PanelMap2          = paste_li_list(PDF, ul, Lines, PanelMap1),
    {Gap, PanelMap2};
    
paste_lines(PDF, ol, Lines, Gap, PanelMap) ->
    PanelMap1          = maybe_line_space(ol, PanelMap),
    PanelMap2          = paste_ol_list(PDF, ol, Lines, 1, PanelMap1),
    {Gap, PanelMap2};
    
paste_lines(PDF, cl, Lines, Gap, PanelMap) ->
    PanelMap1          = maybe_line_space(cl, PanelMap),
    PanelMap2          = paste_li_list(PDF, cl, Lines, PanelMap1),
    {Gap, PanelMap2};
    

paste_lines(PDF, Tag, Lines, Gap, PanelMap) ->
    {Gap1, PanelMap1}  = maybe_adjust_gap(Gap, PanelMap),
    {Widths, Offsets}  = ep_xml_lib:line_specs(Tag, PanelMap1),
    PanelMap2          = maybe_line_space(Tag, PanelMap1),
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
    ok              = maybe_paste_item_symbol(PDF, ul, PanelMap),
    TypeStyle       = ep_panel:get_typestyle(PanelMap),
    Leading         = ep_typespec:leading(TypeStyle, li),
    PanelMap1       = ep_panel:update_content_cursor(Leading, PanelMap),
    paste_li_list(PDF, ul, Rest, PanelMap1);
   
    
paste_li_list(_PDF, cl, [], PanelMap) ->
   PanelMap;

paste_li_list(PDF, cl, Lines, PanelMap) ->
    [Linez | Rest]  = Lines,
    ok              = paste_li(PDF, cl, Linez, PanelMap),
    ok              = maybe_paste_item_symbol(PDF, cl, PanelMap),
    TypeStyle       = ep_panel:get_typestyle(PanelMap),
    Leading         = ep_typespec:leading(TypeStyle, li),
    PanelMap1       = ep_panel:update_content_cursor(Leading, PanelMap),
    paste_li_list(PDF, cl, Rest, PanelMap1).
   

paste_li(PDF, Tag, Lines, PanelMap) ->
    {Widths, Offsets}  = ep_xml_lib:line_specs(Tag, PanelMap),
    Code               = pdf_code(PDF, Tag, [Lines], Widths, Offsets, PanelMap),
    ok                 = paste(PDF, Code).


paste_ol_list(_PDF, ol, [], _Index, PanelMap) ->
   PanelMap;

paste_ol_list(PDF, ol, Lines, Index, PanelMap) ->
    [Linez | Rest]  = Lines,
    ok              = paste_li(PDF, ul, Linez, PanelMap),
    ok              = maybe_paste_index(PDF, ol, Index, PanelMap),
    TypeStyle       = ep_panel:get_typestyle(PanelMap),
    Leading         = ep_typespec:leading(TypeStyle, li),
    PanelMap1       = ep_panel:update_content_cursor(Leading, PanelMap),
    Index1          = Index + 1, 
    paste_ol_list(PDF, ol, Rest, Index1, PanelMap1).
   

maybe_line_space(Tag, PanelMap) ->
   case Tag of
      ul  -> TypeStyle = ep_panel:get_typestyle(PanelMap),
             ep_panel:one_line_space(ul, TypeStyle, PanelMap);
      ol  -> TypeStyle = ep_panel:get_typestyle(PanelMap),
             ep_panel:one_line_space(ol, TypeStyle, PanelMap);
      cl  -> TypeStyle = ep_panel:get_typestyle(PanelMap),
             ep_panel:one_line_space(cl, TypeStyle, PanelMap);
      _   -> PanelMap
   end.


maybe_paste_item_symbol(PDF, Tag, PanelMap) ->
   case Tag of
      ul -> paste_dot(PDF, PanelMap);
      cl -> paste_checkbox(PDF, Tag, PanelMap);
      _  -> ok
   end.


maybe_paste_index(PDF, Tag, Index, PanelMap) ->
   case Tag of
      ol -> paste_index(PDF, Tag, Index, PanelMap);
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


 paste_index(PDF, Tag, Index, PanelMap) ->
   TypeStyle = ep_panel:get_typestyle(PanelMap),
   Size      = ep_typespec:fontsize(TypeStyle, Tag),
   Indent    = (Size div 2),
   Indent1   = ol_indent(Index, Indent),
   Diff      = ep_typespec:leading(TypeStyle, Tag),
   Index1    = integer_to_list(Index) ++ ".",
   {X, Y}    = ep_panel:get_text_position(PanelMap),
   TextX      = X + Indent1,
   TextY      = Y - Diff,
   eg_pdf:save_state(PDF),
   eg_pdf:begin_text(PDF),
   eg_pdf:set_font(PDF, "Helvetica", Size),
   eg_pdf:set_text_pos(PDF, TextX, TextY),
   eg_pdf:text(PDF, Index1),
   eg_pdf:end_text(PDF),
   eg_pdf:restore_state(PDF),
   ok.


ol_indent(Index, Indent) when Index < 10 ->
    Gutter = 15,
    (Indent * 3) + Gutter;

ol_indent(Index, Indent) when Index < 100 ->
    Gutter = 15,
    (Indent * 2) + Gutter;

ol_indent(_Index, Indent) ->
    Gutter = 15,
    Indent + Gutter.



paste_checkbox(PDF, Tag, PanelMap) ->
   TypeStyle = ep_panel:get_typestyle(PanelMap),
   Size      = ep_typespec:fontsize(TypeStyle, Tag) * 0.80,
   Margin    = ep_panel:get_margin(PanelMap),
   Diff      = ep_typespec:leading(TypeStyle, Tag),
   case Tag of
       cl -> {TextX, TextY} = ep_panel:get_text_position(PanelMap),
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

text_placement(ol, PanelMap) ->
   Indent = ep_panel:get_indent(PanelMap),
   {X, Y} =  ep_panel:get_text_position(PanelMap),
   {X + (2 * Indent) , Y};

text_placement(cl, PanelMap) ->
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
    PanelMap1 = ep_copyfit:move_content_cursor(Adjust, PanelMap),
    {Gap1, PanelMap1}.


new_gap(Gap, Adjust) ->
    Gap + Adjust.



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
