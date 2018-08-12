%%% ==========================================================================
%%% ep_paste_lib.erl

%%% @author     Lloyd R. Prentice
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
%% NOTE: Cost parameter supports articles-and-beads which is
%%       on the roadmap   
%% ***********************************************************


%% @doc Paste copy; write out PDF 


paste_copy(Paste, Cost, PanelMap) ->
    PDF       = eg_pdf:new(),
    Job       = ep_job:create("Trial Paste", "LRP"),
    NameString = "text",
    ok        = paste_panel(PDF, Job, PanelMap),

    ok        = paste(PDF, Paste, Cost, PanelMap),
    OutFile   = "./pdf/galleys/" ++
                 ?MODULE_STRING ++
                 "_" ++
                 NameString ++
                 "_test.pdf",
    save(PDF, OutFile).



%% @doc Paste copy; write out PDF 


paste_copy(Paste, Cost, PanelMap, NameString) ->
    PDF       = eg_pdf:new(),
    Job       = ep_job:create("Trial Paste", "LRP"),
    ok        = paste_panel(PDF, Job, PanelMap),

    ok        = paste(PDF, Paste, Cost, PanelMap),
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
            Cost     :: integer(),
            PanelMap :: map()) -> ok.


paste(PDF, Paste, Cost, PanelMap) ->
    paste_elements(PDF, Paste, Cost, PanelMap).


paste_elements(_PDF, [], _Cost,  _PanelMap) ->
    ok;

paste_elements(PDF, Paste, Cost, PanelMap) ->
    [Paste1 | MorePaste] = Paste,
    Tag                = element(1, Paste1),
    Lines              = element(2, Paste1),
    WillFit   = ep_panel:will_fit(Tag, Lines, PanelMap), 
    case WillFit of
       true  -> {Cost1, PanelMap1} = paste_up(PDF, Tag, Lines, Cost, PanelMap),
                PanelMap2          = ep_panel:update_panel(Tag, Lines, PanelMap1),
                Paste2             = MorePaste,
                % Recurse
                paste_elements(PDF, Paste2, Cost1, PanelMap2);
       false -> paste_elements(PDF, [], Cost, PanelMap)
     end.


%% ***********************************************************
%% paste/4 helpers
%% ***********************************************************


%% @doc Paste up content element 

-spec paste_up(PDF            :: identifier(),
               Tag            :: atom(),
               Lines          :: list(),
               Cost           :: tuple(),
               PanelMap       :: map()) -> tuple().

paste_up(PDF, Tag, Lines, Cost, PanelMap) ->
    {Cost1, PanelMap1} = paste_lines(PDF, Tag, Lines, Cost, PanelMap),
    ok                 = paste_if_li(PDF, Tag, PanelMap),
    ok                 = paste_if_ci(PDF, Tag, PanelMap),
    {Cost1, PanelMap1}.



%% @doc Paste lines into panel

-spec paste_lines(PDF     :: identifier(),
                  Tag     :: atom(),
                  Lines   :: list(),
                  Cost    :: integer(),
                  PanelMap :: map()) -> tuple().

paste_lines(_PDF, br, _Lines, Cost, PanelMap) ->
    {Cost, PanelMap};

paste_lines(PDF, Tag, Lines, [], PanelMap) ->
    {Widths, Offsets}  = ep_xml_lib:line_specs(Tag, PanelMap),
    Code               = pdf_code(PDF, Tag, Lines, Widths, Offsets, PanelMap),
    ok                 = paste(PDF, Code),
    {[], PanelMap};

paste_lines(PDF, Tag, Lines, Cost, PanelMap) ->
    {Cost1, PanelMap1} = ep_xml_lib:impose_cost(-1, Cost, PanelMap),
    {Widths, Offsets}  = ep_xml_lib:line_specs(Tag, PanelMap),
    Code               = pdf_code(PDF, Tag, Lines, Widths, Offsets, PanelMap1),
    ok                 = paste(PDF, Code),
    {Cost1, PanelMap1}.



paste_if_li(PDF, Tag, PanelMap) ->
   Margin     = ep_panel:get_margin(PanelMap),
   TypeStyle  = ep_panel:get_typestyle(PanelMap),
   Indent     = ep_typespec:indent(TypeStyle, Tag) div 2,
   Radius     = 2,
   Diff       = ep_typespec:leading(TypeStyle, Tag) - Radius,
   case Tag of
      li  -> 
            {TextX, TextY} = ep_panel:get_text_position(PanelMap),
             DiffX = TextX + Margin + Indent,
             DiffY = TextY - Diff,
             eg_pdf:save_state(PDF),
             eg_pdf:set_line_width(PDF, 1),
             eg_pdf:set_dash(PDF, solid),
             eg_pdf:set_stroke_color(PDF, black),
             eg_pdf:set_fill_color(PDF, black),
             eg_pdf:circle(PDF, {DiffX, DiffY}, Radius),
             eg_pdf:path(PDF, fill_stroke),
             eg_pdf:restore_state(PDF),
             ok;
        _  -> ok
    end.

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
   {TextX, TextY}                       = ep_panel:get_text_position(PanelMap),
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


