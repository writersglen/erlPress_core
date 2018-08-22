%%% ==========================================================================
%%% ep_txt_block.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:      MIT
%%%   File:         ep_txt_block.erl
%%%   Description:  Typseset paragraphs 
%%% @end
%%%
%%% NOTE: Deprecated
%%% ==========================================================================


-module (ep_txt_block).

-export ([fit_copy/2, create/3, text_block/3]).
-export([update_tag/2]).

% -compile(export_all).

-define(TYPETAG, report).
-define(FACE, ep_report_sty:report_faces(p)).
-define(LEADING, 18).
-define(NLINES, 10).
-define(JUSTIFY, justified).   
-define(INDENT, 30).
-define(RADIUS, 10).
-define(MARGIN, 10).
-define(BORDER, 1).
-define(BORDER_STYLE, solid).
-define(BORDER_COLOR, black).
-define(BACKGROUND_COLOR, eg_pdf_op:color(gainsboro)).

%% NOTE: See XXXX for color selections
%%       Justification options: justified, centered, ragged, ragged_left

-include("../../include/ep.hrl").

%% ***********************************************************
%% Text block
%% ***********************************************************


%% @doc Fit text to PanelMap 

-spec fit_copy(Text      :: list(),
               PanelMap  :: integer()) -> list().

fit_copy(Text, PanelMap) ->
    XML = ep_xml_lib:parse_xml(Text),
    ep_xml_lib:fit_xml(XML, PanelMap).



%% @doc Create text block map

-spec create(Text      :: list(),
             Position  :: tuple(),
             Measure   :: integer()) -> map().

create(Text, Position, Measure) ->
   #{ text              => Text 
    , position          => Position
    , measure           => Measure 
    , type_tag          => ?TYPETAG
    , nlines            => ?NLINES 
    , margin            => ?MARGIN
    , face              => ?FACE
    , leading           => ?LEADING 
    , justify           => ?JUSTIFY 
    , indent            => ?INDENT
    , radius            => ?RADIUS
    , border            => ?BORDER
    , border_style      => ?BORDER_STYLE
    , border_color      => ?BORDER_COLOR
    , background_color  => ?BACKGROUND_COLOR 
    }.

%% NOTE: If text is too long, it will spill over bottom
%%       of box 


%% ***********************************************************
%% Text Block 
%% ***********************************************************

%% @doc Display text block

-spec text_block(PDF      :: identifier(),
                 Job      :: map(),
                 BlockMap :: map()) -> ok. 

text_block(PDF, Job, BlockMap) ->
    {X, Y}          = maps:get(position, BlockMap),
    {X1, Y1}        = ep_job:flip_y(Job, BlockMap),

    NLines           = maps:get(nlines, BlockMap),
    Leading          = maps:get(leading, BlockMap),
    Measure          = maps:get(measure, BlockMap),
    Margin           = maps:get(margin, BlockMap),
    TextHeight       = Leading * NLines,

    BoxHeight        = TextHeight + (2 * Margin),
    BoxWidth         = Measure,
    BoxSize          = {BoxWidth, BoxHeight},
    Radius           = maps:get(radius, BlockMap),
    BoxMap           = ep_round_rect:create({X, Y + BoxHeight}, 
                                             BoxSize, Radius),
    BoxMap1          = inherit_values(BlockMap, BoxMap),

   % Draw box
    ep_round_rect:round_rect(PDF, Job, BoxMap1),

   % Draw text
    ep_block:block(PDF, Job, BlockMap),
    ok.


update_tag(Tag, BlockMap) ->
   maps:put(tag, Tag, BlockMap).


%% @doc Transfer values from BlockMap to BoxMap

-spec inherit_values(BlockMap :: map(),
                     BoxMap   :: map()) -> map().

inherit_values(BlockMap, BoxMap) ->
    Border           = maps:get(border, BlockMap),
    BorderStyle      = maps:get(border_style, BlockMap),
    BorderColor      = maps:get(border_color, BlockMap),
    BackgroundColor  = maps:get(background_color, BlockMap),
    Margin           = maps:get(margin, BlockMap),
    BoxMap1          = maps:put(border, Border, BoxMap),
    BoxMap2          = maps:put(border_style, BorderStyle, BoxMap1),
    BoxMap3          = maps:put(border_color, BorderColor, BoxMap2),
    BoxMap4          = maps:put(background_color, BackgroundColor, BoxMap3),
    maps:put(margin, Margin, BoxMap4).

