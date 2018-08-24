%%% ==========================================================================
%%% ep_block.erl


%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:      MIT
%%%   File:         ep_block.erl
%%%   Description:  Display text blocks 
%%% @end
%%%
%%% NOTE: Deprecated -- soon to be deleteed!!!!!!!!!!!!!!!!!!
%%% ==========================================================================


-module (ep_block).

-export([block/3]).

-export([parse_xml/1, normalise_xml/2, rich_text/1, widths/3, offsets/3]).
-export([break_rich_text/3]).
-export([available/2]).

-export([pastable_lines/2, tokens/1, pastable_tokens/2, update_richText/3]).
    
-include("../include/eg.hrl").


%% ******************************************************************************
%% Block/3
%% ******************************************************************************


%% @doc Process content elements into PDf

block(PDF, Job, BlockMap) ->
    Text          = maps:get(text, BlockMap),
    {X, Y}        = ep_job:flip_y(Job, BlockMap),
    XML = parse_xml(Text),
    inner_block(PDF, XML, X, Y, BlockMap).


%% ******************************************************************************
%% Block/3 helpers
%% ******************************************************************************

%% @doc process a parsed XML block of content into a block of PDf text with a blank background

inner_block(_PDF, [], _X, _Y, _BlockMap) ->
       io:format("~n !!!!!!!!!!!!!!!! We're done!  ~n"),
   ok;

inner_block(PDF, [{raw, Xml}], X, Y, BlockMap) ->
   block2(PDF, [{xml, Xml}], X, Y, BlockMap),
   ok;

inner_block(PDF, [{xml, Xml} | T], X, Y, BlockMap) ->
   Height = block2(PDF, {xml, Xml}, X, Y, BlockMap),
   NextY = Y - Height,
   inner_block(PDF, T, X, NextY, BlockMap).

    
block2(PDF, {xml, Xml}, X, Y, BlockMap) ->
    io:format("================ block2/5 - BlockMap: ~p~n~n", [BlockMap]),
    Tag = element(1, Xml),
    BlockMap1 = ep_text_block:update_tag(Tag, BlockMap),
%    BlockMap1 = ep_typespec:update_blockmap(Tag, BlockMap),
    TypeSpec = maps:get(face, BlockMap1),
    ensure_fonts_are_loaded(PDF, TypeSpec),
    Norm = normalise_xml(Xml, TypeSpec),
    {_, RichText} = rich_text(Norm),
    paste_text(PDF, X, Y, RichText, BlockMap1).


%% ******************************************************************************
%% Display content elements
%% ******************************************************************************


paste_text(PDF, X, Y, RichText, BlockMap) ->
   Measure       = maps:get(measure, BlockMap),
   Margin        = maps:get(margin, BlockMap),
   Indent        = maps:get(indent, BlockMap), 
   NLines        = maps:get(nlines, BlockMap),
   Justify       = maps:get(justify, BlockMap),
   Leading       = maps:get(leading, BlockMap),



   Measure1      = Measure - (Margin * 2),

   Widths        = widths(Margin, Measure1, NLines),
   
%   case Justify of
%      justified -> Off =  Margin; 
%      _         -> Off =  Indent + Margin
%   end,
   Off = offsets(Indent, Margin, NLines), 




    MaybeLines = break_rich_text(RichText, Justify, Widths),

    io:format("~n************ paste_text ****************~n"),
    io:format("X: ~p~n", [X]),
    io:format("Justify: ~p~n", [Justify]),
    io:format("Widths: ~p~n", [Widths]),
    io:format("Off: ~p~n~n", [Off]),


    case MaybeLines of
	impossible ->
	    io:format("Cannot break line are widths ok~n");
	{Lines,_,_} ->
	    Code = ep_richText2pdf:richText2pdf(PDF, X, Y, Justify, 0, Lines, 
						Leading, Widths, Off),


            % Tag = element(1, RichText),
	    eg_pdf:begin_text(PDF),
	    eg_pdf:append_stream(PDF, Code),
	    eg_pdf:end_text(PDF),
	    length(Lines) * Leading
    end.  



%% ******************************************************************************
%% Helpers
%% ******************************************************************************


-spec parse_xml(TaggedText :: list()) -> list().  % [{xml, Tag, [], XML]

parse_xml(TaggedText) ->
    eg_xml_lite:parse_all_forms(TaggedText).


-spec normalise_xml(Xml :: list(), TypeSpec :: list()) -> list().

normalise_xml(Xml, TypeSpec) ->
    eg_xml2richText:normalise_xml(Xml, TypeSpec).


-spec rich_text(NormalizedXml :: list()) -> tuple(). % {Tag, RichText}

rich_text(NormalisedXml) ->
    {Tag, _, RichText} = NormalisedXml,
    {Tag, RichText}.


widths(Indent, Measure, NLines) ->
    [Measure - Indent|lists:duplicate(NLines - 1, Measure)].


-spec offsets(Indent :: integer(), 
              Margin :: integer(),
              NLines :: integer()) -> list().

offsets(Indent, Margin, NLines) ->
    [Margin + Indent|lists:duplicate(NLines-1, Margin)].



break_rich_text(RichText, Justify, Widths) ->
    io:format("Entering break_rich_text/3~n~n"),
    ep_line_break:break_richText(RichText, {Justify, Widths}).    


ensure_fonts_are_loaded(PDF, TypeSpec) ->
    Faces = element(2, TypeSpec),
    lists:foreach(fun({_,Face}) ->
			  FontHandler = eg_richText:fontFromFace(Face),
			  Font = FontHandler:fontName(),
        		  eg_pdf:ensure_font_gets_loaded(PDF, Font)
        		  end, Faces).


available(Y, BlockMap) ->
   {_TopX, TopY} = maps:get(position, BlockMap),
   Leading       = maps:get(leading, BlockMap),
   NLines        = maps:get(nlines, BlockMap),
   Filled        = (Y - TopY) div Leading, 
   Available     = NLines - Filled, 
 
   io:format("^^^^^^^ Available: ~p~n~n", [Available]),

   Available.

%% **********************************
%% Copyfit functions
%% **********************************

pastable_lines(Lines, NLines) ->
   lists:sublist(Lines, NLines).

tokens(Line) ->
   TokenList = element(2, Line),
   length(TokenList).

pastable_tokens(Lines, NLines) ->
   PastableLines  = pastable_lines(Lines, NLines),
   PastableTokens = [tokens(Line) || Line <- PastableLines],
   lists:sum(PastableTokens).

update_richText(RichText, Lines, NLines) ->
   PastableTokens = pastable_tokens(Lines, NLines),
   RichList = element(2, RichText),
   UpdatedRichList = lists:sublist(RichList, PastableTokens + 1, 10000),
   {richText, UpdatedRichList}.



  



% deduct(Lines) ->
%   Line      = hd(Lines),
%   TokenList = element(2, Line),
%   length(TokenList).

% token_list(RichText) ->
%   element(2, RichText).

