%%% ==========================================================================
%%% ep_text.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:
%%%   File:         ep_text.erl
%%%   Description:  Display text 
%%% @end

%%% ==========================================================================


-module (ep_text).

-export([create/3, one_line/4]).
-export ([text_lines/3, next_text_line/4, end_text_lines/1]). 
-export([test_sample/2]).

-define(TEXT_COLOR, black).



%% ***********************************************************
%% Create text map 
%% ***********************************************************

%% @doc Create text map

-spec create(Font     :: string(),
             Position :: tuple(),
             Size     :: integer()) -> map().

create(Font, Position, Size) ->
   #{ font          => Font 
    , position      => Position 
    , size          => Size
    , text_color    => ?TEXT_COLOR
    , justification => justified
    , rot           => 0
    , leading       => Size + (Size div 2) 
    }.




one_line(PDF, Text, Job, TextMap) ->
    {PaperStock, PagePosition} = ep_job:stock_position(Job),
    Font          = maps:get(font, TextMap),
    Size          = maps:get(size, TextMap),
    Leading       = maps:get(leading, TextMap),
    % Justification = maps:get(justification, TextMap),
    Rot           = maps:get(rot, TextMap),
    Position      = maps:get(position, TextMap),
    Position1     = ep_lib:impose_xy(Position, PagePosition, PaperStock),
    {X, Y}        = Position1,
    eg_pdf:begin_text(PDF),
    eg_pdf:set_font(PDF, Font, Size),
%    eg_pdf:set_text_pos(PDF, X, Y),
    eg_pdf:set_text_leading(PDF,Leading),
    eg_pdf:text_rotate_position(PDF, X, Y, Rot),
    eg_pdf:textbr(PDF, Text),
    eg_pdf:end_text(PDF),
    ok.



text_lines(PDF, Position, LineHeight) ->
    {X, Y} = Position,
    eg_pdf:begin_text(PDF),
    eg_pdf:set_text_pos(PDF, X, Y),
    eg_pdf:set_text_leading(PDF, LineHeight).

next_text_line(PDF, Text, Font, Size) ->
    eg_pdf:set_font(PDF, Font, Size),
    eg_pdf:textbr(PDF, Text).

end_text_lines(PDF) ->
    eg_pdf:break_text(PDF),
    eg_pdf:end_text(PDF).


test_sample(PDF, Position) ->
    %% Much factoring needed here
    {X, Y} = Position,

    eg_pdf:begin_text(PDF),

    eg_pdf:set_text_pos(PDF, X, Y),
    eg_pdf:set_text_leading(PDF,14),
    eg_pdf:set_font(PDF,"Blahonga", 12),
    eg_pdf:textbr(PDF, "The blahonga font will fall back to Times-Roman"),
    eg_pdf:textbr(PDF, "This is a check of ( ) \\ escape chars"),
    eg_pdf:kernedtext(PDF,
                   [ "This is a test of Kerning: A", 120, "W",
                     120, "A", 95, "Y again" ]),
    eg_pdf:break_text(PDF),
    eg_pdf:textbr(PDF, "This is a text without Kerning: AWAY again"),
    eg_pdf:break_text(PDF),
    eg_pdf:end_text(PDF).

