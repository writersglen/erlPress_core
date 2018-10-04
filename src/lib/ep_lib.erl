%%% *********************************************************

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:      MIT
%%%   File:         ep_lib.erl
%%%   Description:  erlPress library functions 
%%% @end

%%% ==========================================================================


-module (ep_lib).

-export([
    get_userguide_image/1,
    get_userguide_text/1,
    impose_box/3,
    impose_line/3,
    impose_lines/3,
    impose_text/2,
    impose_xy/3,
    list_userguide_images/0,
    list_userguide_text/0,
    months/0,
    page_id/1,
    today/0,
    v_flip/2,
    within/3
]). % a..z

-include("ep_erltypes.hrl").

%% @doc Return randomly  generated string
-spec page_id(Length :: integer()) -> string().
page_id(Length) ->
   AllowedChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
   random_string(Length, AllowedChars).


%% Optimization: It is faster to pick random elements from a tuple
-spec random_string(Length :: integer(), AllowedChars :: list()) -> string().
random_string(Length, AllowedChars) ->
    lists:foldl(
        fun(_, Acc) ->
            [lists:nth(rand:uniform(length(AllowedChars)),
                       AllowedChars)]
            ++ Acc
        end, [], lists:seq(1, Length)).


%% @doc Return list of user guide images
-spec list_userguide_images() -> {ok, list()} | {error, _Reason}.
list_userguide_images() ->
    {ok, Dir} = file:get_cwd(),
    ImageDir = Dir ++ "/priv/userguide/images",
    file:list_dir(ImageDir).


%% @doc Return userguide image
-spec get_userguide_image(Image :: file:filename())
                         -> {ok, binary()} | {error, _Reason}.
get_userguide_image(Image) ->
    {ok, Dir} = file:get_cwd(),
    Path = Dir ++ "/priv/userguide/images/" ++ Image,
    file:read_file(Path).


%% @doc Return list of user text files 
-spec list_userguide_text() -> {ok, list()} | {error, _Reason}.
list_userguide_text() ->
   {ok, Dir} = file:get_cwd(),
   TextDir = Dir ++ "/priv/userguide/text",
   file:list_dir(TextDir).


%% @doc Return userguide text file 
-spec get_userguide_text(TextFile :: list())
                        -> {ok, binary()} | {error, _Reason}.
get_userguide_text(TextFile) ->
   {ok, Dir} = file:get_cwd(),
   Path = Dir ++ "/priv/userguide/text/" ++ TextFile,
   io:format("Path: ~p~n", [Path]),
   file:read_file(Path).


%% @doc Return local datetime as string formated as month day, year
-spec today() -> string().
today() ->
    {Date, _Time} = calendar:local_time(),
    {Year, Month, Day} = Date,
    Year1 = integer_to_list(Year),
    Day1 = integer_to_list(Day),
    MonthList = months(),
    {_Index, MonthString} = lists:keyfind(Month, 1, MonthList),
    lists:concat([MonthString, Day1, ", ", Year1]).


%% @doc Return list of months
-spec months() -> list({1..12, string()}).
months() ->
    [{1, "January "},
     {2, "February "},
     {3, "March "},
     {4, "April "},
     {5, "May "},
     {6, "June "},
     {7, "July "},
     {8, "August "},
     {9, "September "},
     {10, "October "},
     {11, "November "},
     {12, "December "}
    ].

%% ****************************************************************
%%   XY coordinates in PDF are at  bottom left;
%%   Since in English and other languages we read a page
%%   from top to bottom, it feels more natural in page design
%%   to designate XY coordinaes at top left. But this means
%%   that we have to invert the y coordinate before we print
%%   a panel or page grid.
%% ****************************************************************


%% @doc Given placement of page on paper stock, translate coordinate of point
-spec impose_xy(XY :: xy(), PageXY :: xy(), PaperStock :: atom()) -> xy().
impose_xy(XY, PageXY, PaperStock) ->
    {X, Y}          = XY,
    {PageX, _PageY} = PageXY,
    X1              = X + PageX,
    Y1              = v_flip(Y, PaperStock),
    {X1, Y1}.


-spec impose_line(xy1_xy2(), xy(), paper_stock()) -> xy1_xy2().
impose_line(Line, PageXY, PaperStock) ->
    Pt1 = impose_xy(element(1, Line), PageXY, PaperStock),
    Pt2 = impose_xy(element(2, Line), PageXY, PaperStock),
    {Pt1, Pt2}.


-spec impose_lines(list(xy1_xy2()), xy(), paper_stock()) -> xy1_xy2().
impose_lines(Lines, PageXY, PaperStock) ->
    List = [impose_line(Line, PageXY, PaperStock) || Line <- Lines],
    lists:reverse(List).


-spec impose_box(any(), any(), paper_stock()) -> xy().
impose_box(Position, Size, PaperStock) ->
    {X, Y} = Position,
    {_Width, Height} = Size,
    Y1 = v_flip(Y, PaperStock),
    Y2 = Y1 - Height,
    {X, Y2}.


-spec impose_text(xy(), paper_stock()) -> xy().
impose_text(Position, PaperStock) ->
    {X, Y} = Position,
    Y1 = v_flip(Y, PaperStock),
    Y2 = Y1,
    {X, Y2}.

   
-spec v_flip(Y :: integer(), PaperStock ::  atom()) -> integer().
v_flip(Y, PaperStock) ->
   StockHeight = ep_paper_stock:stock_height(PaperStock),
   StockHeight - Y.


%% @doc Return true of A =< X =< B
-spec within(X :: number(), A :: number(), B :: number()) -> boolean().
within(X, A, B) ->
    (X >= A) and (X =< B).

