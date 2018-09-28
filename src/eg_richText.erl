%%==========================================================================
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to permit
%% persons to whom the Software is furnished to do so, subject to the
%% following conditions:
%% 
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
%% USE OR OTHER DEALINGS IN THE SOFTWARE.
%%==========================================================================

-module(eg_richText).

%% OLD AND BROKEN doc ?! (see modern version further down)

%% encapsulates operations on text() and inline() objects

%% ADT 
%%   +deftype richText() = {richText, [inLine()]
%%   +deftype inLine()   = Word | FixedString | Opaque | Space | NL
%%   +deftype Word       = {word, Width, Face, Str},
%%   +deftype FixedStr   = {fixedStr, Width, Face, Str},
%%   +deftype Opaque     = {opaque, Width, X}
%%   +deftype Space      = {space, Width, Face}
%%   +deftype NL         = {nl, Face}
%%   +deftype Face       = {Font, PointSize, Voffset, Color, Breakable}
%%   +deftype Color      = default | {R,G,B}
 
%% Interface 
%%   str2text(Foint, PointSize, Str) -> text()
%%   text2str(text()) -> str().

-export([
    classify_inline/1,
    clone_space/1,
    clone_word/2,
    color/1,
    font/1,
    fontFromFace/1,
    is_breakable/1,
    is_face_breakable/1,
    is_nl/1,
    is_space/1,
    is_word/1,
    lineWidth/1,
    mk_face/5,
    mk_fixedStr/2,
    mk_nl/1,
    mk_space/1,
    mk_test_word/1,
    mk_word/2,
    numberOfSpaces/1,
    pointSize/1,
    richText2str/1,
    str2richText/1,
    str2richText/2,
    str2richText/6,
    string/1,
    test/1,
    width/1,
    width_of/3,
    widthExcludingSpaces/1
]).

-include("../include/eg.hrl").

-export_type([richText/0, any_inline/0, word/0, space/0, nl/0, fixed_str/0,
              opaque/0, milli_points/0, points/0]).

-type richText() :: {richText, [any_inline()]}.
-type any_inline() :: word() | opaque() | space() | nl() | fixed_str().
-type word() :: {word, Width :: milli_points(), Face :: #face{}, string()}.
-type space() :: {space, Width :: milli_points(), Face :: #face{}}.
-type nl() :: {nl, Face :: #face{}}.
-type fixed_str() :: {fixedStr, Width :: milli_points(), _, _}.
-type opaque() :: {opaque, Width :: milli_points(), _}.
-type milli_points() :: integer().
-type points() :: integer().


%% -define(DEBUG, true).

-ifdef(DEBUG).
    dbg_io(Str) -> dbg_io(Str, []).

    dbg_io(Str, Args) ->
        io:format("eg_richText: ~p " ++ Str, [self()] ++ Args),
        ok.
-else.
    dbg_io(_) -> ok.
    dbg_io(_, _) -> ok.
-endif.


test(1) ->
    str2richText("TimesDutch", 12, 0, default, true,
                 "Hello joe how are you today?
                 May I take this opportunity
                 of saying
                 that my favorite color is blue.
                 Have a nice day,
                 from Mr. C. Computer.").


%% @doc   This takes a deep list of tuples and replaces tuples with
%%        equivalents, i.e. " " for {space,_, _}, "\n" for {nl,_}, etc.
-spec richText2str({richText, _L}) -> string().
richText2str({richText, L}) ->
    eg_pdf_op:flatten(lists:map(fun inline2str/1, L)).


%% @private
inline2str({word, _, _Face, Str})     -> Str;
inline2str({space, _, _})             -> " ";
inline2str({nl, _})                   -> "\n";
inline2str({fixedStr, _, _Face, Str}) -> Str;
inline2str(_)                         -> "".


str2richText(Str) ->
    str2richText("Times-Roman", 12, 0, default, true, Str).


str2richText(Str, Pts) ->
    str2richText("Times-Roman", Pts, 0, default, true, Str).


%% @doc   convert string to "word" sequence containing word font/size info
-spec str2richText(Font :: string(), _Point, _Voff, _Color, _Break,
                   Str :: string()) -> richText().
str2richText(Font, Point, Voff, Color, Break, Str) ->
    valid_bool(Break),
    F  = fontHandler(Font),
    Face = #face{font=F, pointSize=Point, vOffset=Voff, 
                 color=Color, breakable=Break},
    L1 = normalise_str(Str, []),
    L2 = lists:map(fun({wd1,S}) ->
                           Width = width_of(F, Point, S),
                           {word, Width, Face, S};
                      (spaces) ->
                           Width = width_of(F, Point, [$\s]),
                           {space, Width, Face};
                      (lineFeed) ->
                           {nl, Face}
                   end, L1),
    {richText, L2}.


valid_bool(true)  -> true;
valid_bool(false) -> true;
valid_bool(X)     -> exit({badarg, str2richText, breakNotBool, was, X}).


%% @doc  split Str into word and space segments, single and duplicate
%%       whitespace chars get replaced by a single 'spaces', NL -> lineFeed 
%% @private
-spec normalise_str(string(), _Acc) -> [spaces | lineFeed | {wd1, Word::string()}].
normalise_str([], L) ->
    lists:reverse(L);

normalise_str([$\n | T], L) ->
    normalise_str(T, [lineFeed | L]);

normalise_str([H | T], L) ->
    case is_white(H) of
        true ->
            T1 = skip_white(T),
            normalise_str(T1, [spaces | L]);
        false ->
            {Word, T1} = collect_word(T, [H]),
            normalise_str(T1, [{wd1, Word} | L])
    end.


%% @private
is_white($\s) -> true;
is_white(160) -> true; %% non-break-space
is_white($\t) -> true;
is_white($\r) -> true;
is_white(_)   -> false.


skip_white(X = [H | T]) ->
    case is_white(H) of
        true -> skip_white(T);
        false -> X
    end;

skip_white([]) ->
    [].


%% @private
collect_word(X = [$\n | _T], L) ->
    {lists:reverse(L), X};

collect_word(X = [H | T], L) ->
    case is_white(H) of
        true -> {lists:reverse(L), X};
        false -> collect_word(T, [H | L])
    end;

collect_word([], L) ->
    {lists:reverse(L), []}.


-spec width_of(any(), number(), string()) -> number().
width_of(Font, PointSize, Str) ->
    PointSize * sizeof(Font, Str).


%% @doc Computes width of Str which is of type FontIndex
%%      Size is correctly adjusted for kerning information
-spec sizeof(_FontIndex, _Str) -> integer().
sizeof(Font, Str) ->
    Widths = lists:map(fun(I) -> char_width(Font, I) end, Str),
    %% dbg_io("Str=|~s| Widths=~p~nFont=~p~n",[Str, Widths, Font]),
    W1 = lists:sum(Widths),
    %% and add the correct kerning info
    kern_adj(Str, W1, Font).


%% @private
-spec char_width(module(), integer()) -> integer().
char_width(Font, I) ->
    case Font:width(I) of
        Width when is_integer(Width) ->
            Width;
        _ ->
            dbg_io("Character ~w in font ~p has no width~n", [I, Font]),
            Font:width($\s)
    end.


%% @private
kern_adj([H1, H2 | T], W, Font) ->
    Extra = Font:kern(H1, H2),
    kern_adj([H2 | T], W + Extra, Font);
kern_adj(_, W, _) ->
    W.

%%----------------------------------------------------------------------
%% access funtions
%%

-spec is_space(tuple()) -> boolean().
is_space(X) -> element(1, X) == space.


-spec is_word(tuple()) -> boolean().
is_word(X) -> element(1, X) == word.


-spec is_nl(tuple()) -> boolean().
is_nl(X) -> element(1, X) == nl.


-spec is_breakable(word() | any()) -> boolean().
is_breakable({word, _, Face, _Str}) -> Face#face.breakable;
is_breakable(_)                     -> false.


%% @doc Make a new word based on the face of an old word
-spec clone_word(word(), string()) -> word().
clone_word({word, _, Face, _}, Str) ->
    Font = Face#face.font,
    PointSize = Face#face.pointSize,
    W = width_of(Font, PointSize, Str),
    {word, W, Face, Str}.


-spec clone_space(word() | nl()) -> any().
clone_space({word, _, Face, _}) ->
    clone_space_from_face(Face);

clone_space({nl, Face}) ->
    clone_space_from_face(Face).


%% @private
-spec clone_space_from_face(#face{}) -> space().
clone_space_from_face(Face) ->
    Font = Face#face.font,
    PointSize = Face#face.pointSize,
    W = width_of(Font, PointSize, [$\s]),
    {space, W, Face}.


-spec width(any_inline()) -> integer().
width({word, W, _, _})     -> W;
width({opaque, W, _})      -> W;
width({space, W, _})       -> W;
width({nl, _})             -> 0;
width({fixedStr, W, _, _}) -> W.


-spec font(any_inline()) -> module().
font({word, _, F, _})     -> F#face.font;
font({opaque, _, _F})     -> unknown;
font({space, _, F})       -> F#face.font;
font({nl, F})             -> F#face.font;
font({fixedStr, _, F, _}) -> F#face.font.


-spec fontFromFace(#face{}) -> atom().
fontFromFace(F) -> F#face.font.


-spec color(any_inline()) -> any().
color({word, _, F, _})  -> F#face.color;
color({opaque, _, _F})  -> unknown;
color({space,_,F})      -> F#face.color;
color({nl,F})           -> F#face.color;
color({fixedStr,_,F,_}) -> F#face.color.


-spec pointSize(any_inline()) -> milli_points().
pointSize({word, _, F, _})     -> F#face.pointSize;
pointSize({opaque, _, _F})     -> unknown;
pointSize({space, _, F})       -> F#face.pointSize;
pointSize({nl, F})             -> F#face.pointSize;
pointSize({fixedStr, _, F, _}) -> F#face.pointSize.


-spec classify_inline(any_inline()) -> atom().
classify_inline({word, _W, _, _})     -> word;
classify_inline({opaque, _W, _})      -> opaque;
classify_inline({space, _W, _})       -> space;
classify_inline({nl,_})               -> nl;
classify_inline({fixedStr, _W, _, _}) -> fixedStr.


-spec string(word() | fixed_str() | space()) -> string().
string({word, _, _, S})  -> S;
string({fixedStr,_,_,S}) -> S;
string({space,_,_})      -> " ".


-spec mk_word(#face{}, string()) -> word().
mk_word(Face, Str) ->
    F = Face#face.font,
    P = Face#face.pointSize,
    Width = width_of(F, P, Str),
    {word, Width, Face, Str}.


-spec mk_fixedStr(#face{}, string()) -> fixed_str().
mk_fixedStr(Face, Str) ->
    F = Face#face.font,
    P = Face#face.pointSize,
    Width = width_of(F, P, Str),
    {fixedStr, Width, Face, Str}.


-spec mk_nl(#face{}) -> nl().
mk_nl(Face) ->
    {nl, Face}.


-spec mk_space(#face{}) -> space().
mk_space(Face) ->
    F = Face#face.font,
    P = Face#face.pointSize,
    Width = P * char_width(F, 32),
    {space, Width, Face}.


%% @doc make a word that we can play with
-spec mk_test_word(string()) -> word().
mk_test_word(Str) ->
    F = fontHandler("Times-Roman"),
    Face = #face{font      = F,
                 pointSize = 16,
                 vOffset   = 0,
                 color     = default,
                 breakable = true},
    Width = width_of(F, 16, Str),
    {word, Width, Face, Str}.


mk_face(Font, PointSize, Breakable, Color, VoffSet) ->
    F = fontHandler(Font),
    #face{font      = F,
          pointSize = PointSize,
          vOffset   = VoffSet,
          color     = Color,
          breakable = Breakable}.


%% @private
fontHandler(Font) ->
    case eg_font_map:handler(Font) of
        undefined ->
            dbg_io("There is no font called:~s~n", [Font]),
            dbg_io("Using Times-Roman~n"),
            eg_font_map:handler("Times-Roman");
        Mod ->
            Mod
    end.


lineWidth(Toks) ->
    lists:foldl(fun(I, S) -> width(I) + S end, 0, Toks).


numberOfSpaces(Toks) ->
    Toks1 = lists:filter(fun(I) -> is_space(I) end, Toks),
    length(Toks1).


widthExcludingSpaces(Toks) ->
    lists:foldl(
        fun(I, S) ->
            case is_space(I) of
                true -> S;
                false -> S + width(I)
            end
        end, 0, Toks).


-spec is_face_breakable(#face{}) -> boolean().
is_face_breakable(F) ->
    F#face.breakable.
