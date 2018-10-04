%%==========================================================================
%% Copyright (C) 2003-2004 Joe Armstrong, Mikael Karlsson 
%%
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
%%
%% Authors:   Joe Armstrong   <joe@sics.se>
%%            Mikael Karlsson <mikael.karlsson@creado.com>
%% Purpose: Erlguten PDF library routines
%%==========================================================================


%%% @doc ErlGuten PDF library routines
%%% @end

-module(eg_pdf_lib).

-export([
    add_object/2,
    delete_object/2,
    find_in_dict/2,
    get_next_ref/1,
    get_ref/1,
    make_object/2,
    make_object_key/1,
    pdf_item/1,
    pdf_item/2,
    search_object/2,
    serialise/1,
    serialise2bin/1,
    store_in_dict/2,
    store_object/2
]).

-export([
    export/2,
    get_objects_of_type/2,
    is_pdf_object_type/2,
    pdf_object_dict_item/2
]).

-export([
    code128/1,
    draw_box/6,
    moveAndShow/4,
    moveAndShow/5,
    moveAndShowRight/5,
    moveAndShowRot/5,
    showGrid/2
]).

-type pdfobject() :: {objkey(), pdftype()}.
-type pdfobjects() :: [pdfobject()].
-type objkey() :: {obj, Ref :: integer(), Gen :: integer()}.

-type pdftype() :: dict() | stream() | ptr() | name() | array()
    |  pdfstring() | boolean() | date() | number() | null.
-type dict() :: {dict, [{string(), pdftype()}]}.
-type stream() :: {stream, dict(), iolist() | binary()}
    | {stream, iolist()| binary()}.
-type name() :: {name, string()}.
-type ptr() :: {ptr, Ref :: integer(), Gen :: integer()}.
-type array() :: {array, [pdftype()]}.
-type date() :: {date, Date :: calendar:datetime() | calendar:date()}.
-type pdfstring() :: {string, string()} | {hexstring, string()}.


%% @doc Find PDFItem related object reference.
%% All pdftypes are <code>{Key, Value}</code> tuples where the Key indicates
%% the type.
-spec search_object(Key :: objkey() | ptr() | integer(),
                    Objects :: pdfobjects()) -> {value, pdfobject()} | false.
search_object(Ref, Objects) when is_integer(Ref) ->
    search_object({obj, Ref, 0}, Objects);

search_object({ptr, I, J}, Objects) ->
    search_object({obj, I, J}, Objects);

search_object({obj, _, _} = Key, Objects) ->
    lists:keysearch(Key, 1, Objects).


%% @doc deletes a PDF object.
%% One shall not be able to remove items from the list, since the new Key
%% generation is based on the Objects list length, so delete is
%% implemented just to set an object to null
-spec delete_object(Key :: objkey() | ptr() | integer(),
                    Objects :: pdfobjects()) -> pdfobjects().
delete_object(Ref, Objects) when is_integer(Ref) ->
    delete_object({obj, Ref, 0}, Objects);

delete_object({ptr, I, J}, Objects) ->
    delete_object({obj, I, J}, Objects);

delete_object(Key, Objects) ->
    case lists:keymember(Key, 1, Objects) of
        true ->
            Object = {Key, null},
            lists:keyreplace(Key, 1, Objects, Object);
        false ->
            Objects
    end.


%% @doc Returns a copy of Objects1 with Objects added or, if the object's
%% reference already is present, replacing the old object.
-spec store_object(Objects :: pdfobject() | pdfobjects(),
                   Objects1 :: pdfobjects()) -> pdfobjects().
store_object({Key, _PDFItem} = Object, Objects) ->
    case lists:keymember(Key, 1, Objects) of
        true ->
            lists:keyreplace(Key, 1, Objects, Object);
        false ->
            [Object | Objects]
    end;

store_object([Object | NewObjects], Objects) ->
    store_object(NewObjects, store_object(Object, Objects));

store_object([], Objects) ->
    Objects.


%% @doc Makes a new object from PDFItem, and adds it to Objects .
%% Returns the Reference number for the added object and the new Object list
-spec add_object(PDFItem :: pdftype(), Objects :: pdfobjects()) ->
    {Ref :: integer(), pdfobjects()}.
add_object(PDFItem, Objects) ->
    Ref = get_next_ref(Objects),
    NewObjects = store_object( make_object( Ref, PDFItem ), Objects ),
    { Ref, NewObjects }.


%% @doc Returns the PDFItem for the Object corresponding to Key.
%% If no object is found and exception is generated.
-spec pdf_item(Key :: objkey(), Objects :: pdfobjects()) -> pdftype().
pdf_item(Key, Objects) ->
    {value, Object} = search_object(Key, Objects),
    pdf_item(Object).


%% @doc Returns the PDFItem for the Object.
-spec pdf_item(Object :: pdfobject()) -> pdftype().
pdf_item({_Key, Value}) ->
    Value.


%% @doc Finds a item in a PDF dictionary with Key
-spec find_in_dict(Key :: string(), Dict :: dict()) -> pdftype() | undefined.
find_in_dict(Key, {dict, Dict}) ->
    case lists:keysearch(Key, 1, Dict) of
        {value, {_, A}} ->
            A;
        false ->
            undefined
    end.


%% @doc Stores a new item in a PDF dictionary, replacing the old
%% one if it exists.
-spec store_in_dict({Key :: string(), Value :: pdftype()}, Dict :: dict()) -> dict().
store_in_dict({Key, _Value} = A, {dict, D}) ->
    case lists:keymember(Key, 1, D) of
        true ->
            {dict, lists:keyreplace(Key, 1, D, A)};
        false ->
            {dict, D ++ [A]} %% Append to keep "Type" etc. first.
    end.


-spec get_ref(Object::pdfobject()) -> integer().
get_ref({{obj, Ref, _}, _}) ->
    Ref.


%% @doc returns next available Reference count
-spec get_next_ref(Objects :: pdfobjects()) -> Ref :: integer().
get_next_ref(Objects) ->
    length(Objects) + 1.


%% @doc Returns a PDF Object with Reference number Ref and PDFItem
-spec make_object(Ref :: integer(), PDFItem :: pdftype()) -> pdfobject().
make_object(Ref, PDFItem) ->
    {make_object_key(Ref), PDFItem}.


%% @doc Returns an Object Key with Reference number Ref
-spec make_object_key(Ref :: integer()) -> objkey().
make_object_key(Ref) ->
    {obj, Ref, 0}.


%% @doc Returns a serialised object as a binary
-spec serialise2bin(pdfobject() | [pdfobject()]) -> list(binary()).
serialise2bin(L) when is_list(L) ->
    lists:map(
        fun(A) ->
            list_to_binary(serialise(A))
        end,
        L);

serialise2bin(A) ->
    list_to_binary(serialise(A)).


%% @doc Returns a serialised object as a deep list of strings
%% For example,
%% <pre><code>
%%
%% {obj,1, 0, 
%%  {stream,
%%    {dict,
%%      {"Type",{name,"XObject"}},
%%      {"Subtype",{name,"Image"}},
%%      {"Width", 997},
%%      {"Height", 744},
%%      {"Filter",{name,"FlateDecode"},
%%      {"DecodeParms", {dict,[{"Predictor", 15},
%%                      {"Colors", 3},
%%                      {"BitsPerComponent", 8},
%%                      {"Columns", 997}]}},
%%      {"ColorSpace",{name,"DeviceRGB}},
%%                      {"BitsPerComponent", 8},
%%                      {"Columns", 997}}
%% }}}</code></pre>
%%
%% <em>becomes</em>
%%
%% <pre><code>
%% 1 0 obj
%% &lt;&lt;
%% /Type  /XObject 
%% /Subtype  /Image 
%% /Width  997 
%% /Height  744 
%% /Filter  /FlateDecode 
%% /DecodeParms &lt;&lt;
%% /Predictor  15 
%% /Colors  3 
%% /BitsPerComponent  8 
%% /Columns  997 
%% &gt;&gt; 
%% /ColorSpace  /DeviceRGB 
%% /BitsPerComponent  8 
%% /Length  1412674 
%% &gt;&gt;
%% </code></pre>
-spec serialise(Object :: pdfobject() | pdftype()) -> iolist().
serialise({{obj, I, J}, K}) ->
    [eg_pdf_op:i2s(I), " ", eg_pdf_op:i2s(J), " obj\n", serialise(K), "endobj\n"];

serialise({stream, S}) ->
    serialise({stream, {dict, []}, S});

serialise({stream, {dict, K}, L}) when is_list(L) ->
    B = list_to_binary([L]),
    serialise({stream, {dict, K}, B});

serialise({stream, {dict, Dict}, B}) when is_binary(B) ->
    Len = size(B),
    NewDict = store_in_dict({"Length", Len}, {dict, Dict}),
    [serialise(NewDict),
     "\nstream\n", B, "\nendstream\n"
    ];

serialise({dict, L}) ->
    ["<<\n", lists:map(fun({I, J}) ->
        ["/", I, " ", serialise(J), "\n"]
                       end, L),
     ">>\n"];

serialise({name, S}) ->
    [" /", S, " "];

serialise({string, S}) ->
    [" (", S, ") "];

serialise({hexstring, S}) ->
    [" <", s2hs(S), "> "];

serialise({ptr, I, J}) ->
    [" ", eg_pdf_op:i2s(I), " ", eg_pdf_op:i2s(J), " R "];

serialise({array, L}) ->
    [" [ ", lists:map(fun(I) -> serialise(I) end, L), " ] "];

serialise({date, Date}) ->
    ["(D:", date(Date), ")"];

serialise({rect, {A, B, C, D}}) ->
    serialise({array, [A, B, C, D]});

serialise(true) ->
    " true ";

serialise(false) ->
    " false ";

serialise(null) ->
    " null ";

serialise(N) when is_integer(N) ->
    [" ", eg_pdf_op:i2s(N), " "];

serialise(F) when is_float(F) ->
    [" ", eg_pdf_op:f2s(F), " "];

serialise(X) ->
    io:format("I cannot serialise:~p~n", [X]),
    exit(serialise).


%% @private
date({Year, Month, Day}) when Year < 100 ->
    date({Year + 2000, Month, Day});

date({Year, Month, Day}) ->
    d2s([Year, Month, Day]);

date({YMD, {H, MIN, SEC}}) ->
    date(YMD) ++ d2s([H, MIN, SEC]).


%% @private
d2s([]) ->
    [];
d2s([H | T]) when H < 10 ->
    "0" ++ eg_pdf_op:i2s(H) ++ d2s(T);
d2s([H | T]) ->
    eg_pdf_op:i2s(H) ++ d2s(T).


%% @doc Erlang string to Hex string
%% @private
s2hs([]) -> [];
s2hs([H | T]) ->
    A = H band 16#0F,
    B = (H band 16#F0) bsr 4,
    [nibble2c(B), nibble2c(A) | s2hs(T)].


%% @private
nibble2c(N) when N < 10 -> N + $0;
nibble2c(N)             -> N - 10 + $A.


%% @doc Returns the value corresponding to Key from a PDF Objects dictionary.
%% ("Pages", "Page", "Font", etc). 
%% If the Key is missing undefined is returned
%% If the Object is not a dictionary not_dict is returned
-spec pdf_object_dict_item(Key::string(), Object::pdfobject()) ->
    pdftype() | undefined | not_dict.
pdf_object_dict_item(Key, Object) ->
    case pdf_item(Object) of
        {dict, Dict} ->
            find_in_dict(Key, {dict, Dict});
        _ ->
            not_dict
    end.


%% @doc Returns true if the object has the type name Type, otherwise false.
%% PDF object types are ("Pages", "Page", "Font", etc).
-spec is_pdf_object_type(Type::string(), Object::pdfobject()) -> boolean().
is_pdf_object_type(Type, Object) ->
    case pdf_object_dict_item("Type", Object) of
        {name, Type} -> true;
        _ -> false
    end.


%% @doc Returns a list of objects with dictionary key "Type" = Type
%% PDF object types are ("Pages", "Page", "Font", etc).
-spec get_objects_of_type(Type :: string(), Objects :: pdfobjects()) -> pdfobjects().
get_objects_of_type(Type, Objects) ->
    lists:filter(
        fun(Object) ->
            is_pdf_object_type(Type, Object)
        end,
        Objects).


%% @doc Make a pdf document ready for export to
%% file or any other media (network etc.).
-spec export(InfoRef :: integer(), Objects :: pdfobjects()) -> binary().
export(InfoRef, Objects) ->
    SortedObjects = lists:keysort(1, Objects),
    BObjects = serialise2bin(SortedObjects),
    l2b([startmark(),
         pdfbmagic(),
         BObjects,
         xref(BObjects),
         trailer(InfoRef, SortedObjects),
         startxref(BObjects),
         endmark()
      ]).


startmark() -> "%PDF-1.4".

endmark() -> "%%EOF\r\n".

%% pdfbmagic() ->
%%   "zG_\\325\\371\\337J\\244\030\\267\\260#s6\\037\\246dR L\\204s\\037".
%% pdfbmagic() ->
%%   [8#015,$%,8#342,8#343,8#317,8#323, 8#015,8#012].
pdfbmagic() -> ["\n"].

%% xref
%% 0 9
%% 0000000000 65535 f 
%% 0000000033 00000 n 
%% 0000000098 00000 n 
%% 0000000144 00000 n 
%% 0000000203 00000 n 
%% 0000000231 00000 n 
%% 0000000409 00000 n 
%% 0000000721 00000 n 
%% 0000000835 00000 n 
%% trailer
%% <<
%% /Size 9
%% /Root 1 0 R
%% /Info 8 0 R
%% >>
%% startxref
%% 1073
%% %%EOF

%% @private
-spec xref(pdfobjects()) -> iolist().
xref(Objects) ->
    {XRefs, _EndAccu} = lists:mapfoldl(
        fun xref/2,
        length(startmark()) + length(pdfbmagic()),
        Objects),
    ["xref\n",
     "0 ", i2l(length(Objects) + 1), "\n",
     xref1(0, "65535 f"),
     XRefs
    ].


%% @private
xref(Obj, Pos) ->
    {xref1(Pos, "00000 n"), Pos + objsize(Obj)}.


%% @private
xref1(I, Str) ->
    lists:flatten(io_lib:format("~10.10.0w ~s \n", [I, Str])).


%% @private
trailer(InfoRef, Objects) ->
    [Root] = get_objects_of_type("Catalog", Objects),
    RootRef = get_ref(Root),
    ["trailer\n",
     "<<\n",
     "/Size ", i2l(length(Objects) + 1), "\n",
     "/Root ", i2l(RootRef), " 0 R\n",
     "/Info ", i2l(InfoRef), " 0 R\n",
     ">>\n"].


%% @private
startxref(Objects) ->
    ["startxref\n",
     i2l(lists:foldl(fun(A, Accu) -> objsize(A) + Accu end,
                     length(startmark()) + length(pdfbmagic()),
                     Objects)),
     "\n"].


%% @doc The length of the indirect object
objsize(Obj) when is_binary(Obj) -> size(Obj);
objsize(Obj) when is_list(Obj)   -> size(l2b(Obj)).


%% @private
-spec l2b(list()) -> binary().
l2b(X) -> list_to_binary(X).


%% @private
-spec i2l(integer()) -> string().
i2l(X) -> integer_to_list(X).


%% @doc showGrid(PDF, pagesize atom from eg_pdf.erl) adds a grid to the current page
showGrid(PDF, Paper) ->
    {_, _, PaperWidth, PaperHeight} = eg_pdf:pagesize(Paper),
    Top = round((PaperHeight - 20) / 25) * 25, % make grid lines fall on multiples of 25
    Bottom = 10,
    Left = 10,
    Right = round((PaperWidth - 20) / 25) * 25,  % make grid lines fall on multiples of 25
    eg_pdf:set_font(PDF, "Helvetica", 8),
    vlines(PDF, Left, Right, Top, Bottom),
    hlines(PDF, Left, Right, Top, Bottom).


%% @private
hlines(PDF, Left, Right, Top, _Bottom) ->
    eg_pdf:save_state(PDF),
    eg_pdf:set_font(PDF, "Helvetica", 6),
    diter(Top, 25, 10,
          fun(Y) ->
              %% eg_pdf:set_fill_gray(PDF,1.0),
              eg_pdf:line(PDF, Left, Y, Left + 20, Y),
              eg_pdf:line(PDF, Right, Y, Right - 20, Y),
              %% eg_pdf:set_fill_gray(PDF,0.8),
              eg_pdf:line(PDF, Left + 20, Y, Right - 20, Y),
              moveAndShow(PDF, Left, Y + 2, "Y=" ++ eg_pdf_op:n2s(Top - Y)),
              moveAndShow(PDF, Right - 20, Y + 2, "Y=" ++ eg_pdf_op:n2s(Y)),
              true
          end),
    eg_pdf:restore_state(PDF).


%% @private
vlines(PDF, _Left, Right, Top, Bottom) ->
    eg_pdf:save_state(PDF),
    eg_pdf:set_font(PDF, "Helvetica", 6),
    diter(Right, 25, 10,
          fun(X) ->
              eg_pdf:line(PDF, X, Top, X, Top - 20),
              moveAndShow(PDF, X - 5, Top - 35, "X=" ++ eg_pdf_op:n2s(X)),
              eg_pdf:line(PDF, X, Bottom, X, Bottom + 20),
              eg_pdf:line(PDF, X, Top - 40, X, Bottom + 35),
              moveAndShow(PDF, X - 5, Bottom + 23, "X=" ++ eg_pdf_op:n2s(X))
          end),
    eg_pdf:restore_state(PDF).


-spec moveAndShow(eg_pdf:pdf_server_pid(), number(), number(), string()) -> ok.
moveAndShow(PDF, X, Y, Str) ->
    eg_pdf:begin_text(PDF),
    eg_pdf:set_text_pos(PDF, X, Y),
    eg_pdf:text(PDF, Str),
    eg_pdf:end_text(PDF).


-spec moveAndShowRot(eg_pdf:pdf_server_pid(), number(), number(),
                     string(), number()) -> ok.
moveAndShowRot(PDF, X, Y, Str, Rot) ->
    eg_pdf:save_state(PDF),
    eg_pdf:begin_text(PDF),
    eg_pdf:text_rotate_position(PDF, X, Y, Rot),
    eg_pdf:text(PDF, Str),
    eg_pdf:end_text(PDF),
    eg_pdf:restore_state(PDF).


-spec moveAndShow(eg_pdf:pdf_server_pid(), number(), number(),
                  string(), number()) -> ok.
moveAndShow(PDF, X, Y, Str, Scale) ->
    eg_pdf:begin_text(PDF),
    eg_pdf:set_text_pos(PDF, X, Y),
    eg_pdf:set_text_scale(PDF, Scale),
    eg_pdf:text(PDF, Str),
    eg_pdf:set_text_scale(PDF, 100),
    eg_pdf:end_text(PDF).


%% @doc Str is drawn to the left of X (X is the _right_ alignment side of the
%% Str text box)
moveAndShowRight(PDF, {Font, Size}, X, Y, Str) when is_integer(X),
                                                    is_integer(Y) ->
    Width = eg_pdf:get_string_width(PDF, Font, Size, Str),
    moveAndShow(PDF, X - Width, Y, Str);

moveAndShowRight(_PDF, _FontSize, _X, _Y, _Str) ->
    ok.


%% @doc downwards iterator
diter(X, _Inc, Stop, _F) when X < Stop ->
    true;

diter(X, Inc, Stop, F) ->
    F(X), diter(X - Inc, Inc, Stop, F).


draw_box(PDF, X, Y, Measure, Lines, MaxRows) ->
    eg_pdf:append_stream(PDF, draw_box1(X, Y, Measure, Lines, MaxRows)).


draw_box1(X1, Y1, Measure, Leading, MaxRows) ->
    %% X1,Y1,Leading are in points
    %% Measure        is in picas
    X2 = X1 + Measure,
    Y2 = Y1 - Leading * MaxRows,
    [" q  0.4 g 0.4 G 0 w ",
     %% verticals
     line(X1, Y1, X1, Y2),
     line(X2, Y1, X2, Y2),
     apply_to_range(0, MaxRows,
                    fun(I) ->
             Y = Y1 - I * Leading,
             line(X1, Y, X2, Y)
         end),
     " Q "].


%% @doc Calls function F for sequential arguments from I to Max.
%% @private
apply_to_range(I, Max, _F) when I > Max ->
    [];

apply_to_range(I, Max, F) ->
    [F(I) | apply_to_range(I + 1, Max, F)].


%% @private
line(X1, Y1, X2, Y2) -> [eg_pdf_op:i2s(X1), " ", eg_pdf_op:i2s(Y1), " m ",
                         eg_pdf_op:i2s(X2), " ", eg_pdf_op:i2s(Y2), " l S "].


-define(B_SHIFT_C, 99).
-define(C_SHIFT_B, 100).
-define(START_A, 103).
-define(START_B, 104).
-define(START_C, 105).
-define(STOP,    106).

-define(DIGIT(X), X >= $0, X =< $9). 
-define(l2i(X), list_to_integer(X)).

-spec code128(string()) -> string().
code128(String0) ->
    {Start, Mode} = code128_start(String0),
    String1 = code128_conv(Mode, String0, []),
    CheckChar = code128_chk(String1, Start),
    Result = lists:flatten([Start | String1] ++ [CheckChar, ?STOP]),
    [code128_trans(X) || X <- Result].


%% @private
code128_trans(X) when X >= 95 -> X + 97;
code128_trans(X)              -> X + 32.


%% @private
code128_chk(String, StartChar) ->
    F = fun(C, {N, Acc}) -> {N + 1, Acc + (N * C)} end,
    {_, Sum} = lists:foldl(F, {1, StartChar}, String),
    (Sum rem 103).


%% @private
code128_conv(_, [], Acc) ->
    lists:reverse(Acc);

code128_conv(c, [A, B, C, D | R], Acc) when ?DIGIT(A), ?DIGIT(B),
                                            ?DIGIT(C), ?DIGIT(D) ->
    code128_conv(c, R, [?l2i([C, D]), ?l2i([A, B]) | Acc]);

code128_conv(c, R, Acc) ->
    code128_conv(b, R, [?C_SHIFT_B | Acc]);

code128_conv(b, R = [A, B, C, D | _], Acc) when ?DIGIT(A), ?DIGIT(B),
                                                ?DIGIT(C), ?DIGIT(D) ->
    code128_conv(c, R, [?B_SHIFT_C | Acc]);

code128_conv(b, [C | R], Acc) ->
    code128_conv(b, R, [C - 32 | Acc]).


%% @private
code128_start([A, B, C, D | _]) when ?DIGIT(A), ?DIGIT(B), ?DIGIT(C), ?DIGIT(D) ->
    {?START_C, c};

code128_start(_String0) ->
    {?START_B, b}.
