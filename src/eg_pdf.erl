%%% @author  <wright@servicelevel.net>
%%% @doc Generate PDF documents main api
%%% Created : 25 April 2010 by  <wright@servicelevel.net>

-module(eg_pdf).

-behaviour(gen_server).

%% API

-define(SERVER, eg_pdf).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3, start_link/1]).


-export([
    add_start_xref/2,
    add_trailer/4,
    add_xref/2,
    append_stream/2,
    begin_text/1,
    bezier/5, bezier/9, bezier_c/4, bezier_v/3, bezier_y/3,
    break_text/1,
    circle/3,
    cms/1,
    color/1,
    default_face/0,
    delete/1,
    ellipse/3,
    end_text/1,
    ensure_font_gets_loaded/2,
    export/1,
    get_page_no/1,
    get_state/1,
    get_string_width/3, get_string_width/4,
    grid/3,
    header/0,
    image/2, image/3, image/4,
    inBuiltFonts/0,
    inches/1,
    kernedtext/2,
    line/2, line/3, line/5,
    lines/2,
    mirror_xaxis/2, mirror_yaxis/2,
    move_to/2,
    new/0,
    new_page/1,
    page_script/2,
    pagesize/1, pagesize/2,
    path/2,
    picas/1,
    points/1,
    poly/2,
    rectangle/3, rectangle/4, rectangle/5, rectangle/6,
    restore_state/1,
    rotate/2,
    round_rect/4,
    round_top_rect/4,
    save_state/1,
    scale/3,
    set_author/2,
    set_char_space/2,
    set_dash/2, set_dash/3,
    set_date/4,
    set_fill_color/2, set_fill_color_CMYK/5, set_fill_color_RGB/4,
    set_fill_gray/2,
    set_font/3,
    set_keywords/2,
    set_line_cap/2,
    set_line_join/2,
    set_line_width/2,
    set_miter_limit/2,
    set_page/2,
    set_pagesize/2, set_pagesize/3,
    set_stroke_color/2, set_stroke_color_CMYK/5, set_stroke_color_RGB/4,
    set_stroke_gray/2,
    set_subject/2,
    set_text_leading/2,
    set_text_pos/3,
    set_text_rendering/2,
    set_text_rise/2,
    set_text_scale/2,
    set_title/2,
    set_word_space/2,
    skew/3,
    text/2,
    text_rotate/2,
    text_rotate_position/4,
    text_transform/7,
    textbr/2,
    transform/7,
    translate/3,
    xref/2
]).

-include("eg.hrl").
-include("eg_erltypes.hrl").


%% @doc Set up Info, Catalog and Pages
%% @private
-spec init_pdf_context() -> #pdfContext{}.
init_pdf_context() ->
    {{Year, Month, Day}, {Hrs, Min, Sec}} = calendar:local_time(),
    Info = #info{creator      = "Erlang",
                 creationDate = {{Year, Month, Day}, {Hrs, Min, Sec}},
                 producer     = "erlguten-3.1",
                 author       = "",
                 title        = "",
                 subject      = "",
                 keywords     = "ErlangKeyword"},
    #pdfContext{info        = Info,
                images      = dict:new(),
                fonts       = [],
                currentpage = 1,
                mediabox    = pagesize(a4)}.

%%
%% --------------------- User functions --------------
%%

%% @doc Spawn pdf building process
-spec new() -> pdf_server_pid().
new() ->
    {ok, PDF} = start_link([init_pdf_context(), <<>>]),
    PDF.


%% @doc Export to PDF file format 
%% return:
-spec export(pdf_server_pid()) -> {PDFDoc :: binary(), PageNo :: integer()} | none().
export(PID) ->
    case gen_server:call(PID, {export}, infinity) of
        {export, PDFDoc, PageNo} ->
            {PDFDoc, PageNo};
        {'EXIT', PID, Reason} ->
            exit(Reason)
    end.


%% @doc clear up - delete pdf building process
-spec delete(pdf_server_pid()) -> ok.
delete(PID)->
    gen_server:cast(PID, {delete}).


%% @doc return the state of the server
get_state(PID) ->
    gen_server:call(PID, {get_state}).


%% @doc Add current page context to PDF document and start on a new page
%% Note page 1 is already created by default and current page set
%% to it after creation of PDF context.
-spec new_page(pdf_server_pid()) -> non_neg_integer() | none().
new_page(PID) ->
    case gen_server:call(PID, {get_new_page}, infinity) of
        {page, PageNo} ->
            PageNo;
        {'EXIT', PID, Reason} ->
            exit(Reason)
    end.


-spec page_script(pdf_server_pid(), any()) -> ok.
page_script(PID, Script) ->
    gen_server:cast(PID, {page_script, Script}).


%% @doc Go to a page already created.    
-spec set_page(pdf_server_pid(), non_neg_integer()) -> ok.
set_page(PID, PageNo)->
    gen_server:cast(PID, {page,{set, PageNo}}).


%% @doc Useful for page numbering functions etc.
-spec get_page_no(pdf_server_pid()) -> non_neg_integer().
get_page_no(PID)->
   case gen_server:call( PID, {get_page_no}, infinity) of
        {page,PageNo}->
            PageNo;
        {'EXIT', PID, Reason} ->
            exit(Reason)
    end.

%%
%% --- Info -----
%%

%% @doc set the Author atribute of the PDF
-spec set_author(pdf_server_pid(), string()) -> ok.
set_author(PID, Author) ->
    gen_server:cast(PID, {info, {author, Author}}).


%% @doc set the Title atribute of the PDF
-spec set_title(pdf_server_pid(), string()) -> ok.
set_title(PID, Title) ->
    gen_server:cast(PID, {info, {title, Title}}).


%% @doc set the Subject atribute of the PDF
-spec set_subject(pdf_server_pid(), string()) -> ok.
set_subject(PID, Subject) ->
    gen_server:cast(PID, {info, {subject, Subject}}).


%% @doc set the Date atribute of the PDF
-spec set_date(pdf_server_pid(), pos_integer(), pos_integer(), pos_integer()) -> ok.
set_date(PID, Year, Month, Day) ->
    gen_server:cast(PID, {info, {date, {Year, Month, Day}}}).


%% @doc set the Keywords atribute of the PDF
set_keywords(PID, Keywords) ->
    gen_server:cast(PID, {info, {keywords, Keywords}}).

%%
%% --- Page ---
%%

%% @doc pagesize returns: bounding box {Xleft, Ybottom, Xright, Ytop}
%%         full pages are always = {0, 0, Width, Height}
-spec pagesize(atom()) -> pagesize_t().
pagesize(a0) -> pagesize(2380, 3368);
pagesize(a1) -> pagesize(1684, 2380);
pagesize(a2) -> pagesize(1190, 1684);
pagesize(a3) -> pagesize(842, 1190);
pagesize(a4) -> pagesize(595, 842);
pagesize(a5) -> pagesize(421, 595);
pagesize(a6) -> pagesize(297, 421);
pagesize(a7) -> pagesize(210, 297);
pagesize(a8) -> pagesize(148, 210);
pagesize(a9) -> pagesize(105, 148);
pagesize(b0) -> pagesize(2836, 4008);
pagesize(b1) -> pagesize(2004, 2836);
pagesize(b2)        -> pagesize(1418, 2004);
pagesize(b3)        -> pagesize(1002, 1418);
pagesize(b4)        -> pagesize(709, 1002);
pagesize(b5)        -> pagesize(501, 709);
pagesize(b6)        -> pagesize(355, 501);
pagesize(b7)        -> pagesize(250, 355);
pagesize(b8)        -> pagesize(178, 250);
pagesize(b9)        -> pagesize(125, 178);
pagesize(b10)       -> pagesize(89, 125);
pagesize(c5e)       -> pagesize(462, 649);
pagesize(comm10e)   -> pagesize(298, 683);
pagesize(dle)       -> pagesize(312, 624);
pagesize(executive) -> pagesize(542, 720);
pagesize(folio)     -> pagesize(595, 935);
pagesize(ledger)    -> pagesize(1224, 792);
pagesize(legal)     -> pagesize(612, 1008);
pagesize(letter)    -> pagesize(612, 792);
pagesize(tabloid)   -> pagesize(792, 1224).


%% @doc create a full page bounding box for a page of size Width x Height
-spec pagesize(integer(), integer()) -> pagesize_t().
pagesize(Width, Height) -> {0, 0, Width, Height}.


-spec set_pagesize(pdf_server_pid(), atom()) -> ok.
set_pagesize(PID, Size) ->
    gen_server:cast(PID, {mediabox, pagesize(Size)}).


-spec set_pagesize(pdf_server_pid(), integer(), integer()) -> ok.
set_pagesize(PID, Width, Height) ->
    gen_server:cast(PID, {mediabox, pagesize(Width, Height)}).

%%
%% -- Fonts --
%%

-spec set_font(pdf_server_pid(), string(), integer()) -> ok.
set_font(PID, Fontname, Size) ->
    gen_server:cast(PID, {font, {set, Fontname, Size}}).


-spec ensure_font_gets_loaded(pdf_server_pid(), string()) -> ok.
ensure_font_gets_loaded(PID, FontName) ->
    gen_server:cast(PID, {ensure_font, FontName}).


%% @doc This function is a bit expensive, but will stick to the public interface.
-spec get_string_width(pdf_server_pid(), string(), integer(), string()) -> integer().
get_string_width(_PID, Fontname, PointSize, Str)->
    get_string_width(Fontname, PointSize, Str).


-spec get_string_width(string(), integer(), string()) -> integer().
get_string_width(Fontname, PointSize, Str)->
    {richText, Inline} = eg_richText:str2richText(Fontname, PointSize,
                                                  0, default, true, Str),
    SumLength = lists:foldl(
        fun(A, Accu) -> eg_richText:width(A) + Accu end,
        0, Inline
    ),
    trunc(SumLength / 1000).

%%
%% units of measure
%%

-spec points(number()) -> number().
points(X) -> X.

-spec picas(number()) -> number().
picas(X) -> X * 6.

-spec inches(number()) -> integer().
inches(X) -> round(X * 72.21).

-spec cms(number()) -> integer().
cms(X) -> round((X * 72.21) / 2.54).

%% @spec color(Color::atom() | {R,G,B}) -> {R,G,B}
%% @doc  R,G,B = 0-255
%%      This may be useful to lookup the rgb value of the color names
-spec color(atom() | rgb8_t()) -> rgb8_t().
color(Color) ->
    eg_pdf_op:color(Color).

%%
%% Text
%%

-spec begin_text(pdf_server_pid()) -> ok.
begin_text(PID) -> append_stream(PID, eg_pdf_op:begin_text()).


-spec end_text(pdf_server_pid()) -> ok.
end_text(PID) -> append_stream(PID, eg_pdf_op:end_text()).


-spec break_text(pdf_server_pid()) -> ok.
break_text(PID) -> append_stream(PID, eg_pdf_op:break_text()).


-spec text(pdf_server_pid(), iolist()) -> ok.
text(PID, Text) -> append_stream(PID, eg_pdf_op:text(Text)).


-spec textbr(pdf_server_pid(), iolist()) -> ok.
textbr(PID, Text) -> append_stream(PID, eg_pdf_op:textbr(Text)).


-spec kernedtext(pdf_server_pid(), string()) -> ok.
kernedtext(PID, Text) -> append_stream(PID, eg_pdf_op:kernedtext(Text)).


-spec set_text_pos(pdf_server_pid(), number(), number()) -> ok.
set_text_pos(PID, X, Y) -> append_stream(PID, eg_pdf_op:set_text_pos(X, Y)).


-spec set_text_leading(pdf_server_pid(), number()) -> ok.
set_text_leading(PID, L) -> append_stream(PID, eg_pdf_op:set_text_leading(L)).


-spec set_text_rendering(pdf_server_pid(), atom() | integer()) -> ok.
set_text_rendering(Pid, Mode) ->
    append_stream(Pid, eg_pdf_op:set_text_rendering(Mode)).


-spec set_char_space(pdf_server_pid(), number()) -> ok.
set_char_space(PID, CS) -> append_stream(PID, eg_pdf_op:set_char_space(CS) ).


-spec set_word_space(pdf_server_pid(), number()) -> ok.
set_word_space(PID, WS) -> append_stream(PID, eg_pdf_op:set_word_space(WS) ).


-spec set_text_scale(pdf_server_pid(), integer()) -> ok.
set_text_scale(PID, SC) -> append_stream(PID, eg_pdf_op:set_text_scale(SC) ).


-spec set_text_rise(pdf_server_pid(), integer()) -> ok.
set_text_rise(PID, Rise) -> append_stream(PID, eg_pdf_op:set_text_rise(Rise)).

%%
%% Graphics operators
%%

-spec path(pdf_server_pid(), eg_pdf_op:path_t()) -> ok.
path(PID, Type) -> append_stream(PID, eg_pdf_op:path(Type)).


-spec move_to(pdf_server_pid(), {number(), number()}) -> ok.
move_to(PID, P) -> append_stream(PID, eg_pdf_op:move_to(P)).


-spec line(pdf_server_pid(), eg_pdf_op:xy_xy_t()) -> ok.
line(PID, From_To) -> append_stream(PID, eg_pdf_op:line(From_To)).


-spec line(pdf_server_pid(), eg_pdf_op:xy_t(), eg_pdf_op:xy_t()) -> ok.
line(PID, From, To) -> line(PID, {From, To}).


-spec line(pdf_server_pid(), number(), number(), number(), number()) -> ok.
line(PID, X1, Y1, X2, Y2) -> line(PID, {{X1, Y1}, {X2, Y2}}).


-spec lines(pdf_server_pid(), list(eg_pdf_op:xy_xy_t())) -> ok.
lines(PID, LineList) -> append_stream(PID, eg_pdf_op:lines(LineList)).

%% @doc Poly paths should be stroked/closed/filled with separate command.
-spec poly(pdf_server_pid(), list(eg_pdf_op:xy_t())) -> ok.
poly(PID, Points) -> append_stream(PID, eg_pdf_op:poly(Points)).


%% @doc Grid assumes sorted XLists and YList, minimum value first
grid(PID, XList, YList) -> append_stream(PID, eg_pdf_op:grid(XList, YList)).


%% @doc This moves to X1,Y1 point as its start and then creates a cubic Bezier
%% curve to X4,Y4 using the points in between as the control points. Bezier
%% paths should be stroked/closed/filled with a separate command.
-spec bezier(pdf_server_pid(), eg_pdf_op:xy_t(), eg_pdf_op:xy_t(),
             eg_pdf_op:xy_t(), eg_pdf_op:xy_t()) -> ok.
bezier(PID, {X1, Y1}, {X2, Y2}, {X3, Y3}, {X4, Y4}) ->
    append_stream(PID, eg_pdf_op:bezier({X1, Y1}, {X2, Y2}, {X3, Y3}, {X4, Y4})).


%% @doc This moves to X1,Y1 point as its start and then creates a cubic Bezier
%% curve to X4,Y4 using the points in between as the control points. Bezier
%% paths should be stroked/closed/filled with a separate command.
-spec bezier(pdf_server_pid(),
             number(), number(), number(), number(),
             number(), number(), number(), number()) -> ok.
bezier(PID, X1, Y1, X2, Y2, X3, Y3, X4, Y4) ->
    bezier(PID, {X1, Y1}, {X2, Y2}, {X3, Y3}, {X4, Y4}).


%% @doc This takes the current point as its start and then creates a cubic
%% Bezier curve to Point3 using the points in between as the control points.
%% Bezier paths should be stroked/closed/filled with a separate command.
-spec bezier_c(pdf_server_pid(), eg_pdf_op:xy_t(), eg_pdf_op:xy_t(),
               eg_pdf_op:xy_t()) -> ok.
bezier_c(PID, Point1, Point2, Point3) ->
    append_stream(PID, eg_pdf_op:bezier_c(Point1, Point2, Point3)).


%% @doc This takes the current point as its start and then creates a cubic
%% Bezier curve to Point2 using the current point and Point1 as the control
%% points. Bezier paths should be stroked/closed/filled with a separate command.
-spec bezier_v(pdf_server_pid(), eg_pdf_op:xy_t(), eg_pdf_op:xy_t()) -> ok.
bezier_v(PID, Point1, Point2) ->
    append_stream(PID, eg_pdf_op:bezier_v(Point1, Point2)).


%% @doc This takes the current point as its start and then creates a cubic
%% Bezier curve to Point3 using the Point1 and Point3 as the control points.
%% Bezier paths should be stroked/closed/filled with a separate command.
-spec bezier_y(pdf_server_pid(), eg_pdf_op:xy_t(), eg_pdf_op:xy_t()) -> ok.
bezier_y(PID, Point1, Point3) ->
    append_stream(PID, eg_pdf_op:bezier_y(Point1, Point3)).


-spec circle(pdf_server_pid(), eg_pdf_op:xy_t(), number()) -> ok.
circle(PID, {X, Y}, R) ->
    append_stream(PID, eg_pdf_op:circle({X, Y}, R)).


-spec ellipse(pdf_server_pid(), eg_pdf_op:xy_t(), eg_pdf_op:xy_t()) -> ok.
ellipse(PID, {X, Y}, {RX, RY})->
    append_stream(PID, eg_pdf_op:ellipse({X, Y}, {RX, RY})).


%% @doc Stroke a rectangle area. If Stroke Type is not appended in arguments,
%% explicit stroke command "path(StrokeType)" has to be executed.
%% X, Y designate the lower left corner of the rectangle.
-spec rectangle(pdf_server_pid(), eg_pdf_op:xy_t(), eg_pdf_op:xy_t()) -> ok.
rectangle(PID, {X, Y}, {WX, WY}) ->
    rectangle(PID, X, Y, WX, WY).


%% @doc Stroke a rectangle area. If Stroke Type is not appended in arguments,
%% explicit stroke command "path(StrokeType)" has to be executed.
%% X, Y designate the lower left corner of the rectangle.
-spec rectangle(pdf_server_pid(), eg_pdf_op:xy_t(), eg_pdf_op:xy_t(),
                eg_pdf_op:path_t()) -> ok.
rectangle(PID, {X, Y}, {WX, WY}, StrokeType) ->
    rectangle(PID, X, Y, WX, WY, StrokeType).


%% @doc Stroke a rectangle area. If Stroke Type is not appended in arguments,
%% explicit stroke command "path(StrokeType)" has to be executed.
%% X, Y designate the lower left corner of the rectangle.
-spec rectangle(pdf_server_pid(), number(), number(), number(), number()) -> ok.
rectangle(PID, X, Y, WX, WY) when is_pid(PID) ->
    append_stream(PID, eg_pdf_op:rectangle(X, Y, WX, WY)).


%% @doc Stroke a rectangle area. If Stroke Type is not appended in arguments,
%% explicit stroke command "path(StrokeType)" has to be executed.
%% X, Y designate the lower left corner of the rectangle.
-spec rectangle(pdf_server_pid(), number(), number(), number(), number(),
                eg_pdf_op:path_t()) -> ok.
rectangle(PID, X, Y, WX, WY, Option) ->
    append_stream(PID, eg_pdf_op:rectangle(X, Y, WX, WY, Option)).


-spec round_rect(pdf_server_pid(), eg_pdf_op:xy_t(), eg_pdf_op:xy_t(),
                 number()) -> ok.
round_rect(PID, Point, Size, Radius)->
    append_stream(PID, eg_pdf_op:round_rect(Point, Size, Radius)).


-spec round_top_rect(pdf_server_pid(), eg_pdf_op:xy_t(), eg_pdf_op:xy_t(),
                     number()) -> ok.
round_top_rect(PID, Point, Size, Radius)->
    append_stream(PID, eg_pdf_op:round_top_rect(Point, Size, Radius)).


%%
%% Line styles
%%

-spec set_line_width(pdf_server_pid(), number()) -> ok.
set_line_width(PID, W) ->
    append_stream(PID, eg_pdf_op:set_line_width(W)).


-spec set_line_cap(pdf_server_pid(), eg_pdf_op:line_cap_t()) -> ok.
set_line_cap(PID, Mode) ->
    append_stream(PID, eg_pdf_op:set_line_cap(Mode)).


-spec set_line_join(pdf_server_pid(), eg_pdf_op:line_join_t()) -> ok.
set_line_join(PID, Mode)->
    append_stream(PID, eg_pdf_op:set_line_join(Mode)).


-spec set_miter_limit(pdf_server_pid(), number()) -> ok.
set_miter_limit(PID, Limit) ->
    append_stream(PID, eg_pdf_op:set_miter_limit(Limit)).


-spec set_dash(pdf_server_pid(), eg_pdf_op:dash_t()) -> ok.
set_dash(PID, Mode) ->
    append_stream(PID, eg_pdf_op:set_dash(Mode)).


-spec set_dash(pdf_server_pid(), list(number()), number()) -> ok.
set_dash(PID, Array, Phase) ->
    append_stream(PID, eg_pdf_op:set_dash(Array, Phase)).


%% @doc Push graphics state
-spec save_state(pdf_server_pid()) -> ok.
save_state(PID) ->
    append_stream(PID, eg_pdf_op:save_state()).


%% @doc Pop graphics state
-spec restore_state(pdf_server_pid()) -> ok.
restore_state(PID) ->
    append_stream(PID, eg_pdf_op:restore_state()).


%% @doc Change geometry
-spec transform(pdf_server_pid(), number(), number(), number(),
                number(), number(), number()) -> ok.
transform(PID, A, B, C, D, E, F) ->
    append_stream(PID, eg_pdf_op:transform(A, B, C, D, E, F)).


%% @doc Change geometry
-spec text_transform(pdf_server_pid(), number(), number(), number(),
                     number(), number(), number()) -> ok.
text_transform(PID, A, B, C, D, E, F)->
    append_stream(PID, eg_pdf_op:text_transform(A, B, C, D, E, F)).


-spec translate(pdf_server_pid(), number(), number()) -> ok.
translate(PID, X, Y) ->
    append_stream(PID, eg_pdf_op:translate(X, Y)).


-spec scale(pdf_server_pid(), integer(), integer()) -> ok.
scale(PID, ScaleX, ScaleY)
    when is_integer(ScaleX), is_integer(ScaleY) ->
    append_stream(PID, eg_pdf_op:scale(ScaleX, ScaleY)).


%% @doc Set rotation in degrees.
-spec rotate(pdf_server_pid(), number()) -> ok.
rotate(PID, Angle) ->
    append_stream(PID, eg_pdf_op:rotate(Angle)).


%% @doc Set text rotation in degrees.
-spec text_rotate(pdf_server_pid(), number()) -> ok.
text_rotate(PID, Angle)->
    append_stream(PID, eg_pdf_op:text_rotate(Angle)).


-spec text_rotate_position(pdf_server_pid(), number(), number(), number()) -> ok.
text_rotate_position(PID, X, Y, Angle) ->
    append_stream(PID, eg_pdf_op:text_rotate_position(X, Y, Angle)).


-spec skew(pdf_server_pid(), number(), number()) -> ok.
skew(PID, XSkewAngle, YSkewAngle) ->
    append_stream(PID, eg_pdf_op:skew(XSkewAngle, YSkewAngle)).


-spec mirror_yaxis(pdf_server_pid(), number()) -> ok.
mirror_yaxis(PID, Xtranslate) ->
    append_stream(PID, eg_pdf_op:mirror_yaxis(Xtranslate)).


-spec mirror_xaxis(pdf_server_pid(), number()) -> ok.
mirror_xaxis(PID, Ytranslate) ->
    append_stream(PID, eg_pdf_op:mirror_xaxis(Ytranslate)).


%% @doc Set color value range 0 - 1
-spec set_fill_color_CMYK(pdf_server_pid(), number(), number(), number(),
                          number()) -> ok.
set_fill_color_CMYK(PID, C, M, Y, K) ->
    append_stream(PID, eg_pdf_op:set_fill_color_CMYK(C, M, Y, K)).


-spec set_stroke_color_CMYK(pdf_server_pid(), number(), number(), number(),
                            number()) -> ok.
set_stroke_color_CMYK(PID, C, M, Y, K) ->
    append_stream(PID, eg_pdf_op:set_stroke_color_CMYK(C, M, Y, K)).


%% @doc Set fill color, value range 0 - 1
-spec set_fill_color_RGB(pdf_server_pid(), number(), number(), number()) -> ok.
set_fill_color_RGB(PID, R, G, B) ->
    append_stream(PID, eg_pdf_op:set_fill_color_RGB(R, G, B)).


-spec set_stroke_color_RGB(pdf_server_pid(), number(), number(), number()) -> ok.
set_stroke_color_RGB(PID, R, G, B) ->
    append_stream(PID, eg_pdf_op:set_stroke_color_RGB(R, G, B)).


-spec set_fill_color(pdf_server_pid(), atom() | rgb8_t()) -> ok.
set_fill_color(PID, Color)->
    append_stream(PID, eg_pdf_op:set_fill_color(Color)).


-spec set_stroke_color(pdf_server_pid(), atom() | rgb8_t()) -> ok.
set_stroke_color(PID, Color) ->
    append_stream(PID, eg_pdf_op:set_stroke_color(Color)).


%% @doc Grayscale fill color; 0.0-Black 1.0-White)
-spec set_fill_gray(pdf_server_pid(), number()) -> ok.
set_fill_gray(PID, Gray) ->
    append_stream(PID, eg_pdf_op:set_fill_gray(Gray)).


%% @doc Grayscale strole color; 0.0-Black 1.0-White)
-spec set_stroke_gray(pdf_server_pid(), number()) -> ok.
set_stroke_gray(PID, Gray) ->
    append_stream(PID, eg_pdf_op:set_stroke_gray(Gray)).


%% Images
%% image(PID, FilePath )
%% image(PID, FilePath, Size)
%% image(PID, FilePath, Pos, Size)
%% Pos is {X,Y}
%% Size is {width, W} | {height, H} | {W,H} | {max, W, H} 
%% The max Size version can be used to set a max limit on width, height or both
%% dimensions (undefined is a valid value for at most 1 W or H value)

-spec image(pdf_server_pid(), file:name_all()) -> ok | {error, _}.
image(PID, FilePath) ->
    save_state(PID),
    case image1(PID, FilePath, {size, {undefined, undefined}}) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            restore_state(PID)
    end.


-spec image(pdf_server_pid(), file:name_all(), eg_pdf_op:xy_t()) -> ok | {error, _}.
image(PID, FilePath, Size) ->
    save_state(PID),
    case image1(PID, FilePath, Size) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            restore_state(PID)
    end.


-spec image(pdf_server_pid(), file:name_all(), eg_pdf_op:xy_t(),
            img_size_t()) -> ok | {error, _}.
image(PID, FilePath, {X, Y}, Size) ->
    save_state(PID),
    translate(PID, X, Y),
    case image1(PID, FilePath, Size) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            restore_state(PID)
    end.


%% @private
-spec image1(pdf_server_pid(), file:name_all(), img_size_t()) -> ok | {error, _}.
image1(PID, FilePath, {max, undefined, H}) ->
    image1(PID, FilePath, {height, H});

image1(PID, FilePath, {max, W, undefined}) ->
    image1(PID, FilePath, {width, W});

image1(PID, FilePath, {max, W, H}) ->
    image1(PID, FilePath, {size, {max, W, H}});

image1(PID, FilePath, {width, W}) ->
    image1(PID, FilePath, {size, {W, undefined}});

image1(PID, FilePath, {height, H}) ->
    image1(PID, FilePath, {size, {undefined, H}});

image1(PID, FilePath, {W, H}) when is_integer(W), is_integer(H) ->
    image1(PID, FilePath, {size, {W, H}});

image1(PID, FilePath, {size, Size}) ->
    case file:open(FilePath, [read]) of
        {ok, IO} ->
            file:close(IO),
            gen_server:cast(PID, {image, FilePath, Size});
        {error, OpenError} ->
            {error, OpenError}
    end.

%%
%% Internals
%%

-spec append_stream(pdf_server_pid(), iolist()) -> ok.
append_stream(PID, String) ->
    gen_server:cast(PID, {stream, {append, String}}).


-spec default_face() -> #face{}.
default_face() ->
    eg_richText:mk_face("Times-Roman", 12, true, default, 0).


inBuiltFonts() ->
    ["Helvetica", "Helvetica-Bold", "Helvetica-Oblique", "Helvetica-BoldOblique",
     "Times-Roman", "Times-Bold", "Times-Italic", "Times-BoldItalic",
     "Courier", "Courier-Bold", "Courier-Oblique", "Courier-BoldOblique",
     "Symbol", "ZapfDingbats"].


%%====================================================================
%% API
%%====================================================================

start_link(Init) ->
    gen_server:start_link({local,pdf}, ?MODULE, Init, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Init) ->
    {ok, Init}.


handle_call({get_page_no}, _From, [PDFC, Stream]) ->
    {reply, {page, PDFC#pdfContext.currentpage}, [PDFC, Stream]};

handle_call({get_new_page}, _From, [PDFC, Stream]) ->
    {Add, PageNo} = handle_newpage(PDFC#pdfContext.pages,
                                   PDFC#pdfContext.currentpage,
                                   [Stream]),
    {reply,
     {page, PageNo},
     [PDFC#pdfContext{pages = Add, currentpage = PageNo}, <<>>]
    };

handle_call({export}, _From, [PDFC, Stream]) ->
    %% add last page if necessary before exporting
    {PDF, PNo} = case Stream of
                     <<>> ->
                         PageNo = PDFC#pdfContext.pages,
                         {handle_export(PDFC), PageNo};
                     _ ->
                         {Add, PageNo} = handle_newpage(
                             PDFC#pdfContext.pages,
                             PDFC#pdfContext.currentpage,
                             [Stream]
                         ),
                         {handle_export(PDFC#pdfContext{pages = Add}), PageNo}
                 end,
    {reply, {export, PDF, PNo}, [PDFC, Stream]};

handle_call({get_state}, _From, [PDFC, Stream]) ->
    {reply, [PDFC, Stream], [PDFC, Stream]}.


handle_cast({mediabox, Mediabox}, [PDFC, Stream]) ->
    {noreply, [PDFC#pdfContext{mediabox = Mediabox}, Stream]};

handle_cast({delete}, [PDFC, Stream]) ->
    {stop, normal, [PDFC, Stream]};

handle_cast({font, {set, Fontname, Size}}, [PDFC, Stream]) ->
    {F, Alias, Fhand} = handle_setfont(PDFC#pdfContext.fonts, Fontname),
    S = list_to_binary(eg_pdf_op:set_font_by_alias(Alias, Size)),
    Binary = <<Stream/binary, S/binary>>,
    {noreply, [PDFC#pdfContext{fonts        = F,
                               font_handler = Fhand}, Binary]};

handle_cast({info, Info}, [PDFC, Stream]) ->
    NewInfo = pdf_handle_info(PDFC#pdfContext.info, Info),
    {noreply, [PDFC#pdfContext{info = NewInfo}, Stream]};

handle_cast({stream, {append, String}}, [PDFC, Stream]) ->
    B = list_to_binary(convert(PDFC#pdfContext.font_handler, String)),
    Binary = <<Stream/binary, B/binary, <<" ">>/binary>>,
    {noreply, [PDFC, Binary]};

handle_cast({image, FilePath, Size}, [PDFC, Stream]) ->
    {I, IMG, {W, H}, ProcSet} = handle_image(PDFC#pdfContext.images,
                                             FilePath, Size,
                                             PDFC#pdfContext.procset),
    S = list_to_binary(eg_pdf_op:set_image(W, H, IMG)),
    Binary = <<Stream/binary, S/binary>>,
    {noreply, [PDFC#pdfContext{images  = I,
                               procset = ProcSet}, Binary]};

handle_cast({page_script, Script}, [PDFC, Stream]) ->
    NewScript = handle_pagescript(PDFC#pdfContext.scripts,
                                  PDFC#pdfContext.currentpage,
                                  Script),
    {noreply, [PDFC#pdfContext{scripts = NewScript}, Stream]};

handle_cast({page, {set, PageNo}}, [PDFC, Stream]) ->
    {NewPages, [NewStream]} = handle_setpage(PDFC#pdfContext.pages, PageNo,
                                             PDFC#pdfContext.currentpage,
                                             [Stream]),
    {noreply, [PDFC#pdfContext{pages       = NewPages,
                               currentpage = PageNo}, NewStream]};

handle_cast({ensure_font, Fontname}, [PDFC, Stream]) ->
    F = ensure_font(eg_font_map:handler(Fontname), PDFC#pdfContext.fonts),
    {noreply, [PDFC#pdfContext{fonts = F}, Stream]}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%% @private
build_pdf(Info, Fonts, Images, Pages, MediaBox, ProcSet) ->
    {Free0, XObjects, O0s} = eg_pdf_image:mk_images(Images, 1, [], []),
    {Free, Fonts1, O1s} = mk_fonts(Fonts, Free0, [], []),
    PageTree = Free,
    {Free1, Ps, O3s} = mk_pages(Pages, PageTree, Free + 1, [], []),

    O2 = {{obj, PageTree, 0},
          mk_page_tree(Ps, Fonts1, XObjects, MediaBox, ProcSet)},
    Root = Free1,
    O4 = {{obj, Root, 0}, mk_catalogue(PageTree)},

    NInfo = Free1 + 1,
    O5 = {{obj, NInfo, 0}, mk_info(Info)},
    {Root, NInfo, O0s ++ O1s ++ [O2 | O3s] ++ [O4, O5]}.


%% @private
mk_fonts([], I, Fs, Os) ->
    A = {{obj, I, 0},
         {dict, lists:map(
             fun({Alias, FontObj}) -> {Alias, {ptr, FontObj, 0}} end,
             lists:reverse(Fs))
         }},
    {I + 1, {ptr, I, 0}, lists:reverse([A | Os])};

mk_fonts([Handler | T], I, Fs, E) ->
    Index = Handler:index(),
    Alias = "F" ++ eg_pdf_op:i2s(Index),
    case Handler:type() of
        internal ->
            O = {{obj, I, 0}, mk_font(Handler)},
            mk_fonts(T, I + 1, [{Alias, I} | Fs], [O | E]);
        {Index, pdf_builtin} ->
            O1 = {{obj, I, 0}, mk_font_1(Handler, I + 1, Index)},
            O2 = {{obj, I + 1, 0}, mk_font_descriptor(Handler, false, 0)},
            mk_fonts(T, I + 2, [{Alias, I} | Fs], [O2, O1 | E]);
        external ->
            O1 = {{obj, I, 0}, mk_font_1(Handler, I + 1, Index)},
            O2 = {{obj, I + 1, 0}, mk_font_descriptor(Handler, true, I + 2)},
            O3 = {{obj, I + 2, 0}, mk_font_file(Handler)},
            mk_fonts(T, I + 3, [{Alias, I} | Fs], [O3, O2, O1 | E])
    end.


%% @private
mk_pages([], _, N, P, O) -> {N, lists:reverse(P), lists:reverse(O)};

mk_pages([{page, Str} | T], Parent, I, L, E) ->
    O1 = {{obj, I, 0}, mk_page_contents(Str)},
    O2 = {{obj, I + 1, 0}, mk_page(Parent, I)},
    mk_pages(T, Parent, I + 2, [I + 1 | L], [O2, O1 | E]);

mk_pages([{page, Str, Script} | T], Parent, I, L, E) ->
    O1 = {{obj, I, 0}, mk_page_contents(Str)},
    O2 = {{obj, I + 1, 0}, mk_script(Script)},
    O3 = {{obj, I + 2, 0}, mk_page(Parent, I, I + 1)},
    mk_pages(T, Parent, I + 3, [I + 2 | L], [O3, O2, O1 | E]).


%% @private
mk_catalogue(PageTree) ->
    {dict, [{"Type", {name, "Catalog"}},
            {"Pages", {ptr, PageTree, 0}}]}.


%% @doc mkFont is used for the 14 inbuilt fonts
%% @private
mk_font(FontHandler) ->
    Index = FontHandler:index(),
    Alias = "F" ++ eg_pdf_op:i2s(Index),
    %% io:format("mkFont Alias=~s FontHandler=~p~n",[Alias, FontHandler]),
    {dict, [{"Type", {name, "Font"}},
            {"Subtype", {name, "Type1"}},
            {"Name", {name, Alias}},
            {"BaseFont", {name, FontHandler:fontName()}},
            {"Encoding", {name, encoding(FontHandler)}}]}.

%% @doc Change the encoding to "MacRomanEncoding" except for "FontSpecific"
%% encodings...
%% This seems to work for everything except those fonts which have a
%% "FontSpecif" encoding. usually the encoding in the AFM file is
%% "AdobeStandardEncoding" - but this gives an error for fonts with encoding
%% "AppleStandard". Setting *everything* to MacRomanEncoding seems to work for
%% all cases except Zapfdingblats which is "FontSpecific"
%% - this might not work with files produced on an apple ?
%% - I have not yet tested this on an apple
%% @private
encoding(M) ->
    case M:encoding() of
        S = "FontSpecific" ->
            S;
%   	S = "AppleStandard" ->
%   	    "MacRomanEncoding";
%   	S = "AdobeStandardEncoding" ->
%   	    S;
        _ ->
            "MacRomanEncoding"
    end.


%% @private
mk_font_1(M, FontDescriptorPrt, Index) ->
    FirstChar = M:firstChar(),
    LastChar = M:lastChar(),
    Widths = make_width(M:encoding(), M, FirstChar, LastChar),
    {dict, [{"Type", {name, "Font"}},
            {"Subtype", {name, "Type1"}},
            {"Name", {name, "F" ++ eg_pdf_op:i2s(Index)}},
            {"BaseFont", {name, M:fontName()}},
            {"Encoding", {name, encoding(M)}},
            {"FirstChar", FirstChar},
            {"LastChar", LastChar},
            {"Widths", {array, Widths}},
            {"FontDescriptor", {ptr, FontDescriptorPrt, 0}}]}.

%% @private
make_width("AdobeStandardEncoding", M, F, L) ->
    Seq = lists:seq(F, L),
    Fu = fun(unknown) -> 0;
            (X) -> X
         end,
    Map = eg_convert:mac2pdf(Seq),
    [Fu(M:width(X)) || X <- Map];

make_width(_, M, _, _) ->
    M:widths().


%% @private
mk_font_descriptor(M, Embedded, I) ->
    {X1, X2, X3, X4} = M:fontBBox(),
    FontBBox = [X1, X2, X3, X4],
    D0 = [{"Type", {name, "FontDescriptor"}},
          {"Ascent", M:ascender()},
          {"CapHeight", M:capHeight()},
          {"Descent", M:descender()},
          {"Flags", M:flags()},
          {"FontBBox", {array, FontBBox}},
          {"FontName", {name, M:fontName()}},
          {"ItalicAngle", M:italicAngle()},
          {"StemV", M:stemV()},
          {"XHeight", M:xHeight()}],
    D = case Embedded of
            true ->
                [{"FontFile", {ptr, I, 0}} | D0];
            false ->
                D0
        end,
    {dict, D}.

%%          {{obj,8,0},
%%           {dict,[{"Type",{name,"FontDescriptor"}},
%%                  {"Ascent",890},
%%                  {"CapHeight",707},
%%                  {"Descent",65306},
%%                  {"Flags",6},
%%                  {"FontBBox",{array,[-100,-65311,1218,895]}},
%%                  {"FontName",{name,"UtopiaMedium"}},
%%                  {"ItalicAngle",0},
%%                  {"StemV",80},
%%                  {"XHeight",512},
%%                  {"FontFile",{ptr,9,0}}]}},
%%          {{obj,9,0},
%%           {stream,94215,
%%                   "stream_9_0_94215",
%%                   [{"Length",94215},
%%                    {"Length1",5750},
%%                    {"Length2",87922},
%%                    {"Length3",543}]}},


%% @private
-spec mk_font_file(module()) -> {stream, _, binary()}.
mk_font_file(Handler) ->
    {Len, Len1, Len2, Len3, Bin} = get_font_program(Handler),
    {stream,
     {dict, [{"Length", Len},
             {"Length1", Len1},
             {"Length2", Len2},
             {"Length3", Len3}]},
     Bin}.


%% @private
-spec this_dir() -> file:filename().
this_dir() ->
    filename:dirname(code:which(?MODULE)).


%% @private
-spec font_dir() -> file:filename().
font_dir() ->
    case code:priv_dir(erlguten) of
    %% TODO: Don't think this is correct
        {error, bad_name} ->
        % io:format(user,"no priv dir:~n",[]),
            filename:join(this_dir(), "../priv/fonts");
        N ->
            filename:join(N, "fonts")
    end.


%% @private
-spec get_font_program(module()) -> error | tuple().
get_font_program(Handler) ->
    File = filename:join(font_dir(), atom_to_list(Handler) ++ ".pfb"),
    io:format(user, "reading Font from:~s~n", [File]),
    P = eg_embed:parse_pfb(File),
    case P of
        [{_, L1, B1}, {_, L2, B2}, {_, L3, B3} | _] ->
            {L1 + L2 + L3, L1, L2, L3, list_to_binary([B1, B2, B3])};
        _ ->
            error
    end.


%% @private
-spec mk_info(#info{}) -> {dict, list({string(), _})}.
mk_info(I) ->
    {dict,
     [{"Creator", {string, I#info.creator}},
      {"CreationDate", {date, I#info.creationDate}},
      {"Producer", {string, I#info.producer}},
      {"Author", {string, I#info.author}},
      {"Title", {string, I#info.title}},
      {"Subject", {string, I#info.subject}},
      {"Keywords", {string, I#info.keywords}}]}.

%% L = [int()] = list of objects representing pages


%% @private
mk_page_tree(L, Fonts, XObjects, _MediaBox = {A, B, C, D}, ProcSet) ->
    ImProcSet = case ProcSet of
                    {imageb, imagec} -> [{name, "ImageB"}, {name, "ImageC"}];
                    {imageb, _} -> [{name, "ImageB"}];
                    {_, imagec} -> [{name, "ImageC"}];
                    _ -> []
                end,
    {dict,
     [{"Type", {name, "Page"}},
      {"Count", length(L)},
      {"MediaBox", {array, [A, B, C, D]}},
      {"Kids", {array, lists:map(fun(I) -> {ptr, I, 0} end, L)}},
      {"Resources",
       {dict, [{"Font", Fonts}, {"XObject", XObjects},
               {"ProcSet",
                {array, [{name, "PDF"}, {name, "Text"} | ImProcSet]}}]}}
     ]}.


%% Fonts = [{Name,PageNo}]
%%   example [{"F1",12},{"F7",15}]

%% @private
-spec mk_script(string()) -> dict_val_t().
mk_script(Script) ->
    {dict, [{"S", {name, "JavaScript"}},
            {"JS", {string, Script}}]}.


%% @private
-spec mk_page(number(), number()) -> dict_val_t().
mk_page(Parent, Contents) ->
    {dict, [{"Type", {name, "Page"}},
            {"Parent", {ptr, Parent, 0}},
            {"Contents", {ptr, Contents, 0}}
    ]}.


%% @private
-spec mk_page(number(), number(), number()) -> dict_val_t().
mk_page(Parent, Contents, Script) ->
    {dict,
     [{"Type", {name, "Page"}},
      {"Parent", {ptr, Parent, 0}},
      {"Contents", {ptr, Contents, 0}},
      {"AA", {dict, [{"O", {ptr, Script, 0}}]}}
     ]}.

% mkPage(Parent, Contents, Script) ->
%     {dict, [{"Type", {name,"Page"}},
% 	    {"Parent", {ptr,Parent,0}},
% 	    {"Contents", {ptr, Contents, 0}},
% 	    {"AA", {dict, [{"O", {ptr, Script, 0}}]}}
% 	   ]}.

%% @private
mk_page_contents(Str) ->
    {stream, Str}.


-spec header() -> string().
header() ->
    "%PDF-1.3" ++ [8#015, $%, 8#342, 8#343, 8#317, 8#323, 8#015, 8#012].

%% Objs = {ObjNo, Startpos}

-spec add_xref(file:iodevice() | atom(), list({_, _})) -> integer().
add_xref(F, Objs) ->
    {ok, P} = file:position(F, cur),
    XrefStart = P,
    Objs1 = lists:map(fun({_I, Pos}) -> xref(Pos, "00000 n") end, Objs),
    L = ["xref\n0 ",
         eg_pdf_op:i2s(length(Objs) + 1),
         "\n",
         xref(0, "65535 f")
        | Objs1],
    file:write(F, L),
    XrefStart.


-spec xref(integer(), string()) -> iolist().
xref(I, Str) ->
    S = lists:flatten(io_lib:format("~10.10.0w", [I])),
    [S, " ", Str, "\r\n"].


-spec add_trailer(file:iodevice() | atom(), list(), any(), any()) -> ok | {error, _}.
add_trailer(F, Objs, Root, Info) ->
    L = ["trailer << /Size ", eg_pdf_op:i2s(length(Objs)+1),
         " /Root ",eg_pdf_op:i2s(Root), " 0 R ",
         " /Info ",eg_pdf_op:i2s(Info), " 0 R >>\n"],
    file:write(F, L).

-spec add_start_xref(file:iodevice() | atom(), integer()) -> ok | {error, _}.
add_start_xref(F, XrefStartPos) ->
    L = ["startxref\n", eg_pdf_op:i2s(XrefStartPos), "\n%%EOF\n"],
    file:write(F, L).


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

%% Internals

%% @private
-spec handle_pagescript(orddict:orddict(), integer(), string()) -> orddict:orddict().
handle_pagescript(ScriptDict, PageNo, Script) ->
    orddict:store(PageNo, Script, ScriptDict).


%% @private
handle_setpage(Pages, PageNo, Current, Stream) ->
    NewPageDict = orddict:store(Current, Stream, Pages),
    NewStream = orddict:fetch(PageNo, NewPageDict),
    {NewPageDict, NewStream}.


%% @private
handle_newpage(Pages, 0, _Stream) ->
    {Pages, 1};

handle_newpage(Pages, Current, Stream) ->
    NewPageDict = orddict:store(Current, Stream, Pages),
    {NewPageDict, orddict:size(NewPageDict) + 1}.


%% @private
handle_export(PDFC) ->
    %% io:format("~nHere handle_export:~p~n",[PDFC]),
    MF = fun(_K, V1, V2) -> {script, V1, V2} end,
    Merged = orddict:merge(MF, PDFC#pdfContext.pages,
                           PDFC#pdfContext.scripts),
    Pages = lists:map(
        fun({_Key, {script, Val, S}}) ->
            {page, Val, S};
           ({_Key, Val}) ->
               {page, Val}
        end,
        Merged),
    {_Root, Ninfo, Os} = build_pdf(PDFC#pdfContext.info,
                                   PDFC#pdfContext.fonts,
                                   dict:to_list(PDFC#pdfContext.images),
                                   Pages,
                                   PDFC#pdfContext.mediabox,
                                   PDFC#pdfContext.procset),
    eg_pdf_lib:export(Ninfo, Os).
%%    Objs = lists:map(fun(I) -> serialise2bin(I) end , Os),
%%    eg_pdf_export:mkdoc(Objs, Root, Ninfo).

%% handle_setfont(FontList, FontName) ->
%%   {FontList1, Alias}
%% Alias = "F" ++ Index
%% alias is a name used in the PDF file to refer to the font.
%%

handle_setfont(FontList, FontName) ->
    case eg_font_map:handler(FontName) of
        undefined ->
            io:format("There is no font called:~s~n", [FontName]),
            io:format("Using Times-Roman~n"),
            handle_setfont(FontList, "Times-Roman");
        Handler ->
            Index = Handler:index(),
            {ensure_font(Handler, FontList), "F" ++ eg_pdf_op:i2s(Index), Handler}
    end.

ensure_font(Handler, FontList) ->
    case lists:member(Handler, FontList) of
        true ->
            FontList;
        false ->
            [Handler | FontList]
    end.

%% @doc  This updates the image dictionary from the pdfContext.images with this new image if
%% it's not already present. It also scales the image information to to fit the maximum
%% sizes received in the Size parameter. This may be {undefined,Height}, {Width, undefined} or {max, width, height}.
%% Filepath is the key into the dictionary. If a dictionary entry already exists for the FIlepath
%% it doesn't put it into the dictionary again, but it does calculate the bounding box for the image.
%% When the number of color components is less than or equal to 2, the Procset has a tuple value
%% of {A,B} where A can be undefined or imageb and B can be undefined or imagec. These cause the 
%% listing of these procedure set in the PDf so that the related procedure set can be loaded in 
%% the Postscript printing device. This is suppoed to be obsolete as of v. 1.4 PDFs. 
handle_image(ImageDict, FilePath, Size, ProcSet) ->
    case dict:find(FilePath, ImageDict) of
        {ok, #image{alias = Alias, width = W, height = H}} ->
            {ImageDict, Alias, set_size(Size, {W, H}), ProcSet};
        error ->
            Alias = "Im" ++ eg_pdf_op:i2s(dict:size(ImageDict) + 1),
            case eg_pdf_image:get_head_info(FilePath) of
                {jpeg_head, {W1, H1, Ncomponents, _Data_precision}} ->
                    NewDict = dict:store(FilePath,
                                         #image{alias  = Alias,
                                                width  = W1,
                                                height = H1},
                                         ImageDict),
                    {NewDict, Alias, set_size(Size, {W1, H1}),
                     imageBC(Ncomponents, ProcSet)};
                {png_head, {W1, H1, Ncomponents, _Data_precision}} ->
                    NewDict = dict:store(FilePath,
                                         #image{alias  = Alias,
                                                width  = W1,
                                                height = H1},
                                         ImageDict),
                    {NewDict, Alias, set_size(Size, {W1, H1}),
                     imageBC(Ncomponents, ProcSet)};

                A ->
                    {error_not_yet_implemented_image_format, A}
            end
    end.

%% @doc Function to scale the image properly if only width or height is set.
set_size({max, W1, H1}, {W2, H2}) ->
    H3 = trunc(W1 * H2 / W2),
    W3 = trunc(H1 * W2 / H2),
    if H3 > H1 ->
        {W3, H1};
        true ->
            {W1, H3}
    end;
set_size({undefined, undefined}, Size2) -> Size2;
set_size({W1, undefined}, {W2, H2})     -> {W1, trunc(W1 * H2 / W2)};
set_size({undefined, H1}, {W2, H2})     -> {trunc(H1 * W2 / H2), H1};
set_size(Size1, _)                      -> Size1.


%% @doc Set the image types for ProcSet. If we have black/white image we set
%% imageb; color then imagec. Both can be set.
imageBC(Ncomp, {_B, C}) when Ncomp =< 2 -> {imageb, C};
imageBC(Ncomp, {B, _C}) when Ncomp > 2  -> {B, imagec}.


pdf_handle_info(I, {author, Author}) ->
    I#info{author = Author};
pdf_handle_info(I, {title, Title}) ->
    I#info{title = Title};
pdf_handle_info(I, {subject, Subject}) ->
    I#info{subject = Subject};
pdf_handle_info(I, {date, {Year, Month, Day}}) ->
    I#info{creationDate = {Year, Month, Day}};
pdf_handle_info(I, {keywords, Keywords}) ->
    I#info{keywords = Keywords}.


%% @private
-spec convert(module() | undefined, string()) -> string().
convert(undefined, S) ->
    eg_convert:mac2pdf(S);

convert(Mod, S) ->
    case Mod:encoding() of
        "FontSpecific" -> S;
        _ -> eg_convert:pdf2mac(S)
    end.
