%%% *********************************************************
%%% ep_box.erl
%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%% License:    MIT
%%% File:       ep_box.erl
%%% Description:
%%%
%%% Boxes "contain" content elements--- text, images, and
%%% PDF elements.
%%% 
%%% Boxes are composed on "page grids" to organize content and
%%% direct "eye path." 
%%% 
%%% All box functions assume that the top left coordinate 
%%% is at X, Y and Y coordinates increment downward.
%%% This conceit makes it easier to layout page grids, since
%%% we read pages top down rather than bottom up.
%%%
%%% PDF, on the other hand, assumes that X, Y is at the bottom
%%% of the box and Y increments upward. Thus, when rendered by 
%%% PDF, Y coordinate must be flipped; e.g. X, Y at the bottom 
%%% left of the box and Y incrementing upward. 
%%%
%%% Y coordinates can be flipped with the function 
%%% ep_lib:v_flip/2 which takes the vertical dimension of 
%%% the paper stock on which the box will be printed as a
%%% parameter; e.g. when flipped, the top of the box will
%%% be relative to the top of the paper stock. 
%%%
%%% {X, Y} = position of box where
%%% X = upper left X coordinate of box in points  
%%% Y = upper left Y coordinate of box in points
%%% Measure | Width = width of box in points
%%% Height = height of box in pnts 
%%% @end
%%% *********************************************************   

-module (ep_box).

-export([
    available/1, available_lines/2, max_lines/2, n_lines/2,
    background/1,
    background_color/1, if_background/1,
    bg_flag/1, set_bg_flag/1, reset_bg_flag/1,
    border/1, border_type/1, border_color/1,
    box_spec/1,
    clip/3,
    continue/1,
    corners/1,
    create/4,
    default_gutter/0, default_leading/0,
    dimensions/1,
    end_x/1, end_y/1,
    fill_color/1,
    filled/1,
    gutter/1, leading/1,
    here/1,
    hide_border/1,
    id/1,
    if_border/1,
    indent/1,
    inner_box/1,
    outer_box/1,
    position/1,
    shift/3,
    show_border/1,
    stroke/1,
    stroke_color/1,
    text_box/1,
    text_color/1,
    text_margin/1,
    total_width/1, total_height/1,
    update_available/2,
    update_background_color/2, update_stroke/2, update_stroke_color/2,
    update_border/2,
    update_border_color/2,
    update_border_type/2,
    update_continue/2,
    update_fill_color/2,
    update_filled/2,
    update_gutter/2,
    update_id/2,
    update_indent/2,
    update_leading/2,
    update_margin/2,
    update_text_color/2,
    update_text_margin/2,
    update_width/2, update_height/2,
    update_x/2, update_y/2,
    update_y_ptr/2,
    v_flip_box/2,
    width/1, measure/1, height/1,
    will_fit/3,
    x/1, y/1,
    y_ptr/1
]). % a..z

-include("ep.hrl").
-include("ep_erltypes.hrl").

-define(GUTTER, 10).
-define(LEADING, 10).

-type ep_box() :: #{
    id               => string(),
    x                => points(),
    y                => points(),
    width            => points(),
    height           => points(),
    gutter           => points(),
    leading          => points(),
    filled           => integer(),
    y_ptr            => points(),
    available        => points(),
    bg_flag          => boolean(),
    border           => points(),
    border_type      => atom(), % solid | dashed | beveled...
    border_color     => color(),
    text_margin      => points(),
    text_color       => color(),
    background_color => color(),
    stroke           => stroke(),
    stroke_color     => color(),
    fill_color       => color(),
    indent           => points(),
    continue         => _
}.

%% ***********************************************************
%% Create box 
%% ***********************************************************

%% @doc Create a new box; all parameters in points

-spec create(X :: points(), Y :: points(), Width :: points(),
      Height :: points()) -> ep_box().

create(X, Y, Width, Height) ->
   #{id               => undefined,
     x                => X,
     y                => Y,
     width            => Width,
     height           => Height,
     gutter           => ?GUTTER,
     leading          => ?LEADING,
     filled           => 0,
     y_ptr            => Y,
     available        => Height,
     bg_flag          => false,
     border           => 1,
     border_type      => solid,
     border_color     => black,
     text_margin      => 10,
     text_color       => black,
     background_color => white,
     stroke           => fill_stroke,
     stroke_color     => white,
     fill_color       => white,
     indent           => 0,
     continue         => undefined
   }.


%% ***********************************************************
%% Fetch box parameters 
%% ***********************************************************

%% @doc Return id of box
-spec id(Box :: ep_box()) -> string().
id(#{id := Id}) -> Id.


%% @doc Return X coordinate of upper left corner of box
-spec x(Box :: ep_box()) -> points().
x(#{x := X}) -> X.


%% @doc Return Y coordinate of upper left corner of box
-spec y(Box :: ep_box()) -> points().
y(#{y := Y}) -> Y.


%% @doc Return width of box
-spec width(Box :: ep_box()) -> points().
width(#{width := W}) -> W.


%% @doc Type setter speak; alias of width;
-spec measure(Box :: ep_box()) -> points().
measure(#{width := W}) -> W.


%% @doc Return height of box
-spec height(Box :: ep_box()) -> points().
height(#{height := H}) -> H.


%% @doc Return width + gutter in points
-spec total_width(Box :: ep_box()) -> points().
total_width(#{width  := Width,
              gutter := Gutter}) ->
   Width + Gutter.


%% @doc Return height + leading in points
-spec total_height(Box :: ep_box()) -> points().
total_height(#{height  := Height,
               leading := Leading}) ->
   Height + Leading.


%% @doc Return right edge of box
-spec end_x(Box :: ep_box()) -> points().
end_x(#{x     := X,
        width := Width}) ->
    X + Width.


%% @doc Return bottom edge of box
-spec end_y(Box :: ep_box()) -> points().
end_y(#{y      := Y,
        height := Height}) ->
    Y + Height.


%% @doc Return gutter width of box; e.g. margin outside right edge
-spec gutter(Box :: ep_box()) -> points().
gutter(#{gutter := G}) -> G.

%% @doc Return leading under box; e.g. margin below bottom edge
-spec leading(Box :: ep_box()) -> points().
leading(#{leading := L}) -> L.


%% @doc Return vertical space at top of box occupied by content
-spec filled(Box :: ep_box()) -> _.
filled(#{filled := F}) -> F.


%% @doc Return default gutter width of box; e.g. margin outside right edge
-spec default_gutter() -> points().
default_gutter() -> ?GUTTER.


%% @doc Return default leading under box; e.g. margin below bottom edge
-spec default_leading() -> points().
default_leading() -> ?LEADING.


%% @doc Return y position for new content 
-spec y_ptr(Box :: ep_box()) -> _.
y_ptr(#{y_ptr := YPtr}) -> YPtr.


%% @doc Return space available for content 
-spec available(Box :: ep_box()) -> _.
available(#{available := Av}) -> Av.


-spec available_lines(ep_box(), points()) -> points().
available_lines(Box, Leading) ->
    Available = available(Box),
    Available div Leading.


%% @doc max_2 and n_lines/2 get leading from TypeSpec
%% See: ep_typespec:new/3 
max_lines(Box, TypeSpec) ->
     Height  = maps:get(height, Box),
     Leading = maps:get(leading, TypeSpec),
     Height div Leading.


%% @doc max_2 and n_lines/2 get leading from TypeSpec
%% See: ep_typespec:new/3
n_lines(Box, TypeSpec) ->
     CurY    = maps:get(cur_y, Box),
     EndY    = maps:get(end_y, Box),
     Border  = maps:get(border, Box),
     Margin  = 2 * Border,
     Leading = maps:get(leading, TypeSpec),
     (CurY - EndY - Margin) div Leading.


-spec update_available(ep_box(), points()) -> ep_box().
update_available(Box, SpaceConsumed) ->
    % Assumes Y and YPtr are inverted
    YPtr = ep_box:y_ptr(Box),
    YPtr1 = YPtr - SpaceConsumed,
    Box1 = ep_box:update_y_ptr(YPtr1, Box),
    Available = ep_box:available(Box1),
    SpaceRemaining = Available - SpaceConsumed,
    maps:put(available, SpaceRemaining, Box1).
 

%% @doc Given leading, returns true if lines will fit in box 
-spec will_fit(Box :: ep_box(), Lines :: integer(),
               Leading :: points()) -> boolean().
will_fit(Box, Lines, Leading) ->
    AvailableLines = available_lines(Box, Leading),
    AvailableLines =< Lines.


%% @doc Return state of background flag; if true, print background
-spec bg_flag(Box :: ep_box()) -> boolean().
bg_flag(#{bg_flag := BgFlag}) -> BgFlag.


-spec set_bg_flag(Box :: ep_box()) -> ep_box().
set_bg_flag(Box) ->
    maps:put(bg_flag, true, Box).


-spec reset_bg_flag(Box :: ep_box()) -> ep_box().
reset_bg_flag(Box) ->
    maps:put(bg_flag, false, Box).


%% @doc Alias for bg_blg/1
if_background(Box) ->
    bg_flag(Box).


%% @doc Return width of border
-spec border(Box :: ep_box()) -> integer().
border(#{border := B}) -> B.


%% @doc Return border type; solid, dashed, beveled
-spec border_type(Box :: ep_box()) -> atom().
border_type(#{border_type := BT}) -> BT.


%% @doc Return border color; white, silver, gray, black, maroon, 
%% red, fuchsia, purple lime, green, olive, yellow, navy, blue, teal, aqua
-spec border_color(Box :: ep_box()) -> atom().
border_color(#{border_color := BC}) -> BC.


%% @doc Return order 
-spec text_margin(Box :: ep_box()) -> integer().
text_margin(#{text_margin := TM}) -> TM.


%% @doc Return text color; white, silver, gray, black, maroon,
%% red, fuchsia, purple lime, green, olive, yellow, navy, blue, teal, aqua
-spec text_color(Box :: ep_box()) -> atom().
text_color(#{text_color := TC}) -> TC.


%% @doc Return background color; white, silver, gray, black, maroon, 
%% red, fuchsia, purple lime, green, olive, yellow, navy, blue, teal, aqua
-spec background_color(Box :: ep_box()) -> atom().
background_color(#{background_color := BC}) -> BC.


%% @doc return stroke type; close, stroke, close_stroke, fill, 
%% fill_even_odd, fill_stroke, fill_then_stroke, fill_stroke_even_odd,
%% close_fill_stroke, close_fill_stroke_even_odd, endpath
%% SEE eg_pdf_op:path/1
-spec stroke(Box :: ep_box()) -> stroke().
stroke(#{stroke := S}) -> S.


%% @doc Return stroke color; white, silver, gray, black, maroon,
%% red, fuchsia, purple lime, green, olive, yellow, navy, blue, teal, aqua
-spec stroke_color(Box :: ep_box()) -> color().
stroke_color(#{stroke_color := SC}) -> SC.


%% @doc Return fill color; white, silver, gray, black, maroon, 
%% red, fuchsia, purple lime, green, olive, yellow, navy, blue, teal, aqua
-spec fill_color(Box :: ep_box()) -> color().
fill_color(#{fill_color := FC}) -> FC.


%% @doc Return text indent
-spec indent(Box :: ep_box()) -> integer().
indent(#{indent := I}) -> I.


%% @doc Return text overflow rule 
continue(#{continue := C}) -> C.


%% ***********************************************************
%% Update box parameters 
%% ***********************************************************

%% @doc Update id
-spec update_id(ID :: string(), Box :: ep_box()) -> ep_box().
update_id(ID, Box) ->
    maps:put(id, ID, Box).


%% @doc Update horizontal position; e.g. X (points)
-spec update_x(X :: points(), Box :: ep_box()) -> ep_box().
update_x(X, Box) ->
    maps:put(x, X, Box).


%% @doc Update vertical position; e.g. Y (points)
-spec update_y(Y :: points(), Box :: ep_box()) -> ep_box().
update_y(Y, Box) ->
    maps:put(y, Y, Box).


%% @doc Update width (points)
-spec update_width(Width :: points(), Box :: ep_box()) -> ep_box().
update_width(Width, Box) ->
    maps:put(width, Width, Box).


%% @doc Update height (points)
-spec update_height(Height :: points(), Box :: ep_box()) -> ep_box().
update_height(Height, Box) ->
    maps:put(height, Height, Box).


-spec update_margin(Margin :: points(), Box :: ep_box()) -> ep_box().
update_margin(Margin, Box) ->
    maps:put(margin, Margin, Box).


%% @doc Update gutter; e.g. outside right margin (pints)
-spec update_gutter(Gutter :: points(), Box :: ep_box()) -> ep_box().
update_gutter(Gutter, Box) ->
    maps:put(gutter, Gutter, Box).


%% @doc Update leading; e.g.  margin below bottom edge (pints)
-spec update_leading(Leading :: points(), Box :: ep_box()) -> ep_box().
update_leading(Leading, Box) ->
    maps:put(leading, Leading, Box).


%% @doc Update filled
-spec update_filled(Filled :: integer(), Box :: ep_box()) -> ep_box().
update_filled(Points, Box) ->
    maps:put(filled, Points, Box).


%% @doc Update pointer to y position of next content 
-spec update_y_ptr(YPtr :: points(), Box :: ep_box()) -> ep_box().
update_y_ptr(YPtr, Box) ->
    maps:put(y_ptr, YPtr, Box).


%% @doc Update width of border (points)
-spec update_border(Border :: points(), Box :: ep_box()) -> ep_box().
update_border(Border, Box)
    when Border >= 0, Border < 5 ->
    maps:put(border, Border, Box).


%% @doc Update border type:
-spec update_border_type(BorderType :: atom(), Box :: ep_box()) -> ep_box().
update_border_type(BorderType, Box) ->
    maps:put(border_type, BorderType, Box).


%% @doc Update border color; white, silver, gray, black, maroon, 
%% red, fuchsia, purple lime, green, olive, yellow, navy, blue, teal, aqua
-spec update_border_color(Color :: color(), Box :: ep_box()) -> ep_box().
update_border_color(Color, Box) ->
    maps:put(border_color, Color, Box).


%% @doc Update text margine; e.g.space between border and text (points) 
-spec update_text_margin(Points :: points(), Box :: ep_box()) -> ep_box().
update_text_margin(Points, Box) ->
    maps:put(text_margin, Points, Box).


%% @doc Update text color; white, silver, gray, black, maroon, 
%% red, fuchsia, purple lime, green, olive, yellow, navy, blue, teal, aqua
-spec update_text_color(color(), Box :: ep_box()) -> ep_box().
update_text_color(Color, Box) ->
    maps:put(text_color, Color, Box).


%% @doc Update background color; white, silver, gray, black, maroon, 
%% red, fuchsia, purple lime, green, olive, yellow, navy, blue, teal, aqua
-spec update_background_color(color(), Box :: ep_box()) -> ep_box().
update_background_color(Color, Box) ->
    maps:put(background_color, Color, Box).


%% @doc Update stroke type; close, stroke, close_stroke, fill, 
%% fill_even_odd, fill_stroke, fill_then_stroke, fill_stroke_even_odd,
%% close_fill_stroke, close_fill_stroke_even_odd, endpath
%% SEE eg_pdf_op:path/1
-spec update_stroke(stroke(), Box :: ep_box()) -> ep_box().
update_stroke(Stroke, Box) ->
    maps:put(stroke, Stroke, Box).


%% @doc Update stroke color; white, silver, gray, black, maroon, 
%% red, fuchsia, purple lime, green, olive, yellow, navy, blue, teal, aqua
-spec update_stroke_color(color(), Box :: ep_box()) -> ep_box().
update_stroke_color(Color, Box) ->
    maps:put(stroke_color, Color, Box).


%% @doc Update fill color; white, silver, gray, black, maroon, 
%% red, fuchsia, purple lime, green, olive, yellow, navy, blue, teal, aqua
-spec update_fill_color(color(), Box :: ep_box()) -> ep_box().
update_fill_color(Color, Box) ->
    maps:put(fill_color, Color, Box).


%% @doc Update text indent (points)
-spec update_indent(Indent :: points(), Box :: ep_box()) -> ep_box().
update_indent(Indent, Box) ->
    maps:put(indent, Indent, Box).


%% @doc Update text overflow rule 
update_continue(Continue, Box) ->
    maps:put(continue, Continue, Box).


%% ***********************************************************
%% Box functionss 
%% ***********************************************************

%% @doc Return upper-left xy coordinates of box 
-spec position(Box :: ep_box()) -> xy().
position(#{x := X,
           y := Y}) ->
    {X, Y}.


%% @doc Return width and height of box 
-spec dimensions(Box :: ep_box()) -> xy().
dimensions(#{width := Width,
             height := Height}) ->
    {Width, Height}.


%% @doc Return position and dimensions of box 
-spec box_spec(Box :: ep_box()) -> xywh().
box_spec(Box) ->
    {X, Y} = position(Box),
    {Width, Height} = dimensions(Box),
    {X, Y, Width, Height}.


%% @doc Return position and dimensions of box 
-spec here(Box :: ep_box()) -> xy().
here(#{x := X,
       y_ptr := YPtr}) ->
    {X, YPtr}.


%% ***********************************************************
%% Shift box position
%% ***********************************************************

%% @doc Shift box up, down, right, or left 
-spec shift(Box :: ep_box(), direction(), N :: integer()) -> ep_box().
shift(Box, up, N) ->
    shift_y(Box, -N);

shift(Box, down, N) ->
    shift_y(Box, N);

shift(Box, right, N) ->
    shift_x(Box, N);

shift(Box, left, N) ->
    shift_x(Box, -N).


%% @doc shift/3 helpers
%% @private
shift_x(Box, Points) ->
    X      = x(Box),
    update_x(Box, X + Points).


%% @private
shift_y(Box, Points) ->
    Y      = y(Box),
    update_y(Box, Y + Points).


%% @doc Clip box top, right, bottom, or left
-spec clip(Box :: ep_box(), edge(), N :: integer()) -> ep_box().
clip(Box, top, Points) ->
    Y         = y(Box),
    NewY      = Y - Points,
    Height    = height(Box),
    NewHeight = Height - Points,
    Box1    = update_y(Box, NewY),
    update_height(Box1, NewHeight);

clip(Box, right, Points) ->
    Width     = width(Box),
    NewWidth  = Width - Points,
    update_width(Box, NewWidth);

 clip(Box, bottom, Points) ->
   Height    = height(Box),
   NewHeight = Height - Points,
   update_height(Box, NewHeight);

clip(Box, left, Points) ->
    X      = x(Box),
    Width  = width(Box),
    NewX   = X + Points,
    NewWidth = Width - Points,
    Box1 = update_x(Box, NewX),
    update_width(Box1, NewWidth).

%% ***********************************************************
%% Border functionss 
%% ***********************************************************

%% @doc Return true if box configured to show border and background
-spec if_border(Box :: ep_box()) -> boolean().
if_border(#{bg_flag := BF}) -> BF.


%% @doc Set border flag 
-spec show_border(Box :: ep_box()) -> ep_box().
show_border(Box) ->
    maps:put(bg_flag, true, Box).


%% @doc Clear border flag
-spec hide_border(Box :: ep_box()) -> ep_box().
hide_border(Box) ->
    maps:put(bg_flag, false, Box).


%% @doc return text box dimensions 
-spec text_box(ep_box()) -> xywh().
text_box(Box) ->
    BgFlag  = if_border(Box),
    case BgFlag of
       true  -> inner_box(Box);
       false -> outer_box(Box)
    end.


%% @doc Return position and dimensions ofBox
-spec outer_box(ep_box()) -> xywh().
outer_box(Box) ->
    box_spec(Box).
%    dimensions(Box).


%% @doc Return position and dimensions of text area in panel 
-spec inner_box(ep_box()) -> xywh().
inner_box(Box) ->
    {X, Y, Width, Height} = box_spec(Box),
    Border                = border(Box),
    TextMargin            = text_margin(Box),
    Margin = Border + TextMargin,
    X1 = X + Margin,
    Y1 = Y + Margin,
    W1 = Width - (2 * Margin),
    H1 = Height - (2 * Margin),
    {X1, Y1, W1, H1}.


%% ***********************************************************
%% Print support functionss 
%% ***********************************************************

%% @doc Return panel background parameters 
-spec background(Box :: ep_box()) -> {points(), color(), color(), color()}.
background(Box) ->
    Border      = border(Box),
    BorderColor = border_color(Box),
    BGColor     = background_color(Box),
    TextColor   = text_color(Box),
    {Border, BorderColor, BGColor, TextColor}.


%% @doc return corner coordiates of box
-spec corners(Box :: ep_box()) -> {xy(), xy(), xy(), xy()}.
corners(Box) ->
    {X, Y, Width, Height} = box_spec(Box),
    NW = {X, Y},
    NE = {X + Width, Y},
    SE = {X + Width, Y + Height},
    SW = {X, Y + Height},
    {NW, NE, SE, SW}.


%% ***********************************************************
%% Print boxes 
%% ***********************************************************

-spec v_flip_box(ep_box(), paper_stock()) -> ep_box().
v_flip_box(Box, PaperStock) ->
   Height = height(Box),
   Y      = y(Box),
   Y1 = ep_lib:v_flip(Y, PaperStock),
   Y2 = Y1 - Height,
   update_y(Y2, Box).
