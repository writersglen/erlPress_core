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

-export([create/4, id/1]).
-export([x/1, y/1, width/1, measure/1, height/1]).
-export([total_width/1, total_height/1]).
-export([end_x/1, end_y/1]).
-export([gutter/1, leading/1]).
-export([filled/1]).
-export([default_gutter/0, default_leading/0]).
-export([y_ptr/1]).
-export([available/1, available_lines/2]).
-export([max_lines/2, n_lines/2]).
-export([will_fit/3]).
-export([bg_flag/1, set_bg_flag/1, reset_bg_flag/1]).
-export([if_background/1, border/1, border_type/1, border_color/1]).
-export([text_margin/1, text_color/1, background_color/1]).
-export([stroke/1, stroke_color/1, fill_color/1]).
-export([indent/1, continue/1]). 
-export([update_id/2]).
-export([update_x/2, update_y/2, update_width/2, update_height/2]).
-export([update_margin/2]).
-export([update_gutter/2, update_leading/2]). 
-export([update_filled/2]).
-export([update_y_ptr/2, update_available/2]).
-export([update_border/2, update_border_type/2, update_border_color/2]).
-export([update_text_margin/2, update_text_color/2, update_background_color/2]).
-export([update_stroke/2, update_stroke_color/2, update_fill_color/2]).
-export([update_indent/2, update_continue/2]).
-export([position/1, dimensions/1, box_spec/1]).
-export([here/1]).
-export([shift/3, clip/3]).
-export([outer_box/1, inner_box/1, text_box/1]).
-export([if_border/1, show_border/1, hide_border/1]).
-export([background/1, corners/1]).
-export([v_flip_box/2]).

-include("../../include/ep.hrl").

-define(GUTTER, 10).
-define(LEADING, 10).

%% ***********************************************************
%% Create box 
%% ***********************************************************

%% @doc Create a new box; all parameters in points

-spec create(X :: integer(), Y :: integer(), Width :: integer(),
      Height :: integer()) -> map().

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

-spec id(Box :: map()) -> string().

id(Box) ->
    maps:get(id, Box).

%% @doc Return X coordinate of upper left corner of box

-spec x(Box :: map()) -> integer().

x(Box) ->
    maps:get(x, Box).

%% @doc Return Y coordinate of upper left corner of box

-spec y(Box :: map()) -> integer().

y(Box) ->
    maps:get(y, Box).

%% @doc Return width of box

-spec width(Box :: map()) -> integer().

width(Box) ->
    maps:get(width, Box).

%% @doc Type setter speak; alias of width;  

-spec measure(Box :: map()) -> integer().

measure(Box) ->
    width(Box).

%% @doc Return height of box

-spec height(Box :: map()) -> integer().

height(Box) ->
    maps:get(height, Box).

%% @doc Return width + gutter in points

-spec total_width(Box :: map()) -> integer().

total_width(Box) ->
   Width = width(Box),
   Gutter = gutter(Box),
   Width + Gutter.

%% @doc Return height + leading in points

-spec total_height(Box :: map()) -> integer().

total_height(Box) ->
   Height = height(Box),
   Leading = leading(Box),
   Height + Leading.



%% @doc Return right edge of box

-spec end_x(Box :: map()) -> integer().

end_x(Box) ->
    X     = x(Box),
    Width = width(Box),
    X + Width.

%% @doc Return bottom edge of box

-spec end_y(Box :: map()) -> integer().

end_y(Box) ->
    Y      = y(Box),
    Height = height(Box),
    Y + Height.

%% @doc Return gutter width of box; e.g. 
%% margin outside right edge

-spec gutter(Box :: map()) -> integer().

gutter(Box) ->
    maps:get(gutter, Box).

%% @doc Return leading under box; e.g. 
%% margin below bottom edge

-spec leading(Box :: map()) -> integer().

leading(Box) ->
    maps:get(leading, Box).

%% @doc Return vertical space at top of box occupied by
%% content 

-spec filled(Box :: map()) -> integer().

filled(Box) ->
    maps:get(filled, Box).



%% @doc Returndefault  gutter width of box; e.g. 
%% margin outside right edge

-spec default_gutter() -> integer().

default_gutter() ->
   ?GUTTER. 

%% @doc Return default leading under box; e.g. 
%% margin below bottom edge

-spec default_leading() -> integer().

default_leading() ->
    ?LEADING.

%% @doc Return y position for new content 

-spec y_ptr(Box :: map()) -> integer().

y_ptr(Box) ->
    maps:get(y_ptr, Box).

%% @doc Return space available for content 

-spec available(Box :: map()) -> integer().

available(Box) ->
    maps:get(available, Box).

available_lines(Box, Leading) ->
    Available = available(Box),
    Available div Leading.

%% max_2 and n_lines/2 get leading from TypeSpec
%% See: ep_typespec:new/3 

max_lines(Box, TypeSpec) ->
     Height  = maps:get(height, Box),
     Leading = maps:get(leading, TypeSpec),
     Height div Leading.

n_lines(Box, TypeSpec) ->
     CurY    = maps:get(cur_y, Box),
     EndY    = maps:get(end_y, Box),
     Border  = maps:get(border, Box),
     Margin  = 2 * Border,
     Leading = maps:get(leading, TypeSpec),
     (CurY - EndY - Margin) div Leading.




update_available(Box, SpaceConsumed) ->
    % Assumes Y and YPtr are inverted
    YPtr = ep_box:y_ptr(Box),
    YPtr1 = YPtr - SpaceConsumed,
    Box1 = ep_box:update_y_ptr(YPtr1, Box),
    Available = ep_box:available(Box1),
    SpaceRemaining = Available - SpaceConsumed,
    maps:put(available, SpaceRemaining, Box1).
 

%% @doc Given leading, returns true if lines will fit in box 

-spec will_fit(Box :: tuple(), Lines :: integer(),
       Leading :: integer()) -> boolean().

will_fit(Box, Lines, Leading) ->
    AvailableLines = available_lines(Box, Leading),
    AvailableLines =< Lines.



%% @doc Return state of background flag; if tue, print 
%% background 

-spec bg_flag(Box :: map()) -> boolean().

bg_flag(Box) ->
    maps:get(bg_flag, Box).

-spec set_bg_flag(Box :: map()) -> map().

set_bg_flag(Box) ->
    maps:put(bg_flag, true, Box).

-spec reset_bg_flag(Box :: map()) -> map().

reset_bg_flag(Box) ->
    maps:put(bg_flag, false, Box).


%% @doc Alias for bg_blg/1

if_background(Box) ->
    bg_flag(Box).

%% @doc Return width of border 

-spec border(Box :: map()) -> integer().

border(Box) ->
    maps:get(border, Box).

%% @doc Return border type; soliid, dashed, beveled 

-spec border_type(Box :: map()) -> atom().

border_type(Box) ->
    maps:get(border_type, Box).

%% @doc Return border color; white, silver, gray, black, maroon, 
%% red, fuchsia, purple lime, green, olive, yellow, navy, blue, teal, aqua
 
-spec border_color(Box :: map()) -> atom().

border_color(Box) ->
    maps:get(border_color, Box).

%% @doc Return order 

-spec text_margin(Box :: map()) -> integer().

text_margin(Box) ->
    maps:get(text_margin, Box).

%% @doc Return text color; white, silver, gray, black, maroon, 
%% red, fuchsia, purple lime, green, olive, yellow, navy, blue, teal, aqua
 
-spec text_color(Box :: map()) -> atom().

text_color(Box) ->
    maps:get(text_color, Box).

%% @doc Return background color; white, silver, gray, black, maroon, 
%% red, fuchsia, purple lime, green, olive, yellow, navy, blue, teal, aqua
 
-spec background_color(Box :: map()) -> atom().

background_color(Box) ->
    maps:get(background_color, Box).




%% @doc return stroke type; close, stroke, close_stroke, fill, 
%% fill_even_odd, fill_stroke, fill_then_stroke, fill_stroke_even_odd,
%% close_fill_stroke, close_fill_stroke_even_odd, endpath
%% SEE eg_pdf_op:path/1
 
-spec stroke(Box :: map()) -> atom().

stroke(Box) ->
    maps:get(stroke, Box).

%% @doc Return stroke color; white, silver, gray, black, maroon, 
%% red, fuchsia, purple lime, green, olive, yellow, navy, blue, teal, aqua
 
-spec stroke_color(Box :: map()) -> atom().

stroke_color(Box) ->
    maps:get(stroke_color, Box).

%% @doc Return fill color; white, silver, gray, black, maroon, 
%% red, fuchsia, purple lime, green, olive, yellow, navy, blue, teal, aqua
 
-spec fill_color(Box :: map()) -> atom().

fill_color(Box) ->
    maps:get(fill_color, Box).

%% @doc Return text indent 

-spec indent(Box :: map()) -> integer().

indent(Box) ->
    maps:get(indent, Box).

%% @doc Return text overflow rule 

continue(Box) ->
    maps:get(continue, Box).


%% ***********************************************************
%% Update box parameters 
%% ***********************************************************

%% @doc Update id

-spec update_id(Box :: map(), ID :: string()) -> map().

update_id(ID, Box) ->
    maps:put(id, ID, Box).

%% @doc Update horizontal position; e.g. X (points)

-spec update_x(Box :: map(), X :: integer()) -> map().

update_x(X, Box) ->
    maps:put(x, X, Box).

%% @doc Update vertical position; e.g. Y (points)

-spec update_y(Box :: map(), Y :: integer()) -> map().

update_y(Y, Box) ->
    maps:put(y, Y, Box).

%% @doc Update width (points)

-spec update_width(Box :: map(), Width :: integer()) -> map().

update_width(Width, Box) ->
    maps:put(width, Width, Box).

%% @doc Update height (points)

-spec update_height(Box :: map(), Height :: integer()) -> map().

update_height(Height, Box) ->
    maps:put(height, Height, Box).

-spec update_margin(Box :: map(), Margin :: integer()) -> map().

update_margin(Margin, Box) ->
    maps:put(margin, Margin, Box).

%% @doc Update gutter; e.g. outside right margin (pints)

-spec update_gutter(Box :: map(), Height :: integer()) -> map().

update_gutter(Gutter, Box) ->
    maps:put(gutter, Gutter, Box).

%% @doc Update leading; e.g.  margin below bottom edge (pints)

-spec update_leading(Box :: map(), Height :: integer()) -> map().

update_leading(Leading, Box) ->
    maps:put(leading, Leading, Box).

%% @doc Update filled

-spec update_filled(Box :: map(), Filled :: integer()) -> map().

update_filled(Points, Box) ->
    maps:put(filled, Points, Box).



%% @doc Update pointer to y position of next content 

-spec update_y_ptr(YPtr :: integer(), Box :: map()) -> map().

update_y_ptr(YPtr, Box) ->
    maps:put(y_ptr, YPtr, Box).

%% @doc Update width of border (points)

-spec update_border(Box :: map(), Border :: integer()) -> map().

update_border(Border, Box) when Border >= 0, Border < 5 ->
    maps:put(border, Border, Box).

%% @doc Update border type:  

-spec update_border_type(Box :: map(), BorderType :: atom()) -> map().

update_border_type(BorderType, Box) ->
    maps:put(border_type, BorderType, Box).

%% @doc Update border color; white, silver, gray, black, maroon, 
%% red, fuchsia, purple lime, green, olive, yellow, navy, blue, teal, aqua
 
-spec update_border_color(Box :: map(), Color :: atom()) -> map().

update_border_color(Color, Box) ->
    maps:put(border_color, Color, Box).

%% @doc Update text margine; e.g.space between border and text (points) 

-spec update_text_margin(Box :: map(), Points :: integer()) -> map().

update_text_margin(Points, Box) ->
    maps:put(text_margin, Points, Box).

%% @doc Update text color; white, silver, gray, black, maroon, 
%% red, fuchsia, purple lime, green, olive, yellow, navy, blue, teal, aqua
 
-spec update_text_color(Box :: map(), Color :: atom()) -> map().

update_text_color(Color, Box) ->
    maps:put(text_color, Color, Box).

%% @doc Update background color; white, silver, gray, black, maroon, 
%% red, fuchsia, purple lime, green, olive, yellow, navy, blue, teal, aqua
 
-spec update_background_color(Box :: map(), Color :: atom()) -> map().

update_background_color(Color, Box) ->
    maps:put(background_color, Color, Box).

%% @doc Update stroke type; close, stroke, close_stroke, fill, 
%% fill_even_odd, fill_stroke, fill_then_stroke, fill_stroke_even_odd,
%% close_fill_stroke, close_fill_stroke_even_odd, endpath
%% SEE eg_pdf_op:path/1
 
-spec update_stroke(Box :: map(), Stroke :: atom()) -> map().

update_stroke(Stroke, Box) ->
    maps:put(stroke, Stroke, Box).

%% @doc Update stroke color; white, silver, gray, black, maroon, 
%% red, fuchsia, purple lime, green, olive, yellow, navy, blue, teal, aqua
 
-spec update_stroke_color(Box :: map(), Color :: atom()) -> map().

update_stroke_color(Color, Box) ->
    maps:put(stroke_color, Color, Box).

%% @doc Update fill color; white, silver, gray, black, maroon, 
%% red, fuchsia, purple lime, green, olive, yellow, navy, blue, teal, aqua
 
-spec update_fill_color(Box :: map(), Color :: atom()) -> map().

update_fill_color(Color, Box) ->
    maps:put(fill_color, Color, Box).

%% @doc Update text indent (points)

-spec update_indent(Box :: map(), Indent :: integer()) -> map().

update_indent(Indent, Box) ->
    maps:put(indent, Indent, Box).

%% @doc Update text overflow rule 

update_continue(Continue, Box) ->
    maps:put(continue, Continue, Box).

%% ***********************************************************
%% Box functionss 
%% ***********************************************************

%% @doc Return upper-left xy coordinates of box 

-spec position(Box :: map()) -> tuple().

position(Box) ->
    X = x(Box),
    Y = y(Box),
    {X, Y}.

%% @doc Return width and height of box 

-spec dimensions(Box :: map()) -> tuple().

dimensions(Box) ->
    Width = width(Box),
    Height = height(Box),
    {Width, Height}.

%% @doc Return position and dimensions of box 

-spec box_spec(Box :: map()) -> tuple().

box_spec(Box) ->
    {X, Y} = position(Box),
    {Width, Height} = dimensions(Box),
    {X, Y, Width, Height}.

%% @doc Return position and dimensions of box 

-spec here(Box :: map()) -> tuple().

here(Box) ->
    X = x(Box),
    YPtr = y_ptr(Box),
    {X, YPtr}.


%% ***********************************************************
%% Shift box position
%% ***********************************************************

%% @doc Shift box up, down, right, or left 

-spec shift(Box :: map(), Direction :: atom(), N :: integer()) -> map().

shift(Box, up, N) ->
    shift_y(Box, -N);

shift(Box, down, N) ->
    shift_y(Box, N);

shift(Box, right, N) ->
    shift_x(Box, N);

shift(Box, left, N) ->
    shift_x(Box, -N).

%% @doc shift/3 helpers

shift_x(Box, Points) ->
    X      = x(Box),
    update_x(Box, X + Points).

shift_y(Box, Points) ->
    Y      = y(Box),
    update_y(Box, Y + Points).

%% @doc Clip box top, right, bottom, or left

-spec clip(Box :: map(), Edge :: atom(), N :: integer()) -> map().

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

-spec if_border(Box :: map()) -> boolean().

if_border(Box) ->
    maps:get(bg_flag, Box).

%% @doc Set border flag 

-spec show_border(Box :: map()) -> map().

show_border(Box) ->
    maps:put(bg_flag, true, Box).

%% @doc Reset border flag 

-spec hide_border(Box :: map()) -> map().

hide_border(Box) ->
    maps:put(bg_flag, false, Box).

%% @doc return text box dimensions 

text_box(Box) ->
    BgFlag  = if_border(Box),
    case BgFlag of
       true  -> inner_box(Box);
       false -> outer_box(Box)
    end.

%% @doc Return position and dimensions ofBox 

outer_box(Box) ->
    box_spec(Box).
%    dimensions(Box).

%% @doc Return position and dimensions of text area in panel 

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

-spec background(Box :: map()) -> tuple().

background(Box) ->
    Border      = border(Box),
    BorderColor = border_color(Box),
    BGColor     = background_color(Box),
    TextColor   = text_color(Box),
    {Border, BorderColor, BGColor, TextColor}.

%% @doc return corner coordiates of box

-spec corners(Box :: map()) -> tuple().

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

v_flip_box(Box, PaperStock) ->
   Height = height(Box),
   Y      = y(Box),
   Y1 = ep_lib:v_flip(Y, PaperStock),
   Y2 = Y1 - Height,
   update_y(Y2, Box).












