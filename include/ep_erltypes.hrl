%%
%% PDF Type Definitions
%%

-type pdf_server_pid() :: pid().

%% X, Y, Width, Height
-type xywh() :: {integer(), integer(), integer(), integer()}.

-type rgb8_t() :: {byte(), byte(), byte()}.

-type img_size_t() :: {max, undefined | number(), undefined | number()}
    | {width | height, number()} | {size, tuple()}
    | {integer(), integer()}.

-type val_t() :: {name | string, string()} | dict_val_t() | ptr_val_t().
-type ptr_val_t() :: {ptr, number(), number()}.
-type dict_val_t() :: {dict, list({string(), val_t()})}.

%%
%% Rich Text Type Definitions
%%

-ifdef(FACE_RECORD_DEFINED).
    -type face() :: #face{}.
-else.
    -type face() :: tuple().
-endif.

-type richText() :: {richText, [any_inline()]}.
-type any_inline() :: word() | opaque() | space() | nl() | fixed_str().
-type word() :: {word, Width :: milli_points(), Face :: face(), string()}.
-type space() :: {space, Width :: milli_points(), Face :: face()}.
-type nl() :: {nl, Face :: face()}.
-type fixed_str() :: {fixedStr, Width :: milli_points(), _, _}.
-type opaque() :: {opaque, Width :: milli_points(), _}.
-type milli_points() :: integer().
-type points() :: integer().

%%
%% Text and Paragraph Layout Type Definitions
%%

-type line_split_t() :: justified | spill | left_justified | ragged
    | right_justified | ragged_left | ragged_force_split | simple_hyphenate
    | preformatted | centered.

-type paper_stock() :: a4 | avery_labels | envelope_no10 | legal | letter | atom().
-type pageformat() :: atom().
-type leading() :: number().

%%
%% Path Drawing and Filling Types
%%

-type line_cap_t() :: flat_cap | round_cap | square_cap | integer().
-type line_join_t() :: miter_join | round_join | bevel_join | integer().
-type dash_t() :: solid | dash | dot | dashdot | string().

-type path_t() :: close | stroke | close_strike | fill | fill_even_odd
    | fill_stroke | fill_then_stroke | fill_stroke_even_odd | close_fill_stroke
    | close_fill_stroke_even_odd | endpath.

%% A pair of coords which defines a point.
-type xy() :: {number(), number()}.

%% A pair of points which defines a box or a line.
-type xy1_xy2() :: {xy(), xy()}.
