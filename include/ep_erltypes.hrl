%%
%% PDF Type Definitions
%%

-type pdf_server_pid() :: pid().

%% X, Y, Width, Height
-type xywh() :: {integer(), integer(), integer(), integer()}.

-type rgb_color() :: {byte(), byte(), byte()}.
-type color() :: atom() | rgb_color().

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

-type paper_stock() :: a4 | avery_labels | avery_labels_5164
    | avery_labels_8168 | envelope_no10 | legal | letter | atom().

-type page_format() :: a0 | a1 | a2 | a3 | a4 | a5 | a6 | a7 | a8 | a9 | b0 |
    b1 | b2 | b3 | b4 | b5 | b6 | b7 | b8 | b9 | b10 | avery_5164 |
    avery_8168 | book1 | book4 | book3 | book4 | book5 | book6 | book7 |
    bookmark | business_card | envelope_no10 | invoice | ledger | legal |
    letter | postcard1 | postcard2 | postcard3 | report | tabloid.

-type leading() :: number().
-type direction() :: up | down | left | right.
-type edge() :: top | right | bottom | left.

%%
%% Path Drawing and Filling Types
%%

-type line_cap_t() :: flat_cap | round_cap | square_cap | integer().
-type line_join_t() :: miter_join | round_join | bevel_join | integer().
-type dash_t() :: solid | dash | dot | dashdot | string().

-type path_t() :: close | close_strike | fill | fill_even_odd
    | fill_stroke | fill_then_stroke | fill_stroke_even_odd | close_fill_stroke
    | close_fill_stroke_even_odd | endpath | stroke().

-type stroke() :: stroke.

%% A pair of integers or floats which defines a point or size.
-type xy() :: {number(), number()}.

%% A pair of integers which defines a point or size.
-type integer_xy() :: {integer(), integer()}.

%% same as integer_xy() just for clarity
-type points_xy() :: {points(), points()}.

%% A pair of points which defines a box or a line.
-type xy1_xy2() :: {xy(), xy()}.

%%
%%----------------------
%%

-type ep_job() :: #{
    title => iolist() | undefined,
    published => iolist() | undefined,
    path => file:filename(),
    directory => file:filename(),
    author => string() | undefined,
    subject => string() | undefined,
    description => string() | undefined,
    keywords => any(),
    start_date => any(),
    deadline => any(),
    paper_stock => paper_stock(),
    page_format => page_format()
}.

-type ep_panel_id() :: {PageNumber :: integer(),
                        PanelIndex :: integer(),
                        PanelName :: string() | atom()}.
-type ep_panel() :: #{
    id                => ep_panel_id(),
    position          => xy(),
    size              => xy(),
    radius            => number(),
    content_cursor    => number(),
    border            => number(),
    border_style      => atom(),
    border_color      => color(),
    background_color  => color(),
    margin            => points(),
    typestyle         => atom(),
    li_fill           => color(),
    indent            => points(),
    rot               => number(),
    jump_prompt       => string()
}.
