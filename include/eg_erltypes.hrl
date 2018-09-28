%%
%% PDF Type Definitions
%%

-type pdf_server_pid() :: pid().
-type pagesize_t() :: {integer(), integer(), integer(), integer()}.

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
