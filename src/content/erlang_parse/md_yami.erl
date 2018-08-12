%%==========================================================================
%%% md_yami.erl
%%%
%%% Copyright (C) 2018 Lloyd R. Prentice
%%%
%%% License: MIT
%%%
%%% Author: Lloyd R. Prentice <lloyd@writersglen.com>, 
%%%
%%% Work-in-progres toward Erlang implementation of CommonMark spec
%%% http://spec.commonmark.org/0.27/
%%% 
%%% Many suble edge cases, so can't guarantee I've caught them all.
%%% Thanks to Richard O'Keefe for insiration and tips.
%%% ==========================================================================


-module (md_yami).

% -export([parse/1]). 

-compile(export_all).

-define(COPY_PATH, "/home/lloyd/ep/src/copy/copy_samples/").
-define(SAMPLE1, "paragraph.md").
-define(SAMPLE2, "test.md").

%% TESTs
%%
%% Requires Dir: copy_samples

test1() ->
   Text = get_file(?SAMPLE1), 
   md_yami:parse(Text).

test2() ->
   Text = get_file(?SAMPLE2), 
   md_yami:parse(Text).


%% From Commonmark spec:  softbreak may be rendered in HTML 
%% either as a line ending or as a space. Here we chose line ending.

-define(soft_line_break, $\n ). 


%% ***************************************************************
%% parse/1  
%% ***************************************************************

parse(Text) ->
   Text1 = convert_line_endings(Text),
   Lines = lines(Text1),
   [ parse_line(Line) || Line <- Lines].
   

get_file(FileName) ->
   CopyPath = ?COPY_PATH ++ FileName,
   {ok, File} = file:read_file(CopyPath),
   binary_to_list(File).




%% ***************************************************************
%% parse/1 -- helpers  
%% ***************************************************************

%% Also see 2.1

parse_line(Line) ->
   Flag1 = is_blank_line(Line),
   Flag2 = is_thematic_break(Line),
   Flag3 = is_heading(Line),
   Flag4 = is_block_quote(Line),
   Flag5 = is_bullet_list_item(Line),
   Flag6 = is_ordered_list_item(Line),
   if
      Flag1 -> do_blank_line();
      Flag2 -> do_thematic_break(Line);
      Flag3 -> do_heading(Line);
      Flag4 -> do_block_quote(Line);
      Flag5 -> do_bullet_list_item(Line);
      Flag6 -> do_ordered_list_item(Line);
      true  -> do_paragraph(Line)
   end.

%% ***************************************************************
%% Indentation 
%% ***************************************************************

%% Indent 

%% > 3 -- may be indented code block
%% < 4 -- may be code fence
%%               closing code fence
%%               HTML start condition
%%               first line of paragraph
%%               block quote     

%% ***************************************************************
%% Markers
%% ***************************************************************

marker_char() ->
   [$*, $-, $_, $#, $`, $~, $<, $[, $>, $-, $+, $*].
 
is_marker_char(Char) ->
   lists:member(Char, marker_char()).

is_digit(Char) ->
  (Char =< $9) and (Char >= $0).
 
analyze(Line) ->
   String = string:strip(Line),
   Chars  = chars_to_newline(String), 
   case length(Chars > 0) of
      true  -> Fact1 = is_marker_char(hd(Chars)),
               Fact2 = is_digit(hd(Chars));
      false -> Fact1 = false,
               Fact2 = false
   end,
   FactList = [Fact1, Fact2],
   [Fact || Fact <- FactList, Fact == true].
      

%% ***************************************************************
%% ***************************************************************
%% ******************** Preliminaries ****************************  
%% ***************************************************************
%% ***************************************************************

%% ***************************************************************
%% 2.1 -- Characters and Lines
%%
%% For now we'll forego Unicode 
%% ***************************************************************

convert_line_endings(Text) ->
   % We'll use UNIX-centric line endings
   Text1 = re:replace(Text, [$\r,$\n], [$\n], [{return, list}, global]),
   re:replace(Text1, [$\r], [$\n], [{return, list}, global]).

lines(Text) ->
   % Parse text into a list of lines
   Text1 = convert_line_endings(Text),
   Text2 =string:strip(Text1, both, $\n),
   list_lines(Text2).

list_lines(Text) ->
    list_blocks(Text, [], []).

list_lines(Text, AccText, Blocks) ->
    Chars = string:left(Text, 2),
    case Chars == "\n\n" of
       true  -> Text1 = string:sub_string(Text, 3),
                AccText1 = lists:reverse(AccText),
                Blocks1  = [AccText1 | Blocks],
                AccText2 = [],
                list_lines(Text1, AccText2, Blocks1);
       false -> [Char | Text1] = Text,
                AccText1 = [Char | AccText],
                list_lines(Text1, AccText1, Blocks)
    end.

whitespace_chars() ->
    [$ , $\t, $\n, $\v, $\f, $\r].

is_whitespace_char(Char) ->
    lists:member(Char, whitespace_chars()).
 
is_non_whitespace_char(Char) ->
    not is_whitespace_char(Char).

ascii_punctuation_characters() ->
   [$!, $", $#, $$, $%, $&, $', $(, $), $*, $+, $,, $-, 
    $., $/, $:, $;, $<, $=, $>, $?, $@, $[, $\\, $], $^,
    $_, $`, ${, $|, $}, $~].


indent(Line) ->
    List = lists:takewhile(fun(C) -> C == $  end, Line),
    length(List).

strip_indent(Text) ->
   string:strip(Text).


strip_spaces(Line) ->
  [Char || Char <- Line, Char /= $ ].

 
chars_to_newline(Line) -> 
    List = lists:takewhile(fun(C) -> C /= $\n end, Line),
    length(List).


%% ***************************************************************
%% 2.2 -- Tabs 
%% ***************************************************************

is_tab(Char) ->
  Char == $\t.


%% ***************************************************************
%% 2.3 -- Insecure Characters
%% ***************************************************************

%% Future

%% ***************************************************************
%% ***************************************************************
%% ******************* Blocks and Inlines ************************  
%% ***************************************************************
%% ***************************************************************

%% ***************************************************************
%% 3.1 -- Precedence  
%% ***************************************************************

%% ***************************************************************
%% 3.2 -- Container Blocks and Leaf Blocks 
%%
%% We can divide blocks into two types: container blocks, which 
%% can contain other blocks, and leaf blocks, which cannot.
%% ***************************************************************

%% ***************************************************************
%% ***************************************************************
%% ******************* Leaf Blocks *******************************  
%% ***************************************************************
%% ***************************************************************

%% ***************************************************************
%% 4.1 -- Thematic Breaks  
%% ***************************************************************

is_thematic_break(Text) ->
   tb_rules(Text).

do_thematic_break(Line) ->
   Flag = is_thematic_break(Line),
   case Flag of
      true  -> {hr, []};
      false -> punt
   end.

tb_rules(Line) ->
    Flag1 = tb_rule1(Line),
    Flag2 = tb_rule2(Line),
    Flag3 = tb_rule3(Line),
    Flag4 = tb_rule4(Line),  
    Flag5 = tb_rule5(Line),
    Flag6 = tb_rule6(Line),
    Flag1 or (Flag2 and Flag3 and Flag4 and Flag5 and Flag6). 

    
tbchars() ->
  [$*, $-, $_, $ ].


tb_rule1(Line) ->
   %% sequence of three or more matching -, _, or * characters
   Line1 = strip_spaces(Line),
   Tag   = string:left(Line1, 3),
   lists:member(Tag, ["***", "---", "___"]).

tb_rule2(Line) ->
   % Thematic break can only contain members of TbChars
   TbChars = tbchars(),
   List = [Char1 || Char1 <- Line, not lists:member(Char1, TbChars)],
   length(List) == 0.

tb_rule3(Line) ->
    % Thematic break must be prefixed with less than four spaces
    string:span(Line, " ") < 4.

tb_rule4(Line) ->
    % Thematic break can only contain members of TbChars
    TbChars = tbchars(),
    List = [Char1 || Char1 <- Line, not lists:member(Char1, TbChars)],
    length(List) == 0.

tb_rule5(Line) ->
    % All non-whitespace characters must be the same
    Flag = length(Line),
    case Flag of
        0  -> false;
        _  -> Line1 = [Char || Char <- Line, not is_whitespace_char(Char)],
                 [Char1 |_Rest] = Line1,
                 List = [Char2 || Char2 <- Line1, Char1 /= Char2], 
                 length(List) == 0
    end.

tb_rule6(Line) -> 
    % List item takes precidence
    Chars = string:left(Line, 2),
    not lists:member(Chars, ["- "]). 


%% ***************************************************************
%% 4.2 -- ATX Headings
%% ***************************************************************

is_heading(Line) ->
    Flag = length(Line),
    case Flag of
       0  -> false;
       _  -> hd(Line) == $#
    end.

do_heading(Block) ->
   Index   = string:chr(Block, $#),
   String  = string:sub_string(Block, Index),
   Span    = string:span(String, [$#]),
   String1 = string:sub_string(String, Span + 1),
   String2 = string:strip(String1, both),
   Tag     = list_to_atom("h" ++ integer_to_list(Span)),
   { Tag, do_text(String2)}.

%% ***************************************************************
%% 4.3 -- Setext Headings
%% ***************************************************************

%% Future

%% ***************************************************************
%% 4.4 -- Indented Code Blocks 
%% ***************************************************************

is_indented_chunk(Block) ->
    Indent = indent(Block),
    Indent > 3.

get_indented_chunk(Chunks, Text) ->
   Index = string:chr(Text, $\n),
   case Index of
      0 -> Chunks1 = [Text | Chunks],
           Text1   = [];
      _ -> Chunk = string:left(Text, Index),
           Chunks1 = [Chunk | Chunks], 
           Text1   = string:sub_string(Text, Index + 1)
   end,
   {Chunks1, Text1}.
 
strip_chunks(Chunks) ->
   [string:sub_string(Chunk, 5) || Chunk  <- Chunks].

%% ***************************************************************
%% 4.5 -- Fenced Code Blocks  
%% ***************************************************************

is_fenced_code_block(Block) ->
   Index = string:span(Block, " "),
   Flag1 = Index < 4,
   Block1 = string:strip(Block),
   Chars = string:left(Block1, 3),
   Flag2 = lists:member(Chars, ["```", "~~~"]),
   Flag1 and Flag2.


%% ***************************************************************
%% 4.6 -- HTML Blocks    
%% ***************************************************************

%% Future

%% ***************************************************************
%% 4.7 -- Link Reference definitions    
%% ***************************************************************

%% Future

%% ***************************************************************
%% 4.8 -- Paragraphs    
%% ***************************************************************

do_paragraph(Line) ->
   do_text(Line).

%% ***************************************************************
%% 4.9 -- Blank Lines    
%% ***************************************************************

is_blank_line(Line) ->
   % Return true if blank line
   List = [Char || Char <- Line, Char /= $ , Char /= $\t], 
   length(List) == 0.

do_blank_line() ->
   {bl, []}.

%% ***************************************************************
%% ***************************************************************
%% ***************** Container Blocks ****************************  
%% ***************************************************************
%% ***************************************************************

%% ***************************************************************
%% 5.1 -- Block Quotes    
%% ***************************************************************


is_block_quote(Line) ->
   Span = string:span(Line, " "),
   Rule1 = Span < 4,
   String = string:strip(Line),
   Char  = string:left(String, 1),
   Rule2 = Char == ">",
   Rule1 and Rule2. 
           
do_block_quote(Text) ->
   {bq, do_text(Text)}.

do_nested_block_quote(Text) ->
   {nbq, do_text(Text)}.


%% ***************************************************************
%% 5.2 Unordered Lists    
%% ***************************************************************

is_bullet_list_item(Line) ->
    Indent = indent(Line),
    String  = string:sub_string(Line, Indent + 1),
    case Indent of
       0    -> Flag = false;
       1    -> Flag = lists:member(String, ["-", "+", "*"]);
       _    -> Chars = string:left(String, 2),
               Flag = lists:member(Chars, ["- ", "+ ", "* "])
    end,
    Flag.

do_bullet_list_item(Line) ->
    String = string:strip(Line),
    {li, String}.

%% ***************************************************************
%% 5.2 Ordered Lists    
%% ***************************************************************

is_ordered_list_item(Line) ->
    LengthDigits = length_leading_digits(Line),
    Flag = (LengthDigits > 0 ) and (LengthDigits < 11),
    case Flag of
       true  -> confirm_ordered_list_item(Line);
       false -> false
    end.
   

do_ordered_list_item(Line) ->
   String = string:strip(Line),
   {li, String}. 

%% ***************************************************************
%% Ordered list helpers   
%% ***************************************************************

length_leading_digits(Line) ->
    List = leading_digits(Line),
    length(List).

leading_digits(Line) ->
    String = string:strip(Line),
    lists:takewhile(fun(C) -> (C =< $9) and (C >= $0) end, String).

strip_digits(Line) ->
    Length = length_leading_digits(Line),
    string:sub_string(Line, Length + 1).

confirm_ordered_list_item(Line) ->
    String = strip_digits(Line),
    case length(String) > 1 of
        true  -> Char = hd(String),
                 lists:member(Char, [$., $)]);
        false -> false
    end.

%% ***************************************************************
%% ***************************************************************
%% ******************** Inlines **********************************  
%% ***************************************************************
%% ***************************************************************

%% ***************************************************************
%% 6.1 -- Backslash Escapes    
%% ***************************************************************

%% ***************************************************************
%% 6.2 -- Entity and Numeric Character References    
%% ***************************************************************

%% ***************************************************************
%% 6.3 -- Code Spans    
%% ***************************************************************

maybe_code_span(Text) ->
    hd(Text) == $`.
   
%% ***************************************************************
%% 6.4 -- Empahsis and Strong Emphasis    
%% ***************************************************************

maybe_emphasis(Text) ->
    Char = hd(Text),
    Flag1 = lists:member(Char, [$*, $_]),
    Chars = string:left(Text, 2),
    Flag2 = not lists:member(Chars, ["**", "__"]), 
    Flag1 and Flag2. 

maybe_strong(Text) ->
    Chars = string:left(Text, 2),
    lists:member(Chars, ["**", "__"]). 

%% ***************************************************************
%% 6.5 -- Links    
%% ***************************************************************

maybe_link(Text) ->
   hd(Text) == $[.

%% ***************************************************************
%% 6.6 -- Images    
%% ***************************************************************

maybe_image(Text) ->
   Chars = string:left(Text, 2),
   Chars == "![".

%% ***************************************************************
%% 6.7 -- Auto Links    
%% ***************************************************************

%% Future

%% ***************************************************************
%% 6.8 -- Raw HTML    
%% ***************************************************************

%% Future

%% ***************************************************************
%% 6.9 -- Hard Line Breaks    
%% ***************************************************************

may_be_hard_line_break(Text, TagList, AccText, Elements) ->
   Flag = is_hard_line_break(Text),
   case Flag of
      true  -> do_hard_line_break(Text, TagList, AccText, Elements);
      false -> Flag1 = hd(Text) == $ ,
               case Flag1 of
                  true  -> Text1 = string:sub_string(Text, 2),
                           AccText1 = [$ | AccText],
                           {Text1, TagList, AccText1, Elements};
                  false -> {Text, TagList, AccText, Elements}
               end
   end.

is_hard_line_break(Text) ->
   Spaces = string:span(Text, " "),
   Char   = string:sub_string(Text, Spaces + 1, Spaces + 1),
   Flag1 = Spaces > 1,
   Flag2 = Char == "\n",
   Flag1 and Flag2.


do_hard_line_break(Text, TagList, AccText, Elements) ->
   Index = string:chr(Text, $\n),
      if
         Index < 1 ->  AccText1 = [$ |AccText],
                       Text1 = string:sub_string(Text, 2), 
                       Elements1 = Elements;
         Index > 1 ->  Txt  = string:sub_string(Text, Index + 1),
                       Text1 = string:strip(Txt),
                       AccText1 = AccText,
                       Elements1 = [{br, []} | Elements]
      end,
      {Text1, TagList, AccText1, Elements1}.
         
%% ***************************************************************
%% 6.10 -- Soft Line Breaks    
%% ***************************************************************

is_soft_line_break(Text) ->
   [Char | _Rest] = Text,
   Char == $\n.

do_soft_line_break(Text, TagList, AccText, Elements) ->
   Text1    = string:sub_string(Text, 2),
   AccText1 = [?soft_line_break | AccText],
   {Text1, TagList, AccText1, Elements}.
   
   

%% ***************************************************************
%% 6.11 -- Textual Content    
%% ***************************************************************

do_text(Text) ->
    do_text(Text, [], [], []).

do_text([], TagList, AccText, Elements) ->
    {Tag, _TagList1} = pop_tag(TagList),
    AccText1 = lists:reverse(AccText),
    Element = {Tag, AccText1},
    Elements1 = [Element | Elements],
    lists:reverse(Elements1);

do_text(Text, TagList, AccText, Elements) ->
   [Char |_ ] = Text,
   case Char of
     $   -> 
            {Text1, TagList1, AccText1, Elements1} =
               may_be_hard_line_break(Text, TagList, AccText, Elements),
            do_text(Text1, TagList1, AccText1, Elements1);
     $\n -> 
            {Text1, TagList1, AccText1, Elements1} =
               do_soft_line_break(Text, TagList, AccText, Elements),
            do_text(Text1, TagList1, AccText1, Elements1);
     $*  ->
            {Text1, TagList1, AccText1, Elements1} =
               do_emphasis(Text, TagList, AccText, Elements),
            do_text(Text1, TagList1, AccText1, Elements1);
     $_  ->
            {Text1, TagList1, AccText1, Elements1} =
               do_emphasis(Text, TagList, AccText, Elements),
            do_text(Text1, TagList1, AccText1, Elements1);
     $`  ->
            {Text1, TagList1, AccText1, Elements1} =
               do_code(Text, TagList, AccText, Elements),
            do_text(Text1, TagList1, AccText1, Elements1);
     _   -> {Text1, AccText1} = do_char(Text, AccText),
            do_text(Text1, TagList, AccText1, Elements)
   end.


%% ***************************************************************
%% Prepend character to text accumulator    
%% ***************************************************************

do_char(Text, AccText) ->
    [Char | Text1] = Text,
    AccText1 = [Char|AccText],
    {Text1, AccText1}.

    
%% ***************************************************************
%% Empahsis and strong emphasis    
%% ***************************************************************

do_emphasis(Text) ->
   do_text(Text).

do_emphasis(Text, TagList, AccText, Elements) ->
    Chars = string:left(Text, 2),
    Flag1 = Chars == "**",
    Flag2 = Chars == "__",
    Flag3 = Flag1 or Flag2,
    case Flag3 of
       true  -> do_bold(Text, TagList, AccText, Elements); 
       false -> do_italic(Text, TagList, AccText, Elements) 
    end.

do_bold(Text, TagList, AccText, Elements) ->
    Text1     = string:sub_string(Text, 3),
    Tag       = b,
    do_element(Tag, Text1, TagList, AccText, Elements).
 
do_italic(Text, TagList, AccText, Elements) ->
    Text1     = string:sub_string(Text, 2),
    Tag       = i,
    do_element(Tag, Text1, TagList, AccText, Elements).
 


%% ***************************************************************
%% Tag Stack    
%% ***************************************************************

no_tags(TagList) ->
    length(TagList) == 0.

push_tag(Tag, TagList) ->
   [Tag | TagList].
     
pop_tag(TagList) ->
    Flag = no_tags(TagList),
    case Flag of
      true  -> {p, []};
      false -> [Tag|TagList1] = TagList,
               {Tag, TagList1}
    end.

top_tag(TagList)  ->
    Flag = no_tags(TagList),
    case Flag of
       true  -> p;
       false -> hd(TagList)
    end.

tags_equal(Tag, TagList) ->
    NoTags = no_tags(TagList),
    case NoTags of
       true  -> false;
       false -> lists:member(Tag, TagList) % Tag == hd(TagList)
    end.  

drop_tag(Tag, TagList) ->
    lists:delete(Tag, TagList).


%% ***************************************************************
%% Block Elements 
%% ***************************************************************

list_blocks(Text) ->
    list_blocks(Text, [], []).

list_blocks([], AccText, Blocks) ->
    AccText1 = lists:reverse(AccText),
    Blocks1 = [AccText1 | Blocks],
    lists:reverse(Blocks1);

list_blocks(Text, AccText, Blocks) ->
    Chars = string:left(Text, 2),
    case Chars == "\n\n" of
       true  -> Text1 = string:sub_string(Text, 3),
                AccText1 = lists:reverse(AccText),
                Blocks1  = [AccText1 | Blocks],
                AccText2 = [],
                list_blocks(Text1, AccText2, Blocks1);
       false -> [Char | Text1] = Text,
                AccText1 = [Char | AccText],
                list_blocks(Text1, AccText1, Blocks)
    end.
         

%% ***************************************************************
%% Elements    
%% ***************************************************************

close_element(Tag, TagList, AccText, Elements) ->
     {AccText1, Elements1} = resolve_element(Tag, AccText, Elements),
     TagList1              = drop_tag(Tag, TagList),
     {TagList1, AccText1, Elements1}.

open_element(Tag, TagList, AccText, Elements) ->
     Tag1                  = top_tag(TagList),
     TagList1              = push_tag(Tag, TagList),
     {AccText1, Elements1} = resolve_element(Tag1, AccText, Elements),
     {TagList1, AccText1, Elements1}.


resolve_element(Tag, AccText, Elements) ->
   Element   = new_element(Tag, AccText),
   Elements1 = push_element(Element, Elements),
   AccText1  = clear_acctext(),
   {AccText1, Elements1}.
   

new_element(Tag, AccText) ->
    AccText1  = lists:reverse(AccText),
    {Tag, AccText1}.

push_element(Element, Elements) ->
    [Element | Elements].


%% ***************************************************************
%% Lists    
%% ***************************************************************

%% ***************************************************************
%% Backslash escapes    
%% ***************************************************************

%% ***************************************************************
%% Code spans    
%% ***************************************************************

do_code(Text) ->
   do_text(Text).

do_code(Text, TagList, AccText, Elements) ->
    Text1     = string:sub_string(Text, 2),
    Tag       = code,
    do_element(Tag, Text1, TagList, AccText, Elements).
     

%% ***************************************************************
%% Links    
%% ***************************************************************

%% ***************************************************************
%% Images    
%% ***************************************************************

%% ***************************************************************
%% Autolinks    
%% ***************************************************************

%% ***************************************************************
%% Raw HTML    
%% ***************************************************************

%% ***************************************************************
%% Hard line breaks    
%% ***************************************************************

%% ***************************************************************
%% Soft line breaks    
%% ***************************************************************

%% ***************************************************************
%% Text    
%% ***************************************************************


%% ***************************************************************
%% do_text/4 helpers    
%% ***************************************************************

do_element(Tag, Text, TagList, AccText, Elements) ->
    TagsEqual = tags_equal(Tag, TagList),
    case TagsEqual of
       true  -> {TagList1, AccText1, Elements1} = close_element(Tag, TagList, AccText, Elements);
       false -> {TagList1, AccText1, Elements1} = open_element(Tag, TagList, AccText, Elements) 
    end,
    {Text, TagList1, AccText1, Elements1}.


%% ***************************************************************
%% AccText    
%% ***************************************************************

clear_acctext() ->
    [].
    









