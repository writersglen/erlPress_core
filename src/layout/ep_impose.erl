%%% ********************************************************* 
%%% ep_impose.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:
%%%   File:         ep_impose.erl
%%%   Description:   Functions to impose standard format pages 
%%%                  on paper stock.
%%%                  Placement coordinates are relative to paper stock
%%%                  top left corner; e.g. paper stock {0, 0}
 
%%% @end

%%% ==========================================================================


-module (ep_impose).

-export([
    formats/1,
    page_fits/2,
    page_size/1, page_width/1, page_height/1,
    pages_across/2, pages_down/2,
    place_page/2,
    place_pages/2, place_pages/3,
    select_stock/1,
    stock_size/1, stock_width/1, stock_height/1
]).

-include("ep_erltypes.hrl").

%% *********************************************
%% Page placement 
%% *********************************************

%% @doc Return list of coordinates on paper stock where page will fit
-spec place_pages(PaperStock :: paper_stock(), Format :: page_format()) -> list().
place_pages(PaperStock, Format) ->
    XList = x_placement(PaperStock, Format),
    YList = y_placement(PaperStock, Format),
    %% FIXME Taking head from a list generator can be replaced with just calling row on first element of YList
    hd([row(XList, Y)  ||  Y <- YList]). 


%% @doc Return list of coordinates for placement of
%% @end N pages on paper stock
-spec place_pages(PaperStock :: paper_stock(), Format :: page_format(),
                  N :: integer()) -> list().
place_pages(PaperStock, Format, N) ->
    Coordinates = place_pages(PaperStock, Format),
    {List1, _List2} = lists:split(N, Coordinates),
    List1.


%% @doc Return coordinates to center page on paper stock
-spec place_page(PaperStock :: paper_stock(), Format :: atom()) ->
    integer_xy().
place_page(PaperStock, Format) ->
   {StockWidth, StockHeight} = stock_size(PaperStock),
   {PageWidth, PageHeight}  = page_size(Format),
   XGutter = (StockWidth - PageWidth) / 2,
   YGutter = (StockHeight -PageHeight) / 2,
   {ep_utils:floor(XGutter), ep_utils:floor(YGutter)}.
 

%% *********************************************
%% Stock dimensions
%% *********************************************

%% @doc Returns stock width and height in points
-spec stock_size(PaperStock :: paper_stock()) -> points_xy().
stock_size(PaperStock) ->
   ep_paper_stock:stock_size_points(PaperStock).


%% @doc Returns width of stock in points
-spec stock_width(PaperStock :: paper_stock()) -> points().
stock_width(PaperStock) ->
   {Width, _Height} = stock_size(PaperStock),
   Width.


%% @doc Returns height of stock in points 
-spec stock_height(PaperStock :: paper_stock()) -> points().
stock_height(PaperStock) ->
   {_Width, Height} = stock_size(PaperStock),
   Height.


%% *********************************************
%% Page dimensions
%% *********************************************

%% @doc Return page width and height in points
-spec page_size(Format :: page_format()) -> points_xy().
page_size(Format) ->
   ep_format:pagesize_points(Format).


-spec page_width(page_format()) -> points().
page_width(Format) ->
   {Width, _Height} = page_size(Format),
   Width.


-spec page_height(page_format()) -> points().
page_height(Format) ->
   {_Width, Height} = page_size(Format),
   Height.


%% *********************************************
%% Select stock 
%% *********************************************

%% @doc Return list of standard paper stock sizes larger than selected format
-spec select_stock(Format :: page_format()) -> paper_stock() | list(paper_stock()).
select_stock(label)         -> avery_labels;
select_stock(avery_5164)    -> avery_labels_5164;
select_stock(avery_8168)    -> avery_labels_8168;
select_stock(envelope_no10) -> envelope_no10;
select_stock(postcard)      -> select_stock(letter);
select_stock(Format) ->
    [Stock || Stock <- ep_paper_stock:standards(),
     ep_impose:page_fits(Stock, Format),
     Stock /= avery_labels].


%% *********************************************
%% Page constraints 
%% *********************************************

%% @doc Return how many pages will fit across stock
-spec pages_across(PaperStock :: paper_stock(), Format :: page_format()) -> integer().
pages_across(PaperStock, Format) ->
   StockWidth = stock_width(PaperStock),
   PageWidth = page_width(Format),
   N = StockWidth / PageWidth,
   ep_utils:floor(N).


%% @doc Return how many pages will fit vertically
%% @end down stock

-spec pages_down(PaperStock :: atom(),
                 Format     :: atom()) -> integer().

pages_down(PaperStock, Format) ->
   StockHeight = stock_height(PaperStock),
   PageHeight = page_height(Format),
   N = StockHeight / PageHeight,
   ep_utils:floor(N).


%% @doc Returns true if paper stock is larger than
%% @end standard format page else false

-spec page_fits(PaperStock :: atom(), Format :: atom()) -> boolean().

page_fits(PaperStock, Format) ->
   PagesWide = pages_across(PaperStock, Format),
   PagesHigh = pages_down(PaperStock, Format),
   (PagesWide > 0) and (PagesHigh > 0).

%% @doc Given paper stock, return list of page formats that will fit on the stock
-spec formats(PaperStock :: paper_stock()) -> list().
formats(PaperStock) ->
    {StockWidth, StockHeight} = stock_size(PaperStock),
    FormatList = format_list(),
    [FormatName
     || {FormatName, {FormatWidth, FormatHeight}, _Comment} <- FormatList,
     StockWidth >= FormatWidth,
     StockHeight >= FormatHeight,
     FormatName /= envelope_no10
    ].


%% *********************************************
%% Page placement helpers
%% *********************************************

%% @private
-spec x_placement(paper_stock(), page_format()) -> list(number()).
x_placement(PaperStock, Format) ->
    StockWidth  = stock_width(PaperStock),
    PageWidth   = page_width(Format),
    PagesWide   = pages_across(PaperStock, Format),
    FreeX       = StockWidth  - (PageWidth * PagesWide),
    XGutter     = FreeX / (PagesWide + 1),
    TotalX      = XGutter + PageWidth,
    XList = place_x(XGutter, TotalX, PagesWide), 
    [ep_utils:floor(X) || X <- XList].


%% @private
-spec y_placement(paper_stock(), page_format()) -> list(number()).
y_placement(PaperStock, Format) ->
    StockHeight = stock_height(PaperStock),
    PageHeight  = page_height(Format),
    PagesHigh   = pages_down(PaperStock, Format),
    FreeY       = StockHeight - (PageHeight * PagesHigh),
    YGutter     = FreeY / (PagesHigh + 1),
    TotalY      = YGutter + PageHeight,
    YList = place_y(YGutter, TotalY, PagesHigh),
    [ep_utils:floor(Y) || Y <- YList].


%% @private
place_x(XGutter, TotalX, PagesWide) ->
    XList = [XGutter],
    PagesWide1 = PagesWide - 1,
    place_more_x(TotalX, PagesWide1, XList).


%% @private
place_more_x(_TotalX, 0, List) ->
    lists:reverse(List);

place_more_x(TotalX, PagesWide, XList) ->
    NextX = hd(XList) + TotalX,
    XList1 = [NextX | XList],
    PagesWide1 = PagesWide - 1,
    place_more_x(TotalX, PagesWide1, XList1).


%% @private
place_y(YGutter, TotalY, PagesHigh) ->
    YList = [YGutter],
    PagesHigh1 = PagesHigh - 1,
    place_more_y(TotalY, PagesHigh1, YList).


%% @private
place_more_y(_TotalY, 0, List) ->
    lists:reverse(List);

place_more_y(TotalY, PagesHigh, YList) ->
    NextY = hd(YList) + TotalY,
    YList1 = [NextY | YList],
    PagesHigh1 = PagesHigh - 1,
    place_more_y(TotalY, PagesHigh1, YList1).


%% @private
row(XList, Y) ->
    [{X, Y} || X <- XList].    


%% @doc Return list of standard formats
%% RESOURCES
%%
%% For business card dimensions see:
%% http://designerstoolbox.com/designresources/postcards/
%% http://resources.printhandbook.com/pages/paper-size-chart.php
-spec format_list() -> list({page_format(), xy(), string()}).
format_list() ->
    [{a0,            {2380, 3368},  "ANSI International A"},
     {a1,            {1684, 2380},   "ANSI International A"},
     {a2,            {1190, 1684},  "ANSI International A"},
     {a3,            {842, 1190},   "ANSI International A"},
     {a4,            {595, 842},    "ANSI International A"},
     {a5,            {421, 595},    "ANSI International A"},
     {a6,            {297, 421},    "ANSI International A"},
     {a7,            {210, 297},    "ANSI International A"},
     {a8,            {148, 210},    "ANSI International A"},
     {a9,            {105, 148},    "ANSI International A"},
     {b0,            {2836, 4008},  "ANSI International B"},
     {b1,            {2004, 2836},  "ANSI International B"},
     {b2,            {1418, 2004},  "ANSI International B"},
     {b3,            {1002, 1418},  "ANSI International B"},
     {b4,            {709, 1002},   "ANSI International B"},
     {b5,            {501, 709},    "ANSI International B"},
     {b6,            {355, 501},    "ANSI International B"},
     {b7,            {250, 355},    "ANSI International B"},
     {b8,            {178, 250},    "ANSI International B"},
     {b9,            {125, 178},    "ANSI International B"},
     {b10,           {89, 125},     "ANSI International B"},
     {avery_5164,    {288, 252},    "Avery 5164 - mailing"},
     {avery_8168,    {252, 360},    "Avery 8168 - media kit"},
     {book1,         {378, 576},    "Createspacei 5.25 x 8"},
     {book4,         {396, 612},    "Createspace 5.5 x 8.5"},
     {book3,         {432, 648},    "Createspace 6 x 9"},
     {book4,         {504, 720},    "Createspace 7 x 10"},
     {book5,         {576, 720},    "Createspace 8 x 10"},
     {book6,         {594, 432},    "Createspace 8.25 x 6"},
     {book7,         {594, 594},    "Createspace 8.25 x 8.25"},
     {bookmark,      {180, 450},    "Check with printer"},
     {business_card, {252, 144},    "Business card US"},
     {envelope_no10, {297, 684},    "10 Official envelope"},
     {invoice,       {612, 792},    "Assume ANSI A letter"},
     {ledger,        {1224, 792},   "ANSI B ledger"},
     {legal,         {612, 1008},   "Legal"},
     {letter,        {612, 792},    "ANSI A letter"},
     {postcard1,     {360, 252},    "5 x 3.5-inches"},
     {postcard2,     {432, 306},    "6 x 4.25-inches"},
     {postcard3,     {504, 360},    "7 x 5-inches"},
     {report,        {612, 792},    "Assume ANSI A letter"},
     {tabloid,       {792, 1224},   "ANSI B tabloid"}
    ].
