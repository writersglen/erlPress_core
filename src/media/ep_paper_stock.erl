%%% ==========================================================================
%%% ep_paper_stock.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:
%%%   File:         ep_paper_stock.erl
%%%   Description:  Standard paper stock dimensions
%%% @end

%%% ==========================================================================


-module (ep_paper_stock).

-export([
    desktop_printer_stock/0,
    standard_sizes/0,
    standards/0,
    stock_e/1, stock_n/1, stock_s/1, stock_w/1,
    stock_size_inches/1, stock_size_picas/1, stock_size_points/1,
    stock_height/1, stock_width/1
]). % a..z

-include("ep_erltypes.hrl").


standards() ->
    %% Unzip will split pairs into 2 lists of all first and all second elements
    {Standards, _Sizes} = lists:unzip(standard_sizes()),
    Standards.


%% @doc Return paper stock dimensions in inches
-spec stock_size_inches(paper_stock()) -> tuple().
stock_size_inches(Stock) ->
   {_Type, Dimensions} = lists:keyfind(Stock, 1, standard_sizes()),
   Dimensions.
   

%% @doc Return paper stock dimensions in picas
-spec stock_size_picas(paper_stock()) -> integer_xy().
stock_size_picas(Stock) ->
   Dimensions = stock_size_inches(Stock),
   ep_metrics:to_picas(Dimensions).


%% @doc Return paper stock dimensions in points
-spec stock_size_points(paper_stock()) -> integer_xy().
stock_size_points(Stock) ->
   Dimensions = stock_size_inches(Stock),
   ep_metrics:to_points(Dimensions).


%% @doc Return paper stock width in points
-spec stock_width(paper_stock()) -> integer().
stock_width(Stock) ->
   {Width, _Height} = stock_size_points(Stock),
   Width.


%% @doc Return paper stock height in points
-spec stock_height(paper_stock()) -> points().
stock_height(Stock) ->
   {_Width, Height} = stock_size_points(Stock),
   Height.


stock_n(_Stock) ->
   {0, 0}.


stock_e(Stock) ->
   {Width, _Height} = stock_size_points(Stock),
   {0, Width}.


stock_s(Stock) ->
   stock_size_points(Stock).


stock_w(Stock) ->
   {_Width, Height} = stock_size_points(Stock),
   {0, Height}.


%% @doc Return list of standard desktop printer stock
-spec desktop_printer_stock() -> list().
desktop_printer_stock() ->
   [letter, 
    legal,
    a4
   ].


%% @doc Return paper stock dimensions
%% http://resources.printhandbook.com/pages/paper-size-chart.php
%% https://www.thebookdesigner.com/2010/09/self-publishing-basics-how-to-pick-the-size-of-your-book/
-spec standard_sizes() -> list({paper_stock(), xy()}).
standard_sizes() ->
    [{a0,            {33.1, 46.8}},
     {a1,            {23.4, 33.1}},
     {a2,            {16.5, 23.4}},
     {a3,            {11.7, 16.5}},
     {a4,            {8.3, 11.7}},
     {a5,            {5.8, 8.3}},
     {a6,            {4.1, 5.8}},
     {a7,            {2.9, 4.1}},
     {a8,            {2.0, 2.9}},
     {a9,            {1.5, 2.0}},
     {a10,           {1.0, 1.5}},
     {avery_labels,  {8.5, 11}},
     {envelope_no10, {4.125, 9.5}},
     {letter,        {8.5, 11}},
     {legal,         {8.5, 14}},
     {tabloid,       {11, 17}}
    ].
