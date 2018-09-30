%%% *********************************************************
%%% ep_page.erl
%%% 
%%% @author:     Lloyd R. Prentice
%%% @copyright   2018 Lloyd R. Prentice
%%% @version     .01
%%% @doc
%%%    License:       MIT 
%%%    File:          ep_page.erl
%%%    Description:   Page functions 
%%% @end
%%% *********************************************************      

-module(ep_page).

-export([
    create/3,
    default_tag_map/1,
    format/1,
    inc_page_no/1,
    page_dimensions/0,
    page_grid/1,
    page_inches/1,
    page_no/1,
    page_picas/1,
    page_points/1,
    project_id/1,
    put_format/2,
    put_page_grid/2,
    put_page_no/2,
    put_section/2,
    reset_page_no/1,
    section/1
]). % a..z

-include("ep_erltypes.hrl").

-type ep_page() :: #{
    project_id => string(),
    paper_stock => paper_stock(),
    page_format => page_format(),
    page_xy => xy(),
    page_number => integer(),
    section => _,
    page_grid => _,
    output_file => file:fd()
}.


%%% *********************************************************      
%%% Create page nap
%%% *********************************************************      

%% @doc NOTE: ep-impose:place_pages/3 returns a list to cover
%%       cases in which more than one page fits on paper stock
-spec create(ep_job(), integer(), file:fd()) -> ep_page().
create(ProjectMap, PageNumber, OFile) ->
    PaperStock = maps:get(paper_stock, ProjectMap),
    PageFormat = maps:get(page_format, ProjectMap),
    PageXY = ep_impose:place_pages(PaperStock, PageFormat, 1),
    #{    paper_stock   => PaperStock
        , page_format   => PageFormat
        , page_xy       => PageXY
        , page_number   => PageNumber
        , section       => undefined
        , page_grid     => undefined
        , output_file   => OFile
    }.


%%% *********************************************************      
%%% Get page attributes 
%%% *********************************************************      

-spec project_id(ep_page()) -> any().
project_id(#{project_id := PI}) -> PI.


-spec format(ep_page()) -> page_format().
format(#{format := F}) -> F.


-spec page_no(ep_page()) -> integer().
page_no(#{page_no := PN}) -> PN.


-spec section(ep_page()) -> any().
section(#{section := S}) -> S.


-spec page_grid(ep_page()) -> any().
page_grid(#{page_grid := PG}) -> PG.


-spec page_inches(string()) -> {string(), xy()}.
page_inches(Format) ->
   Dim = proplists:get_value(Format, page_dimensions()),
   {Format, Dim}.

page_picas(Format) ->
   Dim = page_inches(Format),
   ep_metrics:to_picas(Dim).

page_points(Format) ->
   Dim = page_inches(Format),
   ep_metrics:to_points(Dim).



% page_map(Page) ->
%   {_, PageMap} = Page,
%   PageMap.

% page_grid(Page) ->
%   {_, Page1} = Page,
%   maps:get(page_grid, Page1).


% page_number(PageNo, Project) ->
%    {_, Page} = page_numbers(Project),
%    ep_page:get_page_grid(Page).


%%% *********************************************************      
%%% Put page attributes 
%%% *********************************************************      

put_format(Format, Page) ->
    maps:put(format, Format, Page).

put_page_no(PageNo, Page) ->
    maps:put(page_no, PageNo, Page).

inc_page_no(Page) ->
   PageNo = page_no(Page),
   PageNo1 = PageNo + 1,
   put_page_no(PageNo1, Page).

reset_page_no(Page) ->
   put_page_no(1, Page).

put_section(Section, Page) ->
    maps:put(section, Section, Page).

put_page_grid(PageGrid, Page) ->
    maps:put(page_grid, PageGrid, Page).


   
%% Need imposition functions

% page_map(PageFormat, StockWidth, StockHeight) ->
%   {PageWidth, PageHeight} = stock_size_points(PageFormat),

%   PageX = (StockWidth - PageWidth) div 2,
%   DeltaY = (StockHeight - PageHeight) div 2,
%   PageY = StockHeight - DeltaY,





%%%  *********************************************************      
%%% Create a page; e.g. container box for content
%%% *********************************************************      

%% This is a impostion function

% create_page(PageNo, Project) ->
%   PageMap = ep_job:get_page_map(Project),
%   Page = [{page_id,     ep_lib:page_id(3)},
%   N = {PageX, PageY},
%   E = {PageX + PageWidth, PageY},
%   S = {PageX + PageWidth, PageY - PageHeight},
%   W = {PageX, PageY - PageHeight},

%   Coordinates = [{page_n, N},
%                  {page_e, E},
%                  {page_s, S},
%                  {page_w, W}
%                 ],
%   maps:from_list(Coordinates).
















%% ****************************************************************
%% Return page dimenions
%% **************************************************************** 














%% ****************************************************************
%% Page dimensions
%% http://resources.printhandbook.com/pages/paper-size-chart.php
%% https://www.thebookdesigner.com/2010/09/self-publishing-basics-
%%      how-to-pick-the-size-of-your-book/
%% ****************************************************************

page_dimensions() ->
   [{"book 6 x 9",                {6, 9}},
    {"book 7 x 10",               {7, 10}},
    {"book 8.25 x 6",             {8.25, 6}},
    {"book 8 x 10",               {8, 10}},
    {"book 8.25 x 8.25",          {8.25, 8.25}},
    {"bookmark",                  {2, 6}},
    {"business card",             {3.5, 2}},
    {"business envelope",         {4.125, 9.5}},
    {"invoice",                   {8.5, 11}},
    {"label_5164",                {288, 252}},   % Avery 5164 - mailing
    {"label_8168",                {252, 360}},   % Avery 8168 - media kit
    {"legal letter",              {8.5, 14}},
    {"letter",                    {8.5, 11}},
    {"novel 5.25 x 8",            {5.25,8}},
    {"novel 5.5 x 8.5",           {5.5, 8.5}},
    {"postcard",                  {6, 4}},
    {"report",                    {8.5, 11}}
   ].





default_tag_map(Pts) ->
    {[p],
     [{default, eg_richText:mk_face("Times-Roman", Pts, true, default, 0)},
      {em, eg_richText:mk_face("Times-Italic", Pts, true, default, 0)},

      %% XXX !!! the font ZapfChancery-MediumItalic is not availible
      {red, eg_richText:mk_face("ZapfChancery-MediumItalic", Pts, true,
                                {1, 0, 0}, 0)},
      {blue, eg_richText:mk_face("ZapfChancery-MediumItalic", Pts, true,
                                 {0, 0, 1}, 0)},

      {code, eg_richText:mk_face("Courier", Pts, false, default, 0)},
      {b, eg_richText:mk_face("Times-Bold", Pts, true, default, 0)},
      {hb, eg_richText:mk_face("Helvetica-Bold", Pts, true, default, 0)},
      {helv, eg_richText:mk_face("Helvetica", Pts, true, default, 0)}
     ]}.


%%% *********************************************************      
%%% Get page coordinates 
%%% *********************************************************      

% get_page_map(Job) ->
%    maps:get(page_map, Job).

% page_n(PageMap) ->
%   maps:get(page_n, PageMap).

% page_e(PageMap) ->
%   maps:get(page_e, PageMap).

% page_s(PageMap) ->
%   maps:get(page_s, PageMap).

% page_w(PageMap) ->
%   maps:get(page_w, PageMap).



%%% *********************************************************      
%%% Return page top, right, bottom, and left coordinates
%%
%% For use with eg_pdf:line(PID, From, To). 
%%% *********************************************************      


% page_top_coords(Job) ->
%    PageMap = get_page_map(Job),
%    From = page_n(PageMap),
%    To = page_e(PageMap),
%    {From, To}.

% page_right_coords(Job) ->
%    PageMap = get_page_map(Job),
%    From = page_e(PageMap),
%    To = page_s(PageMap),
%    {From, To}.

% page_bottom_coords(Job) ->
%    PageMap = get_page_map(Job),
%    From = page_s(PageMap),
%    To = page_w(PageMap),
%    {From, To}.

% page_left_coords(Job) ->
%    PageMap = get_page_map(Job),
%    From = page_w(PageMap),
%    To = page_n(PageMap),
%    {From, To}.

% n_crop(Job) ->
%    Delta = 10,
%    PageMap = get_page_map(Job),
%    {NX, NY} = page_n(PageMap),
%    {{NX - Delta, NY}, {NX + Delta, NY},
%     {NX, NY - Delta}, {NX, NY + Delta}
%    }.


% p(Job) ->
%    Delta = 10,
%    PageMap = get_page_map(Job),
%    {EX, EY} = page_e(PageMap),
%    {{EX - Delta, EY}, {EX + Delta, EY},
%     {EX, EY - Delta}, {EX, EY + Delta}
%    }.

% s_crop(Job) ->
%    Delta = 10,
%    PageMap = get_page_map(Job),
%    {SX, SY} = page_s(PageMap),
%    {{SX - Delta, SY}, {SX + Delta, SY},
%     {SX, SY - Delta}, {SX, SY + Delta}
%    }.

% w_crop(Job) ->
%    Delta = 10,
%    PageMap = get_page_map(Job),
%    {WX, WY} = page_w(PageMap),
%    {{WX - Delta, WY}, {WX + Delta, WY},
%     {WX, WY - Delta}, {WX, WY + Delta}
%    }.


% page_scale(PageWidth, PageHeight, Job) ->
%   PaperStock = maps:get(paper_stock, Job),
%   {_, {StockWidth, StockHeight}} = ep_paper:paper_stock_points(PaperStock),
%   Flag = (StockWidth > PageWidth) and (StockHeight > PageHeight),
%   case Flag of
%      true  -> Scale = 1;
%      false -> Scale = 0.5
%   end,
%   Scale.

%%% *********************************************************      
%%% Specify page dimenions 
%%% *********************************************************      

% put_page_map(PageFormat, Job) ->
%   {StockWidth, StockHeight} = get_stock_dim(Job),
%   PageMap = ep_page:page_map(PageFormat, StockWidth, StockHeight),
%   maps:put(page_map, PageMap, Job).

% put_page_grid(PageNo, PageGrid, Job) ->
%   Pages = maps:get(pages, Job),
%   {_, Page} = gb_trees:lookup(PageNo, Pages),
%   {PageNo, Map} = ep_page:put_page_grid(PageGrid, Page),
%   Pages1 =  gb_trees:update(PageNo, Map, Page),
%   maps:put(pages, Pages1, Job).

%%% *********************************************************      
%%% Get page coordinates 
%%% *********************************************************      

% get_page_map(Job) ->
%    maps:get(page_map, Job).

% page_n(Page) ->
%   maps:get(page_n, Page).

% page_e(PageMap) ->
%   maps:get(page_e, PageMap).

% page_s(PageMap) ->
%   maps:get(page_s, PageMap).

% page_w(PageMap) ->
%   maps:get(page_w, PageMap).
