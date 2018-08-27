%%% *********************************************************
%%% ep_job.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:      MIT
%%%   File:         ep_job.erl
%%%   Description:  Create print job specification 
%%% @end

%%% ==========================================================================


-module (ep_job).

-export([create/2, flip_box/2, flip_y/2]).
-export([resource_path/1]).
-export([standard_paper_stock/0, desktop_printer_stock/0]).
-export([paper_stock/1, stock_size/1]).
-export([page_formats/0, page_format/1]).
-export([specified_page_size/1, page_positions/2]).
-export([stock_position/1]).
-export([save_job/2]).

-define(PAPER_STOCK, letter).
%% -define(_PAPER_STOCK, legal).
%% -define(PAPER_STOCK, a4).

-define(PAGE_FORMAT, letter).


%%% *********************************************************      
%%% Create job 
%%% *********************************************************      

%% @doc Create project specification map

-spec create(Title     :: string(),
             Publisher :: string()) -> atom().

create(Title, Publisher) ->
   #{ title          => Title 
    , publisher      => Publisher 
    , path           => "./publishers/" ++ Publisher ++ "/jobs" 
    , directory      => re:replace(Title, " ", "_", [global, {return, list}])
    , author         => undefined
    , subject        => undefined
    , description    => undefined
    , keywords       => undefined
    , start_date     => ep_lib:today()
    , deadline       => undefined
    , paper_stock    => ?PAPER_STOCK 
    , page_format    => ?PAGE_FORMAT 
   }.


flip_box(Job, Map) ->
   {PaperStock, _PagePosition} = ep_job:stock_position(Job),
   Position     = maps:get(position, Map),
   Size         = maps:get(size, Map),
   ep_lib:impose_box(Position, Size, PaperStock).


flip_y(Job, Map) ->
   {PaperStock, PagePosition} = ep_job:stock_position(Job),
   Position     = maps:get(position, Map),
   ep_lib:impose_xy(Position, PagePosition, PaperStock).



resource_path(Job) ->
   Path = maps:get(path, Job),
   Directory = maps:get(directory, Job),
   Path ++ "/" ++ Directory.

%% @doc Return paper stock

-spec standard_paper_stock() -> tuple().

standard_paper_stock() ->
   ep_paper_stock:standards().


%% @doc Return standard desktop printer paper stock 

-spec desktop_printer_stock() -> atom().


desktop_printer_stock() ->
   [ a4
   , avery_labels
   , envelope_no10
   , legal
   , letter
   ].


%% @doc Return paper stock 

-spec paper_stock(Job :: map()) -> atom().

paper_stock(Job) ->
   maps:get(paper_stock, Job).


%% @doc Return paper stock dimensions in points

-spec stock_size(Job :: map()) -> tuple().

stock_size(Job) ->
   Stock = maps:get(paper_stock, Job),
   ep_paper_stock:stock_size_points(Stock).


%% @doc Return list of conventional page formats 

-spec page_formats() -> list().

page_formats() ->
   ep_format:formats().


%% @doc Return specified page format

-spec page_format(Job :: map()) -> atom().

page_format(Job) ->
   maps:get(page_format, Job).


%% @doc Return size of specified page format in points

-spec specified_page_size(Job :: map()) -> tuple().

specified_page_size(Job) ->
   PageFormat = maps:get(page_format, Job),
   ep_format:pagesize_points(PageFormat).


%% @doc Return upper left corner of pages imposed on paper stock

-spec page_positions(Job :: map(), N :: integer()) -> list().

page_positions(Job, N) ->
   PaperStock = paper_stock(Job),
   PageFormat = page_format(Job),
   ep_impose:place_pages(PaperStock, PageFormat, N).

stock_position(Job) ->
    PaperStock = maps:get(paper_stock, Job),
    PagePositions = ep_job:page_positions(Job, 1),
    [Position | _] = PagePositions,
    {PaperStock, Position}.



save_job(PDF, OutFile) ->
    {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(OutFile,[Serialised]),
    eg_pdf:delete(PDF).



