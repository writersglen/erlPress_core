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

-export([
    create/2,
    desktop_printer_stock/0,
    flip_box/2,
    flip_y/2,
    page_format/1,
    page_formats/0,
    page_positions/2,
    paper_stock/1,
    resource_path/1,
    save_job/2,
    specified_page_size/1,
    standard_paper_stock/0,
    stock_position/1,
    stock_size/1
]).

-define(PAPER_STOCK, letter).
%% -define(_PAPER_STOCK, legal).
%% -define(PAPER_STOCK, a4).

-define(PAGE_FORMAT, letter).

-include("ep_erltypes.hrl").


%%% *********************************************************      
%%% Create job 
%%% *********************************************************      

%% @doc Create project specification map

-spec create(Title :: iolist(), Publisher :: iolist()) -> job().

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


-spec flip_box(job(), #{position => number(), size => number()}) -> xy().
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

-spec desktop_printer_stock() -> list(paper_stock()).

desktop_printer_stock() ->
    [a4, avery_labels, envelope_no10, legal, letter].


%% @doc Return paper stock 

-spec paper_stock(Job :: job()) -> paper_stock().

paper_stock(Job) ->
   maps:get(paper_stock, Job).


%% @doc Return paper stock dimensions in points

-spec stock_size(Job :: job()) -> {paper_stock(), xy()}.

stock_size(Job) ->
   Stock = maps:get(paper_stock, Job),
   ep_paper_stock:stock_size_points(Stock).


%% @doc Return list of conventional page formats 

-spec page_formats() -> list().

page_formats() ->
   ep_format:formats().


%% @doc Return specified page format

-spec page_format(Job :: job()) -> page_format().

page_format(Job) ->
   maps:get(page_format, Job).


%% @doc Return size of specified page format in points

-spec specified_page_size(Job :: job()) -> xywh().

specified_page_size(Job) ->
   PageFormat = maps:get(page_format, Job),
   ep_format:pagesize_points(PageFormat).


%% @doc Return upper left corner of pages imposed on paper stock

-spec page_positions(job(), N :: integer()) -> list().

page_positions(Job, N) ->
   PaperStock = paper_stock(Job),
   PageFormat = page_format(Job),
   ep_impose:place_pages(PaperStock, PageFormat, N).


-spec stock_position(job()) -> {paper_stock(), xy()}.
stock_position(Job) ->
    PaperStock = maps:get(paper_stock, Job),
    PagePositions = ep_job:page_positions(Job, 1),
    [Position | _] = PagePositions,
    {PaperStock, Position}.


-spec save_job(pdf_server_pid(), file:filename()) -> ok.
save_job(PDF, OutFile) ->
    {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(OutFile, [Serialised]),
    eg_pdf:delete(PDF).
