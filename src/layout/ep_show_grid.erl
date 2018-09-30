%%==========================================================================
%%% ep_show_grid.erl
%%%
%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%     License:       MIT
%%%     File:          ep_grid.erl
%%%     Description:   Page layout functions 
%%% @end
%%==========================================================================

-module (ep_show_grid).

-export([
    print_layout_grid/1,
    show_grid/2
]).

-include("eg.hrl").

-define(PDF_DIR, "./pdf/").
-define(PAGE_GRID_DIR, "./pdf/").


%% @doc Print layout grid
print_layout_grid(PageSize) ->
    PageType = atom_to_list(PageSize),
    OFile = PageType ++ "_layout.pdf",
    PDF = eg_pdf:new(),

    show_grid(PDF, PageSize),

    {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?PAGE_GRID_DIR ++ OFile, [Serialised]),
    eg_pdf:delete(PDF).


%% @private
show_grid(PDF, Paper) ->
    {_, _, PaperWidth, PaperHeight} = eg_pdf:pagesize(Paper),
    %% make grid lines fall on multiples of 25
    Top1 = round((PaperHeight) / 25) * 25,
    %% Seems necessary to line up 792 pixels with bottom of
    %% paper stock; may be printer specific
    Top = Top1 - 10,
    Bottom = 10,
    Left = 10,
    %% make grid lines fall on multiples of 25
    Right = round((PaperWidth - 20) / 25) * 25,
    eg_pdf:set_font(PDF, "Helvetica", 8),
    eg_pdf:set_stroke_color(PDF, aqua),
    vlines(PDF, Left, Right, Top, Bottom),
    hlines(PDF, Left, Right, Top, Bottom).


%% @private
hlines(PDF, Left, Right, Top, _Bottom) ->
    eg_pdf:save_state(PDF),
    eg_pdf:set_font(PDF, "Helvetica", 6),
    diter(Top, 25, 10,
          fun(Y) ->
              %% eg_pdf:set_fill_gray(PDF,1.0),
              eg_pdf:line(PDF, Left, Y, Left + 20, Y),
              eg_pdf:line(PDF, Right, Y, Right - 20, Y),
              %% eg_pdf:set_fill_gray(PDF,0.8),
              eg_pdf:line(PDF, Left + 20, Y, Right - 20, Y),
              moveAndShow(PDF, Left, Y + 2, "Y=" ++ eg_pdf_op:n2s(Top - Y)),
              moveAndShow(PDF, Right - 20, Y + 2, "Y=" ++ eg_pdf_op:n2s(Top - Y)),
              true
          end),
    eg_pdf:restore_state(PDF).


%% @private
vlines(PDF, _Left, Right, Top, Bottom) ->
    eg_pdf:save_state(PDF),
    eg_pdf:set_font(PDF, "Helvetica", 6),
    diter(Right, 25, 10,
          fun(X) ->
              eg_pdf:line(PDF, X, Top, X, Top - 20),
              moveAndShow(PDF, X - 5, Top - 35, "X=" ++ eg_pdf_op:n2s(X)),
              eg_pdf:line(PDF, X, Bottom, X, Bottom + 20),
              eg_pdf:line(PDF, X, Top - 40, X, Bottom + 35),
              moveAndShow(PDF, X - 5, Bottom + 23, "X=" ++ eg_pdf_op:n2s(X))
          end),
    eg_pdf:restore_state(PDF).


%% @private
moveAndShow(PDF, X, Y, Str) ->
    eg_pdf:begin_text(PDF),
    eg_pdf:set_text_pos(PDF, X, Y),
    eg_pdf:text(PDF, Str),
    eg_pdf:end_text(PDF).


%% @private
diter(X, _Inc, Stop, _F) when X < Stop ->
    true;

diter(X, Inc, Stop, F) ->
    F(X), diter(X - Inc, Inc, Stop, F).
