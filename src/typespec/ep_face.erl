%%% ==========================================================================
%%% ep_face.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version    0.01
%%% @title      
%%% @doc
%%%   License:      MIT
%%%   File:         ep_face.erl
%%%   Description:  Layout primitives 
%%% @end

%%% ==========================================================================

-module (ep_face).

-export([
    create/2,
    make_face/1
]).

-include("ep_erltypes.hrl").

-define(COLOR, eg_pdf_op:color(black)).


%% @doc Create font face map
%% See ep_typestyle:tag_face/3 for application
-spec create(Font :: string(), Size :: integer()) -> ep_font_face().
create(Font, Size) ->
    #{    font        => Font
        , size        => Size
        , breakable   => true
        , color       => ?COLOR
        , voffset     => 0
    }.


%% @doc Define font face
-spec make_face(ep_font_face()) -> ok.
make_face(FaceMap) ->
    Font      = maps:get(font, FaceMap),
    FontSize  = maps:get(size, FaceMap),
    Breakable = maps:get(breakable, FaceMap),
    Color     = maps:get(color, FaceMap),
    VOffset   = maps:get(voffset, FaceMap),
    eg_richText:mk_face(Font, FontSize, Breakable, Color, VOffset).
