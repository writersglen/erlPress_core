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

-export ([create/2,make_face/1]).

-define(COLOR, eg_pdf_op:color(black)).

%% Colors: white, silver, gray, black, maroon, red, fuschia,
%%         purple, lime, green, olive, yellow, navy, blue, teal, aqua 
%%         | {R,G,B}

%% ***********************************************************
%% Create font face map
%% See ep_typestyle:tag_face/3 for application
%% ***********************************************************


%% @doc Create font face map

-spec create(Font :: string(),
             Size :: integer()) -> map().

create(Font, Size) ->
   #{ font        => Font 
    , size        => Size 
    , breakable   => true 
    , color       => ?COLOR 
    , voffset     => 0 
    }.


%% ***********************************************************
%% make_face/3  
%% ***********************************************************

%% @doc Define font face 

-spec make_face(FaceMap :: map()) -> ok.

make_face(FaceMap) ->
    Font      = maps:get(font, FaceMap),
    FontSize  = maps:get(size, FaceMap),
    Breakable = maps:get(breakable, FaceMap),
    Color     = maps:get(color, FaceMap),
    VOffset   = maps:get(voffset, FaceMap),
    eg_richText:mk_face(Font, FontSize, Breakable, Color, VOffset).
