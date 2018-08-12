%%% ==========================================================================
%%% ep_faces.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version    0.01
%%% @title      
%%% @doc
%%%   License:      MIT
%%%   File:         ep_faces.erl
%%%   Description:  Selection of type faces 
%%% @end

%%% ==========================================================================


-module (ep_faces).

% -export ([]).
-compile([export_all]).

-define(COLOR, black).

%% Colors: white, silver, gray, black, maroon, red, fuschia,
%%         purple, lime, green, olive, yellow, navy, blue, teal, aqua 
%%         | {R,G,B}

%% ***********************************************************
%% default 
%% ***********************************************************

default(FontSize) ->
    FaceMap = ep_face:create("Times-Roman", FontSize),
    ep_face:make_face(FaceMap).

%% ***********************************************************
%% Times-Roman
%% ***********************************************************

times(FontSize) ->
    FaceMap = ep_face:create("Times-Roman", FontSize),
    ep_face:make_face(FaceMap).

times_italic(FontSize) ->
    FaceMap = ep_face:create("Times-Italic", FontSize),
    ep_face:make_face(FaceMap).

times_bold(FontSize) ->
    FaceMap = ep_face:create("Times-Bold", FontSize),
    ep_face:make_face(FaceMap).

times_boldItalic(FontSize) ->
    FaceMap = ep_face:create("Times-BoldItalic", FontSize),
    ep_face:make_face(FaceMap).

%% ***********************************************************
%% Courier 
%% ***********************************************************

courier(FontSize) ->
    FaceMap = ep_face:create("Courier", FontSize),
    ep_face:make_face(FaceMap).

courier_oblique(FontSize) ->
    FaceMap = ep_face:create("Courier-Oblique", FontSize),
    ep_face:make_face(FaceMap).

courier_bold(FontSize) ->
    FaceMap = ep_face:create("Courier-Bold", FontSize),
    ep_face:make_face(FaceMap).

courier_boldOblique(FontSize) ->
    FaceMap = ep_face:create("Courier-BoldOblique", FontSize),
    ep_face:make_face(FaceMap).


%% ***********************************************************
%% Helvetica 
%% ***********************************************************

helvetica(FontSize) ->
    FaceMap = ep_face:create("Helvetica", FontSize),
    ep_face:make_face(FaceMap).

helvetica_oblique(FontSize) ->
    FaceMap = ep_face:create("Helvetica_Oblique", FontSize),
    ep_face:make_face(FaceMap).

helvetica_bold(FontSize) ->
    FaceMap = ep_face:create("Helvetica-Bold", FontSize),
    ep_face:make_face(FaceMap).

helvetica_boldOblique(FontSize) ->
    FaceMap = ep_face:create("Helvetica-BoldOblique", FontSize),
    ep_face:make_face(FaceMap).

%% ***********************************************************
%% Symbol 
%% ***********************************************************

symbol(FontSize) ->
    FaceMap = ep_face:create("Symbol", FontSize),
    ep_face:make_face(FaceMap).

%% ***********************************************************
%% ZapfDingbats
%% ***********************************************************

zapfDingbats(FontSize) ->
    FaceMap = ep_face:create("ZapfDingbats", FontSize),
    ep_face:make_face(FaceMap).


