%%% ==========================================================================
%%% ep_cdoc_sty.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version    0.01
%%% @title      
%%% @doc
%%%   License:      MIT
%%%   File:         ep_cdoc_sty.erl
%%%   Description:  Define type styles for code listings 
%%%   Body copy:    Helvetica
%%% @end

%%% ==========================================================================



-module (ep_cdoc_sty).

-export ([cdoc/1]).

-export([cdoc_fontsize/1, cdoc_max_linespace/1]).
-export([cdoc_leading/1, cdoc_justify/1, cdoc_no_indent/1]).
-export([cdoc_indent/1]).


%% ***********************************************************************
%% Type style - ep_cdoc 
%% 
%% Helvetica heads; Helvetica body copy
%% ***********************************************************************


%% @doc Return type specifications

-spec cdoc(Tag :: atom()) -> tuple().

cdoc(Tag) ->
   Faces   = cdoc_faces(Tag),
   Leading = cdoc_leading(Tag),
   Justify = cdoc_preformatted(Tag),
   Indent  = cdoc_no_indent(Tag),
   {Faces, Leading, Justify, Indent}.


%% ***********************************************************************
%% cdoc/1 - helpers
%% ***********************************************************************


%% @doc Return cdoc typefaces 

-spec cdoc_faces(Tag :: atom()) -> tuple().

cdoc_faces(Tag) ->
   case Tag of
      h1 -> ep_typespec:helvetica(h1, 36);
      h2 -> ep_typespec:helvetica(h2, 24);
      h3 -> ep_typespec:helvetica(h3, 18);
      h4 -> ep_typespec:helvetica(h4, 14);
      h5 -> ep_typespec:helvetica(h5, 12);
      h6 -> ep_typespec:helvetica(h6, 10);
      p  -> ep_typespec:helvetica(p, 12);
      ul -> ep_typespec:helvetica(ul, 12);
      ol -> ep_typespec:helvetica(cl, 12);
      cl -> ep_typespec:helvetica(cl, 12);
      li -> ep_typespec:helvetica(li, 12);
      ci -> ep_typespec:helvetica(ci, 12);
      _  -> ep_typespec:helvetica(p, 12)
    end.


%% @doc Return font size 

-spec cdoc_fontsize(Tag :: atom()) -> tuple().


cdoc_fontsize(Tag) ->
  case Tag of
    br -> cdoc_fontsize(p);
    _  -> Faces    = cdoc_faces(Tag),
          FaceList = element(2, Faces),
          Face     = lists:keyfind(Tag, 1, FaceList),
          FaceSpec = element(2, Face),
          element(3, FaceSpec)
    end.


%% @doc Return justification 

-spec cdoc_justify(Tag :: atom()) -> tuple().

cdoc_justify(Tag) ->
   case Tag of
      h1 -> centered;
      h2 -> centered;
      h3 -> centered;
      p  -> preformatted;
      li -> preformatted;
      ci -> preformatted;
      _  -> preformatted 
   end.



%% @doc Return leading 

-spec cdoc_leading(Tag :: atom()) -> tuple().

cdoc_leading(Tag) ->
  FontSize = cdoc_fontsize(Tag),
  case Tag of
     p  -> round(FontSize * 1.25);
     h2 -> round(FontSize * 1.5);
     h3 -> round(FontSize * 1.5);
     ul -> FontSize;
     cl -> FontSize;
     ol -> FontSize;
     li -> round(FontSize * 1.5);
     ci -> round(FontSize * 1.5);
     br -> FontSize;
     _  -> round(FontSize * 1.25)
  end. 


%% @doc Return maximum line space for comfortable reading 

-spec cdoc_max_linespace(Tag :: atom()) -> tuple().

cdoc_max_linespace(Tag) ->
   FontSize = cdoc_fontsize(Tag),
   2 * FontSize.


%% @doc Return justification 

-spec cdoc_preformatted(Tag :: atom()) -> tuple().

cdoc_preformatted(Tag) ->
   case Tag of
      h1 -> centered;
      h2 -> centered;
      h3 -> ragged;
      p  -> preformatted;
      li -> preformatted;
      ci -> preformatted;
      _  -> preformatted 
   end.


%% @doc Return indentation 

-spec cdoc_no_indent(Tag :: atom()) -> integer().

cdoc_no_indent(Tag) ->
    case Tag of
      p  -> 0;
       _ -> 0
    end.

-spec cdoc_indent(Tag :: atom()) -> integer().

cdoc_indent(Tag) ->  
   case Tag of
      p  -> 30;
      li -> 30;
      ci -> 30;
      _  -> 0
   end.

