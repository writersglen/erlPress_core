%%% ==========================================================================
%%% ep_report_hv_sty.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version    0.01
%%% @title      
%%% @doc
%%%   License:      MIT
%%%   File:         ep_report_hv_sty.erl
%%%   Description:  Define type styles for reports
%%%   Body copy:    Helvetica
%%% @end

%%% ==========================================================================

-module (ep_report_hv_sty).

-export ([justify_report_hv/1, ragged_report_hv/1, preformatted_report_hv/1]).
-export([centered_report_hv/1, ragged_left_report_hv/1]).

-export([report_hv_faces/1, report_hv_fontsize/1, report_hv_max_linespace/1]).
-export([report_hv_leading/1, report_hv_justify/1, report_hv_no_indent/1]).
-export([report_hv_indent/1]).


%% ***********************************************************************
%% Type style - report_hv
%% 
%% Helvetica heads; Helvetica body copy
%% ***********************************************************************


%% @doc Return type specifications

-spec justify_report_hv(Tag :: atom()) -> tuple().

justify_report_hv(Tag) ->
   Faces   = report_hv_faces(Tag),
   Leading = report_hv_leading(Tag),
   Justify = report_hv_justify(Tag),
   Indent  = report_hv_indent(Tag),
   {Faces, Leading, Justify, Indent}.


-spec ragged_report_hv(Tag :: atom()) -> tuple().

ragged_report_hv(Tag) ->
   Faces   = report_hv_faces(Tag),
   Leading = report_hv_leading(Tag),
   Justify = report_hv_ragged_right(Tag),
   Indent  = report_hv_indent(Tag),
   {Faces, Leading, Justify, Indent}.


-spec preformatted_report_hv(Tag :: atom()) -> tuple().

preformatted_report_hv(Tag) ->
   Faces   = report_hv_faces(Tag),
   Leading = report_hv_leading(Tag),
   Justify = report_hv_preformatted(Tag),
   Indent  = report_hv_no_indent(Tag),
   {Faces, Leading, Justify, Indent}.


-spec centered_report_hv(Tag :: atom()) -> tuple().

centered_report_hv(Tag) ->
   Faces   = report_hv_faces(Tag),
   Leading = report_hv_leading(Tag),
   Justify = report_hv_centered(Tag),
   Indent  = report_hv_no_indent(Tag),
   {Faces, Leading, Justify, Indent}.


-spec ragged_left_report_hv(Tag :: atom()) -> tuple().

ragged_left_report_hv(Tag) ->
   Faces   = report_hv_faces(Tag),
   Leading = report_hv_leading(Tag),
   Justify = report_hv_ragged_left(Tag),
   Indent  = report_hv_no_indent(Tag),
   {Faces, Leading, Justify, Indent}.


%% ***********************************************************************
%% report_hv/1 - helpers
%% ***********************************************************************


%% @doc Return report typefaces 

-spec report_hv_faces(Tag :: atom()) -> tuple().

report_hv_faces(Tag) ->
   case Tag of
      h1 -> ep_typespec:helvetica(h1, 36);
      h2 -> ep_typespec:helvetica(h2, 24);
      h3 -> ep_typespec:helvetica(h3, 18);
      h4 -> ep_typespec:helvetica(h4, 14);
      h5 -> ep_typespec:helvetica(h5, 12);
      h6 -> ep_typespec:helvetica(h6, 10);
      p  -> ep_typespec:helvetica(p, 12);
      ul -> ep_typespec:helvetica(ul, 12);
      ol -> ep_typespec:helvetica(ol, 12);
      cl -> ep_typespec:helvetica(cl, 12);
      li -> ep_typespec:helvetica(li, 12);
      ci -> ep_typespec:helvetica(ci, 12);
      _  -> ep_typespec:helvetica(p, 12)
    end.


%% @doc Return font size 

-spec report_hv_fontsize(Tag :: atom()) -> tuple().


report_hv_fontsize(Tag) ->
  case Tag of
    br -> report_hv_fontsize(p);
    _  -> Faces    = report_hv_faces(Tag),
          FaceList = element(2, Faces),
          Face     = lists:keyfind(Tag, 1, FaceList),
          FaceSpec = element(2, Face),
          element(3, FaceSpec)
    end.


%% @doc Return leading 

-spec report_hv_leading(Tag :: atom()) -> tuple().

report_hv_leading(Tag) ->
  FontSize = report_hv_fontsize(Tag),
  case Tag of
     p  -> round(FontSize * 1.5);
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

-spec report_hv_max_linespace(Tag :: atom()) -> tuple().

report_hv_max_linespace(Tag) ->
   FontSize = report_hv_fontsize(Tag),
   2 * FontSize.


%% @doc Return justification 

-spec report_hv_justify(Tag :: atom()) -> tuple().

report_hv_justify(Tag) ->
   case Tag of
      h1 -> centered;
      h2 -> centered;
      h3 -> ragged;
      p  -> justified;
      li -> justified;
      ci -> justified;
      _  -> ragged
   end.



-spec report_hv_ragged_right(Tag :: atom()) -> tuple().

report_hv_ragged_right(Tag) ->
   case Tag of
      h1 -> centered;
      h2 -> centered;
      h3 -> ragged;
      p  -> ragged;
      li -> ragged;
      ci -> ragged;
      _  -> ragged
   end.


-spec report_hv_preformatted(Tag :: atom()) -> tuple().

report_hv_preformatted(Tag) ->
   case Tag of
      h1 -> centered;
      h2 -> centered;
      h3 -> preformatted;
      p  -> preformatted;
      li -> preformatted;
      ci -> preformatted;
      _  -> preformatted 
   end.


-spec report_hv_centered(Tag :: atom()) -> tuple().

report_hv_centered(Tag) ->
   case Tag of
      h1 -> centered;
      h2 -> centered;
      h3 -> centered;
      p  -> centered;
      li -> preformatted;
      ci -> preformatted;
      _  -> preformatted 
   end.


-spec report_hv_ragged_left(Tag :: atom()) -> tuple().

report_hv_ragged_left(Tag) ->
   case Tag of
      h1 -> ragged_left;
      h2 -> ragged_left;
      h3 -> ragged_left;
      p  -> ragged_left;
      li -> preformatted;
      ci -> preformatted;
      _  -> preformatted 
   end.


%% @doc Return indentation 

-spec report_hv_no_indent(Tag :: atom()) -> integer().

report_hv_no_indent(Tag) ->
    case Tag of
      p  -> 0;
       _ -> 0
    end.

-spec report_hv_indent(Tag :: atom()) -> integer().

report_hv_indent(Tag) ->  
   case Tag of
      p  -> 30;
      li -> 30;
      ci -> 30;
      _  -> 0
   end.

