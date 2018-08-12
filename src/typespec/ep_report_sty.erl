%%% ==========================================================================
%%% ep_report_sty.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version    0.01
%%% @title      
%%% @doc
%%%   License:      MIT
%%%   File:         ep_report_sty.erl
%%%   Description:  Define type styles for reports
%%5   Body copy:    Times-Roman                
%%% @end

%%% ==========================================================================


-module (ep_report_sty).

-export ([justify_report/1, ragged_report/1, preformatted_report/1]).
-export([centered_report/1, ragged_left_report/1]).

-export([report_fontsize/1, report_max_linespace/1]).
-export([report_faces/1, report_leading/1, report_justify/1]).
-export([report_preformatted/1, report_centered/1, report_ragged_left/1]).
-export([report_no_indent/1, report_indent/1]).


%% ***********************************************************************
%% Type style - report
%% 
%% Helvetica heads; Times-Roman body copy
%% ***********************************************************************


%% @doc Return type specifications

-spec justify_report(Tag :: atom()) -> tuple().

justify_report(Tag) ->
   Faces   = report_faces(Tag),
   Leading = report_leading(Tag),
   Justify = report_justify(Tag),
   Indent  = report_indent(Tag),
   {Faces, Leading, Justify, Indent}.


-spec ragged_report(Tag :: atom()) -> tuple().

ragged_report(Tag) ->
   Faces   = report_faces(Tag),
   Leading = report_leading(Tag),
   Justify = report_ragged_right(Tag),
   Indent  = report_indent(Tag),
   {Faces, Leading, Justify, Indent}.


-spec preformatted_report(Tag :: atom()) -> tuple().

preformatted_report(Tag) ->
   Faces   = report_faces(Tag),
   Leading = report_leading(Tag),
   Justify = report_preformatted(Tag),
   Indent  = report_no_indent(Tag),
   {Faces, Leading, Justify, Indent}.


-spec centered_report(Tag :: atom()) -> tuple().

centered_report(Tag) ->
   Faces   = report_faces(Tag),
   Leading = report_leading(Tag),
   Justify = report_centered(Tag),
   Indent  = report_no_indent(Tag),
   {Faces, Leading, Justify, Indent}.


-spec ragged_left_report(Tag :: atom()) -> tuple().

ragged_left_report(Tag) ->
   Faces   = report_faces(Tag),
   Leading = report_leading(Tag),
   Justify = report_ragged_left(Tag),
   Indent  = report_no_indent(Tag),
   {Faces, Leading, Justify, Indent}.


%% ***********************************************************************
%% report/1 - helpers
%% ***********************************************************************


%% @doc Return report typefaces 

-spec report_faces(Tag :: atom()) -> tuple().

report_faces(Tag) ->
   case Tag of
      h1 -> ep_typespec:helvetica(h1, 36);
      h2 -> ep_typespec:helvetica(h2, 24);
      h3 -> ep_typespec:helvetica(h3, 18);
      h4 -> ep_typespec:helvetica(h4, 14);
      h5 -> ep_typespec:helvetica(h5, 12);
      h6 -> ep_typespec:helvetica(h6, 10);
      p  -> ep_typespec:times(p, 12);
      li -> ep_typespec:times(li, 12);
      ci -> ep_typespec:times(ci, 12);
      _  -> ep_typespec:times(p, 12)
    end.


%% @doc Return font size 

-spec report_fontsize(Tag :: atom()) -> tuple().


report_fontsize(Tag) ->
  case Tag of
    br -> report_fontsize(p);
    _  -> Faces    = report_faces(Tag),
          FaceList = element(2, Faces),
          Face     = lists:keyfind(Tag, 1, FaceList),
          FaceSpec = element(2, Face),
          element(3, FaceSpec)
    end.


%% @doc Return leading 

-spec report_leading(Tag :: atom()) -> tuple().

report_leading(Tag) ->
  FontSize = report_fontsize(Tag),
  case Tag of
     p  -> round(FontSize * 1.5);
     h2 -> round(FontSize * 1.5);
     h3 -> round(FontSize * 1.5);
     ul -> FontSize;
     li -> round(FontSize * 1.5);
     ci -> round(FontSize * 1.5);
     br -> FontSize;
     _  -> round(FontSize * 1.25)
  end. 


%% @doc Return maximum line space for comfortable reading 

-spec report_max_linespace(Tag :: atom()) -> tuple().

report_max_linespace(Tag) ->
   FontSize = report_fontsize(Tag),
   2 * FontSize.


%% @doc Return justification 

-spec report_justify(Tag :: atom()) -> tuple().

report_justify(Tag) ->
   case Tag of
      h1 -> centered;
      h2 -> centered;
      h3 -> ragged;
      p  -> justified;
      li -> justified;
      ci -> justified;
      _  -> ragged
   end.


-spec report_ragged_right(Tag :: atom()) -> tuple().

report_ragged_right(Tag) ->
   case Tag of
      h1 -> centered;
      h2 -> centered;
      h3 -> ragged;
      p  -> ragged;
      li -> ragged;
      ci -> ragged;
      _  -> ragged
   end.


-spec report_preformatted(Tag :: atom()) -> tuple().

report_preformatted(Tag) ->
   case Tag of
      h1 -> centered;
      h2 -> centered;
      h3 -> preformatted;
      p  -> preformatted;
      li -> preformatted;
      ci -> preformatted;
      _  -> preformatted 
   end.


-spec report_centered(Tag :: atom()) -> tuple().

report_centered(Tag) ->
   case Tag of
      h1 -> centered;
      h2 -> centered;
      h3 -> centered;
      p  -> centered;
      li -> preformatted;
      ci -> preformatted;
      _  -> preformatted 
   end.


-spec report_ragged_left(Tag :: atom()) -> tuple().

report_ragged_left(Tag) ->
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

-spec report_no_indent(Tag :: atom()) -> integer().

report_no_indent(Tag) ->
    case Tag of
      p  -> 0;
       _ -> 0
    end.

-spec report_indent(Tag :: atom()) -> integer().

report_indent(Tag) ->  
   case Tag of
      p  -> 30;
      li -> 30;
      ci -> 30;
      _  -> 0
   end.

