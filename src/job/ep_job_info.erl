%%% ==========================================================================
%%% ep_job_info.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:      MIT
%%%   File:         ep_job_info.erl
%%%   Description:  Display job specifications 
%%% @end

%%% ==========================================================================


%% NOTE: This module is a work-in-progress



-module (ep_job_info).

-export ([info_tagmap/1, info/1]). 

% -compile(export_all).

-include("../../include/ep.hrl").

info_tagmap(Points) ->
   {[p],
    [{default, eg_richText:mk_face("Helvetica", Points, true, default, 0)},
     {hb,      eg_richText:mk_face("Helvetica-Bold", Points, true, default, 0)},
     {em,      eg_richText:mk_face("Helvetica-Oblique", Points, true, default, 0)}
   ]}.

info(ProjectMap) ->
   ProjectName  = maps:get(project_name, ProjectMap),
   Client       = maps:get(client, ProjectMap),
   Author       = maps:get(author, ProjectMap),
   Deadline     = maps:get(deadline, ProjectMap),
   Description  = maps:get(description, ProjectMap),
   PaperStock   = maps:get(paper_stock, ProjectMap),
   PageFormat   = maps:get(page_format, ProjectMap),
   {ProjectName, Client, Author, Deadline, 
    Description, PaperStock, PageFormat
   }.





