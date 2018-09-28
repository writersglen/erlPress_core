%%% ==========================================================================
%%% ep_typespec.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version    0.01
%%% @title      
%%% @doc
%%%   License:      MIT
%%%   File:         ep_typespec.erl
%%%   Description:  Type specification functions 
%%%   Note:         erlguten refers to "tagmaps." Since they are not
%%%                 maps in the contemporary Erlang sense, we've 
%%%                 changed the term "tagmap" to "typespec"
%%%
%%%                 A "typestyle" is an atom that names a set of
%%%                 type specifications. See: ep_typestyle:report/1
%%% @end

%%% ==========================================================================



-module (ep_typespec).

-export([fonts/0, get_specs/2]).
-export([faces/2, fontsize/2, leading/2, max_linespace/2, justify/2, indent/2]).
-export([get_fontmap/2, list_faces/2]).
-export([times/2, helvetica/2, courier/2, default/1]).
-export([default_helvetica/1, default_face/0]).
-export([get_face/3]).
-export([face_map/2]).
-export ([get_font/3]). 


%% ***************************************************************
%% fonts/2 - Return all fonts
%% ***************************************************************


%% @doc Return all fonts

fonts() ->
   eg_font_map:all_fonts().


%% ***************************************************************
%% get_specs/2
%% ***************************************************************


%% @doc Return typefaces, leading, jusitification, and indentation

-spec get_specs(TypeStyle  :: tuple(),
                Tag        :: atom()) -> tuple().

get_specs(TypeStyle, Tag) ->
   case TypeStyle of
      justify_report          -> ep_report_sty:justify_report(Tag);
      ragged_report           -> ep_report_sty:ragged_report(Tag);
      preformatted_report     -> ep_report_sty:preformatted_report(Tag);
      centered_report         -> ep_report_sty:centered_report(Tag);

      justify_report_hv       -> ep_report_hv_sty:justify_report_hv(Tag);
      ragged_report_hv        -> ep_report_hv_sty:ragged_report_hv(Tag);
      preformatted_report_hv  -> ep_report_hv_sty:preformatted_report_hv(Tag);
      centered_report_hv      -> ep_report_hv_sty:centered_report_hv(Tag);
      ragged_left_report_hv   -> ep_report_hv_sty:ragged_left_report_hv(Tag);

      cdoc                    -> ep_cdoc_sty:cdoc(Tag);
      _                       -> ep_report_sty:justify_report(Tag)

   end.



%% ***************************************************************
%% faces/2 - Return typefaces
%% ***************************************************************


%% @doc Return typefaces

-spec faces(TypeStyle  :: tuple(),
            Tag        :: atom()) -> tuple().


faces(justify_report, Tag)      -> ep_report_sty:report_faces(Tag);
faces(ragged_report, Tag)       -> ep_report_sty:report_faces(Tag);
faces(preformatted_report, Tag) -> ep_report_sty:report_faces(Tag);
faces(centered_report, Tag)     -> ep_report_sty:report_faces(Tag);
faces(ragged_left_report, Tag)  -> ep_report_sty:report_faces(Tag);

faces(justify_report_hv, Tag)      -> ep_report_hv_sty:report_hv_faces(Tag);
faces(ragged_report_hv, Tag)       -> ep_report_hv_sty:report_hv_faces(Tag);
faces(preformatted_report_hv, Tag) -> ep_report_hv_sty:report_hv_faces(Tag);
faces(centered_report_hv, Tag)     -> ep_report_hv_sty:report_hv_faces(Tag);
faces(ragged_left_report_hv, Tag)  -> ep_report_hv_sty:report_hv_faces(Tag);

faces(cdoc, Tag)                   -> ep_cdoc_sty:cdoc_faces(Tag).


%% ***************************************************************
%% fontsize/2 - Return fontsize 
%% ***************************************************************


%% @doc Return typefaces

-spec fontsize(TypeStyle  :: tuple(),
               Tag        :: atom()) -> tuple().

fontsize(justify_report, Tag)      -> ep_report_sty:report_fontsize(Tag);
fontsize(ragged_report, Tag)       -> ep_report_sty:report_fontsize(Tag);
fontsize(preformatted_report, Tag) -> ep_report_sty:report_fontsize(Tag);
fontsize(centered_report, Tag)     -> ep_report_sty:report_fontsize(Tag);
fontsize(ragged_left_report, Tag)  -> ep_report_sty:report_fontsize(Tag);

fontsize(justify_report_hv, Tag)      -> ep_report_hv_sty:report_hv_fontsize(Tag);
fontsize(ragged_report_hv, Tag)       -> ep_report_hv_sty:report_hv_fontsize(Tag);
fontsize(preformatted_report_hv, Tag) -> ep_report_hv_sty:report_hv_fontsize(Tag);
fontsize(centered_report_hv, Tag)     -> ep_report_hv_sty:report_hv_fontsize(Tag);
fontsize(ragged_left_report_hv, Tag)  -> ep_report_hv_sty:report_hv_fontsize(Tag);

fontsize(cdoc, Tag)                   -> ep_cdoc_sty:cdoc_fontsize(Tag).

%% ***************************************************************
%% max_linespace/2 - Return return maximimum linespace for 
%%                   readability 
%% ***************************************************************


%% @doc Return max_linespace for readability 

-spec max_linespace(TypeStyle  :: tuple(),
                    Tag        :: atom()) -> tuple().

max_linespace(justify_report, Tag)      -> ep_report_sty:report_max_linespace(Tag);
max_linespace(ragged_report, Tag)       -> ep_report_sty:report_max_linespace(Tag);
max_linespace(preformatted_report, Tag) -> ep_report_sty:report_max_linespace(Tag);
max_linespace(centered_report, Tag)     -> ep_report_sty:report_max_linespace(Tag);
max_linespace(ragged_left_report, Tag)  -> ep_report_sty:report_max_linespace(Tag);

max_linespace(justify_report_hv, Tag)      -> ep_report_hv_sty:report_hv_max_linespace(Tag);
max_linespace(ragged_report_hv, Tag)       -> ep_report_hv_sty:report_hv_max_linespace(Tag);
max_linespace(preformatted_report_hv, Tag) -> ep_report_hv_sty:report_hv_max_linespace(Tag);
max_linespace(centered_report_hv, Tag)     -> ep_report_hv_sty:report_hv_max_linespace(Tag);
max_linespace(ragged_left_report_hv, Tag)  -> ep_report_hv_sty:report_hv_max_linespace(Tag);

max_linespace(cdoc, Tag)                   -> ep_cdoc_sty:cdoc_max_linespace(Tag).


%% ***************************************************************
%% leading/2 - Return leading 
%% ***************************************************************


%% @doc Return leading 

-spec leading(TypeStyle  :: tuple(),
              Tag        :: atom()) -> leading().

leading(justify_report, Tag)      -> ep_report_sty:report_leading(Tag);
leading(ragged_report, Tag)       -> ep_report_sty:report_leading(Tag);
leading(preformatted_report, Tag) -> ep_report_sty:report_leading(Tag);
leading(centered_report, Tag)     -> ep_report_sty:report_leading(Tag);
leading(ragged_left_report, Tag)  -> ep_report_sty:report_leading(Tag);

leading(justify_report_hv, Tag)      -> ep_report_hv_sty:report_hv_leading(Tag);
leading(ragged_report_hv, Tag)       -> ep_report_hv_sty:report_hv_leading(Tag);
leading(preformatted_report_hv, Tag) -> ep_report_hv_sty:report_hv_leading(Tag);
leading(centered_report_hv, Tag)     -> ep_report_hv_sty:report_hv_leading(Tag);
leading(ragged_left_report_hv, Tag)  -> ep_report_hv_sty:report_hv_leading(Tag);

leading(cdoc, Tag)                   -> ep_cdoc_sty:cdoc_leading(Tag).


%% ***************************************************************
%% justify/2 - Return justification 
%% ***************************************************************


%% @doc Return justification 

-spec justify(TypeStyle  :: tuple(),
              Tag        :: atom()) -> tuple().

justify(justify_report, Tag)      -> ep_report_sty:report_justify(Tag);
justify(ragged_report, Tag)       -> ep_report_sty:report_justify(Tag);
justify(preformatted_report, Tag) -> ep_report_sty:report_preformatted(Tag);
justify(centered_report, Tag)     -> ep_report_sty:report_centered(Tag);
justify(ragged_left_report, Tag)  -> ep_report_sty:report_ragged_left(Tag);

justify(justify_report_hv, Tag)      -> ep_report_hv_sty:report_hv_justify(Tag);
justify(ragged_report_hv, Tag)       -> ep_report_hv_sty:report_hv_justify(Tag);
justify(preformatted_report_hv, Tag) -> ep_report_hv_sty:report_hv_justify(Tag);
justify(centered_report_hv, Tag)     -> ep_report_hv_sty:report_hv_justify(Tag);
justify(ragged_left_report_hv, Tag)  -> ep_report_hv_sty:report_hv_justify(Tag);

justify(cdoc, Tag)                   -> ep_cdoc_sty:cdoc_justify(Tag).


%% ***************************************************************
%% indent/2 - Return indent 
%% ***************************************************************


%% @doc Return indent 

-spec indent(TypeStyle  :: tuple(),
             Tag        :: atom()) -> tuple().

indent(justify_report, Tag)      -> ep_report_sty:report_indent(Tag);
indent(ragged_report, Tag)       -> ep_report_sty:report_indent(Tag);
indent(preformatted_report, Tag) -> ep_report_sty:report_no_indent(Tag);
indent(centered_report, Tag)     -> ep_report_sty:report_no_indent(Tag);
indent(ragged_left_report, Tag)  -> ep_report_sty:report_no_indent(Tag);

indent(justify_report_hv, Tag)      -> ep_report_hv_sty:report_hv_indent(Tag);
indent(ragged_report_hv, Tag)       -> ep_report_hv_sty:report_hv_indent(Tag);
indent(preformatted_report_hv, Tag) -> ep_report_hv_sty:report_hv_no_indent(Tag);
indent(centered_report_hv, Tag)     -> ep_report_hv_sty:report_hv_no_indent(Tag);
indent(ragged_left_report_hv, Tag)  -> ep_report_hv_sty:report_hv_no_indent(Tag);

indent(cdoc, Tag)                   -> ep_cdoc_sty:cdoc_no_indent(Tag).


%% ***********************************************************************
%% Typeface Specifications 
%% ***********************************************************************


%% @doc Return times typefaces

-spec times(Tag      :: atom(),
            FontSize :: integer()) -> tuple().

times(ul, FontSize) ->
   {[ul],
    [tag_face(default, "Times-Roman", FontSize),
     tag_face(ul,      "Times-Roman", FontSize),
     tag_face(li,      "Times-Roman", FontSize)
    ]};

times(ol, FontSize) ->
   {[ol],
    [tag_face(default, "Times-Roman", FontSize),
     tag_face(ol,      "Times-Roman", FontSize),
     tag_face(li,      "Times-Roman", FontSize)
    ]};

times(cl, FontSize) ->
   {[cl],
    [tag_face(default, "Times-Roman", FontSize),
     tag_face(cl,      "Times-Roman", FontSize),
     tag_face(li,      "Times-Roman", FontSize)
    ]};


times(Tag, FontSize) ->
   {[Tag],
    [tag_face(default, "Times-Roman", FontSize),
     tag_face(Tag,     "Times-Roman", FontSize),
     tag_face(em,      "Times-Italic", FontSize),
     tag_face(b,       "Times-Bold", FontSize),
     tag_face(code,    "Courier", FontSize)
    ]}.

%% @doc Return helvetica typefaces

-spec helvetica(Tag      :: atom(),
                FontSize :: integer()) -> tuple().

helvetica(ul, FontSize) ->
   {[ul],
    [tag_face(default, "Helvetica", FontSize),
     tag_face(ul,      "Helvetica", FontSize),
     tag_face(li,      "Helvetica", FontSize)
    ]};

helvetica(ol, FontSize) ->
   {[ol],
    [tag_face(default, "Helvetica", FontSize),
     tag_face(ol,      "Helvetica", FontSize),
     tag_face(li,      "Helvetica", FontSize)
    ]};

helvetica(cl, FontSize) ->
   {[cl],
    [tag_face(default, "Helvetica", FontSize),
     tag_face(cl,      "Helvetica", FontSize),
     tag_face(li,      "Helvetica", FontSize)
    ]};

helvetica(Tag, FontSize) ->
   {[Tag],
    [tag_face(default, "Helvetica", FontSize),
     tag_face(Tag,     "Helvetica", FontSize),
     tag_face(em,      "Helvetica_Oblique", FontSize),
     tag_face(b,       "Helvetica-Bold", FontSize),
     tag_face(code,    "Courier", FontSize)
    ]}.

%% @doc Return courier typefaces

-spec courier(Tag      :: atom(),
              FontSize :: integer()) -> tuple().

courier(ul, FontSize) ->
   {[ul],
    [tag_face(default, "Courier", FontSize),
     tag_face(ul,      "Courier", FontSize),
     tag_face(li,      "Courier", FontSize)
    ]};

courier(ol, FontSize) ->
   {[ol],
    [tag_face(default, "Courier", FontSize),
     tag_face(ol,      "Courier", FontSize),
     tag_face(li,      "Courier", FontSize)
    ]};

courier(cl, FontSize) ->
   {[cl],
    [tag_face(default, "Courier", FontSize),
     tag_face(cl,      "Courier", FontSize),
     tag_face(li,      "Courier", FontSize)
    ]};


courier(Tag, FontSize) ->
   {[Tag],
    [tag_face(default, "Courier", FontSize),
     tag_face(Tag,     "Courier", FontSize),
     tag_face(em,      "Courier_Italic", FontSize),
     tag_face(b,       "Courier-Bold", FontSize),
     tag_face(code,    "Courier", FontSize)
    ]}.


%% @doc Default type specification

-spec default(FontSize :: integer()) -> tuple().

default(FontSize) ->
   {[h1],
    [tag_face(default, "Times-Roman", FontSize),
     tag_face(em,      "Times-Italic", FontSize),
     tag_face(code,    "Courier", FontSize),
     tag_face(b,       "Times-Bold", FontSize),
     tag_face(hb,      "Helvetica-Bold", FontSize),
     tag_face(helv,    "Helvetica", FontSize)
    ]}.

%% @doc Default helvetica type specification

-spec default_helvetica(FontSize :: integer()) -> tuple().


default_helvetica(FontSize) ->
   {[p],
    [tag_face(default, "Helvetica", FontSize),
     tag_face(em,      "Helvetica-Oblique", FontSize),
     tag_face(code,    "Courier", FontSize),
     tag_face(b,       "Helvetica-Bold", FontSize),
     tag_face(hb,      "Helvetica-Bold", FontSize),
     tag_face(helv,    "Helvetica", FontSize)
    ]}.


%% ***********************************************************************
%% Default typeface 
%% ***********************************************************************


%% @doc Return default typeface 

-spec default_face() -> tuple().

default_face() ->
   eg_pdf:default_face().

%% ***********************************************************************
%% Type specification functions
%% ***********************************************************************


%% @doc Create tagged type face

-spec tag_face(Tag :: atom(),
               Font :: string(),
               FontSize :: integer()) -> tuple().

tag_face(Tag, Font, FontSize) ->
    FaceMap = ep_face:create(Font, FontSize),
    {Tag, ep_face:make_face(FaceMap)}.




%% ***************************************************************
%% get_fontmap/2
%% ***************************************************************


%% @doc Return list of faces

-spec get_fontmap(TypeStyle :: tuple(),
                  Tag       :: atom()) -> tuple().

get_fontmap(TypeStyle, Tag) ->
   Typespecs = get_specs(TypeStyle, Tag),
   element(1, Typespecs).


%% ***************************************************************
%% get_fontmap/2 - Given typestyle and tag, return type faces
%% ***************************************************************


%% @doc Given typestyle and tag, return associated type faces

-spec list_faces(TypeStyle :: tuple(),
                 Tag       :: atom()) -> list().

list_faces(TypeStyle, Tag) ->
    Faces = get_fontmap(TypeStyle, Tag),
    element(2, Faces).


%% ***************************************************************
%% get_face/3 - Given typestyle, style tag, and face tag,
%%              return type face
%%
%% EXAMPLE:   TypeStyle = justify_report
%%            StyleTag  = p
%%            FaceTag   = em
%%
%% RETURNS:   {face,eg_font_10,12,0,{0,0,0},true}
%% ***************************************************************


%% @doc Given type style, style tag and face tag, return typeface 

-spec get_face(TypeStyle :: tuple(),
               StyleTag  :: atom(),
               FaceTag   :: atom()) -> tuple().

get_face(TypeStyle, StyleTag, FaceTag) ->
   FaceList   = list_faces(TypeStyle, StyleTag),
   TaggedFace = lists:keyfind(FaceTag, 1, FaceList),
   element(2, TaggedFace).


%% ***************************************************************
%% face_map/2 - Given typestyle and tag, return face map
%%
%% EXAMPLE:  TypeStyle = justify_report
%%           Tag       = p
%%   
%% RETURNS   #{b => {face,eg_font_11,12,0,{0,0,0},true},
%%           code => {face,eg_font_3,12,0,{0,0,0},true},
%%           default => {face,eg_font_13,12,0,{0,0,0},true},
%%           em => {face,eg_font_10,12,0,{0,0,0},true},
%%           p => {face,eg_font_13,12,0,{0,0,0},true}}
%% ***************************************************************


%% @doc Return face map 

-spec face_map(TypeSyle :: tuple(),
               Tag      :: atom()) -> map().

face_map(TypeStyle, Tag) ->
    FaceList = list_faces(TypeStyle, Tag),
    maps:from_list(FaceList).


%% ***************************************************************
%% get_font/2 - Given type style and tag, return font 
%% ***************************************************************


%% @doc Return erlPress font 

-spec get_font(TypeStyle  :: tuple(),
               StyleTag   :: atom(),
               FaceTag    :: atom())-> atom().

get_font(TypeStyle, StyleTag, FaceTag) ->
    Face = get_face(TypeStyle, StyleTag, FaceTag),
    element(2, Face).







