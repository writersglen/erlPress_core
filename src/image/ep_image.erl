%%% ==========================================================================
%%% ep_image.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:      MIT
%%%   File:         ep_image.erl
%%%   Description:  Manage, scale, and render images 
%%% @end

%%% ==========================================================================


-module (ep_image).

-export ([
    create/3,
    get_image/1,
    image/3,
    image_size/1,
    position/1,
    update_image_size/2,
    update_position/2
]).
 
-define(FORMAT, letter).

 
%% ***************************************************************
%% Create image map 
%% ***************************************************************


%% @doc Create image map
%%      Position is {X,Y}; lower-left corner of image 
%%      Size is {width, W} | {height, H} | {W,H} | {max, W, H} 
%%      The max Size version can be used to set a max limit on width, 
%%      height or both dimensions (undefined is a valid value for at 
%%      most 1 W or H value)

-spec create( FilePath :: list()
            , Position :: tuple()
            , Size     :: tuple()) -> map().

create(ImageFileName, Position, Size) ->
   #{ image_file_name  => ImageFileName 
    , position         => Position    %% NOTE: bottom left xy of image 
    , size             => Size 
    }.


%% ***************************************************************
%% Image to pdf 
%% ***************************************************************

%% @doc Display image

-spec image(PDF      :: identifier(),
            Job      :: map(),
            ImageMap :: map()) -> ok. 

image(PDF, Job, ImageMap) ->
    ImagePosition             = ep_job:flip_y(Job, ImageMap),
    ResourcePath              = ep_job:resource_path(Job),
    ImageFileName             = maps:get(image_file_name, ImageMap),
    FilePath                  = ResourcePath ++ "/images/" ++ ImageFileName,
    Size                      = maps:get(size, ImageMap),
    eg_pdf:image(PDF, FilePath, ImagePosition, Size),
    ok.
    


%% ***************************************************************
%% Get image attributes 
%% ***************************************************************

%% @doc Return position 

-spec position(ImageMap :: map()) -> tuple().

position(ImageMap) ->
    maps:get(position, ImageMap).


%% @doc Return image size 

-spec image_size(ImageMap :: map()) -> tuple().

image_size(ImageMap) ->
    maps:get(size, ImageMap).


%% ***************************************************************
%% Update image attributes 
%% ***************************************************************



%% @doc Update image position 

-spec update_position( Position :: tuple()
                     , ImageMap :: map()) -> tuple().

update_position(Position, ImageMap) ->
    maps:put(position, Position, ImageMap).


%% @doc Update image size 

-spec update_image_size( ImageSize :: tuple(),
                         ImageMap :: map()) -> tuple().

update_image_size(ImageSize, ImageMap) ->
    maps:put(size, ImageSize, ImageMap).


%% ***************************************************************
%% get_image/1 
%% ***************************************************************


%% @doc Return image 

-spec get_image( ImageFilePath :: string()) -> binary(). 

get_image(ImageFilePath) ->
   {ok, Image} = file:read_file(ImageFilePath),
   Image.
