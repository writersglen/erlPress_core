%%% *********************************************************
%%% ep_admin.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:      MIT
%%%   File:         ep_admin.erl
%%%   Description:  administration functions 
%%% @end

%%% ==========================================================================


-module(ep_admin).

-export([config_resource_root/1, get_resource_root/0]).


config_resource_root(RootDir) ->
    {ok, _} = dets:open_file(resources, [{type, set}]),
    Response = dets:insert_new(resources, {root, RootDir}),
    dets:close(resources),
    case Response of
        true -> file:make_dir(RootDir);
        false -> resource_root_file_exists
    end.


get_resource_root() ->
    {ok, Ref} = dets:open_file("./resources"),
    Response = dets:lookup(Ref, root),
    dets:close(Ref),
    element(2, hd(Response)).
