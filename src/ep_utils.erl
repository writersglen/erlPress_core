-module(ep_utils).

%%====================================================================
%% EXPORTS
%%====================================================================
-export([ ceil/1
        , floor/1
        , ceiling/1
        , priv_dir/1 ]).

%%--------------------------------------------------------------------
%% @doc Flooring (rounding) floating point number.
%% @end
%% See: http://erlang.org/pipermail/erlang-questions/2009-March/042371.html
%% @private
-spec floor(float()) -> integer().
%%--------------------------------------------------------------------
floor(X) when is_float(X) ->
     T = trunc(X),
     if X < T -> T - 1;
        true  -> T
     end;
floor(X) when is_integer(X) ->
    X.

%%--------------------------------------------------------------------
%% @doc Ceiling (rounding) floating point number.
%% @end
%% See: http://erlang.org/pipermail/erlang-questions/2009-March/042371.html
%% @private
-spec ceil(float()) -> integer().
%%--------------------------------------------------------------------
ceil(X) when is_float(X) ->
     T = trunc(X),
     if X > T -> T + 1;
        true  -> T
     end;
ceil(X) when is_integer(X) ->
    X.

%% From Carl Wright version of erlguten: eg_pdf_image.erl

ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

%%--------------------------------------------------------------------
%% @doc Returns the module's "priv" directory.
%% @end
%% @private
-spec priv_dir(atom()) -> string().
%%--------------------------------------------------------------------
priv_dir(Mod) ->
    case code:priv_dir(Mod) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(Mod)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end.
