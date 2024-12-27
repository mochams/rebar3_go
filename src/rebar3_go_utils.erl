-module(rebar3_go_utils).

-export([
    check_go_installation/0,
    is_dir/1,
    is_file/1,
    path/2,
    path_is_dir/2,
    path_dirs/1,
    get_go_modules/1,
    path_has_file/2,
    get_arg_value/2,
    run_sh/2,
    ensure_path/1,
    write_file/2,
    find_app/2,
    is_umbrella_project/1
]).

-define(APPS_DIR, "apps").

check_go_installation() ->
    case rebar_utils:sh("go version", [{return_on_error, true}]) of
        {ok, _} -> ok;
        {error, _} = Error -> Error
    end.

is_dir(Dir) ->
    case filelib:is_dir(Dir) of
        true -> ok;
        false -> not_a_directory
    end.

is_file(File) ->
    case filelib:is_file(File) of
        true -> ok;
        false -> not_a_file
    end.

path(BaseDir, Path) ->
    filename:join(BaseDir, Path).

path_is_dir(BaseDir, Path) ->
    filelib:is_dir(path(BaseDir, Path)).

path_dirs(Path) ->
    file:list_dir(Path).

ensure_path(Path) ->
    filelib:ensure_path(Path).

get_go_modules(GoDir) ->
    case rebar3_go_utils:path_dirs(GoDir) of
        {ok, Files} ->
            Dirs = [
                Dir
             || Dir <- Files,
                rebar3_go_utils:path_is_dir(GoDir, Dir)
            ],
            Dirs;
        {error, Reason} ->
            rebar_api:abort("Failed to list Go source directory: ~p", [Reason])
    end.

path_has_file(Path, Filename) ->
    filelib:is_file(path(Path, Filename)).

get_arg_value(Args, ArgName) ->
    ArgValue = proplists:get_value(ArgName, Args),
    case ArgValue of
        undefined -> rebar_api:abort("~s was not provided: ~p", [ArgName, ArgValue]);
        _ -> ArgValue
    end.

run_sh(BuildCmd, Opts) ->
    case rebar_utils:sh(BuildCmd, Opts) of
        {ok, _Output} ->
            ok;
        {error, {_Code, Output}} ->
            rebar_api:error("Action failed:~n~s", [Output]),
            {error, {build_failed, Output}}
    end.

write_file(Path, Content) ->
    case file:write_file(Path, Content) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.

find_app(AppList, AppName) ->
    case lists:keyfind(AppName, 1, AppList) of
        false -> {error, AppName};
        {AppName, App} -> {ok, App}
    end.

is_umbrella_project(AppDir) ->
    BaseName = filename:basename(filename:dirname(AppDir)),
    case BaseName of
        ?APPS_DIR -> true;
        _ -> false
    end.
