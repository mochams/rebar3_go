-module(rebar3_go_utils).

-export([
    check_go_installation/0,
    is_dir/1,
    path/2,
    path_is_dir/2,
    path_dirs/1,
    get_go_modules/1,
    path_has_file/2
]).

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

path(BaseDir, Path) ->
    filename:join(BaseDir, Path).

path_is_dir(BaseDir, Path) ->
    filelib:is_dir(path(BaseDir, Path)).

path_dirs(Path) ->
    file:list_dir(Path).

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
