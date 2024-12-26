-module(rebar3_go_fmt).

-export([run/1]).

-define(GO_SRC, "go_src").
-define(GO_MOD, "go.mod").

run(App) ->
    AppDir = rebar_app_info:dir(App),
    GoDir = rebar3_go_utils:path(AppDir, ?GO_SRC),

    case rebar3_go_utils:check_go_installation() of
        ok ->
            format_files(AppDir, GoDir);
        {error, Reason} ->
            rebar_api:abort("Go installation check failed: ~p", [Reason])
    end.

format_files(AppDir, GoDir) ->
    rebar_api:info("Checking Go source directory: ~s", [GoDir]),

    case rebar3_go_utils:is_dir(GoDir) of
        ok ->
            Modules = rebar3_go_utils:get_go_modules(GoDir),
            format_files(AppDir, GoDir, Modules);
        not_a_directory ->
            rebar_api:warn("Go source directory not found: ~s", [GoDir])
    end.

format_files(AppDir, GoDir, [Dir | Rest]) ->
    ModuleDir = rebar3_go_utils:path(GoDir, Dir),
    case rebar3_go_utils:path_has_file(ModuleDir, ?GO_MOD) of
        true ->
            run_format(ModuleDir),
            format_files(AppDir, GoDir, Rest);
        false ->
            rebar_api:warn("No go.mod found in ~s, skipping", [ModuleDir])
    end;
format_files(_AppDir, GoDir, []) ->
    rebar_api:info("Finished formatting go modules  in ~s", [GoDir]),
    ok.

run_format(ModuleDir) ->
    BuildCmd = "go fmt",

    rebar3_go_utils:run_sh(BuildCmd, [
        {cd, ModuleDir},
        {return_on_error, true},
        use_stdout
    ]).
