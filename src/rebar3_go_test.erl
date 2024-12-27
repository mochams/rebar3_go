-module(rebar3_go_test).

-export([run/1]).

-define(GO_SRC, "go_src").
-define(GO_MOD, "go.mod").

run(App) ->
    AppDir = rebar_app_info:dir(App),
    GoDir = rebar3_go_utils:path(AppDir, ?GO_SRC),

    case rebar3_go_utils:check_go_installation() of
        ok ->
            test_files(AppDir, GoDir);
        {error, Reason} ->
            rebar_api:abort("Go installation check failed: ~p", [Reason])
    end.

test_files(AppDir, GoDir) ->
    rebar_api:info("Go source directory: ~s", [GoDir]),

    case rebar3_go_utils:is_dir(GoDir) of
        ok ->
            Modules = rebar3_go_utils:get_go_modules(GoDir),
            test_files(AppDir, GoDir, Modules);
        not_a_directory ->
            rebar_api:warn("No go_src directory found in ~s", [AppDir])
    end.

test_files(AppDir, GoDir, [Dir | Rest]) ->
    ModuleDir = rebar3_go_utils:path(GoDir, Dir),
    case rebar3_go_utils:path_has_file(ModuleDir, ?GO_MOD) of
        true ->
            run_tests(ModuleDir),
            test_files(AppDir, GoDir, Rest);
        false ->
            rebar_api:warn("No go.mod found in ~s, skipping", [ModuleDir])
    end;
test_files(_AppDir, _GoDir, []) ->
    ok.

run_tests(ModuleDir) ->
    rebar_api:info("Testing module: ~s", [ModuleDir]),

    BuildCmd = "go test ./... -cover",

    rebar3_go_utils:run_sh(BuildCmd, [
        {cd, ModuleDir},
        {abort_on_error, true},
        use_stdout
    ]).
