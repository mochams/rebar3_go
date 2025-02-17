-module(rebar3_go_compile).

-export([run/1]).

-define(PRIV_GO, "priv/go/").
-define(GO_SRC, "go_src").
-define(GO_MOD, "go.mod").

run(App) ->
    AppDir = rebar_app_info:dir(App),
    GoDir = rebar3_go_utils:path(AppDir, ?GO_SRC),

    case rebar3_go_utils:check_go_installation() of
        ok ->
            compile_modules(AppDir, GoDir);
        {error, Reason} ->
            rebar_api:abort("Go installation check failed: ~p", [Reason])
    end.

compile_modules(AppDir, GoDir) ->
    rebar_api:info("Go source directory: ~s", [GoDir]),

    case rebar3_go_utils:is_dir(GoDir) of
        ok ->
            Modules = rebar3_go_utils:get_go_modules(GoDir),
            compile_modules(AppDir, GoDir, Modules);
        not_a_directory ->
            rebar_api:warn("No go_src directory found in ~s", [AppDir])
    end.

compile_modules(AppDir, GoDir, [Dir | Rest]) ->
    ModPath = rebar3_go_utils:path(GoDir, Dir),
    case rebar3_go_utils:path_has_file(ModPath, ?GO_MOD) of
        true ->
            run_compile(AppDir, GoDir, Dir),
            compile_modules(AppDir, GoDir, Rest);
        false ->
            rebar_api:warn("No go.mod found in ~s, skipping", [ModPath])
    end;
compile_modules(_AppDir, _GoDir, []) ->
    ok.

run_compile(AppDir, GoDir, Dir) ->
    ModPath = rebar3_go_utils:path(GoDir, Dir),
    PrivGoDir = rebar3_go_utils:path(AppDir, ?PRIV_GO),

    rebar_api:info("Building module: ~s", [ModPath]),

    BuildCmd = io_lib:format(
        "go build -o ~s ~s",
        [
            rebar3_go_utils:path(PrivGoDir, Dir),
            ModPath
        ]
    ),

    rebar3_go_utils:run_sh(BuildCmd, [
        {cd, AppDir},
        {return_on_error, true},
        use_stdout
    ]).
