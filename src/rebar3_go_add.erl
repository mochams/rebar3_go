-module(rebar3_go_add).

-export([run/2]).

-define(SINGLE_APP, 1).
-define(NO_APPS, 0).
-define(GO_SRC, "go_src").
-define(GO_ENTRY, "main.go").
-define(GO_CONTENT, "package main\n\nfunc main() {\n\tprintln(\"Hello, World!\")\n}\n").

run(Apps, State) ->
    case length(Apps) of
        ?NO_APPS -> rebar_api:abort("No apps found in the project", [?NO_APPS]);
        ?SINGLE_APP -> match_handler(Apps, State);
        _ -> umbrella_app_handler(Apps, State)
    end.

match_handler(Apps, State) ->
    App = lists:nth(1, Apps),
    case rebar3_go_utils:is_umbrella_project(rebar_app_info:dir(App)) of
        true -> umbrella_app_handler(Apps, State);
        false -> standalone_app_handler(App, State)
    end.

standalone_app_handler(App, State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    add_module(
        App,
        rebar3_go_utils:get_arg_value(Args, module_name)
    ).

umbrella_app_handler(Apps, State) ->
    {Args, _} = rebar_state:command_parsed_args(State),

    AppList = [
        {binary_to_list(rebar_app_info:name(App)), App}
     || App <- Apps
    ],

    case
        rebar3_go_utils:find_app(
            AppList,
            rebar3_go_utils:get_arg_value(Args, app_name)
        )
    of
        {ok, App} -> add_module(App, rebar3_go_utils:get_arg_value(Args, module_name));
        {error, Reason} -> rebar_api:abort("App not found: ~p", [Reason])
    end.

add_module(App, ModuleName) ->
    AppDir = rebar_app_info:dir(App),
    create_go_workspace(AppDir),
    create_module(AppDir, ModuleName),
    sync_workspace(AppDir, ModuleName).

create_go_workspace(AppDir) ->
    case rebar3_go_utils:is_file(rebar3_go_utils:path(AppDir, ?GO_SRC)) of
        ok -> ok;
        not_a_file -> initialize_workspace(AppDir)
    end.

initialize_workspace(AppDir) ->
    rebar3_go_utils:run_sh("go work init", [
        {cd, AppDir},
        {return_on_error, true},
        use_stdout
    ]).

create_module(AppDir, ModuleName) ->
    GoDir = rebar3_go_utils:path(AppDir, ?GO_SRC),
    ModuleDir = rebar3_go_utils:path(GoDir, ModuleName),
    rebar3_go_utils:ensure_path(ModuleDir),

    BuildCmd = io_lib:format(
        "go mod init example.com/~s", [ModuleName]
    ),

    rebar3_go_utils:run_sh(BuildCmd, [
        {cd, ModuleDir},
        {return_on_error, true},
        use_stdout
    ]),

    rebar3_go_utils:write_file(
        rebar3_go_utils:path(ModuleDir, ?GO_ENTRY),
        ?GO_CONTENT
    ).

sync_workspace(AppDir, ModuleName) ->
    BuildCmd = io_lib:format(
        "go work use ~s/~s", [?GO_SRC, ModuleName]
    ),

    rebar3_go_utils:run_sh(BuildCmd, [
        {cd, AppDir},
        {return_on_error, true},
        use_stdout
    ]).
