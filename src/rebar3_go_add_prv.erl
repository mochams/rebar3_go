-module(rebar3_go_add_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, add).
-define(NAMESPACE, go).
-define(DEPS, [{default, app_discovery}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        % The 'user friendly' name of the task
        {name, ?PROVIDER},
        % The namespace of the task
        {namespace, ?NAMESPACE},
        % The module implementation of the task
        {module, ?MODULE},
        % The task can be run by the user, always true
        {bare, true},
        % The list of dependencies
        {deps, ?DEPS},
        % How to use the plugin
        {example, "rebar3 go add -m <module_name>"},
        % list of options understood by the plugin
        {opts, [
            {module_name, $m, "module", string, "The name of the module to add"},
            {app_name, $a, "app", string, "The name of the app to add the module to"}
        ]},
        {short_desc, "Add Go modules"},
        {desc, "Add Go modules"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = rebar_state:project_apps(State),
    rebar3_go_add:run(Apps, State),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
