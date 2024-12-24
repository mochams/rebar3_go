-module(rebar3_go).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_go_compile_prv:init(State),
    {ok, State1}.
