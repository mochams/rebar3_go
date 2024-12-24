-module(rebar3_go_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [success].

groups() ->
    [].

init_per_testcase(_Name, Config) ->
    Config.

end_per_testcase(_Name, _Config) ->
    ok.

success(_Config) ->
    ?assert(true).
