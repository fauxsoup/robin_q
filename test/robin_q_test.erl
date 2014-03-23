-module(robin_q_test).
-define(TEST_SET_SIZE, 64).
-define(CLIENTS, 4 * erlang:system_info(schedulers)).
-define(CRAZIES, 10).
-define(REQUESTS_PER_CLIENT, 1000).
-define(SET_TIME_MIN, 10).
-define(SET_TIME_MAX, 50).
-include_lib("eunit/include/eunit.hrl").

standard_test_() ->
    [
        {setup, fun setup_standard/0, fun instantiate_standard/1}
    ].

setup_standard() ->
    {ok, RQ} = robin_q:new(lists:seq(1, ?TEST_SET_SIZE)),
    RQ.

instantiate_standard(RQ) ->
    {inorder, [
            ?_assertError(badarg, robin_q:new(1.0)),
            ?_assertError(badarg, robin_q:new(1)),
            ?_assertError(badarg, robin_q:new(test)),
            ?_assertError(badarg, robin_q:new(<<"test">>)),
            ?_assertMatch({ok, _}, robin_q:new({a, b, c, d})),
            ?_assertMatch({ok, _}, robin_q:new([a, b, c, d])),
            %% Test the round part of round-robin
            {inorder, [ ?_assertMatch({ok, N}, robin_q:next(RQ)) || N <- lists:seq(1, ?TEST_SET_SIZE) ] ++ [ ?_assertMatch({ok, 1}, robin_q:next(RQ)) ]},
            %% Test paralell access
            {inparallel, [ {spawn, [ ?_assertMatch({ok, N} when is_integer(N), robin_q:next(RQ)) || _ <- lists:seq(1, ?REQUESTS_PER_CLIENT) ]} || _ <- lists:seq(1, ?CLIENTS) ]},
            ?_assertError(badarg, robin_q:set(RQ, 1.0)),
            ?_assertError(badarg, robin_q:set(RQ, 1)),
            ?_assertError(badarg, robin_q:set(RQ, test)),
            ?_assertError(badarg, robin_q:set(RQ, <<"test">>)),
            ?_assertMatch(ok, robin_q:set(RQ, lists:seq(1, ?TEST_SET_SIZE))),
            ?_assertMatch({ok, 1}, robin_q:next(RQ))
        ]}.
