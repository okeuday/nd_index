-module(nd_index_SUITE).

%% CT callbacks
-export([all/0,
         groups/0,
         suite/0,
         init_per_suite/1,
         end_per_suite/1,
         group/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% test cases
-export([t_count0/1]).

all() ->
    [{group, count}].

groups() ->
    [{count, [],
      [t_count0]}].

suite() ->
    [{ct_hooks, [cth_surefire]},
     {timetrap, 5100}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

group(_GroupName) ->
    [].

init_per_group(_Group, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

t_count0(_Config) ->
    State = nd_index:new(erlang:make_tuple(2, 0), erlang:make_tuple(2, 15)),
    255 = nd_index:count(State),
    false = nd_index:empty(nd_index:increment(254, State)),
    true = nd_index:empty(nd_index:increment(255, State)),
    false = nd_index:increment(256, State),
    254 = nd_index:count(nd_index:increment(1, State)),
    237 = nd_index:count(nd_index:increment(18, State)),
    ok.

