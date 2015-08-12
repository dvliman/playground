-module(link_handler_SUITE).

-export([all/0]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([create_without_merchant/1, create/1, delete/1]).

all() -> test_utils:all(?MODULE).
init_per_testcase(Test, Config) -> test_utils:init_per_testcase(Test, Config).
end_per_testcase(Test, Config) -> test_utils:end_per_testcase(Test, Config).

create(Config) ->
    ok.

create_without_merchant(Config) ->
    ok.

delete(Config) ->
    Link = proplists:get_value(link1, Config),
    BinLink = binary_to_list(Link),

    _ = link:fetch(Link),                                % link exists
    {ok, <<"2">>} = redis:q(["HGET", "stats", "links"]), % test_utils creates 2

    ok = link:delete(Link),
    error = link:fetch(Link),
    {ok, <<"1">>} = redis:q(["HGET", "stats", "links"]), % 1 left
    {ok, Links} = redis:q(["LRANGE", "links", 0, -1]),
    false = lists:member(BinLink, Links).                % no longer in global lists
