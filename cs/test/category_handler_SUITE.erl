-module(category_handler_SUITE).
-include("../src/cs.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([read_categories/1]).

all() -> test_utils:all(?MODULE).
init_per_testcase(Test, Config) -> test_utils:init_per_testcase(Test, Config).
end_per_testcase(Test, Config) -> test_utils:end_per_testcase(Test, Config).

read_categories(Config) ->
    {Url, _} = proplists:get_value(init, Config),
    Endpoint = Url ++ "/categories",

    Response = test_utils:send_req(Endpoint, [], get, [], [], "200", [], <<"data">>),
    length(Response) =:= length(?CATEGORIES).
