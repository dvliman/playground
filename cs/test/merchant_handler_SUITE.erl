-module(merchant_handler_SUITE).

-export([all/0]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([create/1, bad_data/1]).

all() -> test_utils:all(?MODULE).
init_per_testcase(Test, Config) -> test_utils:init_per_testcase(Test, Config).
end_per_testcase(Test, Config) -> test_utils:end_per_testcase(Test, Config).

create(Config) ->
    {Url, CTJson} = proplists:get_value(init, Config),
    Endpoint = Url ++ "/merchants",

    Props = [{name, <<"walmart">>}, {category, <<"services">>},
             {permalink, <<"walmart">>}, {url, <<"http://walmart.com">>}],

    % can create new merchant
    Payload = jiffy:encode({Props}),
    test_utils:send_req(Endpoint, CTJson, post, Payload, [], "200"),

    % fail if permalink already exists
    test_utils:send_req(Endpoint, CTJson, post, Payload, [], "400"),
    test_utils:send_req(Endpoint, CTJson, post, Payload, [], "400"),

    % fail if category is invalid
    Props1 = lists:keydelete(category, 1, Props) ++ [{category, <<"bad">>}],
    Payload1 = jiffy:encode({Props1}),
    test_utils:send_req(Endpoint, CTJson, post, Payload1, [], "400").

bad_data(_Config) ->
    ok.
