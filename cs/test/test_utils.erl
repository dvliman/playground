-module(test_utils).

-export([all/1, test_all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([test/1, test/2]).
-export([send_req/6, send_req/8]).
-export([create_merchant/0, create_link/1]).

-type config() :: [{atom(), term()}].
-export_type([config/0]).

shuffle_list(L) ->
    random:seed(erlang:now()),
    lists:sort(fun(_, _) -> random:uniform() > 0.5 end, L).

% hardcode all tests for now
test_all() ->
    [{Test, test(Test)} || Test <- [
        category_handler_SUITE,
        merchant_handler_SUITE,
        link_handler_SUITE
    ]].

test(Mod) ->
    test(Mod, shuffle_list(Mod:all())).

test(Mod, Tests) ->
    test_suite(Mod, Tests).

test_suite(Mod, Tests) ->
   Config = case lists:member({init_per_suite, 1}, Mod:module_info(exports)) of
               true -> Mod:init_per_suite([]);
               false -> []
            end,

    try
        Results = [test_single(Mod, Test, Config) || Test <- Tests],
        case lists:usort([R || {_T, R} <- Results]) of
           [ok] -> {all_tests_ok, length(Results)};
            _ -> Results
        end
    after
        case lists:member({end_per_suite, 1}, Mod:module_info(exports)) of
            true -> Mod:end_per_suite(Config);
            false -> ok
        end
    end.

test_single(Mod, Test, Config) ->
    Config1 = case lists:member({init_per_testcase, 2}, Mod:module_info(exports)) of
                  true -> Mod:init_per_testcase(Test, Config);
                  false -> Config
              end,

    try Mod:Test(Config1) of
        _ -> {Test, ok}
    catch
       Kind:Error ->
           Stack = erlang:get_stacktrace(),
           Message = io_lib:format("TEST ERROR ~p: ~p~n\t~p~n\t~p ~n~n", [Kind, Test, Error, Stack]),
           lager:log(error, self(), Message),
           {Test, {error, Error}}
    after
        case lists:member({end_per_testcase, 2}, Mod:module_info(exports)) of
            true -> Mod:end_per_testcase(Test, Config1);
            false -> ok
        end
    end.

send_req(Url, Headers, Method, Body, Options, Status) ->
    send_req(Url, Headers, Method, Body, Options, Status, [], headers).

send_req(Url, Headers, Method, Body, Options, Status, HeadersCheck, headers) ->
    {_RStatus, RHeaders, _RBody} =
        make_request(Url, Headers, Method, Body, Options, Status, HeadersCheck),

    [check_header(Header, Condition, RHeaders) || {Header, Condition} <- HeadersCheck],
        [proplists:get_value(Header, RHeaders) || {Header, return} <- HeadersCheck];

send_req(Url, Headers, Method, Body, Options, Status, HeadersCheck, all) ->
    {_RStatus, RHeaders, RBody} =
        make_request(Url, Headers, Method, Body, Options, Status, HeadersCheck),

    [check_header(Header, Condition, RHeaders) || {Header, Condition} <- HeadersCheck],
    RBody;

send_req(Url, Headers, Method, Body, Options, Status, HeadersCheck, Reply) ->
    {_RStatus, RHeaders, RBody} =
        make_request(Url, Headers, Method, Body, Options, Status, HeadersCheck),

    [check_header(Header, Condition, RHeaders) || {Header, Condition} <- HeadersCheck],
    {Json} = json_decode(RBody),
    proplists:get_value(Reply, Json).

make_request(Url, Headers, Method, Body, Options, Status, HeadersCheck) ->
    {ok, RStatus, RHeaders, RBody} =
        ibrowse:send_req(Url, Headers, Method, Body, Options, 60000),

     case RStatus of
        Status -> ok;
        RStatus ->
            lager:alert("~n\tReq: ~s ~s~n\t\t~p~n\t\t~p~n\t\t~s~n"
                        "\tExpected: ~s ~p~n"
                        "\tGot: ~s~n~s~n\t\t~p~n",
                        [Method, Url, Headers, Options, iolist_to_binary(Body),
                        Status, HeadersCheck, RStatus, iolist_to_binary(RBody), RHeaders]),
            Status = RStatus
    end,
    {RStatus, RHeaders, RBody}.

check_header(_Header, return, _Hs) -> ok;
check_header(Header, {starts_with, Text}, Hs) ->
    {Header, Text, Hs, {match, [{0, _}]}} =
        {Header, Text, Hs, re:run(proplists:get_value(Header, Hs, ""), Text)};
check_header(Header, Text, Hs) ->
    match = case proplists:get_value(Header, Hs) of
                Text      -> match;
                Non_Match -> {{header, Header}, {expected, Text}, {currently_has, Non_Match}, {all_headers, Hs}}
            end.

json_decode(Bin) ->
    try jiffy:decode(Bin)
    catch
        _:{error, {Char, invalid_json}} ->
            {bad_json, {[{invalid_json, Bin}, {character, Char}]}};
        _:{error, {Char, truncated_json}} ->
            {bad_json, {[{truncated_json, Bin}, {character, Char}]}};
        _:{error, {Char, invalid_trailing_data}} ->
            {bad_json, {[{invalid_trailing_data, Bin}, {character, Char}]}};
        _:Error ->
            {bad_json, iolist_to_binary(io_lib:format("~p", Error))}
    end.

all(Module) ->
    [Fun || {Fun, 1} <- Module:module_info(exports),
        not lists:member(Fun, [init_per_suite, end_per_suite, init_per_testcase, end_per_testcase, module_info])].

init_per_suite(Config) -> ensure_cs_started(), Config.
end_per_suite(Config) -> Config.

init_per_testcase(_Testcase, Config) ->
    redis:q(["FLUSHALL"]),

    {Url, CTJson} =  {"http://localhost:8080/api/v1",
                    [{"Content-Type", "application/json"}]},
    [{init, {Url, CTJson}}] ++ bootstrap() ++ Config.

end_per_testcase(_Testcase, Config) -> Config.

ensure_cs_started() ->
    case cs:start() of
        ok -> ok;
        _ -> cs:start()
    end.

bootstrap() ->
    % create 2 merchants
    Merchant1 = create_merchant(),
    Merchant2 = create_merchant(),

    Merchant1Id = proplists:get_value(<<"id">>, Merchant1),
    Merchant2Id = proplists:get_value(<<"id">>, Merchant2),

    % create 2 links, associated to that merchant
    Link1 = create_link(Merchant1Id),
    Link2 = create_link(Merchant2Id),

    Link1Id = proplists:get_value(<<"id">>, Link1),
    Link2Id = proplists:get_value(<<"id">>, Link2),

    [{merchant1, Merchant1Id}, {merchant2, Merchant2Id},
     {link1, Link1Id}, {link2, Link2Id}].

create_merchant() ->
    Props = [{Key, utils:uuid()} || Key <- [<<"name">>, <<"permalink">>,
                <<"url">>, <<"image">>, <<"description">>]],
    Props1 = Props ++ [{<<"category">>, <<"entertainment">>}],
    merchant:create(Props1).

create_link(MerchantId) ->
    Props = [{Key, utils:uuid()} || Key <- [<<"name">>, <<"permalink">>,
                <<"url">>, <<"image">>, <<"description">>]],

    Type     = [{<<"type">>, <<"coupon">>}],
    Merchant = [{<<"merchant_id">>, MerchantId}],
    Category = [{<<"category">>, <<"entertainment">>}],
    Props1 = Props ++ Type ++ Merchant ++ Category,

    link:create(Props1).
