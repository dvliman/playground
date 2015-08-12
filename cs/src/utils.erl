-module(utils).

-export([uuid/0, buuid/0]).
-export([now/0]).
-export([trace/1]).
-export([redis_exists/1]).
-export([proplists_to_list/1, list_to_proplists/1]).
-export([get_config/1, get_config/2]).

uuid()  -> uuid:to_string(uuid:uuid4()).
buuid() -> list_to_binary(uuid()).

get_config(Key) ->
    get_config(Key, undefined).

get_config(Key, Default) ->
    case application:get_env(cs, Key) of
        {ok, Value} -> Value;
        undefined -> Default
    end.

trace(Input) -> Input.

redis_exists(Key) ->
    case redis:q(["EXISTS", Key]) of
        {error, _} -> false;
        {ok, <<"0">>} -> false;
        {ok, <<"1">>} -> true
    end.

proplists_to_list(Props) -> proplists_to_list(Props, []).
proplists_to_list([], Acc) -> Acc;
proplists_to_list([Elem | Tails], Acc)->
    Res = tuple_to_list(Elem),
    proplists_to_list(Tails, Acc ++ Res).

list_to_proplists(XS) -> list_to_proplists(XS, []).
list_to_proplists([], Acc) -> Acc;
list_to_proplists([K,V|T], Acc) ->
    list_to_proplists(T, [{K, V}|Acc]).

now() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:universaltime(),
    Timestamp = lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~.10.0BZ",
                    [Year, Month, Day, Hour, Minute, Second, 0])),
    list_to_binary(Timestamp).
