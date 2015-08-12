-module(link).
-include("cs.hrl").

-export([fetch/1, create/1, delete/1]).
-export([validate_on_create/1]).

fetch(Id) when is_binary(Id) -> fetch(binary_to_list(Id));
fetch(Id) ->
    BinId = list_to_binary(Id),
    Response = redis:q(["HGETALL", ["link:", BinId]]),

    case Response of
        {error, Reason} ->
            lager:notice("fetching link:~p, reason:~p", [Id, Reason]),
            error;
        {ok, []} ->
            lager:notice("fetching deleted link:~p", [Id]),
            error;
        {ok, Data} ->
            utils:list_to_proplists(Data ++ [<<"id">>, BinId])
    end.

create(Props) ->
    Id = utils:uuid(),
    ModifiedTime = [{<<"mtime">>, utils:now()}],

    MerchantId = proplists:get_value(<<"merchant_id">>, Props),
    Type       = proplists:get_value(<<"type">>, Props),
    Category   = proplists:get_value(<<"category">>, Props),
    Permalink  = case proplists:get_value(<<"permalink">>, Props) of
                    undefined -> generate_random_binary();
                    Value -> iolist_to_binary([Value, "-", generate_random_binary()])
                 end,

    Props1 = lists:keyreplace(<<"permalink">>, 1, Props, {<<"permalink">>, Permalink}),
    Fields = utils:proplists_to_list(Props1 ++ ModifiedTime),

    Writes = [
        ["HMSET", ["link:", Id] | Fields],
        ["LPUSH", ["links"], Id],
        ["LPUSH", ["type-links:", Type], Id],
        ["LPUSH", ["merchant-links:", MerchantId], Id],
        ["LPUSH", ["category-links:", Category], Id],
        ["HINCRBY", "stats", "links", 1],
        ["HINCRBY", "stats", ["category-links:", Category], 1]],

    redis:qp(Writes),
    fetch(Id).

delete(Id) ->
    case fetch(Id) of
        {error, _} ->
            lager:info("trying to delete non-existing link:~p", [Id]),
            ok;
        Link ->
            Type = proplists:get_value(<<"type">>, Link),
            Category = proplists:get_value(<<"category">>, Link),
            MerchantId = proplists:get_value(<<"merchant_id">>, Link),

            Writes = [
                ["DEL", ["link:", Id]],
                ["LREM", "links", 1, Id],
                ["LREM", ["type-links:", Type], 1, Id],
                ["LREM", ["merchant-links:", MerchantId], 1, Id],
                ["LREM", ["category-links:", Category], 1, Id],
                ["HINCRBY", "stats", "links", -1],
                ["HINCRBY", "stats", ["category-links:", Category], -1]],

            redis:qp(Writes),
            ok
    end.

validate_on_create(Props) ->
    Spec = [v:required(<<"merchant_id">>,
                       fun(MerchantId) ->
                           case merchant:fetch(MerchantId) of
                               {error, _} -> false;
                               _ -> true
                           end
                       end,
                       merchant_missing),
            v:required(<<"name">>),
            v:required(<<"category">>,
                      fun(Category) -> lists:member(binary_to_list(Category), ?CATEGORIES) end,
                      invalid_category),
            v:required(<<"type">>,
                      fun(Type) -> lists:member(binary_to_list(Type), ?TYPES) end,
                      invalid_type),
            v:optional(<<"permalink">>),
            v:required(<<"url">>),
            v:optional(<<"image">>),
            v:optional(<<"description">>)],
    v:validate(Props, Spec).

generate_random_binary() ->
    base64url:encode(crypto:strong_rand_bytes(10)).
