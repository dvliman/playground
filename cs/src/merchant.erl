-module(merchant).
-include("cs.hrl").

-export([fetch/1, create/1, update/2, delete/1]).
-export([validate_on_create/1, validate_on_update/1]).

fetch(Id) when is_binary(Id) -> fetch(binary_to_list(Id));
fetch(Id) ->
    BinId = list_to_binary(Id),
    Response = redis:q(["HGETALL", ["merchant:", BinId]]),

    case Response of
        {error, Reason} ->
            lager:notice("fetching merchant:~p, reason:~p", [Id, Reason]),
            error;
        {ok, []} ->
            lager:notice("fetching deleted merchant:~p", [Id]),
            error;
        {ok, Data} -> utils:list_to_proplists(Data ++ [<<"id">>, BinId])
    end.

create(Props) ->
    Id = utils:uuid(),
    ModifiedTime = [{<<"mtime">>, utils:now()}],

    Fields   = utils:proplists_to_list(Props ++ ModifiedTime),
    Permalink = proplists:get_value(<<"permalink">>, Props),
    Category  = proplists:get_value(<<"category">>, Props),

    Writes = [
        ["HMSET", ["merchant:", Id] | Fields],
        ["SET",   ["merchant-permalink:", Permalink], Id],
        ["LPUSH", ["merchants"], Id],
        ["LPUSH", ["category-merchants:", Category], Id],
        ["HINCRBY", "stats", "merchants", 1],
        ["HINCRBY", "stats", ["category-merchants:", Category], 1]],

    redis:qp(Writes),
    fetch(Id).

update(Id, Props) ->
    ok.

delete(Id) ->
    ok.

validate_on_create(Props) ->
    Spec = [v:required(<<"name">>),
            v:required(<<"category">>,
                       fun(Category) -> lists:member(binary_to_list(Category), ?CATEGORIES) end,
                       invalid_category),
            v:required(<<"permalink">>,
                       fun(Permalink) -> not utils:redis_exists(["merchant-permalink:", Permalink]) end,
                       already_exists),
            v:required(<<"url">>),
            v:optional(<<"image">>),
            v:optional(<<"banner">>),
            v:optional(<<"description">>)],
    v:validate(Props, Spec).

validate_on_update(Props) ->
    ok.