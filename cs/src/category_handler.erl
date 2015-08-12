-module(category_handler).
-include("cs.hrl").

-export([allowed_methods/2]).
-export([process_get/2]).

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

process_get(Req, State) ->
    Result = lists:map(fun(Category) ->
                          {[{name, list_to_binary(Category)},
                            {value, normalize(Category)}]}
                       end, ?CATEGORIES),
    Body = jiffy:encode({[{data, Result}]}),
    {Body, Req, State}.

normalize(S) ->
    F = fun("and") -> "&";
           ([H|T]) -> [string:to_upper(H) | string:to_lower(T)]
        end,
    list_to_binary(string:join(lists:map(F, string:tokens(S, "-")), " ")).