-module(merchant_handler).
-include("cs.hrl").

-export([allowed_methods/2]).
-export([process_get/2, process_post/3]).

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

process_get(Req, State) ->
    {Id, Req1} = cowboy_req:binding(merchant_id, Req),

    case Id of
        undefined ->
           Json = jiffy:encode({[{hello, world}]}),
           {Json, Req1, State};
        Id ->
            % TODO: handle when not found
            Merchant = merchant:fetch(Id),
            Body = jiffy:encode({[{data, {Merchant}}]}),
            {Body, Req1, State}
    end.

process_post(Props, Req, State) ->
    case merchant:validate_on_create(Props) of
        {error, _} = Errors ->
            rest:write_response(Errors, Req, State);
        ok ->
            Merchant = merchant:create(Props),
            rest:write_response({ok, Merchant}, Req, State)
    end.

